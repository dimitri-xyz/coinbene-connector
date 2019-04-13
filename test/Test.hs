{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE UndecidableInstances  #-}
{-# LANGUAGE FlexibleContexts      #-}

{-# LANGUAGE CPP #-}

#ifdef ETHEREUM
#define MACRO_CURRENCY     ETH
#define MACRO_MARKET_NAME  "ethbrl"
#define MACRO_TEST_VOL     0.01
#else
#define MACRO_CURRENCY     BTC
#define MACRO_MARKET_NAME  "btcbrl"
#define MACRO_TEST_VOL     0.002
#endif


module Main where

import           Data.Maybe                   (fromMaybe)
import           Data.Proxy
import           Data.IORef
import           Control.Monad.State
import           Control.Monad.Time
import           Control.Concurrent           (threadDelay)
import           Control.Concurrent.Async     (async, link, cancel)
import           Control.Concurrent.STM.TVar  (newTVarIO, readTVarIO)

import           Test.Tasty
import           Test.Tasty.HUnit
import           Test.Tasty.Options

import           Network.HTTP.Client          (newManager)
import           Network.HTTP.Client.TLS      (tlsManagerSettings)

import           Market.Interface
import           Coinbene.Adapter
import           Coinbene.Executor
import           Coinbene.Producer
import           Coinbene.Connector

import           Market.Coins (BTC(..), USD(..), BRL(..), LTC(..), ETH(..))

import qualified Coinbene as C
import           Coinbene (API_ID(..), API_KEY(..), qbAsks, qbBids, AskQuote(..), BidQuote(..))

import           Debug.Trace

--------------------------------------------------------------------------------
instance IsOption API_ID where
    defaultValue = error "User must supply API ID (on command line or environment) for authenticated tests."
    parseValue = Just . API_ID
    optionName = return "API_ID"
    optionHelp = return "Customer's API ID for Coinbene account (hex encoded)."

instance IsOption API_KEY where
    defaultValue = error "User must supply API secret key (on command line or environment) for authenticated tests."
    parseValue = Just . API_KEY
    optionName = return "API_KEY"
    optionHelp = return "Customer's API secret key for Coinbene account (hex encoded)."

--------------------------------------------------------------------------------
main = defaultMainWithIngredients ings $
    askOption $ \apikey ->
    askOption $ \apiid ->
    withResource
        (mkConfig -- mkMockConfig defaultExchangeState
            C.Silent -- verbosity level for coinbene-api library
            (apiid  :: API_ID)
            (apikey :: API_KEY))
        (\_ -> return ())
        (tests
            C.Silent -- verbosity level for connector itself
            (Proxy :: Proxy (Price BRL))
            (Proxy :: Proxy (Vol MACRO_CURRENCY)))
  where
    ings = includingOptions
        [ (Option (Proxy :: Proxy API_ID))
        , (Option (Proxy :: Proxy API_KEY))
        ] : defaultIngredients

    mkConfig verbosity apiid apikey = do
        manager <- newManager tlsManagerSettings
        return $ Coinbene manager apiid apikey verbosity

mkMockConfig :: ExchangeMockState C.BRL C.MACRO_CURRENCY -> C.Verbosity -> id -> key -> IO (MockCoinbene C.BRL C.MACRO_CURRENCY)
mkMockConfig initialExchangeMockState verbosity _ _ = do
    bks <- newIORef initialExchangeMockState
    return (MC bks verbosity)

--------------------------------------------------------------------------------
{- # Testing instance for `Exchange config m`

The idea of the `IntoIO` typeclass failed. The producer/executor need to interweave
IO calls in between calls to the `Exchange config m` API and it needs to remember
the state as it goes in and out of IO. So, IntoIO is not sufficient. We will just
do this in IO but change the configuration type to a mock one and write the
corresponding instance for testing.

-}
--------------------------------------------------------------------------------
tests :: forall config p v q c p' v'.
      (C.Exchange config IO, Coin p, Coin v, C.Coin p', C.Coin v', Num p', Num v', ToFromCB p p', ToFromCB v v')
      => C.Verbosity -> Proxy (Price p) -> Proxy (Vol v) -> IO config -> TestTree
tests verbosity _ _ getConfig = testGroup
    (" Coinbene Connector Tests for " <> MACRO_MARKET_NAME <> ". Use compiler flags for other markets.")
    [ testCase "Executor - PlaceLimit test" $ do
        -- "Despite it being an IO action, the resource it returns
        -- will be acquired only once and shared across all the tests in the tree."
        config         <- getConfig
        connectorState <- newTVarIO emptyCoinbeneConnector
        executor
            verbosity
            config
            (Proxy :: Proxy IO)
            connectorState
            (\_ -> return ()) -- no event firing
            (PlaceLimit Ask (Price 97000 :: Price p) (Vol MACRO_TEST_VOL :: Vol v) Nothing)

    , testCase "Executor - Place then CancelLimit test" $ do
        config         <- getConfig
        connectorState <- newTVarIO emptyCoinbeneConnector
        executor verbosity config (Proxy :: Proxy IO) connectorState (\ev -> traceOn (verbosity >= C.Verbose) "order placed!" (return ()) )
                            (PlaceLimit Ask (Price 99000 :: Price p) (Vol MACRO_TEST_VOL :: Vol v) (Just $ COID 0))
        executor verbosity config (Proxy :: Proxy IO) connectorState undefined
                            ((CancelLimit $ COID 0) :: Action p v)

    ---------- Tests that only apply to the Mocking Framework ----------
    --
    -- separate exchange instance for each testcase
    --

    , testCase "Producer - orderbook test" $ do
        config         <- mkMockConfig defaultExchangeState C.Silent undefined undefined
        connectorState <- newTVarIO emptyCoinbeneConnector
        evsRef         <- newIORef
            [ BookEv bk1, BookEv bk2, BookEv bk3, BookEv bk1, BookEv bk2 :: TradingEv p v () ()]

        pthread <- async $ producer 1000000 verbosity config (Proxy :: Proxy IO) connectorState (comparingEventHandler evsRef)
        link pthread

        threadDelay 5000000
        cancel pthread

    , testCase "Producer - cancellation detection" $ do
        config         <- mkMockConfig defaultExchangeState C.Silent undefined undefined
        connectorState <- newTVarIO emptyCoinbeneConnector
        evsRef         <- newIORef [ PlaceEv  (Just $ COID 123)
                                   , CancelEv (Just $ COID 123) :: TradingEv p v () ()
                                   ]
        -- calling the executors before starting the producer guarantees the orders have been
        -- placed (and are in the connector's state) before the producer starts
        executor verbosity config (Proxy :: Proxy IO) connectorState (comparingEventHandler evsRef)
                            (PlaceLimit Ask (Price 99000 :: Price p) (Vol 0.005 :: Vol v) (Just $ COID 123))
        pthread <- async $ producer 1000000 verbosity config (Proxy :: Proxy IO) connectorState (dropBookEvs $ comparingEventHandler evsRef)
        link pthread

        threadDelay 5000000
        cancel pthread

        ems <- readIORef evsRef
        assertEqual "Some events were not issued" [] ems

    , testCase "Producer - Partial execution detection" $ do
        config         <- mkMockConfig partialFillMockState C.Silent undefined undefined
        connectorState <- newTVarIO emptyCoinbeneConnector
        evsRef         <- newIORef [ PlaceEv  (Just $ COID 123) -- before producer starts
                                   , PlaceEv  (Just $ COID 456)
                                   , PlaceEv  (Just $ COID 789)

                                   , FillsEv  [FillEv Ask (Price 120000) (Vol 0.001) (Just $ COID 789)] -- first cycle
                                   , FillsEv  [FillEv Bid (Price     77) (Vol 0.003) (Just $ COID 456)]
                                   , DoneEv   (Just $ COID 456) :: TradingEv p v () ()

                                   , FillsEv  [FillEv Ask (Price  99000) (Vol 0.002) (Just $ COID 123)] -- second cycle

                                   , CancelEv (Just $ COID 123) -- third cycle

                                   , FillsEv  [FillEv Ask (Price  88000) (Vol 0.003) (Just $ COID 789)] -- 4th cycle
                                   , DoneEv   (Just $ COID 789)
                                   ]

        -- calling the executors before starting the producer guarantees the orders have been
        -- placed (and are in the connector's state) before the producer starts
        executor verbosity config (Proxy :: Proxy IO) connectorState (comparingEventHandler evsRef)
                            (PlaceLimit Ask (Price 99000 :: Price p) (Vol 0.005 :: Vol v) (Just $ COID 123))
        executor verbosity config (Proxy :: Proxy IO) connectorState (comparingEventHandler evsRef)
                            (PlaceLimit Bid (Price    77 :: Price p) (Vol 0.003 :: Vol v) (Just $ COID 456))
        executor verbosity config (Proxy :: Proxy IO) connectorState (comparingEventHandler evsRef)
                            (PlaceLimit Ask (Price 88000 :: Price p) (Vol 0.004 :: Vol v) (Just $ COID 789))

        pthread <- async $ producer 1000000 verbosity config (Proxy :: Proxy IO) connectorState (dropBookEvs $ comparingEventHandler evsRef)
        link pthread

        threadDelay 5000000
        cancel pthread

        -- state <- readTVarIO connectorState
        -- print state

        ems <- readIORef evsRef
        assertEqual "Some events were not issued" [] ems

    , testCase "Full connector - Partial execution detection" $ do
        config         <- mkMockConfig partialFillMockState C.Silent undefined undefined
        evsRef         <- newIORef [ PlaceEv  (Just $ COID 123) -- before producer starts
                                   , PlaceEv  (Just $ COID 456)
                                   , PlaceEv  (Just $ COID 789)

                                   , FillsEv  [FillEv Ask (Price 120000) (Vol 0.001) (Just $ COID 789)] -- first cycle
                                   , FillsEv  [FillEv Bid (Price     77) (Vol 0.003) (Just $ COID 456)]
                                   , DoneEv   (Just $ COID 456) :: TradingEv p v () ()

                                   , FillsEv  [FillEv Ask (Price  99000) (Vol 0.002) (Just $ COID 123)] -- second cycle

                                   , CancelEv (Just $ COID 123) -- third cycle

                                   , FillsEv  [FillEv Ask (Price  88000) (Vol 0.003) (Just $ COID 789)] -- 4th cycle
                                   , DoneEv   (Just $ COID 789)
                                   ]

        (producer, executor, terminator) <- coinbeneInit 1000000 verbosity config (Proxy :: Proxy IO) (dropBookEvs $ comparingEventHandler evsRef)

        -- calling the executors before starting the producer guarantees the orders have been
        -- placed (and are in the connector's state) before the producer starts
        executor (PlaceLimit Ask (Price 99000 :: Price p) (Vol 0.005 :: Vol v) (Just $ COID 123))
        executor (PlaceLimit Bid (Price    77 :: Price p) (Vol 0.003 :: Vol v) (Just $ COID 456))
        executor (PlaceLimit Ask (Price 88000 :: Price p) (Vol 0.004 :: Vol v) (Just $ COID 789))

        pthread <- async $ producer
        link pthread

        threadDelay 5000000
        cancel pthread

        -- state <- readTVarIO connectorState
        -- print state

        ems <- readIORef evsRef
        assertEqual "Some events were not issued" [] ems

    ]

--------------------------------------------------------------------------------
dropBookEvs :: (Coin p, Coin v) => (TradingEv p v () () -> IO ()) -> TradingEv p v () () -> IO ()
dropBookEvs handler ev = case ev of
    BookEv _ -> return ()
    other    -> handler other

comparingEventHandler :: (Coin p, Coin v) => IORef ([TradingEv p v () ()]) -> TradingEv p v () () -> IO ()
comparingEventHandler evsRef ev = do
    evs <- readIORef evsRef
    assertEqual "Events differ:" (fromMaybe (error "comparingEventHandler - Run out of events to compare") $ safeHead evs) ev
    writeIORef evsRef (tail evs)

safeHead :: [a] -> Maybe a
safeHead []     = Nothing
safeHead (x:xs) = Just x
--------------------------------------------------------------------------------
data MockCoinbene p v = MC (IORef (ExchangeMockState p v)) C.Verbosity

data ExchangeMockState p v =
    EMS { books    :: [C.QuoteBook p v]
        , nextOID  :: Int
        , reqs     :: [Request p v]
        , getOpens :: [[C.OrderInfo]]
        , getInfos :: [C.OrderInfo]
        } deriving (Show, Eq)

data Request p v
    = NewLimit      C.OrderSide (C.Price p) (C.Vol v) C.OrderID
    | GetBook
    | GetOrderInfo  C.OrderID
    | Cancel        C.OrderID
    | GetOpenOrders
    | GetBalances
    | GetTrades
    deriving (Show, Eq)

----------------------------------------
instance forall p v . (Show p, Show v, C.Coin p, C.Coin v) => C.Exchange (MockCoinbene p v) IO where

    -- FIX ME! "DRY" violations gone wild!
    placeLimit (MC ref verbosity) sd p v = do
        traceOn (verbosity >= C.Verbose)
            "PLACELIMIT"
            (return ())
        oid <- atomicModifyIORef' ref (update sd p v)
        traceOn (verbosity >= C.Verbose)
            ("Placing: " <> show (NewLimit sd (from p) (from v) oid :: Request p v))
            (return oid)
      where
        update sd p v ems =
            let oid = C.OrderID $ ":oid:" <> show (nextOID ems)
             in (ems {nextOID = nextOID ems + 10, reqs = NewLimit sd (from p) (from v) oid : reqs ems}, oid)

    getBook (MC ref verbosity) _ _ = do
        traceOn (verbosity >= C.Verbose)
            "GETBOOK"
            (return ())
        bk' <- atomicModifyIORef' ref update
        traceOn (verbosity >= C.Deafening)
            ("Returning orderbook converted `from`: " <> show bk')
            (return $ from $ bk')
      where
        update ems = (ems {books = tail (books ems)}, head (books ems))

    cancel (MC ref verbosity) oid = do
        traceOn (verbosity >= C.Verbose)
            "CANCEL"
            (return ())
        oid <- atomicModifyIORef' ref (update oid)
        traceOn (verbosity >= C.Verbose)
            ("Cancelling: " <> show oid)
            (return oid)
      where
        update oid ems = (ems {reqs = Cancel oid : reqs ems}, oid)

    getOpenOrders (MC ref verbosity) _ _ = do
        traceOn (verbosity >= C.Verbose)
            "GET OPEN ORDERS"
            (return ())
        infos <- atomicModifyIORef' ref update
        traceOn (verbosity >= C.Verbose)
            ("Open orders: " <> show infos)
            (return infos)

      where
        update ems = (ems {getOpens = tail (getOpens ems)}, head (getOpens ems))

    getOrderInfo (MC ref verbosity) oid = do
        traceOn (verbosity >= C.Verbose)
            ("GET ORDER INFO - " <> show oid)
            (return ())
        info <- atomicModifyIORef' ref update
        traceOn (verbosity >= C.Verbose)
            ("Order Info: " <> show info)
            (return info)
      where
        update ems = if (C.orderID $ head $ getInfos ems) == oid
            then (ems {getInfos = tail (getInfos ems)}, head (getInfos ems))
            else error $ "Mismatch on call to getOrderInfo - requested: " <> show oid
                         <> " available: " <> show (C.orderID $ head $ getInfos ems)


    getBalances   = return undefined
    getTrades     = return undefined

--------------------------------------------------------------------------------
class FromVal a b where
    from :: a -> b

instance (C.Coin a, C.Coin b) => FromVal (C.Price a) (C.Price b) where
    from (C.Price a) = C.Price $ C.readBare $ C.showBare a

instance (C.Coin a, C.Coin b) => FromVal (C.Vol a) (C.Vol b) where
    from (C.Vol a) = C.Vol $ C.readBare $ C.showBare a

instance (FromVal (C.Price p) (C.Price p'), FromVal (C.Vol v) (C.Vol v'))
    => FromVal (C.AskQuote p v) (C.AskQuote p' v') where
    from q@(AskQ { aqPrice = p, aqQuantity = v}) = q { aqPrice = from p, aqQuantity = from v}

instance (FromVal (C.Price p) (C.Price p'), FromVal (C.Vol v) (C.Vol v'))
    => FromVal (C.BidQuote p v) (C.BidQuote p' v') where
    from q@(BidQ { bqPrice = p, bqQuantity = v}) = q { bqPrice = from p, bqQuantity = from v}

instance (FromVal (C.AskQuote p v) (C.AskQuote p' v'), FromVal (C.BidQuote p v) (C.BidQuote p' v'))
    => FromVal (C.QuoteBook p v) (C.QuoteBook p' v') where
    from qb@(C.QuoteBook { qbBids = bs, qbAsks = as}) =
         qb { qbBids = from <$> bs
            , qbAsks = from <$> as
            }

--------------------------------------------------------------------------------
-- Values used in the tests
--------------------------------------------------------------------------------
-- Quotebooks
qa1', qa2', qa3', qa4' :: forall p v. (Num p, Num v, C.Coin p, C.Coin v) => C.AskQuote p v
qb1', qb2'             :: forall p v. (Num p, Num v, C.Coin p, C.Coin v) => C.BidQuote p v

qa1' = C.AskQ (C.Price 1000) (C.Vol 1)
qa2' = C.AskQ (C.Price 1100) (C.Vol 3)
qa3' = C.AskQ (C.Price 1500) (C.Vol 1)
qa4' = C.AskQ (C.Price 2000) (C.Vol 1)

qb1' = C.BidQ (C.Price  900) (C.Vol 2)
qb2' = C.BidQ (C.Price  800) (C.Vol 1)

bk1', bk2' :: forall p v q c. (Num p, Num v, C.Coin p, C.Coin v) => C.QuoteBook p v
bk1' = C.QuoteBook { C.qbAsks = [qa1', qa2', qa3', qa4'], C.qbBids = [qb1', qb2'] }
bk2' = C.QuoteBook { C.qbAsks = [qa2'], C.qbBids = [] }
bk3' = C.QuoteBook { C.qbAsks = [], C.qbBids = [] }

                    ------------ mirrors ------------

qa1, qa2, qa3, qa4 :: forall p v q. (Coin p, Coin v) => Quote p v ()
qb1, qb2           :: forall p v q. (Coin p, Coin v) => Quote p v ()

qa1 = Quote Ask (Price 1000) (Vol 1) ()
qa2 = Quote Ask (Price 1100) (Vol 3) ()
qa3 = Quote Ask (Price 1500) (Vol 1) ()
qa4 = Quote Ask (Price 2000) (Vol 1) ()

qb1 = Quote Bid (Price  900) (Vol 2) ()
qb2 = Quote Bid (Price  800) (Vol 1) ()

bk1, bk2 :: forall p v q c. (Coin p, Coin v) => QuoteBook p v () ()
bk1 = QuoteBook {asks = [qa1, qa2, qa3, qa4], bids = [qb1, qb2], counter = ()}
bk2 = QuoteBook {asks = [qa2], bids = [], counter = ()}
bk3 = QuoteBook {asks = [], bids = [], counter = ()}
--------------------------------------------------------------------------------
-- C.OrderInfo returned by mock API calls

unfilled123 =
    C.LimitOrder
    { C.market     = MACRO_MARKET_NAME
    , C.oSide      = C.Ask
    , C.limitPrice = C.Price 99000
    , C.limitVol   = C.Vol   0.005
    , C.orderID    = C.OrderID ":oid:0"
    , C.created    = C.MilliEpoch 1
    , C.mModified  = Nothing
    , C.status     = C.Unfilled
    , C.filledVol  = C.Vol 0
    , C.filledAmount = C.Cost 0
    , C.mAvePriceAndFees = Nothing
    }

canceled123 = unfilled123 {C.status = C.Canceled}

partiallyFilled123 =
    unfilled123
        { C.status = C.PartiallyFilled
        , C.filledVol = C.Vol 0.002
        , C.filledAmount = C.Cost 198
        , C.mAvePriceAndFees = Just (C.Price 99000, C.Cost 0.01)
        }

partiallyCanceled123 = partiallyFilled123 {C.status = C.Canceled}

----------------------------------------
unfilled456 =
    C.LimitOrder
    { C.market     = MACRO_MARKET_NAME
    , C.oSide      = C.Bid
    , C.limitPrice = C.Price 77
    , C.limitVol   = C.Vol   0.003
    , C.orderID    = C.OrderID ":oid:10"
    , C.created    = C.MilliEpoch 2
    , C.mModified  = Nothing
    , C.status     = C.Unfilled
    , C.filledVol  = C.Vol 0
    , C.filledAmount = C.Cost 0
    , C.mAvePriceAndFees = Nothing
    }

done456 =
    unfilled456
    { C.status = C.Filled
    , C.filledVol = C.Vol 0.003
    , C.filledAmount = C.Cost 0.231
    , C.mAvePriceAndFees = Just (C.Price 77, C.Cost 0.002)
    }

----------------------------------------
unfilled789 =
    C.LimitOrder
    { C.market     = MACRO_MARKET_NAME
    , C.oSide      = C.Ask
    , C.limitPrice = C.Price 88000
    , C.limitVol   = C.Vol   0.004
    , C.orderID    = C.OrderID ":oid:20"
    , C.created    = C.MilliEpoch 2
    , C.mModified  = Nothing
    , C.status     = C.Unfilled
    , C.filledVol  = C.Vol 0
    , C.filledAmount = C.Cost 0
    , C.mAvePriceAndFees = Nothing
    }

partiallyFilled789 =
    unfilled789
        { C.status = C.PartiallyFilled
        , C.filledVol = C.Vol 0.001
        , C.filledAmount = C.Cost 120
        , C.mAvePriceAndFees = Just (C.Price 120000, C.Cost 0.12)
        }

done789 =
    partiallyFilled789
    { C.status = C.Filled
    , C.filledVol = C.Vol 0.004
    , C.filledAmount = C.Cost 384
    , C.mAvePriceAndFees = Just (C.Price 96000, C.Cost 0.2)
    }


--------------------------------------------------------------------------------
-- Exchange API responses

defaultExchangeState = EMS [bk1', bk2', bk3', bk1', bk2'] 0 [] [[],[],[],[],[]]
    [unfilled123, unfilled123, canceled123]

partialFillMockState = EMS [bk1', bk2', bk3', bk1', bk2'] 0 []
    -- each first line entry starts a producer cycle
    [[done456]                       , [partiallyFilled123, partiallyFilled789] ,[partiallyFilled789] , [],[],[]]
    [ partiallyFilled789, unfilled123,                                            partiallyCanceled123,  done789]

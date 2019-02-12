{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE UndecidableInstances  #-}

module Main where

import           Data.Proxy
import           Data.IORef
import           Control.Monad.State
import           Control.Monad.Time
import           System.IO                    (hPutStrLn, stderr)
import           Control.Concurrent           (threadDelay)
import           Control.Concurrent.Async     (async, link)
import           Control.Concurrent.STM.TVar  (newTVarIO)

import           Test.Tasty
import           Test.Tasty.HUnit
import           Test.Tasty.Options

import           Network.HTTP.Client          (newManager)
import           Network.HTTP.Client.TLS      (tlsManagerSettings)

import           Market.Interface
import           Coinbene.Adapter
import           Coinbene.Executor
import           Coinbene.Producer

import           Market.Coins (BTC(..), USD(..), BRL(..), LTC(..))

import qualified Coinbene as C
import           Coinbene (API_ID(..), API_KEY(..), qbAsks, qbBids, AskQuote(..), BidQuote(..))

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
    withResource (mkConfig apiid apikey) (\_ -> return ()) $ tests (Proxy :: Proxy (Price BRL)) (Proxy :: Proxy (Vol BTC))
  where
    ings = includingOptions
        [ (Option (Proxy :: Proxy API_ID))
        , (Option (Proxy :: Proxy API_KEY))
        ] : defaultIngredients

    mkConfig apiid apikey = do 
        manager <- newManager tlsManagerSettings
        return $ Coinbene manager apiid apikey

--------------------------------------------------------------------------------
tests :: forall p v q c p' v'. (Coin p, Coin v, C.Coin p', C.Coin v', Num p', Num v', ToFromCB p p', ToFromCB v v') 
      => Proxy (Price p) -> Proxy (Vol v) -> IO Coinbene -> TestTree
tests _ _ getConfig = testGroup " Coinbene Connector Tests"
    -- [ testCase "Executor - PlaceLimit test" $ do
    --     -- "Despite it being an IO action, the resource it returns will be acquired only once and shared across all the tests in the tree."
    --     config         <- getConfig
    --     connectorState <- newTVarIO emptyCoinbeneConnector
    --     executor (Proxy :: Proxy IO)
    --         config
    --         connectorState
    --         (\_ -> return ()) -- no event firing
    --         (PlaceLimit Ask (Price 22000 :: Price p) (Vol 0.005 :: Vol v) Nothing)
    
    -- [ testCase "Executor - Place then CancelLimit test" $ do
    --     config         <- getConfig
    --     connectorState <- newTVarIO emptyCoinbeneConnector
    --     executor (Proxy :: Proxy IO) config connectorState (\ev -> putStrLn "ORDER PLACED!") (PlaceLimit Ask (Price 19000 :: Price p) (Vol 0.005 :: Vol v) (Just $ COID 0))
    --     executor (Proxy :: Proxy IO) config connectorState undefined                         ((CancelLimit $ COID 0) :: Action p v)

    [ testCase "Producer - orderbook test" $ do
        config         <- getConfig
        connectorState <- newTVarIO emptyCoinbeneConnector
        booksRef       <- newIORef [bk1, bk1, bk1, bk1, bk1, bk1, bk1, bk1, bk1, bk1 :: QuoteBook p v () ()]  -- FIX ME! proper test is [bk1, bk2]

        pthread <- async $ producer 1000000 (Proxy :: Proxy (TimedLogger p' v')) config connectorState (bookHandler booksRef)

        link pthread
        threadDelay 10000000

    ]

bookHandler :: (Coin p, Coin v) => IORef ([QuoteBook p v () ()]) -> TradingEv p v () () -> IO ()
bookHandler ref (BookEv bk) = do
    bks <- readIORef ref
    assertEqual "Produced books differ" (head bks) bk
    writeIORef ref (tail bks)


--------------------------------------------------------------------------------
newtype TestLog p v = TestLog [C.QuoteBook p v]
type TimedLogger p v = State (TestLog p v)

instance HTTP (TimedLogger p v) where
    http = undefined

instance MonadTime (TimedLogger p v) where
    currentTime = undefined

instance (Num p, Num v, C.Coin p, C.Coin v) => IntoIO (TimedLogger p v) where
    intoIO ma = return $ evalState ma (TestLog ([bk1', bk2']:: [C.QuoteBook p v]))
--------------------------------------------------------------------------------
{- FIX ME! This is a major fail. I don't think this can work.
I wanted to make the p and v parameters in `TimedLogger p v` match the same
parameters in the Exchange class signature

class Exchange config m where
    placeLimit    :: (HTTP m, MonadTime m, Coin p, Coin v) => config -> OrderSide -> Price p -> Vol v -> m OrderID

So, we have an extra "forall". A given `TimedLogger p v` fixes a given p and v and thus, this type canNOT have a  
`Exchange Coinbene` instance because in an exchange instance a single monadic type works for all markets. The
`TimedLogger p v` would only work for a single market.

There is also a problem with IO. We will need to interweave IO actions within a stateful computation. The current
IntoIO mechanism seems insufficient for this.

I have to think of a different way to make the testing work.
-}

instance forall p v . (C.Coin p, C.Coin v) => C.Exchange Coinbene (TimedLogger p v) where
    placeLimit    = return undefined

    getBook _ _ _ = do
        book <- gets (\(TestLog bs) -> head bs)
        return (from book)

    getOrderInfo  = return undefined
    cancel        = return undefined
    getOpenOrders = return undefined
    getBalances   = return undefined
    getTrades     = return undefined

--------------------------------------------------------------------------------
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

--------------------------------------------------------------------------------
class FromVal a b where
    from :: a -> b 

instance (C.Coin a, C.Coin b) => FromVal (C.Price a) (C.Price b) where
    from (C.Price a) = C.Price $ C.readBare $ C.showBare a

instance (C.Coin a, C.Coin b) => FromVal (C.Vol a) (C.Vol b) where
    from (C.Vol a) = C.Vol $ C.readBare $ C.showBare a

instance (FromVal (C.Price p) (C.Price p'), FromVal (C.Vol v) (C.Vol v')) => FromVal (C.AskQuote p v) (C.AskQuote p' v') where
    from q@(AskQ { aqPrice = p, aqQuantity = v}) = q { aqPrice = from p, aqQuantity = from v} 

instance (FromVal (C.Price p) (C.Price p'), FromVal (C.Vol v) (C.Vol v')) => FromVal (C.BidQuote p v) (C.BidQuote p' v') where
    from q@(BidQ { bqPrice = p, bqQuantity = v}) = q { bqPrice = from p, bqQuantity = from v} 

instance (FromVal (C.AskQuote p v) (C.AskQuote p' v'), FromVal (C.BidQuote p v) (C.BidQuote p' v')) => FromVal (C.QuoteBook p v) (C.QuoteBook p' v') where
    from qb@(C.QuoteBook { qbBids = bs, qbAsks = as}) =
         qb { qbBids = from <$> bs
            , qbAsks = from <$> as
            }

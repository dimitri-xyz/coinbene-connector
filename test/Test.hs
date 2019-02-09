{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE UndecidableInstances  #-}

module Main where

import           Data.Proxy
import           Control.Monad.State
import           Control.Monad.Time
import           Control.Concurrent.STM.TVar  (newTVarIO)

import           Test.Tasty
import           Test.Tasty.HUnit
import           Test.Tasty.Options

import           Network.HTTP.Client          (newManager)
import           Network.HTTP.Client.TLS      (tlsManagerSettings)

import           Market.Interface
import           Coinbene.Adapter
import           Coinbene.Executor

import           Market.Coins (BTC(..), USD(..), BRL(..), LTC(..))

import qualified Coinbene as C
import           Coinbene (API_ID(..), API_KEY(..))

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
tests :: forall p v q c p' v'. (Coin p, Coin v, C.Coin p', C.Coin v', ToFromCB p p', ToFromCB v v') 
      => Proxy (Price p) -> Proxy (Vol v) -> IO Coinbene -> TestTree
tests _ _ getConfig = testGroup " Coinbene Connector Tests"
    -- [ testCase "Executor - PlaceLimit test" $ do
    --     -- "Despite it being an IO action, the resource it returns will be acquired only once and shared across all the tests in the tree."
    --     config <- getConfig 
    --     executor (Proxy :: Proxy IO)
    --         config
    --         undefined
    --         undefined
    --         (PlaceLimit Ask (Price 19000 :: Price p) (Vol 0.005 :: Vol v) Nothing)
    
    [ testCase "Executor - Place then CancelLimit test" $ do
        config         <- getConfig
        connectorState <- newTVarIO emptyCoinbeneConnector
        executor (Proxy :: Proxy IO) config connectorState (\_ -> return ()) (PlaceLimit Ask (Price 19000 :: Price p) (Vol 0.005 :: Vol v) (Just $ COID 0))
        executor (Proxy :: Proxy IO) config connectorState undefined ((CancelLimit $ COID 0) :: Action p v)



        -- :: Executor TimedLogger p v 
        -- outputPairs <- interpret (selfUpdateState (copyBookStrategy 5) emptyState) (copyInEs :: [Maybe(TradingEv p v q c)])
        -- let outputActions = fmap fst <$> outputPairs
        -- assertEqual "Output list does not match" copyExpoOKAs (fmap removeReasoning <$> outputActions)
    ]

-- executor _proxy _config _state _handler action = return undefined

-- data Action price vol
--     = PlaceLimit
--         { aSide  :: OrderSide
--         , aPrice :: Price price
--         , aVol   :: Vol   vol
--         , amCOID :: Maybe ClientOID }
--     | CancelLimit
--         { aCOID  :: ClientOID }

--------------------------------------------------------------------------------
-- data CoinbeneMockConfig = CoinbeneMockConfig


-- class Monad m => MonadTime m where
--   currentTime :: m UTCTime

-- class Monad m => HTTP m where
--     http :: Request -> Manager -> m (Response LBS.ByteString)

-----------------------------------------
-- instance HTTP IO where
--     http = httpLbs

-- instance MonadTime IO where
--   currentTime = getCurrentTime

newtype TestLog = TestLog Int
type TimedLogger = State TestLog

instance HTTP TimedLogger where
    http = undefined

instance MonadTime TimedLogger where
    currentTime = undefined

instance IntoIO TimedLogger where
    intoIO ma = return $ evalState ma (TestLog 0)



-- class Exchange config m where
--     placeLimit    :: (HTTP m, MonadTime m, Coin p, Coin v) => config -> OrderSide -> Price p -> Vol v -> m OrderID
--     getBook       :: (HTTP m, MonadTime m, Coin p, Coin v) => config -> Proxy (Price p) -> Proxy (Vol v) -> m (QuoteBook p v)
--     getOrderInfo  :: (HTTP m, MonadTime m) => config -> OrderID -> m OrderInfo
--     cancel        :: (HTTP m, MonadTime m) => config -> OrderID -> m OrderID
--     getOpenOrders :: (HTTP m, MonadTime m, Coin p, Coin v) => config -> Proxy (Price p) -> Proxy (Vol v) -> m [OrderInfo]
--     getBalances   :: (HTTP m, MonadTime m) => config            -> m [BalanceInfo]
--     getTrades     :: (HTTP m, MonadTime m, Coin p, Coin v) => config -> Proxy (Price p) -> Proxy (Vol v) -> m [Trade p v]

instance C.Exchange Coinbene TimedLogger where
    placeLimit    = return undefined
    getBook       = return undefined
    getOrderInfo  = return undefined
    cancel        = return undefined
    getOpenOrders = return undefined
    getBalances   = return undefined
    getTrades     = return undefined


-- {-# LANGUAGE ScopedTypeVariables #-}

-- module Main where

-- import Reactive.Banana
-- import Reactive.Banana.Frameworks.Extended
-- import Pipes.Concurrent


-- import Control.Concurrent.STM

-- import Strategy

-- import Market.Types (Coin(..), StrategyAdvice(..))
-- import Market.Coins (BTC(..), USD(..))

-- import Interface

-- import qualified Data.HashMap.Strict as H

-- --------------------------------------------------------------------------------
-- main :: IO ()
-- main = defaultMain $ tests (undefined :: Price BTC) (undefined :: Vol USD)
-- --------------------------------------------------------------------------------

-- tests :: forall p v q c. (Coin p, Coin v) => Price p -> Vol v -> TestTree
-- tests _ _ = testGroup " Trading Strategy Tests"
--     [ testCase "copyBookStrategy - exposure ok" $ do
--         outputPairs <- interpret (selfUpdateState (copyBookStrategy 5) emptyState) (copyInEs :: [Maybe(TradingEv p v q c)])
--         let outputActions = fmap fst <$> outputPairs
--         assertEqual "Output list does not match" copyExpoOKAs (fmap removeReasoning <$> outputActions)

--     , testCase "copyBookStrategy - restricted exposure" $ do
--         outputPairs <- interpret (selfUpdateState (copyBookStrategy 3) emptyState) (copyInEs :: [Maybe(TradingEv p v q c)])
--         let outputActions = fmap fst <$> outputPairs
--         assertEqual "Output list does not match" copyExpoRestrictedAs (fmap removeReasoning <$> outputActions)

--     , testCase "refillAsksStrategy" $ do
--         outputPairs <- interpret (selfUpdateState refillAsksStrategy refillInitialState) (refillInEs :: [Maybe(TradingEv p v q c)])
--         let outputActions = fmap fst <$> outputPairs
--             outputStates  = fmap snd <$> outputPairs
--         assertEqual "Output list does not match"        refillExpectedAs (fmap removeReasoning <$> outputActions)
--         assertEqual "Final output state does not match" refillFinalState (last outputStates)

--     , testCase "exposureControl" $ do
--         let exposureControl' = fmap (fmap (fmap (fmap (\action -> (mempty, action))))) exposureControl
--         outputPairs <- interpret
--                             (selfUpdateState exposureControl' expoInitialState)
--                             (fmap snd expoOutInEs :: [Maybe(TradingEv p v q c)])
--         let outputStates = fmap snd <$> outputPairs
--         assertEqual "Final output state does not match" 
--             (fmap fst (expoOutInEs :: [(Maybe (Vol v), Maybe (TradingEv p v q c))]) ) (fmap realizedExposure <$> outputStates)

--     , testCase "mirrorStrategy - Output/State" $ do
--         outputEvents <- interpret (uncurry (mirrorStrategy 5) . split) (binaryIns :: [Maybe (Either (TradingEv p v q c) (TradingEv p v q c))])
--         assertEqual "Output list does not match" binaryExpectedAs (fmap removeComments <$> outputEvents)

--     , testCase "mirrorStrategy - Refill reissuance" $ do
--         outputEvents <- interpret (uncurry (mirrorStrategy 3) . split) (refillIssuanceIns :: [Maybe (Either (TradingEv p v q c) (TradingEv p v q c))])
--         assertEqual "Output list does not match" refillIssuanceExpectedAs (fmap removeComments <$> outputEvents)

--     , testCase "mirrorStrategy - Late cancellation reissuance" $ do
--         outputEvents <- interpret (uncurry (mirrorStrategy 5) . split) (lateCancellationIssuanceIns :: [Maybe (Either (TradingEv p v q c) (TradingEv p v q c))])
--         assertEqual "Output list does not match" lateCancellationIssuanceExpectedAs (fmap removeComments <$> outputEvents)

--     ]

-- removeReasoning :: StrategyAdvice a -> StrategyAdvice a
-- removeReasoning (Advice (r, a)) = Advice ("", a)

-- removeComments 
--     :: ( Maybe (StrategyAdvice (Action p v)), Maybe (StrategyAdvice (Action p v)))
--     -> ( Maybe (StrategyAdvice (Action p v)), Maybe (StrategyAdvice (Action p v)))
-- removeComments (a,b) = (fmap removeReasoning a, fmap removeReasoning b)

-- --------------------------------------------------------------------------------
-- qa1, qa2, qa3, qa4, qb1 :: forall p v q. (Coin p, Coin v) => Quote p v q 

-- qa1 = Quote Ask (Price 1000) (Vol 1) undefined
-- qa2 = Quote Ask (Price 2000) (Vol 1) undefined
-- qa3 = Quote Ask (Price 1000) (Vol 3) undefined
-- qa4 = Quote Ask (Price 1500) (Vol 1) undefined

-- qb1 = Quote Bid (Price  900) (Vol 1) undefined


-- bk1, bk2, bk3, bk4 :: forall p v q c. (Coin p, Coin v) => QuoteBook p v q c

-- bk1 = QuoteBook {bids = [qb1], asks = [qa1],     counter = undefined}
-- bk2 = QuoteBook {bids = [],    asks = [qa1,qa2], counter = undefined}
-- bk3 = QuoteBook {bids = [qb1], asks = [qa3,qa2], counter = undefined}
-- bk4 = QuoteBook {bids = [],    asks = [qa4,qa2], counter = undefined}

-- copyInEs :: forall p v q c. (Coin p, Coin v) => [Maybe (TradingEv p v q c)]
-- copyInEs = 
--     [ Nothing
--     , Just $ BookEv   bk1
--     , Just $ BookEv   bk2
--     , Nothing
--     , Nothing
--     , Just $ BookEv   bk3
--     , Just $ BookEv   bk2
--     , Just $ BookEv   bk2
--     , Just $ BookEv   bk4
--     ]

-- copyExpoOKAs :: forall p v. (Coin p, Coin v) => [Maybe (StrategyAdvice (Action p v))]
-- copyExpoOKAs =
--     [ Nothing
--     , Just $ Advice ("", ZipList [PlaceLimit Ask (Price 1000) (Vol 1) (Just 0)])
--     , Just $ mempty
--     , Nothing
--     , Nothing
--     , Just $ Advice ("", ZipList [PlaceLimit Ask (Price 1000) (Vol 2) (Just 1)])
--     , Just $ Advice ("", ZipList [CancelLimit 0, CancelLimit 1])
--     , Just $ mempty -- nothing happens, waits to see `CancelEv`
--     , Just $ Advice ("", ZipList [PlaceLimit Ask (Price 1500) (Vol 1) (Just 2)]) 
--     ]

-- copyExpoRestrictedAs :: forall p v. (Coin p, Coin v) => [Maybe (StrategyAdvice (Action p v))]
-- copyExpoRestrictedAs =
--     [ Nothing
--     , Just $ Advice ("", ZipList [PlaceLimit Ask (Price 1000) (Vol 1) (Just 0)])
--     , Just $ mempty
--     , Nothing
--     , Nothing
--     , Just $ Advice ("", ZipList [PlaceLimit Ask (Price 1000) (Vol 2) (Just 1)])
--     , Just $ Advice ("", ZipList [CancelLimit 0, CancelLimit 1])
--     , Just $ mempty
--     , Just $ mempty -- cannot place order, already at maximum exposure
--     ]

-- ------------------------------------------------------------------------------
-- -- Action with ClientOID = Just 2 is somehow missing, but for this event to have been received from the framework, it
-- -- must be the case, that we created this order in the past (otherwise the event would not be dispatched to the strategy)
-- -- Thus, we will ask for a refill. Is this the failure mode we want, though? "when in doubt, refill"
-- refillInEs :: forall p v q c. (Coin p, Coin v) => [Maybe (TradingEv p v q c)]
-- refillInEs = 
--     [ Nothing
--     , Just $ BookEv bk2
--     , Just $ FillsEv [FillEv Ask (Price 1000) (Vol 1) (Just 0)]
--     , Just $ PlaceEv Nothing
--     , Just $ CancelEv (Just 7)
--     , Nothing
--     , Just $ CancelEv (Just 444)
--     , Just $ FillsEv [ FillEv Ask (Price 1500) (Vol 2) (Just 1)
--                      , FillEv Ask (Price 1000) (Vol 3) (Just 0)]
--     , Just $ CancelEv (Just 9)
--     , Just $ DoneEv   (Just 0)
--     ]

-- refillExpectedAs :: forall p v. (Coin p, Coin v) => [Maybe (StrategyAdvice (Action p v))]
-- refillExpectedAs =
--     [ Nothing
--     , Just mempty
--     , Just $ Advice ("", ZipList [PlaceLimit Bid (Price 1000) (Vol 1) Nothing])
--     , Just mempty
--     , Just mempty
--     , Nothing
--     , Just mempty
--     , Just $ Advice ("", ZipList [ PlaceLimit Bid (Price 1500) (Vol 2) Nothing
--                                  , PlaceLimit Bid (Price 1000) (Vol 3) Nothing])
--     , Just mempty
--     , Just mempty
--     ]

-- refillInitialState :: forall p v. (Coin p, Coin v) => ActionState p v
-- refillInitialState = 
--     ActionState
--         { openActionsMap = H.fromList 
--             [ (0, OpenAction {oaSide = Ask, oaPrice = Price 1000, oaVolume = Vol 4, oaCancelled = False, oaExecdVol  = Vol 0})
--             , (1, OpenAction {oaSide = Ask, oaPrice = Price 1500, oaVolume = Vol 5, oaCancelled = False, oaExecdVol  = Vol 1})
--             , (8, OpenAction {oaSide = Ask, oaPrice = Price 3000, oaVolume = Vol 5, oaCancelled = False, oaExecdVol  = Vol 1})
--             , (9, OpenAction {oaSide = Ask, oaPrice = Price 3000, oaVolume = Vol 7, oaCancelled = False, oaExecdVol  = Vol 5})
--             , (7, OpenAction {oaSide = Ask, oaPrice = Price 5000, oaVolume = Vol 2, oaCancelled = False, oaExecdVol  = Vol 1})
--             ]
--         , nextCOID = 10
--         , realizedExposure = Vol (0 :: v)
--         }

-- refillFinalState :: forall p v. (Coin p, Coin v) => Maybe (ActionState p v)
-- refillFinalState = Just $
--     ActionState
--         { openActionsMap = H.fromList 
--             [ (1, OpenAction {oaSide = Ask, oaPrice = Price 1500, oaVolume = Vol 5, oaCancelled = False, oaExecdVol  = Vol 3})
--             , (8, OpenAction {oaSide = Ask, oaPrice = Price 3000, oaVolume = Vol 5, oaCancelled = False, oaExecdVol  = Vol 1})
--             ]
--         , nextCOID = 10
--         , realizedExposure = Vol (6 :: v)
--         }

-- --------------------------------------------------------------------------------
-- expoInitialState :: forall p v. (Coin p, Coin v) => ActionState p v
-- expoInitialState =
--     ActionState
--         { openActionsMap = H.fromList 
--             [ (1, (OpenAction {oaSide = Ask, oaPrice = Price 1500, oaVolume = Vol 5, oaCancelled = False, oaExecdVol  = Vol 3}) )
--             , (8, (OpenAction {oaSide = Ask, oaPrice = Price 3000, oaVolume = Vol 5, oaCancelled = False, oaExecdVol  = Vol 1}) )]
--         , nextCOID = 10
--         , realizedExposure = Vol (6 :: v)
--         }

-- expoOutInEs :: forall p v q c. (Coin p, Coin v) => [(Maybe (Vol v), Maybe (TradingEv p v q c))]
-- expoOutInEs =
--    -- pairs represent: (realized exposure volume just after event, event) 
--     [ (Nothing     , Just $ BookEv bk1)
--     , (Just (Vol 6), Just $ FillsEv [])
--     , (Nothing     , Just $ PlaceEv undefined)
--     , (Nothing     , Nothing)
--     , (Just (Vol 1), Just $ FillsEv [ FillEv Bid (Price 1500) (Vol 2) Nothing
--                                     , FillEv Bid (Price 1000) (Vol 3) Nothing ])
--     , (Nothing     , Just $ BookEv bk3)
--     , (Nothing     , Just $ CancelEv (Just 333))
--     , (Just (Vol 0), Just $ FillsEv [ FillEv Bid (Price 1500) (Vol 1) Nothing ])
--     ]

-- --------------------------------------------------------------------------------
-- -- tests for tracking the orderbook
-- --
-- -- The model adopted here is the one used by reactive-banana.
-- -- Each item on this list happened at a different time (in sequence).
-- -- The outer Maybe defines whether an event happened at this time or not, Nothing means time passed but nothing happened this instant.
-- -- the inner maybes specify whether there is something to do at this time on the corresponding market.
-- -- In other words, an output event has occurred and there should be something to do on at least one exchange.
-- -- the value: Just (Nothing, Nothing) is an "implementation glitch" and should never occur. 


-- binaryIns :: forall p v q c. (Coin p, Coin v) => [Maybe (Either (TradingEv p v q c) (TradingEv p v q c))]
-- binaryIns =
--     [ Nothing
--     , Just $ Left $ FillsEv  []
--     , Just $ Left $ BookEv   bk1
--     , Just $ Left $ PlaceEv  undefined
--     , Just $ Left $ BookEv   bk2
--     , Nothing
--     , Nothing
--     , Nothing
--     , Just     $ Right $ FillsEv [FillEv Ask (Price 1000) (Vol 0.2) (Just 0)]
--     , Just $ Left $ CancelEv (Just 333)
--     , Just $ Left $ BookEv   bk3
--     , Just $ Left $ PlaceEv  undefined
--     , Just $ Left $ BookEv   bk4
--     ]

-- binaryExpectedAs :: forall p v. (Coin p, Coin v) => [ Maybe ( Maybe (StrategyAdvice (Action p v)), Maybe (StrategyAdvice (Action p v)) )]
-- binaryExpectedAs =
--     [ Nothing
--     , Nothing
--     , Just $ (Nothing, Just $ Advice ("", ZipList [PlaceLimit Ask (Price 1000) (Vol 1) (Just 0)]))
--     , Nothing
--     , Just $ (Nothing, Just mempty)
--     , Nothing
--     , Nothing
--     , Nothing
--     , Just $ (Just (Advice ("",ZipList {getZipList = [PlaceLimit Bid (Price 1000) (Vol 0.2) Nothing]})), Nothing)
--     , Nothing
--     , Just $ (Nothing, Just $ Advice ("", ZipList [PlaceLimit Ask (Price 1000) (Vol 2.2) (Just 1)]))
--     , Nothing
--     , Just $ (Nothing, Just $ Advice ("", ZipList [CancelLimit 0, CancelLimit 1, PlaceLimit Ask (Price 1500) (Vol 1) (Just 2) ]))
--     ]


-- --------------------------------------------------------------------------------
-- -- test for unnecessary re-issuance of placements for target that has not yet been refilled.

-- refillIssuanceIns :: forall p v q c. (Coin p, Coin v) => [Maybe (Either (TradingEv p v q c) (TradingEv p v q c))]
-- refillIssuanceIns =
--     [ Nothing
--     , Just $ Left $ BookEv   bk3
--     , Just     $ Right $ FillsEv [FillEv Ask (Price 1000) (Vol 0.2) (Just 0)]
--     , Just $ Left $ BookEv   bk3  -- no exposure available
--     , Just $ Left $ FillsEv [FillEv Bid (Price 1000) (Vol 0.2) Nothing]
--     , Just $ Left $ BookEv   bk3  -- now re-issue target
--     , Just $ Left $ BookEv   bk4  -- clear old levels
--     , Just     $ Right $ CancelEv (Just 0)
--     , Just $ Left $ BookEv   bk4  -- now re-issue target (now the exposure permits)
--     ]

-- refillIssuanceExpectedAs :: forall p v. (Coin p, Coin v) => [ Maybe ( Maybe (StrategyAdvice (Action p v)), Maybe (StrategyAdvice (Action p v)) )]
-- refillIssuanceExpectedAs =
--     [ Nothing
--     , Just (Nothing, Just $ Advice ("", ZipList [PlaceLimit Ask (Price 1000) (Vol 3) (Just 0)]))
--     , Just (Just (Advice ("",ZipList {getZipList = [PlaceLimit Bid (Price 1000) (Vol 0.2) Nothing]})), Nothing)
--     , Just (Nothing, Just mempty)
--     , Nothing
--     , Just (Nothing, Just $ Advice ("", ZipList [PlaceLimit Ask (Price 1000) (Vol 0.2) (Just 1)]))
--     , Just (Nothing, Just $ Advice ("", ZipList [ CancelLimit {aCOID = COID 0}
--                                                 , CancelLimit {aCOID = COID 1}
--                                                 ]))
--     , Just (Just mempty, Nothing)
--     , Just (Nothing, Just $ Advice ("",ZipList [PlaceLimit Ask (Price 1500) (Vol 1.00) (Just 2)]))
--     ]

-- --------------------------------------------------------------------------------
-- -- test for unnecessary re-issuance of placement for target that has been cancelled.

-- lateCancellationIssuanceIns :: forall p v q c. (Coin p, Coin v) => [Maybe (Either (TradingEv p v q c) (TradingEv p v q c))]
-- lateCancellationIssuanceIns =
--     [ Nothing
--     , Just $ Left $ BookEv   bk3
--     , Just $ Left $ BookEv   bk4  -- no more orders at $1000
--     , Just     $ Right $ FillsEv [FillEv Ask (Price 1000) (Vol 3) (Just 0)] -- cancellation was too late, order executed
--     , Just $ Left $ BookEv   bk3  -- only 1 BTC available (3 executed, not recouped, and 1 open tracking book4)
--     , Just     $ Right $ DoneEv (Just 0)
--     , Just $ Left $ FillsEv [FillEv Bid (Price 900) (Vol 3) Nothing]
--     , Just $ Left $ BookEv   bk3  -- now place the other 2
--     ]

-- lateCancellationIssuanceExpectedAs :: forall p v. (Coin p, Coin v) => [ Maybe ( Maybe (StrategyAdvice (Action p v)), Maybe (StrategyAdvice (Action p v)) )]
-- lateCancellationIssuanceExpectedAs =
--     [ Nothing
--     , Just (Nothing, Just $ Advice ("", ZipList [ PlaceLimit Ask (Price 1000) (Vol 3) (Just 0)]))
--     , Just (Nothing, Just $ Advice ("", ZipList [ CancelLimit 0
--                                                 , PlaceLimit Ask (Price 1500) (Vol 1) (Just 1)
--                                                 ]))
--     , Just (Just (Advice ("",ZipList {getZipList = [PlaceLimit Bid (Price 1000) (Vol 3) Nothing]})), Nothing)
--     , Just (Nothing, Just $ Advice ("", ZipList [ CancelLimit 1, PlaceLimit Ask (Price 1000) (Vol 1) (Just 2)]))
--     , Just (Just mempty, Nothing)
--     , Nothing
--     , Just (Nothing, Just $ Advice ("", ZipList [ PlaceLimit Ask (Price 1000) (Vol 2) (Just 3) ]))
--     ]

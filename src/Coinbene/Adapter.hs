module Coinbene.Adapter where

import Coinbene

import Market.Interface

-- XI ME! Here should these be?
type Producer p v q c = IO ()
type Executor p v     = Action p v -> IO ()
type Terminator       = IO ()

-- FIX ME! Not adding dependency on Reactive.Banana for now, so we need this.
type Handler a = a -> IO()


-- class Exchange config m where
--     placeLimit    :: (HTTP m, MonadTime m, Coin p, Coin v) => config -> OrderSide -> Price p -> Vol v -> m OrderID
--     getBook       :: (HTTP m, MonadTime m, Coin p, Coin v) => config -> Proxy (Price p) -> Proxy (Vol v) -> m (QuoteBook p v)
--     getTrades     :: (HTTP m, MonadTime m, Coin p, Coin v) => config -> Proxy (Price p) -> Proxy (Vol v) -> m [Trade p v]
--     getOpenOrders :: (HTTP m, MonadTime m, Coin p, Coin v) => config -> Proxy (Price p) -> Proxy (Vol v) -> m [OrderInfo]
--     getOrderInfo  :: (HTTP m, MonadTime m) => config -> OrderID -> m OrderInfo
--     cancel        :: (HTTP m, MonadTime m) => config -> OrderID -> m OrderID
--     getBalances   :: (HTTP m, MonadTime m) => config            -> m [BalanceInfo]


-- {-# LANGUAGE GeneralizedNewtypeDeriving #-}

-- module Interface 
--     ( module Interface
--     , Price(..)
--     , Vol(..)
--     , OrderSide(..)
--     , Quote(..)
--     , QuoteBook(..)
--     ) where

-- import Data.Hashable
-- import Market.Types ( Price(..)
--                     , Vol(..)
--                     , OrderSide(..)
--                     , Quote(..)
--                     , QuoteBook(..)
--                     )

-- ---------------------------------------
-- newtype ClientOID = COID Int deriving (Show, Eq, Num, Hashable)

-- data FillEv price vol
--   = FillEv
--     { fSide  :: OrderSide
--     , fPrice :: Price price   -- the price that was actually used
--     , fVol   :: Vol   vol     -- the volume executed in this fill
--     , fmCOID :: Maybe ClientOID
--     }
--     deriving (Show, Eq)

-- data TradingEv price vol quoteTail counter
--     = PlaceEv   (Maybe ClientOID)
--     | CancelEv  (Maybe ClientOID)
--     | DoneEv    (Maybe ClientOID)
--     | FillsEv   [FillEv price vol]
--     | BookEv    (QuoteBook price vol quoteTail counter)
--     deriving (Show, Eq)

-- data Action price vol
--     = PlaceLimit
--         { aSide  :: OrderSide
--         , aPrice :: Price price
--         , aVol   :: Vol   vol
--         , amCOID :: Maybe ClientOID }
--     | CancelLimit
--         { aCOID  :: ClientOID }
--     deriving (Show, Eq)










































-- {-# LANGUAGE OverloadedStrings #-}
-- {-# LANGUAGE DeriveGeneric #-}
-- {-# LANGUAGE GeneralizedNewtypeDeriving #-}

-- module Coinbene.Core where

-- -----------------------------------------
-- -- From coinbene-api
-- class (Generic coin, FromJSON coin) => Coin coin where
--   coinSymbol :: Proxy coin -> String
--   showBare   :: coin -> String
--   readBare   :: String -> coin


-- -- From market-model
-- class (NFData coin, RealFrac coin, Show coin, Hashable coin) => Coin coin where
--   name :: coin -> String

-- -----------------------------------------
-- -- Units
-- newtype Vol   a = Vol   a deriving (Show, Eq, Ord, Num, Fractional, Real, RealFrac, Generic)
-- newtype Price a = Price a deriving (Show, Eq, Ord, Num, Fractional, Real, RealFrac, Generic)
-- newtype Cost  a = Cost  a deriving (Show, Eq, Ord, Num, Fractional, Real, RealFrac, Generic)

-- -- For showing the values without the constructor
-- showBarePrice :: Coin p => Price p -> String
-- showBareVol   :: Coin p => Vol   p -> String
-- showBareCost  :: Coin p => Cost  p -> String
-- showBarePrice (Price p) = showBare p
-- showBareVol   (Vol   v) = showBare v
-- showBareCost  (Cost  c) = showBare c

-- -----------------------------------------
-- newtype MilliEpoch = MilliEpoch Word64 deriving (Show, Eq, Ord, Generic, Num, Real, Enum, Integral, FromJSON)

-- showBareMilliEpoch :: MilliEpoch -> String
-- showBareMilliEpoch (MilliEpoch w) = show w

-- newtype OrderID  = OrderID String deriving (Show, Eq, Ord, Generic, FromJSON)
-- data OrderSide   = Bid | Ask deriving (Show, Eq)
-- data OrderStatus = Filled | Unfilled | PartiallyFilled | Canceled | PartiallyCanceled deriving (Show, Eq)

-- data AskQuote p v
--   = AskQ
--     { aqPrice    :: Price p
--     , aqQuantity :: Vol   v
--     } deriving (Show, Eq, Generic)

-- data BidQuote p v
--   = BidQ
--     { bqPrice    :: Price p
--     , bqQuantity :: Vol   v
--     } deriving (Show, Eq, Generic)

-- data QuoteBook p v
--   = QuoteBook
--     { qbAsks::[AskQuote p v]
--     , qbBids::[BidQuote p v]
--     } deriving (Show, Eq, Generic)

-- -----------------------------------------
-- data OrderInfo =
--     LimitOrder
--     { market     :: String
--     , oSide      :: OrderSide
--     , limitPrice :: Price Scientific
--     , limitVol   :: Vol   Scientific
--     , orderID    :: OrderID
--     , created      :: MilliEpoch
--     , mModified    :: Maybe MilliEpoch
--     , status       :: OrderStatus
--     , filledVol    :: Vol  Scientific
--     , filledAmount :: Cost Scientific
--     , mAvePriceAndFees :: Maybe (Price Scientific, Cost Scientific) -- (average price, fees), nothing means "don't know"
--     }
--     deriving (Show, Eq)

-- -----------------------------------------
-- data BalanceInfo =
--     BalanceInfo
--         { biAsset     :: String
--         , biAvailable :: Cost Scientific
--         , biReserved  :: Cost Scientific
--         , biTotal     :: Cost Scientific
--         }
--         deriving (Show)

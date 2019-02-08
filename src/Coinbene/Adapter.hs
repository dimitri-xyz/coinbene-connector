{-# LANGUAGE ScopedTypeVariables    #-}
{-# LANGUAGE MultiParamTypeClasses  #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE UndecidableInstances   #-}
{-# LANGUAGE NoImplicitPrelude      #-}

module Coinbene.Adapter
    ( module Coinbene.Adapter
    , C.Coinbene(..)
    , C.HTTP
    , C.http
    ) where

import           Prelude hiding (lookup)
import           Data.Hashable
import           Data.HashMap.Strict
import           Control.Monad.Time

import qualified Coinbene as C
import           Market.Interface
import           Market.Coins

---------------------------------------
type Producer   p v q c = IO ()
type Executor   p v     = Action p v -> IO ()
type Terminator         = IO ()

class Monad m => IntoIO m where
    intoIO :: m a -> IO a

instance IntoIO IO where
    intoIO = id
---------------------------------------

-- We need this to avoid adding a dependency on reactive-banana
type Handler a = a -> IO()


--------------------------------------------------------------------------------
class ToFromCB a b | a -> b, b -> a where
    toCB   :: a -> b
    fromCB :: b -> a

----------
instance ToFromCB OrderSide C.OrderSide where
    toCB Ask = C.Ask
    toCB Bid = C.Bid

    fromCB C.Ask = Ask
    fromCB C.Bid = Bid

instance ToFromCB a b => ToFromCB (Price a) (C.Price b) where
    toCB   (Price a)   = C.Price (toCB a)
    fromCB (C.Price b) = Price (fromCB b)

instance ToFromCB a b => ToFromCB (Vol a) (C.Vol b) where
    toCB   (Vol a)   = C.Vol (toCB a)
    fromCB (C.Vol b) = Vol (fromCB b)
----------

instance ToFromCB USD C.USDT where
    toCB   a = realToFrac a
    fromCB b = realToFrac b

instance ToFromCB BRL C.BRL where
    toCB   a = realToFrac a
    fromCB b = realToFrac b

instance ToFromCB BTC C.BTC where
    toCB   a = realToFrac a
    fromCB b = realToFrac b

instance ToFromCB LTC C.LTC where
    toCB   a = realToFrac a
    fromCB b = realToFrac b

--------------------------------------------------------------------------------

data CoinbeneState = CoinbeneState [(C.OrderID, Maybe ClientOID)]

-- class Exchange config m where
--     placeLimit    :: (HTTP m, MonadTime m, Coin p, Coin v) => config -> OrderSide -> Price p -> Vol v -> m OrderID
--     cancel        :: (HTTP m, MonadTime m) => config -> OrderID -> m OrderID



data MainAuxMap k1 k2 v = MainAuxMap { mainM :: HashMap k1 (Maybe k2, v), auxM :: HashMap k2 k1 }

lookupMain :: (Eq k1, Hashable k1, Eq k2, Hashable k2) => k1 -> MainAuxMap k1 k2 v -> Maybe (Maybe k2, v)
lookupMain k1 map = lookup k1 (mainM map) -- does NOT check for inconsistency

lookupAux  :: (Eq k1, Hashable k1, Eq k2, Hashable k2) => k2 -> MainAuxMap k1 k2 v -> Maybe (      k1, v)
lookupAux k2 map =
    case lookup k2 (auxM map) of 
        Nothing -> Nothing
        Just k1 -> case lookupMain k1 map of
            Nothing        -> error "lookupAux - missing entry for primary key"
            Just (mk2', v) -> if mk2' == Just k2 
                then Just (k1, v) 
                else error "lookupAux - main entry linking back to wrong secondary key"

-- if k1 does not exist then k2 should not exist or be `Nothing` 
-- if k1 exist then k2 should match existing value
insertMain :: (Eq k1, Hashable k1, Eq k2, Hashable k2) => k1 -> Maybe k2 -> v -> MainAuxMap k1 k2 v -> MainAuxMap k1 k2 v
insertMain k1 mk2 v map = case lookupMain k1 map of
    Nothing -> case mk2 of 
                Nothing -> insertOverwritePair k1 mk2 v map -- new pair (k2 = Nothing)
                Just k2 -> case lookupAux k2 map of
                    Nothing -> insertOverwritePair k1 mk2 v map -- new pair, previously unused k2
                    Just _  -> error "insert - cannot insert in map: cannot create entry with already used secondary key"
    Just (mk2', _) -> if mk2 == mk2'
            -- both exist, and are associated to each other, overwrite
            then insertOverwritePair k1 mk2 v map 
            -- both exist, but they are not associated to each other
            else error "insert - cannot insert keys in map: cannot overwrite entry with different secondary key"
  where
    insertOverwritePair k1  Nothing  v map = map {mainM = insert k1 (Nothing, v) (mainM map)}
    insertOverwritePair k1 (Just k2) v map = map {mainM = insert k1 (Just k2, v) (mainM map), auxM = insert k2 k1 (auxM map)}


-- | deletes corresponding entry from map, if present
deleteMain :: (Eq k1, Hashable k1, Eq k2, Hashable k2) => k1 -> MainAuxMap k1 k2 v -> MainAuxMap k1 k2 v
deleteMain k1 map =
    case lookupMain k1 map of
        Nothing           -> map
        Just (Nothing, v) -> map {mainM = delete k1 (mainM map)}
        Just (Just k2, v) -> map {mainM = delete k1 (mainM map), auxM = delete k2 (auxM map)}

deleteAux  :: forall k1 k2 v. (Eq k1, Hashable k1, Eq k2, Hashable k2) => k2 -> MainAuxMap k1 k2 v -> MainAuxMap k1 k2 v
deleteAux k2 map = case lookupAux k2 map of
    Nothing      -> map
    Just (k1, _) -> deleteMain k1 map
































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

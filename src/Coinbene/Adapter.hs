{-# LANGUAGE ScopedTypeVariables    #-}
{-# LANGUAGE MultiParamTypeClasses  #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE UndecidableInstances   #-}
{-# LANGUAGE NoImplicitPrelude      #-}
{-# LANGUAGE FlexibleInstances      #-}

module Coinbene.Adapter
    ( module Coinbene.Adapter
    , C.Coinbene(..)
    , C.HTTP
    , C.http
    ) where

import           Prelude hiding (lookup)
import           Data.Hashable
import           Data.HashMap.Strict
import           Data.Scientific
import           Control.Monad.Time
import           Control.Concurrent.STM.TVar

import qualified Coinbene as C
import           Market.Interface
import           Market.Coins
import           Debug.Trace

---------------------------------------
traceOn :: Bool -> String -> a -> a
traceOn pred msg val =  if pred then trace msg val else val

---------------------------------------
type Producer   config p v q c = IO ()
type Executor   config p v     = Action p v -> IO ()
type Terminator config         = IO ()

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

instance ToFromCB ETH C.ETH where
    toCB   a = realToFrac a
    fromCB b = realToFrac b

----------
instance (ToFromCB p p', ToFromCB v v') => ToFromCB (Quote p v ()) (C.AskQuote p' v') where
    toCB   = undefined
    fromCB (C.AskQ p v) = Quote { side = Ask, price = fromCB p, volume = fromCB v, qtail = () }

instance ToFromCB (Quote p v ()) (C.AskQuote p' v') => ToFromCB (QuoteBook p v () ()) (C.QuoteBook p' v') where
    toCB   book  = undefined
    fromCB book' = QuoteBook { bids = toBid . fromCB . coinBeneBidsToAsks  <$> C.qbBids book', asks = fromCB <$> C.qbAsks book', counter = () }
      where
        coinBeneBidsToAsks :: C.BidQuote p' v' -> C.AskQuote p' v'
        coinBeneBidsToAsks (C.BidQ p v) = C.AskQ p v

        toBid :: Quote p v q -> Quote p v q
        toBid q = q {side = Bid}

--------------------------------------------------------------------------------
instance Hashable C.OrderID

type CoinbeneConnector  = MainAuxMap C.OrderID ClientOID ConnectorOrderInfo
emptyCoinbeneConnector = emptyM

data ConnectorOrderInfo =
    FillStatus
    { oSide        :: C.OrderSide
    , limitPrice   :: C.Price Scientific
    , limitVol     :: C.Vol   Scientific
    , mModified    :: Maybe C.MilliEpoch
    , status       :: C.OrderStatus
    , filledVol    :: C.Vol  Scientific
    , filledAmount :: C.Cost Scientific
    -- (average price, fees), nothing means "don't know"
    , mAvePriceAndFees :: Maybe (C.Price Scientific, C.Cost Scientific)
    } deriving (Show, Eq)

--------------------------------------------------------------------------------
data MainAuxMap k1 k2 v = MainAuxMap { mainM :: HashMap k1 (k2, v), auxM :: HashMap k2 k1 } deriving Show

emptyM :: MainAuxMap k1 k2 v
emptyM = MainAuxMap { mainM = empty :: HashMap k1 (k2, v), auxM = empty :: HashMap k2 k1 }

lookupMain :: (Eq k1, Hashable k1, Eq k2, Hashable k2) => k1 -> MainAuxMap k1 k2 v -> Maybe (k2, v)
lookupMain k1 map = lookup k1 (mainM map) -- does NOT check for inconsistency

lookupAux  :: (Eq k1, Hashable k1, Eq k2, Hashable k2) => k2 -> MainAuxMap k1 k2 v -> Maybe (k1, v)
lookupAux k2 map =
    case lookup k2 (auxM map) of
        Nothing -> Nothing
        Just k1 -> case lookupMain k1 map of
            Nothing       -> error "lookupAux - missing entry for primary key"
            Just (k2', v) -> if k2' == k2
                then Just (k1, v)
                else error "lookupAux - main entry linking back to wrong secondary key"

-- | insertMain inserts a new association
-- if k1 does not exist then k2 should not exist
-- if k1 exist then k2 should match existing value
-- otherwise error
insertMain :: (Eq k1, Hashable k1, Eq k2, Hashable k2) => k1 -> k2 -> v -> MainAuxMap k1 k2 v -> MainAuxMap k1 k2 v
insertMain k1 k2 v map = case lookupMain k1 map of
    Nothing -> case lookupAux k2 map of
                    Nothing -> insertOverwritePair k1 k2 v map -- new pair, previously unused k2
                    Just _  -> error "insert - cannot insert in map: cannot create entry with already used secondary key"
    Just (k2', _) -> if k2 == k2'
            -- both exist, and are associated to each other, overwrite
            then insertOverwritePair k1 k2 v map
            -- both exist, but they are not associated to each other
            else error "insert - cannot insert keys in map: cannot overwrite entry with different secondary key"
  where
    insertOverwritePair k1 k2 v map = map {mainM = insert k1 (k2, v) (mainM map), auxM = insert k2 k1 (auxM map)}

-- | deletes corresponding entry from map, if present
deleteMain :: (Eq k1, Hashable k1, Eq k2, Hashable k2) => k1 -> MainAuxMap k1 k2 v -> MainAuxMap k1 k2 v
deleteMain k1 map =
    case lookupMain k1 map of
        Nothing           -> map
        Just (k2, v) -> map {mainM = delete k1 (mainM map), auxM = delete k2 (auxM map)}

deleteAux  :: forall k1 k2 v. (Eq k1, Hashable k1, Eq k2, Hashable k2) => k2 -> MainAuxMap k1 k2 v -> MainAuxMap k1 k2 v
deleteAux k2 map = case lookupAux k2 map of
    Nothing      -> map
    Just (k1, _) -> deleteMain k1 map

keys :: MainAuxMap k1 k2 v -> [(k1, k2)]
keys map = toList (fst <$> mainM map)

adjustMain :: (Eq k1, Hashable k1) => (v -> v) -> k1 -> MainAuxMap k1 k2 v -> MainAuxMap k1 k2 v
adjustMain f k1 hmap =
    let f' (k, v) = (k, f v)
     in hmap {mainM = adjust f' k1 (mainM hmap)}

{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Coinbene.Executor where

import           System.IO      (hPutStr, hPutStrLn, stderr)


import           Data.Hashable
import qualified Coinbene as C
import           Coinbene.Adapter

import           Market.Interface

-- later, we will have to...
-- import Interface
-- for now:
-- newtype ClientOID = COID Int deriving (Show, Eq, Num, Hashable)


-- class Exchange config m where
--     placeLimit    :: (HTTP m, MonadTime m, Coin p, Coin v) => config -> OrderSide -> Price p -> Vol v -> m OrderID
--     cancel        :: (HTTP m, MonadTime m) => config -> OrderID -> m OrderID

data CoinbeneState = CoinbeneState [(C.OrderID, Maybe ClientOID)]  


---------------------------------------
execState :: (Coin p, Coin v) => config -> state -> Handler (TradingEv p v q c) -> Executor p v
execState _config _state _handler = print

termState :: (Coin p, Coin v) => config -> state -> Handler (TradingEv p v q c) -> Terminator
termState _config _state _handler = hPutStrLn stderr "\nExecutor exiting!"

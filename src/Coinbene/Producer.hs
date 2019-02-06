module Coinbene.Producer where

import           Data.Hashable
import           Market.Interface
import qualified Coinbene as C

import           Coinbene.Adapter

prodState :: (Coin p, Coin v) => config -> state -> Handler (TradingEv p v q c) -> Producer p v q c
prodState _config _state _handler = return ()

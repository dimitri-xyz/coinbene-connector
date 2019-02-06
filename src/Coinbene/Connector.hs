module Coinbene.Connector where

import           Data.Hashable
import qualified Coinbene as C

import           Market.Interface

import           Coinbene.Adapter
import           Coinbene.Executor
import           Coinbene.Producer

coinbeneInitializer :: (Coin p, Coin v) => Handler (TradingEv p v q c) -> IO (Producer p v q c, Executor p v, Terminator)
coinbeneInitializer fireEvents = return (prodState undefined undefined fireEvents, execState undefined undefined fireEvents, termState undefined undefined fireEvents)




{-# LANGUAGE ScopedTypeVariables #-}

module Coinbene.Producer where

import           Data.Proxy
import           Control.Monad.Time
import           Control.Concurrent.STM.TVar

import           Market.Interface
import           Coinbene.Adapter

---------------------------------------
producer 
    :: forall m p v q c. (HTTP m, MonadTime m, IntoIO m, Coin p, Coin v) 
    => Proxy m -> Coinbene -> TVar CoinbeneConnector -> Handler (TradingEv p v q c) 
    -> Producer p v q c
producer _proxy _config _state _handler = return undefined

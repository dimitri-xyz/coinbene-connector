{-# LANGUAGE ScopedTypeVariables #-}

module Coinbene.Producer where

import           Data.Proxy
import           Control.Monad.Time

import           Market.Interface
import           Coinbene.Adapter

---------------------------------------
-- producer :: (HTTP m, MonadTime m, Coin p, Coin v) => Coinbene -> CoinbeneState -> Handler (TradingEv p v q c) -> Producer p v q c
-- producer _config _state _handler = return undefined

producer 
    :: forall m p v q c. (HTTP m, MonadTime m, IntoIO m, Coin p, Coin v) 
    => Proxy m -> Coinbene -> CoinbeneState -> Handler (TradingEv p v q c) 
    -> Producer p v q c
producer _proxy _config _state _handler = return undefined

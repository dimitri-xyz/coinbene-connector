{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts    #-}

module Coinbene.Connector where

import           Data.Proxy
import           Data.Hashable
import           Control.Monad.Time
import           Control.Concurrent.STM.TVar

import qualified Coinbene as C

import           Market.Interface

import           Coinbene.Adapter
import           Coinbene.Executor
import           Coinbene.Producer

coinbeneInit
    :: forall m p v q c p' v'. ( C.Exchange Coinbene m, HTTP m, MonadTime m, IntoIO m
                               , Coin p, Coin v, C.Coin p', C.Coin v'
                               , (ToFromCB p p'), (ToFromCB v v')
                               , ToFromCB (QuoteBook p v q c) (C.QuoteBook p' v')
                               )
    => Proxy m -> Int -> Handler (TradingEv p v q c)
    -> IO (Producer p v q c, Executor p v, Terminator)

coinbeneInit proxy interval fireEvents =
    return  ( producer interval proxy undefined undefined fireEvents
            , executor          proxy undefined undefined fireEvents
            , terminator        proxy undefined undefined fireEvents)




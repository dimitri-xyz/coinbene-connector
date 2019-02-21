{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts    #-}

module Coinbene.Connector where

import           Data.Proxy
import           Data.Hashable
import           Control.Monad.Time
import           Control.Concurrent.STM.TVar
import           System.Environment           (getEnv)
import           Network.HTTP.Client          (newManager)
import           Network.HTTP.Client.TLS      (tlsManagerSettings)

import qualified Coinbene as C

import           Market.Interface

import           Coinbene.Adapter
import           Coinbene.Executor
import           Coinbene.Producer

coinbeneInit
    :: forall config m p v q c p' v'.
    ( C.Exchange config m, HTTP m, MonadTime m, IntoIO m
    , Coin p, Coin v, C.Coin p', C.Coin v'
    , (ToFromCB p p'), (ToFromCB v v')
    , ToFromCB (QuoteBook p v q c) (C.QuoteBook p' v')
    )
    => Int -> config -> Proxy m -> Handler (TradingEv p v q c)
    -> IO (Producer config p v q c, Executor config p v, Terminator config)

coinbeneInit interval config proxy fireEvents = do
    connectorState <- newTVarIO emptyCoinbeneConnector
    return  ( producer interval config proxy connectorState fireEvents
            , executor          config proxy connectorState fireEvents
            , terminator        config proxy connectorState fireEvents)

getCoinbeneConfig :: IO Coinbene
getCoinbeneConfig = do
        apiid  <- getEnv "COINBENE_API_ID"
        apikey <- getEnv "COINBENE_API_KEY"
        manager <- newManager tlsManagerSettings
        return $ Coinbene manager (C.API_ID apiid) (C.API_KEY apikey)



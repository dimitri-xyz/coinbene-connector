{-# LANGUAGE ScopedTypeVariables  #-}
{-# LANGUAGE FlexibleContexts     #-}

module Coinbene.Producer where

import           Data.Proxy
import           Data.List                    (find)

import           Control.Monad                (forever)
import           Control.Monad.State

import           Control.Exception            (finally)
import           Control.Monad.Time
import           Control.Concurrent           (threadDelay)
import           Control.Concurrent.Async     (async, link, wait, cancel)
import           Control.Concurrent.STM.TVar
import           Control.Concurrent.STM       (atomically)

import           Market.Interface
import           Coinbene.Adapter
import qualified Coinbene as C

---------------------------------------
producer
    :: forall config m p v q c p' v'.  ( C.Exchange config m, HTTP m, MonadTime m, IntoIO m
                                , Coin p, Coin v
                                , C.Coin p', C.Coin v'
                                , (ToFromCB p p'), (ToFromCB v v')
                                , ToFromCB (QuoteBook p v q c) (C.QuoteBook p' v')
                                ) 
    => Int -> config -> Proxy m -> TVar CoinbeneConnector -> Handler (TradingEv p v q c) 
    -> Producer config p v q c
producer interval config proxy state handler = do
    bkThread <- async bookThread
    link bkThread
    -- This thread must cancel the inner bookThread if it receives an exception.
    finally detectThread (cancel bkThread) 

  where
    bookThread = forever $ do
        book <- intoIO $ ( C.getBook config (Proxy :: Proxy (C.Price p')) (Proxy :: Proxy (C.Vol v')) :: m (C.QuoteBook p' v'))
        handler (BookEv $ fromCB $ book)
        threadDelay interval

    detectThread = forever $ do
        infos <- intoIO (C.getOpenOrders config (Proxy :: Proxy (C.Price p')) (Proxy :: Proxy (C.Vol v')) :: m [C.OrderInfo])

        snapshotMainAuxMap <- readTVarIO state
        -- can't use snapshot as basis for update as it may be immediately outdated,
        -- but we will use snapshot to defined what oids will be looked at in this polling cycle
        let oids = fst <$> keys snapshotMainAuxMap
            infoMatches oid info = C.orderID info == oid

        events <- forM oids $ \oid -> case find (infoMatches oid) infos of
            Just info -> updateConnectorTVar oid info state
            Nothing   -> do
                info <- intoIO (C.getOrderInfo config oid :: m C.OrderInfo) -- FIX ME! API never fails! No Maybe!
                updateConnectorTVar oid info state

        -- dispatch all events
        -- ALSO remove entries for `CancelEv` and `DoneEv` events

        threadDelay interval

    updateConnectorTVar :: C.OrderID -> C.OrderInfo -> TVar CoinbeneConnector -> IO [TradingEv p v q c]
    updateConnectorTVar oid info connector =
        atomically $ stateTVar connector $ runState (updateConnectorState oid info)

    updateConnectorState :: C.OrderID -> C.OrderInfo -> State CoinbeneConnector [TradingEv p v q c]
    updateConnectorState = error "Not implemented yet dude!"



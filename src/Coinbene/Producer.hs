{-# LANGUAGE ScopedTypeVariables  #-}
{-# LANGUAGE FlexibleContexts     #-}

module Coinbene.Producer where

import           Data.Proxy
import           Control.Monad                (forever)
import           Control.Exception            (finally)
import           Control.Monad.Time
import           Control.Concurrent           (threadDelay)
import           Control.Concurrent.Async     (async, link, wait, cancel)
import           Control.Concurrent.STM.TVar

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
        infos <- intoIO $ ( C.getOpenOrders config (Proxy :: Proxy (C.Price p')) (Proxy :: Proxy (C.Vol v')) :: m [C.OrderInfo])

        threadDelay interval

-- getOpenOrders :: (HTTP m, MonadTime m, Coin p, Coin v) => config -> Proxy (Price p) -> Proxy (Vol v) -> m [OrderInfo]
-- getOrderInfo  :: (HTTP m, MonadTime m) => config -> OrderID -> m OrderInfo


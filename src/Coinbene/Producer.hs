{-# LANGUAGE ScopedTypeVariables  #-}
{-# LANGUAGE FlexibleContexts     #-}

module Coinbene.Producer where

import           Data.Proxy
import           Control.Monad                (forever)
import           Control.Monad.Time
import           Control.Concurrent           (threadDelay)
import           Control.Concurrent.Async     (async, link, wait)
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
        -- start orderbook thread
        bkThread <- async (bookThread interval)
        link bkThread
        -- now do other stuff
        -- ...
        wait bkThread
  where
    bookThread interval = forever $ do
        book <- intoIO $ ( C.getBook config (Proxy :: Proxy (C.Price p')) (Proxy :: Proxy (C.Vol v')) :: m (C.QuoteBook p' v'))
        handler (BookEv $ fromCB $ book)
        threadDelay interval

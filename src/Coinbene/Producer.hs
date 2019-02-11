{-# LANGUAGE ScopedTypeVariables  #-}
{-# LANGUAGE FlexibleContexts     #-}

module Coinbene.Producer where

import           Data.Proxy
import           Control.Monad                (forever)
import           Control.Monad.Time
import           Control.Concurrent           (threadDelay)
import           Control.Concurrent.Async     (async, link)
import           Control.Concurrent.STM.TVar

import           Market.Interface
import           Coinbene.Adapter
import qualified Coinbene as C

---------------------------------------
producer
    :: forall m p v q c p' v'.  ( C.Exchange Coinbene m, HTTP m, MonadTime m, IntoIO m
                                , Coin p, Coin v
                                , C.Coin p', C.Coin v'
                                , (ToFromCB p p'), (ToFromCB v v')
                                , ToFromCB (QuoteBook p v q c) (C.QuoteBook p' v')
                                ) 
    => Int -> Proxy m -> Coinbene -> TVar CoinbeneConnector -> Handler (TradingEv p v q c) 
    -> Producer p v q c
producer interval proxy config state handler = do
        -- start orderbook thread
        bkt <- async (bookThread interval)
        link bkt
        -- now do other stuff
        -- ...
  where
    bookThread interval = forever $ do
        book <- intoIO $ ( C.getBook config (Proxy :: Proxy (C.Price p')) (Proxy :: Proxy (C.Vol v')) :: m (C.QuoteBook p' v'))
        handler (BookEv $ fromCB $ book)
        threadDelay interval

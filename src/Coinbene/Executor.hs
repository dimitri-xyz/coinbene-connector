{-# LANGUAGE ScopedTypeVariables  #-}
{-# LANGUAGE FlexibleContexts     #-}
{-# LANGUAGE UndecidableInstances #-}

module Coinbene.Executor where

import           Data.Proxy
import           System.IO           (hPutStr, hPutStrLn, stderr)
import           Control.Monad.Time

import           Coinbene.Adapter
import           Market.Interface

import qualified Coinbene as C


-- class Exchange config m where
--     placeLimit    :: (HTTP m, MonadTime m, Coin p, Coin v) => config -> OrderSide -> Price p -> Vol v -> m OrderID
--     cancel        :: (HTTP m, MonadTime m) => config -> OrderID -> m OrderID



---------------------------------------
executor
    :: forall m p v q c p' v'.  ( C.Exchange Coinbene m, HTTP m, MonadTime m, IntoIO m
                                , Coin p, Coin v
                                , C.Coin p', C.Coin v'
                                , (ToFromCB p p'), (ToFromCB v v')
                                ) 
    => Proxy m -> Coinbene -> CoinbeneState -> Handler (TradingEv p v q c) 
    -> Executor p v
executor proxy config _state _handler (PlaceLimit sd price vol mCOID)
    = do
        a <- intoIO $ ( (C.placeLimit config (toCB sd) (toCB price) (toCB vol) ) :: m C.OrderID )
        print a

terminator 
    :: forall m p v q c. (HTTP m, MonadTime m, IntoIO m, Coin p, Coin v) 
    => Proxy m -> Coinbene -> CoinbeneState -> Handler (TradingEv p v q c) 
    -> Terminator
terminator _proxy _config _state _handler = hPutStrLn stderr "\nExecutor exiting!"

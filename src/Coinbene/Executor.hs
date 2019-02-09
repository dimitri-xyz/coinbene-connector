{-# LANGUAGE ScopedTypeVariables  #-}
{-# LANGUAGE FlexibleContexts     #-}
{-# LANGUAGE UndecidableInstances #-}

module Coinbene.Executor where

import           Data.Proxy
import           System.IO                    (hPutStr, hPutStrLn, stderr)
import           Control.Monad.Time
import           Control.Monad.State
import           Control.Monad.STM            (atomically)
import           Control.Concurrent.STM.TVar

import           Coinbene.Adapter
import           Market.Interface

import qualified Coinbene as C

---------------------------------------
executor
    :: forall m p v q c p' v'.  ( C.Exchange Coinbene m, HTTP m, MonadTime m, IntoIO m
                                , Coin p, Coin v
                                , C.Coin p', C.Coin v'
                                , (ToFromCB p p'), (ToFromCB v v')
                                ) 
    => Proxy m -> Coinbene -> TVar CoinbeneConnector -> Handler (TradingEv p v q c) 
    -> Executor p v
executor proxy config state handler (PlaceLimit sd price vol mCOID) = do
    oid <- intoIO $ ( (C.placeLimit config (toCB sd) (toCB price) (toCB vol) ) :: m C.OrderID )
    -- must fire event before updating connector state, see `doc/connector-architecture.md`
    handler (PlaceEv mCOID)
    insertNewOrderInConnectorState mCOID oid state

    print oid -- FIX ME! remove me...

  where
    insertNewOrderInConnectorState :: Maybe ClientOID -> C.OrderID -> TVar CoinbeneConnector -> IO ()
    insertNewOrderInConnectorState mcoid oid connector =
        atomically $ stateTVar state $ runState (insertOrder mcoid oid)

    insertOrder :: Maybe ClientOID -> C.OrderID -> State CoinbeneConnector ()
    insertOrder mcoid oid = do
        orderMap <- get
        put (insertMain oid mcoid () orderMap)

executor proxy config state _handler (CancelLimit coid) = do
    moid <- lookupOID coid state
    case moid of
        Nothing  -> error $ "executor - could not cancel order for ClientOID: " 
                            <> show coid <> " no matching OrderID."
        Just oid -> do
            intoIO ( C.cancel config oid :: m C.OrderID )
            return ()

  where
    lookupOID :: ClientOID -> TVar CoinbeneConnector -> IO (Maybe C.OrderID)
    lookupOID coid connector = atomically $ stateTVar state $ runState (lookupOID' coid)

    lookupOID' :: ClientOID -> State CoinbeneConnector (Maybe C.OrderID)
    lookupOID' coid = do
        orderMap <- get
        return $ fmap fst $ lookupAux coid orderMap


terminator 
    :: forall m p v q c. (HTTP m, MonadTime m, IntoIO m, Coin p, Coin v) 
    => Proxy m -> Coinbene -> TVar CoinbeneConnector -> Handler (TradingEv p v q c) 
    -> Terminator
terminator _proxy _config _state _handler = hPutStrLn stderr "\nExecutor exiting!"

{-# LANGUAGE ScopedTypeVariables  #-}
{-# LANGUAGE FlexibleContexts     #-}
{-# LANGUAGE UndecidableInstances #-}

module Coinbene.Executor where

import           Data.Proxy
import           Data.Scientific
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
    :: forall config m p v q c p' v'.  ( C.Exchange config m, HTTP m, MonadTime m, IntoIO m
                                , Coin p, Coin v
                                , C.Coin p', C.Coin v'
                                , (ToFromCB p p'), (ToFromCB v v')
                                ) 
    => config -> Proxy m -> TVar CoinbeneConnector -> Handler (TradingEv p v q c) 
    -> Executor config p v
executor config proxy state handler (PlaceLimit sd price@(Price p) vol@(Vol v) mCOID) = do
    oid <- intoIO $ ( (C.placeLimit config (toCB sd) (toCB price) (toCB vol) ) :: m C.OrderID )
    -- must fire event before updating connector state, see `doc/connector-architecture.md`
    handler (PlaceEv mCOID)
    insertNewOrderInConnectorState oid mCOID (toCB sd) (realToFrac p) (realToFrac v) state
  where
    insertNewOrderInConnectorState 
        :: C.OrderID -> Maybe ClientOID -> C.OrderSide -> C.Price Scientific -> C.Vol Scientific
        -> TVar CoinbeneConnector -> IO ()
    insertNewOrderInConnectorState oid mcoid sd p v connector =
        atomically $ stateTVar state $ runState (insertOrder oid mcoid sd p v)

    insertOrder
        :: C.OrderID -> Maybe ClientOID -> C.OrderSide -> C.Price Scientific -> C.Vol Scientific
        -> State CoinbeneConnector ()
    insertOrder oid mcoid sd p v = do
        orderMap <- get
        let newOrder =
                FillStatus
                { oSide      = sd
                , limitPrice = p
                , limitVol   = v
                , mModified  = Nothing
                , status     = C.Unfilled
                , filledVol        = C.Vol  0
                , filledAmount     = C.Cost 0
                , mAvePriceAndFees = Nothing
                }
        put (insertMain oid mcoid newOrder orderMap)

executor config proxy state _handler (CancelLimit coid) = do
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
    :: forall config m p v q c. (HTTP m, MonadTime m, IntoIO m, Coin p, Coin v) 
    => config -> Proxy m -> TVar CoinbeneConnector -> Handler (TradingEv p v q c) 
    -> Terminator config
terminator _config _proxy _state _handler = hPutStrLn stderr "\nExecutor exiting!"

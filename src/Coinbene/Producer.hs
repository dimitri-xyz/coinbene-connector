{-# LANGUAGE ScopedTypeVariables  #-}
{-# LANGUAGE FlexibleContexts     #-}

module Coinbene.Producer where

import           Data.Proxy
import           Data.List                    (find)
import           Data.Maybe                   (fromMaybe)

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
    :: forall config m p v q c p' v'.
        ( C.Exchange config m, HTTP m, MonadTime m, IntoIO m
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
        book <- intoIO (C.getBook config (Proxy :: Proxy (C.Price p')) (Proxy :: Proxy (C.Vol v')) :: m (C.QuoteBook p' v'))
        handler (BookEv $ fromCB $ book)
        threadDelay interval

    detectThread = forever $ do
        infos <- intoIO (C.getOpenOrders config (Proxy :: Proxy (C.Price p')) (Proxy :: Proxy (C.Vol v')) :: m [C.OrderInfo])

        snapshotMainAuxMap <- readTVarIO state
        -- can't use snapshot as basis for update as it may be immediately outdated,
        -- but we will use snapshot to defined what oids will be looked at in this polling cycle
        let oids = fst <$> keys snapshotMainAuxMap
            infoMatches oid info = C.orderID info == oid
            dispatch evs = mapM_ handler evs

        forM oids $ \oid -> do
            newInfo <- case find (infoMatches oid) infos of
                    Just info -> return info
                    Nothing   -> intoIO (C.getOrderInfo config oid :: m C.OrderInfo) -- FIX ME! API never fails! No Maybe!

            fillEvs    <- atomicallyUpdateConnector (updateIfNewer newInfo oid (updateFills newInfo)) state
            dispatch fillEvs

            closePairs <- atomicallyUpdateConnector (updateIfNewer newInfo oid (updateClose newInfo)) state
            dispatch $ concat (snd <$> closePairs)
            forM_ (fst <$> closePairs) (\oid -> atomicallyUpdateConnector (removeEntry oid) state)

        threadDelay interval

    atomicallyUpdateConnector :: State CoinbeneConnector a -> TVar CoinbeneConnector -> IO a
    atomicallyUpdateConnector updater connector = atomically $ stateTVar connector $ runState updater

    updateIfNewer :: C.OrderInfo -> C.OrderID -> State ConnectorOrderInfo [a] -> State CoinbeneConnector [a]
    updateIfNewer newInfo oid updater = do
        connectorMap <- get 
        let mCurInfo = lookupMain oid connectorMap
        case mCurInfo of
            Nothing -> return [] -- order with current orderID has disappeared, skip its update in this polling cycle
            Just (mCoid, curInfo) -> do
                let newTime = fromMaybe (C.created newInfo) (C.mModified newInfo)
                    curTime = fromMaybe 0                   (mModified   curInfo) -- force update if missing
                if newTime < curTime -- sanity test
                    then return [] -- order info returned is stale, skip order update
                    else do
                        let (as, updatedEntry) = runState updater curInfo   -- updateOrder newInfo curInfo
                        put (adjustMain (const updatedEntry) oid connectorMap)
                        return as

    isClosingEv :: TradingEv p v q c -> Bool
    isClosingEv (CancelEv _) = True
    isClosingEv (DoneEv   _) = True
    isClosingEv _            = False

    removeEntry :: C.OrderID -> State CoinbeneConnector ()
    removeEntry oid = do
        dict <- get
        put (deleteMain oid dict)

-- data OrderInfo =
--     LimitOrder
--     { market     :: String
--     , oSide      :: OrderSide
--     , limitPrice :: Price Scientific
--     , limitVol   :: Vol   Scientific
--     , orderID    :: OrderID
--     , created      :: MilliEpoch
--     , mModified    :: Maybe MilliEpoch
--     , status       :: OrderStatus
--     , filledVol    :: Vol  Scientific
--     , filledAmount :: Cost Scientific
--     , mAvePriceAndFees :: Maybe (Price Scientific, Cost Scientific) -- (average price, fees), nothing means "don't know"
--     }

-- data ConnectorOrderInfo =
--     FillStatus
--     { oSide        :: C.OrderSide
--     , limitPrice   :: C.Price Scientific
--     , limitVol     :: C.Vol   Scientific
--     , mModified    :: Maybe C.MilliEpoch
--     , status       :: C.OrderStatus
--     , filledVol    :: C.Vol  Scientific
--     , filledAmount :: C.Cost Scientific
--     -- (average price, fees), nothing means "don't know"
--     , mAvePriceAndFees :: Maybe (C.Price Scientific, C.Cost Scientific)

    -- -- ORIGINAL
    -- updateOrder :: C.OrderInfo -> ConnectorOrderInfo -> (ConnectorOrderInfo,[(C.OrderID, TradingEv p v q c)])
    -- updateOrder newInfo curInfo = (curInfo, []) -- (curInfo, [(undefined , PlaceEv (Just $ COID 5678))])

    updateFills :: C.OrderInfo -> State ConnectorOrderInfo [TradingEv p v q c]
    updateFills newInfo = return []

    updateClose :: C.OrderInfo -> State ConnectorOrderInfo [(C.OrderID, [TradingEv p v q c])]
    updateClose newInfo = return []


### Dispatching `BookEv` events

These events do not change the state of the connector. We will simply use a separate thread to continuously poll the exchange and fire these events.

### Dispatching `PlaceEv` events

It seems natural that the Executor itself should issue `PlaceEv` events immediately after it received a corresponding OrderID. Unfortunately, doing that is a little tricky. The problem is that the executor and producer work on separate execution threads and the framework guarantees that a `PlaceEv` will happen before any corresponding `FillsEv` or other event.

Updates to the state are done using the STM monad, but firing of events is not included in this critical section. The executor has two choices:

1. first update the connector state and then (once outside the critical section) fire the corresponding `PlaceEv`;or,
2. first fire the event then (once outside the event network) update the connector state

Choice (2) means the strategy runs before the connector state is updated, which works but seems unsafe as we permanently lose the ClientOID/OrderID association if something goes haywire before the state update. It guarantees that by the time the producer sees the new order, the event has already been issued.

Choice (1) has a synchronization problem. There is a small time interval between the connector state update and the firing of the event. In this mean time, it would be possible for a `FillsEv` to be produced by the producer working on a separate thread, given that the state has already been updated.

So to use choice (1), either:

a) we have to synchronize producer/executor and make the producer wait for the executor to finish producing the `PlaceEv` before issuing any `FillsEv` events, which opens a whole new can of worms about deadlocks, as both producer and executor are now calling the same event network and both waiting on this event signaling. Or,

b) we move the whole `PlaceEv` logic into the producer thread and let the producer deal with it. The executor must mark a new ClientOID/OrderID pair as "PlaceEv not issued" to let the producer know to issue the event before any others.

*The logic on the producer side will already be significantly more complicated than on the executor side. So, despite the risk of running the strategy before updating the connector's state. For now, we will pick choice (2) and fire the event from within the executor. We can revisit this decision later.*


### Dispatching `FillsEv`, `CancelEv` and `DoneEv` events

This is the core of the connector. These events are obtained from calls to /open-orders.
This is made a little tricky because we have multiple threads so the updates need to be
atomic.

The atomicity would ideally require the event dispatching and state updating to be within
the atomic unit (i.e. transactional), but this is not possible as we are using STM for state
updates and the IO monad for dispatching. We will have to break them up into 2.

It seems doing the state updating before the dispatching leads to a cleaner design. We will
adopt this convention (every time considering if it is correct/better than the alternative).

#### Basic structure

The logic is constantly polling /open-orders as follows:

A) the producer disregards any orders that are not part of the state.
B) for orders that are part of the state, there are two possibilities:

    1. They show up in /open-orders
    2. They do not show up in /open-orders

If they show up in `/open-orders`, they are still open. We just need to check if they have executed more. We do this by comparing their current volume with their "state volume" and creating corresponding fills, if necessary.

If an order is part of the connector state, but does NOT show up in the call to `/open-orders`, then it must be closed. It either executed completely or was cancelled. We need to:

1. Find out what happened to the order (call to `/order/info`)
2. update the "fill state"
3. dispatch any still missing `FillsEv` events
4. remove the order from the state (if `/order/info` confirms it's closed)
5. dispatch a final `CancelEv` or `DoneEv`

#### Synchronization

There are quite a few synchronization issues if we receive stale information in either API call (`/open-orders` or `/order/info`). We will assume that the latest call to `/order/info` is authoritative (with an extra sanity check in the "last modified" field). In other words, if it says the order is still open, we will NOT remove the order from the state. There is no problem with this information being wrong as (because the order will still be in the connector state) we will have other chances to update it.

The executor thread may be continuously adding orders to the connector state. We will take a snapshot of the state
and then only update the orders present in the snapshot (other orders may have to wait for the next polling cycle) but obviously we will *never* drop new orders by using stale state information in state updates.


---<<<<<<<<<<<<<<<<< DON'T BELIEVE ANYTHING BEYOND THIS POINT!!!!.... :-P


be updating all

The updating of the connector state due to fills should be identical if either the order open or already closed,

================================
The Ouput is: A new state, new dispatched events (side effect: made new "read only" calls to the API)

There is a specific ordering of the dispatching vs. state updating (as seen above). So, The output seems to be in a Monad!

Do one, then do the other...

The state updating is modifying

1. call to /open-orders => partition orders

A)    MUST REMAIN               B)    *MIGHT* BE DELETED (see note)
                                  - find out what happened

  - fill state updating           - fill state updating
  - fill event dispatching        - fill event dispatching

                                  - delete order from state
                                  - cancel and done event dispatching


I must wait for the call to /order/info before doing any deletions.

IDEA: get info, where you need it, and then do identical processing on all orders.

it seems the fill dispatching should update the fills. That's just applicative!
It's traversable! :-)

It seems I need a:

traverseWithKeys :: Applicative f => (k1 -> k2 -> v1 -> f v2) -> MainAuxMap k1 k2 v1 -> f (MainAuxMap k1 k2 v2)

??? the effect does two things, in order: dispatch the event, add OID that need to be cancelled to an IORef.
??? we process the list of OIDs later.

But, what about the sequencing? (see above) dispatch first, then update.
Well, I can generate a copy of the map and then later do the update. In other words, I do the traverse generating the IO actions I need and the new map. Then I dispatch all the IO actions and finally do an STM update on the map.

This should take care of the fills.

something like.
traverseWithKeys :: (k1 -> k2 -> v -> IO v) -> MainAuxMap k1 k2 v -> IO (MainAuxMap k1 k2 v)
But won't I have to traverse again for cancel/done events? I already got that /order/info (pun intended), should not lose it.



In sequence and single threaded.



It is not clear what is the correct ordering between dispatching `CancelEv/DoneEv` and removing these orders from the state. Considering we are issuing these events from a single thread, it does not seem to matter much. If we dispatch either a `CancelEv` or a `DoneEv` the strategy is not allowed to try to cancel the same ClientOID again (undefined behavior). However, if we flipped the order and removed the order from the state while the strategy was processing a `BookEv`, for example, then the strategy may request for the order to be cancelled again before the events are dispatched, but this would simply be innefective as this very same thread is dispatching all events. So, it seems to work either way.

*We will need to revise the correctness of this choice if we eventually have multiple threads processing these events.*

That's it! We have dispatched all the necessary events :-D

----------------- hmmm.... not sure
### UPDATE 2019-02-11

Calling /open-orders is an optimization as this will work just as well if we call /order/info for every order in the connector state and do exactly the same processing for all without ever calling /open-orders. We just have to do an extra check to see if the order is closed.

In other words, for each order in the connector state:

1. Find out what happened to the order (call to /order/info)
2. dispatch any still missing `FillsEv` events (update state)
3. IF CLOSED: - dispatch a final `CancelEv` or `DoneEv`
              - remove the order from the state



It seems we should start in this simpler model. THIS IS NOT SIMPLER AT ALL!!! Still have to do the checks at the bottom and commit the state after the fills. Its the same thing, just done less efficiently!!!



### Dispatching `BookEv` events

These events does not change the state of the connector. We will simply use a separate thread to continuously poll the exchange and fire these events.

### Dispatching `PlaceEv` events

It seems natural that the Executor itself should issue `PlaceEv` events immediately after it received a corresponding OrderID. Unfortunately, doing that is is a little tricky. The problem is that the executor and producer work on separate execution threads and the framework guarantees that a `PlaceEv` will happen before any corresponding `FillsEv` or other event. 

Updates to the state are done using the STM monad, but firing of events is not included in this critical section. The executor has two choices:

1) first update the connector state and then (once outside the critical section) fire the corresponding `PlaceEv`;or,
2) first fire the event then (once outside the event network) update the connector state

Choice (2) means the strategy runs before the connector state is updated, which works but seems unsafe as we permanently lose the ClientOID/OrderID association if something goes haywire before the state update. It guarantees that by the time the producer sees the new order, the event has already been issued. 

Choice (1) has a synchronization problem. There is a small time interval between the connector state update and the firing of the event. In this mean time, it would be possible for a `FillsEv` to be produced by the producer working on a separate thread, given that the state has already been updated. 

So to use choice (1), either:

a) we have to synchronize producer/executor and make the producer wait for the executor to finish producing the `PlaceEv` before issuing any `FillsEv` events, which opens a whole new can of worms about deadlocks, as both producer and executor are now calling the same event network and both waiting on this event signaling. Or, 

b) we move the whole `PlaceEv` logic into the producer thread and let the producer deal with it. The executor must mark a new ClientOID/OrderID pair as "PlaceEv not issued" to let the producer know to issue the event before any others.

**The logic on the producer side will already be significantly more complicated than on the executor side. So, despite the risk of running the strategy before updating the connector's state. For now, we will pick choice (2) and fire the event from within the executor. We can revisit this decision later.**


### Dispatching `FillsEv`, `CancelEv` and `DoneEv` events

This is the core of the connector. These events are obtained from calls to /open-orders.

The logic is basically constantly polling /open-orders as follows:

A) the producer disregards any orders that are not part of the state.
B) for orders that are part of the state there are two possibilities:
    1. They show up in /open-orders
    2. They do not show up in /open-orders

If they show in /open-orders, they are still open. We just need to check if they have executed more. We do this by comparing their current volume with their "state volume" and creating corresponding fills, if necessary.

If an orde is part of the connector state, but does NOT show up in the call to /open-orders, then it must be closed.
It either executed completely or was cancelled. We need to:

1. Find out what happened to the order (call to /order/info)
2. dispatch any still missing `FillsEv` events
3. dispatch a final `CancelEv` or `DoneEv`
4. remove the order from the state

It is not clear what the correct order between (2) and (3) is. Considering we are issuing these events from a single thread, it does not seem to matter much. If we dispatch either a `CancelEv` or a `DoneEv` the strategy is not allowed to try to cancel the same ClientOID again (undefined behavior). However, if we flipped the order and removed the order from the state while the strategy was processing a `BookEv`, for example, then the strategy may request for the order to be cancelled again, but this would simply be innefective as this very same thread is dispatching all events. So, it seems to work either way.

*We will need to revise the correctness of this choice if we eventually have multiple threads processing these events.*

That's it! We have dispatched all the necessary events :-D

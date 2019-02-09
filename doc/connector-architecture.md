### Dispatching `PlaceEv` events

It seems natural that the Executor itself should issue `PlaceEv` events immediately after it received a corresponding OrderID. Unfortunately, doing that is is a little tricky. The problem is that the executor and producer work on 2 separate threads and the framework guarantees that a `PlaceEv` will happen before any corresponding `FillsEv` or other event. 

Updates to the state are done using the STM monad, but firing of events is not included in this critical section. The executor has two choices:

1) first update the connector state and then (once outside the critical section) fire the corresponding `PlaceEv`;or,
2) first fire the event then (once outside the event network) update the connector state

Choice (2) means the strategy runs before the connector state is updated, which works but seems unsafe as we permanently lose the ClientOID/OrderID association if something goes haywire before the state update. It guarantee that by the time the producer sees the new order, the event has already been issued. 

Choice (1) has a synchronization problem. There is a small time interval between the connector state update and the firing of the event. In this mean time, it would be possible for a `FillsEv` to be produced by the producer working on a separate thread, given that the state has already been updated. 

So to use choice (1), either:

a) we have to synchronize producer/executor and make the producer wait for the executor to finish producing the `PlaceEv` before issuing any `FillsEv` events, which opens a whole new can of worms about deadlocks, as both producer and executor are now calling the same event network and both waiting on this event signaling. Or, 

b) we move the whole `PlaceEv` logic into the producer thread and let the producer deal with it. The executor must mark a new ClientOID/OrderID pair as "PlaceEv not issued" to let the producer know to issue the event before any others.

**The logic on the producer side will already be significantly more complicated than on the executor side. So, despite the risk of running the strategy before updating the connector's state. For now, we will pick choice (2) and fire the event from within the executor. We can revisit this decision later.**


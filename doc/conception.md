
# Raft protocol implementation design

Design is oriented for beeing tested properly. This lead to disable time, be able to control message ordering and delays.

This implementation separate the execution engine from the behaviour implementation, this could provide using async/await implementation with message loop, 
but also tasks execution engine.


# Events

Timers are handled externally, this permit to make some edge and limit cases, to better tests the implementation.


# Communication sub systems

All Nodes are referenced with IDS for communication. These ids are the reference in all communications.

Communication hub -> define the communication means and hosts access (naming).


## Raft Machine

Handle all the state and handle transition changing procedures

### Raft Machine State

Handle all the state's specific behaviour and divise the implementation into localized implementation. The machine state contains a reference to the state. 


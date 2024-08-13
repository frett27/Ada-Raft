Ada Raft
---------

First Toy Implementation of the raft protocol, 
Only the first section is currently in progress.

Disclamer :

This project is NOT Working yet, and not fully qualified.
This project is not currently usable in production, 
implementation is in progress.


Messages are currently not sent using network, but with a message serialization on each Epoch (permitting to test more efficiently).
A first architecture / implementation is done, may be changed in the future.

Some documentation is available here, and will be improve with implementation: [doc](doc)

1st goals :

Be able to use it on server nodes for IOT purposes

- [X] Basic Raft Algorithm
- [X] Leader election
- [X] Log replication
- [ ] Commit broadcast
- [ ] Log compaction
- [ ] Testing - Formal Validation


Further devs :

- [ ] Membership changes

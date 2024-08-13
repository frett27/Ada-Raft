Ada Raft
---------

First Toy Implementation of the raft protocol, 
Only the first section is currently in progress.

This project is not usable in production, and implementation is in progress.

Messages are currently not sent using network, but with a message serialization on each Epoch (permitting to test more efficiently).
A first architecture / implementation is done, may be changed in the future.

Some documentation is available here: [doc](doc)


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

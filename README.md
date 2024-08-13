Ada Raft
---------

First Toy Implementation of the raft protocol, 
Only the first section is currently in progress.

Messages are not sent using network, but with a message serialization on each Epoch (permitting to test more efficiently)

1st goals :

Be able to use it on server nodes

- [X] Basic Raft Algorithm
- [X] Leader election
- [X] Log replication
- [ ] Commit broadcast
- [ ] Log compaction
- [ ] Testing - Formal Validation


Further devs :

- [ ] Membership changes

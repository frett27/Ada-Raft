Ada Raft
---------

First Toy Implementation of the raft protocol, 
Only the first section is currently in progress.

**DISCLAMER** :

This project is NOT Working yet, and not fully qualified.
This project is not currently usable in production, 
implementation is in progress. There are still a lot of elements to complete.

Messages are currently not sent using network, but with a message serialization on each Epoch (permitting to test more efficiently).

A first architecture / implementation is done, may be changed in the future.

Some documentation is available here, and will be improve with implementation: [doc](doc)

1st goals of the implementation :

Be able to use it on several nodes

- [X] Basic Raft Algorithm
- [X] Leader election
- [x] Log replication
- [ ] Test tck - validation for nominal and specific use cases
- [ ] Client Handling / Commit broadcast
- [ ] Log compaction / Snapshots
- [ ] Testing - (Formal Validation ?)


Further devs :

- [ ] Membership changes



seeing if prevote extension, and strategies for log transmission

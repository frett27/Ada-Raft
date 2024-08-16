Ada Raft
---------

First Implementation of the raft protocol, 
Only the first section of the raft protocol, is currently implemented.

The implementation takes currently a bounded log, and entries are sent with a maximum of 10


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
  - [ ] Saving the messages and tests to be able to replay and be consistent in the test
- [ ] Log Compaction (handling of persistence for the log result)
- [ ] Client Handling / Commit broadcast (handling of hi stressing the server)

- [ ] Testing - (Formal Validation)


Further devs :

- [ ] Membership changes (still in reflexion, as the elements above can be error prone for restoration)

seeing if prevote extension, and strategies for log transmission 


ChangeLog
==========

15/08/2024 : Work on RaftSystem Testing (improve the readability of the test)

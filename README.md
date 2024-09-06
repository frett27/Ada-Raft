Ada Raft
---------

First Implementation of the raft protocol, 
Only the first section of the raft protocol specification, is currently implemented.

The implementation currently takes a bounded log, and entries are sent with a maximum of 10


**DISCLAMER** :

This project is NOT currently production proof for a correct use in production, 
Tests are available, and for explorers, you can takes it, test it, and if suitable for your usage, go for it ! :)

Messages are currently not sent using network, it use message serialization on each Epoch (permitting to test more efficiently, the several implementation cases).

A first architecture / implementation is prototyped and done, this may be evolved in the future, depending on readability and further concerns.

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

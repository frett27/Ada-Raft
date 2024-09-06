Ada Raft
---------

Raft is a concensus protocol (as paxos, zab, ..) , concensus permit to have global decision for a sets of agents / nodes / computers, behaving like a unique computer for decisions, see [https://raft.github.io/](https://raft.github.io/) for more general informations.
Famous concensu's based products are : zookeeper, etcd
Lots of products now embed concensus libraries, for redondancy and recovery. (cassendra, kafka, .. to name somes) 

This repo host a basic implementation of the raft protocol using the Ada Langage, suitable for secure and high quality software. This repo has been an holiday toy to learn about the key details of Raft Implementation. 

Only the first section of the raft protocol specification, is currently implemented.

The implementation currently takes a bounded log, and entries are sent with a maximum of 10 (streams for sync, a fixed constant that can be changed at compilation time).


**DISCLAMER** :-) , as usual :

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

15/08/2024 : Work on RaftSystem Testing (improve the readability of the tests)

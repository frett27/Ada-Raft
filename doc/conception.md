
# Raft protocol implementation design

Design is oriented for the library beeing properly and easily tested. This lead to disable time, be able to control message ordering and delays.
=> Time is an issue, to be able to test all conditions

In this implementation, we separate the execution engine (handling messages and behaviours) from the behaviour implementation, this could provide using async/await implementation with message loop, 



# Events

Timers are handled externally, this permit to make some edge and limit cases, to better tests the implementation.


# Communication sub systems

All Nodes are referenced with IDS for communication. These ids are the reference in all communications.

Communication hub -> define the communication means and hosts access (naming).




## Raft Machine

Handle all the state and handle transition changing procedures



```mermaid
classDiagram

	class RaftMachine {
	
        State : aliased RaftServerStruct;

        MState_Leader    : aliased Raft_State_Machine_Leader;
        MState_Candidate : aliased Raft_State_Machine_Candidat;
        MState_Follower  : aliased Raft_State_Machine_Follower;

        Current_Machine_State : Raft_State_Machine_Wide_Access;
    }
    
    RaftMachine ..> Raft_State_Machine_Leader
    RaftMachine ..> Raft_State_Machine_Follower
    RaftMachine ..> Raft_State_Machine_Candidate
    
    class RaftServerStruct {
        Current_Raft_State : RaftStateEnum;

        -- id of the current server
        Current_Id : ServerID;

        Node_State : Raft_Node_State;

        -- volatile for all states
        Commit_Index : TransactionLogIndexPointer :=
         UNDEFINED_TRANSACTION_LOG_INDEX;
        Last_Applied : TransactionLogIndexPointer :=
         UNDEFINED_TRANSACTION_LOG_INDEX;

        -- leader specific implementation
        Leader_State : Raft_Leader_Additional_State;
    }
    
    RaftMachine --> RaftServerStruct
    RaftServerStruct --> Raft_Node_State
    class Raft_Node_State {
        -- persisted
        Current_Term    : Term;
        Voted_For       : ServerID := 0;
        Log             : TLog (TransactionLogIndex'First .. MAX_LOG);
        Log_Upper_Bound : TransactionLogIndexPointer;
    }
    
    class Raft_State_Machine {
    	 MState : Raft_Server_Struct_Access;

        -- Note : to refactor, theses pointers should be in the machine without
        -- extra informations given to the state (to limit complexity)
        Timer_Cancel    : Cancel_Timer;
        Timer_Start     : Start_Timer;
        Sending_Message : Message_Sending;
        
        Handle_Message_Machine_State(M: Message_type'Class)

    }
    
    Raft_State_Machine ..> RaftServerStruct
    
    Raft_State_Machine_Leader <|-- Raft_State_Machine
    Raft_State_Machine_Follower <|-- Raft_State_Machine
    Raft_State_Machine_Candidate <|-- Raft_State_Machine
    
    class Raft_State_Machine_Candidate {
        Server_Vote_Responses : Array_Of_ServerId_Booleans := (others => False);
        Server_Vote_Responses_Status : Array_Of_ServerId_Booleans := (others => False);
    }
```



### Raft Machine State

Handle all the state's specific behaviour and divise the implementation into localized implementation. The machine state contains a reference to the state. 


## Implementation review

- Message serialization, type serialization
- Simplicity of starting a new project
- Possible applications, illustrations






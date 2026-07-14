with GNAT.Sockets; use GNAT.Sockets;
with Ada.Calendar; use Ada.Calendar;
with Raft;       use Raft;

package Cluster_Config is

   Max_Nodes : constant := 16;
   Max_Host_Length : constant := 64;

   subtype Host_String is String (1 .. Max_Host_Length);

   type Node_Config is record
      Id   : ServerID_Type;
      Host : Host_String;
      Host_Length : Natural := 0;
      Port : Port_Type;
   end record;

   type Node_Config_Table is array (1 .. Max_Nodes) of Node_Config;

   type Raft_Settings is record
      Epoch_Interval            : Duration;
      Election_Timeout_Epochs   : Positive;
      Heartbeat_Interval_Epochs : Positive;
      Election_Jitter_Epochs    : Positive;
      Audit_Interval_Epochs     : Positive;
      Compact_Threshold         : Natural;
      Compact_Log_Retention     : Natural;
      Inter_Server_Timeout      : Duration;
   end record;

   function Default_Raft_Settings return Raft_Settings;

   type Cluster_Configuration is record
      Server_Count : ServerID_Type := 0;
      Nodes        : Node_Config_Table;
      Raft         : Raft_Settings;
   end record;

   Config_Error : exception;

   procedure Load
     (Path : String; Config : out Cluster_Configuration);

   function Node_Host (Node : Node_Config) return String;

   function Server_Hostname (SID : ServerID_Type) return String;

end Cluster_Config;

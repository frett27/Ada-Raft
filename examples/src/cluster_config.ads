with GNAT.Sockets; use GNAT.Sockets;
with Ada.Calendar; use Ada.Calendar;
with Raft;       use Raft;

package Cluster_Config is

   Max_Nodes : constant := 16;
   Max_Clients : constant := 8;
   Max_Host_Length : constant := 64;
   Max_Client_Name_Length : constant := 32;

   subtype Host_String is String (1 .. Max_Host_Length);

   type Node_Config is record
      Id   : ServerID_Type;
      Host : Host_String;
      Host_Length : Natural := 0;
      Port : Port_Type;
   end record;

   type Node_Config_Table is array (1 .. Max_Nodes) of Node_Config;

   type Client_Endpoint_Config is record
      Name        : String (1 .. Max_Client_Name_Length);
      Name_Length : Natural := 0;
      Host        : Host_String;
      Host_Length : Natural := 0;
      Port        : Port_Type;
   end record;

   type Client_Endpoint_Table is
     array (1 .. Max_Clients) of Client_Endpoint_Config;

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
      Client_Host  : Host_String;
      Client_Host_Length : Natural := 0;
      Client_Port  : Port_Type := 9200;
      Client_Name  : String (1 .. Max_Client_Name_Length);
      Client_Name_Length : Natural := 0;
      Client_Count : Natural := 0;
      Clients      : Client_Endpoint_Table;
      Raft         : Raft_Settings;
   end record;

   Config_Error : exception;

   procedure Load
     (Path : String; Config : out Cluster_Configuration);

   function Node_Host (Node : Node_Config) return String;

   function Client_Host_Image (Config : Cluster_Configuration) return String;

   function Client_Sender_Name (Config : Cluster_Configuration) return String;

   function Client_Endpoint_Name
     (Client : Client_Endpoint_Config) return String;

   function Client_Endpoint_Host
     (Client : Client_Endpoint_Config) return String;

   function Configured_Client_Count
     (Config : Cluster_Configuration) return Natural;

   function Client_Endpoint
     (Config : Cluster_Configuration; Index : Positive)
      return Client_Endpoint_Config;

   function Server_Hostname (SID : ServerID_Type) return String;

end Cluster_Config;

with Raft; use Raft;

package Cluster_Health is

   --  Thresholds used by raft_monitor (see stress/monitoring.md).
   Overload_Pending_Inbound_Min : constant Natural := 32;
   Overload_Client_Rejected_Min : constant Natural := 8;
   Wedged_Response_Lag_Min      : constant Natural := 8;
   Epoch_Spread_Warning_Min     : constant Natural := 80;

   Max_Field_Length : constant := 64;
   Max_Role_Length  : constant := 16;
   Max_Text_Length  : constant := 4096;

   type Node_Status is record
      Node_Id               : ServerID_Type := 0;
      Reachable             : Boolean := False;
      Role                  : String (1 .. Max_Role_Length) := (others => ' ');
      Role_Length           : Natural := 0;
      Epoch                 : Natural := 0;
      Term                  : Natural := 0;
      Pending_Inbound       : Natural := 0;
      Client_Sends          : Natural := 0;
      Client_Responses      : Natural := 0;
      Client_In_Flight      : Natural := 0;
      Client_Rejected       : Natural := 0;
      Client_Slots_Max      : Natural := 0;
      Inbound_Dropped       : Natural := 0;
      App_Sum               : Integer := 0;
      Raw                   : String (1 .. Max_Text_Length);
      Raw_Length            : Natural := 0;
   end record;

   type Node_Status_Table is
     array (ServerID_Type range <>) of Node_Status;

   procedure Parse_Status_Report
     (Text : String; Status : in out Node_Status);

   function Role_Image (Status : Node_Status) return String;

   function Is_Wedged (Status : Node_Status) return Boolean;

   function Is_Overloaded (Status : Node_Status) return Boolean;

   function Cluster_Verdict
     (Statuses : Node_Status_Table) return String;

   function Count_Leaders (Statuses : Node_Status_Table) return Natural;

   --  Plain-English hints for raft_monitor (not documentation).
   function Node_Tag_Reason (Status : Node_Status) return String;

   function Cluster_Advice (Statuses : Node_Status_Table) return String;

   procedure Print_Advice_Block (Text : String);

end Cluster_Health;

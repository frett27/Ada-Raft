with Ada.Strings;       use Ada.Strings;
with Ada.Strings.Fixed; use Ada.Strings.Fixed;

with TOML;         use TOML;
with TOML.File_IO; use TOML.File_IO;
with Example_Config; use Example_Config;

package body Cluster_Config is

   function Default_Raft_Settings return Raft_Settings is
   begin
      return
        (Epoch_Interval            => Example_Config.Epoch_Interval,
         Election_Timeout_Epochs   => Example_Config.Election_Timeout_Epochs,
         Heartbeat_Interval_Epochs =>
           Example_Config.Heartbeat_Interval_Epochs,
         Election_Jitter_Epochs    => Example_Config.Election_Jitter_Epochs,
         Audit_Interval_Epochs     => Example_Config.Audit_Interval_Epochs,
         Compact_Threshold         => 100,
         Compact_Log_Retention     => 0,
         Inter_Server_Timeout      =>
           Example_Config.Default_Inter_Server_Timeout);
   end Default_Raft_Settings;

   procedure Validate_Raft_Timing (Settings : Raft_Settings) is
      Min_Election : constant Positive :=
        Example_Config.Election_Heartbeat_Ratio
        * Settings.Heartbeat_Interval_Epochs;
   begin
      if Settings.Election_Timeout_Epochs < Min_Election then
         raise Config_Error
           with "raft.election_timeout_epochs must be at least "
                & Positive'Image (Example_Config.Election_Heartbeat_Ratio)
                & " * raft.heartbeat_interval_epochs (min "
                & Positive'Image (Min_Election)
                & ", got "
                & Positive'Image (Settings.Election_Timeout_Epochs)
                & ")";
      end if;
   end Validate_Raft_Timing;

   procedure Set_Host
     (Into : out Host_String; Len : out Natural; Value : String)
   is
   begin
      if Value'Length > Into'Length then
         raise Config_Error with "host string too long: " & Value;
      end if;
      if Value'Length = 0 then
         raise Config_Error with "empty host string";
      end if;
      Into (Into'First .. Into'First + Value'Length - 1) := Value;
      Len := Value'Length;
   end Set_Host;

   function Require_Table
     (Value : TOML_Value; Name : String) return TOML_Value
   is
   begin
      if Value.Is_Null or else Value.Kind /= TOML_Table then
         raise Config_Error with Name & " must be a table";
      end if;
      return Value;
   end Require_Table;

   function Require_Key
     (Table : TOML_Value; Key, Context : String) return TOML_Value
   is
   begin
      if not Table.Has (Key) then
         raise Config_Error with "missing " & Context & "." & Key;
      end if;
      return Table.Get (Key);
   end Require_Key;

   function As_Config_Integer (Value : TOML_Value; Context : String)
     return Integer
   is
   begin
      if Value.Kind /= TOML_Integer then
         raise Config_Error with Context & ": expected integer";
      end if;
      return Integer (Value.As_Integer);
   end As_Config_Integer;

   function As_Config_String (Value : TOML_Value; Context : String)
     return String
   is
   begin
      if Value.Kind /= TOML_String then
         raise Config_Error with Context & ": expected string";
      end if;
      return Value.As_String;
   end As_Config_String;

   procedure Apply_Optional_Positive
     (Table : TOML_Value; Key : String; Value : in out Positive)
   is
      Item : constant TOML_Value := Table.Get_Or_Null (Key);
   begin
      if Item.Is_Present then
         declare
            N : constant Integer :=
              As_Config_Integer (Item, "raft." & Key);
         begin
            if N < 1 then
               raise Config_Error
                 with "raft." & Key & " must be >= 1";
            end if;
            Value := Positive (N);
         end;
      end if;
   end Apply_Optional_Positive;

   procedure Apply_Optional_Natural
     (Table : TOML_Value; Key : String; Value : in out Natural)
   is
      Item : constant TOML_Value := Table.Get_Or_Null (Key);
   begin
      if Item.Is_Present then
         declare
            N : constant Integer :=
              As_Config_Integer (Item, "raft." & Key);
         begin
            if N < 0 then
               raise Config_Error
                 with "raft." & Key & " must be >= 0";
            end if;
            Value := Natural (N);
         end;
      end if;
   end Apply_Optional_Natural;

   procedure Apply_Optional_Epoch_Interval
     (Table : TOML_Value; Key : String; Value : in out Duration)
   is
      Item : constant TOML_Value := Table.Get_Or_Null (Key);
   begin
      if Item.Is_Present then
         declare
            Ms : constant Integer :=
              As_Config_Integer (Item, "raft." & Key);
         begin
            if Ms < 1 then
               raise Config_Error
                 with "raft." & Key & " must be >= 1";
            end if;
            Value := Duration (Ms) / 1000.0;
         end;
      end if;
   end Apply_Optional_Epoch_Interval;

   procedure Load_Raft_Settings
     (Root : TOML_Value; Settings : out Raft_Settings)
   is
      Raft : TOML_Value := Root.Get_Or_Null ("raft");
      Has_Inter_Server_Timeout : Boolean := False;
   begin
      Settings := Default_Raft_Settings;

      if not Raft.Is_Present then
         Validate_Raft_Timing (Settings);
         return;
      end if;

      Raft := Require_Table (Raft, "raft");

      if Raft.Has ("inter_server_timeout_ms") then
         Has_Inter_Server_Timeout := True;
      end if;

      Apply_Optional_Epoch_Interval
        (Raft, "epoch_interval_ms", Settings.Epoch_Interval);
      Apply_Optional_Positive
        (Raft, "election_timeout_epochs", Settings.Election_Timeout_Epochs);
      Apply_Optional_Positive
        (Raft, "heartbeat_interval_epochs", Settings.Heartbeat_Interval_Epochs);
      Apply_Optional_Positive
        (Raft, "election_jitter_epochs", Settings.Election_Jitter_Epochs);
      Apply_Optional_Positive
        (Raft, "audit_interval_epochs", Settings.Audit_Interval_Epochs);
      Apply_Optional_Natural
        (Raft, "compact_threshold", Settings.Compact_Threshold);
      Apply_Optional_Natural
        (Raft, "compact_log_retention", Settings.Compact_Log_Retention);
      Apply_Optional_Epoch_Interval
        (Raft, "inter_server_timeout_ms", Settings.Inter_Server_Timeout);

      if not Has_Inter_Server_Timeout then
         Settings.Inter_Server_Timeout :=
           Settings.Epoch_Interval
           * Duration (Settings.Heartbeat_Interval_Epochs);
      end if;

      if Settings.Compact_Threshold = 0 then
         raise Config_Error
           with "raft.compact_threshold must be >= 1 when set";
      end if;

      Validate_Raft_Timing (Settings);
   end Load_Raft_Settings;

   procedure Load_Node (Node_Value : TOML_Value; Node : out Node_Config) is
      Table : constant TOML_Value :=
        Require_Table (Node_Value, "nodes entry");
   begin
      Node.Id :=
        ServerID_Type
          (As_Config_Integer
             (Require_Key (Table, "id", "nodes"), "nodes.id"));
      Node.Port :=
        Port_Type
          (As_Config_Integer
             (Require_Key (Table, "port", "nodes"), "nodes.port"));
      Node.Host_Length := 0;
      Set_Host
        (Node.Host,
         Node.Host_Length,
         As_Config_String
           (Require_Key (Table, "host", "nodes"), "nodes.host"));
   end Load_Node;

   procedure Validate_Node_Coverage (Config : Cluster_Configuration) is
   begin
      for SID in ServerID_Type range 1 .. Config.Server_Count loop
         declare
            Found : Boolean := False;
         begin
            for I in Config.Nodes'Range loop
               exit when Config.Nodes (I).Id = 0;
               if Config.Nodes (I).Id = SID then
                  Found := True;
                  exit;
               end if;
            end loop;
            if not Found then
               raise Config_Error
                 with "missing node entry for server id "
                      & Trim (ServerID_Type'Image (SID), Left);
            end if;
         end;
      end loop;
   end Validate_Node_Coverage;

   procedure Load
     (Path : String; Config : out Cluster_Configuration)
   is
      Result  : constant Read_Result := Load_File (Path);
      Root    : TOML_Value;
      Cluster : TOML_Value;
      Client  : TOML_Value;
      Nodes   : TOML_Value;
   begin
      Config := (others => <>);
      Config.Raft := Default_Raft_Settings;

      if not Result.Success then
         raise Config_Error with Format_Error (Result);
      end if;

      Root := Require_Table (Result.Value, "root");

      Cluster :=
        Require_Table
          (Require_Key (Root, "cluster", "config"), "cluster");
      Config.Server_Count :=
        ServerID_Type
          (As_Config_Integer
             (Require_Key (Cluster, "servers", "cluster"),
              "cluster.servers"));

      if Config.Server_Count = 0 then
         raise Config_Error with "cluster.servers not set in " & Path;
      end if;

      Client := Root.Get_Or_Null ("client");
      if Client.Is_Present then
         Client := Require_Table (Client, "client");
         if Client.Has ("host") then
            Set_Host
              (Config.Client_Host,
               Config.Client_Host_Length,
               As_Config_String (Client.Get ("host"), "client.host"));
         end if;
         if Client.Has ("port") then
            Config.Client_Port :=
              Port_Type
                (As_Config_Integer (Client.Get ("port"), "client.port"));
         end if;
      end if;

      if Config.Client_Host_Length = 0 then
         Set_Host (Config.Client_Host, Config.Client_Host_Length, "127.0.0.1");
      end if;

      Nodes := Require_Key (Root, "nodes", "config");
      if Nodes.Kind /= TOML_Array then
         raise Config_Error with "nodes must be an array";
      end if;

      if Nodes.Length > Max_Nodes then
         raise Config_Error with "too many nodes in configuration";
      end if;

      for I in 1 .. Nodes.Length loop
         Load_Node (Nodes.Item (I), Config.Nodes (I));
      end loop;

      Validate_Node_Coverage (Config);
      Load_Raft_Settings (Root, Config.Raft);
   end Load;

   function Node_Host (Node : Node_Config) return String is
   begin
      if Node.Host_Length = 0 then
         raise Config_Error
           with "node host missing for id "
                & Trim (ServerID_Type'Image (Node.Id), Left);
      end if;
      return Node.Host
        (Node.Host'First .. Node.Host'First + Node.Host_Length - 1);
   end Node_Host;

   function Client_Host_Image (Config : Cluster_Configuration) return String is
   begin
      return Config.Client_Host (1 .. Config.Client_Host_Length);
   end Client_Host_Image;

   function Server_Hostname (SID : ServerID_Type) return String is
   begin
      return Trim (ServerID_Type'Image (SID), Left);
   end Server_Hostname;

end Cluster_Config;

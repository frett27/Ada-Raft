with Ada.Strings;       use Ada.Strings;
with Ada.Strings.Fixed; use Ada.Strings.Fixed;

with TOML;         use TOML;
with TOML.File_IO; use TOML.File_IO;

package body Cluster_Config is

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

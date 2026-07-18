with GNAT.Sockets; use GNAT.Sockets;

--  Read-only audit/monitor TCP listener. Serves one framed status report per
--  connection. The status text is provided by a callback registered by the
--  engine (avoids an elaboration cycle with Status_Report).
package Network_Node.Audit is

   type Status_Provider_Access is access function return String;

   procedure Set_Status_Provider (Provider : Status_Provider_Access);
   procedure Start (Port_No : Port_Type);
   procedure Request_Stop;

end Network_Node.Audit;

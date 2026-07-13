package body Raft.State_Machine is

   function Image (State : Application_State_Access) return String is
   begin
      if State = null then
         return "<no application state>";
      end if;

      return Image (State.all);
   end Image;

end Raft.State_Machine;

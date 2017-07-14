with Ada.Text_IO; use Ada.Text_IO;
with Components;

package body Hub is

   Living_Room : Components.Access_Hub_Type;

   procedure Register_Listener (Channel : Stream_Access) is
      Message : Access_Message_Type;
   begin
      Access_Message_Type'Output (Channel, Message);

      Components.Add_Device (Living_Room,
                            Message.)
   end Register_Listener;

   ----------------
   -- Initialize --
   ----------------

   procedure Initialize is
      Living_Room : Components.Access_Hub_Type;
   begin

      -- Create new Hub
      Living_Room := Components.Create ("Living_Room");
      Components.Comms.Listen (Register_Listener);
   end Initialize;

   -------------
   -- Execute --
   -------------

   procedure Execute is
   begin
      Components.Comms.Listen ();
   end Execute;

end Hub;

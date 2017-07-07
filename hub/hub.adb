with Ada.Text_IO; use Ada.Text_IO;
with Components;

package body Hub is

   Living_Room : Components.Access_Hub_Type;

   procedure Register_Listener (Device : Components.Access_Device_Type) is
   begin
      Components.Add_Device (Living_Room, Device);
      Put_Line ("Added New Device");
   end Register_Listener;

   ----------------
   -- Initialize --
   ----------------

   procedure Initialize is
      Living_Room : Components.Access_Hub_Type;
   begin

      -- Create new Hub
      Living_Room := Components.Create ("Living_Room");

      -- Attach a listener to register new Devices
      Components.Attach_Register_Listener
        (Living_Room, Register_Listener'Access);

   end Initialize;

   -------------
   -- Execute --
   -------------

   procedure Execute is
   begin
      null;
   end Execute;

end Hub;

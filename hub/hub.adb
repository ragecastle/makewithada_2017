with Ada.Text_IO; use Ada.Text_IO;
with Components.Comms; use Components;
with GNAT.Sockets; use GNAT.Sockets;

package body Hub is

  Living_Room : Components.Access_Hub_Type;

  function Register_Listener (Channel : Stream_Access) return boolean is
    Message : Comms.Message_Type := Comms.Message_Type'Input (Channel);
  begin

    Comms.Message_Type'Output (Channel, Message);
    Put_Line (Message.Kind'Img);
    Put_Line (" "  & Message.Identifier'Img);
    Put_Line ("  " & Message.Message);
    New_Line;

    return False;

  end Register_Listener;

  ----------------
  -- Initialize --
  ----------------

  procedure Initialize is
    Living_Room : Components.Access_Hub_Type;
  begin

    -- Create new Hub
    Living_Room := Components.Create ("Living_Room");
    Comms.Listen (Register_Listener'Access);
  end Initialize;

  -------------
  -- Execute --
  -------------

  procedure Execute is
  begin
    null;
    --        Components.Comms.Listen ();
  end Execute;

end Hub;

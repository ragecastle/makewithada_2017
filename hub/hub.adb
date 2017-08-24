with Ada.Text_IO; use Ada.Text_IO;
with Components.Comms; use Components;
with GNAT.Sockets; use GNAT.Sockets;

package body Hub is

  Living_Room : Components.Access_Hub_Type;
  New_Device : Access_Device_Type;
  New_State_Group : Access_State_Group_Type;
  New_String : Device_String;

  function Register_Listener (Channel : Stream_Access) return boolean is
    Message : Comms.Message_Type := Comms.Message_Type'Input (Channel);
  begin

    Comms.Message_Type'Output (Channel, Message);
    Put_Line (Message.Kind'Img);
    Put_Line (" "  & Message.Identifier'Img);
    Put_Line ("  " & Message.Message);
    New_Line;

    case Message.Kind is
      when Comms.Register =>

        case Message.Identifier is
          when Comms.Device_Name =>
            New_Device := Create (Message.Message);
          when Comms.State_Group_Name =>
            New_State_Group := Create (Message.Message);
            Add_State_Group (New_Device, New_State_Group);
          when Comms.State_Name =>
            Add_State (New_State_Group,
                       Message.Message);
        end case;

      when Comms.Update =>

        case Message.Identifier is
          when Comms.Device_Name =>
            New_Device := Get_Device (Living_Room,
                                      Message.Message);
          when Comms.State_Group_Name =>
            New_String := Message.Message;
          when Comms.State_Name =>
            Set_Current_State (Device => New_Device,
                               Group  => New_String,
                               State  => Message.Message);
        end case;

      when Comms.Command =>
        null;
      when Comms.EOM =>
        return True;
    end case;

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

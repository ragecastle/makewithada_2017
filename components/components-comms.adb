with Ada.Text_IO; use Ada.Text_IO;
with Ada.Exceptions; use Ada.Exceptions;
with Ada.Strings.Fixed; use Ada.Strings;

package body Components.Comms is

   -- TODO: Create a message protocol with a header/message/footer or a JSON
   --       for like message integrity and stuff

   ------------
   -- Listen --
   ------------

   procedure Listen (Handler : in Message_Handler_Type;
                     Port    : in Port_Type := 5876) is
      Address    : Sock_Addr_Type;
      Server     : Socket_Type;
      Socket_Set : Socket_Set_Type;
      Selector   : Selector_Type;
   begin

      Address.Addr := Addresses (Get_Host_By_Name (Host_Name), 1);
      Address.Port := Port;

      Create_Socket (Server);

      Set_Socket_Option
        (Server,
         Socket_Level,
         (Reuse_Address, True));

      Bind_Socket (Server, Address);

      Listen_Socket (Server);

      Set (Socket_Set, Server);

      Create_Selector (Selector);

      loop
         declare
            R_Socket_Set : Socket_Set_Type;
            W_Socket_Set : Socket_Set_Type;
            Status       : Selector_Status;
            use type Selector_Status;
         begin
            Copy (Socket_Set, R_Socket_Set);
            Check_Selector (Selector,
                            R_Socket_Set,
                            W_Socket_Set,
                            Status);

            if Status = Completed then
               declare
                  Channel : Stream_Access;
                  Socket  : Socket_Type;
                  use type Socket_Type;
               begin
                  Get (R_Socket_Set, Socket);
                  if Socket = Server then

                     Accept_Socket (Server,
                                    Socket,
                                    Address);

                     Channel := Stream (Socket);

                     delay 0.5;

                     if Handler (Channel) then
                        Close_Socket (Server);
                     end if;

                     --                       declare
                     --                          Message : String := String'Input (Channel);
                     --                       begin
                     --                          Put_Line (Message);
                     --                       end;

                     -- TODO: Else if log stuff
                  end if;
               end;
               -- TODO: Else if log stuff
            end if;
         end;
      end loop;

   exception when E : others =>
         Ada.Text_IO.Put_Line
           (Exception_Name (E) & ": " & Exception_Message (E));
         Close_Socket (Server);
         Close_Selector (Selector);
   end Listen;

   ----------
   -- Talk --
   ----------

   procedure Talk (Message : in Message_Type;
                   Port    : in Port_Type := 5876) is
      Address  : Sock_Addr_Type;
      Socket   : Socket_Type;
      Channel  : Stream_Access;

   begin
      Address.Addr := Addresses (Get_Host_By_Name (Host_Name), 1);
      Address.Port := Port;
      Create_Socket (Socket);

      Set_Socket_Option
        (Socket,
         Socket_Level,
         (Reuse_Address, True));

      delay 0.5;

      Connect_Socket (Socket, Address);

      Channel := Stream (Socket);

      Message_Type'Output (Channel, Message);

--        Message_Handler (Message.Kind) (Channel, Message);
      --
      --        String'Output (Channel, Message);

      delay 0.5;

      Close_Socket (Socket);
   end Talk;

   --------------
   -- Register --
   --------------

   procedure Register (Device : in Access_Device_Type) is
      Message : Message_Type;
   begin
      Message.Kind := Comms.Register;

      Message.Identifier := Device_Name;
      Message.Message := Device.Name;
      Talk (Message);
      -- TODO: while State_Group.Next loop
      Message.Identifier := State_Group_Name;
      Message.Message := Device.State_Group_List.Name;
      Talk (Message);
      --TODO: while State.Next loop
      Message.Identifier := State_Name;
      Message.Message := Device.State_Group_List.State_List (1);
      Talk (Message);
      -- TODO: end loop;
      -- TODO: end loop;
      Message.Kind := EOM;
      Talk (Message);

      -- TODO: Fix this to send all needed data
   end Register;

   ------------
   -- Update --
   ------------

   procedure Update (Device      : in Access_Device_Type;
                     State_Group : in String) is
      Message : Message_Type;
      State_Group_Name : Device_String;
   begin
      Ada.Strings.Fixed.Move (Source  => State_Group,
                              Target  => State_Group_Name,
                              Drop    => Right);

      Message.Kind := Comms.Register;

      Message.Identifier := Device_Name;
      Message.Message := Device.Name;
      Talk (Message);

      Message.Identifier := Comms.State_Group_Name;
      Message.Message := State_Group_Name;
      Talk (Message);

      Message.Identifier := State_Name;
      Message.Message := Get_Current_State (Device, State_Group_Name);
      Talk (Message);
      -- TODO: Fix this to send all needed data
   end Update;

   -------------
   -- Control --
   -------------

--     procedure Control (Device       : in Access_Device_Type;
--                        Action_Group : in Device_String;
--                        Action       : in Device_String) is
--        Message : Message_Type;
--     begin
--        Message.Device := Device;
--        Talk (Message);
--        -- TODO: Fix this to send all needed data
--     end Control;

end Components.Comms;

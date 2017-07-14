with Ada.Text_IO; use Ada.Text_IO;
with Ada.Exceptions; use Ada.Exceptions;

package body Components.Comms is

   -- TODO: Create a message protocol with a header/message/footer or a JSON
   --       for like message integrity and stuff

   -- TODO: Find a better way to do all of this, for now the POC is good enough

   procedure Hub_Handler (Channel : in Stream_Access;
                          Message : in Message_Type);
   procedure Device_Handler (Channel : in Stream_Access;
                             Message : in Message_Type);
   procedure State_Group_Handler (Channel : in Stream_Access;
                                  Message : in Message_Type);
   procedure Action_Group_Handler (Channel : in Stream_Access;
                                   Message : in Message_Type);
   procedure String_Handler (Channel : in Stream_Access;
                             Message : in Message_Type);

   type Message_Handler_Array is
     array (Message_Kind_Type) of Message_Talk_Handler_Type;

   Message_Handler : Message_Handler_Array :=
     (Hub_Kind          => Hub_Handler'Access,
      Device_Kind       => Device_Handler'Access,
      State_Group_Kind  => State_Group_Handler'Access,
      Action_Group_Kind => Action_Group_Handler'Access,
      String            => String_Handler'Access);

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

                     Handler (Channel);
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

      Message_Handler (Message.Kind) (Channel, Message);
      --
      --        String'Output (Channel, Message);

      delay 0.5;

      Close_Socket (Socket);
   end Talk;

   --------------
   -- Register --
   --------------

   procedure Register (Device : in Access_Device_Type) is
      Message : Message_Type (Device_Kind);
   begin
      Message.Device := Device;
      Talk (Message);
      -- TODO: Fix this to send all needed data
   end Register;

   ------------
   -- Update --
   ------------

   procedure Update (Device           : in Access_Device_Type;
                     State_Group_Type : in Device_String) is
      Message : Message_Type (Device_Kind);
   begin
      Message.Device := Device;
      Talk (Message);
      -- TODO: Fix this to send all needed data
   end Update;

   -------------
   -- Control --
   -------------

   procedure Control (Device       : in Access_Device_Type;
                      Action_Group : in Device_String;
                      Action       : in Device_String) is
      Message : Message_Type (Device_Kind);
   begin
      Message.Device := Device;
      Talk (Message);
      -- TODO: Fix this to send all needed data
   end Control;

   procedure Hub_Handler (Channel : in Stream_Access;
                          Message : in Message_Type) is
   begin
      Access_Hub_Type'Output (Channel, Message.Hub);
   end Hub_Handler;

   procedure Device_Handler (Channel : in Stream_Access;
                          Message : in Message_Type) is
   begin
      Access_Device_Type'Output (Channel, Message.Device);
   end Device_Handler;

   procedure State_Group_Handler (Channel : in Stream_Access;
                          Message : in Message_Type) is
   begin
      Access_State_Group_Type'Output (Channel, Message.State_Group);
   end State_Group_Handler;

   procedure Action_Group_Handler (Channel : in Stream_Access;
                          Message : in Message_Type) is
   begin
      Access_Action_Group_Type'Output (Channel, Message.Action_Group);
   end Action_Group_Handler;

   procedure String_Handler (Channel : in Stream_Access;
                             Message : in Message_Type) is
   begin
      Device_String'Output (Channel, Message.Message);
   end String_Handler;

end Components.Comms;

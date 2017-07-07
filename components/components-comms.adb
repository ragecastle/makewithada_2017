package body Components.Comms is

   -- TODO: Create a message protocol with a header/message/footer or a JSON
   --       for like message integrity and stuff

   -- TODO: Find a better way to do all of this, for now the POC is good enough

   ------------
   -- Listen --
   ------------

   procedure Listen (Port : Port_Type := 5876) is
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

                     declare
                        Message : String := String'Input (Channel);
                     begin
                        Put_Line (Message);
                     end;

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

   procedure Talk (Message : String;
                   Port    : Port_Type := 5876) is
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

      String'Output (Channel, Message);

      delay 0.5;

      Close_Socket (Socket);
   end Talk;

   --------------
   -- Register --
   --------------

   procedure Register (Device : in Access_Device_Type) is
   begin
      Talk (Register);
      -- TODO: Find a better code to ensure the right message is read
      Talk (Message => Device.Name);
      -- TODO: Send the rest of the information
      -- TODO: Find a better way to do this
   end Register;

   ------------
   -- Update --
   ------------

   procedure Update (Device           : in Access_Device_Type;
                     State_Group_Type : in String) is
   begin
      Talk (Device.Name);
      Talk (Get_Current_State (State_Group_Type));
      -- TODO: Find a better way to do this
   end Update;

   -------------
   -- Control --
   -------------

   procedure Control (Device       : in Access_Device_Type;
                      Action_Group : in String;
                      Action       : in String) is
   begin
      Talk (Device.Name);
      Talk (Action_Group);
      Talk (Action);
      -- TODO: Find a better way to do this
   end Control;

end Components.Comms;

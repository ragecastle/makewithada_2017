with GNAT.Sockets; use GNAT.Sockets;

package Components.Comms is

   type Message_Kind_Type is (Register,
                              Update,
                              Command,
                              EOM);

   type Message_Identifier_Type is (Device_Name,
                                    State_Group_Name,
                                    State_Name);

   type Message_Type is record
      Kind       : Message_Kind_Type;
      Identifier : Message_Identifier_Type;
      Message    : Device_String;
   end record;

   type Message_Handler_Type is access
     function (Channel : in Stream_Access) return Boolean;

   type Message_Talk_Handler_Type is access
     procedure (Channel : in Stream_Access;
                Message : in Message_Type);

   procedure Listen (Handler : in Message_Handler_Type;
                     Port    : in Port_Type := 5876);
   --
   -- Open a port to listen for messages

   procedure Talk (Message : in Message_Type;
                   Port    : in Port_Type := 5876);
   --
   -- Send a message to a server

   procedure Register (Device : Access_Device_Type);
   --
   -- Register a Device with a Hub.

   procedure Update (Device      : in Access_Device_Type;
                     State_Group : in String);
   --
   -- Send an update of the given State Group for a Device

--     procedure Control (Device       : in Access_Device_Type;
--                        Action_Group : in Device_String;
--                        Action       : in Device_String);
--     --
   -- Send a request for Action of a given Action Group for a given Device

end Components.Comms;

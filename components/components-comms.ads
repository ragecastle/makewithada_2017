with GNAT.Sockets; use GNAT.Sockets;

package Components.Comms is

   type Message_Kind_Type is (Register,
                              Update,
                              Command);

   type Message_Type (Kind : Message_Kind_Type) is record
      Device_Name : Device_String;
      case Kind is
      when Register =>
         null;
         when Update =>
            State_Group   : Device_String;
            Current_State : Device_String;
         when Command =>
            Action_Group : Device_String;
            Action       : Device_String;
      end case;
   end record;


   function Create_Message (Kind : Message_Kind_Type) return Access_Message_Type;
   --
   -- Creates a new message to be sent

   type Message_Handler_Type is access procedure (Channel : in Stream_Access);

   type Message_Talk_Handler_Type is access procedure (Channel : in Stream_Access;
                                                       Message : Message_Type);

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

   procedure Update (Device           : in Access_Device_Type;
                     State_Group_Type : in Device_String);
   --
   -- Send an update of the given State Group for a Device

   procedure Control (Device       : in Access_Device_Type;
                      Action_Group : in Device_String;
                      Action       : in Device_String);
   --
   -- Send a request for Action of a given Action Group for a given Device

end Components.Comms;

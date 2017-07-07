package Components.Comms is

   procedure Listen (Port : Port_Type := 5876);
   --
   -- Open a port to listen for messages

   procedure Talk (Message : String;
                   Port    : Port_Type := 5876);
   --
   -- Send a message to a server

   procedure Register (Device : Access_Device_Type);
   --
   -- Register a Device with a Hub.

   procedure Update (Device           : in Access_Device_Type;
                     State_Group_Type : in String);
   --
   -- Send an update of the given State Group for a Device

   procedure Control (Device       : in Access_Device_Type;
                      Action_Group : in String;
                      Action       : in String);
   --
   -- Send a request for Action of a given Action Group for a given Device

end Components.Comms;

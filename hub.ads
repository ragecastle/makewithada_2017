with Device; use Device;

package Hub is

   type Access_Hub_Type is private;

   subtype Hub_String is String (1 .. 80);

   function Create (Name : String) return Access_Hub_Type;
   --
   -- Create a new Hub

   procedure Add_Device (Hub    : in out Access_Hub_Type;
                         Device : in     Access_Device_Type);
   --
   -- Add a Device to a given Hub

private

   type Hub_Type;
   type Access_Hub_Type is access Hub_Type;

   type Hub_Type is record
      Name        : Hub_String;
      Device_List : Device.Access_Device_Type;
   end record;

end Hub;

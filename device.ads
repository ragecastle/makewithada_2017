with Ada.Containers.Vectors;

package Device is

   type Access_Device_Type is private;
   type Access_State_Group_Type is private;
   type Access_Action_Group_Type is private;

   subtype Device_String is String (1 .. 80);

   ------------
   -- Device --
   ------------

   function Create (Name : String) return Access_Device_Type;
   --
   -- Create a new Device

   procedure Add_State_Group
     (Device      : in out Access_Device_Type;
      State_Group : in     Access_State_Group_Type);
   --
   -- Add a new State Group to a given Device

   procedure Add_Action_Group
     (Device       : in out Access_Device_Type;
      Action_Group : in     Access_Action_Group_Type);
   --
   -- Add a new Action Group to a given Device

   -----------
   -- State --
   -----------

   function Create (Name : String) return Access_State_Group_Type;
   --
   -- Create a new State Group

   procedure Add_State (Group : in out Access_State_Group_Type;
                        State : in     String);
   --
   -- Add a new State to a given State Group

   procedure Set_Current_State (Device : in out Access_Device_Type;
                                Group  : in     String;
                                State  : in     Natural);
   --
   -- Set the Current State of the given State Group

   procedure Set_Current_State (Device : in out Access_Device_Type;
                                Group  : in     String;
                                State  : in     String);
   --
   -- Set the Current State of the given State Group

   function Get_Current_State (Device : Access_Device_Type;
                               Group  : String)
                               return Device_String;
   --
   -- Return the Current State for the given State Group

   function Get_State_Name (Device : Access_Device_Type;
                            Group  : String;
                            State  : Natural) return Device_String;
   --
   -- Returns the State Name for the State Code for the given State Group

   function Get_State_Code (Device : Access_Device_Type;
                            Group  : String;
                            State  : String) return Natural;
   --
   -- Returns the State Code for the given State Name for the given State Group

   ------------
   -- Action --
   ------------

   function Create (Name : String) return Access_Action_Group_Type;
   --
   -- Create a new Action Group

   procedure Add_Action (Group  : in out Access_Action_Group_Type;
                         Action : in     String);
   --
   -- Add a new Action to a given State Group

private

   type Device_Type;
   type State_Group_Type;
   type Action_Group_Type;

   type Access_Device_Type is access Device_Type;
   type Access_State_Group_Type is access State_Group_Type;
   type Access_Action_Group_Type is access Action_Group_Type;

   package State_Vector is new Ada.Containers.Vectors
     (Index_Type   => Natural,
      Element_Type => Device_String);

   package Action_Vector is new Ada.Containers.Vectors
     (Index_Type   => Natural,
      Element_Type => Device_String);

   type Device_Type is record
      Name        : Device_String;
      State_List  : Access_State_Group_Type;
      Action_List : Access_Action_Group_Type;
      Prev        : Access_Device_Type;
      Next        : Access_Device_Type;
   end record;

   type State_Group_Type is record
      Name          : Device_String;
      State_List    : State_Vector.Vector;
      Current_State : Device_String;
      Prev          : Access_State_Group_Type;
      Next          : Access_State_Group_Type;
   end record;

   type Action_Group_Type is record
      Name        : Device_String;
      Action_List : Action_Vector.Vector;
      Prev        : Access_Action_Group_Type;
      Next        : Access_Action_Group_Type;
   end record;

end Device;

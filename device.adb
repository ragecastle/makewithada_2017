with Ada.Strings.Fixed; use Ada.Strings;

package body Device is

   ------------
   -- Device --
   ------------

   ------------
   -- Create --
   ------------

   function Create (Name : String) return Access_Device_Type is
      Device : Access_Device_Type;
      Device_Name : Device_String;
   begin
      Ada.Strings.Fixed.Move (Source  => Name,
                              Target  => Device_Name,
                              Drop    => Right);

      Device := new Device_Type;
      Device.Name := Device_Name;
      return Device;
   end Create;

   ---------------------
   -- Add_State_Group --
   ---------------------

   procedure Add_State_Group
     (Device      : in out Access_Device_Type;
      State_Group : in     Access_State_Group_Type) is
      Cursor : Access_State_Group_Type;
   begin
      Cursor := Device.State_List;

      if Cursor = null then
         Device.State_List := State_Group;
      else
         while (Cursor.Next /= null) loop
            Cursor := Cursor.Next;
         end loop;
         Cursor.Next := State_Group;
         State_Group.Prev := Cursor;
      end if;

   end Add_State_Group;

   ----------------------
   -- Add_Action_Group --
   ----------------------

   procedure Add_Action_Group
     (Device       : in out Access_Device_Type;
      Action_Group : in     Access_Action_Group_Type) is
      Cursor : Access_Action_Group_Type;
   begin
      Cursor := Device.Action_List;

      if Cursor = null then
         Device.Action_List := Action_Group;
      else
         while (Cursor.Next /= null) loop
            Cursor := Cursor.Next;
         end loop;
         Cursor.Next := Action_Group;
         Action_Group.Prev := Cursor;
      end if;

   end Add_Action_Group;

   -----------
   -- State --
   -----------

   ------------
   -- Create --
   ------------

   function Create (Name : String) return Access_State_Group_Type is
      State_Group : Access_State_Group_Type;
      State_Name : Device_String;
   begin
      Ada.Strings.Fixed.Move (Source  => Name,
                              Target  => State_Name,
                              Drop    => Right);

      State_Group := new State_Group_Type;
      State_Group.Name := State_Name;
      return State_Group;
   end Create;

   ---------------
   -- Add_State --
   ---------------

   procedure Add_State (Group : in out Access_State_Group_Type;
                        State : in     String) is
      State_Name : Device_String;
   begin
      Ada.Strings.Fixed.Move (Source  => State,
                              Target  => State_Name,
                              Drop    => Right);
      Group.State_List.Append (State_Name);
   end Add_State;

   -----------------------
   -- Set_Current_State --
   -----------------------

   procedure Set_Current_State (Group : in out Access_State_Group_Type;
                                State : in     Natural) is
   begin
      if Natural (Group.State_List.Length) > State then
         Group.Current_State := Group.State_List (State);
      end if;
   end Set_Current_State;

   -----------------------
   -- Set_Current_State --
   -----------------------

   procedure Set_Current_State (Group : in out Access_State_Group_Type;
                                State : in     String) is
      State_Name : Device_String;
   begin
      Ada.Strings.Fixed.Move (Source  => State,
                              Target  => State_Name,
                              Drop    => Right);

      if Group.State_List.Contains (State_Name) then
         Group.Current_State := State_Name;
      end if;

   end Set_Current_State;

   -----------------------
   -- Get_Current_State --
   -----------------------

   function Get_Current_State (Group : Access_State_Group_Type)
                               return Device_String is
   begin
      return Group.Current_State;
   end Get_Current_State;

   --------------------
   -- Get_State_Name --
   --------------------

   function Get_State_Name (Group : Access_State_Group_Type;
                            State : Natural) return Device_String is
      Failed : Device_String;
   begin
      Ada.Strings.Fixed.Move (Source  => "Failed",
                              Target  => Failed,
                              Drop    => Right);

      if Natural (Group.State_List.Length) > State then
         return Group.State_List (State);
      else
         return Failed;
      end if;

   end Get_State_Name;

   --------------------
   -- Get_State_Code --
   --------------------

   function Get_State_Code (Group : Access_State_Group_Type;
                            State : String) return Natural is
      State_Name : Device_String;
   begin
      Ada.Strings.Fixed.Move (Source  => State,
                              Target  => State_Name,
                              Drop    => Right);

      if Group.State_List.Contains (State_Name) then
         return Group.State_List.Find_Index (State_Name);
      else
         return 400;
      end if;

   end Get_State_Code;

   ------------
   -- Action --
   ------------

   ------------
   -- Create --
   ------------

   function Create (Name : String) return Access_Action_Group_Type is
      Action_Group : Access_Action_Group_Type;
      Action_Name : Device_String;
   begin
      Ada.Strings.Fixed.Move (Source  => Name,
                              Target  => Action_Name,
                              Drop    => Right);

      Action_Group := new Action_Group_Type;
      Action_Group.Name := Action_Name;

      return Action_Group;

   end Create;

   ----------------
   -- Add_Action --
   ----------------

   procedure Add_Action (Group  : in out Access_Action_Group_Type;
                         Action : in     String) is
      Action_Name : Device_String;
   begin
      Ada.Strings.Fixed.Move (Source  => Action,
                              Target  => Action_Name,
                              Drop    => Right);

      Group.Action_List.Append (Action_Name);
   end Add_Action;


end Device;

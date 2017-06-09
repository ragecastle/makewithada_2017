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
      Group.Current_State := Group.State_List.First_Element;
   end Add_State;

   -----------------------
   -- Set_Current_State --
   -----------------------

   procedure Set_Current_State (Device : in out Access_Device_Type;
                                Group  : in     String;
                                State  : in     Natural) is
      Group_Name : Device_String;
      Cursor : Access_State_Group_Type := Device.State_List;
   begin
      Ada.Strings.Fixed.Move (Source  => Group,
                              Target  => Group_Name,
                              Drop    => Right);

      while (Cursor.Next /= null) loop
         if Cursor.Name = Group_Name then
            exit;
         else
            Cursor := Cursor.Next;
         end if;
      end loop;

      if Natural (Cursor.State_List.Length) > State then
         Cursor.Current_State := Cursor.State_List (State);
      end if;
   end Set_Current_State;

   -----------------------
   -- Set_Current_State --
   -----------------------

   procedure Set_Current_State (Device : in out Access_Device_Type;
                                Group  : in     String;
                                State  : in     String) is
      Group_Name : Device_String;
      State_Name : Device_String;
      Cursor : Access_State_Group_Type := Device.State_List;
   begin
      Ada.Strings.Fixed.Move (Source  => Group,
                              Target  => Group_Name,
                              Drop    => Right);

      Ada.Strings.Fixed.Move (Source  => State,
                              Target  => State_Name,
                              Drop    => Right);


      while (Cursor.Next /= null) loop
         if Cursor.Name = Group_Name then
            exit;
         else
            Cursor := Cursor.Next;
         end if;
      end loop;

      if Cursor.State_List.Contains (State_Name) then
         Cursor.Current_State := State_Name;
      end if;

   end Set_Current_State;

   -----------------------
   -- Get_Current_State --
   -----------------------

   function Get_Current_State (Device : Access_Device_Type;
                               Group  : String)
                               return Device_String is
      Group_Name : Device_String;
      Cursor : Access_State_Group_Type := Device.State_List;
   begin
      Ada.Strings.Fixed.Move (Source  => Group,
                              Target  => Group_Name,
                              Drop    => Right);

      while (Cursor.Next /= null) loop
         if Cursor.Name = Group_Name then
            exit;
         else
            Cursor := Cursor.Next;
         end if;
      end loop;

      return Cursor.Current_State;
   end Get_Current_State;

   --------------------
   -- Get_State_Name --
   --------------------

   function Get_State_Name (Device : Access_Device_Type;
                            Group  : String;
                            State  : Natural) return Device_String is
      Group_Name : Device_String;
      Failed : Device_String;
      Cursor : Access_State_Group_Type := Device.State_List;
   begin
      Ada.Strings.Fixed.Move (Source  => Group,
                              Target  => Group_Name,
                              Drop    => Right);

      Ada.Strings.Fixed.Move (Source  => "Failed",
                              Target  => Failed,
                              Drop    => Right);

      while (Cursor.Next /= null) loop
         if Cursor.Name = Group_Name then
            return Failed;
         else
            Cursor := Cursor.Next;
         end if;
      end loop;

      if Natural (Cursor.State_List.Length) > State then
         return Cursor.State_List (State);
      else
         return Failed;
      end if;

   end Get_State_Name;

   --------------------
   -- Get_State_Code --
   --------------------

   function Get_State_Code (Device : Access_Device_Type;
                            Group  : String;
                            State  : String) return Natural is
      Group_Name : Device_String;
      State_Name : Device_String;
      Cursor : Access_State_Group_Type := Device.State_List;
   begin
      Ada.Strings.Fixed.Move (Source  => Group,
                              Target  => Group_Name,
                              Drop    => Right);

      Ada.Strings.Fixed.Move (Source  => State,
                              Target  => State_Name,
                              Drop    => Right);

      while (Cursor.Next /= null) loop
         if Cursor.Name = Group_Name then
            return 400;
         else
            Cursor := Cursor.Next;
         end if;
      end loop;

      if Cursor.State_List.Contains (State_Name) then
         return Cursor.State_List.Find_Index (State_Name);
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

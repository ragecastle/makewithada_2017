with Ada.Text_IO; use Ada.Text_IO;
with Device;

package body Lightbulb is

   Lightbulb : Device.Access_Device_Type;

   procedure Power_State_Listener is
   begin
      Put_Line ("Changed Power State to: " &
                  Device.Get_Current_State (Lightbulb, "Power"));
   end Power_State_Listener;

   procedure Power_Action_Listener is
   begin
      Put_Line ("Toggling Power");
   end Power_Action_Listener;

   ----------------
   -- Initialize --
   ----------------

   procedure Initialize is
      Power_State_Group  : Device.Access_State_Group_Type;
      Power_Action_Group : Device.Access_Action_Group_Type;
   begin

      -- Create new State Group
      Power_State_Group := Device.Create ("Power");
      -- Add States to State Group
      Device.Add_State (Power_State_Group, "Off");
      Device.Add_State (Power_State_Group, "On");
      -- Attach Listener to State Group
      Device.Attach_State_Listener
        (Power_State_Group, Power_State_Listener'Access);

      -- Create new Action Group
      Power_Action_Group := Device.Create ("Power");
      -- Add Action to Action Group
      Device.Add_Action (Power_Action_Group, "Toggle");
      -- Attach Listener to Action Group
      Device.Attach_Action_Listener
        (Power_Action_Group, Power_Action_Listener'Access);

      -- Create new Device
      Lightbulb := Device.Create ("Lightbulb");
      -- Add the State Group to the Device
      Device.Add_State_Group (Lightbulb, Power_State_Group);

   end Initialize;

   -------------
   -- Execute --
   -------------

   procedure Execute is
   begin
      Device.Set_Current_State (Device => Lightbulb,
                                Group  => "Power",
                                State  => "Off");

      Device.Set_Current_State (Device => Lightbulb,
                                Group  => "Power",
                                State  => "On");

   end Execute;

end Lightbulb;

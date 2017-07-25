with Ada.Text_IO; use Ada.Text_IO;
with Components.Comms; use Components.Comms;

package body Lightbulb is

   Lightbulb : Components.Access_Device_Type;

   procedure Power_State_Listener is
   begin
      Put_Line ("Changed Power State to: " &
                  Components.Get_Current_State (Lightbulb, "Power"));
      Update (Lightbulb, "Power");
   end Power_State_Listener;

   procedure Power_Action_Listener is
   begin
      Put_Line ("Toggling Power");
   end Power_Action_Listener;

   procedure Color_State_Listener is
   begin
      Put_Line ("Changed Color State to: " &
                  Components.Get_Current_State (Lightbulb, "Color"));
   end Color_State_Listener;

   ----------------
   -- Initialize --
   ----------------

   procedure Initialize is
      Power_State_Group  : Components.Access_State_Group_Type;
      Color_State_Group  : Components.Access_State_Group_Type;
      Power_Action_Group : Components.Access_Action_Group_Type;
   begin

      -- Create new State Group
      Power_State_Group := Components.Create ("Power");
      -- Add States to State Group
      Components.Add_State (Power_State_Group, "Off");
      Components.Add_State (Power_State_Group, "On");
      -- Attach Listener to State Group
      Components.Attach_State_Listener
        (Power_State_Group, Power_State_Listener'Access);

      Color_State_Group := Components.Create ("Color");
      Components.Add_State (Color_State_Group, "Red");
      Components.Add_State (Color_State_Group, "Blue");
      Components.Add_State (Color_State_Group, "Yellow");
      Components.Attach_State_Listener
        (Color_State_Group, Color_State_Listener'Access);

      -- Create new Action Group
      Power_Action_Group := Components.Create ("Power");
      -- Add Action to Action Group
      Components.Add_Action (Power_Action_Group, "Toggle");

      -- Create new Device
      Lightbulb := Components.Create ("Lightbulb");
      -- Add the State Group to the Device
      Components.Add_State_Group (Lightbulb, Power_State_Group);
      -- Add the Color State Group to the Device
      Components.Add_State_Group (Lightbulb, Color_State_Group);

      Register (Lightbulb);

   end Initialize;

   -------------
   -- Execute --
   -------------

   procedure Execute is
   begin
      Components.Set_Current_State (Device => Lightbulb,
                                    Group  => "Power",
                                    State  => "Off");

      delay 10.0;

      Components.Set_Current_State (Device => Lightbulb,
                                    Group  => "Power",
                                    State  => "On");

      delay 10.0;

      Components.Set_Current_State (Device => Lightbulb,
                                    Group => "Color",
                                    State => "Red");

      delay 10.0;

      Components.Set_Current_State (Device => Lightbulb,
                                    Group => "Color",
                                    State => "Blue");

      delay 10.0;

      Components.Set_Current_State (Device => Lightbulb,
                                    Group => "Color",
                                    State => "Yellow");

      delay 10.0;


   end Execute;

end Lightbulb;

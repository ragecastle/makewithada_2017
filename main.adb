with Ada.Text_IO; use Ada.Text_IO;
with Device;

procedure Main is
   Lightbulb : Device.Access_Device_Type;
   Power_State_Group : Device.Access_State_Group_Type;
   Power_Action_Group : Device.Access_Action_Group_Type;
begin

   Power_State_Group := Device.Create ("Power");
   Device.Add_State (Power_State_Group, "Off");
   Device.Add_State (Power_State_Group, "On");

   Lightbulb := Device.Create ("Lightbulb");
   Device.Add_State_Group (Lightbulb, Power_State_Group);

   Put_Line (Device.Get_Current_State (Lightbulb, "Power"));

end Main;

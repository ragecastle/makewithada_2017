with Ada.Text_IO; use Ada.Text_IO;
with Test_Example; use Test_Example;

procedure Main is

begin
   Put_Line ("Running main test");
   pragma Assert (Test_Example.Function_Ex (PT => 1) = 2, "Should be 1");

end Main;

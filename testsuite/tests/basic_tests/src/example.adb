with Ada.Text_IO; use Ada.Text_IO;
with Ada.Exceptions; use Ada.Exceptions;
with Test_Example;

procedure Example is

begin
   pragma Assert (Test_Example.Function_Ex (PT => 1) = 1,
                  "{ID}: Should be {X}");

exception
   when E : others =>
      Put_Line (Exception_Message (E));
end Example;

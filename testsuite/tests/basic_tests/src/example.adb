with AUnit.Assertions; use AUnit.Assertions;
with Test_Example;

procedure Example is

begin
   pragma Assert (Test_Example.Function_Ex (PT => 1) = 2, "Should be 1");
end Example;

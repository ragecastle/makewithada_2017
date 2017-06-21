with AUnit.Assertions; use AUnit.Assertions;

procedure Example is
   function Echo (PT : in Integer) return Integer;

   function Echo (PT : in Integer) return Integer
   is
   begin
      return PT;
   end Echo;

begin

   pragma Assert (Echo (PT => 1) = 2, "Should be 1");
end Example;

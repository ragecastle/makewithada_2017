procedure Example is
   function Echo (PT : in Integer) return Integer;

   ---------------------
   -- List_Partitions --
   ---------------------

   function Echo (PT : in Integer) return Integer
   is
   begin
      return PT;
   end Echo;

begin
   pragma Assert (Echo (PT => 1) = 2, "Should be 1");
end Example;

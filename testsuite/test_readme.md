# Testing Setup

Basic useful feature list:


I couldn't find a native CI box that works with ada so we have this Travis implementation.

## Setup
We don't have a methodology locked down yet but this implementation will work for any way we go.
1. Import Aunit (already in project)
2. Import the utility python script (already in project)
3. Add a new directory under testsuite/tests for logically grouped tests
4. Add a ```symbolic link``` to the aunit directory most likely ```../../../aunit```
5. Add a GPR file named tc.gpr and have it look like this

```js 
project TC is
   for Languages use ("Ada");
   for Source_Dirs use ("src","../../../aunit/**");
   for Main use ("example.adb");
   for Object_Dir use "obj";
   for Exec_Dir use "bin";
   package Compiler is
      for Default_Switches ("Ada")
        use ("-fstack-check", --  Generate stack checking code (part of Ada)
             "-gnata",        --  Enable assertions            (part of Ada)
             "-gnato13",      --  Overflow checking            (part of Ada)
             "-gnatf",                      --  Full, verbose error messages
             "-gnatwa",                     --  All optional warnings
             "-gnatVa",                     --  All validity checks
             "-gnaty3abcdefhiklmnoOprstux", --  Style checks
             "-gnat2012",                   --  Use Ada 2012
             "-Wall",                       --  All GCC warnings
             "-O2");                        --  Optimise (level 2/3)
   end Compiler;
end TC;

```

6. Add a out file named ```{name of main}.out``` This file is where things get a little weird. This is the expected output file. Ada Assert will print to STDOUT on a failure. This file tracks those outputs. Really this is only useful if you're testing exceptions or Ada.Text_IO without using an assert. This file can remain empty if you don't plan on using it but it must be there.
7. In src/ add a main file that you used in step 5 and 6. 

## Writing Cases
When writing cases we want to use the following
```js 
pragma Assert (Test_Example.Function_Ex (PT => 1) = 1, "{ID}: Should be {X}"); 
```

Where {ID} is whatever you think youll need to find this test case if it fails and {X} being the expected value. In the event the test fails, the message will be printed and Travis will fail the build

## Examples
### src/examples
Contains a single function that returns the passed in integer.
### testsuite/tests/basic_tests
Contains a basic test setup.



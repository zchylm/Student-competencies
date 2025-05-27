pragma SPARK_Mode (On);

with MyCommandLine;
with Calculator;

with Ada.Text_IO; use Ada.Text_IO;

procedure Main is
begin
   -- Check if a master PIN was provided
   if MyCommandLine.Argument_Count < 1 then
      Put_Line("Error: Master PIN required.");
      return;
   end if;

   -- Initialize the calculator with the master PIN
   Calculator.Initialize(MyCommandLine.Argument(1));
   
   -- Check if initialization was successful
   if not Calculator.Is_Initialized then
      return;
   end if;

   -- Main command processing loop
   loop
      -- Display prompt based on locked state
      Calculator.Display_Prompt;
      
      -- Read and process command
      Calculator.Process_Command;
   end loop;
end Main;

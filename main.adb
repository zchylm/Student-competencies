pragma SPARK_Mode (On);

with MyCommandLine;
with Calculator;

with Ada.Text_IO; use Ada.Text_IO;

procedure Main is
   -- This initialization ensures these variables are initialized before being used
   procedure Initialize is
   begin
      if MyCommandLine.Argument_Count >= 1 then
         declare
            PIN_Str : constant String := MyCommandLine.Argument(1);
         begin
            if PIN_Str'Length = 4 and then
               (for all I in PIN_Str'Range => PIN_Str(I) >= '0' and PIN_Str(I) <= '9') then
               Calculator.Initialize(PIN_Str);
            else
               Put_Line("Error: Master PIN must be a 4-digit number.");
            end if;
         end;
      else
         Put_Line("Error: Master PIN required.");
      end if;
   end Initialize;
begin
   -- Initialize the calculator
   Initialize;
   
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

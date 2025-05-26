pragma SPARK_Mode (On);

with MyCommandLine;
with MyString;
with MyStringTokeniser;
with StringToInteger;
with PIN;
with MemoryStore;
with Interfaces;

with Ada.Text_IO; use Ada.Text_IO;
with Ada.Integer_Text_IO; use Ada.Integer_Text_IO;

procedure Main is
   --  Helper instantiation for bounded lines
   package Lines is new MyString (Max_MyString_Length => 2048);
   Input_Line : Lines.MyString;

   --  Memory database for the calculator
   Mem : MemoryStore.Database;

   --  Stack for operands
   Stack_Size : constant := 512;
   type Stack_Array is array (1..Stack_Size) of Integer;
   Stack : Stack_Array;
   Stack_Top : Natural := 0;

   --  PIN and state management
   Master_PIN : PIN.PIN;
   Is_Locked : Boolean := True;

   --  Stack operations
   function Is_Stack_Empty return Boolean is (Stack_Top = 0);
   function Is_Stack_Full return Boolean is (Stack_Top = Stack_Size);

   procedure Stack_Push(Value : in Integer) is
   begin
      if not Is_Stack_Full then
         Stack_Top := Stack_Top + 1;
         Stack(Stack_Top) := Value;
      else
         Put_Line("Error: Stack overflow");
      end if;
   end Stack_Push;

   procedure Stack_Pop(Value : out Integer) is
   begin
      if not Is_Stack_Empty then
         Value := Stack(Stack_Top);
         Stack_Top := Stack_Top - 1;
      else
         Put_Line("Error: Stack underflow");
         Value := 0;
      end if;
   end Stack_Pop;

   --  Validate if a string is a valid 4-digit PIN
   function Is_Valid_PIN(S : String) return Boolean is
   begin
      return S'Length = 4 and then
             (for all I in S'Range => S(I) >= '0' and S(I) <= '9');
   end Is_Valid_PIN;

   --  Process commands based on tokenized input
   procedure Process_Command(Tokens : in MyStringTokeniser.TokenArray; 
                            Num_Tokens : in Natural) is
      Val1, Val2, Result : Integer;
      Location : MemoryStore.Location_Index;
      Valid_Command : Boolean := True;
   begin
      if Num_Tokens > 0 then
         declare
            Token_Str : String := Lines.To_String(
               Lines.Substring(Input_Line, 
                             Tokens(1).Start, 
                             Tokens(1).Start + Tokens(1).Length - 1));
         begin
            --  Commands available in locked state
      if Token_Str = "unlock" then
         if Num_Tokens < 2 then
            Put_Line("Error: Missing PIN for unlock command");
            return;
         end if;

         declare
            PIN_Str : String := Lines.To_String(
               Lines.Substring(Input_Line, 
                             Tokens(2).Start, 
                             Tokens(2).Start + Tokens(2).Length - 1));
         begin
            if not Is_Valid_PIN(PIN_Str) then
               Put_Line("Error: Invalid PIN format");
               return;
            end if;

            declare
               Input_PIN : PIN.PIN := PIN.From_String(PIN_Str);
            begin
               if Is_Locked and PIN."="(Input_PIN, Master_PIN) then
                  Is_Locked := False;
               end if;
            end;
         end;
         return;
      end if;

      --  All other commands require the calculator to be unlocked
      if Is_Locked then
         Put_Line("Error: Calculator is locked");
         return;
      end if;

      --  Commands available in unlocked state
      if Token_Str = "lock" then
         if Num_Tokens < 2 then
            Put_Line("Error: Missing new PIN for lock command");
            return;
         end if;

         declare
            PIN_Str : String := Lines.To_String(
               Lines.Substring(Input_Line, 
                             Tokens(2).Start, 
                             Tokens(2).Start + Tokens(2).Length - 1));
         begin
            if not Is_Valid_PIN(PIN_Str) then
               Put_Line("Error: Invalid PIN format");
               return;
            end if;

            Master_PIN := PIN.From_String(PIN_Str);
            Is_Locked := True;
         end;

      elsif Token_Str = "push1" then
         if Num_Tokens < 2 then
            Put_Line("Error: Missing value for push1 command");
            return;
         end if;

         declare
            Num_Str : String := Lines.To_String(
               Lines.Substring(Input_Line, 
                             Tokens(2).Start, 
                             Tokens(2).Start + Tokens(2).Length - 1));
            Value : Integer := StringToInteger.From_String(Num_Str);
         begin
            Stack_Push(Value);
         end;

      elsif Token_Str = "push2" then
         if Num_Tokens < 3 then
            Put_Line("Error: Missing values for push2 command");
            return;
         end if;

         declare
            Num1_Str : String := Lines.To_String(
               Lines.Substring(Input_Line, 
                             Tokens(2).Start, 
                             Tokens(2).Start + Tokens(2).Length - 1));
            Num2_Str : String := Lines.To_String(
               Lines.Substring(Input_Line, 
                             Tokens(3).Start, 
                             Tokens(3).Start + Tokens(3).Length - 1));
            Value1 : Integer := StringToInteger.From_String(Num1_Str);
            Value2 : Integer := StringToInteger.From_String(Num2_Str);
         begin
            Stack_Push(Value1);
            Stack_Push(Value2);
         end;

      elsif Token_Str = "pop" then
         if not Is_Stack_Empty then
            Stack_Pop(Val1);  -- Discard the value
         else
            Put_Line("Error: Cannot pop from empty stack");
         end if;

      elsif Token_Str = "+" then
         if Stack_Top >= 2 then
            Stack_Pop(Val2);
            Stack_Pop(Val1);
            
            declare
               use Interfaces;
               Result64 : Integer_64 := Integer_64(Val1) + Integer_64(Val2);
            begin
               if Result64 > Integer_64(Integer'Last) or 
                  Result64 < Integer_64(Integer'First) then
                  Put_Line("Error: Integer overflow in addition");
               else
                  Result := Integer(Result64);
                  Stack_Push(Result);
               end if;
            end;
         else
            Put_Line("Error: Not enough operands for addition");
         end if;

      elsif Token_Str = "-" then
         if Stack_Top >= 2 then
            Stack_Pop(Val2);
            Stack_Pop(Val1);
            
            -- Check for overflow before subtracting
            declare
               use Interfaces;
               Result64 : Integer_64 := Integer_64(Val1) - Integer_64(Val2);
            begin
               if Result64 > Integer_64(Integer'Last) or 
                  Result64 < Integer_64(Integer'First) then
                  Put_Line("Error: Integer overflow in subtraction");
               else
                  Result := Integer(Result64);
                  Stack_Push(Result);
               end if;
            end;
         else
            Put_Line("Error: Not enough operands for subtraction");
         end if;

      elsif Token_Str = "*" then
         if Stack_Top >= 2 then
            Stack_Pop(Val2);
            Stack_Pop(Val1);
            
            -- Using Interfaces.Integer_64 to detect overflow
            declare
               use Interfaces;
               Result64 : Integer_64 := Integer_64(Val1) * Integer_64(Val2);
            begin
               if Result64 > Integer_64(Integer'Last) or 
                  Result64 < Integer_64(Integer'First) then
                  Put_Line("Error: Integer overflow in multiplication");
               else
                  Result := Integer(Result64);
                  Stack_Push(Result);
               end if;
            end;
         else
            Put_Line("Error: Not enough operands for multiplication");
         end if;

      elsif Token_Str = "/" then
         if Stack_Top >= 2 then
            Stack_Pop(Val2);
            Stack_Pop(Val1);
            
            if Val2 = 0 then
               Put_Line("Error: Division by zero");
            else
               Result := Val1 / Val2;
               Stack_Push(Result);
            end if;
         else
            Put_Line("Error: Not enough operands for division");
         end if;

      elsif Token_Str = "storeTo" then
         if Num_Tokens < 2 then
            Put_Line("Error: Missing location for storeTo command");
            return;
         end if;

         declare
            Loc_Str : String := Lines.To_String(
               Lines.Substring(Input_Line, 
                             Tokens(2).Start, 
                             Tokens(2).Start + Tokens(2).Length - 1));
            Loc_Val : Integer := StringToInteger.From_String(Loc_Str);
         begin
            if Loc_Val < 1 or Loc_Val > MemoryStore.Max_Locations then
               Put_Line("Error: Invalid memory location");
               return;
            end if;
            
            Location := MemoryStore.Location_Index(Loc_Val);
            
            if not Is_Stack_Empty then
               Stack_Pop(Val1);
               MemoryStore.Put(Mem, Location, MemoryStore.Int32(Val1));
            else
               Put_Line("Error: Cannot store from empty stack");
            end if;
         end;

      elsif Token_Str = "loadFrom" then
         if Num_Tokens < 2 then
            Put_Line("Error: Missing location for loadFrom command");
            return;
         end if;

         declare
            Loc_Str : String := Lines.To_String(
               Lines.Substring(Input_Line, 
                             Tokens(2).Start, 
                             Tokens(2).Start + Tokens(2).Length - 1));
            Loc_Val : Integer := StringToInteger.From_String(Loc_Str);
         begin
            if Loc_Val < 1 or Loc_Val > MemoryStore.Max_Locations then
               Put_Line("Error: Invalid memory location");
               return;
            end if;
            
            Location := MemoryStore.Location_Index(Loc_Val);
            
            if MemoryStore.Has(Mem, Location) then
               Val1 := Integer(MemoryStore.Get(Mem, Location));
               Stack_Push(Val1);
            else
               Put_Line("Error: Memory location not defined");
            end if;
         end;

      elsif Token_Str = "remove" then
         if Num_Tokens < 2 then
            Put_Line("Error: Missing location for remove command");
            return;
         end if;

         declare
            Loc_Str : String := Lines.To_String(
               Lines.Substring(Input_Line, 
                             Tokens(2).Start, 
                             Tokens(2).Start + Tokens(2).Length - 1));
            Loc_Val : Integer := StringToInteger.From_String(Loc_Str);
         begin
            if Loc_Val < 1 or Loc_Val > MemoryStore.Max_Locations then
               Put_Line("Error: Invalid memory location");
               return;
            end if;
            
            Location := MemoryStore.Location_Index(Loc_Val);
            MemoryStore.Remove(Mem, Location);
         end;

      elsif Token_Str = "list" then
         MemoryStore.Print(Mem);

      else
         Valid_Command := False;
         Put_Line("Error: Unknown command '" & Token_Str & "'");
      end if;
   
         end;
      else
         --  Empty command, do nothing
         return;
      end if;
      end Process_Command;

      

begin
   -- Check if a master PIN was provided
   if MyCommandLine.Argument_Count < 1 then
      Put_Line("Error: Master PIN required.");
      return;
   end if;

   -- Parse and set the master PIN
   declare
      PIN_Str : String := MyCommandLine.Argument(1);
   begin
      if not Is_Valid_PIN(PIN_Str) then
         Put_Line("Error: Master PIN must be a 4-digit number.");
         return;
      end if;
      
      Master_PIN := PIN.From_String(PIN_Str);
   end;

   -- Initialize the memory store
   MemoryStore.Init(Mem);

   -- Main command processing loop
   loop
      -- Display prompt based on locked state
      if Is_Locked then
         Put("locked> ");
      else
         Put("unlocked> ");
      end if;
      
      -- Read and process command
      Lines.Get_Line(Input_Line);
      
      -- Parse the command
      declare
         Tokens : MyStringTokeniser.TokenArray(1..5) := (others => (Start => 1, Length => 0));
         Num_Tokens : Natural;
      begin
         MyStringTokeniser.Tokenise(Lines.To_String(Input_Line), Tokens, Num_Tokens);
         Process_Command(Tokens, Num_Tokens);
      end;
   end loop;
end Main;
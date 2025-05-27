with StringToInteger;
with Ada.Text_IO; use Ada.Text_IO;
with Ada.Integer_Text_IO; use Ada.Integer_Text_IO;
with MyStringTokeniser;

package body Calculator with SPARK_Mode is
   
   procedure Initialize(PIN_Str : String) is
   begin
      if not Is_Valid_PIN(PIN_Str) then
         Put_Line("Error: Master PIN must be a 4-digit number.");
         return;
      end if;
      
      Master_PIN := PIN.From_String(PIN_Str);
      MemoryStore.Init(Mem);
      Is_Locked := True;
      Initialized := True;
   end Initialize;
   
   function Is_Initialized return Boolean is
   begin
      return Initialized;
   end Is_Initialized;
   
   procedure Display_Prompt is
   begin
      if Is_Locked then
         Put("locked> ");
      else
         Put("unlocked> ");
      end if;
   end Display_Prompt;
   
   procedure Process_Command is
      Input_Line : Lines.MyString;
   begin
      -- Read input
      Lines.Get_Line(Input_Line);
      
      -- Process it
      Process_Command_Internal(Input_Line);
   end Process_Command;
   
   procedure Process_Command_Internal(Input_Line : Lines.MyString) is
      Tokens : MyStringTokeniser.TokenArray(1..5) := (others => (Start => 1, Length => 0));
      Num_Tokens : Natural;
   begin
      -- Tokenize and process
      MyStringTokeniser.Tokenise(Lines.To_String(Input_Line), Tokens, Num_Tokens);
      
      if Num_Tokens = 0 then
         --  Empty command, do nothing
         return;
      end if;

      declare
         Command : String := Get_Token_String(Input_Line, Tokens, 1);
      begin
         --  Commands available in locked state
         if Command = "unlock" then
            Handle_Unlock_Command(Input_Line, Tokens, Num_Tokens);
            return;
         end if;

         --  All other commands require the calculator to be unlocked
         if Is_Locked then
            Put_Line("Error: Calculator is locked");
            return;
         end if;

         --  Commands available in unlocked state
         if Command = "lock" then
            Handle_Lock_Command(Input_Line, Tokens, Num_Tokens);
         elsif Command = "push1" then
            Handle_Push1_Command(Input_Line, Tokens, Num_Tokens);
         elsif Command = "push2" then
            Handle_Push2_Command(Input_Line, Tokens, Num_Tokens);
         elsif Command = "pop" then
            if not Is_Stack_Empty then
               declare
                  Discard_Value : Integer;
               begin
                  Stack_Pop(Discard_Value);  -- Discard the value
               end;
            else
               Put_Line("Error: Cannot pop from empty stack");
            end if;
         elsif Command = "+" then
            Handle_Addition;
         elsif Command = "-" then
            Handle_Subtraction;
         elsif Command = "*" then
            Handle_Multiplication;
         elsif Command = "/" then
            Handle_Division;
         elsif Command = "storeTo" then
            Handle_StoreTo_Command(Input_Line, Tokens, Num_Tokens);
         elsif Command = "loadFrom" then
            Handle_LoadFrom_Command(Input_Line, Tokens, Num_Tokens);
         elsif Command = "remove" then
            Handle_Remove_Command(Input_Line, Tokens, Num_Tokens);
         elsif Command = "list" then
            MemoryStore.Print(Mem);
         else
            Put_Line("Error: Unknown command '" & Command & "'");
         end if;
      end;
   end Process_Command_Internal;
   
   -- Stack operations
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

   -- Helper functions
   function Get_Token_String(Input_Line : Lines.MyString;
                           Tokens : MyStringTokeniser.TokenArray; 
                           Token_Index : Positive) return String is
   begin
      return Lines.To_String(
         Lines.Substring(Input_Line, 
                       Tokens(Token_Index).Start, 
                       Tokens(Token_Index).Start + Tokens(Token_Index).Length - 1));
   end Get_Token_String;

   function Is_Valid_PIN(S : String) return Boolean is
   begin
      return S'Length = 4 and then
             (for all I in S'Range => S(I) >= '0' and S(I) <= '9');
   end Is_Valid_PIN;

   -- Pure function with no side effects for SPARK
   function Get_Valid_Location(Input_Line : Lines.MyString;
                             Tokens : MyStringTokeniser.TokenArray;
                             Token_Index : Positive) return Location_Result is
      Loc_Str : String := Get_Token_String(Input_Line, Tokens, Token_Index);
      Loc_Val : Integer := StringToInteger.From_String(Loc_Str);
      Result : Location_Result := (Valid => False, Location => 1);
   begin
      if Loc_Val >= 1 and Loc_Val <= MemoryStore.Max_Locations then
         Result.Valid := True;
         Result.Location := MemoryStore.Location_Index(Loc_Val);
      end if;
      
      return Result;
   end Get_Valid_Location;

   -- Arithmetic operations
   procedure Handle_Addition is
      Val1, Val2, Result : Integer;
   begin
      if Stack_Top >= 2 then
         Stack_Pop(Val2);
         Stack_Pop(Val1);
         
         declare
            Result_Long : Long_Long_Integer := Long_Long_Integer(Val1) + Long_Long_Integer(Val2);
         begin
            if Result_Long > Long_Long_Integer(Integer'Last) or 
               Result_Long < Long_Long_Integer(Integer'First) then
               Put_Line("Error: Integer overflow in addition");
            else
               Result := Integer(Result_Long);
               Stack_Push(Result);
            end if;
         end;
      else
         Put_Line("Error: Not enough operands for addition");
      end if;
   end Handle_Addition;

   procedure Handle_Subtraction is
      Val1, Val2, Result : Integer;
   begin
      if Stack_Top >= 2 then
         Stack_Pop(Val2);
         Stack_Pop(Val1);
         
         declare
            Result_Long : Long_Long_Integer := Long_Long_Integer(Val1) - Long_Long_Integer(Val2);
         begin
            if Result_Long > Long_Long_Integer(Integer'Last) or 
               Result_Long < Long_Long_Integer(Integer'First) then
               Put_Line("Error: Integer overflow in subtraction");
            else
               Result := Integer(Result_Long);
               Stack_Push(Result);
            end if;
         end;
      else
         Put_Line("Error: Not enough operands for subtraction");
      end if;
   end Handle_Subtraction;

   procedure Handle_Multiplication is
      Val1, Val2, Result : Integer;
   begin
      if Stack_Top >= 2 then
         Stack_Pop(Val2);
         Stack_Pop(Val1);
         
         declare
            Result_Long : Long_Long_Integer := Long_Long_Integer(Val1) * Long_Long_Integer(Val2);
         begin
            if Result_Long > Long_Long_Integer(Integer'Last) or 
               Result_Long < Long_Long_Integer(Integer'First) then
               Put_Line("Error: Integer overflow in multiplication");
            else
               Result := Integer(Result_Long);
               Stack_Push(Result);
            end if;
         end;
      else
         Put_Line("Error: Not enough operands for multiplication");
      end if;
   end Handle_Multiplication;

   procedure Handle_Division is
      Val1, Val2, Result : Integer;
   begin
      if Stack_Top >= 2 then
         Stack_Pop(Val2);
         Stack_Pop(Val1);
         
         if Val2 = 0 then
            Put_Line("Error: Division by zero");
            return;
         end if;
         
         declare
            Result_Long : Long_Long_Integer := Long_Long_Integer(Val1) / Long_Long_Integer(Val2);
         begin
            if Result_Long > Long_Long_Integer(Integer'Last) or 
               Result_Long < Long_Long_Integer(Integer'First) then
               Put_Line("Error: Integer overflow in division");
            else
               Result := Integer(Result_Long);
               Stack_Push(Result);
            end if;
         end;
      else
         Put_Line("Error: Not enough operands for division");
      end if;
   end Handle_Division;

   -- Command handlers
   procedure Handle_Unlock_Command(Input_Line : Lines.MyString;
                                 Tokens : MyStringTokeniser.TokenArray; 
                                 Num_Tokens : Natural) is
   begin
      if Num_Tokens < 2 then
         Put_Line("Error: Missing PIN for unlock command");
         return;
      end if;

      declare
         PIN_Str : String := Get_Token_String(Input_Line, Tokens, 2);
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
   end Handle_Unlock_Command;

   procedure Handle_Lock_Command(Input_Line : Lines.MyString;
                               Tokens : MyStringTokeniser.TokenArray; 
                               Num_Tokens : Natural) is
   begin
      if Num_Tokens < 2 then
         Put_Line("Error: Missing new PIN for lock command");
         return;
      end if;

      declare
         PIN_Str : String := Get_Token_String(Input_Line, Tokens, 2);
      begin
         if not Is_Valid_PIN(PIN_Str) then
            Put_Line("Error: Invalid PIN format");
            return;
         end if;

         Master_PIN := PIN.From_String(PIN_Str);
         Is_Locked := True;
      end;
   end Handle_Lock_Command;

   procedure Handle_Push1_Command(Input_Line : Lines.MyString;
                                Tokens : MyStringTokeniser.TokenArray; 
                                Num_Tokens : Natural) is
   begin
      if Num_Tokens < 2 then
         Put_Line("Error: Missing value for push1 command");
         return;
      end if;

      declare
         Num_Str : String := Get_Token_String(Input_Line, Tokens, 2);
         Value : Integer := StringToInteger.From_String(Num_Str);
      begin
         Stack_Push(Value);
      end;
   end Handle_Push1_Command;

   procedure Handle_Push2_Command(Input_Line : Lines.MyString;
                                Tokens : MyStringTokeniser.TokenArray; 
                                Num_Tokens : Natural) is
   begin
      if Num_Tokens < 3 then
         Put_Line("Error: Missing values for push2 command");
         return;
      end if;

      declare
         Num1_Str : String := Get_Token_String(Input_Line, Tokens, 2);
         Num2_Str : String := Get_Token_String(Input_Line, Tokens, 3);
         Value1 : Integer := StringToInteger.From_String(Num1_Str);
         Value2 : Integer := StringToInteger.From_String(Num2_Str);
      begin
         Stack_Push(Value1);
         Stack_Push(Value2);
      end;
   end Handle_Push2_Command;

   procedure Handle_StoreTo_Command(Input_Line : Lines.MyString;
                                  Tokens : MyStringTokeniser.TokenArray; 
                                  Num_Tokens : Natural) is
      Loc_Result : Location_Result;
      Val1 : Integer;
   begin
      if Num_Tokens < 2 then
         Put_Line("Error: Missing location for storeTo command");
         return;
      end if;

      Loc_Result := Get_Valid_Location(Input_Line, Tokens, 2);
      
      if Loc_Result.Valid then
         if not Is_Stack_Empty then
            Stack_Pop(Val1);
            MemoryStore.Put(Mem, Loc_Result.Location, MemoryStore.Int32(Val1));
         else
            Put_Line("Error: Cannot store from empty stack");
         end if;
      else
         Put_Line("Error: Invalid memory location");
      end if;
   end Handle_StoreTo_Command;

   procedure Handle_LoadFrom_Command(Input_Line : Lines.MyString;
                                   Tokens : MyStringTokeniser.TokenArray; 
                                   Num_Tokens : Natural) is
      Loc_Result : Location_Result;
      Val1 : Integer;
   begin
      if Num_Tokens < 2 then
         Put_Line("Error: Missing location for loadFrom command");
         return;
      end if;

      Loc_Result := Get_Valid_Location(Input_Line, Tokens, 2);
      
      if Loc_Result.Valid then
         if MemoryStore.Has(Mem, Loc_Result.Location) then
            Val1 := Integer(MemoryStore.Get(Mem, Loc_Result.Location));
            Stack_Push(Val1);
         else
            Put_Line("Error: Memory location not defined");
         end if;
      else
         Put_Line("Error: Invalid memory location");
      end if;
   end Handle_LoadFrom_Command;

   procedure Handle_Remove_Command(Input_Line : Lines.MyString;
                                 Tokens : MyStringTokeniser.TokenArray; 
                                 Num_Tokens : Natural) is
      Loc_Result : Location_Result;
   begin
      if Num_Tokens < 2 then
         Put_Line("Error: Missing location for remove command");
         return;
      end if;

      Loc_Result := Get_Valid_Location(Input_Line, Tokens, 2);
      
      if Loc_Result.Valid then
         MemoryStore.Remove(Mem, Loc_Result.Location);
      else
         Put_Line("Error: Invalid memory location");
      end if;
   end Handle_Remove_Command;
   
end Calculator;
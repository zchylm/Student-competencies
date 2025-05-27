with MyString;
with MyStringTokeniser;
with PIN;
with MemoryStore;

package Calculator with SPARK_Mode is
   -- Initialize the calculator with a master PIN
   procedure Initialize(PIN_Str : String) with
     Pre => (PIN_Str'Length = 4 and then
             (for all I in PIN_Str'Range => PIN_Str(I) >= '0' and PIN_Str(I) <= '9'));
   
   -- Process a single command (read input, process it)
   procedure Process_Command;
   
   -- Check if calculator is ready
   function Is_Initialized return Boolean;
   
   -- Display the prompt
   procedure Display_Prompt;

private
   -- Package to handle command line input
   package Lines is new MyString(Max_MyString_Length => 2048);
   
   -- Stack constants and types
   Stack_Size : constant := 512;
   type Stack_Array is array (1..Stack_Size) of Integer;
   
   -- Memory and stack state
   Mem : MemoryStore.Database;
   Stack : Stack_Array;
   Stack_Top : Natural := 0;
   
   -- PIN and lock state
   Master_PIN : PIN.PIN;
   Is_Locked : Boolean := True;
   
   -- Flag to indicate successful initialization
   Initialized : Boolean := False;
   
   -- Stack operations
   function Is_Stack_Empty return Boolean is (Stack_Top = 0);
   function Is_Stack_Full return Boolean is (Stack_Top = Stack_Size);
   
   procedure Stack_Push(Value : in Integer) with
     Pre => Stack_Top < Stack_Size,
     Post => Stack_Top = Stack_Top'Old + 1 and Stack(Stack_Top) = Value;
   
   procedure Stack_Pop(Value : out Integer) with
     Pre => Stack_Top > 0,
     Post => Stack_Top = Stack_Top'Old - 1 and Value = Stack(Stack_Top'Old);
   
   -- Helper functions
   function Get_Token_String(Input_Line : Lines.MyString; 
                            Tokens : MyStringTokeniser.TokenArray; 
                            Token_Index : Positive) return String with
     Pre => Token_Index <= Tokens'Last and
            Token_Index >= Tokens'First and
            Tokens(Token_Index).Start >= 1 and
            Tokens(Token_Index).Length >= 0 and
            Tokens(Token_Index).Start + Tokens(Token_Index).Length - 1 <= Lines.Length(Input_Line) and
            Tokens(Token_Index).Start + Tokens(Token_Index).Length - 1 >= Tokens(Token_Index).Start;
   
   function Is_Valid_PIN(S : String) return Boolean with
     Post => (if Is_Valid_PIN'Result then 
              S'Length = 4 and
              (for all I in S'Range => S(I) >= '0' and S(I) <= '9'));
   
   -- Location validation result record
   type Location_Result is record
      Valid : Boolean;
      Location : MemoryStore.Location_Index;
   end record;
   
   -- Pure function for SPARK compatibility (no side effects)
   function Get_Valid_Location(Input_Line : Lines.MyString;
                              Tokens : MyStringTokeniser.TokenArray; 
                              Token_Index : Positive) return Location_Result with
     Pre => Token_Index <= Tokens'Last and
            Token_Index >= Tokens'First and
            Tokens(Token_Index).Start >= 1 and
            Tokens(Token_Index).Length >= 0 and
            Tokens(Token_Index).Start + Tokens(Token_Index).Length - 1 <= Lines.Length(Input_Line) and
            Tokens(Token_Index).Start + Tokens(Token_Index).Length - 1 >= Tokens(Token_Index).Start;
   
   -- Arithmetic operations
   procedure Handle_Addition with
     Pre => Stack_Top >= 2;
   
   procedure Handle_Subtraction with
     Pre => Stack_Top >= 2;
   
   procedure Handle_Multiplication with
     Pre => Stack_Top >= 2;
   
   procedure Handle_Division with
     Pre => Stack_Top >= 2;
   
   -- Command handlers
   procedure Handle_Unlock_Command(Input_Line : Lines.MyString;
                                 Tokens : MyStringTokeniser.TokenArray; 
                                 Num_Tokens : Natural) with
     Pre => Num_Tokens >= 1 and Num_Tokens <= Tokens'Last;
   
   procedure Handle_Lock_Command(Input_Line : Lines.MyString;
                               Tokens : MyStringTokeniser.TokenArray; 
                               Num_Tokens : Natural) with
     Pre => Num_Tokens >= 1 and Num_Tokens <= Tokens'Last;
   
   procedure Handle_Push1_Command(Input_Line : Lines.MyString;
                                Tokens : MyStringTokeniser.TokenArray; 
                                Num_Tokens : Natural) with
     Pre => Num_Tokens >= 1 and Num_Tokens <= Tokens'Last;
   
   procedure Handle_Push2_Command(Input_Line : Lines.MyString;
                                Tokens : MyStringTokeniser.TokenArray; 
                                Num_Tokens : Natural) with
     Pre => Num_Tokens >= 1 and Num_Tokens <= Tokens'Last;
   
   procedure Handle_StoreTo_Command(Input_Line : Lines.MyString;
                                  Tokens : MyStringTokeniser.TokenArray; 
                                  Num_Tokens : Natural) with
     Pre => Num_Tokens >= 1 and Num_Tokens <= Tokens'Last;
   
   procedure Handle_LoadFrom_Command(Input_Line : Lines.MyString;
                                   Tokens : MyStringTokeniser.TokenArray; 
                                   Num_Tokens : Natural) with
     Pre => Num_Tokens >= 1 and Num_Tokens <= Tokens'Last;
   
   procedure Handle_Remove_Command(Input_Line : Lines.MyString;
                                 Tokens : MyStringTokeniser.TokenArray; 
                                 Num_Tokens : Natural) with
     Pre => Num_Tokens >= 1 and Num_Tokens <= Tokens'Last;
   
   -- Main command processor
   procedure Process_Command_Internal(Input_Line : Lines.MyString);
end Calculator;
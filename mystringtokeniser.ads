with Ada.Characters.Latin_1;

package MyStringTokeniser with SPARK_Mode is

   type TokenExtent is record
      Start : Positive;
      Length : Natural;
   end record;

   type TokenArray is array(Positive range <>) of TokenExtent;

   function Is_Whitespace(Ch : Character) return Boolean is
     (Ch = ' ' or Ch = Ada.Characters.Latin_1.LF or
        Ch = Ada.Characters.Latin_1.HT);

   procedure Tokenise(S : in String; Tokens : in out TokenArray; Count : out Natural) with
     Pre => (if S'Length > 0 then S'First <= S'Last) and Tokens'First <= Tokens'Last,
     Post => Count <= Tokens'Length and
     (for all Index in Tokens'First..Tokens'First+(Count-1) =>
          (Tokens(Index).Start >= S'First and
          Tokens(Index).Length > 0) and then
            Tokens(Index).Length-1 <= S'Last - Tokens(Index).Start);

end MyStringTokeniser;
-- Postcondition:
  -- 1. Count <= Tokens'Length
  --    Meaning: The actual number of detected tokens (Count) will never be greater than the capacity of the Tokens array.
  --    Why necessary: This prevents writing beyond the array bounds, ensuring memory safety and avoiding buffer overflows.
  --
  -- 2. For all Index in Tokens'First..Tokens'First+(Count-1):
  --    (Tokens(Index).Start >= S'First)
  --    Meaning: The start position of every token is always within the valid range of the input string.
  --    Why necessary: Ensures that every token refers to a valid position in S.
  --
  --    (Tokens(Index).Length > 0)
  --    Meaning: Each token has a length greater than zero; empty tokens are not allowed.
  --    Why necessary: Guarantees that no meaningless or empty tokens are produced, simplifying further processing logic.
  --
  --    (Tokens(Index).Length-1 <= S'Last - Tokens(Index).Start)
  --    Meaning: The range of each token (from Start to Start+Length-1) does not exceed the bounds of the string S.
  --    Why necessary: Prevents buffer overrun and ensures that all tokens are completely within the input string. 

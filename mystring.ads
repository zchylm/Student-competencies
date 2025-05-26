--  MyString.ads
--
--  This package defines a SPARK-compatible abstract data type for strings.
--  It avoids dynamic-length types, which are not verifiable in SPARK.
--
--  Use MyString instead of String to benefit from:
--    - Fixed-size, memory-safe storage
--    - Contracts that support formal verification
--    - Compatibility with SPARK_Mode
--
--  Do not modify this file.

generic
   Max_MyString_Length : Positive;


package MyString with SPARK_Mode is
   type MyString is private;

   function To_String(M : MyString) return String with
     Post => To_String'Result'Length = Length(M) and
             To_String'Result'First = 1 and To_String'Result'Last = Length(M);

   function From_String(S : String) return MyString with
     Pre => (S'Length <= Max_MyString_Length),
     Post => (Length(From_String'Result) = S'Length);

   function Less(M1 : MyString; M2 : MyString) return Boolean;

   function Equal(M1 : MyString; M2 : MyString) return Boolean with
     Post => (if Equal'Result then
          Length(M1) = Length(M2) and
        (for all I in 1..Length(M1) => Get(M1,I) = Get(M2,I)));

   function Length(M : MyString) return Natural with
     Post => Length'Result <= Max_MyString_Length;

   function Substring(M : MyString; From : Positive; To : Positive) return MyString with
     Pre => From <= To and To <= Length(M),
     Post => Length(Substring'Result) = To - From + 1;

   function Get(M : MyString; Index : Positive) return Character with
     Pre => Index <= Length(M);

   procedure Get_Line(M : out MyString);

private
   type MyStringStr is array(Positive range 1..Max_MyString_Length) of Character;
   type MyString is record
      Length : Natural range 0..Max_MyString_Length;
      Str    : MyStringStr;
   end record;

   function Length(M : MyString) return Natural is
     (M.Length);

   function Get(M : MyString; Index : Positive) return Character is
     (M.Str(Index));

end MyString;

with Interfaces;  -- supplies Integer_32 type

package MemoryStore with SPARK_Mode is

   Max_Locations : constant Positive := 256;

   --  Index of a memory cell (1 .. 256).
   subtype Location_Index is Positive range 1 .. Max_Locations;

   --  The calculator specification allows "any 32-bit signed integer".
   subtype Int32 is Interfaces.Integer_32;  -- range   -2_147_483_648 ..  2_147_483_647

   type Database is private;

   --------------------------------------------------------------------
   --  Basic operations
   --------------------------------------------------------------------
   procedure Init   (D : out Database);

   function  Has    (D : Database; Loc : Location_Index) return Boolean;

   function  Get    (D : Database; Loc : Location_Index) return Int32
     with Pre => Has (D, Loc);

   procedure Put    (D : in out Database;
                     Loc : in     Location_Index;
                     Val : in     Int32);

   procedure Remove (D : in out Database; Loc : in Location_Index);

   function  Length (D : Database) return Natural;
   --  returns number of *defined* cells (0 .. Max_Locations)

   procedure Print  (D : Database);
   --  writes "loc => value" lines for each defined cell, in ascending order

private
   --------------------------------------------------------------------
   --  Internal representation: fixed array of option cells
   --------------------------------------------------------------------
   type Cell_Opt is record
      Valid : Boolean := False;
      Val   : Int32   := 0;
   end record;

   type Mem_Array is array (Location_Index) of Cell_Opt;

   type Database is record
      Mem : Mem_Array;
      Cnt : Natural := 0;
   end record;
end MemoryStore;

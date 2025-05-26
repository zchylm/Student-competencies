with Ada.Text_IO;
with Ada.Integer_Text_IO;

package body MemoryStore is

   ---------------------------------------------------------------------------
   --  Initialisation
   ---------------------------------------------------------------------------
   procedure Init (D : out Database) is
   begin
      --  All cells already have Valid = False by default; just zero counters.
      for Loc in Location_Index loop
         D.Mem (Loc).Valid := False;
         D.Mem (Loc).Val   := 0;
      end loop;
      D.Cnt := 0;
   end Init;

   ---------------------------------------------------------------------------
   --  Query helpers
   ---------------------------------------------------------------------------
   function Has (D : Database; Loc : Location_Index) return Boolean is
     (D.Mem (Loc).Valid);

   function Get (D : Database; Loc : Location_Index) return Int32 is
     (D.Mem (Loc).Val);

   function Length (D : Database) return Natural is
     (D.Cnt);

   ---------------------------------------------------------------------------
   --  Update operations
   ---------------------------------------------------------------------------
   procedure Put
     (D   : in out Database;
      Loc : in     Location_Index;
      Val : in     Int32) is
   begin
      if not D.Mem (Loc).Valid then
         -- inserting a brand-new entry
         D.Cnt := D.Cnt + 1;
      end if;
      D.Mem (Loc).Valid := True;
      D.Mem (Loc).Val   := Val;
   end Put;

   procedure Remove
     (D   : in out Database;
      Loc : in     Location_Index) is
   begin
      if D.Mem (Loc).Valid then
         D.Mem (Loc).Valid := False;
         D.Cnt             := D.Cnt - 1;
      end if;
   end Remove;

   ---------------------------------------------------------------------------
   --  Pretty-print for the "list" command
   ---------------------------------------------------------------------------
   procedure Print (D : Database) is
   begin
      for Loc in Location_Index loop
         if D.Mem (Loc).Valid then
            Ada.Text_IO.Put ("   ");
            Ada.Integer_Text_IO.Put (Loc, Width => 0);
            Ada.Text_IO.Put (" => ");
            Ada.Integer_Text_IO.Put (Integer (D.Mem (Loc).Val), Width => 0);
            Ada.Text_IO.New_Line;
         end if;
      end loop;
   end Print;

end MemoryStore;

--  MyCommandLine.adb
--
--  This file implements a wrapper around Ada.Command_Line to provide
--  command-line argument access. We intentionally do not enable SPARK_Mode
--  here because Ada.Command_Line is not analyzable by the SPARK Prover.
--
--  This file should not be modified.

with Ada.Command_Line;

package body MyCommandLine is
   -- note we do not want SPARK to analyse this implementation since
   -- Ada.Command_Line is not able to be handled by the SPARK Prover

   function Command_Name return String is
   begin
      return Ada.Command_Line.Command_Name;
   end Command_Name;

   function Argument_Count return Natural is
   begin
      return Ada.Command_Line.Argument_Count;
   end Argument_Count;

   function Argument(Number : in Positive) return String is
   begin
      return Ada.Command_Line.Argument(Number);
   end Argument;



end MyCommandLine;

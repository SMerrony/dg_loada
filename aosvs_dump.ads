-- Copyright 2021 S.Merrony

-- Permission is hereby granted, free of charge, to any person obtaining a copy of this software
-- and associated documentation files (the "Software"), to deal in the Software without restriction,
-- including without limitation the rights to use, copy, modify, merge, publish, distribute, sublicense,
-- and/or sell copies of the Software, and to permit persons to whom the Software is furnished to do so,
-- subject to the following conditions:

-- The above copyright notice and this permission notice shall be included in all copies or substantial
-- portions of the Software.

-- THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR IMPLIED, INCLUDING BUT NOT
-- LIMITED TO THE WARRANTIES OF MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT.
-- IN NO EVENT SHALL THE AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER LIABILITY,
-- WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM, OUT OF OR IN CONNECTION WITH THE
-- SOFTWARE OR THE USE OR OTHER DEALINGS IN THE SOFTWARE.

with Ada.Streams.Stream_IO; use Ada.Streams.Stream_IO;
with Ada.Strings.Unbounded; use Ada.Strings.Unbounded;
with Interfaces;            use Interfaces;

package Aosvs_Dump is

   Start_Dump_Byte : constant Unsigned_8 := 0;
   FSB_Byte        : constant Unsigned_8 := 1;
   Name_Block_Byte : constant Unsigned_8 := 2;
   UDA_Byte        : constant Unsigned_8 := 3;
   ACL_Byte        : constant Unsigned_8 := 4;
   Link_Byte       : constant Unsigned_8 := 5;
   Data_Start_Byte : constant Unsigned_8 := 6;
   Data_Block_Byte : constant Unsigned_8 := 7;
   Data_End_Byte   : constant Unsigned_8 := 8;
   End_Dump_Byte   : constant Unsigned_8 := 9;

   MaxBlockSize : constant Integer := 32_768;

   type Fstat_Entry_Rec is record
      DgMnemonic : String (1 .. 4);
      Desc       : Unbounded_String;
      IsDir      : Boolean;
      HasPayload : Boolean;
   end record;

   type Known_Types is array (0 .. 87) of Fstat_Entry_Rec;

   Known_Fstat_Entry_Types : constant Known_Types :=
     (0      => ("FLNK", To_Unbounded_String ("=>Link=>"), False, False),
      1 => ("FDSF", To_Unbounded_String ("System Data File"), False, True),
      2      => ("FMTF", To_Unbounded_String ("Mag Tape File"), False, True),
      3      => ("FGFN", To_Unbounded_String ("Generic File"), False, True),
      10     => ("FDIR", To_Unbounded_String ("<Directory>"), True, False),
      11     => ("FLDU", To_Unbounded_String ("<LDU Directory>"), True, False),
      12 => ("FCPD", To_Unbounded_String ("<Control Point Dir>"), True, False),
      64     => ("FUDF", To_Unbounded_String ("User Data File"), False, True),
      66     => ("FUPD", To_Unbounded_String ("User Profile"), False, True),
      67     => ("FSTF", To_Unbounded_String ("Symbol Table"), False, True),
      68     => ("FTXT", To_Unbounded_String ("Text File"), False, True),
      69     => ("FLOG", To_Unbounded_String ("System Log File"), False, True),
      74     => ("FPRV", To_Unbounded_String ("Program File"), False, True),
      87     => ("FPRG", To_Unbounded_String ("Program File"), False, True),
      others => ("UNKN", To_Unbounded_String ("Unknown"), False, False));

   type Record_Header_Type is record
      RecordType   : Unsigned_8;
      RecordLength : Natural;
   end record;

   type Data_Header_Type is record
      DataHeader     : Record_Header_Type;
      ByteAddress    : Unsigned_32;
      ByteLength     : Unsigned_32;
      AlignmentCount : Unsigned_16;
   end record;

   type SOD_Type is record
      Header             : Record_Header_Type;
      DumpFormatRevision : Unsigned_16;
      DumpTimeSecs       : Unsigned_16;
      DumpTimeMins       : Unsigned_16;
      DumpTimeHours      : Unsigned_16;
      DumpTimeDay        : Unsigned_16;
      DumpTimeMonth      : Unsigned_16;
      DumpTimeYear       : Unsigned_16;
   end record;

   type Blob_Type is array (Positive range <>) of Unsigned_8;

   function Read_Word (Dump_Stream : Stream_Access) return Unsigned_16;
   function Read_Blob
     (Num_Bytes : in Positive; Dump_Stream : Stream_Access;
      Reason    : in Unbounded_String) return Blob_Type;
   function Read_Header
     (Dump_Stream : Stream_Access) return Record_Header_Type;
   function Read_SOD (Dump_Stream : Stream_Access) return SOD_Type;

end Aosvs_Dump;

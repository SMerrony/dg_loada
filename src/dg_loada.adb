--  Copyright 2021,2022 S.Merrony

--  Permission is hereby granted, free of charge, to any person obtaining a copy of this software
--  and associated documentation files (the "Software"), to deal in the Software without restriction,
--  including without limitation the rights to use, copy, modify, merge, publish, distribute, sublicense,
--  and/or sell copies of the Software, and to permit persons to whom the Software is furnished to do so,
--  subject to the following conditions:

--  The above copyright notice and this permission notice shall be included in all copies or substantial
--  portions of the Software.

--  THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR IMPLIED, INCLUDING BUT NOT
--  LIMITED TO THE WARRANTIES OF MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT.
--  IN NO EVENT SHALL THE AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER LIABILITY,
--  WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM, OUT OF OR IN CONNECTION WITH THE
--  SOFTWARE OR THE USE OR OTHER DEALINGS IN THE SOFTWARE.

with Ada.Command_Line;        use Ada.Command_Line;
with Ada.Directories;         use Ada.Directories;
with Ada.Exceptions;
with Ada.Streams.Stream_IO;   use Ada.Streams.Stream_IO;
with Ada.Strings.Unbounded;   use Ada.Strings.Unbounded;
with Ada.Task_Identification; use Ada.Task_Identification;
with Ada.Text_IO;

with GNAT.Traceback.Symbolic;

with Interfaces; use Interfaces;

with Aosvs_Dump; use Aosvs_Dump;

procedure Dg_Loada is

   SemVer : constant String := "1.4.3"; --  TODO Update SemVer on each release

   Dump_File_Name  : Unbounded_String;
   Extracting      : Boolean := False;
   Ignoring_Errors : Boolean := False;
   Listing         : Boolean := False;
   Summary         : Boolean := True;
   Verbose         : Boolean := False;

   ArgIx             : Integer := 1;
   Dump_File         : File_Type;
   Dump_File_Stream  : Stream_Access;
   Write_File        : File_Type;

   --  dump images can legally contain 'too many' directory pops, so we
   --  store the starting directory and never traverse above it...
   Base_Dir    : constant String  := Current_Directory;
   Working_Dir : Unbounded_String := To_Unbounded_String (Base_Dir);

   Buffer              : array (1 .. MaxBlockSize) of Unsigned_8;
   FSB_Type_Indicator  : Integer;
   Current_File_Name   : Unbounded_String;

   SOD                           : SOD_Type;
   Record_Header                 : Record_Header_Type;
   Total_File_Size, Padding_Size : Unsigned_32 := 0;
   Done, In_A_File, Load_It      : Boolean     := False;
   File_Count                    : Natural     := 0;

   Cannot_Create_Link : exception;

   function symlink (fname, linkname : String) return Integer;
   pragma Import (C, symlink);

   procedure Print_Help is
   begin
      Ada.Text_IO.Put_Line ("Usage of dg_loada:");
      Ada.Text_IO.Put_Line ("  -dumpfile <file>  DUMP_II or DUMP_III file to read/load (required)");
      Ada.Text_IO.Put_Line ("  -extract          extract the files from the DUMP_II/III into the current directory");
      Ada.Text_IO.Put_Line ("  -ignoreErrors     do not exit if a file or link cannot be created");
      Ada.Text_IO.Put_Line ("  -list             list the contents of the DUMP_II/III file");
      Ada.Text_IO.Put_Line ("  -summary          concise summary of the DUMP_II/III file contents (default true)");
      Ada.Text_IO.Put_Line ("  -verbose          be rather wordy about what dg_loada is doing");
      Ada.Text_IO.Put_Line ("  -version          show the version number of dg_loada and exit");

      Set_Exit_Status (Failure);
   end Print_Help;

   procedure Load_Buffer (Num_Bytes : Integer; Reason : String)
   is
      Tmp_Blob : Blob_Type (1 .. Num_Bytes);
   begin
      Tmp_Blob            := Read_Blob (Num_Bytes, Dump_File_Stream, Reason);
      for B in 1 .. Num_Bytes loop
         Buffer (B) := Tmp_Blob (B);
      end loop;
   end Load_Buffer;

   function Process_Name_Block (Record_Header : Record_Header_Type) return Unbounded_String
   is
      Name_Bytes : Blob_Type (1 .. Record_Header.Record_Length);
      File_Name, Write_Path, Display_Path : Unbounded_String;
      This_Entry_Type                     : Fstat_Entry_Rec;
   begin
      Name_Bytes :=
        Read_Blob (Record_Header.Record_Length, Dump_File_Stream, "File Name");
      File_Name := Extract_First_String (Name_Bytes);
      File_Name := To_Linux_Filename (File_Name);
      if Summary and Verbose then
         Ada.Text_IO.Put_Line ("");
      end if;
      This_Entry_Type := Known_Fstat_Entry_Types (FSB_Type_Indicator);

      Load_It := This_Entry_Type.Has_Payload;
      if This_Entry_Type.Is_Dir then
         Working_Dir := Working_Dir & "/" & File_Name;
         if Extracting then
            Create_Directory (To_String (Working_Dir));
         end if;
      end if;

      if Listing then
         if Working_Dir = "" then
            Display_Path := File_Name;
         else
            Display_Path := Working_Dir & "/" & File_Name;
         end if;
         Ada.Text_IO.Put
           (To_String (This_Entry_Type.Desc) & "   " &
              To_String (Display_Path));
         if Verbose or else This_Entry_Type.Is_Dir then
            Ada.Text_IO.Put_Line ("");
         else
            Ada.Text_IO.Put ("   ");
         end if;
      end if;

      File_Count := File_Count + 1;

      if Extracting and Load_It then
         if Working_Dir = "" then
            Write_Path := File_Name;
         else
            Write_Path := Working_Dir & "/" & File_Name;
         end if;
         if Verbose then
            Ada.Text_IO.Put_Line (" Creating file: " & To_String (Write_Path));
         end if;
         Create (Write_File, Out_File, To_String (Write_Path));
         --  Ada.Text_IO.Put_Line ("DEBUG: Output file created" );
      end if;

      return File_Name;
   end Process_Name_Block;

   procedure Process_Data_Block is
      DHB       : Data_Header_Type;
      FourBytes : Blob_Type (1 .. 4);
      TwoBytes  : Blob_Type (1 .. 2);
   begin
      --  first get the address and length
      FourBytes := Read_Blob (4, Dump_File_Stream, "Byte Addr");
      DHB.Byte_Address := Unsigned_32 (FourBytes (1));
      DHB.Byte_Address :=
        Shift_Left (DHB.Byte_Address, 8) + Unsigned_32 (FourBytes (2));
      DHB.Byte_Address :=
        Shift_Left (DHB.Byte_Address, 8) + Unsigned_32 (FourBytes (3));
      DHB.Byte_Address :=
        Shift_Left (DHB.Byte_Address, 8) + Unsigned_32 (FourBytes (4));

      FourBytes := Read_Blob (4, Dump_File_Stream, "Byte Length");
      DHB.Byte_Length := Unsigned_32 (FourBytes (1));
      DHB.Byte_Length :=
        Shift_Left (DHB.Byte_Length, 8) + Unsigned_32 (FourBytes (2));
      DHB.Byte_Length :=
        Shift_Left (DHB.Byte_Length, 8) + Unsigned_32 (FourBytes (3));
      DHB.Byte_Length :=
        Shift_Left (DHB.Byte_Length, 8) + Unsigned_32 (FourBytes (4));

      if DHB.Byte_Length > Unsigned_32 (MaxBlockSize) then
         Ada.Text_IO.Put_Line
           (Ada.Text_IO.Standard_Error,
            "ERROR: Maximum Block Size Exceeded.");
         Set_Exit_Status (Failure);
         Abort_Task (Current_Task);
      end if;

      if Verbose then
         Ada.Text_IO.Put_Line
           (" Data block: " & Unsigned_32'Image (DHB.Byte_Length) &
              " (bytes)");
      end if;

      TwoBytes := Read_Blob (2, Dump_File_Stream, "Alignment Count");
      DHB.Alighnment_Count := Unsigned_16 (TwoBytes (1));
      DHB.Alighnment_Count :=
        Shift_Left (DHB.Alighnment_Count, 8) + Unsigned_16 (TwoBytes (2));

      --  skip any alignment bytes - usually just one
      if DHB.Alighnment_Count /= 0 then
         if Verbose then
            Ada.Text_IO.Put_Line
              ("  Skipping " & Unsigned_16'Image (DHB.Alighnment_Count) &
                 " alignment byte(s)");
         end if;
         declare
            Dummy_Blob : Blob_Type (1 .. Integer (DHB.Alighnment_Count));
         begin
            Dummy_Blob := Read_Blob (Integer (DHB.Alighnment_Count), Dump_File_Stream, "Alignment");
         end;
      end if;

      declare
         Data_Blob : Blob_Type (1 .. Integer (DHB.Byte_Length));
      begin
         Data_Blob := Read_Blob (Integer (DHB.Byte_Length), Dump_File_Stream, "Data Block");

         --  large areas of NULLs may be skipped over by DUMP_II/III
         --  this is achieved by simply advancing the byte address so
         --  we must pad out if byte address is beyond end of last block

         if DHB.Byte_Address > (Total_File_Size + 1) then
            Padding_Size := DHB.Byte_Address - Total_File_Size;
            if Extracting then
               if Verbose then
                  Ada.Text_IO.Put_Line ("  Padding with one block");
               end if;
               declare
                  Padding_Blob : Blob_Type (1 .. Integer (Padding_Size));
               begin
                  for B in Padding_Blob'Range loop
                     Padding_Blob (B) := 0;
                  end loop;
                  Blob_Type'Write (Stream (Write_File), Padding_Blob);
               end;
            end if;
            Total_File_Size := Total_File_Size + Padding_Size;
         end if;

         if Extracting then
            --     Ada.Text_IO.Put_Line("Writing " & Unsigned_32'Image(DHB.Byte_Length) & " bytes...");
            Blob_Type'Write (Stream (Write_File), Data_Blob);
         end if;
      end;

      Total_File_Size := Total_File_Size + DHB.Byte_Length;
      In_A_File       := True;

   end Process_Data_Block;

   procedure Process_End_Block is
   begin
      if Is_Open (Write_File) then
         Close (Write_File);
         if Verbose then
            Ada.Text_IO.Put_Line (" File Closed");
         end if;
      end if;
      if In_A_File then
         if Listing then
            Ada.Text_IO.Put_Line (" " & Unsigned_32'Image (Total_File_Size) & " bytes");
         end if;
         Total_File_Size := 0;
         In_A_File       := False;
      else
         if Working_Dir /= Base_Dir then --  Don't go up from start dir
            declare lastSlash : constant Natural := Ada.Strings.Unbounded.Index (Working_Dir, "/", Ada.Strings.Backward);
            begin
               Working_Dir := Head (Working_Dir, lastSlash - 1);
            end;
            if Verbose then
               Ada.Text_IO.Put_Line (" Popped dir - new dir is: ");
            end if;
         end if;

      end if;
      if Verbose then
         Ada.Text_IO.Put_Line ("End Block Processed");
      end if;
   end Process_End_Block;

   procedure Process_Link (Record_Header : Record_Header_Type; Link_Name : Unbounded_String)
   is
      Link_Target_Blob : Blob_Type (1 .. Record_Header.Record_Length);
      Link_Target      : Unbounded_String;
   begin
      Link_Target_Blob := Read_Blob (Record_Header.Record_Length, Dump_File_Stream, "Link Target");
      Link_Target := Extract_First_String (Link_Target_Blob);
      Link_Target := To_Linux_Filename (Link_Target);
      if Listing or Verbose then
         Ada.Text_IO.Put_Line (" -> Link Target: " & To_String (Link_Target));
      end if;
      if Extracting then
         declare
            RC : Integer;
            Target_Str : constant String := To_String (Link_Target) & ASCII.NUL;
            Link_Str   : constant String := To_String (Working_Dir) & "/" &
              To_String (Link_Name) & ASCII.NUL;
         begin
            RC := symlink (Target_Str, Link_Str);
            if RC /= 0 then
               Ada.Text_IO.Put_Line ("ERROR: Could not create symbolic link");
               if not Ignoring_Errors then
                  raise Cannot_Create_Link;
               end if;
            end if;
         end;
      end if;
   end Process_Link;

   ------------------
   --  main program --
   ------------------
begin
   if Argument_Count = 0 then
      Print_Help;
      return;
   end if;

   while ArgIx <= Argument_Count loop
      if Argument (ArgIx) = "-dumpfile" then
         Dump_File_Name := To_Unbounded_String (Argument (ArgIx + 1));
         ArgIx          := ArgIx + 1;
      elsif Argument (ArgIx) = "-extract" then
         Extracting := True;
      elsif Argument (ArgIx) = "-ignoreErrors" then
         Ignoring_Errors := True;
      elsif Argument (ArgIx) = "-list" then
         Listing := True;
      elsif Argument (ArgIx) = "-summary" then
         Summary := True;
      elsif Argument (ArgIx) = "-verbose" then
         Verbose := True;
      elsif Argument (ArgIx) = "-version" then
         Ada.Text_IO.Put ("dg_loada version " & SemVer);
         return;
      else
         Ada.Text_IO.Put_Line
           (Ada.Text_IO.Standard_Error, "ERROR: Invalid option specified");
         Print_Help;
         return;
      end if;
      ArgIx := ArgIx + 1;
   end loop;

   begin
      Open
        (File => Dump_File, Mode => In_File,
         Name => To_String (Dump_File_Name));
   exception
      when others =>
         Ada.Text_IO.Put_Line
           (Ada.Text_IO.Standard_Error,
            "ERROR: Cannot open the file '" & To_String (Dump_File_Name) &
              "'. Does it exist?");
         Set_Exit_Status (Failure);
         return;
   end;

   Dump_File_Stream := Stream (Dump_File);

   --  There should always be a Start Of Dump record
   SOD := Read_SOD (Dump_File_Stream);
   if Summary or Verbose then
      Ada.Text_IO.Put_Line
        ("Summary of dump file : " & To_String (Dump_File_Name));
      Ada.Text_IO.Put_Line
        ("AOS/VS dump version  : " &
           Unsigned_16'Image (SOD.Dump_Format_Version));
      Ada.Text_IO.Put_Line
        ("Dump date (y - m - d): " &
           Unsigned_16'Image (SOD.Dump_Time_Year) & " -" &
           Unsigned_16'Image (SOD.Dump_Time_Month) & " -" &
           Unsigned_16'Image (SOD.Dump_Time_Day));
      Ada.Text_IO.Put_Line
        ("Dump time (h : m : s): " &
           Unsigned_16'Image (SOD.Dump_Time_Hours) & " :" &
           Unsigned_16'Image (SOD.Dump_Time_Mins) & " :" &
           Unsigned_16'Image (SOD.Dump_Time_Secs));
   end if;

   Process_Each_Block :
   while not Done loop
      Record_Header := Read_Header (Dump_File_Stream);
      if Verbose then
         Ada.Text_IO.Put_Line
           ("Found block of type: " &
              Unsigned_8'Image (Record_Header.Record_Type) & ", Length: " &
              Integer'Image (Record_Header.Record_Length));
      end if;
      case Record_Header.Record_Type is
         when Start_Dump_Byte =>
            Ada.Text_IO.Put_Line
              (Ada.Text_IO.Standard_Error,
               "ERROR: Found another Start-Of-Dump inside dump - this should not happen.");
            Set_Exit_Status (Failure);
            Abort_Task (Current_Task);
         when FSB_Byte =>
            Load_Buffer (Record_Header.Record_Length, "FSB");
            FSB_Type_Indicator := Integer (Buffer (2));
            Load_It            := False;
         when Name_Block_Byte =>
            Current_File_Name := Process_Name_Block (Record_Header);
         when UDA_Byte =>
            --  throw away for now
            Load_Buffer (Record_Header.Record_Length, "UDA"); --  TODO Check this is OK
         when ACL_Byte =>
            --  We don't do anything except report ACLs at the moment
            Load_Buffer (Record_Header.Record_Length, "ACL");
            if Verbose then
               Ada.Text_IO.Put_Line
                 (" ACL: "); --  & Unsigned_8'Image(Buffer));
            end if;
         when Link_Byte =>
            Process_Link (Record_Header, Current_File_Name);
         when Data_Start_Byte =>
            --  nothing to do - it's just a record header
            null;
         when Data_Block_Byte =>
            Process_Data_Block;
         when Data_End_Byte =>
            Process_End_Block;
         when End_Dump_Byte =>
            if Summary then
               Ada.Text_IO.Put_Line (File_Count'Image & " objects in dump file");
            end if;
            Ada.Text_IO.Put_Line ("=== End of Dump ===");
            Done := True;
         when others =>
            Ada.Text_IO.Put_Line
              (Ada.Text_IO.Standard_Error,
               "ERROR: Unknown block type: " &
                 Unsigned_8'Image (Record_Header.Record_Type) &
                 " Giving up.");
            Set_Exit_Status (Failure);
            Abort_Task (Current_Task);
      end case;
   end loop Process_Each_Block;

exception
   when E : others =>
      Ada.Text_IO.Put (Ada.Text_IO.Standard_Error, "Unexpected Error: ");
      Ada.Text_IO.Put_Line (Ada.Text_IO.Standard_Error, Ada.Exceptions.Exception_Message (E));
      Ada.Text_IO.Put_Line (Ada.Text_IO.Standard_Error, GNAT.Traceback.Symbolic.Symbolic_Traceback (E));
      raise;

end Dg_Loada;

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

with Ada.Command_Line;        use Ada.Command_Line;
with Ada.Directories;         use Ada.Directories;
with Ada.Exceptions;
with Ada.Streams.Stream_IO;   use Ada.Streams.Stream_IO;
with Ada.Strings.Unbounded;   use Ada.Strings.Unbounded;
with Ada.Task_Identification; use Ada.Task_Identification;
with Ada.Text_IO;

with Interfaces; use Interfaces;

with Aosvs_Dump; use Aosvs_Dump;

procedure Loada is

    SemVer : constant String := "v1.4.1";

    DumpFileName   : Unbounded_String;
    Extracting     : Boolean := False;
    IgnoringErrors : Boolean := False;
    Listing        : Boolean := False;
    Summary        : Boolean := False;
    Verbose        : Boolean := False;

    ArgIx          : Integer := 1;
    DumpFile       : File_Type;
    DumpFileStream : Stream_Access;

    -- dump images can legally contain 'too many' directory pops, so we
    -- store the starting directory and never traverse above it...
    BaseDir    : constant String  := Current_Directory;
    WorkingDir : Unbounded_String := To_Unbounded_String (BaseDir);

    Buffer           : array (1 .. MaxBlockSize) of Unsigned_8;
    CurrentBufferLen : Integer range 0 .. MaxBlockSize;
    CurrentBufferUse : Unbounded_String;
    FsbTypeIndicator : Integer;
    FileName         : Unbounded_String;

    SodRecord                  : SOD_Type;
    RecHdr                     : Record_Header_Type;
    TotalFileSize, PaddingSize : Unsigned_32 := 0;
    Done, InFile, LoadIt       : Boolean     := False;

    procedure Print_Help is
    begin
        Ada.Text_IO.Put
           ("ERROR: Must specify DUMP file name with -dumpFile <dumpfilename> option");
        Set_Exit_Status (Failure);
    end Print_Help;

    procedure Load_Buffer
       (Num_Bytes : in Integer; Reason : in Unbounded_String)
    is
        Tmp_Blob : Blob_Type (1 .. Num_Bytes);
    begin
        CurrentBufferUse := Reason;
        Tmp_Blob         := Read_Blob (Num_Bytes, DumpFileStream, Reason);
        for B in 1 .. Num_Bytes loop
            Buffer (B) := Tmp_Blob (B);
        end loop;
        CurrentBufferLen := Num_Bytes;
    end Load_Buffer;

    function ProcessNB (RecHdr : in Record_Header_Type) return Unbounded_String
    is
        NameBytes : Blob_Type (1 .. RecHdr.RecordLength);
        File_Name, WritePath, DisplayPath : Unbounded_String;
        ThisEntryType                     : Fstat_Entry_Rec;
    begin
        NameBytes :=
           Read_Blob
              (RecHdr.RecordLength, DumpFileStream,
               To_Unbounded_String ("File Name"));
        for Ix in 1 .. RecHdr.RecordLength loop
            exit when NameBytes (Ix) = 0;
            Append (File_Name, Character'Val (NameBytes (Ix)));
        end loop;
        if Summary and Verbose then
            Ada.Text_IO.Put_Line ("");
        end if;
        ThisEntryType := Known_Fstat_Entry_Types (FsbTypeIndicator);
        if ThisEntryType.Desc /= "UNKN" then
            LoadIt := ThisEntryType.HasPayload;
            if ThisEntryType.IsDir then
                WorkingDir := WorkingDir & "/" & File_Name;
                if Extracting then
                    Create_Directory (To_String (WorkingDir));
                end if;
            end if;
        else
            LoadIt := True;
        end if;

        if Summary then
            if WorkingDir = "" then
                DisplayPath := File_Name;
            else
                DisplayPath := WorkingDir & "/" & File_Name;
            end if;
            Ada.Text_IO.Put
               (To_String (ThisEntryType.Desc) & "   " &
                To_String (DisplayPath));
            if Verbose or ThisEntryType.IsDir then
                Ada.Text_IO.Put_Line ("");
            else
                Ada.Text_IO.Put ("   ");
            end if;
        end if;

        if Extracting and LoadIt then
            if WorkingDir = "" then
                WritePath := File_Name;
            else
                WritePath := WorkingDir & "/" & File_Name;
            end if;
            if Verbose then
                Ada.Text_IO.Put (" Creating file: " & To_String (WritePath));
            end if;
            -- TODO Actually create the file!
        end if;

        return File_Name;
    end ProcessNB;

    procedure ProcessDataBlock (RecHdr : in Record_Header_Type) is
        DHB       : Data_Header_Type;
        FourBytes : Blob_Type (1 .. 4);
        TwoBytes  : Blob_Type (1 .. 2);
        DataBlob  : Blob_Type (1 .. Aosvs_Dump.MaxBlockSize);
    begin
        -- first get the address and length
        FourBytes :=
           Read_Blob
              (Num_Bytes => 4, Dump_Stream => DumpFileStream,
               Reason    => To_Unbounded_String ("Byte Addr"));
        DHB.ByteAddress := Unsigned_32 (FourBytes (1));
        DHB.ByteAddress :=
           Shift_Left (DHB.ByteAddress, 8) + Unsigned_32 (FourBytes (2));
        DHB.ByteAddress :=
           Shift_Left (DHB.ByteAddress, 8) + Unsigned_32 (FourBytes (3));
        DHB.ByteAddress :=
           Shift_Left (DHB.ByteAddress, 8) + Unsigned_32 (FourBytes (4));

        FourBytes :=
           Read_Blob
              (Num_Bytes => 4, Dump_Stream => DumpFileStream,
               Reason    => To_Unbounded_String ("Byte Length"));
        DHB.ByteLength := Unsigned_32 (FourBytes (1));
        DHB.ByteLength :=
           Shift_Left (DHB.ByteLength, 8) + Unsigned_32 (FourBytes (2));
        DHB.ByteLength :=
           Shift_Left (DHB.ByteLength, 8) + Unsigned_32 (FourBytes (3));
        DHB.ByteLength :=
           Shift_Left (DHB.ByteLength, 8) + Unsigned_32 (FourBytes (4));

        if DHB.ByteLength > Unsigned_32 (MaxBlockSize) then
            Ada.Text_IO.Put_Line
               (Ada.Text_IO.Standard_Error,
                "ERROR: Maximum Block Size Exceeded.");
            Set_Exit_Status (Failure);
            Abort_Task (Current_Task);
        end if;

        if Verbose then
            Ada.Text_IO.Put_Line
               (" Data block: " & Unsigned_32'Image (DHB.ByteLength) &
                " (bytes)");
        end if;

        TwoBytes :=
           Read_Blob
              (Num_Bytes => 2, Dump_Stream => DumpFileStream,
               Reason    => To_Unbounded_String ("Alignment Count"));
        DHB.AlignmentCount := Unsigned_16 (TwoBytes (1));
        DHB.AlignmentCount :=
           Shift_Left (DHB.AlignmentCount, 8) + Unsigned_16 (TwoBytes (2));

        -- skip any alignment bytes - usually just one
        if DHB.AlignmentCount /= 0 then
            if Verbose then
                Ada.Text_IO.Put_Line
                   ("  Skipping " & Unsigned_16'Image (DHB.AlignmentCount) &
                    " alignment byte(s)");
            end if;
            declare
                t : Blob_Type (1 .. Integer (DHB.AlignmentCount));
            begin
                --t := Read_Blob (Num_Bytes => , Dump_Stream => DumpFileStream, Reason => To_Unbounded_String("Alignment"));
                t :=
                   Read_Blob
                      (Num_Bytes   => Integer (DHB.AlignmentCount),
                       Dump_Stream => DumpFileStream,
                       Reason      => To_Unbounded_String ("Alignment"));
            end;
        end if;

        declare
            db : Blob_Type (1 .. Integer (DHB.ByteLength));
        begin
            db :=
               Read_Blob
                  (Num_Bytes   => Integer (DHB.ByteLength),
                   Dump_Stream => DumpFileStream,
                   Reason      => To_Unbounded_String ("Data Block"));
        end;

        -- large areas of NULLs may be skipped over by DUMP_II/III
        -- this is achieved by simply advancing the byte address so
        -- we must pad out if byte address is beyond end of last block

        if DHB.ByteAddress > (TotalFileSize + 1) then
            PaddingSize := DHB.ByteAddress - TotalFileSize;
            if Extracting then
                if Verbose then
                    Ada.Text_IO.Put_Line ("  Padding with one block");
                end if;
                -- TODO Write the padding block
            end if;
            TotalFileSize := TotalFileSize + PaddingSize;
        end if;

        if Extracting then
            -- TODO Write out the Blob_Type
            null;
        end if;

        TotalFileSize := TotalFileSize + DHB.ByteLength;
        InFile        := True;

    end ProcessDataBlock;

    procedure ProcessEndBlock is
    begin
        if InFile then
            if Extracting and LoadIt then
                -- TODO Close the file
                null;
            end if;
            if Summary then
                Ada.Text_IO.Put_Line
                   (" " & Unsigned_32'Image (TotalFileSize) & " bytes");
            end if;
            TotalFileSize := 0;
            InFile        := False;
        else
            if WorkingDir /= BaseDir then -- Don't go up from start dir
                declare
                    lastSlash : Natural :=
                       Ada.Strings.Unbounded.Index
                          (WorkingDir, "/", Ada.Strings.Backward);
                begin
                    WorkingDir := Head (WorkingDir, lastSlash - 1);
                end;
                if Verbose then
                    Ada.Text_IO.Put_Line (" Popped dir - new dir is: ");
                end if;
            end if;

        end if;
        if Verbose then
            Ada.Text_IO.Put_Line ("End Block Processed");
        end if;
    end ProcessEndBlock;

    procedure Process_Link
       (RecHdr : in Record_Header_Type; LinkName : in Unbounded_String)
    is
        LinkTargetBA : Blob_Type (1 .. RecHdr.RecordLength);
    begin
        LinkTargetBA :=
           Read_Blob
              (RecHdr.RecordLength, DumpFileStream,
               To_Unbounded_String ("Link Target"));
        -- TODO Create the link
    end Process_Link;

begin
    if Argument_Count = 0 then
        Print_Help;
        return;
    end if;

    while ArgIx <= Argument_Count loop
        if Argument (ArgIx) = "-dumpfile" then
            DumpFileName := To_Unbounded_String (Argument (ArgIx + 1));
            ArgIx        := ArgIx + 1;
        elsif Argument (ArgIx) = "-extract" then
            Extracting := True;
        elsif Argument (ArgIx) = "-ignoreErrors" then
            IgnoringErrors := True;
        elsif Argument (ArgIx) = "-list" then
            Listing := True;
        elsif Argument (ArgIx) = "-summary" then
            Summary := True;
        elsif Argument (ArgIx) = "-verbose" then
            Verbose := True;
        elsif Argument (ArgIx) = "-version" then
            Ada.Text_IO.Put ("loada version " & SemVer);
            return;
        else
            Ada.Text_IO.Put_Line
               ("Ada.Text_IO.Standard_Error, ERROR: Invalid option specified");
            return;
        end if;
        ArgIx := ArgIx + 1;
    end loop;

    begin
        Open
           (File => DumpFile, Mode => In_File,
            Name => To_String (DumpFileName));
    exception
        when others =>
            Ada.Text_IO.Put_Line
               (Ada.Text_IO.Standard_Error,
                "ERROR: Cannot open the file '" & To_String (DumpFileName) &
                "'. Does it exist?");
            Set_Exit_Status (Failure);
            return;
    end;

    DumpFileStream := Stream (DumpFile);

    -- There should always be a Start Of Dump record
    SodRecord := Read_SOD (DumpFileStream);
    if Summary or Verbose then
        Ada.Text_IO.Put_Line
           ("Summary of dump file : " & To_String (DumpFileName));
        Ada.Text_IO.Put_Line
           ("AOS/VS dump version  : " &
            Unsigned_16'Image (SodRecord.DumpFormatRevision));
        Ada.Text_IO.Put_Line
           ("Dump date (y - m - d): " &
            Unsigned_16'Image (SodRecord.DumpTimeYear) & " -" &
            Unsigned_16'Image (SodRecord.DumpTimeMonth) & " -" &
            Unsigned_16'Image (SodRecord.DumpTimeDay));
        Ada.Text_IO.Put_Line
           ("Dump time (h : m : s): " &
            Unsigned_16'Image (SodRecord.DumpTimeHours) & " :" &
            Unsigned_16'Image (SodRecord.DumpTimeMins) & " :" &
            Unsigned_16'Image (SodRecord.DumpTimeSecs));
    end if;

    Process_Each_Block :
    while not Done loop
        RecHdr := Read_Header (DumpFileStream);
        if Verbose then
            Ada.Text_IO.Put_Line
               ("Found block of type: " &
                Unsigned_8'Image (RecHdr.RecordType) & ", Length: " &
                Integer'Image (RecHdr.RecordLength));
        end if;
        case RecHdr.RecordType is
            when Start_Dump_Byte =>
                Ada.Text_IO.Put_Line
                   (Ada.Text_IO.Standard_Error,
                    "ERROR: Found another Start-Of-Dump inside dump - this should not happen.");
                Set_Exit_Status (Failure);
                Abort_Task (Current_Task);
            when FSB_Byte =>
                Load_Buffer (RecHdr.RecordLength, To_Unbounded_String ("FSB"));
                FsbTypeIndicator := Integer (Buffer (2));
                LoadIt           := False;
            when Name_Block_Byte =>
                FileName := ProcessNB (RecHdr);
            when UDA_Byte =>
                -- throw away for now
                Load_Buffer
                   (RecHdr.RecordLength,
                    To_Unbounded_String ("UDA")); -- TODO Check this is OK
            when ACL_Byte =>
                -- We don't do anything except report ACLs at the moment
                Load_Buffer (RecHdr.RecordLength, To_Unbounded_String ("ACL"));
                if Verbose then
                    Ada.Text_IO.Put_Line
                       (" ACL: "); -- & Unsigned_8'Image(Buffer));
                end if;
            when Link_Byte =>
                Process_Link (RecHdr, FileName);
            when Data_Start_Byte =>
                -- nothing to do - it's just a record header
                null;
            when Data_Block_Byte =>
                ProcessDataBlock (RecHdr);
            when Data_End_Byte =>
                ProcessEndBlock;
            when End_Dump_Byte =>
                Ada.Text_IO.Put_Line ("=== End of Dump ===");
                Done := True;
            when others =>
                Ada.Text_IO.Put_Line
                   (Ada.Text_IO.Standard_Error,
                    "ERROR: Unknown block type: " &
                    Unsigned_8'Image (RecHdr.RecordType) & " Giving up.");
                Set_Exit_Status (Failure);
                Abort_Task (Current_Task);
        end case;
    end loop Process_Each_Block;

exception
    when Error : others =>
        Ada.Text_IO.Put ("Unexpected Error: ");
        Ada.Text_IO.Put_Line (Ada.Exceptions.Exception_Message (Error));
        raise;

end Loada;

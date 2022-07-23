-- Copyright 2021.2022 S.Merrony

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

with Ada.Characters.Handling;
with Ada.Command_Line;        use Ada.Command_Line;
with Ada.Task_Identification; use Ada.Task_Identification;
with Ada.Text_IO;

package body Aosvs_Dump is

    function Read_Word (Dump_Stream : Stream_Access) return Unsigned_16 is
        DG_Word      : Unsigned_16;
        Byte1, Byte2 : Unsigned_8;
    begin
        Unsigned_8'Read (Dump_Stream, Byte1);
        Unsigned_8'Read (Dump_Stream, Byte2);
        DG_Word := Shift_Left (Unsigned_16 (Byte1), 8) + Unsigned_16 (Byte2);
        return DG_Word;
    end Read_Word;

    function Read_Blob
       (Num_Bytes   : Positive; 
        Dump_Stream : Stream_Access;
        Reason      : String) return Blob_Type
    is
        pragma Unreferenced(Reason);
        Blob : Blob_Type (1 .. Num_Bytes);
    begin
        -- Ada.Text_IO.Put_Line ("DEBUG: Read_Blob called for bytes: " & Integer'Image(Num_Bytes));
        Blob_Type'Read (Dump_Stream, Blob);
        return Blob;
    end Read_Blob;

    function Read_Header
       (Dump_Stream : Stream_Access) return Record_Header_Type
    is
        Hdr          : Record_Header_Type;
        Byte1, Byte2 : Unsigned_8;
    begin
        Unsigned_8'Read (Dump_Stream, Byte1);
        -- the record type is 6-bit
        Hdr.Record_Type   := Shift_Right (Byte1, 2);
        Hdr.Record_Length := Integer (Byte1 and 2#0000_0011#) * 256;
        Unsigned_8'Read (Dump_Stream, Byte2);
        Hdr.Record_Length := Hdr.Record_Length + Integer (Byte2);
        return Hdr;
    end Read_Header;

    function Read_SOD (Dump_Stream : Stream_Access) return SOD_Type is
        SOD : SOD_Type;
    begin
        SOD.Header := Read_Header (Dump_Stream);
        if SOD.Header.Record_Type /= Start_Dump_Byte then
            Ada.Text_IO.Put_Line
               (Ada.Text_IO.Standard_Error,
                "ERROR: This does not appear to be an AOS/VS DUMP_II/III file");
            Set_Exit_Status (Failure);
            Abort_Task (Current_Task);
        end if;
        SOD.Dump_Format_Version := Read_Word (Dump_Stream);
        SOD.Dump_Time_Secs      := Read_Word (Dump_Stream);
        SOD.Dump_Time_Mins      := Read_Word (Dump_Stream);
        SOD.Dump_Time_Hours     := Read_Word (Dump_Stream);
        SOD.Dump_Time_Day       := Read_Word (Dump_Stream);
        SOD.Dump_Time_Month     := Read_Word (Dump_Stream);
        SOD.Dump_Time_Year      := Read_Word (Dump_Stream);
        return SOD;
    end Read_SOD;

    function Extract_First_String(Blob : Blob_Type) return Unbounded_String is
        Str : Unbounded_String;
    begin
        for Ix in Blob'Range loop
            exit when Blob (Ix) = 0;
            Append (Str, Character'Val (Blob (Ix)));
        end loop;
        return Str;
    end Extract_First_String;

    -- Convert to upper-case and replace ":" directory seps with "/"
    function To_Linux_Filename (Aosvs_Filename : Unbounded_String) return Unbounded_String is
        str : String := To_String(Aosvs_Filename);
    begin
        str := Ada.Characters.Handling.To_Upper (str);
        for C in str'Range loop
           if str(C) = ':' then
              str(C) := '/';
            end if;
        end loop;
        return To_Unbounded_String (str);
    end To_Linux_Filename;

end Aosvs_Dump;

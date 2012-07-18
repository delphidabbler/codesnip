{
 * FirstRun.UUnicode.pas
 *
 * Pascal script for use in [Code] Section of CodeSnip's Install.iss.
 *
 * Helper file for working with Unicode. Requires the Unicode version of Inno
 * Setup.
 *
 * $Rev$
 * $Date$
 *
 * ***** BEGIN LICENSE BLOCK *****
 *
 * Version: MPL 1.1
 *
 * The contents of this file are subject to the Mozilla Public License Version
 * 1.1 (the "License"); you may not use this file except in compliance with the
 * License. You may obtain a copy of the License at http://www.mozilla.org/MPL/
 *
 * Software distributed under the License is distributed on an "AS IS" basis,
 * WITHOUT WARRANTY OF ANY KIND, either express or implied. See the License for
 * the specific language governing rights and limitations under the License.
 *
 * The Original Code is FirstRun.UUnicode.pas, formerly Unicode.ps.
 *
 * The Initial Developer of the Original Code is Peter Johnson
 * (http://www.delphidabbler.com/).
 *
 * Portions created by the Initial Developer are Copyright (C) 2007-2012 Peter
 * Johnson. All Rights Reserved.
 *
 * Contributors:
 *    NONE
 *
 * ***** END LICENSE BLOCK *****
}

unit FirstRun.UUnicode;

interface

// Writes the lines of Unicode strings from the given string array to a named
// file. Each line is terminated with CRLF.
procedure WriteStringsToUnicodeFile(FileName: string; Lines: array of string);

implementation

uses
  Windows,
  UMessageBox;

//const
//  // Constants for Windows CreateFile API function.
//  GENERIC_WRITE = $40000000;
//  CREATE_ALWAYS = 2;

//// Import for Windows CreateFileW API function.
//function CreateFileW(
//  lpFileName: PWideChar;
//  dwDesiredAccess, dwShareMode: DWORD;
//  lpSecurityAttributes: PSecurityAttributes;
//  dwCreationDisposition, dwFlagsAndAttributes: DWORD;
//  hTemplateFile: THandle
//): THandle;
//external 'CreateFileW@kernel32.dll stdcall';
//
//// Import for Windows WriteFile API function. Prototype specialised for writing
//// Unicode strings.
//function WriteFile(
//  hFile: THandle;
//  const Buffer: string; // untyped param
//  nNumberOfBytesToWrite: LongWord; // DWORD;
//  var lpNumberOfBytesWritten: LongWord; // DWORD;
//  lpOverlapped: Cardinal // POverlapped
//): BOOL;
//external 'WriteFile@kernel32.dll stdcall';
//
//// Import for Windows CloseHandle API function.
//function CloseHandle(
//  hObject: THandle
//): BOOL;
//external 'CloseHandle@kernel32.dll stdcall';

// Writes the lines of Unicode strings from the given string array to a named
// file. Each line is terminated with CRLF.
procedure WriteStringsToUnicodeFile(FileName: string; Lines: array of string);
var
  F: THandle;             // file handle
  I: Integer;             // loops thru string array
  BytesWritten: LongWord; // dummy for use with WriteFile
  Line: string;           // each line from array
  BOM: WideChar;          // stores UTF-16LE byte order mark
begin
  // Create new file
  F := CreateFileW(
    PChar(FileName),
    GENERIC_WRITE,
    0,
    nil,
    CREATE_ALWAYS,
    FILE_ATTRIBUTE_ARCHIVE,
    0
  );
  if F = LongWord(-1) then
  begin
    TMessageBox.Error( nil, 'Error - can''t create file ' + FileName);
    Exit;
  end;
  // Write BOM for UTF-16LE
  BOM := #$FEFF;
  WriteFile(
    F,
    BOM,
    2,      // Size of a Unicode char
    BytesWritten,
    nil
  );
  // Write each element of string array terminated by CRLF
  for I := 0 to Length(Lines) - 1 do
  begin
    Line := Lines[I] + #13#10;
    WriteFile(
      F,
      Line,
      Length(Line) * 2, // These are 2 byte Unicode chars
      BytesWritten,
      nil
    );
  end;
  // Close the file
  CloseHandle(F);
end;

end.


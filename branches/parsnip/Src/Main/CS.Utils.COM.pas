{
 * This Source Code Form is subject to the terms of the Mozilla Public License,
 * v. 2.0. If a copy of the MPL was not distributed with this file, You can
 * obtain one at http://mozilla.org/MPL/2.0/
 *
 * Copyright (C) 2014, Peter Johnson (www.delphidabbler.com).
 *
 * $Rev$
 * $Date$
 *
 * Various utility routines and classes for working with COM.
}


unit CS.Utils.COM;

interface

function TaskAllocWideString(const S: string): PWideChar;
  {Allocates memory for a wide string using the Shell's task allocator and
  copies a given string into the memory as a wide string. Caller is responsible
  for freeing the buffer and must use the shell's allocator to do this.
    @param S [in] String to convert.
    @return Pointer to buffer containing wide string.
  }

implementation

uses
  // Delphi
  SysUtils,
  ActiveX;

function TaskAllocWideString(const S: string): PWideChar;
  {Allocates memory for a wide string using the Shell's task allocator and
  copies a given string into the memory as a wide string. Caller is responsible
  for freeing the buffer and must use the shell's allocator to do this.
    @param S [in] String to convert.
    @return Pointer to buffer containing wide string.
  }
var
  StrLen: Integer;  // length of string in bytes
begin
  // Store length of string, allowing for terminal #0
  StrLen := Length(S) + 1;
  // Allocate buffer for wide string using task allocator
  Result := CoTaskMemAlloc(StrLen * SizeOf(WideChar));
  if not Assigned(Result) then
    raise EOutOfMemory.Create('TaskAllocWideString: can''t allocate buffer.');
  // Convert string to wide string and store in buffer
  StringToWideChar(S, Result, StrLen);
end;


end.

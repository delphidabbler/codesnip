{
 * UStrUtils.pas
 *
 * String utility routines.
 *
 * Some of the routines are simply wrappers around SysUtils and StrUtils
 * routines with the purpose of (a) collecting them together in one place and
 * (b) providing a standardised parameter order.
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
 * The Original Code is UStrUtils.pas
 *
 * The Initial Developer of the Original Code is Peter Johnson
 * (http://www.delphidabbler.com/).
 *
 * Portions created by the Initial Developer are Copyright (C) 2011 Peter
 * Johnson. All Rights Reserved.
 *
 * Contributor(s)
 *   NONE
 *
 * ***** END LICENSE BLOCK *****
}


unit UStrUtils;


interface


///  <summary>Checks if string Haystack contains string Needle. Case sensitive.
///  </summary>
function StrContainsStr(const Needle, Haystack: string): Boolean;

///  <summary>Returns index of string Needle in string Haystack or 0 if Needle
///  is not in Haystack. Case sensitive.</summary>
function StrPos(const Needle, Haystack: string): Integer; overload;

///  <summary>Returns index of string Needle in string Haystack, beginning the
///  search at Offset. Returns 0 if Haystack is not found or if Offset is less
///  than 1 or greater than length of Haystack. Case sensitive.</summary>
function StrPos(const Needle, Haystack: string; const Offset: Integer): Integer;
  overload;

///  <summary>Returns index of last occurence of string Needle in string
///  Haystack or 0 if Needle is not in Haystack. Case sensitive.</summary>
function StrLastPos(const Needle, Haystack: string): Integer;

///  <summary>Compares Left and Right strings, ignoring case. Returns 0 if both
///  strings are the same, -ve if Left is less than Right or +ve if Left is
///  greater than Right.</summary>
function StrCompareText(const Left, Right: string): Integer;

///  <summary>Converts a string to upper case.</summary>
function StrToUpper(const Str: string): string;

///  <summary>Converts a string to lower case.</summary>
function StrToLower(const Str: string): string;


implementation


uses
  // Delphi
  SysUtils, StrUtils;


function StrCompareText(const Left, Right: string): Integer;
begin
  Result := SysUtils.AnsiCompareText(Left, Right);
end;

function StrContainsStr(const Needle, Haystack: string): Boolean;
begin
  Result := StrUtils.AnsiContainsStr(Haystack, Needle);
end;

function StrLastPos(const Needle, Haystack: string): Integer;
var
  Idx: Integer; // an index of Needle in Haystack
begin
  Result := 0;
  Idx := StrPos(Needle, Haystack);
  if Idx = 0 then
    Exit;
  while Idx > 0 do
  begin
    Result := Idx;
    Idx := StrPos(Needle, Haystack, Idx + 1);
  end;
end;

function StrPos(const Needle, Haystack: string): Integer;
begin
  Result := SysUtils.AnsiPos(Needle, Haystack);
end;

function StrPos(const Needle, Haystack: string; const Offset: Integer): Integer;
begin
  Result := StrUtils.PosEx(Needle, Haystack, Offset);
end;

function StrToUpper(const Str: string): string;
begin
  Result := SysUtils.AnsiUpperCase(Str);
end;

function StrToLower(const Str: string): string;
begin
  Result := SysUtils.AnsiLowerCase(Str);
end;

end.


{
 * UStrUtils.pas
 *
 * String utility routines.
 *
 * Some of the routines are simply wrappers around SysUtils and StrUtils string
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

///  <summary>Checks if Left and Right strings are equal. Case sensitive.
///  </summary>
function StrSameStr(const Left, Right: string): Boolean;

///  <summary>Checks if Left and Right strings are equivalent when case is
///  ignored.</summary>
function StrSameText(const Left, Right: string): Boolean;

///  <summary>Converts a string to upper case.</summary>
function StrToUpper(const Str: string): string;

///  <summary>Converts a string to lower case.</summary>
function StrToLower(const Str: string): string;

///  <summary>Returns a substring of Str starting at index StartIdx of length
///  Count.</summary>
///  <remarks>
///  <para>If Count specifies more than the available number of characters, the
///  substring from StartIdx to the end of the string is returned.</para>
///  <para>If StartIdx is beyond the end of Str the empty string is returned.
///  </para>
///  </remarks>
function StrSlice(const Str: string; const StartIdx, Count: Integer): string;

///  <summary>Returns the leading characters of Str up to a length of Count
///  characters.</summary>
///  <remarks>If Count is greater than the length of Str the whole string is
///  returned.</remarks>
function StrSliceLeft(const Str: string; const Count: Integer): string;

///  <summary>Returns the trailing characters of Str up to a length of Count
///  characters.</summary>
///  <remarks>If Count is greater than the length of Str the whole string is
///  returned.</remarks>
function StrSliceRight(const Str: string; const Count: Integer): string;

///  <summary>Checks if string Str begins with sub string SubStr. Case
///  sensitive.</summary>
function StrStartsStr(const SubStr, Str: string): Boolean;

///  <summary>Checks if string Str begins with sub string SubStr. Case
///  insensitive.</summary>
function StrStartsText(const SubStr, Str: string): Boolean;

///  <summary>Replaces all occurences of FindStr in Str with ReplaceStr.
///  </summary>
function StrReplace(const Str, FindStr, ReplaceStr: string): string;

///  <summary>Trims leading and trailing space characters from a string.
///  </summary>
function StrTrimSpaces(const Str: string): string;

///  <summary>Trims leading space characters from a string.</summary>
function StrTrimLeftSpaces(const Str: string): string;

///  <summary>Trims trailing space characters from a string.</summary>
function StrTrimRightSpaces(const Str: string): string;

///  <summary>Trims leading and trailing characters C from string Str.</summary>
function StrTrimChars(const Str: string; const C: Char): string;

///  <summary>Trims leading characters C from string Str.</summary>
function StrTrimLeftChars(const Str: string; const C: Char): string;

///  <summary>Trims trailing characters C from string Str.</summary>
function StrTrimRightChars(const Str: string; const C: Char): string;

///  <summary>Ensures line breaks in a string are in Windows format (CRLF).
///  </summary>
///  <remarks>Converts both Unix (LF) and Macintosh (CR) to CRLF.</remarks>
function StrWindowsLineBreaks(const Str: string): string;

///  <summary>Ensures line breaks in a string are in Unix format (LF).
///  </summary>
///  <remarks>Converts both Windows (CRLF) and Macintosh (CR) to LF.</remarks>
function StrUnixLineBreaks(const Str: string): string;

///  <summary>Checks if the character at index Idx in Str is one of the
///  delimter characters stored in Delims.</summary>
///  <remarks>Delims must contain only characters that take just one wide
///  character, i.e. they are from the basic multiligual plane.</remarks>
function StrIsDelimiter(const Delims, Str: string; const Idx: Integer): Boolean;

///  <summary>Returns position of last character in Str that matches any
///  character from Delims.</summary>
///  <remarks>Delims must contain only characters that take just one wide
///  character, i.e. they are from the basic multiligual plane.</remarks>
function StrLastDelimiterPos(const Delims, Str: string): Integer;


implementation


uses
  // Delphi
  SysUtils, StrUtils,
  // Project
  UConsts;


function StrCompareText(const Left, Right: string): Integer;
begin
  Result := SysUtils.AnsiCompareText(Left, Right);
end;

function StrContainsStr(const Needle, Haystack: string): Boolean;
begin
  Result := StrUtils.AnsiContainsStr(Haystack, Needle);
end;

function StrIsDelimiter(const Delims, Str: string; const Idx: Integer): Boolean;
begin
  Result := SysUtils.IsDelimiter(Delims, Str, Idx);
end;

function StrLastDelimiterPos(const Delims, Str: string): Integer;
begin
  Result := SysUtils.LastDelimiter(Delims, Str);
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

function StrReplace(const Str, FindStr, ReplaceStr: string): string;
begin
  Result := StrUtils.AnsiReplaceStr(Str, FindStr, ReplaceStr);
end;

function StrSameStr(const Left, Right: string): Boolean;
begin
  Result := SysUtils.AnsiSameStr(Left, Right);
end;

function StrSameText(const Left, Right: string): Boolean;
begin
  Result := SysUtils.AnsiSameText(Left, Right);
end;

function StrSlice(const Str: string; const StartIdx, Count: Integer): string;
begin
  Result := StrUtils.AnsiMidStr(Str, StartIdx, Count);
end;

function StrSliceLeft(const Str: string; const Count: Integer): string;
begin
  Result := StrUtils.AnsiLeftStr(Str, Count);
end;

function StrSliceRight(const Str: string; const Count: Integer): string;
begin
  Result := StrUtils.AnsiRightStr(Str, Count);
end;

function StrStartsStr(const SubStr, Str: string): Boolean;
begin
  Result := StrUtils.AnsiStartsStr(SubStr, Str);
end;

function StrStartsText(const SubStr, Str: string): Boolean;
begin
  Result := StrUtils.AnsiStartsText(SubStr, Str);
end;

function StrToLower(const Str: string): string;
begin
  Result := SysUtils.AnsiLowerCase(Str);
end;

function StrToUpper(const Str: string): string;
begin
  Result := SysUtils.AnsiUpperCase(Str);
end;

function StrTrimChars(const Str: string; const C: Char): string;
begin
  Result := StrTrimLeftChars(StrTrimRightChars(Str, C), C);
end;

function StrTrimLeftChars(const Str: string; const C: Char): string;
var
  Idx: Integer; // index into string
begin
  Idx := 1;
  while (Idx <= Length(Str)) and (Str[Idx] = C) do
    Inc(Idx);
  if Idx > 1 then
    Result := Copy(Str, Idx, MaxInt)
  else
    Result := Str;
end;

function StrTrimLeftSpaces(const Str: string): string;
begin
  Result := SysUtils.TrimLeft(Str);
end;

function StrTrimRightChars(const Str: string; const C: Char): string; overload;
var
  Idx: Integer; // index into string
begin
  Idx := Length(Str);
  while (Idx >= 1) and (Str[Idx] = C) do
    Dec(Idx);
  if Idx < Length(Str) then
    Result := Copy(Str, 1, Idx)
  else
    Result := Str;
end;

function StrTrimRightSpaces(const Str: string): string;
begin
  Result := SysUtils.TrimRight(Str);
end;

function StrTrimSpaces(const Str: string): string;
begin
  Result := SysUtils.Trim(Str);
end;

function StrUnixLineBreaks(const Str: string): string;
begin
  // Replace any CRLF (MSDOS/Windows) line ends with LF
  Result := StrReplace(Str, CRLF, LF);
  // Replace any remaining CR (Mac) line ends with LF
  Result := StrReplace(Result, CR, LF);
end;

function StrWindowsLineBreaks(const Str: string): string;
begin
  // First convert to Unix to get rid of all CR characters (CRs could come from
  // existing Windows CRLF or from Max CR)
  Result := StrUnixLineBreaks(Str);
  // Now have only LFs - convert them all to CRLFs
  Result := StrReplace(Result, LF, CRLF);
end;

end.


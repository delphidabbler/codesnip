{
 * UStrUtils.pas
 *
 * Unicode string utility routines.
 *
 * Some of the routines are simply wrappers around SysUtils and StrUtils string
 * routines with the purpose of (a) collecting them together in one place and
 * (b) providing a standardised parameter order. Other routines are original.
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


uses
  // Delphi
  Classes;


///  <summary>Checks if string Haystack contains string Needle. Case sensitive.
///  </summary>
function StrContainsStr(const Needle, Haystack: UnicodeString): Boolean;

///  <summary>Returns index of string Needle in string Haystack or 0 if Needle
///  is not in Haystack. Case sensitive.</summary>
function StrPos(const Needle, Haystack: UnicodeString): Integer; overload;

///  <summary>Returns index of string Needle in string Haystack, beginning the
///  search at Offset. Returns 0 if Haystack is not found or if Offset is less
///  than 1 or greater than length of Haystack. Case sensitive.</summary>
function StrPos(const Needle, Haystack: UnicodeString; const Offset: Integer):
  Integer; overload;

///  <summary>Returns index of last occurence of string Needle in string
///  Haystack or 0 if Needle is not in Haystack. Case sensitive.</summary>
function StrLastPos(const Needle, Haystack: UnicodeString): Integer;

///  <summary>Compares Left and Right strings, ignoring case. Returns 0 if both
///  strings are the same, -ve if Left is less than Right or +ve if Left is
///  greater than Right.</summary>
function StrCompareText(const Left, Right: UnicodeString): Integer;

///  <summary>Checks if Left and Right strings are equal. Case sensitive.
///  </summary>
function StrSameStr(const Left, Right: UnicodeString): Boolean;

///  <summary>Checks if Left and Right strings are equivalent when case is
///  ignored.</summary>
function StrSameText(const Left, Right: UnicodeString): Boolean;

///  <summary>Converts a string to upper case.</summary>
function StrToUpper(const Str: UnicodeString): UnicodeString;

///  <summary>Converts a string to lower case.</summary>
function StrToLower(const Str: UnicodeString): UnicodeString;

///  <summary>Returns a substring of Str starting at index StartIdx of length
///  Count.</summary>
///  <remarks>
///  <para>If Count specifies more than the available number of characters, the
///  substring from StartIdx to the end of the string is returned.</para>
///  <para>If StartIdx is beyond the end of Str the empty string is returned.
///  </para>
///  </remarks>
function StrSlice(const Str: UnicodeString; const StartIdx, Count: Integer):
  UnicodeString;

///  <summary>Returns the leading characters of Str up to a length of Count
///  characters.</summary>
///  <remarks>If Count is greater than the length of Str the whole string is
///  returned.</remarks>
function StrSliceLeft(const Str: UnicodeString; const Count: Integer):
  UnicodeString;

///  <summary>Returns the trailing characters of Str up to a length of Count
///  characters.</summary>
///  <remarks>If Count is greater than the length of Str the whole string is
///  returned.</remarks>
function StrSliceRight(const Str: UnicodeString; const Count: Integer):
  UnicodeString;

///  <summary>Checks if string Str begins with sub string SubStr. Case
///  sensitive.</summary>
function StrStartsStr(const SubStr, Str: UnicodeString): Boolean;

///  <summary>Checks if string Str begins with sub string SubStr. Case
///  insensitive.</summary>
function StrStartsText(const SubStr, Str: UnicodeString): Boolean;

///  <summary>Replaces all occurences of FindStr in Str with ReplaceStr.
///  </summary>
function StrReplace(const Str, FindStr, ReplaceStr: UnicodeString):
  UnicodeString;

///  <summary>Trims leading and trailing space characters from a string.
///  </summary>
function StrTrimSpaces(const Str: UnicodeString): UnicodeString;

///  <summary>Trims leading space characters from a string.</summary>
function StrTrimLeftSpaces(const Str: UnicodeString): UnicodeString;

///  <summary>Trims trailing space characters from a string.</summary>
function StrTrimRightSpaces(const Str: UnicodeString): UnicodeString;

///  <summary>Trims leading and trailing characters C from string Str.</summary>
function StrTrimChars(const Str: UnicodeString; const C: Char): UnicodeString;

///  <summary>Trims leading characters C from string Str.</summary>
function StrTrimLeftChars(const Str: UnicodeString; const C: Char):
  UnicodeString;

///  <summary>Trims trailing characters C from string Str.</summary>
function StrTrimRightChars(const Str: UnicodeString; const C: Char):
  UnicodeString;

///  <summary>Ensures line breaks in a string are in Windows format (CRLF).
///  </summary>
///  <remarks>Converts both Unix (LF) and Macintosh (CR) to CRLF.</remarks>
function StrWindowsLineBreaks(const Str: UnicodeString): UnicodeString;

///  <summary>Ensures line breaks in a string are in Unix format (LF).
///  </summary>
///  <remarks>Converts both Windows (CRLF) and Macintosh (CR) to LF.</remarks>
function StrUnixLineBreaks(const Str: UnicodeString): UnicodeString;

///  <summary>Checks if the character at index Idx in Str is one of the
///  delimter characters stored in Delims.</summary>
///  <remarks>Delims must contain only characters that take just one wide
///  character, i.e. they are from the basic multiligual plane.</remarks>
function StrIsDelimiter(const Delims, Str: UnicodeString; const Idx: Integer):
  Boolean;

///  <summary>Returns total number of occurences of delimieters from Delim in
///  string Str.</summary>
///  <remarks>Delims must contain only characters that take just one wide
///  character, i.e. they are from the basic multiligual plane.</remarks>
function StrCountDelims(const Delims, Str: UnicodeString): Integer;

///  <summary>Returns position of last character in Str that matches any
///  character from Delims.</summary>
///  <remarks>Delims must contain only characters that take just one wide
///  character, i.e. they are from the basic multiligual plane.</remarks>
function StrLastDelimiterPos(const Delims, Str: UnicodeString): Integer;

///  <summary>Capitalises each word in string Str.</summary>
function StrCapitaliseWords(const Str: UnicodeString): UnicodeString;

///  <summary>Compresses white space in string Str.</summary>
///  <remarks>All sequences of white space in Str are replaced by a single
///  space.</remarks>
function StrCompressWhiteSpace(const Str: UnicodeString): UnicodeString;

///  <summary>Removes all white space from string Str.</summary>
function StrStripWhiteSpace(const Str: UnicodeString): UnicodeString;

///  <summary>Checks if a stream contains white space.</summary>
function StrContainsWhiteSpace(const Str: UnicodeString): Boolean;

///  <summary>Encloses string Str in quotes specified by Quote iff Str contains
///  white space.</summary>
///  <remarks>Opening and closing quote are the same character.</remarks>
function StrQuoteSpaced(const Str: UnicodeString;
  const Quote: Char = '"'): UnicodeString;

///  <summary>Joins all strings from a string list together into a single string
///  with each list element being separated by Delim. Empty string list elements
///  are included in the output string only if AllowEmpty is True.</summary>
function StrJoin(const SL: TStrings; const Delim: UnicodeString;
  const AllowEmpty: Boolean = True): UnicodeString;

///  <summary>Splits string Str at delimiter Delim and records the components in
///  List. If TrimStrs is True spaces are trimmed from each component. If
///  AllowEmpty is True empty components are added to List.</summary>
///  <remarks>String components are trimmed before being checked for emptyness.
///  </remarks>
function StrExplode(Str: UnicodeString; const Delim: UnicodeString;
  const List: TStrings; const AllowEmpty: Boolean = True;
  const TrimStrs: Boolean = False): Integer;

///  <summary>Splits string Str at the first occurence of Delim setting Left to
///  the string preceeding Delim and Right to the string following Delim.
///  Returns True if Delim was found in Str, False if not.</summary>
///  <remarks>Left and Right be empty if Delim is found at the start or end of
///  Str respectively.</remarks>
function StrSplit(const Str: UnicodeString; const Delim: UnicodeString;
  out Left, Right: UnicodeString): Boolean;

///  <summary>Word wraps text Str to form lines of maximum length MaxLen and
///  offsets each line using spaces to form a left margin of size given by
///  Margin.</summary>
///  <remarks>Lines are separated by CRLF.</remarks>
function StrWrap(const Str: UnicodeString; const MaxLen, Margin: Integer):
  UnicodeString;

///  <summary>Checks in string Str forms a valid sentence and, if not, adds a
///  full stop.</summary>
function StrMakeSentence(const Str: UnicodeString): UnicodeString;


implementation


uses
  // Delphi
  SysUtils, StrUtils, Character,
  // Project
  UConsts, UUtils;


function StrCapitaliseWords(const Str: UnicodeString): UnicodeString;
var
  Idx: Integer;           // loops through each character in string
  WantCapital: Boolean;   // flag indicating whether captial letter required
begin
  Result := Str;
  WantCapital := True;
  for Idx := 1 to Length(Str) do
  begin
    if TCharacter.IsLetter(Result[Idx]) then
    begin
      if WantCapital then
        Result[Idx] := TCharacter.ToUpper(Result[Idx]);
      WantCapital := False;
    end
    else
      WantCapital := TCharacter.IsWhiteSpace(Result[Idx]);
  end;
end;

function StrCompareText(const Left, Right: UnicodeString): Integer;
begin
  Result := SysUtils.AnsiCompareText(Left, Right);
end;

function StrCompressWhiteSpace(const Str: UnicodeString): UnicodeString;
var
  Idx: Integer;       // loops thru all characters in string
  ResCount: Integer;  // counts number of characters in result string
  PRes: PChar;        // pointer to characters in result string
begin
  // Set length of result to length of source string and set pointer to it
  SetLength(Result, Length(Str));
  PRes := PChar(Result);
  // Reset count of characters in result string
  ResCount := 0;
  // Loop thru characters of source string
  Idx := 1;
  while Idx <= Length(Str) do
  begin
    if TCharacter.IsWhiteSpace(Str[Idx]) then
    begin
      // Current char is white space: replace by space char and count it
      PRes^ := ' ';
      Inc(PRes);
      Inc(ResCount);
      // Skip past any following white space
      Inc(Idx);
      while TCharacter.IsWhiteSpace(Str[Idx]) do
        Inc(Idx);
    end
    else
    begin
      // Current char is not white space: copy it literally and count it
      PRes^ := Str[Idx];
      Inc(PRes);
      Inc(ResCount);
      Inc(Idx);
    end;
  end;
  // Reduce length of result string if it is shorter than source string
  if ResCount < Length(Str) then
    SetLength(Result, ResCount);
end;

function StrContainsStr(const Needle, Haystack: UnicodeString): Boolean;
begin
  Result := StrUtils.AnsiContainsStr(Haystack, Needle);
end;

function StrContainsWhiteSpace(const Str: UnicodeString): Boolean;
var
  Ch: Char;   // scans through Str
begin
  Result := False;
  for Ch in Str do
    if TCharacter.IsWhiteSpace(Ch) then
      Exit(True);
end;

function StrCountDelims(const Delims, Str: UnicodeString): Integer;
var
  Idx: Integer; //loops thru all characters in string
begin
  Result := 0;
  for Idx := 1 to Length(Str) do
    if StrIsDelimiter(Delims, Str, Idx) then
      Inc(Result);
end;

function StrExplode(Str: UnicodeString; const Delim: UnicodeString;
  const List: TStrings; const AllowEmpty: Boolean = True;
  const TrimStrs: Boolean = False): Integer;
var
  Head: UnicodeString;      // head of Str before delimiter
  Remainder: UnicodeString; // remaining unconsumed part of string

  // ---------------------------------------------------------------------------
  procedure ProcessHead;
    {Modifies Head string as necessary and adds to list if required.
    }
  begin
    if TrimStrs then
      Head := StrTrimSpaces(Head);
    if (Head <> '') or AllowEmpty then
      List.Add(Head)
  end;
  // ---------------------------------------------------------------------------

begin
  // Clear the list
  List.Clear;
  // Check we have some entries in the string
  if Str <> '' then
  begin
    // Repeatedly split string until we have no more entries
    while StrSplit(Str, Delim, Head, Remainder) do
    begin
      ProcessHead;
      // Go round again with remainder of string
      Str := Remainder;
    end;
    // Deal with Head after last delimiter, if any
    ProcessHead;
  end;
  Result := List.Count;
end;

function StrIsDelimiter(const Delims, Str: UnicodeString; const Idx: Integer):
  Boolean;
begin
  Result := SysUtils.IsDelimiter(Delims, Str, Idx);
end;

function StrJoin(const SL: TStrings; const Delim: UnicodeString;
  const AllowEmpty: Boolean = True): UnicodeString;
var
  Idx: Integer; // loops thru all items in string list
begin
  Result := '';
  for Idx := 0 to Pred(SL.Count) do
  begin
    if (SL[Idx] <> '') or AllowEmpty then
      if Result = '' then
        Result := SL[Idx]
      else
        Result := Result + Delim + SL[Idx];
  end;
end;

function StrLastDelimiterPos(const Delims, Str: UnicodeString): Integer;
begin
  Result := SysUtils.LastDelimiter(Delims, Str);
end;

function StrLastPos(const Needle, Haystack: UnicodeString): Integer;
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

function StrMakeSentence(const Str: UnicodeString): UnicodeString;
resourcestring
  // characters that can close sentence
  sSentenceClosers = '.!?';
  sFullStop = '.';
begin
  if StrIsDelimiter(sSentenceClosers, Str, Length(Str)) then
    Result := Str
  else
    Result := Str + sFullStop;
end;

function StrPos(const Needle, Haystack: UnicodeString): Integer;
begin
  Result := SysUtils.AnsiPos(Needle, Haystack);
end;

function StrPos(const Needle, Haystack: UnicodeString; const Offset: Integer):
  Integer;
begin
  Result := StrUtils.PosEx(Needle, Haystack, Offset);
end;

function StrQuoteSpaced(const Str: UnicodeString;
  const Quote: Char = '"'): UnicodeString;
begin
  if StrContainsWhiteSpace(Str) then
    Result := Quote + Str + Quote
  else
    Result := Str;
end;

function StrReplace(const Str, FindStr, ReplaceStr: UnicodeString):
  UnicodeString;
begin
  Result := StrUtils.AnsiReplaceStr(Str, FindStr, ReplaceStr);
end;

function StrSameStr(const Left, Right: UnicodeString): Boolean;
begin
  Result := SysUtils.AnsiSameStr(Left, Right);
end;

function StrSameText(const Left, Right: UnicodeString): Boolean;
begin
  Result := SysUtils.AnsiSameText(Left, Right);
end;

function StrSlice(const Str: UnicodeString; const StartIdx, Count: Integer):
  UnicodeString;
begin
  Result := StrUtils.AnsiMidStr(Str, StartIdx, Count);
end;

function StrSliceLeft(const Str: UnicodeString; const Count: Integer):
  UnicodeString;
begin
  Result := StrUtils.AnsiLeftStr(Str, Count);
end;

function StrSliceRight(const Str: UnicodeString; const Count: Integer):
  UnicodeString;
begin
  Result := StrUtils.AnsiRightStr(Str, Count);
end;

function StrSplit(const Str: UnicodeString; const Delim: UnicodeString;
  out Left, Right: UnicodeString): Boolean;
var
  DelimPos: Integer;  // position of delimiter in source string
begin
  // Find position of first occurence of delimiter in string
  DelimPos := StrPos(Delim, Str);
  if DelimPos > 0 then
  begin
    // Delimiter found: split string at delimiter
    Left := Copy(Str, 1, DelimPos - 1);
    Right := Copy(Str, DelimPos + Length(Delim), MaxInt);
    Result := True;
  end
  else
  begin
    // Delimiter not found: set Left to whole string
    Left := Str;
    Right := '';
    Result := False;
  end;
end;

function StrStartsStr(const SubStr, Str: UnicodeString): Boolean;
begin
  Result := StrUtils.AnsiStartsStr(SubStr, Str);
end;

function StrStartsText(const SubStr, Str: UnicodeString): Boolean;
begin
  Result := StrUtils.AnsiStartsText(SubStr, Str);
end;

function StrStripWhiteSpace(const Str: UnicodeString): UnicodeString;
var
  Idx: Integer;       // loops thru all characters in string
  ResCount: Integer;  // counts number of characters in result string
  PRes: PChar;        // pointer to characters in result string
begin
  // Set length of result to length of source string and set pointer to it
  SetLength(Result, Length(Str));
  PRes := PChar(Result);
  // Reset count of characters in result string
  ResCount := 0;
  // Loop thru characters of source string
  Idx := 1;
  while Idx <= Length(Str) do
  begin
    if not TCharacter.IsWhiteSpace(Str[Idx]) then
    begin
      // Character is not white space: copy to result string
      PRes^ := Str[Idx];
      Inc(ResCount);
      Inc(PRes);
    end;
    Inc(Idx);
  end;
  // Reduce length of result string if it is shorter than source string
  if ResCount < Length(Str) then
    SetLength(Result, ResCount);
end;

function StrToLower(const Str: UnicodeString): UnicodeString;
begin
  Result := SysUtils.AnsiLowerCase(Str);
end;

function StrToUpper(const Str: UnicodeString): UnicodeString;
begin
  Result := SysUtils.AnsiUpperCase(Str);
end;

function StrTrimChars(const Str: UnicodeString; const C: Char): UnicodeString;
begin
  Result := StrTrimLeftChars(StrTrimRightChars(Str, C), C);
end;

function StrTrimLeftChars(const Str: UnicodeString; const C: Char):
  UnicodeString;
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

function StrTrimLeftSpaces(const Str: UnicodeString): UnicodeString;
begin
  Result := SysUtils.TrimLeft(Str);
end;

function StrTrimRightChars(const Str: UnicodeString; const C: Char):
  UnicodeString; overload;
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

function StrTrimRightSpaces(const Str: UnicodeString): UnicodeString;
begin
  Result := SysUtils.TrimRight(Str);
end;

function StrTrimSpaces(const Str: UnicodeString): UnicodeString;
begin
  Result := SysUtils.Trim(Str);
end;

function StrUnixLineBreaks(const Str: UnicodeString): UnicodeString;
begin
  // Replace any CRLF (MSDOS/Windows) line ends with LF
  Result := StrReplace(Str, CRLF, LF);
  // Replace any remaining CR (Mac) line ends with LF
  Result := StrReplace(Result, CR, LF);
end;

function StrWindowsLineBreaks(const Str: UnicodeString): UnicodeString;
begin
  // First convert to Unix to get rid of all CR characters (CRs could come from
  // existing Windows CRLF or from Max CR)
  Result := StrUnixLineBreaks(Str);
  // Now have only LFs - convert them all to CRLFs
  Result := StrReplace(Result, LF, CRLF);
end;

function StrWrap(const Str: UnicodeString; const MaxLen, Margin: Integer):
  UnicodeString;
var
  Word: UnicodeString;  // next word in input Str
  Line: UnicodeString;  // current output line
  Words: TStringList;   // list of words in input Str
  I: Integer;           // loops thru all words in input Str

  // -------------------------------------------------------------------------
  ///  Adds a line of text to output, offseting line by Margin spaces
  procedure AddLine(const Line: string);
  begin
    if Result <> '' then    // not first line: insert new line
      Result := Result + EOL;
    Result := Result + StringOfChar(' ', Margin) + Line;
  end;
  // -------------------------------------------------------------------------

begin
  // Get all words in Str
  Words := TStringList.Create;
  try
    StrExplode(Str, ' ', Words);
    Result := '';
    Line := '';
    // Loop for each word in Str
    for I := 0 to Pred(Words.Count) do
    begin
      Word := Words[I];
      if Length(Line) + Length(Word) + 1 <= MaxLen then
      begin
        // Word fits on current line: add it
        if Line = '' then
          Line := Word  // 1st word on line
        else
          Line := Line + ' ' + Word;
      end
      else
      begin
        // Word doesn't fit on line
        AddLine(Line);
        Line := Word;   // store word as first on next line
      end;
    end;
    if Line <> '' then
      // Residual line after end of loop: add to output
      AddLine(Line);
  finally
    Words.Free;
  end;
end;

end.


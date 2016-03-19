{
 * This Source Code Form is subject to the terms of the Mozilla Public License,
 * v. 2.0. If a copy of the MPL was not distributed with this file, You can
 * obtain one at http://mozilla.org/MPL/2.0/
 *
 * Copyright (C) 2011-2013, Peter Johnson (www.delphidabbler.com).
 *
 * $Rev$
 * $Date$
 *
 * Unicode string utility routines.
 *
 * Some of the routines are simply wrappers around SysUtils and StrUtils string
 * routines with the purpose of (a) collecting them together in one place and
 * (b) providing a standardised parameter order. Other routines are original.
}


unit UStrUtils;


interface


uses
  // Delphi
  Classes,
  // Project
  UConsts;


///  <summary>Checks if string Haystack contains string Needle. Case sensitive.
///  </summary>
function StrContainsStr(const Needle, Haystack: UnicodeString): Boolean;

///  <summary>Checks if string Haystack contains string Needle. Case
///  insensitive.</summary>
function StrContainsText(const Needle, Haystack: UnicodeString): Boolean;

///  <summary>Checks if string Str is present in string array Values. Case
///  insensitive.</summary>
function StrMatchText(const Str: string; const Values: array of string):
  Boolean;

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

///  <summary>Compares Left and Right strings, taking case into account. Returns
///  0 if both strings are the same, -ve if Left is less than Right or +ve if
///  Left is greater than Right.</summary>
function StrCompareStr(const Left, Right: UnicodeString): Integer;

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
///  <remarks>False is returned if SubStr = ''.</remarks>
function StrStartsStr(const SubStr, Str: UnicodeString): Boolean;

///  <summary>Checks if string Str begins with sub string SubStr. Case
///  insensitive.</summary>
///  <remarks>False is returned if SubStr = ''.</remarks>
function StrStartsText(const SubStr, Str: UnicodeString): Boolean;

///  <summary>Replaces all occurences of FindStr in Str with ReplaceStr.
///  </summary>
function StrReplace(const Str, FindStr, ReplaceStr: UnicodeString):
  UnicodeString;

///  <summary>Trims leading and trailing white space characters from a string.
///  </summary>
///  <remarks>White space is considered to be any character from #0..#32.
///  </remarks>
function StrTrim(const Str: UnicodeString): UnicodeString;

///  <summary>Trims leading white space characters from a string.</summary>
///  <remarks>White space is considered to be any character from #0..#32.
///  </remarks>
function StrTrimLeft(const Str: UnicodeString): UnicodeString;

///  <summary>Trims trailing white space characters from a string.</summary>
///  <remarks>White space is considered to be any character from #0..#32.
///  </remarks>
function StrTrimRight(const Str: UnicodeString): UnicodeString;

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
///  delimiter characters stored in Delims.</summary>
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
///  <remarks>Words are recognised only if separated by white space.</remarks>
function StrCapitaliseWords(const Str: UnicodeString): UnicodeString;

///  <summary>Compresses white space in string Str.</summary>
///  <remarks>Each sequence of one or more white space characters in Str is
///  replaced by a single space.</remarks>
function StrCompressWhiteSpace(const Str: UnicodeString): UnicodeString;

///  <summary>Removes all white space from string Str.</summary>
function StrStripWhiteSpace(const Str: UnicodeString): UnicodeString;

///  <summary>Checks if a stream contains white space.</summary>
function StrContainsWhiteSpace(const Str: UnicodeString): Boolean;

///  <summary>Encloses string Str in quotes specified by Quote iff Str contains
///  white space.</summary>
///  <remarks>Opening and closing quote are the same character.</remarks>
function StrQuoteSpaced(const Str: UnicodeString;
  const Quote: Char = DOUBLEQUOTE): UnicodeString;

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
///  <remarks>Either Left or Right will be empty if Delim is found at the start 
///  or end of Str respectively.</remarks>
function StrSplit(const Str: UnicodeString; const Delim: UnicodeString;
  out Left, Right: UnicodeString): Boolean;

///  <summary>Word wraps text Str to form lines of maximum length MaxLen and
///  offsets each line using spaces to form a left margin of size given by
///  Margin.</summary>
///  <remarks>Output lines are separated by CRLF.</remarks>
function StrWrap(const Str: UnicodeString; const MaxLen, Margin: Integer):
  UnicodeString; overload;

///  <summary>Word wraps each paragraph of text in Paras so that each line of a
///  paragraph has lines of maximum length MaxLineLen and is offset by the
///  number of spaces gvien by Margin. Blanks lines are used to separate
///  output paragraphs iff SeparateParas is true.</summary>
///  <remarks>Output lines are separated by CRLF.</remarks>
function StrWrap(const Paras: TStrings; const MaxLineLen, Margin: Integer;
  const SeparateParas: Boolean): UnicodeString; overload;

///  <summary>Checks in string Str forms a valid sentence and, if not, adds a
///  full stop.</summary>
///  <remarks>Trailing white space is ignored when detecting full stops etc.
///  </remarks>
function StrMakeSentence(const Str: UnicodeString): UnicodeString;

///  <summary>Returns one of two strings, TrueStr or FalseStr, depending on
///  value of given Condition.</summary>
function StrIf(const Condition: Boolean; const TrueStr, FalseStr: string):
  string;

///  <summary>Escapes all characters from string S that are included in
///  Escapable with the backslash character followed by the matching character
///  in Escapes.</summary>
function StrBackslashEscape(const S, Escapable, Escapes: string): string;


implementation


uses
  // Delphi
  SysUtils, StrUtils, Character;

{ Internal helper routines }

///  <summary>Checks if character at position Idx in Str can be trimmed from
///  string.</summary>
///  <remarks>Character can be trimmed if it's not part of a multi-character
///  sequence and TrimFn returns True for the character.</remarks>
function IsTrimmableChar(const Str: UnicodeString; const Idx: Integer;
  const TrimFn: TFunc<Char,Boolean>): Boolean;
begin
  Result := (ByteType(Str, Idx) = mbSingleByte) and TrimFn(Str[Idx]);
end;

///  <summary>Trims characters from both ends of a string.</summary>
///  <remarks>Anonymous function TrimFn determines whether a character is to be
///  trimmed.</remarks>
function InternalTrim(const Str: UnicodeString;
  const TrimFn: TFunc<Char,Boolean>): UnicodeString;
var
  TextStart: Integer; // position of start of non-space text
  TextEnd: Integer;   // position of end of non-space text
begin
  TextEnd := Length(Str);
  TextStart := 1;
  while (TextStart <= TextEnd)
    and IsTrimmableChar(Str, TextStart, TrimFn) do
    Inc(TextStart);
  if TextStart > TextEnd then
    Exit('');
  while IsTrimmableChar(Str, TextEnd, TrimFn) do
    Dec(TextEnd);
  Result := Copy(Str, TextStart, TextEnd - TextStart + 1);
end;

///  <summary>Trims characters from the left hand end of a string.</summary>
///  <remarks>Anonymous function TrimFn determines whether a character is to be
///  trimmed.</remarks>
function InternalTrimLeft(const Str: UnicodeString;
  const TrimFn: TFunc<Char,Boolean>): UnicodeString;
var
  TextStart: Integer; // position of start of non-space text
begin
  TextStart := 1;
  while (TextStart <= Length(Str))
    and IsTrimmableChar(Str, TextStart, TrimFn) do
    Inc(TextStart);
  Result := Copy(Str, TextStart, MaxInt);
end;

///  <summary>Trims characters from the right hand end of a string.</summary>
///  <remarks>Anonymous function TrimFn determines whether a character is to be
///  trimmed.</remarks>
function InternalTrimRight(const Str: UnicodeString;
  const TrimFn: TFunc<Char,Boolean>): UnicodeString;
var
  TextEnd: Integer;   // position of end of non-space text
begin
  TextEnd := Length(Str);
  while (TextEnd > 0) and IsTrimmableChar(Str, TextEnd, TrimFn) do
    Dec(TextEnd);
  Result := Copy(Str, 1, TextEnd);
end;

{ Public routines }

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

function StrCompareStr(const Left, Right: UnicodeString): Integer;
begin
  Result := SysUtils.AnsiCompareStr(Left, Right);
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

function StrContainsText(const Needle, Haystack: UnicodeString): Boolean;
begin
  Result := StrUtils.AnsiContainsText(Haystack, Needle);
end;

function StrMatchText(const Str: string; const Values: array of string):
  Boolean;
begin
  Result := StrUtils.MatchText(Str, Values);
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
      Head := StrTrim(Head);
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
  Idx: Integer;       // loops thru all items in string list
  FirstItem: Boolean; // flag true until first item has been added to result
begin
  Result := '';
  FirstItem := True;
  for Idx := 0 to Pred(SL.Count) do
  begin
    if (SL[Idx] <> '') or AllowEmpty then
    begin
      if FirstItem then
      begin
        Result := SL[Idx];
        FirstItem := False;
      end
      else
        Result := Result + Delim + SL[Idx];
    end;
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
var
  Sentence: UnicodeString;            // sentence text without trailing spaces
  TrailingWhiteSpace: UnicodeString;  // records trailing white space from Str
begin
  Sentence := StrTrimRight(Str);
  TrailingWhiteSpace := StrSliceRight(Str, Length(Str) - Length(Sentence));
  if Sentence = '' then
    Exit(TrailingWhiteSpace);
  if not StrIsDelimiter(sSentenceClosers, Sentence, Length(Sentence)) then
    Sentence := Sentence + sFullStop;
  Result := Sentence + TrailingWhiteSpace;
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

function StrQuoteSpaced(const Str: UnicodeString; const Quote: Char):
  UnicodeString;
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
  if SubStr = '' then
    Exit(False);
  Result := StrUtils.AnsiStartsStr(SubStr, Str);
end;

function StrStartsText(const SubStr, Str: UnicodeString): Boolean;
begin
  if SubStr = '' then
    Exit(False);
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

function StrTrim(const Str: UnicodeString): UnicodeString;
begin
  Result := InternalTrim(Str, TCharacter.IsWhiteSpace);
end;

function StrTrimChars(const Str: UnicodeString; const C: Char): UnicodeString;
begin
  Result := InternalTrim(
    Str,
    function (Ch: Char): Boolean begin Result := Ch = C; end
  );
end;

function StrTrimLeft(const Str: UnicodeString): UnicodeString;
begin
  Result := InternalTrimLeft(Str, TCharacter.IsWhiteSpace);
end;

function StrTrimLeftChars(const Str: UnicodeString; const C: Char):
  UnicodeString;
begin
  Result := InternalTrimLeft(
    Str,
    function (Ch: Char): Boolean begin Result := Ch = C; end
  );
end;

function StrTrimRight(const Str: UnicodeString): UnicodeString;
begin
  Result := InternalTrimRight(Str, TCharacter.IsWhiteSpace);
end;

function StrTrimRightChars(const Str: UnicodeString; const C: Char):
  UnicodeString; overload;
begin
  Result := InternalTrimRight(
    Str,
    function (Ch: Char): Boolean begin Result := Ch = C; end
  );
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

function StrWrap(const Paras: TStrings; const MaxLineLen, Margin: Integer;
  const SeparateParas: Boolean): UnicodeString; overload;
var
  Para: string;
  SB: TStringBuilder;
begin
  SB := TStringBuilder.Create;
  try
    for Para in Paras do
    begin
      SB.AppendLine(StrTrimRight(StrWrap(Para, MaxLineLen, Margin)));
      if SeparateParas then
        SB.AppendLine;
    end;
    Result := SB.ToString;
  finally
    SB.Free;
  end;
end;

function StrIf(const Condition: Boolean; const TrueStr, FalseStr: string):
  string;
begin
  if Condition then
    Result := TrueStr
  else
    Result := FalseStr;
end;

function StrBackslashEscape(const S, Escapable, Escapes: string): string;
const
  EscChar = '\';       // the C escape character
var
  EscCount: Integer;    // count of escaped characters in string
  Ch: Char;             // each character in string
  PRes: PChar;          // points to chars in result string
  EscCharPos: Integer;  // position of esc chars in EscapeChars & EscapableChars
begin
  // Check for empty string and treat specially (empty string crashes main code)
  if S = '' then
  begin
    Result := '';
    Exit;
  end;
  // Count escapable characters in string
  EscCount := 0;
  for Ch in S do
  begin
    if StrContainsStr(Ch, Escapable) then
      Inc(EscCount);
  end;
  // Set size of result string and get pointer to it
  SetLength(Result, Length(S) + EscCount);
  PRes := PChar(Result);
  // Replace escapable chars with the escaped version
  for Ch in S do
  begin
    EscCharPos := StrPos(Ch, Escapable);
    if EscCharPos > 0 then
    begin
      PRes^ := EscChar;
      Inc(PRes);
      PRes^ := Escapes[EscCharPos];
    end
    else
      PRes^ := Ch;
    Inc(PRes);
  end;
end;

end.


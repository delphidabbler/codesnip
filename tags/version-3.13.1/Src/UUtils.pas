{
 * UUtils.pas
 *
 * General utility routines.
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
 * The Original Code is UUtils.pas
 *
 * The Initial Developer of the Original Code is Peter Johnson
 * (http://www.delphidabbler.com/).
 *
 * Portions created by the Initial Developer are Copyright (C) 2005-2010 Peter
 * Johnson. All Rights Reserved.
 *
 * Contributor(s)
 *   NONE
 *
 * ***** END LICENSE BLOCK *****
}


unit UUtils;


interface


uses
  // Delphi
  Classes;


procedure CopyFile(const Source, Dest: string);
  {Copies a file, preserving modification date.
    @param Source [in] Name of source file.
    @param Dest [in] Name of destination file.
  }

function FileAge(const FileName: string): Integer;
  {Gets the OS time stamp for a file.
    @param FileName [in] Name of file.
    @return Required OS time stamp or -1 if file does not exist.
  }

function DeleteFiles(const Dir, Wildcard: string): Integer;
  {Deletes all files in a directory that match a wildcard. Sub-directories are
  not deleted.
    @param Dir [in] Directory containing files to be deleted.
    @param Wildcard [in] Wildcard specifying files to be deleted - *.* assumed
      if ''.
    @return Number of files deleted.
  }

procedure EnsureFolders(const Folder: string);
  {Ensures that a folder and all its subfolder exist.
    @param Folder [in] Fully specified name of folder.
  }

function ListFiles(const Dir, Wildcard: string; const List: TStrings;
  IncludeDirs: Boolean = True): Boolean;
  {Gets a list of the files and sub-directories of a directory that match a
  wild card.
    @param Dir [in] Directory to be listed.
    @param Wildcard [in] Wildcard of files to be listed.
    @param List [in] Receives directory listing.
    @param IncludeDirs [in] Flag true if sub-directory names to be included in
      List, False if true files only are required.
    @return True if Dir is a valid directory.
  }

function CapitaliseWords(const S: string): string;
  {Capitalises each word in a string, leaving case of other characters
  unchanged.
    @param S [in] String to be converted.
    @return Capitalised string.
  }

function CompressWhiteSpace(const S: string): string;
  {Compresses white space in a string. All sequences of white space are replaced
  by a single space.
    @param S [in] String containing uncompressed white space.
    @return String with whitespace compressed.
  }

function StripWhiteSpace(const S: string): string;
  {Removes all white space from a string.
    @param S [in] String from which white space is to be removed.
    @return String with whitespace removed.
  }

function ExplodeStr(S: string; const Delim: string; const List: TStrings;
  const AllowEmpty: Boolean = True; const TrimStrs: Boolean = False): Integer;
  {Splits a delimited string into a list of sub-strings separated by a
  delimiter.
    @param S [in] String to be split.
    @param Delim [in] String that delimits sub strings.
    @param List [in] Receives split strings.
    @param AllowEmpty [in] True if empty sub strings are to be included in list.
    @param TrimStrs [in] Determines whether strings are trimmed of trailing and
      leading spaces before adding to list. Can mean a string of spaces is
      ignored if AllowEmpty is True.
    @return Number of strings in List.
  }

function JoinStr(const SL: TStrings; const Delim: string;
  const AllowEmpty: Boolean = True): string;
  {Joins all strings in a string list together into a single delimited string.
    @param SL [in] List of strings to be joined.
    @param Delim [in] String to use to delimit strings.
    @param AllowEmpty [in] True if empty strings are to be included in output.
    @return Joined string.
  }

function LongToShortFilePath(const LongName: string): string;
  {Converts a long file name to the equivalent shortened DOS style 8.3 path.
    @param LongName [in] Long file name to be converted.
    @return Short file name.
  }

function FileToString(const FileName: string): string;
  {Stores content of a file in a string.
    @param FileName [in] Name of file to be read.
    @return String containing file contents.
  }

procedure StringToFile(const Str, FileName: string);
  {Writes a string to a text file.
    @param Str [in] String to be written to file.
    @param FileName [in] Name of file to receive string.
  }

function IsDirectory(const DirName: string): Boolean;
  {Checks if a directory exists.
    @param DirName [in] Name of directory to check.
    @return True if DirName is valid directory.
  }

function QuoteSpacedString(const S: string; const Quote: Char = '"'): string;
  {Surrounds a string in quotes if it contains spaces.
    @param S [in] String to be quoted.
    @param Quote [in] Quote character.
    @return Original string, surrounded by quotes only if it contains spaces.
  }

function FloatToInt(const F: Double): Int64;
  {Converts a floating point number to an integer, rounding to nearest integer.
    @param F [in] Floating point number to be rounded off.
    @return Rounded value as integer.
  }

function MySQLDateToDateTime(const MySQLDate: string): TDateTime;
  {Converts a date in MySQL format into a TDateTime.
    @param MySQLDate [in] Date string in format YYYY-MM-DD.
    @return Binary date value.
  }

function DateStamp: string;
  {Creates a date stamp in RFC1123 format
    @return Current date and time as date stamp in UTC/GMT.
  }

function MakeSentence(const Txt: string): string;
  {Checks if text forms a valid sentence, i.e. it ends with a full stop, a
  question mark or an exclamation mark. If not a full stop is added to the text.
    @param Txt [in] Text to be made into sentence.
    @return Valid sentence.
  }

procedure GetIntf(const Instance: IInterface; const IID: TGUID; out Intf);
  {Get a desired interface pointer to an object instance.
    @param Instance [in] IInterface of instance for which an interface is
      requested. May be nil.
    @param IID [in] Identifier of required interface.
    @param Intf [out] Set to required interface pointer if Instance supports
      interface, or nil if interface not supported or Instance is nil.
  }

function LastPos(const SubStr, Str: string): Integer;
  {Finds position of the last occurence of a sub string in a string.
    @param SubStr [in] String to search for.
    @param Str [in] String in which to search.
    @return Index of last occurence of SubStr in Str or 0 if SubStr is not is
      Str.
  }

function TrimChar(const S: string; const C: Char): string;
  {Trims characters from both ends of a string.
    @param S [in] String to be trimmed.
    @param C [in] Character to be trimmed from string.
    @return Trimmed string.
  }

function UnixLineBreaks(const S: string): string;
  {Converts all DOS and Mac line ends to Unix line ends.
    @param S [in] String to be converted.
    @return Converted string.
  }

function IsBaseFileName(const FileName: string): Boolean;
  {Checks if a file name is a base file name (i.e. contains no path
  information).
    @param FileName [in] File name to be tested.
    @return True if file is a base file name, False otherwise.
  }

procedure Pause(const ADelay: Cardinal);
  {Pauses for a specified number of milliseconds before returning. Performs a
  busy wait.
    @param ADelay [in] Number of milliseconds to pause.
  }

function CountDelims(const S, Delims: string): Integer;
  {Counts occurences of delimiters in a string.
    @param S [in] String containing delimiters.
    @param Delims [in] String containing delimiters to be counted. Delimiters
      must be single-byte characters.
  }

function TextWrap(const Text: string; const Width, Margin: Integer): string;
  {Word wraps text to a specified maximum width and pads left each line with
  spaces to offset lines to a specified margin.
    @param Text [in] Text to be word wrapped.
    @param Width [in] Maximum width of a line of text.
    @param Margin [in] Left margin for wrapped text.
    @return Word wrapped text.
  }

function IsValidDriveLetter(const C: Char): Boolean;
  {Checks if a character is a valid Windows drive letter.
    @param C [in] Character to be tested.
    @return True if C is a valid drive letter, False otherwise.
  }

function IsValidAbsoluteFileName(const FileName: string): Boolean;
  {Checks if a filename is a valid, complete, absolute local file path.
    @param FileName [in] File name to be checked.
    @return True if file name is valid absolute file path, false if not.
  }

function IsValidUNCFileName(const FileName: string): Boolean;
  {Checks if a filename is a valid, complete, UNC file name.
    @param FileName [in] File name to be checked.
    @return True if file name is valid UNC name, false if not.
  }

procedure KeyErrorBeep;
  {Emits a sound indicating a keypress error.
  }

function IsHexDigit(C: Char): Boolean;
  {Checks whether a character is defined as a hex digit.
    @param C [in] Character to be tested.
    @return True if character is a hex digit, False if not.
  }


implementation


uses
  // Delphi
  SysUtils, StrUtils, Windows, ShlObj, ActiveX, Messages, Character,
  // Project
  UConsts;


procedure CopyFile(const Source, Dest: string);
  {Copies a file, preserving modification date.
    @param Source [in] Name of source file.
    @param Dest [in] Name of destination file.
  }
var
  SourceStream, DestStream: TFileStream;  // source and dest file streams
begin
  DestStream := nil;
  SourceStream := TFileStream.Create(Source, fmOpenRead or fmShareDenyWrite);
  try
    DestStream := TFileStream.Create(Dest, fmCreate or fmShareExclusive);
    DestStream.CopyFrom(SourceStream, SourceStream.Size);
    // Set dest file's modification date to same as source file
    FileSetDate(DestStream.Handle, FileGetDate(SourceStream.Handle));
  finally
    FreeAndNil(DestStream);
    FreeAndNil(SourceStream);
  end;
end;

function FileAge(const FileName: string): Integer;
  {Gets the OS time stamp for a file.
    @param FileName [in] Name of file.
    @return Required OS time stamp or -1 if file does not exist.
  }
var
  FH: Integer;  // file handle
begin
  // This function is provided to avoid using FileAge unit in SysUtils since
  // the routine is deprecated in Delphi 2006
  Result := -1;
  if IsDirectory(FileName) then
    Exit;
  FH := FileOpen(FileName, fmOpenRead or fmShareDenyNone);
  if FH <> -1 then
  begin
    Result := FileGetDate(FH);
    FileClose(FH);
  end;
end;

function DeleteFiles(const Dir, Wildcard: string): Integer;
  {Deletes all files in a directory that match a wildcard. Sub-directories are
  not deleted.
    @param Dir [in] Directory containing files to be deleted.
    @param Wildcard [in] Wildcard specifying files to be deleted - *.* assumed
      if ''.
    @return Number of files deleted.
  }
var
  Files: TStringList; // stores files to be deleted
  I: Integer;         // loops thru files in folder
  AFile: string;      // a file to be deleted
  Path: string;       // path to directory
  Attr: Integer;      // attributes of a file
begin
  Result := 0;
  Files := TStringList.Create;
  try
    // Get matching list of files / folders in directory
    if not ListFiles(Dir, Wildcard, Files) then
      Exit;
    Path := IncludeTrailingPathDelimiter(Dir);
    for I := 0 to Pred(Files.Count) do
    begin
      AFile := Path + Files[I];
      Attr := FileGetAttr(AFile);
      // Delete file if it is not a directory
      if (Attr and faDirectory = 0) then
      begin
        if SysUtils.DeleteFile(AFile) then
          // File deleted: count it
          Inc(Result);
      end;
    end;
  finally
    FreeAndNil(Files);
  end;
end;

procedure EnsureFolders(const Folder: string);
  {Ensures that a folder and all its subfolder exist.
    @param Folder [in] Fully specified name of folder.
  }
begin
  // Check there's a folder to create: ForceDirectories raises exception if
  // passed empty string as parameter
  if Length(Folder) = 0 then
    Exit;
  // Create the folders
  ForceDirectories(Folder);
end;

function IsDirectory(const DirName: string): Boolean;
  {Checks if a directory exists.
    @param DirName [in] Name of directory to check.
    @return True if DirName is valid directory.
  }
begin
  Result := DirectoryExists(DirName);
end;

function ListFiles(const Dir, Wildcard: string; const List: TStrings;
  IncludeDirs: Boolean = True): Boolean;
  {Gets a list of the files and sub-directories of a directory that match a
  wild card.
    @param Dir [in] Directory to be listed.
    @param Wildcard [in] Wildcard of files to be listed.
    @param List [in] Receives directory listing.
    @param IncludeDirs [in] Flag true if sub-directory names to be included in
      List, False if true files only are required.
    @return True if Dir is a valid directory.
  }
var
  FileSpec: string;   // full file spec of a wildcard
  Path: string;       // full path of directory, including training backslash
  SR: TSearchRec;     // file search result
  Success: Integer;   // success code for FindXXX routines
const
  faVolumeId = $00000008; // redefined from SysUtils to avoid deprecated warning
begin
  Assert(Assigned(List), 'ListFiles: List is nil');
  // Check if true directory and exit if not
  Result := IsDirectory(Dir);
  if not Result then
    Exit;
  // Build FileSpec from directory and wildcard
  FileSpec := IncludeTrailingPathDelimiter(Dir);
  if Wildcard = '' then
    FileSpec := FileSpec + '*.*'
  else
    FileSpec := FileSpec + Wildcard;
  Path := IncludeTrailingPathDelimiter(Dir);
  // Do search
  Success := FindFirst(FileSpec, faAnyFile, SR);
  try
    while Success = 0 do
    begin
      // only add true files or directories to list
      if (SR.Name <> '.') and (SR.Name <> '..')
        and (SR.Attr and faVolumeId = 0)
        and (IncludeDirs or not IsDirectory(Path + SR.Name)) then
        List.Add(SR.Name);
      Success := FindNext(SR);
    end;
  finally
    SysUtils.FindClose(SR);
  end;
end;

function CapitaliseWords(const S: string): string;
  {Capitalises each word in a string, leaving case of other characters
  unchanged.
    @param S [in] String to be converted.
    @return Capitalised string.
  }
var
  Idx: Integer;           // loops through each character in string
  WantCapital: Boolean;   // flag indicating whether captial letter required
begin
  Result := S;
  WantCapital := True;
  for Idx := 1 to Length(S) do
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

function CompressWhiteSpace(const S: string): string;
  {Compresses white space in a string. All sequences of white space are replaced
  by a single space.
    @param S [in] String containing uncompressed white space.
    @return String with whitespace compressed.
  }
var
  Idx: Integer;       // loops thru all characters in string
  ResCount: Integer;  // counts number of characters in result string
  PRes: PChar;        // pointer to characters in result string
begin
  // Set length of result to length of source string and set pointer to it
  SetLength(Result, Length(S));
  PRes := PChar(Result);
  // Reset count of characters in result string
  ResCount := 0;
  // Loop thru characters of source string
  Idx := 1;
  while Idx <= Length(S) do
  begin
    if TCharacter.IsWhiteSpace(S[Idx]) then
    begin
      // Current char is white space: replace by space char and count it
      PRes^ := ' ';
      Inc(PRes);
      Inc(ResCount);
      // Skip past any following white space
      Inc(Idx);
      while TCharacter.IsWhiteSpace(S[Idx]) do
        Inc(Idx);
    end
    else
    begin
      // Current char is not white space: copy it literally and count it
      PRes^ := S[Idx];
      Inc(PRes);
      Inc(ResCount);
      Inc(Idx);
    end;
  end;
  // Reduce length of result string if it is shorter than source string
  if ResCount < Length(S) then
    SetLength(Result, ResCount);
end;

function StripWhiteSpace(const S: string): string;
  {Removes all white space from a string.
    @param S [in] String from which white space is to be removed.
    @return String with whitespace removed.
  }
var
  Idx: Integer;       // loops thru all characters in string
  ResCount: Integer;  // counts number of characters in result string
  PRes: PChar;        // pointer to characters in result string
begin
  // Set length of result to length of source string and set pointer to it
  SetLength(Result, Length(S));
  PRes := PChar(Result);
  // Reset count of characters in result string
  ResCount := 0;
  // Loop thru characters of source string
  Idx := 1;
  while Idx <= Length(S) do
  begin
    if not TCharacter.IsWhiteSpace(S[Idx]) then
    begin
      // Character is not white space: copy to result string
      PRes^ := S[Idx];
      Inc(ResCount);
      Inc(PRes);
    end;
    Inc(Idx);
  end;
  // Reduce length of result string if it is shorter than source string
  if ResCount < Length(S) then
    SetLength(Result, ResCount);
end;

function SplitStr(const S: string; const Delim: string;
  out S1, S2: string): Boolean;
  {Splits the string S at the first occurence of a delimiter.
    @param S [in] String to be split.
    @param Delim [in] Delimiter separating sub strings.
    @param S1 [out] Sub string preceeding first delimiter or whole string if
      delimiter not in string.
    @param S2 [out] Sub string following delimiter or '' if delimiter not in
      string.
    @return True if delimiter was found in string, False otherwise.
  }
var
  DelimPos: Integer;  // position of delimiter in source string
begin
  // Find position of first occurence of delimiter in string
  DelimPos := AnsiPos(Delim, S);
  if DelimPos > 0 then
  begin
    // Delimiter found: split string at delimiter
    S1 := Copy(S, 1, DelimPos - 1);
    S2 := Copy(S, DelimPos + Length(Delim), MaxInt);
    Result := True;
  end
  else
  begin
    // Delimiter not found: set S1 to whole string
    S1 := S;
    S2 := '';
    Result := False;
  end;
end;

function ExplodeStr(S: string; const Delim: string; const List: TStrings;
  const AllowEmpty: Boolean = True; const TrimStrs: Boolean = False): Integer;
  {Splits a delimited string into a list of sub-strings separated by a
  delimiter.
    @param S [in] String to be split.
    @param Delim [in] String that delimits sub strings.
    @param List [in] Receives split strings.
    @param AllowEmpty [in] True if empty sub strings are to be included in list.
    @param TrimStrs [in] Determines whether strings are trimmed of trailing and
      leading spaces before adding to list. Can mean a string of spaces is
      ignored if AllowEmpty is True.
    @return Number of strings in List.
  }
var
  Item: string;       // current delimited text
  Remainder: string;  // remaining unconsumed part of string

  // ---------------------------------------------------------------------------
  procedure ProcessItem;
    {Modifies current string as necessary item and adds to list if required.
    }
  begin
    if TrimStrs then
      Item := Trim(Item);
    if (Item <> '') or AllowEmpty then
      List.Add(Item)
  end;
  // ---------------------------------------------------------------------------

begin
  // Clear the list
  List.Clear;
  // Check we have some entries in the string
  if S <> '' then
  begin
    // Repeatedly split string until we have no more entries
    while SplitStr(S, Delim, Item, Remainder) do
    begin
      ProcessItem;
      // Go round again with remainder of string
      S := Remainder;
    end;
    // Deal with item after last delimiter, if any
    ProcessItem;
  end;
  // Return number of items added
  Result := List.Count;
end;

function JoinStr(const SL: TStrings; const Delim: string;
  const AllowEmpty: Boolean = True): string;
  {Joins all strings in a string list together into a single delimited string.
    @param SL [in] List of strings to be joined.
    @param Delim [in] String to use to delimit strings.
    @param AllowEmpty [in] True if empty strings are to be included in output.
    @return Joined string.
  }
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

function LongToShortFilePath(const LongName: string): string;
  {Converts a long file name to the equivalent shortened DOS style 8.3 path.
    @param LongName [in] Long file name to be converted.
    @return Short file name.
  }
begin
  SetLength(Result, MAX_PATH);
  SetLength(Result, GetShortPathName(PChar(LongName), PChar(Result), MAX_PATH));
end;

function StreamToString(const Stm: TStream): string;
  {Reads content of a stream into a string. Stream is read from current
  position.
    @param Stm [in] Stream to be read.
    @return String containing stream contents.
  }
var
  SS: TStringStream;  // used to copy stream to string
begin
  SS := TStringStream.Create('');
  try
    SS.CopyFrom(Stm, 0);
    Result := SS.DataString;
  finally
    SS.Free;
  end;
end;

function FileToString(const FileName: string): string;
  {Stores content of a file in a string.
    @param FileName [in] Name of file to be read.
    @return String containing file contents.
  }
var
  FS: TFileStream;  // stream used to read file
begin
  FS := TFileStream.Create(FileName, fmOpenRead or fmShareDenyNone);
  try
    Result := StreamToString(FS);
  finally
    FS.Free;
  end;
end;

procedure StringToStream(const Str: string; const Stm: TStream);
  {Writes a string into a stream. The string is written at the current
  stream position.
    @param Str [in] String to be written to stream.
    @param Stm [in] Stream to receive string.
  }
var
  SS: TStringStream;  // used to copy string to stream
begin
  SS := TStringStream.Create(Str);
  try
    Stm.CopyFrom(SS, SS.Size);
  finally
    FreeAndNil(SS);
  end;
end;

procedure StringToFile(const Str, FileName: string);
  {Writes a string to a text file.
    @param Str [in] String to be written to file.
    @param FileName [in] Name of file to receive string.
  }
var
  FS: TFileStream;  // stream used to write file
begin
  FS := TFileStream.Create(FileName, fmCreate);
  try
    StringToStream(Str, FS);
  finally
    FreeAndNil(FS);
  end;
end;

function ContainsDelims(const S, Delimiters: string): Boolean;
  {Checks if a string contains any specified delimiter characters.
    @param S [in] String to be checked.
    @param Delimiters [in] String of delimiter characters.
    @return True if string contains any delimiters or false if not.
  }
var
  DelimIdx: Integer;  // loops thru delimiters
begin
  Result := False;
  for DelimIdx := 1 to Length(Delimiters) do
    if AnsiContainsStr(S, Delimiters[DelimIdx]) then
    begin
      Result := True;
      Break;
    end;
end;

function ContainsWhiteSpace(const S: string): Boolean;
  {Checks if a string contains white space.
    @param S [in] string to be checked.
    @return True if string contains spaces.
  }
var
  Ch: Char;   // scans through string S
begin
  Result := False;
  for Ch in S do
  begin
    if TCharacter.IsWhiteSpace(Ch) then
    begin
      Result := True;
      Break;
    end;
  end;
end;

function QuoteSpacedString(const S: string; const Quote: Char = '"'): string;
  {Surrounds a string in quotes if it contains spaces.
    @param S [in] String to be quoted.
    @param Quote [in] Quote character.
    @return Original string, surrounded by quotes only if it contains spaces.
  }
begin
  if ContainsWhiteSpace(S) then
    Result := Quote + S + Quote
  else
    Result := S;
end;

function FloatToInt(const F: Double): Int64;
  {Converts a floating point number to an integer, rounding to nearest integer.
    @param F [in] Floating point number to be rounded off.
    @return Rounded value as integer.
  }
begin
  Result := Trunc(F + 0.500001);
end;

function MySQLDateToDateTime(const MySQLDate: string): TDateTime;
  {Converts a date in MySQL format into a TDateTime.
    @param MySQLDate [in] Date string in format YYYY-MM-DD.
    @return Binary date value.
  }
begin
  Result := EncodeDate(
    StrToInt(Copy(MySQLDate, 1, 4)),
    StrToInt(Copy(MySQLDate, 6, 2)),
    StrToInt(Copy(MySQLDate, 9, 2))
  );
end;

function DateStamp: string;
  {Creates a date stamp in RFC1123 format
    @return Current date and time as date stamp in UTC/GMT.
  }
const
  // Pattern to create RFC1123 date formats
  cRFC1123Pattern = 'ddd, dd mmm yyyy HH'':''nn'':''ss ''GMT''';
var
  ST: TSystemTime;  // system time
begin
  // This Windows API function gets system time in UTC/GTM
  // see http://msdn.microsoft.com/en-us/library/ms724390
  GetSystemTime(ST);
  // Format system time in RFC1123 format
  Result := FormatDateTime(cRFC1123Pattern, SystemTimeToDateTime(ST));
end;

function MakeSentence(const Txt: string): string;
  {Checks if text forms a valid sentence, i.e. it ends with a full stop, a
  question mark or an exclamation mark. If not a full stop is added to the text.
    @param Txt [in] Text to be made into sentence.
    @return Valid sentence.
  }
begin
  if IsDelimiter('.!?', Txt, Length(Txt)) then
    Result := Txt
  else
    Result := Txt + '.'
end;

procedure GetIntf(const Instance: IInterface; const IID: TGUID; out Intf);
  {Get a desired interface pointer to an object instance.
    @param Instance [in] IInterface of instance for which an interface is
      requested. May be nil.
    @param IID [in] Identifier of required interface.
    @param Intf [out] Set to required interface pointer if Instance supports
      interface, or nil if interface not supported or Instance is nil.
  }
begin
  if not Supports(Instance, IID, Intf) then
    Pointer(Intf) := nil;
end;

function LastPos(const SubStr, Str: string): Integer;
  {Finds position of the last occurence of a sub string in a string.
    @param SubStr [in] String to search for.
    @param Str [in] String in which to search.
    @return Index of last occurence of SubStr in Str or 0 if SubStr is not is
      Str.
  }
var
  Idx: Integer; // an index of SubStr in Str
begin
  Result := 0;
  Idx := AnsiPos(SubStr, Str);
  if Idx = 0 then
    Exit;
  while Idx > 0 do
  begin
    Result := Idx;
    Idx := PosEx(SubStr, Str, Idx + 1);
  end;
end;

function TrimLeftChar(const S: string; const C: Char): string;
  {Trims characters from the start of a string.
    @param S [in] String to be trimmed.
    @param C [in] Character to be trimmed from string.
    @return Trimmed string.
  }
var
  Idx: Integer; // index into string
begin
  Idx := 1;
  while (Idx <= Length(S)) and (S[Idx] = C) do
    Inc(Idx);
  if Idx > 1 then
    Result := Copy(S, Idx, MaxInt)
  else
    Result := S;
end;

function TrimRightChar(const S: string; const C: Char): string;
  {Trims characters from the end of string.
    @param S [in] String to be trimmed.
    @param C [in] Character to be trimmed from string.
    @return Trimmed string.
  }
var
  Idx: Integer; // index into string
begin
  Idx := Length(S);
  while (Idx >= 1) and (S[Idx] = C) do
    Dec(Idx);
  if Idx < Length(S) then
    Result := Copy(S, 1, Idx)
  else
    Result := S;
end;

function TrimChar(const S: string; const C: Char): string;
  {Trims characters from both ends of a string.
    @param S [in] String to be trimmed.
    @param C [in] Character to be trimmed from string.
    @return Trimmed string.
  }
begin
  Result := TrimLeftChar(TrimRightChar(S, C), C);
end;

function UnixLineBreaks(const S: string): string;
  {Converts all DOS and Mac line ends to Unix line ends.
    @param S [in] String to be converted.
    @return Converted string.
  }
begin
  // Replace any CRLF (MSDOS/Windows) line ends with LF
  Result := ReplaceStr(S, EOL, LF);
  // Replace any remaining CR (Mac) line ends with LF
  Result := ReplaceStr(Result, CR, LF);
end;

function IsBaseFileName(const FileName: string): Boolean;
  {Checks if a file name is a base file name (i.e. contains no path
  information).
    @param FileName [in] File name to be tested.
    @return True if file is a base file name, False otherwise.
  }
begin
  Result := ExtractFileName(FileName) = FileName;
end;

procedure ProcessMessages;
  {Processes all the messages a program's message queue.
  }
var
  Msg: TMsg;  // stores message peeked from message loop
begin
  while PeekMessage(Msg, 0, 0, 0, PM_REMOVE) do
  begin
    if Msg.Message <> WM_QUIT then
    begin
      TranslateMessage(Msg);
      DispatchMessage(Msg);
    end
    else
      Exit;
  end;
end;

procedure Pause(const ADelay: LongWord);
  {Pauses for a specified number of milliseconds before returning. Performs a
  busy wait.
    @param ADelay [in] Number of milliseconds to pause.
  }
var
  StartTC: DWORD;   // tick count when routine called
  CurrentTC: Int64; // tick count at each loop iteration
begin
  StartTC := GetTickCount;
  repeat
    ProcessMessages;
    CurrentTC := GetTickCount;
    if CurrentTC < StartTC then
      // tick count has wrapped around: adjust it
      CurrentTC := CurrentTC + High(DWORD);
  until CurrentTC - StartTC >= ADelay;
end;

function CountDelims(const S, Delims: string): Integer;
  {Counts occurences of delimiters in a string.
    @param S [in] String containing delimiters.
    @param Delims [in] String containing delimiters to be counted. Delimiters
      must be single-byte characters.
  }
var
  Idx: Integer; //loops thru all characters in string
begin
  Result := 0;
  for Idx := 1 to Length(S) do
    if IsDelimiter(Delims, S, Idx) then
      Inc(Result);
end;

function TextWrap(const Text: string; const Width, Margin: Integer): string;
  {Word wraps text to a specified maximum width and pads left each line with
  spaces to offset lines to a specified margin.
    @param Text [in] Text to be word wrapped.
    @param Width [in] Maximum width of a line of text.
    @param Margin [in] Left margin for wrapped text.
    @return Word wrapped text.
  }
var
  Word: string;         // next word in input text
  Line: string;         // current output line
  Words: TStringList;   // list of words in input text
  I: Integer;           // loops thru all words in input text

  // -------------------------------------------------------------------------
  procedure AddLine(const Line: string);
    {Adds a line of text to output, offsetting line by width of margin.
      @param Line [in] Line being output.
    }
  begin
    if Result <> '' then    // not first line: insert new line
      Result := Result + EOL;
    Result := Result + StringOfChar(' ', Margin) + Line;
  end;
  // -------------------------------------------------------------------------

begin
  // Get all words in text
  Words := TStringList.Create;
  try
    ExplodeStr(Text, ' ', Words);
    Result := '';
    Line := '';
    // Loop for each word in text
    for I := 0 to Pred(Words.Count) do
    begin
      Word := Words[I];
      if Length(Line) + Length(Word) + 1 <= Width then
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
        AddLine(Line);  // output line
        Line := Word;   // store word as first on next line
      end;
    end;
    if Line <> '' then
      // Residual line after end of loop: add to output
      AddLine(Line);
  finally
    FreeAndNil(Words);
  end;
end;

function IsValidDriveLetter(const C: Char): Boolean;
  {Checks if a character is a valid Windows drive letter.
    @param C [in] Character to be tested.
    @return True if C is a valid drive letter, False otherwise.
  }
begin
  Result := CharInSet(C, ['A'..'Z', 'a'..'z']);
end;

function IsValidAbsoluteFileName(const FileName: string): Boolean;
  {Checks if a filename is a valid, complete, absolute local file path.
    @param FileName [in] File name to be checked.
    @return True if file name is valid absolute file path, false if not.
  }
begin
  Result := (Length(FileName) > 3)
    and IsValidDriveLetter(FileName[1])
    and (FileName[2] = ':') and (FileName[3] = '\');
end;

function IsValidUNCFileName(const FileName: string): Boolean;
  {Checks if a filename is a valid, complete, UNC file name.
    @param FileName [in] File name to be checked.
    @return True if file name is valid UNC name, false if not.
  }
begin
  Result := (Length(FileName) > 5)
    and AnsiStartsStr('\\', FileName)
    and (PosEx('\', FileName, 4) >= 4);
end;

procedure KeyErrorBeep;
  {Emits a sound indicating a keypress error.
  }
begin
  MessageBeep(UINT(-1));
end;

function IsHexDigit(C: Char): Boolean;
  {Checks whether a character is defined as a hex digit.
    @param C [in] Character to be tested.
    @return True if character is a hex digit, False if not.
  }
begin
  Result := CharInSet(C, ['A'..'F', 'a'..'f', '0'..'9']);
end;

end.


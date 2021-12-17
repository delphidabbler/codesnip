{
 * This Source Code Form is subject to the terms of the Mozilla Public License,
 * v. 2.0. If a copy of the MPL was not distributed with this file, You can
 * obtain one at https://mozilla.org/MPL/2.0/
 *
 * Copyright (C) 2005-2020, Peter Johnson (gravatar.com/delphidabbler).
 *
 * General utility routines.
}


unit UUtils;


interface


uses
  // Delphi
  SysUtils, Classes;


///  <summary>Gets the OS time stamp for a file.</summary>
///  <param name="FileName">string [in] Name of file.</param>
///  <returns>Integer. Required OS time stamp or -1 if file does not exist.
///  </returns>
function FileAge(const FileName: string): Integer;

///  <summary>Deletes all files in a directory that match a wildcard.</summary>
///  <param name="Dir">string [in] Directory containing files to be deleted.
///  </param>
///  <param name="Wildcard">string [in] Wildcard specifying files to be deleted.
///  *.* assumed if WildCard=''.</param>
///  <returns>Integer. Number of files deleted.</returns>
///  <remarks>Sub-directories and their files are ignored.</remarks>
function DeleteFiles(const Dir, Wildcard: string): Integer;

///  <summary>Ensures that a folder and all its subfolder exist.</summary>
///  <param name="Folder">string [in] Fully specified name of folder.</param>
///  <remarks>Any folder that does not exist is created.</remarks>
procedure EnsureFolders(const Folder: string);

///  <summary>Gets a list of the files and sub-directories of a directory that
///  match a wild card.</summary>
///  <param name="Dir">string [in] Directory to be listed.</param>
///  <param name="Wildcard">string [in] Wildcard of files to be listed.</param>
///  <param name="List">TStrings [in] Receives directory listing.</param>
///  <param name="IncludeDirs">Boolean [in] Flag true if sub-directory names are
///  to be included in List, False if only files are required.</param>
///  <param name="RelativeNames">Boolean [in] Flag true if file names are to be
///  relative to Dir, False if full paths are to be recorded.</param>
///  <returns>Boolean. True if Dir is a valid directory, False otherwise.
///  </returns>
///  <remarks>
///  <para>Sub-directories are not recursed.</para>
///  <para>File names include the full file path.</para>
///  </remarks>
function ListFiles(const Dir, Wildcard: string; const List: TStrings;
  IncludeDirs: Boolean = True; RelativeNames: Boolean = False): Boolean;

///  <summary>Converts a long file name to the equivalent shortened DOS style
///  8.3 path.</summary>
///  <param name="LongName">string [in] Long file name to be converted.</param>
///  <returns>string. Short file name.</returns>
function LongToShortFilePath(const LongName: string): string;

///  <summary>Checks if a directory exists.</summary>
///  <param name="DirName">string [in] Name of directory to check.</param>
///  <returns>Boolean. True if DirName is valid directory, False otherwise.
///  </returns>
function IsDirectory(const DirName: string): Boolean;

///  <summary>Converts a floating point number to an integer, rounding to
///  nearest integer.</summary>
///  <param name="F">Double [in] Floating point number to be rounded off.
///  </param>
///  <returns>Integer. Rounded value.</returns>
function FloatToInt(const F: Double): Int64;

///  <summary>Creates a date stamp for current date in RFC1123 format.</summary>
///  <returns>string. Required date and time as date stamp in UTC/GMT.</returns>
function RFC1123DateStamp: string; inline;

///  <summary>Returns the current date and time in GMT/UTC.</summary>
function NowGMT: TDateTime;

///  <summary>Converts a date-time value in SQL format into a TDateTime.
///  </summary>
///  <param name="SQLDateTime">string [in] SQL format date-time value to be
///  converted.</param>
///  <returns>TDateTime. Converted value.</returns>
///  <remarks>SQLDate must be in YYYY-MM-DD hh:mm:ss format.</remarks>
function ParseSQLDateTime(const SQLDateTime: string): TDateTime;

///  <summary>Attempts to convert a date-time value in SQL format into a
///  TDateTime value.</summary>
///  <param name="SQLDateTime">string [in] SQL format date-time value to be
///  converted.</param>
///  <param name="Value">TDateTime [out] Set to converted date-time value if
///  conversion succeeded or undefined if coversion failed.</param>
///  <returns>Boolean. True if conversion succeeded, False otherwise.</returns>
function TryParseSQLDateTime(const SQLDateTime: string; out Value: TDateTime):
  Boolean;

///  <summary>Get a desired interface pointer to an object instance.</summary>
///  <param name="Instance">IInterface [in] Instance for which an interface is
///  requested. May be nil.</param>
///  <param name="IID">TGUID [in] Identifier of required interface.</param>
///  <param name="Intf">Untyped [out] Set to required interface pointer if
///  Instance supports interface, or nil if interface not supported or Instance
///  is nil.</param>
procedure GetIntf(const Instance: IInterface; const IID: TGUID; out Intf);

///  <summary>Checks if a file name is a base file name (i.e. contains no path
///  information).</summary>
///  <param name="FileName">string [in] File name to be tested.</param>
///  <returns>Boolean. True if file is a base file name, False otherwise.
///  </returns>
function IsBaseFileName(const FileName: string): Boolean;

///  <summary>Pauses for a specified number of milliseconds before returning.
///  </summary>
///  <param name="ADelay">Cardinal [in] Number of milliseconds to pause.</param>
///  <remarks>Performs a busy wait.</remarks>
procedure Pause(const ADelay: Cardinal);

///  <summary>Checks if a character is a valid Windows drive letter.</summary>
///  <param name="C">Char [in] Character to be tested.</param>
///  <returns>Boolean. True if C is a valid drive letter, False otherwise.
///  </returns>
function IsValidDriveLetter(const C: Char): Boolean;

///  <summary>Emits a sound indicating a keypress error.</summary>
procedure KeyErrorBeep;

///  <summary>Checks whether a character is a valid hex digit.</summary>
///  <param name="C">Char [in] Character to be tested.</param>
///  <returns>Boolean. True if character is a hex digit, False if not.</returns>
function IsHexDigit(C: Char): Boolean;

///  <summary>Gets the base resource name from a forward slash delimited URI.
///  </summary>
///  <param name="URI">string [in] Full URI.</param>
///  <returns>string. Name following last slash in URI.</returns>
function URIBaseName(const URI: string): string;

///  <summary>Attempts to convert string S into a Cardinal value.</summary>
///  <param name="S">string [in] String to be converted.</param>
///  <param name="Value">Cardinal [out] Value of converted string. Undefined if
///  conversion fails.</param>
///  <returns>Boolean. True if conversion succeeds, False if not.</returns>
function TryStrToCardinal(const S: string; out Value: Cardinal): Boolean;

///  <summary>Test a given number of bytes from the start of two byte arrays for
///  equality.</summary>
///  <param name="BA1">TBytes [in] First byte array to be compared.</param>
///  <param name="BA2">TBytes [in] Second byte array to be compared.</param>
///  <param name="Count">Cardinal [in] Number of bytes to be compared. Must be
///  greater than zero.</param>
///  <returns>True if the required number of bytes in the arrays are equal and
///  both arrays have at least Count bytes. Otherwise False is returned.
///  </returns>
///  <remarks>If either BA1 or BA1 have less than Count bytes then False is
///  returned, regardless of whether the arrays are equal.</remarks>
function IsEqualBytes(const BA1, BA2: TBytes; const Count: Cardinal):
  Boolean; overload;

///  <summary>Checks if two byte arrays are equal.</summary>
///  <param name="BA1">TBytes [in] First byte array to be compared.</param>
///  <param name="BA2">TBytes [in] Second byte array to be compared.</param>
///  <returns>True if the two arrays are equal, False if not.</returns>
///  <remarks>If both arrays are empty they are considered equal.</remarks>
function IsEqualBytes(const BA1, BA2: TBytes): Boolean; overload;

///  <summary>Attempts to convert string S into a Word value.</summary>
///  <param name="S">string [in] String to be converted.</param>
///  <param name="W">Cardinal [out] Value of converted string. Undefined if
///  conversion fails.</param>
///  <returns>Boolean. True if conversion succeeds, False if not.</returns>
///  <remarks>String must represent a non-negative integer that is representable
///  as a Word.</remarks>
function TryStrToWord(const S: string; out W: Word): Boolean;


implementation


uses
  // Delphi
  Windows, ShlObj, ActiveX, Messages, Character, Math, DateUtils,
  // Project
  UConsts, UStrUtils;


function FileAge(const FileName: string): Integer;
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
var
  Files: TStringList; // stores files to be deleted
  I: Integer;         // loops thru files in folder
  AFile: string;      // a file to be deleted
  Attr: Integer;      // attributes of a file
begin
  Result := 0;
  Files := TStringList.Create;
  try
    // Get matching list of files / folders in directory
    if not ListFiles(Dir, Wildcard, Files) then
      Exit;
    for I := 0 to Pred(Files.Count) do
    begin
      AFile := Files[I];
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
begin
  // Check there's a folder to create: ForceDirectories raises exception if
  // passed empty string as parameter
  if Length(Folder) = 0 then
    Exit;
  // Create the folders
  ForceDirectories(Folder);
end;

function IsDirectory(const DirName: string): Boolean;
begin
  Result := DirectoryExists(DirName);
end;

function ListFiles(const Dir, Wildcard: string; const List: TStrings;
  IncludeDirs: Boolean = True; RelativeNames: Boolean = False): Boolean;
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
        if RelativeNames then
          List.Add(SR.Name)
        else
          List.Add(Path + SR.Name);
      Success := FindNext(SR);
    end;
  finally
    SysUtils.FindClose(SR);
  end;
end;

function LongToShortFilePath(const LongName: string): string;
begin
  SetLength(Result, MAX_PATH);
  SetLength(Result, GetShortPathName(PChar(LongName), PChar(Result), MAX_PATH));
end;

function FloatToInt(const F: Double): Int64;
begin
  // We don't just use Round() on its own because don't want bankers rounding.
  Result := Round(SimpleRoundTo(F, 0));
end;

function NowGMT: TDateTime;
var
  ST: TSystemTime;
begin
  // This Windows API function gets system time in UTC/GMT
  // see http://msdn.microsoft.com/en-us/library/ms724390
  GetSystemTime(ST);
  Result := SystemTimeToDateTime(ST);
end;

function RFC1123DateStamp: string;
const
  // Pattern to create RFC1123 date formats
  cRFC1123Pattern = 'ddd, dd mmm yyyy HH'':''nn'':''ss ''GMT''';
begin
  Result := FormatDateTime(cRFC1123Pattern, NowGMT);
end;

function ParseSQLDateTime(const SQLDateTime: string): TDateTime;
resourcestring
  sBadDate = '"%s" is not a valid SQL DateTime';
begin
  if not TryParseSQLDateTime(SQLDateTime, Result) then
    raise EConvertError.CreateFmt(sBadDate, [SQLDateTime]);
end;

function TryParseSQLDateTime(const SQLDateTime: string; out Value: TDateTime):
  Boolean;
var
  Year, Month, Day, Hour, Min, Sec: Word;
begin
  if not TryStrToWord(Copy(SQLDateTime, 1, 4), Year) then
    Exit(False);
  if not TryStrToWord(Copy(SQLDateTime, 6, 2), Month) then
    Exit(False);
  if not TryStrToWord(Copy(SQLDateTime, 9, 2), Day) then
    Exit(False);
  if not TryStrToWord(Copy(SQLDateTime, 12, 2), Hour) then
    Exit(False);
  if not TryStrToWord(Copy(SQLDateTime, 15, 2), Min) then
    Exit(False);
  if not TryStrToWord(Copy(SQLDateTime, 18, 2), Sec) then
    Exit(False);
  Result := TryEncodeDateTime(Year, Month, Day, Hour, Min, Sec, 0, Value);
end;

procedure GetIntf(const Instance: IInterface; const IID: TGUID; out Intf);
begin
  if not Supports(Instance, IID, Intf) then
    Pointer(Intf) := nil;
end;

function IsBaseFileName(const FileName: string): Boolean;
begin
  Result := (FileName <> '') and (ExtractFileName(FileName) = FileName);
end;

procedure Pause(const ADelay: LongWord);

  // ---------------------------------------------------------------------------
  ///  Processes all the messages in program's message queue.
  procedure ProcessMessages;
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
  // ---------------------------------------------------------------------------

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

function IsValidDriveLetter(const C: Char): Boolean;
begin
  Result := CharInSet(C, ['A'..'Z', 'a'..'z']);
end;

procedure KeyErrorBeep;
begin
  MessageBeep(UINT(-1));
end;

function IsHexDigit(C: Char): Boolean;
begin
  Result := CharInSet(C, ['A'..'F', 'a'..'f', '0'..'9']);
end;

function URIBaseName(const URI: string): string;
var
  LastSlashPos: Integer;  // position of last '/' in URI
begin
  LastSlashPos := StrLastPos('/', URI);
  if LastSlashPos = 0 then
    Exit(URI);
  Result := StrSliceRight(URI, Length(URI) - LastSlashPos);
end;

function TryStrToCardinal(const S: string; out Value: Cardinal): Boolean;
var
  Value64: Int64; // receives 64 bit value of conversion
begin
  Result := TryStrToInt64(S, Value64)
    and (Int64Rec(Value64).Hi = 0);
  if Result then
    Value := Int64Rec(Value64).Lo;
end;

function IsEqualBytes(const BA1, BA2: TBytes; const Count: Cardinal):
  Boolean;
var
  I: Integer;
begin
  Assert(Count > 0, 'IsEqualBytes: Count must be greater than zero');
  if (Length(BA1) < Int64(Count)) or (Length(BA2) < Int64(Count)) then
    Exit(False);
  for I := 0 to Pred(Count) do
    if BA1[I] <> BA2[I] then
      Exit(False);
  Result := True;
end;

function IsEqualBytes(const BA1, BA2: TBytes): Boolean;
var
  I: Integer;
begin
  if Length(BA1) <> Length(BA2) then
    Exit(False);
  for I := 0 to Pred(Length(BA1)) do
    if BA1[I] <> BA2[I] then
      Exit(False);
  Result := True;
end;

function TryStrToWord(const S: string; out W: Word): Boolean;
var
  I: Integer;
begin
  Result := TryStrToInt(S, I);
  if not Result then
    Exit;
  if (I < 0) or (I > High(Word)) then
    Exit(False);
  W := Word(I);
end;

end.


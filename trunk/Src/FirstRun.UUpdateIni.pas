{
 * FirstRun.UUpdateIni.pas
 *
 * Pascal script for use in [Code] Section of CodeSnip's Install.iss.
 *
 * Creates per-user and application-wide ini files in correct data folders. Can
 * also create the ini files from the information stored in the single ini file
 * used in versions of CodeSnip prior to version 1.9.
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
 * The Original Code is FirstRun.UUpdateIni.pas, formerly UpdateIni.ps.
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

unit FirstRun.UUpdateIni;

interface

// Creates new style per user and common config files from content of single,
// old style config file used before CodeSnip v1.9.
procedure CreateIniFilesFromOldStyle;

// Copies config files of a previous installation to new installation, updating
// to Unicode format if necessary.
procedure CopyConfigFiles(InstallID: Integer);

// Deletes any highlighter preferences from new installation's user config file.
procedure DeleteHighligherPrefs;

// Gets version number of new installation's user config file.
function UserConfigFileVer: Integer;

// Adds Prefs:CodeGen section along with default data to new installation's user
// config file.
procedure CreateDefaultCodeGenEntries;

// Deletes proxt password entry from new installation's user config file.
procedure DeleteProxyPassword;

// Updates both common and user config files with correct version information.
// This records both config file versions and (common config file only) version
// number of program being installed.
procedure StampConfigFiles;

implementation

uses
  SysUtils, Types, Classes, Windows,
  FirstRun.UDataLocations, FirstRun.UUnicode, UAppInfo, UIOUtils;

// TODO: NOTE inserted from snippets database (duplicates FirstRun.UUpdateDBase)
procedure CopyFile(const Source, Dest: string);
var
  SourceStream, DestStream: Classes.TFileStream; // source and dest file streams
begin
  DestStream := nil;
  // Open source and dest file streams
  SourceStream := Classes.TFileStream.Create(
    Source, SysUtils.fmOpenRead or SysUtils.fmShareDenyWrite
  );
  try
    DestStream := Classes.TFileStream.Create(
      Dest, Classes.fmCreate or SysUtils.fmShareExclusive
    );
    // Copy file from source to dest
    DestStream.CopyFrom(SourceStream, SourceStream.Size);
    // Set dest file's modification date to same as source file
    SysUtils.FileSetDate(
      DestStream.Handle, SysUtils.FileGetDate(SourceStream.Handle)
    );
  finally
    // Close files
    DestStream.Free;
    SourceStream.Free;
  end;
end;

// #################### FROM INNO SETUP SOURCE

{ TODO: If any of the Inno setup source is used then move it into it own unit
        that meets with Inno setup license requirements }

function GetIniString(const Section, Key: String; Default: String;
  const Filename: String): String;
var
  BufSize, Len: Integer;
begin
  { On Windows 9x, Get*ProfileString can modify the lpDefault parameter, so
  make sure it's unique and not read-only }
  UniqueString(Default);
  BufSize := 256;
  while True do begin
    SetString(Result, nil, BufSize);
    if Filename <> '' then
      Len := GetPrivateProfileString(PChar(Section), PChar(Key), PChar(Default),
        @Result[1], BufSize, PChar(Filename))
    else
      Len := GetProfileString(PChar(Section), PChar(Key), PChar(Default),
        @Result[1], BufSize);
    { Work around bug present on Windows NT/2000 (not 95): When lpDefault is
    too long to fit in the buffer, nSize is returned (null terminator counted)
    instead of nSize-1 (what it's supposed to return). So don't trust the
    returned length; calculate it ourself.
    Note: This also ensures the string can never include embedded nulls. }
    if Len <> 0 then
      Len := StrLen(PChar(Result));
    { Break if the string fits, or if it's apparently 64 KB or longer.
    No point in increasing buffer size past 64 KB because the length returned by
    Windows 2000 seems to be mod 65536. And Windows 95 returns 0 on values
    longer than ~32 KB. Note: The docs say the function returns "nSize minus
    one" if the buffer is too small, but I'm willing to bet it can be "minus
    two" if the last character is double-byte. Let's just be extremely paranoid
    and check for BufSize-8. }
    if (Len < BufSize-8) or (BufSize >= 65536) then begin
      SetLength(Result, Len);
      Break;
    end;
    { Otherwise double the buffer size and try again }
    BufSize := BufSize * 2;
  end;
end;

function GetIniInt(const Section, Key: String;
  const Default, Min, Max: Longint; const Filename: String): Longint;
{ Reads a Longint from an INI file. If the Longint read is not between Min/Max
then it returns Default. If Min=Max then Min/Max are ignored }
var
  S: String;
  E: Integer;
begin
  S := GetIniString(Section, Key, '', Filename);
  if S = '' then
    Result := Default
  else begin
    Val(S, Result, E);
    if (E <> 0) or ((Min <> Max) and ((Result < Min) or (Result > Max))) then
      Result := Default;
  end;
end;

function SetIniString(const Section, Key, Value, Filename: String): Boolean;
begin
  if Filename <> '' then
    Result := WritePrivateProfileString(PChar(Section), PChar(Key),
      PChar(Value), PChar(Filename))
  else
    Result := WriteProfileString(PChar(Section), PChar(Key),
      PChar(Value));
end;

function SetIniInt(const Section, Key: String; const Value: Longint;
  const Filename: String): Boolean;
begin
  Result := SetIniString(Section, Key, IntToStr(Value), Filename);
end;

procedure DeleteIniSection(const Section, Filename: String);
begin
  if Filename <> '' then
    WritePrivateProfileString(PChar(Section), nil, nil,
      PChar(Filename))
  else
    WriteProfileString(PChar(Section), nil, nil);
end;

// ################### END

// Reads an ANSI config file, converts content to Unicode and writes that to a
// UTF-16LE encoded Unicode config file with BOM. Does nothing if old and new
// file names are the same.
// *** Requires Unicode Inno Setup. ***
procedure CopyAnsiToUnicodeConfigFile(OldFileName, NewFileName: string);
var
  Lines: TStringDynArray;  // lines of text read from ANSI .ini file
begin
  if CompareText(OldFileName, NewFileName) = 0 then
    Exit;
  // reads an ANSI file and converts contents to Unicode, using system default
  // encoding
  Lines := TFileIO.ReadAllLines(OldFileName, TEncoding.Default);
//  LoadStringsFromFile(OldFileName, Lines);
  // writes string array to UTF-16LE file
  ForceDirectories(ExtractFileDir(NewFileName));
  WriteStringsToUnicodeFile(NewFileName, Lines);
end;

// Makes a copy of a Unicode config file with a different name. Does nothing if
// old and new file names are the same.
procedure CopyUnicodeConfigFiles(OldFileName, NewFileName: string);
begin
  if CompareText(OldFileName, NewFileName) = 0 then
    Exit;
  ForceDirectories(ExtractFileDir(NewFileName));
  CopyFile(OldFileName, NewFileName);
end;

// Checks if a config file is ANSI. If False is returned the file is assumed to
// be Unicode.
function IsAnsiConfigFile(InstallID: Integer): Boolean;
begin
  Result := InstallID <= piV3;
end;

// Gets version number of new installation's user config file.
function UserConfigFileVer: Integer;
begin
  Result := GetIniInt('IniFile', 'Version', 1, 0, 0, gCurrentUserConfigFile);
end;

// Create a new empty UTF-16LE encoding config file. Used to ensure the config
// file writing routines write in Unicode. This works because built in Inno
// Setup ini functions call Windows API WritePrivateProfileString which only
// writes Unicode if the file it is writing to is detected as Unicode. If not it
// writes ANSI text.
procedure CreateUnicodeConfigFile(FileName: string);
var
  NulLines: TStringDynArray;   // dummy empty string array
begin
  ForceDirectories(ExtractFileDir(FileName));
  SetLength(NulLines, 0);
  WriteStringsToUnicodeFile(FileName, NulLines);
end;

// Copies config files of a previous installation to new installation, updating
// to Unicode format if necessary.
procedure CopyConfigFiles(InstallID: Integer);
var
  OldCommonConfigFile: string;
  OldUserConfigFile: string;
begin
  OldCommonConfigFile := gCommonConfigFiles[InstallID];
  OldUserConfigFile := gUserConfigFiles[InstallID];
  if OldCommonConfigFile <> '' then
  begin
    if IsAnsiConfigFile(InstallID) then
      CopyAnsiToUnicodeConfigFile(OldCommonConfigFile, gCurrentCommonConfigFile)
    else
      CopyUnicodeConfigFiles(OldCommonConfigFile, gCurrentCommonConfigFile);
  end;
  if OldUserConfigFile <> '' then
  begin
    if IsAnsiConfigFile(InstallID) then
      CopyAnsiToUnicodeConfigFile(OldUserConfigFile, gCurrentUserConfigFile)
    else
      CopyUnicodeConfigFiles(OldUserConfigFile, gCurrentUserConfigFile);
  end;
end;

// Updates both common and user config files with correct version information.
// This records both config file versions and (common config file only) version
// number of program being installed.
procedure StampConfigFiles;
begin
  // Flag ini file versions
  if not FileExists(gCurrentUserConfigFile) then
    CreateUnicodeConfigFile(gCurrentUserConfigFile);
  SetIniInt('IniFile', 'Version', 8, gCurrentUserConfigFile);
  if not FileExists(gCurrentCommonConfigFile) then
    CreateUnicodeConfigFile(gCurrentCommonConfigFile);
  SetIniInt('IniFile', 'Version', 6, gCurrentCommonConfigFile);
  // Record application version in common ini file
  SetIniString(
    'Application',
    'Version',
    TAppInfo.ProgramReleaseVersion,
    gCurrentCommonConfigFile
  );
end;

// Deletes any highlighter preferences from new installation's user config file.
procedure DeleteHighligherPrefs;
begin
  DeleteIniSection('Prefs:Hiliter', gCurrentUserConfigFile);
end;

// Creates new style per user and common config files from content of single,
// old style config file used before CodeSnip v1.9.
procedure CreateIniFilesFromOldStyle;
var
  I: Integer; // loops thru all highlight elements
begin
  // Create per-user settings file:
  // Copy file
  CopyAnsiToUnicodeConfigFile(
    gUserConfigFiles[piOriginal], gCurrentUserConfigFile
  );
  // Delete unwanted sections:
  // - Application section now in common config file
  // - Source code output format: format lost when updating from CodeSnip pre
  //   1.7 since section was SourceOutput, but format preserved from v1.7 since
  //   current Prefs:SourceCode section used
  // - Highlighting style is deliberately lost since CodeSnip v3 has new default
  //   style and main display uses style, therefore sections HiliteOutput
  //   (pre v1.7.5) and Prefs:Hiliter (v1.7 and later) deleted
  DeleteIniSection('Application', gCurrentUserConfigFile);
  DeleteIniSection('SourceOutput', gCurrentUserConfigFile);
  DeleteIniSection('HiliteOutput', gCurrentUserConfigFile);
  for I := 0 to 11 do
    DeleteIniSection('HiliteOutput:Elem' + IntToStr(I), gCurrentUserConfigFile);
  DeleteHighligherPrefs;
  // Main window's overview tabs have changed in v3: so we reset to 0 (default)
  SetIniInt('MainWindow', 'OverviewTab', 0, gCurrentUserConfigFile);

  // Create common (application specific) settings file
  // Copy file
  CopyAnsiToUnicodeConfigFile(
     gUserConfigFiles[piOriginal], gCurrentCommonConfigFile
  );
  // Delete unwanted sections
  // following sections belong in per-user file
  DeleteIniSection('Cmp:D2', gCurrentCommonConfigFile);
  DeleteIniSection('Cmp:D3', gCurrentCommonConfigFile);
  DeleteIniSection('Cmp:D4', gCurrentCommonConfigFile);
  DeleteIniSection('Cmp:D5', gCurrentCommonConfigFile);
  DeleteIniSection('Cmp:D6', gCurrentCommonConfigFile);
  DeleteIniSection('Cmp:D7', gCurrentCommonConfigFile);
  DeleteIniSection('Cmp:D2005w32', gCurrentCommonConfigFile);
  DeleteIniSection('Cmp:D2006w32', gCurrentCommonConfigFile);
  DeleteIniSection('Cmp:D2007', gCurrentCommonConfigFile);
  DeleteIniSection('Cmp:FPC', gCurrentCommonConfigFile);
  DeleteIniSection('FindCompiler', gCurrentCommonConfigFile);
  DeleteIniSection('FindText', gCurrentCommonConfigFile);
  DeleteIniSection('FindXRefs', gCurrentCommonConfigFile);
  DeleteIniSection('MainWindow', gCurrentCommonConfigFile);
  DeleteIniSection('Prefs:General', gCurrentCommonConfigFile);
  DeleteIniSection('Prefs:Printing', gCurrentCommonConfigFile);
  DeleteIniSection('Prefs:SourceCode', gCurrentCommonConfigFile);
  DeleteIniSection('Prefs:Hiliter', gCurrentCommonConfigFile);
  // following sections may occur when updating from CodeSnip v1.7.5 or earlier
  DeleteIniSection('SourceOutput', gCurrentCommonConfigFile);
  DeleteIniSection('HiliteOutput', gCurrentCommonConfigFile);
  for I := 0 to 11 do
    DeleteIniSection(
      'HiliteOutput:Elem' + IntToStr(I), gCurrentCommonConfigFile
    );
end;

// Adds Prefs:CodeGen section along with default data to new installation's user
// config file.
procedure CreateDefaultCodeGenEntries;
begin
  if not FileExists(gCurrentUserConfigFile) then
    CreateUnicodeConfigFile(gCurrentUserConfigFile);
  SetIniInt(
    'Prefs:CodeGen',
    'SwitchOffWarnings',
    0,
    gCurrentUserConfigFile
  );
  SetIniInt(
    'Prefs:CodeGen',
    'WarningCount',
    8,
    gCurrentUserConfigFile
  );
  SetIniString(
    'Prefs:CodeGen',
    'Warning0.Symbol',
    'UNSAFE_TYPE',
    gCurrentUserConfigFile
  );
  SetIniString(
    'Prefs:CodeGen',
    'Warning0.MinCompiler',
    '15.00',
    gCurrentUserConfigFile
  );
  SetIniString(
    'Prefs:CodeGen',
    'Warning1.Symbol',
    'UNSAFE_CAST',
    gCurrentUserConfigFile
  );
  SetIniString(
    'Prefs:CodeGen',
    'Warning1.MinCompiler',
    '15.00',
    gCurrentUserConfigFile
  );
  SetIniString(
    'Prefs:CodeGen',
    'Warning2.Symbol',
    'UNSAFE_CODE',
    gCurrentUserConfigFile
  );
  SetIniString(
    'Prefs:CodeGen',
    'Warning2.MinCompiler',
    '15.00',
    gCurrentUserConfigFile
  );
  SetIniString(
    'Prefs:CodeGen',
    'Warning3.Symbol',
    'SYMBOL_PLATFORM',
    gCurrentUserConfigFile
  );
  SetIniString(
    'Prefs:CodeGen',
    'Warning3.MinCompiler',
    '14.00',
    gCurrentUserConfigFile
  );
  SetIniString(
    'Prefs:CodeGen',
    'Warning4.Symbol',
    'SYMBOL_DEPRECATED',
    gCurrentUserConfigFile
  );
  SetIniString(
    'Prefs:CodeGen',
    'Warning4.MinCompiler',
    '14.00',
    gCurrentUserConfigFile
  );
  SetIniString(
    'Prefs:CodeGen',
    'Warning5.Symbol',
    'SYMBOL_LIBRARY',
    gCurrentUserConfigFile
  );
  SetIniString(
    'Prefs:CodeGen',
    'Warning5.MinCompiler',
    '14.00',
    gCurrentUserConfigFile
  );
  SetIniString(
    'Prefs:CodeGen',
    'Warning6.Symbol',
    'IMPLICIT_STRING_CAST',
    gCurrentUserConfigFile
  );
  SetIniString(
    'Prefs:CodeGen',
    'Warning6.MinCompiler',
    '20.00',
    gCurrentUserConfigFile
  );
  SetIniString(
    'Prefs:CodeGen',
    'Warning7.Symbol',
    'EXPLICIT_STRING_CAST',
    gCurrentUserConfigFile
  );
  SetIniString(
    'Prefs:CodeGen',
    'Warning7.MinCompiler',
    '20.00',
    gCurrentUserConfigFile
  );
end;

// Deletes proxt password entry from new installation's user config file.
procedure DeleteProxyPassword;
begin
  if not FileExists(gCurrentUserConfigFile) then
    CreateUnicodeConfigFile(gCurrentUserConfigFile);
  SetIniString(
    'ProxyServer',
    'Password',
    '',
    gCurrentUserConfigFile
  );
end;

end.


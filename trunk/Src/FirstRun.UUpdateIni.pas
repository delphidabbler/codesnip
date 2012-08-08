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

// Create a new empty UTF-16LE encoded config file. Used to ensure the config
// file writing routines write in Unicode. This works because built in Inno
// Setup ini functions call Windows API WritePrivateProfileString which only
// writes Unicode if the file it is writing to is detected as Unicode. If not it
// writes ANSI text.
procedure CreateUnicodeConfigFile(FileName: string);

// Updates config file copied from old stye (pre CodeSnip v1.9) file
procedure UpdateOldStyleIniFile;

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

// Updates Prefs:CodeGen section from format prior to version 9 to version 9
// format
procedure UpdateCodeGenEntries;

// Deletes proxy password entry from new installation's user config file.
procedure DeleteProxyPassword;

// Updates both common and user config files with correct version information.
// This records both config file versions and (common config file only) version
// number of program being installed.
procedure StampConfigFiles;

// Checks if program version from config file is same as current program
// version.
function IsCurrentProgramVer: Boolean;

// Checks if current user's config file has a proxy password.
function HasProxyPassword: Boolean;

// Deletes unused key that determines detail pane index.
procedure DeleteDetailsPaneIndex;

implementation

uses
  SysUtils, Types, Classes, Windows,
  FirstRun.UDataLocations, FirstRun.UIniFile, UAppInfo, UIOUtils;

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
  // writes string array to UTF-16LE file
  ForceDirectories(ExtractFileDir(NewFileName));
  TFileIO.WriteAllLines(NewFileName, Lines, TEncoding.Unicode, True);
end;

// Makes a copy of a Unicode config file with a different name. Does nothing if
// old and new file names are the same.
procedure CopyUnicodeConfigFiles(OldFileName, NewFileName: string);
begin
  if CompareText(OldFileName, NewFileName) = 0 then
    Exit;
  ForceDirectories(ExtractFileDir(NewFileName));
  TFileIO.CopyFile(OldFileName, NewFileName);
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

// Gets version number of program from user config file. Returns empty string if
// no program version information is present
function ConfigFileProgramVer: string;
begin
  Result := GetIniString(
    'IniFile', 'ProgramVersion', '', gCurrentUserConfigFile
  );
end;

// Create a new empty UTF-16LE encoded config file. Used to ensure the config
// file writing routines write in Unicode. This works because built in Inno
// Setup ini functions call Windows API WritePrivateProfileString which only
// writes Unicode if the file it is writing to is detected as Unicode. If not it
// writes ANSI text.
procedure CreateUnicodeConfigFile(FileName: string);
begin
  ForceDirectories(ExtractFileDir(FileName));
  TFileIO.WriteAllText(FileName, '', TEncoding.Unicode, True);
end;

// Copies config files of a previous installation to new installation, updating
// to Unicode format if necessary.
procedure CopyConfigFiles(InstallID: Integer);
var
  OldUserConfigFile: string;
begin
  OldUserConfigFile := gUserConfigFiles[InstallID];
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
  // Config file Meta data: config file version & program version
  if not FileExists(gCurrentUserConfigFile) then
    CreateUnicodeConfigFile(gCurrentUserConfigFile);
  SetIniInt('IniFile', 'Version', 9, gCurrentUserConfigFile);
  SetIniString(
    'IniFile',
    'ProgramVersion',
    TAppInfo.ProgramReleaseVersion,
    gCurrentUserConfigFile
  );
end;

// Checks if program version from config file is same as current program
// version.
function IsCurrentProgramVer: Boolean;
begin
  Result := ConfigFileProgramVer = TAppInfo.ProgramReleaseVersion;
end;

// Deletes any highlighter preferences from new installation's user config file.
procedure DeleteHighligherPrefs;
begin
  DeleteIniSection('Prefs:Hiliter', gCurrentUserConfigFile);
end;

// Updates config file copied from old stye (pre CodeSnip v1.9) file
procedure UpdateOldStyleIniFile;
var
  I: Integer; // loops thru all highlight elements
begin
  // Delete unwanted sections:
  // - Application section: now in common config file
  // - Source code output format: format lost when updating from CodeSnip pre
  //   1.7 since section was SourceOutput, but format preserved from v1.7 since
  //   current Prefs:SourceCode section used
  // - Highlighting style is deliberately lost since CodeSnip v3 & v4 have
  //   different default style and main display uses that style, therefore
  //   section's HiliteOutput (pre v1.7.5) and Prefs:Hiliter (v1.7.5 and later)
  //   deleted.
  DeleteIniSection('Application', gCurrentUserConfigFile);
  DeleteIniSection('SourceOutput', gCurrentUserConfigFile);
  DeleteIniSection('HiliteOutput', gCurrentUserConfigFile);
  for I := 0 to 11 do
    DeleteIniSection('HiliteOutput:Elem' + IntToStr(I), gCurrentUserConfigFile);
  DeleteHighligherPrefs;
  // Main window's overview tabs changed at v3: so we reset to 0 (default)
  SetIniInt('MainWindow', 'OverviewTab', 0, gCurrentUserConfigFile);
end;

// Adds Prefs:CodeGen section along with default data to new installation's user
// config file.
procedure CreateDefaultCodeGenEntries;
begin
  if not FileExists(gCurrentUserConfigFile) then
    CreateUnicodeConfigFile(gCurrentUserConfigFile);
  SetIniInt(
    'Prefs:CodeGen',
    'EmitWarnDirs',
    0,
    gCurrentUserConfigFile
  );
  SetIniInt(
    'Prefs:CodeGen',
    'WarningCount',
    8,
    gCurrentUserConfigFile
  );
  // We don't set warning state: it defaults to required "off" value
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

// Updates Prefs:CodeGen section from format prior to version 9 to version 9
// format
procedure UpdateCodeGenEntries;
begin
  // Key that determines if warnings are emitted changes from SwitchOffWarnings
  // to EmitWarnDirs.
  if IniKeyExists(
    'Prefs:CodeGen', 'SwitchOffWarnings', gCurrentUserConfigFile
  ) then
  begin
    SetIniInt(
      'Prefs:CodeGen',
      'EmitWarnDirs',
      GetIniInt(
        'Prefs:CodeGen', 'SwitchOffWarnings', 0, 0, 0, gCurrentUserConfigFile
      ),
      gCurrentUserConfigFile
    );
    DeleteIniKey('Prefs:CodeGen', 'SwitchOffWarnings', gCurrentUserConfigFile);
  end
  else
    SetIniInt('Prefs:CodeGen', 'EmitWarnDirs', 0, gCurrentUserConfigFile);
end;

// Deletes proxy password entry from new installation's user config file.
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

// Checks if current user's config file has a proxy password.
function HasProxyPassword: Boolean;
begin
  Result := GetIniString('ProxyServer', 'Password', '', gCurrentUserConfigFile)
    <> '';
end;

// Deletes unused key that determines detail pane index.
procedure DeleteDetailsPaneIndex;
begin
  DeleteIniKey('MainWindow', 'DetailTab', gCurrentUserConfigFile);
end;

end.


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

type
  ///  <summary>Class that manages the updating of older config files to the
  ///  current format. Missing files will also be converted.</summary>
  TUserConfigFileUpdater = class(TObject)
  strict private
    var
      ///  <summary>Name of config file.</summary>
      fCfgFileName: string;
    ///  <summary>Copies an ANSI format config file from SrcFileName, converted
    ///  to UTF-16LE encoded Unicode.</summary>
    procedure CopyANSIFile(const SrcFileName: string);
    ///  <summary>Copies a Unicode config file from SrcFileName.</summary>
    procedure CopyUnicodeFile(const SrcFileName: string);
  public
    const
      ///  <summary>Current user config file version.</summary>
      FileVersion = 9;
  public
    ///  <summary>Constructs object to operate on given config file.</summary>
    ///  <remarks>Given file may or may not exist.</remarks>
    constructor Create(const CfgFileName: string);
    ///  <summary>Creates a new empty UTF-16LE encoded config file.</summary>
    ///  <remarks>Used to ensure the config file writing routines write in
    ///  Unicode. This works because ini access routines call Windows API
    ///  WritePrivateProfileString which only writes Unicode if the file it is
    ///  writing to is detected as Unicode. If not it writes ANSI text.
    ///  </remarks>
    procedure CreateNewFile;
    ///  <summary>Copies config file from SrcFileName. File created is always
    ///  Uincode and is converted if source file is ANSI, as specified by IsAnsi
    ///  parameter.</summary>
    procedure CopyFile(const SrcFileName: string; const IsAnsi: Boolean);
    ///  <summary>Gets file version number of config file.</summary>
    function FileVer: Integer;
    ///  <summary>Checks if program version in given config file is same as
    ///  current program version.</summary>
    class function IsCurrentProgramVer(const CfgFileName: string): Boolean;
      overload;
    ///  <summary>Checks if program version in config file is same as current
    ///  program version.</summary>
    function IsCurrentProgramVer: Boolean; overload;
    ///  <summary>Updates config file currently in original (pre v1.9) format to
    ///  current format.</summary>
    procedure UpdateFromOriginal;
    ///  <summary>Deletes any highlighter preferences.</summary>
    procedure DeleteHighligherPrefs;
    ///  <summary>Checks a proxy password is present in file.</summary>
    function HasProxyPassword: Boolean;
    ///  <summary>Deletes proxy password entry.</summary>
    procedure DeleteProxyPassword;
    ///  <summary>Adds Prefs:CodeGen section along with default data.</summary>
    procedure CreateDefaultCodeGenEntries;
    ///  <summary>Updates Prefs:CodeGen section from format prior to version 9
    ///  to version 9 format.</summary>
    procedure UpdateCodeGenEntries;
    ///  <summary>Deletes unused key that determines detail pane index.
    ///  </summary>
    procedure DeleteDetailsPaneIndex;
    ///  <summary>Stamp config file with current program and file versions.
    ///  </summary>
    procedure Stamp;
  end;

// TODO: Move this routine to UDataLocations
// Checks if a config file is ANSI. If False is returned the file is assumed to
// be Unicode.
function IsAnsiConfigFile(InstallID: Integer): Boolean;

implementation

uses
  SysUtils, Types, Classes, Windows,
  FirstRun.UDataLocations, FirstRun.UIniFile, UAppInfo, UIOUtils;

// Checks if a config file is ANSI. If False is returned the file is assumed to
// be Unicode.
function IsAnsiConfigFile(InstallID: Integer): Boolean;
begin
  Result := InstallID <= piV3;
end;

{ TUserConfigFileUpdater }

procedure TUserConfigFileUpdater.CopyANSIFile(const SrcFileName: string);
var
  Lines: TStringDynArray;  // lines of text read from ANSI .ini file
begin
  if CompareText(SrcFileName, fCfgFileName) = 0 then
    Exit;
  // reads an ANSI file and converts contents to Unicode, using system default
  // encoding
  Lines := TFileIO.ReadAllLines(SrcFileName, TEncoding.Default);
  // writes string array to UTF-16LE file
  ForceDirectories(ExtractFileDir(fCfgFileName));
  TFileIO.WriteAllLines(fCfgFileName, Lines, TEncoding.Unicode, True);
end;

procedure TUserConfigFileUpdater.CopyFile(const SrcFileName: string;
  const IsAnsi: Boolean);
begin
  if SrcFileName <> '' then
  begin
    if IsAnsi then
      CopyANSIFile(SrcFileName)
    else
      CopyUnicodeFile(SrcFileName);
  end;
end;

procedure TUserConfigFileUpdater.CopyUnicodeFile(const SrcFileName: string);
begin
  if CompareText(SrcFileName, fCfgFileName) = 0 then
    Exit;
  ForceDirectories(ExtractFileDir(fCfgFileName));
  TFileIO.CopyFile(SrcFileName, fCfgFileName);
end;

constructor TUserConfigFileUpdater.Create(const CfgFileName: string);
begin
  inherited Create;
  fCfgFileName := CfgFileName;
end;

procedure TUserConfigFileUpdater.CreateDefaultCodeGenEntries;
begin
  if not FileExists(fCfgFileName) then
    CreateNewFile;
  SetIniInt('Prefs:CodeGen', 'EmitWarnDirs', 0, fCfgFileName);
  SetIniInt('Prefs:CodeGen', 'WarningCount', 8, fCfgFileName);
  // We don't set warning state: it defaults to required "off" value
  SetIniString(
    'Prefs:CodeGen', 'Warning0.Symbol', 'UNSAFE_TYPE', fCfgFileName
  );
  SetIniString('Prefs:CodeGen', 'Warning0.MinCompiler', '15.00', fCfgFileName);
  SetIniString(
    'Prefs:CodeGen', 'Warning1.Symbol', 'UNSAFE_CAST', fCfgFileName
  );
  SetIniString('Prefs:CodeGen', 'Warning1.MinCompiler', '15.00', fCfgFileName);
  SetIniString(
    'Prefs:CodeGen', 'Warning2.Symbol', 'UNSAFE_CODE', fCfgFileName
  );
  SetIniString('Prefs:CodeGen', 'Warning2.MinCompiler', '15.00', fCfgFileName);
  SetIniString(
    'Prefs:CodeGen', 'Warning3.Symbol', 'SYMBOL_PLATFORM', fCfgFileName
  );
  SetIniString('Prefs:CodeGen','Warning3.MinCompiler', '14.00', fCfgFileName);
  SetIniString(
    'Prefs:CodeGen', 'Warning4.Symbol', 'SYMBOL_DEPRECATED', fCfgFileName
  );
  SetIniString('Prefs:CodeGen', 'Warning4.MinCompiler', '14.00', fCfgFileName);
  SetIniString(
    'Prefs:CodeGen', 'Warning5.Symbol', 'SYMBOL_LIBRARY', fCfgFileName
  );
  SetIniString('Prefs:CodeGen', 'Warning5.MinCompiler', '14.00', fCfgFileName);
  SetIniString(
    'Prefs:CodeGen', 'Warning6.Symbol', 'IMPLICIT_STRING_CAST', fCfgFileName
  );
  SetIniString('Prefs:CodeGen', 'Warning6.MinCompiler', '20.00', fCfgFileName);
  SetIniString(
    'Prefs:CodeGen', 'Warning7.Symbol', 'EXPLICIT_STRING_CAST', fCfgFileName
  );
  SetIniString('Prefs:CodeGen', 'Warning7.MinCompiler', '20.00', fCfgFileName);
end;

procedure TUserConfigFileUpdater.CreateNewFile;
begin
  ForceDirectories(ExtractFileDir(fCfgFileName));
  TFileIO.WriteAllText(fCfgFileName, '', TEncoding.Unicode, True);
end;

procedure TUserConfigFileUpdater.DeleteDetailsPaneIndex;
begin
  if not FileExists(fCfgFileName) then
    CreateNewFile;
  DeleteIniKey('MainWindow', 'DetailTab', fCfgFileName);
end;

procedure TUserConfigFileUpdater.DeleteHighligherPrefs;
begin
  if not FileExists(fCfgFileName) then
    CreateNewFile;
  DeleteIniSection('Prefs:Hiliter', fCfgFileName);
end;

procedure TUserConfigFileUpdater.DeleteProxyPassword;
begin
  if not FileExists(fCfgFileName) then
    CreateNewFile;
  SetIniString('ProxyServer', 'Password', '', fCfgFileName);
end;

function TUserConfigFileUpdater.FileVer: Integer;
begin
  Result := GetIniInt('IniFile', 'Version', 1, fCfgFileName);
end;

function TUserConfigFileUpdater.HasProxyPassword: Boolean;
begin
  Result := GetIniString('ProxyServer', 'Password', '', fCfgFileName) <> '';
end;

function TUserConfigFileUpdater.IsCurrentProgramVer: Boolean;
begin
  Result := IsCurrentProgramVer(fCfgFileName);
end;

class function TUserConfigFileUpdater.IsCurrentProgramVer(
  const CfgFileName: string): Boolean;
var
  CfgProgVer: string;  // program version from config file
begin
  CfgProgVer := GetIniString('IniFile', 'ProgramVersion', '', CfgFileName);
  Result := CfgProgVer = TAppInfo.ProgramReleaseVersion;
end;

procedure TUserConfigFileUpdater.Stamp;
begin
  if not FileExists(fCfgFileName) then
    CreateNewFile;
  SetIniInt('IniFile', 'Version', FileVersion, fCfgFileName);
  SetIniString(
    'IniFile', 'ProgramVersion', TAppInfo.ProgramReleaseVersion, fCfgFileName
  );
end;

procedure TUserConfigFileUpdater.UpdateCodeGenEntries;
begin
  // Key that determines if warnings are emitted changes from SwitchOffWarnings
  // to EmitWarnDirs.
  if not FileExists(fCfgFileName) then
    CreateNewFile;
  if IniKeyExists('Prefs:CodeGen', 'SwitchOffWarnings', fCfgFileName) then
  begin
    SetIniInt(
      'Prefs:CodeGen',
      'EmitWarnDirs',
      GetIniInt('Prefs:CodeGen', 'SwitchOffWarnings', 0, fCfgFileName),
      fCfgFileName
    );
    DeleteIniKey('Prefs:CodeGen', 'SwitchOffWarnings', fCfgFileName);
  end
  else
    SetIniInt('Prefs:CodeGen', 'EmitWarnDirs', 0, fCfgFileName);
end;

procedure TUserConfigFileUpdater.UpdateFromOriginal;
var
  I: Integer; // loops thru all highlight elements
begin
  if not FileExists(fCfgFileName) then
    CreateNewFile;
  // Delete unwanted sections:
  // - Application section: now in common config file
  // - Source code output format: format lost when updating from CodeSnip pre
  //   1.7 since section was SourceOutput, but format preserved from v1.7 since
  //   current Prefs:SourceCode section used
  // - Highlighting style is deliberately lost since CodeSnip v3 & v4 have
  //   different default style and main display uses that style, therefore
  //   section's HiliteOutput (pre v1.7.5) and Prefs:Hiliter (v1.7.5 and later)
  //   deleted.
  DeleteIniSection('Application', fCfgFileName);
  DeleteIniSection('SourceOutput', fCfgFileName);
  DeleteIniSection('HiliteOutput', fCfgFileName);
  for I := 0 to 11 do
    DeleteIniSection('HiliteOutput:Elem' + IntToStr(I), fCfgFileName);
  DeleteHighligherPrefs;
  // Main window's overview tabs changed at v3: so we reset to 0 (default)
  SetIniInt('MainWindow', 'OverviewTab', 0, fCfgFileName);
end;

end.


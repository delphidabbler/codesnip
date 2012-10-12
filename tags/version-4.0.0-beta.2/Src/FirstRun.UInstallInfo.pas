{
 * FirstRun.UInstallInfo.pas
 *
 * Pascal script for use in [Code] Section of CodeSnip's Install.iss.
 *
 * Gets information about location of application data folder and any existing
 * config files and databaseon on the system where CodeSnip is being installed.
 * Sets global variables storing folder paths that indicate where config files
 * and database can be found.
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
 * The Original Code is FirstRun.UInstallInfo.
 *
 * The Initial Developer of the Original Code is Peter Johnson
 * (http://www.delphidabbler.com/).
 *
 * Portions created by the Initial Developer are Copyright (C) 2008-2012 Peter
 * Johnson. All Rights Reserved.
 *
 * Contributors:
 *    NONE
 *
 * ***** END LICENSE BLOCK *****
}


unit FirstRun.UInstallInfo;


interface

{
  User config file and database locations in different CodeSnip versions
  ----------------------------------------------------------------------

  Version numbers refer to the CodeSnip release.

  + Versions up to v1.8.11:
    - Config file: %AppData%\DelphiDabbler\CodeSnip\CodeSnip.ini
    - No user database.

  + From v1.9 to v1.9.4:
    - Config file: %AppData%\DelphiDabbler\CodeSnip\User.ini
    - No user database.

  + All v2 versions:
    - Config file: %AppData%\DelphiDabbler\CodeSnip\User.ini
    - Database directory: %AppData%\DelphiDabbler\CodeSnip\UserData

  + All v3 versions:
    - Config file: %AppData%\DelphiDabbler\CodeSnip\User.3.ini
    - Database directory: %AppData%\DelphiDabbler\CodeSnip\UserData.3

  + From v4.0:
    - Config file: %AppData%\DelphiDabbler\CodeSnip.4\User.config
    - Database directory: %AppData%\DelphiDabbler\CodeSnip.4\UserDatabase
}

type
  ///  <summary>Identifiers for different type of CodeSnip installations that
  ///  can be detected.</summary>
  TInstallId = (
    piNone,       // CodeSnip not installed
    piOriginal,   // "original" installation up to v1.8.11
    piV1_9,       // v1.9 to v1.9.4
    piV2,         // all v2 versions
    piV3,         // all v3 versions
    piV4          // from v4.0 (including alpha & beta code v3.98.x & v3.99.x
  );

type
  ///  <summary>Class that provides information about CodeSnip installations.
  ///  </summary>
  TInstallInfo = class(TObject)
  strict private
    const
      ///  <summary>ID earliest version of CodeSnip.</summary>
      FirstVersionID = piOriginal;
      ///  <summary>ID of current version of CodeSnip.</summary>
      CurrentVersionID = piv4;
      ///  <summary>Array mapping install IDs to relative paths to user config
      ///  file for for that installation version.</summary>
      ConfigFileNames: array[FirstVersionID..CurrentVersionID] of string =
        (
          'DelphiDabbler\CodeSnip\CodeSnip.ini',
          'DelphiDabbler\CodeSnip\User.ini',
          'DelphiDabbler\CodeSnip\User.ini',
          'DelphiDabbler\CodeSnip\User.3.ini',
          'DelphiDabbler\CodeSnip.4\User.config'
        );
      ///  <summary>Array mapping install IDs to relative paths to user database
      ///  directories for for that installation version.</summary>
      DatabaseDirs: array[FirstVersionID..CurrentVersionID] of string =
        (
          '',
          '',
          'DelphiDabbler\CodeSnip\UserData',
          'DelphiDabbler\CodeSnip\UserData.3',
          'DelphiDabbler\CodeSnip.4\UserDatabase'
        );
    var
      ///  <summary>Value of InstallID property.</summary>
      fInstallID: TInstallId;
    ///  <summary>Converts given relative directory path or file name into an
    ///  absolute path by pre-prending the user's application data folder.
    ///  </summary>
    ///  <remarks>If Name = '' then '' is returned.</remarks>
    class function MakeFullPath(const Name: string): string;
    ///  <summary>Detects latest version of CodeSnip for which user data can be
    ///  found and sets InstallID property accordingly.</summary>
    procedure DetectInstall;
  public
    ///  <summary>Creates object to provide information about latest CodeSnip
    ///  installation for which user data can be found.</summary>
    constructor Create;
    ///  <summary>Returns full path to user config file for current version of
    ///  CodeSnip.</summary>
    class function CurrentUserConfigFileName: string;
    ///  <summary>Returns full path to database directory for current version of
    ///  CodeSnip.</summary>
    class function CurrentUserDatabaseDir: string;
    ///  <summary>Returns full path to user config file version of CodeSnip for
    ///  which user data is available.</summary>
    ///  <remarks>This can be current version's config file if present.
    ///  </remarks>
    function PreviousUserConfigFileName: string;
    ///  <summary>Informs if user config file returned by
    ///  PreviousUserConfigFileName is in ANSI format.</summary>
    function IsPreviousUserConfigFileANSI: Boolean;
    ///  <summary>Returns full path to user database directory for latest
    ///  version of CodeSnip for which a user database is available.</summary>
    ///  <remarks>This can be current version's database directory if present.
    ///  </remarks>
    function PreviousUserDatabaseDir: string;
    ///  <summary>Returns full path on user database file for latest version of
    ///  CodeSnip for which a user database is available.</summary>
    ///  <remarks>This can be current version's database file if present.
    ///  </remarks>
    function PreviousUserDatabaseFileName: string;
    ///  <summary>ID of latest CodeSnip install found.</summary>
    ///  <remarks>This is ID of latest version for which user data can be found.
    ///  </remarks>
    property InstallID: TInstallId read fInstallID;
  end;

implementation


uses
  // Delphi
  SysUtils, IOUtils,
  // Project
  UIOUtils, UStrUtils, USystemInfo;


{ TInstallInfo }

constructor TInstallInfo.Create;
begin
  inherited Create;
  DetectInstall;
end;

class function TInstallInfo.CurrentUserConfigFileName: string;
begin
  Result := MakeFullPath(ConfigFileNames[CurrentVersionID]);
end;

class function TInstallInfo.CurrentUserDatabaseDir: string;
begin
  Result := MakeFullPath(DatabaseDirs[CurrentVersionID]);
end;

procedure TInstallInfo.DetectInstall;

  // Checks if given Unicode format file is empty.
  function IsEmptyUnicodeCfgFile(const FileName: string): Boolean;
  var
    Content: string;  // content of file
  begin
    Content := StrTrim(TFileIO.ReadAllText(FileName, TEncoding.Unicode, True));
    Result := Content = '';
  end;

begin
  if TFile.Exists(MakeFullPath(ConfigFileNames[piV4]))
    and not IsEmptyUnicodeCfgFile(MakeFullPath(ConfigFileNames[piV4])) then
    fInstallID := piV4
  else if TFile.Exists(MakeFullPath(ConfigFileNames[piV3])) then
    fInstallID := piV3
  else if TDirectory.Exists(MakeFullPath(DatabaseDirs[piV2])) then
    fInstallID := piV2
  else if TFile.Exists(MakeFullPath(ConfigFileNames[piV1_9])) then
    fInstallID := piV1_9
  else if TFile.Exists(MakeFullPath(ConfigFileNames[piOriginal])) then
    fInstallID := piOriginal
  else
    fInstallID := piNone;
end;

function TInstallInfo.IsPreviousUserConfigFileANSI: Boolean;
begin
  Result := fInstallID <= piV3;
end;

class function TInstallInfo.MakeFullPath(const Name: string): string;
begin
  if Name = '' then
    Exit('');
  Result := IncludeTrailingPathDelimiter(TSystemFolders.PerUserAppData) + Name;
end;

function TInstallInfo.PreviousUserConfigFileName: string;
begin
  Result := MakeFullPath(ConfigFileNames[fInstallID]);
end;

function TInstallInfo.PreviousUserDatabaseDir: string;
begin
  Result := MakeFullPath(DatabaseDirs[fInstallID]);
end;

function TInstallInfo.PreviousUserDatabaseFileName: string;
begin
  Result := IncludeTrailingPathDelimiter(PreviousUserDatabaseDir)
    + 'database.xml';
end;

end.

{
 * FirstRun.UDataLocations.pas
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
 * The Original Code is FirstRun.UDataLocations.pas, formerly DataLocations.ps.
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


unit FirstRun.UDataLocations;


interface

{
  User config file and database locations in different CodeSnip versions
  ----------------------------------------------------------------------

  Note that the location of %AppData% varies according to the current user.
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
  TInstallInfo = class(TObject)
  strict private
    const
      FirstVersionID = piOriginal;
      CurrentVersionID = piv4;
      ConfigFileNames: array[FirstVersionID..CurrentVersionID] of string =
        (
          'DelphiDabbler\CodeSnip\CodeSnip.ini',
          'DelphiDabbler\CodeSnip\User.ini',
          'DelphiDabbler\CodeSnip\User.ini',
          'DelphiDabbler\CodeSnip\User.3.ini',
          'DelphiDabbler\CodeSnip.4\User.config'
        );
      DatabaseDirs: array[FirstVersionID..CurrentVersionID] of string =
        (
          '',
          '',
          'DelphiDabbler\CodeSnip\UserData',
          'DelphiDabbler\CodeSnip\UserData.3',
          'DelphiDabbler\CodeSnip.4\UserDatabase'
        );
    var
      fInstallID: TInstallId;
    class function MakeFullPath(const Name: string): string;
    procedure DetectInstall;
  public
    constructor Create;
    class function CurrentUserConfigFileName: string;
    class function CurrentUserDatabaseDir: string;
    function PreviousUserConfigFileName: string;
    function IsPreviousUserConfigFileANSI: Boolean;
    function PreviousUserDatabaseDir: string;
    function PreviousUserDatabaseFileName: string;
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

  function IsEmptyUnicodeCfgFile(const FileName: string): Boolean;
  var
    Content: string;
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


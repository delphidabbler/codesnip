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

const
  // Identifiers for different types of previous installations of CodeSnip
  piNone = 0;           // CodeSnip not installed
  piOriginal = 1;       // original locations used up to v1.8.11
  piV1_9 = 2;           // v1.9 to v1.9.4
  piV2 = 3;             // all v2 versions
  piV3 = 4;             // all v3 versions
  piV4 = 5;             // from v4.0 (including alpha and beta code v3.99.x
  // Values of first and last indentifiers for different types of CodeSnip
  // installations
  piFirstVersionID = 1;
  piLastVersionID = 5;
  // Identifiers of type of current installation
  piCurrent = piV4;

var
  // Records info about previous install
  gPrevInstallID: Integer;

  // Arrays of paths to user config file and user database
  gUserConfigFiles: array[piFirstVersionID..piLastVersionID] of string;
  gUserDatabaseDirs: array[piFirstVersionID..piLastVersionID] of string;

  // Path to user config file for program being installed
  gCurrentUserConfigFile: string;

// Initialises global variables that (a) store location of config files and
// databases used by different versions of CodeSnip and (b) record type of any
// previous installation that was detected.
procedure InitGlobals;

// Checks if database and config files need to be converted and / or copied to
// new locations for application being installed.
function DataConversionRequired: Boolean;

implementation

uses
  SysUtils, IOUtils,
  UAppInfo, USystemInfo;

// Checks if database and config files need to be converted and / or copied to
// new locations for application being installed.
function DataConversionRequired: Boolean;
begin
  Result := (gPrevInstallID <> piNone) and (gPrevInstallID < piCurrent);
end;

// Records the application's data directories for each different arrangement
// used in different versions of CodeSnip.
procedure InitAppDataFolders;
var
  AppData: string;      // path to user's application data directory
begin
  // Record system's user application data directory
  AppData := IncludeTrailingPathDelimiter(TSystemFolders.PerUserAppData);

  // Record paths to config files and database for each installation type
  gUserConfigFiles[piOriginal] :=
    AppData + 'DelphiDabbler\CodeSnip\CodeSnip.ini';
  gUserDatabaseDirs[piOriginal] := '';

  gUserConfigFiles[piV1_9] :=
    AppData + 'DelphiDabbler\CodeSnip\User.ini';
  gUserDatabaseDirs[piV1_9] := '';

  gUserConfigFiles[piV2] :=
    AppData + 'DelphiDabbler\CodeSnip\User.ini';
  gUserDatabaseDirs[piV2] :=
    AppData + 'DelphiDabbler\CodeSnip\UserData';

  gUserConfigFiles[piV3] :=
    AppData + 'DelphiDabbler\CodeSnip\User.3.ini';
  gUserDatabaseDirs[piV3] :=
    AppData + 'DelphiDabbler\CodeSnip\UserData.3';

  gUserConfigFiles[piV4] :=
    AppData + 'DelphiDabbler\CodeSnip.4\User.config';
  gUserDatabaseDirs[piV4] :=
    AppData + 'DelphiDabbler\CodeSnip.4\UserDatabase';

  // Record installation type current being installed
  gCurrentUserConfigFile := gUserConfigFiles[piCurrent];

end;

// Attempts to detect and identify any previous installation by checking for
// known files and directories.
procedure DetectPrevInstall;
begin
  if FileExists(gUserConfigFiles[piV4]) then
    gPrevInstallID := piV4
  else if FileExists(gUserConfigFiles[piV3]) then
    gPrevInstallID := piV3
  else if TDirectory.Exists(gUserDatabaseDirs[piV2]) then
    gPrevInstallID := piV2
  else if FileExists(gUserDatabaseDirs[piV1_9]) then
    gPrevInstallID := piV1_9
  else if FileExists(gUserConfigFiles[piOriginal]) then
    gPrevInstallID := piOriginal
  else
    gPrevInstallID := piNone;
end;

// Initialises global variables that (a) store location of user config file and
// user database used by different versions of CodeSnip and (b) record type of
// any previous installation that was detected.
procedure InitGlobals;
begin
  InitAppDataFolders;
  DetectPrevInstall;
end;

end.


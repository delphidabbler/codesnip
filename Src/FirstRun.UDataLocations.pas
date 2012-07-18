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

{
  File locations in different CodeSnip versions
  ---------------------------------------------

  Note that the location of %AppData% varies according to the current user.
  Version numbers refer to the CodeSnip release.

  + Versions up to v1.8.11:
    - Config file: %AppData%\DelphiDabbler\CodeSnip\CodeSnip.ini
    - Main database directory: %AppData%\DelphiDabbler\CodeSnip\Data
    - No user defined database.

  + From v1.9 to v1.9.4:
    - Two config files:
      o %ProgramData%\DelphiDabbler\CodeSnip\Common.ini
      o %AppData%\DelphiDabbler\CodeSnip\User.ini
    - Main database directory:
        %ProgramData%\DelphiDabbler\CodeSnip\Data
    - No user defined database.

  + All v2 versions:
    - Two config files:
      o %ProgramData%\DelphiDabbler\CodeSnip\Common.ini
      o %AppData%\DelphiDabbler\CodeSnip\User.ini
    - Main database directory:
        %ProgramData%\DelphiDabbler\CodeSnip\Data
    - User defined database directory:
        %AppData%\DelphiDabbler\CodeSnip\UserData

  + All v3 versions:
    - Two config files:
      o %ProgramData%\DelphiDabbler\CodeSnip\Common.ini
      o %AppData%\DelphiDabbler\CodeSnip\User.3.ini
    - Main database directory:
        %ProgramData%\DelphiDabbler\CodeSnip\Data
    - User defined database directory:
        %AppData%\DelphiDabbler\CodeSnip\UserData.3

  + From v4.0:
    - Two config files:
      o %ProgramData%\DelphiDabbler\CodeSnip.4\Common.config
      o %AppData%\DelphiDabbler\CodeSnip.4\User.config
    - Main database directory:
        %ProgramData%\DelphiDabbler\CodeSnip.4\Database
    - User defined database directory:
        %AppData%\DelphiDabbler\CodeSnip.4\UserDatabase
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

  // Arrays of paths to common and per-user config files.
  gCommonConfigFiles: array[piFirstVersionID..piLastVersionID] of string;
  gUserConfigFiles: array[piFirstVersionID..piLastVersionID] of string;
  // Array of paths to main and user database directories
  gMainDatabaseDirs: array[piFirstVersionID..piLastVersionID] of string;
  gUserDatabaseDirs: array[piFirstVersionID..piLastVersionID] of string;

  // Paths to common and user config files for program being installed
  gCurrentCommonConfigFile: string;
  gCurrentUserConfigFile: string;

// Checks if database and config files need to be converted and / or copied to
// new locations for application being installed.
function DataConversionRequired: Boolean;
begin
  Result := (gPrevInstallID <> piNone) and (gPrevInstallID < piCurrent);
end;

// Gets application data folder of user who started installation by using
// helper app run as original user.
function GetAppData: string;
var
  HelperApp: string;    // name of setup helper app
  TmpDir: string;       // temp dir where helper app writes required info
  TmpFile: string;      // temp file that received helper app's info
  Res: Integer;         // return code from helper app: ignored
begin
  // Create name of a temporary directory in which helper app can store data
  TmpDir := ExpandConstant('{commonappdata}\DelphiDabbler\~Tmp');
  TmpFile := TmpDir + '\data';
  ForceDirectories(TmpDir);
  // Copy helper app into temp directory
  HelperApp := ExpandConstant('{#SetupHelper}');
  ExtractTemporaryFile(HelperApp);
  // Run helper app as original user. Helper app writes information relating to
  // original user to a ini file in temp directory created above.
  ExecAsOriginalUser(
    ExpandConstant('{tmp}\' + HelperApp),
    '"' + TmpFile + '"',
    '',
    SW_HIDE,
    ewWaitUntilTerminated,
    Res
  );
  // Get original user's application data directory from temp file
  Result := AddBackSlash(
    GetIniString('SpecialFolders', 'CSIDL_APPDATA', '', TmpFile)
  );
  // Delete temporary directory
  DelTree(TmpDir, True, True, True);
end;

// Records the application's data directories for each different arrangement
// used in different versions of CodeSnip.
procedure InitAppDataFolders;
var
  AppData: string;      // path to user's application data directory
  ProgramData: string;  // path to common application data directory
begin
  // Record system's common and user application data directories
  ProgramData := ExpandConstant('{commonappdata}\');
  AppData := GetAppData;

  // Record paths to config files and database for each installation type
  gCommonConfigFiles[piOriginal] := '';
  gUserConfigFiles[piOriginal] :=
    AppData + 'DelphiDabbler\CodeSnip\CodeSnip.ini';
  gMainDatabaseDirs[piOriginal] :=
    AppData + 'DelphiDabbler\CodeSnip\Data';
  gUserDatabaseDirs[piOriginal] := '';

  gCommonConfigFiles[piV1_9] :=
    ProgramData + 'DelphiDabbler\CodeSnip\Common.ini';
  gUserConfigFiles[piV1_9] :=
    AppData + 'DelphiDabbler\CodeSnip\User.ini';
  gMainDatabaseDirs[piV1_9] :=
    ProgramData + 'DelphiDabbler\CodeSnip\Data';
  gUserDatabaseDirs[piV1_9] := ''

  gCommonConfigFiles[piV2] :=
    ProgramData + 'DelphiDabbler\CodeSnip\Common.ini';
  gUserConfigFiles[piV2] :=
    AppData + 'DelphiDabbler\CodeSnip\User.ini';
  gMainDatabaseDirs[piV2] :=
    ProgramData + 'DelphiDabbler\CodeSnip\Data';
  gUserDatabaseDirs[piV2] :=
    AppData + 'DelphiDabbler\CodeSnip\UserData';

  gCommonConfigFiles[piV3] :=
    ProgramData + 'DelphiDabbler\CodeSnip\Common.ini';
  gUserConfigFiles[piV3] :=
    AppData + 'DelphiDabbler\CodeSnip\User.3.ini';
  gMainDatabaseDirs[piV3] :=
    ProgramData + 'DelphiDabbler\CodeSnip\Data';
  gUserDatabaseDirs[piV3] :=
    AppData + 'DelphiDabbler\CodeSnip\UserData.3';

  gCommonConfigFiles[piV4] :=
    ProgramData + 'DelphiDabbler\CodeSnip.4\Common.config';
  gUserConfigFiles[piV4] :=
    AppData + 'DelphiDabbler\CodeSnip.4\User.config';
  gMainDatabaseDirs[piV4] :=
    ProgramData + 'DelphiDabbler\CodeSnip.4\Database';
  gUserDatabaseDirs[piV4] :=
    AppData + 'DelphiDabbler\CodeSnip.4\UserDatabase';

  // Record installation type current being installed
  gCurrentCommonConfigFile := gCommonConfigFiles[piCurrent];
  gCurrentUserConfigFile := gUserConfigFiles[piCurrent];

end;

// Attempts to detect and identify any previous installation by checking for
// known files and directories.
procedure DetectPrevInstall;
begin
  if FileExists(gCommonConfigFiles[piV4]) or
    FileExists(gUserConfigFiles[piV4]) then
    gPrevInstallID := piV4
  else if FileExists(gCommonConfigFiles[piV3])
    and FileExists(gUserConfigFiles[piV3]) then
    gPrevInstallID := piV3
  else if FileExists(gCommonConfigFiles[piV2])
    and FileExists(gUserDatabaseDirs[piV2] + '\database.xml') then
    gPrevInstallID := piV2
  else if FileExists(gCommonConfigFiles[piV1_9]) then
    gPrevInstallID := piV1_9
  else if FileExists(gUserConfigFiles[piOriginal]) then
    gPrevInstallID := piOriginal
  else
    gPrevInstallID := piNone;
end;

// Initialises global variables that (a) store location of config files and
// databases used by different versions of CodeSnip and (b) record type of any
// previous installation that was detected.
procedure InitGlobals;
begin
  InitAppDataFolders;
  DetectPrevInstall;
end;


{
 * FirstRun.UMain.pas
 *
 * Pascal script for use in [Code] Section of CodeSnip's Install.iss.
 *
 * Implements all event handlers that hook into Setup program to perform
 * required customisations.
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
 * The Original Code is FirstRun.UMain.pas, formerly EventHandlers.ps.
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


unit FirstRun.UMain;

interface

// Initialises global variables that provide information on location of data
// folders and database and about any earlier installation that has been
// detected.
function InitializeSetup: Boolean;

// Performs any required data conversion
procedure PerformRequiredDataConversion;

implementation

uses
  SysUtils, Windows, IOUtils,
  FirstRun.UDataLocations, FirstRun.UUpdateDBase, FirstRun.UUpdateIni,
  UMessageBox, UUtils;

var
  // Flag true if user wants to update user database and config file
  gDataConversionRequested: Boolean;

  // Flag true if user wants to have old data deleted after conversion
  gDeleteOldDataRequested: Boolean;


// Initialises global variables that provide information on location of data
// folders and database and about any earlier installation that has been
// detected.
function InitializeSetup: Boolean;
begin
  InitGlobals;
  Result := True;
end;

// Performs any required data conversion
procedure PerformRequiredDataConversion;
var
  PrevInstallID: Integer; // loops through previous installs
  FileName: string;       // name of a config file
  DirName: string;        // name of a database directory
begin
  // Check if data conversion is needed
  if DataConversionRequired then
  begin
    // Check if user has requested data conversion
    if gDataConversionRequested then
    begin
      case gPrevInstallID of
        piOriginal:
        begin
          CopyDatabases(piOriginal);
          CreateIniFilesFromOldStyle;
        end;
        piV1_9:
        begin
          CopyDatabases(piV1_9);
          CopyConfigFiles(piV1_9);
          DeleteHighligherPrefs;  // default highlighting changes in v3
        end;
        piV2:
        begin
          CopyDatabases(piV2);
          CopyConfigFiles(piV2);
          DeleteHighligherPrefs;  // default highlighting changes in v3
        end;
        piV3:
        begin
          CopyDatabases(piV3);
          CopyConfigFiles(piV3);
        end;
      end;
    end;

    // Check if old user database and config file should be deleted
    if gDeleteOldDataRequested then
    begin
      for PrevInstallID := piOriginal to piCurrent - 1 do
      begin
        FileName := gUserConfigFiles[PrevInstallId];
        if (FileName <> '') and (FileName <> gCurrentUserConfigFile)
          and FileExists(FileName) then
          SysUtils.DeleteFile(FileName);
      end;
      for PrevInstallID := piOriginal to piCurrent - 1 do
      begin
        DirName := gUserDatabaseDirs[PrevInstallID];
        if (DirName <> '') and (DirName <> gUserDatabaseDirs[piCurrent])
          and TDirectory.Exists(DirName) then
          TDirectory.Delete(DirName, True);
      end;
    end;
  end;

  // Check if any additional default data needs to be added to older ini files
  if UserConfigFileVer < 6 then
    // user ini file versions before 6 don't have the Prefs:CodeGen section and
    // default entries for predefined warnings
    CreateDefaultCodeGenEntries;

  // Check if there's a proxy server password in incompatible format and delete
  // it if so. Inform user.
  if (UserConfigFileVer < 7) and HasProxyPassword then
  begin
    DeleteProxyPassword;
    TMessageBox.Information(
      nil,
      'Your existing proxy server password has been deleted. This is because '
       + 'the format for storing the password has been changed to permit '
       + 'non European characters to be used.'#13#10#13#10
       + 'When you start CodeSnip please select the Tools | Proxy Server '
       + 'menu option and re-enter your password in the dialog box.'#13#10#13#10
       + 'Sorry for the inconvenience.'
    );
  end;

  // Ensure user config file has correct ini file and program version
  // information. This creates config file if it doesn't exist
  StampConfigFiles;
end;

end.


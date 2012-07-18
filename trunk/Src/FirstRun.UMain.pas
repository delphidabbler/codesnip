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


var
  // Reference to custom wizard page that gets info from user about whether to
  // preserve config data and database. Whether page is displayed depends on
  // whether we can update config files and database.
  gConfigPage: TInputOptionWizardPage;

  // Flag true if user wants to update database and config files
  gDataConversionRequested: Boolean;

  // Flag true if user wants to have old data deleted after conversion
  gDeleteOldDataRequested: Boolean;


// Called during Setup's initialization. Initialises global variables that
// provide information location of data folders and database and about any
// earlier installation that has been detected.
function InitializeSetup: Boolean;
begin
  InitGlobals;
  Result := True;
end;

// Called when wizard is initialising. Creates custom page used to inform
// whether existing old-style config files and database should be preserved.
procedure InitializeWizard;
begin
  // Create custom page
  gConfigPage := CreateInputOptionPage(
    wpSelectProgramGroup,
    'Configure CodeSnip',
    'Should Setup preserve your old installation''s database and '
      + 'configuration?',
    'This version of CodeSnip stores its data differently to your earlier '
      + 'version. Setup can attempt to copy any existing database files and '
      + 'preserve your earlier settings. Choose what action to take below.',
    False,
    False
  );
  // Add check boxes if required
  if DataConversionRequired then
  begin
    gConfigPage.Add('Copy existing settings and database');
    gConfigPage.Values[0] := True;
    gConfigPage.Add('Delete old settings and database after copying');
    gConfigPage.Values[1] := False;
  end;
end;

// Called to determine whether page specified by PageID should be displayed.
// Display of custom page is inhibited only if no data conversion is necessary.
function ShouldSkipPage(PageID: Integer): Boolean;
begin
  Result := False;
  if PageID = gConfigPage.ID then
  begin
    if not DataConversionRequired then
      Result := True;
  end;
end;

// Called when "Next" button is clicked on page specified by CurPageID. Used to
// record state of check boxes on custom page.
function NextButtonClick(CurPageID: Integer): Boolean;
begin
  Result := True;
  if CurPageID = gConfigPage.ID then
  begin
    gDataConversionRequested := gConfigPage.Values[0];
    gDeleteOldDataRequested := gConfigPage.Values[1];
  end;
end;

// Helper routine for UpdateReadyMemo. Appends text to a string followed by two
// newlines (per NewLine parameter) providing text is not an empty string.
// Returns updated string in Res.
procedure AddTextToMemo(Text: string; var Res: string; NewLine: string);
begin
  if Text <> '' then
    Res := Res + Text + NewLine + NewLine;
end;

// Called to determine text to be displayed on "Ready to install" wizard page.
// We use all given information and add information about any data conversion to
// be carried out.
// See Inno Setup docs for details of parameters.
function UpdateReadyMemo(Space, NewLine, MemoUserInfoInfo, MemoDirInfo,
  MemoTypeInfo, MemoComponentsInfo, MemoGroupInfo,
  MemoTasksInfo: String): String;
begin
  Result := '';
  // Use all given info: empty strings ignored
  AddTextToMemo(MemoUserInfoInfo, Result, NewLine);
  AddTextToMemo(MemoDirInfo, Result, NewLine);
  AddTextToMemo(MemoTypeInfo, Result, NewLine);
  AddTextToMemo(MemoComponentsInfo, Result, NewLine);
  AddTextToMemo(MemoGroupInfo, Result, NewLine);
  AddTextToMemo(MemoTasksInfo, Result, NewLine);
  // Add custom information
  if DataConversionRequired then
  begin
    Result := Result +  'Data preservation:';
    Result := Result + NewLine + Space;
    if gDataConversionRequested then
      Result := Result + 'Copy existing database and settings'
    else
      Result := Result + 'Don''t copy database and settings'
        + NewLine + Space + Space
        + 'A new database must be downloaded when the program is run'
        + NewLine + Space + Space
        + 'Registered programs will need re-registering';
    Result := Result + NewLine + Space;
    if gDeleteOldDataRequested then
      Result := Result + 'Old database and settings will be deleted'
    else
      Result := Result + 'Old database and settings will be retained'
  end;
end;

// Called to perform pre-install and post-install tasks. We performs any
// required data conversion at the post-install stage.
procedure CurStepChanged(CurStep: TSetupStep);
var
  PrevInstallID: Integer; // loops through previous installs
  FileName: string;       // name of a config file
  DirName: string;        // name of a database directory
begin
  // We perform ini file post-install
  if CurStep <> ssPostInstall then
    Exit;

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

    // Check if old data should be deleted
    if gDeleteOldDataRequested then
    begin
      for PrevInstallID := piOriginal to piCurrent - 1 do
      begin
        FileName := gCommonConfigFiles[PrevInstallId];
        if (FileName <> '') and (FileName <> gCurrentCommonConfigFile)
          and FileExists(FileName) then
          DeleteFile(FileName);
        FileName := gUserConfigFiles[PrevInstallId];
        if (FileName <> '') and (FileName <> gCurrentUserConfigFile)
          and FileExists(FileName) then
          DeleteFile(FileName);
      end;
      for PrevInstallID := piOriginal to piCurrent - 1 do
      begin
        DirName := gMainDatabaseDirs[PrevInstallID];
        if (DirName <> '') and (DirName <> gMainDatabaseDirs[piCurrent])
          and DirExists(DirName) then
          DelTree(DirName, True, True, True);
        DirName := gUserDatabaseDirs[PrevInstallID];
        if (DirName <> '') and (DirName <> gUserDatabaseDirs[piCurrent])
          and DirExists(DirName) then
          DelTree(DirName, True, True, True);
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
  if (UserConfigFileVer < 7) and
    (
      GetIniString('ProxyServer', 'Password', '', gCurrentUserConfigFile) <> ''
    ) then
  begin
    DeleteProxyPassword;
    MsgBox(
      'Your existing proxy server password has been deleted. This is because '
       + 'the format for storing the password has been changed to permit '
       + 'non European characters to be used.'#13#10#13#10
       + 'When you start CodeSnip please select the Tools | Proxy Server '
       + 'menu option and re-enter your password in the dialog box.'#13#10#13#10
       + 'Sorry for the inconvenience.',
      mbInformation,
      MB_OK
    );
  end;

  // Ensure ini files have correct ini file and program version information
  // This creates ini files if they don't exist
  StampConfigFiles;

  // Display message if no database is installed
  if not MainDatabaseExists then
    MsgBox(
      'The Code Snippets database is not currently installed. '
        + 'Therefore when you first start CodeSnip no snippets will be '
        + 'displayed.'#10#10
        + 'You can download the database using the program''s "Database | '
        + 'Update From Web" menu option.',
      mbInformation,
      MB_OK
    );
end;


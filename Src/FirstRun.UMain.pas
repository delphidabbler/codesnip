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


uses
  // Project
  FirstRun.UConfigFile, FirstRun.UDatabase, FirstRun.UInstallInfo;


type
  ///  <summary>Enumeration of changes that can be made to brought forward
  ///  config files that result in data loss.</summary>
  TFirstRunCfgChanges = (
    frcRegistration,    // local registration record lost
    frcHiliter,         // syntax highlighter customisation lost
    frcProxyPwd,        // internet proxy password lost
    frcSourceFormat     // source code output formatting lost
  );

type
  ///  <summary>Set of first run changes to brought forward config file that can
  ///  result in data loss.</summary>
  TFirstRunCfgChangeSet = set of TFirstRunCfgChanges;

type
  ///  <summary>Class that provides information about current and previous
  ///  installations of CodeSnip along with operations to carry forward user
  ///  preferences and database and to update user config file when necessary.
  ///  </summary>
  TFirstRun = class(TObject)
  strict private
    var
      ///  <summary>Object that provides information about current and earlier
      ///  installations.</summary>
      fInstallInfo: TInstallInfo;
      ///  <summary>Object that interogates and updates user's config file.
      ///  </summary>
      fConfigFile: TUserConfigFileUpdater;
      ///  <summary>Object used to copy forward older versions of user database.
      ///  </summary>
      fDatabase: TUserDatabaseUpdater;
    ///  <summary>Checks if config file uses earlier format for storing proxy
    ///  server passwords.</summary>
    function HasOldStyleProxyPwd: Boolean;
  public
    ///  <summary>Constructs object and owned object.</summary>
    constructor Create;
    ///  <summary>Frees object and owned objects.</summary>
    destructor Destroy; override;
    ///  <summary>Checks if a user config file exists for an earlier CodeSnip
    ///  installation.</summary>
    function HaveOldCfgFile: Boolean;
    ///  <summary>Copies user config file from an earlier CodeSnip installation.
    ///  </summary>
    procedure BringForwardCfgFile;
    ///  <summary>Updates current version's user config file in place as
    ///  necessary and notifies caller of changes via Changes parameter.
    /// </summary>
    procedure UpdateCfgFile(out Changes: TFirstRunCfgChangeSet);
    ///  <summary>Checks if a user database exist for an earlier CodeSnip
    ///  installation.</summary>
    function HaveOldUserDB: Boolean;
    ///  <summary>Copies user database from an earlier CodeSnip installation.
    ///  </summary>
    procedure BringForwardUserDB;
    ///  <summary>Creates a new, empty, Unicode encoded config file for current
    ///  installation.</summary>
    procedure CreateEmptyCfgFile;
    ///  <summary>
    function IsProgramUpdated: Boolean;
  end;

type
  ///  <summary>Static class that manages program's first run processing.
  ///  </summary>
  ///  <remarks>
  ///  <para>Designed to be called before any program preferences are read.
  ///  </para>
  ///  <para>If current major version of program has not been run before and
  ///  user data from an earlier version is detected then a dialogue box is
  ///  displayed to give user various options to bring forward data.</para>
  ///  <para>If major version is unchanged but minor version, or user config
  ///  file version, has changed then config file may be modified.</para>
  ///  </remarks>
  TFirstRunMgr = class(TObject)
  strict private
    ///  <summary>Checks if user config file exists for current program version.
    ///  </summary>
    class function CfgFileExists: Boolean;
    ///  <summary>Determines if this is first time this major version of the
    ///  program has been run.</summary>
    class function IsFirstRun: Boolean;
    ///  <summary>Determines if program has been updated since last run.
    ///  </summary>
    class function IsProgramUpdated: Boolean;
  public
    ///  <summary>Runs start-up checks to detect if program has been run before
    ///  and performs any required user config and user database updates.
    ///  </summary>
    class procedure Execute;
  end;


implementation


uses
  // Delphi
  SysUtils, IOUtils, Forms,
  // Project
  FirstRun.FmV4ConfigDlg;


{ TFirstRun }

procedure TFirstRun.BringForwardCfgFile;
begin
  Assert(HaveOldCfgFile,
    ClassName + '.BringForwardCfgFile: Old config file does not exist');
  fConfigFile.CopyFile(
    fInstallInfo.PreviousUserConfigFileName,
    fInstallInfo.IsPreviousUserConfigFileANSI
  );
end;

procedure TFirstRun.BringForwardUserDB;
begin
  Assert(HaveOldUserDB,
    ClassName + '.BringForwardUserDB: Old user database does not exist');
  fDatabase.CopyDatabase(fInstallInfo.PreviousUserDatabaseDir);
end;

constructor TFirstRun.Create;
begin
  inherited Create;
  fInstallInfo := TInstallInfo.Create;
  fConfigFile := TUserConfigFileUpdater.Create(
    fInstallInfo.CurrentUserConfigFileName
  );
  fDatabase := TUserDatabaseUpdater.Create(
    fInstallInfo.CurrentUserDatabaseDir
  );
end;

procedure TFirstRun.CreateEmptyCfgFile;
begin
  fConfigFile.CreateNewFile;
end;

destructor TFirstRun.Destroy;
begin
  fDatabase.Free;
  fConfigFile.Free;
  fInstallInfo.Free;
  inherited;
end;

function TFirstRun.HasOldStyleProxyPwd: Boolean;
begin
  Result := (fConfigFile.FileVer <= 6) and fConfigFile.HasProxyPassword;
end;

function TFirstRun.HaveOldCfgFile: Boolean;
begin
  Result := TFile.Exists(fInstallInfo.PreviousUserConfigFileName);
end;

function TFirstRun.HaveOldUserDB: Boolean;
begin
  Result := TFile.Exists(fInstallInfo.PreviousUserDatabaseFileName);
end;

function TFirstRun.IsProgramUpdated: Boolean;
begin
  Result := fConfigFile.IsCurrentProgramVer;
end;

procedure TFirstRun.UpdateCfgFile(out Changes: TFirstRunCfgChangeSet);
begin
  Changes := [];
  case fInstallInfo.InstallID of
    piOriginal:
    begin
      fConfigFile.UpdateFromOriginal;
      Include(Changes, frcHiliter);
      Include(Changes, frcRegistration);
      Include(Changes, frcSourceFormat);
    end;
    piV1_9, piV2:
    begin
      fConfigFile.DeleteHighligherPrefs;
      Include(Changes, frcHiliter);
    end;
    piV3:
    begin
      if HasOldStyleProxyPwd then
      begin
        fConfigFile.DeleteProxyPassword;
        Include(Changes, frcProxyPwd);
      end;
    end;
  end;
  if fConfigFile.FileVer < 6 then
    // User ini file versions before 6 don't have the Prefs:CodeGen section and
    // default entries for predefined warnings.
    // NOTE: This works for a new config file providing it has not been stamped.
    fConfigFile.CreateDefaultCodeGenEntries;

  if fConfigFile.FileVer < 9 then
  begin
    fConfigFile.DeleteDetailsPaneIndex; // can be v8 file even tho not supported
    fConfigFile.UpdateCodeGenEntries;
  end;

  fConfigFile.Stamp;
end;

{ TFirstRunMgr }

class function TFirstRunMgr.CfgFileExists: Boolean;
begin
  Result := TFile.Exists(TInstallInfo.CurrentUserConfigFileName);
end;

class procedure TFirstRunMgr.Execute;
var
  FR: TFirstRun;
  Changes: TFirstRunCfgChangeSet;
begin
  if IsFirstRun then
  begin
    FR := TFirstRun.Create;
    try
      if FR.HaveOldCfgFile or FR.HaveOldUserDB then
        TV4ConfigDlg.Execute(Application, FR);
      if not CfgFileExists then
      begin
        FR.CreateEmptyCfgFile;
        FR.UpdateCfgFile(Changes);
      end;
    finally
      FR.Free;
    end;
  end
  else if IsProgramUpdated then
  begin
    FR := TFirstRun.Create;
    try
      FR.UpdateCfgFile(Changes);
    finally
      FR.Free;
    end;
  end;
end;

class function TFirstRunMgr.IsFirstRun: Boolean;
begin
  Result := not CfgFileExists;
end;

class function TFirstRunMgr.IsProgramUpdated: Boolean;
begin
  Result := not TUserConfigFileUpdater.IsCurrentProgramVer(
    TInstallInfo.CurrentUserConfigFileName
  );
end;

end.


{
 * This Source Code Form is subject to the terms of the Mozilla Public License,
 * v. 2.0. If a copy of the MPL was not distributed with this file, You can
 * obtain one at http://mozilla.org/MPL/2.0/
 *
 * Copyright (C) 2008-2012, Peter Johnson (www.delphidabbler.com).
 *
 * $Rev$
 * $Date$
 *
 * Implements classes that handle application start up, determine if it the
 * first run of the current version and perform necessary updates to per-user
 * config file and database.
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
      fUserConfigFile: TUserConfigFileUpdater;
      ///  <summary>Object that interogates and updates common config file.
      ///  </summary>
      fCommonConfigFile: TCommonConfigFileUpdater;
      ///  <summary>Object used to copy forward older versions of user database.
      ///  </summary>
      fDatabase: TUserDatabaseUpdater;
    {$IFNDEF PORTABLE}
    ///  <summary>Checks if config file uses earlier format for storing proxy
    ///  server passwords.</summary>
    function HasOldStyleProxyPwd: Boolean;
    {}{$ENDIF}
  public
    ///  <summary>Constructs object and owned object.</summary>
    constructor Create;
    ///  <summary>Frees object and owned objects.</summary>
    destructor Destroy; override;
    ///  <summary>Checks if a user config file exists for an earlier CodeSnip
    ///  installation.</summary>
    function HaveOldUserCfgFile: Boolean;
    ///  <summary>Copies user config file from an earlier CodeSnip installation.
    ///  </summary>
    procedure BringForwardUserCfgFile;
    ///  <summary>Updates current version's user config file in place as
    ///  necessary and notifies caller of changes via Changes parameter.
    /// </summary>
    procedure UpdateUserCfgFile(out Changes: TFirstRunCfgChangeSet);
    ///  <summary>Checks if a user database exist for an earlier CodeSnip
    ///  installation.</summary>
    function HaveOldUserDB: Boolean;
    ///  <summary>Copies user database from an earlier CodeSnip installation.
    ///  </summary>
    procedure BringForwardUserDB;
    ///  <summary>Creates a new, empty, Unicode encoded per-user config file for
    ///  current installation.</summary>
    procedure CreateEmptyUserCfgFile;
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
    class function UserCfgFileExists: Boolean;
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
  SysUtils, IOUtils, Forms
  {$IFNDEF PORTABLE}
  // Project
  ,
  FirstRun.FmV4ConfigDlg;
  {$ELSE}
  ;
  {$ENDIF}


{ TFirstRun }

procedure TFirstRun.BringForwardUserCfgFile;
begin
  Assert(HaveOldUserCfgFile,
    ClassName + '.BringForwardUserCfgFile: Old config file does not exist');
  fUserConfigFile.CopyFile(
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
  fUserConfigFile := TUserConfigFileUpdater.Create(
    fInstallInfo.CurrentUserConfigFileName
  );
  fCommonConfigFile := TCommonConfigFileUpdater.Create(
    fInstallInfo.CurrentCommonConfigFileName
  );
  fDatabase := TUserDatabaseUpdater.Create(
    fInstallInfo.CurrentUserDatabaseDir
  );
end;

procedure TFirstRun.CreateEmptyUserCfgFile;
begin
  fUserConfigFile.CreateNewFile;
end;

destructor TFirstRun.Destroy;
begin
  fDatabase.Free;
  fCommonConfigFile.Free;
  fUserConfigFile.Free;
  fInstallInfo.Free;
  inherited;
end;

{$IFNDEF PORTABLE}
function TFirstRun.HasOldStyleProxyPwd: Boolean;
begin
  Result := (fUserConfigFile.FileVer <= 6) and fUserConfigFile.HasProxyPassword;
end;
{$ENDIF}

function TFirstRun.HaveOldUserCfgFile: Boolean;
begin
  Result := TFile.Exists(fInstallInfo.PreviousUserConfigFileName);
end;

function TFirstRun.HaveOldUserDB: Boolean;
begin
  Result := TFile.Exists(fInstallInfo.PreviousUserDatabaseFileName);
end;

function TFirstRun.IsProgramUpdated: Boolean;
begin
  Result := fUserConfigFile.IsCurrentProgramVer;
end;

procedure TFirstRun.UpdateUserCfgFile(out Changes: TFirstRunCfgChangeSet);
begin
  Changes := [];

  {$IFNDEF PORTABLE}
  case fInstallInfo.InstallID of
    piOriginal:
    begin
      fUserConfigFile.UpdateFromOriginal;
      Include(Changes, frcHiliter);
      Include(Changes, frcRegistration);
      Include(Changes, frcSourceFormat);
    end;
    piV1_9, piV2:
    begin
      fUserConfigFile.DeleteHighligherPrefs;
      Include(Changes, frcHiliter);
    end;
    piV3:
    begin
      if HasOldStyleProxyPwd then
      begin
        fUserConfigFile.DeleteProxyPassword;
        Include(Changes, frcProxyPwd);
      end;
    end;
  end;
  {$ENDIF}

  if fUserConfigFile.FileVer < 6 then
    // User ini file versions before 6 don't have the Prefs:CodeGen section and
    // default entries for predefined warnings.
    // NOTE: This works for a new config file providing it has not been stamped:
    // we rely on this for portable version.
    fUserConfigFile.CreateDefaultCodeGenEntries;

  {$IFNDEF PORTABLE}
  if fUserConfigFile.FileVer < 9 then
  begin
    fUserConfigFile.DeleteDetailsPaneIndex;
    fUserConfigFile.UpdateCodeGenEntries;
  end;
  {$ENDIF}

  fUserConfigFile.Stamp;
  // NOTE: strictly speaking we only need to stamp common config file in
  // portable version. Installer does this in normal version. However, it does
  // no harm to stamp this file twice - belt and braces!
  fCommonConfigFile.Stamp;
end;

{ TFirstRunMgr }

class procedure TFirstRunMgr.Execute;
var
  FR: TFirstRun;
  Changes: TFirstRunCfgChangeSet;
begin
  if IsFirstRun then
  begin
    FR := TFirstRun.Create;
    try
      {$IFNDEF PORTABLE}
      if FR.HaveOldUserCfgFile or FR.HaveOldUserDB then
        TV4ConfigDlg.Execute(Application, FR);
      {$ENDIF}
      if not UserCfgFileExists then
      begin
        FR.CreateEmptyUserCfgFile;
        FR.UpdateUserCfgFile(Changes);
      end;
    finally
      FR.Free;
    end;
  end
  else if IsProgramUpdated then
  begin
    FR := TFirstRun.Create;
    try
      FR.UpdateUserCfgFile(Changes);
    finally
      FR.Free;
    end;
  end;
end;

class function TFirstRunMgr.IsFirstRun: Boolean;
begin
  Result := not UserCfgFileExists;
end;

class function TFirstRunMgr.IsProgramUpdated: Boolean;
begin
  Result := not TUserConfigFileUpdater.IsCurrentProgramVer(
    TInstallInfo.CurrentUserConfigFileName
  );
end;

class function TFirstRunMgr.UserCfgFileExists: Boolean;
begin
  Result := TFile.Exists(TInstallInfo.CurrentUserConfigFileName);
end;

end.


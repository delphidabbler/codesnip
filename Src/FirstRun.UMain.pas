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
  FirstRun.UUpdateIni, FirstRun.UUpdateDBase;

type
  TFirstRunCfgChanges = (
    frcRegistration,
    frcHiliter,
    frcProxyPwd,
    frcSourceFormat
  );

type
  TFirstRunCfgChangeSet = set of TFirstRunCfgChanges;

type
  TFirstRun = class(TObject)
  strict private
    var
      fConfigFile: TUserConfigFileUpdater;
      fDatabase: TUserDatabaseUpdater;
    function HasOldStyleProxyPwd: Boolean;
  public
    constructor Create;
    destructor Destroy; override;
    function HaveOldCfgFile: Boolean;
    // Brings forward config file from older version
    procedure BringForwardCfgFile;
    // Updates config file in place and notofies of changes
    procedure UpdateCfgFile(out Changes: TFirstRunCfgChangeSet);
    function HaveOldUserDB: Boolean;
    procedure BringForwardUserDB;
    procedure CreateEmptyCfgFile;
    function IsProgramUpdated: Boolean;
  end;

type
  TFirstRunMgr = class(TObject)
  strict private
    class function CfgFileExists: Boolean;
    class function IsFirstRun: Boolean;
    class function IsProgramUpdated: Boolean;
  public
    class procedure Execute;
  end;

implementation

uses
  // Delphi
  SysUtils, IOUtils, Forms,
  // Project
  FirstRun.FmV4ConfigDlg, FirstRun.UDataLocations;

{ TFirstRun }

procedure TFirstRun.BringForwardCfgFile;
begin
  Assert(HaveOldCfgFile,
    ClassName + '.BringForwardCfgFile: Old config file does not exist');
  fConfigFile.CopyFile(
    gUserConfigFiles[gPrevInstallID],
    FirstRun.UDataLocations.IsAnsiConfigFile(gPrevInstallID)
  );
end;

procedure TFirstRun.BringForwardUserDB;
begin
  Assert(HaveOldUserDB,
    ClassName + '.BringForwardUserDB: Old user database does not exist');
  fDatabase.CopyDatabase(gUserDatabaseDirs[gPrevInstallID]);
end;

constructor TFirstRun.Create;
begin
  inherited Create;
  fConfigFile := TUserConfigFileUpdater.Create(gCurrentUserConfigFile);
  fDatabase := TUserDatabaseUpdater.Create(gUserDatabaseDirs[piCurrent]);
end;

procedure TFirstRun.CreateEmptyCfgFile;
begin
  fConfigFile.CreateNewFile;
end;

destructor TFirstRun.Destroy;
begin
  fDatabase.Free;
  fConfigFile.Free;
  inherited;
end;

function TFirstRun.HasOldStyleProxyPwd: Boolean;
begin
  Result := (fConfigFile.FileVer <= 6) and fConfigFile.HasProxyPassword;
end;

function TFirstRun.HaveOldCfgFile: Boolean;
begin
  Result := TFile.Exists(gUserConfigFiles[gPrevInstallID]);
end;

function TFirstRun.HaveOldUserDB: Boolean;
begin
  Result := TFile.Exists(
    IncludeTrailingPathDelimiter(gUserDatabaseDirs[gPrevInstallID])
      + 'database.xml'
  );
end;

function TFirstRun.IsProgramUpdated: Boolean;
begin
  Result := fConfigFile.IsCurrentProgramVer;
end;

procedure TFirstRun.UpdateCfgFile(out Changes: TFirstRunCfgChangeSet);
begin
  Changes := [];
  case gPrevInstallID of
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
  Result := TFile.Exists(gCurrentUserConfigFile);
end;

class procedure TFirstRunMgr.Execute;
var
  FR: TFirstRun;
  Changes: TFirstRunCfgChangeSet;
begin
  FirstRun.UDataLocations.InitGlobals;
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
    gCurrentUserConfigFile
  );
end;

end.


{
 * This Source Code Form is subject to the terms of the Mozilla Public License,
 * v. 2.0. If a copy of the MPL was not distributed with this file, You can
 * obtain one at http://mozilla.org/MPL/2.0/
 *
 * Copyright (C) 2008-2013, Peter Johnson (www.delphidabbler.com).
 *
 * $Rev$
 * $Date$
 *
 * Implements a class that provides information about CodeSnip installations.
}


unit FirstRun.UInstallInfo;


interface


type
  ///  <summary>Identifiers for different type of CodeSnip installations that
  ///  can be detected.</summary>
  TInstallId = (
    piNone,       // CodeSnip not installed
    piOriginal,   // "original" installation up to v1.8.11
    piV1_9,       // v1.9 to v1.9.4
    piV2,         // all v2 versions
    piV3,         // all v3 versions
    piV4,         // from v4.0 (including alpha & beta code v3.98.x & v3.99.x
    piV5          // v5 (including all alpha and beta code from v4.97)
  );

type
  ///  <summary>Class that provides information about CodeSnip installations.
  ///  </summary>
  TInstallInfo = class(TObject)
  strict private
    const
      ///  <summary>ID of current version of CodeSnip.</summary>
      CurrentVersionID = High(TInstallId);
    var
      ///  <summary>Value of InstallID property.</summary>
      fInstallID: TInstallId;
    ///  <summary>Maps given install IDs to relative paths to user config file
    ///  for that installation version.</summary>
    class function UserConfigFileNames(const InstID: TInstallId): string;
    ///  <summary>Maps given install IDs to relative paths to user database
    ///  directory for that installation version.</summary>
    class function DatabaseDirs(const InstID: TInstallId): string;
    ///  <summary>Converts given relative directory path or file name into an
    ///  absolute path by pre-prending the user's application data folder.
    ///  </summary>
    ///  <remarks>If Name = '' then '' is returned.</remarks>
    class function MakeFullUserPath(const Name: string): string;
    ///  <summary>Informs if user config file used by given installation ID is
    ///  in ANSI format.</summary>
    class function IsUserConfigFileANSI(const InstallID: TInstallId): Boolean;
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
    ///  <summary>Returns full path to common config file for current version of
    ///  CodeSnip.</summary>
    function CurrentCommonConfigFileName: string;
    ///  <summary>ID of latest CodeSnip install found.</summary>
    ///  <remarks>This is ID of latest version for which user data can be found.
    ///  </remarks>
    property InstallID: TInstallId read fInstallID;
  end;

implementation


uses
  // Delphi
  SysUtils,
  IOUtils,
  // Project
  CS.Init.CommandLineOpts,
  FirstRun.UIniFile,
  UAppInfo,
  UIOUtils,
  UStrUtils,
  USystemInfo;


{ TInstallInfo }

constructor TInstallInfo.Create;
begin
  inherited Create;
  DetectInstall;
end;

function TInstallInfo.CurrentCommonConfigFileName: string;
begin
  Result := IncludeTrailingPathDelimiter(TAppInfo.CommonAppDir)
    + 'Common.config';
end;

class function TInstallInfo.CurrentUserConfigFileName: string;
begin
  Result := MakeFullUserPath(UserConfigFileNames(CurrentVersionID));
end;

class function TInstallInfo.CurrentUserDatabaseDir: string;
begin
  Result := MakeFullUserPath(DatabaseDirs(CurrentVersionID));
end;

class function TInstallInfo.DatabaseDirs(const InstID: TInstallId): string;

  // Gets user database directory from config file associated with InstID.
  // Returns DefaultDir if config file doesn't reference database directory.
  function DBDirFromCfg(const DefaultDir: string): string;
  begin
    Result := GetIniString(
      'Database',
      'UserDataDir',
      DefaultDir,
      UserConfigFileNames(InstID)
    );
  end;

begin
  Result := '';
  if TCommandLineOpts.IsPortable then
  begin
    case InstID of
      // v3 and earlier did not support portability
      piV4: Result := 'AppData\UserDB';
      piV5: Result := 'AppData.5\Database';
    end;
  end
  else
  begin
    case InstID of
      piV2: Result := 'DelphiDabbler\CodeSnip\UserData';
      piV3: Result := 'DelphiDabbler\CodeSnip\UserData.3';
      piV4: Result := DBDirFromCfg('DelphiDabbler\CodeSnip.4\UserDatabase');
      piV5: Result := DBDirFromCfg('DelphiDabbler\CodeSnip.5\Database');
    end;
  end;
end;

procedure TInstallInfo.DetectInstall;

  // Checks if user config file for given installation exists and is non-empty.
  function NonEmptyCfgFileExists(const InstID: TInstallID): Boolean;
  var
    CfgFileName: string;  // cfg file name
    Content: string;      // content of cfg file
    Encoding: TEncoding;  // encoding of cfg file
  begin
    CfgFileName := MakeFullUserPath(UserConfigFileNames(InstID));
    if (CfgFileName = EmptyStr) or not TFile.Exists(CfgFileName) then
      Exit(False);
    if IsUserConfigFileANSI(InstID) then
      Encoding := TEncoding.Default
    else
      Encoding := TEncoding.Unicode;
    Content := StrTrim(
      TFileIO.ReadAllText(CfgFileName, Encoding, True)
    );
    Result := Content <> '';
  end;

begin
  fInstallID := piNone;
  if NonEmptyCfgFileExists(piV5) then
    fInstallID := piV5
  else if NonEmptyCfgFileExists(piV4) then
    fInstallID := piV4
  else if not TCommandLineOpts.IsPortable then
  begin
    if NonEmptyCfgFileExists(piV3) then
      fInstallID := piV3
    else if TDirectory.Exists(MakeFullUserPath(DatabaseDirs(piV2))) then
      fInstallID := piV2
    else if NonEmptyCfgFileExists(piV1_9) then
      fInstallID := piV1_9
    else if NonEmptyCfgFileExists(piOriginal) then
      fInstallID := piOriginal;
  end;
end;

function TInstallInfo.IsPreviousUserConfigFileANSI: Boolean;
begin
  Result := IsUserConfigFileANSI(fInstallID);
end;

class function TInstallInfo.IsUserConfigFileANSI(const InstallID: TInstallId):
  Boolean;
begin
  if TCommandLineOpts.IsPortable then
    Result := False
  else
    Result := InstallID <= piV3;
end;

class function TInstallInfo.MakeFullUserPath(const Name: string): string;
begin
  if Name = '' then
    Exit('');
  Result := IncludeTrailingPathDelimiter(
    StrIf(
      TCommandLineOpts.IsPortable,
      TAppInfo.AppExeDir,
      TSystemFolders.PerUserAppData
    )
  ) + Name;
end;

function TInstallInfo.PreviousUserConfigFileName: string;
begin
  Result := MakeFullUserPath(UserConfigFileNames(fInstallID));
end;

function TInstallInfo.PreviousUserDatabaseDir: string;
begin
  Result := MakeFullUserPath(DatabaseDirs(fInstallID));
end;

function TInstallInfo.PreviousUserDatabaseFileName: string;
begin
  Result := IncludeTrailingPathDelimiter(PreviousUserDatabaseDir)
    + 'database.xml';
end;

class function TInstallInfo.UserConfigFileNames(
  const InstID: TInstallId): string;
begin
  Result := '';
  if TCommandLineOpts.IsPortable then
  begin
    case InstID of
      // v3 and earlier did not support portability
      piV4: Result := 'AppData\User.config';
      piV5: Result := 'AppData.5\User.config';
    end;
  end
  else
  begin
    case InstID of
      piOriginal: Result := 'DelphiDabbler\CodeSnip\CodeSnip.ini';
      piV1_9, piV2: Result := 'DelphiDabbler\CodeSnip\User.ini';
      piV3: Result := 'DelphiDabbler\CodeSnip\User.3.ini';
      piV4: Result := 'DelphiDabbler\CodeSnip.4\User.config';
      piV5: Result := 'DelphiDabbler\CodeSnip.5\User.config';
    end;
  end;
end;

end.


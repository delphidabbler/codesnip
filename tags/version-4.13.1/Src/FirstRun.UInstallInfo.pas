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

  + From v4.0 (standard version):
    - Config file: %AppData%\DelphiDabbler\CodeSnip.4\User.config
    - Database directory: %AppData%\DelphiDabbler\CodeSnip.4\UserDatabase
  + From v4.0 (portable version):
    - Config file: <exe-dir>\AppData\User.config
    - Database directory: <exe-dir>\AppData\UserDB
    where <exe-dir> is the directory from which the program is executing.
}

type
  ///  <summary>Identifiers for different type of CodeSnip installations that
  ///  can be detected.</summary>
  TInstallId = (
    piNone,       // CodeSnip not installed
    {$IFNDEF PORTABLE}
    piOriginal,   // "original" installation up to v1.8.11
    piV1_9,       // v1.9 to v1.9.4
    piV2,         // all v2 versions
    piV3,         // all v3 versions
    {$ENDIF}
    piV4          // from v4.0 (including alpha & beta code v3.98.x & v3.99.x
  );

type
  ///  <summary>Class that provides information about CodeSnip installations.
  ///  </summary>
  TInstallInfo = class(TObject)
  strict private
    const
      ///  <summary>ID of current version of CodeSnip.</summary>
      CurrentVersionID = High(TInstallId);
      ///  <summary>Array mapping install IDs to relative paths to user config
      ///  file for for that installation version.</summary>
      UserConfigFileNames: array[Low(TInstallId)..CurrentVersionID] of string =
        (
          {$IFNDEF PORTABLE}
          '',
          'DelphiDabbler\CodeSnip\CodeSnip.ini',
          'DelphiDabbler\CodeSnip\User.ini',
          'DelphiDabbler\CodeSnip\User.ini',
          'DelphiDabbler\CodeSnip\User.3.ini',
          'DelphiDabbler\CodeSnip.4\User.config'
          {$ELSE}
          '',
          'AppData\User.config'
          {$ENDIF}
        );
      ///  <summary>Array mapping install IDs to relative paths to user database
      ///  directories for for that installation version.</summary>
      DatabaseDirs: array[Low(TInstallId)..CurrentVersionID] of string =
        (
          {$IFNDEF PORTABLE}
          '',
          '',
          '',
          'DelphiDabbler\CodeSnip\UserData',
          'DelphiDabbler\CodeSnip\UserData.3',
          'DelphiDabbler\CodeSnip.4\UserDatabase'
          {$ELSE}
          '',
          'AppData\UserDB'
          {$ENDIF}
        );
    var
      ///  <summary>Value of InstallID property.</summary>
      fInstallID: TInstallId;
    ///  <summary>Converts given relative directory path or file name into an
    ///  absolute path by pre-prending the user's application data folder.
    ///  </summary>
    ///  <remarks>If Name = '' then '' is returned.</remarks>
    class function MakeFullUserPath(const Name: string): string;
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
  SysUtils, IOUtils,
  // Project
  UAppInfo, UIOUtils, UStrUtils, USystemInfo;


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
  Result := MakeFullUserPath(UserConfigFileNames[CurrentVersionID]);
end;

class function TInstallInfo.CurrentUserDatabaseDir: string;
begin
  Result := MakeFullUserPath(DatabaseDirs[CurrentVersionID]);
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
  if TFile.Exists(MakeFullUserPath(UserConfigFileNames[piV4]), False) and not
    IsEmptyUnicodeCfgFile(MakeFullUserPath(UserConfigFileNames[piV4])) then
    fInstallID := piV4
  {$IFNDEF PORTABLE}
  else if TFile.Exists(MakeFullUserPath(UserConfigFileNames[piV3]), False) then
    fInstallID := piV3
  else if TDirectory.Exists(MakeFullUserPath(DatabaseDirs[piV2])) then
    fInstallID := piV2
  else if TFile.Exists(
    MakeFullUserPath(UserConfigFileNames[piV1_9]), False
  ) then
    fInstallID := piV1_9
  else if TFile.Exists(
    MakeFullUserPath(UserConfigFileNames[piOriginal]), False
  ) then
    fInstallID := piOriginal
  {$ENDIF}
  else
    fInstallID := piNone;
end;

function TInstallInfo.IsPreviousUserConfigFileANSI: Boolean;
begin
  {$IFNDEF PORTABLE}
  Result := fInstallID <= piV3;
  {$ELSE}
  Result := False;
  {$ENDIF}
end;

class function TInstallInfo.MakeFullUserPath(const Name: string): string;
begin
  if Name = '' then
    Exit('');
  {$IFNDEF PORTABLE}
  Result := IncludeTrailingPathDelimiter(TSystemFolders.PerUserAppData) + Name;
  {$ELSE}
  Result := IncludeTrailingPathDelimiter(TAppInfo.AppExeDir) + Name;
  {$ENDIF}
end;

function TInstallInfo.PreviousUserConfigFileName: string;
begin
  Result := MakeFullUserPath(UserConfigFileNames[fInstallID]);
end;

function TInstallInfo.PreviousUserDatabaseDir: string;
begin
  Result := MakeFullUserPath(DatabaseDirs[fInstallID]);
end;

function TInstallInfo.PreviousUserDatabaseFileName: string;
begin
  Result := IncludeTrailingPathDelimiter(PreviousUserDatabaseDir)
    + 'database.xml';
end;

end.


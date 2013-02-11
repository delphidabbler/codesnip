{
 * This Source Code Form is subject to the terms of the Mozilla Public License,
 * v. 2.0. If a copy of the MPL was not distributed with this file, You can
 * obtain one at http://mozilla.org/MPL/2.0/
 *
 * Copyright (C) 2012-2013, Peter Johnson (www.delphidabbler.com).
 *
 * $Rev$
 * $Date$
 *
 * Encapsulates code executed at program's startup. Handles processing used on
 * the first run of the application and, for the portable version only, the
 * creation of a user data sub-directory of the program directory.
}


unit UStartUp;

interface

uses
  // Delphi
  SysUtils;   // Ensure Exception class and handling are available

type

  TStartUp = record
  strict private
    class procedure ErrorMessage(const Msg: string); static;
  public
    class function Execute: Boolean; static;
  end;

implementation

uses
  // Delphi
  Windows,
  {$IFDEF PORTABLE}
  // Delphi
  IOUtils,
  // Project
  UAppInfo, UUtils,
  {$ENDIF}
  // Project
  FirstRun.UMain, UConsts, USystemInfo;

{ TStartUp }

class procedure TStartUp.ErrorMessage(const Msg: string);
resourcestring
  sTitle = 'CodeSnip';
  sPrefix = 'CODESNIP CANNOT START!';
begin
  MessageBeep(MB_ICONERROR);
  MessageBox(0, PChar(sPrefix + EOL2 + Msg), PChar(sTitle), MB_OK);
end;

class function TStartUp.Execute: Boolean;
resourcestring
  sOSError = 'Windows 2000 or later is required';
  sIEError = 'Internet Explorer v6 or later is required.';
{$IFDEF PORTABLE}
var
  WorkingDir: string;
{$ENDIF}
begin

  if not TOSInfo.IsWinNT or not TOSInfo.CheckReportedOS(TOSInfo.Win2K) then
  begin
    ErrorMessage(sOSError);
    Exit(False);
  end;
  if TOSInfo.BrowserVer < 6 then
  begin
    ErrorMessage(sIEError);
    Exit(False);
  end;

  Result := True;

  TFirstRunMgr.Execute;

  {$IFDEF PORTABLE}
  // Ensure and use user data directory for portable
  WorkingDir := TAppInfo.AppExeDir + '\UserData';
  // don't use TDirectory.CreateDirectory: don't want an exception here
  EnsureFolders(WorkingDir);
  if TDirectory.Exists(WorkingDir) then
    TDirectory.SetCurrentDirectory(WorkingDir);
  {$ENDIF}
end;

end.

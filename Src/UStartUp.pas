{
 * This Source Code Form is subject to the terms of the Mozilla Public License,
 * v. 2.0. If a copy of the MPL was not distributed with this file, You can
 * obtain one at http://mozilla.org/MPL/2.0/
 *
 * Copyright (C) 2012-2020, Peter Johnson (gravatar.com/delphidabbler).
 *
 * Performs start-up processing that must be performed before the rest of the
 * program is run.
}


unit UStartUp;


interface


uses
  // Delphi
  SysUtils;   // Ensure Exception class and handling are available


type
  ///  <summary>Encapsulation of code executed at program startup.</summary>
  TStartUp = record
  strict private
    ///  <summary>Displays an error message box informing the user that the
    ///  program cannot start, using the given message as an explanation.
    ///  </summary>
    class procedure ErrorMessage(const Msg: string); static;
  public
    ///  <summary>Checks if the program can be run and, if so, performs the
    ///  necessary start-up processing.</summary>
    ///  <returns>Boolean. True if the program can be run or False if the
    ///  program must not be allowed to continue.</returns>
    ///  <remarks>
    ///  <para>Displays a simple error message if the program cannot be run
    ///  because the operating system or IE version is not supported.</para>
    ///  <para>Performs special processing on first run of a new version of the
    ///  program.</para>
    ///  <para>For the portable edition only, ensures a user data directory has
    ///  been created.</para>
    ///  </remarks>
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
  sIEError = 'Internet Explorer v%d or later is required.';
{$IFDEF PORTABLE}
var
  WorkingDir: string;
{$ENDIF}
begin
  // Check if program can be run. Exit if not.
  if not TOSInfo.IsWinNT or not TOSInfo.CheckReportedOS(TOSInfo.Win2K) then
  begin
    ErrorMessage(sOSError);
    Exit(False);
  end;
  if not TIEInfo.IsSupportedBrowser then
  begin
    ErrorMessage(Format(sIEError, [TIEInfo.MinSupportedVersion]));
    Exit(False);
  end;

  Result := True;

  // Do any required "first run" processing
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


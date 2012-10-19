{
 * This Source Code Form is subject to the terms of the Mozilla Public License,
 * v. 2.0. If a copy of the MPL was not distributed with this file, You can
 * obtain one at http://mozilla.org/MPL/2.0/
 *
 * Copyright (C) 2012, Peter Johnson (www.delphidabbler.com).
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
  public
    class procedure Execute; static;
  end;

implementation

uses
  {$IFDEF PORTABLE}
  // Delphi
  IOUtils,
  // Project
  UAppInfo, UUtils,
  {$ENDIF}
  // Project
  FirstRun.UMain;

{ TStartUp }

class procedure TStartUp.Execute;
{$IFDEF PORTABLE}
var
  WorkingDir: string;
{$ENDIF}
begin
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

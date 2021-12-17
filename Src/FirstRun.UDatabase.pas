{
 * This Source Code Form is subject to the terms of the Mozilla Public License,
 * v. 2.0. If a copy of the MPL was not distributed with this file, You can
 * obtain one at https://mozilla.org/MPL/2.0/
 *
 * Copyright (C) 2008-2020, Peter Johnson (gravatar.com/delphidabbler).
 *
 * Implements a class that handles updating of user database from an earlier
 * version.
}


unit FirstRun.UDatabase;


interface


type
  ///  <summary>Class that handles updating of user database from an earlier
  ///  version.</summary>
  TUserDatabaseUpdater = class(TObject)
  strict private
    var
      ///  <summary>User database directory.</summary>
      fDatabaseDir: string;
    ///  <summary>Copies content of given database directory to current database
    ///  directory.</summary>
    procedure CopyDirectory(const SrcDir: string);
  public
    ///  <summary>Constructs object to update database in given directory.
    ///  </summary>
    constructor Create(const DatabaseDir: string);
    ///  <summary>Copies database from given directory into current database
    ///  directory.</summary>
    procedure CopyDatabase(const SrcDir: string);
  end;


implementation


uses
  // Delphi
  SysUtils, IOUtils,
  // Project
  UIOUtils, UStrUtils;


{ TUserDatabaseUpdater }

procedure TUserDatabaseUpdater.CopyDatabase(const SrcDir: string);
begin
  if (SrcDir = '')
    or not TDirectory.Exists(ExcludeTrailingPathDelimiter(SrcDir)) then
    Exit;
  CopyDirectory(SrcDir);
end;

procedure TUserDatabaseUpdater.CopyDirectory(const SrcDir: string);
var
  SourcePath: string;
  DestPath: string;
  FindRec: TSearchRec;
begin
  SourcePath := IncludeTrailingPathDelimiter(SrcDir);
  DestPath := IncludeTrailingPathDelimiter(fDatabaseDir);
  if StrSameText(SourcePath, DestPath) then
    Exit;
  if not TDirectory.Exists(ExcludeTrailingPathDelimiter(SourcePath)) then
    Exit;
  if not TDirectory.Exists(ExcludeTrailingPathDelimiter(DestPath)) then
    ForceDirectories(DestPath);
  if FindFirst(SourcePath + '*', faAnyFile, FindRec) = 0 then
  begin
    repeat
      if (FindRec.Attr and faDirectory) = 0 then
        TFileIO.CopyFile(SourcePath + FindRec.Name, DestPath + FindRec.Name);
    until FindNext(FindRec) <> 0;
    FindClose(FindRec);
  end;
end;

constructor TUserDatabaseUpdater.Create(const DatabaseDir: string);
begin
  inherited Create;
  fDatabaseDir := ExcludeTrailingPathDelimiter(DatabaseDir);
end;

end.


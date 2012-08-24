{
  * FirstRun.UDatabase.pas
  *
  * Pascal script for use in [Code] Section of CodeSnip's Install.iss.
  *
  * Copies existing CodeSnip main and user databases from an original location
  * to location required by current version of CodeSnip.
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
  * The Original Code is FirstRun.UDatabase.pas.
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


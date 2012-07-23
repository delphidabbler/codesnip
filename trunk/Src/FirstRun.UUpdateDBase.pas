{
  * FirstRun.UUpdateDBase.pas
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
  * The Original Code is FirstRun.UUpdateDBase.pas, formerly UpdateDBase.ps.
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

unit FirstRun.UUpdateDBase;

interface

// Copies user databases from it's old location identified by PrevInstallID to
// correct location for directory being installed.
procedure CopyDatabases(PrevInstallID: Integer);

implementation

uses
  SysUtils, Classes,
  FirstRun.UDataLocations, UUtils;

// TODO: NOTE inserted from snippets database
procedure CopyFile(const Source, Dest: string);
var
  SourceStream, DestStream: Classes.TFileStream; // source and dest file streams
begin
  DestStream := nil;
  // Open source and dest file streams
  SourceStream := Classes.TFileStream.Create(
    Source, SysUtils.fmOpenRead or SysUtils.fmShareDenyWrite
  );
  try
    DestStream := Classes.TFileStream.Create(
      Dest, Classes.fmCreate or SysUtils.fmShareExclusive
    );
    // Copy file from source to dest
    DestStream.CopyFrom(SourceStream, SourceStream.Size);
    // Set dest file's modification date to same as source file
    SysUtils.FileSetDate(
      DestStream.Handle, SysUtils.FileGetDate(SourceStream.Handle)
    );
  finally
    // Close files
    DestStream.Free;
    SourceStream.Free;
  end;
end;

// Copies all files from one directory for another. Does nothing if SourceDir
// doesn't exist or if both directories are the same. If DestDir does not exist
// it is created.
procedure CopyDirectory(SourceDir, DestDir: string);
var
  SourcePath: string;
  DestPath: string;
  FindRec: TSearchRec;
begin
  if CompareText(SourceDir, DestDir) = 0 then
    Exit;
  if not IsDirectory(SourceDir) then
    Exit;
  if not IsDirectory(DestDir) then
    ForceDirectories(DestDir);
  SourcePath := IncludeTrailingPathDelimiter(SourceDir);
  DestPath := IncludeTrailingPathDelimiter(DestDir);
  if FindFirst(SourcePath + '*', faAnyFile, FindRec) = 0 then
  begin
    repeat
      if (FindRec.Attr and faDirectory) = 0 then
      begin
        CopyFile(SourcePath + FindRec.Name, DestPath + FindRec.Name);
      end;
    until not FindNext(FindRec) <> 0;
    FindClose(FindRec);
  end;
end;

// Copies user databases from it's old location identified by PrevInstallID to
// correct location for directory being installed.
procedure CopyDatabases(PrevInstallID: Integer);
var
  OldUserDatabase: string;
begin
  OldUserDatabase := gUserDatabaseDirs[PrevInstallID];
  if OldUserDatabase <> '' then
    CopyDirectory(OldUserDatabase, gUserDatabaseDirs[piCurrent]);
end;

end.


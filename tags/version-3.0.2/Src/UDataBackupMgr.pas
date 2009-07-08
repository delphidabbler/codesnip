{
 * UDataBackupMgr.pas
 *
 * Static class that manages backups of data files. It can back up the local
 * data directory, restore the backup and delete it.
 *
 * v0.1 of 02 Apr 2006  - Original version.
 * v0.2 of 04 Apr 2006  - Changed to use TAppInfo class renamed from
 *                        TAppLocations.
 * v1.0 of 24 May 2006  - Improved and corrected comments.
 * v1.1 of 04 Oct 2008  - Changed TDataBackupMgr to derive from
 *                        TNoConstructObject and hence prevented it from being
 *                        constructed.
 *                      - Made private section strict.
 *
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
 * The Original Code is UDataBackupMgr.pas
 *
 * The Initial Developer of the Original Code is Peter Johnson
 * (http://www.delphidabbler.com/).
 *
 * Portions created by the Initial Developer are Copyright (C) 2006-2008 Peter
 * Johnson. All Rights Reserved.
 *
 * ***** END LICENSE BLOCK *****
}


unit UDataBackupMgr;


interface


uses
  // Project
  UBaseObjects;


type

  {
  TDataBackupMgr:
    Static class that manages backups of data files. It can back up the local
    data directory, restore the backup and delete it.
  }
  TDataBackupMgr = class(TNoConstructObject)
  strict private
    class function DataDir: string;
      {Gets name of directory where data files are stored.
        @return Required directory name.
      }
    class function BackupDir: string;
      {Gets name of directory where backup files are stored.
        @return Required directory name.
      }
    class function BackupExists: Boolean;
      {Checks if a current backup exists.
        @return True if backup exists, False if not.
      }
    class procedure CopyDirFiles(const SourceDir, DestDir: string);
      {Copies all files from one directory to another. Any sub directories are
      ignored. Assumes that both directories exist.
        @param SourceDir [in] Name of directory storing files to be copied.
        @param DestDir [in] Name of directory to receive copied files.
      }
    class procedure DeleteFilesFromDir(const Dir: string);
      {Deletes all files in a directory. Any sub directories are left untouched.
      Takes no action if directory doesn't exist.
        @param Dir [in] Name of directory files to be deleted from.
      }
  public
    class procedure Backup;
      {Backs up CodeSnip data files.
      }
    class procedure RestoreBackup;
      {Restores back up, replacing current data files. If no backup exists the
      data directory is cleared.
      }
    class procedure DeleteBackup;
      {Deletes current backup.
      }
  end;


implementation


uses
  // Delphi
  Classes,
  // Project
  UAppInfo, UUtils;


{ TDataBackupMgr }

class procedure TDataBackupMgr.Backup;
  {Backs up CodeSnip data files.
  }
begin
  EnsureFolders(BackupDir);
  DeleteFilesFromDir(BackupDir);
  EnsureFolders(DataDir);
  CopyDirFiles(DataDir, BackupDir);
end;

class function TDataBackupMgr.BackupDir: string;
  {Gets name of directory where backup files are stored.
    @return Required directory name.
  }
begin
  Result := DataDir + '.bak';
end;

class function TDataBackupMgr.BackupExists: Boolean;
  {Checks if a current backup exists.
    @return True if backup exists, False if not.
  }
begin
  // Backup is deemed to exist if backup directory exists
  // A backup may be empty
  Result := IsDirectory(BackupDir);
end;

class procedure TDataBackupMgr.CopyDirFiles(const SourceDir, DestDir: string);
  {Copies all files from one directory to another. Any sub directories are
  ignored. Assumes that both directories exist.
    @param SourceDir [in] Name of directory storing files to be copied.
    @param DestDir [in] Name of directory to receive copied files.
  }
var
  DirContents: TStringList; // list of directory contents
  Idx: Integer;             // loops thru contents of directory
begin
  DirContents := TStringList.Create;
  try
    ListFiles(SourceDir, '*.*', DirContents);
    for Idx := 0 to Pred(DirContents.Count) do
      if not IsDirectory(DirContents[Idx]) then
        CopyFile(
          SourceDir + '\' + DirContents[Idx],
          DestDir + '\' + DirContents[Idx]
        );
  finally
    DirContents.Free;
  end;
end;


class function TDataBackupMgr.DataDir: string;
  {Gets name of directory where data files are stored.
    @return Required directory name.
  }
begin
  Result := TAppInfo.AppDataDir;
end;

class procedure TDataBackupMgr.DeleteBackup;
  {Deletes current backup.
  }
begin
  if BackupExists then
  begin
    DeleteFilesFromDir(BackupDir);
    RmDir(BackupDir);
  end;
end;

class procedure TDataBackupMgr.DeleteFilesFromDir(const Dir: string);
  {Deletes all files in a directory. Any sub directories are left untouched.
  Takes no action if directory doesn't exist.
    @param Dir [in] Name of directory files to be deleted from.
  }
begin
  if IsDirectory(Dir) then
    DeleteFiles(Dir, '*.*');
end;

class procedure TDataBackupMgr.RestoreBackup;
  {Restores back up, replacing current data files. If no backup exists the
  data directory is cleared.
  }
begin
  EnsureFolders(DataDir);
  DeleteFilesFromDir(DataDir);
  if BackupExists then
    CopyDirFiles(BackupDir, DataDir);
end;

end.


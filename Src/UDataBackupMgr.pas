{
 * This Source Code Form is subject to the terms of the Mozilla Public License,
 * v. 2.0. If a copy of the MPL was not distributed with this file, You can
 * obtain one at https://mozilla.org/MPL/2.0/
 *
 * Copyright (C) 2006-2020, Peter Johnson (gravatar.com/delphidabbler).
 *
 * Static class that manages backups of data files. It can back up the local
 * database directory, restore the backup and delete it.
}


unit UDataBackupMgr;


interface


uses
  // Project
  UBaseObjects;


type

  {
  TDataBackupMgr:
    Static class that manages backups of main database. The database is backed
    up into a single file.
  }
  TDataBackupMgr = class(TNoConstructObject)
  strict private
    const cBakFileID = SmallInt($CBAC); // Main database backup file ID
    class function BackupFileName: string;
      {Name of file used to receive database backup.
        @return Required file name.
      }
    class function DataDir: string;
      {Name of directory that contains main database files.
        @return Required directory name.
      }
    class function BackupDir: string;
      {Gets name of directory where backup file is stored.
        @return Required directory name.
      }
    class function BackupExists: Boolean;
      {Checks if a current backup file exists.
        @return True if backup file exists, False if not.
      }
    class procedure DeleteFilesFromDir(const Dir: string);
      {Deletes all files in a directory. Any sub directories are left untouched.
      Takes no action if directory doesn't exist.
        @param Dir [in] Name of directory files to be deleted from.
      }
  public
    class procedure Backup;
      {Backs up CodeSnip data files into a single file.
      }
    class procedure RestoreBackup;
      {Restores back up, replacing current data files. If no backup exists the
      database directory is cleared.
      }
    class procedure DeleteBackup;
      {Deletes current backup file.
      }
  end;


implementation


uses
  // Delphi
  SysUtils, Windows {for inlining},
  // Project
  UAppInfo, UFolderBackup, UUtils;


{ TDataBackupMgr }

class procedure TDataBackupMgr.Backup;
  {Backs up CodeSnip data files into a single file.
  }
begin
  EnsureFolders(BackupDir);
  SysUtils.DeleteFile(BackupFileName);
  EnsureFolders(DataDir);
  with TFolderBackup.Create(DataDir, BackupFileName, cBakFileID) do
    try
      Backup;
    finally
      Free;
    end;
end;

class function TDataBackupMgr.BackupDir: string;
  {Gets name of directory where backup file is stored.
    @return Required directory name.
  }
begin
  Result := ExcludeTrailingPathDelimiter(TAppInfo.UserAppDir);
end;

class function TDataBackupMgr.BackupExists: Boolean;
  {Checks if a current backup file exists.
    @return True if backup file exists, False if not.
  }
begin
  Result := FileExists(BackupFileName);
end;

class function TDataBackupMgr.BackupFileName: string;
  {Name of file used to receive database backup.
    @return Required file name.
  }
begin
  Result := IncludeTrailingPathDelimiter(BackupDir) + 'main-db.bak';
end;

class function TDataBackupMgr.DataDir: string;
  {Name of directory that contains main database files.
    @return Required directory name.
  }
begin
  Result := TAppInfo.AppDataDir;
end;

class procedure TDataBackupMgr.DeleteBackup;
  {Deletes current backup file.
  }
begin
  if BackupExists then
    SysUtils.DeleteFile(BackupFileName);
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
  database directory is cleared.
  }
begin
  EnsureFolders(DataDir);
  DeleteFilesFromDir(DataDir);
  if BackupExists then
  begin
    with TFolderBackup.Create(DataDir, BackupFileName, cBakFileID) do
      try
        Restore;
      finally
        Free;
      end;
  end;
end;

end.


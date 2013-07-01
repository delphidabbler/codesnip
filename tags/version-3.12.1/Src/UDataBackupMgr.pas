{
 * UDataBackupMgr.pas
 *
 * Static class that manages backups of data files. It can back up the local
 * database directory, restore the backup and delete it.
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
 * The Original Code is UDataBackupMgr.pas
 *
 * The Initial Developer of the Original Code is Peter Johnson
 * (http://www.delphidabbler.com/).
 *
 * Portions created by the Initial Developer are Copyright (C) 2006-2009 Peter
 * Johnson. All Rights Reserved.
 *
 * Contributor(s)
 *   NONE
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


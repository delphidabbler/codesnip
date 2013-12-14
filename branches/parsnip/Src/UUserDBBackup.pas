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
 * Implements a class that can create and restore backups of the snippets
 * database.
}


unit UUserDBBackup;


// TODO: Rename this unit to remove "user" from name (CS.Database.Backup ?)


interface


uses
  // Project
  UFolderBackup;


type

  // TODO: rename this class to remove the term "user"
  {
  TUserDBBackup:
    Sealed class that can create and restore backups of the snippets database.
    Backups are single files. See UFolderBackup for details of file format.
  }
  TUserDBBackup = class sealed(TFolderBackup)
  strict private
    const cFileID = SmallInt($DBAC);  // Database backup file ID
  public
    constructor Create(const BackupFile: string);
      {Class constructor. Sets up object to backup snippets database to a
      specified file.
        @param BackupFile [in] Name of backup file.
      }
  end;


implementation


uses
  // Project
  UAppInfo;


{ TUserDBBackup }

constructor TUserDBBackup.Create(const BackupFile: string);
  {Class constructor. Sets up object to backup snippets database to a specified
  file.
    @param BackupFile [in] Name of backup file.
  }
begin
  inherited Create(TAppInfo.UserDataDir, BackupFile, cFileID);
end;

end.


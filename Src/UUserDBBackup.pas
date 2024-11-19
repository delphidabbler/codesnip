{
 * This Source Code Form is subject to the terms of the Mozilla Public License,
 * v. 2.0. If a copy of the MPL was not distributed with this file, You can
 * obtain one at https://mozilla.org/MPL/2.0/
 *
 * Copyright (C) 2008-2021, Peter Johnson (gravatar.com/delphidabbler).
 *
 * Implements a class that can create and restore backups of the user database.
}


unit UUserDBBackup;


{TODO -cRefactoring: Rename this unit/classes/methods: the names refer to the
        CodeSnip 4 database structure but the code now works with collections}

interface


uses
  // Project
  DB.UCollections,
  UFolderBackup;


type

  {
  TUserDBBackup:
    Sealed class that can create and restore backups of the user database.
    Backups are single files. See UFolderBackup for details of file format.
  }
  TUserDBBackup = class sealed(TFolderBackup)
  strict private
    class function MakeFileID(const ACollection: TCollection): SmallInt;
  public
    constructor Create(const BackupFile: string;
      const ACollection: TCollection);
      {Class constructor. Sets up object to backup user database to a specified
      file.
        @param BackupFile [in] Name of backup file.
      }
  end;


implementation


uses
  // Delphi
  SysUtils,
  // Project
  UAppInfo;


{ TUserDBBackup }

constructor TUserDBBackup.Create(const BackupFile: string;
  const ACollection: TCollection);
  {Class constructor. Sets up object to backup user database to a specified
  file.
    @param BackupFile [in] Name of backup file.
  }
begin
  inherited Create(
    ACollection.Location.Directory,
    BackupFile,
    MakeFileID(ACollection),
    ACollection.UID.ToArray
  );
end;

class function TUserDBBackup.MakeFileID(const ACollection: TCollection):
  SmallInt;
begin
  // Backup file ID is $Fxxx where xxx is ordinal value of format kind.
  // The $F indicates that the file is a backup of a collection data format.
  Result := SmallInt($F000 or UInt16(Ord(ACollection.CollectionFormatKind)));
end;

end.


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

  {TODO -cVault: Rename TUserDBBackup class and UUserDBBackup unit.}

  ///  <summary>Sealed class that can create and restore backups of vaults.
  ///  Backups are single files.</summary>
  ///  <remarks>See <c>UFolderBackup</c> for details of the file format.
  ///  </remarks>
  TUserDBBackup = class sealed(TFolderBackup)
  strict private
    class function MakeFileID(const ACollection: TVault): SmallInt;
  public
    ///  <summary>Object constructor. Sets up the object to backup the given
    ///  vault to the given backup file.</summary>
    constructor Create(const BackupFile: string; const ACollection: TVault);
  end;


implementation


uses
  // Delphi
  SysUtils,
  // Project
  UAppInfo;


{ TUserDBBackup }

constructor TUserDBBackup.Create(const BackupFile: string;
  const ACollection: TVault);
begin
  inherited Create(
    ACollection.Storage.Directory,
    BackupFile,
    MakeFileID(ACollection),
    ACollection.UID.ToArray
  );
end;

class function TUserDBBackup.MakeFileID(const ACollection: TVault):
  SmallInt;
begin
  // Backup file ID is $Fxxx where xxx is ordinal value of format kind.
  // The $F indicates that the file is a backup of a collection data format.
  Result := SmallInt($F000 or UInt16(Ord(ACollection.Storage.Format)));
end;

end.


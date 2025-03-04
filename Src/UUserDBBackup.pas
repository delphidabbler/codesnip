{
 * This Source Code Form is subject to the terms of the Mozilla Public License,
 * v. 2.0. If a copy of the MPL was not distributed with this file, You can
 * obtain one at https://mozilla.org/MPL/2.0/
 *
 * Copyright (C) 2008-2021, Peter Johnson (gravatar.com/delphidabbler).
 *
 * Implements a class that can create and restore backups of vaults.
}


unit UUserDBBackup;


interface


uses
  // Project
  DB.Vaults,
  UFolderBackup;


type

  {TODO -cVault: Rename TUserDBBackup class and UUserDBBackup unit.}

  ///  <summary>Sealed class that can create and restore backups of vaults.
  ///  Backups are single files.</summary>
  ///  <remarks>See <c>UFolderBackup</c> for details of the file format.
  ///  </remarks>
  TVaultBackup = class sealed(TFolderBackup)
  strict private
    class function MakeFileID(const AVault: TVault): SmallInt;
  public
    ///  <summary>Object constructor. Sets up the object to backup the given
    ///  vault to the given backup file.</summary>
    constructor Create(const BackupFile: string; const AVault: TVault);
  end;


implementation


uses
  // Delphi
  SysUtils,
  // Project
  UAppInfo;


{ TVaultBackup }

constructor TVaultBackup.Create(const BackupFile: string;
  const AVault: TVault);
begin
  inherited Create(
    AVault.Storage.Directory, BackupFile, MakeFileID(AVault), AVault.UID.ToArray
  );
end;

class function TVaultBackup.MakeFileID(const AVault: TVault):
  SmallInt;
begin
  // Backup file ID is $Fxxx where xxx is ordinal value of format kind.
  // The $F indicates that the file is a backup of a vault data format.
  Result := SmallInt($F000 or UInt16(Ord(AVault.Storage.Format)));
end;

end.


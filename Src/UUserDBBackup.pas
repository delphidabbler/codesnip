{
 * UUserDBBackup.pas
 *
 * Implements a class that can create and restore backups of the user database.
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
 * The Original Code is UUserDBBackup.pas
 *
 * The Initial Developer of the Original Code is Peter Johnson
 * (http://www.delphidabbler.com/).
 *
 * Portions created by the Initial Developer are Copyright (C) 2008-2010 Peter
 * Johnson. All Rights Reserved.
 *
 * Contributor(s)
 *   NONE
 *
 * ***** END LICENSE BLOCK *****
}


unit UUserDBBackup;


interface


uses
  // Project
  UFolderBackup;


type

  {
  TUserDBBackup:
    Sealed class that can create and restore backups of the user database.
    Backups are single files. See UFolderBackup for details of file format.
  }
  TUserDBBackup = class sealed(TFolderBackup)
  strict private
    const cFileID = SmallInt($DBAC);  // User database backup file ID
  public
    constructor Create(const BackupFile: string);
      {Class constructor. Sets up object to backup user database to a specified
      file.
        @param BackupFile [in] Name of backup file.
      }
  end;


implementation


uses
  // Project
  UAppInfo;


{ TUserDBBackup }

constructor TUserDBBackup.Create(const BackupFile: string);
  {Class constructor. Sets up object to backup user database to a specified
  file.
    @param BackupFile [in] Name of backup file.
  }
begin
  inherited Create(TAppInfo.UserDataDir, BackupFile, cFileID);
end;

end.


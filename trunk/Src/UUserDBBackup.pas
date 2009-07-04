{
 * UUserDBBackup.pas
 *
 * Implements a class that can create and restore backups of the user database.
 *
 * v1.0 of 13 Sep 2008  - Original version.
 * v1.1 of 22 May 2009  - Fixed bug where restoring files greater than 32Kb
 *                        caused a stream read error. Changed file format to
 *                        ver 2 and kept ability to read v1 files.
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
 * The Original Code is UUserDBBackup.pas
 *
 * The Initial Developer of the Original Code is Peter Johnson
 * (http://www.delphidabbler.com/).
 *
 * Portions created by the Initial Developer are Copyright (C) 2008-2009 Peter
 * Johnson. All Rights Reserved.
 *
 * ***** END LICENSE BLOCK *****
}


unit UUserDBBackup;


interface


type

  {
  TUserDBBackup:
    Class that can create and restore backups of the user database. Backups are
    single files.
  }
  TUserDBBackup = class(TObject)
  strict private
    var
      fBackupFile: string;          // Name of backup file
      fUserDBDir: string;           // User database directory}
    const
      cWatermark = SmallInt($FFFF); // File watermark (v2 and later)
    function UserDBFileSpec(const FileName: string): string;
      {Builds full path to a file in user database directory.
        @param FileName [in] Base name of file.
        @return Fully specified file name.
      }
  public
    constructor Create(const BackupFile: string);
      {Class constructor. Sets up object to backup to a specified file.
        @param BackupFile [in] Name of backup file.
      }
    procedure Backup;
      {Creates a backup of user database in a single file.
      }
    procedure Restore;
      {Restores user database from a backup file.
      }
  end;


implementation


uses
  // Delphi
  SysUtils, Classes,
  // Project
  UAppInfo, UCheckSum, UDataStreamReader, UDataStreamWriter, UDOSDateTime,
  UExceptions, UUtils;

{
  User database backup file format
  --------------------------------

  File compriises text characters. Numbers are encoded in hex format. There are
  two formats:

  Version 1 Format
  ----------------

    FileCount: SmallInt       - number of files encoded in backup file

  followed by FileCount file records of:

    Name: SizedString;        - name of file without path information
    FileDate: LongInt;        - file's modification date (DOS file stamp as
                                LongInt).
    MD5: String[32];          - MD5 checksum of original file on server
                                (MD5 of Content should match this value)
    Content: SizedString;     - file contents

  Version 2 Format
  ----------------

    $FFFF                     - Indicator of post-v1 file type
    $0002                     - Indicator for v2 file type
                                (and later versions will increment this
    FileCount: SmallInt       - number of files encoded in backup file

  followed by FileCount file records of:

    Name: SizedString;        - name of file without path information
    FileDate: LongInt;        - file's modification date (DOS file stamp as
                                LongInt).
    MD5: String[32];          - MD5 checksum of original file on server
                                (MD5 of Content should match this value)
    Content: SizedLongString; - file contents

  Data types
  ----------

    SmallInt        - 16 bit integer encoded as 4 hex digits
    LongInt         - 32 bit integer encoded as 8 hex digits
    SizedString     - SmallInt specifying string length followed by specified
                      number of characters
    SizedLongString - LongInt specifying string length followed by specified
                      number of characters
    String[32]      - 32 character fixed length string
}


{ TUserDBBackup }

procedure TUserDBBackup.Backup;
  {Creates a backup of user database in a single file.
  }
var
  Writer: TDataStreamWriter;  // object used to write data to stream
  Files: TStringList;         // list of files in database directory
  FileName: string;           // references each file in database
  Content: string;            // content of each database file
  DOSDateTime: IDOSDateTime;  // date stamp of each database file
begin
  Files := nil;
  // Create output stream to backup file
  Writer := TDataStreamWriter.Create(
    TFileStream.Create(fBackupFile, fmCreate), True
  );
  try
    // Get list of files in database and write number of files to output
    Files := TStringList.Create;
    ListFiles(fUserDBDir, '*.*', Files, False);
    Writer.WriteSmallInt(cWatermark);   // file marker
    Writer.WriteSmallInt($0002);        // file version
    Writer.WriteSmallInt(Files.Count);  // number of files
    // Write details of each file to output: file name, date stamp, checksum and
    // content
    for FileName in Files do
    begin
      Writer.WriteSizedString(FileName);
      DOSDateTime := TDOSDateTimeFactory.CreateFromFile(
        UserDBFileSpec(FileName)
      );
      Writer.WriteLongInt(DOSDateTime.DateStamp);
      Content := FileToString(UserDBFileSpec(FileName));
      Writer.WriteString(TCheckSum.Calculate(Content), 32);
      Writer.WriteSizedLongString(Content);
    end;
  finally
    FreeAndNil(Files);
    FreeAndNil(Writer);
  end;
end;

constructor TUserDBBackup.Create(const BackupFile: string);
  {Class constructor. Sets up object to backup to a specified file.
    @param BackupFile [in] Name of backup file.
  }
begin
  inherited Create;
  fBackupFile := BackupFile;
  fUserDBDir := PathToDir(TAppInfo.UserDataDir);
end;

procedure TUserDBBackup.Restore;
  {Restores user database from a backup file.
  }
var
  Reader: TDataStreamReader;  // object used to read data from file
  FileCount: Integer;         // number of files to restore
  Idx: Integer;               // loops through all files in backup
  FileName: string;           // name of file to restore
  MD5: string;                // checksum of file to restore
  Content: string;            // content of file to restore
  DOSDateTime: IDOSDateTime;  // date stamp of file to restore
  HeaderWord: SmallInt;       // first word value in file
  Version: SmallInt;          // file version
resourcestring
  // Error message
  sBadFileContent = 'Invalid content for file "%s"';
begin
  // Make sure database folder exists
  EnsureFolders(fUserDBDir);
  // Create reader to access data in backup file
  Reader := TDataStreamReader.Create(
    TFileStream.Create(fBackupFile, fmOpenRead or fmShareDenyNone)
  );
  try
    // Get number of files stored in backup file and process each one
    // Read 1st word of file. If it is $FFFF we have v2 or later file, otherwise
    // it's v1
    HeaderWord := Reader.ReadSmallInt;
    if HeaderWord = cWatermark then
    begin
      // v2 or later: read version then file count
      Version := Reader.ReadSmallInt;
      FileCount := Reader.ReadSmallInt;
    end
    else
    begin
      // v1: file count was first word of file
      Version := 1;
      FileCount := HeaderWord;
    end;
    for Idx := 1 to FileCount do
    begin
      // Get file details: name, date stamp, checksum and content
      FileName := UserDBFileSpec(Reader.ReadSizedString);
      DOSDateTime := TDOSDateTimeFactory.CreateFromDOSTimeStamp(
        Reader.ReadLongInt
      );
      MD5 := Reader.ReadString(32);
      if Version = 1 then
        Content := Reader.ReadSizedString
      else
        Content := Reader.ReadSizedLongString;
      if not TCheckSum.Compare(Content, MD5) then
        raise ECodeSnip.CreateFmt(sBadFileContent, [FileName]);
      // Write file and set date stamp
      StringToFile(Content, FileName);
      DOSDateTime.ApplyToFile(FileName);
    end;
  finally
    FreeAndNil(Reader);
  end;
end;

function TUserDBBackup.UserDBFileSpec(const FileName: string): string;
  {Builds full path to a file in user database directory.
    @param FileName [in] Base name of file.
    @return Fully specified file name.
  }
begin
  Result := DirToPath(fUserDBDir) + FileName;
end;

end.


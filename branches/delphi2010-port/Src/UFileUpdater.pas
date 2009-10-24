{
 * UFileUpdater.pas
 *
 * Object that updates local CodeSnip data files using a supplied stream of
 * data.
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
 * The Original Code is UFileUpdater.pas
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


unit UFileUpdater;


interface


uses
  // Delphi
  Classes,
  // Project
  UDataStreamIO, UExceptions;


type

  {
  TFileUpdater:
    Object that updates local CodeSnip data files using a supplied stream of
    data.
  }
  TFileUpdater = class(TObject)
  strict private
    fReader: TDataStreamReader; // Provides methods to read data stream
    fLocalDir: string;          // Local data directory
    procedure UndoUpdate;
      {Reverts data files to state they were in before update.
      }
    procedure UpdateFile;
      {Creates a file from data stream.
        @except Raises EFileList if file checksum is incorrect.
      }
    procedure ValidateFile(const Name, Content, MD5: TDataStreamString);
      {Validates checksum of a file.
        @param Name [in] Name of file
        @param File [in] Content from data stream.
        @param MD5 [in] Checksum of file on webserver.
        @except Raises EFileList if check sum of file content doesn't
          match expected checksum.
      }
    procedure WriteFile(const Name, Content: TDataStreamString;
      const UnixDate: Int64);
      {Writes local database file.
        @param Name [in] Name of file.
        @param Content [in] File content.
        @param UnixDate [in] Date date stamp to be applied to file. This is in
          Unix time stamp format and is GMT.
      }
  public
    constructor Create(const LocalDir: string; const UpdateData: TStream);
      {Class constructor. Sets up object to update local files.
        @param LocalDir [in] Directory storing local data files.
        @param UpdateData [in] Stream containing details of files to be updated.
      }
    destructor Destroy; override;
      {Class destructor. Tears down object.
      }
    procedure Execute;
      {Performs file updates.
        @except Untrapped exception raised if an error is detected in update
          process.
      }
  end;

  {
  EFileUpdater:
    Class of exception raised by TFileUpdater.
  }
  EFileUpdater = class(ECodeSnip);


implementation


uses
  // Delphi
  SysUtils,
  // Project
  UCheckSum, UDataBackupMgr, UDOSDateTime, UUtils;


resourcestring
  // Exception message template
  sCheckSumError = 'The file "%0:s" downloaded from the web service was '
    + 'corrupt';


{
  Data stream format
  ------------------

  Stream comproses text characters. Numbers are encoded as hex.

  Format is:

    FileCount: SmallInt       - number of files encoded in datastream

  followed by FileCount file records of:

    Name: SizedString;        - name of file
    UnixDate: Int64;          - file's modification date (GMT) in Unix format
                                (must be converted to local time and DOS date
                                stamp format)
    MD5: String[32];          - MD5 checksum of original file on server
                                (MD5 of Content should match this value)
    Content: SizedString;     - file contents

  Data types are:

    SmallInt      - 16 bit integer encoded as 4 hex digits
    Int64         - 64 bit integer encoded as 16 hex digits
    SizedString   - SmallInt specifying string length followed by specified
                    number of characters
    String[32]    - 32 character fixed length string

}


{ TFileUpdater }

constructor TFileUpdater.Create(const LocalDir: string;
  const UpdateData: TStream);
  {Class constructor. Sets up object to update local files.
    @param LocalDir [in] Directory storing local data files.
    @param UpdateData [in] Stream containing details of files to be updated.
  }
begin
  inherited Create;
  fLocalDir := LocalDir;
  fReader := TDataStreamReader.Create(UpdateData);
end;

destructor TFileUpdater.Destroy;
  {Class destructor. Tears down object.
  }
begin
  FreeAndNil(fReader);
  inherited;
end;

procedure TFileUpdater.Execute;
  {Performs file updates.
    @except Untrapped exception raised if an error is detected in update
      process.
  }
var
  FileCount: Integer;   // number of files to copy to local directory
begin
  TDataBackupMgr.Backup;
  try
    // Clear local data directory
    UUtils.DeleteFiles(fLocalDir, '*.*');
    // Copy in new files
    FileCount := fReader.ReadSmallInt;
    while FileCount > 0 do
    begin
      UpdateFile;
      Dec(FileCount);
    end;
  except
    // Error: restore backup
    UndoUpdate;
    raise;
  end;
  // OK: delete backup
  TDataBackupMgr.DeleteBackup;
end;

procedure TFileUpdater.UndoUpdate;
  {Reverts data files to state they were in before update.
  }
begin
  TDataBackupMgr.RestoreBackup;
  TDataBackupMgr.DeleteBackup;
end;

procedure TFileUpdater.UpdateFile;
  {Creates a file from data stream.
    @except Raises EFileList if file checksum is incorrect.
  }
var
  Name: TDataStreamString;       // name of file
  UnixDate: Int64;    // update date of file (per server - Unix format & GMT)
  MD5: TDataStreamString;        // MD5 checksum of file on server
  Content: TDataStreamString;    // file content
begin
  // Get info about file from data stream
  Name := fReader.ReadSizedString;
  UnixDate := fReader.ReadInt64;
  MD5 := fReader.ReadString(32);
  Content := fReader.ReadSizedString;
  // Validate and create file
  ValidateFile(Name, Content, MD5);
  WriteFile(Name, Content, UnixDate);
end;

procedure TFileUpdater.ValidateFile(const Name, Content,
  MD5: TDataStreamString);
  {Validates checksum of a file.
    @param Name [in] Name of file
    @param File [in] Content from data stream.
    @param MD5 [in] Checksum of file on webserver.
    @except Raises EFileList if check sum of file content doesn't
      match expected checksum.
  }
begin
  if not TCheckSum.Compare(Content, MD5) then
    raise EFileUpdater.CreateFmt(sChecksumError, [Name]);
end;

procedure TFileUpdater.WriteFile(const Name, Content: TDataStreamString;
  const UnixDate: Int64);
  {Writes local database file.
    @param Name [in] Name of file.
    @param Content [in] File content.
    @param UnixDate [in] Date date stamp to be applied to file. This is in Unix
      time stamp format and is GMT.
  }
var
  FilePath: string;   // full path to local file
  Date: IDOSDateTime; // object that encapsulates DOS date time value
begin
  FilePath := IncludeTrailingPathDelimiter(fLocalDir) + string(Name);
  UUtils.StringToFile(string(Content), FilePath);
  Date := TDOSDateTimeFactory.CreateFromUnixTimeStamp(UnixDate);
  Date.ApplyToFile(FilePath);
end;

end.


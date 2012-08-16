{
 * UFolderBackup.pas
 *
 * Class that can back up all the files in a folder into a single file. All
 * files to be backed up must be text files.
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
 * The Original Code is UFolderBackup.pas
 *
 * The Initial Developer of the Original Code is Peter Johnson
 * (http://www.delphidabbler.com/).
 *
 * Portions created by the Initial Developer are Copyright (C) 2009-2010 Peter
 * Johnson. All Rights Reserved.
 *
 * Contributor(s)
 *   NONE
 *
 * ***** END LICENSE BLOCK *****
}


unit UFolderBackup;


interface


type

  {
  TFolderBackup:
    Class that can back up and restore all the files in a folder to or from a
    single file. All files to be backed up must be text files.
  }
  TFolderBackup = class(TObject)
  strict private
    var
      fSrcFolder: string;   // Folder to be backed up or restored
      fBakFile: string;     // Full path to backup file
      fFileID: SmallInt;    // Identifies the type of file
    const
      cWatermark = SmallInt($FFFF); // File watermark (v2 and later)
    function SourceFileSpec(const FileName: string): string;
      {Full path to a file in backed up folder.
        @param FileName [in] Base name of file.
        @return Full file spec of FileName in source folder.
      }
  public
    constructor Create(const SrcFolder, BakFile: string;
      const FileID: SmallInt);
      {Class constructor. Creates backup object for specified folder and backup
      file.
        @param SrcFolder [in] Folder to be backed up / restored. Must contain
          only text files. Sub folders are ignored.
        @param BakFile [in] Name of backup file.
        @param FileID [in] File type identifier.
      }
    procedure Backup;
      {Backs up source folder into single backup file. Backup file is marked
      with the file ID passed to constructor.
      }
    procedure Restore;
      {Restores folder from backup file.
        @except ECodeSnip raised if any checksum errors are found or, for v3 and
          later format files, if file type id is incorrect.
      }
  end;


implementation


uses
  // Delphi
  SysUtils, Classes,
  // DelphiDabbler library
  PJMD5,
  // Project
  UDataStreamIO, UDOSDateTime, UEncodings, UExceptions, UUtils;


{
  Backup file format
  ------------------

  File comprises ASCII text characters. Numbers are encoded in hex format. Only
  text files can be backed up. There are three supported file formats. All three
  formats can be read while only v3 files are written.

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

    $FFFF                     - indicator of post-v1 file type
    $0002                     - indicator for v2 file type
    FileCount: SmallInt       - number of files encoded in backup file

  followed by FileCount file records of:

    Name: SizedString;        - name of file without path information
    FileDate: LongInt;        - file's modification date (DOS file stamp as
                                LongInt).
    MD5: String[32];          - MD5 checksum of original file on server
                                (MD5 of Content should match this value)
    Content: SizedLongString; - file contents

  Version 3 Format
  ----------------

    $FFFF                     - indicator of post-v1 file type
    $0003                     - indicator for v3 file type
    FileID: SmallInt          - indicator for file type (user defined)
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


{ TFolderBackup }

procedure TFolderBackup.Backup;
  {Backs up source folder into single backup file. Backup file is marked with
  the file ID passed to constructor.
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
    TFileStream.Create(fBakFile, fmCreate), True
  );
  try
    // Get list of files in database
    Files := TStringList.Create;
    ListFiles(fSrcFolder, '*.*', Files, False);
    // Write file header, including number of files
    Writer.WriteSmallInt(cWatermark);   // file marker
    Writer.WriteSmallInt($0003);        // file version
    Writer.WriteSmallInt(fFileID);      // file ID
    Writer.WriteSmallInt(Files.Count);  // number of files
    // Write details of each file
    for FileName in Files do
    begin
      Writer.WriteSizedString(FileName);
      DOSDateTime := TDOSDateTimeFactory.CreateFromFile(
        SourceFileSpec(FileName)
      );
      Writer.WriteLongInt(DOSDateTime.DateStamp);
      Content := FileToString(SourceFileSpec(FileName));
      Writer.WriteString(TPJMD5.Calculate(Windows1252BytesOf(Content)), 32);
      Writer.WriteSizedLongString(Content);
    end;
  finally
    FreeAndNil(Files);
    FreeAndNil(Writer);
  end;
end;

constructor TFolderBackup.Create(const SrcFolder, BakFile: string;
  const FileID: SmallInt);
  {Class constructor. Creates backup object for specified folder and backup
  file.
    @param SrcFolder [in] Folder to be backed up / restored. Must contain only
      text files. Sub folders are ignored.
    @param BakFile [in] Name of backup file.
    @param FileID [in] File type identifier.
  }
begin
  inherited Create;
  fSrcFolder := ExcludeTrailingPathDelimiter(SrcFolder);
  fBakFile := BakFile;
  fFileID := FileID;
end;

procedure TFolderBackup.Restore;
  {Restores folder from backup file.
    @except ECodeSnip raised if any checksum errors are found or, for v3 and
      later format files, if file type id is incorrect.
  }
var
  Reader: TDataStreamReader;  // object used to read data from file
  FileCount: Integer;         // number of files to restore
  Idx: Integer;               // loops through all files in backup
  FileName: string;           // name of file to restore
  MD5: TPJMD5Digest;          // checksum of file to restore
  Content: Windows1252String; // content of file to restore
  DOSDateTime: IDOSDateTime;  // date stamp of file to restore
  HeaderWord: SmallInt;       // first word value in file
  Version: SmallInt;          // file version
resourcestring
  // Error message
  sBadFileContent = 'Invalid content for file "%s"';
  sBadFileID = 'Invalid identifier for backup file "%s"';
  sUnsupportedVersion = 'Backup file "%s" has an unsupported file format';
begin
  // Make sure restore folder exists
  EnsureFolders(fSrcFolder);
  // Create reader to access data in backup file
  Reader := TDataStreamReader.Create(
    TFileStream.Create(fBakFile, fmOpenRead or fmShareDenyNone), True
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
      if Version > 3 then
        // version 4 or later: we only support up to v3 file format
        raise ECodeSnip.CreateFmt(sUnsupportedVersion, [fBakFile]);
      if Version = 3 then
      begin
        // version 3 : read file id
        if Reader.ReadSmallInt <> fFileID then
          raise ECodeSnip.CreateFmt(sBadFileID, [fBakFile]);
      end;
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
      FileName := SourceFileSpec(string(Reader.ReadSizedString));
      DOSDateTime := TDOSDateTimeFactory.CreateFromDOSTimeStamp(
        Reader.ReadLongInt
      );
      MD5 := string(Reader.ReadString(32)); // string cast to TPJMD5Digest
      if Version = 1 then
        Content := Reader.ReadSizedString
      else
        Content := Reader.ReadSizedLongString;
      if TPJMD5.Calculate(Content) <> MD5 then
        raise ECodeSnip.CreateFmt(sBadFileContent, [FileName]);
      // Write file and set date stamp
      StringToFile(string(Content), FileName);
      DOSDateTime.ApplyToFile(FileName);
    end;
  finally
    FreeAndNil(Reader);
  end;
end;

function TFolderBackup.SourceFileSpec(const FileName: string): string;
  {Full path to a file in backed up folder.
    @param FileName [in] Base name of file.
    @return Full file spec of FileName in source folder.
  }
begin
  Result := IncludeTrailingPathDelimiter(fSrcFolder) + FileName;
end;

end.


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

// TODO: Tidy up and re comment this unit.

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
  SysUtils, Classes, Generics.Collections, IOUtils,
  // DelphiDabbler library
  PJMD5,
  // Project
  UBaseObjects, UDataStreamIO, UDOSDateTime, UEncodings, UExceptions, UUtils;


type

  TBackupFileWriter = class(TObject)
  strict private
    var fFiles: TStrings;
    var fStream: TStream;
    var fFileID: SmallInt;
    procedure WriteHeader;
    procedure WriteFileInfo(const FileName: string);
  public
    constructor Create(const Files: TStrings; const Stream: TStream;
      const FileID: SmallInt);
    destructor Destroy; override;
    procedure Save;
  end;

  ///  <summary>
  ///  Abstract base class for classes that load backup files. There is a
  ///  different sub class for each supported version of the backup file format.
  ///  </summary>
  TBackupFileLoader = class abstract(TObject)
  public
    type
      ///  Provides information about a file to be restored from backup
      TFileInfo = record
        ///  File name without path
        Name: string;
        ///  File's timestamp
        TimeStamp: LongInt;
        ///  Content of file as bytes to be written to file
        Content: TBytes;
      end;
    ///  Value used for FileID in file formats that do not support it
    const NulFileID = 0;
  strict private
    var fFileID: SmallInt;                  // Value of FileID property
    var fFiles: TList<TFileInfo>;           // List of file info records
    var fReader: TDataStreamReader;         // Value of Reader property
    var fBinReader: TBinDataStreamReader;
    var fEncoding: TEncoding;
  strict protected
    ///  Checks that two checksums that relate to a specified file and the same.
    ///  Raises an exception if checksums are different.
    procedure TestChecksums(const FileName: string;
      const Required, Actual: TPJMD5Digest);
    ///  Gets the encoding used for the backup file.
    function GetFileEncoding: TEncoding; virtual; abstract;
    ///  Reads header information from the backup file.
    procedure ReadHeader(out FileID, FileCount: SmallInt); virtual; abstract;
    ///  Reads information about a file to be restored from the backup file.
    procedure ReadFileInfo(out FileInfo: TFileInfo); virtual; abstract;
    ///  Reference to object used to read data from the backup file stream.
    property Reader: TDataStreamReader read fReader;
    property BinReader: TBinDataStreamReader read fBinReader;
  public
    ///  Object constructor. Sets up object to get data from a given stream.
    constructor Create(const Stream: TStream);
    ///  Object destructor. Tears down object.
    destructor Destroy; override;
    ///  Loads the backup file from the stream storing required information in
    ///  the properties and file list.
    procedure Load;
    ///  Backup file identifier.
    property FileID: SmallInt read fFileID;
    ///  Gets enumerator for file list.
    function GetEnumerator: TEnumerator<TFileInfo>;
  end;

  TBackupFileLoaderClass = class of TBackupFileLoader;

  ///  <summary>
  ///  Factory class that creates the required TBackupFileLoader object to load
  ///  different backup file formats.
  ///  </summary>
  TBackupFileLoaderFactory = class sealed(TNoConstructObject)
  strict private
    ///  Latest supported file format version.
    const LastSupportedVersion = 4;
    ///  Gets file format version from backup file stream.
    class function GetFileVersion(const Stream: TStream): SmallInt;
  public
    ///  Creates an instance of the correct backup file loader required to load
    ///  the backup file from the given stream.
    class function Create(const Stream: TStream): TBackupFileLoader;
  end;

  ///  <summary>
  ///  Loads and provides access to data from a backup file that has the version
  ///  1 file format.
  ///  </summary>
  TV1BackupFileLoader = class(TBackupFileLoader)
  strict protected
    ///  Returns the backup file's text encoding
    function GetFileEncoding: TEncoding; override;
    ///  Reads header information from the backup file.
    procedure ReadHeader(out FileID, FileCount: SmallInt); override;
    ///  Reads information about a file to be restored from the backup file.
    ///  Validates the file's content against a recorded checksum.
    procedure ReadFileInfo(out FileInfo: TBackupFileLoader.TFileInfo); override;
  end;

  ///  <summary>
  ///  Loads and provides access to data from a backup file that has the version
  ///  2 file format.
  ///  </summary>
  TV2BackupFileLoader = class(TBackupFileLoader)
  strict protected
    ///  Returns the backup file's text encoding
    function GetFileEncoding: TEncoding; override;
    ///  Reads header information from the backup file.
    procedure ReadHeader(out FileID, FileCount: SmallInt); override;
    ///  Reads information about a file to be restored from the backup file.
    ///  Validates the file's content against a recorded checksum.
    procedure ReadFileInfo(out FileInfo: TBackupFileLoader.TFileInfo); override;
  end;

  ///  <summary>
  ///  Loads and provides access to data from a backup file that has the version
  ///  3 file format.
  ///  </summary>
  TV3BackupFileLoader = class(TBackupFileLoader)
  strict protected
    ///  Returns the backup file's text encoding
    function GetFileEncoding: TEncoding; override;
    ///  Reads header information from the backup file.
    procedure ReadHeader(out FileID, FileCount: SmallInt); override;
    ///  Reads information about a file to be restored from the backup file.
    ///  Validates the file's content against a recorded checksum.
    procedure ReadFileInfo(out FileInfo: TBackupFileLoader.TFileInfo); override;
  end;

  TV4BackupFileLoader = class(TBackupFileLoader)
  strict protected
    ///  Returns the backup file's text encoding
    function GetFileEncoding: TEncoding; override;
    ///  Reads header information from the backup file.
    procedure ReadHeader(out FileID, FileCount: SmallInt); override;
    ///  Reads information about a file to be restored from the backup file.
    ///  Validates the file's content against a recorded checksum.
    procedure ReadFileInfo(out FileInfo: TBackupFileLoader.TFileInfo); override;
  end;

  ///  Type of exception raised by backup file loader objects.
  EBackupFileLoader = class(ECodeSnip);

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

  FS: TFileStream;
  FileWriter: TBackupFileWriter;
begin
  // Get list of files in database
  Files := TStringList.Create;
  try
    ListFiles(fSrcFolder, '*.*', Files, False);
    FS := TFileStream.Create(fBakFile, fmCreate);
    try
      FileWriter := TBackupFileWriter.Create(Files, FS, fFileID);
      try
        FileWriter.Save;
      finally
        FileWriter.Free;
      end;
    finally
      FS.Free;
    end;
  finally
    Files.Free;
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
  BakFileStream: TStream;                 // stream onto backup file
  BakFileLoader: TBackupFileLoader;       // loads & analyses backup file
  FileSpec: string;                       // name & path of each file to restore
  DOSDateTime: IDOSDateTime;              // date stamp of each file to restore
  FileInfo: TBackupFileLoader.TFileInfo;  // info about each file to restore
resourcestring
  // Error message
  sBadFileID = 'Invalid file ID for file "%s"';
begin
  // Make sure restore folder exists
  EnsureFolders(fSrcFolder);
  // Load backup file contents
  BakFileLoader := nil;
  BakFileStream := TFileStream.Create(fBakFile, fmOpenRead or fmShareDenyNone);
  try
    BakFileLoader := TBackupFileLoaderFactory.Create(BakFileStream);
    BakFileLoader.Load;
    // Test for correct file ID if present (NulFileID indicates not present)
    if (BakFileLoader.FileID <> TBackupFileLoader.NulFileID)
      and (BakFileLoader.FileID <> fFileID) then
      raise EBackupFileLoader.CreateFmt(sBadFileID, [FileSpec]);
    // Restore each file
    for FileInfo in BakFileLoader do
    begin
      FileSpec := SourceFileSpec(FileInfo.Name);
      DOSDateTime := TDOSDateTimeFactory.CreateFromDOSTimeStamp(
        FileInfo.TimeStamp
      );
      // restore file
      TFile.WriteAllBytes(FileSpec, FileInfo.Content);
      DOSDateTime.ApplyToFile(FileSpec);
    end;
  finally
    BakFileLoader.Free;
    BakFileStream.Free;
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

{ TBackupFileLoader }

constructor TBackupFileLoader.Create(const Stream: TStream);
begin
  inherited Create;
  fFiles := TList<TFileInfo>.Create;
  fEncoding := GetFileEncoding;
  fReader := TDataStreamReader.Create(Stream, fEncoding, []);
  fBinReader := TBinDataStreamReader.Create(Stream, fEncoding, []);
end;

destructor TBackupFileLoader.Destroy;
begin
  fBinReader.Free;
  fReader.Free;
  TEncodingHelper.FreeEncoding(fEncoding);
  fFiles.Free;
  inherited;
end;

function TBackupFileLoader.GetEnumerator: TEnumerator<TFileInfo>;
begin
  Result := fFiles.GetEnumerator;
end;

procedure TBackupFileLoader.Load;
resourcestring
  // Error message
  sReadError = 'Error reading backup file: %s';
var
  FileCount: SmallInt;  // number of files in backup
  I: Integer;           // loops through all files in backup
  FI: TFileInfo;        // stores information about each file
begin
  try
    ReadHeader(fFileID, FileCount);
    for I := 1 to FileCount do
    begin
      ReadFileInfo(FI);
      fFiles.Add(FI);
    end;
  except
    on E: EStreamError do
      raise EBackupFileLoader.CreateFmt(sReadError, [E.Message]);
    else
      raise;
  end;
end;

procedure TBackupFileLoader.TestChecksums(const FileName: string;
  const Required, Actual: TPJMD5Digest);
resourcestring
  // Error message
  sCorruptFile = 'Backup file is corrupt. Checksum error in file %s';
begin
  if Required <> Actual then
    raise EBackupFileLoader.CreateFmt(sCorruptFile, [FileName]);
end;

{ TBackupFileLoaderFactory }

class function TBackupFileLoaderFactory.Create(
  const Stream: TStream): TBackupFileLoader;
resourcestring
  // Error messages
  sUnknownVersion = 'Unsupported backup file version: %d';
  sBadFormat = 'Backup file is not in required format';
var
  Version: SmallInt;  // backup file version
const
  Map: array[1..4] of TBackupFileLoaderClass = (
    TV1BackupFileLoader,
    TV2BackupFileLoader,
    TV3BackupFileLoader,
    TV4BackupFileLoader
  );
begin
  try
    Version := GetFileVersion(Stream);
    if Version > LastSupportedVersion then
      raise EBackupFileLoader.CreateFmt(sUnknownVersion, [Version]);
    Result := Map[Version].Create(Stream);
  except
    on E: EStreamError do
      raise EBackupFileLoader.Create(sBadFormat);
    else
      raise;
  end;
end;

class function TBackupFileLoaderFactory.GetFileVersion(
  const Stream: TStream): SmallInt;
var
  Reader: TDataStreamReader;  // reads formatted data from stream
  FirstWord: SmallInt;        // first 16 bit word in stream
begin
  Reader := TDataStreamReader.Create(Stream, TEncoding.ASCII, []);
  try
    FirstWord := Reader.ReadSmallInt;
    if FirstWord <> SmallInt($FFFF) then
      Result := 1 // file doesn't begin with watermark -> version 1 file
    else
      Result := Reader.ReadSmallInt;  // file version stored at file offset 4
    Stream.Position := 0;
  finally
    Reader.Free;
  end;
end;

{ TV1BackupFileLoader }

function TV1BackupFileLoader.GetFileEncoding: TEncoding;
begin
  Result := TEncoding.Default;
end;

procedure TV1BackupFileLoader.ReadFileInfo(
  out FileInfo: TBackupFileLoader.TFileInfo);
var
  Content: AnsiString;    // content of file being restored
  Checksum: TPJMD5Digest; // file checksum recorded in backup
begin
  FileInfo.Name := Reader.ReadSizedString;
  FileInfo.TimeStamp := Reader.ReadLongInt;
  Checksum := Reader.ReadString(32);
  // checksum was made of content as ANSI string so we load it as such, test
  // checksum and only then convert to Unicode
  Content := Reader.ReadSizedAnsiString;
  TestChecksums(FileInfo.Name, Checksum, TPJMD5.Calculate(Content));
  SetLength(FileInfo.Content, Length(Content));
  if Length(Content) > 0 then
    Move(Content[1], FileInfo.Content[0], Length(Content));
end;

procedure TV1BackupFileLoader.ReadHeader(out FileID, FileCount: SmallInt);
begin
  FileID := NulFileID;
  FileCount := Reader.ReadSmallInt;
end;

{ TV2BackupFileLoader }

function TV2BackupFileLoader.GetFileEncoding: TEncoding;
begin
  Result := TEncoding.Default;
end;

procedure TV2BackupFileLoader.ReadFileInfo(
  out FileInfo: TBackupFileLoader.TFileInfo);
var
  Content: AnsiString;    // content of file being restored
  Checksum: TPJMD5Digest; // file checksum recorded in backup
begin
  FileInfo.Name := Reader.ReadSizedString;
  FileInfo.TimeStamp := Reader.ReadLongInt;
  Checksum := Reader.ReadString(32);
  // Checksum was made of content as ANSI string so we load it as such, test
  // checksum and only then convert to Unicode
  Content := Reader.ReadSizedLongAnsiString;
  TestChecksums(FileInfo.Name, Checksum, TPJMD5.Calculate(Content));
  SetLength(FileInfo.Content, Length(Content));
  if Length(Content) > 0 then
    Move(Content[1], FileInfo.Content[0], Length(Content));
end;

procedure TV2BackupFileLoader.ReadHeader(out FileID, FileCount: SmallInt);
begin
  FileID := NulFileID;
  Reader.ReadSmallInt;  // skip over watermark 'FFFF'
  Reader.ReadSmallInt;  // skip over version number '0002'
  FileCount := Reader.ReadSmallInt;
end;

{ TV3BackupFileLoader }

function TV3BackupFileLoader.GetFileEncoding: TEncoding;
begin
  // Earlier version of CodeSnip that supported file format v3 wrote the backup
  // file using the system default encoding while later versions of CodeSnip
  // used Windows-1252. Windows-1252 is used here for greater compatibility with
  // files written by later program versions.
  Result := TEncodingHelper.GetEncoding(TEncodingHelper.Windows1252CharSetName);
end;

procedure TV3BackupFileLoader.ReadFileInfo(
  out FileInfo: TBackupFileLoader.TFileInfo);
var
  Content: Windows1252String; // content of file being restored
  Checksum: TPJMD5Digest;     // file checksum recorded in backup
begin
  FileInfo.Name := Reader.ReadSizedString;
  FileInfo.TimeStamp := Reader.ReadLongInt;
  Checksum := Reader.ReadString(32);
  // checksum was made of content as ANSI string so we load it as such, test
  // checksum and only then convert to Unicode
  Content := Reader.ReadSizedLongAnsiString;
  TestChecksums(FileInfo.Name, Checksum, TPJMD5.Calculate(Content));
  SetLength(FileInfo.Content, Length(Content));
  if Length(Content) > 0 then
    Move(Content[1], FileInfo.Content[0], Length(Content));
end;

procedure TV3BackupFileLoader.ReadHeader(out FileID, FileCount: SmallInt);
begin
  Reader.ReadSmallInt;  // skip over watermark 'FFFF'
  Reader.ReadSmallInt;  // skip over version number '0003'
  FileID := Reader.ReadSmallInt;
  FileCount := Reader.ReadSmallInt;
end;

{ TV4BackupFileLoader }

function TV4BackupFileLoader.GetFileEncoding: TEncoding;
begin
  Result := TEncoding.UTF8;
end;

procedure TV4BackupFileLoader.ReadFileInfo(
  out FileInfo: TBackupFileLoader.TFileInfo);
var
  Checksum: TPJMD5Digest;
begin
  FileInfo.Name := BinReader.ReadSmallSizedString;
  FileInfo.TimeStamp := BinReader.ReadLongInt;
  CheckSum := BinReader.ReadSmallSizedBytes;
  FileInfo.Content := BinReader.ReadLongSizedBytes;
  TestCheckSums(FileInfo.Name, Checksum, TPJMD5.Calculate(FileInfo.Content));
end;

procedure TV4BackupFileLoader.ReadHeader(out FileID, FileCount: SmallInt);
begin
  Reader.ReadSmallInt;  // skip over watermark 'FFFF'
  Reader.ReadSmallInt;  // skip over version number '0004'
  Reader.ReadSmallInt;  // skip over padding '0000'
  Reader.ReadSmallInt;  // skip over padding '0000'
  FileID := Reader.ReadSmallInt;
  FileCount := Reader.ReadSmallInt;
end;

{ TBackupFileWriter }

constructor TBackupFileWriter.Create(const Files: TStrings;
  const Stream: TStream; const FileID: SmallInt);
begin
  inherited Create;
  fFiles := Files;
  fStream := Stream;
  fFileID := FileID;
end;

destructor TBackupFileWriter.Destroy;
begin

  inherited;
end;

procedure TBackupFileWriter.Save;
var
  FileName: string;
begin
  WriteHeader;
  for FileName in fFiles do
    WriteFileInfo(FileName);
end;

procedure TBackupFileWriter.WriteFileInfo(const FileName: string);
var
  DOSDateTime: IDOSDateTime;
  FileBytes: TBytes;
  BinWriter: TBinDataStreamWriter;
begin
  // Get content and date stamp of file
  FileBytes := TFile.ReadAllBytes(FileName);
  DOSDateTime := TDOSDateTimeFactory.CreateFromFile(FileName);
  // Write the data
  BinWriter := TBinDataStreamWriter.Create(fStream, TEncoding.UTF8, []);
  try
    BinWriter.WriteSmallSizedString(ExtractFileName(FileName));
    BinWriter.WriteLongInt(DOSDateTime.DateStamp);
    BinWriter.WriteSmallSizedBytes(TPJMD5.Calculate(FileBytes));
    BinWriter.WriteLongSizedBytes(FileBytes);
  finally
    BinWriter.Free;
  end;
end;

procedure TBackupFileWriter.WriteHeader;
var
  TextWriter: TDataStreamWriter;
begin
  // Header is written in formatted text for backwards compatibilty
  TextWriter := TDataStreamWriter.Create(fStream, TEncoding.ASCII, []);
  try
    TextWriter.WriteSmallInt(SmallInt($FFFF));
    TextWriter.WriteSmallInt(4);
    TextWriter.WriteSmallInt(0);
    TextWriter.WriteSmallInt(0);
    TextWriter.WriteSmallInt(fFileID);
    TextWriter.WriteSmallInt(fFiles.Count);
  finally
    TextWriter.Free;
  end;
end;

end.


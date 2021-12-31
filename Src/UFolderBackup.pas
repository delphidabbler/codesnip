{
 * This Source Code Form is subject to the terms of the Mozilla Public License,
 * v. 2.0. If a copy of the MPL was not distributed with this file, You can
 * obtain one at https://mozilla.org/MPL/2.0/
 *
 * Copyright (C) 2009-2021, Peter Johnson (gravatar.com/delphidabbler).
 *
 * Code that that can back up and restore all the files in a folder to or from a
 * single file. Legacy support for restoring data from legacy backup file
 * formats is provided.
}


unit UFolderBackup;


interface


type

  ///  <summary>
  ///  Class that can back up and restore all the files in a folder to or from
  ///  a single file.
  ///  </summary>
  TFolderBackup = class(TObject)
  strict private
    var fSrcFolder: string;   // Folder to be backed up or restored
    var fBakFile: string;     // Full path to backup file
    var fFileID: SmallInt;    // Identifies the type of file
  public
    ///  <summary>
    ///  Creates object for a specific folder and backup file type.
    ///  </summary>
    ///  <param name="SrcFolder">Folder containing files to be backed up.
    ///  Sub-directories are ignored.</param>
    ///  <param name="BakFile">Name of backup file.</param>
    ///  <param name="FileID">Backup file type identifier. Specified backup file
    ///  must have this identifier.</param>
    constructor Create(const SrcFolder, BakFile: string;
      const FileID: SmallInt);
    ///  <summary>
    ///  Backs up the files from the source folder passed to constructor into a
    ///  single backup file.
    ///  </summary>
    ///  <remarks>
    ///  Backup file is marked with the file ID passed to constructor.
    ///  </remarks>
    procedure Backup;
    ///  <summary>
    ///  Restores files from backup file into source folder passed to
    ///  constructor
    ///  </summary>
    ///  <remarks>
    ///  An exception is raised if file type id is not the one passed to the
    ///  constructor or if any checksum errors are detected.
    ///  </remarks>
    procedure Restore;
  end;


implementation


uses
  // Delphi
  SysUtils, Classes, Generics.Collections,
  // DelphiDabbler library
  PJMD5,
  // Project
  UBaseObjects, UDataStreamIO, UDOSDateTime, UEncodings, UExceptions, UIOUtils,
  UUtils;


type

  ///  Reference to TBackupFileLoader classes
  TBackupFileLoaderClass = class of TBackupFileLoader;

  ///  Static class that provides information about backup files that is of use
  ///  to other classes.
  TBackupFileInfo = class(TNoConstructObject)
  public
    ///  Returns a reference to the class used to load files saved in the latest
    ///  backup file format.
    class function CurrentLoader: TBackupFileLoaderClass;
    ///  Coverts a string containing a file watermark into a byte array.
    class function WatermarkBytes(const Watermark: string): TBytes;
  end;

  ///  Class that writes files to a backup file using the latest file format.
  TBackupFileWriter = class(TObject)
  strict private
    var fFiles: TStrings;   // Files to be backed up
    var fStream: TStream;   // Stream to receive backed up files
    var fFileID: SmallInt;  // ID of backup file
    ///  Writes file header
    procedure WriteHeader;
    ///  Writes meta information and contents of a specified file.
    procedure WriteFileInfo(const FileName: string);
  public
    ///  Sets up object to write specified files to a given stream marked with
    ///  given file ID.
    constructor Create(const Files: TStrings; const Stream: TStream;
      const FileID: SmallInt);
    ///  Generates backup file and writes it to stream passed to constructor.
    procedure Generate;
  end;

  ///  Abstract base class for classes that load backup files. There is a
  ///  different sub class for each supported version of the backup file format.
  TBackupFileLoader = class abstract(TObject)
  public
    type
      ///  Provides information about a file to be restored from backup
      TFileInfo = record
        Name: string;           // File name without path
        TimeStamp: LongInt;     // File's timestamp
        Content: TBytes;        // Content of file as bytes
        Checksum: TPJMD5Digest; // Checksum recorded in backup file
      end;
    const NulFileID = 0;  // Used for File ID is formats that don't support it
  strict private
    var fFileID: SmallInt;          // Value of FileID property
    var fFiles: TList<TFileInfo>;   // List of file info records
    var fEncoding: TEncoding;       // Text encoding used in backup file
    var fStream: TStream;           // Stream containing backup data being read
    var fReader: TDataStreamReader; // Object used to read data from stream
    ///  Checks that a specified file checksum matches that recorded in backup
    ///  file. Raises an exception if checksums are different.
    procedure TestChecksums(const FileInfo: TFileInfo);
  strict protected
    ///  Gets an instance of the encoding used for the backup file.
    function GetFileEncoding: TEncoding; virtual; abstract;
    ///  Creates a new data reader to read data from backup file stream.
    function CreateReader(const Stream: TStream): TDataStreamReader; virtual;
      abstract;
    ///  Skips over watermark at start of file.
    procedure SkipWatermark;
    ///  Reads header information from the backup file.
    procedure ReadHeader(out FileID, FileCount: SmallInt); virtual; abstract;
    ///  Reads information about a file to be restored from the backup file.
    procedure ReadFileInfo(out FileInfo: TFileInfo); virtual; abstract;
    ///  Reference to encoding used to read text from the backup file stream.
    property Encoding: TEncoding read fEncoding;
    ///  Reference to object used to read data from the backup file stream.
    property Reader: TDataStreamReader read fReader;
  public
    ///  Sets up object to get data from a given stream.
    constructor Create(const Stream: TStream); virtual;
    ///  Tears down object.
    destructor Destroy; override;
    ///  Loads the backup file from the stream storing required information in
    ///  this object's properties and file list.
    procedure Load;
    ///  Backup file identifier.
    property FileID: SmallInt read fFileID;
    ///  Gets enumerator for file list.
    function GetEnumerator: TEnumerator<TFileInfo>;
    ///  Gets file's unique watermark
    class function GetWatermark: TBytes; virtual; abstract;
  end;

  ///  Base class for loaders that read data as formatted text.
  TTextBackupFileLoader = class abstract(TBackupFileLoader)
  strict protected
    ///  Creates a new formatted text reader object to read data from backup
    ///  file stream.
    function CreateReader(const Stream: TStream): TDataStreamReader; override;
  end;

  ///  Base class for loaders that read data as formatted binary.
  TBinaryBackupFileLoader = class abstract(TBackupFileLoader)
  strict protected
    ///  Creates a new binary data reader object to read data from backup file
    ///  stream.
    function CreateReader(const Stream: TStream): TDataStreamReader; override;
  end;

  ///  Loads and provides access to data from a backup file that has the version
  ///  1 file format.
  TV1BackupFileLoader = class(TTextBackupFileLoader)
  strict protected
    ///  Returns the backup file's text encoding
    function GetFileEncoding: TEncoding; override;
    ///  Reads header information from the backup file.
    procedure ReadHeader(out FileID, FileCount: SmallInt); override;
    ///  Reads information about a file to be restored from the backup file.
    procedure ReadFileInfo(out FileInfo: TBackupFileLoader.TFileInfo); override;
    ///  Returns the watermark that identifies the file format. In the v1 file
    ///  format there is no watermark so an empty byte array is returned.
    class function GetWatermark: TBytes; override;
  end;

  ///  Loads and provides access to data from a backup file that has the version
  ///  2 file format.
  TV2BackupFileLoader = class(TTextBackupFileLoader)
  strict protected
    ///  Returns the backup file's text encoding
    function GetFileEncoding: TEncoding; override;
    ///  Reads header information from the backup file.
    procedure ReadHeader(out FileID, FileCount: SmallInt); override;
    ///  Reads information about a file to be restored from the backup file.
    procedure ReadFileInfo(out FileInfo: TBackupFileLoader.TFileInfo); override;
    ///  Returns the watermark that identifies the file format.
    class function GetWatermark: TBytes; override;
  end;

  ///  Loads and provides access to data from a backup file that has the version
  ///  3 file format.
  TV3BackupFileLoader = class(TTextBackupFileLoader)
  strict protected
    ///  Returns the backup file's text encoding
    function GetFileEncoding: TEncoding; override;
    ///  Reads header information from the backup file.
    procedure ReadHeader(out FileID, FileCount: SmallInt); override;
    ///  Reads information about a file to be restored from the backup file.
    procedure ReadFileInfo(out FileInfo: TBackupFileLoader.TFileInfo); override;
    ///  Returns the watermark that identifies the file format.
    class function GetWatermark: TBytes; override;
  end;

  ///  Loads and provides access to data from a backup file that has the version
  ///  4 file format.
  TV4BackupFileLoader = class(TBinaryBackupFileLoader)
  strict protected
    ///  Returns the backup file's text encoding
    function GetFileEncoding: TEncoding; override;
    ///  Reads header information from the backup file.
    procedure ReadHeader(out FileID, FileCount: SmallInt); override;
    ///  Reads information about a file to be restored from the backup file.
    procedure ReadFileInfo(out FileInfo: TBackupFileLoader.TFileInfo); override;
    ///  Returns the watermark that identifies the file format.
    class function GetWatermark: TBytes; override;
  end;

  ///  Factory class that creates the required TBackupFileLoader object to load
  ///  different backup file formats. Determines required class by examining
  ///  watermarks.
  TBackupFileLoaderFactory = class sealed(TNoConstructObject)
  strict private
    ///  List of backup file loader classes: one for each file format.
    class var fClassList: TArray<TBackupFileLoaderClass>;
    ///  Gets file loader class from watermark contained at start of given
    ///  stream.
    class function GetLoaderClass(const Stream: TStream):
      TBackupFileLoaderClass;
  public
    ///  Populates list of loader classes.
    class constructor Create;
    ///  Creates an instance of the correct backup file loader to load the
    ///  backup file from the given stream. Loader type depends on file format
    ///  used for backup file.
    class function Create(const Stream: TStream): TBackupFileLoader;
  end;

  ///  Type of exception raised by backup file loader objects.
  EBackupFileLoader = class(ECodeSnip);

{ TFolderBackup }

procedure TFolderBackup.Backup;
var
  Files: TStringList;             // list of files to back up
  FS: TFileStream;                // stream onto backup file
  FileWriter: TBackupFileWriter;  // writes backup file
begin
  Files := TStringList.Create;
  try
    ListFiles(fSrcFolder, '*.*', Files, False);
    FS := TFileStream.Create(fBakFile, fmCreate);
    try
      FileWriter := TBackupFileWriter.Create(Files, FS, fFileID);
      try
        FileWriter.Generate;
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
begin
  inherited Create;
  fSrcFolder := ExcludeTrailingPathDelimiter(SrcFolder);
  fBakFile := BakFile;
  fFileID := FileID;
end;

procedure TFolderBackup.Restore;
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
      FileSpec := IncludeTrailingPathDelimiter(fSrcFolder) + FileInfo.Name;
      DOSDateTime := TDOSDateTimeFactory.CreateFromDOSTimeStamp(
        FileInfo.TimeStamp
      );
      TFileIO.WriteAllBytes(FileSpec, FileInfo.Content);
      DOSDateTime.ApplyToFile(FileSpec);
    end;
  finally
    BakFileLoader.Free;
    BakFileStream.Free;
  end;
end;

{ TBackupFileLoader }

constructor TBackupFileLoader.Create(const Stream: TStream);
begin
  inherited Create;
  fFiles := TList<TFileInfo>.Create;
  fEncoding := GetFileEncoding;
  fStream := Stream;
  fReader := CreateReader(Stream);
end;

destructor TBackupFileLoader.Destroy;
begin
  TEncodingHelper.FreeEncoding(fEncoding);
  fReader.Free;
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
    SkipWatermark;
    ReadHeader(fFileID, FileCount);
    for I := 1 to FileCount do
    begin
      ReadFileInfo(FI);
      TestChecksums(FI);
      fFiles.Add(FI);
    end;
  except
    on E: EStreamError do
      raise EBackupFileLoader.CreateFmt(sReadError, [E.Message]);
    else
      raise;
  end;
end;

procedure TBackupFileLoader.SkipWatermark;
begin
  fStream.Position := Length(GetWatermark);
end;

procedure TBackupFileLoader.TestChecksums(const FileInfo: TFileInfo);
resourcestring
  // Error message
  sCorruptFile = 'Backup file is corrupt. Checksum error in file %s';
begin
  if TPJMD5.Calculate(FileInfo.Content) <> FileInfo.Checksum then
    raise EBackupFileLoader.CreateFmt(sCorruptFile, [FileInfo.Name]);
end;

{ TTextBackupFileLoader }

function TTextBackupFileLoader.CreateReader(
  const Stream: TStream): TDataStreamReader;
begin
  Result := TTextStreamReader.Create(Stream, Encoding, []);
end;

{ TBinaryBackupFileLoader }

function TBinaryBackupFileLoader.CreateReader(
  const Stream: TStream): TDataStreamReader;
begin
  Result := TBinaryStreamReader.Create(Stream, Encoding, []);
end;

{ TV1BackupFileLoader }

function TV1BackupFileLoader.GetFileEncoding: TEncoding;
begin
  Result := TEncoding.Default;
end;

class function TV1BackupFileLoader.GetWatermark: TBytes;
begin
  SetLength(Result, 0);
end;

procedure TV1BackupFileLoader.ReadFileInfo(
  out FileInfo: TBackupFileLoader.TFileInfo);
begin
  FileInfo.Name := Reader.ReadSizedString16;
  FileInfo.TimeStamp := Reader.ReadInt32;
  FileInfo.Checksum := Reader.ReadBytes(SizeOf(FileInfo.Checksum));
  // checksum was made of content as ANSI string so we load it as raw data, not
  // as a string which would perform a coversion and change the checksum
  FileInfo.Content := Reader.ReadSizedRawData16;
end;

procedure TV1BackupFileLoader.ReadHeader(out FileID, FileCount: SmallInt);
begin
  FileID := NulFileID;
  FileCount := Reader.ReadInt16;
end;

{ TV2BackupFileLoader }

function TV2BackupFileLoader.GetFileEncoding: TEncoding;
begin
  Result := TEncoding.Default;
end;

class function TV2BackupFileLoader.GetWatermark: TBytes;
begin
  Result := TBackupFileInfo.WatermarkBytes('FFFF0002');
end;

procedure TV2BackupFileLoader.ReadFileInfo(
  out FileInfo: TBackupFileLoader.TFileInfo);
begin
  FileInfo.Name := Reader.ReadSizedString16;
  FileInfo.TimeStamp := Reader.ReadInt32;
  FileInfo.Checksum := Reader.ReadBytes(SizeOf(FileInfo.Checksum));
  // checksum was made of content as ANSI string so we load it as raw data, not
  // as a string which would perform a coversion and change the checksum
  FileInfo.Content := Reader.ReadSizedRawData32;
end;

procedure TV2BackupFileLoader.ReadHeader(out FileID, FileCount: SmallInt);
begin
  FileID := NulFileID;
  FileCount := Reader.ReadInt16;
end;

{ TV3BackupFileLoader }

function TV3BackupFileLoader.GetFileEncoding: TEncoding;
begin
  // Earlier version of CodeSnip that supported file format v3 wrote the backup
  // file using the system default encoding while later versions of CodeSnip
  // used Windows-1252. Windows-1252 is used here for greater compatibility with
  // files written by later program versions.
  Result := TEncodingHelper.GetEncoding(TEncodingHelper.Windows1252CodePage);
end;

class function TV3BackupFileLoader.GetWatermark: TBytes;
begin
  Result := TBackupFileInfo.WatermarkBytes('FFFF0003');
end;

procedure TV3BackupFileLoader.ReadFileInfo(
  out FileInfo: TBackupFileLoader.TFileInfo);
begin
  FileInfo.Name := Reader.ReadSizedString16;
  FileInfo.TimeStamp := Reader.ReadInt32;
  FileInfo.Checksum := Reader.ReadBytes(SizeOf(FileInfo.Checksum));
  // checksum was made of content as ANSI string so we load it as raw data, not
  // as a string which would perform a coversion and change the checksum
  FileInfo.Content := Reader.ReadSizedRawData32;
end;

procedure TV3BackupFileLoader.ReadHeader(out FileID, FileCount: SmallInt);
begin
  FileID := Reader.ReadInt16;
  FileCount := Reader.ReadInt16;
end;

{ TV4BackupFileLoader }

function TV4BackupFileLoader.GetFileEncoding: TEncoding;
begin
  Result := TEncoding.UTF8;
end;

class function TV4BackupFileLoader.GetWatermark: TBytes;
begin
  Result := TBackupFileInfo.WatermarkBytes('FFFF000400000000');
end;

procedure TV4BackupFileLoader.ReadFileInfo(
  out FileInfo: TBackupFileLoader.TFileInfo);
begin
  FileInfo.Name := Reader.ReadSizedString16;
  FileInfo.TimeStamp := Reader.ReadInt32;
  FileInfo.Checksum := Reader.ReadBytes(SizeOf(FileInfo.Checksum));
  FileInfo.Content := Reader.ReadSizedRawData32;
end;

procedure TV4BackupFileLoader.ReadHeader(out FileID, FileCount: SmallInt);
begin
  FileID := Reader.ReadInt16;
  FileCount := Reader.ReadInt16;
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

procedure TBackupFileWriter.Generate;
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
  BinWriter: TBinaryStreamWriter;
begin
  // Get content and date stamp of file
  FileBytes := TFileIO.ReadAllBytes(FileName);
  DOSDateTime := TDOSDateTimeFactory.CreateFromFile(FileName);
  // Write the data
  BinWriter := TBinaryStreamWriter.Create(fStream, TEncoding.UTF8, []);
  try
    BinWriter.WriteSizedString16(ExtractFileName(FileName));
    BinWriter.WriteInt32(DOSDateTime.DateStamp);
    BinWriter.WriteBytes(TPJMD5.Calculate(FileBytes));
    BinWriter.WriteSizedBytes32(FileBytes);
  finally
    BinWriter.Free;
  end;
end;

procedure TBackupFileWriter.WriteHeader;
var
  BinWriter: TBinaryStreamWriter;
begin
  BinWriter := TBinaryStreamWriter.Create(fStream, False);
  try
    BinWriter.WriteBytes(
      TBackupFileInfo.CurrentLoader.GetWatermark
    );
    BinWriter.WriteInt16(fFileID);
    BinWriter.WriteInt16(fFiles.Count);
  finally
    BinWriter.Free;
  end;
end;

{ TBackupFileLoaderFactory }

class function TBackupFileLoaderFactory.Create(
  const Stream: TStream): TBackupFileLoader;
resourcestring
  // Error messages
  sUnknownFormat = 'Unsupported backup file format';
  sBadFormat = 'Backup file is not in required format';
var
  LoaderCls: TBackupFileLoaderClass;
begin
  try
    LoaderCls := GetLoaderClass(Stream);
    Stream.Position := 0;
    if not Assigned(LoaderCls) then
      raise EBackupFileLoader.Create(sUnknownFormat);
    Result := LoaderCls.Create(Stream);
  except
    on E: EStreamError do
      raise EBackupFileLoader.Create(sBadFormat);
    else
      raise;
  end;
end;

class constructor TBackupFileLoaderFactory.Create;
begin
  fClassList := TArray<TBackupFileLoaderClass>.Create(
    TV1BackupFileLoader, TV2BackupFileLoader,
    TV3BackupFileLoader, TV4BackupFileLoader
  );
end;

class function TBackupFileLoaderFactory.GetLoaderClass(
  const Stream: TStream): TBackupFileLoaderClass;

  // Gets size in bytes of largest watermark of any file format
  function MaxWatermarkSize: Integer;
  var
    LoaderCls: TBackupFileLoaderClass;
    Size: Integer;
  begin
    Result := 0;
    for LoaderCls in fClassList do
    begin
      Size := Length(LoaderCls.GetWatermark);
      if Size > Result then
        Result := Size;
    end;
  end;

  // Loads and returns given number of bytes from start of stream. If stream has
  // less than Count bytes returned byte array is padded to size with 0 bytes.
  function LoadFirstFewBytes(Count: Integer): TBytes;
  begin
    SetLength(Result, Count);
    FillChar(Pointer(Result)^, Count, 0);
    Stream.Read(Pointer(Result)^, Length(Result));
  end;

  // Checks if Buf begin with the bytes of the Test byte array. The size of Buf
  // must be at least as great as the size of Test.
  function FirstBytesMatch(const Buf, Test: TBytes): Boolean;
  var
    I: Integer;
  begin
    Assert(Length(Buf) >= Length(Test), ClassName
      + '.GetLoaderClass:FirstBytesMatch: Buf shorter than Test');
    if Length(Test) = 0 then
      Exit(False);
    Result := True;
    for I := 0 to Pred(Length(Test)) do
    begin
      if Buf[I] <> Test[I] then
        Exit(False);
    end;
  end;

var
  Buffer: TBytes;                     // first few bytes of stream
  LoaderCls: TBackupFileLoaderClass;  // class of each supported loader
  Watermark: TBytes;                  // watermark of each file format
begin
  // load sufficient bytes from stream to compare with watermark
  Buffer := LoadFirstFewBytes(MaxWatermarkSize);
  // assume v1 format
  Result := TV1BackupFileLoader;
  // scan all loader classes check to see if their watermark matches first few
  // bytes of stream (doesn't work for v1 which has no watermark)
  for LoaderCls in fClassList do
  begin
    Watermark := LoaderCls.GetWatermark;
    if FirstBytesMatch(Buffer, Watermark) then
      Exit(LoaderCls);
  end;
  // if we get here we have either v1 or unsupported file format: if stream
  // starts with 'FFFF' it's not v1
  if FirstBytesMatch(
    Buffer, TEncoding.ASCII.GetBytes('FFFF')
  ) then
    Exit(nil);
end;

{ TBackupFileInfo }

class function TBackupFileInfo.CurrentLoader: TBackupFileLoaderClass;
begin
  Result := TV4BackupFileLoader;
end;

class function TBackupFileInfo.WatermarkBytes(const Watermark: string):
  TBytes;
begin
  Result := TEncoding.ASCII.GetBytes(Watermark);
end;

end.


{
 * This Source Code Form is subject to the terms of the Mozilla Public License,
 * v. 2.0. If a copy of the MPL was not distributed with this file, You can
 * obtain one at http://mozilla.org/MPL/2.0/
 *
 * Copyright (C) 2020 Peter Johnson (www.delphidabbler.com).
 *
 * Implements classes that can pack and unpack files to anf from single package
 * files.
}


unit CS.Database.IO.Packages;

interface

uses
  // VCL
  SysUtils,
  Classes,
  Generics.Collections,
  // Library
  PJMD5,
  // Project
  UDataStreamIO,
  UExceptions,
  UIStringList;

type

  ///  <summary>Class that combines files into a single package and outputs the
  ///  package to a stream.</summary>
  ///  <remarks>All files to be combined must be in the same directory.
  ///  </remarks>
  TPackageWriter = class(TObject)
  strict private
    var
      ///  <summary>Directory from which to read files to be packaged.</summary>
      fDir: string;
      ///  <summary>Names of files to be packaged.</summary>
      fFiles: IStringList;
      ///  <summary>Stream to receive packaged files.</summary>
      fOutStream: TStream;
      ///  <summary>Specifies type of package being created.</summary>
      fPackageType: SmallInt;
    ///  <summary>Writes package header to output stream.</summary>
    procedure WriteHeader;
    ///  <summary>Writes meta information and contents of given file to output
    ///  stream.</summary>
    procedure WriteFileInfo(const AFileName: string);
  public
    ///  <summary>Constructs object used to write specified files to a package.
    ///  </summary>
    ///  <param name="ADirectory">string [in] Directory containing files to be
    ///  packaged.</param>
    ///  <param name="AFiles">TStrings [in] List of names of files to be
    ///  packaged. File names must exclude path information and all files must
    ///  exists in ADirectory.</param>
    ///  <param name="AOutStream">TStream [in] Stream to receive package.
    ///  </param>
    ///  <param name="AID">SmallInt [in] Type of package to be created. See
    ///  package file format documentation for valid package types.</param>
    constructor Create(const ADirectory: string; AFiles: IStringList;
      const AOutStream: TStream; const APackageType: SmallInt);
    ///  <summary>Generates package and writes it to stream.</summary>
    procedure Generate;
  end;

  ///  <summary>Abstract base class for classes that load packages. There is a
  ///  different sub class for each supported version of the package file
  ///  format.</summary>
  TPackageLoader = class abstract(TObject)
  public
    type
      ///  <summary>Provides information about a file contained within package.
      ///  </summary>
      TFileInfo = record
        ///  <summary>File name without path.</summary>
        Name: string;
        ///  <summary>File's time stamp.</summary>
        TimeStamp: LongInt;
        ///  <summary>Content of file as bytes.</summary>
        Content: TBytes;
        ///  <summary>Checksum recorded in package.</summary>
        Checksum: TPJMD5Digest;
      end;
  strict private
    var
      ///  <summary>Value of FileID property.</summary>
      fFileID: SmallInt;
      ///  <summary>List of file info records.</summary>
      fFiles: TList<TFileInfo>;
      ///  <summary>Text encoding used in package file.</summary>
      fEncoding: TEncoding;
      /// <summary>Stream containing package data being read.</summary>
      fStream: TStream;
      /// <summary>Object used to read data from stream.</summary>
      fReader: TDataStreamReader;
    ///  <summary>Checks that a checksum of specified file matches that recorded
    ///  in package file. Raises an exception if checksums are different.
    ///  </summary>
    procedure TestChecksums(const FileInfo: TFileInfo);
  strict protected
    ///  <summary>Gets an instance of the encoding used for the package file.
    ///  </summary>
    function GetFileEncoding: TEncoding; virtual; abstract;
    ///  <summary>Creates a new data reader to read data from package stream.
    ///  </summary>
    function CreateReader(const Stream: TStream): TDataStreamReader; virtual;
      abstract;
    ///  <summary>Skips over watermark at start of file.</summary>
    procedure SkipWatermark;
    ///  <summary>Reads header information from the package file.</summary>
    procedure ReadHeader(out FileID, FileCount: SmallInt); virtual; abstract;
    ///  <summary>Reads information about a file contained in the package
    ///  </summary>
    procedure ReadFileInfo(out FileInfo: TFileInfo); virtual; abstract;
    ///  <summary>Reference to encoding used to read text from package.
    ///  </summary>
    property Encoding: TEncoding read fEncoding;
    ///  <summary>Reference to object used to read data from the package stream.
    ///  </summary>
    property Reader: TDataStreamReader read fReader;
  public
    ///  <summary>Sets up object to get data from a given stream.</summary>
    constructor Create(const Stream: TStream); virtual;
    ///  <summary>Tears down object.</summary>
    destructor Destroy; override;
    ///  Loads the package file from the stream storing required information in
    ///  this object's properties and file list.
    procedure Load;
    ///  Gets enumerator for file list.
    function GetEnumerator: TEnumerator<TFileInfo>;
    ///  Gets file's unique watermark
    class function GetWatermark: TBytes; virtual; abstract;
    ///  Package file identifier.
    property FileID: SmallInt read fFileID;
  end;

  ///  <summary>Type of exception raised by the package loader.</summary>
  EPackageLoader = class(ECodeSnip);



implementation

uses
  // Project
  UDOSDateTime,
  UEncodings,
  UIOUtils;

type
  TPackageLoaderClass = class of TPackageLoader;

  ///  <summary>Loads packages that use a binary format.</summary>
  ///  <remarks>All currently supported packages have this format.</remarks>
  TBinaryPackageLoader = class abstract(TPackageLoader)
  strict protected
    ///  <summary>Creates a new binary data reader object to read data from the
    ///  package contained in the given stream.</summary>
    function CreateReader(const Stream: TStream): TDataStreamReader; override;
    ///  <summary>Returns the package's text encoding.</summary>
    function GetFileEncoding: TEncoding; override;
    ///  <summary>Reads header information from the package.</summary>
    procedure ReadHeader(out FileID, FileCount: SmallInt); override;
    ///  <summary>Reads information about the next file contained stored in the
    ///  package.</summary>
    procedure ReadFileInfo(out FileInfo: TPackageLoader.TFileInfo); override;
    ///  <summary>Returns the watermark that identifies the file format as a
    ///  byte array.</summary>
    class function GetWatermark: TBytes; override; abstract;
  end;

  ///  <summary>Loads and provides access to data from a file that has the
  ///  version 4 format.</summary>
  TV4PackageLoader = class(TBinaryPackageLoader)
  strict private
    const
      ///  <summary>Watermark found at start of all v4 packages.</summary>
      cV4Watermark = 'FFFF000400000000';
  strict protected
    ///  <summary>Returns the watermark that identifies v4 format packages as
    ///  a byte array.</summary>
    class function GetWatermark: TBytes; override;
  end;

  ///  <summary>Loads and provides access to data from a package that has the
  ///  version 5 format.</summary>
  TV5PackageLoader = class(TBinaryPackageLoader)
  strict private
    const
      ///  <summary>Watermark found at start of all v5 packages.</summary>
      cV5Watermark = 'FFFF000500000000';
  strict protected
    ///  <summary>Returns the watermark that identifies v5 format packages as
    ///  a byte array.</summary>
    class function GetWatermark: TBytes; override;
  end;

  ///  <summary>Factory that creates the required TPackageLoader object to load
  ///  different package formats.</summary>
  ///  <remarks>Determines required class by examining watermarks.</remarks>
  TPackageLoaderFactory = record
  strict private
    const
      ///  <summary>Length of supported watermarks.</summary>
      cWatermarkLength = 16;
    class var
      ///  <summary>List of package file loader classes: one for each file
      ///  format.</summary>
    fClassList: TArray<TPackageLoaderClass>;
    ///  <summary>Gets class of package loader associated with any watermark
    ///  present at start of given stream.</summary>
    ///  <remarks>Returns nil if stream does not begin with a supported
    ///  watermark.</remarks>
    class function GetLoaderClass(const Stream: TStream):
      TPackageLoaderClass; static;
  public
    ///  <summary>Populates list of loader classes.</summary>
    class constructor Create;
    ///  <summary>Creates and returns an instance of the correct package loader
    ///  class to load the package from the given stream.</summary>
    ///  <remarks>Loader type depends on the packages file format. This is
    ///  determined by the watermark that must be present at the start of the
    ///  stream.</remarks>
    ///  <exception>EStreamError raised if stream doesn't contain a supported
    ///  package format.</exception>
    class function Create(const Stream: TStream): TPackageLoader; static;
  end;

  ///  <summary>Helper record that provides information about package files.
  ///  </summary>
  TPackageInfo = record
  public
    ///  <summary>Returns a reference to the loader class for the most current
    /// package format.</summary>
    class function CurrentLoader: TPackageLoaderClass; static;
    ///  <summary>Converts the given watermark string containing into a byte
    ///  array.</summary>
    ///  <remarks>Watermark is expected to be a valid UTF-8 string. All
    ///  supported watermarks use UTF-8 characters that are also valid ASCII
    ///  characters.</remarks>
    class function WatermarkBytes(const Watermark: string): TBytes; static;
  end;


{ TPackageWriter }

constructor TPackageWriter.Create(const ADirectory: string; AFiles: IStringList;
  const AOutStream: TStream; const APackageType: SmallInt);
begin
  inherited Create;
  fFiles := AFiles;
  fOutStream := AOutStream;
  fPackageType := APackageType;
end;

procedure TPackageWriter.Generate;
var
  FileName: string;
begin
  WriteHeader;
  for FileName in fFiles do
    WriteFileInfo(IncludeTrailingPathDelimiter(fDir) + FileName);
end;

procedure TPackageWriter.WriteFileInfo(const AFileName: string);
var
  DOSDateTime: IDOSDateTime;
  FileBytes: TBytes;
  BinWriter: TBinaryStreamWriter;
begin
  // Get content and date stamp of file
  FileBytes := TFileIO.ReadAllBytes(AFileName);
  DOSDateTime := TDOSDateTimeFactory.CreateFromFile(AFileName);
  // Write the data
  BinWriter := TBinaryStreamWriter.Create(fOutStream, TEncoding.UTF8, []);
  try
    BinWriter.WriteSizedString16(ExtractFileName(AFileName));
    BinWriter.WriteInt32(DOSDateTime.DateStamp);
    BinWriter.WriteBytes(TPJMD5.Calculate(FileBytes));
    BinWriter.WriteSizedBytes32(FileBytes);
  finally
    BinWriter.Free;
  end;
end;

procedure TPackageWriter.WriteHeader;
var
  BinWriter: TBinaryStreamWriter;
begin
  BinWriter := TBinaryStreamWriter.Create(fOutStream, False);
  try
    BinWriter.WriteBytes(
      TPackageInfo.CurrentLoader.GetWatermark
    );
    BinWriter.WriteInt16(fPackageType);
    BinWriter.WriteInt16(fFiles.Count);
  finally
    BinWriter.Free;
  end;
end;

{ TPackageLoader }

constructor TPackageLoader.Create(const Stream: TStream);
begin
  inherited Create;
  fFiles := TList<TFileInfo>.Create;
  fEncoding := GetFileEncoding;
  fStream := Stream;
  fReader := CreateReader(Stream);
end;

destructor TPackageLoader.Destroy;
begin
  TEncodingHelper.FreeEncoding(fEncoding);
  fReader.Free;
  fFiles.Free;
  inherited;
end;

function TPackageLoader.GetEnumerator: TEnumerator<TFileInfo>;
begin
  Result := fFiles.GetEnumerator;
end;

procedure TPackageLoader.Load;
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
      raise EPackageLoader.CreateFmt(sReadError, [E.Message]);
    else
      raise;
  end;
end;

procedure TPackageLoader.SkipWatermark;
begin
  fStream.Position := Length(GetWatermark);
end;

procedure TPackageLoader.TestChecksums(const FileInfo: TFileInfo);
resourcestring
  // Error message
  sCorruptFile = 'Backup file is corrupt. Checksum error in file %s';
begin
  if TPJMD5.Calculate(FileInfo.Content) <> FileInfo.Checksum then
    raise EPackageLoader.CreateFmt(sCorruptFile, [FileInfo.Name]);
end;

{ TBinaryPackageLoader }

function TBinaryPackageLoader.CreateReader(
  const Stream: TStream): TDataStreamReader;
begin
  Result := TBinaryStreamReader.Create(Stream, Encoding, []);
end;

function TBinaryPackageLoader.GetFileEncoding: TEncoding;
begin
  Result := TEncoding.UTF8;
end;

procedure TBinaryPackageLoader.ReadFileInfo(
  out FileInfo: TPackageLoader.TFileInfo);
begin
  FileInfo.Name := Reader.ReadSizedString16;
  FileInfo.TimeStamp := Reader.ReadInt32;
  FileInfo.Checksum := Reader.ReadBytes(SizeOf(FileInfo.Checksum));
  FileInfo.Content := Reader.ReadSizedRawData32;
end;

procedure TBinaryPackageLoader.ReadHeader(out FileID, FileCount: SmallInt);
begin
  FileID := Reader.ReadInt16;
  FileCount := Reader.ReadInt16;
end;

{ TV4PackageLoader }

class function TV4PackageLoader.GetWatermark: TBytes;
begin
  Result := TPackageInfo.WatermarkBytes(cV4Watermark);
end;

{ TV5PackageLoader }

class function TV5PackageLoader.GetWatermark: TBytes;
begin
  Result := TPackageInfo.WatermarkBytes(cV5Watermark);
end;

{ TPackageLoaderFactory }

class constructor TPackageLoaderFactory.Create;
begin
  fClassList := TArray<TPackageLoaderClass>.Create(
    TV4PackageLoader, TV5PackageLoader
  );
end;

class function TPackageLoaderFactory.Create(
  const Stream: TStream): TPackageLoader;
resourcestring
  // Error messages
  sUnknownFormat = 'Unsupported backup file format';
  sBadFormat = 'Backup file is not in required format';
var
  LoaderCls: TPackageLoaderClass;
begin
  try
    LoaderCls := GetLoaderClass(Stream);
    Stream.Position := 0;
    if not Assigned(LoaderCls) then
      raise EPackageLoader.Create(sUnknownFormat);
    Result := LoaderCls.Create(Stream);
  except
    on E: EStreamError do
      raise EPackageLoader.Create(sBadFormat);
    else
      raise;
  end;
end;

class function TPackageLoaderFactory.GetLoaderClass(
  const Stream: TStream): TPackageLoaderClass;

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
    Assert(
      Length(Buf) >= Length(Test),
      'TPackageLoaderFactory.GetLoaderClass:FirstBytesMatch: '
        + 'Buf shorter than Test'
      );
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
  LoaderCls: TPackageLoaderClass;  // class of each supported loader
  Watermark: TBytes;                  // watermark of each file format
begin
  Result := nil;
  // load sufficient bytes from stream to compare with watermark
  Buffer := LoadFirstFewBytes(cWatermarkLength);
  for LoaderCls in fClassList do
  begin
    Watermark := LoaderCls.GetWatermark;
    if FirstBytesMatch(Buffer, Watermark) then
      Exit(LoaderCls);
  end;
end;

{ TPackageInfo }

class function TPackageInfo.CurrentLoader: TPackageLoaderClass;
begin
  Result := TV5PackageLoader;
end;

class function TPackageInfo.WatermarkBytes(const Watermark: string): TBytes;
begin
  Result := TEncoding.UTF8.GetBytes(Watermark);
end;

end.


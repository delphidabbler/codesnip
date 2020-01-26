{
 * This Source Code Form is subject to the terms of the Mozilla Public License,
 * v. 2.0. If a copy of the MPL was not distributed with this file, You can
 * obtain one at http://mozilla.org/MPL/2.0/
 *
 * Copyright (C) 2013-2014, Peter Johnson (www.delphidabbler.com).
 *
 * $Rev$
 * $Date$
 *
 * Implements classes that can load and save the snippets database in CodeSnip's
 * native file format.
 *
 * NOTE: This native file format was introduced with CodeSnip 5.
}


unit CS.Database.IO.Native;

interface

uses
  SysUtils,
  Classes,
  Generics.Collections,

  CS.ActiveText,
  CS.Database.IO.Types,
  CS.Database.SnippetsTable,
  CS.Database.Types,
  CS.SourceCode.Languages,
  CS.Utils.Dates,
  Compilers.UGlobals,
  UDataStreamIO,
  UExceptions,
  UIStringList;

type
  TBinaryStreamReaderEx = class(TBinaryStreamReader)
  public
    function ReadISO8601Date: TUTCDateTime;
  end;

  TBinaryStreamWriterEx = class(TBinaryStreamWriter)
  public
    procedure WriteISO8601Date(const ADate: TUTCDateTime);
  end;

  TDBSnippetProp = (
    spID, spTitle, spDescription, spSourceCode, spLanguageID, spModified,
    spCreated, spRequiredModules, spRequiredSnippets, spXRefs, spNotes, spKind,
    spCompileResults, spTags, spOrigin, spTestInfo, spStarred
  );

  TDBNativeIOBase = class abstract(TInterfacedObject)
  protected // NOTE: use of strict here causes IDE not to see TSnippetInfo !
    type
      TSnippetInfo = record
        ID: TSnippetID;
        LastModified: TUTCDateTime;
      end;
    type
      TMasterInfo = class(TObject)
      strict private
        var
          fVersion: Integer;
          fLastModified: TUTCDateTime;
          fWatermark: string;
          fSnippets: TList<TSnippetInfo>;
          fTags: ITagSet;
        procedure Init;
      public
        constructor Create;
        destructor Destroy; override;
        procedure Parse(const Reader: TBinaryStreamReaderEx);
        property Watermark: string read fWatermark;
        property Version: Integer read fVersion;
        property LastModified: TUTCDateTime read fLastModified;
        property Snippets: TList<TSnippetInfo> read fSnippets;
        property Tags: ITagSet read fTags;
      end;
  strict private
    procedure ValidateMasterFileInfo(const MI: TMasterInfo);
  strict protected
    const
      EOFByte = $FF;
    var
      fDBPath: string;
    function SnippetFileName(const ID: TSnippetID): string;
    procedure LoadMasterFile(const MI: TMasterInfo);
    function IsSupportedVersion(const Version: Integer): Boolean;
    function MakeFullPath(const RelFileName: string): string;
  public
    constructor Create(const DBPath: string);
  end;

  TDBNativeWriter = class sealed(TDBNativeIOBase, IDatabaseWriter)
  strict private
    var
      fExistingSnippets: TDictionary<TSnippetID,TUTCDateTime>;
      fLastModified: TUTCDateTime;
    procedure ReadMasterFileData;
    procedure WritePropCode(const Writer: TBinaryStreamWriterEx;
      const Code: TDBSnippetProp);
    procedure WriteString32Prop(const Writer: TBinaryStreamWriterEx;
      const PropCode: TDBSnippetProp; const Str: string;
      const Optional: Boolean);
    procedure WriteString16Prop(const Writer: TBinaryStreamWriterEx;
      const PropCode: TDBSnippetProp; const Str: string;
      const Optional: Boolean);
    procedure WriteBooleanProp(const Writer: TBinaryStreamWriterEx;
      const PropCode: TDBSnippetProp; const AValue: Boolean;
      const Optional: Boolean);
    procedure WriteLanguageIDProp(const Writer: TBinaryStreamWriterEx;
      const PropCode: TDBSnippetProp; const LangID: TSourceCodeLanguageID;
      const Optional: Boolean);
    procedure WriteDateProp(const Writer: TBinaryStreamWriterEx;
      const PropCode: TDBSnippetProp; const Date: TUTCDateTime;
      const Optional: Boolean);
    procedure WriteMarkupProp(const Writer: TBinaryStreamWriterEx;
      const PropCode: TDBSnippetProp; ActiveText: IActiveText;
      const Optional: Boolean);
    procedure WriteStringsProp(const Writer: TBinaryStreamWriterEx;
      const PropCode: TDBSnippetProp; Strings: IStringList;
      const Optional: Boolean);
    procedure WriteSnippetIDListProp(const Writer: TBinaryStreamWriterEx;
      const PropCode: TDBSnippetProp; IDs: ISnippetIDList;
      const Optional: Boolean);
    procedure WriteKindIDProp(const Writer: TBinaryStreamWriterEx;
      const PropCode: TDBSnippetProp; const Kind: TSnippetKindID;
      const Optional: Boolean);
    procedure WriteCompileResultsProp(const Writer: TBinaryStreamWriterEx;
      const PropCode: TDBSnippetProp; const Results: TCompileResults;
      const Optional: Boolean);
    procedure WriteTagsProp(const Writer: TBinaryStreamWriterEx;
      const PropCode: TDBSnippetProp; Tags: ITagSet;
      const Optional: Boolean);
    // WriteOriginProp is always "optional": it is never output if origin is
    // local.
    procedure WriteOriginProp(const Writer: TBinaryStreamWriterEx;
      const PropCode: TDBSnippetProp; Origin: ISnippetOrigin);
    procedure WriteTestInfoProp(const Writer: TBinaryStreamWriterEx;
      const PropCode: TDBSnippetProp; const TestInfo: TSnippetTestInfo;
      const Optional: Boolean);
    procedure WriteSnippetFile(const ASnippet: TDBSnippet);
    procedure WriteChangedAndNewSnippetFiles(const ATable: TDBSnippetsTable;
      const ALastModified: TUTCDateTime);
    procedure WriteMasterFile(const ATable: TDBSnippetsTable; ATagSet: ITagSet;
      const ALastModified: TUTCDateTime);
    procedure RemoveUnwantedFiles(const ATable: TDBSnippetsTable);
  public
    constructor Create(const DBPath: string);
    destructor Destroy; override;
    procedure Save(const ATable: TDBSnippetsTable; ATagSet: ITagSet;
      const ALastModified: TUTCDateTime);
  end;

  TDBNativeReader = class sealed(TDBNativeIOBase, IDatabaseLoader)
  strict private
    procedure HandleException(const E: Exception);
    procedure ValidateSnippetFileHeader(const Reader: TBinaryStreamReaderEx);
    procedure LoadSnippet(const AID: TSnippetID;
      const ATable: TDBSnippetsTable);
    procedure LoadSnippetProperties(const ASnippet: TDBSnippet;
      const Reader: TBinaryStreamReaderEx);
    function ReadMarkup(const Reader: TBinaryStreamReaderEx): IActiveText;
    function ReadSnippetIDs(const Reader: TBinaryStreamReaderEx):
      ISnippetIDList;
    function ReadCompileResults(const Reader: TBinaryStreamReaderEx):
      TCompileResults;
    function ReadTags(const Reader: TBinaryStreamReaderEx): ITagSet;
    function ReadOrigin(const Reader: TBinaryStreamReaderEx): ISnippetOrigin;
  public
    constructor Create(const DBPath: string);
    destructor Destroy; override;
    procedure Load(const ATable: TDBSnippetsTable; out ATagSet: ITagSet;
      out ALastModified: TUTCDateTime);
    function DatabaseExists: Boolean;
  end;

  EDBNativeIO = class(ECodeSnip);

implementation

uses
  IOUtils,

  Collections.Base,
  Collections.Sets,

  CS.ActiveText.Helper,
  CS.ActiveText.Renderers.REML,
  CS.Database.SnippetOrigins,
  CS.Database.Snippets,
  CS.Database.Tags,
  Compilers.UCompilers,
  IntfCommon,
  UComparers,
  UConsts,
  UIOUtils,
  UStrUtils,
  UUtils;

const
  WatermarkUID = '0562C0E6F20B4C2F945D1FDEC6393C4C';
  MasterFileWatermark  = 'CSDB-MASTER-' + WatermarkUID;
  SnippetFileWatermark = 'CSDB-SNIPPET-' + WatermarkUID;
  CurrentVersion = 7;
  DBFileNamePrefix = 'csdb';
  MasterFileName = DBFileNamePrefix + '.master.dat';
  SnippetFileNameFmt = DBFileNamePrefix + '.snippet.%s.dat';

{ TDBNativeIOBase }

constructor TDBNativeIOBase.Create(const DBPath: string);
begin
  Assert(DBPath <> '', ClassName + '.Create: DBPath is empty string');
  inherited Create;
  fDBPath := IncludeTrailingPathDelimiter(DBPath);
end;

function TDBNativeIOBase.IsSupportedVersion(const Version: Integer): Boolean;
begin
  Result := Version = CurrentVersion;
end;

procedure TDBNativeIOBase.LoadMasterFile(const MI: TMasterInfo);
var
  Reader: TBinaryStreamReaderEx;
begin
  Reader := TBinaryStreamReaderEx.Create(
    TFileStream.Create(
      MakeFullPath(MasterFileName), fmOpenRead or fmShareDenyWrite
    ),
    TEncoding.UTF8,
    [dsOwnsStream]
  );
  try
    MI.Parse(Reader);
  finally
    Reader.Free;
  end;
  ValidateMasterFileInfo(MI);
end;

function TDBNativeIOBase.MakeFullPath(const RelFileName: string): string;
begin
  Result := fDBPath + RelFileName;
end;

function TDBNativeIOBase.SnippetFileName(const ID: TSnippetID): string;
var
  IDComponent: string;
begin
  // NOTE: all valid characters in a snippet ID are also valid in a file name
  IDComponent := ID.ToString;
  if StrMatchText(
    IDComponent,
    [
      'CON', 'PRN', 'AUX', 'NUL',
      'COM1', 'COM2', 'COM3', 'COM4', 'COM5', 'COM6', 'COM7', 'COM8', 'COM9',
      'LPT1', 'LPT2', 'LPT3', 'LPT4', 'LPT5', 'LPT6', 'LPT7', 'LPT8', 'LPT9'
    ]
  ) then
    // ID is one of reserved Windows file names. Prepending '#' to makes it a
    // valid name because '#' is a valid file name character but is not valid in
    // a snippet ID.
    IDComponent := '#' + IDComponent;
  Result := Format(SnippetFileNameFmt, [IDComponent]);
end;

procedure TDBNativeIOBase.ValidateMasterFileInfo(const MI: TMasterInfo);
resourcestring
  sBadWatermark = 'Invalid watermark at start of master file';
  sBadVersion = 'Invalid or unsupported master file version number %d';
begin
  if MI.Watermark <> MasterFileWatermark then
    raise EDBNativeIO.Create(sBadWatermark);
  if not IsSupportedVersion(MI.Version) then
    raise EDBNativeIO.CreateFmt(sBadVersion, [MI.Version]);
end;

{ TDBNativeIOBase.TMasterInfo }

constructor TDBNativeIOBase.TMasterInfo.Create;
begin
  inherited Create;
  fSnippets := TList<TSnippetInfo>.Create;
  fTags := TTagSet.Create;
  Init;
end;

destructor TDBNativeIOBase.TMasterInfo.Destroy;
begin
  fSnippets.Free;
  inherited;
end;

procedure TDBNativeIOBase.TMasterInfo.Init;
begin
  fVersion := 0;
  fWatermark := '';
  fLastModified := TUTCDateTime.CreateNull;
  fSnippets.Clear;
  fTags.Clear;
end;

procedure TDBNativeIOBase.TMasterInfo.Parse(
  const Reader: TBinaryStreamReaderEx);
var
  SnippetCount: Integer;
  SnippetInfo: TSnippetInfo;
  TagCount: Integer;
  I: Integer;
resourcestring
  sStreamError = 'Master file data is corrupt:' + EOL2 + '%s';
  sConvertError = 'Conversion error while reading master file:' + EOL2 + '%s';
begin
  try
    fWatermark := TEncoding.UTF8.GetString(
      Reader.ReadBytes(Length(MasterFileWatermark))
    );
    fVersion := StrToIntDef(TEncoding.UTF8.GetString(Reader.ReadBytes(4)), 0);
    fLastModified := Reader.ReadISO8601Date;
    fSnippets.Clear;
    SnippetCount := Reader.ReadInt32;
    for I := 1 to SnippetCount do
    begin
      SnippetInfo.ID := TSnippetID.Create(Reader.ReadSizedString16);
      SnippetInfo.LastModified := Reader.ReadISO8601Date;
      fSnippets.Add(SnippetInfo);
    end;
    fTags.Clear;
    TagCount := Reader.ReadInt32;
    for I := 1 to TagCount do
    begin
      fTags.Add(TTag.Create(Reader.ReadSizedString16));
      Reader.ReadInt32;   // skip reserved value: not used
    end;
  except
    on E: Exception do
    begin
      if (E is ESnippetID) or (E is ETag) or (E is EConvertError) then
        raise EDBNativeIO.CreateFmt(sConvertError, [E.Message]);
      if (E is EStreamError) then
        raise EDBNativeIO.CreateFmt(sStreamError, [E.Message]);
      raise;
    end;
  end;
end;

{ TDBNativeWriter }

constructor TDBNativeWriter.Create(const DBPath: string);
begin
  inherited Create(DBPath);
  fExistingSnippets := TDictionary<TSnippetID,TUTCDateTime>.Create(
    TSnippetID.TComparator.Create
  );
end;

destructor TDBNativeWriter.Destroy;
begin
  fExistingSnippets.Free;
  inherited;
end;

procedure TDBNativeWriter.ReadMasterFileData;
var
  MI: TMasterInfo;
  SI: TSnippetInfo;
begin
  MI := TMasterInfo.Create;
  try
    LoadMasterFile(MI);
    fLastModified := MI.LastModified;
    for SI in MI.Snippets do
      fExistingSnippets.Add(SI.ID, SI.LastModified);
  finally
    MI.Free;
  end;
end;

procedure TDBNativeWriter.RemoveUnwantedFiles(const ATable: TDBSnippetsTable);
var
  Snippet: TDBSnippet;
  AllFiles: TStringList;
  WantedFiles: THashSet<string>;
  FileName: string;
begin
  // Build list of files that are supposed to be in directory. These are the
  // master database file and a snippet file for all existing snippets.
  WantedFiles := THashSet<string>.Create(
    TRulesFactory<string>.CreateFromComparator(TTextComparator.Create)
  );
  try
    WantedFiles.Add(MasterFileName);
    for Snippet in ATable do
    begin
      WantedFiles.Add(SnippetFileName(Snippet.GetID))
    end;
    // Build a list of files that are actually in the database
    AllFiles := TStringList.Create;
    try
      ListFiles(
        ExcludeTrailingPathDelimiter(fDBPath), '*.*', AllFiles, False, True
      );
      // Remove files in directory that are not required. These include any
      // files for deleted snippets, any legacy files and anything else placed
      // there by a third party.
      for FileName in AllFiles do
      begin
        if not WantedFiles.Contains(FileName) then
          TFile.Delete(MakeFullPath(FileName));
      end;
    finally
      AllFiles.Free;
    end;
  finally
    WantedFiles.Free;
  end;
end;

procedure TDBNativeWriter.Save(const ATable: TDBSnippetsTable; ATagSet: ITagSet;
  const ALastModified: TUTCDateTime);
begin
  fExistingSnippets.Clear;
  if FileExists(MakeFullPath(MasterFileName)) then
    ReadMasterFileData
  else
    fLastModified := TUTCDateTime.CreateNull;
  WriteChangedAndNewSnippetFiles(ATable, ALastModified);
  WriteMasterFile(ATable, ATagSet, ALastModified);
  RemoveUnwantedFiles(ATable);
end;

procedure TDBNativeWriter.WriteBooleanProp(const Writer: TBinaryStreamWriterEx;
  const PropCode: TDBSnippetProp; const AValue, Optional: Boolean);
begin
  if Optional and not AValue then
    Exit;
  WritePropCode(Writer, PropCode);
  Writer.WriteByte(Ord(AValue));
end;

procedure TDBNativeWriter.WriteChangedAndNewSnippetFiles(
  const ATable: TDBSnippetsTable; const ALastModified: TUTCDateTime);
var
  Snippet: TDBSnippet;
begin
  for Snippet in ATable do
  begin
    if not fExistingSnippets.ContainsKey(Snippet.GetID) or
      (fExistingSnippets[Snippet.GetID] < Snippet.GetModified) then
      WriteSnippetFile(Snippet);
  end;
end;

procedure TDBNativeWriter.WriteCompileResultsProp(
  const Writer: TBinaryStreamWriterEx; const PropCode: TDBSnippetProp;
  const Results: TCompileResults; const Optional: Boolean);
var
  Success, Failure: IStringList;
  CompilerID: TCompilerID;
  CompRes: TCompileResult;
  Compilers: ICompilers;
begin
  Compilers := TCompilersFactory.CreateCompilers;
  Success := TIStringList.Create;
  Failure := TIStringList.Create;
  for CompilerID := Low(Results) to High(Results) do
  begin
    CompRes := Results[CompilerID];
    if CompRes in [crSuccess, crWarning] then
      Success.Add(Compilers[CompilerID].GetIDString);
    if CompRes = crError then
      Failure.Add(Compilers[CompilerID].GetIDString);
  end;
  if Optional and Success.IsEmpty and Failure.IsEmpty then
    Exit;
  WritePropCode(Writer, PropCode);
  Writer.WriteSizedString16List(Success);
  Writer.WriteSizedString16List(Failure);
end;

procedure TDBNativeWriter.WriteDateProp(const Writer: TBinaryStreamWriterEx;
  const PropCode: TDBSnippetProp; const Date: TUTCDateTime;
  const Optional: Boolean);
begin
  if Optional and Date.IsNull then
    Exit;
  WritePropCode(Writer, PropCode);
  Writer.WriteISO8601Date(Date);
end;

procedure TDBNativeWriter.WriteKindIDProp(const Writer: TBinaryStreamWriterEx;
  const PropCode: TDBSnippetProp; const Kind: TSnippetKindID;
  const Optional: Boolean);
begin
  if Optional and (Kind = skFreeform) then
    Exit;
  WritePropCode(Writer, PropCode);
  Writer.WriteByte(Ord(Kind));
end;

procedure TDBNativeWriter.WriteLanguageIDProp(
  const Writer: TBinaryStreamWriterEx; const PropCode: TDBSnippetProp;
  const LangID: TSourceCodeLanguageID; const Optional: Boolean);
begin
  if Optional and LangID.IsDefault then
    Exit;
  WritePropCode(Writer, PropCode);
  Writer.WriteSizedString16(LangID.ToString);
end;

procedure TDBNativeWriter.WriteMarkupProp(const Writer: TBinaryStreamWriterEx;
  const PropCode: TDBSnippetProp; ActiveText: IActiveText;
  const Optional: Boolean);
begin
  if Optional and ActiveText.IsEmpty then
    Exit;
  WritePropCode(Writer, PropCode);
  Writer.WriteSizedString32(
    TActiveTextREMLRenderer.Render(ActiveText, EmptyStr)
  );
end;

procedure TDBNativeWriter.WriteMasterFile(const ATable: TDBSnippetsTable;
  ATagSet: ITagSet; const ALastModified: TUTCDateTime);
var
  Writer: TBinaryStreamWriterEx;
  Snippet: TDBSnippet;
  Tag: TTag;
begin
  Writer := TBinaryStreamWriterEx.Create(
    TFileStream.Create(MakeFullPath(MasterFileName), fmCreate),
    TEncoding.UTF8,
    [dsOwnsStream]
  );
  try
    Writer.WriteBytes(TEncoding.ASCII.GetBytes(MasterFileWatermark));
    Writer.WriteBytes(
      TEncoding.ASCII.GetBytes(Format('%.4d', [CurrentVersion]))
    );
    Writer.WriteISO8601Date(ALastModified);
    Writer.WriteInt32(ATable.Size);
    for Snippet in ATable do
    begin
      Writer.WriteSizedString16(Snippet.GetID.ToString);
      Writer.WriteISO8601Date(Snippet.GetModified);
    end;
    Writer.WriteInt32(ATagSet.Count);
    for Tag in ATagSet do
    begin
      Writer.WriteSizedString16(Tag.ToString);
      Writer.WriteInt32(0);   // reserved value
    end;
  finally
    Writer.Free;
  end;
end;

procedure TDBNativeWriter.WriteOriginProp(const Writer: TBinaryStreamWriterEx;
  const PropCode: TDBSnippetProp; Origin: ISnippetOrigin);
var
  OriginSource: TSnippetOriginSource;
begin
  if Origin.Source = sosLocal then
    Exit;
  WritePropCode(Writer, PropCode);
  OriginSource := Origin.Source;
  Writer.WriteBuffer(OriginSource, SizeOf(OriginSource));
  Writer.WriteSizedString16(Origin.OriginalID);
  Writer.WriteISO8601Date(Origin.Modified);
end;

procedure TDBNativeWriter.WritePropCode(const Writer: TBinaryStreamWriterEx;
  const Code: TDBSnippetProp);
begin
  Writer.WriteByte(Ord(Code));
end;

procedure TDBNativeWriter.WriteSnippetFile(const ASnippet: TDBSnippet);
var
  Writer: TBinaryStreamWriterEx;
begin
  Writer := TBinaryStreamWriterEx.Create(
    TFileStream.Create(MakeFullPath(SnippetFileName(ASnippet.GetID)), fmCreate),
    TEncoding.UTF8,
    [dsOwnsStream]
  );
  try
    Writer.WriteBytes(TEncoding.ASCII.GetBytes(SnippetFileWatermark));
    Writer.WriteBytes(
      TEncoding.ASCII.GetBytes(Format('%.4d', [CurrentVersion]))
    );
    WriteString16Prop(Writer, spID, ASnippet.GetID.ToString, False);
    WriteString16Prop(Writer, spTitle, ASnippet.GetTitle, False);
    WriteMarkupProp(Writer, spDescription, ASnippet.GetDescription, True);
    WriteString32Prop(Writer, spSourceCode, ASnippet.GetSourceCode, False);
    WriteLanguageIDProp(Writer, spLanguageID, ASnippet.GetLanguageID, True);
    WriteDateProp(Writer, spModified, ASnippet.GetModified, False);
    WriteDateProp(Writer, spCreated, ASnippet.GetCreated, False);
    WriteStringsProp(
      Writer, spRequiredModules, ASnippet.GetRequiredModules, True
    );
    WriteSnippetIDListProp(
      Writer, spRequiredSnippets, ASnippet.GetRequiredSnippets, True
    );
    WriteSnippetIDListProp(Writer, spXRefs, ASnippet.GetXRefs, True);
    WriteMarkupProp(Writer, spNotes, ASnippet.GetNotes, True);
    WriteKindIDProp(Writer, spKind, ASnippet.GetKindID, True);
    WriteCompileResultsProp(
      Writer, spCompileResults, ASnippet.GetCompileResults, True
    );
    WriteTagsProp(Writer, spTags, ASnippet.GetTags, True);
    WriteOriginProp(Writer, spOrigin, ASnippet.GetOrigin);
    WriteTestInfoProp(Writer, spTestInfo, ASnippet.GetTestInfo, True);
    WriteBooleanProp(Writer, spStarred, ASnippet.GetStarred, True);

    // Write "EOF" marker as last byte in file
    Writer.WriteByte(EOFByte);
  finally
    Writer.Free;
  end;
end;

procedure TDBNativeWriter.WriteSnippetIDListProp(
  const Writer: TBinaryStreamWriterEx; const PropCode: TDBSnippetProp;
  IDs: ISnippetIDList; const Optional: Boolean);
var
  ID: TSnippetID;
  IDStrs: IStringList;
begin
  IDStrs := TIStringList.Create;
  for ID in IDs do
    IDStrs.Add(ID.ToString);
  WriteStringsProp(Writer, PropCode, IDStrs, Optional);
end;

procedure TDBNativeWriter.WriteString16Prop(const Writer: TBinaryStreamWriterEx;
  const PropCode: TDBSnippetProp; const Str: string; const Optional: Boolean);
begin
  if Optional and (Str = EmptyStr) then
    Exit;
  WritePropCode(Writer, PropCode);
  Writer.WriteSizedString16(Str);
end;

procedure TDBNativeWriter.WriteString32Prop(const Writer: TBinaryStreamWriterEx;
  const PropCode: TDBSnippetProp; const Str: string; const Optional: Boolean);
begin
  if Optional and (Str = EmptyStr) then
    Exit;
  WritePropCode(Writer, PropCode);
  Writer.WriteSizedString32(Str);
end;

procedure TDBNativeWriter.WriteStringsProp(const Writer: TBinaryStreamWriterEx;
  const PropCode: TDBSnippetProp; Strings: IStringList;
  const Optional: Boolean);
begin
  if Optional and Strings.IsEmpty then
    Exit;
  WritePropCode(Writer, PropCode);
  Writer.WriteSizedString16List(Strings);
end;

procedure TDBNativeWriter.WriteTagsProp(const Writer: TBinaryStreamWriterEx;
  const PropCode: TDBSnippetProp; Tags: ITagSet; const Optional: Boolean);
var
  Tag: TTag;
  TagStrs: IStringList;
begin
  TagStrs := TIStringList.Create;
  for Tag in Tags do
    TagStrs.Add(Tag.ToString);
  WriteStringsProp(Writer, PropCode, TagStrs, Optional);
end;

procedure TDBNativeWriter.WriteTestInfoProp(const Writer: TBinaryStreamWriterEx;
  const PropCode: TDBSnippetProp; const TestInfo: TSnippetTestInfo;
  const Optional: Boolean);
begin
  if Optional and (TestInfo = stiNone) then
    Exit;
  WritePropCode(Writer, PropCode);
  Writer.WriteByte(Ord(TestInfo));
end;

{ TDBNativeReader }

constructor TDBNativeReader.Create(const DBPath: string);
begin
  inherited Create(DBPath);
end;

function TDBNativeReader.DatabaseExists: Boolean;
const
  WatermarkSize = Length(MasterFileWatermark);
  VersionSize = 4;
  HeaderSize = WatermarkSize + VersionSize;
var
  WaterMark: string;
  Version: Integer;
  Reader: TBinaryStreamReaderEx;
begin
  Result := False;
  try
    if not TFile.Exists(MakeFullPath(MasterFileName), False) then
      Exit;
    Reader := TBinaryStreamReaderEx.Create(
      TFileStream.Create(
        MakeFullPath(MasterFileName), fmOpenRead or fmShareDenyWrite
      ),
      TEncoding.UTF8,
      [dsOwnsStream]
    );
    try
      if Reader.Size < HeaderSize then
        Exit;
      { TODO: Use extract method on code to use to read master file watermark
              and version shared with TMasterInfo.Parse (use public class method
              in TMasterInfo?}
      Watermark := TEncoding.UTF8.GetString(
        Reader.ReadBytes(Length(MasterFileWatermark))
      );
      if Watermark <> MasterFileWatermark then
        Exit;
      Version := StrToIntDef(
        TEncoding.UTF8.GetString(Reader.ReadBytes(VersionSize)), 0
      );
      if not IsSupportedVersion(Version) then
        Exit;
      Result := True;
    finally
      Reader.Free;
    end;
  except
    // Swallow any exception. Result will be False
  end;
end;

destructor TDBNativeReader.Destroy;
begin

  inherited;
end;

procedure TDBNativeReader.HandleException(const E: Exception);
resourcestring
  sStreamError = 'Snippet file is corrupt:' + EOL2 + '%s';
  sConvertError = 'Conversion error reading snippet file:' + EOL2 + '%s';
begin
  if (E is EStreamError) then
    raise EDBNativeIO.CreateFmt(sStreamError, [E.Message]);
  if (E is ESnippetID) or (E is ETag) or (E is EConvertError) then
    raise EDBNativeIO.CreateFmt(sConvertError, [E.Message]);
  raise E;
end;

procedure TDBNativeReader.Load(const ATable: TDBSnippetsTable;
  out ATagSet: ITagSet; out ALastModified: TUTCDateTime);
var
  MI: TMasterInfo;
  SI: TSnippetInfo;
begin
  ATable.Clear;
  try
    MI := TMasterInfo.Create;
    try
      LoadMasterFile(MI);
      ALastModified := MI.LastModified;
      ATagSet := (MI.Tags as IClonable).Clone as ITagSet;
      for SI in MI.Snippets do
        LoadSnippet(SI.ID, ATable);
    finally
      MI.Free;
    end;
  except
    ATable.Clear;
    raise;
  end;
end;

procedure TDBNativeReader.LoadSnippet(const AID: TSnippetID;
  const ATable: TDBSnippetsTable);
var
  Reader: TBinaryStreamReaderEx;
  Snippet: TDBSnippet;
begin
  Reader := TBinaryStreamReaderEx.Create(
    TFileStream.Create(
      MakeFullPath(SnippetFileName(AID)), fmOpenRead or fmShareDenyWrite
    ),
    TEncoding.UTF8,
    [dsOwnsStream]
  );
  Snippet := TDBSnippet.Create(AID);
  try
    try
      ValidateSnippetFileHeader(Reader);
      LoadSnippetProperties(Snippet, Reader);
      ATable.Add(Snippet);
    finally
      Reader.Free;
    end;
  except
    Snippet.Free;
    if ExceptObject is Exception then
      HandleException(ExceptObject as Exception);
  end;
end;

procedure TDBNativeReader.LoadSnippetProperties(const ASnippet: TDBSnippet;
  const Reader: TBinaryStreamReaderEx);

  function NextPropCode(out PropCode: TDBSnippetProp): Boolean;
  var
    B: Byte;
  begin
    B := Reader.ReadByte;
    if B = EOFByte then
      Exit(False);
    PropCode := TDBSnippetProp(B);
    Result := True;
  end;

var
  PropCode: TDBSnippetProp;
begin
  while NextPropCode(PropCode) do
  begin
    case PropCode of
      spID:
        // skip over ID - we already have it
        // TODO: Check ID in snippet file is same as that of given snippet
        Reader.ReadSizedString16;
      spTitle:
        ASnippet.SetTitle(Reader.ReadSizedString16);
      spDescription:
        ASnippet.SetDescription(ReadMarkup(Reader));
      spSourceCode:
        ASnippet.SetSourceCode(Reader.ReadSizedString32);
      spLanguageID:
        ASnippet.SetLanguageID(
          TSourceCodeLanguageID.Create(Reader.ReadSizedString16)
        );
      spModified:
        ASnippet.SetModified(Reader.ReadISO8601Date);
      spCreated:
        ASnippet.SetCreated(Reader.ReadISO8601Date);
      spRequiredModules:
        ASnippet.SetRequiredModules(Reader.ReadSizedString16List);
      spRequiredSnippets:
        ASnippet.SetRequiredSnippets(ReadSnippetIDs(Reader));
      spXRefs:
        ASnippet.SetXRefs(ReadSnippetIDs(Reader));
      spNotes:
        ASnippet.SetNotes(ReadMarkup(Reader));
      spKind:
        ASnippet.SetKindID(TSnippetKindID(Reader.ReadByte));
      spCompileResults:
        ASnippet.SetCompileResults(ReadCompileResults(Reader));
      spTags:
        ASnippet.SetTags(ReadTags(Reader));
      spOrigin:
        ASnippet.SetOrigin(ReadOrigin(Reader));
      spTestInfo:
        ASnippet.SetTestInfo(TSnippetTestInfo(Reader.ReadByte));
      spStarred:
        ASnippet.SetStarred(Boolean(Reader.ReadByte));
      else
        ; // ignore any property codes we don't recognise
    end;
  end;
end;

function TDBNativeReader.ReadCompileResults(
  const Reader: TBinaryStreamReaderEx): TCompileResults;
var
  Succeeds, Fails: IStringList;
  CompilerID: TCompilerID;
  Compilers: ICompilers;
  IDStr: string;
begin
  Succeeds := Reader.ReadSizedString16List;
  Succeeds.CaseSensitive := False;
  Fails := Reader.ReadSizedString16List;
  Fails.CaseSensitive := False;
  Compilers := TCompilersFactory.CreateCompilers;
  for CompilerID := Low(Result) to High(Result) do
  begin
    IDStr := Compilers[CompilerID].GetIDString;
    if Succeeds.Contains(IDStr) then
      Result[CompilerID] := crSuccess
    else if Fails.Contains(IDStr) then
      Result[CompilerID] := crError
    else
      Result[CompilerID] := crQuery;
  end;
end;

function TDBNativeReader.ReadMarkup(const Reader: TBinaryStreamReaderEx):
  IActiveText;
begin
  try
    Result := TActiveTextHelper.ParseREML(Reader.ReadSizedString32);
  except
    // error: provide an empty property value
    Result := TActiveTextFactory.CreateActiveText;
  end;
end;

function TDBNativeReader.ReadOrigin(const Reader: TBinaryStreamReaderEx):
  ISnippetOrigin;
var
  OriginSource: TSnippetOriginSource;
  OriginalID: string;
  Modified: TUTCDateTime;
resourcestring
  sError = 'Invalid origin record found for snippet with local origin';
begin
  // If this property is present snippet is linked to space: the property is
  // never present in file if snippet is not linked. Therefore return value is
  // never a local origin instance.
  Reader.ReadBuffer(OriginSource, SizeOf(OriginSource));
  if OriginSource = sosLocal then
    raise EConvertError.Create(sError);
  OriginalID := Reader.ReadSizedString16;
  Modified := Reader.ReadISO8601Date;
  Result := TRemoteSnippetOrigin.Create(OriginSource, OriginalID, Modified);
end;

function TDBNativeReader.ReadSnippetIDs(const Reader: TBinaryStreamReaderEx):
  ISnippetIDList;
var
  IDStr: string;
  IDStrs: IStringList;
begin
  IDStrs := Reader.ReadSizedString16List;
  Result := TSnippetIDList.Create;
  for IDStr in IDStrs do
    Result.Add(TSnippetID.Create(IDStr));
end;

function TDBNativeReader.ReadTags(const Reader: TBinaryStreamReaderEx):
  ITagSet;
var
  TagStr: string;
  TagStrs: IStringList;
begin
  TagStrs := Reader.ReadSizedString16List;
  Result := TTagSet.Create;
  for TagStr in TagStrs do
    Result.Add(TTag.Create(TagStr));
end;

procedure TDBNativeReader.ValidateSnippetFileHeader(
  const Reader: TBinaryStreamReaderEx);
var
  Watermark: string;
  Version: Integer;
resourcestring
  sBadWatermark = 'Invalid watermark at start of snippet file';
  sBadVersion = 'Invalid or unsupported snippet file version';
begin
  Watermark := TEncoding.UTF8.GetString(
    Reader.ReadBytes(Length(SnippetFileWatermark))
  );
  if Watermark <> SnippetFileWatermark then
    raise EDBNativeIO.Create(sBadWatermark);
  Version := StrToIntDef(TEncoding.UTF8.GetString(Reader.ReadBytes(4)), 0);
  if not IsSupportedVersion(Version) then
    raise EDBNativeIO.CreateFmt(sBadVersion, [Version]);
end;

{ TBinaryStreamReaderEx }

function TBinaryStreamReaderEx.ReadISO8601Date: TUTCDateTime;
begin
  Result := TUTCDateTime.CreateFromISO8601String(ReadSizedString16);
end;

{ TBinaryStreamWriterEx }

procedure TBinaryStreamWriterEx.WriteISO8601Date(const ADate: TUTCDateTime);
begin
  WriteSizedString16(ADate.ToISO8601String);
end;

end.


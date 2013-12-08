{
 * This Source Code Form is subject to the terms of the Mozilla Public License,
 * v. 2.0. If a copy of the MPL was not distributed with this file, You can
 * obtain one at http://mozilla.org/MPL/2.0/
 *
 * Copyright (C) 2013, Peter Johnson (www.delphidabbler.com).
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

  CS.Database.Core.SnippetsTable,
  CS.Database.Types,
  CS.Markup,
  CS.SourceCode.Languages,
  CS.Utils.Dates,

  UDataStreamIO,
  UExceptions,
  UIStringList;

type
  TDBNativeIOBase = class abstract(TObject)
  protected // NOTE: use of strict here causes IDE not to see TSnippetInfo !
    type
      TSnippetInfo = record
        ID: TDBSnippetID;
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
        procedure Init;
      public
        constructor Create;
        destructor Destroy; override;
        procedure Parse(const Reader: TBinaryStreamReader);
        property Watermark: string read fWatermark;
        property Version: Integer read fVersion;
        property LastModified: TUTCDateTime read fLastModified;
        property Snippets: TList<TSnippetInfo> read fSnippets;
      end;
  strict private
    procedure ValidateMasterFileInfo(const MI: TMasterInfo);
  strict protected
    const
      EOFByte = $FF;
    var
      fDBPath: string;
    function SnippetFileName(const ID: TDBSnippetID): string;
    procedure LoadMasterFile(const MI: TMasterInfo);
    function IsSupportedVersion(const Version: Integer): Boolean;
  public
    constructor Create(const DBPath: string);
  end;

  TDBNativeWriter = class sealed(TDBNativeIOBase)
  strict private
    var
      fExistingSnippets: TDictionary<TDBSnippetID,TUTCDateTime>;
      fLastModified: TUTCDateTime;
    procedure ReadMasterFileData;
    procedure WritePropCode(const Writer: TBinaryStreamWriter;
      const Code: TDBSnippetProp);
    procedure WriteString32Prop(const Writer: TBinaryStreamWriter;
      const PropCode: TDBSnippetProp; const Str: string;
      const Optional: Boolean);
    procedure WriteString16Prop(const Writer: TBinaryStreamWriter;
      const PropCode: TDBSnippetProp; const Str: string;
      const Optional: Boolean);
    procedure WriteBooleanProp(const Writer: TBinaryStreamWriter;
      const PropCode: TDBSnippetProp; const AValue: Boolean;
      const Optional: Boolean);
    procedure WriteLanguageIDProp(const Writer: TBinaryStreamWriter;
      const PropCode: TDBSnippetProp; const LangID: TSourceCodeLanguageID;
      const Optional: Boolean);
    procedure WriteDateProp(const Writer: TBinaryStreamWriter;
      const PropCode: TDBSnippetProp; const Date: TUTCDateTime;
      const Optional: Boolean);
    procedure WriteMarkupProp(const Writer: TBinaryStreamWriter;
      const PropCode: TDBSnippetProp; const Markup: TMarkup;
      const Optional: Boolean);
    procedure WriteStringsProp(const Writer: TBinaryStreamWriter;
      const PropCode: TDBSnippetProp; Strings: IStringList;
      const Optional: Boolean);
    procedure WriteSnippetIDListProp(const Writer: TBinaryStreamWriter;
      const PropCode: TDBSnippetProp; IDs: IDBSnippetIDList;
      const Optional: Boolean);
    procedure WriteKindProp(const Writer: TBinaryStreamWriter;
      const PropCode: TDBSnippetProp; const Kind: TDBSnippetKind;
      const Optional: Boolean);
    procedure WriteCompileResultsProp(const Writer: TBinaryStreamWriter;
      const PropCode: TDBSnippetProp; const Results: TDBCompileResults;
      const Optional: Boolean);
    procedure WriteTagsProp(const Writer: TBinaryStreamWriter;
      const PropCode: TDBSnippetProp; Tags: IDBTagList;
      const Optional: Boolean);
    // WriteLinkInfoProp is always "optional": it is never output if not linked.
    procedure WriteLinkInfoProp(const Writer: TBinaryStreamWriter;
      const PropCode: TDBSnippetProp; LinkInfo: ISnippetLinkInfo);
    procedure WriteTestInfoProp(const Writer: TBinaryStreamWriter;
      const PropCode: TDBSnippetProp; const TestInfo: TSnippetTestInfo;
      const Optional: Boolean);
    procedure WriteSnippetFile(const ASnippet: TDBSnippet);
    procedure WriteChangedAndNewSnippetFiles(const ATable: TDBSnippetsTable;
      const ALastModified: TUTCDateTime);
    procedure WriteMasterFile(const ATable: TDBSnippetsTable;
      const ALastModified: TUTCDateTime);
    procedure RemoveDeletedSnippetFiles(const ATable: TDBSnippetsTable);
  public
    constructor Create(const DBPath: string);
    destructor Destroy; override;
    procedure Save(const ATable: TDBSnippetsTable;
      const ALastModified: TUTCDateTime);
  end;

  TDBNativeReader = class sealed(TDBNativeIOBase)
  strict private
    procedure HandleException(const E: Exception);
    procedure ValidateSnippetFileHeader(const Reader: TBinaryStreamReader);
    procedure LoadSnippet(const AID: TDBSnippetID;
      const ATable: TDBSnippetsTable);
    procedure LoadSnippetProperties(const ASnippet: TDBSnippet;
      const Reader: TBinaryStreamReader);
    function ReadMarkup(const Reader: TBinaryStreamReader): TMarkup;
    function ReadStrings(const Reader: TBinaryStreamReader): IStringList;
    function ReadSnippetIDs(const Reader: TBinaryStreamReader):
      IDBSnippetIDList;
    function ReadCompileResults(const Reader: TBinaryStreamReader):
      TDBCompileResults;
    function ReadTags(const Reader: TBinaryStreamReader): IDBTagList;
    function ReadLinkInfo(const Reader: TBinaryStreamReader): ISnippetLinkInfo;
  public
    constructor Create(const DBPath: string);
    destructor Destroy; override;
    procedure Load(const ATable: TDBSnippetsTable;
      out ALastModified: TUTCDateTime);
  end;

  EDBNativeIO = class(ECodeSnip);

implementation

uses
  IOUtils,

  CS.Database.SnippetLinks,
  CS.Database.Snippets,
  CS.Database.Tags,
  Compilers.UGlobals,
  UConsts,
  UIOUtils,
  UStrUtils,
  UUtils;

const
  MasterFileWatermark = 'CSUSERDBMASTER';
  SnippetFileWatermark = 'CSUSERDBSNIPPET';
  CurrentVersion = 7;
  MasterFileName = 'csdb.master.dat';
  SnippetFileNameFmt = 'csdb.snippet.%s.dat';

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
  Reader: TBinaryStreamReader;
begin
  Reader := TBinaryStreamReader.Create(
    TFileStream.Create(MasterFileName, fmOpenRead or fmShareDenyWrite),
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

function TDBNativeIOBase.SnippetFileName(const ID: TDBSnippetID): string;
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
end;

procedure TDBNativeIOBase.TMasterInfo.Parse(const Reader: TBinaryStreamReader);
var
  SnippetCount: Integer;
  I: Integer;
  SnippetInfo: TSnippetInfo;
resourcestring
  sStreamError = 'Master file data is corrupt:' + EOL2 + '%s';
  sConvertError = 'Conversion error while reading master file:' + EOL2 + '%s';
begin
  try
    fWatermark := TEncoding.UTF8.GetString(
      Reader.ReadBytes(Length(MasterFileWatermark))
    );
    fVersion := StrToIntDef(TEncoding.UTF8.GetString(Reader.ReadBytes(4)), 0);
    fLastModified := TUTCDateTime.CreateFromISO8601String(
      Reader.ReadSizedString16
    );
    SnippetCount := Reader.ReadInt32;
    fSnippets.Clear;
    for I := 1 to SnippetCount do
    begin
      SnippetInfo.ID := TDBSnippetID.Create(Reader.ReadSizedString16);
      SnippetInfo.LastModified := TUTCDateTime.CreateFromISO8601String(
        Reader.ReadSizedString16
      );
      fSnippets.Add(SnippetInfo);
    end;
  except
    on E: Exception do
    begin
      if (E is EDBSnippetID) or (E is EConvertError) then
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
  fExistingSnippets := TDictionary<TDBSnippetID,TUTCDateTime>.Create(
    TDBSnippetID.TEqualityComparer.Create
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

procedure TDBNativeWriter.RemoveDeletedSnippetFiles(
  const ATable: TDBSnippetsTable);
var
  SnippetID: TDBSnippetID;
begin
  for SnippetID in fExistingSnippets.Keys do
  begin
    if not ATable.Contains(SnippetID) then
       TFile.Delete(fDBPath + SnippetFileName(SnippetID));
  end;
end;

procedure TDBNativeWriter.Save(const ATable: TDBSnippetsTable;
  const ALastModified: TUTCDateTime);
begin
  fExistingSnippets.Clear;
  if FileExists(fDBPath + MasterFileName) then
    ReadMasterFileData
  else
    fLastModified := TUTCDateTime.CreateNull;
  WriteChangedAndNewSnippetFiles(ATable, ALastModified);
  WriteMasterFile(ATable, ALastModified);
  RemoveDeletedSnippetFiles(ATable);
end;

procedure TDBNativeWriter.WriteBooleanProp(const Writer: TBinaryStreamWriter;
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
  const Writer: TBinaryStreamWriter; const PropCode: TDBSnippetProp;
  const Results: TDBCompileResults; const Optional: Boolean);

  procedure WriteSet(const S: TCompilerIDs);
  var
    SetSize: Integer;
    Elem: TCompilerID;
  begin
    SetSize := 0;
    for Elem in S do
      Inc(SetSize);
    Writer.WriteByte(SetSize);
    for Elem in S do
      Writer.WriteByte(Ord(Elem));
  end;

begin
  if Optional and (Results.IsNull) then
    Exit;
  WritePropCode(Writer, PropCode);
  WriteSet(Results.Succeeds);
  WriteSet(Results.Fails);
end;

procedure TDBNativeWriter.WriteDateProp(const Writer: TBinaryStreamWriter;
  const PropCode: TDBSnippetProp; const Date: TUTCDateTime;
  const Optional: Boolean);
begin
  if Optional and Date.IsNull then
    Exit;
  WritePropCode(Writer, PropCode);
  Writer.WriteSizedString16(Date.ToISO8601String);
end;

procedure TDBNativeWriter.WriteKindProp(const Writer: TBinaryStreamWriter;
  const PropCode: TDBSnippetProp; const Kind: TDBSnippetKind;
  const Optional: Boolean);
begin
  if Optional and (Kind = skFreeform) then
    Exit;
  WritePropCode(Writer, PropCode);
  Writer.WriteByte(Ord(Kind));
end;

procedure TDBNativeWriter.WriteLanguageIDProp(const Writer: TBinaryStreamWriter;
  const PropCode: TDBSnippetProp; const LangID: TSourceCodeLanguageID;
  const Optional: Boolean);
begin
  if Optional and LangID.IsDefault then
    Exit;
  WritePropCode(Writer, PropCode);
  Writer.WriteSizedString16(LangID.ToString);
end;

procedure TDBNativeWriter.WriteLinkInfoProp(const Writer: TBinaryStreamWriter;
  const PropCode: TDBSnippetProp; LinkInfo: ISnippetLinkInfo);
var
  SynchSpaceID: TGUID;
begin
  if not LinkInfo.IsLinked then
    Exit;
  WritePropCode(Writer, PropCode);
  SynchSpaceID := LinkInfo.SynchSpaceID;
  Writer.WriteBuffer(SynchSpaceID, SizeOf(SynchSpaceID));
  Writer.WriteSizedString16(LinkInfo.LinkedSnippetID.ToString);
end;

procedure TDBNativeWriter.WriteMarkupProp(const Writer: TBinaryStreamWriter;
  const PropCode: TDBSnippetProp; const Markup: TMarkup;
  const Optional: Boolean);
begin
  if Optional and Markup.IsEmpty then
    Exit;
  WritePropCode(Writer, PropCode);
  Writer.WriteByte(Ord(Markup.Kind));
  Writer.WriteSizedString32(Markup.Source);
end;

procedure TDBNativeWriter.WriteMasterFile(const ATable: TDBSnippetsTable;
  const ALastModified: TUTCDateTime);
var
  Writer: TBinaryStreamWriter;
  Snippet: TDBSnippet;
begin
  Writer := TBinaryStreamWriter.Create(
    TFileStream.Create(MasterFileName, fmCreate),
    TEncoding.UTF8,
    [dsOwnsStream]
  );
  try
    Writer.WriteBytes(TEncoding.ASCII.GetBytes(MasterFileWatermark));
    Writer.WriteBytes(
      TEncoding.ASCII.GetBytes(Format('%.4d', [CurrentVersion]))
    );
    Writer.WriteSizedString16(ALastModified.ToISO8601String);
    Writer.WriteInt32(ATable.Size);
    for Snippet in ATable do
    begin
      Writer.WriteSizedString16(Snippet.GetID.ToString);
      Writer.WriteSizedString16(Snippet.GetModified.ToISO8601String);
    end;
  finally
    Writer.Free;
  end;
end;

procedure TDBNativeWriter.WritePropCode(const Writer: TBinaryStreamWriter;
  const Code: TDBSnippetProp);
begin
  Writer.WriteByte(Ord(Code));
end;

procedure TDBNativeWriter.WriteSnippetFile(const ASnippet: TDBSnippet);
var
  Writer: TBinaryStreamWriter;
begin
  Writer := TBinaryStreamWriter.Create(
    TFileStream.Create(SnippetFileName(ASnippet.GetID), fmCreate),
    TEncoding.UTF8,
    [dsOwnsStream]
  );
  try
    Writer.WriteBytes(TEncoding.ASCII.GetBytes(SnippetFileWatermark));
    Writer.WriteBytes(
      TEncoding.ASCII.GetBytes(Format('%.4d', [CurrentVersion]))
    );
    WriteString16Prop(Writer, spID, ASnippet.GetID.ToString, False);
    WriteString16Prop(Writer, spTitle, ASnippet.GetTitle, True);
    WriteMarkupProp(Writer, spDescription, ASnippet.GetDescription, True);
    WriteString32Prop(Writer, spSourceCode, ASnippet.GetSourceCode, True);
    WriteLanguageIDProp(Writer, spLanguageID, ASnippet.GetLanguageID, True);
    WriteDateProp(Writer, spModified, ASnippet.GetModified, True);
    WriteDateProp(Writer, spCreated, ASnippet.GetCreated, True);
    WriteStringsProp(
      Writer, spRequiredModules, ASnippet.GetRequiredModules, True
    );
    WriteSnippetIDListProp(
      Writer, spRequiredSnippets, ASnippet.GetRequiredSnippets, True
    );
    WriteSnippetIDListProp(Writer, spXRefs, ASnippet.GetXRefs, True);
    WriteMarkupProp(Writer, spNotes, ASnippet.GetNotes, True);
    WriteKindProp(Writer, spKind, ASnippet.GetKind, True);
    WriteCompileResultsProp(
      Writer, spCompileResults, ASnippet.GetCompileResults, True
    );
    WriteTagsProp(Writer, spTags, ASnippet.GetTags, True);
    WriteLinkInfoProp(Writer, spLinkInfo, ASnippet.GetLinkInfo);
    WriteTestInfoProp(Writer, spTestInfo, ASnippet.GetTestInfo, True);
    WriteBooleanProp(Writer, spStarred, ASnippet.GetStarred, True);

    // Write "EOF" marker as last byte in file
    Writer.WriteByte(EOFByte);
  finally
    Writer.Free;
  end;
end;

procedure TDBNativeWriter.WriteSnippetIDListProp(
  const Writer: TBinaryStreamWriter; const PropCode: TDBSnippetProp;
  IDs: IDBSnippetIDList; const Optional: Boolean);
var
  ID: TDBSnippetID;
begin
  if Optional and (IDs.Count = 0) then
    Exit;
  WritePropCode(Writer, PropCode);
  Writer.WriteInt32(IDs.Count);
  for ID in IDs do
    Writer.WriteSizedString16(ID.ToString);
end;

procedure TDBNativeWriter.WriteString16Prop(const Writer: TBinaryStreamWriter;
  const PropCode: TDBSnippetProp; const Str: string; const Optional: Boolean);
begin
  if Optional and (Str = EmptyStr) then
    Exit;
  WritePropCode(Writer, PropCode);
  Writer.WriteSizedString16(Str);
end;

procedure TDBNativeWriter.WriteString32Prop(const Writer: TBinaryStreamWriter;
  const PropCode: TDBSnippetProp; const Str: string; const Optional: Boolean);
begin
  if Optional and (Str = EmptyStr) then
    Exit;
  WritePropCode(Writer, PropCode);
  Writer.WriteSizedString32(Str);
end;

procedure TDBNativeWriter.WriteStringsProp(const Writer: TBinaryStreamWriter;
  const PropCode: TDBSnippetProp; Strings: IStringList;
  const Optional: Boolean);
var
  S: string;
begin
  if Optional and (Strings.Count = 0) then
    Exit;
  WritePropCode(Writer, PropCode);
  Writer.WriteInt32(Strings.Count);
  for S in Strings do
    Writer.WriteSizedString32(S);
end;

procedure TDBNativeWriter.WriteTagsProp(const Writer: TBinaryStreamWriter;
  const PropCode: TDBSnippetProp; Tags: IDBTagList; const Optional: Boolean);
var
  Tag: TDBTag;
begin
  if Optional and (Tags.Count = 0) then
    Exit;
  WritePropCode(Writer, PropCode);
  Writer.WriteInt32(Tags.Count);
  for Tag in Tags do
    Writer.WriteSizedString16(Tag.ToString);
end;

procedure TDBNativeWriter.WriteTestInfoProp(const Writer: TBinaryStreamWriter;
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
  if (E is EDBSnippetID) or (E is EConvertError) then
    raise EDBNativeIO.CreateFmt(sConvertError, [E.Message]);
  raise E;
end;

procedure TDBNativeReader.Load(const ATable: TDBSnippetsTable;
  out ALastModified: TUTCDateTime);
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

procedure TDBNativeReader.LoadSnippet(const AID: TDBSnippetID;
  const ATable: TDBSnippetsTable);
var
  Reader: TBinaryStreamReader;
  Snippet: TDBSnippet;
begin
  Reader := TBinaryStreamReader.Create(
    TFileStream.Create(SnippetFileName(AID), fmOpenRead or fmShareDenyWrite),
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
  const Reader: TBinaryStreamReader);

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
        ASnippet.SetModified(
          TUTCDateTime.CreateFromISO8601String(Reader.ReadSizedString16)
        );
      spCreated:
        ASnippet.SetCreated(
          TUTCDateTime.CreateFromISO8601String(Reader.ReadSizedString16)
        );
      spRequiredModules:
        ASnippet.SetRequiredModules(ReadStrings(Reader));
      spRequiredSnippets:
        ASnippet.SetRequiredSnippets(ReadSnippetIDs(Reader));
      spXRefs:
        ASnippet.SetXRefs(ReadSnippetIDs(Reader));
      spNotes:
        ASnippet.SetNotes(ReadMarkup(Reader));
      spKind:
        ASnippet.SetKind(TDBSnippetKind(Reader.ReadByte));
      spCompileResults:
        ASnippet.SetCompileResults(ReadCompileResults(Reader));
      spTags:
        ASnippet.SetTags(ReadTags(Reader));
      spLinkInfo:
        ASnippet.SetLinkInfo(ReadLinkInfo(Reader));
      spTestInfo:
        ASnippet.SetTestInfo(TSnippetTestInfo(Reader.ReadByte));
      spStarred:
        ASnippet.SetStarred(Boolean(Reader.ReadByte));
      else
        ; // ignore any property codes we don't recognise
    end;
  end;
end;

function TDBNativeReader.ReadCompileResults(const Reader: TBinaryStreamReader):
  TDBCompileResults;

  function ReadSet: TCompilerIDs;
  var
    SetSize: Integer;
    I: Integer;
  begin
    SetSize := Reader.ReadByte;
    Result := [];
    for I := 1 to SetSize do
      Include(Result, TCompilerID(Reader.ReadByte));
  end;

var
  Succeeds, Fails: TCompilerIDs;
begin
  Succeeds := ReadSet;
  Fails := ReadSet;
  Result := TDBCompileResults.Create(Succeeds, Fails);
end;

function TDBNativeReader.ReadLinkInfo(const Reader: TBinaryStreamReader):
  ISnippetLinkInfo;
var
  SynchSpaceID: TGUID;
  LinkedSnippetID: TDBSnippetID;
begin
  // If this property is present snippet is linked to space: the property is
  // never present in file if snippet is not linked. Therefore return value is
  // never a null instance.
  Reader.ReadBuffer(SynchSpaceID, SizeOf(SynchSpaceID));
  LinkedSnippetID := TDBSnippetID.Create(Reader.ReadSizedString16);
  Result := TSnippetLinkInfo.Create(SynchSpaceID, LinkedSnippetID);
end;

function TDBNativeReader.ReadMarkup(const Reader: TBinaryStreamReader): TMarkup;
var
  Kind: TMarkupKind;
  Source: string;
begin
  Kind := TMarkupKind(Reader.ReadByte);
  Source := Reader.ReadSizedString32;
  Result := TMarkup.Create(Source, Kind);
end;

function TDBNativeReader.ReadSnippetIDs(const Reader: TBinaryStreamReader):
  IDBSnippetIDList;
var
  Count: Integer;
  I: Integer;
begin
  Count := Reader.ReadInt32;
  Result := TDBSnippetIDList.Create;
  for I := 1 to Count do
    Result.Add(TDBSnippetID.Create(Reader.ReadSizedString16));
end;

function TDBNativeReader.ReadStrings(const Reader: TBinaryStreamReader):
  IStringList;
var
  Count: Integer;
  I: Integer;
begin
  Count := Reader.ReadInt32;
  Result := TIStringList.Create;
  for I := 1 to Count do
    Result.Add(Reader.ReadSizedString32);
end;

function TDBNativeReader.ReadTags(const Reader: TBinaryStreamReader):
  IDBTagList;
var
  Count: Integer;
  I: Integer;
begin
  Count := Reader.ReadInt32;
  Result := TDBTagList.Create;
  for I := 1 to Count do
    Result.Add(TDBTag.Create(Reader.ReadSizedString16));
end;

procedure TDBNativeReader.ValidateSnippetFileHeader(
  const Reader: TBinaryStreamReader);
var
  Watermark: string;
  Version: Integer;
resourcestring
  sBadWatermark = 'Invalid watermark at start of snippet file';
  sBadVersion = 'Invalid or unsupported snippet file version';
begin
  Watermark := TEncoding.UTF8.GetString(
    Reader.ReadBytes(Length(MasterFileWatermark))
  );
  if Watermark <> SnippetFileWatermark then
    raise EDBNativeIO.Create(sBadWatermark);
  Version := StrToIntDef(TEncoding.UTF8.GetString(Reader.ReadBytes(4)), 0);
  if not IsSupportedVersion(Version) then
    raise EDBNativeIO.CreateFmt(sBadVersion, [Version]);
end;

end.


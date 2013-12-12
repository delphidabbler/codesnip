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
 * Implements a class that can load the snippets database in the formats used
 * for the user database in CodeSnip v2 to v4.
}


unit CS.Database.IO.Legacy;

interface

uses
  // Delphi
  SysUtils,
  XMLIntf,
  // 3rd party
  Collections.Dictionaries,
  // Project
  CS.Database.IO.Types,
  CS.Database.SnippetsTable,
  CS.Database.Types,
  CS.Utils.Dates,
  UExceptions,
  UXMLDocumentEx;

type
  TDBLegacyUserDBReader = class(TInterfacedObject, IDatabaseLoader)
  strict private
    const
      // Database file name
      DatabaseFileName = 'database.xml';
      // watermark (never changes for all versions)
      Watermark = '531257EA-1EE3-4B0F-8E46-C6E7F7140106';
      // supported file format versions
      EarliestVersion = 1;
      LatestVersion = 6;
  strict private
    var
      fDBPath: string;
      fXMLDoc: IXMLDocumentEx;  // Extended XML document object
      fVersion: Integer;
      ///  <summary>Maps category IDs to tag name based on category name.
      ///  </summary>
      fCategoryMap: TDictionary<string,string>;
    procedure HandleException(E: Exception);
    function XMLFileName: string;
    function LegacySnippetName(SnippetNode: IXMLNode): string;
    procedure OpenXMLDoc;
    procedure ReadCategoryInfo;
    procedure ReadSnippets(const ATable: TDBSnippetsTable);
    procedure LoadSnippet(SnippetNode: IXMLNode;
      const ATable: TDBSnippetsTable);
    procedure LoadSnippetProperties(SnippetNode: IXMLNode;
      const ASnippet: TDBSnippet);
    function GetLastModifiedFileDate(ATable: TDBSnippetsTable): TUTCDateTime;
    function BuildTagSet: ITagSet;
  public
    constructor Create(const DBPath: string);
    destructor Destroy; override;
    procedure Load(const ATable: TDBSnippetsTable; out ATagSet: ITagSet;
      out ALastModified: TUTCDateTime);
    function DatabaseExists: Boolean;
  end;

  EDBLegacyUserDBReader = class(ECodeSnip);

implementation

uses
  // Delphi
  Character,
  Classes,
  IOUtils,
  ActiveX,
  XMLDom,
  // 3rd party
  Collections.Base,
  // Project
  CS.ActiveText,
  CS.ActiveText.Helper,
  CS.Database.SnippetLinks,
  CS.Database.Snippets,
  CS.Database.Tags,
  CS.SourceCode.Languages,
  Compilers.UGlobals,
  DB.USnippetKind,
  UComparers,
  UConsts,
  UDOSDateTime,
  UIOUtils,
  UIStringList,
  UStructs,
  UStrUtils,
  UXMLDocConsts,
  UXMLDocHelper;

{ TDBLegacyUserDBReader }

resourcestring
  // Error message
  sMissingNode = 'Document has no %s node.';
  sMissingSource = 'Source code file name missing for legacy snippet named '
    + '"%s"';
  sFileNotFound = 'Database file "%s" missing.';

function TDBLegacyUserDBReader.BuildTagSet: ITagSet;
var
  TagName: string;
begin
  Result := TTagSet.Create;
  for TagName in fCategoryMap.Values do
    Result.Add(TTag.Create(TagName));
end;

constructor TDBLegacyUserDBReader.Create(const DBPath: string);
var
  StrRules: TRules<string>;
begin
  inherited Create;
  fDBPath := DBPath;
  StrRules := TRules<string>.Create(
    TTextComparator.Create, TTextComparator.Create
  );
  fCategoryMap := TDictionary<string,string>.Create(
    StrRules, StrRules
  );
  fVersion := -1; // dummy value changed by OpenXMLDoc when Load is called
  // For some reason we must call OleInitialize here rather than in
  // initialization section
  OleInitialize(nil);
  fXMLDoc := TXMLDocHelper.CreateXMLDoc;
end;

function TDBLegacyUserDBReader.DatabaseExists: Boolean;
const
  ChunkSize = 512;
  XMLProcInst = '<? xml version="1.0"';
  Watermark = '<codesnip-data watermark="531257EA-1EE3-4B0F-8E46-C6E7F7140106"';
var
  Chunk: string;  // chunk of up to 512 ASCII chars read from file
begin
  Result := False;
  try
    if not TFile.Exists(XMLFileName, False) then
      Exit;
    // Beginning of file can be treated as ASCII. There is never a BOM
    Chunk := TEncoding.ASCII.GetString(
      TFileIO.ReadBytes(XMLFileName, ChunkSize)
    );
    if not StrStartsStr(XMLProcInst, Chunk) then
      Exit; // Not XML
    if not StrContainsStr(Watermark, Chunk) then
      Exit; // No valid open tag or watermark attribute
    Result := True;
  except
    // swallow any exception (Result will be False)
  end;
end;

destructor TDBLegacyUserDBReader.Destroy;
begin
  fCategoryMap.Free;
  inherited;
end;

function TDBLegacyUserDBReader.GetLastModifiedFileDate(
  ATable: TDBSnippetsTable): TUTCDateTime;
var
  Snippet: TDBSnippet;
begin
  Result := TUTCDateTime.CreateNull;
  for Snippet in ATable do
  begin
    if Result < Snippet.GetModified then
      Result := Snippet.GetModified;
  end;
  if Result.IsNull then
    Result := TUTCDateTime.Now;
end;

procedure TDBLegacyUserDBReader.HandleException(E: Exception);
resourcestring
  sXMLError = 'Error reading database file XML:' + EOL2 + '%s';
  sParseError = 'Error parsing database file:' + EOL2 + '%s';
  sSourceFileError = 'Error reading source code file:' + EOL2 + '%s';
begin
  if (E is EXMLDocError) or (E is ECodeSnipXML) or (E is EDOMParseError) then
    raise EDBLegacyUserDBReader.CreateFmt(sXMLError, [E.Message]);
  if (E is ESnippetID) or (E is ETag) then
    raise EDBLegacyUserDBReader.CreateFmt(sParseError, [E.Message]);
  if (E is EIOUtils) or (E is EStreamError) then
    raise EDBLegacyUserDBReader.CreateFmt(sSourceFileError, [E.Message]);
  raise E;
end;

function TDBLegacyUserDBReader.LegacySnippetName(SnippetNode: IXMLNode):
  string;
begin
  Result := SnippetNode.Attributes[cSnippetNameAttr];
end;

procedure TDBLegacyUserDBReader.Load(const ATable: TDBSnippetsTable;
  out ATagSet: ITagSet; out ALastModified: TUTCDateTime);
begin
  ATable.Clear;
  try
    OpenXMLDoc;
    ReadCategoryInfo;
    ReadSnippets(ATable);
    ATagSet := BuildTagSet;
    ALastModified := GetLastModifiedFileDate(ATable);
  except
    ATable.Clear;
    if ExceptObject is Exception then
      HandleException(ExceptObject as Exception);
  end;
end;

procedure TDBLegacyUserDBReader.LoadSnippet(SnippetNode: IXMLNode;
  const ATable: TDBSnippetsTable);
var
  Snippet: TDBSnippet;
begin
  Snippet := TDBSnippet.Create(
    TSnippetID.Create(LegacySnippetName(SnippetNode))
  );
  try
    LoadSnippetProperties(SnippetNode, Snippet);
    ATable.Add(Snippet);
  except
    Snippet.Free;
    raise;
  end;
end;

procedure TDBLegacyUserDBReader.LoadSnippetProperties(SnippetNode: IXMLNode;
  const ASnippet: TDBSnippet);

  // Gets text of property with given sub-tag of current snippet node in XML
  // document.
  function GetPropertyText(const PropTagName: string): string;
  begin
    Result := TXMLDocHelper.GetSubTagText(fXMLDoc, SnippetNode, PropTagName);
  end;

  // Returns value of current snippet'ss Tags property from data in XML
  // document. A single tag is returned which uses as its text the description
  // of the legacy snippet's category.
  function GetTagsProperty: ITagSet;
  var
    CatID: string;
    TagName: string;
  begin
    Result := TTagSet.Create;
    CatID := GetPropertyText(cCatIDNode);
    if not fCategoryMap.TryGetValue(CatID, TagName) then
      Exit;
    Result.Add(TTag.Create(TagName));
  end;

  // Returns the name of the file containing the snippet's source code.
  function GetSourceCodeFileName: string;
  var
    DataFileName: string; // base name of source code file
  begin
    DataFileName := GetPropertyText(cSourceCodeFileNode);
    if DataFileName = '' then
      raise EDBLegacyUserDBReader.CreateFmt(
        sMissingSource, [ASnippet.GetID.ToString]
      );
    Result := IncludeTrailingPathDelimiter(fDBPath) + DataFileName;
  end;

  // Returns the date and time the snippet's source code file was last saved.
  function GetLastUpdateDate: TUTCDateTime;
  var
    DOSDate: IDOSDateTime;
  begin
    DOSDate := TDOSDateTimeFactory.CreateFromFile(GetSourceCodeFileName);
    if DOSDate.DateStamp = -1 then
      Exit(TUTCDateTime.CreateNull);
    Result := TUTCDateTime.CreateFromLocalDateTime(
      FileDateToDateTime(DOSDate.DateStamp)
    );
  end;

  // Returns the snippet's source code, read from the data file referenced in
  // the XML document.
  function GetSourceCodeProperty: string;
  var
    Encoding: TEncoding;
  begin
    // Before database v5, source code files used default ANSI encoding. From v5
    // UTF-8 with no BOM was used.
    if fVersion < 5 then
      Encoding := TEncoding.Default
    else
      Encoding := TEncoding.UTF8;
    Result := TFileIO.ReadAllText(GetSourceCodeFileName, Encoding, False);
  end;

  // Returns the value of the XML document's <standard-format> node. Returns
  // False if there is no such node.
  function GetStandardFormatValue: Boolean;
    {Gets value of standard format node.
      @return True if standard format, False if not.
    }
  begin
    Result := TXMLDocHelper.GetStandardFormat(
      fXMLDoc, SnippetNode, False
    );
  end;

  // Returns the value of the snippet's Kind property. This is a direct match
  // with legacy snippet's property of the same name.
  function GetKindProperty: TSnippetKind;
    {Gets value of Kind node.
      @return Kind that matches node value.
    }
  var
    Default: TSnippetKind;            // default value
  begin
    // In earlier file format versions we have no Kind node, so we calculate
    // kind from StandardFormat value. If Kind node is present StandardFormat is
    // ignored.
    if GetStandardFormatValue then
      Default := skRoutine
    else
      Default := skFreeform;
    Result := TXMLDocHelper.GetSnippetKind(
      fXMLDoc, SnippetNode, Default
    );
  end;

  // Returns mark-up of snippet's Notes property. This value is derived from
  // either the legacy snippet's Comments and Credits properties or from its
  // Extra property.
  function GetNotesProperty: IActiveText;
  begin
    // We get Notes data from different nodes depending on file version
    try
      if fVersion = 1 then
        // version 1: build Notes from comments, credits and credits URL
        // nodes
        Result := TActiveTextHelper.ParseCommentsAndCredits(
          GetPropertyText(cCommentsNode),
          GetPropertyText(cCreditsNode),
          GetPropertyText(cCreditsUrlNode)
        )
      else
        // version 2 & later: build Notes from REML in "extra" node
        Result := TActiveTextHelper.ParseREML(
          GetPropertyText(cExtraNode)
        );
    except
      // error: provide an empty property value
      Result := TActiveTextFactory.CreateActiveText;
    end;
  end;

  // Returns markup of snippets Description property. This is derived from the
  // similar property of the legacy snippet.
  function GetDescriptionProperty: IActiveText;
  var
    Desc: string; // text read from description node
  begin
    Desc := GetPropertyText(cDescriptionNode);
    if Desc <> '' then
    begin
      if fVersion < 6 then
        // versions before 6: description is stored as plain text
        Result := TActiveTextHelper.ParsePlainText(Desc)
      else
        // version 6 & later: description is stored as REML
        Result := TActiveTextHelper.ParseREML(Desc)
    end
    else
      Result := TActiveTextFactory.CreateActiveText;
  end;

  // Returns the text of the snippet's Title property. This is derived from the
  // the legacy snippet's DisplayName or, if that is not provided, its Name
  // property.
  function GetTitleProperty: string;
  begin
    Result := GetPropertyText(cDisplayNameNode);
    if StrIsBlank(Result) then
      Result := LegacySnippetName(SnippetNode);
  end;

  // Returns the snippet's LanguageID property value which is deduced to be
  // either Pascal or Text depending on whether the legacy snippet was to be
  // highlighted or not.
  function GetLanguageIDProperty: TSourceCodeLanguageID;
  begin
    if TXMLDocHelper.GetHiliteSource(fXMLDoc, SnippetNode, True) then
      Result := TSourceCodeLanguageID.Create('Pascal')
    else
      Result := TSourceCodeLanguageID.Create('Text');
  end;

  // Converts the given list of legacy snippet names into a list of snippets
  // with IDs based on the legacy names.
  function ConvertSnippetIDList(LegacyNameList: IStringList): ISnippetIDList;
  var
    LegacyName: string;
  begin
    Result := TSnippetIDList.Create;
    for LegacyName in LegacyNameList do
      Result.Add(TSnippetID.Create(LegacyName));
  end;

  // Reads a list of Pascal names from the given sub-tag of the current
  // snippet's node in the XML document.
  function GetPascalNameListFor(const SubTag: string): IStringList;
  begin
    Result := TIStringList.Create;
    TXMLDocHelper.GetPascalNameList(
      fXMLDoc, fXMLDoc.FindFirstChildNode(SnippetNode, SubTag), Result
    );
  end;

  // Returns the snippet's CompileResults property value. This is derived from
  // the legacy snippet's own CompileResult property, which has a different
  // format to that of the new snippet.
  function GetCompileResultsProperty: TCompileResults;
  begin
    Result := TXMLDocHelper.GetCompilerResults(
      fXMLDoc, SnippetNode
    );
  end;

  // ---------------------------------------------------------------------------

begin
  ASnippet.SetCreated(GetLastUpdateDate);
  ASnippet.SetModified(ASnippet.GetCreated);
  ASnippet.SetTitle(GetTitleProperty);
  ASnippet.SetDescription(GetDescriptionProperty);
  ASnippet.SetSourceCode(GetSourceCodeProperty);
  ASnippet.SetLanguageID(GetLanguageIDProperty);
  ASnippet.SetRequiredModules(GetPascalNameListFor(cUnitsNode));
  ASnippet.SetRequiredSnippets(
    ConvertSnippetIDList(GetPascalNameListFor(cDependsNode))
  );
  ASnippet.SetXRefs(
    ConvertSnippetIDList(GetPascalNameListFor(cXRefNode))
  );
  ASnippet.SetNotes(GetNotesProperty);
  ASnippet.SetKind(GetKindProperty);
  ASnippet.SetCompileResults(GetCompileResultsProperty);
  ASnippet.SetTags(GetTagsProperty);
  // We create a link back to the snippet's original ID in case it might be
  // useful. We use the special "LegacyDB" synch space.
  ASnippet.SetLinkInfo(
    TSnippetLinkInfo.Create(
      TSnippetSynchSpaceIDs.LegacyDB,
      TSnippetID.Create(LegacySnippetName(SnippetNode))
    )
  );
  // Note that the snippet's TestInfo and Starred properties have no equivalent
  // property in legacy snippets, so the values are not set here: defaults are
  // acceptable.
end;

procedure TDBLegacyUserDBReader.OpenXMLDoc;
var
  XMLFile: string;
begin
  XMLFile := XMLFileName;
  if not TFile.Exists(XMLFile, False) then
    raise EDBLegacyUserDBReader.CreateFmt(sFileNotFound, [DatabaseFileName]);
  fXMLDoc.LoadFromFile(XMLFile);
  fXMLDoc.Active := True;
  TXMLDocHelper.ValidateProcessingInstr(fXMLDoc);
  fVersion := TXMLDocHelper.ValidateRootNode(
    fXMLDoc,
    cUserDataRootNode,
    Watermark,
    TRange.Create(EarliestVersion, LatestVersion)
  );
  // Both a categories and a snippets node must exist
  if fXMLDoc.FindNode(cUserDataRootNode + '\' + cCategoriesNode) = nil then
    raise EDBLegacyUserDBReader.CreateFmt(sMissingNode, [cCategoriesNode]);
  if fXMLDoc.FindNode(cUserDataRootNode + '\' + cSnippetsNode) = nil then
    raise EDBLegacyUserDBReader.CreateFmt(sMissingNode, [cSnippetsNode]);
end;

procedure TDBLegacyUserDBReader.ReadCategoryInfo;
var
  CatListNode: IXMLNode;
  CatNodes: IXMLSimpleNodeList;
  CatNode: IXMLNode;
  CatID: string;
  TagName: string;
begin
  // Find <categories> node
  CatListNode := fXMLDoc.FindNode(cUserDataRootNode + '\' + cCategoriesNode);
  if not Assigned(CatListNode) then
    raise EDBLegacyUserDBReader.CreateFmt(sMissingNode, [cCategoriesNode]);
  // Find all <category> nodes in <categories>
  CatNodes := fXMLDoc.FindChildNodes(CatListNode, cCategoryNode);
  fCategoryMap.Clear;
  for CatNode in CatNodes do
  begin
    // Category ID is attribute of <category> node
    CatID := CatNode.Attributes[cCategoryIdAttr];
    if not fCategoryMap.ContainsKey(CatID) then
    begin
      // Category description is in <description> sub-tag of <category>. We
      // convert description to a valid tag name and add to category->tag name
      // map
      TagName := TTag.MakeValidTagString(
        TXMLDocHelper.GetSubTagText(
          fXMLDoc, CatNode, cDescriptionNode
        )
      );
      fCategoryMap.Add(CatID, TagName);
    end;
  end;
end;

procedure TDBLegacyUserDBReader.ReadSnippets(const ATable: TDBSnippetsTable);
var
  SnippetListNode: IXMLNode;
  SnippetNodes: IXMLSimpleNodeList;
  SnippetNode: IXMLNode;
begin
  // Find <routines> SnippetNode
  SnippetListNode := fXMLDoc.FindNode(cUserDataRootNode + '\' + cSnippetsNode);
  if not Assigned(SnippetListNode) then
    raise EDBLegacyUserDBReader.CreateFmt(sMissingNode, [cSnippetsNode]);
  // Find all <routine> nodes in <categories>
  SnippetNodes := fXMLDoc.FindChildNodes(SnippetListNode, cSnippetNode);
  for SnippetNode in SnippetNodes do
    LoadSnippet(SnippetNode, ATable);
end;

function TDBLegacyUserDBReader.XMLFileName: string;
begin
  Result := IncludeTrailingPathDelimiter(fDBPath) + DatabaseFileName;
end;

end.


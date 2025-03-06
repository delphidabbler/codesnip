{
 * This Source Code Form is subject to the terms of the Mozilla Public License,
 * v. 2.0. If a copy of the MPL was not distributed with this file, You can
 * obtain one at https://mozilla.org/MPL/2.0/
 *
 * Copyright (C) 2008-2023, Peter Johnson (gravatar.com/delphidabbler).
 *
 * Implements classes that can import and export snippets from and to XML.
}


unit DB.IO.ImportExport.CS4;


interface


uses
  // Delphi
  SysUtils,
  Classes,
  XMLIntf,
  Generics.Collections,
  // Project
  DB.UCategory,
  DB.USnippet,
  UBaseObjects,
  UEncodings,
  UIStringList,
  USnippetIDs,
  UXMLDocHelper,
  UXMLDocumentEx;


type
  ///  <summary>Encapsulates data that describes a snippet that has been read
  ///  from an import file.</summary>
  TSnippetInfo = record
    ///  <summary>Snippet key.</summary>
    Key: string;
    ///  <summary>Description of snippet.</summary>
    Data: TSnippetEditData;
    ///  <summary>Copies given TSnippetInfo record to this one.</summary>
    procedure Assign(const Src: TSnippetInfo);
    ///  <summary>Initialises record to null value.</summary>
    procedure Init;
  end;

type
  ///  <summary>Dynamic array of TSnippetInfo records.</summary>
  TSnippetInfoList = array of TSnippetInfo;

type
  ///  <summary>Imports code snippets from CodeSnip 4 import/export format
  ///  files.</summary>
  TCS4SnippetImporter = class(TNoPublicConstructObject)
  strict private
    const
      {TODO -cVault: Let user select or create a category rather than imposing
              this one}
      ///  <summary>ID of category used to import snippets.</summary>
      ImportCatID = 'imports';
    var
      ///  <summary>Version of file being imported.</summary>
      fVersion: Integer;
      ///  <summary>List of snippets read from XML.</summary>
      fSnippetInfo: TSnippetInfoList;
    ///  <summary>Extended XML document object.</summary>
    fXMLDoc: IXMLDocumentEx;
    ///  <summary>Retrieves a list of all snippet nodes from XML document.
    ///  </summary>
    function GetAllSnippetNodes: IXMLSimpleNodeList;
    ///  <summary>Imports code snippets from the given byte array containing the
    ///  XML data.</summary>
    ///  <remarks>Data must contain valid XML in a suitable encoding.</remarks>
    procedure Execute(const Data: TBytes);
    ///  <summary>Validates XML document and returns file version if valid.
    ///  </summary>
    ///  <exception>ECodeImporter raised if XML is not valid.</exception>
    function ValidateDoc: Integer;
    ///  <summary>Checks if the special import category exists and creates it if
    ///  not.</summary>
    class procedure EnsureImportCategoryExists;
    ///  <summary>Constructs and initialises object ready to perform import.
    ///  </summary>
    constructor InternalCreate;

  public
    ///  <summary>Destroys object.</summary>
    destructor Destroy; override;
    ///  <summary>Imports snippets from XML.</summary>
    ///  <param name="SnippetInfo">TSnippetInfoList [out] Receives information
    ///  about each imported snippet.</param>
    ///  <param name="Data">TBytes [in] Byte array containing XML data.</param>
    class procedure ImportData(out SnippetInfo: TSnippetInfoList;
      const Data: TBytes);
  end;

type
  ///  <summary>Class of exception raised when <c>TCS4SnippetImporter</c>
  ///  encounters invalid XML.</summary>
  ECS4SnippetImporter = class(ECodeSnipXML);

type
  ///  <summary>Exports code snippets to CodeSnip 4 import/export format file
  ///  </summary>
  TCS4SnippetExporter = class(TNoPublicConstructObject)
  strict private
    var
      fSnippetKeyMap: TDictionary<TSnippetID,string>;
      ///  <summary>List of snippets to be exported.</summary>
      fSnippets: TSnippetList;
      ///  <summary>Extended XML document object.</summary>
      fXMLDoc: IXMLDocumentEx;
    ///  <summary>Examines given exception and converts into ECodeExporter if it
    ///  is an expected exception. Unexpected exceptions are re-raised.
    ///  </summary>
    ///  <exception>An exception is always raised.</exception>
    procedure HandleException(const EObj: TObject);
    ///  <summary>Returns a list of snippet keys for each snippet in
    ///  <c>SnipList</c>. Returned keys are found by looking up the new key
    ///  corresponding to the snippet's original key in <c>fSnippetKeyMap</c>.
    ///  </summary>
    function SnippetKeys(const SnipList: TSnippetList): IStringList;
    ///  <summary>Writes a XML node that contains a list of pascal names.
    ///  </summary>
    ///  <param name="ParentNode">IXMLNode [in] Node under which this name list
    ///  node is to be created.</param>
    ///  <param name="ListNodeName">string [in] Name of list node.</param>
    ///  <param name="PasNames">IStringList [in] List of pascal names to be
    ///  written as child nodes of name list.</param>
    procedure WriteReferenceList(const ParentNode: IXMLNode;
      const ListNodeName: string; PasNames: IStringList);
    ///  <summary>Writes XML node containing CodeSnip version information.
    ///  </summary>
    ///  <param name="ParentNode">IXMLNode [in] Node under which this node is to
    ///  be created.</param>
    procedure WriteProgInfo(const ParentNode: IXMLNode);
    ///  <summary>Writes nodes containing details of all exported snippets.
    ///  </summary>
    ///  <param name="ParentNode">IXMLNode [in] Node under which snippets are to
    ///  be written.</param>
    procedure WriteSnippets(const ParentNode: IXMLNode);
    ///  <summary>Writes an XML snippet node and child nodes that describe a
    ///  snippet.</summary>
    ///  <param name="ParentNode">IXMLNode [in] Node under which snippet node is
    ///  to be written.</param>
    ///  <param name="Snippet">TSnippet [in] Reference to snippet to be
    ///  described in XML.</param>
    procedure WriteSnippet(const ParentNode: IXMLNode; const Snippet: TSnippet);
    ///  <summary>Performs the export.</summary>
    ///  <returns>TEncodedData. Encoded data containing exported XML.</returns>
    ///  <exception>ECodeExporter raised if a known error is encountered.
    ///  </exception>
    function Execute: TEncodedData;
    ///  <summary>Constructs and initialises object ready to perform export.
    ///  </summary>
    ///  <param name="SnipList">TSnippetList [in] List of snippets to be
    ///  exported.</param>
    constructor InternalCreate(const SnipList: TSnippetList);
  public
    ///  <summary>Destroys object.</summary>
    destructor Destroy; override;
    ///  <summary>Exports snippets as XML.</summary>
    ///  <param name="SnipList">TSnippetList [in] List of snippets to be
    ///  exported.</param>
    ///  <returns>TEncodedData. Encoded data containing exported XML.</returns>
    class function ExportSnippets(const SnipList: TSnippetList): TEncodedData;
  end;

type
  ///  <summary>Class of exception raised when <c>TCS4SnippetExporter</c>
  ///  detects an expected error.</summary>
  ECS4SnippetExporter = class(ECodeSnipXML);


implementation


uses
  // Delphi
  ActiveX,
  XMLDom,
  // Project
  ActiveText.UMain,
  DB.IO.Common.CS4,
  DB.UMain,
  DB.USnippetKind,
  DB.Vaults,
  UAppInfo,
  USnippetExtraHelper,
  UStructs,
  UStrUtils;

type
  TCS4ImportExportDocHelper = class(TCS4FormatHelper)
  public
    const
      // watermark (never changes for all versions)
      Watermark = 'B46969D4-D367-4F5F-833E-F165FBA78631';
      // file version numbers
      EarliestVersion = 1;  // earliest file version supported by importer
      LatestVersion = 7;    // current file version written by exporter
      // XML node names
      ExportRootNodeName = 'codesnip-export';
      ProgVersionNodeName = 'prog-version';
      SourceCodeTextNodeName= 'source-code-text';
  end;

{ TCS4SnippetExporter }

destructor TCS4SnippetExporter.Destroy;
begin
  fSnippetKeyMap.Free;
  fXMLDoc := nil;
  inherited;
end;

function TCS4SnippetExporter.Execute: TEncodedData;
var
  RootNode: IXMLNode; // document root node
resourcestring
  // Comment written to XML file
  sFileComment = 'This file was generated by CodeSnip. Do not edit.';
begin
  // Create and configure XML document
  fXMLDoc := TCS4ImportExportDocHelper.CreateXMLDoc;
  try
    fXMLDoc.Active := True;

    // Add XML root nodes
    TCS4ImportExportDocHelper.CreateXMLProcInst(fXMLDoc);
    TCS4ImportExportDocHelper.CreateComment(fXMLDoc, sFileComment);
    RootNode := TCS4ImportExportDocHelper.CreateRootNode(
      fXMLDoc,
      TCS4ImportExportDocHelper.ExportRootNodeName,
      TCS4ImportExportDocHelper.Watermark,
      TCS4ImportExportDocHelper.LatestVersion
    );

    // Write document content
    WriteProgInfo(RootNode);
    WriteSnippets(RootNode);

    // Save XML as UTF-8 with no BOM
    fXMLDoc.Encoding := 'UTF-8';
    Result := TEncodedData.Create(fXMLDoc.SaveToBytes, etUTF8);
  except
    HandleException(ExceptObject);
  end;
end;

class function TCS4SnippetExporter.ExportSnippets(const SnipList: TSnippetList):
  TEncodedData;
var
  Instance: TCS4SnippetExporter;
begin
  Instance := InternalCreate(SnipList);
  try
    Result := Instance.Execute;
  finally
    Instance.Free;
  end;
end;

procedure TCS4SnippetExporter.HandleException(const EObj: TObject);
begin
  if (EObj is EFileStreamError) or (EObj is ECodeSnipXML) then
    raise ECS4SnippetExporter.Create(EObj as Exception);
  raise EObj;
end;

constructor TCS4SnippetExporter.InternalCreate(const SnipList: TSnippetList);
var
  Snippet: TSnippet;
begin
  inherited InternalCreate;
  fSnippets := SnipList;
  fSnippetKeyMap := TDictionary<TSnippetID,string>.Create(
    TSnippetID.TComparer.Create
  );
  // Create map of actual snippet ID to new unique key with default vault
  for Snippet in SnipList do
    fSnippetKeyMap.Add(
      Snippet.ID,
      (Database as IDatabaseEdit).GetUniqueSnippetKey(TVaultID.Default)
    );
end;

function TCS4SnippetExporter.SnippetKeys(const SnipList: TSnippetList):
  IStringList;
var
  Snippet: TSnippet;  // references each snippet in list
begin
  Result := TIStringList.Create;
  for Snippet in SnipList do
    if fSnippetKeyMap.ContainsKey(Snippet.ID) then
      Result.Add(fSnippetKeyMap[Snippet.ID]);
end;

procedure TCS4SnippetExporter.WriteProgInfo(const ParentNode: IXMLNode);
begin
  fXMLDoc.CreateElement(
    ParentNode,
    TCS4ImportExportDocHelper.ProgVersionNodeName,
    TAppInfo.ProgramReleaseVersion
  );
end;

procedure TCS4SnippetExporter.WriteReferenceList(const ParentNode: IXMLNode;
  const ListNodeName: string; PasNames: IStringList);
begin
  // Don't write list tags if no items
  if PasNames.Count = 0 then
    Exit;
  // Write the list
  TCS4ImportExportDocHelper.WritePascalNameList(
    fXMLDoc, ParentNode, ListNodeName, PasNames
  );
end;

procedure TCS4SnippetExporter.WriteSnippet(const ParentNode: IXMLNode;
  const Snippet: TSnippet);
var
  SnippetNode: IXMLNode; // new snippet node
begin
  // Create snippet node with attribute that specifies snippet key.
  // Snippet is exported under a new, unique key within the Default vault.
  // Since no vault information is saved, we need choose one vault in order
  // to generate the key, and the Default vault is the only one guaranteed to be
  // present.
  SnippetNode := fXMLDoc.CreateElement(
    ParentNode, TCS4ImportExportDocHelper.SnippetNodeName
  );
  SnippetNode.Attributes[
    TCS4ImportExportDocHelper.SnippetNodeNameAttr
  ] := fSnippetKeyMap[Snippet.ID];
  // Add nodes for properties: (ignore category and xrefs)
  // description node is written even if empty (which it shouldn't be)
  fXMLDoc.CreateElement(
    SnippetNode,
    TCS4ImportExportDocHelper.DescriptionNodeName,
    TSnippetExtraHelper.BuildREMLMarkup(Snippet.Description)
  );
  // Snippet's display name always written: if display name is specified we use
  // it, otherwise we use the original snippet key.
  if not StrIsEmpty(Snippet.DisplayName, True) then
    fXMLDoc.CreateElement(
      SnippetNode,
      TCS4ImportExportDocHelper.DisplayNameNodeName,
      Snippet.DisplayName
    )
  else
    fXMLDoc.CreateElement(
      SnippetNode, TCS4ImportExportDocHelper.DisplayNameNodeName, Snippet.Key
    );
  // source code is stored directly in XML, not in external file
  fXMLDoc.CreateElement(
    SnippetNode,
    TCS4ImportExportDocHelper.SourceCodeTextNodeName,
    Snippet.SourceCode
  );
  // write highlight source flag
  {TODO -cRefactor: Move following method call into TCS4FormatHelper}
  fXMLDoc.CreateElement(
    SnippetNode,
    TCS4ImportExportDocHelper.HighlightSourceNodeName,
    IntToStr(Ord(Snippet.HiliteSource))
  );
  {TODO -cRefactor: Move code that writes Extra into TCS4FormatHelper}
  // extra info is written only if present
  if Snippet.Extra.HasContent then
    fXMLDoc.CreateElement(
      SnippetNode,
      TCS4ImportExportDocHelper.ExtraNodeName,
      TSnippetExtraHelper.BuildREMLMarkup(Snippet.Extra)
    );
  // write kind
  TCS4ImportExportDocHelper.WriteSnippetKind(fXMLDoc, SnippetNode, Snippet.Kind);
  // compiler results value: only write known results
  TCS4ImportExportDocHelper.WriteCompilerResults(
    fXMLDoc, SnippetNode, Snippet.Compatibility
  );
  // depends and units lists
  {TODO -cRefactor: Pull writing Depends node into TCS4FormatHelper}
  WriteReferenceList(
    SnippetNode,
    TCS4ImportExportDocHelper.DependsNodeName,
    SnippetKeys(Snippet.Depends)
  );
  WriteReferenceList(
    SnippetNode,
    TCS4ImportExportDocHelper.UnitsNodeName,
    TIStringList.Create(Snippet.Units)
  );
end;

procedure TCS4SnippetExporter.WriteSnippets(const ParentNode: IXMLNode);
var
  Node: IXMLNode;       // new snippets list node
  Snippet: TSnippet;    // refers to each exported snippet
begin
  // Add snippets list node
  Node := fXMLDoc.CreateElement(
    ParentNode, TCS4ImportExportDocHelper.SnippetsNodeName
  );
  // Add child node for each exported snippet
  for Snippet in fSnippets do
    WriteSnippet(Node, Snippet);
end;

{ TCS4SnippetImporter }

destructor TCS4SnippetImporter.Destroy;
begin
  fXMLDoc := nil;
  OleUninitialize;
  inherited;
end;

class procedure TCS4SnippetImporter.EnsureImportCategoryExists;
resourcestring
  ImportCatDesc = 'Imported Snippets';
var
  ImportCatData: TCategoryData;
begin
  if not Assigned(Database.Categories.Find(ImportCatID)) then
  begin
    ImportCatData.Init;
    ImportCatData.Desc := ImportCatDesc;
    (Database as IDatabaseEdit).AddCategory(ImportCatID, ImportCatData);
  end;
end;

procedure TCS4SnippetImporter.Execute(const Data: TBytes);

  ///  Reads list of units from under SnippetNode into Units list.
  procedure GetUnits(const SnippetNode: IXMLNode; Units: IStringList);
  var
    UnitNode: IXMLNode; // unit list node: nil if no list
  begin
    UnitNode := fXMLDoc.FindFirstChildNode(
      SnippetNode, TCS4ImportExportDocHelper.UnitsNodeName
    );
    Units.Clear;
    TCS4ImportExportDocHelper.GetPascalNameList(fXMLDoc, UnitNode, Units);
  end;

  ///  Reads list of a snippet's required snippets from under SnippetNode into
  ///  Depends list.
  {TODO -cRefactor: Pull reading Depends node into TCS4FormatHelper ???}
  procedure GetDepends(const SnippetNode: IXMLNode;
    const Depends: ISnippetIDList);
  var
    DependsNode: IXMLNode;      // depends node list: nil if no list
    SnippetNames: IStringList;  // list of names of snippets in depends list
    SnippetName: string;        // each snippet name in SnippetNames
  begin
    DependsNode := fXMLDoc.FindFirstChildNode(
      SnippetNode, TCS4ImportExportDocHelper.DependsNodeName
    );
    SnippetNames := TIStringList.Create;
    TCS4ImportExportDocHelper.GetPascalNameList(fXMLDoc, DependsNode, SnippetNames);
    Depends.Clear;
    for SnippetName in SnippetNames do
      // Note: in building snippet ID list we assume each snippet is from the
      // default vault. It may not be, but there is no way of telling from XML.
      Depends.Add(TSnippetID.Create(SnippetName, TVaultID.Default));
  end;

  // Reads description node and converts to active text.
  function GetDescription(const SnippetNode: IXMLNode): IActiveText;
  var
    Desc: string; // text read from description node
  begin
    Desc := TCS4ImportExportDocHelper.GetSubTagText(
      fXMLDoc, SnippetNode, TCS4ImportExportDocHelper.DescriptionNodeName
    );
    if Desc <> '' then
    begin
      if fVersion < 6 then
        // versions before 6: description is stored as plain text
        Result := TSnippetExtraHelper.PlainTextToActiveText(Desc)
      else
        // version 6 & later: description is stored as REML
        Result := TSnippetExtraHelper.BuildActiveText(Desc)
    end
    else
      Result := TActiveTextFactory.CreateActiveText;
  end;

resourcestring
  // Error message
  sParseError = 'Import file has an invalid format';
var
  SnippetNodes: IXMLSimpleNodeList; // list of snippet nodes
  SnippetNode: IXMLNode;            // each snippet node in list
  Idx: Integer;                     // loops thru snippet node list
begin
  // Load XML document
  try
    fXMLDoc.LoadFromBytes(Data);
    fXMLDoc.Active := True;

    // Validate loaded document and get version number
    fVersion := ValidateDoc;

    // Read in all snippets
    SnippetNodes := GetAllSnippetNodes;
    SetLength(fSnippetInfo, SnippetNodes.Count);
    for Idx := 0 to Pred(SnippetNodes.Count) do
    begin
      // Read a snippet node
      SnippetNode := SnippetNodes[Idx];
      fSnippetInfo[Idx].Key := SnippetNode.Attributes[
        TCS4ImportExportDocHelper.SnippetNodeNameAttr
      ];
      fSnippetInfo[Idx].Data :=
        (Database as IDatabaseEdit).GetEditableSnippetInfo;
      fSnippetInfo[Idx].Data.Props.Cat := ImportCatID;
      fSnippetInfo[Idx].Data.Props.Desc := GetDescription(SnippetNode);
      fSnippetInfo[Idx].Data.Props.DisplayName :=
        TCS4ImportExportDocHelper.GetSubTagText(
          fXMLDoc, SnippetNode, TCS4ImportExportDocHelper.DisplayNameNodeName
        );
      if fSnippetInfo[Idx].Data.Props.DisplayName = '' then
        fSnippetInfo[Idx].Data.Props.DisplayName := fSnippetInfo[Idx].Key;
      fSnippetInfo[Idx].Data.Props.SourceCode := TCS4ImportExportDocHelper.GetSubTagText(
        fXMLDoc, SnippetNode, TCS4ImportExportDocHelper.SourceCodeTextNodeName
      );
      fSnippetInfo[Idx].Data.Props.HiliteSource :=
        TCS4ImportExportDocHelper.GetHiliteSource(fXMLDoc, SnippetNode, True);
      // how we read extra property depends on version of file
      case fVersion of
        1:
          fSnippetInfo[Idx].Data.Props.Extra :=
            TSnippetExtraHelper.BuildActiveText(
              TCS4ImportExportDocHelper.GetSubTagText(
                fXMLDoc, SnippetNode, TCS4ImportExportDocHelper.CommentsNodeName
              ),
              TCS4ImportExportDocHelper.GetSubTagText(
                fXMLDoc, SnippetNode, TCS4ImportExportDocHelper.CreditsNodeName
              ),
              TCS4ImportExportDocHelper.GetSubTagText(
                fXMLDoc, SnippetNode, TCS4ImportExportDocHelper.CreditsUrlNodeName
              )
            );
        else // later versions
          fSnippetInfo[Idx].Data.Props.Extra :=
            TSnippetExtraHelper.BuildActiveText(
              TCS4ImportExportDocHelper.GetSubTagText(
                fXMLDoc, SnippetNode, TCS4ImportExportDocHelper.ExtraNodeName
              )
            );
      end;
      // how we read kind property depends on version of file
      case fVersion of
        1, 2:
          // for version 1 and 2, we have StandardFormat instead of Kind:
          // map standard format value onto a kind
          if TCS4ImportExportDocHelper.GetStandardFormat(fXMLDoc, SnippetNode, False) then
            fSnippetInfo[Idx].Data.Props.Kind := skRoutine
          else
            fSnippetInfo[Idx].Data.Props.Kind := skFreeform;
        else // later versions
          // for later versions we have Kind value: use Freeform if missing
          fSnippetInfo[Idx].Data.Props.Kind := TCS4ImportExportDocHelper.GetSnippetKind(
            fXMLDoc, SnippetNode, skFreeForm
          );
      end;
      fSnippetInfo[Idx].Data.Props.CompilerResults :=
        TCS4ImportExportDocHelper.GetCompilerResults(
          fXMLDoc, SnippetNode
        );
      GetUnits(SnippetNode, fSnippetInfo[Idx].Data.Refs.Units);
      GetDepends(SnippetNode, fSnippetInfo[Idx].Data.Refs.Depends);
      fSnippetInfo[Idx].Data.Refs.XRef.Clear;
    end;
  except
    on E: EDOMParseError do
      raise ECS4SnippetImporter.Create(sParseError);
    on E: ECodeSnipXML do
      raise ECS4SnippetImporter.Create(E);
    else
      raise;
  end;
end;

function TCS4SnippetImporter.GetAllSnippetNodes: IXMLSimpleNodeList;
var
  SnippetsNode: IXMLNode; // node under which all snippets are stored
begin
  SnippetsNode := fXMLDoc.FindNode(
    TCS4ImportExportDocHelper.ExportRootNodeName
      + '\'
      + TCS4ImportExportDocHelper.SnippetsNodeName
  );
  Result := fXMLDoc.FindChildNodes(
    SnippetsNode, TCS4ImportExportDocHelper.SnippetNodeName
  );
end;

class procedure TCS4SnippetImporter.ImportData(
  out SnippetInfo: TSnippetInfoList; const Data: TBytes);
var
  Idx: Integer; // loops through all imported snippets
  Instance: TCS4SnippetImporter;
begin
  Instance := InternalCreate;
  try
    Instance.Execute(Data);
    SetLength(SnippetInfo, Length(Instance.fSnippetInfo));
    for Idx := Low(Instance.fSnippetInfo) to High(Instance.fSnippetInfo) do
      SnippetInfo[Idx].Assign(Instance.fSnippetInfo[Idx]);
  finally
    Instance.Free;
  end;
end;

constructor TCS4SnippetImporter.InternalCreate;
begin
  inherited InternalCreate;
  OleInitialize(nil);
  fXMLDoc := TCS4ImportExportDocHelper.CreateXMLDoc;
  SetLength(fSnippetInfo, 0);
  EnsureImportCategoryExists;
end;

function TCS4SnippetImporter.ValidateDoc: Integer;
var
  SnippetsNode: IXMLNode;           // node where snippets are recorded
  SnippetNodes: IXMLSimpleNodeList; // list of nodes describing snippets
resourcestring
  // Error message
  sMissingNode = 'Invalid document: no <%s> node present';
begin
  TCS4ImportExportDocHelper.ValidateProcessingInstr(fXMLDoc);
  Result := TCS4ImportExportDocHelper.ValidateRootNode(
    fXMLDoc,
    TCS4ImportExportDocHelper.ExportRootNodeName,
    TCS4ImportExportDocHelper.Watermark,
    TRange.Create(
      TCS4ImportExportDocHelper.EarliestVersion, TCS4ImportExportDocHelper.LatestVersion
    )
  );
  // Must be a snippets node
  SnippetsNode := fXMLDoc.FindNode(
    TCS4ImportExportDocHelper.ExportRootNodeName
      + '\'
      + TCS4ImportExportDocHelper.SnippetsNodeName
  );
  if not Assigned(SnippetsNode) then
    raise ECS4SnippetImporter.CreateFmt(
      sMissingNode, [TCS4ImportExportDocHelper.SnippetsNodeName]
    );
  // Must be at least one snippet node
  SnippetNodes := fXMLDoc.FindChildNodes(
    SnippetsNode, TCS4ImportExportDocHelper.SnippetNodeName
  );
  if SnippetNodes.Count = 0 then
    raise ECS4SnippetImporter.CreateFmt(
      sMissingNode, [TCS4ImportExportDocHelper.SnippetNodeName]
    );
end;

{ TSnippetInfo }

procedure TSnippetInfo.Assign(const Src: TSnippetInfo);
begin
  Key := Src.Key;
  Data.Assign(Src.Data);
end;

procedure TSnippetInfo.Init;
begin
  Key := '';
  Data.Init;
end;

end.


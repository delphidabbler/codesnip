{
 * This Source Code Form is subject to the terms of the Mozilla Public License,
 * v. 2.0. If a copy of the MPL was not distributed with this file, You can
 * obtain one at https://mozilla.org/MPL/2.0/
 *
 * Copyright (C) 2008-2023, Peter Johnson (gravatar.com/delphidabbler).
 *
 * Implements classes that can import and export user defined snippets from and
 * to XML.
}


unit UCodeImportExport;


interface


uses
  // Delphi
  SysUtils,
  Classes,
  XMLIntf,
  // Project
  DB.USnippet,
  UBaseObjects,
  UEncodings,
  UIStringList,
  UXMLDocHelper,
  UXMLDocumentEx;


type
  ///  <summary>Encapsulates data that describes a snippet that has been read
  ///  from an import file.</summary>
  TSnippetInfo = record
    ///  <summary>Snippet name.</summary>
    Name: string;
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
  ///  <summary>Imports code snippets from XML.</summary>
  TCodeImporter = class(TNoPublicConstructObject)
  strict private
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
  ///  <summary>Class of exception raised when TCodeImporter encounters invalid
  ///  XML.</summary>
  ECodeImporter = class(ECodeSnipXML);

type
  ///  <summary>Exports code snippets to XML.</summary>
  TCodeExporter = class(TNoPublicConstructObject)
  strict private
    var
      ///  <summary>List of snippets to be exported.</summary>
      fSnippets: TSnippetList;
      ///  <summary>Extended XML document object.</summary>
      fXMLDoc: IXMLDocumentEx;
    ///  <summary>Examines given exception and converts into ECodeExporter if it
    ///  is an expected exception. Unexpected exceptions are re-raised.
    ///  </summary>
    ///  <exception>An exception is always raised.</exception>
    procedure HandleException(const EObj: TObject);
    ///  <summary>Returns a list of snippet names from snippets list.</summary>
    function SnippetNames(const SnipList: TSnippetList): IStringList;
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
  ///  <summary>Class of exception raised when TCodeExporter detects an expected
  ///  error.</summary>
  ECodeExporter = class(ECodeSnipXML);


implementation


uses
  // Delphi
  ActiveX,
  XMLDom,
  // Project
  ActiveText.UMain,
  DB.UCollections,
  DB.UMain,
  DB.USnippetKind,
  UAppInfo,
  UReservedCategories,
  USnippetExtraHelper,
  USnippetIDs,
  UStructs,
  UXMLDocConsts;


const
  // XML file markers: attributes of root node
  // watermark (never changes for all versions)
  cWatermark        = 'B46969D4-D367-4F5F-833E-F165FBA78631';
  // file version numbers
  cEarliestVersion  = 1;  // earliest file version supported by importer
  cLatestVersion    = 7;  // current file version written by exporter

{ TCodeExporter }

destructor TCodeExporter.Destroy;
begin
  fXMLDoc := nil;
  inherited;
end;

function TCodeExporter.Execute: TEncodedData;
var
  RootNode: IXMLNode; // document root node
resourcestring
  // Comment written to XML file
  sFileComment = 'This file was generated by CodeSnip. Do not edit.';
begin
  // Create and configure XML document
  fXMLDoc := TXMLDocHelper.CreateXMLDoc;
  try
    fXMLDoc.Active := True;

    // Add XML root nodes
    TXMLDocHelper.CreateXMLProcInst(fXMLDoc);
    TXMLDocHelper.CreateComment(fXMLDoc, sFileComment);
    RootNode := TXMLDocHelper.CreateRootNode(
      fXMLDoc, cExportRootNode, cWatermark, cLatestVersion
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

class function TCodeExporter.ExportSnippets(const SnipList: TSnippetList):
  TEncodedData;
var
  Instance: TCodeExporter;
begin
  Instance := InternalCreate(SnipList);
  try
    Result := Instance.Execute;
  finally
    Instance.Free;
  end;
end;

procedure TCodeExporter.HandleException(const EObj: TObject);
begin
  if (EObj is EFileStreamError) or (EObj is ECodeSnipXML) then
    raise ECodeExporter.Create(EObj as Exception);
  raise EObj;
end;

constructor TCodeExporter.InternalCreate(const SnipList: TSnippetList);
begin
  inherited InternalCreate;
  fSnippets := SnipList;
end;

function TCodeExporter.SnippetNames(
  const SnipList: TSnippetList): IStringList;
var
  Snippet: TSnippet;  // references each snippet in list
begin
  Result := TIStringList.Create;
  for Snippet in SnipList do
    Result.Add(Snippet.Key);
end;

procedure TCodeExporter.WriteProgInfo(const ParentNode: IXMLNode);
begin
  fXMLDoc.CreateElement(
    ParentNode, cProgVersionNode, TAppInfo.ProgramReleaseVersion
  );
end;

procedure TCodeExporter.WriteReferenceList(const ParentNode: IXMLNode;
  const ListNodeName: string; PasNames: IStringList);
begin
  // Don't write list tags if no items
  if PasNames.Count = 0 then
    Exit;
  // Write the list
  TXMLDocHelper.WritePascalNameList(
    fXMLDoc, ParentNode, ListNodeName, PasNames
  );
end;

procedure TCodeExporter.WriteSnippet(const ParentNode: IXMLNode;
  const Snippet: TSnippet);
var
  SnippetNode: IXMLNode; // new snippet node
begin
  // Create snippet node with attribute that specifies snippet key
  SnippetNode := fXMLDoc.CreateElement(ParentNode, cSnippetNode);
  SnippetNode.Attributes[cSnippetNameAttr] := Snippet.Key;
  // Add nodes for properties: (ignore category and xrefs)
  // description node is written even if empty (which it shouldn't be)
  fXMLDoc.CreateElement(
    SnippetNode,
    cDescriptionNode,
    TSnippetExtraHelper.BuildREMLMarkup(Snippet.Description)
  );
  // Snippet's display name is only written if different to Snippet's key
  if Snippet.Key <> Snippet.DisplayName then
    fXMLDoc.CreateElement(SnippetNode, cDisplayNameNode, Snippet.DisplayName);
  // source code is stored directly in XML, not in external file
  fXMLDoc.CreateElement(SnippetNode, cSourceCodeTextNode, Snippet.SourceCode);
  // write highlight source flag
  fXMLDoc.CreateElement(
    SnippetNode, cHighlightSource, IntToStr(Ord(Snippet.HiliteSource))
  );
  // extra info is written only if present
  if Snippet.Extra.HasContent then
    fXMLDoc.CreateElement(
      SnippetNode,
      cExtraNode,
      TSnippetExtraHelper.BuildREMLMarkup(Snippet.Extra)
    );
  // write kind
  TXMLDocHelper.WriteSnippetKind(fXMLDoc, SnippetNode, Snippet.Kind);
  // compiler results value: only write known results
  TXMLDocHelper.WriteCompilerResults(
    fXMLDoc, SnippetNode, Snippet.Compatibility
  );
  // depends and units lists
  WriteReferenceList(
    SnippetNode, cDependsNode, SnippetNames(Snippet.Depends)
  );
  WriteReferenceList(
    SnippetNode, cUnitsNode, TIStringList.Create(Snippet.Units)
  );
end;

procedure TCodeExporter.WriteSnippets(const ParentNode: IXMLNode);
var
  Node: IXMLNode;       // new snippets list node
  Snippet: TSnippet;    // refers to each exported snippet
begin
  // Add snippets list node
  Node := fXMLDoc.CreateElement(ParentNode, cSnippetsNode);
  // Add child node for each exported snippet
  for Snippet in fSnippets do
    WriteSnippet(Node, Snippet);
end;

{ TCodeImporter }

destructor TCodeImporter.Destroy;
begin
  fXMLDoc := nil;
  OleUninitialize;
  inherited;
end;

procedure TCodeImporter.Execute(const Data: TBytes);

  ///  Reads list of units from under SnippetNode into Units list.
  procedure GetUnits(const SnippetNode: IXMLNode; Units: IStringList);
  var
    UnitNode: IXMLNode; // unit list node: nil if no list
  begin
    UnitNode := fXMLDoc.FindFirstChildNode(SnippetNode, cUnitsNode);
    Units.Clear;
    TXMLDocHelper.GetPascalNameList(fXMLDoc, UnitNode, Units);
  end;

  ///  Reads list of a snippet's required snippets from under SnippetNode into
  ///  Depends list.
  procedure GetDepends(const SnippetNode: IXMLNode;
    const Depends: ISnippetIDList);
  var
    DependsNode: IXMLNode;      // depends node list: nil if no list
    SnippetNames: IStringList;  // list of names of snippets in depends list
    SnippetName: string;        // each snippet name in SnippetNames
  begin
    DependsNode := fXMLDoc.FindFirstChildNode(SnippetNode, cDependsNode);
    SnippetNames := TIStringList.Create;
    TXMLDocHelper.GetPascalNameList(fXMLDoc, DependsNode, SnippetNames);
    Depends.Clear;
    for SnippetName in SnippetNames do
      // Note: in building snippet ID list we assume each snippet is from the
      // standard user collection. It may not be, but there is no way of telling
      // from XML.
      Depends.Add(TSnippetID.Create(SnippetName, TCollectionID.__TMP__UserDBCollectionID));
  end;

  // Reads description node and converts to active text.
  function GetDescription(const SnippetNode: IXMLNode): IActiveText;
  var
    Desc: string; // text read from description node
  begin
    Desc := TXMLDocHelper.GetSubTagText(fXMLDoc, SnippetNode, cDescriptionNode);
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
      fSnippetInfo[Idx].Name := SnippetNode.Attributes[cSnippetNameAttr];
      fSnippetInfo[Idx].Data :=
        (Database as IDatabaseEdit).GetEditableSnippetInfo;
      fSnippetInfo[Idx].Data.Props.Cat := TReservedCategories.ImportsCatID;
      fSnippetInfo[Idx].Data.Props.Desc := GetDescription(SnippetNode);
      fSnippetInfo[Idx].Data.Props.DisplayName := TXMLDocHelper.GetSubTagText(
        fXMLDoc, SnippetNode, cDisplayNameNode
      );
      fSnippetInfo[Idx].Data.Props.SourceCode := TXMLDocHelper.GetSubTagText(
        fXMLDoc, SnippetNode, cSourceCodeTextNode
      );
      fSnippetInfo[Idx].Data.Props.HiliteSource := TXMLDocHelper.GetHiliteSource(
        fXMLDoc, SnippetNode, True
      );
      // how we read extra property depends on version of file
      case fVersion of
        1:
          fSnippetInfo[Idx].Data.Props.Extra :=
            TSnippetExtraHelper.BuildActiveText(
              TXMLDocHelper.GetSubTagText(fXMLDoc, SnippetNode, cCommentsNode),
              TXMLDocHelper.GetSubTagText(fXMLDoc, SnippetNode, cCreditsNode),
              TXMLDocHelper.GetSubTagText(fXMLDoc, SnippetNode, cCreditsUrlNode)
            );
        else // later versions
          fSnippetInfo[Idx].Data.Props.Extra :=
            TSnippetExtraHelper.BuildActiveText(
              TXMLDocHelper.GetSubTagText(fXMLDoc, SnippetNode, cExtraNode)
            );
      end;
      // how we read kind property depends on version of file
      case fVersion of
        1, 2:
          // for version 1 and 2, we have StandardFormat instead of Kind:
          // map standard format value onto a kind
          if TXMLDocHelper.GetStandardFormat(fXMLDoc, SnippetNode, False) then
            fSnippetInfo[Idx].Data.Props.Kind := skRoutine
          else
            fSnippetInfo[Idx].Data.Props.Kind := skFreeform;
        else // later versions
          // for later versions we have Kind value: use Freeform if missing
          fSnippetInfo[Idx].Data.Props.Kind := TXMLDocHelper.GetSnippetKind(
            fXMLDoc, SnippetNode, skFreeForm
          );
      end;
      fSnippetInfo[Idx].Data.Props.CompilerResults :=
        TXMLDocHelper.GetCompilerResults(
          fXMLDoc, SnippetNode
        );
      GetUnits(SnippetNode, fSnippetInfo[Idx].Data.Refs.Units);
      GetDepends(SnippetNode, fSnippetInfo[Idx].Data.Refs.Depends);
      fSnippetInfo[Idx].Data.Refs.XRef.Clear;
    end;
  except
    on E: EDOMParseError do
      raise ECodeImporter.Create(sParseError);
    on E: ECodeSnipXML do
      raise ECodeImporter.Create(E);
    else
      raise;
  end;
end;

function TCodeImporter.GetAllSnippetNodes: IXMLSimpleNodeList;
var
  SnippetsNode: IXMLNode; // node under which all snippets are stored
begin
  SnippetsNode := fXMLDoc.FindNode(cExportRootNode + '\' + cSnippetsNode);
  Result := fXMLDoc.FindChildNodes(SnippetsNode, cSnippetNode);
end;

class procedure TCodeImporter.ImportData(out SnippetInfo: TSnippetInfoList;
  const Data: TBytes);
var
  Idx: Integer; // loops through all imported snippets
  Instance: TCodeImporter;
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

constructor TCodeImporter.InternalCreate;
begin
  inherited InternalCreate;
  // Set up XML document that will read data
  OleInitialize(nil);
  fXMLDoc := TXMLDocHelper.CreateXMLDoc;
  // Initialise fields that receive imported data
  SetLength(fSnippetInfo, 0);
end;

function TCodeImporter.ValidateDoc: Integer;
var
  SnippetsNode: IXMLNode;           // node where snippets are recorded
  SnippetNodes: IXMLSimpleNodeList; // list of nodes describing snippets
resourcestring
  // Error message
  sMissingNode = 'Invalid document: no <%s> node present';
begin
  TXMLDocHelper.ValidateProcessingInstr(fXMLDoc);
  Result := TXMLDocHelper.ValidateRootNode(
    fXMLDoc,
    cExportRootNode,
    cWatermark,
    TRange.Create(cEarliestVersion, cLatestVersion)
  );
  // Must be a snippets node
  SnippetsNode := fXMLDoc.FindNode(cExportRootNode + '\' + cSnippetsNode);
  if not Assigned(SnippetsNode) then
    raise ECodeImporter.CreateFmt(sMissingNode, [cSnippetsNode]);
  // Must be at least one snippet node
  SnippetNodes := fXMLDoc.FindChildNodes(SnippetsNode, cSnippetNode);
  if SnippetNodes.Count = 0 then
    raise ECodeImporter.CreateFmt(sMissingNode, [cSnippetNode]);
end;

{ TSnippetInfo }

procedure TSnippetInfo.Assign(const Src: TSnippetInfo);
begin
  Name := Src.Name;
  Data.Assign(Src.Data);
end;

procedure TSnippetInfo.Init;
begin
  Name := '';
  Data.Init;
end;

end.


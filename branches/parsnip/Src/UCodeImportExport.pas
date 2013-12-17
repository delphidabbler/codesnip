{
 * This Source Code Form is subject to the terms of the Mozilla Public License,
 * v. 2.0. If a copy of the MPL was not distributed with this file, You can
 * obtain one at http://mozilla.org/MPL/2.0/
 *
 * Copyright (C) 2008-2013, Peter Johnson (www.delphidabbler.com).
 *
 * $Rev$
 * $Date$
 *
 * Implements classes that can import and export snippets and user information
 * from and to export files.
}


unit UCodeImportExport;

// TODO: change to support export to new db format
// TODO: revisit some commenting re XML when support added for new db format

interface


uses
  // Delphi
  SysUtils,
  Classes,
  XMLIntf,
  // Project
  CS.Database.Types,
  UBaseObjects,
  UEncodings,
  UExceptions,
  UIStringList,
  UUserDetails,
  UXMLDocHelper,
  UXMLDocumentEx;


type
  ///  <summary>Encapsulates data that describes a snippet that has been read
  ///  from an import file.</summary>
  TSnippetInfo = record
    ///  <summary>Snippet name.</summary>
    Name: string;
    ///  <summary>Description of snippet.</summary>
    Snippet: IEditableSnippet;
  end;

type
  ///  <summary>Dynamic array of TSnippetInfo records.</summary>
  TSnippetInfoList = array of TSnippetInfo;

type
  ///  <summary>Encapsulates user info from export files.</summary>
  TUserInfo = record
  strict private
    var
      fDetails: TUserDetails;
      fComments: string;
  public
    ///  <summary>User's personal details.</summary>
    property Details: TUserDetails read fDetails;
    ///  <summary>User's comments.</summary>
    property Comments: string read fComments;
    ///  <summary>Initialises record to given values.</summary>
    constructor Create(const UserDetails: TUserDetails;
      const UserComments: string);
    ///  <summary>Returns a new record with null field values.</summary>
    class function CreateNull: TUserInfo; static;
    ///  <summary>Checks if record is null, i.e. empty.</summary>
    function IsNull: Boolean;
  end;

type
  ///  <summary>Imports code snippets and user info from export files.</summary>
  TCodeImporter = class(TNoPublicConstructObject)
  strict private
    ///  <summary>Version of file being imported.</summary>
    fVersion: Integer;
    ///  <summary>Information about user who created export.</summary>
    fUserInfo: TUserInfo;
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
    ///  <summary>Imports snippets and optional user data from XML.</summary>
    ///  <param name="UserInfo">TUserInfo [out] Receives user information. Set
    ///  to null if no user information was available.</param>
    ///  <param name="SnippetInfo">TSnippetInfoList [out] Receives information
    ///  about each imported snippet.</param>
    ///  <param name="Data">TBytes [in] Byte array containing XML data.</param>
    class procedure ImportData(out UserInfo: TUserInfo;
      out SnippetInfo: TSnippetInfoList; const Data: TBytes);
  end;

type
  ///  <summary>Class of exception raised when TCodeImporter encounters invalid
  ///  XML.</summary>
  ECodeImporter = class(ECodeSnipXML);

type
  ///  <summary>Exports code snippets and user info to XML.</summary>
  TCodeExporter = class(TNoPublicConstructObject)
  strict private
    var
      ///  <summary>User information to be written to XML.</summary>
      fUserInfo: TUserInfo;
      ///  <summary>IDs of snippets to be exported.</summary>
      fSnippetIDs: ISnippetIDList;
      ///  <summary>Extended XML document object.</summary>
      fXMLDoc: IXMLDocumentEx;
    ///  <summary>Examines given exception and converts into ECodeExporter if it
    ///  is an expected exception. Unexpected exceptions are re-raised.
    ///  </summary>
    ///  <exception>An exception is always raised.</exception>
    procedure HandleException(const EObj: TObject);
    ///  <summary>Returns a list of snippet names from snippets list.</summary>
    function SnippetNames(const SnipList: ISnippetIDList): IStringList;
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
    ///  <summary>Writes a node and sub-nodes containing any information about
    ///  user who created export file.</summary>
    ///  <param name="ParentNode">IXMLNode [in] Node under which user info node
    ///  is to be written.</param>
    procedure WriteUserInfo(const ParentNode: IXMLNode);
    ///  <summary>Writes nodes containing details of all exported snippets.
    ///  </summary>
    ///  <param name="ParentNode">IXMLNode [in] Node under which snippets are to
    ///  be written.</param>
    procedure WriteSnippets(const ParentNode: IXMLNode);
    ///  <summary>Writes an XML snippet node and child nodes that describe a
    ///  snippet.</summary>
    ///  <param name="ParentNode">IXMLNode [in] Node under which snippet node is
    ///  to be written.</param>
    ///  <param name="SnippetID">TSnippetID [in] ID of snippet to be written to
    ///  XML.</param>
    procedure WriteSnippet(const ParentNode: IXMLNode;
      const SnippetID: TSnippetID);
    ///  <summary>Performs the export.</summary>
    ///  <returns>TEncodedData. Encoded data containing exported XML.</returns>
    ///  <exception>ECodeExporter raised if a known error is encountered.
    ///  </exception>
    function Execute: TEncodedData;
    ///  <summary>Constructs and initialises object ready to perform export.
    ///  </summary>
    ///  <param name="UserInfo">TUserInfo [in] User information to be exported.
    ///  Ignored if null.</param>
    ///  <param name="SnipList">ISnippetIDList [in] IDs of snippets to be
    ///  exported.</param>
    constructor InternalCreate(const UserInfo: TUserInfo;
      SnipList: ISnippetIDList);
  public
    ///  <summary>Destroys object.</summary>
    destructor Destroy; override;
    ///  <summary>Exports user information and snippets as XML.</summary>
    ///  <param name="UserInfo">TUserInfo [in] User information to be exported.
    ///  Ignored if null.</param>
    ///  <param name="SnipList">ISnippetIDList [in] IDs of snippets to be
    ///  exported.</param>
    ///  <returns>TEncodedData. Encoded data containing exported XML.</returns>
    class function ExportSnippets(const UserInfo: TUserInfo;
      SnipList: ISnippetIDList): TEncodedData;
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
  CS.ActiveText,
  CS.ActiveText.Helper,
  CS.ActiveText.Renderers.REML,
  CS.Database.Snippets,
  CS.Database.Tags,
  CS.SourceCode.Languages,
  DB.UMain,
  IntfCommon,
  UAppInfo,
  UConsts,
  UREMLDataIO,
  UStructs,
  UStrUtils,
  UXMLDocConsts;


const
  // XML file markers: attributes of root node
  // watermark (never changes for all versions)
  cWatermark        = 'B46969D4-D367-4F5F-833E-F165FBA78631';
  // file version numbers
  cEarliestVersion  = 1;  // earliest file version supported by importer
  cLatestVersion    = 6;  // current file version written by exporter


{ TUserInfo }

constructor TUserInfo.Create(const UserDetails: TUserDetails;
  const UserComments: string);
begin
  fDetails := UserDetails;
  fComments := UserComments;
end;

class function TUserInfo.CreateNull: TUserInfo;
begin
  Result := TUserInfo.Create(TUserDetails.CreateNull, '');
end;

function TUserInfo.IsNull: Boolean;
begin
  Result := Details.IsNull and (Comments = '');
end;

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
    if not fUserInfo.IsNull then
      WriteUserInfo(RootNode);
    WriteSnippets(RootNode);

    // Save XML as UTF-8 with no BOM
    fXMLDoc.Encoding := 'UTF-8';
    Result := TEncodedData.Create(fXMLDoc.SaveToBytes, etUTF8);
  except
    HandleException(ExceptObject);
  end;
end;

class function TCodeExporter.ExportSnippets(const UserInfo: TUserInfo;
  SnipList: ISnippetIDList): TEncodedData;
begin
  with InternalCreate(UserInfo, SnipList) do
    try
      Result := Execute;
    finally
      Free;
    end;
end;

procedure TCodeExporter.HandleException(const EObj: TObject);
begin
  if (EObj is EFileStreamError) or (EObj is ECodeSnipXML) then
    raise ECodeExporter.Create(EObj as Exception);
  raise EObj;
end;

constructor TCodeExporter.InternalCreate(const UserInfo: TUserInfo;
  SnipList: ISnippetIDList);
begin
  inherited InternalCreate;
  fSnippetIDs := (SnipList as IClonable).Clone as ISnippetIDList;
  fUserInfo := UserInfo;
end;

function TCodeExporter.SnippetNames(
  const SnipList: ISnippetIDList): IStringList;
var
  SnippetID: TSnippetID;  // references each snippet in list
begin
  Result := TIStringList.Create;
  for SnippetID in SnipList do
    Result.Add(SnippetID.ToString);
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
  if PasNames.IsEmpty then
    Exit;
  // Write the list
  TXMLDocHelper.WritePascalNameList(
    fXMLDoc, ParentNode, ListNodeName, PasNames
  );
end;

procedure TCodeExporter.WriteSnippet(const ParentNode: IXMLNode;
  const SnippetID: TSnippetID);
var
  SnippetNode: IXMLNode;  // new snippet node
  Snippet: ISnippet;      // snippet being written out
begin
  Snippet := Database.LookupSnippet(SnippetID);
  // Create snippet node with attribute that specifies snippet name
  SnippetNode := fXMLDoc.CreateElement(ParentNode, cSnippetNode);
  SnippetNode.Attributes[cSnippetNameAttr] := SnippetID.ToString;
  // Add nodes for properties: (ignore category and xrefs)
  // description node is written even if empty (which it shouldn't be)
  fXMLDoc.CreateElement(
    SnippetNode,
    cDescriptionNode,
    TActiveTextREMLRenderer.Render(Snippet.Description, EOL)
  );
  // Snippet's display name is only written if different to Snippet's name
  if Snippet.ID.ToString <> Snippet.Title then
    fXMLDoc.CreateElement(SnippetNode, cDisplayNameNode, Snippet.Title);
  // source code is stored directly in XML, not in external file
  fXMLDoc.CreateElement(SnippetNode, cSourceCodeTextNode, Snippet.SourceCode);
  // write highlight source flag
  fXMLDoc.CreateElement(
    SnippetNode,
    cHighlightSource,
    IntToStr(Ord(Snippet.LanguageID = TSourceCodeLanguageID.Create('Pascal')))
  );
  // "extra" tag is written only if snippet has a non-empty Notes property
  if not Snippet.Notes.IsEmpty then
    fXMLDoc.CreateElement(
      SnippetNode,
      cExtraNode,
      TActiveTextREMLRenderer.Render(Snippet.Notes, EOL)
    );
  // write kind
  TXMLDocHelper.WriteSnippetKindID(fXMLDoc, SnippetNode, Snippet.KindID);
  // compiler results value: only write known results
  TXMLDocHelper.WriteCompilerResults(
    fXMLDoc, SnippetNode, Snippet.CompileResults
  );
  // depends and units lists
  WriteReferenceList(
    SnippetNode, cDependsNode, SnippetNames(Snippet.RequiredSnippets)
  );
  WriteReferenceList(
    SnippetNode, cUnitsNode, TIStringList.Create(Snippet.RequiredModules)
  );
end;

procedure TCodeExporter.WriteSnippets(const ParentNode: IXMLNode);
var
  Node: IXMLNode;         // new snippets list node
  SnippetID: TSnippetID;  // ID of each exported snippet
begin
  // Add snippets list node
  Node := fXMLDoc.CreateElement(ParentNode, cSnippetsNode);
  // Add child node for each exported snippet
  for SnippetID in fSnippetIDs do
    WriteSnippet(Node, SnippetID);
end;

procedure TCodeExporter.WriteUserInfo(const ParentNode: IXMLNode);
var
  UserInfoNode: IXMLNode; // new user info parent node
begin
  // Add user info node
  UserInfoNode := fXMLDoc.CreateElement(ParentNode, cUserInfoNode);
  // Add separate child node for each piece of user info
  fXMLDoc.CreateElement(UserInfoNode, cUserNameNode, fUserInfo.Details.Name);
  fXMLDoc.CreateElement(UserInfoNode, cUserEmailNode, fUserInfo.Details.Email);
  fXMLDoc.CreateElement(UserInfoNode, cUserCommentsNode, fUserInfo.Comments);
end;

{ TCodeImporter }

destructor TCodeImporter.Destroy;
begin
  fXMLDoc := nil;
  OleUninitialize;
  inherited;
end;

procedure TCodeImporter.Execute(const Data: TBytes);

  // Returns list of required modules (units) from under SnippetNode.
  { TODO: the following code that builds required snippets list is broken:
          imported snippet IDs are wrong if required snippet is also in import
          file (the ID it had when exported is used. Some magic with linked
          spaces is probably needed. }
  function GetRequiredModules(const SnippetNode: IXMLNode): IStringList;
  var
    UnitNode: IXMLNode; // unit list node: nil if no list
  begin
    UnitNode := fXMLDoc.FindFirstChildNode(SnippetNode, cUnitsNode);
    Result := TIStringList.Create;
    TXMLDocHelper.GetPascalNameList(fXMLDoc, UnitNode, Result);
  end;

  // Returns list of required snippets from under SnippetNode.
  function GetRequiredSnippets(const SnippetNode: IXMLNode): ISnippetIDList;
  var
    DependsNode: IXMLNode;      // depends node list: nil if no list
    SnippetNames: IStringList;  // list of names of snippets in depends list
    SnippetName: string;        // each snippet name in SnippetNames
  begin
    DependsNode := fXMLDoc.FindFirstChildNode(SnippetNode, cDependsNode);
    SnippetNames := TIStringList.Create;
    TXMLDocHelper.GetPascalNameList(fXMLDoc, DependsNode, SnippetNames);
    Result := TSnippetIDList.Create;
    for SnippetName in SnippetNames do
      Result.Add(TSnippetID.Create(SnippetName));
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
        Result := TActiveTextHelper.ParsePlainText(Desc)
      else
        // version 6 & later: description is stored as REML
        Result := TActiveTextHelper.ParseREML(Desc)
    end
    else
      Result := TActiveTextFactory.CreateActiveText;
  end;

  // Gets value of Title property: uses value of display-name node if present,
  // otherwise uses snippet ID.
  function GetTitleProperty(const SnippetNode: IXMLNode; const IDStr: string):
    string;
  begin
    Result := TXMLDocHelper.GetSubTagText(
      fXMLDoc, SnippetNode, cDisplayNameNode
    );
    if StrIsBlank(Result) then
      Result := IDStr;
  end;

  // Constructs a tag set containing a single "imported" tag.
  function BuildTags: ITagSet;
  resourcestring
    sImportTagStr = 'Imported';
  begin
    Result := TTagSet.Create;
    Result.Add(TTag.Create(sImportTagStr));
  end;

resourcestring
  sParseError = 'Import file has an invalid format';
var
  UserNode: IXMLNode;               // node containing any user info
  SnippetNodes: IXMLSimpleNodeList; // list of snippet nodes
  SnippetNode: IXMLNode;            // each snippet node in list
  Idx: Integer;                     // loops thru snippet node list
  Snippet: IEditableSnippet;        // each snippet read from file
begin
  // Load XML document
  try
    fXMLDoc.LoadFromBytes(Data);
    fXMLDoc.Active := True;

    // Validate loaded document and get version number
    fVersion := ValidateDoc;

    // Get user info
    UserNode :=  fXMLDoc.FindNode(cExportRootNode + '\' + cUserInfoNode);
    if Assigned(UserNode) then
    begin
      fUserInfo := TUserInfo.Create(
        TUserDetails.Create(
          TXMLDocHelper.GetSubTagText(fXMLDoc, UserNode, cUserNameNode),
          TXMLDocHelper.GetSubTagText(fXMLDoc, UserNode, cUserEmailNode)
        ),
        TXMLDocHelper.GetSubTagText(fXMLDoc, UserNode, cUserCommentsNode)
      );
    end;

    // Read in all snippets
    SnippetNodes := GetAllSnippetNodes;
    SetLength(fSnippetInfo, SnippetNodes.Count);
    for Idx := 0 to Pred(SnippetNodes.Count) do
    begin
      // Read a snippet node
      SnippetNode := SnippetNodes[Idx];
      fSnippetInfo[Idx].Name := SnippetNode.Attributes[cSnippetNameAttr];
      Snippet := Database.NewSnippet;
      Snippet.Tags := BuildTags;
      Snippet.Description := GetDescription(SnippetNode);
      Snippet.Title := GetTitleProperty(SnippetNode, fSnippetInfo[Idx].Name);
      Snippet.SourceCode := TXMLDocHelper.GetSubTagText(
        fXMLDoc, SnippetNode, cSourceCodeTextNode
      );
      if TXMLDocHelper.GetHiliteSource(fXMLDoc, SnippetNode, True) then
        Snippet.LanguageID := TSourceCodeLanguageID.Create('Pascal')
      else
        Snippet.LanguageID := TSourceCodeLanguageID.Create('Text');
      // how we read Notes property depends on version of file
      case fVersion of
        1:
          Snippet.Notes := TActiveTextHelper.ParseCommentsAndCredits(
            TXMLDocHelper.GetSubTagText(fXMLDoc, SnippetNode, cCommentsNode),
            TXMLDocHelper.GetSubTagText(fXMLDoc, SnippetNode, cCreditsNode),
            TXMLDocHelper.GetSubTagText(fXMLDoc, SnippetNode, cCreditsUrlNode)
          );
        else // later versions
          Snippet.Notes := TActiveTextHelper.ParseREML(
            TXMLDocHelper.GetSubTagText(fXMLDoc, SnippetNode, cExtraNode)
          );
      end;
      // how we read kind property depends on version of file
      case fVersion of
        1, 2:
          // for version 1 and 2, we have StandardFormat instead of Kind:
          // map standard format value onto a kind
          if TXMLDocHelper.GetStandardFormat(fXMLDoc, SnippetNode, False) then
            Snippet.KindID := skRoutine
          else
            Snippet.KindID := skFreeform;
        else // later versions
          // for later versions we have Kind value: use Freeform if missing
          Snippet.KindID := TXMLDocHelper.GetSnippetKindID(
            fXMLDoc, SnippetNode, skFreeForm
          );
      end;
      Snippet.CompileResults := TXMLDocHelper.GetCompilerResults(
        fXMLDoc, SnippetNode
      );
      Snippet.RequiredModules := GetRequiredModules(SnippetNode);
      Snippet.RequiredSnippets := GetRequiredSnippets(SnippetNode);
      // No cross references stored in export file
      fSnippetInfo[Idx].Snippet := Snippet;
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

class procedure TCodeImporter.ImportData(out UserInfo: TUserInfo;
  out SnippetInfo: TSnippetInfoList; const Data: TBytes);
var
  Idx: Integer; // loops through all imported snippets
begin
  with InternalCreate do
    try
      Execute(Data);
      UserInfo := fUserInfo;
      SetLength(SnippetInfo, Length(fSnippetInfo));
      for Idx := Low(fSnippetInfo) to High(fSnippetInfo) do
        SnippetInfo[Idx] := fSnippetInfo[Idx];
    finally
      Free;
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
  fUserInfo := TUserInfo.CreateNull;
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

end.


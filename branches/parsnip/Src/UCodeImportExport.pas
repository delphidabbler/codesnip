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
 * Implements classes that can import and export user defined snippets and user
 * information from and to XML.
}


unit UCodeImportExport;


interface


uses
  // Delphi
  SysUtils,
  Classes,
  XMLIntf,
  // Project
  CS.Database.Types,
  DB.USnippet,
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
  ///  <summary>Encapsulates user info from export files.</summary>
  TUserInfo = record
    ///  <summary>User's personal details.</summary>
    Details: TUserDetails;
    ///  <summary>User's comments.</summary>
    Comments: string;
    ///  <summary>Initialises record to given values.</summary>
    constructor Create(const UserDetails: TUserDetails;
      const UserComments: string);
    ///  <summary>Returns a new record with null field values.</summary>
    class function CreateNul: TUserInfo; static;
    ///  <summary>Copies given TUserInfo record to this one.</summary>
    procedure Assign(const Src: TUserInfo);
    ///  <summary>Initialises record to null value.</summary>
    procedure Init;
    ///  <summary>Checks if record is null, i.e. empty.</summary>
    function IsNul: Boolean;
  end;

type
  ///  <summary>Imports code snippets and user info from XML.</summary>
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
  DB.UMain,
  IntfCommon,
  UAppInfo,
  UConsts,
  UREMLDataIO,
  UReservedCategories,
  UStructs,
  UXMLDocConsts;


const
  // XML file markers: attributes of root node
  // watermark (never changes for all versions)
  cWatermark        = 'B46969D4-D367-4F5F-833E-F165FBA78631';
  // file version numbers
  cEarliestVersion  = 1;  // earliest file version supported by importer
  cLatestVersion    = 6;  // current file version written by exporter


{ TUserInfo }

procedure TUserInfo.Assign(const Src: TUserInfo);
begin
  Details.Assign(Src.Details);
  Comments := Src.Comments;
end;

constructor TUserInfo.Create(const UserDetails: TUserDetails;
  const UserComments: string);
begin
  Details := UserDetails;
  Comments := UserComments;
end;

class function TUserInfo.CreateNul: TUserInfo;
begin
  Result.Init;
end;

procedure TUserInfo.Init;
begin
  Details.Init;
  Comments := '';
end;

function TUserInfo.IsNul: Boolean;
begin
  Result := Details.IsNul and (Comments = '');
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
    if not fUserInfo.IsNul then
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
  if PasNames.Count = 0 then
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
  Snippet: TSnippet;      // snippet being written out
begin
  Snippet := _Database.Lookup(SnippetID);
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
    SnippetNode, cHighlightSource, IntToStr(Ord(Snippet.HiliteSource))
  );
  // "extra" tag is written only if snippet has a non-empty Notes property
  if not Snippet.Notes.IsEmpty then
    fXMLDoc.CreateElement(
      SnippetNode,
      cExtraNode,
      TActiveTextREMLRenderer.Render(Snippet.Notes, EOL)
    );
  // write kind
  TXMLDocHelper.WriteSnippetKind(fXMLDoc, SnippetNode, Snippet.Kind);
  // compiler results value: only write known results
  TXMLDocHelper.WriteCompilerResults(
    fXMLDoc, SnippetNode, Snippet.Compatibility
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
      // Note: in building snippet ID list we assume each snippet is user-
      // defined. It may not be, but there is no way of telling from XML.
      Depends.Add(TSnippetID.Create(SnippetName));
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

resourcestring
  sParseError = 'Import file has an invalid format';
  sImportTagStr = 'Imported';
var
  UserNode: IXMLNode;               // node containing any user info
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

    // Get user info
    UserNode :=  fXMLDoc.FindNode(cExportRootNode + '\' + cUserInfoNode);
    if Assigned(UserNode) then
    begin
      fUserInfo.Details.Name := TXMLDocHelper.GetSubTagText(
        fXMLDoc, UserNode, cUserNameNode
      );
      fUserInfo.Details.Email := TXMLDocHelper.GetSubTagText(
        fXMLDoc, UserNode, cUserEmailNode
      );
      fUserInfo.Comments := TXMLDocHelper.GetSubTagText(
        fXMLDoc, UserNode, cUserCommentsNode
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
      fSnippetInfo[Idx].Data :=
        (_Database as IDatabaseEdit).GetEditableSnippetInfo;
      with fSnippetInfo[Idx].Data do
      begin
        Props.Cat := TReservedCategories.ImportsCatID;
        Props.Tags.Add(TTag.Create(sImportTagStr));
        Props.Desc := GetDescription(SnippetNode);
        Props.Title := TXMLDocHelper.GetSubTagText(
          fXMLDoc, SnippetNode, cDisplayNameNode
        );
        Props.SourceCode := TXMLDocHelper.GetSubTagText(
          fXMLDoc, SnippetNode, cSourceCodeTextNode
        );
        Props.HiliteSource := TXMLDocHelper.GetHiliteSource(
          fXMLDoc, SnippetNode, True
        );
        // how we read Notes property depends on version of file
        case fVersion of
          1:
            Props.Notes := TActiveTextHelper.ParseCommentsAndCredits(
              TXMLDocHelper.GetSubTagText(fXMLDoc, SnippetNode, cCommentsNode),
              TXMLDocHelper.GetSubTagText(fXMLDoc, SnippetNode, cCreditsNode),
              TXMLDocHelper.GetSubTagText(fXMLDoc, SnippetNode, cCreditsUrlNode)
            );
          else // later versions
            Props.Notes := TActiveTextHelper.ParseREML(
              TXMLDocHelper.GetSubTagText(fXMLDoc, SnippetNode, cExtraNode)
            );
        end;
        // how we read kind property depends on version of file
        case fVersion of
          1, 2:
            // for version 1 and 2, we have StandardFormat instead of Kind:
            // map standard format value onto a kind
            if TXMLDocHelper.GetStandardFormat(fXMLDoc, SnippetNode, False) then
              Props.Kind := skRoutine
            else
              Props.Kind := skFreeform;
          else // later versions
            // for later versions we have Kind value: use Freeform if missing
            Props.Kind := TXMLDocHelper.GetSnippetKind(
              fXMLDoc, SnippetNode, skFreeForm
            );
        end;
        Props.CompilerResults := TXMLDocHelper.GetCompilerResults(
          fXMLDoc, SnippetNode
        );
        GetUnits(SnippetNode, Refs.RequiredModules);
        GetDepends(SnippetNode, Refs.RequiredSnippets);
        Refs.XRefs.Clear;
      end;
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
      UserInfo.Assign(fUserInfo);
      SetLength(SnippetInfo, Length(fSnippetInfo));
      for Idx := Low(fSnippetInfo) to High(fSnippetInfo) do
        SnippetInfo[Idx].Assign(fSnippetInfo[Idx]);
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
  fUserInfo.Init;
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


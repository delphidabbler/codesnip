{
 * UCodeImportExport.pas
 *
 * Implements classes that can import and export user defined snippets and user
 * information from and to XML.
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
 * The Original Code is UCodeImportExport.pas
 *
 * The Initial Developer of the Original Code is Peter Johnson
 * (http://www.delphidabbler.com/).
 *
 * Portions created by the Initial Developer are Copyright (C) 2008-2011 Peter
 * Johnson. All Rights Reserved.
 *
 * Contributor(s)
 *   NONE
 *
 * ***** END LICENSE BLOCK *****
}


unit UCodeImportExport;


interface


uses
  // Delphi
  SysUtils, Classes, XMLIntf,
  // Project
  DB.USnippet, UBaseObjects, UEncodings,  UExceptions, UIStringList,
  UUserDetails, UXMLDocHelper, UXMLDocumentEx;


type

  {
  TSnippetInfo:
    Record that encapsulates data read from an import file that describes a
    snippet.
  }
  TSnippetInfo = record
    Name: string;           // Snippet name
    Data: TSnippetEditData; // Describes a snippet
    procedure Assign(const Src: TSnippetInfo);
      {Sets this record's fields to be same as another TSnippetInfo record.
      Object fields are copied appropriately.
        @param Src [in] Record containing fields to be copied.
      }
    procedure Init;
      {Initialises record to nul values.
      }
  end;

  {
  TSnippetInfoList:
    Dynamic array of TSnippetInfo records.
  }
  TSnippetInfoList = array of TSnippetInfo;

  {
  TUserInfo:
    Record that encapsulates user info optionally stored in export files.
  }
  TUserInfo = record
    Details: TUserDetails;  // User's personal details
    Comments: string;       // User's comments
    constructor Create(const UserDetails: TUserDetails;
      const UserComments: string);
      {Initialises all a fields of a record.
        @param UserDetails [in] Information about user.
        @param UserComments [in] User's comments.
      }
    class function CreateNul: TUserInfo; static;
      {Create a new nul record.
        @return Required initialised record.
      }
    procedure Assign(const Src: TUserInfo);
      {Sets this record's fields to be same as another TUserInfo record.
        @param Src [in] Record containing fields to be copied.
      }
    procedure Init;
      {Initialises record to nul values.
      }
    function IsNul: Boolean;
      {Checks if record is nul (empty).
        @return True if record is nul, False if not.
      }
  end;

  {
  TCodeImporter:
    Class that imports code snippets and user info from XML.
  }
  TCodeImporter = class(TNoPublicConstructObject)
  strict private
    fVersion: Integer;              // Version of file being imported
    fUserInfo: TUserInfo;           // Information about user who created export
    fSnippetInfp: TSnippetInfoList; // List of snippets read from XML
    fXMLDoc: IXMLDocumentEx;        // Extended XML document object
    function GetAllSnippetNodes: IXMLSimpleNodeList;
      {Retrieves a list of all snippet nodes in XML document.
        @return Required node list.
      }
    procedure Execute(const Data: TBytes);
      {Performs the import.
        @param Data [in] Byte array containing XML data.
      }
    function ValidateDoc: Integer;
      {Validates XML document read from stream and gets file version.
        @return XML file version number.
        @except ECodeImporter raised if XML is not valid.
      }
    constructor InternalCreate;
      {Private class constructor. Sets up object to import data.
      }
  public
    destructor Destroy; override;
      {Class destructor. Tidies up object.
      }
    class procedure ImportData(out UserInfo: TUserInfo;
      out SnippetInfo: TSnippetInfoList; const Data: TBytes);
      {Imports snippets and optional user data from XML.
        @param UserInfo [out] Receives user info. Set to nul if no user info was
          available.
        @param SnippetInfo [out] Receives information about each snippet read.
        @param Data [in] Byte array containing XML data.
      }
  end;

  {
  ECodeImporter:
    Class of exception raised when TCodeImporter encounters invalid XML.
  }
  ECodeImporter = class(ECodeSnipXML);

  {
  TCodeExporter:
    Class that exports code snippets and user info to XML.
  }
  TCodeExporter = class(TNoPublicConstructObject)
  strict private
    var fUserInfo: TUserInfo;     // User information to be written to XML
    var fSnippets: TSnippetList;  // List of snippets to be exported
    var fXMLDoc: IXMLDocumentEx;  // Extended XML document object
    procedure HandleException(const EObj: TObject);
      {Handles exceptions by converting expected exceptions into ECodeExporter.
      Unexpected exceptions are re-raised.
        @param EObj [in] Reference to exception to be handled.
        @except Always raise an exception.
      }
    function SnippetNames(const SnipList: TSnippetList): IStringList;
      {Builds a list of snippet names from a snippet list.
        @param SnipList [in] List of snippets.
        @return List containing names of all snippets names.
      }
    procedure WriteReferenceList(const ParentNode: IXMLNode;
      const ListNodeName: string; PasNames: IStringList);
      {Writes a XML node that contains a list of pascal names.
        @param ParentNode [in] Node under which this list node is to be created.
        @param ListNodeName [in] Tag name of list node.
        @param PasNames [in] List of pascal names to be written as child nodes
          of list.
      }
    procedure WriteProgInfo(const ParentNode: IXMLNode);
      {Writes XML node containing CodeSnip version information.
        @param ParentNode [in] Node under which this node is to be created.
      }
    procedure WriteUserInfo(const ParentNode: IXMLNode);
      {Writes a node and sub-nodes containing any information about user who
      created export file.
        @param ParentNode [in] Node under which user info node to be written.
      }
    procedure WriteSnippets(const ParentNode: IXMLNode);
      {Writes a snippets node and sub-nodes containing details of all exported
      snippets.
        @param ParentNode [in] Node under which snippet list node to be
          created.
      }
    procedure WriteSnippet(const ParentNode: IXMLNode; const Snippet: TSnippet);
      {Writes an XML snippet node and child nodes that describe a snippet.
        @param ParentNode [in] Node under which snippet node is to be created.
        @param Snippet [in] Reference to snippet to be described in XML.
      }
    function Execute: TEncodedData;
      {Performs the export.
        @return Encoded data containing exported XML.
        @except ECodeExporter raised if a known error is encountered.
      }
    constructor InternalCreate(const UserInfo: TUserInfo;
      const SnipList: TSnippetList);
      {Private object constructor. Sets up object to export data.
        @param UserInfo [in] User information to be exported.
        @param SnipList [in] List of snippets to be exported.
      }
  public
    destructor Destroy; override;
      {Object destructor: tidies up object.
      }
    class function ExportSnippets(const UserInfo: TUserInfo;
      const SnipList: TSnippetList): TEncodedData;
      {Exports user information and snippets as XML.
        @param UserInfo [in] User information to be exported. Ignored if nul.
        @param SnipList [in] List of snippets to be exported.
        @return Encoding data containing exported XML.
      }
  end;

  {
  ECodeExporter:
    Class of exception raised when TCodeExporter detects an expected error.
  }
  ECodeExporter = class(ECodeSnipXML);


implementation


uses
  // Delphi
  ActiveX, XMLDom,
  // Project
  DB.UMain, UAppInfo, UREMLDataIO, UReservedCategories, USnippetExtraHelper,
  USnippetIDs, USnippetKindInfo, UStructs, UXMLDocConsts;


const
  // XML file markers: attributes of root node
  // watermark (never changes for all versions)
  cWatermark        = 'B46969D4-D367-4F5F-833E-F165FBA78631';
  // file version numbers
  cEarliestVersion  = 1;  // earliest file version supported by importer
  cLatestVersion    = 5;  // current file version written by exporter


{ TUserInfo }

procedure TUserInfo.Assign(const Src: TUserInfo);
  {Sets this record's fields to be same as another TUserInfo record.
    @param Src [in] Record containing fields to be copied.
  }
begin
  Details.Assign(Src.Details);
  Comments := Src.Comments;
end;

constructor TUserInfo.Create(const UserDetails: TUserDetails;
  const UserComments: string);
  {Initialises all a fields of a record.
    @param UserDetails [in] Information about user.
    @param UserComments [in] User's comments.
  }
begin
  Details := UserDetails;
  Comments := UserComments;
end;

class function TUserInfo.CreateNul: TUserInfo;
  {Create a new nul record.
    @return Required initialised record.
  }
begin
  Result.Init;
end;

procedure TUserInfo.Init;
  {Initialises record to nul values.
  }
begin
  Details.Init;
  Comments := '';
end;

function TUserInfo.IsNul: Boolean;
  {Checks if record is nul (empty).
    @return True if record is nul, False if not.
  }
begin
  Result := Details.IsNul and (Comments = '');
end;

{ TCodeExporter }

destructor TCodeExporter.Destroy;
  {Object destructor: tidies up object.
  }
begin
  fXMLDoc := nil;
  inherited;
end;

function TCodeExporter.Execute: TEncodedData;
  {Performs the export.
    @return Encoded data containing exported XML.
    @except ECodeExporter raised if a known error is encountered.
  }
var
  RootNode: IXMLNode;   // document root node
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
  const SnipList: TSnippetList): TEncodedData;
  {Exports user information and snippets as XML.
    @param UserInfo [in] User information to be exported. Ignored if nul.
    @param SnipList [in] List of snippets to be exported.
    @return Encoding data containing exported XML.
  }
begin
  with InternalCreate(UserInfo, SnipList) do
    try
      Result := Execute;
    finally
      Free;
    end;
end;

procedure TCodeExporter.HandleException(const EObj: TObject);
  {Handles exceptions by converting expected exceptions into ECodeExporter.
  Unexpected exceptions are re-raised.
    @param EObj [in] Reference to exception to be handled.
    @except Always raise an exception.
  }
begin
  if (EObj is EFileStreamError) or (EObj is ECodeSnipXML) then
    raise ECodeExporter.Create(EObj as Exception);
  raise EObj;
end;

constructor TCodeExporter.InternalCreate(const UserInfo: TUserInfo;
  const SnipList: TSnippetList);
  {Private object constructor. Sets up object to export data.
    @param UserInfo [in] User information to be exported.
    @param SnipList [in] List of snippets to be exported.
  }
begin
  inherited InternalCreate;
  fSnippets := SnipList;
  fUserInfo := UserInfo;
end;

function TCodeExporter.SnippetNames(
  const SnipList: TSnippetList): IStringList;
  {Builds a list of snippet names from a snippet list.
    @param SnipList [in] List of snippets.
    @return List containing names of all snippet names.
  }
var
  Snippet: TSnippet;  // references each snippet in list
begin
  Result := TIStringList.Create;
  for Snippet in SnipList do
    Result.Add(Snippet.Name);
end;

procedure TCodeExporter.WriteProgInfo(const ParentNode: IXMLNode);
  {Writes XML node containing CodeSnip version information.
    @param ParentNode [in] Node under which this node is to be created.
  }
begin
  fXMLDoc.CreateElement(
    ParentNode, cProgVersionNode, TAppInfo.ProgramReleaseVersion
  );
end;

procedure TCodeExporter.WriteReferenceList(const ParentNode: IXMLNode;
  const ListNodeName: string; PasNames: IStringList);
  {Writes a XML node that contains a list of pascal names.
    @param ParentNode [in] Node under which this list node is to be created.
    @param ListNodeName [in] Tag name of list node.
    @param PasNames [in] List of pascal names to be written as child nodes of
      list.
  }
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
  {Writes an XML snippet node and child nodes that describe a snippet.
    @param ParentNode [in] Node under which snippet node is to be created.
    @param Snippet [in] Reference to snippet to be described in XML.
  }
var
  SnippetNode: IXMLNode; // new snippet node
begin
  // Create snippet node with attribute that specifies snippet name
  SnippetNode := fXMLDoc.CreateElement(ParentNode, cSnippetNode);
  SnippetNode.Attributes[cSnippetNameAttr] := Snippet.Name;
  // Add nodes for properties: (ignore category and xrefs)
  fXMLDoc.CreateElement(SnippetNode, cDescriptionNode, Snippet.Description);
  // source code is stored directly in XML, not in external file
  fXMLDoc.CreateElement(SnippetNode, cSourceCodeTextNode, Snippet.SourceCode);
  // extra info is written only if present
  if not Snippet.Extra.IsEmpty then
    fXMLDoc.CreateElement(
      SnippetNode,
      cExtraNode,
      TSnippetExtraHelper.BuildREMLMarkupLowestVer(Snippet.Extra)
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
  {Writes a snippet node and sub-nodes containing details of all exported
  snippets.
    @param ParentNode [in] Node under which snippet list node to be created.
  }
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

procedure TCodeExporter.WriteUserInfo(const ParentNode: IXMLNode);
  {Writes a node and sub-nodes containing any information about user who
  created export file.
    @param ParentNode [in] Node under which user info node to be written.
  }
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
  {Class destructor. Tidies up object.
  }
begin
  fXMLDoc := nil;
  OleUninitialize;
  inherited;
end;

procedure TCodeImporter.Execute(const Data: TBytes);
  {Performs the import.
    @param Data [in] Byte array containing XML data.
  }

  // ---------------------------------------------------------------------------
  procedure GetUnits(const SnippetNode: IXMLNode; Units: IStringList);
    {Gets a list of units required by a snippet.
      @param SnippetNode [in] Node of snippet for which units are required.
      @param Units [in] Receives list of unit names. Cleared if there are no
        unit names.
    }
  var
    UnitNode: IXMLNode; // unit list node: nil if no list
  begin
    UnitNode := fXMLDoc.FindFirstChildNode(SnippetNode, cUnitsNode);
    Units.Clear;
    TXMLDocHelper.GetPascalNameList(fXMLDoc, UnitNode, Units);
  end;

  procedure GetDepends(const SnippetNode: IXMLNode;
    const Depends: ISnippetIDList);
    {Gets a list of snippets on which a snippet depends.
      @param SnippetNode [in] Node of snippet for which dependencies are
        required.
      @param Depends [in] Receives list of required snippets. Cleared if the are
        no required snippets.
    }
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
      Depends.Add(TSnippetID.Create(SnippetName, True));
  end;
  // ---------------------------------------------------------------------------

resourcestring
  // Error message
  sParseError = 'Import file has an invalid format';
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
    SetLength(fSnippetInfp, SnippetNodes.Count);
    for Idx := 0 to Pred(SnippetNodes.Count) do
    begin
      // Read a snippet node
      SnippetNode := SnippetNodes[Idx];
      fSnippetInfp[Idx].Name := SnippetNode.Attributes[cSnippetNameAttr];
      fSnippetInfp[Idx].Data :=
        (Database as IDatabaseEdit).GetEditableSnippetInfo;
      with fSnippetInfp[Idx].Data do
      begin
        Props.Cat := TReservedCategories.ImportsCatName;
        Props.Desc :=
          TXMLDocHelper.GetSubTagText(fXMLDoc, SnippetNode, cDescriptionNode);
        Props.SourceCode :=
          TXMLDocHelper.GetSubTagText(
            fXMLDoc, SnippetNode, cSourceCodeTextNode
          );
        // how we read extra property depends on version of file
        case fVersion of
          1:
            Props.Extra := TSnippetExtraHelper.BuildActiveText(
              TXMLDocHelper.GetSubTagText(fXMLDoc, SnippetNode, cCommentsNode),
              TXMLDocHelper.GetSubTagText(fXMLDoc, SnippetNode, cCreditsNode),
              TXMLDocHelper.GetSubTagText(fXMLDoc, SnippetNode, cCreditsUrlNode)
            );
          else // later versions
            Props.Extra := TSnippetExtraHelper.BuildActiveText(
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
        GetUnits(SnippetNode, Refs.Units);
        GetDepends(SnippetNode, Refs.Depends);
        Refs.XRef.Clear;
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
  {Retrieves a list of all snippet nodes in XML document.
    @return Required node list.
  }
var
  SnippetsNode: IXMLNode; // node under which all snippets are stored
begin
  SnippetsNode := fXMLDoc.FindNode(cExportRootNode + '\' + cSnippetsNode);
  Result := fXMLDoc.FindChildNodes(SnippetsNode, cSnippetNode);
end;

class procedure TCodeImporter.ImportData(out UserInfo: TUserInfo;
  out SnippetInfo: TSnippetInfoList; const Data: TBytes);
  {Imports snippets and optional user data from XML.
    @param UserInfo [out] Receives user info. Set to nul if no user info was
      available.
    @param SnippetInfo [out] Receives information about each snippet read.
    @param Data [in] Byte array containing XML data.
  }
var
  Idx: Integer; // loops through all imported snippets
begin
  with InternalCreate do
    try
      Execute(Data);
      UserInfo.Assign(fUserInfo);
      SetLength(SnippetInfo, Length(fSnippetInfp));
      for Idx := Low(fSnippetInfp) to High(fSnippetInfp) do
        SnippetInfo[Idx].Assign(fSnippetInfp[Idx]);
    finally
      Free;
    end;
end;

constructor TCodeImporter.InternalCreate;
  {Private class constructor. Sets up object to import data.
  }
begin
  inherited InternalCreate;
  // Set up XML document that will read data
  OleInitialize(nil);
  fXMLDoc := TXMLDocHelper.CreateXMLDoc;
  // Initialise fields that receive imported data
  SetLength(fSnippetInfp, 0);
  fUserInfo.Init;
end;

function TCodeImporter.ValidateDoc: Integer;
  {Validates XML document read from stream and gets file version.
    @return XML file version number.
    @except ECodeImporter raised if XML is not valid.
  }
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
  {Sets this record's fields to be same as another TSnippetInfo record.
  Object fields are copied appropriately.
    @param Src [in] Record containing fields to be copied.
  }
begin
  Name := Src.Name;
  Data.Assign(Src.Data);
end;

procedure TSnippetInfo.Init;
  {Initialises record to nul values.
  }
begin
  Name := '';
  Data.Init;
end;

end.


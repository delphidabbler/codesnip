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
 * Portions created by the Initial Developer are Copyright (C) 2008-2010 Peter
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
  Classes, XMLIntf,
  // Project
  UBaseObjects, UExceptions, UIStringList, USnippets, UUserDetails,
  UXMLDocHelper, UXMLDocumentEx;


type

  {
  TRoutineInfo:
    Record that encapsulates data read from an import file that describes a
    snippet.
  }
  TRoutineInfo = record
    Name: string;           // Snippet name
    Data: TSnippetEditData; // Describes a snippet
    procedure Assign(const Src: TRoutineInfo);
      {Sets this record's fields to be same as another TRoutineInfo record.
      Object fields are copied appropriately.
        @param Src [in] Record containing fields to be copied.
      }
    procedure Init;
      {Initialises record to nul values.
      }
  end;

  {
  TRoutineInfoList:
    Dynamic array of TRoutineInfo records.
  }
  TRoutineInfoList = array of TRoutineInfo;

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
    fRoutineInfo: TRoutineInfoList; // List of snippets read from XML
    fStream: TStream;               // Stream containing XML data to be imported
    fXMLDoc: IXMLDocumentEx;        // Extended XML document object
    function GetAllRoutineNodes: IXMLSimpleNodeList;
      {Retrieves a list of all "routine" nodes in XML document.
        @return Required node list.
      }
    procedure Execute;
      {Performs the import.
      }
    function ValidateDoc: Integer;
      {Validates XML document read from stream and gets file version.
        @return XML file version number.
        @except ECodeImporter raised if XML is not valid.
      }
    constructor InternalCreate(const Stream: TStream);
      {Private class constructor. Sets up object to import data.
        @param Stream [in] Stream containing XML data to be imported.
      }
  public
    destructor Destroy; override;
      {Class destructor. Tidies up object.
      }
    class procedure ImportData(out UserInfo: TUserInfo;
      out RoutineInfo: TRoutineInfoList; const Stream: TStream);
      {Imports snippets and optional user data from XML.
        @param UserInfo [out] Receives user info. Set to nul if no user info was
          available.
        @param RoutineInfo [out] Receives information about each snippet read.
        @param Stream [in] Stream containing XML data.
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
    fUserInfo: TUserInfo;     // User information to be written to XML
    fStream: TStream;         // Stream that receives XML
    fRoutines: TRoutineList;  // List of snippets to be exported
    fXMLDoc: IXMLDocumentEx;  // Extended XML document object
    procedure HandleException(const EObj: TObject);
      {Handles exceptions by converting expected exceptions into ECodeExporter.
      Unexpected exceptions are re-raised.
        @param EObj [in] Reference to exception to be handled.
        @except Always raise an exception.
      }
    function RoutineNames(const Routines: TRoutineList): IStringList;
      {Builds a list of snippet names from a snippet list.
        @param Routines [in] List of snippets.
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
    procedure WriteRoutines(const ParentNode: IXMLNode);
      {Writes a "routines" node and sub-nodes containing details of all exported
      snippets.
        @param ParentNode [in] Node under which "routine" list node to be
          created.
      }
    procedure WriteRoutine(const ParentNode: IXMLNode; const Routine: TRoutine);
      {Writes an XML "routine" node and child nodes that describe a snippet.
        @param ParentNode [in] Node under which "routine" node is to be created.
        @param Routine [in] Reference to snippet to be described in XML.
      }
    procedure Execute;
      {Performs the export.
        @except ECodeExporter raised if a known error is encountered.
      }
    constructor InternalCreate(const UserInfo: TUserInfo;
      const Routines: TRoutineList; const Stm: TStream);
      {Private class constructor. Sets up object to export data.
        @param UserInfo [in] User information to be exported.
        @param Routines [in] List of snippets to be exported.
        @param Stm [in] Stream to receive exported XML.
      }
  public
    destructor Destroy; override;
      {Class destructor: tidies up object.
      }
    class procedure ExportRoutines(const UserInfo: TUserInfo;
      const Routines: TRoutineList; const Stm: TStream);
      {Exports user information and snippets as XML.
        @param UserInfo [in] User information to be exported. Ignored if nul.
        @param Routines [in] List of snippets to be exported.
        @param Stream [in] Stream to receive exported XML.
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
  SysUtils, ActiveX, XMLDom,
  // Project
  UAppInfo, UREMLDataIO, UReservedCategories, URoutineExtraHelper, USnippetIDs,
  UStructs, UXMLDocConsts;


const
  // XML file markers: attributes of root node
  // watermark (never changes for all versions)
  cWatermark        = 'B46969D4-D367-4F5F-833E-F165FBA78631';
  // file format versions
  cVersion1         = 1;
  cVersion2         = 2;
  cVersion3         = 3;
  cVersion4         = 4;
  cEarliestVersion  = cVersion1;
  cMinOutputVersion = cVersion3;
  cLatestVersion    = cVersion4;


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
  {Class destructor: tidies up object.
  }
begin
  fXMLDoc := nil;
  inherited;
end;

procedure TCodeExporter.Execute;
  {Performs the export.
    @except ECodeExporter raised if a known error is encountered.
  }

  // ---------------------------------------------------------------------------
  function MinVersion: Integer;
    {Determines the minimum version number of the export file. This depends on
    attributes of the exported snippets.
      @return Required version number.
    }
  var
    Snippet: TRoutine;        // each exported snippet
    MinREMLVer: TREMLVersion; // lowest version no. of REML code in extra prop
    AREMLVer: TREMLVersion;   // required REML version for a snippet
  begin
    MinREMLVer := TREMLAnalyser.FIRST_VERSION;
    for Snippet in fRoutines do
    begin
      AREMLVer := TREMLAnalyser.LowestWriterVersion(Snippet.Extra);
      if AREMLVer > MinREMLVer then
        MinREMLVer := AREMLVer;
    end;
    // Minimum export file version number is at least 3, and 4 if extra property
    // contains data that requires later version of REML
    if MinREMLVer < cMinOutputVersion then
      Result := cMinOutputVersion
    else
      Result := cVersion4;
  end;
  // ---------------------------------------------------------------------------

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
      fXMLDoc, cExportRootNode, cWatermark, MinVersion
    );

    // Write document content
    WriteProgInfo(RootNode);
    if not fUserInfo.IsNul then
      WriteUserInfo(RootNode);
    WriteRoutines(RootNode);

    // Save XML to stream
    fXMLDoc.SaveToStream(fStream);

  except
    HandleException(ExceptObject);
  end;
end;

class procedure TCodeExporter.ExportRoutines(const UserInfo: TUserInfo;
  const Routines: TRoutineList; const Stm: TStream);
  {Exports user information and routines as XML.
    @param UserInfo [in] User information to be exported. Ignored if nul.
    @param Routines [in] List of snippets to be exported.
    @param Stream [in] Stream to receive exported XML.
  }
begin
  with InternalCreate(UserInfo, Routines, Stm) do
    try
      Execute;
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
  const Routines: TRoutineList; const Stm: TStream);
  {Private class constructor. Sets up object to export data.
    @param UserInfo [in] User information to be exported.
    @param Routines [in] List of snippets to be exported.
    @param Stm [in] Stream to receive exported XML.
  }
begin
  inherited InternalCreate;
  fRoutines := Routines;
  fStream := Stm;
  fUserInfo := UserInfo;
end;

function TCodeExporter.RoutineNames(
  const Routines: TRoutineList): IStringList;
  {Builds a list of snippet names from a snippet list.
    @param Routines [in] List of snippets.
    @return List containing names of all snippet names.
  }
var
  Routine: TRoutine;  // references each snippet in list
begin
  Result := TIStringList.Create;
  for Routine in Routines do
    Result.Add(Routine.Name);
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

procedure TCodeExporter.WriteRoutine(const ParentNode: IXMLNode;
  const Routine: TRoutine);
  {Writes an XML "routine" node and child nodes that describe a snippet.
    @param ParentNode [in] Node under which "routine" node is to be created.
    @param Routine [in] Reference to snippet to be described in XML.
  }
var
  RoutineNode: IXMLNode;      // new "routine" node
begin
  // Create "routine" node with attribute that specifies snippet name
  RoutineNode := fXMLDoc.CreateElement(ParentNode, cRoutineNode);
  RoutineNode.Attributes[cRoutineNameAttr] := Routine.Name;
  // Add nodes for properties: (ignore category and xrefs)
  fXMLDoc.CreateElement(RoutineNode, cDescriptionNode, Routine.Description);
  // source code is stored directly in XML, not in external file
  fXMLDoc.CreateElement(RoutineNode, cSourceCodeTextNode, Routine.SourceCode);
  // extra info is written only if present
  if not Routine.Extra.IsEmpty then
    fXMLDoc.CreateElement(
      RoutineNode,
      cExtraNode,
      TRoutineExtraHelper.BuildREMLMarkupLowestVer(Routine.Extra)
    );
  // write kind
  TXMLDocHelper.WriteSnippetKind(fXMLDoc, RoutineNode, Routine.Kind);
  // compiler results value: only write known results
  TXMLDocHelper.WriteCompilerResults(
    fXMLDoc, RoutineNode, Routine.Compatibility
  );
  // depends and units lists
  WriteReferenceList(
    RoutineNode, cDependsNode, RoutineNames(Routine.Depends)
  );
  WriteReferenceList(
    RoutineNode, cUnitsNode, TIStringList.Create(Routine.Units)
  );
end;

procedure TCodeExporter.WriteRoutines(const ParentNode: IXMLNode);
  {Writes a "routine" node and sub-nodes containing details of all exported
  snippets.
    @param ParentNode [in] Node under which "routines" list node to be created.
  }
var
  Node: IXMLNode;       // new "routines" list node
  Routine: TRoutine;    // refers to each exported snippet
begin
  // Add "routines" list node
  Node := fXMLDoc.CreateElement(ParentNode, cRoutinesNode);
  // Add child node for each exported snippet
  for Routine in fRoutines do
    WriteRoutine(Node, Routine);
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

procedure TCodeImporter.Execute;
  {Performs the import.
  }

  // ---------------------------------------------------------------------------
  procedure GetUnits(const RoutineNode: IXMLNode; Units: IStringList);
    {Gets a list of units required by a snippet.
      @param RoutineNode [in] Node of snippet for which units are required.
      @param Units [in] Receives list of unit names. Cleared if there are no
        unit names.
    }
  var
    UnitNode: IXMLNode; // unit list node: nil if no list
  begin
    UnitNode := fXMLDoc.FindFirstChildNode(RoutineNode, cUnitsNode);
    Units.Clear;
    TXMLDocHelper.GetPascalNameList(fXMLDoc, UnitNode, Units);
  end;

  procedure GetDepends(const RoutineNode: IXMLNode;
    const Depends: ISnippetIDList);
    {Gets a list of snippets on which a snippet depends.
      @param RoutineNode [in] Node of snippet for which dependencies are
        required.
      @param Depends [in] Receives list of required snippets. Cleared if the are
        no required snippets.
    }
  var
    DependsNode: IXMLNode;      // depends node list: nil if no list
    RoutineNames: IStringList;  // list of names of snippets in depends list
    RoutineName: string;        // each snippet name in RoutineNames
  begin
    DependsNode := fXMLDoc.FindFirstChildNode(RoutineNode, cDependsNode);
    RoutineNames := TIStringList.Create;
    TXMLDocHelper.GetPascalNameList(fXMLDoc, DependsNode, RoutineNames);
    Depends.Clear;
    for RoutineName in RoutineNames do
      // Note: in building TRoutineIDList we assume each snippet is user-
      // defined. It may not be, but there is no way of telling from XML.
      Depends.Add(TSnippetID.Create(RoutineName, True));
  end;
  // ---------------------------------------------------------------------------

resourcestring
  // Error message
  sParseError = 'Import file has an invalid format';
var
  UserNode: IXMLNode;               // node containing any user info
  RoutineNodes: IXMLSimpleNodeList; // list of "routine" nodes
  RoutineNode: IXMLNode;            // each "routine" node in list
  Idx: Integer;                     // loops thru "routines" node list
begin
  // Load XML document
  try
    fXMLDoc.LoadFromStream(fStream);
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
    RoutineNodes := GetAllRoutineNodes;
    SetLength(fRoutineInfo, RoutineNodes.Count);
    for Idx := 0 to Pred(RoutineNodes.Count) do
    begin
      // Read a "routine" node
      RoutineNode := RoutineNodes[Idx];
      fRoutineInfo[Idx].Name := RoutineNode.Attributes[cRoutineNameAttr];
      fRoutineInfo[Idx].Data :=
        (Snippets as ISnippetsEdit).GetEditableRoutineInfo;
      with fRoutineInfo[Idx].Data do
      begin
        Props.Cat := TReservedCategories.ImportsCatName;
        Props.Desc :=
          TXMLDocHelper.GetSubTagText(fXMLDoc, RoutineNode, cDescriptionNode);
        Props.SourceCode :=
          TXMLDocHelper.GetSubTagText(
            fXMLDoc, RoutineNode, cSourceCodeTextNode
          );
        // how we read extra property depends on version of file
        case fVersion of
          cVersion1:
            Props.Extra := TRoutineExtraHelper.BuildActiveText(
              TXMLDocHelper.GetSubTagText(fXMLDoc, RoutineNode, cCommentsNode),
              TXMLDocHelper.GetSubTagText(fXMLDoc, RoutineNode, cCreditsNode),
              TXMLDocHelper.GetSubTagText(fXMLDoc, RoutineNode, cCreditsUrlNode)
            );
          else // later versions
            Props.Extra := TRoutineExtraHelper.BuildActiveText(
              TXMLDocHelper.GetSubTagText(fXMLDoc, RoutineNode, cExtraNode)
            );
        end;
        // how we read kind property depends on version of file
        case fVersion of
          cVersion1, cVersion2:
            // for version 1 and 2, we have StandardFormat instead of Kind:
            // map standard format value onto a kind
            if TXMLDocHelper.GetStandardFormat(fXMLDoc, RoutineNode, False) then
              Props.Kind := skRoutine
            else
              Props.Kind := skFreeform;
          else // later versions
            // for later versions we have Kind value: use Freeform if missing
            Props.Kind := TXMLDocHelper.GetSnippetKind(
              fXMLDoc, RoutineNode, skFreeForm
            );
        end;
        Props.CompilerResults := TXMLDocHelper.GetCompilerResults(
          fXMLDoc, RoutineNode
        );
        GetUnits(RoutineNode, Refs.Units);
        GetDepends(RoutineNode, Refs.Depends);
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

function TCodeImporter.GetAllRoutineNodes: IXMLSimpleNodeList;
  {Retrieves a list of all "routine" nodes in XML document.
    @return Required node list.
  }
var
  RoutinesNode: IXMLNode; // node under which all snippets are stored
begin
  RoutinesNode := fXMLDoc.FindNode(cExportRootNode + '\' + cRoutinesNode);
  Result := fXMLDoc.FindChildNodes(RoutinesNode, cRoutineNode);
end;

class procedure TCodeImporter.ImportData(out UserInfo: TUserInfo;
  out RoutineInfo: TRoutineInfoList; const Stream: TStream);
  {Imports snippets and optional user data from XML.
    @param UserInfo [out] Receives user info. Set to nul if no user info was
      available.
    @param RoutineInfo [out] Receives information about each snippet read.
    @param Stream [in] Stream containing XML data.
    @except ECodeImporter raised if data is not in valid format.
  }
var
  Idx: Integer; // loops through all imported snippets
begin
  with InternalCreate(Stream) do
    try
      Execute;
      UserInfo.Assign(fUserInfo);
      SetLength(RoutineInfo, Length(fRoutineInfo));
      for Idx := Low(fRoutineInfo) to High(fRoutineInfo) do
        RoutineInfo[Idx].Assign(fRoutineInfo[Idx]);
    finally
      Free;
    end;
end;

constructor TCodeImporter.InternalCreate(const Stream: TStream);
  {Private class constructor. Sets up object to import data.
    @param Stream [in] Stream containing XML data to be imported.
  }
begin
  inherited InternalCreate;
  fStream := Stream;
  // Set up XML document that will read data
  OleInitialize(nil);
  fXMLDoc := TXMLDocHelper.CreateXMLDoc;
  // Initialise fields that receive imported data
  SetLength(fRoutineInfo, 0);
  fUserInfo.Init;
end;

function TCodeImporter.ValidateDoc: Integer; 
  {Validates XML document read from stream and gets file version.
    @return XML file version number.
    @except ECodeImporter raised if XML is not valid.
  }
var
  RoutinesNode: IXMLNode;           // node where snippets are recorded
  RoutineNodes: IXMLSimpleNodeList; // list of nodes describing snippets
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
  // Must be a "routines" node
  RoutinesNode := fXMLDoc.FindNode(cExportRootNode + '\' + cRoutinesNode);
  if not Assigned(RoutinesNode) then
    raise ECodeImporter.CreateFmt(sMissingNode, [cRoutinesNode]);
  // Must be at least one routine node
  RoutineNodes := fXMLDoc.FindChildNodes(RoutinesNode, cRoutineNode);
  if RoutineNodes.Count = 0 then
    raise ECodeImporter.CreateFmt(sMissingNode, [cRoutineNode]);
end;

{ TRoutineInfo }

procedure TRoutineInfo.Assign(const Src: TRoutineInfo);
  {Sets this record's fields to be same as another TRoutineInfo record.
  Object fields are copied appropriately.
    @param Src [in] Record containing fields to be copied.
  }
begin
  Name := Src.Name;
  Data.Assign(Src.Data);
end;

procedure TRoutineInfo.Init;
  {Initialises record to nul values.
  }
begin
  Name := '';
  Data.Init;
end;

end.


{
 * UXMLDataIO.pas
 *
 * Implements classes that can read and write databases stored in XML format
 * with associated source code files.
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
 * The Original Code is UXMLDataIO.pas
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


unit UXMLDataIO;


interface


uses
  // Delphi
  XMLIntf,
  // Project
  UIStringList, UREMLDataIO, USnipData, USnippets, UXMLDocumentEx;


type

  {
  TXMLDataIO:
    Base class for classes that read and write databases stored in an XML file
    and linked data files.
  }
  TXMLDataIO = class(TInterfacedObject)
  strict protected
    fDBDir: string;           // Database directory
    fXMLDoc: IXMLDocumentEx;  // Extended XML document object
    function PathToXMLFile: string;
      {Gets fully specified path to the XML file. Path depends on which database
      is being accessed.
        @return Required path.
      }
    function DataFile(const FileName: string): string;
      {Gets full path to a file name. Path depends on which database is being
      accessed.
        @param FileName [in] File name for which path is required.
        @return Required full path to file.
      }
    function DataDir: string;
      {Gets name of directory storing the database being accessed. Path varies
      according to which database is being accessed.
        @return Path to directory.
      }
    function FindCategoryNode(const Cat: string): IXMLNode;
      {Finds a specified category node in the file.
        @param Cat [in] Name of required category.
        @return Required node or nil if node doesn't exist.
      }
    function FindRoutineNode(const RoutineName: string): IXMLNode;
      {Finds a specified <routine> node for a snippet in the file.
        @param RoutineName [in] Name of required snippet.
        @return Required node or nil if node doesn't exist.
      }
  public
    constructor Create(const DBDir: string);
      {Class constructor. Creates object and XML document for a given database.
        @param DBDir [in] Directory where database is stored.
      }
    destructor Destroy; override;
      {Class destructor. Tears down object.
      }
  end;

  {
  TXMLDataReader:
    Class that can read a database from an XML file and various linked data
    files.
  }
  TXMLDataReader = class(TXMLDataIO,
    IDataReader
  )
  strict private
    fVersion: Integer;
      {XML document version number}
    function ValidateDoc: Integer;
      {Validates XML document and gets file version.
        @return XML file version number.
        @except EDataIO raised if XML is not valid.
      }
    procedure HandleCorruptDatabase(const EObj: TObject);
      {Called when a corrupt database is encountered. Deletes all files and
      raises exception.
        @param EObj [in] Reference to exception that caused this method to be
          called.
        @except Exception always raised.
      }
    function GetRoutineReferences(const Routine, RefName: string): IStringList;
      {Get list of all specified references made by a snippet.
        @param Routine [in] Name of required snippet.
        @param RefName [in] Name of node containing snippet's references.
        @return List of names of references.
      }
  public
    constructor Create(const DBDir: string);
      {Class constructor. Sets up object and loads XML from file if database
      master file exists, otherwise creates a minimal empty document.
        @param DBDir [in] Directory where database is stored.
      }
    { IDataReader methods }
    function DatabaseExists: Boolean;
      {Check if the database exists. This method is always called first. No
      other methods are called if this method returns false.
        @return True if database exists, False if not.
      }
    function GetAllCatNames: IStringList;
      {Get names of all categories in database.
        @return List of category names.
      }
    procedure GetCatProps(const Cat: string; var Props: TCategoryData);
      {Get properties of a category.
        @param Cat [in] Name of required category.
        @param Props [in/out] Empty properties passed in. Record fields set to
          values of category properties by implementor.
      }
    function GetCatRoutines(const Cat: string): IStringList;
      {Get names of all snippets in a category.
        @param Cat [in] Name of category containing snippets.
        @return List of snippet names.
      }
    procedure GetRoutineProps(const Routine: string; var Props: TSnippetData);
      {Get properties of a snippet.
        @param Routine [in] Name of required snippet.
        @param Props [in/out] Empty properties passed in. Record fields set to
          values of routine properties.
      }
    function GetRoutineXRefs(const Routine: string): IStringList;
      {Get list of all snippets that are cross referenced by a snippet.
        @param Routine [in] Name of snippet we need cross references for.
        @return List of snippet names.
      }
    function GetRoutineDepends(const Routine: string): IStringList;
      {Get list of all snippets on which a given snippet depends.
        @param Routine [in] Name of required snippet.
        @return List of snippet names.
      }
    function GetRoutineUnits(const Routine: string): IStringList;
      {Get list of all units referenced by a snippet.
        @param Routine [in] Name of required snippet.
        @return List of unit names.
      }
  end;

  {
  TXMLDataWriter:
    Class that can write a database to an XML file and various linked data
    files.
  }
  TXMLDataWriter = class(TXMLDataIO,
    IDataWriter
  )
  strict private
    fFileNum: Integer;          // Number of next available unused data file
    fRoutinesNode: IXMLNode;    // Reference to <routines> node in document
    fCategoriesNode: IXMLNode;  // Reference to <categories> node in document
    fMinREMLVer: TREMLVersion;  // Minimum REML version needed for Extra props
    procedure WriteNameList(const Parent: IXMLNode;
      const ListName, ItemName: string; const Items: IStringList);
      {Writes a list of names to XML.
        @param Parent [in] Reference to node under which list is to be stored.
        @param ListName [in] Name of tag that encloses the list items.
        @param ItemName [in] Name of each list item tag.
        @param Items [in] List of names in list. One ItemName node per name.
      }
    procedure WriteReferenceList(const RoutineName, ListName: string;
      const Items: IStringList);
      {Writes a snippet's reference list to XML.
        @param RoutineName [in] Name of snippet whose reference list is to be
          written.
        @param ListName [in] Name of tag that encloses list entry.
        @param Items [in] List of items in reference list.
      }
    procedure HandleException(const EObj: TObject);
      {Handles exceptions raised by converting expected exceptions into
      ECodeSnip derived exceptions.
        @param EObj [in] Reference to exception to be handled.
        @except Always raise an exception.
      }
  protected // must not be strict
    { IDataWriter methods }
    procedure Initialise;
      {Initialise the database. Always called before any other methods.
      }
    procedure WriteCatProps(const CatName: string; const Props: TCategoryData);
      {Write the properties of a category. Always called before WriteCatRoutines
      for a given category, so can be used to perform any per-category
      initialisation.
        @param CatName [in] Name of category.
        @param Props [in] Properties of category.
      }
    procedure WriteCatRoutines(const CatName: string;
      const Routines: IStringList);
      {Write the list of snippets belonging to a category. Always called after
      WriteCatProps for any given category.
        @param CatName [in] Name of category.
        @param Routines [in] List of names of snippets.
      }
    procedure WriteRoutineProps(const RoutineName: string;
      const Props: TSnippetData);
      {Write the properties of a snippet. Always called after all categories are
      written and before WriteRoutineUnits, so can be used to perform any per-
      routine intialisation.
        @param RoutineName [in] Name of snippet.
        @param Props [in] Properties of snippet.
      }
    procedure WriteRoutineUnits(const RoutineName: string;
      const Units: IStringList);
      {Write the list of units required by a snippet.
        @param RoutineName [in] Name of snippet.
        @param Units [in] List of names of required units.
      }
    procedure WriteRoutineDepends(const RoutineName: string;
      const Depends: IStringList);
      {Write the list of snippets on which a snippet depends.
        @param RoutineName [in] Name of snippet.
        @param Depends [in] List of snippet names.
      }
    procedure WriteRoutineXRefs(const RoutineName: string;
      const XRefs: IStringList);
      {Write the list of snippets that a snippet cross-references.
        @param RoutineName [in] Name of snippet.
        @param XRefs [in] List of routine snippets.
      }
    procedure Finalise;
      {Finalises the database. Always called after all other methods.
      }
  end;


implementation


uses
  // Delphi
  SysUtils, Classes, ActiveX, XMLDom,
  // Project
  UActiveText, UConsts, UExceptions, URoutineExtraHelper, UStructs, UUtils,
  UXMLDocConsts, UXMLDocHelper;


const
  // Database file name
  cDatabaseFileName     = 'database.xml';
  // File markers: attributes of root node
  // watermark (never changes for all versions)
  cWatermark            = '531257EA-1EE3-4B0F-8E46-C6E7F7140106';
  // supported file format versions
  cVersion1             = 1;
  cVersion2             = 2;
  cVersion3             = 3;
  cVersion4             = 4;
  cEarliestVersion      = cVersion1;
  cMinOutputVersion     = cVersion3;
  cLatestVersion        = cVersion4;


{ Support routines }

procedure Error(const Msg: string); overload;
  {Raises an exception with a specified message.
    @param Msg [in] Message to pass to exception.
    @except EDataIO always raised.
  }
begin
  raise EDataIO.Create(Msg);
end;

procedure Error(const FmtStr: string; const Args: array of const); overload;
  {Raises exception with a message built from a format string and arguements.
    @param FmtStr [in] Format string to use to format message.
    @param Args [in] Arguments to be included in message.
    @except EDataIO always raised.
  }
begin
  raise EDataIO.CreateFmt(FmtStr, Args);
end;

{ TXMLDataIO }

resourcestring
  // Error message
  sMissingNode = 'Document has no %s node.';

constructor TXMLDataIO.Create(const DBDir: string);
  {Class constructor. Creates object and XML document for a given database.
    @param DBDir [in] Directory where database is stored.
  }
begin
  inherited Create;
  fDBDir := DBDir;
  // For some reason we must call OleInitialize here rather than in
  // initialization section
  OleInitialize(nil);
  fXMLDoc := TXMLDocHelper.CreateXMLDoc;
end;

function TXMLDataIO.DataDir: string;
  {Gets name of directory storing the database being accessed. Path varies
  according to which database is being accessed.
    @return Path to directory.
  }
begin
  Result := ExcludeTrailingPathDelimiter(fDBDir);
end;

function TXMLDataIO.DataFile(const FileName: string): string;
  {Gets full path to a file name. Path depends on which database is being
  accessed.
    @param FileName [in] File name for which path is required.
    @return Required full path to file.
  }
begin
  Result := IncludeTrailingPathDelimiter(DataDir) + FileName;
end;

destructor TXMLDataIO.Destroy;
  {Class destructor. Tears down object.
  }
begin
  // For some reason we must call OleUninitialize here rather than in
  // finalization section
  OleUninitialize;
  inherited;
end;

function TXMLDataIO.FindCategoryNode(const Cat: string): IXMLNode;
  {Finds a specified category node in the file.
    @param Cat [in] Name of required category.
    @return Required node or nil if node doesn't exist.
  }
var
  CatListNode: IXMLNode;  // node that contains category nodes
begin
  Result := nil;
  // Find <categories> node
  CatListNode := fXMLDoc.FindNode(cUserDataRootNode + '\' + cCategoriesNode);
  if not Assigned(CatListNode) then
    Error(sMissingNode, [cCategoriesNode]);
  // Find required <category> node
  Result := fXMLDoc.FindFirstChildNode(
    CatListNode, cCategoryNode, cCategoryIdAttr, Cat
  )
end;

function TXMLDataIO.FindRoutineNode(const RoutineName: string): IXMLNode;
  {Finds a specified <routine> node for a snippet in the file.
    @param RoutineName [in] Name of required snippet.
    @return Required node or nil if node doesn't exist.
  }
var
  RoutineListNode: IXMLNode;  // list node that contains <routine> nodes
begin
  Result := nil;
  // Find <routines> node
  RoutineListNode := fXMLDoc.FindNode(cUserDataRootNode + '\' + cRoutinesNode);
  if not Assigned(RoutineListNode) then
    Error(sMissingNode, [cRoutinesNode]);
  // Find required <routine> node
  Result := fXMLDoc.FindFirstChildNode(
    RoutineListNode, cRoutineNode, cRoutineNameAttr, RoutineName
  );
end;

function TXMLDataIO.PathToXMLFile: string;
  {Gets fully specified path to the XML file. Path depends on which database is
  being accessed.
    @return Required path.
  }
begin
  Result := DataFile(cDatabaseFileName);
end;

{ TXMLDataReader }

resourcestring
  // Error messages
  sNoCategoriesNode = 'No categories node in XML file';
  sCatNotFound = 'Can''t find reference to category "%s" in XML file';
  sSnippetNotFound = 'Can''t find reference to snippet "%s" in XML file';
  sMissingSource = 'Source code file name missing for snippet "%s"';
  sDBError = 'The database is corrupt and had been deleted.' + EOL2 + '%s';

constructor TXMLDataReader.Create(const DBDir: string);
  {Class constructor. Sets up object and loads XML from file if database master
  file exists, otherwise creates a minimal empty document.
    @param DBDir [in] Directory where database is stored.
  }
var
  RootNode: IXMLNode; // reference to document's root node
begin
  inherited Create(DBDir);
  if DatabaseExists then
  begin
    // Database exists: load it
    fXMLDoc.LoadFromFile(WideString(PathToXMLFile));
    fXMLDoc.Active := True;
    try
      fVersion := ValidateDoc;
    except
      HandleCorruptDatabase(ExceptObject);
    end;
  end
  else
  begin
    // Database doesn't exist: create sufficient nodes for main code to find
    fXMLDoc.Active := True;
    TXMLDocHelper.CreateXMLProcInst(fXMLDoc);
    RootNode := TXMLDocHelper.CreateRootNode(
      fXMLDoc, cUserDataRootNode, cWatermark, cLatestVersion
    );
    fXMLDoc.CreateElement(RootNode, cCategoriesNode);
    fXMLDoc.CreateElement(RootNode, cRoutinesNode);
  end;
end;

function TXMLDataReader.DatabaseExists: Boolean;
  {Check if the database exists. This method is always called first. No
  other methods are called if this method returns false.
    @return True if database exists, False if not.
  }
begin
  Result := FileExists(PathToXMLFile);
end;

function TXMLDataReader.GetAllCatNames: IStringList;
  {Get names of all categories in database.
    @return List of category names.
  }
var
  CatListNode: IXMLNode;        // node containing list of categories
  CatNodes: IXMLSimpleNodeList; // list of all category nodes of categories
  CatNode: IXMLNode;            // a node in CatNodes
begin
  try
    Result := TIStringList.Create;
    CatListNode := fXMLDoc.FindNode(cUserDataRootNode + '\' + cCategoriesNode);
    if not Assigned(CatListNode) then
      Error(sNoCategoriesNode);
    CatNodes := fXMLDoc.FindChildNodes(CatListNode, cCategoryNode);
    for CatNode in CatNodes do
      Result.Add(CatNode.Attributes[cCategoryIdAttr]);
  except
    HandleCorruptDatabase(ExceptObject);
  end;
end;

procedure TXMLDataReader.GetCatProps(const Cat: string;
  var Props: TCategoryData);
  {Get properties of a category.
    @param Cat [in] Name of required category.
    @param Props [in/out] Empty properties passed in. Record fields set to
      values of category properties by implementor.
  }
var
  CatNode: IXMLNode;  // reference to node for required category
begin
  try
    CatNode := FindCategoryNode(Cat);
    if not Assigned(CatNode) then
      // Properties will not be requested for a category that doesn't exist in
      // this database, so this should never happen
      Error(sCatNotFound);
    Props.Desc := TXMLDocHelper.GetSubTagText(
      fXMLDoc, CatNode, cDescriptionNode
    );
  except
    HandleCorruptDatabase(ExceptObject);
  end;
end;

function TXMLDataReader.GetCatRoutines(const Cat: string): IStringList;
  {Get names of all snippets in a category.
    @param Cat [in] Name of category containing snippets.
    @return List of snippet names.
  }
var
  CatNode: IXMLNode;  // reference to required category node
begin
  try
    Result := TIStringList.Create;
    CatNode := FindCategoryNode(Cat);
    if not Assigned(CatNode) then
      // This is not an error since it is possible that a category exists in
      // another database and loader will request info from here also
      Exit;
    TXMLDocHelper.GetPascalNameList(
      fXMLDoc, fXMLDoc.FindFirstChildNode(CatNode, cCatRoutinesNode), Result
    );
  except
    HandleCorruptDatabase(ExceptObject);
  end;
end;

function TXMLDataReader.GetRoutineDepends(const Routine: string): IStringList;
  {Get list of all snippets on which a given snippet depends.
    @param Routine [in] Name of required snippet.
    @return List of snippet names.
  }
begin
  Result := GetRoutineReferences(Routine, cDependsNode);
end;

procedure TXMLDataReader.GetRoutineProps(const Routine: string;
  var Props: TSnippetData);
  {Get properties of a snippet.
    @param Routine [in] Name of required snippet.
    @param Props [in/out] Empty properties passed in. Record fields set to
      values of routine properties.
  }
var
  RoutineNode: IXMLNode;  // node for required snippet

  // ---------------------------------------------------------------------------
  function GetPropertyText(const PropTagName: string): string;
    {Gets text of a specified property.
      @param PropTagName [in] Tag associated with property.
      @return Property value from tag's text.
    }
  begin
    Result := TXMLDocHelper.GetSubTagText(fXMLDoc, RoutineNode, PropTagName);
  end;

  function GetSourceCodePropertyText: string;
    {Gets source code from a file referenced from XML file.
      @return Source code.
    }
  var
    DataFileName: string; // name of file containing source code
  begin
    DataFileName := GetPropertyText(cSourceCodeFileNode);
    if DataFileName = '' then
      Error(sMissingSource, [Routine]);
    try
      Result := FileToString(DataFile(DataFileName));
    except
      // convert file errors to EDataIO
      on E: EFOpenError do
        raise EDataIO.Create(E);
      else
        raise;
    end;
  end;

  function GetStandardFormatProperty: Boolean;
    {Gets value of standard format node.
      @return True if standard format, False if not. If node present result
        depends of which database is being accessed.
    }
  begin
    Result := TXMLDocHelper.GetStandardFormat(
      fXMLDoc, RoutineNode, False
    );
  end;

  function GetKindProperty: TSnippetKind;
    {Gets value of Kind node.
      @return Kind that matches node value.
    }
  var
    Default: TSnippetKind;  // default value
  begin
    // In earlier versions of the document we have no Kind node, so we calculate
    // kind value from StandardFormat value. If Kind node is present
    // StandardFormat is ignored
    if GetStandardFormatProperty then
      Default := skRoutine
    else
      Default := skFreeform;
    Result := TXMLDocHelper.GetSnippetKind(fXMLDoc, RoutineNode, Default);
  end;

  function GetExtraProperty: IActiveText;
    {Builds Extra active text object from file data.
      @return Required active text.
    }
  begin
    // We get extra data from different nodes depending on file version
    try
      if fVersion = cVersion1 then
        // version 1: build extra data from comments, credits and credits URL
        // nodes
        Result := TRoutineExtraHelper.BuildActiveText(
          GetPropertyText(cCommentsNode),
          GetPropertyText(cCreditsNode),
          GetPropertyText(cCreditsUrlNode)
        )
      else
        // version 2: build extra data from extra node
        Result := TRoutineExtraHelper.BuildActiveText(
          GetPropertyText(cExtraNode)
        );
    except
      // error: provide an empty property value
      Result := TActiveTextFactory.CreateActiveText;
    end;
  end;
  // ---------------------------------------------------------------------------

begin
  try
    // Find routine node
    RoutineNode := FindRoutineNode(Routine);
    if not Assigned(RoutineNode) then
      Error(sSnippetNotFound, [Routine]);
    // Routine found: read properties
    Props.Cat := GetPropertyText(cCatIdNode);
    Props.Kind := GetKindProperty;
    Props.Desc := GetPropertyText(cDescriptionNode);
    Props.Extra := GetExtraProperty;
    Props.SourceCode := GetSourceCodePropertyText;
    Props.CompilerResults := TXMLDocHelper.GetCompilerResults(
      fXMLDoc, RoutineNode
    );
  except
    HandleCorruptDatabase(ExceptObject);
  end;
end;

function TXMLDataReader.GetRoutineReferences(const Routine,
  RefName: string): IStringList;
  {Get list of all specified references made by a snippet.
    @param Routine [in] Name of required snippet.
    @param RefName [in] Name of node containing snippet's references.
    @return List of names of references.
  }
var
  RoutineNode: IXMLNode;  // node for required routine
begin
  try
    Result := TIStringList.Create;
    RoutineNode := FindRoutineNode(Routine);
    if not Assigned(RoutineNode) then
      Error(sSnippetNotFound, [Routine]);
    // References are contained in a list of contained <pascal-name> nodes
    TXMLDocHelper.GetPascalNameList(
      fXMLDoc, fXMLDoc.FindFirstChildNode(RoutineNode, RefName), Result
    );
  except
    HandleCorruptDatabase(ExceptObject);
  end;
end;

function TXMLDataReader.GetRoutineUnits(const Routine: string): IStringList;
  {Get list of all units referenced by a snippet.
    @param Routine [in] Name of required snippet.
    @return List of unit names.
  }
begin
  Result := GetRoutineReferences(Routine, cUnitsNode);
end;

function TXMLDataReader.GetRoutineXRefs(const Routine: string): IStringList;
  {Get list of all snippets that are cross referenced by a snippet.
    @param Routine [in] Name of snippet we need cross references for.
    @return List of snippet names.
  }
begin
  Result := GetRoutineReferences(Routine, cXRefNode);
end;

procedure TXMLDataReader.HandleCorruptDatabase(const EObj: TObject);
  {Called when a corrupt database is encountered. Deletes all files and raises
  exception.
    @param EObj [in] Reference to exception that caused this method to be
      called.
    @except Exception always raised.
  }
resourcestring
  // Error message
  sParseError = 'User database file has an invalid format';
begin
  DeleteFiles(DataDir, '*.*');
  if EObj is EDOMParseError then
    Error(sDBError, [sParseError]);
  if (EObj is EDataIO) or (EObj is EXMLDocError) or (EObj is ECodeSnipXML)
    or (EObj is EFileStreamError) then
    // we have an ECodeSnip. Raise new exception containing old message
    Error(sDBError, [(EObj as Exception).Message])
  else
    // not an expected error: just re-raise
    raise EObj;
end;

function TXMLDataReader.ValidateDoc: Integer;
  {Validates XML document and gets file version.
    @return XML file version number.
    @except EDataIO raised if XML is not valid.
  }
begin
  TXMLDocHelper.ValidateProcessingInstr(fXMLDoc);
  Result := TXMLDocHelper.ValidateRootNode(
    fXMLDoc,
    cUserDataRootNode,
    cWatermark,
    TRange.Create(cEarliestVersion, cLatestVersion)
  );
  // Both a categories and a routines node must exist
  if fXMLDoc.FindNode(cUserDataRootNode + '\' + cCategoriesNode) = nil then
    Error(sMissingNode, [cCategoriesNode]);
  if fXMLDoc.FindNode(cUserDataRootNode + '\' + cRoutinesNode) = nil then
    Error(sMissingNode, [cRoutinesNode]);
end;

{ TXMLDataWriter }

procedure TXMLDataWriter.Finalise;
  {Finalises the database. Always called after all other methods.
  }
var
  FS: TFileStream;      // stream onto output file
  RequiredVer: Integer; // required file version number
begin
  // Set required database file version
  if fMinREMLVer < 3 then
    RequiredVer := cVersion3
  else
    RequiredVer := cVersion4;
  fXMLDoc.DocumentElement.SetAttribute(cRootVersionAttr, RequiredVer);
  // We use a TFileStream and TXMLDocument.SaveToStream rather than calling
  // TXMLDocument.SaveToFile so that any problem creating file is reported via
  // a known Delphi exception that can be handled.
  EnsureFolders(DataDir);
  try
    FS := TFileStream.Create(PathToXMLFile, fmCreate);
    try
      fXMLDoc.SaveToStream(FS);
    finally
      FreeAndNil(FS);
    end;
  except
    HandleException(ExceptObject);
  end;
end;

procedure TXMLDataWriter.HandleException(const EObj: TObject);
  {Handles exceptions raised by converting expected exceptions into ECodeSnip
  derived exceptions.
    @param EObj [in] Reference to exception to be handled.
    @except Always raises an exception.
  }
begin
  if (EObj is EFileStreamError) or (EObj is ECodeSnip) then
    raise EDataIO.Create(EObj as Exception);
  raise EObj;
end;

procedure TXMLDataWriter.Initialise;
  {Initialise the database. Always called before any other methods.
  }
var
  RootNode: IXMLNode;   // document root node
resourcestring
  // Comment written to XML file
  sFileComment = 'This file was generated by CodeSnip. Do not edit.';
begin
  try
    // Make sure database folder exists
    EnsureFolders(DataDir);

    // Delete current data files
    DeleteFiles(DataDir, '*.dat');

    // Initialise file count
    fFileNum := 0;

    // Assume lowest version of REML is required
    fMinREMLVer := TREMLAnalyser.FIRST_VERSION;

    // Create minimal document containing tags required to be present by other
    // methods
    fXMLDoc.Active := True;
    // xml processing instruction: id file as XML
    TXMLDocHelper.CreateXMLProcInst(fXMLDoc);
    // comments
    TXMLDocHelper.CreateComment(fXMLDoc, sFileComment);
    // root node: assume latest version needed
    RootNode := TXMLDocHelper.CreateRootNode(
      fXMLDoc, cUserDataRootNode, cWatermark, cLatestVersion
    );
    // empty <categories> and <routines> nodes
    fCategoriesNode := fXMLDoc.CreateElement(RootNode, cCategoriesNode);
    fRoutinesNode := fXMLDoc.CreateElement(RootNode, cRoutinesNode);
  except
    HandleException(ExceptObject);
  end;
end;

procedure TXMLDataWriter.WriteCatProps(const CatName: string;
  const Props: TCategoryData);
  {Write the properties of a category. Always called before WriteCatRoutines for
  a given category, so can be used to perform any per-category initialisation.
    @param CatName [in] Name of category.
    @param Props [in] Properties of category.
  }
var
  CatNode: IXMLNode;  // referenced to required category node
begin
  try
    // Create <category id='CatName'> node
    CatNode := fXMLDoc.CreateElement(fCategoriesNode, cCategoryNode);
    CatNode.Attributes[cCategoryIdAttr] := CatName;
    fXMLDoc.CreateElement(CatNode, cDescriptionNode, Props.Desc);
  except
    HandleException(ExceptObject);
  end;
end;

procedure TXMLDataWriter.WriteCatRoutines(const CatName: string;
  const Routines: IStringList);
  {Write the list of snippets belonging to a category. Always called after
  WriteCatProps for any given category.
    @param CatName [in] Name of category.
    @param Routines [in] List of names of snippets.
  }
var
  CatNode: IXMLNode;  // reference to required category node
begin
  try
    // Don't write list if no routines
    if Routines.Count = 0 then
      Exit;
    // Find required category node
    CatNode := FindCategoryNode(CatName);
    Assert(Assigned(CatNode),
      ClassName + '.WriteCatRoutines: Can''t find category node');
    // Write the list
    WriteNameList(CatNode, cCatRoutinesNode, cPascalNameNode, Routines);
  except
    HandleException(ExceptObject);
  end;
end;

procedure TXMLDataWriter.WriteNameList(const Parent: IXMLNode; const ListName,
  ItemName: string; const Items: IStringList);
  {Writes a list of names to XML.
    @param Parent [in] Reference to node under which list is to be stored.
    @param ListName [in] Name of tag that encloses the list items.
    @param ItemName [in] Name of each list item tag.
    @param Items [in] List of names in list. One ItemName node per name.
  }
var
  ListNode: IXMLNode; // reference to enclosing list node
  Item: string;       // a name item in list
begin
  ListNode := fXMLDoc.CreateElement(Parent, ListName);
  for Item in Items do
    fXMLDoc.CreateElement(ListNode, ItemName, Item);
end;

procedure TXMLDataWriter.WriteReferenceList(const RoutineName, ListName: string;
  const Items: IStringList);
  {Writes a snippet's reference list to XML.
    @param RoutineName [in] Name of snippet whose reference list is to be
      written.
    @param ListName [in] Name of tag that encloses list entry.
    @param Items [in] List of items in reference list.
  }
var
  RoutineNode: IXMLNode;  // reference to routine's node
begin
  try
    // Don't write list tags if no items
    if Items.Count = 0 then
      Exit;
    // Find <routine name='RoutineName'> tag
    RoutineNode := FindRoutineNode(RoutineName);
    Assert(Assigned(RoutineNode),
      ClassName + '.WriteReferenceList: Can''t find routine node');
    // Write the list
    TXMLDocHelper.WritePascalNameList(
      fXMLDoc, RoutineNode, ListName, Items
    );
  except
    HandleException(ExceptObject);
  end;
end;

procedure TXMLDataWriter.WriteRoutineDepends(const RoutineName: string;
  const Depends: IStringList);
  {Write the list of snippets on which a snippet depends.
    @param RoutineName [in] Name of snippet.
    @param Depends [in] List of snippet names.
  }
begin
  WriteReferenceList(RoutineName, cDependsNode, Depends);
end;

procedure TXMLDataWriter.WriteRoutineProps(const RoutineName: string;
  const Props: TSnippetData);
  {Write the properties of a snippet. Always called after all categories are
  written and before WriteRoutineUnits, so can be used to perform any per-
  routine intialisation.
    @param RoutineName [in] Name of snippet.
    @param Props [in] Properties of snippet.
  }
var
  RoutineNode: IXMLNode;        // routine's node
  FileName: string;             // name of file where source code stored
  AREMLVer: TREMLVersion;       // version of REML required for a snippet
const
  // mask used to format source code file name
  cFileNameMask = '%d.dat';
begin
  try
    // Create <routine id='RoutineName'> node
    RoutineNode := fXMLDoc.CreateElement(fRoutinesNode, cRoutineNode);
    RoutineNode.Attributes[cRoutineNameAttr] := RoutineName;
    // Add properties
    fXMLDoc.CreateElement(RoutineNode, cCatIdNode, Props.Cat);
    fXMLDoc.CreateElement(RoutineNode, cDescriptionNode, Props.Desc);
    // source code is written to file and filename stored in XML
    Inc(fFileNum);
    FileName := Format(cFileNameMask, [fFileNum]);
    StringToFile(Props.SourceCode, DataFile(FileName));
    fXMLDoc.CreateElement(RoutineNode, cSourceCodeFileNode, FileName);
    // extra property is only written if value exists
    if not Props.Extra.IsEmpty then
    begin
      fXMLDoc.CreateElement(
        RoutineNode,
        cExtraNode,
        TRoutineExtraHelper.BuildREMLMarkupLowestVer(Props.Extra)
      );
      // note if a later REML version is required
      AREMLVer := TREMLAnalyser.LowestWriterVersion(Props.Extra);
      if AREMLVer > fMinREMLVer then
        fMinREMLVer := AREMLVer;
    end;
    // Kind property replaces StandardFormat
    TXMLDocHelper.WriteSnippetKind(fXMLDoc, RoutineNode, Props.Kind);
    // compiler results value: only write known results
    TXMLDocHelper.WriteCompilerResults(
      fXMLDoc, RoutineNode, Props.CompilerResults
    );
  except
    HandleException(ExceptObject);
  end;
end;

procedure TXMLDataWriter.WriteRoutineUnits(const RoutineName: string;
  const Units: IStringList);
  {Write the list of units required by a snippet.
    @param RoutineName [in] Name of snippet.
    @param Units [in] List of names of required units.
  }
begin
  WriteReferenceList(RoutineName, cUnitsNode, Units);
end;

procedure TXMLDataWriter.WriteRoutineXRefs(const RoutineName: string;
  const XRefs: IStringList);
  {Write the list of snippets that a snippet cross-references.
    @param RoutineName [in] Name of snippet.
    @param XRefs [in] List of routine snippets.
  }
begin
  WriteReferenceList(RoutineName, cXRefNode, XRefs);
end;

end.


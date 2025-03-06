{
 * This Source Code Form is subject to the terms of the Mozilla Public License,
 * v. 2.0. If a copy of the MPL was not distributed with this file, You can
 * obtain one at https://mozilla.org/MPL/2.0/
 *
 * Copyright (C) 2008-2023, Peter Johnson (gravatar.com/delphidabbler).
 *
 * Implements classes that can read and write databases stored in XML format
 * with associated source code files.
}


unit DB.IO.Vault.CS4;


interface


uses
  // Delphi
  XMLIntf,
  // Project
  DB.MetaData,
  DB.UCategory,
  DB.USnippet,
  DB.IO.Vault,
  UIStringList,
  UREMLDataIO,
  UXMLDocumentEx;


type

  ///  <summary>Base class for classes that read and write vault data in the
  ///  CodeSnip 4 user data format.</summary>
  TCS4VaultStorage = class(TInterfacedObject)
  strict protected
    var
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
    function FindCategoryNode(const CatID: string): IXMLNode;
      {Finds a specified category node in the file.
        @param CatID [in] Id of required category.
        @return Required node or nil if node doesn't exist.
      }
    function FindSnippetNode(const SnippetKey: string): IXMLNode;
      {Finds a specified snippet node for a snippet in the file.
        @param SnippetKey [in] Key of required snippet.
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

  ///  <summary>Reads a vault's data from storage in the CodeSnip 4 user data
  ///  format.</summary>
  TCS4VaultStorageReader = class(TCS4VaultStorage,
    IVaultStorageReader
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
    function GetSnippetReferences(const SnippetKey, RefName: string):
      IStringList;
      {Get list of all specified references made by a snippet.
        @param SnippetKey [in] Key of required snippet.
        @param RefName [in] Name of node containing snippet's references.
        @return List of references.
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
    function GetAllCatIDs: IStringList;
      {Get ids of all categories in database.
        @return List of category IDs.
      }
    procedure GetCatProps(const CatID: string; var Props: TCategoryData);
      {Get properties of a category.
        @param CatID [in] Id of required category.
        @param Props [in/out] Empty properties passed in. Record fields set to
          values of category properties by implementor.
      }
    function GetCatSnippets(const CatID: string): IStringList;
      {Get keys of all snippets in a category.
        @param CatID [in] Id of category containing snippets.
        @return List of snippet keys.
      }
    procedure GetSnippetProps(const SnippetKey: string;
      var Props: TSnippetData);
      {Get properties of a snippet.
        @param SnippetKey [in] Key of required snippet.
        @param Props [in/out] Empty properties passed in. Record fields set to
          values of snippet properties.
      }
    function GetSnippetXRefs(const SnippetKey: string): IStringList;
      {Get list of all snippets that are cross referenced by a snippet.
        @param SnippetKey [in] Key of snippet we need cross references for.
        @return List of snippet keys.
      }
    function GetSnippetDepends(const SnippetKey: string): IStringList;
      {Get list of all snippets on which a given snippet depends.
        @param SnippetKey [in] Key of required snippet.
        @return List of snippet keys.
      }
    function GetSnippetUnits(const SnippetKey: string): IStringList;
      {Get list of all units referenced by a snippet.
        @param SnippetKey [in] Key of required snippet.
        @return List of unit names.
      }

    ///  <summary>Gets the vault's meta data.</summary>
    ///  <returns><c>TMetaData</c>. Null value.</returns>
    ///  <remarks>
    ///  <para>Meta data is not supported by the data format.</para>
    ///  <para>Method of <c>IDataReader</c>.</para>
    ///  </remarks>
    function GetMetaData: TMetaData;
  end;

  ///  <summary>Writes a vault's data to storage in the CodeSnip 4 user data
  ///  format.</summary>
  TCS4VaultStorageWriter = class(TCS4VaultStorage,
    IVaultStorageWriter
  )
  strict private
    fFileNum: Integer;          // Number of next available unused data file
    fSnippetsNode: IXMLNode;    // Reference to snippets node in document
    fCategoriesNode: IXMLNode;  // Reference to categories node in document
    procedure WriteNameList(const Parent: IXMLNode;
      const ListName, ItemName: string; const Items: IStringList);
      {Writes a list of names to XML.
        @param Parent [in] Reference to node under which list is to be stored.
        @param ListName [in] Name of tag that encloses the list items.
        @param ItemName [in] Name of each list item tag.
        @param Items [in] List of names in list. One ItemName node per name.
      }
    procedure WriteReferenceList(const SnippetKey, ListName: string;
      const Items: IStringList);
      {Writes a snippet's reference list to XML.
        @param SnippetKey [in] Key of snippet whose reference list is to be
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
  public
    { IDataWriter methods }
    procedure Initialise;
      {Initialise the database. Always called before any other methods.
      }
    procedure WriteCatProps(const CatID: string; const Props: TCategoryData);
      {Write the properties of a category. Always called before WriteCatSnippets
      for a given category, so can be used to perform any per-category
      initialisation.
        @param CatID [in] ID of category.
        @param Props [in] Properties of category.
      }
    procedure WriteCatSnippets(const CatID: string;
      const SnipList: IStringList);
      {Write the list of snippets keys belonging to a category. Always called
      after WriteCatProps for any given category.
        @param CatID [in] ID of category.
        @param SnipList [in] List of snippet keys.
      }
    procedure WriteSnippetProps(const SnippetKey: string;
      const Props: TSnippetData);
      {Write the properties of a snippet. Always called after all categories are
      written and before WriteSnippetUnits, so can be used to perform any per-
      snippet intialisation.
        @param SnippetKey [in] Snippet's key.
        @param Props [in] Properties of snippet.
      }
    procedure WriteSnippetUnits(const SnippetKey: string;
      const Units: IStringList);
      {Write the list of units required by a snippet.
        @param SnippetKey [in] Snippet's key.
        @param Units [in] List of names of required units.
      }
    procedure WriteSnippetDepends(const SnippetKey: string;
      const Depends: IStringList);
      {Write the list of snippets on which a snippet depends.
        @param SnippetKey [in] Snippet's key.
        @param Depends [in] List of snippet keys.
      }
    procedure WriteSnippetXRefs(const SnippetKey: string;
      const XRefs: IStringList);
      {Write the list of snippets that a snippet cross-references.
        @param SnippetKey [in] Snippet's key.
        @param XRefs [in] List of snippet keys.
      }

    ///  <summary>Writes the vault's meta data.</summary>
    ///  <param name="AMetaData"><c>TMetaData</c> [in] Meta data to be written.
    ///  </param>
    ///  <remarks>
    ///  <para>This data format does not support metadata, so this method does
    ///  nothing.</para>
    ///  <para>Method of <c>IDataWriter</c>.</para>
    ///  </remarks>
    procedure WriteMetaData(const AMetaData: TMetaData);

    procedure Finalise;
      {Finalises the database. Always called after all other methods.
      }
  end;


implementation


uses
  // Delphi
  SysUtils,
  Classes,
  ActiveX,
  XMLDom,
  // Project
  ActiveText.UMain,
  DB.IO.Common.CS4,
  DB.USnippetKind,
  UConsts,
  UExceptions,
  UIOUtils,
  USnippetExtraHelper,
  UStructs,
  UUtils,
  UXMLDocHelper;


type
  TCS4VaultFormatHelper = class(TCS4FormatHelper)
  public
    const
      // Database file name
      DatabaseFileName = 'database.xml';
      // File markers: attributes of root node
      // watermark (never changes for all versions)
      Watermark = '531257EA-1EE3-4B0F-8E46-C6E7F7140106';
      // supported file format versions
      EarliestVersion = 1;
      LatestVersion = 6;
      // node & attribute names
      CatIdNodeName = 'cat-id';
      UserDataRootNodeName = 'codesnip-data';
      CategoriesNodeName = 'categories';
      CategoryNodeName = 'category';
      CategoryNodeIdAttrName = 'id';
      CatSnippetsNodeName = 'cat-routines';
      SourceCodeNodeName = 'source-code';
      XRefNodeName = 'xref';
  end;


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

{ TCS4VaultStorage }

resourcestring
  // Error message
  sMissingNode = 'Document has no %s node.';

constructor TCS4VaultStorage.Create(const DBDir: string);
  {Class constructor. Creates object and XML document for a given database.
    @param DBDir [in] Directory where database is stored.
  }
begin
  inherited Create;
  fDBDir := DBDir;
  // For some reason we must call OleInitialize here rather than in
  // initialization section
  OleInitialize(nil);
  fXMLDoc := TCS4VaultFormatHelper.CreateXMLDoc;
end;

function TCS4VaultStorage.DataDir: string;
  {Gets name of directory storing the database being accessed. Path varies
  according to which database is being accessed.
    @return Path to directory.
  }
begin
  Result := ExcludeTrailingPathDelimiter(fDBDir);
end;

function TCS4VaultStorage.DataFile(const FileName: string): string;
  {Gets full path to a file name. Path depends on which database is being
  accessed.
    @param FileName [in] File name for which path is required.
    @return Required full path to file.
  }
begin
  Result := IncludeTrailingPathDelimiter(DataDir) + FileName;
end;

destructor TCS4VaultStorage.Destroy;
  {Class destructor. Tears down object.
  }
begin
  // For some reason we must call OleUninitialize here rather than in
  // finalization section
  OleUninitialize;
  inherited;
end;

function TCS4VaultStorage.FindCategoryNode(const CatID: string): IXMLNode;
  {Finds a specified category node in the file.
    @param CatID [in] Id of required category.
    @return Required node or nil if node doesn't exist.
  }
var
  CatListNode: IXMLNode;  // node that contains category nodes
begin
  Result := nil;
  // Find <categories> node
  CatListNode := fXMLDoc.FindNode(
    TCS4VaultFormatHelper.UserDataRootNodeName
      + '\'
      + TCS4VaultFormatHelper.CategoriesNodeName
  );
  if not Assigned(CatListNode) then
    Error(sMissingNode, [TCS4VaultFormatHelper.CategoriesNodeName]);
  // Find required <category> node
  Result := fXMLDoc.FindFirstChildNode(
    CatListNode,
    TCS4VaultFormatHelper.CategoryNodeName,
    TCS4VaultFormatHelper.CategoryNodeIdAttrName,
    CatID
  )
end;

function TCS4VaultStorage.FindSnippetNode(const SnippetKey: string): IXMLNode;
  {Finds a specified snippet node for a snippet in the file.
    @param SnippetKey [in] Key of required snippet.
    @return Required node or nil if node doesn't exist.
  }
var
  SnippetListNode: IXMLNode;  // list node that contains snippets nodes
begin
  Result := nil;
  // Find snippets node
  SnippetListNode := fXMLDoc.FindNode(
    TCS4VaultFormatHelper.UserDataRootNodeName
      + '\'
      + TCS4VaultFormatHelper.SnippetsNodeName
  );
  if not Assigned(SnippetListNode) then
    Error(sMissingNode, [TCS4VaultFormatHelper.SnippetsNodeName]);
  // Find required snippet node
  Result := fXMLDoc.FindFirstChildNode(
    SnippetListNode,
    TCS4VaultFormatHelper.SnippetNodeName,
    TCS4VaultFormatHelper.SnippetNodeNameAttr,
    SnippetKey
  );
end;

function TCS4VaultStorage.PathToXMLFile: string;
  {Gets fully specified path to the XML file. Path depends on which database is
  being accessed.
    @return Required path.
  }
begin
  Result := DataFile(TCS4VaultFormatHelper.DatabaseFileName);
end;

{ TCS4VaultStorageReader }

resourcestring
  // Error messages
  sNoCategoriesNode = 'No categories node in XML file';
  sCatNotFound = 'Can''t find reference to category "%s" in XML file';
  sSnippetNotFound = 'Can''t find reference to snippet key "%s" in XML file';
  sMissingSource = 'Source code file name missing for snippet "%s"';
  sDBError = 'The database is corrupt and had been deleted.' + EOL2 + '%s';

constructor TCS4VaultStorageReader.Create(const DBDir: string);
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
    fXMLDoc.LoadFromFile(PathToXMLFile);
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
    TCS4VaultFormatHelper.CreateXMLProcInst(fXMLDoc);
    RootNode := TCS4VaultFormatHelper.CreateRootNode(
      fXMLDoc,
      TCS4VaultFormatHelper.UserDataRootNodeName,
      TCS4VaultFormatHelper.Watermark,
      TCS4VaultFormatHelper.LatestVersion
    );
    fXMLDoc.CreateElement(RootNode, TCS4VaultFormatHelper.CategoriesNodeName);
    fXMLDoc.CreateElement(RootNode, TCS4VaultFormatHelper.SnippetsNodeName);
  end;
end;

function TCS4VaultStorageReader.DatabaseExists: Boolean;
  {Check if the database exists. This method is always called first. No
  other methods are called if this method returns false.
    @return True if database exists, False if not.
  }
begin
  Result := FileExists(PathToXMLFile);
end;

function TCS4VaultStorageReader.GetAllCatIDs: IStringList;
  {Get ids of all categories in database.
    @return List of category names.
  }
var
  CatListNode: IXMLNode;        // node containing list of categories
  CatNodes: IXMLSimpleNodeList; // list of all category nodes of categories
  CatNode: IXMLNode;            // a node in CatNodes
begin
  try
    Result := TIStringList.Create;
    CatListNode := fXMLDoc.FindNode(
      TCS4VaultFormatHelper.UserDataRootNodeName
        + '\'
        + TCS4VaultFormatHelper.CategoriesNodeName
    );
    if not Assigned(CatListNode) then
      Error(sNoCategoriesNode);
    CatNodes := fXMLDoc.FindChildNodes(
      CatListNode, TCS4VaultFormatHelper.CategoryNodeName
    );
    for CatNode in CatNodes do
      Result.Add(
        CatNode.Attributes[TCS4VaultFormatHelper.CategoryNodeIdAttrName]
      );
  except
    HandleCorruptDatabase(ExceptObject);
  end;
end;

procedure TCS4VaultStorageReader.GetCatProps(const CatID: string;
  var Props: TCategoryData);
  {Get properties of a category.
    @param CatID [in] Id of required category.
    @param Props [in/out] Empty properties passed in. Record fields set to
      values of category properties by implementor.
  }
var
  CatNode: IXMLNode;  // reference to node for required category
begin
  try
    CatNode := FindCategoryNode(CatID);
    if not Assigned(CatNode) then
      // Properties will not be requested for a category that doesn't exist in
      // this database, so this should never happen
      Error(sCatNotFound);
    Props.Desc := TCS4VaultFormatHelper.GetSubTagText(
      fXMLDoc, CatNode, TCS4VaultFormatHelper.DescriptionNodeName
    );
  except
    HandleCorruptDatabase(ExceptObject);
  end;
end;

function TCS4VaultStorageReader.GetCatSnippets(const CatID: string):
  IStringList;
  {Get keys of all snippets in a category.
    @param CatID [in] Id of category containing snippets.
    @return List of snippet keys.
  }
var
  CatNode: IXMLNode;  // reference to required category node
begin
  try
    Result := TIStringList.Create;
    CatNode := FindCategoryNode(CatID);
    if not Assigned(CatNode) then
      // This is not an error since it is possible that a category exists in
      // another database and loader will request info from here also
      Exit;
    TCS4VaultFormatHelper.GetPascalNameList(
      fXMLDoc, fXMLDoc.FindFirstChildNode(
        CatNode, TCS4VaultFormatHelper.CatSnippetsNodeName
      ),
      Result
    );
  except
    HandleCorruptDatabase(ExceptObject);
  end;
end;

function TCS4VaultStorageReader.GetMetaData: TMetaData;
begin
  // Meta data not supported by this data format
  Result := TMetaData.CreateNull;
end;

function TCS4VaultStorageReader.GetSnippetDepends(const SnippetKey: string):
  IStringList;
  {Get list of all snippets on which a given snippet depends.
    @param SnippetKey [in] Key of required snippet.
    @return List of snippet keys.
  }
begin
  Result := GetSnippetReferences(
    SnippetKey, TCS4VaultFormatHelper.DependsNodeName
  );
end;

procedure TCS4VaultStorageReader.GetSnippetProps(const SnippetKey: string;
  var Props: TSnippetData);
  {Get properties of a snippet.
    @param SnippetKey [in] Key of required snippet.
    @param Props [in/out] Empty properties passed in. Record fields set to
      values of snippet's properties.
  }
var
  SnippetNode: IXMLNode;  // node for required snippet

  function GetPropertyText(const PropTagName: string): string;
    {Gets text of a specified property.
      @param PropTagName [in] Tag associated with property.
      @return Property value from tag's text.
    }
  begin
    Result := TCS4VaultFormatHelper.GetSubTagText(
      fXMLDoc, SnippetNode, PropTagName
    );
  end;

  function GetSourceCodePropertyText: string;
    {Gets source code from a file referenced from XML file.
      @return Source code.
    }
  var
    DataFileName: string; // name of file containing source code
  begin
    DataFileName := GetPropertyText(TCS4VaultFormatHelper.SourceCodeNodeName);
    if DataFileName = '' then
      Error(sMissingSource, [SnippetKey]);
    try
      // load the file: before file v5 files used default encoding, from v5
      // UTF-8 with no BOM was used
      if fVersion < 5 then
        Result := TFileIO.ReadAllText(
          DataFile(DataFileName), TEncoding.Default, False
        )
      else
        Result := TFileIO.ReadAllText(
          DataFile(DataFileName), TEncoding.UTF8, False
        );
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
      @return True if standard format, False if not.
    }
  begin
    Result := TCS4VaultFormatHelper.GetStandardFormat(
      fXMLDoc, SnippetNode, False
    );
  end;

  function GetKindProperty: TSnippetKind;
    {Gets value of Kind node.
      @return Kind that matches node value.
    }
  var
    Default: TSnippetKind;  // default value
  begin
    // In earlier file format versions we have no Kind node, so we calculate
    // kind from StandardFormat value. If Kind node is present StandardFormat is
    // ignored.
    if GetStandardFormatProperty then
      Default := skRoutine
    else
      Default := skFreeform;
    Result := TCS4VaultFormatHelper.GetSnippetKind(
      fXMLDoc, SnippetNode, Default
    );
  end;

  function GetExtraProperty: IActiveText;
    {Builds Extra active text object from file data.
      @return Required active text.
    }
  begin
    // We get extra data from different nodes depending on file version
    try
      if fVersion = 1 then
        // version 1: build extra data from comments, credits and credits URL
        // nodes
        Result := TSnippetExtraHelper.BuildActiveText(
          GetPropertyText(TCS4VaultFormatHelper.CommentsNodeName),
          GetPropertyText(TCS4VaultFormatHelper.CreditsNodeName),
          GetPropertyText(TCS4VaultFormatHelper.CreditsUrlNodeName)
        )
      else
        // version 2 & later: build extra data from REML in extra node
        Result := TSnippetExtraHelper.BuildActiveText(
          GetPropertyText(TCS4VaultFormatHelper.ExtraNodeName)
        );
    except
      // error: provide an empty property value
      Result := TActiveTextFactory.CreateActiveText;
    end;
  end;

  function GetDescriptionProperty: IActiveText;
  var
    Desc: string; // text read from description node
  begin
    Desc := GetPropertyText(TCS4VaultFormatHelper.DescriptionNodeName);
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

begin
  try
    // Find snippet node
    SnippetNode := FindSnippetNode(SnippetKey);
    if not Assigned(SnippetNode) then
      Error(sSnippetNotFound, [SnippetKey]);
    // Snippet found: read properties
    Props.Cat := GetPropertyText(TCS4VaultFormatHelper.CatIdNodeName);
    Props.DisplayName := GetPropertyText(TCS4VaultFormatHelper.DisplayNameNodeName);
    Props.Kind := GetKindProperty;
    Props.Desc := GetDescriptionProperty;
    Props.Extra := GetExtraProperty;
    Props.SourceCode := GetSourceCodePropertyText;
    Props.HiliteSource := TCS4VaultFormatHelper.GetHiliteSource(
      fXMLDoc, SnippetNode, True
    );
    Props.CompilerResults := TCS4VaultFormatHelper.GetCompilerResults(
      fXMLDoc, SnippetNode
    );
  except
    HandleCorruptDatabase(ExceptObject);
  end;
end;

function TCS4VaultStorageReader.GetSnippetReferences(const SnippetKey,
  RefName: string): IStringList;
  {Get list of all specified references made by a snippet.
    @param SnippetKey [in] Key of required snippet.
    @param RefName [in] Name of node containing snippet's references.
    @return List of references.
  }
var
  SnippetNode: IXMLNode;  // node for required snippet
begin
    {TODO -cRefactor: Pull reading snippet references into TCS4VaultFormatHelper ???}
    try
    Result := TIStringList.Create;
    SnippetNode := FindSnippetNode(SnippetKey);
    if not Assigned(SnippetNode) then
      Error(sSnippetNotFound, [SnippetKey]);
    // References are contained in a list of contained <pascal-name> nodes
    TCS4VaultFormatHelper.GetPascalNameList(
      fXMLDoc, fXMLDoc.FindFirstChildNode(SnippetNode, RefName), Result
    );
  except
    HandleCorruptDatabase(ExceptObject);
  end;
end;

function TCS4VaultStorageReader.GetSnippetUnits(const SnippetKey: string):
  IStringList;
  {Get list of all units referenced by a snippet.
    @param SnippetKey [in] Key of required snippet.
    @return List of unit keys.
  }
begin
  Result := GetSnippetReferences(SnippetKey, TCS4VaultFormatHelper.UnitsNodeName);
end;

function TCS4VaultStorageReader.GetSnippetXRefs(const SnippetKey: string):
  IStringList;
  {Get list of all snippets that are cross referenced by a snippet.
    @param SnippetKey [in] Key of snippet we need cross references for.
    @return List of snippet keys.
  }
begin
  Result := GetSnippetReferences(
    SnippetKey, TCS4VaultFormatHelper.XRefNodeName
  );
end;

procedure TCS4VaultStorageReader.HandleCorruptDatabase(const EObj: TObject);
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

function TCS4VaultStorageReader.ValidateDoc: Integer;
  {Validates XML document and gets file version.
    @return XML file version number.
    @except EDataIO raised if XML is not valid.
  }
begin
  TCS4VaultFormatHelper.ValidateProcessingInstr(fXMLDoc);
  Result := TCS4VaultFormatHelper.ValidateRootNode(
    fXMLDoc,
    TCS4VaultFormatHelper.UserDataRootNodeName,
    TCS4VaultFormatHelper.Watermark,
    TRange.Create(
      TCS4VaultFormatHelper.EarliestVersion, TCS4VaultFormatHelper.LatestVersion
    )
  );
  // Both a categories and a snippets node must exist
  if fXMLDoc.FindNode(
    TCS4VaultFormatHelper.UserDataRootNodename
      + '\'
      + TCS4VaultFormatHelper.CategoriesNodeName
  ) = nil then
    Error(sMissingNode, [TCS4VaultFormatHelper.CategoriesNodeName]);
  if fXMLDoc.FindNode(
    TCS4VaultFormatHelper.UserDataRootNodeName
      + '\'
      + TCS4VaultFormatHelper.SnippetsNodeName
  ) = nil then
    Error(sMissingNode, [TCS4VaultFormatHelper.SnippetsNodeName]);
end;

{ TCS4VaultStorageWriter }

procedure TCS4VaultStorageWriter.Finalise;
  {Finalises the database. Always called after all other methods.
  }
var
  FS: TFileStream;      // stream onto output file
begin
  fXMLDoc.DocumentElement.SetAttribute(
    TCS4VaultFormatHelper.RootVersionAttr, TCS4VaultFormatHelper.LatestVersion
  );
  // We use a TFileStream and TXMLDocument.SaveToStream rather than calling
  // TXMLDocument.SaveToFile so that any problem creating file is reported via
  // a known Delphi exception that can be handled.
  EnsureFolders(DataDir);
  try
    FS := TFileStream.Create(PathToXMLFile, fmCreate);
    try
      fXMLDoc.Encoding := 'UTF-8';
      fXMLDoc.SaveToStream(FS);
    finally
      FreeAndNil(FS);
    end;
  except
    HandleException(ExceptObject);
  end;
end;

procedure TCS4VaultStorageWriter.HandleException(const EObj: TObject);
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

procedure TCS4VaultStorageWriter.Initialise;
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

    // Create minimal document containing tags required to be present by other
    // methods
    fXMLDoc.Active := True;
    // xml processing instruction: id file as XML
    TCS4VaultFormatHelper.CreateXMLProcInst(fXMLDoc);
    // comments
    TCS4VaultFormatHelper.CreateComment(fXMLDoc, sFileComment);
    // root node
    RootNode := TCS4VaultFormatHelper.CreateRootNode(
      fXMLDoc,
      TCS4VaultFormatHelper.UserDataRootNodeName,
      TCS4VaultFormatHelper.Watermark,
      TCS4VaultFormatHelper.LatestVersion
    );
    // empty categories and snippets nodes
    fCategoriesNode := fXMLDoc.CreateElement(
      RootNode, TCS4VaultFormatHelper.CategoriesNodeName
    );
    fSnippetsNode := fXMLDoc.CreateElement(
      RootNode, TCS4VaultFormatHelper.SnippetsNodeName
    );
  except
    HandleException(ExceptObject);
  end;
end;

procedure TCS4VaultStorageWriter.WriteCatProps(const CatID: string;
  const Props: TCategoryData);
  {Write the properties of a category. Always called before WriteCatSnippets for
  a given category, so can be used to perform any per-category initialisation.
    @param CatID [in] ID of category.
    @param Props [in] Properties of category.
  }
var
  CatNode: IXMLNode;  // referenced to required category node
begin
  try
    // Create <category id='CatName'> node
    CatNode := fXMLDoc.CreateElement(
      fCategoriesNode, TCS4VaultFormatHelper.CategoryNodeName
    );
    CatNode.Attributes[TCS4VaultFormatHelper.CategoryNodeIdAttrName] := CatID;
    fXMLDoc.CreateElement(
      CatNode, TCS4VaultFormatHelper.DescriptionNodeName, Props.Desc
    );
  except
    HandleException(ExceptObject);
  end;
end;

procedure TCS4VaultStorageWriter.WriteCatSnippets(const CatID: string;
  const SnipList: IStringList);
  {Write the list of snippets belonging to a category. Always called after
  WriteCatProps for any given category.
    @param CatID [in] ID of category.
    @param SnipList [in] List of snippet keyss.
  }
var
  CatNode: IXMLNode;  // reference to required category node
begin
  try
    // Don't write list if no snippets
    if SnipList.Count = 0 then
      Exit;
    // Find required category node
    CatNode := FindCategoryNode(CatID);
    Assert(Assigned(CatNode),
      ClassName + '.WriteCatSnippets: Can''t find category node');
    // Write the list
    {TODO -cRefactor: Call TCS4VaultFormatHelper.WritePascalNameList instead? If this
          is done then TCS4VaultFormatHelper.PascalNameNodeName can be made private
          again.}
    WriteNameList(
      CatNode,
      TCS4VaultFormatHelper.CatSnippetsNodeName,
      TCS4VaultFormatHelper.PascalNameNodeName,
      SnipList
    );
  except
    HandleException(ExceptObject);
  end;
end;

procedure TCS4VaultStorageWriter.WriteMetaData(const AMetaData: TMetaData);
begin
  // Do nothing: meta data not supported.
end;

procedure TCS4VaultStorageWriter.WriteNameList(const Parent: IXMLNode;
  const ListName, ItemName: string; const Items: IStringList);
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

procedure TCS4VaultStorageWriter.WriteReferenceList(const SnippetKey,
  ListName: string; const Items: IStringList);
  {Writes a snippet's reference list to XML.
    @param SnippetKey [in] Key of snippet whose reference list is to be
      written.
    @param ListName [in] Name of tag that encloses list entry.
    @param Items [in] List of items in reference list.
  }
var
  SnippetNode: IXMLNode;  // reference to snippet's node
begin
  try
    // Don't write list tags if no items
    if Items.Count = 0 then
      Exit;
    // Find snippet node
    SnippetNode := FindSnippetNode(SnippetKey);
    Assert(Assigned(SnippetNode),
      ClassName + '.WriteReferenceList: Can''t find snippet node');
    // Write the list
    TCS4VaultFormatHelper.WritePascalNameList(
      fXMLDoc, SnippetNode, ListName, Items
    );
  except
    HandleException(ExceptObject);
  end;
end;

procedure TCS4VaultStorageWriter.WriteSnippetDepends(const SnippetKey: string;
  const Depends: IStringList);
  {Write the list of snippets on which a snippet depends.
    @param SnippetKey [in] Snippet's key.
    @param Depends [in] List of snippet keys.
  }
begin
  {TODO -cRefactor: Pull writing Depends node into TCS4VaultFormatHelper}
  WriteReferenceList(SnippetKey, TCS4VaultFormatHelper.DependsNodeName, Depends);
end;

procedure TCS4VaultStorageWriter.WriteSnippetProps(const SnippetKey: string;
  const Props: TSnippetData);
  {Write the properties of a snippet. Always called after all categories are
  written and before WriteSnippetsUnits, so can be used to perform any per-
  snippet intialisation.
    @param SnippetKey [in] Snippet's key.
    @param Props [in] Properties of snippet.
  }
var
  SnippetNode: IXMLNode;  // snippet's node
  FileName: string;       // name of file where source code stored
const
  // mask used to format source code file name
  cFileNameMask = '%d.dat';
begin
  try
    // Create snippet node
    SnippetNode := fXMLDoc.CreateElement(
      fSnippetsNode, TCS4VaultFormatHelper.SnippetNodeName
    );
    SnippetNode.Attributes[TCS4VaultFormatHelper.SnippetNodeNameAttr] := SnippetKey;
    // Add properties
    fXMLDoc.CreateElement(
      SnippetNode, TCS4VaultFormatHelper.CatIdNodeName, Props.Cat
    );
    // description node is written even if empty (which it shouldn't be)
    fXMLDoc.CreateElement(
      SnippetNode,
      TCS4VaultFormatHelper.DescriptionNodeName,
      TSnippetExtraHelper.BuildREMLMarkup(Props.Desc)
    );
    // source code is written to a UTF-8 encoded file with no BOM and filename
    // is stored in XML
    Inc(fFileNum);
    FileName := Format(cFileNameMask, [fFileNum]);
    TFileIO.WriteAllText(
      DataFile(FileName), Props.SourceCode, TEncoding.UTF8, False
    );
    fXMLDoc.CreateElement(
      SnippetNode, TCS4VaultFormatHelper.SourceCodeNodeName, FileName
    );
    {TODO -cRefactor: Move following method call into TCS4VaultFormatHelper}
    fXMLDoc.CreateElement(
      SnippetNode,
      TCS4VaultFormatHelper.HighlightSourceNodeName,
      IntToStr(Ord(Props.HiliteSource))
    );
    fXMLDoc.CreateElement(
      SnippetNode, TCS4VaultFormatHelper.DisplayNameNodeName, Props.DisplayName
    );
    {TODO -cRefactor: Move code that writes Extra into TCS4VaultFormatHelper}
    // extra node is only written if extra property has a value
    if Props.Extra.HasContent then
    begin
      fXMLDoc.CreateElement(
        SnippetNode,
        TCS4VaultFormatHelper.ExtraNodeName,
        TSnippetExtraHelper.BuildREMLMarkup(Props.Extra)
      );
    end;
    TCS4VaultFormatHelper.WriteSnippetKind(fXMLDoc, SnippetNode, Props.Kind);
    // only known compiler results are written
    TCS4VaultFormatHelper.WriteCompilerResults(
      fXMLDoc, SnippetNode, Props.CompilerResults
    );
  except
    HandleException(ExceptObject);
  end;
end;

procedure TCS4VaultStorageWriter.WriteSnippetUnits(const SnippetKey: string;
  const Units: IStringList);
  {Write the list of units required by a snippet.
    @param SnippetKey [in] Snippet's key.
    @param Units [in] List of names of required units.
  }
begin
  WriteReferenceList(SnippetKey, TCS4VaultFormatHelper.UnitsNodeName, Units);
end;

procedure TCS4VaultStorageWriter.WriteSnippetXRefs(const SnippetKey: string;
  const XRefs: IStringList);
  {Write the list of snippets that a snippet cross-references.
    @param SnippetKey [in] Snippet's key.
    @param XRefs [in] List of cross references snippets.
  }
begin
  WriteReferenceList(SnippetKey, TCS4VaultFormatHelper.XRefNodeName, XRefs);
end;

end.


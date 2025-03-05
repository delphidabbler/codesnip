{
 * This Source Code Form is subject to the terms of the Mozilla Public License,
 * v. 2.0. If a copy of the MPL was not distributed with this file, You can
 * obtain one at https://mozilla.org/MPL/2.0/
 *
 * Copyright (C) 2025, Peter Johnson (gravatar.com/delphidabbler).
 *
 * Implements classes that can read and write vaults stored in the CodeSnip
 * Vault native data format.
}


unit DB.IO.Vault.Native;

interface

uses
  // Delphi
  XMLIntf,
  // Project
  Compilers.UGlobals,
  DB.MetaData,
  DB.UCategory,
  DB.USnippet,
  DB.USnippetKind,
  DB.IO.Vault,
  UIStringList,
  UVersionInfo,
  UXMLDocumentEx;

type

  ///  <summary>Base class for classes that read and write vault data in the
  ///  CodeSnip Vault native data format.</summary>
  TNativeVaultStorage = class abstract(TInterfacedObject)
  strict private
    var
      ///  <summary>Value of <c>DataDirectory</c> property.</summary>
      fDataDirectory: string;
      ///  <summary>Value of <c>XMLDoc</c> property.</summary>
      fXMLDoc: IXMLDocumentEx;
  strict protected
    const
      ///  <summary>Name of vault's XML file.</summary>
      XMLFileName = 'vault.xml';
      ///  <summary>Extension used for source code files.</summary>
      SourceCodeFileExt = '.source';
      ///  <summary>Name of vaults's license file, if any.</summary>
      LicenseTextFileName = 'license.txt';
      ///  <summary>Watermark included in all native data format files.
      ///  </summary>
      Watermark = '882BAD68-8C3E-44E3-BBF4-17E55143DAF8';
      ///  <summary>Version number of the earliest supported data file format.
      ///  </summary>
      EarliestFileVersion: TVersionNumber = (V1: 1; V2: 0; V3: 0; V4: 0);
      ///  <summary>Version number of the current data file format.</summary>
      CurrentFileVersion: TVersionNumber = (V1: 1; V2: 0; V3: 0; V4: 0);

      // XML node and attribute names
      RootNodeName = 'vault';
      RootNodeWatermarkAttr = 'watermark';
      RootNodeVersionMajorAttr = 'version-major';
      RootNodeVersionMinorAttr = 'version-minor';
      CategoriesNodeName = 'categories';
      CategoryNodeName = 'category';
      CategoryNodeIdAttr = 'id';
      CategoryDescriptionNodeName = 'description';
      CategorySnippetsListNodeName = 'snippet-keys';
      CategorySnippetsListItemNodeName = 'key';
      SnippetsNodeName = 'snippets';
      SnippetNodeName = 'snippet';
      SnippetNodeKeyAttr = 'key';
      SnippetNodeCategoryAttr = 'category';
      SnippetNodeKindAttr = 'kind';
      SnippetDescriptionNodeName = 'description';
      SnippetSourceNodeName = 'source-code';
      SnippetSourceNodeFileAttr = 'file-name';
      SnippetSourceNodeLanguageAttr = 'language';
      SnippetDisplayNameNodeName = 'display-name';
      SnippetNotesNodeName = 'notes';
      SnippetCompileResultsNodeName = 'compile-results';
      SnippetCompilerNodeName = 'compiler';
      SnippetCompilerNodeIdAttr = 'id';
      SnippetCompilerNodeResultAttr = 'result';
      SnippetTestInfoNodeName = 'tests';
      SnippetTestInfoNodeLevelAttr = 'level';
      SnippetTestInfoNodeUrlAttr = 'url';
      SnippetUnitsListNodeName = 'required-units';
      SnippetUnitsListItemNodeName = 'unit';
      SnippetDependsListNodeName = 'required-snippets';
      SnippetDependsListItemNodeName = 'key';
      SnippetXRefsListNodeName = 'xrefs';
      SnippetXRefsListItemNodeName = 'key';
      LicenseNodeName = 'license';
      LicenseNodeSPDXAttr = 'spdx';
      LicenseNodeNameAttr = 'name';
      LicenseNodeURLAttr = 'url';
      LicenseNodeLicenseFileAttr = 'license-file-name';
      CopyrightNodeName = 'copyright';
      CopyrightNodeDateAttr = 'date';
      CopyrightNodeHolderAttr = 'holder';
      CopyrightNodeURLAttr = 'holder-url';
      CopyrightContributorsListNodeName = 'contributors';
      CopyrightContributorsListItemNodeName = 'name';
      AcknowledgementsListNodeName = 'acknowledgements';
      AcknowledgementsListItemNodeName = 'name';

      ///  <summary>Map of snippet kinds to the representative value used in the
      ///  XML file.</summary>
      SnippetKindValues: array[TSnippetKind] of string = (
        'freeform', 'routine', 'const', 'type', 'unit', 'class'
      );

      ///  <summary>Map of compile result values to the representative value
      ///  used in the XML file.</summary>
      ///  <remarks><c>crWarning</c> is not supported and maps to the same value
      ///  as <c>crSuccess</c>.</remarks>
      CompileResultValues: array[TCompileResult] of Char = ('y', 'y', 'n', 'q');

      ///  <summary>Map of compiler IDs to the representative value used in the
      ///  XML file.</summary>
      CompilerIDs: array[TCompilerID] of string = (
        'd2', 'd3', 'd4', 'd5', 'd6', 'd7',
        'd2005', 'd2006', 'd2007', 'd2009', 'd2010',
        'dxe', 'dxe2', 'dxe3', 'dxe4', 'dxe5', 'dxe6', 'dxe7', 'dxe8',
        'd10.0', 'd10.1', 'd10.2', 'd10.3', 'd10.4', 'd11', 'd12',
        'fpc'
      );

      ///  <summary>Map of test information to the representative value used in
      ///  the XML file.</summary>
      TestInfoValues: array[TSnippetTestInfo] of string = (
        'none', 'basic', 'advanced', 'unit-tests', 'demo-code'
      );

  strict protected

    ///  <summary>Directory containing the vault.</summary>
    property DataDirectory: string read fDataDirectory;

    ///  <summary>XML document object.</summary>
    property XMLDoc: IXMLDocumentEx read fXMLDOc;

    ///  <summary>Creates and activates a minimal XML document containing a
    ///  processing instruction and a root node with the required attributes.
    ///  </summary>
    ///  <returns><c>IXMLNode</c>. Reference to the root node.</returns>
    function InitXMLDocAndRootNode: IXMLNode;

    ///  <summary>Finds a specified category node.</summary>
    ///  <param name="ACatID"><c>string</c> [in] ID of required category.
    ///  </param>
    ///  <returns><c>IXMLNode</c>. Found node or nil if the node was not found.
    ///  </returns>
    function FindCategoryNode(const ACatID: string): IXMLNode;

    ///  <summary>Finds a specified snippet node.</summary>
    ///  <param name="ASnippetKey"><c>string</c> [in] Key of required snippet.
    ///  </param>
    ///  <returns><c>IXMLNode</c>. Found node or nil if the node was not found.
    ///  </returns>
    function FindSnippetNode(const ASnippetKey: string): IXMLNode;

    ///  <summary>Returns fully specified path of the XML file.</summary>
    function PathToXMLFile: string;

    ///  <summary>Returns fully specified path of a file within the vault
    ///  directory.</summary>
    function FilePath(const AFileName: string): string;

  public

    ///  <summary>Object constructor. Creates an XML document to access the
    ///  vaults's XML file.</summary>
    ///  <param name="ADataDirectory"><c>string</c> [in] Full path to the
    ///  directory that contains the vault's data files.</param>
    constructor Create(const ADataDirectory: string);

  end;

  ///  <summary>Reads a vault's data from storage in the CodeSnip Vault native
  ///  format.</summary>
  TNativeVaultStorageReader = class sealed(TNativeVaultStorage,
    IVaultStorageReader
  )
  strict private
    var
      ///  <summary>Flag that indicates if unit & depends-upon lists are
      ///  permitted.</summary>
      fCanReadRequiredLists: Boolean;

    ///  <summary>Validates an XML document loaded from storage. Returns
    ///  normally on success or raises exception on error.</summary>
    ///  <exception><c>EDataIO</c> raised if there is no valid root node with
    ///  the expected watermark and a valid version number.</exception>
    procedure ValidateDoc;

    ///  <summary>Reads a list of text nodes that each have the same tag name,
    ///  within an enclosing node.</summary>
    ///  <param name="AParentNode"><c>IXMLNode</c> [in] Reference to the parent
    ///  node of the node that encloses the list items.</param>
    ///  <param name="AListNodeName"><c>string</c> [in] Name of the node that
    ///  encloses the list items.</param>
    ///  <param name="AItemNodeName"><c>string</c> [in] Name of each list item
    ///  node.</param>
    ///  <returns><c>IStringList</c> [in] Text value of each list item.
    ///  </returns>
    function GetEnclosedListItems(const AParentNode: IXMLNode;
      const AListNodeName, AItemNodeName: string): IStringList;

    ///  <summary>Reads a list of items that are referenced by a given snippet.
    ///  </summary>
    ///  <param name="ASnippetKey"><c>string</c> [in] Key of snippet making the
    ///  references.</param>
    ///  <param name="AListNodeName"><c>string</c> [in] Name of the XML node
    ///  that contains the list items.</param>
    ///  <param name="AItemNodeName"><c>string</c> [in] Name of each list item
    ///  XML node within the list.</param>
    ///  <returns><c>IStringList</c> [in] List of referenced items.</returns>
    function GetSnippetReferences(const ASnippetKey, AListNodeName,
      AListItemName: string): IStringList;

    ///  <summary>Handles exceptions raised by converting expected exceptions
    ///  into <c>ECodeSnip</c> derived exceptions.</summary>
    ///  <param name="EObj"><c>TObject</c> [in] Reference to exception object to
    ///  be handled.</param>
    ///  <exception>Always raises an exception. Expected exceptions are
    ///  re-raised as <c>EDataIO</c> exceptions. Unexpected exceptions are
    ///  re-raised unchanged.</exception>
    procedure HandleException(const EObj: TObject);
  public

    ///  <summary>Object constructor. Loads XML from file if the vault exists,
    ///  otherwise creates a minimal empty document.</summary>
    ///  <param name="ADataDirectory"><c>string</c> [in] Full path to the
    ///  directory that contains the vault's data files.</param>
    constructor Create(const ADirectory: string);

    ///  <summary>Checks if the vault exists.</summary>
    ///  <returns><c>Boolean</c>. Returns <c>True</c> if the vault exists or
    ///  <c>False</c> if not.</returns>
    ///  <remarks>
    ///  <para>This method is always called before any other <c>IDataReader</c>
    ///  methods. If it returns <c>False</c> then no other <c>IDataReader</c>
    ///  methods are called. Therefore other methods can safely assume that the
    ///  vault exists.</para>
    ///  <para>Method of <c>IDataReader</c>.</para>
    ///  </remarks>
    function DatabaseExists: Boolean;
    {TODO -cRefactor: Rename DatabaseExists to VaultExists.}

    ///  <summary>Gets the unique IDs of all categories referenced in the
    ///  vault.</summary>
    ///  <returns><c>IStringList</c>. List of category IDs.</returns>
    ///  <remarks>Method of <c>IDataReader</c>.</remarks>
    function GetAllCatIDs: IStringList;

    ///  <summary>Gets the properties of a given category.</summary>
    ///  <param name="CatID"><c>string</c> [in] ID of the required category.
    ///  </param>
    ///  <param name="Props"><c>TCategoryData</c> [in/out] An empty,
    ///  initialised, properties record is passed in and a record with the
    ///  properties read from the vault data is passed out.</param>
    ///  <remarks>Method of <c>IDataReader</c>.</remarks>
    procedure GetCatProps(const CatID: string; var Props: TCategoryData);

    ///  <summary>Gets the unique keys of all snippets in a category within the
    ///  vault.</summary>
    ///  <param name="CatID"><c>string</c> [in] ID of the required category.
    ///  </param>
    ///  <returns><c>IStringList</c>. List of snippet keys.</returns>
    ///  <remarks>Method of <c>IDataReader</c>.</remarks>
    function GetCatSnippets(const CatID: string): IStringList;

    ///  <summary>Gets the properties of a given snippet.</summary>
    ///  <param name="SnippetKey"><c>string</c> [in] Key of the required
    ///  snippet.</param>
    ///  <param name="Props"><c>TSnippetData</c> [in/out] An empty,
    ///  initialised, properties record is passed in and a record with the
    ///  properties read from the vault data is passed out.</param>
    ///  <remarks>Method of <c>IDataReader</c>.</remarks>
    procedure GetSnippetProps(const SnippetKey: string;
      var Props: TSnippetData);

    ///  <summary>Gets a list of the keys of all snippets within the vault that
    ///  are cross-referenced by a given snippet.</summary>
    ///  <param name="SnippetKey"><c>string</c> [in] Key of snippet for which
    ///  cross references are required.</param>
    ///  <returns><c>IStringList</c>. List of keys of cross referenced snippets.
    ///  </returns>
    ///  <remarks>Method of <c>IDataReader</c>.</remarks>
    function GetSnippetXRefs(const SnippetKey: string): IStringList;

    ///  <summary>Gets a list of the keys of all snippets on which a given
    ///  snippet depends in order to compile.</summary>
    ///  <param name="SnippetKey"><c>string</c> [in] Key of snippet for which
    ///  dependencies are required.</param>
    ///  <returns><c>IStringList</c>. List of keys of required snippets.
    ///  </returns>
    ///  <remarks>Method of <c>IDataReader</c>.</remarks>
    function GetSnippetDepends(const SnippetKey: string): IStringList;

    ///  <summary>Gets a list of the keys of all unit which a given snippet
    ///  requires in order to compile.</summary>
    ///  <param name="SnippetKey"><c>string</c> [in] Key of snippet for which
    ///  units are required.</param>
    ///  <returns><c>IStringList</c>. List of required units.
    ///  </returns>
    ///  <remarks>Method of <c>IDataReader</c>.</remarks>
    function GetSnippetUnits(const SnippetKey: string): IStringList;

    ///  <summary>Gets the vault's meta data.</summary>
    ///  <returns><c>TMetaData</c>. The required meta data. Will be null if
    ///  is no meta data.</returns>
    ///  <remarks>Method of <c>IDataReader</c>.</remarks>
    function GetMetaData: TMetaData;
  end;

  ///  <summary>Writes a vault's data to storage in the CodeSnip Vault native
  ///  format.</summary>
  TNativeVaultStorageWriter = class sealed(TNativeVaultStorage,
    IVaultStorageWriter
  )
  strict private
    var
      ///  <summary>Reference to root node.</summary>
      fRootNode: IXMLNode;
      ///  <summary>Reference to top-level snippets XML node.</summary>
      fSnippetsNode: IXMLNode;
      ///  <summary>Reference to top-level categories XML node.</summary>
      fCategoriesNode: IXMLNode;
      ///  <summary>Flag that indicates if unit & depends-upon lists can be
      ///  written.</summary>
      fCanWriteRequiredLists: Boolean;

    ///  <summary>Handles exceptions raised by converting expected exceptions
    ///  into <c>ECodeSnip</c> derived exceptions.</summary>
    ///  <param name="EObj"><c>TObject</c> [in] Reference to exception object to
    ///  be handled.</param>
    ///  <exception>Always raises an exception. Expected exceptions are
    ///  re-raised as <c>EDataIO</c> exceptions. Unexpected exceptions are
    ///  re-raised unchanged.</exception>
    procedure HandleException(const EObj: TObject);

    ///  <summary>Writes a list of text nodes that each have the same tag name,
    ///  within an enclosing node.</summary>
    ///  <param name="AParent"><c>IXMLNode</c> [in] Reference to the parent node
    ///  of the node that encloses the list items.</param>
    ///  <param name="AListNodeName"><c>string</c> [in] Name of the node that
    ///  encloses the list items.</param>
    ///  <param name="AItemNodeName"><c>string</c> [in] Name of each list item
    ///  node.</param>
    ///  <param name="AItems"><c>IStringList</c> [in] Text value of each list
    ///  item.</param>
    procedure WriteEnclosedList(const AParent: IXMLNode;
      const AListNodeName, AItemNodeName: string; const AItems: IStringList);

    ///  <summary>Writes a reference list associated with a snippet to XML.
    ///  </summary>
    ///  <param name="ASnippetKey"><c>string</c> [in] Key of snippet.</param>
    ///  <param name="AListNodeName"><c>string</c> [in] Name of the XML node
    ///  that contains the list item.</param>
    ///  <param name="AItemNodeName"><c>string</c> [in] Name of each list item
    ///  XML node within the list.</param>
    ///  <param name="AItems"><c>IStringList</c> [in] List of referenced items.
    ///  </param>
    procedure WriteReferenceList(const ASnippetKey, AListNodeName,
      AItemNodeName: string; const AItems: IStringList);

  public
    ///  <summary>Initialises the vault write.</summary>
    ///  <remarks>
    ///  <para>Always called before all other <c>IDataWriter</c> methods.</para>
    ///  <para>Method of <c>IDataWriter</c>.</para>
    ///  </remarks>
    procedure Initialise;

    ///  <summary>Writes the properties of a given category.</summary>
    ///  <param name="CatID"><c>string</c> [in] ID of category.</param>
    ///  <param name="Props"><c>TCategoryData</c> [in] Properties of the
    ///  category.</param>
    ///  <remarks>
    ///  <para>Always called before <c>WriteCatSnippets</c> for a given
    ///  category, so can be used to perform any per-category initialisation.
    ///  </para>
    ///  <para>Method of <c>IDataWriter</c>.</para>
    ///  </remarks>
    procedure WriteCatProps(const CatID: string; const Props: TCategoryData);

    ///  <summary>Writes a list of keys of snippets belonging to a given
    ///  category.</summary>
    ///  <param name="CatID"><c>string</c> [in] ID of category.</param>
    ///  <param name="SnipList"><c>IStringList</c> [in] List of snippet keys.
    ///  </param>
    ///  <remarks>
    ///  <para>Always called after <c>WriteCatProps</c> for a given category.
    ///  </para>
    ///  <para>Method of <c>IDataWriter</c>.</para>
    ///  </remarks>
    procedure WriteCatSnippets(const CatID: string;
      const SnipList: IStringList);

    ///  <summary>Writes the property of a given snippet.</summary>
    ///  <param name="SnippetKey"><c>string</c> [in] Snippet's key.</param>
    ///  <param name="Props"><c>TSnippetData</c> [in] Properties of the snippet.
    ///  </param>
    ///  <remarks>
    ///  <para>Always called after all categories are written and before
    ///  <c>WriteSnippetUnits</c>, so can be used to perform any per-snippet
    ///  intialisation that is required.</para>
    ///  <para>Method of <c>IDataWriter</c>.</para>
    ///  </remarks>
    procedure WriteSnippetProps(const SnippetKey: string;
      const Props: TSnippetData);

    ///  <summary>Writes a list of units required to compile a snippet.
    ///  </summary>
    ///  <param name="SnippetKey"><c>string</c> [in] Snippet's key.</param>
    ///  <param name="Units"><c>IStringList</c> [in] List of unit names.</param>
    ///  <remarks>Method of <c>IDataWriter</c>.</remarks>
    procedure WriteSnippetUnits(const SnippetKey: string;
      const Units: IStringList);

    ///  <summary>Writes a list of keys of snippets that a snippet depends on in
    ///  order to compile.</summary>
    ///  <param name="SnippetKey"><c>string</c> [in] Snippet's key.</param>
    ///  <param name="Depends"><c>IStringList</c> [in] List of keys of required
    ///  snippets.</param>
    ///  <remarks>Method of <c>IDataWriter</c>.</remarks>
    procedure WriteSnippetDepends(const SnippetKey: string;
      const Depends: IStringList);

    ///  <summary>Writes a list of keys of snippets that are cross referenced
    ///  by a given snippet.</summary>
    ///  <param name="SnippetKey"><c>string</c> [in] Snippet's key.</param>
    ///  <param name="Depends"><c>IStringList</c> [in] List of keys of cross
    ///  referenced snippets.</param>
    ///  <remarks>Method of <c>IDataWriter</c>.</remarks>
    procedure WriteSnippetXRefs(const SnippetKey: string;
      const XRefs: IStringList);

    ///  <summary>Writes the vault's meta data.</summary>
    ///  <param name="AMetaData"><c>TMetaData</c> [in] Meta data to be written.
    ///  </param>
    ///  <remarks>Method of <c>IDataWriter</c>.</remarks>
    procedure WriteMetaData(const AMetaData: TMetaData);

    ///  <summary>Finalises the vault write.</summary>
    ///  <remarks>
    ///  <para>Always called after all other <c>IDataWriter</c> methods.</para>
    ///  <para>Method of <c>IDataWriter</c>.</para>
    ///  </remarks>
    procedure Finalise;
  end;

implementation

uses
  // Delphi
  SysUtils,
  Classes,
  IOUtils,
  ActiveX,
  XMLDom,
  // Project
  ActiveText.UMain,
  UExceptions,
  UIOUtils,
  USnippetExtraHelper,
  UStrUtils,
  UUtils,
  UXMLDocHelper;

resourcestring
  // TNativeDataRW error message
  sMissingNode = 'Document has no %s node.';
  // TNativeVaultStorageReader error messages
  sParseError = 'Error parsing XML file';
  sBadDataFormat = 'Invalid native vault data format: %s';
  sNoRootNode = 'Invalid document: no root element present';
  sBadRootName = 'Invalid document: root element must be named <%s>';
  sBadWatermark = 'Invalid document: watermark is incorrect';
  sBadVersion = 'Invalid document: unsupported document version %d.%d';
  sNoCategoriesNode = 'No categories node in XML file';
  sCatNotFound = 'Can''t find reference to category "%s" in XML file';
  sMissingSource = 'Source code file name missing for snippet "%s"';
  sBadKind = 'Missing or invalid snippet kind for snippet "%s"';
  sSnippetNotFound = 'Can''t find reference to snippet key "%s" in XML file';
  sBadTestInfo = 'Invalid test information for snippet "%s"';
  sMissingLicenseText = 'License text file "%s" is missing';

{ TNativeVaultStorage }

constructor TNativeVaultStorage.Create(const ADataDirectory: string);
begin
  inherited Create;
  fDataDirectory := ADataDirectory;
  // For some reason we must call OleInitialize here rather than in
  // initialization section
  OleInitialize(nil);
  fXMLDoc := TXMLDocHelper.CreateXMLDoc;
end;

function TNativeVaultStorage.FilePath(const AFileName: string): string;
begin
  Result := TPath.Combine(DataDirectory, AFileName);
end;

function TNativeVaultStorage.FindCategoryNode(const ACatID: string): IXMLNode;
var
  CatListNode: IXMLNode;  // node that contains category nodes
begin
  Result := nil;
  // Find <categories> node
  CatListNode := XMLDoc.FindNode(RootNodeName + '\' + CategoriesNodeName);
  if not Assigned(CatListNode) then
    raise EDataIO.CreateFmt(sMissingNode, [CategoriesNodeName]);
  // Find required <category> node
  Result := fXMLDoc.FindFirstChildNode(
    CatListNode, CategoryNodeName, CategoryNodeIdAttr, ACatID
  )
end;

function TNativeVaultStorage.FindSnippetNode(const ASnippetKey: string):
  IXMLNode;
var
  SnippetListNode: IXMLNode;  // list node that contains snippets nodes
begin
  Result := nil;
  // Find snippets node
  SnippetListNode := XMLDoc.FindNode(RootNodeName + '\' + SnippetsNodeName);
  if not Assigned(SnippetListNode) then
    raise EDataIO.CreateFmt(sMissingNode, [SnippetsNodeName]);
  // Find required snippet node
  Result := XMLDoc.FindFirstChildNode(
    SnippetListNode, SnippetNodeName, SnippetNodeKeyAttr, ASnippetKey
  );
end;

function TNativeVaultStorage.InitXMLDocAndRootNode: IXMLNode;
begin
  XMLDoc.Active := True;
  TXMLDocHelper.CreateXMLProcInst(XMLDoc);
  XMLDoc.Encoding := 'UTF-8';
  // root node
  Result := XMLDoc.CreateNode(RootNodeName);
  Result.SetAttribute(RootNodeWatermarkAttr, Watermark);
  Result.SetAttribute(RootNodeVersionMajorAttr, CurrentFileVersion.V1);
  Result.SetAttribute(RootNodeVersionMinorAttr, CurrentFileVersion.V2);
  XMLDoc.ChildNodes.Add(Result);
end;

function TNativeVaultStorage.PathToXMLFile: string;
begin
  Result := FilePath(XMLFileName);
end;

{ TNativeVaultStorageReader }

constructor TNativeVaultStorageReader.Create(const ADirectory: string);
var
  RootNode: IXMLNode; // reference to document's root node
begin
  inherited Create(ADirectory);
  if DatabaseExists then
  begin
    // Database exists: load it
    XMLDoc.LoadFromFile(PathToXMLFile);
    XMLDoc.Active := True;
    try
      ValidateDoc;
    except
      HandleException(ExceptObject);
    end;
  end
  else
  begin
    // Database doesn't exist: create sufficient nodes for main code to find
    RootNode := InitXMLDocAndRootNode;
    XMLDoc.CreateElement(RootNode, CategoriesNodeName);
    XMLDoc.CreateElement(RootNode, SnippetsNodeName);
  end;
end;

function TNativeVaultStorageReader.DatabaseExists: Boolean;
begin
  Result := TFile.Exists(PathToXMLFile);
end;

function TNativeVaultStorageReader.GetAllCatIDs: IStringList;
var
  CatListNode: IXMLNode;        // node containing list of categories
  CatNodes: IXMLSimpleNodeList; // list of all category nodes of categories
  CatNode: IXMLNode;            // a node in CatNodes
begin
  try
    Result := TIStringList.Create;
    CatListNode := XMLDoc.FindNode(RootNodeName + '\' + CategoriesNodeName);
    if not Assigned(CatListNode) then
      raise EDataIO.Create(sNoCategoriesNode);
    CatNodes := XMLDoc.FindChildNodes(CatListNode, CategoryNodeName);
    for CatNode in CatNodes do
      Result.Add(CatNode.Attributes[CategoryNodeIdAttr]);
  except
    HandleException(ExceptObject);
  end;
end;

procedure TNativeVaultStorageReader.GetCatProps(const CatID: string;
  var Props: TCategoryData);
var
  CatNode: IXMLNode;  // reference to node for required category
begin
  try
    CatNode := FindCategoryNode(CatID);
    if not Assigned(CatNode) then
      // Properties will not be requested for a category that doesn't exist in
      // this database, so this should never happen
      raise EDataIO.CreateFmt(sCatNotFound, [CatID]);
    Props.Desc := TXMLDocHelper.GetSubTagText(
      XMLDoc, CatNode, CategoryDescriptionNodeName
    );
  except
    HandleException(ExceptObject);
  end;
end;

function TNativeVaultStorageReader.GetCatSnippets(const CatID: string):
  IStringList;
var
  CatNode: IXMLNode;  // reference to required category node
begin
  try
    CatNode := FindCategoryNode(CatID);
    if not Assigned(CatNode) then
      {TODO -cVault: Check the following comment and decide if this still
              applies. Replace Exit with exception if necessary.}
      // This is not an error since it is possible that a category exists in
      // another vault and loader will request info from here also
      Exit(TIStringList.Create);
    Result := GetEnclosedListItems(
      CatNode, CategorySnippetsListNodeName, CategorySnippetsListItemNodeName
    );
  except
    HandleException(ExceptObject);
  end;
end;

function TNativeVaultStorageReader.GetEnclosedListItems(
  const AParentNode: IXMLNode; const AListNodeName, AItemNodeName: string):
  IStringList;
var
  ListNode: IXMLNode;
  ItemNode: IXMLNode;
  NodeList: IXMLSimpleNodeList;
begin
  Assert(Assigned(AParentNode),
    ClassName + '.GetEnclosedListItems: AParentNode is nil');
  Result := TIStringList.Create;
  ListNode := XMLDoc.FindFirstChildNode(AParentNode, AListNodeName);
  if not Assigned(ListNode) then
    Exit;   // it is sometimes permitted for snippet lists to be omitted
  NodeList := XMLDoc.FindChildNodes(ListNode, AItemNodeName);
  for ItemNode in NodeList do
    if ItemNode.IsTextElement then
      Result.Add(ItemNode.Text);
end;

function TNativeVaultStorageReader.GetMetaData: TMetaData;
var
  RootNode: IXMLNode;
  LicenseNode: IXMLNode;
  CopyrightNode: IXMLNode;
  LicenseTextFileName: string;
  LicenseText: string;
begin
  Result := TMetaData.Create([
    TMetaDataCap.License, TMetaDataCap.Copyright, TMetaDataCap.Acknowledgements
  ]);

  LicenseNode := XMLDoc.FindNode(RootNodeName + '\' + LicenseNodeName);
  if Assigned(LicenseNode) then
  begin
    LicenseTextFileName := LicenseNode.Attributes[LicenseNodeLicenseFileAttr];
    if (LicenseTextFileName <> '') then
    begin
      if not TFile.Exists(FilePath(LicenseTextFileName)) then
        raise EDataIO.CreateFmt(sMissingLicenseText, [LicenseTextFileName]);
      LicenseText := TFileIO.ReadAllText(
        FilePath(LicenseTextFileName), TEncoding.UTF8, False
      );
    end
    else
      LicenseText := '';
    Result.LicenseInfo := TLicenseInfo.Create(
      LicenseNode.Attributes[LicenseNodeNameAttr],
      LicenseNode.Attributes[LicenseNodeSPDXAttr],
      LicenseNode.Attributes[LicenseNodeURLAttr],
      LicenseText
    );
  end;

  CopyrightNode := XMLDoc.FindNode(RootNodeName + '\' + CopyrightNodeName);
  if Assigned(CopyrightNode) then
  begin
    Result.CopyrightInfo := TCopyrightInfo.Create(
      CopyrightNode.Attributes[CopyrightNodeDateAttr],
      CopyrightNode.Attributes[CopyrightNodeHolderAttr],
      CopyrightNode.Attributes[CopyrightNodeURLAttr],
      GetEnclosedListItems(
        CopyrightNode,
        CopyrightContributorsListNodeName,
        CopyrightContributorsListItemNodeName
      )
    );
  end;

  RootNode := XMLDoc.FindNode(RootNodeName);
  if not Assigned(RootNode) then
    raise EDataIO.Create(sNoRootNode);
  Result.Acknowledgements := GetEnclosedListItems(
    RootNode, AcknowledgementsListNodeName, AcknowledgementsListItemNodeName
  );
end;

function TNativeVaultStorageReader.GetSnippetDepends(const SnippetKey: string):
  IStringList;
begin
  if fCanReadRequiredLists then
    Result := GetSnippetReferences(
      SnippetKey, SnippetDependsListNodeName, SnippetDependsListItemNodeName
    )
  else
    Result := TIStringList.Create;
end;

procedure TNativeVaultStorageReader.GetSnippetProps(const SnippetKey: string;
  var Props: TSnippetData);
var
  SnippetNode: IXMLNode;  // node for required snippet

  // Gets text of a sepecified property that is stored in the named subtag of
  // the given parent node.
  function GetPropertyText(const AParentNode: IXMLNode;
    const ASubTagNodeName: string): string;
  begin
    if not Assigned(AParentNode) then
      Exit('');
    Result := TXMLDocHelper.GetSubTagText(XMLDoc, AParentNode, ASubTagNodeName);
  end;

  // Returns a reference to the source code node
  function SourceCodeNode: IXMLNode;
  begin
    Result := XMLDoc.FindFirstChildNode(SnippetNode, SnippetSourceNodeName);
  end;

  // Returns source code language from source code node attribute
  function GetSourceCodeLanguage: string;
  begin
    Result := SourceCodeNode.Attributes[SnippetSourceNodeLanguageAttr];
  end;

  // Checks if source code language is Pascal
  function IsPascalSource: Boolean;
  begin
    Result := GetSourceCodeLanguage = 'pascal';
  end;

  // Gets source code from file referenced in source code node attribute
  function GetSourceCodePropertyText: string;
  var
    DataFileName: string; // name of file containing source code
  begin
    DataFileName := SourceCodeNode.Attributes[SnippetSourceNodeFileAttr];
    if DataFileName = '' then
      raise EDataIO.CreateFmt(sMissingSource, [SnippetKey]);
    Result := TFileIO.ReadAllText(
      FilePath(DataFileName), TEncoding.UTF8, False
    );
  end;

  // Gets kind property value from snippet node attribute
  function GetKindProperty: TSnippetKind;

    // Looks up AKindStr in lookup table and passes out matching TSnippetKind
    // value, if found. Returns True if found & false if not
    function TryLookupKind(const AKindStr: string; out AKind: TSnippetKind):
      Boolean;
    var
      K: TSnippetKind;
    begin
      Result := False;
      for K := Low(SnippetKindValues) to High(SnippetKindValues) do
      begin
        if StrSameText(SnippetKindValues[K], AKindStr) then
        begin
          AKind := K;
          Exit(True);
        end;
      end;
    end;

  var
    Value: string;  // text value of Kind node
  begin
    if not IsPascalSource then
      // being permissive here: strictly speaking snippet kind attribute must
      // have value 'freeform'.
      Exit(skFreeform);

    Value := SnippetNode.Attributes[SnippetNodeKindAttr];
    if not TryLookupKind(Value, Result) then
      raise EDataIO.CreateFmt(sBadKind, [SnippetKey]);
  end;

  // Gets REML from the named child node of the snippet node and parses it as
  // active text.
  function GetActiveText(const ANodeName: string): IActiveText;
  var
    REML: string;
  begin
    REML := GetPropertyText(SnippetNode, ANodeName);
    if REML <> '' then
      Result := TSnippetExtraHelper.BuildActiveText(REML)
    else
      Result := TActiveTextFactory.CreateActiveText;
  end;

  // Get compilation results from compile results node and its child nodes
  function GetCompileResults: TCompileResults;

    // Looks up compiler ID string in lookup table and passes the corresponding
    // compiler ID in Match if found. Returns True if found, False if not.
    function TryLookupCompID(IDStr: string; out Match: TCompilerID): Boolean;
    var
      CompID: TCompilerID;  // loops thru all compiler IDs
    begin
      Result := False;
      for CompID := Low(TCompilerID) to High(TCompilerID) do
      begin
        if StrSameText(CompilerIDs[CompID], IDStr) then
        begin
          Match := CompID;
          Exit(True);
        end;
      end;
    end;

  var
    ListNode: IXMLNode;               // node that enclose compiler result nodes
    ResultsNodes: IXMLSimpleNodeList; // list of compiler-result nodes
    ResultNode: IXMLNode;             // a compiler-result node
    CompID: TCompilerID;              // loops thru compiler IDs
    CompResultStr: string;            // compiler id string from result node
  begin
    // Initialise all results to unknown (query)
    for CompID := Low(TCompilerID) to High(TCompilerID) do
      Result[CompID] := crQuery;

    // Find enclosing node: valid if this is not present
    ListNode := XMLDoc.FindFirstChildNode(
      SnippetNode, SnippetCompileResultsNodeName
    );
    if not Assigned(ListNode) then
      Exit;

    // Get list of compiler-result nodes contained in list and process each one
    ResultsNodes := XMLDoc.FindChildNodes(ListNode, SnippetCompilerNodeName);
    for ResultNode in ResultsNodes do
    begin
      // get compile result identifier
      CompResultStr := StrToLower(
        ResultNode.Attributes[SnippetCompilerNodeResultAttr]
      );
      if CompResultStr = '' then
        CompResultStr := '?';
      // add specified result function result
      if TryLookupCompID(
        ResultNode.Attributes[SnippetCompilerNodeIdAttr], CompID
      ) then
      begin
        case CompResultStr[1] of
          'y': Result[CompID] := crSuccess;
          'n': Result[CompID] := crError;
          else Result[CompID] := crQuery;
        end;
      end;
    end;
  end;

  // Get test info from test node attributes
  procedure SetTestInfoProperty(var Props: TSnippetData);

    function TryLookupTestLevel(const AIDStr: string;
      var ALevel: TSnippetTestInfo): Boolean;
    var
      TestID: TSnippetTestInfo;
    begin
      Result := False;
      for TestID := Low(TSnippetTestInfo) to High(TSnippetTestInfo) do
      begin
        if StrSameText(TestInfoValues[TestID], AIDStr) then
        begin
          ALevel := TestID;
          Exit(True);
        end;
      end;
    end;

  var
    TestNode: IXMLNode;
    Level: TSnippetTestInfo;
    URL: string;
  begin
    URL := '';
    Level := stiNone;
    TestNode := XMLDoc.FindFirstChildNode(SnippetNode, SnippetTestInfoNodeName);
    if not Assigned(TestNode) then
      Exit;
    if not TryLookupTestLevel(
      TestNode.Attributes[SnippetTestInfoNodeLevelAttr], Level
    ) then
      Exit;
    URL := TestNode.Attributes[SnippetTestInfoNodeUrlAttr];
    Props.TestInfo := Level;
    {TODO -cVault: When Snippet test URL is implemented, add following line}
    // Props.TestURL := URL;
  end;

begin
  try
    // Find snippet node
    SnippetNode := FindSnippetNode(SnippetKey);
    if not Assigned(SnippetNode) then
      raise EDataIO.CreateFmt(sSnippetNotFound, [SnippetKey]);
    // Snippet found: read properties
    Props.Cat := SnippetNode.Attributes[SnippetNodeCategoryAttr];
    Props.DisplayName := GetPropertyText(
      SnippetNode, SnippetDisplayNameNodeName
    );
    Props.Kind := GetKindProperty;
    Props.Desc := GetActiveText(SnippetDescriptionNodeName);
    Props.Extra := GetActiveText(SnippetNotesNodeName);
    Props.SourceCode := GetSourceCodePropertyText;
    {TODO -cVault: Replace Props.HiliteSource with Props.SourceCodeLanguage
            field.}
    Props.HiliteSource := IsPascalSource;
    Props.CompilerResults := GetCompileResults;
    SetTestInfoProperty(Props);

    // Record whether required units / snippets list are permitted
    {TODO -cVault: Change following line to use Props.SourceLanguage once
          implemented to check if language is Pascal: can't write the lists
          if not.}
    fCanReadRequiredLists := Props.HiliteSource;
  except
    HandleException(ExceptObject);
  end;
end;

function TNativeVaultStorageReader.GetSnippetReferences(const ASnippetKey,
  AListNodeName, AListItemName: string): IStringList;
var
  SnippetNode: IXMLNode;
begin
  // Find snippet node
  SnippetNode := FindSnippetNode(ASnippetKey);
  if not Assigned(SnippetNode) then
    raise EDataIO.CreateFmt(sSnippetNotFound, [ASnippetKey]);
  Result := GetEnclosedListItems(SnippetNode, AListNodeName, AListItemName);
end;

function TNativeVaultStorageReader.GetSnippetUnits(const SnippetKey: string):
  IStringList;
begin
  if fCanReadRequiredLists then
    Result := GetSnippetReferences(
      SnippetKey, SnippetUnitsListNodeName, SnippetUnitsListItemNodeName
    )
  else
    Result := TIStringList.Create;
end;

function TNativeVaultStorageReader.GetSnippetXRefs(const SnippetKey: string):
  IStringList;
begin
  Result := GetSnippetReferences(
    SnippetKey, SnippetXRefsListNodeName, SnippetXRefsListItemNodeName
  )
end;

procedure TNativeVaultStorageReader.HandleException(const EObj: TObject);
begin
  {TODO -cVault: Query whether database files should be deleted on error.
          This is not being done while debugging}
//  DeleteFiles(DataDir, '*.*');
  if EObj is EDOMParseError then
    // Expected DOM parsing error
    raise EDataIO.CreateFmt(sBadDataFormat, [sParseError]);
  if (EObj is EDataIO)
    or (EObj is EXMLDocError)
    or (EObj is ECodeSnipXML)
    or (EObj is EFileStreamError)
    or (EObj is EFOpenError)
    or (EObj is EActiveTextParserError) then
    // Expected other error: Raise new exception containing old message
    raise EDataIO.CreateFmt(sBadDataFormat, [(EObj as Exception).Message])
  else
    // Mot an expected error: just re-raise
    raise EObj;
end;

procedure TNativeVaultStorageReader.ValidateDoc;
var
  RootNode: IXMLNode;
  Version: TVersionNumber;
begin
  RootNode := XMLDoc.DocumentElement;

  if not Assigned(RootNode) then
    raise EDataIO.Create(sNoRootNode);

  if RootNode.NodeName <> RootNodeName then
    raise EDataIO.CreateFmt(sBadRootName, [RootNodeName]);

  if RootNode.Attributes[RootNodeWatermarkAttr] <> Watermark then
    raise EDataIO.Create(sBadWatermark);

  Version := TVersionNumber.Nul;
  Version.V1 := RootNode.Attributes[RootNodeVersionMajorAttr];
  Version.V2 := RootNode.Attributes[RootNodeVersionMinorAttr];
  if (Version < EarliestFileVersion) or (Version > CurrentFileVersion) then
    raise EDataIO.CreateFmt(sBadVersion, [Version.V1, Version.V2]);
end;

{ TNativeVaultStorageWriter }

procedure TNativeVaultStorageWriter.Finalise;
var
  FS: TFileStream;      // stream onto output file
begin
  // We use a TFileStream and TXMLDocument.SaveToStream rather than calling
  // TXMLDocument.SaveToFile so that any problem creating file is reported via
  // a known Delphi exception that can be handled.
  EnsureFolders(DataDirectory);
  try
    FS := TFileStream.Create(PathToXMLFile, fmCreate);
    try
      XMLDoc.Encoding := 'UTF-8';
      XMLDoc.SaveToStream(FS);
    finally
      FreeAndNil(FS);
    end;
  except
    HandleException(ExceptObject);
  end;
end;

procedure TNativeVaultStorageWriter.HandleException(const EObj: TObject);
begin
  if (EObj is EFileStreamError) or (EObj is ECodeSnip) then
    raise EDataIO.Create(EObj as Exception);
  raise EObj;
end;

procedure TNativeVaultStorageWriter.Initialise;
//var
//  RootNode: IXMLNode;   // document root node
begin
  try
    // Make sure database folder exists, empty of source code files
    TDirectory.CreateDirectory(DataDirectory);
    DeleteFiles(DataDirectory, '*' + SourceCodeFileExt);

    fRootNode := InitXMLDocAndRootNode;

    // create empty categories and snippets nodes
    fCategoriesNode := XMLDoc.CreateElement(fRootNode, CategoriesNodeName);
    fSnippetsNode := XMLDoc.CreateElement(fRootNode, SnippetsNodeName);
  except
    HandleException(ExceptObject);
  end;
end;

procedure TNativeVaultStorageWriter.WriteCatProps(const CatID: string;
  const Props: TCategoryData);
var
  CatNode: IXMLNode;  // referenced to required category node
begin
  try
    // Create <category id='CatName'> node
    CatNode := XMLDoc.CreateElement(fCategoriesNode, CategoryNodeName);
    CatNode.Attributes[CategoryNodeIdAttr] := CatID;
    XMLDoc.CreateElement(CatNode, CategoryDescriptionNodeName, Props.Desc);
  except
    HandleException(ExceptObject);
  end;
end;

procedure TNativeVaultStorageWriter.WriteCatSnippets(const CatID: string;
  const SnipList: IStringList);
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
    WriteEnclosedList(
      CatNode,
      CategorySnippetsListNodeName,
      CategorySnippetsListItemNodeName,
      SnipList
    );
  except
    HandleException(ExceptObject);
  end;
end;

procedure TNativeVaultStorageWriter.WriteEnclosedList(const AParent: IXMLNode;
  const AListNodeName, AItemNodeName: string; const AItems: IStringList);
var
  ListNode: IXMLNode; // reference to enclosing list node
  Item: string;       // a name item in list
begin
  ListNode := XMLDoc.CreateElement(AParent, AListNodeName);
  for Item in AItems do
    XMLDoc.CreateElement(ListNode, AItemNodeName, Item);
end;

procedure TNativeVaultStorageWriter.WriteMetaData(const AMetaData: TMetaData);
var
  LicenseNode: IXMLNode;
  CopyrightNode: IXMLNode;
begin
  // Write license info
  LicenseNode := XMLDoc.CreateElement(fRootNode, LicenseNodeName);
  LicenseNode.Attributes[LicenseNodeSPDXAttr] := AMetaData.LicenseInfo.SPDX;
  LicenseNode.Attributes[LicenseNodeNameAttr] := AMetaData.LicenseInfo.Name;
  LicenseNode.Attributes[LicenseNodeURLAttr] := AMetaData.LicenseInfo.URL;
  if StrIsEmpty(AMetaData.LicenseInfo.Text) then
    LicenseNode.Attributes[LicenseNodeLicenseFileAttr] := ''
  else
  begin
    // license text file: license text is written to a UTF-8 encoded file with
    // no BOM and filename is stored in XML
    LicenseNode.Attributes[LicenseNodeLicenseFileAttr] := LicenseTextFileName;
    TFileIO.WriteAllText(
      FilePath(LicenseTextFileName),
      AMetaData.LicenseInfo.Text,
      TEncoding.UTF8,
      False
    );
  end;

  CopyrightNode := XMLDoc.CreateElement(fRootNode, CopyrightNodeName);
  CopyrightNode.Attributes[CopyrightNodeDateAttr] :=
    AMetaData.CopyrightInfo.Date;
  CopyrightNode.Attributes[CopyrightNodeHolderAttr] :=
    AMetaData.CopyrightInfo.Holder;
  CopyrightNode.Attributes[CopyrightNodeURLAttr]
    := AMetaData.CopyrightInfo.HolderURL;
  if AMetaData.CopyrightInfo.Contributors.Count > 0 then
    WriteEnclosedList(
      CopyrightNode,
      CopyrightContributorsListNodeName,
      CopyrightContributorsListItemNodeName,
      AMetaData.CopyrightInfo.Contributors
    );

  WriteEnclosedList(
    fRootNode,
    AcknowledgementsListNodeName,
    AcknowledgementsListItemNodeName,
    AMetaData.Acknowledgements
  );

end;

procedure TNativeVaultStorageWriter.WriteReferenceList(const ASnippetKey,
  AListNodeName, AItemNodeName: string; const AItems: IStringList);
var
  SnippetNode: IXMLNode;  // reference to snippet's node
begin
  try
    // Don't write list if no items
    if AItems.Count = 0 then
      Exit;
    // Find snippet node
    SnippetNode := FindSnippetNode(ASnippetKey);
    Assert(Assigned(SnippetNode),
      ClassName + '.WriteReferenceList: Can''t find snippet node');
    // Write the list
    WriteEnclosedList(SnippetNode, AListNodeName, AItemNodeName, AItems);
  except
    HandleException(ExceptObject);
  end;
end;

procedure TNativeVaultStorageWriter.WriteSnippetDepends(
  const SnippetKey: string; const Depends: IStringList);
begin
  if not fCanWriteRequiredLists then
    Exit;
  WriteReferenceList(
    SnippetKey,
    SnippetDependsListNodeName,
    SnippetDependsListItemNodeName,
    Depends
  );
end;

procedure TNativeVaultStorageWriter.WriteSnippetProps(const SnippetKey: string;
  const Props: TSnippetData);
var
  SnippetNode: IXMLNode;  // snippet's node
  FileName: string;       // name of file where source code stored
  SourceCodeNode: IXMLNode;
  CompResultsNode: IXMLNode;  // node that stores all compiler results
  CompResultNode: IXMLNode;   // each compiler result node
  CompID: TCompilerID;        // loops thru all supported compilers
  TestInfoNode: IXMLNode;
begin
  try
    // Record whether required units / snippets list are permitted
    {TODO -cVault: Change following line to use Props.SourceLanguage once
          implemented to check if language is Pascal: can't write the lists
          if not.}
    fCanWriteRequiredLists := Props.HiliteSource;
    // Create snippet node
    SnippetNode := XMLDoc.CreateElement(fSnippetsNode, SnippetNodeName);
    SnippetNode.Attributes[SnippetNodeKeyAttr] := SnippetKey;
    SnippetNode.Attributes[SnippetNodeCategoryAttr] := Props.Cat;
    {TODO -cVault: change following test to test for Language = 'pascal'
            instead of HiliteSource once Language prop is created}
    if Props.HiliteSource then
      SnippetNode.Attributes[SnippetNodeKindAttr] :=
        SnippetKindValues[Props.Kind]
    else
      SnippetNode.Attributes[SnippetNodeKindAttr] :=
        SnippetKindValues[skFreeform];

    // Add property nodes
    // display name
    XMLDoc.CreateElement(
      SnippetNode, SnippetDisplayNameNodeName, Props.DisplayName
    );
    // description node: only written if Desc property has a value
    if Props.Desc.HasContent then
      XMLDoc.CreateElement(
        SnippetNode,
        SnippetDescriptionNodeName,
        TSnippetExtraHelper.BuildREMLMarkup(Props.Desc)
      );
    // notes node: only written if Extra property has a value
    if Props.Extra.HasContent then
      {TODO -cVault: rename Extra snippets property as Notes}
      XMLDoc.CreateElement(
        SnippetNode,
        SnippetNotesNodeName,
        TSnippetExtraHelper.BuildREMLMarkup(Props.Extra)
      );
    // source code node
    FileName := SnippetKey + SourceCodeFileExt;
    SourceCodeNode := XMLDoc.CreateElement(SnippetNode, SnippetSourceNodeName);
    SourceCodeNode.Attributes[SnippetSourceNodeFileAttr] := FileName;
    {TODO -cVault: change snippet props to have a language field instead of
            HiliteSource and use it to set the following attribute}
    SourceCodeNode.Attributes[SnippetSourceNodeLanguageAttr] :=
      StrIf(Props.HiliteSource, 'pascal', 'text');
    // source code file: source code is written to a UTF-8 encoded file with no
    // BOM and filename is stored in XML
    TFileIO.WriteAllText(
      FilePath(FileName), Props.SourceCode, TEncoding.UTF8, False
    );
    {TODO -cVault: change following test to test for Language = 'pascal'
            instead of HiliteSource once Language prop is created}
    if Props.HiliteSource and (Props.Kind <> skFreeform) then
    begin
      // compile results list: must be omitted if snippet is not Pascal &
      // even then, only known compiler results are written
      CompResultsNode := XMLDoc.CreateElement(
        SnippetNode, SnippetCompileResultsNodeName
      );
      for CompID := Low(TCompilerID) to High(TCompilerID) do
      begin
        if Props.CompilerResults[CompID] <> crQuery then
        begin
          CompResultNode := XMLDoc.CreateElement(
            CompResultsNode, SnippetCompilerNodeName
          );
          CompResultNode.Attributes[SnippetCompilerNodeIdAttr] :=
            CompilerIDs[CompID];
          CompResultNode.Attributes[SnippetCompilerNodeResultAttr] :=
            CompileResultValues[Props.CompilerResults[CompID]];
        end;
      end;
    end;
    // test info
    TestInfoNode := XMLDoc.CreateElement(SnippetNode, SnippetTestInfoNodeName);
    TestInfoNode.Attributes[SnippetTestInfoNodeLevelAttr] :=
      TestInfoValues[Props.TestInfo];
    {TODO -cVault: Change following line that always writes an empty URL once
            TestURL property added to snippets.}
    TestInfoNode.Attributes[SnippetTestInfoNodeUrlAttr] := '';
  except
    HandleException(ExceptObject);
  end;
end;

procedure TNativeVaultStorageWriter.WriteSnippetUnits(const SnippetKey: string;
  const Units: IStringList);
begin
  if not fCanWriteRequiredLists then
    Exit;
  WriteReferenceList(
    SnippetKey,
    SnippetUnitsListNodeName,
    SnippetUnitsListItemNodeName,
    Units
  );
end;

procedure TNativeVaultStorageWriter.WriteSnippetXRefs(const SnippetKey: string;
  const XRefs: IStringList);
begin
  WriteReferenceList(
    SnippetKey,
    SnippetXRefsListNodeName,
    SnippetXRefsListItemNodeName,
    XRefs
  );
end;

end.

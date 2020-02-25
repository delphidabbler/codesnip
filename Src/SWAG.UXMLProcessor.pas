{
 * This Source Code Form is subject to the terms of the Mozilla Public License,
 * v. 2.0. If a copy of the MPL was not distributed with this file, You can
 * obtain one at http://mozilla.org/MPL/2.0/
 *
 * Copyright (C) 2020, Peter Johnson (gravatar.com/delphidabbler).
 *
 * Implements a class that interfaces with a local copy of the DelphiDabbler
 * version of the SWAG databases to get snippet and category information.
}


unit SWAG.UXMLProcessor;


interface


uses
  // VCL
  Generics.Collections,
  // Project
  SWAG.UCommon,
  UXMLDocHelper,
  UXMLDocumentEx;


type

  ///  <summary>Class that interfaces with a local copy of the DelphiDabbler
  ///  version of the SWAG databases to get snippet and category information.
  ///  </summary>
  ///  <remarks>The database meta data is stored in an XML file and source code
  ///  is stored in text files.</remarks>
  TSWAGXMLProcessor = class(TObject)
  strict private
    const
      // Name of SWAG XML file
      cSWAGXMLFile = 'swag.xml';
      // Supported DelphiDabbler SWAG versions on GitHub
      cSupportedSWAGVersion   = 1;
      // XML nodes and attributes used in DelphiDabbler master SWAG file on
      // GitHub
      cSWAGRootNode           = 'swag';
      cRootVersionAttr        = 'version';
      cCategoriesNode         = 'categories';
      cCategoryNode           = 'category';
      cCategoryIDAttr         = 'id';
      cCategoryTitleNode      = 'title';
      cSnippetsNode           = 'snippets';
      cSnippetNode            = 'snippet';
      cSnippetIDAttr          = 'id';
      cSnippetCatIdNode       = 'category-id';
      cSnippetFileNameNode    = 'file-name';
      cSnippetDateNode        = 'date';
      cSnippetAuthorNode      = 'author';
      cSnippetIsDocumentNode  = 'is-document';
      cSnippetTitleNode       = 'title';
    var
      ///  <summary>Object used to access and search SWAG XML meta data file.
      ///  </summary>
      fXMLDoc: IXMLDocumentEx;
      ///  <summary>Directory storing downloaded SWAG files.</summary>
      fSWAGRootDir: string;
    ///  <summary>Converts an array of tags into a path suitable for use with
    ///  IXMLDocumentEx.FindNode.</summary>
    class function NodePath(const Tags: array of string): string;
    ///  <summary>Validates XML document as a valid DelphiDabbler SWAG XML
    ///  document.</summary>
    ///  <exception>ESWAGXMLProcessor raised if document fails validation.
    ///  </exception>
    procedure ValidateXMLDoc;
    ///  <summary>Validates fields of given snippet record as read from XML and
    ///  source code files.</summary>
    ///  <param name="Snippet">TSWAGSnippet [in] Snippet record to be validated.
    ///  </param>
    ///  <exception>ESWAGXMLProcessor raised if snippet fails validation.
    ///  </exception>
    procedure ValidateSnippet(const Snippet: TSWAGSnippet);
    ///  <summary>Validates fields of given partial snippet record as read from
    ///  XML.</summary>
    ///  <param name="Snippet">TSWAGSnippet [in] Snippet record to be validated.
    ///  </param>
    ///  <exception>ESWAGXMLProcessor raised if snippet fails validation.
    ///  </exception>
    procedure ValidatePartialSnippet(const Snippet: TSWAGSnippet);
    ///  <summary>Gets source code for a snippet from file and stores it in the
    ///  snippet record.</summary>
    ///  <param name="Snippet">TSWAGSnippet [in/out] Snippet for which source
    ///  code is required. Snippet is updated with required source code.</param>
    ///  <exception>ESWAGXMLProcessor raised if snippet source code can&#39;t be
    ///  loaded.</exception>
    procedure GetSnippetSourceCode(var Snippet: TSWAGSnippet);
  public
    ///  <summary>Constructor that sets up object ready to to process XML.
    ///  </summary>
    constructor Create;
    ///  <summary>Tears down object.</summary>
    destructor Destroy; override;
    ///  <summary>Initialises processor to read database data from the given
    ///  directory.</summary>
    ///  <remarks>Must be called before any other methods.</remarks>
    procedure Initialise(const SWAGDirName: string);
    ///  <summary>Gets a list of all categories from the SWAG XML file.
    ///  </summary>
    ///  <param name="CatList">TList&lt;TSWAGCategory&gt; [in] Receives required
    ///  list of categories.</param>
    ///  <exception>ESWAGXMLProcessor raised if categories can't be read or are
    ///  invalid.</exception>
    procedure GetCategories(CatList: TList<TSWAGCategory>);
    ///  <summary>Gets partial information about all snippets belonging to a
    ///  category from the SWAG XML file.</summary>
    ///  <param name="CatID">string [in] ID of category for which snippets are
    ///  required.</param>
    ///  <param name="SnippetList">TList&lt;TSWAGSnippet&gt; [in] Receives list
    ///  of snippets read.</param>
    ///  <exception>ESWAGXMLProcessor raised if partial snippets can't be read
    ///  or are invalid.</exception>
    procedure GetPartialSnippets(const CatID: string;
      SnippetList: TList<TSWAGSnippet>);
    ///  <summary>Gets a single snippet from the SWAG XML file.</summary>
    ///  <param name="SnippetID">Cardinal [in] Unique ID of the required
    ///  snippet.</param>
    ///  <returns>TSWAGSnippet. Details of the required snippet.</returns>
    ///  <exception>ESWAGXMLProcessor raised if snippet can't be read or is
    ///  invalid.</exception>
    function GetSnippet(const SnippetID: Cardinal): TSWAGSnippet;

  end;

  ///  <summary>Class of exception raised when expected errors are found in
  ///  methods of TSWAGXMLProcessor.</summary>
  ESWAGXMLProcessor = class(ECodeSnipXML);


implementation


uses
  // VCL
  SysUtils,
  Classes,
  IOUtils,
  ActiveX,
  XMLIntf,
  Math,
  DateUtils,
  // Project
  UConsts,
  UIOUtils,
  UStrUtils,
  UUtils;


{ TSWAGXMLProcessor }

constructor TSWAGXMLProcessor.Create;
begin
  inherited Create;
  OleInitialize(nil);
end;

destructor TSWAGXMLProcessor.Destroy;
begin
  if Assigned(fXMLDoc) then
    fXMLDoc.Active := False;
  fXMLDoc := nil;
  OleUninitialize;
  inherited;
end;

procedure TSWAGXMLProcessor.GetCategories(CatList: TList<TSWAGCategory>);
var
  CategoriesNode: IXMLNode;
  CategoryNodes: IXMLSimpleNodeList;
  CategoryNode: IXMLNode;
  Category: TSWAGCategory;
resourcestring
  sMissingNode = 'Invalid SWAG XML file: no categories information found';
  sMissingID = 'Invalid SWAG XML file: missing category ID';
  sMissingTitle = 'Invalid SWAG XML file: missing title for category "%s"';
begin
  CategoriesNode := fXMLDoc.FindNode(
    NodePath([cSWAGRootNode, cCategoriesNode])
  );
  if not Assigned(CategoriesNode) then
    raise ESWAGXMLProcessor.CreateFmt(sMissingNode, [cCategoriesNode]);
  CategoryNodes := fXMLDoc.FindChildNodes(CategoriesNode, cCategoryNode);
  for CategoryNode in CategoryNodes do
  begin
    Category.ID := CategoryNode.Attributes[cCategoryIDAttr];
    if StrIsEmpty(Category.ID, True) then
      raise ESWAGXMLProcessor.Create(sMissingID);
    Category.Title := TXMLDocHelper.GetSubTagText(
      fXMLDoc, CategoryNode, cCategoryTitleNode
    );
    if StrIsEmpty(Category.Title, True) then
      raise ESWAGXMLProcessor.CreateFmt(sMissingTitle, [Category.ID]);
    CatList.Add(Category);
  end;
end;

procedure TSWAGXMLProcessor.GetPartialSnippets(const CatID: string;
  SnippetList: TList<TSWAGSnippet>);
var
  AllSnippetsNode: IXMLNode;
  SnippetNodes: IXMLSimpleNodeList;
  SnippetNode: IXMLNode;
  Snippet: TSWAGSnippet;
  SnippetID: Integer;
resourcestring
  sSnippetsNodeMissing = 'Invalid SWAG XML file: no snippet information found';
  sBadSourceID = 'Invalid SWAG XML file: invalid snippet ID encountered';
begin
  AllSnippetsNode := fXMLDoc.FindNode(NodePath([cSWAGRootNode, cSnippetsNode]));
  if not Assigned(AllSnippetsNode) then
    raise ESWAGXMLProcessor.Create(sSnippetsNodeMissing);
  SnippetNodes := fXMLDoc.FindChildNodes(AllSnippetsNode, cSnippetNode);
  if not Assigned(SnippetNodes) then
    Exit;
  for SnippetNode in SnippetNodes do
  begin
    if StrSameText(
      TXMLDocHelper.GetSubTagText(fXMLDoc, SnippetNode, cSnippetCatIdNode),
      CatID
    ) then
    begin
      if not TryStrToInt(SnippetNode.Attributes[cSnippetIDAttr], SnippetID) then
        raise ESWAGXMLProcessor.Create(sBadSourceID);
      Snippet.ID := SnippetID;
      Snippet.Title := TXMLDocHelper.GetSubTagText(
        fXMLDoc, SnippetNode, cSnippetTitleNode
      );
      ValidatePartialSnippet(Snippet);
      SnippetList.Add(Snippet);
    end;
  end;
end;

function TSWAGXMLProcessor.GetSnippet(const SnippetID: Cardinal): TSWAGSnippet;
var
  AllSnippetsNode: IXMLNode;
  SnippetNode: IXMLNode;
  DateStr: string;
resourcestring
  sSnippetsNodeMissing = 'Invalid SWAG XML file: no snippet information found';
  sSnippetNotFound = 'Invalid SWAG XML file: snippet with ID %d not found';
begin
  // Find required snippet node
  AllSnippetsNode := fXMLDoc.FindNode(NodePath([cSWAGRootNode, cSnippetsNode]));
  if not Assigned(AllSnippetsNode) then
    raise ESWAGXMLProcessor.Create(sSnippetsNodeMissing);
  SnippetNode := fXMLDoc.FindFirstChildNode(
    AllSnippetsNode, cSnippetNode, cSnippetIDAttr, SnippetID
  );
  if not Assigned(SnippetNode) then
    raise ESWAGXMLProcessor.CreateFmt(sSnippetNotFound, [SnippetID]);
  // Get snippet info from snippet node
  Result.ID := SnippetID;
  Result.Category := TXMLDocHelper.GetSubTagText(
    fXMLDoc, SnippetNode, cSnippetCatIdNode
  );
  Result.FileName := TXMLDocHelper.GetSubTagText(
    fXMLDoc, SnippetNode, cSnippetFileNameNode
  );
  DateStr := TXMLDocHelper.GetSubTagText(
    fXMLDoc, SnippetNode, cSnippetDateNode
  );
  if not TryParseSQLDateTime(DateStr, Result.DateStamp) then
    Result.DateStamp := 0.0;
  Result.Title := TXMLDocHelper.GetSubTagText(
    fXMLDoc, SnippetNode, cSnippetTitleNode
  );
  Result.Author := TXMLDocHelper.GetSubTagText(
    fXMLDoc, SnippetNode, cSnippetAuthorNode
  );
  Result.IsDocument := StrSameText(
    TXMLDocHelper.GetSubTagText(fXMLDoc, SnippetNode, cSnippetIsDocumentNode),
    '1'
  );
  GetSnippetSourceCode(Result);
  ValidateSnippet(Result);
end;

procedure TSWAGXMLProcessor.GetSnippetSourceCode(var Snippet: TSWAGSnippet);
var
  FilePath: string;
  Code: string;
resourcestring
  sSourceCodeNotFound = 'Invalid SWAG database: '
    + 'source code file for snippet %d not found';
begin
  Assert(Snippet.ID > 0, ClassName + '.GetSnippetSourceCode: '
    + 'Snippet.ID not set');
  Assert(not StrIsEmpty(Snippet.Category),
    ClassName + '.GetSnippetSourceCode: Snippet.Category not set');
  Assert(not StrIsEmpty(Snippet.FileName),
    ClassName + '.GetSnippetSourceCode: Snippet.FileName not set');
  FilePath := StrJoin(
    [fSWAGRootDir, Snippet.Category, Snippet.FileName], PathDelim
  );
  if not TFile.Exists(FilePath, False) then
    raise ESWAGXMLProcessor.CreateFmt(sSourceCodeNotFound, [Snippet.ID]);
  Code := TFileIO.ReadAllText(FilePath, TEncoding.Default);
  // Some if not all the source code files end in the EOF (SUB) character
  if (Length(Code) > 0) and (Code[Length(Code)] = EOF) then
    Code := StrSliceLeft(Code, Length(Code) - 1);
  Snippet.SourceCode := Code;
end;

procedure TSWAGXMLProcessor.Initialise(const SWAGDirName: string);
resourcestring
  sFileReadError = 'Can''t read SWAG database file "%0:s"' + EOL2
    + 'Reported error is: "%1:s"';
  sXMLReadError = 'Error parsing XML in "%0:s"' + EOL2
    + 'Reported error is: "%1:s"';
var
  Content: TBytes;
  XMLFilePath: string;
begin
  fSWAGRootDir := ExcludeTrailingPathDelimiter(SWAGDirName);
  XMLFilePath := fSWAGRootDir + PathDelim + cSWAGXMLFile;
  try
    Content := TFileIO.ReadAllBytes(XMLFilePath);
  except
    on E: EStreamError do
      raise ESWAGXMLProcessor.CreateFmt(
        sFileReadError, [XMLFilePath, E.Message]
      );
    else
      raise;
  end;
  if Assigned(fXMLDoc) then
    fXMLDoc.Active := False;
  fXMLDoc := TXMLDocHelper.CreateXMLDoc;
  try
    fXMLDoc.LoadFromBytes(Content);
    fXMLDoc.Active := True;
  except
    on E: Exception do
      raise ESWAGXMLProcessor.CreateFmt(
        sXMLReadError, [XMLFilePath, E.Message]
      );
  end;
  ValidateXMLDoc;
end;

class function TSWAGXMLProcessor.NodePath(const Tags: array of string): string;
begin
  Result := StrJoin(Tags, '\', False);
end;

procedure TSWAGXMLProcessor.ValidatePartialSnippet(const Snippet: TSWAGSnippet);
resourcestring
  sBadID = 'Invalid SWAG XML file: snippet ID not set';
  sBadTitle = 'Invalid SWAG XML file: snippet %d has no title';
begin
  if Snippet.ID < 1 then
    raise ESWAGXMLProcessor.Create(sBadID);
  if StrIsEmpty(Snippet.Title, True) then
    raise ESWAGXMLProcessor.CreateFmt(sBadTitle, [Snippet.ID]);
end;

procedure TSWAGXMLProcessor.ValidateSnippet(const Snippet: TSWAGSnippet);
resourcestring
  sBadCatID = 'Invalid SWAG XML file: snippet %d has no category';
  sBadFileName = 'Invalid SWAG XML file: snippet %d has no file name';
  sBadDateStamp = 'Invalid SWAG XML file: snippet %d has no date stamp';
  sBadAuthor = 'Invalid SWAG XML file: snippet %d has no author';
  sBadSourceCode = 'Invalid SWAG XML file: snippet %d has no source code';
begin
  ValidatePartialSnippet(Snippet);
  if StrIsEmpty(Snippet.Category, True) then
    raise ESWAGXMLProcessor.CreateFmt(sBadCatID, [Snippet.ID]);
  if StrIsEmpty(Snippet.FileName, True) then
    raise ESWAGXMLProcessor.CreateFmt(sBadFileName, [Snippet.ID]);
  if DateUtils.SameDateTime(Snippet.DateStamp, 0.0) then
    raise ESWAGXMLProcessor.CreateFmt(sBadDateStamp, [Snippet.ID]);
  if StrIsEmpty(Snippet.Author, True) then
    raise ESWAGXMLProcessor.CreateFmt(sBadAuthor, [Snippet.ID]);
  if StrIsEmpty(Snippet.SourceCode, True) then
    raise ESWAGXMLProcessor.CreateFmt(sBadSourceCode, [Snippet.ID]);
end;

procedure TSWAGXMLProcessor.ValidateXMLDoc;
var
  RootNode, CategoriesNode, SourcesNode: IXMLNode;
  VersionStr: string;
  Version: Integer;
resourcestring
  // Error messages
  sNoRootNode = 'Invalid SWAG XML file: no root element present';
  sBadRootName = 'Invalid SWAG XML file: root element must be named <%s>';
  sBadVersion = 'Invalid SWAG XML file: invalid version attribute of <%s>';
  sUnknownVersion = 'Invalid SWAG XML file: unsupported document version %d';
  sMissingNode = 'Invalid SWAG XML file: no <%s> node present';
begin
  // Check for valid <?xml> processing instruction
  TXMLDocHelper.ValidateProcessingInstr(fXMLDoc);

  // Check for valid <swag> root node
  RootNode := fXMLDoc.DocumentElement;
  if not Assigned(RootNode) then
    raise ESWAGXMLProcessor.Create(sNoRootNode);
  if RootNode.NodeName <> cSWAGRootNode then
    raise ESWAGXMLProcessor.CreateFmt(sBadRootName, [cSWAGRootNode]);
  VersionStr := RootNode.Attributes[cRootVersionAttr];
  if not TryStrToInt(VersionStr, Version) then
    raise ESWAGXMLProcessor.CreateFmt(sBadVersion, [cSWAGRootNode]);
  if Version <> cSupportedSWAGVersion then
    raise ESWAGXMLProcessor.CreateFmt(sUnknownVersion, [Version]);

  // Check for compulsory <categories> node
  CategoriesNode := fXMLDoc.FindNode(
    NodePath([cSWAGRootNode, cCategoriesNode])
  );
  if not Assigned(CategoriesNode) then
    raise ESWAGXMLProcessor.CreateFmt(sMissingNode, [cCategoriesNode]);

  // Check for compulsory <sources> node
  SourcesNode := fXMLDoc.FindNode(
    NodePath([cSWAGRootNode, cSnippetsNode])
  );
  if not Assigned(SourcesNode) then
    raise ESWAGXMLProcessor.CreateFmt(sMissingNode, [cSnippetsNode]);
end;

end.

{
 * This Source Code Form is subject to the terms of the Mozilla Public License,
 * v. 2.0. If a copy of the MPL was not distributed with this file, You can
 * obtain one at https://mozilla.org/MPL/2.0/
 *
 * Copyright (C) 2020, Peter Johnson (gravatar.com/delphidabbler).
 *
 * Implements a class that interfaces with a local copy of the DelphiDabbler
 * version of the SWAG databases to get packet and category information.
}


unit SWAG.UXMLProcessor;


interface


uses
  // VCL
  XMLIntf,
  Generics.Collections,
  // Project
  SWAG.UCommon,
  UXMLDocHelper,
  UXMLDocumentEx;


type

  ///  <summary>Class that interfaces with a local copy of the DelphiDabbler
  ///  version of the SWAG databases to get packet and category information.
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
      cPacketsNode            = 'packets';
      cPacketNode             = 'packet';
      cPacketIDAttr           = 'id';
      cPacketCatIdNode        = 'category-id';
      cPacketFileNameNode     = 'file-name';
      cPacketDateNode         = 'date';
      cPacketAuthorNode       = 'author';
      cPacketIsDocumentNode   = 'is-document';
      cPacketTitleNode        = 'title';
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
    ///  <summary>Validates fields of given packet record as read from XML and
    ///  source code files.</summary>
    ///  <param name="Packet">TSWAGPacket [in] Packet record to be validated.
    ///  </param>
    ///  <exception>ESWAGXMLProcessor raised if packet fails validation.
    ///  </exception>
    procedure ValidatePacket(const Packet: TSWAGPacket);
    ///  <summary>Validates fields of given partial packet record as read from
    ///  XML.</summary>
    ///  <param name="Packet">TSWAGPacket [in] Packet record to be validated.
    ///  </param>
    ///  <exception>ESWAGXMLProcessor raised if packet fails validation.
    ///  </exception>
    procedure ValidatePartialPacket(const Packet: TSWAGPacket);
    ///  <summary>Gets source code for a packet from file and stores it in the
    ///  packet record.</summary>
    ///  <param name="Packet">TSWAGPacket [in/out] Packet for which source
    ///  code is required. Packet is updated with required source code.</param>
    ///  <exception>ESWAGXMLProcessor raised if packet source code can&#39;t be
    ///  loaded.</exception>
    procedure GetPacketSourceCode(var Packet: TSWAGPacket);
    ///  <summary>Read and validate a positive integer value from a node
    ///  attribute</summary>
    ///  <param name="Node">IXMLNode [in] Node whose attribute is to be read.
    ///  </param>
    ///  <param name="Attr">string [in] Name of attribute.</param>
    ///  <param name="ErrMsg">string [in] Exception error messsage to be used on
    ///  error.</param>
    ///  <returns>Required postive integer value.</returns>
    ///  <exception>ESWAGXMLProcessor raised if attribute value is missing or
    ///  is not a positive integer.</exception>
    function GetPositiveIntAttribute(Node: IXMLNode; const Attr: string;
      const ErrMsg: string): Cardinal;
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
    ///  <summary>Gets partial information about all packets belonging to a
    ///  category from the SWAG XML file.</summary>
    ///  <param name="CatID">Cardinal [in] ID of category for which packets are
    ///  required.</param>
    ///  <param name="PacketList">TList&lt;TSWAGPacket&gt; [in] Receives list
    ///  of packets read.</param>
    ///  <exception>ESWAGXMLProcessor raised if partial packets can't be read
    ///  or are invalid.</exception>
    procedure GetPartialPackets(const CatID: Cardinal;
      PacketList: TList<TSWAGPacket>);
    ///  <summary>Gets a single packet from the SWAG XML file.</summary>
    ///  <param name="PacketID">Cardinal [in] Unique ID of the required packet.
    ///  </param>
    ///  <returns>TSWAGPacket. Details of the required packet.</returns>
    ///  <exception>ESWAGXMLProcessor raised if packet can't be read or is
    ///  invalid.</exception>
    function GetPacket(const PacketID: Cardinal): TSWAGPacket;

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
  sBadSourceID = 'Invalid SWAG XML file: invalid or missing category ID';
begin
  CategoriesNode := fXMLDoc.FindNode(
    NodePath([cSWAGRootNode, cCategoriesNode])
  );
  if not Assigned(CategoriesNode) then
    raise ESWAGXMLProcessor.CreateFmt(sMissingNode, [cCategoriesNode]);
  CategoryNodes := fXMLDoc.FindChildNodes(CategoriesNode, cCategoryNode);
  for CategoryNode in CategoryNodes do
  begin
    Category.ID := GetPositiveIntAttribute(
      CategoryNode, cCategoryIDAttr, sBadSourceID
    );
    Category.Title := TXMLDocHelper.GetSubTagText(
      fXMLDoc, CategoryNode, cCategoryTitleNode
    );
    if StrIsEmpty(Category.Title, True) then
      raise ESWAGXMLProcessor.CreateFmt(sMissingTitle, [Category.ID]);
    CatList.Add(Category);
  end;
end;

function TSWAGXMLProcessor.GetPacket(const PacketID: Cardinal): TSWAGPacket;
var
  AllPacketsNode: IXMLNode;
  PacketNode: IXMLNode;
  DateStr: string;
  PacketCatID: Cardinal;
resourcestring
  sPacketsNodeMissing = 'Invalid SWAG XML file: no packet information found';
  sPacketNotFound = 'Invalid SWAG XML file: packet with ID %d not found';
  sBadPacketCategory = 'Invalid SWAG XML file: packet has invalid or missing '
    + 'category ID';
begin
  // Find required Packet node
  AllPacketsNode := fXMLDoc.FindNode(NodePath([cSWAGRootNode, cPacketsNode]));
  if not Assigned(AllPacketsNode) then
    raise ESWAGXMLProcessor.Create(sPacketsNodeMissing);
  PacketNode := fXMLDoc.FindFirstChildNode(
    AllPacketsNode, cPacketNode, cPacketIDAttr, PacketID
  );
  if not Assigned(PacketNode) then
    raise ESWAGXMLProcessor.CreateFmt(sPacketNotFound, [PacketID]);
  // Get Packet info from Packet node
  Result.ID := PacketID;
  if not TryStrToCardinal(
    TXMLDocHelper.GetSubTagText(fXMLDoc, PacketNode, cPacketCatIdNode),
    PacketCatID
  ) then
    raise ESWAGXMLProcessor.Create(sBadPacketCategory);
  Result.Category := PacketCatID;
  Result.FileName := TXMLDocHelper.GetSubTagText(
    fXMLDoc, PacketNode, cPacketFileNameNode
  );
  DateStr := TXMLDocHelper.GetSubTagText(
    fXMLDoc, PacketNode, cPacketDateNode
  );
  if not TryParseSQLDateTime(DateStr, Result.DateStamp) then
    Result.DateStamp := 0.0;
  Result.Title := TXMLDocHelper.GetSubTagText(
    fXMLDoc, PacketNode, cPacketTitleNode
  );
  Result.Author := TXMLDocHelper.GetSubTagText(
    fXMLDoc, PacketNode, cPacketAuthorNode
  );
  Result.IsDocument := StrSameText(
    TXMLDocHelper.GetSubTagText(fXMLDoc, PacketNode, cPacketIsDocumentNode),
    '1'
  );
  GetPacketSourceCode(Result);
  ValidatePacket(Result);
end;

procedure TSWAGXMLProcessor.GetPacketSourceCode(var Packet: TSWAGPacket);
var
  FilePath: string;
  Code: string;
resourcestring
  sSourceCodeNotFound = 'Invalid SWAG database: '
    + 'source code file for packet %d not found';
begin
  Assert(Packet.ID > 0, ClassName + '.GetPacketSourceCode: '
    + 'Packet.ID not set');
  Assert(not StrIsEmpty(Packet.FileName),
    ClassName + '.GetPacketSourceCode: Packet.FileName not set');
  FilePath := StrJoin(
    [fSWAGRootDir, Packet.FileName], PathDelim
  );
  if not TFile.Exists(FilePath, False) then
    raise ESWAGXMLProcessor.CreateFmt(sSourceCodeNotFound, [Packet.ID]);
  Code := TFileIO.ReadAllText(FilePath, TEncoding.Default);
  // Some if not all the source code files end in the EOF (SUB) character
  if (Length(Code) > 0) and (Code[Length(Code)] = EOF) then
    Code := StrSliceLeft(Code, Length(Code) - 1);
  Packet.SourceCode := Code;
end;

procedure TSWAGXMLProcessor.GetPartialPackets(const CatID: Cardinal;
  PacketList: TList<TSWAGPacket>);
var
  AllPacketsNode: IXMLNode;
  PacketNodes: IXMLSimpleNodeList;
  PacketNode: IXMLNode;
  Packet: TSWAGPacket;
  CatIDFromNode: Cardinal;
resourcestring
  sPacketsNodeMissing = 'Invalid SWAG XML file: no packet information found';
  sBadSourceID = 'Invalid SWAG XML file: missing or invalid packet ID';
begin
  AllPacketsNode := fXMLDoc.FindNode(NodePath([cSWAGRootNode, cPacketsNode]));
  if not Assigned(AllPacketsNode) then
    raise ESWAGXMLProcessor.Create(sPacketsNodeMissing);
  PacketNodes := fXMLDoc.FindChildNodes(AllPacketsNode, cPacketNode);
  if not Assigned(PacketNodes) then
    Exit;
  for PacketNode in PacketNodes do
  begin
    if TryStrToCardinal(
      TXMLDocHelper.GetSubTagText(fXMLDoc, PacketNode, cPacketCatIdNode),
      CatIDFromNode
    ) and (CatIDFromNode > 0) and (CatIDFromNode = CatID) then
    begin
      Packet.ID := GetPositiveIntAttribute(
        PacketNode, cPacketIDAttr, sBadSourceID
      );
      Packet.Title := TXMLDocHelper.GetSubTagText(
        fXMLDoc, PacketNode, cPacketTitleNode
      );
      ValidatePartialPacket(Packet);
      PacketList.Add(Packet);
    end;
  end;
end;

function TSWAGXMLProcessor.GetPositiveIntAttribute(Node: IXMLNode; const Attr,
  ErrMsg: string): Cardinal;
begin
  if not TryStrToCardinal(Node.Attributes[Attr], Result) or (Result = 0) then
    raise ESWAGXMLProcessor.Create(ErrMsg);
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

procedure TSWAGXMLProcessor.ValidatePacket(const Packet: TSWAGPacket);
resourcestring
  sBadCatID = 'Invalid SWAG XML file: packet %d has invalid category';
  sBadFileName = 'Invalid SWAG XML file: packet %d has no file name';
  sBadDateStamp = 'Invalid SWAG XML file: packet %d has no date stamp';
  sBadAuthor = 'Invalid SWAG XML file: packet %d has no author';
  sBadSourceCode = 'Invalid SWAG XML file: packet %d has no source code';
begin
  ValidatePartialPacket(Packet);
  if Packet.Category = 0 then
    raise ESWAGXMLProcessor.CreateFmt(sBadCatID, [Packet.ID]);
  if StrIsEmpty(Packet.FileName, True) then
    raise ESWAGXMLProcessor.CreateFmt(sBadFileName, [Packet.ID]);
  if DateUtils.SameDateTime(Packet.DateStamp, 0.0) then
    raise ESWAGXMLProcessor.CreateFmt(sBadDateStamp, [Packet.ID]);
  if StrIsEmpty(Packet.Author, True) then
    raise ESWAGXMLProcessor.CreateFmt(sBadAuthor, [Packet.ID]);
  if StrIsEmpty(Packet.SourceCode, True) then
    raise ESWAGXMLProcessor.CreateFmt(sBadSourceCode, [Packet.ID]);
end;

procedure TSWAGXMLProcessor.ValidatePartialPacket(const Packet: TSWAGPacket);
resourcestring
  sBadID = 'Invalid SWAG XML file: packet ID not set';
  sBadTitle = 'Invalid SWAG XML file: packet %d has no title';
begin
  if Packet.ID < 1 then
    raise ESWAGXMLProcessor.Create(sBadID);
  if StrIsEmpty(Packet.Title, True) then
    raise ESWAGXMLProcessor.CreateFmt(sBadTitle, [Packet.ID]);
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
    NodePath([cSWAGRootNode, cPacketsNode])
  );
  if not Assigned(SourcesNode) then
    raise ESWAGXMLProcessor.CreateFmt(sMissingNode, [cPacketsNode]);
end;

end.

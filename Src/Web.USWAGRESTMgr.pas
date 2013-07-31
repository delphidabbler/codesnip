{
 * This Source Code Form is subject to the terms of the Mozilla Public License,
 * v. 2.0. If a copy of the MPL was not distributed with this file, You can
 * obtain one at http://mozilla.org/MPL/2.0/
 *
 * Copyright (C) 2013, Peter Johnson (www.delphidabbler.com).
 *
 * $Rev$
 * $Date$
 *
 * Implements a class that interfaces with the DelphiDabbler SWAG web service.
 * For details of the web service see http://swag.delphidabbler.com/docs/api/
}


unit Web.USWAGRESTMgr;


interface


uses
  // Delphi
  XMLIntf,
  Generics.Collections,
  // Project
  Web.UBaseWebService,
  UXMLDocumentEx,
  UURIParams,
  SWAG.UCommon;


type
  ///  <summary>Record that encapsulates a range of records to be retrieved from
  ///  the SWAG database via the SWAG REST API.</summary>
  TSWAGRESTRange = record
    ///  <summary>Starting index of first record in range.</summary>
    Start: Cardinal;
    ///  <summary>Maximum number of records to be retrieved.</summary>
    Length: Cardinal;
    ///  <summary>Constructs a new range with given start index and length.
    ///  </summary>
    constructor Create(AStart: Cardinal; ALength: Cardinal);
    ///  <summary>Constructs a new null range of zero length.</summary>
    class function CreateNull: TSWAGRESTRange; static;
    ///  <summary>Checks if a range is null (i.e. it's length is zero).
    ///  </summary>
    function IsNull: Boolean;
    ///  <summary>Converts range into a string with format suitable for passing
    ///  as a parameter value to the REST API.</summary>
    function ToString: string;
  end;

type
  ///  <summary>Class that interfaces with the DelphiDabbler SWAG web service
  ///  to get data from and information about the SWAG database.</summary>
  ///  <remarks>
  ///  <para>This class interfaces with the database via its RESTful API which
  ///  is documented ar http://swag.delphidabbler.com/docs/api/</para>
  ///  <para>Data is retrieved XML format.</para>
  ///  </remarks>
  TSWAGRESTMgr = class sealed(TBaseWebService)
  strict private
    const
      ///  <summary>Web service's base URL template.</summary>
      ScriptURLTplt = 'http://swag.%s/api/v1';
      ///  <summary>User agent sent to web service.</summary>
      UserAgent = 'CodeSnip';

  strict private

    ///  <summary>Adds a "limit" parameter to the given parameters object where
    ///  the parameter value is determined by the given range.</summary>
    procedure AddLimitParam(Params: TURIParams; const Range: TSWAGRESTRange);

    ///  <summary>Adds a "fields" parameter to the given parameters object where
    ///  the parameter value is determined by the given array of field names.
    ///  </summary>
    procedure AddFieldsParam(Params: TURIParams; const Fields: array of string);

    ///  <summary>Creates and returns a new parameters object, populated with
    ///  parameters common to every REST API call.</summary>
    function CreateParams: TURIParams;

    ///  <summary>Parses an XML node that describes a SWAG snippet.</summary>
    ///  <param name="XMLDoc">IXMLDocumentEx [in] Reference to an XML DOM
    ///  document containing the response from a suitable call to the web
    ///  service.</param>
    ///  <param name="Node">IXMLNode [in] XML node containing snippet data.
    ///  </param>
    ///  <param name="Snippet">TSWAGSnippet [out] Receives snippet data. Only
    ///  those fields of Snippet that correspond to the fields requested from
    ///  the web service are set.</param>
    ///  <exception>Raises EWebServiceFailure if any of the content within Node
    ///  is not valid.</exception>
    procedure ParseSnippetNode(XMLDoc: IXMLDocumentEx; Node: IXMLNode;
      out Snippet: TSWAGSNippet);

    ///  <summary>Validates an XML DOM document containing a response from the
    ///  web service to check if conforms to the expected format.</summary>
    ///  <param name="XMLDoc">IXMLDocumentEx [in] Reference to XML DOM document.
    ///  </param>
    ///  <exception>Raises EWebServiceFailure if the format of the response is
    ///  invalid.</exception>
    ///  <exception>Raises EWebServiceError if the response format is valid but
    ///  represents an error condition.</exception>
    procedure ValidateResponse(XMLDoc: IXMLDocumentEx);

    ///  <summary>Creates an XML DOM document from XML source code and validates
    ///  it.</summary>
    ///  <param name="XML">string [in] XML source code.</param>
    ///  <returns>IXMLDocumentEx. Required XML DOM document.</returns>
    ///  <exception>Raises EWebServiceFailure if the format of the XML is
    ///  invalid.</exception>
    ///  <exception>Raises EWebServiceError if the XML format is valid but
    ///  represents an error condition.</exception>
    function CreateValidXMLDoc(const XML: string): IXMLDocumentEx;

    ///  <summary>Parses the given XML response that results from a web service
    ///  request for details of all categories in the SWAG database.</summary>
    ///  <param name="XML">XML code containing response.</param>
    ///  <param name="CatList">TList&lt;TSWAGCategory&gt; [in] List that
    ///  receives details of all categories described by XML.</param>
    ///  <exception>Raises EWebServiceFailure if the format of the XML is
    ///  not a valid response for this request.</exception>
    ///  <exception>Raises EWebServiceError if the XML format is valid but
    ///  represents an error condition.</exception>
    procedure ParseCategoriesResponse(const XML: string;
      CatList: TList<TSWAGCategory>);

    ///  <summary>Parses the given XML response that results from a web service
    ///  request for a set of zero of more snippets from the SWAG database.
    ///  </summary>
    ///  <param name="XML">XML code containing response.</param>
    ///  <param name="SnippetList">TList&lt;TSWAGSnippet&gt; [in] List that
    ///  receives details of all snippets described by XML.</param>
    ///  <exception>Raises EWebServiceFailure if the format of the XML is
    ///  not a valid response for this request.</exception>
    ///  <exception>Raises EWebServiceError if the XML format is valid but
    ///  represents an error condition.</exception>
    procedure ParseSnippetsResponse(const XML: string;
      SnippetList: TList<TSWAGSnippet>);

    ///  <summary>Parses the given XML response that results from a web service
    ///  request for a single snippet from the SWAG database.</summary>
    ///  <param name="XML">XML code containing response.</param>
    ///  <param name="Snippet">TSWAGSnippet [out] Recieves information about
    ///  the snippet described by XML.</param>
    ///  <exception>Raises EWebServiceFailure if the format of the XML is
    ///  not a valid response for this request.</exception>
    ///  <exception>Raises EWebServiceError if the XML format is valid but
    ///  represents an error condition.</exception>
    procedure ParseSnippetResponse(const XML: string;
      out Snippet: TSWAGSnippet);

    ///  <summary>Parses the given XML response that results from a web service
    ///  request for a count of a (sub)set of snippets in SWAG database.
    ///  </summary>
    ///  <param name="XML">XML code containing response.</param>
    ///  <returns>Cardinal [in] The required snippet count.</returns>
    ///  <exception>Raises EWebServiceFailure if the format of the XML is
    ///  not a valid response for this request.</exception>
    ///  <exception>Raises EWebServiceError if the XML format is valid but
    ///  represents an error condition.</exception>
    function ParseSnippetCount(const XML: string): Cardinal;

  public

    ///  <summary>Creates a new instance of the web service object.</summary>
    constructor Create;

    ///  <summary>Gets a list of all categories in the SWAG database.</summary>
    ///  <param name="CatList">TList&lt;TSWAGCategory&gt; [in] Receives required
    ///  list of categories.</param>
    ///  <exception>Raises EWebServiceFailure if the response from the web
    ///  service not a valid response for this request.</exception>
    ///  <exception>Raises EWebServiceError if the XML format is valid but
    ///  represents an error condition.</exception>
    ///  <exception>Raises EHTTPError, EWebConnectionError or
    ///  EWebTransmissionError if there is a problem connecting to or receiving
    ///  data from the web service.</exception>
    procedure GetCategories(CatList: TList<TSWAGCategory>);

    ///  <summary>Gets a range of snippets from a given category of the SWAG
    ///  database.</summary>
    ///  <param name="CatID">string [in] ID of category for which snippets are
    ///  required.</param>
    ///  <param name="Fields">array of string [in] Names of snippet fields
    ///  required. See the REST API documentation for valid field names.</param>
    ///  <param name="Limit">TSWAGRESTRange [in] Range of records required.
    ///  </param>
    ///  <param name="SnippetList">TList&lt;TSWAGSnippet&gt; [in] Receives list
    ///  of snippets read, if any.</param>
    ///  <exception>Raises EWebServiceFailure if the response from the web
    ///  service not a valid response for this request.</exception>
    ///  <exception>Raises EWebServiceError if the XML format is valid but
    ///  represents an error condition (see remarks).</exception>
    ///  <exception>Raises EHTTPError, EWebConnectionError or
    ///  EWebTransmissionError if there is a problem connecting to or receiving
    ///  data from the web service.</exception>
    ///  <remarks>
    ///  <para>The range provided in Limit limits the number of snippets
    ///  returned to a managable amount. The snippets returned are those with
    ///  starting index of Limit.Start and the number returned will be between
    ///  zero and Limit.Length.</para>
    ///  <para>The fields set in each TSWAGSnippet record contained in
    ///  SnippetList depend on the fields requested in the Fields parameter.
    ///  </para>
    ///  <para>The web service will signal an error condition if CatID is
    ///  not valid or if Fields is empty or contains an invalid field name.
    ///  </para>
    ///  </remarks>
    procedure GetSnippets(const CatID: string; const Fields: array of string;
      const Limit: TSWAGRESTRange; SnippetList: TList<TSWAGSnippet>);
      overload;

    ///  <summary>Gets all snippets in a given category of the SWAG database.
    ///  </summary>
    ///  <param name="CatID">string [in] ID of category for which snippets are
    ///  required.</param>
    ///  <param name="Fields">array of string [in] Names of snippet fields
    ///  required. See the REST API documentation for valid field names.</param>
    ///  <param name="SnippetList">TList&lt;TSWAGSnippet&gt; [in] Receives list
    ///  of snippets read.</param>
    ///  <exception>Raises EWebServiceFailure if the response from the web
    ///  service not a valid response for this request.</exception>
    ///  <exception>Raises EWebServiceError if the XML format is valid but
    ///  represents an error condition (see remarks).</exception>
    ///  <exception>Raises EHTTPError, EWebConnectionError or
    ///  EWebTransmissionError if there is a problem connecting to or receiving
    ///  data from the web service.</exception>
    ///  <remarks>
    ///  <para>The fields set in each TSWAGSnippet record contained in
    ///  SnippetList depend on the fields requested in the Fields parameter.
    ///  </para>
    ///  <para>The web service will signal an error condition if CatID is
    ///  not valid or if Fields is empty or contains an invalid field name.
    ///  </para>
    ///  </remarks>
    procedure GetSnippets(const CatID: string; const Fields: array of string;
      SnippetList: TList<TSWAGSnippet>); overload;

    ///  <summary>Gets a range of snippets from the entire SWAG database.
    ///  </summary>
    ///  <param name="Fields">array of string [in] Names of snippet fields
    ///  required. See the REST API documentation for valid field names.</param>
    ///  <param name="Limit">TSWAGRESTRange [in] Range of records required.
    ///  </param>
    ///  <param name="SnippetList">TList&lt;TSWAGSnippet&gt; [in] Receives list
    ///  of snippets read, if any.</param>
    ///  <exception>Raises EWebServiceFailure if the response from the web
    ///  service not a valid response for this request.</exception>
    ///  <exception>Raises EWebServiceError if the XML format is valid but
    ///  represents an error condition (see remarks).</exception>
    ///  <exception>Raises EHTTPError, EWebConnectionError or
    ///  EWebTransmissionError if there is a problem connecting to or receiving
    ///  data from the web service.</exception>
    ///  <remarks>
    ///  <para>The range provided in Limit limits the number of snippets
    ///  returned to a managable amount. The snippets returned are those with
    ///  starting index of Limit.Start and the number returned will be between
    ///  zero and Limit.Length.</para>
    ///  <para>The fields set in each TSWAGSnippet record contained in
    ///  SnippetList depend on the fields requested in the Fields parameter.
    ///  </para>
    ///  <para>The web service will signal an error condition if Fields is empty
    ///  or contains an invalid field name.</para>
    ///  </remarks>
    procedure GetSnippets(const Fields: array of string;
      const Limit: TSWAGRESTRange; SnippetList: TList<TSWAGSnippet>); overload;

    ///  <summary>Gets all snippets in the SWAG database.</summary>
    ///  <param name="Fields">array of string [in] Names of snippet fields
    ///  required. See the REST API documentation for valid field names.</param>
    ///  <param name="SnippetList">TList&lt;TSWAGSnippet&gt; [in] Receives list
    ///  of snippets read, if any.</param>
    ///  <exception>Raises EWebServiceFailure if the response from the web
    ///  service not a valid response for this request.</exception>
    ///  <exception>Raises EWebServiceError if the XML format is valid but
    ///  represents an error condition (see remarks).</exception>
    ///  <exception>Raises EHTTPError, EWebConnectionError or
    ///  EWebTransmissionError if there is a problem connecting to or receiving
    ///  data from the web service.</exception>
    ///  <remarks>
    ///  <para>The fields set in each TSWAGSnippet record contained in
    ///  SnippetList depend on the fields requested in the Fields parameter.
    ///  </para>
    ///  <para>The web service will signal an error condition if Fields is empty
    ///  or contains an invalid field name.</para>
    ///  </remarks>
    procedure GetSnippets(const Fields: array of string;
      SnippetList: TList<TSWAGSnippet>); overload;

    ///  <summary>Gets a single snippet from the SWAG database.</summary>
    ///  <param name="SnippetID">Cardinal [in] Unique ID of the required
    ///  snippet.</param>
    ///  <param name="Fields">array of string [in] Names of snippet fields
    ///  required. See the REST API documentation for valid field names.</param>
    ///  <param name="Snippet">TSWAGSnippet [out] Receives the details of the
    ///  required snippet.</param>
    ///  <exception>Raises EWebServiceFailure if the response from the web
    ///  service is not a valid response for this request.</exception>
    ///  <exception>Raises EWebServiceError if the XML format is valid but
    ///  represents an error condition (see remarks).</exception>
    ///  <exception>Raises EHTTPError, EWebConnectionError or
    ///  EWebTransmissionError if there is a problem connecting to or receiving
    ///  data from the web service.</exception>
    ///  <remarks>
    ///  <para>The fields set in the returned Snippet record depend on the
    ///  fields requested in the Fields parameter.</para>
    ///  <para>The web service will signal an error condition if SnippetID is
    ///  not valid or if Fields is empty or contains an invalid field name.
    ///  </para>
    ///  </remarks>
    procedure GetSnippet(const SnippetID: Cardinal;
      const Fields: array of string; out Snippet: TSWAGSnippet);

    ///  <summary>Gets the number of snippets in a category or in the whole
    ///  SWAG database.</summary>
    ///  <param name="CatID">string [in] Name of category for which the snippet
    ///  count is required. Omitting this parameter, or passing '*' to it cause
    ///  the count of all snippets in the database to be returned.</param>
    ///  <returns>Cardinal. The required snippet count.</returns>
    ///  <exception>Raises EWebServiceFailure if the response from the web
    ///  service not a valid response for this request.</exception>
    ///  <exception>Raises EWebServiceError if the XML format is valid but
    ///  represents an error condition (see remarks).</exception>
    ///  <exception>Raises EHTTPError, EWebConnectionError or
    ///  EWebTransmissionError if there is a problem connecting to or receiving
    ///  data from the web service.</exception>
    ///  <remarks>The web service will signal an error condition if CatID, where
    ///  provided, is not a valid category ID or '*'.</remarks>
    function GetSnippetCount(const CatID: string = '*'): Cardinal;
  end;


implementation


uses
  // Delphi
  SysUtils,
  // Project
  UIStringList,
  UStrUtils,
  UURIEncode,
  UUtils,
  UXMLDocHelper,
  Web.UInfo,
  Web.UExceptions;


{ TSWAGRESTMgr }

procedure TSWAGRESTMgr.AddFieldsParam(Params: TURIParams;
  const Fields: array of string);
var
  FieldList: IStringList;
begin
  FieldList := TIStringList.Create(Fields);
  Params.Add('fields', FieldList.GetText(',', False));
end;

procedure TSWAGRESTMgr.AddLimitParam(Params: TURIParams;
  const Range: TSWAGRESTRange);
begin
  Params.Add('limit', Range.ToString);
end;

constructor TSWAGRESTMgr.Create;
begin
  inherited Create(TWebServiceInfo.Create(ScriptURLTplt, UserAgent));
end;

function TSWAGRESTMgr.CreateParams: TURIParams;
begin
  Result := TURIParams.Create;
  Result.Add('format', 'xml');
  Result.Add('suppressResponseCodes', 'true');
end;

function TSWAGRESTMgr.CreateValidXMLDoc(const XML: string): IXMLDocumentEx;
begin
  Result := TXMLDocumentEx.Create(nil);
  Result.LoadFromXML(XML);
  ValidateResponse(Result);
end;

procedure TSWAGRESTMgr.GetCategories(CatList: TList<TSWAGCategory>);
const
  ResourcePath = 'categories';
var
  Params: TURIParams;
  Response: string;
begin
  Params := CreateParams;
  try
    Response := GetText(ResourcePath, Params);
    ParseCategoriesResponse(Response, CatList);
  finally
    Params.Free;
  end;
end;

procedure TSWAGRESTMgr.GetSnippet(const SnippetID: Cardinal;
  const Fields: array of string; out Snippet: TSWAGSnippet);
const
  ResourcePathTplt = 'snippet/%d';
var
  Params: TURIParams;
  Response: string;
begin
  Params := CreateParams;
  try
    AddFieldsParam(Params, Fields);
    Response := GetText(Format(ResourcePathTplt, [SnippetID]), Params);
    ParseSnippetResponse(Response, Snippet);
  finally
    Params.Free;
  end;
end;

function TSWAGRESTMgr.GetSnippetCount(const CatID: string): Cardinal;
const
  ResourcePathTplt = 'snippet-count/%s';
var
  Params: TURIParams;
  Response: string;
begin
  Params := CreateParams;
  try
    Response := GetText(Format(ResourcePathTplt, [URIEncode(CatID)]), Params);
    Result := ParseSnippetCount(Response);
  finally
    Params.Free;
  end;
end;

procedure TSWAGRESTMgr.GetSnippets(const CatID: string;
  const Fields: array of string; const Limit: TSWAGRESTRange;
  SnippetList: TList<TSWAGSnippet>);
const
  ResourcePathTplt = 'snippets/%s';
var
  Params: TURIParams;
  Response: string;
begin
  Params := CreateParams;
  try
    AddFieldsParam(Params, Fields);
    if not Limit.IsNull then
      AddLimitParam(Params, Limit);
    Response := GetText(Format(ResourcePathTplt, [URIEncode(CatID)]), Params);
    ParseSnippetsResponse(Response, SnippetList);
  finally
    Params.Free;
  end;
end;

procedure TSWAGRESTMgr.GetSnippets(const CatID: string;
  const Fields: array of string; SnippetList: TList<TSWAGSnippet>);
begin
  GetSnippets(CatID, Fields, TSWAGRESTRange.CreateNull, SnippetList);
end;

procedure TSWAGRESTMgr.GetSnippets(const Fields: array of string;
  SnippetList: TList<TSWAGSnippet>);
begin
  GetSnippets('*', Fields, SnippetList);
end;

procedure TSWAGRESTMgr.GetSnippets(const Fields: array of string;
  const Limit: TSWAGRESTRange; SnippetList: TList<TSWAGSnippet>);
begin
  GetSnippets('*', Fields, Limit, SnippetList);
end;

procedure TSWAGRESTMgr.ParseCategoriesResponse(const XML: string;
  CatList: TList<TSWAGCategory>);
var
  XMLDOc: IXMLDocumentEx;
  CategoriesNode: IXMLNode;
  CategoryNodes: IXMLSimpleNodeList;
  CategoryNode: IXMLNode;
  Category: TSWAGCategory;
resourcestring
  sCategoriesNodeMissing = 'Response has no category information';
begin
  XMLDoc := CreateValidXMLDoc(XML);
  CategoriesNode := XMLDoc.FindNode('response\categories');
  if not Assigned(CategoriesNode) then
    raise EWebServiceFailure.Create(sCategoriesNodeMissing);
  CategoryNodes := XMLDoc.FindChildNodes(CategoriesNode, 'category');
  for CategoryNode in CategoryNodes do
  begin
    Category.ID := TXMLDocHelper.GetSubTagText(XMLDoc, CategoryNode, 'id');
    Category.Title := TXMLDocHelper.GetSubTagText(
      XMLDoc, CategoryNode, 'title'
    );
    CatList.Add(Category);
  end;
end;

function TSWAGRESTMgr.ParseSnippetCount(const XML: string): Cardinal;
var
  XMLDOc: IXMLDocumentEx;
  CountNode: IXMLNode;
resourcestring
  sBadCountNode = 'Response has missing or invalid count information';
begin
  XMLDoc := CreateValidXMLDoc(XML);
  CountNode := XMLDoc.FindNode('response\snippetCount');
  if not Assigned(CountNode)
    or not CountNode.IsTextElement
    or not TryStrToCardinal(CountNode.Text, Result) then
    raise EWebServiceFailure.Create(sBadCountNode);
end;

procedure TSWAGRESTMgr.ParseSnippetNode(XMLDoc: IXMLDocumentEx; Node: IXMLNode;
  out Snippet: TSWAGSNippet);
var
  NumStr: string;
  DateStr: string;
resourcestring
  sBadID = 'Invalid value for snippet id';
  sBadFileNumber = 'Invalid value for snippet file number';
begin
  NumStr := TXMLDocHelper.GetSubTagText(XMLDoc, Node, 'id');
  if NumStr <> '' then
  begin
    if not TryStrToCardinal(NumStr, Snippet.ID) then
      raise EWebServiceFailure.Create(sBadID);
  end
  else
    Snippet.ID := 0;

  Snippet.Category := TXMLDocHelper.GetSubTagText(XMLDoc, Node, 'category');

  Snippet.FileName := TXMLDocHelper.GetSubTagText(XMLDoc, Node, 'file_name');

  DateStr := TXMLDocHelper.GetSubTagText(XMLDoc, Node, 'datestamp');
  if DateStr <> '' then
    Snippet.DateStamp := ParseSQLDateTime(DateStr)
  else
    Snippet.DateStamp := 0.0;

  Snippet.Title := TXMLDocHelper.GetSubTagText(XMLDoc, Node, 'title');

  Snippet.Author := TXMLDocHelper.GetSubTagText(XMLDoc, Node, 'author');

  Snippet.SourceCode := TXMLDocHelper.GetSubTagText(
    XMLDoc, Node, 'source_code'
  );

  Snippet.IsDocument := StrMatchText(
    TXMLDocHelper.GetSubTagText(XMLDoc, Node, 'is_document'),
    ['1', 'yes', 'y', 'true']
  );
end;

procedure TSWAGRESTMgr.ParseSnippetResponse(const XML: string;
  out Snippet: TSWAGSnippet);
var
  XMLDoc: IXMLDocumentEx;
  SnippetNode: IXMLNode;
resourcestring
  sSnippetNodeMissing = 'Response has no snippet information';
begin
  XMLDoc := CreateValidXMLDoc(XML);
  SnippetNode := XMLDoc.FindNode('response\snippet');
  if not Assigned(SnippetNode) then
    raise EWebServiceFailure.Create(sSnippetNodeMissing);
  ParseSnippetNode(XMLDoc, SnippetNode, Snippet);
end;

procedure TSWAGRESTMgr.ParseSnippetsResponse(const XML: string;
  SnippetList: TList<TSWAGSnippet>);
var
  XMLDOc: IXMLDocumentEx;
  SnippetsNode: IXMLNode;
  SnippetNodes: IXMLSimpleNodeList;
  SnippetNode: IXMLNode;
  Snippet: TSWAGSnippet;
resourcestring
  sSnippetsNodeMissing = 'Response has no snippets information';
begin
  XMLDoc := CreateValidXMLDoc(XML);
  SnippetsNode := XMLDoc.FindNode('response\snippets');
  if not Assigned(SnippetsNode) then
    raise EWebServiceFailure.Create(sSnippetsNodeMissing);
  SnippetNodes := XMLDoc.FindChildNodes(SnippetsNode, 'snippet');
  for SnippetNode in SnippetNodes do
  begin
    ParseSnippetNode(XMLDoc, SnippetNode, Snippet);
    SnippetList.Add(Snippet);
  end;
end;

procedure TSWAGRESTMgr.ValidateResponse(XMLDoc: IXMLDocumentEx);
resourcestring
  sMissingRootNode = 'Response root node not found';
  sBadStatus = 'Response status missing or invalid';
  sMissingErrInfo = 'Error information missing';
  sBadErrStatus = 'Error status code missing or mal-formed';
  sBadErrCode = 'Error code missing or mal-formed';
  sMissingErrMessage = 'Error message missing';
var
  RootNode: IXMLNode;
  Status: string;
  ErrNode: IXMLNode;
  ErrStatus: Integer;
  ErrCode: Integer;
  ErrMessage: string;
begin
  TXMLDocHelper.ValidateProcessingInstr(XMLDoc);
  RootNode := XMLDoc.FindNode('response');
  if not Assigned(RootNode) then
    raise EWebServiceFailure.Create(sMissingRootNode);
  Status := TXMLDocHelper.GetSubTagText(XMLDoc, RootNode, 'status');
  if not StrSameText(Status, 'ok') and not StrSameText(Status, 'error') then
    raise EWebServiceFailure.Create(sBadStatus);
  if StrSameText(Status, 'ok') then
    Exit;
  // we have error response - validate response and get details (don't validate
  // 'response\error\status' node since we don't make use of it).
  ErrNode := XMLDoc.FindNode('response\error');
  if not Assigned(ErrNode) then
    raise EWebServiceFailure(sMissingErrInfo);
  if not TryStrToInt(
    TXMLDocHelper.GetSubTagText(XMLDOc, ErrNode, 'status'), ErrStatus
  ) then
    raise EWebServiceFailure.Create(sBadErrStatus);
  if not TryStrToInt(
    TXMLDocHelper.GetSubTagText(XMLDoc, ErrNode, 'code'), ErrCode
  ) then
    raise EWebServiceFailure.Create(sBadErrCode);
  ErrMessage := TXMLDocHelper.GetSubTagText(XMLDoc, ErrNode, 'message');
  if ErrMessage = '' then
    raise EWebServiceFailure.Create(sMissingErrMessage);
  // error response is valid: raise exception for error message and code
  raise EWebServiceError.Create(ErrMessage, ErrCode);
end;

{ TSWAGRESTRange }

constructor TSWAGRESTRange.Create(AStart, ALength: Cardinal);
begin
  Start := AStart;
  Length := ALength;
end;

class function TSWAGRESTRange.CreateNull: TSWAGRESTRange;
begin
  Result := TSWAGRESTRange.Create(0, 0);
end;

function TSWAGRESTRange.IsNull: Boolean;
begin
  Result := Length = 0;
end;

function TSWAGRESTRange.ToString: string;
begin
  Result := Format('%d,%d', [Start, Length]);
end;

end.


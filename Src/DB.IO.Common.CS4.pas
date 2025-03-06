{
 * This Source Code Form is subject to the terms of the Mozilla Public License,
 * v. 2.0. If a copy of the MPL was not distributed with this file, You can
 * obtain one at https://mozilla.org/MPL/2.0/
 *
 * Copyright (C) 2025, Peter Johnson (gravatar.com/delphidabbler).
 *
 * Implements a static class that helps with input and output that is common to
 * more than one supported CodeSnip v4 XML data format.
}


unit DB.IO.Common.CS4;

interface

uses
  // Delphi
  XMLIntf,
  // Project
  Compilers.UGlobals,
  DB.USnippetKind,
  UIStringList,
  UStructs,
  UXMLDocHelper,
  UXMLDocumentEx;

type
  ///  <summary>Static class that helps with input and output that is common to
  ///  more than one supported CodeSnip v4 XML data format.</summary>
  TCS4FormatHelper = class(TXMLDocHelper)
  strict private
    const
      RootWatermarkAttr = 'watermark';
      SnippetKindNodeName = 'kind';
      CompilerResultsNodeName = 'compiler-results';
      CompilerResultNodeName = 'compiler-result';
      CompilerResultIdAttr = 'id';
      StandardFormatNodeName = 'standard-format';
      CompilerIDValues: array[TCompilerID] of string = (
        'd2', 'd3', 'd4', 'd5', 'd6', 'd7',
        'd2005', 'd2006', 'd2007', 'd2009', 'd2010',
        'dXE', 'dXE2', 'dXE3', 'dDX4' {error, but in use so can't fix},
        'dXE5', 'dXE6', 'dXE7', 'dXE8',
        'd10s', 'd101b', 'd102t', 'd103r', 'd104s', 'd11a', 'd12y',
        'fpc'
      );
      SnippetKindValues: array[TSnippetKind] of string = (
        'freeform', 'routine', 'const', 'type', 'unit', 'class'
      );
  public
    const
      RootVersionAttr = 'version';
      HighlightSourceNodeName = 'highlight-source';
      ExtraNodeName = 'extra';
      {TODO -cRefactor: Remove support for old file formats that used
          credits, credits URL & comments nodes}
      CommentsNodeName = 'comments';
      CreditsNodeName = 'credits';
      CreditsUrlNodeName = 'credits-url';
      DependsNodeName = 'depends';
      UnitsNodeName = 'units';
      DescriptionNodeName = 'description';
      SnippetNodeName = 'routine';
      SnippetNodeNameAttr = 'name';
      SnippetsNodeName = 'routines';
      DisplayNameNodeName = 'display-name';
      PascalNameNodeName = 'pascal-name';
  public
    ///  <summary>Creates an XML document root node.</summary>
    ///  <param name="AXMLDoc"><c>IXMLDocumentEx</c> [in] Document in which to
    ///  insert root node.</param>
    ///  <param name="ANodeName"><c>string</c> [in] Name of root node.</param>
    ///  <param name="AWatermark"><c>string</c> [in] Value of root node's
    ///  watermark attribute.</param>
    ///  <param name="AVersion"><c>Integer</c> [in] Value of root node's version
    ///  attribute.</param>
    ///  <returns><c>IXMLNode</c>. Reference to new root node.</returns>
    class function CreateRootNode(const AXMLDoc: IXMLDocumentEx;
      const ANodeName, AWatermark: string; const AVersion: Integer): IXMLNode;

    ///  <summary>Validates the root node of an XML document.</summary>
    ///  <param name="AXMLDoc"><c>IXMLDocumentEx</c> [in] XML document to be
    ///  validated.</param>
    ///  <param name="ANodeName"><c>string></c> [in] Name of root mode.</param>
    ///  <param name="AWatermark"><c>string</c> [in] Required value of root
    ///  node's watermark attribute.</param>
    ///  <param name="AVersions"><c>TRange</c> [in] Range of acceptable file
    ///  version numbers.</param>
    ///  <returns><c>Integer</c>. Document version.</returns>
    ///  <exception><c>ECodeSnipXML</c> is raised if validation fails.
    ///  </exception>
    class function ValidateRootNode(const AXMLDoc: IXMLDocumentEx;
      const ANodeName, AWatermark: string; const AVersions: TRange): Integer;

    ///  <summary>Creates a comment at the top level of an XML document.
    ///  </summary>
    ///  <param name="AXMLDoc"><c>IXMLDocumentEx</c> [in] XML document in which
    ///  comment is inserted.</param>
    ///  <param name="AComment"><c>string</c> [in] Comment to be inserted.
    ///  </param>
    class procedure CreateComment(const AXMLDoc: IXMLDocumentEx;
      const AComment: string);

    {TODO -cVault: query whether GetStandardFormat is needed if support dropped
            for early CS4 XML formats.}
    ///  <summary>Gets value of a &lt;standard-format&gt; node of a snippet in
    ///  an XML document.</summary>
    ///  <param name="AXMLDoc"><c>IXMLDocumentEx</c> [in] XML document
    ///  containing snippet.</param>
    ///  <param name="ASnippetNode"><c>IXMLNode</c> [in] Snippet node that
    ///  contains the &lt;standard-format&gt; tag.</param>
    ///  <param name="ADefault"><c>Boolean</c> [in] Value to use if node doesn't
    ///  exist or has a non-standard value.</param>
    ///  <returns><c>Boolean</c>. Value of node, or default value.</returns>
    class function GetStandardFormat(const AXMLDoc: IXMLDocumentEx;
      const ASnippetNode: IXMLNode; const ADefault: Boolean): Boolean;

    ///  <summary>Sets value of a &lt;highlight-source&gt; node of a snippet in
    ///  an XML document.</summary>
    ///  <param name="AXMLDoc"><c>IXMLDocumentEx</c> [in] XML document
    ///  containing snippet.</param>
    ///  <param name="ASnippetNode"><c>IXMLNode</c> [in] Snippet node that
    ///  contains the &lt;highlight-source&gt; tag.</param>
    ///  <param name="ADefault"><c>Boolean</c> [in] Value to use if node doesn't
    ///  exist or has a non-standard value.</param>
    ///  <returns><c>Boolean</c>. Value of node, or default value.</returns>
    class function GetHiliteSource(const AXMLDoc: IXMLDocumentEx;
      const ASnippetNode: IXMLNode; const ADefault: Boolean): Boolean;

    ///  <summary>Gets value of &lt;kind&gt; node of a snippet in an XML
    ///  document.</summary>
    ///  <param name="AXMLDoc"><c>IXMLDocumentEx</c> [in] XML document
    ///  containing snippet.</param>
    ///  <param name="ASnippetNode"><c>IXMLNode</c> [in] Snippet node that
    ///  contains the &lt;kind&gt; tag.</param>
    ///  <param name="ADefault"><c>TSnippetKind</c> [in] Value to use if node
    ///  doesn't exist or has a non-standard value.</param>
    ///  <returns><c>TSnippetKind</c>. Required snippet kind.</returns>
    class function GetSnippetKind(const AXMLDoc: IXMLDocumentEx;
      const ASnippetNode: IXMLNode; const ADefault: TSnippetKind): TSnippetKind;

    ///  <summary>Writes a &lt;kind&gt; node to a an XML document.</summary>
    ///  <param name="AXMLDoc"><c>IXMLDocumentEx</c> [in] XML document that will
    ///  be updated.</param>
    ///  <param name="ASnippetNode"><c>IXMLNode</c> [in] Snippet node that will
    ///  contain the &lt;kind&gt; tag.</param>
    ///  <param name="AValue"><c>TSnippetKind</c> [in] Value of the &lt;kind&gt;
    ///  node.</param>
    class procedure WriteSnippetKind(const AXMLDoc: IXMLDocumentEx;
      const ASnippetNode: IXMLNode; const AValue: TSnippetKind);

    ///  <summary>Gets compile results for a snippet in an XML document.
    ///  </summary>
    ///  <param name="AXMLDoc"><c>IXMLDocumentEx</c> [in] XML document
    ///  containing snippet.</param>
    ///  <param name="ASnippetNode"><c>IXMLNode</c> [in] Snippet node that
    ///  contains compile results.</param>
    ///  <returns><c>TCompileResults</c>. Array of compile results. Includes
    ///  default results for missing compilers.</returns>
    class function GetCompilerResults(const AXMLDoc: IXMLDocumentEx;
      const ASnippetNode: IXMLNode): TCompileResults;

    ///  <summary>Writes compile results for a snippet to an XML document.
    ///  </summary>
    ///  <param name="AXMLDoc"><c>IXMLDocumentEx</c> [in] XML document that will
    ///  be updated.</param>
    ///  <param name="ASnippetNode"><c>IXMLNode</c> [in] Snippet node that will
    ///  contain the compile results.</param>
    ///  <param name="ACompRes"><c>TCompileResults</c> [in] Array of compile
    ///  results.</param>
    class procedure WriteCompilerResults(const AXMLDoc: IXMLDocumentEx;
      const ASnippetNode: IXMLNode; const ACompRes: TCompileResults);

    ///  <summary>Gets a list of names from &lt;pascal-name&gt; elements in an
    ///  XML document.</summary>
    ///  <param name="AXMLDoc"><c>IXMLDocumentEx</c> [in] XML document
    ///  containing the name list.</param>
    ///  <param name="AListNode"><c>IXMLNode</c> [in] XML node that contains all
    ///  the list nodes.</param>
    ///  <param name="ANameList"><c>IStringList</c> [in] Receives text of all
    ///  the &lt;pascal-name&gt; elements in the list.</param>
    class procedure GetPascalNameList(const AXMLDoc: IXMLDocumentEx;
      const AListNode: IXMLNode; const ANameList: IStringList);

    ///  <summary>Writes a Pascal name list to an XML document.</summary>
    ///  <param name="AXMLDoc"><c>IXMLDocumentEx</c> [in] XML document into
    ///  which the list is written.</param>
    ///  <param name="AParent"><c>IXMLNode</c> [in] Parent node that is to
    ///  contain the list.</param>
    ///  <param name="AListName"><c>string</c> [in] Name of new list node that
    ///  is to be the parent of the list.</param>
    ///  <param name="ANameList"><c>IStringList</c> [in] List of Pascal names
    ///  to be written.</param>
    class procedure WritePascalNameList(const AXMLDoc: IXMLDocumentEx;
      const AParent: IXMLNode; const AListName: string;
      const ANameList: IStringList);
  end;

implementation

uses
  // Project
  UStrUtils;

{ TCS4FormatHelper }

class procedure TCS4FormatHelper.CreateComment(const AXMLDoc: IXMLDocumentEx;
  const AComment: string);
begin
  AXMLDoc.ChildNodes.Add(AXMLDoc.CreateNode(' ' + AComment + ' ', ntComment));
end;

class function TCS4FormatHelper.CreateRootNode(const AXMLDoc: IXMLDocumentEx;
  const ANodeName, AWatermark: string; const AVersion: Integer): IXMLNode;
begin
  Result := AXMLDoc.CreateNode(ANodeName);
  Result.SetAttribute(RootWatermarkAttr, AWatermark);
  Result.SetAttribute(RootVersionAttr, AVersion);
  AXMLDoc.ChildNodes.Add(Result);
end;

class function TCS4FormatHelper.GetCompilerResults(
  const AXMLDoc: IXMLDocumentEx; const ASnippetNode: IXMLNode): TCompileResults;

  // Converts an identifier string to a compiler ID. Returns True if AIDStr is
  // valid, False if not.
  function IDStrToCompID(AIDStr: string; out AMatch: TCompilerID): Boolean;
  var
    CompID: TCompilerID;  // loops thru all compiler IDs
  begin
    {TODO -cVault: drop support for detecting CodeSnip v3 entries}
    // 'dXE4' can be encountered when reading files written by CodeSnip 3, which
    // uses correct 'dXE4' symbol for Delphi XE4 instead of 'dDX4' used
    // (erroneously) by CodeSnip 4. So the following two lines convert the
    // CodeSnip 3 value to the CodeSnip 4 value before testing.
    if AIDStr = 'dXE4' then
      AIDStr := CompilerIDValues[ciDXE4];
    Result := False;
    for CompID := Low(TCompilerID) to High(TCompilerID) do
    begin
      if CompilerIDValues[CompID] = AIDStr then
      begin
        Result := True;
        AMatch := CompID;
        Break;
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
  ListNode := AXMLDoc.FindFirstChildNode(ASnippetNode, CompilerResultsNodeName);
  if not Assigned(ListNode) then
    Exit;

  // Get list of compiler result nodes contained in list and process each one
  ResultsNodes := AXMLDoc.FindChildNodes(ListNode, CompilerResultNodeName);
  for ResultNode in ResultsNodes do
  begin
    if ResultNode.IsTextElement then
    begin
      // get compile result identifier
      CompResultStr := ResultNode.Text;
      if CompResultStr = '' then
        CompResultStr := '?';
      // add specified result to returned array
      if IDStrToCompID(
        ResultNode.Attributes[CompilerResultIdAttr], CompID
      ) then
      begin
        {TODO -cRefactor: Use a lookup table instead of case statement}
        case CompResultStr[1] of
          'Y': Result[CompID] := crSuccess;
          'N': Result[CompID] := crError;
          'W': Result[CompiD] := crWarning;
          else Result[CompID] := crQuery;
        end;
      end;
    end;
  end;
end;

class function TCS4FormatHelper.GetHiliteSource(const AXMLDoc: IXMLDocumentEx;
  const ASnippetNode: IXMLNode; const ADefault: Boolean): Boolean;
var
  ValueText: string;
begin
  ValueText := GetSubTagText(AXMLDoc, ASnippetNode, HighlightSourceNodeName);
  if ValueText <> '' then
    Result := ValueText <> '0'
  else
    Result := ADefault;
end;

class procedure TCS4FormatHelper.GetPascalNameList(const AXMLDoc: IXMLDocumentEx;
  const AListNode: IXMLNode; const ANameList: IStringList);
var
  NameNode: IXMLNode;
  NodeList: IXMLSimpleNodeList;
begin
  ANameList.Clear;
  if not Assigned(AListNode) then
    Exit;   // this is permitted since Pascal name lists may be empty or missing
  NodeList := AXMLDoc.FindChildNodes(AListNode, PascalNameNodeName);
  for NameNode in NodeList do
    if NameNode.IsTextElement then
      ANameList.Add(NameNode.Text);
end;

class function TCS4FormatHelper.GetSnippetKind(const AXMLDoc: IXMLDocumentEx;
  const ASnippetNode: IXMLNode; const ADefault: TSnippetKind): TSnippetKind;
var
  ValueText: string;
begin
  {TODO -cRefactor: Use a lookup table instead of if ... else if  .. else tests}
  ValueText := GetSubTagText(AXMLDoc, ASnippetNode, SnippetKindNodeName);
  if StrSameText(ValueText, SnippetKindValues[skFreeform]) then
    Result := skFreeform
  else if StrSameText(ValueText, SnippetKindValues[skRoutine]) then
    Result := skRoutine
  else if StrSameText(ValueText, SnippetKindValues[skConstant]) then
    Result := skConstant
  else if StrSameText(ValueText, SnippetKindValues[skTypeDef]) then
    Result := skTypeDef
  else if StrSameText(ValueText, SnippetKindValues[skUnit]) then
    Result := skUnit
  else if StrSameText(ValueText, SnippetKindValues[skClass]) then
    Result := skClass
  else
    Result := ADefault;
end;

class function TCS4FormatHelper.GetStandardFormat(const AXMLDoc: IXMLDocumentEx;
  const ASnippetNode: IXMLNode; const ADefault: Boolean): Boolean;
var
  ValueText: string;
begin
  ValueText := GetSubTagText(AXMLDoc, ASnippetNode, StandardFormatNodeName);
  if ValueText <> '' then
    Result := ValueText <> '0'
  else
    Result := ADefault;
end;

class function TCS4FormatHelper.ValidateRootNode(const AXMLDoc: IXMLDocumentEx;
  const ANodeName, AWatermark: string; const AVersions: TRange): Integer;
var
  RootNode: IXMLNode; // document root node
resourcestring
  // Error messages
  sNoRootNode = 'Invalid document: no root element present';
  sBadRootName = 'Invalid document: root element must be named <%s>';
  sBadWatermark = 'Invalid document: watermark is incorrect';
  sBadVersion = 'Invalid document: unsupported document version %d';
begin
  RootNode := AXMLDoc.DocumentElement;
  // There must be a root node
  if not Assigned(RootNode) then
    raise ECodeSnipXML.Create(sNoRootNode);
  // Correct root node must be present, with valid watermark and version
  if RootNode.NodeName <> ANodeName then
    raise ECodeSnipXML.CreateFmt(sBadRootName, [ANodeName]);
  if RootNode.Attributes[RootWatermarkAttr] <> AWatermark then
    raise ECodeSnipXML.Create(sBadWatermark);
  Result := RootNode.Attributes[RootVersionAttr];
  if not AVersions.Contains(Result) then
    raise ECodeSnipXML.CreateFmt(sBadVersion, [Result]);
end;

class procedure TCS4FormatHelper.WriteCompilerResults(
  const AXMLDoc: IXMLDocumentEx; const ASnippetNode: IXMLNode;
  const ACompRes: TCompileResults);
const
  {TODO -cRefactor: Move this map to private class consts to make available to
          GetCompilerResults.}
  {TODO -cVault: Make CompResMap comply with CS4 XML doc specs - ? should be Q}
  // Map of compiler results onto character representation store in XML file.
  CompResMap: array[TCompileResult] of Char = ('Y', 'W', 'N', '?');
var
  CompResultsNode: IXMLNode;  // node that stores all compiler results
  CompResultNode: IXMLNode;   // each compiler result node
  CompID: TCompilerID;        // loops thru all supported compilers
begin
  // compiler results value: only write known results
  CompResultsNode := AXMLDoc.CreateElement(ASnippetNode, CompilerResultsNodeName);
  for CompID := Low(TCompilerID) to High(TCompilerID) do
  begin
    if ACompRes[CompID] <> crQuery then
    begin
      CompResultNode := AXMLDoc.CreateElement(
        CompResultsNode, CompilerResultNodeName, CompResMap[ACompRes[CompID]]
      );
      CompResultNode.Attributes[CompilerResultIdAttr] :=
        CompilerIDValues[CompID];
    end;
  end;
end;

class procedure TCS4FormatHelper.WritePascalNameList(
  const AXMLDoc: IXMLDocumentEx; const AParent: IXMLNode;
  const AListName: string; const ANameList: IStringList);
var
  ListNode: IXMLNode;
  Name: string;
begin
  ListNode := AXMLDoc.CreateElement(AParent, AListName);
  for Name in ANameList do
    AXMLDoc.CreateElement(ListNode, PascalNameNodeName, Name);
end;

class procedure TCS4FormatHelper.WriteSnippetKind(const AXMLDoc: IXMLDocumentEx;
  const ASnippetNode: IXMLNode; const AValue: TSnippetKind);
begin
  AXMLDoc.CreateElement(
    ASnippetNode, SnippetKindNodeName, SnippetKindValues[AValue]
  );
end;

end.

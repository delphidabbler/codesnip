{
 * UXMLDocHelper.pas
 *
 * Implements a static class that helps with input and output to CodeSnip XML
 * documents.
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
 * The Original Code is UXMLDocHelper.pas
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


unit UXMLDocHelper;


interface


uses
  // Delphi
  XMLIntf,
  // Project
  Compilers.UGlobals, UExceptions, UIStringList, USnippets, UStructs,
  UXMLDocumentEx;


type

  {
  TXMLDocHelper:
    Static class that helps with input and output to CodeSnip XML documents.
    Provides functionality common to two or more XML read/write classes.
  }
  TXMLDocHelper = class(TObject)
  strict private
    class function FindRootNodeType(const XMLDoc: IXMLDocumentEx;
      const ANodeType: TNodeType): IXMLNode;
      {Finds a specified type of root node.
        @param XMLDoc [in] Document containing node.
        @param ANodeType [in] Type of node required.
        @return Reference to found node or nil if no node found.
      }
  public
    class function CreateXMLDoc: IXMLDocumentEx;
      {Creates a new XML document object with required properties.
        @return New XML document object.
      }
    class procedure CreateXMLProcInst(const XMLDoc: IXMLDocumentEx);
      {Creates xml processing instruction in document.
        @param XMLDoc [in] Document in which processing instruction is inserted.
      }
    class procedure CreateComment(const XMLDoc: IXMLDocumentEx;
      const Comment: string);
      {Creates a comment at the top level of an XML document.
        @param XMLDoc [in] Document in which comment is inserted.
        @param Comment [in] Comment to be inserted.
      }
    class function CreateRootNode(const XMLDoc: IXMLDocumentEx;
      const NodeName, Watermark: string; const Version: Integer): IXMLNode;
      {Creates a root in XML document.
        @param XMLDoc [in] Document in which to insert root node.
        @param NodeName [in] Name of root node.
        @param Watermark [in] Value of root node's "watermark" attribute.
        @param Version [in] Value of root node's "version" attribute.
        @return Reference to new root node.
      }
    class function GetSubTagText(const XMLDoc: IXMLDocumentEx;
      const ParentNode: IXMLNode; const SubTagName: string): string;
      {Gets text of subtag of a parent node in an XML document.
        @param XMLDoc [in] XML document containing sub tag.
        @param ParentNode [in] Parent node of sub tag.
        @param SubTagName [in] Name of desired subtag.
        @return Sub tag's text if sub tag exists and is a text node, ''
          otherwise.
      }
    class procedure GetPascalNameList(const XMLDoc: IXMLDocumentEx;
      const ListNode: IXMLNode; const NameList: IStringList);
      {Gets a list of names in <pascal-name> elements with a list.
        @param XMLDoc [in] XML document containing name list.
        @param ListNode [in] Node that contains list.
        @param NameList [in] Receives text of all <pascal-name> elements in
          list.
      }
    class function GetCompilerResults(const XMLDoc: IXMLDocumentEx;
      const RoutineNode: IXMLNode): TCompileResults;
      {Gets compiler results for a snippet in an XML document.
        @param XMLDoc [in] XML document containing snippet.
        @param RoutineNode [in] Document node that contains compiler results
          tag.
        @return Array of compiler results. Provides defaults for missing
          compilers.
      }
    class function GetStandardFormat(const XMLDoc: IXMLDocumentEx;
      const RoutineNode: IXMLNode; const Default: Boolean): Boolean;
      {Gets value of a <standard-format> node of a snippet in an XML document.
        @param XMLDoc [in] XML document containing snippet.
        @param RoutineNode [in] Snippet node that contains standard format tag.
        @param Default [in] Value to use if node doesn't exist or has
          non-standard value.
        @return Value of node, or default value.
      }
    class function GetSnippetKind(const XMLDoc: IXMLDocumentEx;
      const RoutineNode: IXMLNode; const Default: TSnippetKind): TSnippetKind;
      {Gets value of <kind> node of a snippet in an XML document.
        @param XMLDoc [in] XML document containing snippet.
        @param RoutineNode [in] Snippet node that contains kind tag.
        @param Default [in] Value to use if node doesn't exist or has
          non-standard value.
        @return Required snippet kind.
      }
    class procedure WriteCompilerResults(const XMLDoc: IXMLDocumentEx;
      const RoutineNode: IXMLNode; const CompRes: TCompileResults);
      {Writes compile results for a snippet to XML document.
        @param XMLDoc [in] XML document to receive compile results.
        @param RoutineNode [in] Node containing snippet that received compile
          results.
        @param CompRes [in] Array of compiler results.
      }
    class procedure WritePascalNameList(const XMLDoc: IXMLDocumentEx;
      const Parent: IXMLNode; const ListName: string; const Names: IStringList);
      {Writes a Pascal name list to an XML document.
        @param XMLDoc [in] XML document to which list is written.
        @param Parent [in] Parent node that is to contain name list.
        @param ListName [in] Name of new list node that is parent of list.
        @param Names [in] List of Pascal names.
      }
    class procedure WriteSnippetKind(const XMLDoc: IXMLDocumentEx;
      const RoutineNode: IXMLNode; const Value: TSnippetKind);
      {Writes a <kind> node to a an XML document.
        @param XMLDoc [in] XML document to receive the node.
        @param RoutineNode [in] Node containing snippet that receives kind node.
        @param Value [in] Value of <kind> node.
      }
    class function ValidateRootNode(const XMLDoc: IXMLDocumentEx;
      const ANodeName, AWatermark: string; const AVersions: TRange): Integer;
      {Validates the root node of an XML document.
        @param XMLDoc [in] XML document to be validated.
        @param ANodeName [in] Name of root mode.
        @param AWatermark [in] Required value of root node's "watermark"
          attribute.
        @param AVersions [in] Range of acceptable file version numbers.
        @return Document version.
        @except ECodeSnipXML raised on error.
      }
    class procedure ValidateProcessingInstr(const XMLDoc: IXMLDocumentEx);
      {Checks that an XML document has a valid xml processing instruction.
        @param XMLDoc [in] Document to be checked.
        @except ECodeSnipXML raised on error.
      }
  end;

  {
  ECodeSnipXML:
    Class of exception raised by TXMLDocHelper validation methods.
  }
  ECodeSnipXML = class(ECodeSnip);


implementation


uses
  // Delphi
  SysUtils, Windows {for inlining},
  // Project
  USnippetKindInfo, UXMLDocConsts;


{ TXMLDocHelper }

class procedure TXMLDocHelper.CreateComment(const XMLDoc: IXMLDocumentEx;
  const Comment: string);
  {Creates a comment at the top level of an XML document.
    @param XMLDoc [in] Document in which comment is inserted.
    @param Comment [in] Comment to be inserted.
  }
begin
  XMLDoc.ChildNodes.Add(XMLDoc.CreateNode(' ' + Comment + ' ', ntComment));
end;

class function TXMLDocHelper.CreateRootNode(const XMLDoc: IXMLDocumentEx;
  const NodeName, Watermark: string; const Version: Integer): IXMLNode;
  {Creates a root in XML document.
    @param XMLDoc [in] Document in which to insert root node.
    @param NodeName [in] Name of root node.
    @param Watermark [in] Value of root node's "watermark" attribute.
    @param Version [in] Value of root node's "version" attribute.
    @return Reference to new root node.
  }
begin
  Result := XMLDoc.CreateNode(NodeName);
  Result.SetAttribute(cRootWatermarkAttr, Watermark);
  Result.SetAttribute(cRootVersionAttr, Version);
  XMLDoc.ChildNodes.Add(Result);
end;

class function TXMLDocHelper.CreateXMLDoc: IXMLDocumentEx;
  {Creates a new XML document object with required properties.
    @return New XML document object.
  }
begin
  Result := TXMLDocumentEx.Create(nil);
  Result.Options := [doNodeAutoIndent];
  Result.ParseOptions := [poPreserveWhiteSpace];
end;

class procedure TXMLDocHelper.CreateXMLProcInst(const XMLDoc: IXMLDocumentEx);
  {Creates xml processing instruction in document.
    @param XMLDoc [in] Document in which processing instruction is inserted.
  }
begin
  XMLDoc.ChildNodes.Add(
    XMLDoc.CreateNode(cXMLNode, ntProcessingInstr, cXMLNodeText)
  );
end;

class function TXMLDocHelper.FindRootNodeType(const XMLDoc: IXMLDocumentEx;
  const ANodeType: TNodeType): IXMLNode;
  {Finds a specified type of root node.
    @param XMLDoc [in] Document containing node.
    @param ANodeType [in] Type of node required.
    @return Reference to found node or nil if no node found.
  }
var
  Idx: Integer; // loops thru immediate child nodes of xml document
begin
  Result := nil;
  for Idx := 0 to Pred(XMLDoc.ChildNodes.Count) do
  begin
    if XMLDoc.ChildNodes.Nodes[Idx].NodeType = ANodeType then
    begin
      Result := XMLDoc.ChildNodes.Nodes[Idx];
      Break;
    end;
  end;
end;

class function TXMLDocHelper.GetCompilerResults(const XMLDoc: IXMLDocumentEx;
  const RoutineNode: IXMLNode): TCompileResults;
  {Gets compiler results for a snippet in an XML document.
    @param XMLDoc [in] XML document containing snippet.
    @param RoutineNode [in] Document node that contains compiler results tag.
    @return Array of compiler results. Provides defaults for missing compilers.
  }

  // -------------------------------------------------------------------------
  function IDStrToCompID(const IDStr: string;
    out Match: TCompilerID): Boolean;
    {Converts an identifier string to a compiler ID.
      @param IDStr [in] Identifier string.
      @param Match [out] Set to compiler ID that matches IDStr. Undefined if
        IDStr not recognised.
      @return True if IDStr is recognised, False if not.
    }
  var
    CompID: TCompilerID;  // loops thru all compiler IDs
  begin
    Result := False;
    for CompID := Low(TCompilerID) to High(TCompilerID) do
    begin
      if cCompilerIDs[CompID] = IDStr then
      begin
        Result := True;
        Match := CompID;
        Break;
      end;
    end;
  end;
  // -------------------------------------------------------------------------

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
  ListNode := XMLDoc.FindFirstChildNode(RoutineNode, cCompilerResultsNode);
  if not Assigned(ListNode) then
    Exit;

  // Get list of compiler-result nodes contained in list and process each one
  ResultsNodes := XMLDoc.FindChildNodes(ListNode, cCompilerResultNode);
  for ResultNode in ResultsNodes do
  begin
    if ResultNode.IsTextElement then
    begin
      // get compile result identifier
      CompResultStr := ResultNode.Text;
      if CompResultStr = '' then
        CompResultStr := '?';
      // add specified result function result
      if IDStrToCompID(
        ResultNode.Attributes[cCompilerResultIdAttr], CompID
      ) then
      begin
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

class procedure TXMLDocHelper.GetPascalNameList(const XMLDoc: IXMLDocumentEx;
  const ListNode: IXMLNode; const NameList: IStringList);
  {Gets a list of names in <pascal-name> elements with a list.
    @param XMLDoc [in] XML document containing name list.
    @param ListNode [in] Node that contains list.
    @param NameList [in] Receives text of all <pascal-name> elements in list.
  }
var
  NameNode: IXMLNode;             // name of a node in the list
  NodeList: IXMLSimpleNodeList;   // list of matching child nodes if ListNode
begin
  NameList.Clear;
  if not Assigned(ListNode) then
    Exit;   // this is permitted since snippet lists can be empty or missing
  NodeList := XMLDoc.FindChildNodes(ListNode, cPascalNameNode);
  for NameNode in NodeList do
    if NameNode.IsTextElement then
      NameList.Add(NameNode.Text);
end;

class function TXMLDocHelper.GetSnippetKind(const XMLDoc: IXMLDocumentEx;
  const RoutineNode: IXMLNode; const Default: TSnippetKind): TSnippetKind;
  {Gets value of <kind> node of a snippet in an XML document.
    @param XMLDoc [in] XML document containing snippet.
    @param RoutineNode [in] Snippet node that contains kind tag.
    @param Default [in] Value to use if node doesn't exist or has non-standard
      value.
    @return Required snippet kind.
  }
var
  Value: string;  // text value of Kind node
begin
  Value := GetSubTagText(XMLDoc, RoutineNode, cKindNode);
  if AnsiSameText(Value, 'freeform') then
    Result := skFreeform
  else if AnsiSameText(Value, 'routine') then
    Result := skRoutine
  else if AnsiSameText(Value, 'const') then
    Result := skConstant
  else if AnsiSameText(Value, 'type') then
    Result := skTypeDef
  else
    Result := Default;
end;

class function TXMLDocHelper.GetStandardFormat(const XMLDoc: IXMLDocumentEx;
  const RoutineNode: IXMLNode; const Default: Boolean): Boolean;
  {Gets value of a <standard-format> node of a snippet in an XML document.
    @param XMLDoc [in] XML document containing snippet.
    @param RoutineNode [in] Snippet node that contains standard format tag.
    @param Default [in] Value to use if node doesn't exist or has non-standard
      value.
    @return Value of node, or default value.
  }
var
  Value: string;  // text value of Kind node
begin
  Value := GetSubTagText(XMLDoc, RoutineNode, cStandardFormatNode);
  if Value <> '' then
    Result := Value <> '0'
  else
    Result := Default;
end;

class function TXMLDocHelper.GetSubTagText(const XMLDoc: IXMLDocumentEx;
  const ParentNode: IXMLNode; const SubTagName: string): string;
  {Gets text of subtag of a parent node in an XML document.
    @param XMLDoc [in] XML document containing sub tag.
    @param ParentNode [in] Parent node of sub tag.
    @param SubTagName [in] Name of desired subtag.
    @return Sub tag's text if sub tag exists and is a text node, '' otherwise.
  }
var
  PropNode: IXMLNode; // sub tag's node
begin
  Result := '';
  PropNode := XMLDoc.FindFirstChildNode(ParentNode, SubTagName);
  if Assigned(PropNode) and (PropNode.IsTextElement) then
    Result := PropNode.Text;
end;

class procedure TXMLDocHelper.ValidateProcessingInstr(
  const XMLDoc: IXMLDocumentEx);
  {Checks that an XML document has a valid xml processing instruction.
    @param XMLDoc [in] Document to be checked.
    @except ECodeSnipXML raised on error.
  }
var
  XMLNode: IXMLNOde;  // xml processing node
resourcestring
  // Error messages
  sNoXMLProcInst = 'Invalid document: must begin with a valid XML processing '
    + 'instruction';
begin
  // Must have correct processing instruction (<?xml .... ?>)
  XMLNode := FindRootNodeType(XMLDoc, ntProcessingInstr);
  if not Assigned(XMLNode) or (XMLNode.NodeName <> cXMLNode)
    or (XMLNode.NodeType <> ntProcessingInstr) then
    raise ECodeSnipXML.Create(sNoXMLProcInst);
end;

class function TXMLDocHelper.ValidateRootNode(const XMLDoc: IXMLDocumentEx;
  const ANodeName, AWatermark: string; const AVersions: TRange): Integer;
  {Validates the root node of an XML document.
    @param XMLDoc [in] XML document to be validated.
    @param ANodeName [in] Name of root mode.
    @param AWatermark [in] Required value of root node's "watermark" attribute.
    @param AVersions [in] Range of acceptable file version numbers.
    @return Document version.
    @except ECodeSnipXML raised on error.
  }
var
  RootNode: IXMLNode; // document root node
resourcestring
  // Error messages
  sNoRootNode = 'Invalid document: no root element present';
  sBadRootName = 'Invalid document: root element must be named <%s>';
  sBadWatermark = 'Invalid document: watermark is incorrect';
  sBadVersion = 'Invalid document: unsupported document version %d';
begin
  RootNode := XMLDoc.DocumentElement;
  // There must be a root node
  if not Assigned(RootNode) then
    raise ECodeSnipXML.Create(sNoRootNode);
  // Correct root node must be present, with valid watermark and version
  if RootNode.NodeName <> ANodeName then
    raise ECodeSnipXML.CreateFmt(sBadRootName, [ANodeName]);
  if RootNode.Attributes[cRootWatermarkAttr] <> AWatermark then
    raise ECodeSnipXML.Create(sBadWatermark);
  Result := RootNode.Attributes[cRootVersionAttr];
  if not AVersions.Contains(Result) then
    raise ECodeSnipXML.CreateFmt(sBadVersion, [Result]);
end;

class procedure TXMLDocHelper.WriteCompilerResults(const XMLDoc: IXMLDocumentEx;
  const RoutineNode: IXMLNode; const CompRes: TCompileResults);
  {Writes compile results for a snippet to XML document.
    @param XMLDoc [in] XML document to receive compile results.
    @param RoutineNode [in] Node containing snippet that received compile
      results.
    @param CompRes [in] Array of compiler results.
  }
const
  // map of compiler results onto character representation store in XML file.
  cCompResMap: array[TCompileResult] of Char = ('Y', 'W', 'N', '?');
var
  CompResultsNode: IXMLNode;  // node that stores all compiler results
  CompResultNode: IXMLNode;   // each compiler result node
  CompID: TCompilerID;        // loops thru all supported compilers
begin
  // compiler results value: only write known results
  CompResultsNode := XMLDoc.CreateElement(RoutineNode, cCompilerResultsNode);
  for CompID := Low(TCompilerID) to High(TCompilerID) do
  begin
    if CompRes[CompID] <> crQuery then
    begin
      CompResultNode := XMLDoc.CreateElement(
        CompResultsNode, cCompilerResultNode,
        cCompResMap[CompRes[CompID]]
      );
      CompResultNode.Attributes[cCompilerResultIdAttr] := cCompilerIDs[CompID];
    end;
  end;
end;

class procedure TXMLDocHelper.WritePascalNameList(const XMLDoc: IXMLDocumentEx;
  const Parent: IXMLNode; const ListName: string; const Names: IStringList);
  {Writes a Pascal name list to an XML document.
    @param XMLDoc [in] XML document to which list is written.
    @param Parent [in] Parent node that is to contain name list.
    @param ListName [in] Name of new list node that is parent of list.
    @param Names [in] List of Pascal names.
  }
var
  ListNode: IXMLNode; // reference to enclosing list node
  Name: string;       // a name item in list
begin
  ListNode := XMLDoc.CreateElement(Parent, ListName);
  for Name in Names do
    XMLDoc.CreateElement(ListNode, cPascalNameNode, Name);
end;

class procedure TXMLDocHelper.WriteSnippetKind(const XMLDoc: IXMLDocumentEx;
  const RoutineNode: IXMLNode; const Value: TSnippetKind);
  {Writes a <kind> node to a an XML document.
    @param XMLDoc [in] XML document to receive the node.
    @param RoutineNode [in] Node containing snippet that receives kind node.
    @param Value [in] Value of <kind> node.
  }
const
  cValues: array[TSnippetKind] of string = (
    'freeform', 'routine', 'const', 'type'
  );
begin
  XMLDoc.CreateElement(RoutineNode, cKindNode, cValues[Value]);
end;

end.


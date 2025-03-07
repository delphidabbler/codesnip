{
 * This Source Code Form is subject to the terms of the Mozilla Public License,
 * v. 2.0. If a copy of the MPL was not distributed with this file, You can
 * obtain one at https://mozilla.org/MPL/2.0/
 *
 * Copyright (C) 2008-2021, Peter Johnson (gravatar.com/delphidabbler).
 *
 * Implements a static class that helps with input and output to any XML
 * documents.
}


unit UXMLDocHelper;


interface


uses
  // Delphi
  XMLIntf,
  // Project
  Compilers.UGlobals,
  DB.SnippetKind,
  UBaseObjects,
  UExceptions,
  UIStringList,
  UStructs,
  UXMLDocumentEx;


type

  ///  <summary>Static class that helps with input and output to any XML
  ///  documents.</summary>
  ///  <remarks>Do not contruct instances of this class.</remarks>
  TXMLDocHelper = class(TNoConstructObject)
  strict private
    const
      // XML processing instruction node
      XMLNodeName = 'xml';
      // XML processing node version attribute
      XMLNodeVersionAttr = 'version="1.0"';

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
    class function GetSubTagText(const XMLDoc: IXMLDocumentEx;
      const ParentNode: IXMLNode; const SubTagName: string): string;
      {Gets text of subtag of a parent node in an XML document.
        @param XMLDoc [in] XML document containing sub tag.
        @param ParentNode [in] Parent node of sub tag.
        @param SubTagName [in] Name of desired subtag.
        @return Sub tag's text if sub tag exists and is a text node, ''
          otherwise.
      }
    class procedure ValidateProcessingInstr(const XMLDoc: IXMLDocumentEx);
      {Checks that an XML document has a valid xml processing instruction.
        @param XMLDoc [in] Document to be checked.
        @except ECodeSnipXML raised on error.
      }
  end;

  {TODO -cVault: Replace ECodeSnipXML with EDataIO}
  {
  ECodeSnipXML:
    Class of exception raised by TXMLDocHelper validation methods.
  }
  ECodeSnipXML = class(ECodeSnip);


implementation


uses
  // Delphi
  Windows {for inlining},
  // Project
  UStrUtils;


{ TXMLDocHelper }

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
    XMLDoc.CreateNode(XMLNodeName, ntProcessingInstr, XMLNodeVersionAttr)
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
  if not Assigned(XMLNode) or (XMLNode.NodeName <> XMLNodeName)
    or (XMLNode.NodeType <> ntProcessingInstr) then
    raise ECodeSnipXML.Create(sNoXMLProcInst);
end;

end.


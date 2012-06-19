{
 * UXMLDocumentEx.pas
 *
 * Implements extensions to TXMLDocument and IXMLDocument that provide some
 * helper methods. Also provides a simple list object that can contain XML
 * nodes with enumerator.
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
 * The Original Code is UXMLDocumentEx.pas
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


unit UXMLDocumentEx;


interface


uses
  // Project
  Generics.Collections, XMLIntf, XMLDoc, XMLDom;


type

  {
  IXMLSimpleNodeList:
    Interface to a simple list of XML nodes. This interface is designed to
    enable creation and manipulation of a list of IXMLNode objects without
    reference to XML document.
  }
  IXMLSimpleNodeList = interface(IInterface)
    ['{93ABE85F-FBAC-4FDA-B071-5DD1C8537844}']
    function GetItem(Idx: Integer): IXMLNode;
      {Retrieves a node from the list.
        @param Idx [in] Index of required item.
        @return Reference to node at specified index.
      }
    function Add(const Node: IXMLNode): Integer;
      {Adds a node to the list.
        @param Node [in] Node to be added to list.
        @return Index of node in list.
      }
    function Count: Integer;
      {Gets number of items in list.
        @return Number of items.
      }
    function GetEnumerator: TEnumerator<IXMLNode>;
      {Creates list enumerator.
        @return Enumerator instance.
      }
    property Items[Idx: Integer]: IXMLNode read GetItem; default;
      {Indexed access to items in list}
  end;

  {
  IXMLDocumentEx:
    Extension of IXMLDocument from XMLIntf unit that provides some methods that
    help with searching for nodes in the document.
  }
  IXMLDocumentEx = interface(IXMLDocument)
    ['{2BE340ED-DD60-4DC4-B3F7-EF9C1C46DA6D}']
    function FindNode(PathToNode: DOMString): IXMLNode;
      {Finds a node specified by a path from, and including the root node.
        @param PathToNode [in] Path to node from root node, e.g
          root\node1\node2.
        @return Reference to required node or nil if node not found.
      }
    function FindChildNodes(const ParentNode: IXMLNode;
      const NodeName: DOMString): IXMLSimpleNodeList;
      overload;
      {Finds all child nodes of a parent node that are elements and have a
      specified name.
        @param ParentNode [in] Node containing child nodes to be found.
        @param NodeName [in] Name of element nodes to be found.
        @return List of matching nodes.
      }
    function FindChildNodes(const ParentNode: IXMLNode;
      const NodeName, AttribName: DOMString;
      const AttribValue: OleVariant): IXMLSimpleNodeList;
      overload;
      {Finds all child nodes of a parent node that are elements, have a
      specified name and have a specified attribute value.
        @param ParentNode [in] Node containing child nodes to be found.
        @param NodeName [in] Name of element nodes to be found.
        @param AttribName [in] Name of attribute to be found.
        @param AttribValue [in] Required attribute value.
        @return List of matching nodes.
      }
    function FindFirstChildNode(const ParentNode: IXMLNode;
      const NodeName: DOMString): IXMLNode;
      overload;
      {Finds first child node of a parent node that has a given name.
        @param ParentNode [in] Node containing child nodes to be found.
        @param NodeName [in] Name of node being searched for.
        @return Reference to required node or nil if node not found.
      }
    function FindFirstChildNode(const ParentNode: IXMLNode;
      const NodeName, AttribName: DOMString;
      const AttribValue: OleVariant): IXMLNode;
      overload;
      {Finds first child node of a parent node that has a given name and
      attribute value.
        @param ParentNode [in] Node containing child nodes to be found.
        @param NodeName [in] Name of node being searched for.
        @param AttribName [in] Name of attribute whose value to be tested.
        @param AttribValue [in] Require attribute value.
        @return Reference to required node or nil if node not found.
      }
    function CreateElement(const Parent: IXMLNode;
      const NodeName: DOMString): IXMLNode; overload;
      {Creates a new parented XML node in document.
        @param Parent [in] Parent node of new node.
        @param NodeName [in] Name of new node.
      }
    function CreateElement(const Parent: IXMLNode;
      const NodeName, Text: DOMString): IXMLNode; overload;
      {Creates a new parented XML text node in document.
        @param Param [in] Parent node of new node.
        @param NodeName [in] Name of new node.
        @param Text [in] Text stored in new node.
      }
  end;

  {
  TXMLSimpleNodeList:
    Implementation of a simple list of XML nodes that can be created and
    manipulated without reference to XML document.
  }
  TXMLSimpleNodeList = class(TInterfacedObject,
    IXMLSimpleNodeList
  )
  strict private
    var fList: TList<IXMLNode>;   // List of nodes
  public
    constructor Create;
      {Object constructor. Sets up object.
      }
    destructor Destroy; override;
      {Object destructor. Tears down object.
      }
    { IXMLSimpleNodeList }
    function GetItem(Idx: Integer): IXMLNode;
      {Retrieves a node from the list.
        @param Idx [in] Index of required item.
        @return Reference to node at specified index.
      }
    function Add(const Node: IXMLNode): Integer;
      {Adds a node to the list.
        @param Node [in] Node to be added to list.
        @return Index of node in list.
      }
    function Count: Integer;
      {Gets number of items in list.
        @return Number of items.
      }
    function GetEnumerator: TEnumerator<IXMLNode>;
      {Creates list enumerator.
        @return Enumerator instance.
      }
  end;

  {
  TXMLDocumentEx:
    Extension of TXMLDocument from XMLDoc unit that implements the methods of
    IXMLDocumentEx.
  }
  TXMLDocumentEx = class(TXMLDocument,
    IXMLDocument, IXMLDocumentEx
  )
  public
    { IXMLDocumentEx methods }
    function FindNode(PathToNode: DOMString): IXMLNode;
      {Finds a node specified by a path from, and including the root node.
        @param PathToNode [in] Path to node from root node, e.g
          root\node1\node2.
        @return Reference to required node or nil if node not found.
      }
    function FindChildNodes(const ParentNode: IXMLNode;
      const NodeName: DOMString): IXMLSimpleNodeList;
      overload;
      {Finds all child nodes of a parent node that are elements and have a
      specified name.
        @param ParentNode [in] Node containing child nodes to be found.
        @param NodeName [in] Name of element nodes to be found.
        @return List of matching nodes.
      }
    function FindChildNodes(const ParentNode: IXMLNode;
      const NodeName, AttribName: DOMString;
      const AttribValue: OleVariant): IXMLSimpleNodeList;
      overload;
      {Finds all child nodes of a parent node that are elements, have a
      specified name and have a specified attribute value.
        @param ParentNode [in] Node containing child nodes to be found.
        @param NodeName [in] Name of element nodes to be found.
        @param AttribName [in] Name of attribute to be found.
        @param AttribValue [in] Required attribute value.
        @return List of matching nodes.
      }
    function FindFirstChildNode(const ParentNode: IXMLNode;
      const NodeName: DOMString): IXMLNode;
      overload;
      {Finds first child node of a parent node that has a given name.
        @param ParentNode [in] Node containing child nodes to be found.
        @param NodeName [in] Name of node being searched for.
        @return Reference to required node or nil if node not found.
      }
    function FindFirstChildNode(const ParentNode: IXMLNode;
      const NodeName, AttribName: DOMString;
      const AttribValue: OleVariant): IXMLNode;
      overload;
      {Finds first child node of a parent node that has a given name and
      attribute value.
        @param ParentNode [in] Node containing child nodes to be found.
        @param NodeName [in] Name of node being searched for.
        @param AttribName [in] Name of attribute whose value to be tested.
        @param AttribValue [in] Require attribute value.
        @return Reference to required node or nil if node not found.
      }
    function CreateElement(const Parent: IXMLNode;
      const NodeName: DOMString): IXMLNode; overload;
      {Creates a new parented XML node in document.
        @param Parent [in] Parent node of new node.
        @param NodeName [in] Name of new node.
      }
    function CreateElement(const Parent: IXMLNode;
      const NodeName, Text: DOMString): IXMLNode; overload;
      {Creates a new parented XML text node in document.
        @param Param [in] Parent node of new node.
        @param NodeName [in] Name of new node.
        @param Text [in] Text stored in new node.
      }
  end;


implementation


uses
  // Project
  UIStringList;


{ TXMLDocumentEx }

function TXMLDocumentEx.FindChildNodes(const ParentNode: IXMLNode;
  const NodeName: DOMString): IXMLSimpleNodeList;
  {Finds all child nodes of a parent node that are elements and have a specified
  name.
    @param ParentNode [in] Node containing child nodes to be found.
    @param NodeName [in] Name of element nodes to be found.
    @return List of matching nodes.
  }
var
  NodeList: IXMLNodeList; // list of child nodes of parent
  Node: IXMLNode;         // a node from NodeList
  Idx: Integer;           // loops thru nodes of NodeList.
begin
  Assert(Assigned(ParentNode),
    ClassName + '.FindChildNodes: ParentNode is nil');
  Result := TXMLSimpleNodeList.Create;
  NodeList := ParentNode.ChildNodes;
  for Idx := 0 to Pred(NodeList.Count) do
  begin
    Node := NodeList[Idx];
    if (Node.NodeType = ntElement) and (Node.NodeName = NodeName) then
      Result.Add(Node);
  end;
end;

function TXMLDocumentEx.CreateElement(const Parent: IXMLNode;
  const NodeName: DOMString): IXMLNode;
  {Creates a new parented XML node in document.
    @param Parent [in] Parent node of new node.
    @param NodeName [in] Name of new node.
  }
begin
  Result := CreateNode(NodeName);
  Parent.ChildNodes.Add(Result);
end;

function TXMLDocumentEx.CreateElement(const Parent: IXMLNode; const NodeName,
  Text: DOMString): IXMLNode;
  {Creates a new parented XML text node in document.
    @param Param [in] Parent node of new node.
    @param NodeName [in] Name of new node.
    @param Text [in] Text stored in new node.
  }
begin
  Result := CreateElement(Parent, NodeName);
  Result.Text := Text;
end;

function TXMLDocumentEx.FindChildNodes(const ParentNode: IXMLNode;
  const NodeName, AttribName: DOMString;
  const AttribValue: OleVariant): IXMLSimpleNodeList;
  {Finds all child nodes of a parent node that are elements, have a specified
  name and have a specified attribute value.
    @param ParentNode [in] Node containing child nodes to be found.
    @param NodeName [in] Name of element nodes to be found.
    @param AttribName [in] Name of attribute to be found.
    @param AttribValue [in] Required attribute value.
    @return List of matching nodes.
  }
var
  NodeList: IXMLNodeList; // list of child nodes of parent
  Node: IXMLNode;         // a node from NodeList
  Idx: Integer;           // loops thru nodes of NodeList.
begin
  Assert(Assigned(ParentNode), ClassName + 'FindChildNodes: ParentNode is nil');
  Result := TXMLSimpleNodeList.Create;
  NodeList := ParentNode.ChildNodes;
  for Idx := 0 to Pred(NodeList.Count) do
  begin
    Node := NodeList[Idx];
    if (Node.NodeType = ntElement) and (Node.NodeName = NodeName)
      and (Node.Attributes[AttribName] = AttribValue) then
      Result.Add(Node);
  end;
end;

function TXMLDocumentEx.FindFirstChildNode(const ParentNode: IXMLNode;
  const NodeName: DOMString): IXMLNode;
  {Finds first child node of a parent node that has a given name.
    @param ParentNode [in] Node containing child nodes to be found.
    @param NodeName [in] Name of node being searched for.
    @return Reference to required node or nil if node not found.
  }
begin
  Result := ParentNode.ChildNodes.FindNode(NodeName);
end;

function TXMLDocumentEx.FindFirstChildNode(const ParentNode: IXMLNode;
  const NodeName, AttribName: DOMString;
  const AttribValue: OleVariant): IXMLNode;
  {Finds first child node of a parent node that has a given name and attribute
  value.
    @param ParentNode [in] Node containing child nodes to be found.
    @param NodeName [in] Name of node being searched for.
    @param AttribName [in] Name of attribute whose value to be tested.
    @param AttribValue [in] Require attribute value.
    @return Reference to required node or nil if node not found.
  }
var
  Nodes: IXMLSimpleNodeList;  // list of all matching nodes
begin
  Nodes := FindChildNodes(ParentNode, NodeName, AttribName, AttribValue);
  if Nodes.Count > 0 then
    Result := Nodes[0]
  else
    Result := nil;
end;

function TXMLDocumentEx.FindNode(PathToNode: DOMString): IXMLNode;
  {Finds a node specified by a path from, and including the root node.
    @param PathToNode [in] Path to node from root node, e.g root\node1\node2.
    @return Reference to required node or nil if node not found.
  }
var
  NodeNames: IStringList;   // list of names of nodes in path
  NodeName: string;         // name of a node in list
  NodeList: IXMLNodeList;   // list of child nodes of a node in path
  Node: IXMLNode;           // refers to each node in path
begin
  Node := nil;
  NodeList := Self.ChildNodes;
  NodeNames := TIStringList.Create(PathToNode, '\', False);
  for NodeName in NodeNames do
  begin
    Node := NodeList.FindNode(NodeName);
    if not Assigned(Node) then
      Break;
    NodeList := Node.ChildNodes;
  end;
  Result := Node;
end;

{ TXMLSimpleNodeList }

function TXMLSimpleNodeList.Add(const Node: IXMLNode): Integer;
  {Adds a node to the list.
    @param Node [in] Node to be added to list.
    @return Index of node in list.
  }
begin
  Result := fList.Add(Node);
end;

function TXMLSimpleNodeList.Count: Integer;
  {Gets number of items in list.
    @return Number of items.
  }
begin
  Result := fList.Count;
end;

constructor TXMLSimpleNodeList.Create;
  {Object constructor. Sets up object.
  }
begin
  inherited;
  fList := TList<IXMLNode>.Create;
end;

destructor TXMLSimpleNodeList.Destroy;
  {Object destructor. Tears down object.
  }
begin
  fList.Free;
  inherited;
end;

function TXMLSimpleNodeList.GetEnumerator: TEnumerator<IXMLNode>;
  {Creates list enumerator.
    @return Enumerator instance.
  }
begin
  Result := fList.GetEnumerator;
end;

function TXMLSimpleNodeList.GetItem(Idx: Integer): IXMLNode;
  {Retrieves a node from the list.
    @param Idx [in] Index of required item.
    @return Reference to node at specified index.
  }
begin
  Result := fList[Idx];
end;

end.


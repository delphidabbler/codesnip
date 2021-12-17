{
 * This Source Code Form is subject to the terms of the Mozilla Public License,
 * v. 2.0. If a copy of the MPL was not distributed with this file, You can
 * obtain one at https://mozilla.org/MPL/2.0/
 *
 * Copyright (C) 2008-2020, Peter Johnson (gravatar.com/delphidabbler).
 *
 * Implements extensions to TXMLDocument and IXMLDocument that provide some
 * helper methods. Also provides a simple list object that can contain XML
 * nodes with enumerator.
}


unit UXMLDocumentEx;


interface


uses
  // Delphi
  SysUtils,
  // Project
  Generics.Collections, XMLIntf, XMLDoc, XMLDom;


type
  ///  <summary>
  ///  Interface to a simple list of XML nodes. This interface is designed to
  ///  enable creation and manipulation of a list of IXMLNode objects without
  ///  reference to XML document..
  ///  </summary>
  IXMLSimpleNodeList = interface(IInterface)
    ['{93ABE85F-FBAC-4FDA-B071-5DD1C8537844}']
    ///  <summary>Retrieves a node from the list by index.</summary>
    function GetItem(Idx: Integer): IXMLNode;
    ///  <summary>Adds a given node to the list and returns index of new node
    ///  in list.</summary>
    function Add(const Node: IXMLNode): Integer;
    ///  <summary>Returns number of items in list.</summary>
    function Count: Integer;
    ///  <summary>Creates list enumerator.</summary>
    ///  <remarks>This method is mainly for use by the compiler and permits the
    ///  list to be used in for..in loops.</remarks>
    function GetEnumerator: TEnumerator<IXMLNode>;
    ///  <summary>Indexed access to list items.</summary>
    property Items[Idx: Integer]: IXMLNode read GetItem; default;
  end;

type
  ///  <summary>
  ///  Extension of XMLIntf.IXMLDocument.
  ///  </summary>
  ///  <remarks>
  ///  Provides some methods that help with searching for nodes in the document,
  ///  creation of new nodes and loading and saving documents from and to byte
  ///  arrays.
  ///  </remarks>
  IXMLDocumentEx = interface(IXMLDocument)
    ['{2BE340ED-DD60-4DC4-B3F7-EF9C1C46DA6D}']

    ///  <summary>Finds a node specified by a path from, and including the
    ///  document's root node.</summary>
    ///  <param name="PathToNode">DOMString [in] Path to node from root node.
    ///  </param>
    ///  <returns>IXMLNode - Reference to required node or nil if node not
    ///  found.</returns>
    ///  <remarks>Node in path are separated by backslash, e.g.
    ///  root\node1\node2</remarks>
    function FindNode(PathToNode: DOMString): IXMLNode;

    ///  <summary>Finds all child nodes of a parent node that are elements and
    ///  have a specified name.</summary>
    ///  <param name="ParentNode">IXMLNode [in] Node containing child nodes to
    ///  be found.</param>
    ///  <param name="NodeName">DOMString [in] Name of required element nodes.
    ///  </param>
    ///  <returns>IXMLSimpleNodeList - List of matching nodes.</returns>
    function FindChildNodes(const ParentNode: IXMLNode;
      const NodeName: DOMString): IXMLSimpleNodeList;
      overload;

    ///  <summary>Finds all child nodes of a parent node that are elements, have
    ///  a specified name and have a specified attribute value.</summary>
    ///  <param name="ParentNode">IXMLNode [in] Node containing child nodes to
    ///  be found.</param>
    ///  <param name="NodeName">DOMString [in] Name of required element nodes.
    ///  </param>
    ///  <param name="AttribName">DOMString [in] Name of required attribute.
    ///  </param>
    ///  <param name="AttribValue">OleVariant [in] Required value of AttribName.
    ///  </param>
    ///  <returns>IXMLSimpleNodeList - List of matching nodes.</returns>
    function FindChildNodes(const ParentNode: IXMLNode;
      const NodeName, AttribName: DOMString;
      const AttribValue: OleVariant): IXMLSimpleNodeList;
      overload;

    ///  <summary>Finds first child node that has a given name.</summary>
    ///  <param name="ParentNode">IXMLNode [in] Node containing child nodes to
    ///  be searched.</param>
    ///  <param name="NodeName">DOMString [in] Name of required element node.
    ///  </param>
    ///  <returns>IXMLNode - Reference to found node or nil if no node found.
    ///  </returns>
    function FindFirstChildNode(const ParentNode: IXMLNode;
      const NodeName: DOMString): IXMLNode;
      overload;

    ///  <summary>Finds first child node that has a given name and attribute
    ///  value.</summary>
    ///  <param name="ParentNode">IXMLNode [in] Node containing child nodes to
    ///  be found.</param>
    ///  <param name="NodeName">DOMString [in] Name of required element node.
    ///  </param>
    ///  <param name="AttribName">DOMString [in] Name of required attribute.
    ///  </param>
    ///  <param name="AttribValue">OleVariant [in] Required value of AttribName.
    ///  </param>
    ///  <returns>IXMLNode - Reference to found node or nil if no node found.
    ///  </returns>
    function FindFirstChildNode(const ParentNode: IXMLNode;
      const NodeName, AttribName: DOMString;
      const AttribValue: OleVariant): IXMLNode;
      overload;

    ///  <summary>Creates a new parented XML node in document.</summary>
    ///  <param name="Parent">IXMLNode [in] Parent node of new node.</param>
    ///  <param name="NodeName">DOMString [in] Name of new node.</param>
    ///  <returns>IXMLNode - Requied new node.</returns>
    function CreateElement(const Parent: IXMLNode;
      const NodeName: DOMString): IXMLNode; overload;

    ///  <summary>Creates a new parented XML text node in document.</summary>
    ///  <param name="Parent">IXMLNode [in] Parent node of new node.</param>
    ///  <param name="NodeName">DOMString [in] Name of new node.</param>
    ///  <param name="Text">DOMString [in] Text stored in new node.</param>
    ///  <returns>IXMLNode - Requied new node.</returns>
    function CreateElement(const Parent: IXMLNode;
      const NodeName, Text: DOMString): IXMLNode; overload;

    ///  <summary>Loads XML document from data stored in byte array.</summary>
    ///  <param name="Bytes">TBytes [in] Array of bytes to load.</param>
    procedure LoadFromBytes(const Bytes: TBytes);

    ///  <summary>Stores content of XML document in byte array using encoding
    ///  specified in inherited Encoding property.</summary>
    ///  <returns>TBytes - Array of bytes storing XML document.</returns>
    function SaveToBytes: TBytes;
  end;

type
  ///  <summary>
  ///  Implementation of a simple list of XML nodes that can be created and
  ///  manipulated without reference to XML document.
  ///  </summary>
  TXMLSimpleNodeList = class(TInterfacedObject,
    IXMLSimpleNodeList
  )
  strict private
    var
      ///  <summary>List of nodes.</summary>
      fList: TList<IXMLNode>;
  public
    ///  <summary>Object constructor. Creates object with empty list.</summary>
    constructor Create;
    ///  <summary>Object destructor. Tears down object.</summary>
    destructor Destroy; override;

    { IXMLSimpleNodeList methods }
    ///  <summary>Retrieves a node from the list by index.</summary>
    function GetItem(Idx: Integer): IXMLNode;
    ///  <summary>Adds a given node to the list and returns index of new node
    ///  in list.</summary>
    function Add(const Node: IXMLNode): Integer;
    ///  <summary>Returns number of items in list.</summary>
    function Count: Integer;
    ///  <summary>Creates list enumerator.</summary>
    function GetEnumerator: TEnumerator<IXMLNode>;
  end;

type
  ///  <summary>
  ///  Extension of TXMLDocument from XMLDoc unit that implements the methods of
  ///  IXMLDocumentEx.
  ///  </summary>
  TXMLDocumentEx = class(TXMLDocument,
    IXMLDocument, IXMLDocumentEx
  )
  public
    { IXMLDocumentEx methods }

    ///  <summary>Finds a node specified by a path from, and including the
    ///  document's root node.</summary>
    ///  <param name="PathToNode">DOMString [in] Path to node from root node.
    ///  </param>
    ///  <returns>IXMLNode - Reference to required node or nil if node not
    ///  found.</returns>
    ///  <remarks>Node in path are separated by backslash, e.g.
    ///  root\node1\node2</remarks>
    function FindNode(PathToNode: DOMString): IXMLNode;

    ///  <summary>Finds all child nodes of a parent node that are elements and
    ///  have a specified name.</summary>
    ///  <param name="ParentNode">IXMLNode [in] Node containing child nodes to
    ///  be found.</param>
    ///  <param name="NodeName">DOMString [in] Name of required element nodes.
    ///  </param>
    ///  <returns>IXMLSimpleNodeList - List of matching nodes.</returns>
    function FindChildNodes(const ParentNode: IXMLNode;
      const NodeName: DOMString): IXMLSimpleNodeList;
      overload;

    ///  <summary>Finds all child nodes of a parent node that are elements, have
    ///  a specified name and have a specified attribute value.</summary>
    ///  <param name="ParentNode">IXMLNode [in] Node containing child nodes to
    ///  be found.</param>
    ///  <param name="NodeName">DOMString [in] Name of required element nodes.
    ///  </param>
    ///  <param name="AttribName">DOMString [in] Name of required attribute.
    ///  </param>
    ///  <param name="AttribValue">OleVariant [in] Required value of AttribName.
    ///  </param>
    ///  <returns>IXMLSimpleNodeList - List of matching nodes.</returns>
    function FindChildNodes(const ParentNode: IXMLNode;
      const NodeName, AttribName: DOMString;
      const AttribValue: OleVariant): IXMLSimpleNodeList;
      overload;

    ///  <summary>Finds first child node that has a given name.</summary>
    ///  <param name="ParentNode">IXMLNode [in] Node containing child nodes to
    ///  be searched.</param>
    ///  <param name="NodeName">DOMString [in] Name of required element node.
    ///  </param>
    ///  <returns>IXMLNode - Reference to found node or nil if no node found.
    ///  </returns>
    function FindFirstChildNode(const ParentNode: IXMLNode;
      const NodeName: DOMString): IXMLNode;
      overload;

    ///  <summary>Finds first child node that has a given name and attribute
    ///  value.</summary>
    ///  <param name="ParentNode">IXMLNode [in] Node containing child nodes to
    ///  be found.</param>
    ///  <param name="NodeName">DOMString [in] Name of required element node.
    ///  </param>
    ///  <param name="AttribName">DOMString [in] Name of required attribute.
    ///  </param>
    ///  <param name="AttribValue">OleVariant [in] Required value of AttribName.
    ///  </param>
    ///  <returns>IXMLNode - Reference to found node or nil if no node found.
    ///  </returns>
    function FindFirstChildNode(const ParentNode: IXMLNode;
      const NodeName, AttribName: DOMString;
      const AttribValue: OleVariant): IXMLNode;
      overload;

    ///  <summary>Creates a new parented XML node in document.</summary>
    ///  <param name="Parent">IXMLNode [in] Parent node of new node.</param>
    ///  <param name="NodeName">DOMString [in] Name of new node.</param>
    ///  <returns>IXMLNode - Requied new node.</returns>
    function CreateElement(const Parent: IXMLNode;
      const NodeName: DOMString): IXMLNode; overload;

    ///  <summary>Creates a new parented XML text node in document.</summary>
    ///  <param name="Parent">IXMLNode [in] Parent node of new node.</param>
    ///  <param name="NodeName">DOMString [in] Name of new node.</param>
    ///  <param name="Text">DOMString [in] Text stored in new node.</param>
    ///  <returns>IXMLNode - Requied new node.</returns>
    function CreateElement(const Parent: IXMLNode;
      const NodeName, Text: DOMString): IXMLNode; overload;

    ///  <summary>Loads XML document from data stored in byte array.</summary>
    ///  <param name="Bytes">TBytes [in] Array of bytes to load.</param>
    procedure LoadFromBytes(const Bytes: TBytes);

    ///  <summary>Stores content of XML document in byte array using encoding
    ///  specified in inherited Encoding property.</summary>
    ///  <returns>TBytes - Array of bytes storing XML document.</returns>
    function SaveToBytes: TBytes;
  end;


implementation


uses
  // Delphi
  Classes,
  // Project
  UIStringList;


{ TXMLDocumentEx }

function TXMLDocumentEx.FindChildNodes(const ParentNode: IXMLNode;
  const NodeName: DOMString): IXMLSimpleNodeList;
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
begin
  Result := CreateNode(NodeName);
  Parent.ChildNodes.Add(Result);
end;

function TXMLDocumentEx.CreateElement(const Parent: IXMLNode; const NodeName,
  Text: DOMString): IXMLNode;
begin
  Result := CreateElement(Parent, NodeName);
  Result.Text := Text;
end;

function TXMLDocumentEx.FindChildNodes(const ParentNode: IXMLNode;
  const NodeName, AttribName: DOMString;
  const AttribValue: OleVariant): IXMLSimpleNodeList;
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
begin
  Result := ParentNode.ChildNodes.FindNode(NodeName);
end;

function TXMLDocumentEx.FindFirstChildNode(const ParentNode: IXMLNode;
  const NodeName, AttribName: DOMString;
  const AttribValue: OleVariant): IXMLNode;
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

procedure TXMLDocumentEx.LoadFromBytes(const Bytes: TBytes);
var
  Stm: TBytesStream;  // stream used to perform actual loading
begin
  Stm := TBytesStream.Create(Bytes);
  try
    LoadFromStream(Stm);
  finally
    Stm.Free;
  end;
end;

function TXMLDocumentEx.SaveToBytes: TBytes;
var
  Stm: TBytesStream;  // stream to which XML doc writes data
begin
  Stm := TBytesStream.Create;
  try
    SaveToStream(Stm);
    Result := Stm.Bytes;
    // we need to set length because Stm.Bytes may have many trailing 0 bytes
    SetLength(Result, Stm.Size);
  finally
    Stm.Free;
  end;
end;

{ TXMLSimpleNodeList }

function TXMLSimpleNodeList.Add(const Node: IXMLNode): Integer;
begin
  Result := fList.Add(Node);
end;

function TXMLSimpleNodeList.Count: Integer;
begin
  Result := fList.Count;
end;

constructor TXMLSimpleNodeList.Create;
begin
  inherited;
  fList := TList<IXMLNode>.Create;
end;

destructor TXMLSimpleNodeList.Destroy;
begin
  fList.Free;
  inherited;
end;

function TXMLSimpleNodeList.GetEnumerator: TEnumerator<IXMLNode>;
begin
  Result := fList.GetEnumerator;
end;

function TXMLSimpleNodeList.GetItem(Idx: Integer): IXMLNode;
begin
  Result := fList[Idx];
end;

end.


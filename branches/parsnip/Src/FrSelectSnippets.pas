{
 * This Source Code Form is subject to the terms of the Mozilla Public License,
 * v. 2.0. If a copy of the MPL was not distributed with this file, You can
 * obtain one at http://mozilla.org/MPL/2.0/
 *
 * Copyright (C) 2009-2013, Peter Johnson (www.delphidabbler.com).
 *
 * $Rev$
 * $Date$
 *
 * Provides an abstract base class for frames that enable categorised snippets
 * to be selected by means of a tree view displaying check boxes.
}


unit FrSelectSnippets;


interface


uses
  // Delphi
  ImgList,
  Controls,
  Classes,
  ComCtrls,
  // Project
  CS.Database.Types,
  DB.UCategory,
  DB.USnippet,
  FrCheckedTV,
  USnippetsTVDraw;


type

  {
  TSelectSnippetsFrame:
    Displays a two-level tree of snippet categories with associated snippets.
    Each category and snippet has a check box that can be checked to select
    them. A property is exposed that gives access to selected snippets.
  }
  TSelectSnippetsFrame = class(TCheckedTVFrame)
  strict private
    type
      {
      TTVDraw:
        Object used to draw snippets tree view nodes.
      }
      TTVDraw = class(TSnippetsTVDraw)
      strict protected
        function IsUserDefinedNode(const Node: TTreeNode): Boolean; override;
          {Checks if a node represents a user defined snippets object.
            @param Node [in] Node to be checked.
            @return True if node represents user defined object, False if not.
          }
        function IsSectionHeadNode(const Node: TTreeNode): Boolean;
          override;
          {Checks if a node represents a section header.
            @param Node [in] Node to be checked.
            @return True if node is a section header, False if not.
          }
      end;
    var
      ///  <summary>Object that renders tree view nodes.</summary>
      fTVDraw: TTVDraw;
      ///  <summary>IDs of selected snippets.</summary>
      fSelectedSnippets: ISnippetIDList;
    function IsSnippetNode(const Node: TCheckedTreeNode): Boolean;
      {Checks if node represents a snippet.
        @param Node [in] Node to be checked.
        @return True if node represents a snippet.
      }
    function SnippetFromNode(const Node: TCheckedTreeNode): TSnippet;
      {Gets snippet object associated with a node.
        @param Node [in] Node associated with snippet. Must be a valid node.
        @return Reference to snippet.
      }
  strict protected
    procedure AddNodes; override;
      {Adds nodes for each category and the snippets it contains to empty tree
      view.
      }
    procedure RecordChanges; override;
      {Updates tree view's underlying data when state of treeview changes.
      }
    procedure SetLeafNodeState(const Node: TCheckedTreeNode); override;
      {Sets leaf node's state when tree view's underlying data changes. Called
      once for each leaf node.
        @param Node [in] Leaf node whose state to set.
      }
  public
    constructor Create(AOwner: TComponent); override;
      {Class constructor. Sets up object.
        @param AOwner [in] Not used.
      }
    destructor Destroy; override;
      {Class destructor. Tears down object.
      }
    ///  <summary>Clears all selections.</summary>
    procedure Clear;
    ///  <summary>Selects the snippets with the given IDs.</summary>
    ///  <remarks>Any pre-existing selections a cleared.</remarks>
    procedure SelectSnippets(SnippetIDs: ISnippetIDList);
    ///  <summary>Selects the snippet with the given ID.</summary>
    ///  <remarks>Any pre-existing selections a cleared.</remarks>
    procedure SelectSnippet(const SnippetID: TSnippetID);
    ///  <summary>Returns a list of IDs of all the snippets that are currently
    ///  selected.</summary>
    function GetSelection: ISnippetIDList;
    ///  <summary>Checks if any snippets are selected and returns True if so or
    ///  False if not.</summary>
    function HasSelection: Boolean;
  end;


implementation


uses
  // Delphi
  SysUtils,
  StdCtrls,
  // Project
  DB.UMain,
  IntfCommon,
  UGroups,
  USnippetIDs;


{$R *.dfm}


{ TSelectSnippetsBaseFrame }

procedure TSelectSnippetsFrame.AddNodes;
  {Adds nodes for each category and the snippets it contains to empty tree view.
  }
var
  Cat: TCategory;               // reference to a category
  CatNode: TCheckedTreeNode;    // tree node representing a category
  Snippet: TSnippet;            // reference to snippets in a category
  Grouping: TGrouping;          // groups/sorts snippets by category
  Group: TGroupItem;            // group representing a category
begin
  // Create grouping of all snippets by category, with categories alpha sorted
  Grouping := TCategoryGrouping.Create(Database.SelectAll);
  try
    for Group in Grouping do
    begin
      Cat := (Group as TCategoryGroupItem).Category;
      if Group.IsEmpty or Cat.SnippetIDs.IsEmpty then
        Continue;
      CatNode := AddNode(nil, Group.Title, Cat);
      for Snippet in Group.SnippetList do
        AddNode(CatNode, Snippet.Title, Snippet);
    end;
  finally
    Grouping.Free;
  end;
end;

procedure TSelectSnippetsFrame.Clear;
begin
  fSelectedSnippets.Clear;
  DataChanged;
end;

constructor TSelectSnippetsFrame.Create(AOwner: TComponent);
  {Class constructor. Sets up object.
    @param AOwner [in] Not used.
  }
begin
  inherited;
  fSelectedSnippets := TSnippetIDList.Create;
  fTVDraw := TTVDraw.Create;
  tvChecked.OnCustomDrawItem := fTVDraw.CustomDrawItem;
end;

destructor TSelectSnippetsFrame.Destroy;
  {Class destructor. Tears down object.
  }
begin
  fTVDraw.Free;
  inherited;
end;

function TSelectSnippetsFrame.GetSelection: ISnippetIDList;
begin
  Result := (fSelectedSnippets as IClonable).Clone as ISnippetIDList;
end;

function TSelectSnippetsFrame.HasSelection: Boolean;
begin
  Result := not fSelectedSnippets.IsEmpty;
end;

function TSelectSnippetsFrame.IsSnippetNode(
  const Node: TCheckedTreeNode): Boolean;
  {Checks if node represents a snippet.
    @param Node [in] Node to be checked.
    @return True if node represents a snippet.
  }
begin
  Result := Assigned(Node) and Assigned(Node.Data) and
    (TObject(Node.Data) is TSnippet);
end;

procedure TSelectSnippetsFrame.RecordChanges;
  {Updates tree view's underlying data when state of treeview changes.
  }
var
  CatNode: TCheckedTreeNode;     // loops thru all category nodes
  SnippetNode: TCheckedTreeNode; // loops thru all snippet nodes in a category
begin
  fSelectedSnippets.Clear;
  // Loop through all categories
  CatNode := FirstNode;
  while Assigned(CatNode) do
  begin
    Assert(not IsSnippetNode(CatNode),
      ClassName + '.RecordChanges: CatNode is a snippet node');
    // Loop through each snippet in category, updating list if node checked
    SnippetNode := CatNode.GetFirstChild;
    while Assigned(SnippetNode) do
    begin
      Assert(IsSnippetNode(SnippetNode),
        ClassName + 'RecordChanges: SnippetNode is not a snippet node');
      if SnippetNode.IsChecked then
        fSelectedSnippets.Add(SnippetFromNode(SnippetNode).ID);
      SnippetNode := SnippetNode.GetNextSibling;
    end;
    CatNode := CatNode.GetNextSibling;
  end;
end;

procedure TSelectSnippetsFrame.SelectSnippet(const SnippetID: TSnippetID);
begin
  fSelectedSnippets.Clear;
  fSelectedSnippets.Add(SnippetID);
  DataChanged;
end;

procedure TSelectSnippetsFrame.SelectSnippets(SnippetIDs: ISnippetIDList);
begin
  Assert(Assigned(SnippetIDs),
    ClassName + '.SelectSnippets: SnippetIDs is nil');
  (fSelectedSnippets as IAssignable).Assign(SnippetIDs);
  DataChanged;
end;

procedure TSelectSnippetsFrame.SetLeafNodeState(
  const Node: TCheckedTreeNode);
  {Sets leaf node's state when tree view's underlying data changes. Called once
  for each leaf node.
    @param Node [in] Leaf node whose state to set.
  }
begin
  Assert(IsSnippetNode(Node),
    ClassName + '.SetLeafNodeState: Node is not a snippet node');
  // check the snippet node if its snippet is in currently selected snippets
  if fSelectedSnippets.Contains(SnippetFromNode(Node).ID) then
    Node.Check := cbChecked
  else
    Node.Check := cbUnchecked;
end;

function TSelectSnippetsFrame.SnippetFromNode(
  const Node: TCheckedTreeNode): TSnippet;
  {Gets snippet object associated with a node.
    @param Node [in] Node associated with snippet. Must be a valid node.
    @return Reference to snippet.
  }
begin
  Assert(IsSnippetNode(Node),
    ClassName + '.SnippetFromNode: Node is not a snippet node');
  Result := TSnippet(Node.Data);
end;

{ TSelectSnippetsBaseFrame.TTVDraw }

function TSelectSnippetsFrame.TTVDraw.IsSectionHeadNode(
  const Node: TTreeNode): Boolean;
  {Checks if a node represents a section header.
    @param Node [in] Node to be checked.
    @return True if node is a section header, False if not.
  }
begin
  // Header section is a category
  Result := TObject(Node.Data) is TCategory;
end;

function TSelectSnippetsFrame.TTVDraw.IsUserDefinedNode(
  const Node: TTreeNode): Boolean;
  {Checks if a node represents a user defined snippets object.
    @param Node [in] Node to be checked.
    @return True if node represents user defined object, False if not.
  }
var
  SnipObj: TObject; // object referenced in Node.Data
begin
  SnipObj := TObject(Node.Data);
  Result := False;
  if SnipObj is TSnippet then
    Result := True
  else if SnipObj is TCategory then
    Result := (SnipObj as TCategory).UserDefined;
end;

end.


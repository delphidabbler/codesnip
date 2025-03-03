{
 * This Source Code Form is subject to the terms of the Mozilla Public License,
 * v. 2.0. If a copy of the MPL was not distributed with this file, You can
 * obtain one at https://mozilla.org/MPL/2.0/
 *
 * Copyright (C) 2009-2021, Peter Johnson (gravatar.com/delphidabbler).
 *
 * Provides an abstract base class for frames that enable categorised snippets
 * to be selected by means of a tree view displaying check boxes.
}


unit FrSelectSnippetsBase;


interface


uses
  // Delphi
  ImgList,
  Controls,
  Classes,
  ComCtrls,
  // Project
  DB.UCollections,
  DB.UCategory,
  DB.USnippet,
  FrCheckedTV,
  USnippetsTVDraw;


type

  {
  TSelectSnippetsBaseFrame:
    Abstract base class for frames that enable snippets to be selected. Displays
    a two-level tree of snippets categories with associated snippets. Each
    category and snippet has a check box that can be checked to select them. A
    property is exposed that gives access to selected snippets. Subclasses must
    determine which categories and snippets are displayed.
  }
  TSelectSnippetsBaseFrame = class(TCheckedTVFrame)
  strict private
    type
      {
      TTVDraw:
        Object used to draw snippets tree view nodes.
      }
      TTVDraw = class(TSnippetsTVDraw)
      strict protected

        ///  <summary>Gets the collection ID, if any, associated with a  tree
        ///  node.</summary>
        ///  <param name="Node"><c>TTreeNode</c> [in] Node to be checked.
        ///  </param>
        ///  <returns><c>TVaultID</c>. Associated vault ID. If <c>Node</c> has
        ///  no associated vault then a null vault ID is returned.</returns>
        function GetCollectionID(const Node: TTreeNode): TVaultID;
          override;

        function IsSectionHeadNode(const Node: TTreeNode): Boolean;
          override;
          {Checks if a node represents a section header.
            @param Node [in] Node to be checked.
            @return True if node is a section header, False if not.
          }
      end;
    var
      fTVDraw: TTVDraw;                 // Object that renders tree view nodes
      fSelectedSnippets: TSnippetList;  // Value of SelectedSnippets property
    procedure SetSelectedSnippets(const Value: TSnippetList);
      {Write access method for SelectedSnippets property. Updates state of items
      in tree view and triggers OnChange event.
        @param Value [in] New list of snippets. If nil list is cleared.
      }
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
    function CanAddCatNode(const Cat: TCategory): Boolean;
      virtual; abstract;
      {Checks if a category node should be added to treeview.
        @param Cat [in] Category to be checked.
        @return True if category is to be added, False if not.
      }
    function CanAddSnippetNode(const Snippet: TSnippet): Boolean;
      virtual; abstract;
      {Checks if a snippet node should be added to treeview.
        @param Snippet [in] Snippet to be checked.
        @return True if snippet is to be added, False if not.
      }
  public
    constructor Create(AOwner: TComponent); override;
      {Class constructor. Sets up object.
        @param AOwner [in] Not used.
      }
    destructor Destroy; override;
      {Class destructor. Tears down object.
      }
    property SelectedSnippets: TSnippetList
      read fSelectedSnippets write SetSelectedSnippets;
      {List of selected snippets. When set all snippets in list are checked in
      tree view. When user toggles checked state of nodes snippet list is
      updated to include all checked snippets}
  end;


implementation


uses
  // Delphi
  SysUtils,
  StdCtrls,
  // Project
  DB.UMain,
  UGroups;


{$R *.dfm}


{ TSelectSnippetsBaseFrame }

procedure TSelectSnippetsBaseFrame.AddNodes;
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
  Grouping := TCategoryGrouping.Create(Database.Snippets);
  try
    for Group in Grouping do
    begin
      Cat := (Group as TCategoryGroupItem).Category;
      if Group.IsEmpty or not CanAddCatNode(Cat) then
        Continue;
      CatNode := AddNode(nil, Group.Title, Cat);
      for Snippet in Group.SnippetList do
        if CanAddSnippetNode(Snippet) then
          AddNode(CatNode, Snippet.DisplayName, Snippet);
    end;
  finally
    FreeAndNil(Grouping);
  end;
end;

constructor TSelectSnippetsBaseFrame.Create(AOwner: TComponent);
  {Class constructor. Sets up object.
    @param AOwner [in] Not used.
  }
begin
  inherited;
  fSelectedSnippets := TSnippetList.Create;
  fTVDraw := TTVDraw.Create;
  tvChecked.OnCustomDrawItem := fTVDraw.CustomDrawItem;
end;

destructor TSelectSnippetsBaseFrame.Destroy;
  {Class destructor. Tears down object.
  }
begin
  FreeAndNil(fTVDraw);
  FreeAndNil(fSelectedSnippets);
  inherited;
end;

function TSelectSnippetsBaseFrame.IsSnippetNode(
  const Node: TCheckedTreeNode): Boolean;
  {Checks if node represents a snippet.
    @param Node [in] Node to be checked.
    @return True if node represents a snippet.
  }
begin
  Result := Assigned(Node) and Assigned(Node.Data) and
    (TObject(Node.Data) is TSnippet);
end;

procedure TSelectSnippetsBaseFrame.RecordChanges;
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
        fSelectedSnippets.Add(SnippetFromNode(SnippetNode));
      SnippetNode := SnippetNode.GetNextSibling;
    end;
    CatNode := CatNode.GetNextSibling;
  end;
end;

procedure TSelectSnippetsBaseFrame.SetLeafNodeState(
  const Node: TCheckedTreeNode);
  {Sets leaf node's state when tree view's underlying data changes. Called once
  for each leaf node.
    @param Node [in] Leaf node whose state to set.
  }
begin
  Assert(IsSnippetNode(Node),
    ClassName + '.SetLeafNodeState: Node is not a snippet node');
  // check the snippet node if its snippet is in currently selected snippets
  if fSelectedSnippets.Contains(SnippetFromNode(Node)) then
    Node.Check := cbChecked
  else
    Node.Check := cbUnchecked;
end;

procedure TSelectSnippetsBaseFrame.SetSelectedSnippets(
  const Value: TSnippetList);
  {Write access method for SelectedSnippets property. Updates state of items in
  tree view and triggers OnChange event.
    @param Value [in] New list of snippets. If nil list is cleared.
  }
begin
  if Assigned(Value) then
    fSelectedSnippets.Assign(Value)
  else
    fSelectedSnippets.Clear;
  // Refresh tree view state now ata has changed
  DataChanged;
end;

function TSelectSnippetsBaseFrame.SnippetFromNode(
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

function TSelectSnippetsBaseFrame.TTVDraw.GetCollectionID(
  const Node: TTreeNode): TVaultID;
var
  SnipObj: TObject; // object referenced in Node.Data
begin
  SnipObj := TObject(Node.Data);
  if SnipObj is TSnippet then
    Result := (SnipObj as TSnippet).CollectionID
  else
    Result := TVaultID.CreateNull
end;

function TSelectSnippetsBaseFrame.TTVDraw.IsSectionHeadNode(
  const Node: TTreeNode): Boolean;
  {Checks if a node represents a section header.
    @param Node [in] Node to be checked.
    @return True if node is a section header, False if not.
  }
begin
  // Header section is a category
  Result := TObject(Node.Data) is TCategory;
end;

end.


{
 * FrSelectSnippets.pas
 *
 * Implements a frame that enables one or more routines in the snippets database
 * to be selected via a tree view. User defined items are highlighted.
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
 * The Original Code is FrSelectSnippets.pas
 *
 * The Initial Developer of the Original Code is Peter Johnson
 * (http://www.delphidabbler.com/).
 *
 * Portions created by the Initial Developer are Copyright (C) 2006-2009 Peter
 * Johnson. All Rights Reserved.
 *
 * Contributor(s)
 *   NONE
 *
 * ***** END LICENSE BLOCK *****
}


unit FrSelectSnippets;

{$WARN UNSAFE_CAST OFF}

interface


uses
  // Delphi
  ImgList, Controls, Classes, ComCtrls,
  // Project
  FrCheckedTV, USnippets, USnippetsTVDraw;


type

  {
  TSelectSnippetsFrame:
    Implements a frame that enables one or more routines in the snippets
    database to be selected. It displays a two-level tree of snippets categories
    with their associated routines. Each category and routine has a check box
    that can be checked to select them. A property is exposed that gives access
    to selected routines. User defined routines and cateogries are highlighted.
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
      fTVDraw: TTVDraw;                 // object that renders tree view nodes
      fSelectedRoutines: TRoutineList;  // value of SelectedRoutines property
    procedure SetSelectedRoutines(const Value: TRoutineList);
      {Write access method for SelectedRoutines property. Updates state of items
      in tree view and triggers OnChange event.
        @param Value [in] New list of routines. If nil list is cleared.
      }
    function IsRoutineNode(const Node: TCheckedTreeNode): Boolean;
      {Checks if node represents a routine.
        @param Node [in] Node to be checked.
        @return True if node represents a routine.
      }
    function RoutineFromNode(const Node: TCheckedTreeNode): TRoutine;
      {Gets routine object associated with a node.
        @param Node [in] Node associated with routine. Must be a valid node.
        @return Reference to routine.
      }
  strict protected
    procedure AddNodes; override;
      {Adds nodes for each category and the routines it contains to empty tree
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
    property SelectedRoutines: TRoutineList
      read fSelectedRoutines write SetSelectedRoutines;
      {List of selected routines. When set all routines in list are checked in
      tree view. When user toggles checked state of nodes routine list is
      updated to include all checked routines}
  end;


implementation


uses
  // Delphi
  SysUtils, StdCtrls, Graphics,
  // Project
  UColours;


{$R *.dfm}


{ TSelectSnippetsFrame }

procedure TSelectSnippetsFrame.AddNodes;
  {Adds nodes for each category and the routines it contains to empty tree view.
  }
var
  Cat: TCategory;               // reference to a category
  CatNode: TCheckedTreeNode;    // tree node representing a category
  Routine: TRoutine;            // reference to routines in a category
begin
  // Add each category as top level node
  for Cat in Snippets.Categories do
  begin
    // Don't add category node if has no routines
    if Cat.Routines.Count = 0 then
      Continue;
    CatNode := AddNode(nil, Cat.Description, Cat);
    // Add each routine in category as children of category node
    for Routine in Cat.Routines do
      AddNode(CatNode, Routine.Name, Routine);
  end;
end;

constructor TSelectSnippetsFrame.Create(AOwner: TComponent);
  {Class constructor. Sets up object.
    @param AOwner [in] Not used.
  }
begin
  inherited;
  fSelectedRoutines := TRoutineList.Create;
  fTVDraw := TTVDraw.Create;
  tvChecked.OnCustomDrawItem := fTVDraw.CustomDrawItem;
end;

destructor TSelectSnippetsFrame.Destroy;
  {Class destructor. Tears down object.
  }
begin
  FreeAndNil(fTVDraw);
  FreeAndNil(fSelectedRoutines);
  inherited;
end;

function TSelectSnippetsFrame.IsRoutineNode(
  const Node: TCheckedTreeNode): Boolean;
  {Checks if node represents a routine.
    @param Node [in] Node to be checked.
    @return True if node represents a routine.
  }
begin
  Result := Assigned(Node) and Assigned(Node.Data) and
    (TObject(Node.Data) is TRoutine);
end;

procedure TSelectSnippetsFrame.RecordChanges;
  {Updates tree view's underlying data when state of treeview changes.
  }
var
  CatNode: TCheckedTreeNode;     // loops thru all category nodes
  RoutineNode: TCheckedTreeNode; // loops thru all routine nodes in a category
begin
  fSelectedRoutines.Clear;
  // Loop through all categories
  CatNode := FirstNode;
  while Assigned(CatNode) do
  begin
    Assert(not IsRoutineNode(CatNode),
      ClassName + '.RecordChanges: CatNode is a routine node');
    // Loop through each routine in category, updating list if node checked
    RoutineNode := CatNode.GetFirstChild;
    while Assigned(RoutineNode) do
    begin
      Assert(IsRoutineNode(RoutineNode),
        ClassName + 'RecordChanges: RoutineNode is not a routine node');
      if RoutineNode.IsChecked then
        fSelectedRoutines.Add(RoutineFromNode(RoutineNode));
      RoutineNode := RoutineNode.GetNextSibling;
    end;
    CatNode := CatNode.GetNextSibling;
  end;
end;

function TSelectSnippetsFrame.RoutineFromNode(
  const Node: TCheckedTreeNode): TRoutine;
  {Gets routine object associated with a node.
    @param Node [in] Node associated with routine. Must be a valid node.
    @return Reference to routine.
  }
begin
  Assert(IsRoutineNode(Node),
    ClassName + '.RoutineFromNode: Node is not a routine node');
  Result := TRoutine(Node.Data);
end;

procedure TSelectSnippetsFrame.SetLeafNodeState(
  const Node: TCheckedTreeNode);
  {Sets leaf node's state when tree view's underlying data changes. Called once
  for each leaf node.
    @param Node [in] Leaf node whose state to set.
  }
begin
  Assert(IsRoutineNode(Node),
    ClassName + '.RoutineFromNode: Node is not a routine node');
  // check the routine node if its routine is in currently selected routines
  if fSelectedRoutines.Contains(RoutineFromNode(Node)) then
    Node.Check := cbChecked
  else
    Node.Check := cbUnchecked;
end;

procedure TSelectSnippetsFrame.SetSelectedRoutines(
  const Value: TRoutineList);
  {Write access method for SelectedRoutines property. Updates state of items in
  tree view and triggers OnChange event.
    @param Value [in] New list of routines. If nil list is cleared.
  }
begin
  if Assigned(Value) then
    fSelectedRoutines.Assign(Value)
  else
    fSelectedRoutines.Clear;
  // Refresh tree view state now that data ha changed
  DataChanged;
end;

{ TSelectSnippetsFrame.TTVDraw }

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
  if SnipObj is TRoutine then
    Result := (SnipObj as TRoutine).UserDefined
  else if SnipObj is TCategory then
    Result := (SnipObj as TCategory).UserDefined;
end;

end.


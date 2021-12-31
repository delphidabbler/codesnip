{
 * This Source Code Form is subject to the terms of the Mozilla Public License,
 * v. 2.0. If a copy of the MPL was not distributed with this file, You can
 * obtain one at https://mozilla.org/MPL/2.0/
 *
 * Copyright (C) 2006-2021, Peter Johnson (gravatar.com/delphidabbler).
 *
 * Implements a frame containing a treeview that has check boxes associated with
 * tree nodes and updates parent check boxes according to state of child nodes.
}


unit FrCheckedTV;


interface


uses
  // Delphi
  ImgList, Controls, Classes, ComCtrls, StdCtrls, Forms,
  // Project
  UKeysHelper, UTVCheckBoxes;


type

  {
  TCheckedTreeNode:
    Custom tree node class that has an associated check box that can appear
    checked, unchecked and greyed in either hot or normal state.
  }
  TCheckedTreeNode = class(TTreeNode)
  strict private
    fHot: Boolean;  // Value of Hot property
    procedure SetImage(const ImgIdx: Integer);
      {Sets node's image. ImageIndex and SelectedImageIndex properties are set
      to same value.
        @param ImgIdx [in] Required image index.
      }
    function GetCheck: TCheckBoxState;
      {Read accessor for Check property. Calculates value from ImageIndex.
        @return Current property value.
      }
    procedure SetCheck(const Value: TCheckBoxState);
      {Write accessor for Check property. Updates node's image.
        @param Value [in] New property value.
      }
    procedure SetHot(const Value: Boolean);
      {Write accessor for Hot property. Updates node's image.
        @param Value [in] New value of property.
      }
    function GetParent: TCheckedTreeNode;
      {Read accessor for Parent property.
        @return Value of Parent property.
      }
    function GetTopLevelParent: TCheckedTreeNode;
      {Read accessor for TopLevelParent property.
        @return Reference to top level parent node or Self if node has no parent.
      }
  public
    function IsChecked: Boolean;
      {Checks if node is checked.
        @return True if node is checked.
      }
    function IsUnchecked: Boolean;
      {Checks if node is unchecked.
        @return True if node is unchecked.
      }
    function HasParent: Boolean;
      {Checks if node has a parent node.
        @return True if node has parent.
      }
    function GetNextSibling: TCheckedTreeNode;
      {Gets reference to node's next sibling.
        @return Reference to next sibling or nil if no more siblings.
      }
    function GetFirstChild: TCheckedTreeNode;
      {Gets reference to a node's first child node.
        @return Reference to child node or nil if no child nodes.
      }
    procedure ToggleCheck;
      {Toggles node's check box off if currently checked or greyed or on if
      currently unchecked.
      }
    property Parent: TCheckedTreeNode read GetParent;
      {Node's parent or nil if node has no parent. This is a re-implementation
      of inherited property}
    property TopLevelParent: TCheckedTreeNode read GetTopLevelParent;
      {Top level parent of node. Self if node is a top level node}
    property Check: TCheckBoxState read GetCheck write SetCheck;
      {State of node's associated checkbox image}
    property Hot: Boolean read fHot write SetHot;
      {Whether node's associated checkbox is "hot" or not}
  end;

  {
  TCheckedTVFrame:
    Abstract frame base class containing a treeview that has check boxes
    associated with tree nodes and updates parent check boxes according to state
    of child nodes.
  }
  TCheckedTVFrame = class(TFrame)
    ilChecks: TImageList;
    tvChecked: TTreeView;
    procedure FrameEnter(Sender: TObject);
    procedure tvCheckedCollapsing(Sender: TObject; Node: TTreeNode;
      var AllowCollapse: Boolean);
    procedure tvCheckedKeyPress(Sender: TObject; var Key: Char);
    procedure tvCheckedMouseDown(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer);
    procedure tvCheckedMouseMove(Sender: TObject; Shift: TShiftState; X,
      Y: Integer);
    procedure tvCheckedCreateNodeClass(Sender: TCustomTreeView;
      var NodeClass: TTreeNodeClass);
  strict private
    fOnChange: TNotifyEvent;    // Event handler for OnChange event
    fCanCollapse: Boolean;      // Value of CanCollapse property
    fTVChecks: TTVCheckBoxes;   // Object that manages check box image lists
    fHotNode: TCheckedTreeNode; // Tree node currently under mouse cursor
    procedure SetCanCollapse(Flag: Boolean);
      {Write access method for CanCollapse property.
        @param Flag [in] New property value.
      }
    procedure MakeHotNode(const Node: TCheckedTreeNode);
      {Makes a node's check box appear "hot". Sets any previous hot check box
      back to normal state.
        @param Node [in] Node whose check box to appear hot.
      }
    procedure SetParentStatesFromChildren(Node: TCheckedTreeNode);
      {Sets state of a node parent, grandparent etc nodes according to state of
      node and its siblings. Parent nodes are checked if all children are
      checked, unchecked if all children unchecked and greyed if children have
      mixed states.
        @param Node [in] Node whose parent states are to be updated.
      }
    procedure SetParentStates;
      {Sets state of all parent nodes according to state of their child nodes.
      Parent nodes are checked if all children are checked, unchecked if all
      children unchecked and greyed if children have mixed states.
      }
    procedure CopyParentStateToChildren(ParentNode: TCheckedTreeNode);
      {Copies state of check box of parent node to all its child nodes and
      children's child nodes.
        @param ParentNode [in] Parent node whose state is to be copied.
      }
    procedure PopulateTreeView;
      {Populates treeview with required nodes.
      }
    procedure ToggleNode(const Node: TCheckedTreeNode);
      {Toggles checked state of a node and updates parent or child nodes
      accordingly. Changed are recorded and OnChange event triggered.
        @param Node [in] Node whose check state is to be toggled.
      }
    procedure CheckBoxChangeHandler(Sender: TObject);
      {Handles TTVCheckBoxes.OnChange event triggered when appearance of check
      boxes changes. Refreshes tree view.
        @param Sender [in] Not used.
      }
  strict protected
    procedure AddNodes; virtual; abstract;
      {Called when tree view is to be populated with nodes.
      }
    procedure SetLeafNodeState(const Node: TCheckedTreeNode); virtual; abstract;
      {Called to get checked state of a leaf node when tree view's underlying
      data changes. Implementor should set node's check state.
        @param Node [in] Node to have check state set.
      }
    procedure RecordChanges; virtual; abstract;
      {Called when state of treeview changes to enable tree view's underlying
      data to be changed.
      }
    function AddNode(const Parent: TCheckedTreeNode; const Text: string;
      const Data: Pointer): TCheckedTreeNode;
      {Adds new unchecked tree node. This method can be called from descendant
      classes to add new nodes to tree from their implementation of the AddNodes
      method.
        @param Parent [in] Parent of new node or nil if new node is top level.
        @param Text [in] Text to display in new node.
        @param Data [in] User defined data assoicated with new node.
        @return Reference to new node.
      }
    function FirstNode: TCheckedTreeNode;
      {Gets reference to first top level node in tree view.
        @return Node reference.
      }
    procedure UpdateTreeViewState;
      {Updates check boxes of each node in treeview. Child nodes state is
      requested and parent node are set according to state of child nodes.
      }
    procedure DataChanged;
      {Notifies that state of tree view's underlying data has changed. Leaf node
      state is requested and parent node states are set accroding to state of
      child nodes.
      }
    procedure DoChanged;
      {Triggers OnChange event.
      }
  public
    constructor Create(AOwner: TComponent); override;
      {Class constructor. Sets up object and populates tree view.
        @param AOwner [in] Component that owns this frame.
      }
    destructor Destroy; override;
      {Class destructor. Tears down object.
      }
    procedure ExpandTree;
    procedure CollapseTree;
    property CanCollapse: Boolean
      read fCanCollapse write SetCanCollapse default False;
      {Determines whether tree nodes can be collapsed. Also switches on buttons
      for user to use to make nodes expand and collapse}
    property OnChange: TNotifyEvent
      read fOnChange write fOnChange;
      {Event triggered when state of a tree node changes}
  end;


implementation


uses
  // Delphi
  SysUtils;


{$R *.dfm}


{ TCheckedTVFrame }

function TCheckedTVFrame.AddNode(const Parent: TCheckedTreeNode;
  const Text: string; const Data: Pointer): TCheckedTreeNode;
  {Adds new unchecked tree node. This method can be called from descendant
  classes to add new nodes to tree from their implementation of the AddNodes
  method.
    @param Parent [in] Parent of new node or nil if new node is top level.
    @param Text [in] Text to display in new node.
    @param Data [in] User defined data assoicated with new node.
    @return Reference to new node.
  }
begin
  Result := tvChecked.Items.AddChild(Parent, Text) as TCheckedTreeNode;
  Result.Data := Data;
  Result.Check := cbUnchecked;
end;

procedure TCheckedTVFrame.CheckBoxChangeHandler(Sender: TObject);
  {Handles TTVCheckBoxes.OnChange event triggered when appearance of check boxes
  changes. Refreshes tree view.
    @param Sender [in] Not used.
  }
begin
  tvChecked.Refresh;
end;

procedure TCheckedTVFrame.CollapseTree;
begin
  if not fCanCollapse then
    Exit;
  tvChecked.Items.BeginUpdate;
  try
    tvChecked.FullCollapse;
    if Assigned(tvChecked.Selected) then
      tvChecked.Selected.MakeVisible;
  finally
    tvChecked.Items.EndUpdate;
  end;
end;

procedure TCheckedTVFrame.CopyParentStateToChildren(
  ParentNode: TCheckedTreeNode);
  {Copies state of check box of parent node to all its child nodes and
  children's child nodes.
    @param ParentNode [in] Parent node whose state is to be copied.
  }
var
  ChildNode: TCheckedTreeNode; // reference to each of parent's child nodes
begin
  ChildNode := ParentNode.GetFirstChild;
  while Assigned(ChildNode) do
  begin
    ChildNode.Check := ParentNode.Check;
    CopyParentStateToChildren(ChildNode);
    ChildNode := ChildNode.GetNextSibling;
  end;
end;

constructor TCheckedTVFrame.Create(AOwner: TComponent);
  {Class constructor. Sets up object and populates tree view.
    @param AOwner [in] Component that owns this frame.
  }
begin
  inherited;
  fTVChecks := TTVCheckBoxes.Create(ilChecks);
  fTVChecks.OnChange := CheckBoxChangeHandler;
  PopulateTreeView;
end;

procedure TCheckedTVFrame.DataChanged;
  {Notifies that state of tree view's underlying data has changed. Leaf node
  state is requested and parent node states are set accroding to state of child
  nodes.
  }

  // ---------------------------------------------------------------------------
  procedure SetLeafNodeStates(const Node: TCheckedTreeNode);
    {Recursively sets state of leaf nodes.
      @param Node [in] Node to process. If Node is leaf node its state is
        requested. If Node is a parent we set state of its child nodes
        recursively.
    }
  var
    ChildNode: TCheckedTreeNode;  // reference to each child of node
  begin
    if Node.HasChildren then
    begin
      ChildNode := Node.GetFirstChild;
      while Assigned(ChildNode) do
      begin
        SetLeafNodeStates(ChildNode);
        ChildNode := ChildNode.GetNextSibling;
      end;
    end
    else
      SetLeafNodeState(Node);
  end;
  // ---------------------------------------------------------------------------

var
  Node: TCheckedTreeNode; // reference to each top level node
begin
  // Loop through all top level nodes setting their leaf node states
  Node := FirstNode;
  while Assigned(Node) do
  begin
    SetLeafNodeStates(Node);
    Node := Node.GetNextSibling;
  end;
  // Set state of parent nodes based on state of leaf node
  SetParentStates;
  // Notify that tree view has (may have) changed
  DoChanged;
end;

destructor TCheckedTVFrame.Destroy;
  {Class destructor. Tears down object.
  }
begin
  FreeAndNil(fTVChecks);
  inherited;
end;

procedure TCheckedTVFrame.DoChanged;
  {Triggers OnChange event.
  }
begin
  if Assigned(fOnChange) then
    fOnChange(Self);
end;

procedure TCheckedTVFrame.ExpandTree;
begin
  if not fCanCollapse then
    Exit;
  tvChecked.Items.BeginUpdate;
  try
    tvChecked.FullExpand;
    if Assigned(tvChecked.Selected) then
      tvChecked.Selected.MakeVisible;
  finally
    tvChecked.Items.EndUpdate;
  end;
end;

function TCheckedTVFrame.FirstNode: TCheckedTreeNode;
  {Gets reference to first top level node in tree view.
    @return Node reference.
  }
begin
  Result := tvChecked.Items.GetFirstNode as TCheckedTreeNode;
end;

procedure TCheckedTVFrame.FrameEnter(Sender: TObject);
  {Handles event triggered when frame is entered. Moves focus to contained tree
  view.
    @param Sender [in] Not used.
  }
begin
  if Assigned(tvChecked) then
    tvChecked.SetFocus;
end;

procedure TCheckedTVFrame.MakeHotNode(const Node: TCheckedTreeNode);
  {Makes a node's check box appear "hot". Sets any previous hot check box back
  to normal state.
    @param Node [in] Node whose check box to appear hot.
  }
begin
  if Node <> fHotNode then
  begin
    if Assigned(fHotNode) then
      fHotNode.Hot := False;
    if Assigned(Node) then
      Node.Hot := True;
    fHotNode := Node;
  end;
end;

procedure TCheckedTVFrame.PopulateTreeView;
  {Populates treeview with required nodes.
  }
begin
  // Get descendant to add nodes to tree view
  AddNodes;
  // Expand treeview and display top of tree
  tvChecked.FullExpand;
  if FirstNode <> nil then
    FirstNode.MakeVisible;
end;

procedure TCheckedTVFrame.SetCanCollapse(Flag: Boolean);
begin
  if fCanCollapse = Flag then
    Exit;
  tvChecked.ShowRoot := Flag;
  tvChecked.ShowButtons := Flag;
  fCanCollapse := Flag;
end;

procedure TCheckedTVFrame.SetParentStates;
  {Sets state of all parent nodes according to state of their child nodes.
  Parent nodes are checked if all children are checked, unchecked if all
  children unchecked and greyed if children have mixed states.
  }
var
  Node: TCheckedTreeNode; // references each of top level tree nodes
begin
  Node := FirstNode;
  while Assigned(Node) do
  begin
    SetParentStatesFromChildren(Node);
    Node := Node.GetNextSibling;
  end;
end;

procedure TCheckedTVFrame.SetParentStatesFromChildren(
  Node: TCheckedTreeNode);
  {Sets state of a node parent, grandparent etc nodes according to state of node
  and its siblings. Parent nodes are checked if all children are checked,
  unchecked if all children unchecked and greyed if children have mixed states.
    @param Node [in] Node whose parent states are to be updated.
  }
var
  ChildNode: TCheckedTreeNode;    // references child nodes of node
  CheckedChildCount: Integer;     // count of checked child nodes
  UnCheckedChildCount: Integer;   // count of unchecked child nodes
begin
  // If child node do nothing
  if not Node.HasChildren then
    Exit;
  // We have parent node
  // count nodes checked and unchecked children (some may be grey)
  CheckedChildCount := 0;
  UnCheckedChildCount := 0;
  ChildNode := Node.GetFirstChild;
  while Assigned(ChildNode) do
  begin
    if ChildNode.HasChildren then
      // child is itself a parent: update its check state recursively
      SetParentStatesFromChildren(ChildNode);
    if ChildNode.IsChecked then
      Inc(CheckedChildCount)
    else if ChildNode.IsUnchecked then
      Inc(UncheckedChildCount);
    ChildNode := ChildNode.GetNextSibling;
  end;
  // Set state of node according to checked / unchecked count
  if UncheckedChildCount = Node.Count then
    Node.Check := cbUnchecked
  else if CheckedChildCount = Node.Count then
    Node.Check := cbChecked
  else
    Node.Check := cbGrayed;
end;

procedure TCheckedTVFrame.ToggleNode(const Node: TCheckedTreeNode);
  {Toggles checked state of a node and updates parent or child nodes
  accordingly. Changed are recorded and OnChange event triggered.
    @param Node [in] Node whose check state is to be toggled.
  }
begin
  Assert(Assigned(Node), ClassName + '.ToggleNode: Node is nil');
  Node.ToggleCheck;
  if Node.HasChildren then
    CopyParentStateToChildren(Node);
  if Node.HasParent then
    SetParentStatesFromChildren(Node.TopLevelParent);
  // Refresh tree view display to display revised glyphs
  tvChecked.Refresh;
  // Update data from selections
  RecordChanges;
  // Trigger OnChange event
  DoChanged;
end;

procedure TCheckedTVFrame.tvCheckedCollapsing(Sender: TObject;
  Node: TTreeNode; var AllowCollapse: Boolean);
  {Handler for tree view's OnCollapsing event. Prevents tree from being
  collapsed.
    @param Sender [in] Not used.
    @param Node [in] Not used.
    @param AllowCollapse [in/out] Whether collapsing allowed. Set to false.
  }
begin
  AllowCollapse := fCanCollapse;
end;

procedure TCheckedTVFrame.tvCheckedCreateNodeClass(
  Sender: TCustomTreeView; var NodeClass: TTreeNodeClass);
  {Handler for tree view's OnCreateNodeClass event. Passes back custom tree node
  class to use when creating tree nodes.
    @param Sender [in] Not used.
    @param NodeClass [in/out] Set to required tree node class type.
  }
begin
  NodeClass := TCheckedTreeNode;
end;

procedure TCheckedTVFrame.tvCheckedKeyPress(Sender: TObject;
  var Key: Char);
  {Handles key presses on tree view. If space is pressed check state of selected
  node is toggled. All other keypressed are inhibited to prevent initial
  characters from changing selection.
    @param Sender [in] Not used.
    @param Key [in/out] Stores pressed key when called. Set to #0 to inhibit
      further processing.
  }
begin
  if (Key = ' ') and Assigned(tvChecked.Selected) then
    ToggleNode(tvChecked.Selected as TCheckedTreeNode);
  Key := #0;  // inhibits selection of entries when intial letter pressed
end;

procedure TCheckedTVFrame.tvCheckedMouseDown(Sender: TObject;
  Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
  {Handles mouse clicks on treeview. Unmodified mouse down events are used to
  toggle check state of tree nodes. Clicks outside nodes are ignored.
    @param Sender [in] Not used.
    @param Button [in] Mouse button pressed. We only accept left button.
    @param Shift [in] Shift and button state. We only accept unmodified clicks.
    @param X [in] X co-ordinate of mouse click.
    @param Y [in] Y co-ordinate of mouse click.
  }
var
  SelNode: TCheckedTreeNode;  // reference to currently selected tree node
  HitTestInfo: THitTests;     // info about where in treeview mouse pressed
begin
  SelNode := tvChecked.GetNodeAt(X, Y) as TCheckedTreeNode;
  HitTestInfo := tvChecked.GetHitTestInfoAt(X, Y);
  if (htOnIcon in HitTestInfo) and (Button = mbLeft)
    and not HasShiftKeys(Shift) and Assigned(SelNode) then
    ToggleNode(SelNode);
end;

procedure TCheckedTVFrame.tvCheckedMouseMove(Sender: TObject;
  Shift: TShiftState; X, Y: Integer);
  {Handler for tree view's OnMouseMove event. Sets check box of any node under
  the mouse to hot state.
    @param Sender [in] Not used.
    @param Shift [in] Not used.
    @param X [in] X coordinate of mouse.
    @param Y [in] Y coordinate of mouse.
  }
var
  SelNode: TCheckedTreeNode;  // reference to currently selected tree node
  HitTestInfo: THitTests;     // info about where in treeview mouse pressed
begin
  HitTestInfo := tvChecked.GetHitTestInfoAt(X, Y);
  if (htOnItem in HitTestInfo) then
    SelNode := tvChecked.GetNodeAt(X, Y) as TCheckedTreeNode
  else
    SelNode := nil;
  MakeHotNode(SelNode);
end;

procedure TCheckedTVFrame.UpdateTreeViewState;
  {Updates check boxes of each node in treeview. Child nodes state is requested
  and parent node are set according to state of child nodes. Should be called
  when state of data represented in tree view changes.
  }

  procedure SetLeafNodeStates(const Node: TCheckedTreeNode);
    {Recursively sets state of leaf nodes.
      @param Node [in] Node to process. If Node is leaf node its state is
        requested. If Node is a parent we set state of its child nodes
        recursively.
    }
  var
    ChildNode: TCheckedTreeNode;  // reference to each child of node
  begin
    if Node.HasChildren then
    begin
      ChildNode := Node.GetFirstChild;
      while Assigned(ChildNode) do
      begin
        SetLeafNodeStates(ChildNode);
        ChildNode := ChildNode.GetNextSibling;
      end;
    end
    else
      SetLeafNodeState(Node);
  end;

var
  Node: TCheckedTreeNode; // reference to each top level node
begin
  // Loop through all top level nodes setting their leaf node states
  Node := FirstNode;
  while Assigned(Node) do
  begin
    SetLeafNodeStates(Node);
    Node := Node.GetNextSibling as TCheckedTreeNode;
  end;
  // Set state of parent nodes based on state of leaf node
  SetParentStates;
  // Notify that tree view has (may have) changed
  DoChanged;
end;

{ TCheckedTreeNode }

function TCheckedTreeNode.GetCheck: TCheckBoxState;
  {Read accessor for Check property. Calculates value from ImageIndex.
    @return Current property value.
  }
begin
  Result := TTVCheckBoxes.ImageIdxToCheckState(Self.ImageIndex);
end;

function TCheckedTreeNode.GetFirstChild: TCheckedTreeNode;
  {Gets reference to a node's first child node.
    @return Reference to child node or nil if no child nodes.
  }
begin
  // This method simply casts first child from TTreeNode to TCheckedTreeNode
  Result := inherited GetFirstChild as TCheckedTreeNode;
end;

function TCheckedTreeNode.GetNextSibling: TCheckedTreeNode;
  {Gets reference to node's next sibling.
    @return Reference to next sibling or nil if no more siblings.
  }
begin
  // This method simply casts next sibling from TTreeNode to TCheckedTreeNode
  Result := inherited GetNextSibling as TCheckedTreeNode;
end;

function TCheckedTreeNode.GetParent: TCheckedTreeNode;
  {Read accessor for Parent property.
    @return Value of parent property.
  }
begin
  // This method simply converts Parent from TTreeNode to TCheckedTreeNode
  Result := inherited Parent as TCheckedTreeNode;
end;

function TCheckedTreeNode.GetTopLevelParent: TCheckedTreeNode;
  {Read accessor for TopLevelParent property.
    @return Reference to top level parent node or Self if node has no parent.
  }
begin
  if not HasParent then
    Result := Self
  else
    Result := Parent.TopLevelParent;
end;

function TCheckedTreeNode.HasParent: Boolean;
  {Checks if node has a parent node.
    @return True if node has parent.
  }
begin
  Result := Assigned(Parent);
end;

function TCheckedTreeNode.IsChecked: Boolean;
  {Checks if node is checked.
    @return True if node is checked.
  }
begin
  Result := Check = cbChecked;
end;

function TCheckedTreeNode.IsUnchecked: Boolean;
  {Checks if node is unchecked.
    @return True if node is unchecked.
  }
begin
  Result := Check = cbUnchecked;
end;

procedure TCheckedTreeNode.SetCheck(const Value: TCheckBoxState);
  {Write accessor for Check property. Updates node's image.
    @param Value [in] New property value.
  }
begin
  SetImage(TTVCheckBoxes.CheckImageIdx(Value, Hot));
end;

procedure TCheckedTreeNode.SetHot(const Value: Boolean);
  {Write accessor for Hot property. Updates node's image.
    @param Value [in] New value of property.
  }
begin
  if fHot <> Value then
  begin
    fHot := Value;
    SetImage(TTVCheckBoxes.CheckImageIdx(GetCheck, Value));
  end;
end;

procedure TCheckedTreeNode.SetImage(const ImgIdx: Integer);
  {Sets node's image. ImageIndex and SelectedImageIndex properties are set to
  same value.
    @param ImgIdx [in] Required image index.
  }
begin
  ImageIndex := ImgIdx;
  SelectedIndex := ImgIdx;
end;

procedure TCheckedTreeNode.ToggleCheck;
  {Toggles node's check box off if currently checked or greyed or on if
  currently unchecked.
  }
begin
  case Check of
    cbChecked, cbGrayed: Check := cbUnchecked;
    cbUnchecked: Check := cbChecked;
  end;
end;

end.


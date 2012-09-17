{
 * FrOverview.pas
 *
 * Titled frame that displays lists of snippets, arranged in different ways and
 * manages user interaction with the displayed items.
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
 * The Original Code is FrOverview.pas
 *
 * The Initial Developer of the Original Code is Peter Johnson
 * (http://www.delphidabbler.com/).
 *
 * Portions created by the Initial Developer are Copyright (C) 2005-2012 Peter
 * Johnson. All Rights Reserved.
 *
 * Contributor(s)
 *   NONE
 *
 * ***** END LICENSE BLOCK *****
}


unit FrOverview;


interface


uses
  // Delphi
  ComCtrls, Controls, Classes, Windows, ExtCtrls, StdCtrls, ToolWin, Menus,
  // Project
  FrTitled, IntfFrameMgrs, IntfNotifier, UCommandBars, UOverviewTreeState,
  USnippets, USnippetsTVDraw, UView, UViewItemTreeNode;


type

  {
  TOverviewFrame:
    Titled frame that displays lists of snippets grouped in various ways and
    manages user interaction with the displayed items. The frame implements
    display manager interfaces, command bar interfaces and notifies application
    of user-initiated events via a notifier object.
  }
  TOverviewFrame = class(TTitledFrame,
    ITabbedDisplayMgr,                    // uses tabs to select different views
    IOverviewDisplayMgr,                   // for loading and selecting snippets
    IPaneInfo,                                // provides information about pane
    ISetNotifier,                                        // sets notifier object
    ICommandBarConfig                         // configuration of "command bars"
  )
    tcDisplayStyle: TTabControl;
    tvSnippets: TTreeView;
    tbarOverview: TToolBar;
    mnuOverview: TPopupMenu;
    procedure tcDisplayStyleChange(Sender: TObject);
    procedure tcDisplayStyleChanging(Sender: TObject; var AllowChange: Boolean);
    procedure tcDisplayStyleMouseDown(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer);
    procedure tvSnippetsChanging(Sender: TObject; Node: TTreeNode;
      var AllowChange: Boolean);
    procedure tvSnippetsCreateNodeClass(Sender: TCustomTreeView;
      var NodeClass: TTreeNodeClass);
    procedure tvSnippetsDeletion(Sender: TObject; Node: TTreeNode);
    procedure tvSnippetsEnter(Sender: TObject);
    procedure tvSnippetsKeyDown(Sender: TObject; var Key: Word;
      Shift: TShiftState);
    procedure tvSnippetsKeyPress(Sender: TObject; var Key: Char);
    procedure tvSnippetsKeyUp(Sender: TObject; var Key: Word;
      Shift: TShiftState);
    procedure tvSnippetsMouseDown(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer);
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
      fTVDraw: TTVDraw;             // Object that renders tree view nodes
      fNotifier: INotifier;         // Notifies app of user initiated events
      fCanChange: Boolean;          // Whether selected node allowed to change
      fSelectedItem: TViewItem;     // Current selected view item in tree view
      fPrevSelectedItem: TViewItem; // Previous selected view item in tree view
      fRoutineList: TRoutineList;   // List of currently displayed snippets
      fTreeStates: array of TOverviewTreeState;
                                    // Array of tree state objects: one per tab
      fCommandBars: TCommandBarMgr; // Configures popup menu and toolbar
    const
      cPermittedKeys = [            // Keypresses handled by treeview as default
        VK_UP, VK_DOWN, VK_PRIOR, VK_NEXT, VK_LEFT, VK_RIGHT, VK_HOME, VK_END
      ];
    procedure SelectNode(const Node: TTreeNode; const MakeVisible: Boolean);
      {Selects a specified node and optionally make it visible in the tree view.
        @param Node [in] Node to be selected.
        @param MakeVisible [in] Flag indicating if node is to be made visible.
          Ignored if node is nil.
      }
    procedure SelectionChange(const Node: TTreeNode); overload;
      {Records view item associated with a selected node and, if item has
      changed, triggers action to notify program of selection change.
        @param Node [in] Selected node.
      }
    procedure SelectionChange(const Item: TViewItem); overload;
      {Records new selected view item and, if item has changed, triggers action
      to notify program of selection change.
        @param Item [in] Selected item.
      }
    procedure Redisplay;
      {Redisplays all snippets within current snippet list in required style.
      }
    procedure InternalSelectItem(const Item: TViewItem);
      {Selects the tree node associated with a view item in the tree view if
      view item is in the tree view, otherwise deselects current item.
        @param Item [in] View item to be selected.
      }
    function FindItemNode(const Item: TViewItem): TViewItemTreeNode;
      {Finds node representing a view item in the tree view.
        @param Item [in] View item to be found (may be nil).
        @return Tree node associated with view item or nil if no match or if
          Item is nil.
      }
    function GetSectionNode(const Node: TTreeNode): TViewItemTreeNode;
      {Gets reference to the top level parent (section) node for any tree node.
        @param Node [in] Node whose section node is required.
        @return Required section node. Maybe Node if Node is a section node.
      }
    function GetNextSectionNode: TTreeNode;
      {Gets the next section node to that containing the currently selected
      node.
        @return Required node. If current node is last section node it is
          returned. If there is no selected node the first section node is
          returned.
      }
    function GetPreviousSectionNode: TTreeNode;
      {Gets the previous section node to that containing the currently selected
      node.
        @return Required node. If current node is first section node it is
          returned. If there is no selected node the first section node is
          returned.
      }
  protected // interface implementations - do not make strict
    { ITabbedDisplayMgr }
    procedure SelectTab(const TabIdx: Integer);
      {Selects tab with specified index.
        @param TabIdx [in] Index of tab to be selected.
      }
    function SelectedTab: Integer;
      {Returns index of currently selected tab.
        @return Required tab index.
      }
    procedure NextTab;
      {Switches to next tab, or return to first tab if current tab is last.
      }
    procedure PreviousTab;
      {Switches to previous tab, or return to last tab if current tab is first.
      }
    { IOverviewDisplayMgr }
    procedure Initialise(const TabIdx: Integer);
      {Initialise frame with specified tab selected.
        @param TabIdx [in] Index of required tab.
      }
    procedure Display(const RoutineList: TRoutineList);
      {Displays the snippets in the current overview tab.
        @param RoutineList [in] List of snippets to be displayed or nil if
          nothing to be displayed.
      }
    procedure Clear;
      {Clears the display.
      }
    procedure SelectItem(const ViewItem: TViewItem);
      {Selects a view item in the overview pane.
        @param ViewItem [in] Item to be selected.
      }
    procedure UpdateTreeState(const State: TTreeNodeAction);
      {Updates expand / collapse state of treevew.
        @param State [in] Required expand / collapse state.
      }
    function CanUpdateTreeState(const State: TTreeNodeAction): Boolean;
      {Checks if a specified expand / collapse tree view action can be
      performed.
        @param State [in] Expand / collapse action being queried.
        @return True if action can be performed, False if not.
      }
    procedure SaveTreeState;
      {Saves current expansion state of treeview in memory.
      }
    procedure RestoreTreeState;
      {Restores last saved treeview expansion state from memory.
      }
    { IPaneInfo }
    function IsInteractive: Boolean;
      {Checks if the pane is currently interactive with user.
        @return True if pane is interactive, False if not.
      }
    { ISetNotifier }
    procedure SetNotifier(const Notifier: INotifier);
      {Sets the object's notifier object to be called in response to user input.
        @param Notifier [in] Required notifier object.
      }
    { ICommandBarConfig }
    property CommandBars: TCommandBarMgr
      read fCommandBars implements ICommandBarConfig;
      {References contained object implementing ICommandBarConfig interfaces}
  public
    constructor Create(AOwner: TComponent); override;
      {Class constructor. Sets up object.
        @param AOwner [in] Component that owns frame.
      }
    destructor Destroy; override;
      {Class destructor. Tears down object.
      }
  end;


implementation


uses
  // Delphi
  SysUtils,
  // Project
  UKeysHelper, UOverviewTreeBuilder;


{$R *.dfm}

{ TOverviewFrame }

function TOverviewFrame.CanUpdateTreeState(
  const State: TTreeNodeAction): Boolean;
  {Checks if a specified expand / collapse tree view action can be performed.
    @param State [in] Expand / collapse action being queried.
    @return True if action can be performed, False if not.
  }
begin
  case State of
    taExpandNode:
      Result := Assigned(tvSnippets.Selected) and
        not GetSectionNode(tvSnippets.Selected).Expanded;
    taExpandAll:
      Result := True;
    taCollapseNode:
      Result := Assigned(tvSnippets.Selected) and
        GetSectionNode(tvSnippets.Selected).Expanded;
    taCollapseAll:
      Result := True;
    else
      Result := False;
  end;
end;

procedure TOverviewFrame.Clear;
  {Clears the display.
  }
begin
  SelectItem(nil);
  Display(nil);
end;

constructor TOverviewFrame.Create(AOwner: TComponent);
  {Class constructor. Sets up object.
    @param AOwner [in] Component that owns frame.
  }
var
  TabIdx: Integer;  // loops through tabs
begin
  inherited;
  // Create delegated (contained) command bar manager for toolbar and popup menu
  fCommandBars := TCommandBarMgr.Create(Self);
  fCommandBars.AddCommandBar(
    cOverviewToolBar, TToolBarWrapper.Create(tbarOverview)
  );
  fCommandBars.AddCommandBar(
    cOverviewPopupMenu, TPopupMenuWrapper.Create(mnuOverview)
  );
  // Create new empty objects to store current and previous selected view items
  fSelectedItem := TViewItem.Create;
  fPrevSelectedItem := TViewItem.Create;
  // Create treeview draw object
  fTVDraw := TTVDraw.Create;
  tvSnippets.OnCustomDrawItem := fTVDraw.CustomDrawItem;
  // Create list to store displayed snippets
  fRoutineList := TRoutineList.Create;
  // Create objects used to remember state of each tree view
  SetLength(fTreeStates, tcDisplayStyle.Tabs.Count);
  for TabIdx := 0 to Pred(tcDisplayStyle.Tabs.Count) do
    fTreeStates[TabIdx] := TOverviewTreeState.Create(tvSnippets);
end;

destructor TOverviewFrame.Destroy;
  {Class destructor. Tears down object.
  }
var
  TabIdx: Integer;  // loops through tabs
begin
  for TabIdx := Pred(tcDisplayStyle.Tabs.Count) downto 0 do
    FreeAndNil(fTreeStates[TabIdx]);
  FreeAndNil(fTVDraw);
  FreeAndNil(fPrevSelectedItem);
  FreeAndNil(fSelectedItem);
  FreeAndNil(fRoutineList); // does not free referenced snippets
  fCommandBars.Free;
  inherited;
end;

procedure TOverviewFrame.Display(const RoutineList: TRoutineList);
  {Displays the snippets in the current overview tab.
    @param RoutineList [in] List of snippets to be displayed or nil if nothing
      to be displayed.
  }
begin
  // Only do update if new snippet list is different to current one
  if not fRoutineList.IsEqual(RoutineList) then
  begin
    // Take copy of new list
    fRoutineList.Assign(RoutineList);
    Redisplay;
  end;
end;

function TOverviewFrame.FindItemNode(const Item: TViewItem): TViewItemTreeNode;
  {Finds node representing a view item in the tree view.
    @param Item [in] View item to be found (may be nil).
    @return Tree node associated with view item or nil if no match or if Item is
      nil.
  }
var
  Idx: Integer;             // loops thru all tree nodes
  Node: TViewItemTreeNode;  // reference to a tree node in tree view
begin
  // Assume we can't find node
  Result := nil;
  if Assigned(Item) then
  begin
    // Search thru all nodes in tree view
    for Idx := 0 to Pred(tvSnippets.Items.Count) do
    begin
      // Check if current node is required one ...
      Node := tvSnippets.Items[Idx] as TViewItemTreeNode;
      if Node.ViewItem.IsEqual(Item) then
      begin
        // ... and get out if so
        Result := Node;
        Break;
      end;
    end;
  end;
end;

function TOverviewFrame.GetNextSectionNode: TTreeNode;
  {Gets the next section node to that containing the currently selected node.
    @return Required node. If current node is last section node it is returned.
      If there is no selected node the first section node is returned.
  }
var
  ThisSectionNode: TTreeNode; // reference to current section node
  SelectedNode: TTreeNode;    // currently selected node
begin
  SelectedNode := tvSnippets.Selected;
  if Assigned(SelectedNode) then
  begin
    ThisSectionNode := GetSectionNode(SelectedNode);
    Result := ThisSectionNode.GetNextSibling;
    if not Assigned(Result) then
      Result := ThisSectionNode;
  end
  else
    Result := tvSnippets.Items.GetFirstNode;
end;

function TOverviewFrame.GetPreviousSectionNode: TTreeNode;
  {Gets the previous section node to that containing the currently selected
  node.
    @return Required node. If current node is first section node it is returned.
      If there is no selected node the first section node is returned.
  }
var
  ThisSectionNode: TTreeNode; // reference to current section node
  SelectedNode: TTreeNode;    // currently selected node
begin
  SelectedNode := tvSnippets.Selected;
  if Assigned(SelectedNode) then
  begin
    ThisSectionNode := GetSectionNode(SelectedNode);
    Result := ThisSectionNode.GetPrevSibling;
    if not Assigned(Result) then
      Result := ThisSectionNode;
  end
  else
    Result := tvSnippets.Items.GetFirstNode;
end;

function TOverviewFrame.GetSectionNode(
  const Node: TTreeNode): TViewItemTreeNode;
  {Gets reference to the top level parent (section) node for any tree node.
    @param Node [in] Node whose section node is required.
    @return Required section node. Maybe Node if Node is a section node.
  }
var
  CurrentNode: TTreeNode; // iterates through all parent nodes
begin
  CurrentNode := Node;
  while (CurrentNode.Level > 0) do
    CurrentNode := CurrentNode.Parent;
  Result := CurrentNode as TViewItemTreeNode;
end;

procedure TOverviewFrame.Initialise(const TabIdx: Integer);
  {Initialise frame with specified tab selected.
    @param TabIdx [in] Index of required tab.
  }
begin
  Assert((TabIdx >= 0) and (TabIdx < tcDisplayStyle.Tabs.Count),
    ClassName + '.Initialise: TabIdx out range');
  tcDisplayStyle.TabIndex := TabIdx;
  Redisplay;
end;

procedure TOverviewFrame.InternalSelectItem(const Item: TViewItem);
  {Selects the tree node associated with a view item in the tree view if view
  item is in the tree view, otherwise deselects current item.
    @param Item [in] View item to be selected.
  }
var
  Node: TViewItemTreeNode;  // tree node associated with view item
begin
  // Get node associated with given view item
  Node := FindItemNode(Item);
  if Assigned(Node) then
    // We found node: select it and make sure visible
    SelectNode(Node, True)
  else
  begin
    // Can't find item: show top of tree
    fCanChange := False;
    try
      tvSnippets.Deselect(tvSnippets.Selected);
      tvSnippets.Selected := nil;
      tvSnippets.TopItem := tvSnippets.Items.GetFirstNode;
    finally
      fCanChange := True;
    end;
  end;
end;

function TOverviewFrame.IsInteractive: Boolean;
  {Checks if the pane is currently interactive with user.
    @return True if pane is interactive, False if not.
  }
var
  Idx: Integer; // loops thru frame's owned components
begin
  // This pane is interactive if one of owned controls has focus
  Result := False;
  for Idx := 0 to Pred(ComponentCount) do
    if (Components[Idx] is TWinControl)
      and (Components[Idx] as TWinControl).Focused then
    begin
      Result := True;
      Break;
    end;
end;

procedure TOverviewFrame.NextTab;
  {Switches to next tab, or return to first tab if current tab is last.
  }
begin
  if SelectedTab = Pred(tcDisplayStyle.Tabs.Count) then
    SelectTab(0)
  else
    SelectTab(Succ(SelectedTab));
end;

procedure TOverviewFrame.PreviousTab;
  {Switches to previous tab, or return to last tab if current tab is first.
  }
begin
  if SelectedTab = 0 then
    SelectTab(Pred(tcDisplayStyle.Tabs.Count))
  else
    SelectTab(Pred(SelectedTab));
end;

procedure TOverviewFrame.Redisplay;
  {Redisplays all snippets within current snippet list in required style.
  }
var
  Builder: TOverviewTreeBuilder;  // builds overview tree with correct grouping
  BuilderClasses:                 // overview builder classes for each grouping
    array of TOverviewTreeBuilderClass;
begin
  // Store list of overview tree builder classes: one for each tab
  SetLength(BuilderClasses, tcDisplayStyle.Tabs.Count);
  BuilderClasses[cCategorisedTab] := TOverviewCategorisedTreeBuilder;
  BuilderClasses[cAlphabeticTab] := TOverviewAlphabeticTreeBuilder;
  BuilderClasses[cKindTab] := TOverviewSnipKindTreeBuilder;
  Builder := nil;
  tvSnippets.Items.BeginUpdate;
  try
    // Clear tree view
    fCanChange := False;
    tvSnippets.Items.Clear;
    if fRoutineList.Count = 0 then
      Exit;
    // Build new treeview using grouping determined by selected tab
    Builder := BuilderClasses[tcDisplayStyle.TabIndex].Create(
      tvSnippets, fRoutineList
    );
    Builder.Build;
    // Restore state of treeview based on last time it was displayed
    tvSnippets.FullExpand;
    RestoreTreeState;
  finally
    FreeAndNil(Builder);
    fCanChange := True;
    tvSnippets.Items.EndUpdate;
  end;
  // Reselect current view item if possible
  InternalSelectItem(fSelectedItem);
end;

procedure TOverviewFrame.RestoreTreeState;
  {Restores last saved treeview expansion state from memory.
  }
begin
  fTreeStates[tcDisplayStyle.TabIndex].RestoreState;
end;

procedure TOverviewFrame.SaveTreeState;
  {Saves current expansion state of treeview in memory.
  }
begin
  fTreeStates[tcDisplayStyle.TabIndex].SaveState;
end;

function TOverviewFrame.SelectedTab: Integer;
  {Returns index of currently selected tab.
    @return Required tab index.
  }
begin
  Result := tcDisplayStyle.TabIndex;
end;

procedure TOverviewFrame.SelectionChange(const Node: TTreeNode);
  {Records view item associated with a selected node and, if item has changed,
  triggers action to notify program of selection change.
    @param Node [in] Selected node.
  }
begin
  if Assigned(Node) and (Node is TViewItemTreeNode) then
    SelectionChange((Node as TViewItemTreeNode).ViewItem);
end;

procedure TOverviewFrame.SelectionChange(const Item: TViewItem);
  {Records new selected view item and, if item has changed, triggers action to
  notify program of selection change.
    @param Item [in] Selected item.
  }
begin
  // Record new selected item
  fSelectedItem.Assign(Item);
  if not fSelectedItem.IsEqual(fPrevSelectedItem) then
  begin
    // Item has actually changed: store as previously selected item
    fPrevSelectedItem.Assign(fSelectedItem);
    // Notify application of change
    if Assigned(fNotifier) then
      fNotifier.ShowViewItem(fSelectedItem);
  end;
end;

procedure TOverviewFrame.SelectItem(const ViewItem: TViewItem);
  {Selects a view item in the overview pane.
    @param ViewItem [in] Item to be selected.
  }
begin
  // Select in tree view
  InternalSelectItem(ViewItem);
  // Record view item as selected one
  fSelectedItem.Assign(ViewItem);
  fPrevSelectedItem.Assign(fSelectedItem);
end;

procedure TOverviewFrame.SelectNode(const Node: TTreeNode;
  const MakeVisible: Boolean);
  {Selects a specified node and optionally make it visible in the tree view.
    @param Node [in] Node to be selected.
    @param MakeVisible [in] Flag indicating if node is to be made visible.
      Ignored if node is nil.
  }
begin
  fCanChange := True;
  try
    tvSnippets.Selected := Node;
    tvSnippets.Select(Node, []);
    if MakeVisible and Assigned(Node) then
      Node.MakeVisible;
  finally
    fCanChange := False;
  end;
end;

procedure TOverviewFrame.SelectTab(const TabIdx: Integer);
  {Selects tab with specified index.
    @param TabIdx [in] Tab to be selected.
  }
begin
  Assert((TabIdx >= 0) and (TabIdx < tcDisplayStyle.Tabs.Count),
    ClassName + '.SelectTab: TabIdx out range');
  // Need to ensure tree state has been saved before changing tab and
  // redisplaying. If this method was called for current tab index we assume
  // it's via tcDisplayStyleChange when tree state will have already been saved
  // in tcDisplayStyleChanging. If not, we need to save tree state before
  // changing tab index.
  if tcDisplayStyle.TabIndex <> TabIdx then
  begin
    SaveTreeState;
    tcDisplayStyle.TabIndex := TabIdx;
  end;
  Redisplay;
end;

procedure TOverviewFrame.SetNotifier(const Notifier: INotifier);
  {Sets the object's notifier object to be called in response to user input.
    @param Notifier [in] Required notifier object.
  }
begin
  fNotifier := Notifier;
end;

procedure TOverviewFrame.tcDisplayStyleChange(Sender: TObject);
  {Handles tab set's tab change event. Notifies application of display style
  change via notifier object.
    @param Sender [in] Not used.
  }
begin
  if Assigned(fNotifier) then
    fNotifier.ChangeOverviewStyle(tcDisplayStyle.TabIndex);
end;

procedure TOverviewFrame.tcDisplayStyleChanging(Sender: TObject;
  var AllowChange: Boolean);
  {Handles tab sets tab changing event, called just before tab changes. Stores
  current state of tree in tab that is closing.
    @param Sender [in] Not used.
    @param AllowChanges [in/out] Not used or modified. Permits tab change.
  }
begin
  SaveTreeState;
end;

procedure TOverviewFrame.tcDisplayStyleMouseDown(Sender: TObject;
  Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
  {Handles event triggered when user clicks on tab control. Sets focus to tab
  control. This does not always happen automatically.
    @param Sender [in] Not used.
    @param Button [in] Not used.
    @param Shift [in] Not used.
    @param X [in] X co-ordinate of mouse in client co-ordinates.
    @param Y [in] Y co-ordinate of mouse in client co-ordinates.
  }
begin
 if htOnItem in tcDisplayStyle.GetHitTestInfoAt(X, Y) then
    tcDisplayStyle.SetFocus;
end;

procedure TOverviewFrame.tvSnippetsChanging(Sender: TObject;
  Node: TTreeNode; var AllowChange: Boolean);
  {Handles event triggered when snippets tree view selection is about to change.
  We allow or prevent this according to state of fCanChange flag.
    @param Sender [in] Not used.
    @param Node [in] Not used.
    @param AllowChange [in/out] Set true to permit selection change or false to
      prevent.
  }
begin
  AllowChange := fCanChange;
end;

procedure TOverviewFrame.tvSnippetsCreateNodeClass(Sender: TCustomTreeView;
  var NodeClass: TTreeNodeClass);
  {Event handler called when a tree node is being created that provdes class
  type to be used for new node. We notify that node is to be TViewItemTreeNode.
    @param Sender [in] Not used.
    @param NodeClass [in/out] Set to TViewItemTreeNode.
  }
begin
  NodeClass := TViewItemTreeNode;
end;

procedure TOverviewFrame.tvSnippetsDeletion(Sender: TObject;
  Node: TTreeNode);
  {Handles event triggered when a tree node is to be deleted. We free associated
  view item.
    @param Sender [in] Not used.
    @param Node [in] Reference to node being freed (of type TViewItemTreeNode).
  }
begin
  if Assigned(Node) then
    (Node as TViewItemTreeNode).ViewItem.Free;
end;

procedure TOverviewFrame.tvSnippetsEnter(Sender: TObject);
  {Handles snippets treeview's OnEnter event. Checks if any node is selected
  and, if not, selects the first node.
    @param Sender [in] Not used.
  }
begin
  if (tvSnippets.Items.Count > 0) and not Assigned(tvSnippets.Selected) then
  begin
    SelectNode(tvSnippets.Items.GetFirstNode, True);
    SelectionChange(tvSnippets.Items.GetFirstNode);
  end;
end;

procedure TOverviewFrame.tvSnippetsKeyDown(Sender: TObject; var Key: Word;
  Shift: TShiftState);
  {Handles a key down event on tree view. We allow movement using keys in
  cPermittedKeys with no shift keys along with Ctrl+Home and Ctrl+End. We permit
  the selection to change when one of these keys is pressed.
    @param Sender [in] Not used.
    @param Key [in/out] Key being pressed. Set to 0 when key to be inhibited.
    @param Shift [in] Modifier keys.
  }
begin
  if HasShiftKeys(Shift) then
  begin
    // shift keys are pressed: handle just Ctrl+Home, Ctrl+End, Ctrl+Up and
    // Ctrl+Down specially.
    case Key of
      VK_HOME:
        if ExtractShiftKeys(Shift) = [ssCtrl] then
          if tvSnippets.Items.Count > 0 then
            SelectNode(tvSnippets.Items.GetFirstNode, True);
      VK_END:
        if ExtractShiftKeys(Shift) = [ssCtrl] then
          if tvSnippets.Items.Count > 0 then
            SelectNode(tvSnippets.Items[Pred(tvSnippets.Items.Count)], True);
      VK_UP:
        if ExtractShiftKeys(Shift) = [ssCtrl] then
          if tvSnippets.Items.Count > 0 then
            SelectNode(GetPreviousSectionNode, True);
      VK_DOWN:
        if ExtractShiftKeys(Shift) = [ssCtrl] then
          if tvSnippets.Items.Count > 0 then
            SelectNode(GetNextSectionNode, True);
    end;
    // permit Alt+F4 and inhibit all other default processing
    if not IsKeyCombination(VK_F4, [ssAlt], Key, Shift) then
      Key := 0;
  end
  else if Key in cPermittedKeys then
    // no shift keys and one of permitted keys are pressed: permit default
    // tree view processing (KeyUp event resets fCanChange).
    fCanChange := True;
end;

procedure TOverviewFrame.tvSnippetsKeyPress(Sender: TObject; var Key: Char);
  {Handles treeview's keypress event. Prevents further processing of return key
  press to inhibit beep from treeview control.
    @param Sender [in] Not used.
    @param Key [in/out] Key pressed. Set to #0 to prevent further processing.
  }
begin
  inherited;
  if Ord(Key) = VK_RETURN then
    Key := #0; // don't allow RETURN key
end;

procedure TOverviewFrame.tvSnippetsKeyUp(Sender: TObject; var Key: Word;
  Shift: TShiftState);
  {Handles a key up event on tree view. We allow movement using keys in
  cPermittedKey. Release of one these keys follows a selection change and we
  trigger a notification of this event from here.
    @param Sender [in] Not used.
    @param Key [in/out] Key pressed. Not altered.
    @param Shift [in] Modifier keys.
  }
var
  Node: TTreeNode;  // selected tree node
begin
  Node := tvSnippets.Selected;
  if (not HasShiftKeys(Shift) and (Key in cPermittedKeys)) or
    (
      (ExtractShiftKeys(Shift) = [ssCtrl]) and
      (Key in [VK_HOME, VK_END, VK_UP, VK_DOWN])
    ) then
  begin
    // One of keys triggering selection change was released. We get reference to                          `
    // selected node and trigger notification via SelectionChange method
    if Assigned(Node) and (Node is TViewItemTreeNode) then
      SelectionChange(Node);
  end
  // Check for RETURN key with no modifiers: toggle node expand / collapse when
  // a section header has focus
  else if not HasShiftKeys(Shift) and (Key = VK_RETURN) then
  begin
    if Assigned(Node) and (Node.Level = 0) then
    begin
      if Node.Expanded then
        UpdateTreeState(taCollapseNode)
      else
        UpdateTreeState(taExpandNode);
    end;
  end;
  // Prevent further changes
  fCanChange := False;
end;

procedure TOverviewFrame.tvSnippetsMouseDown(Sender: TObject;
  Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
  {Handles mouse down events on the snippets tree view. If the mouse click is
  on a tree node we allow the node to be selected.
    @param Sender [in] Not used.
    @param Button [in] Not used.
    @param Shift [in] Not used.
    @param X [in] position of mouse.
    @param Y [in] position of mouse.
  }
var
  Node: TTreeNode;  // tree node clicked
begin
  // Check if mouse click on a tree node
  if [htOnItem, htOnRight] * tvSnippets.GetHitTestInfoAt(X, Y) <> [] then
  begin
    // Get node clicked
    Node := tvSnippets.GetNodeAt(X, Y);
    if Assigned(Node) and (Node is TViewItemTreeNode) then
    begin
      SelectNode(Node, False);
      SelectionChange(Node);
    end;
  end;
end;

procedure TOverviewFrame.UpdateTreeState(const State: TTreeNodeAction);
  {Updates expand / collapse state of treevew.
    @param State [in] Required expand / collapse state.
  }
var
  Node: TViewItemTreeNode;        // currently selected node
  SectionNode: TViewItemTreeNode; // section node to which Node belongs
begin
  // Get current node (if any) and corresponding section node (if any)
  Node := FindItemNode(fSelectedItem);
  if Assigned(Node) then
    SectionNode := GetSectionNode(Node)
  else
    SectionNode := nil;
  // Perform expand or collapse
  tvSnippets.Items.BeginUpdate;
  try
    case State of
      taExpandNode:
      begin
        // expand section to which selected node belongs maintaining &
        // re-selecting current selection
        if Assigned(SectionNode) then
          SectionNode.Expanded := True;
        InternalSelectItem(fSelectedItem);
      end;
      taExpandAll:
      begin
        // expand all tree, maintaining & re-selecting current selection
        tvSnippets.FullExpand;
        InternalSelectItem(fSelectedItem);
      end;
      taCollapseNode:
      begin
        // collapse section to which selected node belongs
        // select the section node
        SelectNode(SectionNode, False);
        // collapse section and notify change of selection
        if Assigned(SectionNode) then
        begin
          SectionNode.Expanded := False;
          SelectionChange(SectionNode);
        end;
      end;
      taCollapseAll:
      begin
        // collapse whole tree
        // select the node
        SelectNode(SectionNode, False);
        // collapse whole tree and notify change of selection
        tvSnippets.FullCollapse;
        if Assigned(SectionNode) then
          SelectionChange(SectionNode);
      end;
    end;
  finally
    tvSnippets.Items.EndUpdate;
  end;
end;

{ TOverviewFrame.TTVDraw }

function TOverviewFrame.TTVDraw.IsSectionHeadNode(
  const Node: TTreeNode): Boolean;
  {Checks if a node represents a section header.
    @param Node [in] Node to be checked.
    @return True if node is a section header, False if not.
  }
begin
  Result := (Node as TViewItemTreeNode).ViewItem.Kind
    in [vkCategory, vkAlphabet, vkSnipKind];
end;

function TOverviewFrame.TTVDraw.IsUserDefinedNode(
  const Node: TTreeNode): Boolean;
  {Checks if a node represents a user defined snippets object.
    @param Node [in] Node to be checked.
    @return True if node represents user defined object, False if not.
  }
var
  ViewItem: TViewItem;  // view item represented by node
begin
  ViewItem := (Node as TViewItemTreeNode).ViewItem;
  Result := ((ViewItem.Kind = vkRoutine) and ViewItem.Routine.UserDefined)
    or ((ViewItem.Kind = vkCategory) and ViewItem.Category.UserDefined);
end;

end.


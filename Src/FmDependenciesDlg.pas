{
 * This Source Code Form is subject to the terms of the Mozilla Public License,
 * v. 2.0. If a copy of the MPL was not distributed with this file, You can
 * obtain one at https://mozilla.org/MPL/2.0/
 *
 * Copyright (C) 2009-2023, Peter Johnson (gravatar.com/delphidabbler).
 *
 * Implements a dialogue box that displays all the dependencies and dependents
 * of a snippet.
}


unit FmDependenciesDlg;


interface


uses
  // Delphi
  ComCtrls, StdCtrls, Controls, ExtCtrls, Classes, Windows, ActnList,
  // Project
  DB.UCollections,
  DB.USnippet, FmGenericViewDlg, UBaseObjects, USearch, USnippetIDs,
  USnippetsTVDraw;


type
  {
  TDependenciesDlg:
    Tabbed dialog box that displays all the dependencies and dependents of a
    snippet.
  }
  TDependenciesDlg = class(TGenericViewDlg, INoPublicConstruct)
    lbDependents: TListBox;
    lblCircularRef: TLabel;
    lblNoDependencies: TLabel;
    lblNoDependents: TLabel;
    pcBody: TPageControl;
    tsDependsUpon: TTabSheet;
    tsRequiredBy: TTabSheet;
    tvDependencies: TTreeView;
    btnSelectAndClose: TButton;
    alSelectAndClose: TActionList;
    actSelectAndClose: TAction;
    procedure FormDestroy(Sender: TObject);
    procedure lbDependentsDrawItem(Control: TWinControl; Index: Integer;
      Rect: TRect; State: TOwnerDrawState);
    procedure pcBodyMouseDown(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer);
    procedure tvDependenciesCollapsing(Sender: TObject; Node: TTreeNode;
      var AllowCollapse: Boolean);
    procedure actSelectAndCloseExecute(Sender: TObject);
    procedure actSelectAndCloseUpdate(Sender: TObject);
  public
    type
      TTabID = (tiDependsUpon, tiRequiredBy);
      TTabIDs = set of TTabID;
  strict private
    type
      {
      TTVDraw:
        Object used to draw snippets tree view nodes.
      }
      TTVDraw = class(TSnippetsTVDraw)
      strict private
        fRootID: TSnippetID;  // ID of snippet whose dependency nodes displayed
      strict protected
        ///  <summary>Gets the collection ID, if any, associated with a  tree
        ///  node.</summary>
        ///  <param name="Node"><c>TTreeNode</c> [in] Node to be checked.
        ///  </param>
        ///  <returns><c>TCollectionID</c>. Associated collection ID. If
        ///  <c>Node</c> has no associated collection then a null collection ID
        ///  is returned.</returns>
        function GetCollectionID(const Node: TTreeNode): TCollectionID;
          override;
//        function IsUserDefinedNode(const Node: TTreeNode): Boolean;
//          override;
//          {Checks if a node represents a user defined snippets object.
//            @param Node [in] Node to be checked.
//            @return True if node represents user defined object, False if not.
//          }
        function IsErrorNode(const Node: TTreeNode): Boolean;
          override;
          {Checks if a node represents an error condition.
            @param Node [in] Node to be checked.
            @return True if node represents error condition, False if not.
          }
      public
        constructor Create(const RootID: TSnippetID);
          {Class constructor. Sets up object.
            @param ID [in] ID snippet for which dependencies are displayed.
          }
      end;
    var
      fSnippetID: TSnippetID;     // Snippet whose dependencies are displayed
      fDisplayName: string;       // Display name of snippet
      fDependsList: TSnippetList; // List of dependencies to be displayed
      fTVDraw: TTVDraw;           // Customises appearance of tree view}
      fTabs: TTabIDs;             // Specifies tabs to be displayed
      fCanSelect: Boolean;        // Specifies if dependencies can be selected
      fSearch: ISearch;           // Search that can select dependencies
    procedure PopulateRequiredByList;
      {Populates list box with items for each snippet required to compile the
      specified snippet.
      }
    procedure PopulateTreeView;
      {Populates treeview with nodes for each snippet in dependency list.
      }
    procedure AddDependencies(const Parent: TTreeNode;
      const DependsList: TSnippetList);
      {Adds tree nodes for snippets in a dependency list.
        @param Parent [in] Parent node for nodes from dependency list.
        @param DependsList [in] Dependency list containing snippets to be added
          to treeview.
      }
    procedure DisplayCircularRefWarning;
      {Displays circular reference warning label.
      }
    function GetDisplayName: string;
      {Gets display name for snippet for which dependencies are being displayed.
        @return Required display name: snippet's name if available, otherwise an
          untitled string.
      }
  strict protected
    procedure ConfigForm; override;
      {Configure controls on form.
      }
    procedure ArrangeForm; override;
      {Arranges controls on form.
      }
  public
    class procedure Execute(const AOwner: TComponent;
      const SnippetID: TSnippetID; const DisplayName: string;
      const DependsList: TSnippetList; const Tabs: TTabIDs;
      const AHelpKeyword: string); overload;
      {Displays dialogue box containing details of a snippet's dependencies.
        @param AOwner [in] Component that owns the dialog box.
        @param SnippetID [in] ID of snippet for which dependencies are to be
          displayed.
        @param DisplayName [in] Display name of snippet for which dependencies
          are to be displayed.
        @param DependsList [in] List of dependencies.
        @param Tabs [in] Tabs to be displayed in dialogue box.
        @param AHelpKeyword [in] A-link help keyword ofrequired help topic.
      }
    class function Execute(const AOwner: TComponent; const Snippet: TSnippet;
      const Tabs: TTabIDs; const PermitSelection: Boolean;
      const AHelpKeyword: string): ISearch; overload;
      {Displays dialogue box containing details of a snippet's dependencies.
        @param AOwner [in] Component that owns the dialog box.
        @param Snippet [in] Snippet for which dependencies are to be displayed.
        @param Tabs [in] Tabs to be displayed in dialogue box.
        @param PermitSelection [in] Determines whether listed dependencies can
          be selected. When False this method always returns nil.
        @param AHelpKeyword [in] A-link help keyword ofrequired help topic.
        @returns A search object that can be used to select the dependent
          snippets or nil if no snippets can be selected. The result is only non
          nil if PermitSelection is True and the user chooses to make a
          selection.
      }
  end;


implementation


uses
  // Delphi
  SysUtils,
  Graphics,
  // Project
  DB.UMain,
  DB.USnippetKind,
  UBox,
  UColours,
  UCtrlArranger,
  UFontHelper,
  UPreferences;

{$R *.dfm}


{ TDependenciesDlg }

procedure TDependenciesDlg.actSelectAndCloseExecute(Sender: TObject);
  {Creates a suitable search to select the snippets on the active tab when
  the dialogue box is closed in response to the Select & Close button being
  clicked.
    @param Sender [in] Not used.
  }
var
  Snippet: TSnippet;          // snippet for which dependencies required
  Filter: IXRefSearchFilter;  // xref filter needed to select snippets
begin
  Snippet := Database.Snippets.Find(fSnippetID);
  Assert(Assigned(Snippet),
    ClassName + '.actSelectAndCloseExecute: Snippet id not found');
  if pcBody.ActivePage = tsDependsUpon then
  begin
    Filter := TSearchFilterFactory.CreateXRefSearchFilter(
      Snippet, [soRequired, soRequiredRecurse]
    );
  end
  else {pcBody.ActivePage = tsRequiredBy}
  begin
    Filter := TSearchFilterFactory.CreateXRefSearchFilter(
      Snippet, [soRequiredReverse]
    );
  end;
  fSearch := TSearchFactory.CreateSearch(Filter);
end;

procedure TDependenciesDlg.actSelectAndCloseUpdate(Sender: TObject);
  {Updates visibility and enabled state of select-and-close action.
    @param Sender [in] Select-and-close action that triggered the event.
  }
begin
  (Sender as TAction).Visible := fCanSelect;
  if fCanSelect then
  begin
    if pcBody.ActivePage = tsDependsUpon then
      (Sender as TAction).Enabled := tvDependencies.Items.Count > 0
    else {pcBody.ActivePage = tsRequiredBy}
      (Sender as TAction).Enabled := lbDependents.Count > 0;
  end;
end;

procedure TDependenciesDlg.AddDependencies(const Parent: TTreeNode;
  const DependsList: TSnippetList);
  {Adds tree nodes for snippets in a dependency list.
    @param Parent [in] Parent node for nodes from dependency list.
    @param DependsList [in] Dependency list containing snippets to be added to
      treeview.
  }
var
  RequiredSnippet: TSnippet;  // iterates through snippets in DependsList
  ChildNode: TTreeNode;       // a node added to treeview
begin
  for RequiredSnippet in DependsList do
  begin
    // Add node for snippet from dependency list
    ChildNode := tvDependencies.Items.AddChild(
      Parent, RequiredSnippet.DisplayName
    );
    ChildNode.Data := RequiredSnippet;  // reference to associated snippet
    // Check for circular reference. If detetected display warning otherwise
    // recursively add child nodes for snippet's dependency list
    if (RequiredSnippet.ID <> fSnippetID) then
      AddDependencies(ChildNode, RequiredSnippet.Depends)
    else
      DisplayCircularRefWarning;
  end;
end;

procedure TDependenciesDlg.ArrangeForm;
  {Arranges controls on form.
  }
begin
  inherited;
  // Position extra button in bottom button line
  TCtrlArranger.MoveToLeftOf(btnClose, btnSelectAndClose, 16);
  btnSelectAndClose.Top := btnClose.Top;
  // Position "no dependencies" and "no dependents" message labels in form
  lblNoDependencies.Left :=
    (tsDependsUpon.ClientWidth - lblNoDependencies.Width) div 2;
  lblNoDependencies.Top :=
    (tsDependsUpon.ClientHeight - lblNoDependencies.Height) div 2;
  lblNoDependents.Left :=
    (tsRequiredBy.ClientWidth - lblNoDependents.Width) div 2;
  lblNoDependents.Top :=
    (tsRequiredBy.ClientHeight - lblNoDependents.Height) div 2;
  // Adjust size of treeview
  if lblCircularRef.Visible then
  begin
    // move label
    lblCircularRef.Top := tsDependsUpon.ClientHeight
      - lblCircularRef.Height - 8;
    // circular reference: make room to see circular reference label
    tvDependencies.Align := alTop;
    tvDependencies.Height := lblCircularRef.Top - 6;
  end
  else
    // no circular reference: use all space for treeview
    tvDependencies.Align := alClient;
end;

procedure TDependenciesDlg.ConfigForm;
  {Configure controls on form.
  }
resourcestring
  // no dependents and no dependencies message templates
  sNoDepends = '%s has no dependencies';
  sNoRequires = '%s has no dependents';
  // form caption template
  sTitle = 'Dependencies for %s';
begin
  inherited;
  // Set form caption
  Caption := Format(sTitle, [GetDisplayName]);
  // Determine which tabs are visible
  tsDependsUpon.TabVisible := tiDependsUpon in fTabs;
  tsRequiredBy.TabVisible := tiRequiredBy in fTabs;
  // Set "no dependencies" and "no dependents" labels in case needed
  lblNoDependencies.Caption := Format(sNoDepends, [GetDisplayName]);
  lblNoDependencies.Font.Style := [fsBold];
  lblNoDependents.Caption := Format(sNoRequires, [GetDisplayName]);
  lblNoDependents.Font.Style := [fsBold];
  // Set "circular reference" label's colour and visibility
  lblCircularRef.Font.Color := clWarningText;
  lblCircularRef.Visible := False;
  // Provide custom draw support for treeview
  fTVDraw := TTVDraw.Create(fSnippetID);
  tvDependencies.OnCustomDrawItem := fTVDraw.CustomDrawItem;
  // Populate treeview with dependency information
  PopulateTreeView;
  // Populate Required By list view with dependent snippets
  PopulateRequiredByList;
  // Hide treeview, revealing "no dependencies" label if no dependencies
  if tvDependencies.Items.Count = 0 then
    tvDependencies.Visible := False;
  // Hide list box, revealing "no dependents" label if no dependents
  if lbDependents.Count = 0 then
    lbDependents.Visible := False;
  // Set default search result
  fSearch := nil;
end;

procedure TDependenciesDlg.DisplayCircularRefWarning;
  {Displays circular reference warning label.
  }
begin
  lblCircularRef.Visible := True;
end;

class function TDependenciesDlg.Execute(const AOwner: TComponent;
  const Snippet: TSnippet; const Tabs: TTabIDs; const PermitSelection: Boolean;
  const AHelpKeyword: string): ISearch;
var
  Dlg: TDependenciesDlg;
begin
  Assert(Tabs <> [], ClassName + '.Execute: Tabs is []');
  Dlg := InternalCreate(AOwner);
  try
    Dlg.fSnippetID := Snippet.ID;
    Dlg.fDisplayName := Snippet.DisplayName;
    Dlg.fDependsList := Snippet.Depends;
    Dlg.fTabs := Tabs;
    Dlg.fCanSelect := PermitSelection;
    Dlg.HelpKeyword := AHelpKeyword;
    if Dlg.ShowModal = mrOK then
      Result := Dlg.fSearch
    else
      Result := nil;
  finally
    Dlg.Free;
  end;
end;

class procedure TDependenciesDlg.Execute(const AOwner: TComponent;
  const SnippetID: TSnippetID; const DisplayName: string;
  const DependsList: TSnippetList; const Tabs: TTabIDs;
  const AHelpKeyword: string);
var
  Dlg: TDependenciesDlg;
begin
  Assert(Tabs <> [], ClassName + '.Execute: Tabs is []');
  Dlg := InternalCreate(AOwner);
  try
    Dlg.fSnippetID := SnippetID;
    Dlg.fDisplayName := DisplayName;
    Dlg.fDependsList := DependsList;
    Dlg.fTabs := Tabs;
    Dlg.fCanSelect := False;
    Dlg.HelpKeyword := AHelpKeyword;
    Dlg.ShowModal;
  finally
    Dlg.Free;
  end;
end;

procedure TDependenciesDlg.FormDestroy(Sender: TObject);
  {Form destruction event handler. Frees owned object.
    @param Sender [in] Not used.
  }
var
  Idx: Integer;
begin
  inherited;
  fTVDraw.Free;
  for Idx := Pred(lbDependents.Items.Count) downto 0 do
    lbDependents.Items.Objects[Idx].Free;
end;

function TDependenciesDlg.GetDisplayName: string;
  {Gets display name for snippet for which dependencies are being displayed.
    @return Required display name: snippet's name if available, otherwise an
      untitled string.
  }
resourcestring
  sUntitled = '<Untitled Snippet>'; // display name when snippet has no name
begin
  if fDisplayName <> '' then
    Exit(fDisplayName);
  if fSnippetID.Name <> '' then
    Exit(fSnippetID.Name);
  Result := sUntitled;
end;

procedure TDependenciesDlg.lbDependentsDrawItem(Control: TWinControl;
  Index: Integer; Rect: TRect; State: TOwnerDrawState);
var
  LB: TListBox;
  Canvas: TCanvas;

//  function IsUserDefinedItem: Boolean;
//  begin
//    Result := (LB.Items.Objects[Index] as TBox<Boolean>).Value;
//  end;

  function ExtractCollectionItem: TCollectionID;
  begin
    Result := (LB.Items.Objects[Index] as TBox<TCollectionID>).Value;
  end;

begin
  LB := Control as TListBox;
  Canvas := LB.Canvas;
  if not (odSelected in State) then
//    Canvas.Font.Color := Preferences.DBHeadingColours[IsUserDefinedItem];
    Canvas.Font.Color := Preferences.GetDBHeadingColour(ExtractCollectionItem);
  Canvas.TextRect(
    Rect,
    Rect.Left + 2,
    (Rect.Top + Rect.Bottom - Canvas.TextHeight(LB.Items[Index])) div 2,
    LB.Items[Index]
  );
end;

procedure TDependenciesDlg.pcBodyMouseDown(Sender: TObject;
  Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
begin
  if htOnItem in pcBody.GetHitTestInfoAt(X, Y) then
    pcBody.SetFocus;
end;

procedure TDependenciesDlg.PopulateRequiredByList;
var
  Dependents: ISnippetIDList;
  SnippetID: TSnippetID;
  ThisSnippet: TSnippet;
  ASnippet: TSnippet;
begin
  lbDependents.Items.BeginUpdate;
  try
    lbDependents.Clear;
    ThisSnippet := Database.Snippets.Find(fSnippetID);
    // must only try to get dependents for snippet if it is in database
    if (tiRequiredBy in fTabs) and Assigned(ThisSnippet) then
    begin
      Dependents := (Database as IDatabaseEdit).GetDependents(ThisSnippet);
      for SnippetID in Dependents do
      begin
        ASnippet := Database.Snippets.Find(SnippetID);
        Assert(Assigned(ASnippet),
          ClassName + '.PopulateRequiredByList: Snippet id not found');
        lbDependents.Items.AddObject(
          ASnippet.DisplayName, TBox<TCollectionID>.Create(ASnippet.CollectionID)
//          ASnippet.DisplayName, TBox<Boolean>.Create(ASnippet.UserDefined)
        );
      end;
    end;
  finally
    lbDependents.Items.EndUpdate;
  end;
end;

procedure TDependenciesDlg.PopulateTreeView;
  {Populates treeview with nodes for each snippet in dependency list.
  }
begin
  tvDependencies.Items.BeginUpdate;
  try
    tvDependencies.Items.Clear;
    if tiDependsUpon in fTabs then
    begin
      AddDependencies(nil, fDependsList);
      tvDependencies.FullExpand;
    end;
  finally
    tvDependencies.Items.EndUpdate;
  end;
end;

procedure TDependenciesDlg.tvDependenciesCollapsing(Sender: TObject;
  Node: TTreeNode; var AllowCollapse: Boolean);
  {Handles event triggered when tree view wants to collapse. We don't allow
  this.
    @param Sender [in] Not used.
    @param Node [in] Not used.
    @param AllowChange [in/out] Set false to prevent tree view collapsing.
  }
begin
  AllowCollapse := False;
end;

{ TDependenciesDlg.TTVDraw }

constructor TDependenciesDlg.TTVDraw.Create(
  const RootID: TSnippetID);
  {Class constructor. Sets up object.
    @param ID [in] ID snippet for which dependencies are displayed.
  }
begin
  inherited Create;
  fRootID := RootID;
end;

function TDependenciesDlg.TTVDraw.GetCollectionID(
  const Node: TTreeNode): TCollectionID;
begin
  if not Assigned(Node.Data) then
    Result := TCollectionID.CreateNull
  else
    Result := TSnippet(Node.Data).CollectionID;
end;

function TDependenciesDlg.TTVDraw.IsErrorNode(
  const Node: TTreeNode): Boolean;
  {Checks if a node represents an error condition.
    @param Node [in] Node to be checked.
    @return True if node represents error condition, False if not.
  }
begin
  Result := Assigned(Node.Data) and (TSnippet(Node.Data).ID = fRootID);
end;

//function TDependenciesDlg.TTVDraw.IsUserDefinedNode(
//  const Node: TTreeNode): Boolean;
//  {Checks if a node represents a user defined snippets object.
//    @param Node [in] Node to be checked.
//    @return True if node represents user defined object, False if not.
//  }
//begin
//  if not Assigned(Node.Data) then
//    Result := True
//  else
//    Result := TSnippet(Node.Data).UserDefined;
//end;

end.


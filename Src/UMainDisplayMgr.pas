{
 * This Source Code Form is subject to the terms of the Mozilla Public License,
 * v. 2.0. If a copy of the MPL was not distributed with this file, You can
 * obtain one at http://mozilla.org/MPL/2.0/
 *
 * Copyright (C) 2005-2013, Peter Johnson (www.delphidabbler.com).
 *
 * $Rev$
 * $Date$
 *
 * Class that manages and co-ordinates the display of the program's main UI.
 * Calls into subsidiary manager objects to perform display operations.
}


unit UMainDisplayMgr;


interface


uses
  // Project
  Compilers.UGlobals, IntfFrameMgrs, UView;


type
  ///  <summary>Enumeration that determines how view items are displayed in
  ///  detail pane.</summary>
  ///  <remarks>
  ///  <para>ddmverwrite - Displays view item in an existing tab if possible.
  ///  If view already displayed its tab is selected otherwise view overwrites
  ///  content of selected tab. If no tabs are displayed a new one is created.
  ///  </para>
  ///  <para>ddmRequestNewTab - Displays view item in a new tab unless it is
  ///  already displayed when it's tab is selected.</para>
  ///  <para>ddmForceNewTab - Always displays view item in a new tab.</para>
  ///  </remarks>
  TDetailPageDisplayMode = (
    ddmOverwrite,
    ddmRequestNewTab,
    ddmForceNewTab
  );

type
  ///  <summary>Enumeration that determines which tabs in detail pane are closed
  ///  by TMainDisplayManager when CloseDetailsTabs method is called.</summary>
  ///  <remarks>
  ///  <para>dtcSelected - Close selected tab only.</para>
  ///  <para>dtcAllExceptSelected - Close all open tabs excepted selected tab.
  ///  </para>
  ///  <para>dtcAll - Close all open tabs.</para>
  ///  </remarks>
  TDetailPageTabCloseOptions = (
    dtcSelected,
    dtcAllExceptSelected,
    dtcAll
  );

type
  ///  <summary>
  ///  Manages and co-ordinates the display of the program's main UI. Calls into
  ///  subsidiary manager objects to perform display operations.
  ///  </summary>
  TMainDisplayMgr = class(TObject)
  strict private
    var
      ///  <summary>Manager object for overview pane.</summary>
      fOverviewMgr: IInterface;
      ///  <summary>Manager object for details pane.</summary>
      fDetailsMgr: IInterface;
      ///  <summary>Records index of any detail pane tab that is being changed.
      ///  </summary>
      ///  <remarks>Must be set before the view is changed for use after it has
      ///  changed. May be used to refresh or delete a tab whose view has been
      ///  updated or deleted.</remarks>
      fChangingDetailPageIdx: Integer;
      ///  <summary>Flag that records whether a change to an exisiting view
      ///  object is in progress.</summary>
      ///  <remarks>When true a view object is in the process of being updated
      ///  or deleted.</remarks>
      fPendingViewChange: Boolean;
      ///  <summary>Flag that records whether database is in the process of
      ///  being updated or deleted.</summary>
      ///  <remarks>Will be true when fPendingViewChange is true, but will also
      ///  be true in other circumstances, such as when a view is to be added.
      ///  </remarks>
      fPendingChange: Boolean;

    ///  <summary>Redisplays the current grouping in overview pane's tree-view
    ///  using the snippets included in the current database query.</summary>
    procedure RedisplayOverview;

    ///  <summary>Creates a new tab in the detail pane and displays the given
    ///  view in it.</summary>
    procedure ShowInNewDetailPage(View: IView);

    ///  <summary>Displays given view in selected tab of detail pane.</summary>
    procedure DisplayInSelectedDetailView(View: IView);

    ///  <summary>Redisplays the current view in the selected detail pane tab,
    ///  if any.</summary>
    ///  <remarks>If there are no tabs in detail pane, its display is cleared.
    ///  </remarks>
    procedure RefreshDetailPage;

    ///  <summary>Makes preparations for a change in the database.</summary>
    ///  <remarks>Records any display information needed to update the display
    ///  after the change.</remarks>
    procedure PrepareForDBChange;

    ///  <summary>Makes preparations for a change to a given database view
    ///  object.</summary>
    ///  <remarks>Records any display information for given view that is
    ///  required to update display after the view has changed. Also makes
    ///  temporary view changes to avoid referencing a invalid view object after
    ///  the change.</remarks>
    procedure PrepareForDBViewChange(View: IView);

    ///  <summary>Handles database change events by updating the display as
    ///  necessary.</summary>
    ///  <param name="Sender">TObject [in] Object that triggered event. Not
    ///  used.</param>
    ///  <param name="EvtInfo">IInterface [in] Object that carries information
    ///  about the database change event.</param>
    procedure DBChangeEventHandler(Sender: TObject; const EvtInfo: IInterface);

    ///  <summary>Adds a view representing a new database object to the display.
    ///  </summary>
    ///  <param name="View">IView [in] View to be displayed.</param>
    ///  <remarks>This method *must* only be called after a call to
    ///  PrepareForDBChange has been made.</remarks>
    procedure AddDBView(View: IView);

    ///  <summary>Updates the display to reflect changes to a database view.
    ///  </summary>
    ///  <param name="TabIdx">Integer [in] Index of detail pane tab where view
    ///  is displayed. May be -1 if view not displayed in detail pane.</param>
    ///  <param name="View">IView [in] Changed database view to be updated in
    ///  display.</param>
    ///  <remarks>Some of the required display changes are completed in
    ///  PrepareForDBViewChange, which *must* have been called before this
    ///  method.</remarks>
    procedure UpdateDBView(TabIdx: Integer; View: IView);

    ///  <summary>Deletes a database view from display.</summary>
    ///  <param name="TabIdx">Integer [in] Index of detail pane tab that
    ///  contained view to be deleted.</param>
    ///  <remarks>Some of the required display changes are completed in
    ///  PrepareForDBViewChange, which *must* have been called before this
    ///  method.</remarks>
    procedure DeleteDBView(TabIdx: Integer);

    ///  <summary>Displays a view item.</summary>
    ///  <param name="ViewItem">IView [in] View item to be displayed.</param>
    ///  <param name="Mode">TDetailPageDisplayMode [in] Determines how view is
    ///  displayed in detail pane.</param>
    ///  <remarks>View item is selected in overview pane, if present, and shown
    ///  in detail pane.</remarks>
    procedure DisplayViewItem(ViewItem: IView; Mode: TDetailPageDisplayMode);
      overload;

  public
    ///  <summary>Object contructor. Sets up object to work with given frame
    ///  manager objects.</summary>
    ///  <param name="OverviewMgr">IInterface [in] Object that manages overview
    ///  frame.</param>
    ///  <param name="DetailsMgr">IInterface [in] Object that manages detail
    ///  frame.</param>
    constructor Create(const OverviewMgr, DetailsMgr: IInterface);

    ///  <summary>Object destructor. Tears down object.</summary>
    destructor Destroy; override;

    ///  <summary>Performs start-up initialisation of display.</summary>
    ///  <param name="OverviewGroupingIdx">Integer [in] Index of grouping to
    ///  be used by overview frame.</param>
    procedure Initialise(const OverviewGroupingIdx: Integer);

    ///  <summary>Re-starts display.</summary>
    ///  <remarks>All snippets in current query are shown in overview pane and
    ///  all detail pane tabs are closed.</remarks>
    procedure ReStart;

    ///  <summary>Display a view item.</summary>
    ///  <param name="ViewItem">IView [in] View item to be displayed.</param>
    ///  <param name="NewTab">Boolean [in] Determines if view is dislayed in
    ///  a new tab in detail pane.</param>
    ///  <remarks>
    ///  <para>View item is selected in overview pane, if present, and shown in
    ///  detail pane.</para>
    ///  <para>When new tab is requested one is created unless view item is
    ///  already displayed, when that tab is re-used.</para>
    ///  </remarks>
    procedure DisplayViewItem(ViewItem: IView; NewTab: Boolean); overload;

    ///  <summary>Refreshes display.</summary>
    ///  <remarks>Re-selects current view in overview pane and re-displays
    ///  current tab view in details pane, if any.</remarks>
    procedure Refresh;

    ///  <summary>Completely refreshes display.</summary>
    ///  <remarks>Forces a total redraw of overview pane and all tabs of details
    ///  pane.</remarks>
    procedure CompleteRefresh;

    ///  <summary>Updates display to show current query.</summary>
    ///  <remarks>Overview pane is re-displayed only if query has changed.
    ///  Detail pane is refreshed.</remarks>
    procedure UpdateDisplayedQuery;

    ///  <summary>Selects next tab in detail pane.</summary>
    ///  <remarks>Does nothing when there are less than 2 tabs.</remarks>
    procedure SelectNextDetailsTab;

    ///  <summary>Selects previous tab in detail pane.</summary>
    ///  <remarks>Does nothing when there are less than 2 tabs.</remarks>
    procedure SelectPreviousDetailsTab;

    ///  <summary>Closes one or more tabs in detail pane, according to value
    ///  of Options parameter.</summary>
    ///  <remarks>A new tab is selected, if there is one, and overview pane is
    ///  updated re change in selection.</remarks>
    procedure CloseDetailsTabs(const Option: TDetailPageTabCloseOptions);

    ///  <summary>Checks if it is possible to close any tab in details pane.
    ///  </summary>
    function CanCloseDetailsTab: Boolean;

    ///  <summary>Creates a new tab in detail pane.</summary>
    procedure CreateNewDetailsTab;

    ///  <summary>Displays welcome page in detail pane.</summary>
    ///  <remarks>A new tab is used unless welcome page is currently displayed
    ///  in which case its tab is selected.</remarks>
    procedure ShowWelcomePage;

    ///  <summary>Displays a new tab in detail pane that displays page informing
    ///  that the database has been updated.</summary>
    procedure ShowDBUpdatedPage;

    ///  <summary>Checks if a specified expand / collapse action on the overview
    ///  pane's treeview is permitted.</summary>
    ///  <param name="State">TTreeNodeAction [in] Required expand / collapse
    ///  action.</param>
    ///  <returns>Boolean. True if action can be performed.</returns>
    function CanUpdateOverviewTreeState(const State: TTreeNodeAction): Boolean;

    ///  <summary>Updates the expanded / collapsed state of nodes in the
    ///  overview pane's treeview control.</summary>
    ///  <param name="State">TTreeNodeAction [in] Required expand / collapse
    ///  action.</param>
    procedure UpdateOverviewTreeState(const State: TTreeNodeAction);

    ///  <summary>Checks whether copying to clipboard in selected detail tab is
    ///  possible.</summary>
    function CanCopy: Boolean;

    ///  <summary>Copies selected text in selected detail tab to clipboard.
    ///  </summary>
    procedure CopyToClipboard;

    ///  <summary>Checks whether selection of all text in selected detail tab is
    ///  possible.</summary>
    function CanSelectAll: Boolean;

    ///  <summary>Selects all text in current detail tab.</summary>
    procedure SelectAll;

    ///  <summary>Selects grouping with given index in overview pane.</summary>
    procedure SelectOverviewGrouping(GroupingIdx: Integer);

    ///  <summary>Returns index of current grouping in overview pane.</summary>
    function SelectedOverviewGrouping: Integer;

    ///  <summary>Selects tab with given index in detail pane.</summary>
    procedure SelectDetailTab(TabIdx: Integer);

    ///  <summary>Returns index of selected tab in detail pane.</summary>
    function SelectedDetailTab: Integer;

    ///  <summary>Returns currently selected view.</summary>
    ///  <remarks>
    ///  <para>This is the view displayed in selected tab in detail pane.</para>
    ///  <para>If detail pane has no tabs then null view is returned.</para>
    ///  </remarks>
    function CurrentView: IView;

    /// <summary>Prepares display ready for database to be reloaded.</summary>
    procedure PrepareForDBReload;
  end;


implementation


uses
  // Delphi
  SysUtils,
  // Project
  CS.Database.Types,
  DB.UMain,
  UBox,
  UPreferences,
  UQuery,
  UUtils;


{ TMainDisplayMgr }

procedure TMainDisplayMgr.AddDBView(View: IView);
begin
  Assert(fPendingChange, ClassName + '.AddView: no change pending');
  Assert(Supports(View, ISnippetView),
    ClassName + '.AddView: View not a snippet item');
  RedisplayOverview;
  (fOverviewMgr as IOverviewDisplayMgr).SelectItem(View);
  if Preferences.ShowNewSnippetsInNewTabs then
    DisplayViewItem(View, ddmForceNewTab)
  else
    DisplayViewItem(View, ddmOverwrite);
end;

function TMainDisplayMgr.CanCloseDetailsTab: Boolean;
begin
  Result := not (fDetailsMgr as IDetailPaneDisplayMgr).IsEmptyTabSet;
end;

function TMainDisplayMgr.CanCopy: Boolean;
begin
  Result := (fDetailsMgr as IClipboardMgr).CanCopy;
end;

function TMainDisplayMgr.CanSelectAll: Boolean;
begin
  Result := (fDetailsMgr as ISelectionMgr).CanSelectAll;
end;

function TMainDisplayMgr.CanUpdateOverviewTreeState(
  const State: TTreeNodeAction): Boolean;
begin
  Result := (fOverviewMgr as IOverviewDisplayMgr).CanUpdateTreeState(State);
end;

procedure TMainDisplayMgr.CloseDetailsTabs(
  const Option: TDetailPageTabCloseOptions);
begin
  case Option of
    dtcSelected:
      (fDetailsMgr as IDetailPaneDisplayMgr).CloseTab(
        (fDetailsMgr as IDetailPaneDisplayMgr).SelectedTab
      );
    dtcAllExceptSelected:
      (fDetailsMgr as IDetailPaneDisplayMgr).CloseMultipleTabs(True);
    dtcAll:
      (fDetailsMgr as IDetailPaneDisplayMgr).CloseMultipleTabs(False);
  end;
  (fOverviewMgr as IOverviewDisplayMgr).SelectItem(CurrentView);
  RefreshDetailPage;
end;

procedure TMainDisplayMgr.CompleteRefresh;
begin
  RedisplayOverview;
  RefreshDetailPage;
  (fDetailsMgr as IDetailPaneDisplayMgr).Reload;
end;

procedure TMainDisplayMgr.CopyToClipboard;
begin
  (fDetailsMgr as IClipboardMgr).CopyToClipboard;
end;

constructor TMainDisplayMgr.Create(const OverviewMgr, DetailsMgr: IInterface);
begin
  Assert(Assigned(OverviewMgr),
    ClassName + '.Create: OverviewMgr is nil');
  Assert(Supports(OverviewMgr, IPaneInfo),
    ClassName + '.Create: OverviewMgr must support IPaneInfo');
  Assert(Supports(OverviewMgr, IOverviewDisplayMgr),
    ClassName + '.Create: OverviewMgr must support IOverviewDisplayMgr');

  Assert(Assigned(DetailsMgr),
    ClassName + '.Create: DetailsMgr is nil');
  Assert(Supports(DetailsMgr, IPaneInfo),
    ClassName + '.Create: DetailsMgr must support IPaneInfo');
  Assert(Supports(DetailsMgr, IDetailPaneDisplayMgr),
    ClassName + '.Create: DetailsMgr must support IDetailPaneDisplayMgr');
  Assert(Supports(DetailsMgr, IClipboardMgr),
    ClassName + '.Create: DetailsMgr must support IClipboardMgr');
  Assert(Supports(DetailsMgr, ISelectionMgr),
    ClassName + '.Create: DetailsMgr must support IDetailPaneDisplayMgr');

  inherited Create;

  // Record subsidiary display managers
  fOverviewMgr := OverviewMgr;
  fDetailsMgr := DetailsMgr;
  Database.AddChangeEventHandler(DBChangeEventHandler);
end;

procedure TMainDisplayMgr.CreateNewDetailsTab;
var
  NewPageView: IView; // view item for new tab
begin
  // NOTE: Always uses new tab, even if view exists, by design
  NewPageView := TViewFactory.CreateNewTabView;
  ShowInNewDetailPage(NewPageView);
end;

function TMainDisplayMgr.CurrentView: IView;
begin
  Result := (fDetailsMgr as IDetailPaneDisplayMgr).SelectedView;
end;

procedure TMainDisplayMgr.DBChangeEventHandler(Sender: TObject;
  const EvtInfo: IInterface);
var
  EventInfo: IDatabaseChangeEventInfo;  // information about the event

  ///  <summary>Extracts snippet ID from EvtInfo object.</summary>
  function EvtInfoToSnippetID: TSnippetID;
  begin
    Result := (EventInfo.Info as TBox<TSnippetID>).Value;
  end;

  function EvtInfoToTag: TTag;
  begin
    Result := (EventInfo.Info as TBox<TTag>).Value;
  end;

begin
  EventInfo := EvtInfo as IDatabaseChangeEventInfo;
  // TODO -cDatabase: add support for changes to tags when such events are added
  case EventInfo.Kind of
    evChangeBegin:
      PrepareForDBChange;

    evChangeEnd:
      (fOverviewMgr as IOverviewDisplayMgr).EndUpdate;


    evBeforeSnippetChange, evBeforeSnippetDelete:
      PrepareForDBViewChange(
        TViewFactory.CreateSnippetView(EvtInfoToSnippetID)
      );

    evBeforeTagDelete:
      PrepareForDBViewChange(
        TViewFactory.CreateTagView(EvtInfoToTag)
      );

    evSnippetChanged:
      UpdateDBView(
        fChangingDetailPageIdx,
        TViewFactory.CreateSnippetView(EvtInfoToSnippetID)
      );

    evSnippetDeleted, evTagDeleted:
      DeleteDBView(fChangingDetailPageIdx);

    evSnippetAdded:
      AddDBView(TViewFactory.CreateSnippetView(EvtInfoToSnippetID));

  end;
end;

procedure TMainDisplayMgr.DeleteDBView(TabIdx: Integer);
begin
  Assert(fPendingViewChange, ClassName + '.DeleteView: no view change pending');
  try
    RedisplayOverview;
    if TabIdx >= 0 then
      // view is displayed in detail pane: close its tab
      (fDetailsMgr as IDetailPaneDisplayMgr).CloseTab(TabIdx);
    // current view may have changed due to view deletion, so refresh it
    RefreshDetailPage;
    (fOverviewMgr as IOverviewDisplayMgr).SelectItem(CurrentView);
  finally
    fPendingViewChange := False;
  end;
end;

destructor TMainDisplayMgr.Destroy;
begin
  Database.RemoveChangeEventHandler(DBChangeEventHandler);
  inherited;
end;

procedure TMainDisplayMgr.DisplayInSelectedDetailView(View: IView);
begin
  (fDetailsMgr as IDetailPaneDisplayMgr).Display(
    View, (fDetailsMgr as IDetailPaneDisplayMgr).SelectedTab
  );
end;

procedure TMainDisplayMgr.DisplayViewItem(ViewItem: IView; NewTab: Boolean);
const
  TabDisplayMap: array[Boolean] of TDetailPageDisplayMode = (
    ddmOverwrite, ddmRequestNewTab
  );
begin
  DisplayViewItem(ViewItem, TabDisplayMap[NewTab]);
end;

procedure TMainDisplayMgr.DisplayViewItem(ViewItem: IView;
  Mode: TDetailPageDisplayMode);
var
  TabIdx: Integer;  // index of details tab showing given view (-1 if no tab)
begin
  (fOverviewMgr as IOverviewDisplayMgr).SelectItem(ViewItem);
  if (fDetailsMgr as IDetailPaneDisplayMgr).IsEmptyTabSet
    or (Mode = ddmForceNewTab) then
  begin
    // no tabs => new tab
    ShowInNewDetailPage(ViewItem);
    Exit;
  end;
  Assert(Mode in [ddmOverwrite, ddmRequestNewTab], ClassName
    + '.DisplayViewItem: Mode in [ddmOverwrite, ddmRequestNewTab] expected');
  TabIdx := (fDetailsMgr as IDetailPaneDisplayMgr).FindTab(ViewItem.GetKey);
  if TabIdx >= 0 then
  begin
    // tab already exists for this view: switch to it
    (fDetailsMgr as IDetailPaneDisplayMgr).SelectTab(TabIdx);
    Exit;
  end;
  // tab doesn't already exist
  if Mode = ddmRequestNewTab then
  begin
    // new tab required
    ShowInNewDetailPage(ViewItem);
  end
  else
  begin
    Assert(Mode = ddmOverwrite,
      ClassName + '.DisplayViewItem: Mode = ddmOverwrite expected');
    // overwrite exiting tab
    DisplayInSelectedDetailView(ViewItem);
  end;
end;

procedure TMainDisplayMgr.Initialise(const OverviewGroupingIdx: Integer);
begin
  (fOverviewMgr as IOverviewDisplayMgr).Initialise(OverviewGroupingIdx);
end;

procedure TMainDisplayMgr.PrepareForDBChange;
begin
  fPendingChange := True;
  // save the state of the overview tree view ready for later restoration
  // and freeze updates until DB change complete
  (fOverviewMgr as IOverviewDisplayMgr).SaveTreeState;
  (fOverviewMgr as IOverviewDisplayMgr).BeginUpdate;
end;

procedure TMainDisplayMgr.PrepareForDBReload;
begin
  // save tree state so that correct state can been reloaded after update
  // completes
  (fOverviewMgr as IOverviewDisplayMgr).SaveTreeState;
  (fOverviewMgr as IOverviewDisplayMgr).Clear;
  (fDetailsMgr as IDetailPaneDisplayMgr).CloseMultipleTabs(False);
  RefreshDetailPage;  // deletes any displayed view item
end;

procedure TMainDisplayMgr.PrepareForDBViewChange(View: IView);
begin
  fChangingDetailPageIdx := (fDetailsMgr as IDetailPaneDisplayMgr).FindTab(
    View.GetKey
  );
  if (fChangingDetailPageIdx >= 0) and
    (
      fChangingDetailPageIdx =
        (fDetailsMgr as IDetailPaneDisplayMgr).SelectedTab
    )
    then
    begin
      // NOTE: Clear overview pane here to ensure no hanging references to
      // deleted views. Same principle applies to overwriting view in detail
      // pane.
      (fOverviewMgr as IOverviewDisplayMgr).Clear;
      DisplayInSelectedDetailView(TViewFactory.CreateNullView);
    end;
  fPendingViewChange := True;
end;

procedure TMainDisplayMgr.RedisplayOverview;
begin
  (fOverviewMgr as IOverviewDisplayMgr).Display(Query.Selection, True);
end;

procedure TMainDisplayMgr.Refresh;
begin
  // Redisplays current view in overview pane and current tab of detail pane
  (fOverviewMgr as IOverviewDisplayMgr).SelectItem(CurrentView);
  RefreshDetailPage;
end;

procedure TMainDisplayMgr.RefreshDetailPage;
begin
  if not (fDetailsMgr as IDetailPaneDisplayMgr).IsEmptyTabSet then
    DisplayInSelectedDetailView(CurrentView)
  else
    (fDetailsMgr as IDetailPaneDisplayMgr).Clear;
end;

procedure TMainDisplayMgr.ReStart;
begin
  // Clear all detail pane tabs and force re-displayed of overview
  (fDetailsMgr as IDetailPaneDisplayMgr).CloseMultipleTabs(False);
  (fOverviewMgr as IOverviewDisplayMgr).Display(Query.Selection, True);
end;

procedure TMainDisplayMgr.SelectAll;
begin
  // Only details pane supports text selection
  (fDetailsMgr as ISelectionMgr).SelectAll;
end;

procedure TMainDisplayMgr.SelectDetailTab(TabIdx: Integer);
begin
  (fDetailsMgr as IDetailPaneDisplayMgr).SelectTab(TabIdx);
  (fOverviewMgr as IOverviewDisplayMgr).SelectItem(
    (fDetailsMgr as IDetailPaneDisplayMgr).SelectedView
  );
end;

function TMainDisplayMgr.SelectedDetailTab: Integer;
begin
  Result := (fDetailsMgr as IDetailPaneDisplayMgr).SelectedTab;
end;

function TMainDisplayMgr.SelectedOverviewGrouping: Integer;
begin
  Result := (fOverviewMgr as IOVerviewDisplayMgr).SelectedGroupingIdx;
end;

procedure TMainDisplayMgr.SelectNextDetailsTab;
begin
  (fDetailsMgr as IDetailPaneDisplayMgr).NextTab;
end;

procedure TMainDisplayMgr.SelectOverviewGrouping(GroupingIdx: Integer);
begin
  (fOverviewMgr as IOVerviewDisplayMgr).SelectGrouping(GroupingIdx);
end;

procedure TMainDisplayMgr.SelectPreviousDetailsTab;
begin
  (fDetailsMgr as IDetailPaneDisplayMgr).PreviousTab;
end;

procedure TMainDisplayMgr.ShowDBUpdatedPage;
begin
  // NOTE: Normally this page is only shown when there are no tab displayed in
  // details pane.
  DisplayViewItem(TViewFactory.CreateDBUpdateInfoView, ddmForceNewTab);
end;

procedure TMainDisplayMgr.ShowInNewDetailPage(View: IView);
var
  NewTabIdx: Integer; // index of new detail pane tab
begin
  NewTabIdx := (fDetailsMgr as IDetailPaneDisplayMgr).CreateTab(View);
  (fDetailsMgr as IDetailPaneDisplayMgr).SelectTab(NewTabIdx);
end;

procedure TMainDisplayMgr.ShowWelcomePage;
begin
  DisplayViewItem(TViewFactory.CreateStartPageView, ddmRequestNewTab);
end;

procedure TMainDisplayMgr.UpdateDBView(TabIdx: Integer; View: IView);
begin
  Assert(fPendingViewChange, ClassName + '.UpdateView: no view change pending');
  try
    RedisplayOverview;
    if TabIdx >= 0 then
      // view is displayed in detail pane: update display
      (fDetailsMgr as IDetailPaneDisplayMgr).Display(
        View, TabIdx
      );
    if TabIdx <> SelectedDetailTab then
      // current view may reference changed view, so it is updated.
      RefreshDetailPage;
    (fOverviewMgr as IOverviewDisplayMgr).SelectItem(CurrentView);
  finally
    fPendingViewChange := False;
  end;
end;

procedure TMainDisplayMgr.UpdateDisplayedQuery;
begin
  // Update overview to show only found snippets
  (fOverviewMgr as IOverviewDisplayMgr).Display(Query.Selection, False);
  // Redisplay detail pane
  RefreshDetailPage;
end;

procedure TMainDisplayMgr.UpdateOverviewTreeState(const State: TTreeNodeAction);
begin
  (fOverviewMgr as IOverviewDisplayMgr).UpdateTreeState(State);
end;

end.


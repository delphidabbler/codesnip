{
 * UMainDisplayMgr.pas
 *
 * Class that manages and co-ordinates the display of the program's main UI.
 * Calls into subsidiary manager objects to perform display operations.
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
 * The Original Code is UMainDisplayMgr.pas
 *
 * The Initial Developer of the Original Code is Peter Johnson
 * (http://www.delphidabbler.com/).
 *
 * Portions created by the Initial Developer are Copyright (C) 2005-2011 Peter
 * Johnson. All Rights Reserved.
 *
 * Contributor(s)
 *   NONE
 *
 * ***** END LICENSE BLOCK *****
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

    ///  <summary>Creates and returns view object for a database object.
    ///  </summary>
    ///  <param name="DBObj">TObject [in] Database object for which view
    ///  required. Must be a valid databse object or nil.</param>
    ///  <returns>IView. Required view object. If DBObj is nil a null view is
    ///  returned.</returns>
    function DBEventInfoToView(EvtInfo: TObject): IView;

    ///  <summary>Gets reference to manager object for tab set that is currently
    ///  "interactive".</summary>
    ///  <returns>ITabbedDisplayMgr. Reference to required tab set manager
    ///  object, or nil if no tab set in interactive.</returns>
    ///  <remarks>Both overview and detail pane has a tab set. The tab set in
    ///  the frame that has (keyboard) focus is "interactive".</remarks>
    function GetInteractiveTabMgr: ITabbedDisplayMgr;

    ///  <summary>Redisplays the current grouping in overview pane's tree-view
    ///  using the snippets included in the current database query.</summary>
    procedure RedisplayOverview;

    ///  <summary>Creates a new tab in the detail pane and displays the given
    ///  view in it.</summary>
    procedure ShowInNewDetailPage(View: IView);

    ///  <summary>Displays given view in selected tab of detail pabe.</summary>
    procedure DisplayInSelectedDetailView(View: IView);

    ///  <summary>Redisplays the current view in the selected detail pane tab,
    ///  if any.</summary>
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

    ///  <summary>Updates that display to reflect changes to a database view.
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

  public
    ///  <summary>Object contructor. Sets up object to work with given frame
    ///  manager objects.</summary>
    constructor Create(const OverviewMgr, DetailsMgr: IInterface);
      {Class constructor.
        @param OverviewMgr [in] Manager object for Overview pane.
        @param DetailsMgr [in] Manager object for Details pane.
      }
    destructor Destroy; override;
      {Class destructor. Tears down object.
      }
    procedure Initialise;
      {Initialises display. All snippets in current query are shown in overview
      pane and detail pane is cleared.
      }

    ///  <summary>Clears whole display: overview pane is cleared and all detail
    ///  tabs are closed.</summary>
    procedure ClearAll;
    ///  <summary>Displays a view item.</summary>
    ///  <param name="ViewItem">IView [in] View item to be displayed.</param>
    ///  <param name="Mode">TDetailPageDisplayMode [in] Determines how view is
    ///  displayed in detail pane.</param>
    ///  <remarks>View item is selected in overview pane, if present, and shown
    ///  in detail pane.</remarks>
    procedure DisplayViewItem(ViewItem: IView; Mode: TDetailPageDisplayMode);
    ///  <summary>Refreshes display: re-selects current view in overview pane
    ///  re-displays current tab view in details pane, if any.</summary>
    procedure Refresh;
    procedure UpdateDisplayedQuery;
      {Notifies display manager that query current query has changed. Updates
      display to reflect changes.
      }
    procedure SelectNextActiveTab;
      {Selects next tab in currently active tab set. Does nothing if there is no
      active tab set.
      }
    procedure SelectPreviousActiveTab;
      {Selects previous tab in currently active tab set. Does nothing if there
      is no active tab set.
      }
    procedure CloseSelectedDetailsTab;
    function CanCloseDetailsTab: Boolean;
    procedure CreateNewDetailsTab;
    procedure ShowWelcomePage;
    procedure ShowDBUpdatedPage;
    function CanCopy: Boolean;
      {Checks whether copying to clipboard is currently supported.
        @return True if clipboard copying supported, false if not.
      }
    procedure UpdateOverviewTreeState(const State: TTreeNodeAction);
      {Updates the expanded / collapsed state of nodes in the overview pane's
      treeview control.
        @param State [in] Determines which expand / collapse action is to be
          performed.
      }
    function CanUpdateOverviewTreeState(const State: TTreeNodeAction): Boolean;
      {Checks if a sepcified expand / collapse action on the overview pane's
      treeview can be permitted.
        @param State [in] Specifies the expand / collapse action being queried.
      }
    procedure CopyToClipboard;
      {Copies selected text to clipboard.
      }
    function CanSelectAll: Boolean;
      {Checks whether selection of all text in current detail view is permitted.
        @return True if text can be selected, false otherwise.
      }
    procedure SelectAll;
      {Selects all text in current tab of detail pane.
      }
    function CurrentView: IView;
    procedure SelectOverviewTab(TabIdx: Integer);
    function SelectedOverviewTab: Integer;
    procedure SelectDetailTab(TabIdx: Integer);
    function SelectedDetailTab: Integer;
  end;


implementation


uses
  // Delphi
  SysUtils,
  // Project
  DB.UCategory, DB.UMain, DB.USnippet, UPreferences, UQuery;


{ TMainDisplayMgr }

procedure TMainDisplayMgr.AddDBView(View: IView);
begin
  Assert(fPendingChange, ClassName + '.AddView: no change pending');
  Assert(Supports(View, ISnippetView) or Supports(View, ICategoryView),
    ClassName + '.AddView: View not a database item');
  // todo: ensure this is called after PrepareForDBChange called.
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
  {Checks whether copying to clipboard is currently supported.
    @return True if clipboard copying supported, false if not.
  }
begin
  Result := (fDetailsMgr as IClipboardMgr).CanCopy;
end;

function TMainDisplayMgr.CanSelectAll: Boolean;
  {Checks whether selection of all text in current detail view is permitted.
    @return True if text can be selected, false otherwise.
  }
begin
  Result := (fDetailsMgr as ISelectionMgr).CanSelectAll;
end;

function TMainDisplayMgr.CanUpdateOverviewTreeState(
  const State: TTreeNodeAction): Boolean;
  {Checks if a sepcified expand / collapse action on the overview pane's
  treeview can be permitted.
    @param State [in] Specifies the expand / collapse action being queried.
  }
begin
  Result := (fOverviewMgr as IOverviewDisplayMgr).CanUpdateTreeState(State);
end;

procedure TMainDisplayMgr.ClearAll;
begin
  (fOverviewMgr as IOverviewDisplayMgr).Clear;
  (fDetailsMgr as IDetailPaneDisplayMgr).CloseMultipleTabs(False);
end;

procedure TMainDisplayMgr.CloseSelectedDetailsTab;
begin
  (fDetailsMgr as IDetailPaneDisplayMgr).CloseTab(
    (fDetailsMgr as ITabbedDisplayMgr).SelectedTab
  );
  (fOverviewMgr as IOverviewDisplayMgr).SelectItem(CurrentView);
  RefreshDetailPage;
end;

procedure TMainDisplayMgr.CopyToClipboard;
  {Copies selected text to clipboard.
  }
begin
  (fDetailsMgr as IClipboardMgr).CopyToClipboard;
end;

constructor TMainDisplayMgr.Create(const OverviewMgr, DetailsMgr: IInterface);
  {Class constructor. Sets up object to work with subsidiary manager objects.
    @param OverviewMgr [in] Manager object for Overview pane.
    @param DetailsMgr [in] Manager object for Details pane.
  }
begin
  Assert(Assigned(OverviewMgr),
    ClassName + '.Create: OverviewMgr is nil');
  Assert(Supports(OverviewMgr, ITabbedDisplayMgr),
    ClassName + '.Create: OverviewMgr must support ITabbedDisplayMgr');
  Assert(Supports(OverviewMgr, IPaneInfo),
    ClassName + '.Create: OverviewMgr must support IPaneInfo');
  Assert(Supports(OverviewMgr, IOverviewDisplayMgr),
    ClassName + '.Create: OverviewMgr must support IOverviewDisplayMgr');

  Assert(Assigned(DetailsMgr),
    ClassName + '.Create: DetailsMgr is nil');
  Assert(Supports(DetailsMgr, ITabbedDisplayMgr),
    ClassName + '.Create: DetailsMgr must support ITabbedDisplayMgr');
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
begin
  EventInfo := EvtInfo as IDatabaseChangeEventInfo;
  case EventInfo.Kind of
    evChangeBegin:
      PrepareForDBChange;

    evBeforeSnippetChange, evBeforeSnippetDelete,
    evBeforeCategoryChange, evBeforeCategoryDelete:
      PrepareForDBViewChange(DBEventInfoToView(EventInfo.Info));

    evSnippetChanged, evCategoryChanged:
      UpdateDBView(fChangingDetailPageIdx, DBEventInfoToView(EventInfo.Info));

    evSnippetDeleted, evCategoryDeleted:
      DeleteDBView(fChangingDetailPageIdx);

    evSnippetAdded, evCategoryAdded:
      AddDBView(DBEventInfoToView(EventInfo.Info));
  end;
end;

function TMainDisplayMgr.DBEventInfoToView(EvtInfo: TObject): IView;
begin
  // TODO: Move this to TViewItemFactory as CreateDBView method (param=DBObj)?
  Result := nil;
  if not Assigned(EvtInfo) then
    Result := TViewFactory.CreateNulView
  else if EvtInfo is TSnippet then
    Result := TViewFactory.CreateSnippetView(EvtInfo as TSnippet)
  else if EvtInfo is TCategory then
    Result := TViewFactory.CreateCategoryView(EvtInfo as TCategory);
  Assert(Assigned(Result), ClassName + '.DBEventInfoToView: Result is nil');
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
  {Class destructor. Tears down object.
  }
begin
  Database.RemoveChangeEventHandler(DBChangeEventHandler);
  inherited;
end;

procedure TMainDisplayMgr.DisplayInSelectedDetailView(View: IView);
begin
  (fDetailsMgr as IDetailPaneDisplayMgr).Display(
    View, (fDetailsMgr as ITabbedDisplayMgr).SelectedTab
  );
end;

procedure TMainDisplayMgr.DisplayViewItem(ViewItem: IView;
  Mode: TDetailPageDisplayMode);
var
  TabIdx: Integer;
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

function TMainDisplayMgr.GetInteractiveTabMgr: ITabbedDisplayMgr;
  {Gets reference to manager object for interactive tab set.
    @return Required tab manager or nil if no tab is interactive.
  }
begin
  Result := nil;
  if (fOverviewMgr as IPaneInfo).IsInteractive then
    Result := fOverviewMgr as ITabbedDisplayMgr
  else if (fDetailsMgr as IPaneInfo).IsInteractive then
    Result := fDetailsMgr as ITabbedDisplayMgr;
end;

procedure TMainDisplayMgr.Initialise;
  {Initialises display. All snippets in cuurent query are shown in overview pane
  and detail pane is cleared.
  }
begin
  // Clear all tabs and force re-displayed of overview
  (fDetailsMgr as IDetailPaneDisplayMgr).CloseMultipleTabs(False);
  (fOverviewMgr as IOverviewDisplayMgr).Display(Query.Selection, True);
end;

procedure TMainDisplayMgr.PrepareForDBChange;
  {Makes preparations for a change in the database.
  }
begin
  fPendingChange := True;
  // simply save the state of the overview tree view ready for later restoration
  (fOverviewMgr as IOverviewDisplayMgr).SaveTreeState;
end;

procedure TMainDisplayMgr.PrepareForDBViewChange(View: IView);
begin
  fChangingDetailPageIdx := (fDetailsMgr as IDetailPaneDisplayMgr).FindTab(
    View.GetKey
  );
  if (fChangingDetailPageIdx >= 0) and
    (fChangingDetailPageIdx = (fDetailsMgr as ITabbedDisplayMgr).SelectedTab)
    then
    begin
      // NOTE: Clear overview pane here to ensure no hanging references to
      // deleted views. Same principle applies to overwriting view in detail
      // pane.
      (fOverviewMgr as IOverviewDisplayMgr).Clear;
      DisplayInSelectedDetailView(TViewFactory.CreateNulView);
    end;
  fPendingViewChange := True;
end;

procedure TMainDisplayMgr.RedisplayOverview;
begin
  (fOverviewMgr as IOverviewDisplayMgr).Display(Query.Selection, True);
end;

procedure TMainDisplayMgr.Refresh;
begin
  // Redisplays current view in overview pane and active tab of detail pane
  (fOverviewMgr as IOverviewDisplayMgr).SelectItem(CurrentView);
  RefreshDetailPage;
end;

procedure TMainDisplayMgr.RefreshDetailPage;
begin
  if not (fDetailsMgr as IDetailPaneDisplayMgr).IsEmptyTabSet then
    DisplayInSelectedDetailView(CurrentView);
end;

procedure TMainDisplayMgr.SelectAll;
  {Selects all text in current tab of detail pane.
  }
begin
  // Only details pane supports text selection
  (fDetailsMgr as ISelectionMgr).SelectAll;
end;

procedure TMainDisplayMgr.SelectDetailTab(TabIdx: Integer);
begin
  (fDetailsMgr as ITabbedDisplayMgr).SelectTab(TabIdx);
  (fOverviewMgr as IOverviewDisplayMgr).SelectItem(
    (fDetailsMgr as IDetailPaneDisplayMgr).SelectedView
  );
end;

function TMainDisplayMgr.SelectedDetailTab: Integer;
begin
  Result := (fDetailsMgr as ITabbedDisplayMgr).SelectedTab;
end;

function TMainDisplayMgr.SelectedOverviewTab: Integer;
begin
  Result := (fOverviewMgr as ITabbedDisplayMgr).SelectedTab;
end;

procedure TMainDisplayMgr.SelectNextActiveTab;
  {Selects next tab in currently active tab set. Does nothing if there is no
  active tab set.
  }
var
  TabMgr: ITabbedDisplayMgr;  // reference to active tab manager object
begin
  TabMgr := GetInteractiveTabMgr;
  if Assigned(TabMgr) then
    TabMgr.NextTab;
end;

procedure TMainDisplayMgr.SelectOverviewTab(TabIdx: Integer);
begin
  (fOverviewMgr as ITabbedDisplayMgr).SelectTab(TabIdx);
end;

procedure TMainDisplayMgr.SelectPreviousActiveTab;
  {Selects previous tab in currently active tab set. Does nothing if there is no
  active tab set.
  }
var
  TabMgr: ITabbedDisplayMgr;  // reference to active tab manager object
begin
  TabMgr := GetInteractiveTabMgr;
  if Assigned(TabMgr) then
    TabMgr.PreviousTab;
end;

procedure TMainDisplayMgr.ShowDBUpdatedPage;
begin
  // NOTE: Normally this page is only shown when there are no tabs displayed
  DisplayViewItem(TViewFactory.CreateDBUpdateInfoView, ddmForceNewTab);
end;

procedure TMainDisplayMgr.ShowInNewDetailPage(View: IView);
var
  NewTabIdx: Integer;
begin
  NewTabIdx := (fDetailsMgr as IDetailPaneDisplayMgr).CreateTab(View);
  (fDetailsMgr as ITabbedDisplayMgr).SelectTab(NewTabIdx);
end;

procedure TMainDisplayMgr.ShowWelcomePage;
begin
  // TODO: May want a user option for welcome page to overwrite or use new tab
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
  {Updates the expanded / collapsed state of nodes in the overview pane's
  treeview control.
    @param State [in] Determines which expand / collapse action is to be
      performed.
  }
begin
  (fOverviewMgr as IOverviewDisplayMgr).UpdateTreeState(State);
end;

end.


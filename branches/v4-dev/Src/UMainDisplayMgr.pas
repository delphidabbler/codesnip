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

  {
  TMainDisplayMgr:
    Manages and co-ordinates the display of the program's main UI. Calls into
    subsidiary manager objects to perform display operations.
  }
  TMainDisplayMgr = class(TObject)
  strict private
    fOverviewMgr: IInterface;     // Manager object for overview pane
    fDetailsMgr: IInterface;      // Manager object for details pane
    fDetailPagePendingChange: Integer;
    function GetInteractiveTabMgr: ITabbedDisplayMgr;
      {Gets reference to manager object for interactive tab set.
        @return Required tab manager or nil if no tab is interactive.
      }
    function GetSelectedOverviewTab: Integer;
      {Read accessor for SelectedOverviewTab property. Gets currently selected
      overview tab.
        @return Index of selected tab.
      }
    procedure SetSelectedOverviewTab(const Value: Integer);
      {Write accessor for SelectedOverviewTab property. Selects required tab.
        @param Value [in] Index of tab to be selected.
      }
    function GetSelectedDetailTab: Integer;
      {Read accessor for SelectedDetailTab property. Gets currently selected
      overview tab.
        @return Index of selected tab.
      }
    procedure SetSelectedDetailTab(const Value: Integer);
      {Write accessor for SelectedDetailTab property. Selects required tab.
        @param Value [in] Index of tab to be selected.
      }
    function GetCurrentView: IView;
    procedure DisplayInSelectedDetailView(View: IView);
    procedure RedisplayOverview;
    procedure ShowInNewDetailPage(View: IView);
    procedure DisplayNewSnippetOrCategory(View: IView);
    procedure RefreshDetailPage;
  public
    constructor Create(const OverviewMgr, DetailsMgr: IInterface);
      {Class constructor. Sets up object to work with subsidiary manager
      objects.
        @param OverviewMgr [in] Manager object for Overview pane.
        @param DetailsMgr [in] Manager object for Details pane.
      }
    destructor Destroy; override;
      {Class destructor. Tears down object.
      }
    procedure Initialise;
      {Initialises display. All snippets in database are shown in overview pane
      and detail pane is cleared.
      }

    procedure SnippetAdded(View: IView);
    procedure SnippetChanged(View: IView);
    procedure SnippetDeleted;
    procedure CategoryAdded(View: IView);
    procedure CategoryChanged(View: IView);
    procedure CategoryDeleted;
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
    procedure QueryUpdated;
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
    procedure PrepareForChange;
      {Makes preparations for a change in the database display.
      }
    procedure PrepareForViewChange(View: IView);
    property CurrentView: IView
      read GetCurrentView;
      {Information about currently displayed view}
    property SelectedOverviewTab: Integer
      read GetSelectedOverviewTab write SetSelectedOverviewTab;
      {Gets and sets an overview tab. Writing the property selects the required
      tab}
    property SelectedDetailTab: Integer
      read GetSelectedDetailTab write SetSelectedDetailTab;
      {Gets and sets a detail pane tab. Writing the property selects the
      required tab}
  end;


implementation


uses
  // Delphi
  SysUtils,
  // Project
  UPreferences, UQuery;


{ TMainDisplayMgr }

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

procedure TMainDisplayMgr.CategoryAdded(View: IView);
begin
  // TODO: extract method from this and SnippetAdded code: identical code
  DisplayNewSnippetOrCategory(View);
end;

procedure TMainDisplayMgr.CategoryChanged(View: IView);
begin
  RedisplayOverview;
  if fDetailPagePendingChange >= 0 then
    // category is displayed in detail pane: update display
    (fDetailsMgr as IDetailPaneDisplayMgr).Display(
      View, fDetailPagePendingChange
    );
  (fOverviewMgr as IOverviewDisplayMgr).SelectItem(CurrentView);
end;

procedure TMainDisplayMgr.CategoryDeleted;
begin
  RedisplayOverview;
  if fDetailPagePendingChange >= 0 then
    // category is displayed in detail pane: close its tab
    (fDetailsMgr as IDetailPaneDisplayMgr).CloseTab(fDetailPagePendingChange);
  (fOverviewMgr as IOverviewDisplayMgr).SelectItem(CurrentView);
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
end;

procedure TMainDisplayMgr.CreateNewDetailsTab;
var
  NewPageView: IView; // view item for new tab
begin
  // NOTE: Always uses new tab, even if view exists, by design
  NewPageView := TViewItemFactory.CreateNewTabView;
  ShowInNewDetailPage(NewPageView);
end;

destructor TMainDisplayMgr.Destroy;
  {Class destructor. Tears down object.
  }
begin
  inherited;
end;

procedure TMainDisplayMgr.DisplayInSelectedDetailView(View: IView);
begin
  (fDetailsMgr as IDetailPaneDisplayMgr).Display(
    View, (fDetailsMgr as ITabbedDisplayMgr).SelectedTab
  );
end;

procedure TMainDisplayMgr.DisplayNewSnippetOrCategory(View: IView);
begin
  Assert(Supports(View, ISnippetView) or Supports(View, ICategoryView),
    ClassName + '.DisplayNewSnippetOrCategory: View not snippet or category');
  RedisplayOverview;
  (fOverviewMgr as IOverviewDisplayMgr).SelectItem(View);
  if Preferences.ShowNewSnippetsInNewTabs then
    DisplayViewItem(View, ddmForceNewTab)
  else
    DisplayViewItem(View, ddmOverwrite);
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

function TMainDisplayMgr.GetCurrentView: IView;
begin
  Result := (fDetailsMgr as IDetailPaneDisplayMgr).SelectedView;
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

function TMainDisplayMgr.GetSelectedDetailTab: Integer;
  {Read accessor for SelectedDetailTab property. Gets currently selected
  detail tab.
    @return Index of selected tab.
  }
begin
  Result := (fDetailsMgr as ITabbedDisplayMgr).SelectedTab;
end;

function TMainDisplayMgr.GetSelectedOverviewTab: Integer;
  {Read accessor for SelectedOverviewTab property. Gets currently selected
  overview tab.
    @return Index of selected tab.
  }
begin
  Result := (fOverviewMgr as ITabbedDisplayMgr).SelectedTab;
end;

procedure TMainDisplayMgr.Initialise;
  {Initialises display. All snippets in database are shown in overview pane and
  detail pane is cleared.
  }
begin
  // Clear all tabs and force re-displayed of overview
  (fDetailsMgr as IDetailPaneDisplayMgr).CloseMultipleTabs(False);
  (fOverviewMgr as IOverviewDisplayMgr).Display(Query.Selection, True);
end;

procedure TMainDisplayMgr.PrepareForChange;
  {Makes preparations for a change in the database display.
  }
begin
  // simply save the state of the overview tree view ready for later restoration
  (fOverviewMgr as IOverviewDisplayMgr).SaveTreeState;
end;

procedure TMainDisplayMgr.PrepareForViewChange(View: IView);
begin
  // TODO: Change IDetailPaneDisplayMgr.FindTab to take IView parameter
  fDetailPagePendingChange := (fDetailsMgr as IDetailPaneDisplayMgr).FindTab(
    View.GetKey
  );
  if (fDetailPagePendingChange >= 0) and
    (fDetailPagePendingChange = (fDetailsMgr as ITabbedDisplayMgr).SelectedTab)
    then
    begin
      // TODO: Check if overview pane actually needs clearing here
      (fOverviewMgr as IOverviewDisplayMgr).Clear;
      DisplayInSelectedDetailView(TViewItemFactory.CreateNulView);
    end;
end;

procedure TMainDisplayMgr.QueryUpdated;
  // TODO: rename this method to something like UpdateSelectionFromQuery
  {Notifies display manager that current query has changed. Updates display to
  reflect changes.
  }
begin
  // Update overview to show only found snippets
  (fOverviewMgr as IOverviewDisplayMgr).Display(Query.Selection, False);
  // Redisplay detail pane
  RefreshDetailPage;
end;

procedure TMainDisplayMgr.RedisplayOverview;
begin
  // TODO: Note calling SaveTreeState here could be buggy
  (fOverviewMgr as IOverviewDisplayMgr).SaveTreeState;
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

procedure TMainDisplayMgr.SetSelectedDetailTab(const Value: Integer);
  {Write accessor for SelectedDetailTab property. Selects required tab.
    @param Value [in] Index of tab to be selected.
  }
begin
  (fDetailsMgr as ITabbedDisplayMgr).SelectTab(Value);
  (fOverviewMgr as IOverviewDisplayMgr).SelectItem(
    (fDetailsMgr as IDetailPaneDisplayMgr).SelectedView
  );
end;

procedure TMainDisplayMgr.SetSelectedOverviewTab(const Value: Integer);
  {Write accessor for SelectedOverviewTab property. Selects required tab.
    @param Value [in] Index of tab to be selected.
  }
begin
  (fOverviewMgr as ITabbedDisplayMgr).SelectTab(Value);
end;

procedure TMainDisplayMgr.ShowDBUpdatedPage;
begin
  // NOTE: Normally this page is only shown when there are no tabs displayed
  DisplayViewItem(TViewItemFactory.CreateDBUpdateInfoView, ddmForceNewTab);
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
  DisplayViewItem(TViewItemFactory.CreateStartPageView, ddmRequestNewTab);
end;

procedure TMainDisplayMgr.SnippetAdded(View: IView);
begin
  DisplayNewSnippetOrCategory(View);
end;

procedure TMainDisplayMgr.SnippetChanged(View: IView);
begin
  // We assume that edited snippet is being displayed in selected detail tab,
  // because only way of editing a snippet is from this tab.
  RedisplayOverview;
  (fOverviewMgr as IOverviewDisplayMgr).SelectItem(View);
  DisplayInSelectedDetailView(View);
end;

procedure TMainDisplayMgr.SnippetDeleted;
begin
  RedisplayOverview;
  (fDetailsMgr as IDetailPaneDisplayMgr).CloseTab(
    (fDetailsMgr as ITabbedDisplayMgr).SelectedTab
  );
  // NOTE: closing tab triggers a notification to display a different tab which
  // then takes care of updating view
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


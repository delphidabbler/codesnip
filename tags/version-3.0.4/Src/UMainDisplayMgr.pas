{
 * UMainDisplayMgr.pas
 *
 * Class that manages and co-ordinates the display of the program's main UI.
 * Calls into subsidiary manager objects to perform display operations.
 *
 * v0.1 of 30 Jan 2005  - Original version.
 * v0.2 of 17 Mar 2005  - Exposed object providing detailed information about
 *                        currently selected view item as property.
 * v0.3 of 28 Jan 2006  - Added new Refresh method that redisplays current view
 *                        item if displayed in compiler check pane.
 * v1.0 of 24 May 2006  - Improved and corrected comments.
 * v1.1 of 23 Nov 2006  - Added method that shows / hides test units in Compiler
 *                        Check tab.
 *                      - Added property to get id of currently selected tab in
 *                        detail pane.
 *                      - Changed remaining call to .Free to calls to
 *                        FreeAndNil.
 * v1.2 of 02 Dec 2006  - Modified to work with revised Compiler Check Frame
 *                        manager.
 *                      - Modified DisplayCompileResults to call compiler check
 *                        pane's new dynamic compilation results display code.
 * v1.3 of 03 Feb 2007  - Changed Filter method to return number of records
 *                        found rather than a boolean.
 * v1.4 of 04 Feb 2007  - Changed to work with new Query global object and to
 *                        handle removal of now redundant TDetailView class:
 *                        - Removed Filter method and replaced with QueryUpdated
 *                          method to be called when query changes.
 *                        - Removed fSelectedRoutines field and CurrentSearch
 *                          property: required information now in query object.
 *                        - Replaced all other references to TDetailView objects
 *                          with either calls to query object or to TViewItem
 *                          objects.
 *                        - Replaced CurrentDetailView and CurrentViewItem
 *                          properties with CurrentView property.
 * v1.5 of 11 Feb 2007  - Deleted unused SelectDetailTab property and accessor
 *                        method.
 *                      - Deleted unused ActiveWebBrowser property and accessor
 *                        method.
 *                      - Added new public SelectNextActiveTab and
 *                        SelectPreviousActiveTab methods and supporting code.
 * v1.6 of 16 Feb 2007  - Added new Clear method to clear overview and detail
 *                        panes and modified Initialise method to use Clear.
 * v2.0 of 16 Feb 2007  - Major update. Two major changes were:
 *                        - This TMainDisplayMgr no longer manipulates and
 *                          interogates the information and compiler check panes
 *                          of the details frame. TDetailFrame now supports all
 *                          relevant interfaces and dispatches requests and
 *                          commands to the relevant child pane. Therefore this
 *                          code was revised to communicate only with
 *                          TDetailFrame and TOverviewFrame. All calls that
 *                          required knowledges of the child panes was changed
 *                          to make calls on TDetailFrame.
 *                        - Revised to work with redefined and new display
 *                          interfaces from IntfFrameMgrs and fact that these
 *                          interfaces no longer form a heirachy.
 *                          TMainDisplayMgr casts the managed frames to the
 *                          appropriate interfaces when calling their methods.
 * v2.1 of 05 Jun 2008  - Replaced methods use to select tabs in overview and
 *                        details panes with properties that both select tabs
 *                        and get index of selected tabs.
 *                      - Initialisation code no longer selects Information pane
 *                        but leaves selected pane visible.
 * v2.2 of 09 Jan 2009  - TMainDisplayMgr.DisplayCompileResults now has a
 *                        parameter that specifies compilers object whose
 *                        results are to be displayed.
 * v2.3 of 06 Jun 2009  - Removed TMainDisplayMgr.ShowTestUnit method.
 *                      - Added new TMainDisplayMgr UpdateOverviewTreeState and
 *                        CanUpdateOverviewTreeState methods.
 *
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
 * Portions created by the Initial Developer are Copyright (C) 2005-2009 Peter
 * Johnson. All Rights Reserved.
 *
 * ***** END LICENSE BLOCK *****
}


unit UMainDisplayMgr;


interface


uses
  // Project
  IntfCompilers, IntfFrameMgrs, UView;


type

  {
  TMainDisplayMgr:
    Manages and co-ordinates the display of the program's main UI. Calls into
    subsidiary manager objects to perform display operations.
  }
  TMainDisplayMgr = class(TObject)
  private
    fOverviewMgr: IInterface;
      {Manager object for overview pane}
    fDetailsMgr: IInterface;
      {Manager object for details pane}
    fCurrentView: TViewItem;
      {Value of CurrentView property}
    procedure InternalDisplayViewItem(const ViewItem: TViewItem;
      const Force: Boolean);
      {Displays a view item. Updates current view item, selects item in overview
      if possible and displays full details in detail pane. Optionally forces
      redisplay of compiler pane.
        @param ViewItem [in] Item to be displayed (may be nil).
        @param Force [in] True if compiler pane redisplay is to be forced.
      }
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
    procedure Clear;
      {Clears the main display, i.e. overview and detail panes.
      }
    procedure DisplayViewItem(const ViewItem: TViewItem);
      {Displays a view item. Updates current view item, selects item in overview
      if possible and displays full details in detail pane.
        @param ViewItem [in] Item to be displayed (may be nil).
      }
    procedure Refresh;
      {Refreshes current display, forcing redisplay of info and compiler check
      panes.
      }
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
    procedure DisplayCompileResults(const ACompilers: ICompilers);
      {Displays results of a test compilation in Compiler Check tab of Details
      pane.
        @param ACompilers [in] Compilers object containing required results.
      }
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
    property CurrentView: TViewItem
      read fCurrentView;
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
  UQuery;


{ TMainDisplayMgr }

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

procedure TMainDisplayMgr.Clear;
  {Clears the main display, i.e. overview and detail panes.
  }
begin
  DisplayViewItem(nil);
  (fOverviewMgr as IOverviewDisplayMgr).Clear;
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
  Assert(Assigned(OverviewMgr), 'TMainDisplayMgr.Create: OverviewMgr is nil');
  Assert(Assigned(DetailsMgr), 'TMainDisplayMgr.Create: DetailsMgr is nil');
  inherited Create;
  // Record subsidiary display managers
  fOverviewMgr := OverviewMgr;
  fDetailsMgr := DetailsMgr;
  // Create owned view object: stores current view
  fCurrentView := TViewItem.Create;
end;

destructor TMainDisplayMgr.Destroy;
  {Class destructor. Tears down object.
  }
begin
  FreeAndNil(fCurrentView);
  inherited;
end;

procedure TMainDisplayMgr.DisplayCompileResults(const ACompilers: ICompilers);
  {Displays results of a test compilation.
    @param ACompilers [in] Compilers object containing required results.
  }
begin
 (fDetailsMgr as ICompCheckDisplayMgr).DisplayCompileResults(ACompilers);
end;

procedure TMainDisplayMgr.DisplayViewItem(const ViewItem: TViewItem);
  {Displays a view item. Updates current view item, selects item in overview if
  possible and displays full details in detail pane.
    @param ViewItem [in] Item to be displayed (may be nil).
  }
begin
  InternalDisplayViewItem(ViewItem, False);
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
  overview tab.
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
  // First we clear the display
  Clear;
  // Now we display current query in overview pane
  QueryUpdated;
end;

procedure TMainDisplayMgr.InternalDisplayViewItem(const ViewItem: TViewItem;
  const Force: Boolean);
  {Displays a view item. Updates current view item, selects item in overview
  if possible and displays full details in detail pane. Optionally forces
  redisplay of compiler pane.
    @param ViewItem [in] Item to be displayed (may be nil).
    @param Force True [in] if compiler pane redisplay is to be forced.
  }
begin
  // Record view item
  fCurrentView.Assign(ViewItem);
  // Select item in overview pane and display in detail pane
  (fOverviewMgr as IOverviewDisplayMgr).SelectItem(fCurrentView);
  (fDetailsMgr as IViewItemDisplayMgr).Display(fCurrentView, Force);
end;

procedure TMainDisplayMgr.QueryUpdated;
  {Notifies display manager that current query has changed. Updates display to
  reflect changes.
  }
begin
  // Update overview to show only found snippets
  (fOverviewMgr as IOverviewDisplayMgr).Display(Query.Selection);
  // Force redisplay of current view item (overview list may not have it and it
  // may need search items highlighting or unhighlighting in detail view)
  Refresh;
end;

procedure TMainDisplayMgr.Refresh;
  {Refreshes current display, forcing redisplay of info and compiler check
  panes.
  }
begin
  InternalDisplayViewItem(fCurrentView, True);
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
end;

procedure TMainDisplayMgr.SetSelectedOverviewTab(const Value: Integer);
  {Write accessor for SelectedOverviewTab property. Selects required tab.
    @param Value [in] Index of tab to be selected.
  }
begin
  (fOverviewMgr as ITabbedDisplayMgr).SelectTab(Value);
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


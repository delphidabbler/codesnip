{
 * FrDetail.pas
 *
 * Title frame that displays, and manages user interaction with, the detail pane
 * tabs.
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
 * The Original Code is FrDetail.pas
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


unit FrDetail;


interface


uses
  // Delphi
  Forms, ComCtrls, ExtCtrls, Controls, StdCtrls, Classes, ActiveX, ActnList,
  ImgList, Generics.Collections,
  // Project
  Compilers.UGlobals, FrTitled, FrBrowserBase, FrDetailView, IntfFrameMgrs,
  IntfNotifier, UCommandBars, UView;


type

  {
  TDetailFrame:
    Title frame that displays and manages user interaction with the detail
    pane tabs. The frame implements display manager interfaces and notifies
    application of user-initiated events via a notifier object.
  }
  TDetailFrame = class(TTitledFrame,
    ITabbedDisplayMgr,                    // uses tabs to select different views
    IEditableTabbedDisplayMgr,           // enables tabs to be added and deleted
    IViewItemDisplayMgr,                                 // displays a view item
    IPaneInfo,                                // provides information about pane
    IWBCustomiser,                             // customises web browser control
    IClipboardMgr,                                          // clipboard manager
    ISelectionMgr,                                          // selection manager
    ISetNotifier,                                        // sets notifier object
    ICommandBarConfig                               // command bar configuration
  )
    pcDetail: TPageControl;
    procedure pcDetailChange(Sender: TObject);
  strict private
    type
      TCommandBarItem = record
        Action: TCustomAction;
        ID: TCommandBarID;
        constructor Create(AAction: TCustomAction; AID: TCommandBarID);
      end;
    type
      TCommandBarItems = TList<TCommandBarItem>;
  strict private
    fExternal: IDispatch;
    fImages: TCustomImageList;
    fNotifier: INotifier;
    fCommandBarItems: TCommandBarItems;
      {Object used to notify application of user-initiated events}
    function TabToPane(const TabIdx: Integer): IInterface;
      {Gets reference to manager object of frame associated with a tab.
        @param TabIdx [in] Index of tab.
        @return Required frame manager object reference.
      }
    function SelectedPane: IInterface;
      {Gets reference to manager object of frame in currently selected tab.
        @return Reference to frame's manager object.
      }
    procedure DeactivateBrowserPanes;
      {Deactivates all child panes containing browser controls.
      }
    function TabCount: Integer;
      // TODO: comment this method
    function IsEmptyTabSet: Boolean;
      // TODO: comment this method
    // TODO: comment this method
    procedure CloseSelectedTab;
    // TODO: comment this method
    procedure CloseAllTabs(ExcludeSelected: Boolean);
  protected // interface implementations
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
    ///  <summary>Creates new tab and returns its index.</summary>
    ///  <remarks>Method of IEditableTabbedDisplayMgr.</remarks>
    function NewTab: Integer;
    // TODO: comment this method
    procedure CloseTabs(const Action: TCloseTabAction);
    // TODO: comment this method
    function CanCloseSelectedTab: Boolean;
    { IViewItemDisplayMgr }
    ///  <summary>Displays a given view in currently selected tab.</summary>
    ///  <param name="View">IView [in] View to be displayed.</param>
    ///  <param name="Force">Boolean [in[ Forces view item to be (re)displayed
    ///  even if view has not changed.</param>
    ///  <remarks>Method of IViewItemDisplayMgr.</remarks>
    procedure Display(View: IView; const Force: Boolean = False);
    // TODO: Comment this method
    function GetCurrentView: IView;
    { IPaneInfo }
    function IsInteractive: Boolean;
      {Checks if the pane is currently interactive with user.
        @return True if pane is interactive, False if not.
      }
    { IWBCustomiser }
    procedure SetExternalObj(Obj: IDispatch);
      {Provides an object to be used to extend a web browsers' external objects.
        @param Obj [in] External object extender.
      }
    procedure SetDragDropHandler(Obj: IDropTarget);
      {Provides an object to be used by web browser controls to handle drag-drop
      operations.
        @param Obj [in] Drag-drop handler.
      }
    { IClipboard }
    function CanCopy: Boolean;
      {Checks whether text can be copied to clipboard from current child frame.
        @return True if text can be copied.
      }
    procedure CopyToClipboard;
      {Copies selected text from current child frame to clipboard.
      }
    { ISelectionMgr }
    function CanSelectAll: Boolean;
      {Checks whether text can be selected in current child frame.
        @return True if text can be selected.
      }
    procedure SelectAll;
      {Selects all text in active control of current child frame.
      }
    { ISetNotifier }
    procedure SetNotifier(const Notifier: INotifier);
      {Sets the object's notifier object to be called in response to user input.
        @param Notifier [in] Required Notifier object.
      }
    { ICommandBarConfig }
    procedure AddAction(const Action: TCustomAction;
      const ID: TCommandBarID);
      {Adds an action item to a command bar. Passes request to all subsidiary
      tabs.
        @param Action [in] Action to be associated with menu item.
        @param ID [in] Specifies command bar to add action to.
      }
    procedure AddSpacer(const ID: TCommandBarID);
      {Adds a spacer to a command bar. Passes request to all subsidiary tabs.
        @param Kind [in] Specifies command bar to add spacer to.
      }
    procedure SetImages(const Images: TCustomImageList);
      {Sets image list to be used by command bars. Passes on to all subsidiary
      tabs.
        @param Images [in] Image list to be used.
      }
    public
      constructor Create(AOwner: TComponent); override;
      destructor Destroy; override;
  end;


implementation


uses
  // Delphi
  SysUtils,
  // Project
  UExceptions;


{$R *.dfm}


{ TDetailFrame }

procedure TDetailFrame.AddAction(const Action: TCustomAction;
  const ID: TCommandBarID);
  {Adds an action item to a command bar. Passes request to all subsidiary tabs.
    @param Action [in] Action to be associated with menu item.
    @param ID [in] Specifies command bar to add action to.
  }
var
  Idx: Integer; // loops through all tabsheets
begin
  fCommandBarItems.Add(TCommandBarItem.Create(Action, ID));
  for Idx := 0 to Pred(TabCount) do
    if Supports(TabToPane(Idx), ICommandBarConfig) then
      (TabToPane(Idx) as ICommandBarConfig).AddAction(Action, ID);
end;

procedure TDetailFrame.AddSpacer(const ID: TCommandBarID);
  {Adds a spacer to a command bar. Passes request to all subsidiary tabs.
    @param Kind [in] Specifies command bar to add spacer to.
  }
var
  Idx: Integer; // loops through all tabsheets
begin
  fCommandBarItems.Add(TCommandBarItem.Create(nil, ID));
  for Idx := 0 to Pred(TabCount) do
    if Supports(TabToPane(Idx), ICommandBarConfig) then
      (TabToPane(Idx) as ICommandBarConfig).AddSpacer(ID);
end;

function TDetailFrame.CanCloseSelectedTab: Boolean;
begin
  Result := not IsEmptyTabSet;
end;

function TDetailFrame.CanCopy: Boolean;
  {Checks whether text can be copied to clipboard from current child frame.
    @return True if text can be copied.
  }
begin
  // Get clipboard manager for current tab and check if it supports copying
  if not IsEmptyTabSet and Supports(SelectedPane, IClipboardMgr) then
    Result := (SelectedPane as IClipboardMgr).CanCopy
  else
    Result := False;
end;

function TDetailFrame.CanSelectAll: Boolean;
  {Checks whether text can be selected in current child frame.
    @return True if text can be selected.
  }
begin
  // Get selection manager for current tab and check if it supports selection
  if not IsEmptyTabSet and Supports(SelectedPane, ISelectionMgr) then
    Result := IsInteractive and (SelectedPane as ISelectionMgr).CanSelectAll
  else
    Result := False;
end;

procedure TDetailFrame.CloseAllTabs(ExcludeSelected: Boolean);
var
  Idx: Integer;                   // loops thru all tabs in page control
  SelectedTab: TTabSheet;         // references currently selected tab
  CloseTabList: TList<TTabSheet>; // list of tabs to close
  ClosingTab: TTabSheet;          // each tab to be closed
begin
  if IsEmptyTabSet then
    Exit;
  SelectedTab := pcDetail.ActivePage;
  CloseTabList := TList<TTabSheet>.Create;
  try
    for Idx := 0 to Pred(pcDetail.PageCount) do
    begin
      if not ExcludeSelected or (pcDetail.Pages[Idx] <> SelectedTab) then
        CloseTabList.Add(pcDetail.Pages[Idx]);
    end;
    for ClosingTab in CloseTabList do
      ClosingTab.Free;
  finally
    CloseTabList.Free;
  end;
  if IsEmptyTabSet then
    Exit;
  Assert(pcDetail.PageCount = 1, ClassName + '.CloseAllTabs: 1 tab expected');
  SelectTab(0);
end;

procedure TDetailFrame.CloseSelectedTab;
var
  ClosingTab: TTabSheet;
  ClosingTabIdx: Integer;
begin
  if IsEmptyTabSet then
    Exit;
  ClosingTabIdx := SelectedTab;
  ClosingTab := pcDetail.ActivePage;
  ClosingTab.Free;
  if IsEmptyTabSet then
    Exit;
  // Select another tab
  if ClosingTabIdx >= TabCount then
    SelectTab(Pred(TabCount))
  else
    SelectTab(ClosingTabIdx);
end;

procedure TDetailFrame.CloseTabs(const Action: TCloseTabAction);
begin
  case Action of
    ctaSelected:
      CloseSelectedTab;
    ctaAll:
      CloseAllTabs(False);
    ctaAllExceptSelected:
      CloseAllTabs(True);
  end;
end;

procedure TDetailFrame.CopyToClipboard;
  {Copies selected text from current child frame to clipboard.
  }
begin
  // Ask clipboard manager for current tab to perform copying
  if Supports(SelectedPane, IClipboardMgr) then
    (SelectedPane as IClipboardMgr).CopyToClipboard;
end;

constructor TDetailFrame.Create(AOwner: TComponent);
begin
  inherited;
  fCommandBarItems := TCommandBarItems.Create;
end;

procedure TDetailFrame.DeactivateBrowserPanes;
  {Deactivates all child panes containing browser controls.
  }
var
  Idx: Integer; // loops through all tabsheets
begin
  for Idx := 0 to Pred(TabCount) do
    if Supports(TabToPane(Idx), IWBDisplayMgr) then
      (TabToPane(Idx) as IWBDisplayMgr).Deactivate;
end;

destructor TDetailFrame.Destroy;
begin
  fCommandBarItems.Free;
  inherited;
end;

procedure TDetailFrame.Display(View: IView; const Force: Boolean);
begin
  if IsEmptyTabSet then
    pcDetail.ActivePageIndex := NewTab;
  (SelectedPane as IViewItemDisplayMgr).Display(View, Force);
  pcDetail.ActivePage.Caption := View.Description;
end;

function TDetailFrame.GetCurrentView: IView;
begin
  Result := (SelectedPane as IViewItemDisplayMgr).GetCurrentView;
end;

function TDetailFrame.IsEmptyTabSet: Boolean;
begin
  Result := TabCount = 0;
end;

function TDetailFrame.IsInteractive: Boolean;
  {Checks if the pane is currently interactive with user.
    @return True if pane is interactive, False if not.
  }
var
  Idx: Integer; // loops thru frame's owned components
begin
  // This frame is interactive if one of owned control has focus or if one
  // of contained frames is interactive
  Result := False;
  for Idx := 0 to Pred(ComponentCount) do
  begin
    if Supports(Components[Idx], IPaneInfo) then
      Result := (Components[Idx] as IPaneInfo).IsInteractive
    else if (Components[Idx] is TWinControl) then
      Result := (Components[Idx] as TWinControl).Focused;
    if Result then
      Break;
  end;
end;

function TDetailFrame.NewTab: Integer;
var
  TS: TTabSheet;
  Frame: TDetailViewFrame;
  Cmd: TCommandBarItem;
begin
  // create page control
  TS := TTabSheet.Create(pcDetail);
  TS.PageControl := pcDetail;
  Result := TS.PageIndex;
  // create a frame parented by tab sheet
  Frame := TDetailViewFrame.Create(Self);
  Frame.Name := ''; // important to avoid name clashes
  Assert(Supports(Frame, IViewItemDisplayMgr),
    ClassName + '.NewTab: New frame does not support IViewItemDisplayMgr');
  Frame.Parent := TS;
  Frame.Align := alClient;
  if Supports(Frame, IWBCustomiser) then
    (Frame as IWBCustomiser).SetExternalObj(fExternal);
  if Supports(Frame, ICommandBarConfig) then
    (Frame as ICommandBarConfig).SetImages(fImages);
  if Supports(Frame, ICommandBarConfig) then
  begin
    for Cmd in fCommandBarItems do
    begin
      if Assigned(Cmd.Action) then
        (Frame as ICommandBarConfig).AddAction(Cmd.Action, Cmd.ID)
      else
        (Frame as ICommandBarConfig).AddSpacer(Cmd.ID);
    end;
  end;
  SelectTab(Result);
end;

procedure TDetailFrame.NextTab;
  {Switches to next tab, or return to first tab if current tab is last.
  }
begin
  if SelectedTab = Pred(TabCount) then
    SelectTab(0)
  else
    SelectTab(Succ(SelectedTab));
end;

procedure TDetailFrame.pcDetailChange(Sender: TObject);
  {Handles event triggered when detail page changes. Notifies application of
  display style change via notifier object.
    @param Sender [in] Not used.
  }
begin
  SelectTab(pcDetail.ActivePageIndex);
//  fNotifier.ChangeDetailPane(pcDetail.ActivePageIndex);
end;

procedure TDetailFrame.PreviousTab;
  {Switches to previous tab, or return to last tab if current tab is first.
  }
begin
  if SelectedTab = 0 then
    SelectTab(Pred(TabCount))
  else
    SelectTab(Pred(SelectedTab));
end;

procedure TDetailFrame.SelectAll;
  {Selects all text in active control of current child frame.
  }
begin
  // Ask selection manager for current tab to perform selection
  if Supports(SelectedPane, ISelectionMgr) then
    (SelectedPane as ISelectionMgr).SelectAll;
end;

function TDetailFrame.SelectedPane: IInterface;
  {Gets reference to manager object of frame in currently selected tab.
    @return Reference to frame's manager object.
  }
begin
  Result := TabToPane(SelectedTab);
end;

function TDetailFrame.SelectedTab: Integer;
  {Returns index of currently selected tab.
    @return Tab index.
  }
begin
  Result := pcDetail.ActivePageIndex;
end;

procedure TDetailFrame.SelectTab(const TabIdx: Integer);
  {Selects tab with specified index.
    @param TabIdx [in] Tab to be selected.
  }
begin
  Assert((TabIdx >= 0) and (TabIdx < TabCount),
    ClassName + '.SelectTab: TabIdx out range');
//  if TabIdx = pcDetail.ActivePageIndex then
//    Exit;
  DeactivateBrowserPanes;
  pcDetail.ActivePageIndex := TabIdx;
  (SelectedPane as IWBDisplayMgr).Activate;
  if Assigned(fNotifier) then
    fNotifier.ShowViewItem(
      (SelectedPane as IViewItemDisplayMgr).GetCurrentView
    );
end;

procedure TDetailFrame.SetDragDropHandler(Obj: IDropTarget);
  {Provides an object to be used by web browser controls to handle drag-drop
  operations.
    @param Obj [in] Drag-drop handler.
  }
var
  Idx: Integer;
begin
  for Idx := 0 to Pred(TabCount) do
    if Supports(TabToPane(Idx), IWBCustomiser) then
      (TabToPane(Idx) as IWBCustomiser).SetDragDropHandler(Obj);
end;

procedure TDetailFrame.SetExternalObj(Obj: IDispatch);
  {Provides an object to be used to extend a web browsers' external objects.
    @param Obj [in] External object extender.
  }
var
  Idx: Integer; // loops thru all tabsheets
begin
  fExternal := Obj;
  for Idx := 0 to Pred(TabCount) do
    if Supports(TabToPane(Idx), IWBCustomiser) then
      (TabToPane(Idx) as IWBCustomiser).SetExternalObj(Obj);
end;

procedure TDetailFrame.SetImages(const Images: TCustomImageList);
  {Sets image list to be used by command bars. Passes on to all subsidiary tabs.
    @param Images [in] Image list to be used.
  }
var
  Idx: Integer; // loops through all tabsheets
begin
  fImages := Images;
  for Idx := 0 to Pred(TabCount) do
    if Supports(TabToPane(Idx), ICommandBarConfig) then
      (TabToPane(Idx) as ICommandBarConfig).SetImages(Images);
end;

procedure TDetailFrame.SetNotifier(const Notifier: INotifier);
  {Sets the object's notifier object to be called in response to user input.
    @param Notifier [in] Required Notifier object.
  }
begin
  fNotifier := Notifier;
end;

function TDetailFrame.TabCount: Integer;
begin
  Result := pcDetail.PageCount;
end;

function TDetailFrame.TabToPane(const TabIdx: Integer): IInterface;
  {Gets reference to manager object of frame associated with a tab.
    @param TabIdx [in] Index of tab.
    @return Required frame manager object reference.
  }
var
  TS: TTabSheet;
  Frame: TDetailViewFrame;
  CtrlIdx: Integer;
begin
  Assert(TabIdx >= 0, ClassName + '.TabToPane: TabIdx < 0');
  Assert(TabIdx < TabCount, ClassName + '.TabToPane: TabIdx too large');
  TS := pcDetail.Pages[TabIdx];
  Frame := nil;
  for CtrlIdx := 0 to Pred(TS.ControlCount) do
  begin
    if (TS.Controls[CtrlIdx] is TDetailViewFrame) then
    begin
      Frame := TS.Controls[CtrlIdx] as TDetailViewFrame;
      Break;
    end;
  end;
  Assert(Assigned(Frame), ClassName + '.TabToPane: Frame not found');
  Result := Frame as IInterface;
end;

{ TDetailFrame.TCommandBarItem }

constructor TDetailFrame.TCommandBarItem.Create(AAction: TCustomAction;
  AID: TCommandBarID);
begin
  Action := AAction;
  ID := AID;
end;

end.


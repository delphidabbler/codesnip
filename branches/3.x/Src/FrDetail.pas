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
 * Portions created by the Initial Developer are Copyright (C) 2005-2012 Peter
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
  ImgList,
  // Project
  Compilers.UGlobals, FrTitled, FrCompCheck, FrBrowserBase, FrDetailView,
  FrInfo, IntfFrameMgrs, IntfNotifier, UCommandBars, UView;


type

  {
  TDetailFrame:
    Title frame that displays and manages user interaction with the detail
    pane tabs. The frame implements display manager interfaces and notifies
    application of user-initiated events via a notifier object.
  }
  TDetailFrame = class(TTitledFrame,
    ITabbedDisplayMgr,                    // uses tabs to select different views
    IViewItemDisplayMgr,                                 // displays a view item
    ICompCheckDisplayMgr,              // displays compile results and test unit
    IPaneInfo,                                // provides information about pane
    IWBCustomiser,                             // customises web browser control
    IClipboardMgr,                                          // clipboard manager
    ISelectionMgr,                                          // selection manager
    ISetNotifier,                                        // sets notifier object
    ICommandBarConfig                               // command bar configuration
  )
    pcDetail: TPageControl;
    tsInfo: TTabSheet;
    tsCompiler: TTabSheet;
    frmInfo: TInfoFrame;
    frmCompCheck: TCompCheckFrame;
    procedure pcDetailChange(Sender: TObject);
    procedure pcDetailMouseDown(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer);
  private
    fNotifier: INotifier;
      {Object used to notify application of user-initiated events}
    function TabToPane(const TabIdx: Integer): IInterface;
      {Gets reference to manager object of frame associated with a tab.
        @param TabIdx [in] Index of tab.
        @return Required frame manager object reference.
        @except EBug raised if tab index is out of range.
      }
    function SelectedPane: IInterface;
      {Gets reference to manager object of frame in currently selected tab.
        @return Reference to frame's manager object.
      }
    procedure DeactivateBrowserPanes;
      {Deactivates all child panes containing browser controls.
      }
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
    { IViewItemDisplayMgr }
    procedure Display(const View: TViewItem; const Force: Boolean = False);
      {Displays compiler support information for a view item in all child panes.
        @param View [in] Information about view item to be displayed.
        @param Force [in] Forces view item to be re-displayed even if not
          changed.
      }
    { ICompCheckDisplayMgr }
    procedure DisplayCompileResults(const ACompilers: ICompilers);
      {Displays results of test compilation in compiler check pane.
        @param ACompilers [in] Compilers object containing required results.
      }
    { IPaneInfo }
    function IsInteractive: Boolean;
      {Checks if the pane is currently interactive with user.
        @return True if pane is interactive, False if not.
      }
    { IWBCustomiser }
    procedure SetExternalObj(const Obj: IDispatch);
      {Provides an object to be used to extend a web browsers' external objects.
        @param Obj [in] External object extender.
      }
    procedure SetDragDropHandler(const Obj: IDropTarget);
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
  for Idx := 0 to Pred(pcDetail.PageCount) do
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
  for Idx := 0 to Pred(pcDetail.PageCount) do
    if Supports(TabToPane(Idx), ICommandBarConfig) then
      (TabToPane(Idx) as ICommandBarConfig).AddSpacer(ID);
end;

function TDetailFrame.CanCopy: Boolean;
  {Checks whether text can be copied to clipboard from current child frame.
    @return True if text can be copied.
  }
begin
  // Get clipboard manager for current tab and check if it supports copying
  if Supports(SelectedPane, IClipboardMgr) then
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
  if Supports(SelectedPane, ISelectionMgr) then
    Result := IsInteractive and (SelectedPane as ISelectionMgr).CanSelectAll
  else
    Result := False;
end;

procedure TDetailFrame.CopyToClipboard;
  {Copies selected text from current child frame to clipboard.
  }
begin
  // Ask clipboard manager for current tab to perform copying
  if Supports(SelectedPane, IClipboardMgr) then
    (SelectedPane as IClipboardMgr).CopyToClipboard;
end;

procedure TDetailFrame.DeactivateBrowserPanes;
  {Deactivates all child panes containing browser controls.
  }
var
  Idx: Integer; // loops through all tabsheets
begin
  for Idx := 0 to Pred(pcDetail.PageCount) do
    if Supports(TabToPane(Idx), IWBDisplayMgr) then
      (TabToPane(Idx) as IWBDisplayMgr).Deactivate;
end;

procedure TDetailFrame.Display(const View: TViewItem;
  const Force: Boolean);
  {Displays compiler support information for a view item in all child panes.
    @param View [in] Information about view item to be displayed.
    @param Force [in] Forces view item to be re-displayed even if not changed.
  }
var
  Idx: Integer; // loops through all tabsheets
begin
  for Idx := 0 to Pred(pcDetail.PageCount) do
    if Supports(TabToPane(Idx), IViewItemDisplayMgr) then
      (TabToPane(Idx) as IViewItemDisplayMgr).Display(View, Force);
  if Assigned(fNotifier) then
    fNotifier.ShowHint('');   // ugly fix for bug #3577408
end;

procedure TDetailFrame.DisplayCompileResults(const ACompilers: ICompilers);
  {Displays results of test compilation in compiler check pane.
    @param ACompilers [in] Compilers object containing required results.
  }
begin
  if SelectedTab <> cCompCheckTab then
    fNotifier.ChangeDetailPane(cCompCheckTab);
  (TabToPane(cCompCheckTab) as ICompCheckDisplayMgr).DisplayCompileResults(
    ACompilers
  );
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

procedure TDetailFrame.NextTab;
  {Switches to next tab, or return to first tab if current tab is last.
  }
begin
  if SelectedTab = Pred(pcDetail.PageCount) then
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
  fNotifier.ChangeDetailPane(pcDetail.ActivePageIndex);
end;

procedure TDetailFrame.pcDetailMouseDown(Sender: TObject; Button: TMouseButton;
  Shift: TShiftState; X, Y: Integer);
  {Handles event triggered when user clicks on one of page control tabs. Ensures
  page control has focus. This does always happen automatically.
    @param Sender [in] Not used.
    @param Button [in] Not used.
    @param Shift [in] Not used.
    @param X [in] X co-ordinate of mouse in client co-ordinates.
    @param Y [in] Y co-ordinate of mouse in client co-ordinates.
  }
begin
  if htOnItem in pcDetail.GetHitTestInfoAt(X, Y) then
    pcDetail.SetFocus;
end;

procedure TDetailFrame.PreviousTab;
  {Switches to previous tab, or return to last tab if current tab is first.
  }
begin
  if SelectedTab = 0 then
    SelectTab(Pred(pcDetail.PageCount))
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
  Assert((TabIdx >= 0) and (TabIdx < pcDetail.PageCount),
    ClassName + '.SelectTab: TabIdx out range');
  DeactivateBrowserPanes;
  pcDetail.ActivePageIndex := TabIdx;
  (SelectedPane as IWBDisplayMgr).Activate;
end;

procedure TDetailFrame.SetDragDropHandler(const Obj: IDropTarget);
  {Provides an object to be used by web browser controls to handle drag-drop
  operations.
    @param Obj [in] Drag-drop handler.
  }
var
  Idx: Integer;
begin
  for Idx := 0 to Pred(pcDetail.PageCount) do
    if Supports(TabToPane(Idx), IWBCustomiser) then
      (TabToPane(Idx) as IWBCustomiser).SetDragDropHandler(Obj);
end;

procedure TDetailFrame.SetExternalObj(const Obj: IDispatch);
  {Provides an object to be used to extend a web browsers' external objects.
    @param Obj [in] External object extender.
  }
var
  Idx: Integer; // loops thru all tabsheets
begin
  for Idx := 0 to Pred(pcDetail.PageCount) do
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
  for Idx := 0 to Pred(pcDetail.PageCount) do
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

function TDetailFrame.TabToPane(const TabIdx: Integer): IInterface;
  {Gets reference to manager object of frame associated with a tab.
    @param TabIdx [in] Index of tab.
    @return Required frame manager object reference.
    @except EBug raised if tab index is out of range.
  }
begin
  case TabIdx of
    cInfoTab: Result := frmInfo;
    cCompCheckTab: Result := frmCompCheck;
    else raise EBug.Create(ClassName + '.TabToPane: tab index out of range');
  end;
end;

end.


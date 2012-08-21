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
  ImgList, Generics.Collections,
  // Project
  Compilers.UGlobals, FrTitled, FrBrowserBase, FrDetailView, IntfFrameMgrs,
  IntfNotifier, UCommandBars, UView, Menus;

type

  {
    TDetailFrame:
    Title frame that displays and manages user interaction with the detail
    pane tabs. The frame implements display manager interfaces and notifies
    application of user-initiated events via a notifier object.
    }
  TDetailFrame = class(TTitledFrame,
    ITabbedDisplayMgr,
    IPaneInfo,
    IDetailPaneDisplayMgr,
    IWBCustomiser,
    ISetNotifier,
    ICommandBarConfig,
    ISelectionMgr,
    IClipboardMgr
  )
    frmDetailView: TDetailViewFrame;
    mnuTabs: TPopupMenu;
    tcViews: TTabControl;
    procedure tcViewsChange(Sender: TObject);
    procedure tcViewsMouseDown(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer);
  strict private
    var
      ///  <summary>Notification object used to notify other parts of program
      ///  about changes in this frame.</summary>
      fNotifier: INotifier;
      // TODO: remove following unused field
      fDisplayedView: IView;
      ///  <summary>List of views associated with tabs.</summary>
      ///  <remarks>Index of a view in this list is same as index of its tab
      ///  in tab set.</remarks>
      fViews: TList<IView>;
      ///  <summary>Command bar that wraps tab set's context menu.</summary>
      fTabSetCmdBar: TPopupMenuWrapper;
    ///  <summary>Returns number of tabs currently displayed.</summary>
    function TabCount: Integer;
    ///  <summary>Selects tab at given index and displays its associated view.
    ///  </summary>
    procedure InternalSelectTab(TabIdx: Integer);
    ///  <summary>Displays given view in deatil view pane. If ForceReload is
    ///  True then view is totally reloaded, otherwise it is displayed normally.
    ///  </summary>
    procedure InternalDisplay(View: IView; ForceReload: Boolean);
    ///  <summary>Deletes a tab at given index along with its associated view.
    ///  </summary>
    procedure InternalDeleteTab(TabIdx: Integer);
    ///  <summary>Sends notification via notifier object that selected tab has
    ///  changed.</summary>
    procedure NotifyTabChange;
  public
    ///  <summary>Constructs frame and its owned objects.</summary>
    constructor Create(AOwner: TComponent); override;
    ///  <summary>Destroys frame and its owned objects.</summary>
    destructor Destroy; override;

    ///  <summary>Selects tab with given index and displays associated view.
    ///  </summary>
    ///  <remarks>Method of ITabbedDisplayMgr and IDetailPaneDisplayMgr.
    ///  </remarks>
    procedure SelectTab(const TabIdx: Integer);
    procedure ITabbedDisplayMgr.SelectTab = SelectTab;
    procedure IDetailPaneDisplayMgr.SelectTab = SelectTab;

    ///  <summary>Gets index of currently selected tab.</summary>
    ///  <remarks>Method of ITabbedDisplayMgr.</remarks>
    function SelectedTab: Integer;

    ///  <summary>Switches to next tab in sequence or goes to first tab if
    ///  current tab is last.</summary>
    ///  <remarks>Method of ITabbedDisplayMgr.</remarks>
    procedure NextTab;

    ///  <summary>Switches to previous tab in sequence or goes to last tab if
    ///  current tab is first.</summary>
    ///  <remarks>Method of ITabbedDisplayMgr.</remarks>
    procedure PreviousTab;

    ///  <summary>Checks if the frame, or one of its child controls, is
    ///  currently interactive with the user.</summary>
    ///  <remarks>Method of IPaneInfo.</remarks>
    function IsInteractive: Boolean;

    ///  <summary>Returns view associated with currently selected tab.</summary>
    ///  <remarks>
    ///  <para>If tab set is empty, a null view is returned.</para>
    ///  <para>Method of IDetailPaneDisplayMgr.</para>
    ///  </remarks>
    function SelectedView: IView;

    ///  <summary>Finds index of tab displaying given view or -1 if no such tab.
    ///  </summary>
    ///  <remarks>Method of IDetailPaneDisplayMgr.</remarks>
    function FindTab(ViewKey: IViewKey): Integer;

    ///  <summary>Associates given view with tab at given index.</summary>
    ///  <remarks>
    ///  <para>If specified tab is currently selected the view is displayed.
    ///  </para>
    ///  <para>Method of IDetailPaneDisplayMgr.</para>
    ///  </remarks>
    procedure Display(View: IView; const TabIdx: Integer);

    ///  <summary>Reloads the currently displayed view.</summary>
    ///  <remarks>
    ///  <para>If tab set is empty, null view is reloaded.</para>
    ///  <para>Method of IDetailPaneDisplayMgr.</para>
    ///  </remarks>
    procedure Reload;

    ///  <summary>Closes all tabs and deletes all views.</summary>
    ///  <remarks>Method of IDetailPaneDisplayMgr.</remarks>
    procedure Clear;

    ///  <summary>Creates a new tab displaying given view and returns its index.
    ///  </summary>
    ///  <remarks>Method of IDetailPaneDisplayMgr.</remarks>
    function CreateTab(View: IView): Integer;

    ///  <summary>Checks if tab set is empty, i.e. there are no tabs displayed.
    ///  </summary>
    ///  <remarks>Method of IDetailPaneDisplayMgr.</remarks>
    function IsEmptyTabSet: Boolean;

    ///  <summary>Closes tab at given index.</summary>
    ///  <remarks>
    ///  <para>If closed tab was selected, another tab is selected and its view
    ///  displayed, otherwise currently selected tab remains unchanged.</para>
    ///  <para>Method of IDetailPaneDisplayMgr.</para>
    ///  </remarks>
    procedure CloseTab(const TabIdx: Integer);

    ///  <summary>Closes all tabs, or all except selected tabs, depending on
    ///  whether KeepSelected is False or True, respectively.</summary>
    ///  <remarks>Method of IDetailPaneDisplayMgr.</remarks>
    procedure CloseMultipleTabs(const KeepSelected: Boolean);

    ///  <summary>Uses the given object to extend the browser control's
    ///  'external object'.</summary>
    ///  <remarks>Method of IWBCustomiser.</remarks>
    procedure SetExternalObj(Obj: IDispatch);

    ///  <summary>Uses the given object to handle drag-drop operations for the
    ///  browser control.</summary>
    ///  <remarks>Method of IWBCustomiser.</remarks>
    procedure SetDragDropHandler(Obj: IDropTarget);

    ///  <summary>Sets the notifier object to be called in reponse to user
    ///  input.</summary>
    ///  <remarks>Method of ISetNotifier.</remarks>
    procedure SetNotifier(const Notifier: INotifier);

    ///  <summary>Adds given action to command bar with given ID.</summary>
    ///  <remarks>Method of ICommandBarConfig.</remarks>
    procedure AddAction(const Action: TCustomAction; const ID: TCommandBarID);
      overload;

    ///  <summary>Adds given action to command bars with given IDs.</summary>
    ///  <remarks>Method of ICommandBarConfig.</remarks>
    procedure AddAction(const Action: TCustomAction; const IDs: TCommandBarIDs);
      overload;

    ///  <summary>Adds a spacer to command bar with given ID.</summary>
    ///  <remarks>Method of ICommandBarConfig.</remarks>
    procedure AddSpacer(const ID: TCommandBarID); overload;

    ///  <summary>Adds a spacer to command bars with given IDs.</summary>
    ///  <remarks>Method of ICommandBarConfig.</remarks>
    procedure AddSpacer(const IDs: TCommandBarIDs); overload;

    ///  <summary>Specifies image list to be used by all hostes command bars.
    ///  </summary>
    ///  <remarks>Method of ICommandBarConfig.</remarks>
    procedure SetImages(const Images: TCustomImageList);

    ///  <summary>Checks whether any text can be selected in any selected view.
    ///  </summary>
    ///  <remarks>Method of ISelectionMgr.</remarks>
    function CanSelectAll: Boolean;

    ///  <summary>Selects all text in any current view.</summary>
    ///  <remarks>Method of ISelectionMgr.</remarks>
    procedure SelectAll;

    ///  <summary>Checks whether anything can currently be copied to the
    ///  clipboard.</summary>
    ///  <remarks>Method of IClipboardMgr.</remarks>
    function CanCopy: Boolean;

    ///  <summary>Copies any selected text in current view to clipboard.
    ///  </summary>
    ///  <remarks>Method of IClipboardMgr.</remarks>
    procedure CopyToClipboard;
  end;


implementation


uses
  // Delphi
  SysUtils, Generics.Defaults,
  // Project
  UExceptions;

{$R *.dfm}

{ TDetailFrame }

procedure TDetailFrame.AddAction(const Action: TCustomAction;
  const ID: TCommandBarID);
begin
  if ID = cDetailTabSetPopupMenu then
    fTabSetCmdBar.AddAction(Action)
  else
    (frmDetailView as ICommandBarConfig).AddAction(Action, ID);
end;

procedure TDetailFrame.AddAction(const Action: TCustomAction;
  const IDs: TCommandBarIDs);
var
  ID: TCommandBarID;
begin
  for ID in IDs do
    AddAction(Action, ID);
end;

procedure TDetailFrame.AddSpacer(const IDs: TCommandBarIDs);
var
  ID: TCommandBarID;
begin
  for ID in IDs do
    AddSpacer(ID);
end;

procedure TDetailFrame.AddSpacer(const ID: TCommandBarID);
begin
  if ID = cDetailTabSetPopupMenu then
    fTabSetCmdBar.AddSpacer
  else
    (frmDetailView as ICommandBarConfig).AddSpacer(ID);
end;

function TDetailFrame.CanCopy: Boolean;
begin
  Result := not IsEmptyTabSet and (frmDetailView as IClipboardMgr).CanCopy;
end;

function TDetailFrame.CanSelectAll: Boolean;
begin
  Result := not IsEmptyTabSet and (frmDetailView as ISelectionMgr).CanSelectAll;
end;

procedure TDetailFrame.Clear;
begin
  InternalDisplay(TViewFactory.CreateNulView, False);
end;

procedure TDetailFrame.CloseMultipleTabs(const KeepSelected: Boolean);
var
  Idx: Integer;
  SelectedIdx: Integer;
begin
  if IsEmptyTabSet then
    Exit;
  SelectedIdx := SelectedTab;
  for Idx := Pred(TabCount) downto 0 do
  begin
    if (Idx = SelectedIdx) and KeepSelected then
      Continue;
    InternalDeleteTab(Idx);
  end;
  if KeepSelected then
    tcViews.TabIndex := 0 // only selected tab remains: new index
  else
    tcViews.TabIndex := -1;
end;

procedure TDetailFrame.CloseTab(const TabIdx: Integer);
var
  SelectedIdx: Integer;
  ClosingSelectedIdx: Boolean;
begin
  // TODO: protect against empty tab set
  ClosingSelectedIdx := SelectedTab = TabIdx;
  SelectedIdx := SelectedTab;
  InternalDeleteTab(TabIdx);
  if ClosingSelectedIdx then
  begin
    if SelectedIdx = TabCount then
      Dec(SelectedIdx);
    tcViews.TabIndex := SelectedIdx;
  end
  else
  begin
    if TabIdx < SelectedIdx then
      // keeps existing selected tab displayed
      tcViews.TabIndex := Pred(SelectedIdx);
  end;
end;

procedure TDetailFrame.CopyToClipboard;
begin
  if IsEmptyTabSet then
    Exit;
  (frmDetailView as IClipboardMgr).CopyToClipboard;
end;

constructor TDetailFrame.Create(AOwner: TComponent);
begin
  inherited;
  fViews := TList<IView>.Create;
  fDisplayedView := TViewFactory.CreateNulView;
  fTabSetCmdBar := TPopupMenuWrapper.Create(mnuTabs);
end;

function TDetailFrame.CreateTab(View: IView): Integer;
begin
  Result := tcViews.Tabs.Add(View.Description);
  fViews.Add(View);
  InternalDisplay(View, False); // stores View in fViews again
end;

destructor TDetailFrame.Destroy;
begin
  fTabSetCmdBar.Free;
  fViews.Free;
  inherited;
end;

procedure TDetailFrame.Display(View: IView; const TabIdx: Integer);
begin
  Assert(Assigned(View), ClassName + '.Display: View is nil');
  fViews[TabIdx] := TViewFactory.Clone(View);
  tcViews.Tabs[TabIdx] := View.Description;
  if TabIdx = SelectedTab then
    InternalDisplay(fViews[TabIdx], False);
end;

function TDetailFrame.FindTab(ViewKey: IViewKey): Integer;
var
  Idx: Integer;
begin
  for Idx := 0 to Pred(fViews.Count) do
    if ViewKey.IsEqual(fViews[Idx].GetKey) then
      Exit(Idx);
  Result := -1;
end;

procedure TDetailFrame.InternalDeleteTab(TabIdx: Integer);
begin
  tcViews.Tabs.Delete(TabIdx);
  fViews.Delete(TabIdx);
end;

procedure TDetailFrame.InternalDisplay(View: IView; ForceReload: Boolean);
begin
  (frmDetailView as IViewItemDisplayMgr).Display(View, ForceReload);
end;

procedure TDetailFrame.InternalSelectTab(TabIdx: Integer);
begin
  tcViews.TabIndex := TabIdx;
  InternalDisplay(SelectedView, False);  // SelectedView allows for TabIdx = -1
end;

function TDetailFrame.IsEmptyTabSet: Boolean;
begin
  Result := TabCount = 0;
end;

function TDetailFrame.IsInteractive: Boolean;
var
  Idx: Integer;
begin
  if IsEmptyTabSet then
    Exit(False);
  if (frmDetailView as IPaneInfo).IsInteractive then
    Exit(True);
  for Idx := 0 to Pred(ComponentCount) do
    if (Components[Idx] is TWinControl)
      and (Components[Idx] as TWinControl).Focused then
      Exit(True);
  Result := False;
end;

procedure TDetailFrame.NextTab;
begin
  if IsEmptyTabSet then
    Exit;
  if SelectedTab = Pred(TabCount) then
    InternalSelectTab(0)
  else
    InternalSelectTab(Succ(SelectedTab));
end;

procedure TDetailFrame.NotifyTabChange;
begin
  if Assigned(fNotifier) then
    fNotifier.ChangeDetailPane(tcViews.TabIndex);
end;

procedure TDetailFrame.PreviousTab;
begin
  if IsEmptyTabSet then
    Exit;
  if SelectedTab = 0 then
    InternalSelectTab(Pred(TabCount))
  else
    InternalSelectTab(Pred(SelectedTab));
end;

procedure TDetailFrame.Reload;
begin
  InternalDisplay(SelectedView, True);  // SelectedView allows for TabIdx = -1
end;

procedure TDetailFrame.SelectAll;
begin
  if IsEmptyTabSet then
    Exit;
  (frmDetailView as ISelectionMgr).SelectAll;
end;

function TDetailFrame.SelectedTab: Integer;
begin
  if IsEmptyTabSet then
    Exit(-1);
  Result := tcViews.TabIndex;
end;

function TDetailFrame.SelectedView: IView;
begin
  if IsEmptyTabSet then
    Result := TViewFactory.CreateNulView
  else
    Result := fViews[SelectedTab];
end;

procedure TDetailFrame.SelectTab(const TabIdx: Integer);
begin
  InternalSelectTab(TabIdx);
end;

procedure TDetailFrame.SetDragDropHandler(Obj: IDropTarget);
begin
  (frmDetailView as IWBCustomiser).SetDragDropHandler(Obj);
end;

procedure TDetailFrame.SetExternalObj(Obj: IDispatch);
begin
  (frmDetailView as IWBCustomiser).SetExternalObj(Obj);
end;

procedure TDetailFrame.SetImages(const Images: TCustomImageList);
begin
  fTabSetCmdBar.SetImages(Images);
  (frmDetailView as ICommandBarConfig).SetImages(Images);
end;

procedure TDetailFrame.SetNotifier(const Notifier: INotifier);
begin
  fNotifier := Notifier;
end;

function TDetailFrame.TabCount: Integer;
begin
  Result := tcViews.Tabs.Count;
  Assert(Result = fViews.Count,
    ClassName + '.TabCount: Tabs.Count differs from fViews.Count');
end;

procedure TDetailFrame.tcViewsChange(Sender: TObject);
begin
  // User changed tab
  // notify program via notifier: this will result in instruction back to select
  // the tab - this allows for other controls to change the tab
  Assert(tcViews.TabIndex >= 0, ClassName + '.tcViewsChange: tab index < 0');
  NotifyTabChange;
end;

procedure TDetailFrame.tcViewsMouseDown(Sender: TObject; Button: TMouseButton;
  Shift: TShiftState; X, Y: Integer);
var
  TabIdx: Integer;
begin
  if htOnItem in tcViews.GetHitTestInfoAt(X, Y) then
  begin
    // ensure tab set has focus when a tab is clicked
    tcViews.SetFocus;
    if Button = mbRight then
    begin
      // select tab when right clicked
      TabIdx := tcViews.IndexOfTabAt(X, Y);
      if (TabIdx >= 0) and (TabIdx < tcViews.Tabs.Count) then
      begin
        tcViews.TabIndex := TabIdx;
        NotifyTabChange;
      end;
    end;
  end;
end;

end.


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
    tcViews: TTabControl;
    procedure tcViewsChange(Sender: TObject);
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
    fNotifier: INotifier;
    fCommandBarItems: TCommandBarItems;
    fDisplayedView: IView;
    fViews: TList<IView>;
    function TabCount: Integer;
    procedure NotifyTabChange(TabIdx: Integer);
    procedure InternalSelectTab(TabIdx: Integer);
    procedure InternalDisplay(View: IView);
    procedure InternalDeleteTab(TabIdx: Integer);
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;

    // ITabbedDisplayMgr
    procedure SelectTab(const TabIdx: Integer);
    function SelectedTab: Integer;
    procedure NextTab;
    procedure PreviousTab;

    // IPaneInfo
    function IsInteractive: Boolean;

    // IDetailPaneDisplayMgr
    function SelectedView: IView;
    procedure IDetailPaneDisplayMgr.SelectTab = SelectTab;
    function FindTab(ViewKey: IViewKey): Integer;
    procedure Display(View: IView; const TabIdx: Integer);
    function CreateTab(View: IView): Integer;
    function IsEmptyTabSet: Boolean;
    procedure CloseTab(const TabIdx: Integer);
    procedure CloseMultipleTabs(const KeepSelected: Boolean);

    // IWBCustomiser
    procedure SetExternalObj(Obj: IDispatch);
    procedure SetDragDropHandler(Obj: IDropTarget);

    // ISetNotifier
    procedure SetNotifier(const Notifier: INotifier);

    // ICommandBarConfig
    procedure AddAction(const Action: TCustomAction; const ID: TCommandBarID);
    procedure AddSpacer(const ID: TCommandBarID);
    procedure SetImages(const Images: TCustomImageList);

    // ISelectionMgr
    function CanSelectAll: Boolean;
    procedure SelectAll;

    // IClipboardMgr
    function CanCopy: Boolean;
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
  (frmDetailView as ICommandBarConfig).AddAction(Action, ID);
end;

procedure TDetailFrame.AddSpacer(const ID: TCommandBarID);
begin
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
    NotifyTabChange(-1);  // empty: change of tab selection
end;

procedure TDetailFrame.CloseTab(const TabIdx: Integer);
var
  SelectedIdx: Integer;
  ClosingSelectedIdx: Boolean;
begin
  ClosingSelectedIdx := SelectedTab = TabIdx;
  SelectedIdx := SelectedTab;
  InternalDeleteTab(TabIdx);
  if ClosingSelectedIdx then
  begin
    if SelectedIdx = TabCount then
      Dec(SelectedIdx);
    // new tab (view) selected: notify tab change
    NotifyTabChange(SelectedIdx);
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
  fCommandBarItems := TCommandBarItems.Create;
  fViews := TList<IView>.Create;
  fDisplayedView := TViewItemFactory.CreateNulView;
end;

function TDetailFrame.CreateTab(View: IView): Integer;
begin
  Result := tcViews.Tabs.Add(View.Description);
  fViews.Add(View);
  InternalDisplay(View); // stores View in fViews again
end;

destructor TDetailFrame.Destroy;
begin
  fViews.Free;
  fCommandBarItems.Free;
  inherited;
end;

procedure TDetailFrame.Display(View: IView; const TabIdx: Integer);
begin
  Assert(Assigned(View), ClassName + '.Display: View is nil');

  fViews[TabIdx] := TViewItemFactory.Clone(View);
  tcViews.Tabs[TabIdx] := View.Description;
  if TabIdx = SelectedTab then
    InternalDisplay(fViews[TabIdx]);
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

procedure TDetailFrame.InternalDisplay(View: IView);
begin
  (frmDetailView as IViewItemDisplayMgr).Display(View);
end;

procedure TDetailFrame.InternalSelectTab(TabIdx: Integer);
begin
  tcViews.TabIndex := TabIdx;
  InternalDisplay(SelectedView);  // SelectedView allows for TabIdx = -1
end;

function TDetailFrame.IsEmptyTabSet: Boolean;
begin
  Result := TabCount = 0;
end;

function TDetailFrame.IsInteractive: Boolean;
begin
  Result := not IsEmptyTabSet and (frmDetailView as IPaneInfo).IsInteractive;
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

procedure TDetailFrame.NotifyTabChange(TabIdx: Integer);
begin
  // NOTE: should only be called after some INTERNAL action changes tab
  if Assigned(fNotifier) then
    fNotifier.ChangeDetailPane(TabIdx);
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
    Result := TViewItemFactory.CreateNulView
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
  NotifyTabChange(tcViews.TabIndex);
end;

{ TDetailFrame.TCommandBarItem }

constructor TDetailFrame.TCommandBarItem.Create(AAction: TCustomAction;
  AID: TCommandBarID);
begin
  Action := AAction;
  ID := AID;
end;

end.


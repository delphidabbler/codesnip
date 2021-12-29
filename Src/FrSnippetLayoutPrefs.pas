{
 * This Source Code Form is subject to the terms of the Mozilla Public License,
 * v. 2.0. If a copy of the MPL was not distributed with this file, You can
 * obtain one at https://mozilla.org/MPL/2.0/
 *
 * Copyright (C) 2012-2020, Peter Johnson (gravatar.com/delphidabbler).
 *
 * Implements a frame that allows user to customise appearance of different
 * kinds of snippets in main display.
 * Designed for use as one of the tabs in the preferences dialog box.
}


unit FrSnippetLayoutPrefs;

interface

uses
  // Delphi
  StdCtrls, ImgList, Controls, Classes, ActnList, Buttons,
  // Project
  DB.USnippetKind, FrPrefsBase, UPreferences, USnippetPageStructure;

type
  TSnippetLayoutPrefsFrame = class(TPrefsBaseFrame)
    cbSnippetKinds: TComboBox;
    lbAvailableFragments: TListBox;
    lbUsedFragments: TListBox;
    btnIncludeFragment: TSpeedButton;
    btnExcludeFragment: TSpeedButton;
    btnMoveFragmentUp: TSpeedButton;
    btnMoveFragmentDown: TSpeedButton;
    alFrame: TActionList;
    actMoveFragmentUp: TAction;
    actMoveFragmentDown: TAction;
    actIncludeFragment: TAction;
    actExcludeFragment: TAction;
    ilFrame: TImageList;
    btnRestoreDefaults: TButton;
    actRestoreDefaults: TAction;
    lblInstructions: TLabel;
    lblSnippetKinds: TLabel;
    lblAvailable: TLabel;
    lblUsed: TLabel;
    procedure actIncludeFragmentUpdate(Sender: TObject);
    procedure actExcludeFragmentUpdate(Sender: TObject);
    procedure actMoveFragmentUpUpdate(Sender: TObject);
    procedure actMoveFragmentDownUpdate(Sender: TObject);
    procedure cbSnippetKindsChange(Sender: TObject);
    procedure actIncludeFragmentExecute(Sender: TObject);
    procedure actExcludeFragmentExecute(Sender: TObject);
    procedure actMoveFragmentUpExecute(Sender: TObject);
    procedure actMoveFragmentDownExecute(Sender: TObject);
    procedure actRestoreDefaultsExecute(Sender: TObject);
  strict private
    var
      fPageStructs: TSnippetPageStructures;
      fUIUpdated: Boolean;
    function PartIdFromStrings(Strings: TStrings; Idx: Integer):
      TSnippetPagePartId;
    function SelectedKind: TSnippetKind;
    procedure UpdateFragmentInfo;
    procedure Changed;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    procedure Activate(const Prefs: IPreferences; const Flags: UInt64);
      override;
    procedure Deactivate(const Prefs: IPreferences); override;
    ///  <summary>Checks if preference changes require that main window UI is
    ///  updated.</summary>
    ///  <remarks>Called when dialog box containing frame is closing.</remarks>
    function UIUpdated: Boolean; override;
    procedure ArrangeControls; override;
    function DisplayName: string; override;
    class function Index: Byte; override;
  end;


implementation

uses
  // Delphi
  Windows, Graphics,
  // Project
  FmPreferencesDlg, UClassHelpers, UCtrlArranger;

{$R *.dfm}

{ TSnippetLayoutPrefsFrame }

procedure TSnippetLayoutPrefsFrame.actExcludeFragmentExecute(Sender: TObject);
var
  SrcIdx: Integer;
  SrcPartId: TSnippetPagePartId;
  DestIdx: Integer;
begin
  SrcIdx := lbUsedFragments.ItemIndex;
  Assert(SrcIdx >= 0,
    ClassName + '.actExcludeFragmentExecute: No used item selected');
  SrcPartId := PartIdFromStrings(lbUsedFragments.Items, SrcIdx);
  DestIdx := lbAvailableFragments.Items.AddObject(
    TAllSnippetPageParts.Parts[SrcPartId].DisplayName, TObject(SrcPartId)
  );
  fPageStructs[SelectedKind].DeletePart(SrcIdx);
  lbAvailableFragments.ItemIndex := DestIdx;
  lbUsedFragments.Items.Delete(SrcIdx);
  if SrcIdx >= lbUsedFragments.Items.Count then
    lbUsedFragments.ItemIndex := Pred(lbUsedFragments.Items.Count)
  else
    lbUsedFragments.ItemIndex := SrcIdx;
  Changed;
end;

procedure TSnippetLayoutPrefsFrame.actExcludeFragmentUpdate(Sender: TObject);
begin
  actExcludeFragment.Enabled := (lbUsedFragments.Count >= 0)
    and (lbUsedFragments.ItemIndex >= 0);
end;

procedure TSnippetLayoutPrefsFrame.actIncludeFragmentExecute(Sender: TObject);
var
  SrcIdx: Integer;
  SrcPartId: TSnippetPagePartId;
  DestIdx: Integer;
begin
  SrcIdx := lbAvailableFragments.ItemIndex;
  Assert(SrcIdx >= 0,
    ClassName + '.actIncludeFragmentExecute: No available item selected');
  SrcPartId := PartIdFromStrings(lbAvailableFragments.Items, SrcIdx);
  DestIdx := lbUsedFragments.Items.AddObject(
    TAllSnippetPageParts.Parts[SrcPartId].DisplayName, TObject(SrcPartId)
  );
  fPageStructs[SelectedKind].InsertPart(DestIdx, SrcPartId);
  lbUsedFragments.ItemIndex := DestIdx;
  lbAvailableFragments.Items.Delete(SrcIdx);
  if SrcIdx >= lbAvailableFragments.Items.Count then
    lbAvailableFragments.ItemIndex := Pred(lbAvailableFragments.Items.Count)
  else
    lbAvailableFragments.ItemIndex := SrcIdx;
  Changed;
end;

procedure TSnippetLayoutPrefsFrame.actIncludeFragmentUpdate(Sender: TObject);
begin
  actIncludeFragment.Enabled := (lbAvailableFragments.Items.Count >= 0)
    and (lbAvailableFragments.ItemIndex >= 0);
end;

procedure TSnippetLayoutPrefsFrame.Activate(const Prefs: IPreferences;
  const Flags: UInt64);
begin
  fPageStructs.Assign(Prefs.PageStructures);
  UpdateFragmentInfo;
end;

procedure TSnippetLayoutPrefsFrame.actMoveFragmentDownExecute(Sender: TObject);
var
  OldIdx: Integer;
  NewIdx: Integer;
begin
  OldIdx := lbUsedFragments.ItemIndex;
  NewIdx := Succ(OldIdx);
  fPageStructs[SelectedKind].MovePart(OldIdx, NewIdx);
  lbUsedFragments.Items.Move(OldIdx, NewIdx);
  lbUsedFragments.ItemIndex := NewIdx;
  Changed;
end;

procedure TSnippetLayoutPrefsFrame.actMoveFragmentDownUpdate(Sender: TObject);
begin
  actMoveFragmentDown.Enabled := (lbUsedFragments.Items.Count >= 0)
    and (lbUsedFragments.ItemIndex < Pred(lbUsedFragments.Count));
end;

procedure TSnippetLayoutPrefsFrame.actMoveFragmentUpExecute(Sender: TObject);
var
  OldIdx: Integer;
  NewIdx: Integer;
begin
  OldIdx := lbUsedFragments.ItemIndex;
  NewIdx := Pred(OldIdx);
  fPageStructs[SelectedKind].MovePart(OldIdx, NewIdx);
  lbUsedFragments.Items.Move(OldIdx, NewIdx);
  lbUsedFragments.ItemIndex := NewIdx;
  Changed;
end;

procedure TSnippetLayoutPrefsFrame.actMoveFragmentUpUpdate(Sender: TObject);
begin
  actMoveFragmentUp.Enabled := (lbUsedFragments.Items.Count >= 0)
    and (lbUsedFragments.ItemIndex > 0)
end;

procedure TSnippetLayoutPrefsFrame.actRestoreDefaultsExecute(Sender: TObject);
begin
  TDefaultPageStructures.SetDefaults(fPageStructs);
  UpdateFragmentInfo;
  Changed;
end;

procedure TSnippetLayoutPrefsFrame.ArrangeControls;
begin
  lblInstructions.Width := Self.Width - 6;
  TCtrlArranger.SetLabelHeight(lblInstructions);
  TCtrlArranger.AlignVCentres(
    TCtrlArranger.BottomOf(lblInstructions, 12),
    [lblSnippetKinds, cbSnippetKinds]
  );
  TCtrlArranger.MoveToRightOf(lblSnippetKinds, cbSnippetKinds, 8);
  TCtrlArranger.AlignLefts(
    [lblInstructions, lblSnippetKinds, lblAvailable, lbAvailableFragments], 3
  );
  TCtrlArranger.AlignTops(
    [lblAvailable, lblUsed],
    TCtrlArranger.BottomOf([lblSnippetKinds, cbSnippetKinds], 12)
  );
  TCtrlArranger.AlignTops(
    [lbAvailableFragments, lbUsedFragments],
    TCtrlArranger.BottomOf([lblAvailable, lblUsed], 4)
  );
  TCtrlArranger.MoveBelow(
    [lbAvailableFragments, lbUsedFragments], btnRestoreDefaults, 12
  );
  lbAvailableFragments.Width :=
    (Self.Width - 6 - 2 * btnMoveFragmentUp.Width - 24) div 2;
  lbUsedFragments.Width := lbAvailableFragments.Width;
  TCtrlArranger.AlignRights(
    [btnMoveFragmentUp, btnMoveFragmentDown], Self.Width - 3
  );
  TCtrlArranger.MoveToLeftOf(
    [btnMoveFragmentUp, btnMoveFragmentDown], lbUsedFragments, 8
  );
  TCtrlArranger.AlignLefts(
    [btnIncludeFragment, btnExcludeFragment],
    (lbUsedFragments.Left + TCtrlArranger.RightOf(lbAvailableFragments) -
      btnIncludeFragment.Width) div 2
  );
  btnRestoreDefaults.Left :=
    (lbUsedFragments.Left + TCtrlArranger.RightOf(lbAvailableFragments) -
      btnRestoreDefaults.Width) div 2
end;

procedure TSnippetLayoutPrefsFrame.cbSnippetKindsChange(Sender: TObject);
begin
  UpdateFragmentInfo;
end;

procedure TSnippetLayoutPrefsFrame.Changed;
begin
  fUIUpdated := True;
end;

constructor TSnippetLayoutPrefsFrame.Create(AOwner: TComponent);
var
  SKInfo: TSnippetKindInfo;
begin
  inherited;
  ilFrame.LoadFromResource(RT_RCDATA, 'ACTIONIMAGES', 16, clFuchsia);
  RefreshActions;
  HelpKeyword := 'SnippetLayoutPrefs';
  fPageStructs := TSnippetPageStructures.Create;
  for SKInfo in TSnippetKindInfoList.Items do
    cbSnippetKinds.Items.AddObject(SKInfo.DisplayName, TObject(SKInfo.Kind));
  cbSnippetKinds.ItemIndex := 0;
end;

procedure TSnippetLayoutPrefsFrame.Deactivate(const Prefs: IPreferences);
begin
  Prefs.PageStructures := fPageStructs;
end;

destructor TSnippetLayoutPrefsFrame.Destroy;
begin
  fPageStructs.Free;
  inherited;
end;

function TSnippetLayoutPrefsFrame.DisplayName: string;
resourcestring
  sDisplayName = 'Snippet Layout'; // display name
begin
  Result := sDisplayName;
end;

class function TSnippetLayoutPrefsFrame.Index: Byte;
begin
  Result := 12;
end;

function TSnippetLayoutPrefsFrame.PartIdFromStrings(Strings: TStrings;
  Idx: Integer): TSnippetPagePartId;
begin
  Result := TSnippetPagePartId(Strings.Objects[Idx]);
end;

function TSnippetLayoutPrefsFrame.SelectedKind: TSnippetKind;
var
  SelIdx: Integer;
begin
  SelIdx := cbSnippetKinds.ItemIndex;
  Assert(SelIdx >= 0, ClassName + '.SelectedKind: No snippet kind selected');
  Result := TSnippetKind(cbSnippetKinds.Items.Objects[SelIdx]);
end;

function TSnippetLayoutPrefsFrame.UIUpdated: Boolean;
begin
  Result := fUIUpdated;
end;

procedure TSnippetLayoutPrefsFrame.UpdateFragmentInfo;
var
  Part: TSnippetPagePart;
  PartId: TSnippetPagePartId;
begin
  lbAvailableFragments.Items.BeginUpdate;
  try
    lbAvailableFragments.Items.Clear;
    for PartId := Low(TSnippetPagePartId) to High(TSnippetPagePartId) do
      if not fPageStructs[SelectedKind].HasPart(PartId) then
        lbAvailableFragments.Items.AddObject(
          TAllSnippetPageParts.Parts[PartId].DisplayName, TObject(PartId)
        );
    if lbAvailableFragments.Items.Count >= 0 then
      lbAvailableFragments.ItemIndex := 0;
  finally
    lbAvailableFragments.Items.EndUpdate;
  end;

  lbUsedFragments.Items.BeginUpdate;
  try
    lbUsedFragments.Items.Clear;
    for Part in fPageStructs[SelectedKind].Parts do
      lbUsedFragments.Items.AddObject(Part.DisplayName, TObject(Part.Id));
    if lbUsedFragments.Items.Count >= 0 then
      lbUsedFragments.ItemIndex := 0;
  finally
    lbUsedFragments.Items.EndUpdate;
  end;
end;

initialization

// Register frame with preferences dialog box
TPreferencesDlg.RegisterPage(TSnippetLayoutPrefsFrame);

end.

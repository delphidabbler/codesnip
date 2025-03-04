{
 * This Source Code Form is subject to the terms of the Mozilla Public License,
 * v. 2.0. If a copy of the MPL was not distributed with this file, You can
 * obtain one at https://mozilla.org/MPL/2.0/
 *
 * Copyright (C) 2012-2022, Peter Johnson (gravatar.com/delphidabbler).
 *
 * Implements a frame that allows user to set application display preferences.

 * Designed for use as one of the pages in the preferences dialogue box.
}


unit FrDisplayPrefs;


interface


uses
  // Delphi
  Controls,
  StdCtrls,
  Classes,
  Graphics,
  Generics.Collections,
  // Project
  DB.Vaults,
  FrPrefsBase,
  UColorBoxEx,
  UColorDialogEx,
  UI.Adapters.VaultList,
  UPreferences;


type

  TDisplayPrefsFrame = class(TPrefsBaseFrame)
    lblOverviewTree: TLabel;
    cbOverviewTree: TComboBox;
    chkHideEmptySections: TCheckBox;
    chkSnippetsInNewTab: TCheckBox;
    lblGroupHeadingColour: TLabel;
    lblVaultColours: TLabel;
    btnDefColours: TButton;
    lblSourceBGColour: TLabel;
    lblOverviewFontSize: TLabel;
    cbOverviewFontSize: TComboBox;
    lblDetailFontSize: TLabel;
    cbDetailFontSize: TComboBox;
    lblHiliterInfo: TLabel;
    cbVaults: TComboBox;
    procedure chkHideEmptySectionsClick(Sender: TObject);
    procedure btnDefColoursClick(Sender: TObject);
    procedure FontSizeChange(Sender: TObject);
    procedure cbVaultsChange(Sender: TObject);
  strict private
    var
      ///  <summary>Flag indicating if changes affect UI.</summary>
      fUIChanged: Boolean;

      ///  <summary>Local copy of snippet heading / tree node colour for each
      ///  vault.</summary>
      fSnippetHeadingColours: TDictionary<TVaultID,TColor>;

      fGroupHeadingColourBox: TColorBoxEx;
      fGroupHeadingColourDlg: TColorDialogEx;
      fSnippetHeadingColourBox: TColorBoxEx;
      fSnippetHeadingColourDlg: TColorDialogEx;
      fSourceBGColourBox: TColorBoxEx;
      fSourceBGColourDlg: TColorDialogEx;

      fVaultList: TVaultListAdapter;

    procedure SelectOverviewTreeState(const State: TOverviewStartState);
      {Selects combo box item associated with a overview treeview startup state.
        @param State [in] Startup state to be selected.
      }
    function OverviewTreeStateDesc(const State: TOverviewStartState): string;
      {Gets description of an overview treeview startup state.
        @param State [in] State for which description is required.
        @return Required description.
      }
    function CreateCustomColourBox(const ColourDlg: TColorDialogEx;
      ChangeHandler: TNotifyEvent):
      TColorBoxEx;
    procedure ColourBoxChangeHandler(Sender: TObject);
    procedure SnippetHeadingColourBoxChange(Sender: TObject);
    procedure PopulateFontSizeCombos;
    procedure SetTabOrder;
    function SelectedVaultID: TVaultID;
  public
    constructor Create(AOwner: TComponent); override;
      {Object constructor. Sets up frame and populates controls.
        @param AOwner [in] Component that owns frame.
      }

    ///  <summary>Object destructor. Frees owned objects.</summary>
    destructor Destroy; override;

    procedure Activate(const Prefs: IPreferences; const Flags: UInt64);
      override;
      {Called when page activated. Updates controls.
        @param Prefs [in] Object that provides info used to update controls.
      }
    procedure Deactivate(const Prefs: IPreferences); override;
      {Called when page is deactivated. Stores information entered by user.
        @param Prefs [in] Object used to store information.
      }
    ///  <summary>Checks if preference changes require that main window UI is
    ///  updated.</summary>
    ///  <remarks>Called when dialog box containing frame is closing.</remarks>
    function UIUpdated: Boolean; override;
    procedure ArrangeControls; override;
      {Arranges controls on frame. Called after frame has been sized.
      }
    function DisplayName: string; override;
      {Caption that is displayed in the tab sheet that contains this frame when
      displayed in the preference dialog box.
        @return Required display name.
      }
    class function Index: Byte; override;
      {Index number that determines the location of the tab containing this
      frame when displayed in the preferences dialog box.
        @return Required index number.
      }
  end;


implementation


uses
  // Delphi
  SysUtils,
  Generics.Defaults,
  Math,
  ExtCtrls,
  // Project
  FmPreferencesDlg,
  UColours,
  UCtrlArranger,
  UFontHelper,
  UGraphicUtils,
  UMessageBox;


{$R *.dfm}


resourcestring
  // Error messages
  sErrBadOverviewFontSize   = 'Invalid font size';
  sErrBadOverviewFontRange  = 'Font size out of range. '
                              + 'Enter a value between %0:d and %1:d';

{ TDisplayPrefsFrame }

procedure TDisplayPrefsFrame.Activate(const Prefs: IPreferences;
  const Flags: UInt64);
  {Called when page activated. Updates controls.
    @param Prefs [in] Object that provides info used to update controls.
  }
var
  Vault: TVault;
begin
  cbVaults.ItemIndex := fVaultList.IndexOfUID(TVaultID.Default);
  Assert(cbVaults.ItemIndex >= 0,
    ClassName + '.Activate: no default vault found in cbVaults');
  SelectOverviewTreeState(Prefs.OverviewStartState);
  chkHideEmptySections.OnClick := nil;  // prevent OnClick when Checked set
  chkHideEmptySections.Checked := not Prefs.ShowEmptySections;
  chkHideEmptySections.OnClick := chkHideEmptySectionsClick;
  chkSnippetsInNewTab.Checked := Prefs.ShowNewSnippetsInNewTabs;
  fGroupHeadingColourBox.Selected := Prefs.GroupHeadingColour;
  fSnippetHeadingColours.Clear;
  for Vault in TVaults.Instance do
    fSnippetHeadingColours.Add(
      Vault.UID, Prefs.GetSnippetHeadingColour(Vault.UID)
    );
  fSnippetHeadingColourBox.Selected :=
    Prefs.GetSnippetHeadingColour(SelectedVaultID);
  fSourceBGColourBox.Selected := Prefs.SourceCodeBGcolour;
  Prefs.GroupHeadingCustomColours.CopyTo(
    fGroupHeadingColourDlg.CustomColors, True
  );
  Prefs.SourceCodeBGCustomColours.CopyTo(fSourceBGColourDlg.CustomColors, True);
  cbOverviewFontSize.Tag := Prefs.OverviewFontSize; // store font size in .Tag
  cbOverviewFontSize.Text := IntToStr(Prefs.OverviewFontSize);
  cbDetailFontSize.Tag := Prefs.DetailFontSize;     // store font size in .Tag
  cbDetailFontSize.Text := IntToStr(Prefs.DetailFontSize);

end;

procedure TDisplayPrefsFrame.ArrangeControls;
  {Arranges controls on frame. Called after frame has been sized.
  }
begin
  // Align controls on left
  TCtrlArranger.AlignLefts(
    [
      lblOverviewTree, chkSnippetsInNewTab, chkHideEmptySections,
      lblGroupHeadingColour, lblVaultColours, lblSourceBGColour,
      btnDefColours, lblOverviewFontSize, lblDetailFontSize, lblHiliterInfo
    ],
    0
  );
  // Align vaults combo indented from left
  cbVaults.Left := 8;

  // Align controls on right: make sure they are all to right of everything
  // on left that is on same line as any of them.
  TCtrlArranger.AlignLefts(
    [
      cbOverviewTree, fGroupHeadingColourBox, fSnippetHeadingColourBox,
      fSourceBGColourBox, cbOverviewFontSize, cbDetailFontSize
    ],
    TCtrlArranger.RightOf(
      [
        lblOverviewTree, lblGroupHeadingColour, cbVaults, lblSourceBGColour,
        lblOverviewFontSize, lblDetailFontSize
      ],
      8
    )
  );

  // Align rows:
  // 1st row
  TCtrlArranger.AlignVCentres(3, [lblOverviewTree, cbOverviewTree]);
  // 2nd row
  TCtrlArranger.MoveBelow(
    [lblOverviewTree, cbOverviewTree], chkSnippetsInNewTab, 12
  );
  chkSnippetsInNewTab.Width := Self.Width - 16;
  // 3rd row
  TCtrlArranger.MoveBelow(chkSnippetsInNewTab, chkHideEmptySections, 8);
  chkHideEmptySections.Width := Self.Width - 16;
  // 4th row
  TCtrlArranger.AlignVCentres(
    TCtrlArranger.BottomOf(chkHideEmptySections, 12),
    [lblGroupHeadingColour, fGroupHeadingColourBox]
  );
  // 5th row
  TCtrlArranger.MoveBelow(
    [lblGroupHeadingColour, fGroupHeadingColourBox],
    lblVaultColours,
    12
  );
  // 6th row
  TCtrlArranger.AlignVCentres(
    TCtrlArranger.BottomOf(lblVaultColours, 6),
    [cbVaults, fSnippetHeadingColourBox]
  );
  // 7th row
  TCtrlArranger.AlignVCentres(
    TCtrlArranger.BottomOf([cbVaults, fSnippetHeadingColourBox], 18),
    [lblSourceBGColour, fSourceBGColourBox]
  );
  // 8th row
  TCtrlArranger.MoveBelow(
    [lblSourceBGColour, fSourceBGColourBox], btnDefColours, 12
  );
  // 9th row
  TCtrlArranger.AlignVCentres(
    TCtrlArranger.BottomOf(btnDefColours, 12),
    [lblOverviewFontSize, cbOverviewFontSize]
  );
  // 10th row
  TCtrlArranger.AlignVCentres(
    TCtrlArranger.BottomOf([lblOverviewFontSize, cbOverviewFontSize], 8),
    [lblDetailFontSize, cbDetailFontSize]
  );
  // 11th row
  TCtrlArranger.MoveBelow(
    [lblDetailFontSize, cbDetailFontSize], lblHiliterInfo, 12
  );
  lblHiliterInfo.Width := Self.ClientWidth;
  TCtrlArranger.SetLabelHeight(lblHiliterInfo);
end;

procedure TDisplayPrefsFrame.btnDefColoursClick(Sender: TObject);
var
  Vault: TVault;
begin
  // Restores default heading and source code background colours in colour
  // combo boxes
  fGroupHeadingColourBox.Selected := clDefGroupHeading;
  fSnippetHeadingColourBox.Selected := clDefSnippetHeading;
  for Vault in TVaults.Instance do
    fSnippetHeadingColours[Vault.UID] := clDefSnippetHeading;
  fSourceBGColourBox.Selected := clSourceBg;
  fUIChanged := True;
end;

procedure TDisplayPrefsFrame.cbVaultsChange(Sender: TObject);
begin
  fSnippetHeadingColourBox.Selected :=
    fSnippetHeadingColours[SelectedVaultID];
end;

procedure TDisplayPrefsFrame.chkHideEmptySectionsClick(Sender: TObject);
  {Handles clicks on "Hide Empty Sections" check box. Flags UI preferences has
  having changed.
    @param Sender [in] Ignored.
  }
begin
  fUIChanged := True;
end;

procedure TDisplayPrefsFrame.ColourBoxChangeHandler(Sender: TObject);
begin
  fUIChanged := True;
end;

constructor TDisplayPrefsFrame.Create(AOwner: TComponent);
  {Class constructor. Sets up frame and populates controls.
    @param AOwner [in] Component that owns frame.
  }
resourcestring
  sHeadingColourDlgTitle = 'Heading Colour';
  sGroupHeadingColourDlgTitle = 'Group Heading Colour';
  sSourceBGColourDlgTitle = 'Source Code Background Colour';
var
  OTStateIdx: TOverviewStartState;  // loops thru each overview tree start state
begin
  inherited;
  HelpKeyword := 'DisplayPrefs';
  // Populate overview tree start state
  for OTStateIdx := Low(TOverviewStartState) to High(TOverviewStartState) do
    cbOverviewTree.Items.AddObject(
      OverviewTreeStateDesc(OTStateIdx), TObject(OTStateIdx)
    );
  // Create colour dialogue boxes
  fGroupHeadingColourDlg := TColorDialogEx.Create(Self);
  fGroupHeadingColourDlg.Title := sGroupHeadingColourDlgTitle;
  fSnippetHeadingColourDlg := TColorDialogEx.Create(Self);
  fSnippetHeadingColourDlg.Title := sHeadingColourDlgTitle;
  fSourceBGColourDlg := TColorDialogEx.Create(Self);
  fSourceBGColourDlg.Title := sSourceBGColourDlgTitle;
  // Create colour combo boxes
  fGroupHeadingColourBox := CreateCustomColourBox(
    fGroupHeadingColourDlg, ColourBoxChangeHandler
  );
  lblGroupHeadingColour.FocusControl := fGroupHeadingColourBox;
  fSnippetHeadingColourBox := CreateCustomColourBox(
    fSnippetHeadingColourDlg, SnippetHeadingColourBoxChange
  );
  fSnippetHeadingColourBox.OnChange := SnippetHeadingColourBoxChange;
  lblVaultColours.FocusControl := cbVaults;
  fSourceBGColourBox := CreateCustomColourBox(
    fSourceBGColourDlg, ColourBoxChangeHandler
  );
  lblSourceBGColour.FocusControl := fSourceBGColourBox;

  PopulateFontSizeCombos;

  fSnippetHeadingColours := TDictionary<TVaultID,TColor>.Create(
    TVaultID.TComparer.Create
  );

  fVaultList := TVaultListAdapter.Create;
  fVaultList.ToStrings(cbVaults.Items);
  Assert(cbVaults.Items.Count > 0, ClassName + '.Create: no vaults');

  SetTabOrder;
end;

function TDisplayPrefsFrame.CreateCustomColourBox(
  const ColourDlg: TColorDialogEx; ChangeHandler: TNotifyEvent): TColorBoxEx;
begin
  // Create and initialise custom color combo box
  Result := TColorBoxEx.Create(Self);  // automatically freed
  Result.Parent := Self;
  Result.Width := 137;
  Result.Height := 22;
  // cbCustomColor not included in Style since assigning ColorDialog property
  // sets this style
  Result.Style := [cbStandardColors, cbExtendedColors, cbSystemColors,
    cbPrettyNames];
  Result.ItemHeight := 16;
  if Assigned(ColourDlg) then
    Result.ColorDialog := ColourDlg;
  Result.OnChange := ChangeHandler;
end;

procedure TDisplayPrefsFrame.Deactivate(const Prefs: IPreferences);
  {Called when page is deactivated. Stores information entered by user.
    @param Prefs [in] Object used to store information.
  }
var
  Vault: TVault;
begin
  Prefs.ShowNewSnippetsInNewTabs := chkSnippetsInNewTab.Checked;
  Prefs.ShowEmptySections := not chkHideEmptySections.Checked;
  Prefs.OverviewStartState := TOverviewStartState(
    cbOverviewTree.Items.Objects[cbOverviewTree.ItemIndex]
  );
  Prefs.GroupHeadingColour := fGroupHeadingColourBox.Selected;
  Prefs.SourceCodeBGcolour := fSourceBGColourBox.Selected;
  Prefs.GroupHeadingCustomColours.CopyFrom(
    fGroupHeadingColourDlg.CustomColors, True
  );

  for Vault in TVaults.Instance do
    Prefs.SetSnippetHeadingColour(
      Vault.UID, fSnippetHeadingColours[Vault.UID]
    );
  Prefs.SourceCodeBGCustomColours.CopyFrom(
    fSourceBGColourDlg.CustomColors, True
  );
  // Setting following properties to -1 causes preferences object to use their
  // default font size
  Prefs.OverviewFontSize := StrToIntDef(cbOverviewFontSize.Text, -1);
  Prefs.DetailFontSize := StrToIntDef(cbDetailFontSize.Text, -1);
end;

destructor TDisplayPrefsFrame.Destroy;
begin
  fVaultList.Free;
  fSnippetHeadingColours.Free;
  inherited;
end;

function TDisplayPrefsFrame.DisplayName: string;
  {Caption that is displayed in the tab sheet that contains this frame when
  displayed in the preference dialog box.
    @return Required display name.
  }
resourcestring
  sDisplayName = 'Display'; // display name
begin
  Result := sDisplayName;
end;

procedure TDisplayPrefsFrame.FontSizeChange(Sender: TObject);
var
  Size: Integer;  // font size entered by user
  CB: TComboBox;  // combo box that triggered event
begin
  inherited;
  Assert(Sender is TComboBox,
    ClassName + '.FontSizeChange: Sender not TComboBox');
  CB := Sender as TComboBox;
  // Do nothing if combo box text field cleared
  if CB.Text = '' then
    Exit;
  if TryStrToInt(CB.Text, Size) then
  begin
    if TFontHelper.IsInCommonFontSizeRange(Size) then
    begin
      // Combo has valid value entered: update
      CB.Tag := Size;
      fUIChanged := True;
    end
    else
    begin
      // Font size out of range
      TMessageBox.Error(
        ParentForm,
        Format(
          sErrBadOverviewFontRange,
          [TFontHelper.CommonFontSizes.Min, TFontHelper.CommonFontSizes.Max]
        )
      );
      CB.Text := IntToStr(CB.Tag);
    end;
  end
  else
  begin
    // Combo has invalid value: say so
    TMessageBox.Error(ParentForm, sErrBadOverviewFontSize);
    CB.Text := IntToStr(CB.Tag);
  end;
end;

class function TDisplayPrefsFrame.Index: Byte;
  {Index number that determines the location of the tab containing this frame
  when displayed in the preferences dialog box.
    @return Required index number.
  }
begin
  Result := 10;
end;

function TDisplayPrefsFrame.OverviewTreeStateDesc(
  const State: TOverviewStartState): string;
  {Gets description of an overview treeview startup state.
    @param State [in] State for which description is required.
    @return Required description.
  }
resourcestring
  // Startup state descriptions
  sOTSExpanded = 'Fully expanded';
  sOTSCollapsed = 'Fully collapsed';
const
  // Map of overview tree start states to descriptions
  cOTSStartStates: array[TOverviewStartState] of string = (
    sOTSExpanded, sOTSCollapsed
  );
begin
  Result := cOTSStartStates[State];
end;

procedure TDisplayPrefsFrame.PopulateFontSizeCombos;
begin
  cbOverviewFontSize.Clear;
  TFontHelper.ListCommonFontSizes(cbOverviewFontSize.Items);
  cbDetailFontSize.Clear;
  TFontHelper.ListCommonFontSizes(cbDetailFontSize.Items);
end;

function TDisplayPrefsFrame.SelectedVaultID: TVaultID;
begin
  Assert(cbVaults.ItemIndex >= 0,
    ClassName + '.SelectedVaultID: no vault selected');
  Result := fVaultList.Vault(cbVaults.ItemIndex).UID;
end;

procedure TDisplayPrefsFrame.SelectOverviewTreeState(
  const State: TOverviewStartState);
  {Selects combo box item associated with a overview treeview startup state.
    @param State [in] Startup state to be selected.
  }
var
  CBIdx: Integer; // loops through each entry in combo box
begin
  for CBIdx := 0 to Pred(cbOverviewTree.Items.Count) do
  begin
    if State = TOverviewStartState(cbOverviewTree.Items.Objects[CBIdx]) then
    begin
      cbOverviewTree.ItemIndex := CBIdx;
      Break;
    end;
  end;
end;

procedure TDisplayPrefsFrame.SetTabOrder;
begin
  cbOverviewTree.TabOrder := 0;
  chkSnippetsInNewTab.TabOrder := 1;
  chkHideEmptySections.TabOrder := 2;
  fGroupHeadingColourBox.TabOrder := 3;
  cbVaults.TabOrder := 4;
  fSnippetHeadingColourBox.TabOrder := 5;
  fSourceBGColourBox.TabOrder := 6;
  btnDefColours.TabOrder := 7;
  cbOverviewFontSize.TabOrder := 8;
  cbDetailFontSize.TabOrder := 9;
end;

procedure TDisplayPrefsFrame.SnippetHeadingColourBoxChange(Sender: TObject);
begin
  ColourBoxChangeHandler(Sender);
  fSnippetHeadingColours[SelectedVaultID] :=
    fSnippetHeadingColourBox.Selected
end;

function TDisplayPrefsFrame.UIUpdated: Boolean;
begin
  Result := fUIChanged;
end;

initialization

// Register frame with preferences dialog box
TPreferencesDlg.RegisterPage(TDisplayPrefsFrame);

end.


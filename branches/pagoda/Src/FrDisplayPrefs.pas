{
 * This Source Code Form is subject to the terms of the Mozilla Public License,
 * v. 2.0. If a copy of the MPL was not distributed with this file, You can
 * obtain one at http://mozilla.org/MPL/2.0/
 *
 * Copyright (C) 2012-2014, Peter Johnson (www.delphidabbler.com).
 *
 * $Rev$
 * $Date$
 *
 * Implements a frame that allows user to set application display preferences.
 * Designed for use as one of the tabs in the preferences dialogue box.
}


unit FrDisplayPrefs;


interface


uses
  // Delphi
  Controls, StdCtrls, Classes,
  // Project
  FrPrefsBase, UColorBoxEx, UColorDialogEx, UPreferences;


type
  TDisplayPrefsFrame = class(TPrefsBaseFrame)
    lblOverviewTree: TLabel;
    cbOverviewTree: TComboBox;
    chkHideEmptySections: TCheckBox;
    chkSnippetsInNewTab: TCheckBox;
    btnDefColours: TButton;
    lblSourceBGColour: TLabel;
    procedure chkHideEmptySectionsClick(Sender: TObject);
    procedure btnDefColoursClick(Sender: TObject);
  strict private
    var
      ///  <summary>Flag indicating if changes affect UI.</summary>
      fUIChanged: Boolean;
      fSourceBGColourBox: TColorBoxEx;
      fSourceBGColourDlg: TColorDialogEx;
    procedure SelectOverviewTreeState(const State: TOverviewStartState);
      {Selects combo box item associated with a overview treeview startup state.
        @param State [in] Startup state to be selected.
      }
    function OverviewTreeStateDesc(const State: TOverviewStartState): string;
      {Gets description of an overview treeview startup state.
        @param State [in] State for which description is required.
        @return Required description.
      }
    function CreateCustomColourBox(const ColourDlg: TColorDialogEx):
      TColorBoxEx;
    procedure ColourBoxChangeHandler(Sender: TObject);
  public
    constructor Create(AOwner: TComponent); override;
      {Class constructor. Sets up frame and populates controls.
        @param AOwner [in] Component that owns frame.
      }
    procedure Activate(const Prefs: IPreferences); override;
      {Called when page activated. Updates controls.
        @param Prefs [in] Object that provides info used to update controls.
      }
    procedure Deactivate(const Prefs: IPreferences); override;
      {Called when page is deactivated. Stores information entered by user.
        @param Prefs [in] Object used to store information.
      }
    ///  <summary>Checks if preference changes require that main window UI is
    ///  updated.</summary>
    ///  <remarks>Called when dialogue box containing frame is closing.
    ///  </remarks>
    function UIUpdated: Boolean; override;
    procedure ArrangeControls; override;
      {Arranges controls on frame. Called after frame has been sized.
      }
    function DisplayName: string; override;
      {Caption that is displayed in the tab sheet that contains this frame when
      displayed in the preference dialogue box.
        @return Required display name.
      }
    class function Index: Byte; override;
      {Index number that determines the location of the tab containing this
      frame when displayed in the preferences dialogue box.
        @return Required index number.
      }
  end;


implementation


uses
  // Delphi
  Math, Graphics, ExtCtrls,
  // Project
  FmPreferencesDlg, UColours, UCtrlArranger, UGraphicUtils;


{$R *.dfm}


{ TDisplayPrefsFrame }

procedure TDisplayPrefsFrame.Activate(const Prefs: IPreferences);
  {Called when page activated. Updates controls.
    @param Prefs [in] Object that provides info used to update controls.
  }
begin
  SelectOverviewTreeState(Prefs.OverviewStartState);
  chkHideEmptySections.OnClick := nil;  // prevent OnClick when Checked set
  chkHideEmptySections.Checked := not Prefs.ShowEmptySections;
  chkHideEmptySections.OnClick := chkHideEmptySectionsClick;
  chkSnippetsInNewTab.Checked := Prefs.ShowNewSnippetsInNewTabs;
  fSourceBGColourBox.Selected := Prefs.SourceCodeBGcolour;
  Prefs.SourceCodeBGCustomColours.CopyTo(fSourceBGColourDlg.CustomColors, True);
end;

procedure TDisplayPrefsFrame.ArrangeControls;
  {Arranges controls on frame. Called after frame has been sized.
  }
begin
  TCtrlArranger.AlignLefts(
    [
      lblOverviewTree, chkHideEmptySections, chkSnippetsInNewTab,
      lblSourceBGColour, btnDefColours
    ],
    0
  );
  TCtrlArranger.AlignLefts(
    [cbOverviewTree, fSourceBGColourBox],
    TCtrlArranger.RightOf(
      [lblOverviewTree, lblSourceBGColour],
      8
    )
  );
  TCtrlArranger.AlignVCentres(3, [lblOverviewTree, cbOverviewTree]);
  TCtrlArranger.MoveBelow(
    [lblOverviewTree, cbOverviewTree], chkSnippetsInNewTab, 24
  );
  TCtrlArranger.MoveBelow(chkSnippetsInNewTab, chkHideEmptySections, 8);
  TCtrlArranger.AlignVCentres(
    TCtrlArranger.BottomOf(chkHideEmptySections, 24),
    [lblSourceBGColour, fSourceBGColourBox]
  );
  TCtrlArranger.MoveBelow(
    [lblSourceBGColour, fSourceBGColourBox], btnDefColours, 12
  );
  chkHideEmptySections.Width := Self.Width - 16;
  chkSnippetsInNewTab.Width := Self.Width - 16;
end;

procedure TDisplayPrefsFrame.btnDefColoursClick(Sender: TObject);
begin
  // Restores source code background colour in colour combo box
  fSourceBGColourBox.Selected := clSourceBg;
  fUIChanged := True;
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
  // Create colour dialogue box
  fSourceBGColourDlg := TColorDialogEx.Create(Self);
  fSourceBGColourDlg.Title := sSourceBGColourDlgTitle;
  // Create colour combo box
  fSourceBGColourBox := CreateCustomColourBox(fSourceBGColourDlg);
  fSourceBGColourBox.TabOrder := 3;
  lblSourceBGColour.FocusControl := fSourceBGColourBox;
end;

function TDisplayPrefsFrame.CreateCustomColourBox(
  const ColourDlg: TColorDialogEx): TColorBoxEx;
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
  Result.OnChange := ColourBoxChangeHandler;
end;

procedure TDisplayPrefsFrame.Deactivate(const Prefs: IPreferences);
  {Called when page is deactivated. Stores information entered by user.
    @param Prefs [in] Object used to store information.
  }
begin
  Prefs.ShowNewSnippetsInNewTabs := chkSnippetsInNewTab.Checked;
  Prefs.ShowEmptySections := not chkHideEmptySections.Checked;
  Prefs.OverviewStartState := TOverviewStartState(
    cbOverviewTree.Items.Objects[cbOverviewTree.ItemIndex]
  );
  Prefs.SourceCodeBGcolour := fSourceBGColourBox.Selected;
  Prefs.SourceCodeBGCustomColours.CopyFrom(
    fSourceBGColourDlg.CustomColors, True
  );
end;

function TDisplayPrefsFrame.DisplayName: string;
  {Caption that is displayed in the tab sheet that contains this frame when
  displayed in the preference dialogue box.
    @return Required display name.
  }
resourcestring
  sDisplayName = 'Display'; // display name
begin
  Result := sDisplayName;
end;

class function TDisplayPrefsFrame.Index: Byte;
  {Index number that determines the location of the tab containing this frame
  when displayed in the preferences dialogue box.
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

function TDisplayPrefsFrame.UIUpdated: Boolean;
begin
  Result := fUIChanged;
end;

initialization

// Register frame with preferences dialogue box
TPreferencesDlg.RegisterPage(TDisplayPrefsFrame);

end.


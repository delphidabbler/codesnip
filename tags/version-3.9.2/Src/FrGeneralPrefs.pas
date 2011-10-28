{
 * FrGeneralPrefs.pas
 *
 * Implements a frame that allows user to set general application preferences.
 * Designed for use as one of the tabs in the preferences dialog box.
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
 * The Original Code is FrGeneralPrefs.pas
 *
 * The Initial Developer of the Original Code is Peter Johnson
 * (http://www.delphidabbler.com/).
 *
 * Portions created by the Initial Developer are Copyright (C) 2007-2010 Peter
 * Johnson. All Rights Reserved.
 *
 * Contributor(s)
 *   NONE
 *
 * ***** END LICENSE BLOCK *****
}


unit FrGeneralPrefs;


interface


uses
  // Delphi
  Controls, StdCtrls, Classes,
  // Project
  FrPrefsBase, UMeasurement, UPreferences;


type

  {
  TGeneralPrefsFrame:
    Frame that allows user to set general application preferences. Can persist
    preferences entered by user. Note: Designed for use in preferences dialog
    box.
  }
  TGeneralPrefsFrame = class(TPrefsBaseFrame)
    cbOverviewTree: TComboBox;
    cbUnits: TComboBox;
    gbDisplay: TGroupBox;
    gbMeasurement: TGroupBox;
    lblOverviewTree: TLabel;
    lblUnits: TLabel;
  strict private
    procedure SelectUnits(const MU: TMeasurementUnits);
      {Selects combo box item associated with a measurement unit.
        @param Units [in] Measurement unit to be selected.
      }
    procedure SelectOverviewTreeState(const State: TOverviewStartState);
      {Selects combo box item associated with a overview treeview startup state.
        @param State [in] Startup state to be selected.
      }
    function OverviewTreeStateDesc(const State: TOverviewStartState): string;
      {Gets description of an overview treeview startup state.
        @param State [in] State for which description is required.
        @return Required description.
      }
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
  Math,
  // Project
  FmPreferencesDlg, UGraphicUtils;


{$R *.dfm}


{ TGeneralPrefsFrame }

procedure TGeneralPrefsFrame.Activate(const Prefs: IPreferences);
  {Called when page activated. Updates controls.
    @param Prefs [in] Object that provides info used to update controls.
  }
begin
  SelectOverviewTreeState(Prefs.OverviewStartState);
  SelectUnits(Prefs.MeasurementUnits);
end;

procedure TGeneralPrefsFrame.ArrangeControls;
  {Arranges controls on frame. Called after frame has been sized.
  }
const
  Col1Left = 8;       // position of left of first column of controls
var
  Col2Left: Integer;  // position of left of second column of controls
begin
  lblOverviewTree.Left := Col1Left;
  lblUnits.Left := Col1Left;
  Col2Left := Col1Left + Max(
    StringExtent(lblUnits.Caption, lblUnits.Font).cx,
    StringExtent(lblOverviewTree.Caption, lblOverviewTree.Font).cx
  ) + 8;
  cbOverviewTree.Left := Col2Left;
  cbUnits.Left := Col2Left;
end;

constructor TGeneralPrefsFrame.Create(AOwner: TComponent);
  {Class constructor. Sets up frame and populates controls.
    @param AOwner [in] Component that owns frame.
  }
var
  OTStateIdx: TOverviewStartState;  // loops thru each overview tree start state
  UnitsIdx: TMeasurementUnits;      // loops thru each measurement unit
begin
  inherited;
  HelpKeyword := 'GeneralPrefs';
  // Populate overview tree start state
  for OTStateIdx := Low(TOverviewStartState) to High(TOverviewStartState) do
    cbOverviewTree.Items.AddObject(
      OverviewTreeStateDesc(OTStateIdx), TObject(OTStateIdx)
    );
  // Populate measurement unit combo
  for UnitsIdx := Low(TMeasurementUnits) to High(TMeasurementUnits) do
    cbUnits.Items.AddObject(UMeasurement.UnitName(UnitsIdx), TObject(UnitsIdx));
end;

procedure TGeneralPrefsFrame.Deactivate(const Prefs: IPreferences);
  {Called when page is deactivated. Stores information entered by user.
    @param Prefs [in] Object used to store information.
  }
begin
  Prefs.OverviewStartState := TOverviewStartState(
    cbOverviewTree.Items.Objects[cbOverviewTree.ItemIndex]
  );
  Prefs.MeasurementUnits := TMeasurementUnits(
    cbUnits.Items.Objects[cbUnits.ItemIndex]
  );
end;

function TGeneralPrefsFrame.DisplayName: string;
  {Caption that is displayed in the tab sheet that contains this frame when
  displayed in the preference dialog box.
    @return Required display name.
  }
resourcestring
  sDisplayName = 'General'; // display name
begin
  Result := sDisplayName;
end;

class function TGeneralPrefsFrame.Index: Byte;
  {Index number that determines the location of the tab containing this frame
  when displayed in the preferences dialog box.
    @return Required index number.
  }
begin
  Result := 10;
end;

function TGeneralPrefsFrame.OverviewTreeStateDesc(
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

procedure TGeneralPrefsFrame.SelectOverviewTreeState(
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

procedure TGeneralPrefsFrame.SelectUnits(const MU: TMeasurementUnits);
  {Selects combo box item associated with a measurement unit.
    @param Units [in] Measurement unit to be selected.
  }
var
  CBIdx: Integer; // loops through each entry in combo box
begin
  for CBIdx := 0 to Pred(cbUnits.Items.Count) do
  begin
    if MU = TMeasurementUnits(cbUnits.Items.Objects[CBIdx]) then
    begin
      cbUnits.ItemIndex := CBIdx;
      Break;
    end;
  end;
end;

initialization

// Register frame with preferences dialog box
TPreferencesDlg.RegisterPage(TGeneralPrefsFrame);

end.


{
 * This Source Code Form is subject to the terms of the Mozilla Public License,
 * v. 2.0. If a copy of the MPL was not distributed with this file, You can
 * obtain one at http://mozilla.org/MPL/2.0/
 *
 * Copyright (C) 2007-2020, Peter Johnson (gravatar.com/delphidabbler).
 *
 * Implements a frame that allows user to set general application preferences.
 * Designed for use as one of the tabs in the Preferences dialogue box.
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
    cbUnits: TComboBox;
    gbMeasurement: TGroupBox;
    lblUnits: TLabel;
  strict private
    procedure SelectUnits(const MU: TMeasurementUnits);
      {Selects combo box item associated with a measurement unit.
        @param Units [in] Measurement unit to be selected.
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
  Math,
  // Project
  FmPreferencesDlg, UCtrlArranger, UGraphicUtils;


{$R *.dfm}


{ TGeneralPrefsFrame }

procedure TGeneralPrefsFrame.Activate(const Prefs: IPreferences);
  {Called when page activated. Updates controls.
    @param Prefs [in] Object that provides info used to update controls.
  }
begin
  SelectUnits(Prefs.MeasurementUnits);
end;

procedure TGeneralPrefsFrame.ArrangeControls;
  {Arranges controls on frame. Called after frame has been sized.
  }
const
  Col1Left = 8;       // position of left of first column of controls
begin
  lblUnits.Left := Col1Left;
  TCtrlArranger.MoveToRightOf(lblUnits, cbUnits, 8);
  gbMeasurement.Top := 0;
  TCtrlArranger.AlignVCentres(20, [lblUnits, cbUnits]);
  gbMeasurement.ClientHeight := TCtrlArranger.TotalControlHeight(gbMeasurement)
    + 12;
end;

constructor TGeneralPrefsFrame.Create(AOwner: TComponent);
  {Class constructor. Sets up frame and populates controls.
    @param AOwner [in] Component that owns frame.
  }
var
  UnitsIdx: TMeasurementUnits;      // loops thru each measurement unit
begin
  inherited;
  HelpKeyword := 'GeneralPrefs';
  // Populate measurement unit combo
  for UnitsIdx := Low(TMeasurementUnits) to High(TMeasurementUnits) do
    cbUnits.Items.AddObject(UMeasurement.UnitName(UnitsIdx), TObject(UnitsIdx));
end;

procedure TGeneralPrefsFrame.Deactivate(const Prefs: IPreferences);
  {Called when page is deactivated. Stores information entered by user.
    @param Prefs [in] Object used to store information.
  }
begin
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
  sDisplayName = 'Misc.'; // display name
begin
  Result := sDisplayName;
end;

class function TGeneralPrefsFrame.Index: Byte;
  {Index number that determines the location of the tab containing this frame
  when displayed in the preferences dialog box.
    @return Required index number.
  }
begin
  Result := 255;
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

function TGeneralPrefsFrame.UIUpdated: Boolean;
begin
  Result := False;
end;

initialization

// Register frame with preferences dialog box
TPreferencesDlg.RegisterPage(TGeneralPrefsFrame);

end.


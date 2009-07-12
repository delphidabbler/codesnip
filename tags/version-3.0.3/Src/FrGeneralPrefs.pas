{
 * FrGeneralPrefs.pas
 *
 * Implements a frame that allows user to set general application preferences.
 * Designed for use as one of the tabs in the preferences dialog box.
 *
 * v1.0 of 07 Sep 2007  - Original version.
 * v1.1 of 17 Oct 2007  - Refactored code that gets names of measurement units.
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
 * The Original Code is FrGeneralPrefs.pas
 *
 * The Initial Developer of the Original Code is Peter Johnson
 * (http://www.delphidabbler.com/).
 *
 * Portions created by the Initial Developer are Copyright (C) 2007 Peter
 * Johnson. All Rights Reserved.
 *
 * Contributor(s): None
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
    gpMeasurement: TGroupBox;
    lblUnits: TLabel;
    cbUnits: TComboBox;
  private
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
  end;


implementation


{$R *.dfm}


{ TGeneralPrefsFrame }

procedure TGeneralPrefsFrame.Activate(const Prefs: IPreferences);
  {Called when page activated. Updates controls.
    @param Prefs [in] Object that provides info used to update controls.
  }
begin
  SelectUnits(Prefs.MeasurementUnits);
end;

constructor TGeneralPrefsFrame.Create(AOwner: TComponent);
  {Class constructor. Sets up frame and populates controls.
    @param AOwner [in] Component that owns frame.
  }
var
  UnitsIdx: TMeasurementUnits;  // loops through each measurement unit
begin
  inherited;
  // Populate measurement unit combo
  for UnitsIdx := Low(TMeasurementUnits) to High(TMeasurementUnits) do
    cbUnits.Items.AddObject(UnitName(UnitsIdx), TObject(UnitsIdx));
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

end.


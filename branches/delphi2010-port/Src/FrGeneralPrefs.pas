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
 * Portions created by the Initial Developer are Copyright (C) 2007-2009 Peter
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
    gpMeasurement: TGroupBox;
    lblUnits: TLabel;
    cbUnits: TComboBox;
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
  // Project
  FmPreferencesDlg;


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
begin
  cbUnits.Left := lblUnits.Left + lblUnits.Width + 8;
end;

constructor TGeneralPrefsFrame.Create(AOwner: TComponent);
  {Class constructor. Sets up frame and populates controls.
    @param AOwner [in] Component that owns frame.
  }
var
  UnitsIdx: TMeasurementUnits;  // loops through each measurement unit
begin
  inherited;
  HelpKeyword := 'GeneralPrefs';
  // Populate measurement unit combo
  for UnitsIdx := Low(TMeasurementUnits) to High(TMeasurementUnits) do
    { TODO -oSelf -cD2010Change : Added UMeasurement prefix to UnitName }
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


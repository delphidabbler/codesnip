{
 * This Source Code Form is subject to the terms of the Mozilla Public License,
 * v. 2.0. If a copy of the MPL was not distributed with this file, You can
 * obtain one at http://mozilla.org/MPL/2.0/
 *
 * Copyright (C) 2013, Peter Johnson (www.delphidabbler.com).
 *
 * $Rev$
 * $Date$
 *
 * Implements a frame to be hosted in the preferences diaogue box that permits
 * the user to set their application and database update preferences.
}

{ TODO -cWeb: Remove this frame and all associated prefs once database update
  code revised not to use web service. }

unit FrUpdatePrefs;


interface


uses
  // Delphi
  Classes, Controls, StdCtrls,
  // Project
  FrPrefsBase, UPreferences;


type
  ///  <summary>Frame that allows the user to set their application and database
  ///  update preferences.</summary>
  ///  <remarks>Designed for use within the preferences dialog box.</remarks>
  TUpdatePrefsFrame = class(TPrefsBaseFrame)
    lblProgAutoCheckFreq: TLabel;
    lblDBAutoCheckFreq: TLabel;
    cbProgAutoCheckFreq: TComboBox;
    cbDBAutoCheckFreq: TComboBox;
    ///  <summary>Handles OnChange events on auto-update frequency check boxes.
    ///  Sets changed flag since display may need to be updated.</summary>
    procedure CBAutoCheckFreqChanged(Sender: TObject);
  strict private
    var
      ///  <summary>Flag indicating if user has changed any update frequencies.
      ///  </summary>
      fChanged: Boolean;
    ///  <summary>Populates the given combo box with values for each available
    ///  auto update check frequency.</summary>
    procedure PopulateFrequencyCB(const CB: TComboBox);
    ///  <summary>Gets the currently selected auto-update frequency from the
    ///  given combo box.</summary>
    function GetFrequencyFromCB(const CB: TComboBox): Word;
    ///  <summary>Selects the given combo box item that relates to the given
    ///  auto-update check frequency.</summary>
    procedure SelectFrequencyInCB(const CB: TComboBox; const Freq: Word);
  public
    ///  <summary>Constructs new frame instance and initialises controls.
    ///  </summary>
    constructor Create(AOwner: TComponent); override;
    ///  <summary>Updates controls to reflect values recorded in given
    ///  preferences object.</summary>
    ///  <remarks>Called when the page is activated.</remarks>
    procedure Activate(const Prefs: IPreferences); override;
    ///  <summary>Updates given preferences object from values entered in
    ///  controls.</summary>
    ///  <remarks>Called when the page is deactivated.</remarks>
    procedure Deactivate(const Prefs: IPreferences); override;
    ///  <summary>Checks if preference changes require that main window UI is
    ///  updated.</summary>
    ///  <returns>Boolean. Always False.</returns>
    ///  <remarks>Called when dialog box containing frame is closing.</remarks>
    function UIUpdated: Boolean; override;
    ///  <summary>Arranges the frame's controls.</summary>
    ///  <remarks>Called after frame has been (re)sized.</remarks>
    procedure ArrangeControls; override;
    ///  <summary>Returns the caption that is to be displayed to identify this
    ///  to the user.</summary>
    function DisplayName: string; override;
    ///  <summary>Returns a number that determines the position of this frame
    ///  relative to other frames in host control.</summary>
    class function Index: Byte; override;
  end;


implementation


uses
  // Delphi
  Math,
  // Project
  FmPreferencesDlg, UCtrlArranger, UGraphicUtils;


{$R *.dfm}


{ TUpdatePrefsFrame }

procedure TUpdatePrefsFrame.Activate(const Prefs: IPreferences);
begin
  SelectFrequencyInCB(cbProgAutoCheckFreq, Prefs.AutoCheckProgramFrequency);
  SelectFrequencyInCB(cbDBAutoCheckFreq, Prefs.AutoCheckDatabaseFrequency);
end;

procedure TUpdatePrefsFrame.ArrangeControls;
begin
  TCtrlArranger.AlignLefts(
    [lblProgAutoCheckFreq, lblDBAutoCheckFreq], 0
  );
  TCtrlArranger.AlignLefts(
    [cbProgAutoCheckFreq, cbDBAutoCheckFreq],
    Max(
      StringExtent(lblProgAutoCheckFreq.Caption, lblProgAutoCheckFreq.Font).cx,
      StringExtent(lblDBAutoCheckFreq.Caption, lblDBAutoCheckFreq.Font).cx
    ) + 12
  );
  TCtrlArranger.AlignVCentres(0, [lblProgAutoCheckFreq, cbProgAutoCheckFreq]);
  TCtrlArranger.AlignVCentres(
    TCtrlArranger.BottomOf([lblProgAutoCheckFreq, cbProgAutoCheckFreq], 12),
    [lblDBAutoCheckFreq, cbDBAutoCheckFreq]
  );
end;

procedure TUpdatePrefsFrame.CBAutoCheckFreqChanged(Sender: TObject);
begin
  fChanged := True;
end;

constructor TUpdatePrefsFrame.Create(AOwner: TComponent);
begin
  inherited;
  PopulateFrequencyCB(cbProgAutoCheckFreq);
  PopulateFrequencyCB(cbDBAutoCheckFreq);
end;

procedure TUpdatePrefsFrame.Deactivate(const Prefs: IPreferences);
begin
  if cbProgAutoCheckFreq.ItemIndex >= 0 then
    Prefs.AutoCheckProgramFrequency := GetFrequencyFromCB(cbProgAutoCheckFreq);
  if cbDBAutoCheckFreq.ItemIndex >= 0 then
    Prefs.AutoCheckDatabaseFrequency := GetFrequencyFromCB(cbDBAutoCheckFreq);
end;

function TUpdatePrefsFrame.DisplayName: string;
resourcestring
  sDisplayName = 'Updates';
begin
  Result := sDisplayName;
end;

function TUpdatePrefsFrame.GetFrequencyFromCB(const CB: TComboBox): Word;
begin
  if CB.ItemIndex = -1 then
    Exit(0);
  Result := Word(CB.Items.Objects[CB.ItemIndex]);
end;

class function TUpdatePrefsFrame.Index: Byte;
begin
  Result := 60;
end;

procedure TUpdatePrefsFrame.PopulateFrequencyCB(const CB: TComboBox);
resourcestring
  s0 = 'Never';
  s1 = 'Daily';
  s3 = 'Every 3 days';
  s7 = 'Weekly';
  s14 = 'Fortnightly';
  s28 = 'Monthly';
const
  FreqMap: array[0..5] of record
    Text: string;
    Value: Word;
  end = (
    (Text: s0; Value: 0),
    (Text: s1; Value: 1),
    (Text: s3; Value: 3),
    (Text: s7; Value: 7),
    (Text: s14; Value: 14),
    (Text: s28; Value: 28)
  );
var
  I: Integer;
begin
  CB.Clear;
  for I := Low(FreqMap) to High(FreqMap) do
    CB.Items.AddObject(FreqMap[I].Text, TObject(FreqMap[I].Value));
end;

procedure TUpdatePrefsFrame.SelectFrequencyInCB(const CB: TComboBox;
  const Freq: Word);
var
  I: Integer;
begin
  for I := 0 to Pred(CB.Items.Count) do
  begin
    if Word(CB.Items.Objects[I]) = Freq then
    begin
      CB.ItemIndex := I;
      Exit;
    end;
  end;
  CB.ItemIndex := -1;
end;

function TUpdatePrefsFrame.UIUpdated: Boolean;
begin
  Result := fChanged;
end;

initialization

// Register frame with preferences dialog box
TPreferencesDlg.RegisterPage(TUpdatePrefsFrame);

end.


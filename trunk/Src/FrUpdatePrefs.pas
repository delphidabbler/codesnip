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
    chkAutoCheckProgramUpdates: TCheckBox;
    chkAutoCheckDatabaseUpdates: TCheckBox;
  public
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
  // Project
  FmPreferencesDlg, UCtrlArranger;


{$R *.dfm}


{ TUpdatePrefsFrame }

procedure TUpdatePrefsFrame.Activate(const Prefs: IPreferences);
begin
  chkAutoCheckProgramUpdates.Checked := Prefs.AutoCheckProgramUpdates;
  chkAutoCheckDatabaseUpdates.Checked := Prefs.AutoCheckDatabaseUpdates;
end;

procedure TUpdatePrefsFrame.ArrangeControls;
begin
  TCtrlArranger.AlignLefts(
    [chkAutoCheckProgramUpdates, chkAutoCheckDatabaseUpdates], 0
  );
  chkAutoCheckProgramUpdates.Top := 4;
  TCtrlArranger.MoveBelow(
    chkAutoCheckProgramUpdates, chkAutoCheckDatabaseUpdates, 12
  );
end;

procedure TUpdatePrefsFrame.Deactivate(const Prefs: IPreferences);
begin
  Prefs.AutoCheckProgramUpdates := chkAutoCheckProgramUpdates.Checked;
  Prefs.AutoCheckDatabaseUpdates := chkAutoCheckDatabaseUpdates.Checked;
end;

function TUpdatePrefsFrame.DisplayName: string;
resourcestring
  sDisplayName = 'Updates';
begin
  Result := sDisplayName;
end;

class function TUpdatePrefsFrame.Index: Byte;
begin
  Result := 60;
end;

function TUpdatePrefsFrame.UIUpdated: Boolean;
begin
  Result := False;
end;

initialization

// Register frame with preferences dialog box
TPreferencesDlg.RegisterPage(TUpdatePrefsFrame);

end.

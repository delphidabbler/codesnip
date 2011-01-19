{
 * FrNewsPrefs.pas
 *
 * Implements a frame that allows user to set preferences that relate to news
 * items. Designed for use as one of the tabs in the preferences dialog box.
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
 * The Original Code is FrNewsPrefs.pas
 *
 * The Initial Developer of the Original Code is Peter Johnson
 * (http://www.delphidabbler.com/).
 *
 * Portions created by the Initial Developer are Copyright (C) 2011 Peter
 * Johnson. All Rights Reserved.
 *
 * Contributor(s)
 *   NONE
 *
 * ***** END LICENSE BLOCK *****
}


unit FrNewsPrefs;


interface


uses
  // Delphi
  Controls, StdCtrls, Spin, Classes,
  // Project
  FrPrefsBase, UPreferences;


type
  {
  TNewsPrefsFrame:
    Frame that allows user to set preferences that relate to news items. Can
    persist preferences entered by user. Note: Designed for use in preferences
    dialog box.
  }
  TNewsPrefsFrame = class(TPrefsBaseFrame)
    lblAgePrefix: TLabel;
    lblAgeSuffix: TLabel;
    seAge: TSpinEdit;
  public
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
  FmPreferencesDlg, UCtrlArranger;


{$R *.dfm}

{ TNewPrefsFrame }

procedure TNewsPrefsFrame.Activate(const Prefs: IPreferences);
  {Called when page activated. Updates controls.
    @param Prefs [in] Object that provides info used to update controls.
  }
begin
  seAge.Value := Prefs.NewsAge;
end;

procedure TNewsPrefsFrame.ArrangeControls;
  {Arranges controls on frame. Called after frame has been sized.
  }
begin
  lblAgePrefix.Left := 0;
  TCtrlArranger.MoveToRightOf(lblAgePrefix, seAge, 6);
  TCtrlArranger.MoveToRightOf(seAge, lblAgeSuffix, 6);
  TCtrlArranger.AlignVCentres(8, [lblAgePrefix, seAge, lblAgeSuffix]);
end;

procedure TNewsPrefsFrame.Deactivate(const Prefs: IPreferences);
  {Called when page is deactivated. Stores information entered by user.
    @param Prefs [in] Object used to store information.
  }
begin
  Prefs.NewsAge := seAge.Value;
end;

function TNewsPrefsFrame.DisplayName: string;
  {Caption that is displayed in the tab sheet that contains this frame when
  displayed in the preference dialog box.
    @return Required display name.
  }
resourcestring
  sDisplayName = 'News'; // display name
begin
  Result := sDisplayName;
end;

class function TNewsPrefsFrame.Index: Byte;
  {Index number that determines the location of the tab containing this frame
  when displayed in the preferences dialog box.
    @return Required index number.
  }
begin
  Result := 50;
end;

initialization

// Register frame with preferences dialog box
TPreferencesDlg.RegisterPage(TNewsPrefsFrame);

end.

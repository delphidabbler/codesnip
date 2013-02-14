{
 * This Source Code Form is subject to the terms of the Mozilla Public License,
 * v. 2.0. If a copy of the MPL was not distributed with this file, You can
 * obtain one at http://mozilla.org/MPL/2.0/
 *
 * Copyright (C) 2011-2012, Peter Johnson (www.delphidabbler.com).
 *
 * $Rev$
 * $Date$
 *
 * Implements a frame that allows user to set preferences that relate to news
 * items.
 * Designed for use as one of the tabs in the Preferences dialogue box.
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
    constructor Create(AOwner: TComponent); override;
      {Object constructor. Sets up frame.
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
    ///  <remarks>Called when dialog box containing frame is closing. Always
    ///  returns False because these preferences never affect UI.</remarks>
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

constructor TNewsPrefsFrame.Create(AOwner: TComponent);
  {Object constructor. Sets up frame.
    @param AOwner [in] Component that owns frame.
  }
begin
  inherited;
  HelpKeyword := 'NewsPrefs';
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

function TNewsPrefsFrame.UIUpdated: Boolean;
begin
  Result := False;
end;

initialization

// Register frame with preferences dialog box
TPreferencesDlg.RegisterPage(TNewsPrefsFrame);

end.

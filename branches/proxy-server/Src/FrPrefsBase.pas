{
 * FrPrefsBase.pas
 *
 * Implements frame that forms an abstract base class for all frames displayed
 * in preferences dialog box.
 *
 * v1.0 of 09 Nov 2006  - Original version.
 * v2.0 of 07 Sep 2007  - Major update:
 *                        - No longer pure abstract class. Default
 *                          implementations of LoadPrefs and SavePrefs methods
 *                          provided.
 *                        - Changed to load and save data to local preferences
 *                          object rather than main preferences.
 *                        - Added Activate and Deactivate abstract methods
 *                          called when frame is displayed or hidden. Used to
 *                          update controls and store entered data.
 * v2.1 of 19 Jul 2009  - Added new abstract ArrangeControls method that
 *                        preferences dialog box calls to get frames to
 *                        arrange controls after frames have been sized.
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
 * The Original Code is FrPrefsBase.pas
 *
 * The Initial Developer of the Original Code is Peter Johnson
 * (http://www.delphidabbler.com/).
 *
 * Portions created by the Initial Developer are Copyright (C) 2006-2009 Peter
 * Johnson. All Rights Reserved.
 *
 * ***** END LICENSE BLOCK *****
}


unit FrPrefsBase;


interface


uses
  // Delphi
  Forms, Windows,
  // Project
  UPreferences;


type

  {
  TPrefsBaseFrame:
    Frame that forms an abstract base class for all frames displayed in
    preferences dialog box.
  }
  TPrefsBaseFrame = class(TFrame)
  public
    procedure SavePrefs(const Prefs: IPreferences); virtual;
      {Saves information user entered in frame. By default the method simply
      calls Deactivate. May be overridden to save any custom data that doesn't
      use Prefs object.
        @param Prefs [in] Object used to store information.
      }
    procedure LoadPrefs(const Prefs: IPreferences); virtual;
      {Initialises controls. By default the method simply calls Activate. May be
      overridden to load any custom data that doesn't use Prefs object.
        @param Prefs [in] Object that provides info used to update controls.
      }
    procedure Activate(const Prefs: IPreferences); virtual; abstract;
      {Called when page activated. Must update controls from preferences.
        @param Prefs [in] Object that provides info used to update controls.
      }
    procedure Deactivate(const Prefs: IPreferences); virtual; abstract;
      {Called when page is deactivated. Must store information entered by user.
        @param Prefs [in] Object used to store information.
      }
    procedure ArrangeControls; virtual; abstract;
      {Arranges controls on frame. Called after frame has been sized.
      }
  end;


implementation


{$R *.dfm}

{ TPrefsBaseFrame }

procedure TPrefsBaseFrame.LoadPrefs(const Prefs: IPreferences);
  {Initialises controls. By default the method simply calls Activate. May be
  overridden to load any custom data that doesn't use Prefs object.
    @param Prefs [in] Object that provides info used to update controls.
  }
begin
  Activate(Prefs);
end;

procedure TPrefsBaseFrame.SavePrefs(const Prefs: IPreferences);
  {Saves information user entered in frame. By default the method simply calls
  Deactivate. May be overridden to save any custom data that doesn't use Prefs
  object.
    @param Prefs [in] Object used to store information.
  }
begin
  Deactivate(Prefs);
end;

end.


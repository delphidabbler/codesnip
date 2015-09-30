{
 * This Source Code Form is subject to the terms of the Mozilla Public License,
 * v. 2.0. If a copy of the MPL was not distributed with this file, You can
 * obtain one at http://mozilla.org/MPL/2.0/
 *
 * Copyright (C) 2006-2014, Peter Johnson (www.delphidabbler.com).
 *
 * $Rev$
 * $Date$
 *
 * Implements a frame that forms an abstract base class for all frames displayed
 * in the Preferences dialogue box.
}


unit FrPrefsBase;


interface


uses
  // Delphi
  Forms,
  // Project
  UPreferences;


type

  {
  TPrefsFrameClass:
    Class reference for preferences frames. All preferences frame must descend
    from TPrefsBaseFrame.
  }
  TPrefsFrameClass = class of TPrefsBaseFrame;

  {
  TPrefsBaseFrame:
    Frame that forms an abstract base class for all frames displayed in
    preferences dialogue box.
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
    ///  <summary>Checks if preference changes require that main window UI is
    ///  updated.</summary>
    ///  <remarks>Called when dialogue box containing frame is closing.</remarks>
    function UIUpdated: Boolean; virtual; abstract;
    procedure ArrangeControls; virtual; abstract;
      {Arranges controls on frame. Called after frame has been sized.
      }
    function DisplayName: string; virtual; abstract;
      {Provides caption that is displayed in the tab sheet that contains this
      frame when displayed in the preference dialogue box.
        @return Required display name.
      }
    class function Index: Byte; virtual; abstract;
      {Provides an index number that determines the order in which the tabs
      containing frames are displayed in the preferences dialogue box. Gaps
      between indexes should be left where possible to allow for insertion of
      new entries at a later date.
        @return Required index number.
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


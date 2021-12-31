{
 * This Source Code Form is subject to the terms of the Mozilla Public License,
 * v. 2.0. If a copy of the MPL was not distributed with this file, You can
 * obtain one at https://mozilla.org/MPL/2.0/
 *
 * Copyright (C) 2006-2021, Peter Johnson (gravatar.com/delphidabbler).
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
    preferences dialog box.
  }
  TPrefsBaseFrame = class(TFrame)
  strict protected
    class function IsFlagSupported(const Flag: UInt64): Boolean; inline;
    class function ExtractFrameFlag(const Flag: UInt64): UInt32; inline;
    ///  <summary>Returns reference to form that hosts the frame.</summary>
    function ParentForm: TForm;
  public
    procedure SavePrefs(const Prefs: IPreferences); virtual;
      {Saves information user entered in frame. By default the method simply
      calls Deactivate. May be overridden to save any custom data that doesn't
      use Prefs object.
        @param Prefs [in] Object used to store information.
      }
    procedure LoadPrefs(const Prefs: IPreferences; const Flags: UInt64);
      virtual;
      {Initialises controls. By default the method simply calls Activate. May be
      overridden to load any custom data that doesn't use Prefs object.
        @param Prefs [in] Object that provides info used to update controls.
      }
    procedure Activate(const Prefs: IPreferences; const Flags: UInt64);
      virtual; abstract;
      {Called when page activated. Must update controls from preferences.
        @param Prefs [in] Object that provides info used to update controls.
      }
    procedure Deactivate(const Prefs: IPreferences); virtual; abstract;
      {Called when page is deactivated. Must store information entered by user.
        @param Prefs [in] Object used to store information.
      }
    ///  <summary>Checks if preference changes require that main window UI is
    ///  updated.</summary>
    ///  <remarks>Called when dialog box containing frame is closing.</remarks>
    function UIUpdated: Boolean; virtual; abstract;
    procedure ArrangeControls; virtual; abstract;
      {Arranges controls on frame. Called after frame has been sized.
      }
    function DisplayName: string; virtual; abstract;
      {Provides caption that is displayed in the tab sheet that contains this
      frame when displayed in the preference dialog box.
        @return Required display name.
      }
    class function Index: Byte; virtual; abstract;
      {Provides an index number that determines the order in which the tabs
      containing frames are displayed in the preferences dialog box. Gaps
      between indexes should be left where possible to allow for insertion of
      new entries at a later date.
        @return Required index number.
      }
    class function MakeFrameFlag(const Flag: UInt32): UInt64; inline;
  end;


implementation

uses
  // Delphi
  SysUtils,
  Controls;


{$R *.dfm}

{ TPrefsBaseFrame }

class function TPrefsBaseFrame.ExtractFrameFlag(const Flag: UInt64): UInt32;
begin
  if not IsFlagSupported(Flag) then
    Exit(0);
  Result := Int64Rec(Flag).Lo;
end;

class function TPrefsBaseFrame.IsFlagSupported(const Flag: UInt64): Boolean;
begin
  Result := Int64Rec(Flag).Hi = UInt32(Index);
end;

procedure TPrefsBaseFrame.LoadPrefs(const Prefs: IPreferences;
  const Flags: UInt64);
  {Initialises controls. By default the method simply calls Activate. May be
  overridden to load any custom data that doesn't use Prefs object.
    @param Prefs [in] Object that provides info used to update controls.
  }
begin
  Activate(Prefs, Flags);
end;

class function TPrefsBaseFrame.MakeFrameFlag(const Flag: UInt32): UInt64;
begin
  // Frame flag is in form $IIIIIIIIFFFFFFFF where $IIIIIIII is the frame's
  // index number and $FFFFFFFF is the 32 bit flag or bitmask of flags
  Int64Rec(Result).Hi := UInt32(Index);
  Int64Rec(Result).Lo := Flag;
end;

function TPrefsBaseFrame.ParentForm: TForm;
var
  ParentCtrl: TWinControl;  // reference to parent controls
begin
  // Loop through parent controls until form found or top level parent reached
  ParentCtrl := Self.Parent;
  while Assigned(ParentCtrl) and not (ParentCtrl is TForm) do
    ParentCtrl := ParentCtrl.Parent;
  if ParentCtrl is TForm then
    Result := ParentCtrl as TForm
  else
    Result := nil;
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


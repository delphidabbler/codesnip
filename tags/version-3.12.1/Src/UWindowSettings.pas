{
 * UWindowSettings.pas
 *
 * Implements a class that can save and record position of main window along
 * with some window widgets.
 *
 * Requires DelphiDabbler Window State Components v5.3 of later.
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
 * The Original Code is UWindowSettings.pas
 *
 * The Initial Developer of the Original Code is Peter Johnson
 * (http://www.delphidabbler.com/).
 *
 * Portions created by the Initial Developer are Copyright (C) 2006-2010 Peter
 * Johnson. All Rights Reserved.
 *
 * Contributor(s)
 *   NONE
 *
 * ***** END LICENSE BLOCK *****
}


unit UWindowSettings;


interface


uses
  // Delphi
  Classes,
  // DelphiDabbler library
  PJWdwState;


type

  {
  TWindowSettings:
    Class that can save and record position of main window along with some of
    window's widgets.
  }
  TWindowSettings = class(TPJCustomWdwState)
  strict private
    var
      fSplitterPos: Integer;      // Value of SplitterPos property
      fOverviewTab: Integer;      // Value of OverviewTab property
      fDetailTab: Integer;        // Value of DetailTab property
    const
      cDefLeftPanelWidth  = 186;  // Default width of left hand panel
  strict protected
    procedure ReadWdwState(var Left, Top, Width, Height, State: Integer);
      override;
      {Reads window state from persistent local settings. Also reads information
      about location, size and state of other main window widgets and sets
      appropriate property. If persistent storage not present window defaults to
      designed size and is placed at centre of work area.
        @param Left [in/out] Location of left hand side of window. Designed
          value passed in. Set to value from persistent storage if available,
          otherwise set to a value that centres window horizontally in work
          area.
        @param Top [in/out] Location of top of window. Designed value passed in.
          Set to value from persistent storage if available, otherwise set to a
          value that centres window vertically in work area.
        @param Width [in/out] Width of window. Passed in as designed width. Set
          to width from persistent storage if available, otherwise unchanged.
        @param Height [in/out] Height of window. Passed in as designed height.
          Set to height from persistent storage if available, otherwise
          unchanged.
        @param State [in/out] State of window (ordinal value of TWindowState
          enumeration). Passed in as designed state. Set to state from
          persistent storage if available, otherwise unchanged.
      }
    procedure SaveWdwState(const Left, Top, Width, Height, State: Integer);
      override;
      {Writes window state to persistent local settings. Also writes information
      about location, size and state of other main window widgets per assoicated
      property values.
        @param Left [in] Location of left hand side of window.
        @param Top [in] Location of top of window.
        @param Width [in] Width of window.
        @param Height [in] Height of window.
        @param State [in] State of window (ordinal value of TWindowState
          enumeration). If state is minimized this is ignored and normal state
          used.
      }
  public
    const
      MinLeftPanelWidth  = 160; // minimum width of left panel
      MinRightPanelWidth = 300; // minimum width of right panel
    constructor Create(AOwner: TComponent); override;
      {Class constructor. Sets up object and default property values.
        @param AOwner [in] Owning component (must be a TForm).
      }
    property SplitterPos: Integer
      read fSplitterPos write fSplitterPos default cDefLeftPanelWidth;
      {Position of main window's vertical splitter}
    property OverviewTab: Integer
      read fOverviewTab write fOverviewTab;
      {Index of selected tab in overview pane}
    property DetailTab: Integer
      read fDetailTab write fDetailTab;
      {Index of selected tab in detail pane}
  end;


implementation


uses
  // Delphi
  SysUtils, Forms, Windows,
  // Project
  USettings, UStructs;


{ TWindowSettings }

constructor TWindowSettings.Create(AOwner: TComponent);
  {Class constructor. Sets up object and default property values.
    @param AOwner [in] Owning component (must be a TForm).
  }
begin
  inherited;
  AutoSaveRestore := False;           // need to call Save and Restore manually
  Options := [woFitWorkArea];         // keep main window inside work area
  fSplitterPos := cDefLeftPanelWidth; // default splitter position
end;

procedure TWindowSettings.ReadWdwState(var Left, Top, Width, Height,
  State: Integer);
  {Reads window state from persistent local settings. Also reads information
  about location, size and state of other main window widgets and sets
  appropriate property. If persistent storage not present window defaults to
  designed size and is placed at centre of work area.
    @param Left [in/out] Location of left hand side of window. Designed value
      passed in. Set to value from persistent storage if available, otherwise
      set to a value that centres window horizontally in work area.
    @param Top [in/out] Location of top of window. Designed value passed in. Set
      to value from persistent storage if available, otherwise set to a value
      that centres window vertically in work area.
    @param Width [in/out] Width of window. Passed in as designed width. Set to
      width from persistent storage if available, otherwise unchanged.
    @param Height [in/out] Height of window. Passed in as designed height. Set
      to height from persistent storage if available, otherwise unchanged.
    @param State [in/out] State of window (ordinal value of TWindowState
      enumeration). Passed in as designed state. Set to state from persistent
      storage if available, otherwise unchanged.
  }
var
  Section: ISettingsSection;  // object used to access persistent storage
  WorkArea: TRectEx;          // screen's workarea
begin
  // Set default values: centre form in workarea using designed width & height
  WorkArea := Screen.WorkAreaRect;
  Left := (WorkArea.Width - Width) div 2;
  Top := (WorkArea.Height - Height) div 2;
  // Read values from storage
  Section := Settings.ReadSection(ssMainWindow);
  Left := StrToIntDef(Section.ItemValues['Left'], Left);
  Top := StrToIntDef(Section.ItemValues['Top'], Top);
  Width := StrToIntDef(Section.ItemValues['Width'], Width);
  Height := StrToIntDef(Section.ItemValues['Height'], Height);
  State := StrToIntDef(Section.ItemValues['State'], State);
  if TWindowState(State) = wsMinimized then
    State := Ord(wsNormal);   // we don't allow minimized: use normal
  fSplitterPos := StrToIntDef(Section.ItemValues['SplitterPos'], fSplitterPos);
  fOverviewTab := StrToIntDef(Section.ItemValues['OverviewTab'], fOverviewTab);
  fDetailTab := StrToIntDef(Section.ItemValues['DetailTab'], fDetailTab);
end;

procedure TWindowSettings.SaveWdwState(const Left, Top, Width, Height,
  State: Integer);
  {Writes window state to persistent local settings. Also writes information
  about location, size and state of other main window widgets per assoicated
  property values.
    @param Left [in] Location of left hand side of window.
    @param Top [in] Location of top of window.
    @param Width [in] Width of window.
    @param Height [in] Height of window.
    @param State [in] State of window (ordinal value of TWindowState
      enumeration). If state is minimized this is ignored and normal state used.
  }
var
  Section: ISettingsSection;  // object used to access persistent storage
begin
  Section := Settings.ReadSection(ssMainWindow);
  Section.ItemValues['Left'] := IntToStr(Left);
  Section.ItemValues['Top'] := IntToStr(Top);
  Section.ItemValues['Width'] := IntToStr(Width);
  Section.ItemValues['Height'] := IntToStr(Height);
  Section.ItemValues['State'] := IntToStr(State);
  Section.ItemValues['SplitterPos'] := IntToStr(fSplitterPos);
  Section.ItemValues['OverviewTab'] := IntToStr(fOverviewTab);
  Section.ItemValues['DetailTab'] := IntToStr(fDetailTab);
  Section.Save;
end;

end.


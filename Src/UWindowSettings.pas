{
 * This Source Code Form is subject to the terms of the Mozilla Public License,
 * v. 2.0. If a copy of the MPL was not distributed with this file, You can
 * obtain one at https://mozilla.org/MPL/2.0/
 *
 * Copyright (C) 2006-2020, Peter Johnson (gravatar.com/delphidabbler).
 *
 * Implements classes that can save and record position of a window along with
 * information about any window widgets.
 *
 * Requires DelphiDabbler Window State Components v5.3 of later.
}


unit UWindowSettings;


interface


uses
  // Delphi
  Classes,
  // DelphiDabbler library
  PJWdwState;


type
  ///  <summary>Class that can save and record the position of the main program
  ///  window along with some of the windows widgets.</summary>
  TMainWindowSettings = class(TPJCustomWdwState)
  strict private
    var
      ///  <summary>Value of SplitterPos property.</summary>
      fSplitterPos: Integer;
      ///  <summary>Value of OverviewTab property.</summary>
      fOverviewTab: Integer;
    const
      ///  <summary>Default width of form's left hand panel.</summary>
      cDefLeftPanelWidth  = 186;

  strict protected
    ///  <summary>Reads window size, position and state from persistent settings
    ///  along with information about some main window widgets, setting the
    ///  appropriate properties.</summary>
    ///  <remarks>If persistent information is not present the window defaults
    ///  to its designed size and is placed at centre of work area.</remarks>
    ///  <param name="Left">Integer [in/out] Location of left hand side of
    ///  window. Input value is ignored. Set to value read from persistent
    ///  storage if available, otherwise set to a value that centres window
    ///  horizontally in work area.</param>
    ///  <param name="Top">Integer [in/out] Location of top of window. Input
    ///  value is ignored. Set to value read from persistent storage if
    ///  available, otherwise set to a value that centres window vertically in
    ///  work area.</param>
    ///  <param name="Width">Integer [in/out] Width of window. Designed width is
    ///  passed in. Set to width read from persistent storage if available,
    ///  otherwise left unchanged.</param>
    ///  <param name="Height">Integer [in/out] Height of window. Designed height
    ///  is passed in. Set to height read from persistent storage if available,
    ///  otherwise left unchanged.</param>
    ///  <param name="State">Integer [in/out] State of window (ordinal value of
    ///  TWindowState enumeration). Designed state is pased in. Set to state
    ///  read from persistent storage if available, otherwise left unchanged.
    ///  Any minimized state is ignored and normal state used in its place.
    ///  </param>
    procedure ReadWdwState(var Left, Top, Width, Height, State: Integer);
      override;

    ///  <summary>Writes window size, position and state to persistent settings
    ///  along with information about some window widgets stored in the
    ///  associated properties.</summary>
    ///  <param name="Left">Integer [in] Location of left hand side of window.
    ///  </param>
    ///  <param name="Top">Integer [in] Location of top of window.</param>
    ///  <param name="Width">Integer [in] Width of window.</param>
    ///  <param name="Height">Integer [in] Height of window.</param>
    ///  <param name="State">Integer [in] State of window (ordinal value of
    ///  TWindowState enumeration).</param>
    procedure SaveWdwState(const Left, Top, Width, Height, State: Integer);
      override;

  public
    const
      ///  <summary>Minimum width of left panel.</summary>
      MinLeftPanelWidth  = 160;
      ///  <summary>Minimum width of right panel.</summary>
      MinRightPanelWidth = 300;

  public
    ///  <summary>Creates and sets up component and default property values.
    ///  </summary>
    ///  <param name="AOwner">TComponent [in] Owner of this component. AOwner
    ///  must not be nil and must have a non-empty Name property.</param>
    ///  <remarks>AOwner should be the form whose size, position and state are
    ///  to be recorded.</remarks>
    constructor Create(AOwner: TComponent); override;

    ///  <summary>Position of main window's vertical splitter.</summary>
    property SplitterPos: Integer
      read fSplitterPos write fSplitterPos default cDefLeftPanelWidth;

    ///  <summary>Index of selected tab in overview pane.</summary>
    property OverviewTab: Integer
      read fOverviewTab write fOverviewTab;
  end;

type
  ///  <summary>Class that can save and record the position of a dialogue box
  ///  window.</summary>
  TDlgWindowSettings = class(TPJCustomWdwState)
  strict private
    var
      ///  <summary>Value of FixedSize property.</summary>
      fFixedSize: Boolean;

  strict protected
    ///  <summary>Reads window size and, optionally, position from persistent
    ///  settings.</summary>
    ///  <remarks>If persistent information is not present the window defaults
    ///  to its designed size and is located relative to its owner window
    ///  according to its aligner.</remarks>
    ///  <param name="Left">Integer [in/out] Location of left hand side of
    ///  window. Input value is as set by window's aligner. Set to value read
    ///  from persistent storage if available, otherwise unchanged.</param>
    ///  <param name="Top">Integer [in/out] Location of top of window. Input
    ///  value is as set by window's aligner. Set to value read from persistent
    ///  storage if available, otherwise unchanged.</param>
    ///  <param name="Width">Integer [in/out] Width of window. Designed width is
    ///  passed in. Set to width read from persistent storage if available AND
    ///  FixedSize property is False, otherwise left unchanged.</param>
    ///  <param name="Height">Integer [in/out] Height of window. Designed height
    ///  is passed in. Set to height read from persistent storage if available
    ///  AND FixedSize property is False, otherwise left unchanged.</param>
    ///  <param name="State">Integer [in/out] State of window (ordinal value of
    ///  TWindowState enumeration). Input value is ignored. Always set to
    ///  wsNormal.</param>
    procedure ReadWdwState(var Left, Top, Width, Height, State: Integer);
      override;

    ///  <summary>Writes window size and, optionally, position to persistent
    ///  settings.</summary>
    ///  <param name="Left">Integer [in] Location of left hand side of window.
    ///  </param>
    ///  <param name="Top">Integer [in] Location of top of window.</param>
    ///  <param name="Width">Integer [in] Width of window. Ignored if FixedSize
    ///  is True.</param>
    ///  <param name="Height">Integer [in] Height of window. Ignored if
    ///  FxiedSize is True.</param>
    ///  <param name="State">Integer [in] State of window (ordinal value of
    ///  TWindowState enumeration). Ignored.</param>
    procedure SaveWdwState(const Left, Top, Width, Height, State: Integer);
      override;

  public
    ///  <summary>Creates and sets up component and default property values.
    ///  </summary>
    ///  <param name="AOwner">TComponent [in] Owner of this component. AOwner
    ///  must not be nil and must have a non-empty Name property.</param>
    ///  <remarks>AOwner should be the form whose size and, optionally, position
    ///  are to be recorded.</remarks>
    constructor Create(AOwner: TComponent); override;

    ///  <summary>Determines if dialogue box is to be treated as fixed (True) or
    ///  variable (False) size.</summary>
    ///  <remarks>When True width and height information are not written to
    ///  persistent storage, or are ignored if present.</remarks>
    property FixedSize: Boolean read fFixedSize write fFixedSize default True;
  end;


implementation


uses
  // Delphi
  SysUtils, Forms, Windows,
  // Project
  USettings, UStructs;


{ TMainWindowSettings }

constructor TMainWindowSettings.Create(AOwner: TComponent);
begin
  Assert(Assigned(AOwner), ClassName + '.Create: AOwner is nil');
  inherited;
  AutoSaveRestore := False;           // need to call Save and Restore manually
  Options := [woFitWorkArea];         // keep main window inside work area
  fSplitterPos := cDefLeftPanelWidth; // default splitter position
end;

procedure TMainWindowSettings.ReadWdwState(var Left, Top, Width, Height,
  State: Integer);
var
  Section: ISettingsSection;  // object used to access persistent storage
  WorkArea: TRectEx;          // screen's workarea
begin
  // Set default values: centre form in workarea using designed width & height
  WorkArea := Screen.WorkAreaRect;
  Left := (WorkArea.Width - Width) div 2;
  Top := (WorkArea.Height - Height) div 2;
  // Read values from storage
  // ** Don't use Owner.Name for sub-section: breaks sub-classed code that needs
  //    main form settings.
  Section := Settings.ReadSection(ssWindowState, 'MainForm');
  Left := Section.GetInteger('Left', Left);
  Top := Section.GetInteger('Top', Top);
  Width := Section.GetInteger('Width', Width);
  Height := Section.GetInteger('Height', Height);
  State := Section.GetInteger('State', State);
  if TWindowState(State) = wsMinimized then
    State := Ord(wsNormal);   // we don't allow minimized: use normal
  fSplitterPos := Section.GetInteger('SplitterPos', fSplitterPos);
  fOverviewTab := Section.GetInteger('OverviewTab', fOverviewTab);
end;

procedure TMainWindowSettings.SaveWdwState(const Left, Top, Width, Height,
  State: Integer);
var
  Section: ISettingsSection;  // object used to access persistent storage
begin
  // ** Don't use Owner.Name for sub-section: breaks sub-classed code that needs
  //    main form settings.
  Section := Settings.ReadSection(ssWindowState, 'MainForm');
  Section.SetInteger('Left', Left);
  Section.SetInteger('Top', Top);
  Section.SetInteger('Width', Width);
  Section.SetInteger('Height', Height);
  Section.SetInteger('State', State);
  Section.SetInteger('SplitterPos', fSplitterPos);
  Section.SetInteger('OverviewTab', fOverviewTab);
  Section.Save;
end;

{ TDlgWindowSettings }

constructor TDlgWindowSettings.Create(AOwner: TComponent);
begin
  Assert(Assigned(AOwner), ClassName + '.Create: AOwner is nil');
  Assert(AOwner.Name <> '', ClassName + '.Create: AOwner.Name is empty string');
  inherited;
  AutoSaveRestore := False;           // need to call Save and Restore manually
  Options := [woFitWorkArea];         // keep main window inside work area
  fFixedSize := True;
end;

procedure TDlgWindowSettings.ReadWdwState(var Left, Top, Width, Height,
  State: Integer);
var
  Section: ISettingsSection;  // object used to access persistent storage
begin
  // Assume that dialogue box has been aligned in default position when this
  // method called.
  Section := Settings.ReadSection(ssWindowState, Owner.Name);
  Left := Section.GetInteger('Left', Left);
  Top := Section.GetInteger('Top', Top);
  if not fFixedSize then
  begin
    Width := Section.GetInteger('Width', Width);
    Height := Section.GetInteger('Height', Height);
  end;
  State := Ord(wsNormal); // we don't allow minimized or maximized for dialogues
end;

procedure TDlgWindowSettings.SaveWdwState(const Left, Top, Width, Height,
  State: Integer);
var
  Section: ISettingsSection;  // object used to access persistent storage
begin
  Section := Settings.ReadSection(ssWindowState, Owner.Name);
  Section.SetInteger('Left', Left);
  Section.SetInteger('Top', Top);
  // Never save state and only save size if dialogue not fixed size
  if not fFixedSize then
  begin
    Section.SetInteger('Width', Width);
    Section.SetInteger('Height', Height);
  end;
  Section.Save;
end;

end.


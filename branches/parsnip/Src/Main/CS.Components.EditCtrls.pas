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
 * Custom edit control components used in CodeSnip project.
 *
 * WARNING: Includes hacks where an edit control is redeclared with the same
 * name in order to modify or extend its functionality. These hacks are fully
 * explained in comments. They are used to that controls from the Delphi IDE
 * VCL palette can continue to be used, avoiding the need to create the modified
 * controls dynamically.
}


unit CS.Components.EditCtrls;


interface


uses
  // Delphi
  StdCtrls,
  Messages,
  Graphics,
  // 3rd party
  SynEdit;


type
  ///  <summary>Override for TMemo controls to add a TextHint property that
  ///  displays "cue text" in the memo control body when the control does not
  ///  have focus and no text has been entered.</summary>
  ///  <remarks>
  ///  <para>WARNING. This is a HACK that forces any TMemo in the same unit
  ///  where this unit is referenced to be treated as if it was this extended
  ///  version of the control.</para>
  ///  <para>For this hack to work this unit must be referenced in the uses
  ///  clause of the unit using the TMemo BEFORE the TMemo is first declared:
  ///  for forms this is always the interface section.</para>
  ///  <para>The hint is always displayed in the same font face as used for the
  ///  control but in grey italics.</para>
  ///  </remarks>
  TMemo = class(StdCtrls.TMemo)
  strict private
    var
      ///  <summary>Value of TextHint property.</summary>
      fTextHint: string;
  protected
    ///  <summary>Paints any required cue text in grey italics using the
    ///  control's font.</summary>
    procedure WMPaint(var Message: TWMPaint); message WM_PAINT;
  published
    ///  <summary>Specifies the "cue text" to be displayed when the control is
    ///  empty and does not have focus.</summary>
    property TextHint: string read fTextHint write fTextHint;
  end;

  ///  <summary>Override for TEdit controls that changes how "cue text" (per the
  ///  TextHint property) is displayed from the default: it is now displayed in
  ///  grey italics.</summary>
  ///  <remarks>
  ///  <para>WARNING. This is a HACK that forces any TMemo in the same unit
  ///  where this unit is referenced to be treated as if it was this extended
  ///  version of the control.</para>
  ///  <para>For this hack to work this unit must be referenced in the uses
  ///  clause of the unit using the TMemo BEFORE the TMemo is first declared:
  ///  for forms this is always the interface section.</para>
  ///  </remarks>
  TEdit = class(StdCtrls.TEdit)
  protected
    ///  <summary>Paints any required cue text in grey italics using the
    ///  control's font.</summary>
    procedure WMPaint(var Message: TWMPaint); message WM_PAINT;
    ///  <summary>Inhibits default processing of cue text.</summary>
    procedure DoSetTextHint(const Value: string); override;
  end;

  ///  <summary>Sub-class of TSynEdit that adds TextHint property that displays
  ///  "cue text" in the control body when the control does not have focus and
  ///  no text has been entered.</summary>
  ///  <remarks>SynEdit controls in CodeSnip are created on the fly at run time.
  ///  Code that creates the controls should construct instances of TSynEditEx
  ///  instead of TSynEdit to get this additional functionality.</remarks>
  TSynEditEx = class(SynEdit.TSynEdit)
  strict private
    var
      ///  <summary>Value of TextHint property.</summary>
      fTextHint: string;
      ///  <summary>Flag that indicates whether the control currently has focus.
      ///  </summary>
      ///  <remarks>Used instead of the control's Focused property to determine
      ///  when cue text can be displayed. This is because Focused does not
      ///  always seem to have the expected value when the control gets focus.
      ///  </remarks>
      fHasFocus: Boolean;
  protected
    ///  <summary>Records that the control has lost focus and forces a repaint
    ///  cue text needs to be displayed.</summary>
    procedure WMKillFocus(var Msg: TWMKillFocus); message WM_KILLFOCUS;
    ///  <summary>Records that the control has focus and forces a repaint if
    ///  if cue text was displayed and must be cleared.</summary>
    procedure WMSetFocus(var Msg: TWMSetFocus); message WM_SETFOCUS;
    ///  <summary>Paints any required cue text in grey italics using the
    ///  control's font.</summary>
    procedure Paint; override;
  public
    ///  <summary>Specifies the "cue text" to be displayed when the control is
    ///  empty and does not have focus.</summary>
    property TextHint: string read fTextHint write fTextHint;
  end;


implementation


uses
  // Delphi
  Controls,
  Types,
  // Project
  UFontHelper,
  UStrUtils;


///  <summary>Renders the given "cue" text on the canvas of the given control
///  using the control's font but in grey italics.</summary>
procedure RenderCueText(ACtrl: TWinControl; const ATopLeft: TPoint;
  const AFont: TFont; const ACueText: string);
var
  CtrlCanvas: TControlCanvas;
begin
  CtrlCanvas := TControlCanvas.Create;
  try
    CtrlCanvas.Control := ACtrl;
    CtrlCanvas.Font := AFont;   // requires TControl class helper
    CtrlCanvas.Font.Color := clGray;
    CtrlCanvas.Font.Style := [fsItalic];
    CtrlCanvas.TextOut(ATopLeft.X, ATopLeft.Y, ACueText);
  finally
    CtrlCanvas.Free;
  end;
end;

{ TMemo }

procedure TMemo.WMPaint(var Message: TWMPaint);
begin
  inherited;
  if (Text = '') and not Focused then
    RenderCueText(Self, Point(1, 1), Font, TextHint);
end;

{ TEdit }

procedure TEdit.DoSetTextHint(const Value: string);
begin
  // Do nothing: we want to inhibit default behaviour
end;

procedure TEdit.WMPaint(var Message: TWMPaint);
begin
  inherited;
  if (Text = '') and not Focused then
    RenderCueText(Self, Point(1, 1), Font, TextHint);
end;

{ TSynEditEx }

procedure TSynEditEx.Paint;
var
  CueFont: TFont;
begin
  inherited;
  if not fHasFocus and StrIsBlank(Text) then
  begin
    CueFont := TFont.Create;
    try
      TFontHelper.SetDefaultFont(CueFont);
      RenderCueText(Self, Point(fGutterWidth + 2, 1), CueFont, TextHint);
    finally
      CueFont.Free;
    end;
  end;
end;

procedure TSynEditEx.WMKillFocus(var Msg: TWMKillFocus);
begin
  inherited;
  fHasFocus := False;
  if StrIsBlank(Text) then
    Paint;
end;

procedure TSynEditEx.WMSetFocus(var Msg: TWMSetFocus);
begin
  fHasFocus := True;
  if StrIsBlank(Text) then
    Paint;
  inherited
end;

end.


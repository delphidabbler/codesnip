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
  Classes, StdCtrls, Messages, Graphics;


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


implementation


uses
  // Delphi
  Controls,
  // Project
  UClassHelpers;


///  <summary>Renders the given "cue" text on the canvas of the given control
///  using the control's font but in grey italics.</summary>
procedure RenderCueText(Ctrl: TWinControl; const ACueText: string);
var
  CtrlCanvas: TControlCanvas;
begin
  CtrlCanvas := TControlCanvas.Create;
  try
    CtrlCanvas.Control := Ctrl;
    CtrlCanvas.Font := Ctrl.GetFont;   // requires TControl class helper
    CtrlCanvas.Font.Color := clGray;
    CtrlCanvas.Font.Style := [fsItalic];
    CtrlCanvas.TextOut(1, 1, ACueText);
  finally
    CtrlCanvas.Free;
  end;
end;

{ TMemo }

procedure TMemo.WMPaint(var Message: TWMPaint);
begin
  inherited;
  if (Text = '') and not Focused then
    RenderCueText(Self, TextHint);
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
    RenderCueText(Self, TextHint);
end;

end.

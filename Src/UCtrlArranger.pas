{
 * UCtrlArranger.pas
 *
 * Implements a static class that provides methods to assist in aligning a the
 * controls on a form or frame etc.
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
 * The Original Code is UCtrlArranger.pas
 *
 * The Initial Developer of the Original Code is Peter Johnson
 * (http://www.delphidabbler.com/).
 *
 * Portions created by the Initial Developer are Copyright (C) 2009-2011 Peter
 * Johnson. All Rights Reserved.
 *
 * Contributors:
 *   NONE
 *
 * ***** END LICENSE BLOCK *****
}


unit UCtrlArranger;


interface


uses
  // Delphi
  Controls, StdCtrls,
  // Projects
  UBaseObjects;


type

  {
  TCtrlArranger:
    Static class used to arrange and size controls in forms and frames etc.
  }
  TCtrlArranger = class(TNoConstructObject)
  public
    class function SetLabelHeight(const Lbl: TLabel): Integer;
      {Sets height of a label to accommodate the text it contains in its font.
        @param Lbl [in] Label whose height is to be set.
        @return New label height.
      }
    class procedure SetLabelHeights(const Container: TControl;
      const IgnoreAutoSize: Boolean = False);
      {Sets heights of labels owned by a container control such as a form to
      accommodate the text they contain in the appropriate font.
        @param Container [in] Control that owns the labels.
        @param IgnoreAutoSize [in] Flagthat determines if labels with AutoSize
          property set to true are sized. When False only labels with AutoSize
          true are sized, otherwise all labels are sized.
      }
    class function BottomOf(const Ctrl: TControl; const BM: Integer = 0):
      Integer; overload;
      {Gets Y coordinate of bottom of a control, leaving space for an optional
      margin under the control, relative to its parent controls in pixels.
        @param Ctl [in] Control to check.
        @param BM [in] Bottom margin to leave under control. Optional.
        @return Required position.
      }
    class function BottomOf(const Ctrls: array of TControl;
      const BM: Integer = 0): Integer; overload;
      {Gets Y co-ordinate of bottom of the lowermost of a set of controls,
      leaving space for an optional margin under the controls.
        @param Ctrls [in] Controls to be examined.
        @param BM [in] Bottom margin to leave under all controls. Optional.
        @return Required position. This is maximum value of bottom of all
          controls.
      }
    class function RightOf(const Ctrl: TControl; const RM: Integer = 0):
      Integer; overload;
      {Returns horizontal position of right hand edge of given control, plus an
        optional margin.
        @param Ctrl [in] Control for which location of right hand side required.
        @return Required position.
      }
    ///  <summary>Returns horizontal position of right most right hand edge of
    ///  given controls, plus an optional margin.</summary>
    ///  <remarks>Control array must not be empty.</remarks>
    class function RightOf(const Ctrls: array of TControl;
      const RM: Integer = 0): Integer; overload;
    class procedure MoveToLeftOf(const RefCtrl, Ctrl: TControl;
      const Margin: Integer = 0);
      {Moves a control to the left of a reference control optionally separated
      by a margin.
        @param RefCtrl [in] Control relative to which control is moved.
        @param Ctrl [in] Control being moved.
        @param Margin [in] Optional margin, in pixels, required between Ctrl and
          RefCtrl.
      }
    class procedure MoveToRightOf(const RefCtrl, Ctrl: TControl;
      const Margin: Integer = 0); overload;
      {Moves a control to the right of a reference control optionally separated
      by a margin.
        @param RefCtrl [in] Control relative to which control is moved.
        @param Ctrl [in] Control being moved.
        @param Margin [in] Optional margin, in pixels, required between Ctrl and
          RefCtrl.
      }
    class procedure MoveToRightOf(const RefCtrls: array of TControl;
      const Ctrl: TControl; const Margin: Integer = 0); overload;
    class procedure MoveBelow(const RefCtrl, Ctrl: TControl;
      const Margin: Integer = 0); overload;
    class procedure MoveBelow(const RefCtrls: array of TControl;
      const Ctrl: TControl; const Margin: Integer = 0); overload;
    class function AlignVCentres(const ATop: Integer;
      const Ctrls: array of TControl): Integer;
      {Vertically centres a list of controls.
        @param ATop [in] Top of tallest control to be aligned.
        @param Ctrls [in] Array of controls to be aligned.
        @return Height occupied by controls (= height of tallest control).
      }
    ///  <summary>Aligns tops of all given controls with top of first control
    ///  and returns that top position.</summary>
    ///  <remarks>Array of controls must not be empty.</remarks>
    class function AlignTops(const Ctrls: array of TControl): Integer; overload;
    ///  <summary>Aligns tops of all given controls at given position.</summary>
    class procedure AlignTops(const Ctrls: array of TControl;
      const ATop: Integer); overload;
    ///  <summary>Aligns bottoms of all given controls with bottom of first
    ///  control and returns that bottom position.</summary>
    ///  <remarks>Array of controls must not be empty.</remarks>
    class function AlignBottoms(const Ctrls: array of TControl): Integer;
      overload;
    ///  <summary>Aligns bottoms of all given controls at given position.
    ///  </summary>
    class procedure AlignBottoms(const Ctrls: array of TControl;
      const ABottom: Integer); overload;
    ///  <summary>Aligns left hand sides of all given controls with left of
    ///  first control and returns the that value.</summary>
    ///  <remarks>Array of controls must not be empty.</remarks>
    class function AlignLefts(const Ctrls: array of TControl): Integer;
      overload;
    ///  <summary>Aligns right hand sidea of all given controls at given
    ///  position.</summary>
    class procedure AlignRights(const Ctrls: array of TControl;
      const ARight: Integer); overload;
    ///  <summary>Aligns right hand sides of all given controls with right of
    ///  first control and returns the that value.</summary>
    ///  <remarks>Array of controls must not be empty.</remarks>
    class function AlignRights(const Ctrls: array of TControl): Integer;
      overload;
    ///  <summary>Aligns left hand sidea of all given controls at given
    ///  position.</summary>
    class procedure AlignLefts(const Ctrls: array of TControl;
      const ALeft: Integer); overload;
    class function MaxContainerHeight(const Containers: array of TWinControl):
      Integer;
      {Checks the maximum height of controls parented by a set of controls. Can
      be used to determine the height of a control that has to be able to
      display all the controls from any of the containers.
        @param Containers [in] Controls that parent the controls being measured.
        @return Required height.
      }
    class function TotalControlHeight(const Container: TWinControl): Integer;
      {Gets the height that a container needs to be to accommodate all its
      contained controls.
        @param Container [in] Container to be checked.
        @return Required height.
      }
    class function TotalControlWidth(const Container: TWinControl): Integer;
      {Gets the width that a container needs to be to accommodate all its
      contained controls.
        @param Container [in] Container to be checked.
        @return Required width.
      }
  end;


implementation


uses
  // Delphi
  Math,
  // Project
  UGraphicUtils;


{ TCtrlArranger }

class function TCtrlArranger.AlignBottoms(const Ctrls: array of TControl):
  Integer;
begin
  Assert(Length(Ctrls) > 0, ClassName + '.AlignBottoms: control array empty');
  Result := Ctrls[0].Top + Ctrls[0].Height;
  AlignBottoms(Ctrls, Result);
end;

class procedure TCtrlArranger.AlignBottoms(const Ctrls: array of TControl;
  const ABottom: Integer);
var
  Ctrl: TControl;
begin
  for Ctrl in Ctrls do
    Ctrl.Top := ABottom - Ctrl.Height;
end;

class function TCtrlArranger.AlignLefts(const Ctrls: array of TControl):
  Integer;
begin
  Assert(Length(Ctrls) > 0, ClassName + '.AlignLefts: control array empty');
  Result := Ctrls[0].Left;
  AlignLefts(Ctrls, Result);
end;

class procedure TCtrlArranger.AlignLefts(const Ctrls: array of TControl;
  const ALeft: Integer);
var
  Ctrl: TControl;
begin
  for Ctrl in Ctrls do
    Ctrl.Left := ALeft;
end;

class function TCtrlArranger.AlignRights(const Ctrls: array of TControl):
  Integer;
begin
  Assert(Length(Ctrls) > 0, ClassName + '.AlignRights: control array empty');
  Result := Ctrls[0].Left + Ctrls[0].Width;
  AlignRights(Ctrls, Result);
end;

class procedure TCtrlArranger.AlignRights(const Ctrls: array of TControl;
  const ARight: Integer);
var
  Ctrl: TControl;
begin
  for Ctrl in Ctrls do
    Ctrl.Left := ARight - Ctrl.Width;
end;

class function TCtrlArranger.AlignTops(const Ctrls: array of TControl): Integer;
begin
  Assert(Length(Ctrls) > 0, ClassName + '.AlignTops: control array empty');
  Result := Ctrls[0].Top;
  AlignTops(Ctrls, Result);
end;

class procedure TCtrlArranger.AlignTops(const Ctrls: array of TControl;
  const ATop: Integer);
var
  Ctrl: TControl;
begin
  for Ctrl in Ctrls do
    Ctrl.Top := ATop;
end;

class function TCtrlArranger.AlignVCentres(const ATop: Integer;
  const Ctrls: array of TControl): Integer;
  {Vertically centres a list of controls.
    @param ATop [in] Top of tallest control to be aligned.
    @param Ctrls [in] Array of controls to be aligned.
    @return Height occupied by controls (= height of tallest control).
  }
var
  Ctrl: TControl; // references each control in Ctrls array
begin
  Result := 0;
  for Ctrl in Ctrls do
    Result := Max(Result, Ctrl.Height);
  for Ctrl in Ctrls do
    Ctrl.Top := ATop + (Result - Ctrl.Height) div 2;
end;

class function TCtrlArranger.BottomOf(const Ctrl: TControl;
  const BM: Integer): Integer;
  {Gets Y coordinate of bottom of a control, leaving space for an optional
  margin unser the control, relative to its parent controls in pixels.
    @param Ctl [in] Control to check.
    @param BM [in] Bottom margin to leave under control. Optional.
    @return Required position.
  }
begin
  Result := Ctrl.Top + Ctrl.Height + BM;
end;

class function TCtrlArranger.BottomOf(const Ctrls: array of TControl;
  const BM: Integer): Integer;
  {Gets Y co-ordinate of bottom of the lowermost of a set of controls, leaving
  space for an optional margin under the controls.
    @param Ctrls [in] Controls to be examined.
    @param BM [in] Bottom margin to leave under all controls. Optional.
    @return Required position. This is maximum value of bottom of all controls.
  }
var
  Ctrl: TControl; // references each control in Ctrls array
begin
  Result := 0;
  for Ctrl in Ctrls do
    Result := Max(Result, BottomOf(Ctrl));
  Inc(Result, BM);
end;

class function TCtrlArranger.MaxContainerHeight(
  const Containers: array of TWinControl): Integer;
  {Checks the maximum height of controls parented by a set of controls. Can be
  used to determine the height of a control that has to be able to display all
  the controls from any of the containers.
    @param Containers [in] Controls that parent the controls being measured.
    @return Required height.
  }
var
  Container: TWinControl;   // references each container in Containers array
begin
  Result := 0;
  for Container in Containers do
    Result := Max(Result, TotalControlHeight(Container));
end;

class procedure TCtrlArranger.MoveBelow(const RefCtrl, Ctrl: TControl;
  const Margin: Integer);
begin
  Ctrl.Top := BottomOf(RefCtrl, Margin);
end;

class procedure TCtrlArranger.MoveBelow(const RefCtrls: array of TControl;
  const Ctrl: TControl; const Margin: Integer);
begin
  Ctrl.Top := BottomOf(RefCtrls, Margin);
end;

class procedure TCtrlArranger.MoveToLeftOf(const RefCtrl, Ctrl: TControl;
  const Margin: Integer);
  {Moves a control to the left of a reference control optionally separated by a
  margin.
    @param RefCtrl [in] Control relative to which control is moved.
    @param Ctrl [in] Control being moved.
    @param Margin [in] Optional margin, in pixels, required between Ctrl and
      RefCtrl.
  }
begin
  Ctrl.Left := RefCtrl.Left - Margin - Ctrl.Width;
end;

class procedure TCtrlArranger.MoveToRightOf(const RefCtrl, Ctrl: TControl;
  const Margin: Integer);
  {Moves a control to the right of a reference control optionally separated by a
  margin.
    @param RefCtrl [in] Control relative to which control is moved.
    @param Ctrl [in] Control being moved.
    @param Margin [in] Optional margin, in pixels, required between Ctrl and
      RefCtrl.
  }
begin
  Ctrl.Left := RightOf(RefCtrl) + Margin;
end;

class procedure TCtrlArranger.MoveToRightOf(const RefCtrls: array of TControl;
  const Ctrl: TControl; const Margin: Integer);
begin
//  Ctrl.Left :=
end;

class function TCtrlArranger.RightOf(const Ctrls: array of TControl;
  const RM: Integer = 0): Integer;
var
  Ctrl: TControl;
begin
  Assert(Length(Ctrls) > 0, ClassName + '.RightOf: Ctrls array is empty.');
  Result := -MaxInt;
  for Ctrl in Ctrls do
    Result := Max(Result, RightOf(Ctrl));
  Inc(Result, RM);
end;

class function TCtrlArranger.RightOf(const Ctrl: TControl;
  const RM: Integer = 0): Integer;
begin
  Result := Ctrl.Left + Ctrl.Width + RM;
end;

class function TCtrlArranger.SetLabelHeight(const Lbl: TLabel): Integer;
  {Sets height of a label to accommodate the text it contains in its font.
    @param Lbl [in] Label whose height is to be set.
    @return New label height.
  }
begin
  Lbl.Height := StringExtent(Lbl.Caption, Lbl.Font, Lbl.Width).cy;
  Result := Lbl.Height;
end;

class procedure TCtrlArranger.SetLabelHeights(const Container: TControl;
  const IgnoreAutoSize: Boolean);
  {Sets heights of labels owned by a container control such as a form to
  accommodate the text they contain in the appropriate font.
    @param Container [in] Control that owns the labels.
    @param IgnoreAutoSize [in] Flagthat determines if labels with AutoSize
      property set to true are sized. When False only labels with AutoSize true
      are sized, otherwise all labels are sized.
  }
var
  Idx: Integer; // loops through all components owned by container control
  Lbl: TLabel;  // references each label component
begin
  for Idx := 0 to Pred(Container.ComponentCount) do
  begin
    if Container.Components[Idx] is TLabel then
    begin
      Lbl := Container.Components[Idx] as TLabel;
      if IgnoreAutoSize or not Lbl.AutoSize then
        SetLabelHeight(Lbl);
    end;
  end;
end;

class function TCtrlArranger.TotalControlHeight(const Container: TWinControl):
  Integer;
  {Gets the height that a container needs to be to accommodate all its contained
  controls.
    @param Container [in] Container to be checked.
    @return Required height.
  }
var
  CtrlIdx: Integer;         // loops through all controls in Container
  Ctrls: array of TControl; // array of controls contained in each container
begin
  SetLength(Ctrls, Container.ControlCount);
  for CtrlIdx := 0 to Pred(Container.ControlCount) do
    Ctrls[CtrlIdx + Low(Ctrls)] := Container.Controls[CtrlIdx];
  Result := BottomOf(Ctrls);
end;

class function TCtrlArranger.TotalControlWidth(
  const Container: TWinControl): Integer;
  {Gets the width that a container needs to be to accommodate all its contained
  controls.
    @param Container [in] Container to be checked.
    @return Required width.
  }
var
  CtrlIdx: Integer; // loops through all controls in Container
  Ctrl: TControl;   // references each control in Container
begin
  Result := 0;
  for CtrlIdx := 0 to Pred(Container.ControlCount) do
  begin
    Ctrl := Container.Controls[CtrlIdx];
    Result := Max(Result, Ctrl.Left + Ctrl.Width);
  end;
end;

end.


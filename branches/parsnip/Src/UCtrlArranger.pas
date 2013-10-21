{
 * This Source Code Form is subject to the terms of the Mozilla Public License,
 * v. 2.0. If a copy of the MPL was not distributed with this file, You can
 * obtain one at http://mozilla.org/MPL/2.0/
 *
 * Copyright (C) 2009-2013, Peter Johnson (www.delphidabbler.com).
 *
 * $Rev$
 * $Date$
 *
 * Implements a static class that provides methods to assist in aligning a the
 * controls in forms, frames etc.
}


unit UCtrlArranger;


interface


uses
  // Delphi
  Classes, Controls, StdCtrls,
  // Projects
  UBaseObjects;


type
  ///  <summary>Static class used to arrange and size controls in forms, frames
  ///  etc.</summary>
  TCtrlArranger = class(TNoConstructObject)
  public
    ///  <summary>Sets height of given label to accommodate its caption text in
    ///  its current font. The new height is returned.</summary>
    class function SetLabelHeight(const Lbl: TLabel): Integer;

    ///  <summary>Sets heights of all labels owned by given component to
    ///  accomodate their captions in their current fonts, unless label has its
    ///  AutoSize property set to True.</summary>
    class procedure SetLabelHeights(const AOwner: TComponent);

    ///  <summary>Returns Y co-ordinate of bottom of given control, allowing
    ///  space for optional margin.</summary>
    class function BottomOf(const Ctrl: TControl; const Margin: Integer = 0):
      Integer; overload;

    ///  <summary>Returns Y co-ordinate of bottom of lower-most control in given
    ///  array, allowing space for optional margin.</summary>
    class function BottomOf(const Ctrls: array of TControl;
      const Margin: Integer = 0): Integer; overload;

    ///  <summary>Returns Y co-ordinate of top of top-most control in given
    ///  array, allowing space for optional margin.</summary>
    class function TopOf(const Ctrls: array of TControl;
      const Margin: Integer = 0): Integer;

    ///  <summary>Returns X co-ordinate of right hand side of given control,
    ///  allowing space for optional margin.</summary>
    class function RightOf(const Ctrl: TControl; const Margin: Integer = 0):
      Integer; overload;

    ///  <summary>Returns X co-ordinate of right-most right hand side of
    ///  controls in given array, allowing space for optional margin.</summary>
    class function RightOf(const Ctrls: array of TControl;
      const Margin: Integer = 0): Integer; overload;

    ///  <summary>Returns X co-ordinate of left-most left hand side of
    ///  controls in given array, allowing space for optional margin.</summary>
    class function LeftOf(const Ctrls: array of TControl;
      const Margin: Integer = 0): Integer;

    ///  <summary>Locates control Ctrl to the left of reference control RefCtrl,
    ///  optionally separated by given margin.</summary>
    class procedure MoveToLeftOf(const RefCtrl, Ctrl: TControl;
      const Margin: Integer = 0); overload;

    ///  <summary>Locates control Ctrl to left of left most edge of controls
    ///  in RefCtrl, optionally separated by given margin.</summary>
    class procedure MoveToLeftOf(const RefCtrls: array of TControl;
      const Ctrl: TControl; const Margin: Integer = 0); overload;

    ///  <summary>Locates control Ctrl to right of reference control RefCtrl,
    ///  optionally separated by given margin.</summary>
    class procedure MoveToRightOf(const RefCtrl, Ctrl: TControl;
      const Margin: Integer = 0); overload;

    ///  <summary>Locates control Ctrl to right of right most edge of controls
    ///  in RefCtrl, optionally separated by given margin.</summary>
    class procedure MoveToRightOf(const RefCtrls: array of TControl;
      const Ctrl: TControl; const Margin: Integer = 0); overload;

    ///  <summary>Locates control Ctrl below reference control RefCtrl,
    ///  optionally separated by given margin.</summary>
    class procedure MoveBelow(const RefCtrl, Ctrl: TControl;
      const Margin: Integer = 0); overload;

    ///  <summary>Locates control Ctrl below bottom-most of reference controls
    ///  in RefCtrls, optionally separated by given margin.</summary>
    class procedure MoveBelow(const RefCtrls: array of TControl;
      const Ctrl: TControl; const Margin: Integer = 0); overload;

    ///  <summary>Vertically centres all controls in Ctrls array aligned so that
    ///  top of top-most control is located at ATop. Returns total height
    ///  occupied by controls.</summary>
    class function AlignVCentres(const ATop: Integer;
      const Ctrls: array of TControl): Integer;

    ///  <summary>Vertically centres all controls in the Ctrls array within the
    ///  range defined by the top-most and bottom-most co-ordinates of the
    ///  controls in the RefCtrls array.</summary>
    class procedure AlignVCentresTo(const RefCtrls: array of TControl;
      const Ctrls: array of TControl);

    ///  <summary>Horizontally centres all controls in the Ctrls array within
    ///  the range defined by the left-most and right-most co-ordinates of the
    ///  controls in the RefCtrls array.</summary>
    class procedure AlignHCentresTo(const RefCtrls: array of TControl;
      const Ctrls: array of TControl);

    ///  <summary>Aligns tops of all controls in Ctrls array with top of first
    ///  control in array and returns that top position.</summary>
    ///  <remarks>Array of controls must not be empty.</remarks>
    class function AlignTops(const Ctrls: array of TControl): Integer; overload;

    ///  <summary>Aligns tops of all controls in Ctrls array at position given
    ///  by ATop.</summary>
    class procedure AlignTops(const Ctrls: array of TControl;
      const ATop: Integer); overload;

    ///  <summary>Aligns bottoms of all controls in Ctrls array at position
    ///  given by ABottom.</summary>
    class procedure AlignBottoms(const Ctrls: array of TControl;
      const ABottom: Integer); overload;

    ///  <summary>Aligns bottoms of all controls in Ctrls array with bottom of
    ///  first control in array and returns that bottom position.</summary>
    ///  <remarks>Array of controls must not be empty.</remarks>
    class function AlignBottoms(const Ctrls: array of TControl): Integer;
      overload;

    ///  <summary>Aligns left hand sides of all controls in Ctrls array at
    ///  position given by ALeft.</summary>
    class procedure AlignLefts(const Ctrls: array of TControl;
      const ALeft: Integer); overload;

    ///  <summary>Aligns left hand sides of all controls in Ctrls array with
    ///  left of first control in array and returns that left position.
    ///  </summary>
    ///  <remarks>Array of controls must not be empty.</remarks>
    class function AlignLefts(const Ctrls: array of TControl): Integer;
      overload;

    ///  <summary>Aligns right hand sides of all controls in Ctrls array at
    ///  position given by ARight.</summary>
    class procedure AlignRights(const Ctrls: array of TControl;
      const ARight: Integer); overload;

    ///  <summary>Aligns right hand sides of all controls in Ctrls array with
    ///  right of first control in array and returns that right position.
    ///  </summary>
    ///  <remarks>Array of controls must not be empty.</remarks>
    class function AlignRights(const Ctrls: array of TControl): Integer;
      overload;

    ///  <summary>Returns the total height needed to display all the controls
    ///  parented by given container.</summary>
    class function TotalControlHeight(const Container: TWinControl): Integer;

    ///  <summary>Returns the total width needed to display all the controls
    ///  parented by given container.</summary>
    class function TotalControlWidth(const Container: TWinControl): Integer;

    ///  <summary>Returns maximum height of controls parented by given set of
    ///  parent controls.</summary>
    ///  <remarks>Designed for use in determining the height of a control that
    ///  has to accomodate all the controls from any of the containers, for
    ///  example a page control where the containers are the pages.</remarks>
    class function MaxContainerHeight(const Containers: array of TWinControl):
      Integer;
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

class procedure TCtrlArranger.AlignHCentresTo(const RefCtrls,
  Ctrls: array of TControl);
var
  Left, Right: Integer;
  Ctrl: TControl;
begin
  Left := LeftOf(RefCtrls);
  Right := RightOf(RefCtrls);
  for Ctrl in Ctrls do
    Ctrl.Left := (Right + Left - Ctrl.Width) div 2;
end;

class procedure TCtrlArranger.AlignBottoms(const Ctrls: array of TControl;
  const ABottom: Integer);
var
  Ctrl: TControl; // each control in Ctrls
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
  Ctrl: TControl; // each control in Ctrls
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
  Ctrl: TControl; // each control in Ctrls
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
  Ctrl: TControl; // each control in Ctrls
begin
  for Ctrl in Ctrls do
    Ctrl.Top := ATop;
end;

class function TCtrlArranger.AlignVCentres(const ATop: Integer;
  const Ctrls: array of TControl): Integer;
var
  Ctrl: TControl; // each control in Ctrls
begin
  Result := 0;
  for Ctrl in Ctrls do
    Result := Max(Result, Ctrl.Height);
  for Ctrl in Ctrls do
    Ctrl.Top := ATop + (Result - Ctrl.Height) div 2;
end;

class procedure TCtrlArranger.AlignVCentresTo(const RefCtrls,
  Ctrls: array of TControl);
var
  Top, Bottom: Integer;
  Ctrl: TControl;
begin
  Top := TopOf(RefCtrls);
  Bottom := BottomOf(RefCtrls);
  for Ctrl in Ctrls do
    Ctrl.Top := (Top + Bottom - Ctrl.Height) div 2;
end;

class function TCtrlArranger.BottomOf(const Ctrl: TControl;
  const Margin: Integer): Integer;
begin
  Result := Ctrl.Top + Ctrl.Height + Margin;
end;

class function TCtrlArranger.BottomOf(const Ctrls: array of TControl;
  const Margin: Integer): Integer;
var
  Ctrl: TControl; // each control in Ctrls
begin
  Result := 0;
  for Ctrl in Ctrls do
    Result := Max(Result, BottomOf(Ctrl));
  Inc(Result, Margin);
end;

class function TCtrlArranger.LeftOf(const Ctrls: array of TControl;
  const Margin: Integer): Integer;
var
  Ctrl: TControl;
begin
  Result := MaxInt;
  for Ctrl in Ctrls do
    Result := Min(Result, Ctrl.Left);
  Dec(Result, Margin);
end;

class function TCtrlArranger.MaxContainerHeight(
  const Containers: array of TWinControl): Integer;
var
  Container: TWinControl;   // each container in Containers
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
begin
  Ctrl.Left := RefCtrl.Left - Margin - Ctrl.Width;
end;

class procedure TCtrlArranger.MoveToLeftOf(const RefCtrls: array of TControl;
  const Ctrl: TControl; const Margin: Integer);
begin
  Ctrl.Left := TCtrlArranger.LeftOf(RefCtrls, Margin) - Ctrl.Width;
end;

class procedure TCtrlArranger.MoveToRightOf(const RefCtrls: array of TControl;
  const Ctrl: TControl; const Margin: Integer);
begin
  Ctrl.Left := RightOf(RefCtrls, Margin);
end;

class procedure TCtrlArranger.MoveToRightOf(const RefCtrl, Ctrl: TControl;
  const Margin: Integer);
begin
  Ctrl.Left := RightOf(RefCtrl, Margin);
end;

class function TCtrlArranger.RightOf(const Ctrls: array of TControl;
  const Margin: Integer = 0): Integer;
var
  Ctrl: TControl; // each control in Ctrls
begin
  Result := 0;
  for Ctrl in Ctrls do
    Result := Max(Result, RightOf(Ctrl));
  Inc(Result, Margin);
end;

class function TCtrlArranger.RightOf(const Ctrl: TControl;
  const Margin: Integer = 0): Integer;
begin
  Result := Ctrl.Left + Ctrl.Width + Margin;
end;

class function TCtrlArranger.SetLabelHeight(const Lbl: TLabel): Integer;
begin
  if Lbl.WordWrap then
    Lbl.Height := StringExtent(Lbl.Caption, Lbl.Font, Lbl.Width).cy
  else
    Lbl.Height := StringExtent(Lbl.Caption, Lbl.Font).cy;
  Result := Lbl.Height;
end;

class procedure TCtrlArranger.SetLabelHeights(const AOwner: TComponent);
var
  Idx: Integer; // loops through all components owned AOwner
  Lbl: TLabel;  // references each label component
begin
  for Idx := 0 to Pred(AOwner.ComponentCount) do
  begin
    if AOwner.Components[Idx] is TLabel then
    begin
      Lbl := AOwner.Components[Idx] as TLabel;
      if not Lbl.AutoSize then
        SetLabelHeight(Lbl);
    end;
  end;
end;

class function TCtrlArranger.TopOf(const Ctrls: array of TControl;
  const Margin: Integer): Integer;
var
  Ctrl: TControl;
begin
  Result := MaxInt;
  for Ctrl in Ctrls do
    Result := Min(Result, Ctrl.Top);
  Dec(Result, Margin);
end;

class function TCtrlArranger.TotalControlHeight(const Container: TWinControl):
  Integer;
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


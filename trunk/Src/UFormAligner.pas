{
 * This Source Code Form is subject to the terms of the Mozilla Public License,
 * v. 2.0. If a copy of the MPL was not distributed with this file, You can
 * obtain one at http://mozilla.org/MPL/2.0/
 *
 * Copyright (C) 2007-2013, Peter Johnson (www.delphidabbler.com).
 *
 * $Rev$
 * $Date$
 *
 * Defines a classes that implement IAligner and can align a form over an owning
 * control.
}


unit UFormAligner;


interface


uses
  // Delphi
  Forms,
  // Projects
  IntfAligner;


type

  {
  TFormAligner:
    Class that can align a form over an owning control. Adopts different
    alignment depending on type of owning control.
  }
  TFormAligner = class(TInterfacedObject,
    IFormAligner
  )
  public
    { IAligner method }
    procedure AlignForm(const AForm: TCustomForm);
      {Aligns a form relative to Owner, or, if owner is nil, to either active
      form or application's main form.
        @param AForm [in] Form to be aligned.
      }
  end;

type
  ///  <summary>Class that aligns a form centrally over its owning TWinControl.
  ///  </summary>
  ///  <remarks>Unlike TFormAligner, the form is always centralised, regardless
  ///  of the type of the owning control.</remarks>
  TSimpleFormAligner = class(TInterfacedObject,
    IFormAligner
  )
  public
    ///  <summary>Aligns the given form over its owner, or, if the owner is nil,
    ///  over either the active form or the application's main form.</summary>
    ///  <remarks>
    ///  <para>The given form's owner must either be nil or be a TWinControl
    ///  descendant.</para>
    ///  <para>Method of IAligner.</para>
    ///  </remarks>
    procedure AlignForm(const AForm: TCustomForm);
  end;



implementation


uses
  // Delphi
  Controls, Types,
  // Project
  UDlgHelper, UStructs;


{ TFormAligner }

procedure TFormAligner.AlignForm(const AForm: TCustomForm);
  {Aligns a form relative to Owner, or, if owner is nil, to either active form
  or application's main form.
    @param AForm [in] Form to be aligned.
  }
begin
  TDlgAligner.AlignToOwner(AForm);
end;

{ TSimpleFormAligner }

procedure TSimpleFormAligner.AlignForm(const AForm: TCustomForm);
var
  FormBounds: TRectEx;      // bounds of AForm
  Owner: TWinControl;       // owner win control
  WorkArea: TRectEx;        // desktop work area
  RelOffset: TPoint;        // relative offset of AForm to its owner
  OwnerAbsTopLeft: TPoint;  // top left of owner control in screen co-ords
begin
  Assert(Assigned(AForm), ClassName + '.AlignForm: AForm is nil');
  Assert(not Assigned(AForm.Owner) or (AForm.Owner is TWinControl),
    ClassName + '.AlignForm: AForm.Owner must be nil or a TWinControl');

  // Get owner control
  if Assigned(AForm.Owner) then
    Owner := AForm.Owner as TWinControl
  else if Assigned(Screen.ActiveCustomForm) then
    Owner := Screen.ActiveCustomForm
  else
    Owner := Application.MainForm;
  Assert(Assigned(Owner),
    ClassName + '.AlignForm: Application has no main form.');

  // Calculate aligned form position
  FormBounds := TRectEx.CreateBounds(0, 0, AForm.Width, AForm.Height);
  RelOffset := Point(
    (Owner.Width - FormBounds.Width) div 2,
    (Owner.Height - FormBounds.Height) div 2
  );
  OwnerAbsTopLeft := Owner.ClientToScreen(Point(0, 0));
  FormBounds.OffsetBy(
    OwnerAbsTopLeft.X + RelOffset.X, OwnerAbsTopLeft.Y + RelOffset.Y
  );

  // Adjust for position if necesary to keep on screen
  WorkArea := Screen.MonitorFromRect(FormBounds).WorkareaRect;
  if FormBounds.Right > WorkArea.Right then
    FormBounds.OffsetBy(WorkArea.Right - FormBounds.Right, 0);
  if FormBounds.Left < WorkArea.Left then
    FormBounds.OffsetBy(WorkArea.Left - FormBounds.Left, 0);
  if FormBounds.Bottom > WorkArea.Bottom then
    FormBounds.OffsetBy(0, WorkArea.Bottom - FormBounds.Bottom);
  if FormBounds.Top < WorkArea.Top then
    FormBounds.OffsetBy(0, WorkArea.Top - FormBounds.Top);

  // Place the form
  AForm.BoundsRect := FormBounds;
end;

end.


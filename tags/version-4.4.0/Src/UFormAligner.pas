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
 * Defines a class that implements IAligner and can align a form over an owning
 * control. Adopts different alignment depending on type of owning control.
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


implementation


uses
  // Project
  UDlgHelper;


{ TFormAligner }

procedure TFormAligner.AlignForm(const AForm: TCustomForm);
  {Aligns a form relative to Owner, or, if owner is nil, to either active form
  or application's main form.
    @param AForm [in] Form to be aligned.
  }
begin
  TDlgAligner.AlignToOwner(AForm);
end;

end.


{
 * UFormAligner.pas
 *
 * Defines a class that implements IAligner and can align a form over an owning
 * control. Adopts different alignment depending on type of owning control.
 *
 * v1.0 of 07 Feb 2007  - Original version.
 * v2.0 of 26 Sep 2007  - Substantially changed to pass alignment of form off to
 *                        TDlgAligner.
 *                      - Changed to use renamed IFormAligner interface.
 * v2.1 of 11 Jun 2008  - Changed to use renamed UDlgHelper unit.
 *                      - Now use new TDlgAligner.AlignToOwner method to perform
 *                        form alignment.
 *
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
 * The Original Code is UFormAligner.pas
 *
 * The Initial Developer of the Original Code is Peter Johnson
 * (http://www.delphidabbler.com/).
 *
 * Portions created by the Initial Developer are Copyright (C) 2007-2008 Peter
 * Johnson. All Rights Reserved.
 *
 * Contributor(s): None
 *
 * ***** END LICENSE BLOCK *****
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
  protected
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


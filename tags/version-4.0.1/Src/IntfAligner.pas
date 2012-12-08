{
 * This Source Code Form is subject to the terms of the Mozilla Public License,
 * v. 2.0. If a copy of the MPL was not distributed with this file, You can
 * obtain one at http://mozilla.org/MPL/2.0/
 *
 * Copyright (C) 2007-2012, Peter Johnson (www.delphidabbler.com).
 *
 * $Rev$
 * $Date$
 *
 * Declares an interface to objects that can align a form on screen.
}


unit IntfAligner;


interface


uses
  // Delphi
  Windows, Forms;


type

  {
  IFormAligner:
    Interface to object that can align a form on screen.
  }
  IFormAligner = interface(IInterface)
    ['{1B2EDAFC-487B-4F8A-9921-84483B78DBBF}']
    procedure AlignForm(const AForm: TCustomForm);
      {Aligns a form on screen in implementation determined manner.
        @param AForm [in] Form to be aligned.
      }
  end;


implementation

end.


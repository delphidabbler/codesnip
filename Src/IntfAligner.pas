{
 * This Source Code Form is subject to the terms of the Mozilla Public License,
 * v. 2.0. If a copy of the MPL was not distributed with this file, You can
 * obtain one at https://mozilla.org/MPL/2.0/
 *
 * Copyright (C) 2007-2021, Peter Johnson (gravatar.com/delphidabbler).
 *
 * Declares an interface supported by objects that can align a form on screen.
}


unit IntfAligner;


interface


uses
  // Delphi
  Windows, Forms;


type
  ///  <summary>Interface supported by objects that can align a form on screen.
  ///  </summary>
  IFormAligner = interface(IInterface)
    ['{1B2EDAFC-487B-4F8A-9921-84483B78DBBF}']
    ///  <summary>Aligns a form on screen in an implementation determined
    ///  manner.</summary>
    ///  <param name="AForm">TCustomForm [in] Form to be aligned.</param>
    procedure AlignForm(const AForm: TCustomForm);
  end;


implementation

end.


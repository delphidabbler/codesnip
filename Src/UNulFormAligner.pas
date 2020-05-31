{
 * This Source Code Form is subject to the terms of the Mozilla Public License,
 * v. 2.0. If a copy of the MPL was not distributed with this file, You can
 * obtain one at http://mozilla.org/MPL/2.0/
 *
 * Copyright (C) 2007-2020, Peter Johnson (gravatar.com/delphidabbler).
 *
 * Defines a do nothing class that implements IAligner for use with forms that
 * do not support alignment.
}


unit UNulFormAligner;


interface


uses
  // Delphi
  Forms,
  // Project
  IntfAligner;


type

  {
  TNulAligner:
    Do nothing IAligner implementation. Used for forms that do not support
    alignment.
  }
  TNulAligner = class(TInterfacedObject,
    IFormAligner
  )
  public
    {Method of IFormAligner}
    procedure AlignForm(const AForm: TCustomForm);
      {Does nothing.
        @param AForm [in] Not used.
      }
  end;


implementation


{ TNulAligner }

procedure TNulAligner.AlignForm(const AForm: TCustomForm);
  {Does nothing.
    @param AForm [in] Not used.
  }
begin
  // Do nothing
end;

end.


{
 * This Source Code Form is subject to the terms of the Mozilla Public License,
 * v. 2.0. If a copy of the MPL was not distributed with this file, You can
 * obtain one at https://mozilla.org/MPL/2.0/
 *
 * Copyright (C) 2007-2020, Peter Johnson (gravatar.com/delphidabbler).
 *
 * Implements a class that maintains and provides access to a hidden rich edit
 * control.
}


unit UHiddenRichEdit;


interface


uses
  // Delphi
  ComCtrls,
  // Project
  UHiddenWindow;


type

  {
  THiddenRichEdit:
    Class that maintains and provides access to a hidden rich edit control.
  }
  THiddenRichEdit = class(TObject)
  strict private
    fRichEdit: TRichEdit;
      {Reference to hidden rich edit control}
    fHiddenWindow: THiddenWindow;
      {Reference to hidden window used as parent for rich edit control}
  strict protected
    property RichEdit: TRichEdit read fRichEdit;
      {Reference to hidden rich edit control}
  public
    constructor Create;
      {Class constructor. Sets up object and creates hidden rich edit control.
      }
    destructor Destroy; override;
      {Class destructor. Tears down object.
      }
  end;


implementation


uses
  // Delphi
  SysUtils;


{ THiddenRichEdit }

constructor THiddenRichEdit.Create;
  {Class constructor. Sets up object and creates hidden rich edit control.
  }
begin
  inherited;
  fHiddenWindow := THiddenWindow.Create;
  fRichEdit := TRichEdit.CreateParented(fHiddenWindow.Handle);
end;

destructor THiddenRichEdit.Destroy;
  {Class destructor. Tears down object.
  }
begin
  FreeAndNil(fRichEdit);
  FreeAndNil(fHiddenWindow);
  inherited;
end;

end.


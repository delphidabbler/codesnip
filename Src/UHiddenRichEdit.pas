{
 * UHiddenRichEdit.pas
 *
 * Implements a class that maintains and provides access to a hidden rich edit
 * control.
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
 * The Original Code is UHiddenRichEdit.pas
 *
 * The Initial Developer of the Original Code is Peter Johnson
 * (http://www.delphidabbler.com/).
 *
 * Portions created by the Initial Developer are Copyright (C) 2007-2009 Peter
 * Johnson. All Rights Reserved.
 *
 * Contributor(s)
 *   NONE
 *
 * ***** END LICENSE BLOCK *****
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
  private
    fRichEdit: TRichEdit;
      {Reference to hidden rich edit control}
    fHiddenWindow: THiddenWindow;
      {Reference to hidden window used as parent for rich edit control}
  protected
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


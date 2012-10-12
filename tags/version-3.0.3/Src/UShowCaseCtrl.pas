{
 * UShowCaseCtrl.pas
 *
 * Implements a simple transparent control designed to be placed over other
 * controls to prevent user interaction with the "show cased" controls.
 *
 * v1.0 of 22 Apr 2008  - Original version.
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
 * The Original Code is UShowCaseCtrl.pas
 *
 * The Initial Developer of the Original Code is Peter Johnson
 * (http://www.delphidabbler.com/).
 *
 * Portions created by the Initial Developer are Copyright (C) 2008 Peter
 * Johnson. All Rights Reserved.
 *
 * Contributor(s): None
 *
 * ***** END LICENSE BLOCK *****
}


unit UShowCaseCtrl;


interface


uses
  // Delphi
  Controls;


type

  {
  TShowCaseCtrl:
    Simple transparent control designed to be placed over other controls to
    prevent user interaction with the "show cased" controls.
  }
  TShowCaseCtrl = class(TCustomControl)
  protected
    procedure CreateParams(var Params: TCreateParams); override;
      {Modifies window creation parameters. Ensures control is transparent.
        @param Params [in/out] In: default parameters. Out: transparent added to
          required styles.
      }
  end;


implementation


uses
  // Delphi
  Windows;


{ TShowCaseCtrl }

procedure TShowCaseCtrl.CreateParams(var Params: TCreateParams);
  {Modifies window creation parameters. Ensures control is transparent.
    @param Params [in/out] In: default parameters. Out: transparent added to
      required styles.
  }
begin
  inherited;
  Params.ExStyle := Params.ExStyle or WS_EX_TRANSPARENT;
end;

end.
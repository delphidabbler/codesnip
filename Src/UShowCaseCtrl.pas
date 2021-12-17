{
 * This Source Code Form is subject to the terms of the Mozilla Public License,
 * v. 2.0. If a copy of the MPL was not distributed with this file, You can
 * obtain one at https://mozilla.org/MPL/2.0/
 *
 * Copyright (C) 2008-2020, Peter Johnson (gravatar.com/delphidabbler).
 *
 * Implements a simple transparent control designed to be placed over other
 * controls to prevent user interaction with the "show cased" controls.
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
  strict protected
    procedure CreateParams(var Params: TCreateParams); override;
      {Modifies window creation parameters. Ensures control is transparent.
        @param Params [in/out] In: default parameters. Out: transparent added to
          required styles.
      }
  public
    procedure Invalidate; override;
      {Overrides window invalidation code to do nothing.
      NOTE: we don't want any repainting since this will overwrite the showcased
      item.
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

procedure TShowCaseCtrl.Invalidate;
  {Overrides window invalidation code to do nothing.
  NOTE: we don't want any repainting since this will overwrite the showcased
  item}
begin
  // Do nothing.
  // Do not call inherited.
end;

end.


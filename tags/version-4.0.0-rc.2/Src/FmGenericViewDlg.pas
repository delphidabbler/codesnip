{
 * This Source Code Form is subject to the terms of the Mozilla Public License,
 * v. 2.0. If a copy of the MPL was not distributed with this file, You can
 * obtain one at http://mozilla.org/MPL/2.0/
 *
 * Copyright (C) 2005-2012, Peter Johnson (www.delphidabbler.com).
 *
 * $Rev$
 * $Date$
 *
 * Implements a base class for dialogue boxes that have a single Close button.
}


unit FmGenericViewDlg;


interface


uses
  // Delphi
  StdCtrls, Controls, ExtCtrls, Classes,
  // Project
  FmGenericDlg;


type

  {
  TGenericViewDlg:
    Generic OK dialog box used as a base class for dialog boxes that permit
    viewing rather then editing of data. Adds a Close button that closes the
    dialog box.
  }
  TGenericViewDlg = class(TGenericDlg)
    btnClose: TButton;
  protected
    procedure ArrangeForm; override;
      {Positions controls and sets form size according to body panel dimensions.
      }
    function ModalResultOnEsc: Integer; override;
      {Gets modal result returned from dialog when user presses ESC key.
        @return Required modal result.
      }
  end;


implementation


uses
  // Project
  UCtrlArranger;


{$R *.DFM}


{ TGenericViewDlg }

procedure TGenericViewDlg.ArrangeForm;
  {Positions controls and sets form size according to body panel dimensions.
  }
begin
  // Arrange inherited controls and size the form
  inherited;
  // Arrange Close button
  btnClose.Top := btnHelp.Top;
  if btnHelp.Visible then
    TCtrlArranger.MoveToLeftOf(btnHelp, btnClose, 4)
  else
    btnClose.Left := btnHelp.Left;
end;

function TGenericViewDlg.ModalResultOnEsc: Integer;
  {Gets modal result returned from dialog when user presses ESC key.
    @return Required modal result.
  }
begin
  Result := btnClose.ModalResult;
end;

end.


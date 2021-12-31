{
 * This Source Code Form is subject to the terms of the Mozilla Public License,
 * v. 2.0. If a copy of the MPL was not distributed with this file, You can
 * obtain one at https://mozilla.org/MPL/2.0/
 *
 * Copyright (C) 2005-2021, Peter Johnson (gravatar.com/delphidabbler).
 *
 * Implements a base class for modal dialogue boxes that have both OK and Cancel
 * buttons.
}


unit FmGenericOKDlg;


interface


uses
  // Delphi
  StdCtrls, Controls, ExtCtrls, Classes,
  // Project
  FmGenericModalDlg, FmGenericDlg;


type

  {
  TGenericOKDlg:
    Generic OK dialogue box used as a base class for modal dialogue boxes that
    permit editing of data. It adds OK and Cancel buttons to the form that close
    the dialogue box with the appropriate modal result.
  }
  TGenericOKDlg = class(TGenericModalDlg)
    btnCancel: TButton;
    btnOK: TButton;
  strict protected
    procedure ArrangeForm; override;
      {Positions OK and Cancel buttons.
      }
    function ModalResultOnEsc: Integer; override;
      {Gets modal result returned from dialog when user presses ESC key.
        @return Required modal result.
      }
  end;


implementation


uses
  // Delphi
  Windows,
  // Project
  UCtrlArranger;


{$R *.DFM}


{ TGenericOKDlg }

procedure TGenericOKDlg.ArrangeForm;
  {Positions OK and Cancel buttons.
  }
begin
  inherited;
  btnOK.Top := btnHelp.Top;
  btnCancel.Top := btnOK.Top;
  if btnHelp.Visible then
    TCtrlArranger.MoveToLeftOf(btnHelp, btnCancel, 4)
  else
    btnCancel.Left := btnHelp.Left;
  TCtrlArranger.MoveToLeftOf(btnCancel, btnOK, 4);
end;

function TGenericOKDlg.ModalResultOnEsc: Integer;
  {Gets modal result returned from dialog when user presses ESC key.
    @return Required modal result.
  }
begin
  Result := btnCancel.ModalResult;
end;

end.


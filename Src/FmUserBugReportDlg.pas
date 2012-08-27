{
 * This Source Code Form is subject to the terms of the Mozilla Public License,
 * v. 2.0. If a copy of the MPL was not distributed with this file, You can
 * obtain one at http://mozilla.org/MPL/2.0/
 *
 * Copyright (C) 2009-2012, Peter Johnson (www.delphidabbler.com).
 *
 * $Rev$
 * $Date$
 *
 * Implements a dialogue box that informs users how to report bugs.
}


unit FmUserBugReportDlg;


interface


uses
  // Delphi
  Classes, ActnList, ExtActns, StdCtrls, Controls, ExtCtrls,
  // Project
  FmBugReportBaseDlg;


type

  {
  TUserBugReportDlg:
    dialog box that informs users how to report bugs.
  }
  TUserBugReportDlg = class(TBugReportBaseDlg)
    lblInstruct1: TLabel;
    lblInstruct2: TLabel;
    lblInstruct3: TLabel;
  strict protected
    procedure ArrangeForm; override;
      {Arranges labels on form.
      }
  public
    class procedure Execute(AOwner: TComponent);
      {Creates and displays bug report dialog box.
        @param Owner [in] Component that owns dialog box. Dialog box is aligned
          over this component if it is a form. If Owner it is nil or not a form
          the dialog is aligned over the active form.
      }
  end;


implementation


uses
  // Project
  UCtrlArranger;


{$R *.dfm}

{ TUserBugReportDlg }

procedure TUserBugReportDlg.ArrangeForm;
  {Arranges labels on form.
  }
begin
  TCtrlArranger.SetLabelHeights(Self);
  lblInstruct2.Top := TCtrlArranger.BottomOf(lblInstruct1, 6);
  lblInstruct3.Top := TCtrlArranger.BottomOf(lblInstruct2, 6);
  lblBugTracker.Top := TCtrlArranger.BottomOf(lblInstruct3, 6);
  inherited;
end;

class procedure TUserBugReportDlg.Execute(AOwner: TComponent);
  {Creates and displays bug report dialog box.
    @param Owner [in] Component that owns dialog box. Dialog box is aligned over
      this component if it is a form. If Owner it is nil or not a form the
      dialog is aligned over the active form.
  }
begin
  with Create(AOwner) do
    try
      ShowModal;
    finally
      Free;
    end;
end;

end.

{
 * FmUserBugReportDlg.pas
 *
 * Implements a dialog box that informs users how to report bugs.
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
 * The Original Code is FmUserBugReportDlg.pas
 *
 * The Initial Developer of the Original Code is Peter Johnson
 * (http://www.delphidabbler.com/).
 *
 * Portions created by the Initial Developer are Copyright (C) 2009-2010 Peter
 * Johnson. All Rights Reserved.
 *
 * Contributor(s)
 *   NONE
 *
 * ***** END LICENSE BLOCK *****
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

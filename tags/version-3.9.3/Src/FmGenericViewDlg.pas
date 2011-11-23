{
 * FmGenericViewDlg.pas
 *
 * Implements a dialog box base class derived from TGenericDlg that adds a Close
 * button to the dialog.
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
 * The Original Code is FmGenericViewDlg.pas
 *
 * The Initial Developer of the Original Code is Peter Johnson
 * (http://www.delphidabbler.com/).
 *
 * Portions created by the Initial Developer are Copyright (C) 2005-2011 Peter
 * Johnson. All Rights Reserved.
 *
 * Contributor(s)
 *   NONE
 *
 * ***** END LICENSE BLOCK *****
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


{ ##
  @FILE                     FmGenericOKDlg.pas
  @COMMENTS                 Implements a dialog box base class derived from
                            TGenericDlg that adds an OK and Cancel button to the
                            dialog.
  @PROJECT_NAME             CodeSnip
  @PROJECT_DESC             Offline viewer for routines from the online
                            DelphiDabbler CodeSnip database.
  @DEPENDENCIES             None
  @HISTORY(
    @REVISION(
      @VERSION              0.1
      @DATE                 30/01/2005
      @COMMENTS             Original version.
    )
    @REVISION(
      @VERSION              1.0
      @DATE                 05/06/2006
      @COMMENTS             + Implemented new ModalResultOnEsc method to return
                              modal result of Cancel button when ESC key
                              pressed.
                            + Made minor changes to comments.
    )
    @REVISION(
      @VERSION              1.1
      @DATE                 08/02/2007
      @COMMENTS             Removed redundant FormKeyDown event handler that
                            duplicated code in inherited handler.
    )
  )
}


{
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
 * The Original Code is FmGenericOKDlg.pas
 *
 * The Initial Developer of the Original Code is Peter Johnson
 * (http://www.delphidabbler.com/).
 *
 * Portions created by the Initial Developer are Copyright (C) 2005-2007 Peter
 * Johnson. All Rights Reserved.
 *
 * Contributor(s): None
 *
 * ***** END LICENSE BLOCK *****
}


unit FmGenericOKDlg;


interface


uses
  // Delphi
  StdCtrls, Controls, ExtCtrls, Classes,
  // Project
  FmGenericDlg;


type

  {
  TGenericOKDlg:
    Generic OK dialog box used as a base class for dialog boxes that permit
    editing of data. It adds OK and Cancel buttons to the form that close the
    dialog box with the appropriate modal result.
  }
  TGenericOKDlg = class(TGenericDlg)
    btnCancel: TButton;
    btnOK: TButton;
  protected
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
  Windows;


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
    btnCancel.Left := btnHelp.Left - btnCancel.Width - 4
  else
    btnCancel.Left := btnHelp.Left;
  btnOK.Left := btnCancel.Left - btnOK.Width - 4;
end;

function TGenericOKDlg.ModalResultOnEsc: Integer;
  {Gets modal result returned from dialog when user presses ESC key.
    @return Required modal result.
  }
begin
  Result := btnCancel.ModalResult;
end;

end.


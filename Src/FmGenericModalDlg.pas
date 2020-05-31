{
 * This Source Code Form is subject to the terms of the Mozilla Public License,
 * v. 2.0. If a copy of the MPL was not distributed with this file, You can
 * obtain one at http://mozilla.org/MPL/2.0/
 *
 * Copyright (C) 2013-2020, Peter Johnson (gravatar.com/delphidabbler).
 *
 * Implements an abstract base class for all the program's modal dialogue boxes.
}


unit FmGenericModalDlg;


interface


uses
  // Project
  StdCtrls, Controls, ExtCtrls, Classes,
  // Delphi
  FmGenericDlg;


type
  TGenericModalDlg = class(TGenericDlg)
    procedure FormKeyDown(Sender: TObject; var Key: Word; Shift: TShiftState);
  strict protected
    function ModalResultOnEsc: Integer; virtual; abstract;
      {Gets modal result returned from dialog when user presses ESC key. Set
      to mrNone to disable special handling of ESC key.
        @return Required modal result.
      }
  end;


implementation


uses
  // Delphi
  Windows;


{$R *.dfm}

procedure TGenericModalDlg.FormKeyDown(Sender: TObject; var Key: Word;
  Shift: TShiftState);
  {Checks for escape key press, with no modifiers, and closes dialog with a
  descendant supplied modal result. If modal result is mrNone then dialog is
  not closed.
    @param Sender [in] Not used.
    @param Key [in/out] Key pressed.
    @param Shift [in] Modifier keys pressed.
  }
begin
  inherited;
  if (Key = VK_ESCAPE) and (Shift = []) then
  begin
    // Setting ModalResult to a value <> mrNone closes the form. ShowModal
    // returns the assigned value.
    ModalResult := ModalResultOnEsc;
  end
end;

end.

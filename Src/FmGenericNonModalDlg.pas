{
 * This Source Code Form is subject to the terms of the Mozilla Public License,
 * v. 2.0. If a copy of the MPL was not distributed with this file, You can
 * obtain one at https://mozilla.org/MPL/2.0/
 *
 * Copyright (C) 2013-2020, Peter Johnson (gravatar.com/delphidabbler).
 *
 * Implements a base class for all the program's non-modal dialogue boxes.
}


unit FmGenericNonModalDlg;


interface


uses
  // Delphi
  StdCtrls, Controls, ExtCtrls, Classes,
  // Project
  FmGenericDlg;


type
  ///  <summary>Base class for all non-modal dialogue boxes in the program.
  ///  </summary>
  ///  <remarks>By default non-modal dialogue boxes are fixed size tool windows.
  ///  </remarks>
  TGenericNonModalDlg = class(TGenericDlg)
    btnClose: TButton;
    ///  <summary>Traps key down events on form. Closes dialogue box if
    ///  unshifted ESC key detected.</summary>
    procedure FormKeyDown(Sender: TObject; var Key: Word; Shift: TShiftState);
    ///  <summary>Handles clicks on Close button by closing form.</summary>
    procedure btnCloseClick(Sender: TObject);
  strict protected
    ///  <summary>Positions controls and sizes dialogue box form.</summary>
    ///  <remarks>This override positions Close button in addition to inherited
    ///  behaviour.</remarks>
    procedure ArrangeForm; override;
  end;


implementation


uses
  // Delphi
  Windows,
  // Project
  UCtrlArranger;


{$R *.dfm}


{ TGenericNonModalDlg }

procedure TGenericNonModalDlg.ArrangeForm;
begin
  inherited;
  // Arrange Close to left of any Help button
  TCtrlArranger.AlignTops([btnHelp, btnClose]);
  if btnHelp.Visible then
    TCtrlArranger.MoveToLeftOf(btnHelp, btnClose, 4)
  else
    btnClose.Left := btnHelp.Left;
end;

procedure TGenericNonModalDlg.btnCloseClick(Sender: TObject);
begin
  Close;
end;

procedure TGenericNonModalDlg.FormKeyDown(Sender: TObject; var Key: Word;
  Shift: TShiftState);
begin
  inherited;
  if (Key = VK_ESCAPE) and (Shift = []) then
    Close;
end;

end.

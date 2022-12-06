{
 * This Source Code Form is subject to the terms of the Mozilla Public License,
 * v. 2.0. If a copy of the MPL was not distributed with this file, You can
 * obtain one at https://mozilla.org/MPL/2.0/
 *
 * Copyright (C) 2005-2021, Peter Johnson (gravatar.com/delphidabbler).
 *
 * Implements a base class for all the program's dialogue boxes.
}


unit FmGenericDlg;


interface


uses
  // Delphi
  StdCtrls, Controls, ExtCtrls, Classes,
  // Project
  IntfAligner, UI.Forms.Root;


type

  {
  TGenericDlg:
    Implements a base class for all the program's dialogue boxes, whether modal
    or mode-less. Displays and handles help button, sizes the dialogue form and
  }
  TGenericDlg = class(TRootForm)
    bvlBottom: TBevel;
    btnHelp: TButton;
    pnlBody: TPanel;
    procedure btnHelpClick(Sender: TObject);
    procedure FormCreate(Sender: TObject);
  strict protected
    procedure ArrangeControls; override;
    procedure InitialiseControls; override;
    function Aligner: IFormAligner; override;
  end;


implementation


uses
  // Delphi
  Windows,
  // Project
  UCtrlArranger, UDlgHelper, UFormAligner;


{$R *.DFM}


{ TGenericDlg }

function TGenericDlg.Aligner: IFormAligner;
begin
  Result := TFormAligner.Create;
end;

procedure TGenericDlg.ArrangeControls;
begin
  bvlBottom.SetBounds(8, TCtrlArranger.BottomOf(pnlBody, 6), pnlBody.Width, 2);
  if bvlBottom.Visible then
    btnHelp.Top := TCtrlArranger.BottomOf(bvlBottom, 6)
  else
    btnHelp.Top := bvlBottom.Top;
  ClientWidth := pnlBody.Width + 16;
  ClientHeight := TCtrlArranger.BottomOf(btnHelp, 6);
  btnHelp.Left := ClientWidth - 8 - btnHelp.Width;
end;

procedure TGenericDlg.btnHelpClick(Sender: TObject);
  {Displays default help for the dialog box using inherited DisplayHelp method.
    @param Sender [in] Not used.
  }
begin
  DisplayHelp;
end;

procedure TGenericDlg.FormCreate(Sender: TObject);
  {Handles form's OnCreate event. Sets handle of Owner control as parent of
  form's window. If Owner doesn't have handle then either active form or main
  form used as parent.
    @param Sender [in] Not used.
  }
begin
  inherited;
  TDlgHelper.SetDlgParentToOwner(Self);
end;

procedure TGenericDlg.InitialiseControls;
begin
  inherited;
  Self.DisableHelp := not btnHelp.Visible;
end;

end.


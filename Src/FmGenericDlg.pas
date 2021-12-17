{
 * This Source Code Form is subject to the terms of the Mozilla Public License,
 * v. 2.0. If a copy of the MPL was not distributed with this file, You can
 * obtain one at https://mozilla.org/MPL/2.0/
 *
 * Copyright (C) 2005-2020, Peter Johnson (gravatar.com/delphidabbler).
 *
 * Implements a base class for all the program's dialogue boxes.
}


unit FmGenericDlg;


interface


uses
  // Delphi
  StdCtrls, Controls, ExtCtrls, Classes,
  // Project
  IntfAligner, FmHelpAware;


type

  {
  TGenericDlg:
    Implements a base class for all the program's dialogue boxes, whether modal
    or mode-less. Displays and handles help button, sizes the dialogue form and
  }
  TGenericDlg = class(THelpAwareForm)
    bvlBottom: TBevel;
    btnHelp: TButton;
    pnlBody: TPanel;
    procedure btnHelpClick(Sender: TObject);
    procedure FormCreate(Sender: TObject);
  strict protected
    procedure ArrangeForm; virtual;
      {Positions controls and sets form size according to body panel dimensions.
      }
    procedure ConfigForm; virtual;
      {Method for overriding by subclasses to configure and possibly dynamically
      resize the body panel. This method is called before the form is arranged
      and size. Does nothing in this implementation.
      }
    procedure InitForm; override;
      {Initialises inherited DisableHelp property according to whether help
      button is visible or not.
      }
    procedure CustomiseForm; override; final;
      {Enables form to be customised and the arranged / sizes the form. Sub
      classes must override ConfigForm and ArrangeForm rather than this method.
      }
    function GetAligner: IFormAligner; override;
      {Creates and returns reference to an object that is used to align the form
      to the owner.
        @return Required aligner object instance.
      }
  end;


implementation


uses
  // Delphi
  Windows,
  // Project
  UCtrlArranger, UDlgHelper, UFormAligner;


{$R *.DFM}


{ TGenericDlg }

procedure TGenericDlg.ArrangeForm;
  {Positions controls and sets form size according to body panel dimensions.
  }
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

procedure TGenericDlg.ConfigForm;
  {Method for overriding by subclasses to configure and possibly dynamically
  resize the body panel. This method is called before the form is arranged and
  size. Does nothing in this implementation.
  }
begin
  // Do nothing
end;

procedure TGenericDlg.CustomiseForm;
  {Enables form to be customised and the arranged / sizes the form. Sub classes
  should override ConfigForm and ArrangeForm rather than this method.
  }
begin
  inherited;
  ConfigForm;
  ArrangeForm;
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

function TGenericDlg.GetAligner: IFormAligner;
  {Creates and returns reference to an object that is used to align the form to
  the owner.
    @return Required aligner object instance.
  }
begin
  Result := TFormAligner.Create;
end;

procedure TGenericDlg.InitForm;
  {Initialises inherited DisableHelp property according to whether help button
  is visible or not.
  }
begin
  inherited;
  Self.DisableHelp := not btnHelp.Visible;
end;

end.


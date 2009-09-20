{
 * FmGenericDlg.pas
 *
 * Implements an abstract base class for dialog boxes. Displays and handles help
 * button and sizes the dialog form and arranges controls. Also handles ESC key
 * presses and provides a help button.
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
 * The Original Code is FmGenericDlg.pas
 *
 * The Initial Developer of the Original Code is Peter Johnson
 * (http://www.delphidabbler.com/).
 *
 * Portions created by the Initial Developer are Copyright (C) 2005-2009 Peter
 * Johnson. All Rights Reserved.
 *
 * Contributor(s)
 *   NONE
 *
 * ***** END LICENSE BLOCK *****
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
    Generic abstract base class for dialog boxes. Displays and handles help
    button and sizes the dialog form and arranges controls. Also handles ESC key
    presses and provides a help button.
  }
  TGenericDlg = class(THelpAwareForm)
    bvlBottom: TBevel;
    btnHelp: TButton;
    pnlBody: TPanel;
    procedure btnHelpClick(Sender: TObject);
    procedure FormKeyDown(Sender: TObject; var Key: Word;
      Shift: TShiftState);
    procedure FormCreate(Sender: TObject);
  protected
    function ModalResultOnEsc: Integer; virtual; abstract;
      {Gets modal result returned from dialog when user presses ESC key. Set
      to mrNone to disable special handling of ESC key.
        @return Required modal result.
      }
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

procedure TGenericDlg.FormKeyDown(Sender: TObject; var Key: Word;
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


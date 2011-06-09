{
 * FmCompilersDlg.FrSwitches.pas
 *
 * Implements a frame used to edit switches used for a compiler being edited in
 * TCompilersDlg.
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
 * The Original Code is FmCompilersDlg.FrSwitches.pas
 *
 * The Initial Developer of the Original Code is Peter Johnson
 * (http://www.delphidabbler.com/).
 *
 * Portions created by the Initial Developer are Copyright (C) 2011 Peter
 * Johnson. All Rights Reserved.
 *
 * Contributor(s)
 *   NONE
 *
 * ***** END LICENSE BLOCK *****
}


unit FmCompilersDlg.FrSwitches;


interface


uses
  // Delphi
  StdCtrls, Controls, Classes,
  // Project
  FmCompilersDlg.FrBase;


type
  ///  <summary>
  ///  Frame used to edit switches used for a compiler being edited in
  ///  TCompilersDlg.
  ///  </summary>
  TCompilersDlgSwitchesFrame = class(TCompilersDlgBaseFrame)
    lblExplainSwitches: TLabel;
    lblSwitches: TLabel;
    lblSwitch: TLabel;
    btnDefSwitches: TButton;
    lbSwitches: TListBox;
    edSwitch: TEdit;
    btnAdd: TButton;
    btnReplace: TButton;
    btnDelete: TButton;
    ///  <summary>Handles clicks on switches list box. Copies selected switch
    ///  to edit control.</summary>
    procedure lbSwitchesClick(Sender: TObject);
    ///  <summary>Handles clicks on Add button. Adds switch in edit control to
    ///  list box.</summary>
    procedure btnAddClick(Sender: TObject);
    ///  <summary>Handles clicks on Defaults button. Restores switches in list
    ///  box to compiler defaults.</summary>
    procedure btnDefSwitchesClick(Sender: TObject);
    ///  <summary>Handles clicks on Delete button. Deletes currently selected
    ///  switch from list box.</summary>
    procedure btnDeleteClick(Sender: TObject);
    ///  <summary>Handles clicks on Replace button. Replaces selected switch in
    ///  list box with value entered in edit control.</summary>
    procedure btnReplaceClick(Sender: TObject);
    ///  <summary>Handles changes in edit control. Updates button state
    ///  depending on whether any text is in edit control.</summary>
    procedure edSwitchChange(Sender: TObject);
  strict private
    ///  <summary>Updates state of buttons that manipulate command line
    ///  switches.</summary>
    procedure UpdateSwitchButtons;
    ///  <summary>Stores list of command line switches in list box.</summary>
    ///  <param name="Switches">string [in] Comma separated list of switches.
    ///  </param>
    procedure PopulateSwitchList(const Switches: string);
  strict protected
    ///  <summary>Initialises frame to display details of current compiler.
    ///  </summary>
    procedure Initialise; override;
  public
    ///  <summary>Arranges controls in frame.</summary>
    procedure ArrangeControls; override;
    ///  <summary>Updates current compiler object with edited information.
    ///  </summary>
    procedure UpdateCompiler; override;
  end;


implementation


uses
  // Project
  UCtrlArranger, UStrUtils;

{$R *.dfm}


{ TCompilersDlgSwitchesFrame }

procedure TCompilersDlgSwitchesFrame.ArrangeControls;
begin
  TCtrlArranger.SetLabelHeights(Self);
  edSwitch.Top := TCtrlArranger.BottomOf(lblSwitch, 4);
  lblSwitches.Top := TCtrlArranger.BottomOf(edSwitch, 8);
  lblExplainSwitches.Top := TCtrlArranger.BottomOf(btnDelete, 8);
  lbSwitches.Top := lblExplainSwitches.Top;
  lblSwitches.Top := lbSwitches.Top - lblSwitches.Height - 4;
end;

procedure TCompilersDlgSwitchesFrame.btnAddClick(Sender: TObject);
begin
  lbSwitches.Items.Add(StrTrim(edSwitch.Text));
  edSwitch.Text := '';
  UpdateSwitchButtons;
end;

procedure TCompilersDlgSwitchesFrame.btnDefSwitchesClick(Sender: TObject);
begin
  PopulateSwitchList(Compiler.GetDefaultSwitches);
  UpdateSwitchButtons;
end;

procedure TCompilersDlgSwitchesFrame.btnDeleteClick(Sender: TObject);
begin
  Assert(lbSwitches.ItemIndex >= 0,
    ClassName + '.btnDeleteClick: lbSwitches.ItemIndex < 0');
  lbSwitches.Items.Delete(lbSwitches.ItemIndex);
  UpdateSwitchButtons;
end;

procedure TCompilersDlgSwitchesFrame.btnReplaceClick(Sender: TObject);
begin
  Assert(lbSwitches.ItemIndex >= 0,
    ClassName + '.btnReplaceClick: lbSwitches.ItemIndex < 0');
  lbSwitches.Items[lbSwitches.ItemIndex] := StrTrim(edSwitch.Text);
  edSwitch.Text := '';
  UpdateSwitchButtons;
end;

procedure TCompilersDlgSwitchesFrame.edSwitchChange(Sender: TObject);
begin
  UpdateSwitchButtons;
end;

procedure TCompilersDlgSwitchesFrame.Initialise;
begin
  PopulateSwitchList(Compiler.GetSwitches);
  UpdateSwitchButtons;
end;

procedure TCompilersDlgSwitchesFrame.lbSwitchesClick(Sender: TObject);
begin
  edSwitch.Text := lbSwitches.Items[lbSwitches.ItemIndex];
  UpdateSwitchButtons;
  edSwitch.SetFocus;
end;

procedure TCompilersDlgSwitchesFrame.PopulateSwitchList(const Switches: string);
begin
  StrExplode(Switches, ',', lbSwitches.Items, False);
end;

procedure TCompilersDlgSwitchesFrame.UpdateCompiler;
begin
  Compiler.SetSwitches(StrJoin(lbSwitches.Items, ',', False));
end;

procedure TCompilersDlgSwitchesFrame.UpdateSwitchButtons;
begin
  btnAdd.Enabled := (StrTrim(edSwitch.Text) <> '') and
    (lbSwitches.Items.IndexOf(StrTrim(edSwitch.Text)) = -1);
  btnReplace.Enabled := btnAdd.Enabled and
    (lbSwitches.ItemIndex >= 0);
  btnDelete.Enabled := lbSwitches.ItemIndex >= 0;
end;

end.

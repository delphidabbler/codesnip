{
 * This Source Code Form is subject to the terms of the Mozilla Public License,
 * v. 2.0. If a copy of the MPL was not distributed with this file, You can
 * obtain one at http://mozilla.org/MPL/2.0/
 *
 * Copyright (C) 2013, Peter Johnson (www.delphidabbler.com).
 *
 * $Rev$
 * $Date$
 *
 * Implements a dialogue box that enables the user to enter a new name for a
 * named highlighter.
}


unit FmNewHiliterNameDlg;


interface


uses
  // Delphi
  Controls, StdCtrls, ExtCtrls, Classes,
  // Project
  FmGenericOKDlg, UBaseObjects, UIStringList;


type
  TNewHiliterNameDlg = class(TGenericOKDlg, INoPublicConstruct)
    lblNames: TLabel;
    cbNames: TComboBox;
    lblHelp: TLabel;
    procedure cbNamesChange(Sender: TObject);
    procedure btnOKClick(Sender: TObject);
  strict private
    var
      fNewName: string;
      fNames: IStringList;
    procedure UpdateControls;
  strict protected
    procedure ArrangeForm; override;
    procedure InitForm; override;
  public
    class function Execute(Owner: TComponent; const Names: array of string;
      out NewName: string): Boolean;
  end;


implementation


uses
  // Delphi
  SysUtils, Math,
  // Project
  UCtrlArranger, UMessageBox, UStrUtils;

{$R *.dfm}


{ TNewHiliterNameDlg }

procedure TNewHiliterNameDlg.ArrangeForm;
begin
  lblNames.Top := 0;
  TCtrlArranger.MoveBelow(lblNames, cbNames, 6);
  TCtrlArranger.MoveBelow(cbNames, lblHelp, 4);
  TCtrlArranger.AlignLefts([lblNames, cbNames], 0);
  pnlBody.ClientWidth := Max(
    TCtrlArranger.TotalControlWidth(pnlBody) + 8,
    TCtrlArranger.RightOf(btnHelp) - btnOK.Left
  );
  cbNames.Width := pnlBody.ClientWidth;
  pnlBody.ClientHeight := TCtrlArranger.TotalControlHeight(pnlBody) + 24;
  inherited;
end;

procedure TNewHiliterNameDlg.btnOKClick(Sender: TObject);
resourcestring
  sQuestion = '"%s" already exists. Overwrite it?';
var
  ChosenName: string;
begin
  ChosenName := StrTrim(cbNames.Text);
  // check for errors
  ModalResult := mrNone;
  if ChosenName = '' then
    Exit;
  if fNames.Contains(ChosenName)
    and not TMessageBox.Confirm(Self, Format(sQuestion, [ChosenName])) then
    Exit;
  // all OK
  ModalResult := mrOK;
  fNewName := ChosenName;
end;

procedure TNewHiliterNameDlg.cbNamesChange(Sender: TObject);
begin
  UpdateControls;
end;

class function TNewHiliterNameDlg.Execute(Owner: TComponent;
  const Names: array of string; out NewName: string): Boolean;
begin
  with InternalCreate(Owner) do
    try
      fNames := TIStringList.Create(Names);
      fNames.CaseSensitive := False;
      Result := ShowModal = mrOK;
      if Result then
        NewName := fNewName;
    finally
      Free;
    end;
end;

procedure TNewHiliterNameDlg.InitForm;
begin
  inherited;
  fNames.CopyTo(cbNames.Items, True);
  cbNames.Text := '';
  UpdateControls;
end;

procedure TNewHiliterNameDlg.UpdateControls;
begin
  btnOK.Enabled := cbNames.Text <> '';
end;

end.

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
 * Implements a dialogue box that enables the user to enter a syntax highlighter
 * name.
}


unit FmNewHiliterNameDlg;


interface


uses
  // Delphi
  Controls, StdCtrls, ExtCtrls, Classes,
  // Project
  FmGenericOKDlg, UBaseObjects, UIStringList;


type
  ///  <summary>Class that implements a dialogue box that enables the user to
  ///  enter a syntax highlighter name.</summary>
  TNewHiliterNameDlg = class(TGenericOKDlg, INoPublicConstruct)
    cbNames: TComboBox;
    lblHelp: TLabel;
    lblNames: TLabel;
    ///  <summary>Checks validity of chosen name and, if valid, records it and
    ///  closes dialogue box.</summary>
    procedure btnOKClick(Sender: TObject);
    ///  <summary>Updates dialogue box controls when selection in names combo
    ///  box changes.</summary>
    procedure cbNamesChange(Sender: TObject);
  strict private
    var
      ///  <summary>Records chosen highlighter name.</summary>
      fNewName: string;
      ///  <summary>List of existing highlighter names.</summary>
      fNames: IStringList;
    ///  <summary>Updates control state based on current entries in dialogue
    ///  box.</summary>
    procedure UpdateControls;
  strict protected
    ///  <summary>Arranges controls on form and size dialogue box window to fit.
    ///  </summary>
    procedure ArrangeForm; override;
    ///  <summary>Initialises form's controls.</summary>
    procedure InitForm; override;
  public
    ///  <summary>Displays dialogue box and passes any entered highlighter name
    ///  back to caller.</summary>
    ///  <param name="Owner">TComponent [in] Owning control. Dialogue box is
    ///  aligned over it.</param>
    ///  <param name="Names">array of string [in] Existing highlighter names.
    ///  </param>
    ///  <param name="NewName">string [out] Set to name entered by user if OK
    ///  pressed or undefined if dialogue box is cancelled.</param>
    ///  <returns>Boolean. True if OK button pressed or False if dialogue box
    ///  cancelled.</returns>
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


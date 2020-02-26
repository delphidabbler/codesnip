{
 * This Source Code Form is subject to the terms of the Mozilla Public License,
 * v. 2.0. If a copy of the MPL was not distributed with this file, You can
 * obtain one at http://mozilla.org/MPL/2.0/
 *
 * Copyright (C) 2013-2020, Peter Johnson (gravatar.com/delphidabbler).
 *
 * Implements a frame used to edit namespaces used for a compiler being edited
 * in TCompilersDlg.
}


unit FmCompilersDlg.FrNamespaces;


interface


uses
  // Delphi
  StdCtrls, Controls, Classes,
  // Project
  FmCompilersDlg.FrBase;


type
  ///  <summary>Frame used to edit namespaces used for a compiler being edited
  ///  in TCompilersDlg.</summary>
  TCompilersDlgNamespacesFrame = class(TCompilersDlgBaseFrame)
    lblExplainNamespaces: TLabel;
    lblNamespaces: TLabel;
    lblNamespace: TLabel;
    btnDefSwitches: TButton;
    lbNamespaces: TListBox;
    edNamespace: TEdit;
    btnAdd: TButton;
    btnReplace: TButton;
    btnDelete: TButton;
    ///  <summary>Handles clicks on namespaces list box. Copies selected
    ///  namespace to edit control.</summary>
    procedure lbNamespacesClick(Sender: TObject);
    ///  <summary>Handles clicks on Add button. Adds namespace in edit control
    ///  to list box.</summary>
    procedure btnAddClick(Sender: TObject);
    ///  <summary>Handles clicks on Defaults button. Restores namespaces in list
    ///  box to compiler defaults.</summary>
    procedure btnDefSwitchesClick(Sender: TObject);
    ///  <summary>Handles clicks on Delete button. Deletes currently selected
    ///  namespace from list box.</summary>
    procedure btnDeleteClick(Sender: TObject);
    ///  <summary>Handles clicks on Replace button. Replaces selected namespace
    ///  in list box with value entered in edit control.</summary>
    procedure btnReplaceClick(Sender: TObject);
    ///  <summary>Handles changes in edit control. Updates button state
    ///  depending on whether any text is in edit control.</summary>
    procedure edNamespaceChange(Sender: TObject);
  strict private
    ///  <summary>Returns namespace from edit control after validation.
    ///  </summary>
    ///  <exception>Raises EDataEntry if entered namespace is not a valid Pascal
    ///  identifier.</exception>
    function GetValidNamespace: string;
    ///  <summary>Updates state of buttons that manipulate namespaces.</summary>
    procedure UpdateButtons;
    ///  <summary>Stores list of namespaces in list box.</summary>
    ///  <param name="Namespaces">string [in] Comma separated list of
    ///  namespaces.</param>
    procedure PopulateNamespacesList(const Namespaces: string);
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
  // Delphi
  SysUtils,
  // Project
  UCtrlArranger, UExceptions, UStrUtils;

{$R *.dfm}


{ TCompilersDlgNamespacesFrame }

procedure TCompilersDlgNamespacesFrame.ArrangeControls;
begin
  TCtrlArranger.SetLabelHeights(Self);
  edNamespace.Top := TCtrlArranger.BottomOf(lblNamespace, 4);
  lblNamespaces.Top := TCtrlArranger.BottomOf(edNamespace, 16);
  lblExplainNamespaces.Top := TCtrlArranger.BottomOf(btnDelete, 12);
  lbNamespaces.Top := lblExplainNamespaces.Top;
  lblNamespaces.Top := lbNamespaces.Top - lblNamespaces.Height - 4;
end;

procedure TCompilersDlgNamespacesFrame.btnAddClick(Sender: TObject);
begin
  lbNamespaces.Items.Add(GetValidNamespace);
  edNamespace.Text := '';
  UpdateButtons;
end;

procedure TCompilersDlgNamespacesFrame.btnDefSwitchesClick(Sender: TObject);
begin
  PopulateNamespacesList(Compiler.GetDefaultRTLNamespaces);
  UpdateButtons;
end;

procedure TCompilersDlgNamespacesFrame.btnDeleteClick(Sender: TObject);
begin
  Assert(lbNamespaces.ItemIndex >= 0,
    ClassName + '.btnDeleteClick: lbNamespaces.ItemIndex < 0');
  lbNamespaces.Items.Delete(lbNamespaces.ItemIndex);
  UpdateButtons;
end;

procedure TCompilersDlgNamespacesFrame.btnReplaceClick(Sender: TObject);
begin
  Assert(lbNamespaces.ItemIndex >= 0,
    ClassName + '.btnReplaceClick: lbNamespaces.ItemIndex < 0');
  lbNamespaces.Items[lbNamespaces.ItemIndex] := GetValidNamespace;
  edNamespace.Text := '';
  UpdateButtons;
end;

procedure TCompilersDlgNamespacesFrame.edNamespaceChange(Sender: TObject);
begin
  UpdateButtons;
end;

function TCompilersDlgNamespacesFrame.GetValidNamespace: string;
resourcestring
  sError = 'Invalid namespace. A namespace must be a valid Pascal identifier, '
    + 'optionally containing dots.';
begin
  Result := StrTrim(edNamespace.Text);
  if not IsValidIdent(Result, True) then
    raise EDataEntry.Create(sError);
end;

procedure TCompilersDlgNamespacesFrame.Initialise;
begin
  PopulateNamespacesList(Compiler.GetRTLNamespaces);
  UpdateButtons;
end;

procedure TCompilersDlgNamespacesFrame.lbNamespacesClick(Sender: TObject);
begin
  edNamespace.Text := lbNamespaces.Items[lbNamespaces.ItemIndex];
  UpdateButtons;
end;

procedure TCompilersDlgNamespacesFrame.PopulateNamespacesList(
  const Namespaces: string);
begin
  StrExplode(Namespaces, ' ', lbNamespaces.Items, False);
end;

procedure TCompilersDlgNamespacesFrame.UpdateButtons;
begin
  btnAdd.Enabled := (StrTrim(edNamespace.Text) <> '') and
    (lbNamespaces.Items.IndexOf(StrTrim(edNamespace.Text)) = -1);
  btnReplace.Enabled := btnAdd.Enabled and (lbNamespaces.ItemIndex >= 0);
  btnDelete.Enabled := lbNamespaces.ItemIndex >= 0;
end;

procedure TCompilersDlgNamespacesFrame.UpdateCompiler;
begin
  Compiler.SetRTLNamespaces(StrJoin(lbNamespaces.Items, ' ', False));
end;

end.

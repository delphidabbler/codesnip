{
 * This Source Code Form is subject to the terms of the Mozilla Public License,
 * v. 2.0. If a copy of the MPL was not distributed with this file, You can
 * obtain one at https://mozilla.org/MPL/2.0/
 *
 * Copyright (C) 2026, Peter Johnson (gravatar.com/delphidabbler).
 *
 * Implements a dialogue box that lets the user rename a selected vault.
}


unit UI.Forms.RenameVaultDlg;

interface

uses
  // Delphi
  StdCtrls,
  Controls,
  ExtCtrls,
  Classes,
  // Project
  FmGenericOKDlg,
  UI.Adapters.VaultList,
  UBaseObjects;

type
  ///  <summary>Class that implements a dialogue box that enables the user to
  ///  rename a selected vault.</summary>
  TRenameVaultDlg = class(TGenericOKDlg, INoPublicConstruct)
    lblVaults: TLabel;
    cbVaults: TComboBox;
    lblNewName: TLabel;
    edNewName: TEdit;
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure btnOKClick(Sender: TObject);
  strict private
    var
      fVaultList: TVaultListAdapter;
    ///  <summary>Validates the new vault name entered by the user.</summary>
    ///  <exception><c>EDataEntry</c> raised if the validation fails.
    ///  </exception>
    procedure ValidateNewName;
  strict protected
    ///  <summary>Positions controls and sets form size according to body panel
    ///  dimensions.</summary>
    procedure ArrangeForm; override;
    ///  <summary>Initialises controls.</summary>
    procedure ConfigForm; override;
  public
    ///  <summary>Displays the Rename Vault dialogue box to enable the user to
    ///  rename a selected vault.</summary>
    ///  <param name="AOwner"><c>TComponent</c> [in] Reference to the component
    ///  that owns this dialogue box.</param>
    ///  <returns><c>Boolean</c>. <c>True</c> if the user accepts the renaming
    ///  or <c>False</c> otherwise.</returns>
    ///  <remarks>If the user OKs the renaming then the vault is renamed. But,
    ///  the database is not updated. This is left for the caller to do.
    ///  </remarks>
    class function Execute(const AOwner: TComponent): Boolean;
  end;

implementation

{$R *.dfm}

uses
  // Project
  DB.Vaults,
  UCtrlArranger,
  UExceptions,
  UStrUtils;

{ TRenameVaultDlg }

procedure TRenameVaultDlg.ArrangeForm;
begin
  TCtrlArranger.SetLabelHeights(Self);

  TCtrlArranger.AlignLefts([lblVaults, cbVaults, lblNewName, edNewName], 0);
  lblVaults.Top := 0;
  TCtrlArranger.MoveBelow(lblVaults, cbVaults, 6);
  TCtrlArranger.MoveBelow(cbVaults, lblNewName, 12);
  TCtrlArranger.MoveBelow(lblNewName, edNewName, 6);

  cbVaults.Width := pnlBody.ClientWidth;
  edNewName.Width := pnlBody.ClientWidth;

  pnlBody.ClientHeight := TCtrlArranger.TotalControlHeight(pnlBody) + 12;
  inherited;
end;

procedure TRenameVaultDlg.btnOKClick(Sender: TObject);
begin
  ModalResult := mrNone;
  ValidateNewName;
  fVaultList.Vault(cbVaults.ItemIndex).Name := StrTrim(edNewName.Text);
  ModalResult := mrOK;
end;

procedure TRenameVaultDlg.ConfigForm;
begin
  inherited;
  fVaultList.ToStrings(cbVaults.Items);
  cbVaults.ItemIndex := fVaultList.IndexOfUID(TVaultID.Default);
end;

class function TRenameVaultDlg.Execute(const AOwner: TComponent): Boolean;
var
  Dlg: TRenameVaultDlg;
begin
  Dlg := TRenameVaultDlg.InternalCreate(AOwner);
  try
    Result := Dlg.ShowModal = mrOK;
  finally
    Dlg.Free;
  end;
end;

procedure TRenameVaultDlg.FormCreate(Sender: TObject);
begin
  inherited;
  fVaultList := TVaultListAdapter.Create;
end;

procedure TRenameVaultDlg.FormDestroy(Sender: TObject);
begin
  fVaultList.Free;
  inherited;
end;

procedure TRenameVaultDlg.ValidateNewName;
resourcestring
  sNoName = 'A new vault name must be provided';
  sDefName = '"%s" can''t be used - it is the default vault name';
  sUnchangedName = 'Can''t rename a vault using the same name';
  sDupName = 'A vault name "%s" already exists';
var
  SelectedName: string;
  NewName: string;
begin
  SelectedName := fVaultList.Vault(cbVaults.ItemIndex).Name;
  NewName := StrTrim(edNewName.Text);

  if StrIsEmpty(NewName, True) then
    raise EDataEntry.Create(sNoName, edNewName);
  if StrSameText(NewName, TVaults.Instance.DefaultVaultName) then
    raise EDataEntry.CreateFmt(sDefName, [NewName], edNewName);
  if StrSameText(NewName, SelectedName) then
    raise EDataEntry.Create(sUnchangedName, edNewName);
  if TVaults.Instance.ContainsName(NewName) then
    raise EDataEntry.CreateFmt(sDupName, [NewName], edNewName);
end;

end.


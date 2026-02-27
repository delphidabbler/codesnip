{
 * This Source Code Form is subject to the terms of the Mozilla Public License,
 * v. 2.0. If a copy of the MPL was not distributed with this file, You can
 * obtain one at https://mozilla.org/MPL/2.0/
 *
 * Copyright (C) 2026, Peter Johnson (gravatar.com/delphidabbler).
 *
 * Implements a dialogue box that lets the user detach a vault from CodeSnip's
 * list of registered vaults.
}


unit UI.Forms.DetachVaultDlg;

interface

uses
  // Delphi
  Controls,
  StdCtrls,
  ExtCtrls,
  Classes,
  // Project
  DB.Vaults,
  FmGenericOKDlg,
  UBaseObjects,
  UI.Adapters.VaultList;

type
  ///  <summary>Class that implements a dialogue box that lets the user detach a
  ///  vault from CodeSnip's list of registered vaults.</summary>
  TDetachVaultDlg = class(TGenericOKDlg, INoPublicConstruct)
    lblVaults: TLabel;
    cbVaults: TComboBox;
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure btnOKClick(Sender: TObject);
  strict private
    var
      fVaultList: TVaultListAdapter;
    ///  <summary>Returns the ID of the vault currently selected in the drop
    ///  down list of vaults.</summary>
    function SelectedVaultID: TVaultID;
  strict protected
    ///  <summary>Positions controls and sets form size according to body panel
    ///  dimensions.</summary>
    procedure ArrangeForm; override;
    ///  <summary>Initialises controls.</summary>
    procedure ConfigForm; override;
  public
    ///  <summary>Displays the Detach Vault dialogue box to enable the user to
    ///  select a vault to be detached from the registered vault list.</summary>
    ///  <param name="AOwner"><c>TComponent</c> [in] Reference to the component
    ///  that owns this dialogue box.</param>
    ///  <param name="AVaultID"><c>TVaultID</c> [out] Set to the ID of the vault
    ///  to be detached, if the user OKs. If the user cancels then the value of
    ///  this parameter is undefined.</param>
    ///  <returns><c>Boolean</c>. <c>True</c> if the user accepts the detachment
    ///  or <c>False</c> otherwise.</returns>
    ///  <remarks>The caller is responsible for performing the detachment.
    ///  </remarks>
    class function Execute(const AOwner: TComponent; out AVaultID: TVaultID):
      Boolean;
  end;

implementation

{$R *.dfm}

uses
  // Project
  UCtrlArranger,
  UExceptions,
  UMessageBox;

{ TDetachVaultDlg }

procedure TDetachVaultDlg.ArrangeForm;
begin
  TCtrlArranger.SetLabelHeights(Self);

  TCtrlArranger.AlignLefts([lblVaults, cbVaults], 0);
  lblVaults.Top := 0;
  TCtrlArranger.MoveBelow(lblVaults, cbVaults, 6);

  cbVaults.Width := pnlBody.ClientWidth;

  pnlBody.ClientHeight := TCtrlArranger.TotalControlHeight(pnlBody) + 12;

  inherited;
end;

procedure TDetachVaultDlg.btnOKClick(Sender: TObject);
resourcestring
  sDefVaultErr = 'The default vault cannot be detached from the vault list';
  sConfirm = 'Are you sure you want to detach the vault?'
    + sLineBreak + sLineBreak
    + 'This step can''t be undone, but you can reattach the vault later using '
    + 'the Vaults | Add Vault menu option';
begin
  ModalResult := mrNone;
  if SelectedVaultID = TVaultID.Default then
    raise EDataEntry.Create(sDefVaultErr, cbVaults);
  if not TMessageBox.Confirm(Self, sConfirm) then
    Exit;
  ModalResult := mrOk;
end;

procedure TDetachVaultDlg.ConfigForm;
begin
  inherited;
  fVaultList.ToStrings(cbVaults.Items);
  cbVaults.ItemIndex := fVaultList.IndexOfUID(TVaultID.Default);
end;

class function TDetachVaultDlg.Execute(const AOwner: TComponent;
  out AVaultID: TVaultID): Boolean;
var
  Dlg: TDetachVaultDlg;
begin
  Dlg := TDetachVaultDlg.InternalCreate(AOwner);
  try
    Result := Dlg.ShowModal = mrOk;
    if Result then
    begin
      AVaultID := Dlg.SelectedVaultID;
    end;
  finally
    Dlg.Free;
  end;
end;

procedure TDetachVaultDlg.FormCreate(Sender: TObject);
begin
  inherited;
  fVaultList := TVaultListAdapter.Create;
end;

procedure TDetachVaultDlg.FormDestroy(Sender: TObject);
begin
  fVaultList.Free;
  inherited;
end;

function TDetachVaultDlg.SelectedVaultID: TVaultID;
begin
  Result := fVaultList.Vault(cbVaults.ItemIndex).UID;
end;

end.

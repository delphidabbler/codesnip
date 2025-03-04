{
 * This Source Code Form is subject to the terms of the Mozilla Public License,
 * v. 2.0. If a copy of the MPL was not distributed with this file, You can
 * obtain one at https://mozilla.org/MPL/2.0/
 *
 * Copyright (C) 2022-2023, Peter Johnson (gravatar.com/delphidabbler).
 *
 * Implements a dialogue box that enables the user to choose a vault from which
 * to delete all snippets.
}


unit UI.Forms.DeleteVaultDlg;

interface

uses
  // Delphi
  Forms,
  StdCtrls,
  Controls,
  ExtCtrls,
  Classes,
  // Project
  DB.Vaults,
  FmGenericOKDlg,
  FrBrowserBase,
  FrHTMLDlg,
  FrFixedHTMLDlg,
  UBaseObjects,
  UI.Adapters.VaultList;

type
  TDeleteVaultDlg = class(TGenericOKDlg, INoPublicConstruct)
    edConfirm: TEdit;
    frmWarning: TFixedHTMLDlgFrame;
    lblConfirm: TLabel;
    lblVaults: TLabel;
    cbVaults: TComboBox;
    procedure btnOKClick(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
  strict private
    const
      cConfirmText = 'DELETE MY SNIPPETS';
    var
      fPermissionGranted: Boolean;
      fVault: TVault;
      fVaultList: TVaultListAdapter;
    function SelectedVault: TVault;
    function IsValidPassword: Boolean;
  strict protected
    ///  <summary>Protected constructor that sets up form.</summary>
    constructor InternalCreate(AOwner: TComponent); override;
    procedure ConfigForm; override;
    procedure ArrangeForm; override;
  public
    class function Execute(AOwner: TComponent; out AVault: TVault): Boolean;
  end;

implementation

uses
  // Delphi
  SysUtils,
  // Project
  UCtrlArranger,
  UMessageBox;

{$R *.dfm}

procedure TDeleteVaultDlg.ArrangeForm;
begin
  frmWarning.Height := frmWarning.DocHeight;
  TCtrlArranger.AlignLefts([frmWarning, lblConfirm, lblVaults], 0);
  TCtrlArranger.AlignRights([frmWarning, cbVaults, edConfirm]);
  TCtrlArranger.AlignVCentres(
    TCtrlArranger.BottomOf(frmWarning, 12),
    [lblVaults, cbVaults]
  );
  TCtrlArranger.AlignVCentres(
    TCtrlArranger.BottomOf([lblVaults, cbVaults], 12),
    [lblConfirm, edConfirm]
  );
  pnlBody.ClientHeight := TCtrlArranger.TotalControlHeight(pnlBody) + 8;
  inherited;
end;

procedure TDeleteVaultDlg.btnOKClick(Sender: TObject);
resourcestring
  sBadPassword = 'Invalid confirmation text entered';
begin
  inherited;
  fPermissionGranted := IsValidPassword;
  fVault := SelectedVault;
  if not fPermissionGranted then
  begin
    TMessageBox.Error(Self, sBadPassword);
    edConfirm.Text := '';
    ModalResult := mrNone;
  end;
end;

procedure TDeleteVaultDlg.ConfigForm;
begin
  inherited;
  frmWarning.Initialise('dlg-dbdelete.html');
  fVaultList.ToStrings(cbVaults.Items);
  cbVaults.ItemIndex := fVaultList.IndexOfUID(TVaultID.Default);
end;

class function TDeleteVaultDlg.Execute(AOwner: TComponent; out AVault: TVault):
  Boolean;
var
  Dlg: TDeleteVaultDlg;
begin
  Dlg := InternalCreate(AOwner);
  try
    Dlg.ShowModal;
    Result := Dlg.fPermissionGranted;
    if Result then
      AVault := Dlg.fVault;
  finally
    Dlg.Free;
  end;
end;

procedure TDeleteVaultDlg.FormCreate(Sender: TObject);
begin
  inherited;
  fVaultList := TVaultListAdapter.Create;
end;

procedure TDeleteVaultDlg.FormDestroy(Sender: TObject);
begin
  fVaultList.Free;
  inherited;
end;

constructor TDeleteVaultDlg.InternalCreate(AOwner: TComponent);
begin
  Assert(Supports(Self, INoPublicConstruct), ClassName + '.InternalCreate: '
    + 'Form''s protected constructor can''t be called');
  inherited InternalCreate(AOwner);
end;

function TDeleteVaultDlg.IsValidPassword: Boolean;
begin
  Result := edConfirm.Text = cConfirmText;
end;

function TDeleteVaultDlg.SelectedVault: TVault;
begin
  Result := fVaultList.Vault(cbVaults.ItemIndex);
end;

end.

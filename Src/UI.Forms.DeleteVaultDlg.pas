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
    lblClearOrDelete: TLabel;
    rbDeleteVault: TRadioButton;
    rbKeepVault: TRadioButton;
    procedure btnOKClick(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
  strict private
    var
      fPermissionGranted: Boolean;
      fVault: TVault;
      fVaultList: TVaultListAdapter;
    function SelectedVault: TVault;
    function IsValidPassword(const AExpectedPwd: string): Boolean;
    function KeepVault: Boolean;
  strict protected
    ///  <summary>Protected constructor that sets up form.</summary>
    constructor InternalCreate(AOwner: TComponent); override;
    procedure ConfigForm; override;
    procedure ArrangeForm; override;
  public
    class function Execute(AOwner: TComponent; out AVault: TVault;
      out AKeepVault: Boolean): Boolean;
  end;

implementation

uses
  // Delphi
  SysUtils,
  // Project
  UCtrlArranger,
  UMessageBox,
  UStrUtils;

{$R *.dfm}

procedure TDeleteVaultDlg.ArrangeForm;
begin
  frmWarning.Top := 0;
  frmWarning.Height := frmWarning.DocHeight;
  TCtrlArranger.SetLabelHeights(Self);
  edConfirm.Width := pnlBody.ClientWidth;
  TCtrlArranger.AlignLefts(
    [frmWarning, lblConfirm, lblVaults, lblClearOrDelete, edConfirm], 0
  );
  TCtrlArranger.AlignRights([frmWarning, cbVaults, edConfirm]);
  rbKeepVault.Left := 24;
  TCtrlArranger.MoveToRightOf(rbKeepVault, rbDeleteVault, 24);

  TCtrlArranger.MoveBelow(frmWarning, lblClearOrDelete, 12);
  TCtrlArranger.AlignVCentres(
    TCtrlArranger.BottomOf(lblClearOrDelete, 8),
    [rbKeepVault, rbDeleteVault]
  );
  TCtrlArranger.AlignVCentres(
    TCtrlArranger.BottomOf([rbKeepVault, rbDeleteVault], 12),
    [lblVaults, cbVaults]
  );
  TCtrlArranger.MoveBelow([lblVaults, cbVaults], lblConfirm, 12);
  TCtrlArranger.MoveBelow(lblConfirm, edConfirm, 8);
  pnlBody.ClientHeight := TCtrlArranger.TotalControlHeight(pnlBody) + 8;
  inherited;
end;

procedure TDeleteVaultDlg.btnOKClick(Sender: TObject);
resourcestring
  sDefaultVault = 'You can''t delete the default vault';
  sBadPassword = 'Invalid confirmation text entered';
  sNoPassword = 'No confirmation text entered';
  sNoVaultChosen = 'No vault selected';

  procedure Fail(const AErrMsg: string);
  begin
    fPermissionGranted := False;
    ModalResult := mrNone;
    fVault := nil;
    edConfirm.Text := '';
    TMessageBox.Error(Self, AErrMsg);
  end;

begin
  inherited;

  fVault := SelectedVault;

  if not Assigned(fVault) then
  begin
    Fail(sNoVaultChosen);
    Exit;
  end;

  if fVault.IsDefault and not KeepVault then
  begin
    Fail(sDefaultVault);
    Exit;
  end;

  fPermissionGranted := IsValidPassword(fVault.Name);
  if not fPermissionGranted then
  begin
    Fail(StrIf(StrIsEmpty(edConfirm.Text), sNoPassword, sBadPassword));
    Exit;
  end;

end;

procedure TDeleteVaultDlg.ConfigForm;
begin
  inherited;
  frmWarning.Initialise('dlg-deletevault.html');
  fVaultList.ToStrings(cbVaults.Items);
  cbVaults.ItemIndex := -1;   // don't pre-select any of the vaults
end;

class function TDeleteVaultDlg.Execute(AOwner: TComponent; out AVault: TVault;
  out AKeepVault: Boolean): Boolean;
var
  Dlg: TDeleteVaultDlg;
begin
  Dlg := InternalCreate(AOwner);
  try
    Dlg.ShowModal;
    Result := Dlg.fPermissionGranted;
//    Result := False; /////////////////////////////////////////////
    if Result then
    begin
      AVault := Dlg.fVault;
      AKeepVault := Dlg.KeepVault;
    end;
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

function TDeleteVaultDlg.IsValidPassword(const AExpectedPwd: string): Boolean;
begin
  Result := StrSameText(edConfirm.Text, AExpectedPwd);
end;

function TDeleteVaultDlg.KeepVault: Boolean;
begin
  Result := rbKeepVault.Checked;
end;

function TDeleteVaultDlg.SelectedVault: TVault;
begin
  if cbVaults.ItemIndex >= 0 then
    Result := fVaultList.Vault(cbVaults.ItemIndex)
  else
    Result := nil;
end;

end.

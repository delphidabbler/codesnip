{
 * This Source Code Form is subject to the terms of the Mozilla Public License,
 * v. 2.0. If a copy of the MPL was not distributed with this file, You can
 * obtain one at https://mozilla.org/MPL/2.0/
 *
 * Copyright (C) 2024, Peter Johnson (gravatar.com/delphidabbler).
 *
 * Implements a dialogue box that enables the user to choose a vault to backup
 * or restore along with the directory to backup to or restore from.
}


unit FmCollectionBackup;

interface

uses
  // Delphi
  Classes,
  Controls,
  StdCtrls,
  ExtCtrls,
  // Project
  DB.Vaults,
  FmGenericOKDlg,
  UI.Adapters.VaultList;

type
  TVaultBackupDlg = class(TGenericOKDlg)
    lblVaults: TLabel;
    cbVaults: TComboBox;
    lblPath: TLabel;
    edPath: TEdit;
    btnBrowse: TButton;
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure btnBrowseClick(Sender: TObject);
    procedure btnOKClick(Sender: TObject);
  strict private
    var
      fFileName: string;
      fVault: TVault;
      fVaultList: TVaultListAdapter;
    function GetFilePathFromEditCtrl: string;
  strict protected
    procedure ConfigForm; override;
    procedure ArrangeForm; override;
  public
    class function Execute(AOwner: TComponent;
      out AFileName: string; out AVault: TVault): Boolean;
  end;

implementation

{$R *.dfm}

uses
  // Delphi
  IOUtils,
  Dialogs,
  // Project
  UCtrlArranger,
  UMessageBox,
  UOpenDialogHelper,
  USaveDialogEx,
  UStrUtils;

procedure TVaultBackupDlg.ArrangeForm;
begin
  TCtrlArranger.AlignLefts([lblVaults, cbVaults, lblPath, edPath], 0);
  // row 1
  lblVaults.Top := 0;
  // row 2
  TCtrlArranger.MoveBelow(lblVaults, cbVaults, 6);
  // row 3
  TCtrlArranger.MoveBelow(cbVaults, lblPath, 12);
  // row 4
  TCtrlArranger.MoveToRightOf(edPath, btnBrowse, 6);
  TCtrlArranger.AlignVCentres(
    TCtrlArranger.BottomOf(lblPath, 6), [edPath, btnBrowse]
  );
  pnlBody.ClientWidth := TCtrlArranger.TotalControlWidth(pnlBody);
  pnlBody.ClientHeight := TCtrlArranger.TotalControlHeight(pnlBody) + 8;
  inherited;
end;

procedure TVaultBackupDlg.btnBrowseClick(Sender: TObject);
var
  SaveDlg: TSaveDialogEx;       // save dialog box used to name backup file
resourcestring
  // Dialog box caption
  sCaption = 'Save Backup';
begin
  // Get backup file name from user via standard save dialog box
  SaveDlg := TSaveDialogEx.Create(nil);
  try
    SaveDlg.Title := sCaption;
    SaveDlg.Options := [ofShowHelp, ofExtensionDifferent, ofPathMustExist,
      ofNoTestFileCreate, ofEnableSizing];
    SaveDlg.HelpKeyword := 'SaveBackupDlg';
    if SaveDlg.Execute then
      edPath.Text := SaveDlg.FileName;
  finally
    SaveDlg.Free;
  end;
end;

procedure TVaultBackupDlg.btnOKClick(Sender: TObject);
begin
  fFileName := GetFilePathFromEditCtrl;
  fVault := fVaultList.Vault(cbVaults.ItemIndex);
end;

procedure TVaultBackupDlg.ConfigForm;
begin
  inherited;
  fVaultList.ToStrings(cbVaults.Items);
  cbVaults.ItemIndex := fVaultList.IndexOfUID(TVaultID.Default);
end;

class function TVaultBackupDlg.Execute(AOwner: TComponent;
  out AFileName: string; out AVault: TVault): Boolean;
var
  Dlg: TVaultBackupDlg;
begin
  Dlg := TVaultBackupDlg.Create(AOwner);
  Result := Dlg.ShowModal = mrOK;
  if Result then
  begin
    AFileName := Dlg.fFileName;
    AVault := Dlg.fVault;
  end;
end;

procedure TVaultBackupDlg.FormCreate(Sender: TObject);
begin
  inherited;
  fVaultList := TVaultListAdapter.Create;
end;

procedure TVaultBackupDlg.FormDestroy(Sender: TObject);
begin
  fVaultList.Free;
  inherited;
end;

function TVaultBackupDlg.GetFilePathFromEditCtrl: string;
begin
  Result := StrTrim(edPath.Text);
end;

end.

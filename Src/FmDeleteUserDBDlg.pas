{
 * This Source Code Form is subject to the terms of the Mozilla Public License,
 * v. 2.0. If a copy of the MPL was not distributed with this file, You can
 * obtain one at https://mozilla.org/MPL/2.0/
 *
 * Copyright (C) 2022-2023, Peter Johnson (gravatar.com/delphidabbler).
 *
 * Implements a dialogue box that enables the user to choose a collection from
 * which to delete all snippets.
}


unit FmDeleteUserDBDlg;

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
  TDeleteUserDBDlg = class(TGenericOKDlg, INoPublicConstruct)
    edConfirm: TEdit;
    frmWarning: TFixedHTMLDlgFrame;
    lblConfirm: TLabel;
    lblCollection: TLabel;
    cbCollection: TComboBox;
    procedure btnOKClick(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
  strict private
    const
      cConfirmText = 'DELETE MY SNIPPETS';
    var
      fPermissionGranted: Boolean;
      fCollection: TVault;
      fCollList: TVaultListAdapter;
    function SelectedCollection: TVault;
    function IsValidPassword: Boolean;
  strict protected
    ///  <summary>Protected constructor that sets up form.</summary>
    constructor InternalCreate(AOwner: TComponent); override;
    procedure ConfigForm; override;
    procedure ArrangeForm; override;
  public
    class function Execute(AOwner: TComponent; out ACollection: TVault): Boolean;
  end;

implementation

uses
  // Delphi
  SysUtils,
  // Project
  UCtrlArranger,
  UMessageBox;

{$R *.dfm}

procedure TDeleteUserDBDlg.ArrangeForm;
begin
  frmWarning.Height := frmWarning.DocHeight;
  TCtrlArranger.AlignLefts([frmWarning, lblConfirm, lblCollection], 0);
  TCtrlArranger.AlignRights([frmWarning, cbCollection, edConfirm]);
  TCtrlArranger.AlignVCentres(
    TCtrlArranger.BottomOf(frmWarning, 12),
    [lblCollection, cbCollection]
  );
  TCtrlArranger.AlignVCentres(
    TCtrlArranger.BottomOf([lblCollection, cbCollection], 12),
    [lblConfirm, edConfirm]
  );
  pnlBody.ClientHeight := TCtrlArranger.TotalControlHeight(pnlBody) + 8;
  inherited;
end;

procedure TDeleteUserDBDlg.btnOKClick(Sender: TObject);
resourcestring
  sBadPassword = 'Invalid confirmation text entered';
begin
  inherited;
  fPermissionGranted := IsValidPassword;
  fCollection := SelectedCollection;
  if not fPermissionGranted then
  begin
    TMessageBox.Error(Self, sBadPassword);
    edConfirm.Text := '';
    ModalResult := mrNone;
  end;
end;

procedure TDeleteUserDBDlg.ConfigForm;
begin
  inherited;
  frmWarning.Initialise('dlg-dbdelete.html');
  fCollList.ToStrings(cbCollection.Items);
  cbCollection.ItemIndex := fCollList.IndexOfUID(TVaultID.Default);
end;

class function TDeleteUserDBDlg.Execute(AOwner: TComponent;
  out ACollection: TVault): Boolean;
var
  Dlg: TDeleteUserDBDlg;
begin
  Dlg := InternalCreate(AOwner);
  try
    Dlg.ShowModal;
    Result := Dlg.fPermissionGranted;
    if Result then
      ACollection := Dlg.fCollection;
  finally
    Dlg.Free;
  end;
end;

procedure TDeleteUserDBDlg.FormCreate(Sender: TObject);
begin
  inherited;
  fCollList := TVaultListAdapter.Create;
end;

procedure TDeleteUserDBDlg.FormDestroy(Sender: TObject);
begin
  fCollList.Free;
  inherited;
end;

constructor TDeleteUserDBDlg.InternalCreate(AOwner: TComponent);
begin
  Assert(Supports(Self, INoPublicConstruct), ClassName + '.InternalCreate: '
    + 'Form''s protected constructor can''t be called');
  inherited InternalCreate(AOwner);
end;

function TDeleteUserDBDlg.IsValidPassword: Boolean;
begin
  Result := edConfirm.Text = cConfirmText;
end;

function TDeleteUserDBDlg.SelectedCollection: TVault;
begin
  Result := fCollList.Vault(cbCollection.ItemIndex);
end;

end.

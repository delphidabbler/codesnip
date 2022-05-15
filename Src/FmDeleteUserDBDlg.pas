{
 * This Source Code Form is subject to the terms of the Mozilla Public License,
 * v. 2.0. If a copy of the MPL was not distributed with this file, You can
 * obtain one at https://mozilla.org/MPL/2.0/
 *
 * Copyright (C) 2022, Peter Johnson (gravatar.com/delphidabbler).
 *
 * Implements a dialogue box that asks user to confirm deletion of user-defined
 * snippets database.
}


unit FmDeleteUserDBDlg;

interface

uses
  // Delphi
  Forms, StdCtrls, Controls, ExtCtrls, Classes,
  // Project
  FmGenericOKDlg,
  FrBrowserBase, FrHTMLDlg, FrFixedHTMLDlg,
  UBaseObjects;

type
  TDeleteUserDBDlg = class(TGenericOKDlg, INoPublicConstruct)
    edConfirm: TEdit;
    frmWarning: TFixedHTMLDlgFrame;
    procedure btnOKClick(Sender: TObject);
  strict private
    const
      cConfirmText = 'DELETE MY SNIPPETS';
    var
      fPermissionGranted: Boolean;
  strict protected
    ///  <summary>Protected constructor that sets up form.</summary>
    constructor InternalCreate(AOwner: TComponent); override;
    procedure ConfigForm; override;
    procedure ArrangeForm; override;
    function IsValidPassword: Boolean;
  public
    class function Execute(AOwner: TComponent): Boolean;
  end;

implementation

uses
  // Delphi
  SysUtils,
  // Project
  UCtrlArranger, UMessageBox;

{$R *.dfm}

procedure TDeleteUserDBDlg.ArrangeForm;
begin
  frmWarning.Height := frmWarning.DocHeight;
  edConfirm.Left := 0;
  TCtrlArranger.MoveBelow(frmWarning, edConfirm, 12);
  TCtrlArranger.AlignHCentresTo([frmWarning], [edConfirm]);
  pnlBody.ClientHeight := TCtrlArranger.TotalControlHeight(pnlBody) + 8;
  inherited;
end;

procedure TDeleteUserDBDlg.btnOKClick(Sender: TObject);
resourcestring
  sBadPassword = 'Invalid confirmation text entered';
begin
  inherited;
  fPermissionGranted := IsValidPassword;
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
//  frmWarning.OnBuildCSS := BuildCSS;
  frmWarning.Initialise('dlg-dbdelete.html');
end;

class function TDeleteUserDBDlg.Execute(AOwner: TComponent): Boolean;
begin
  with InternalCreate(AOwner) do
    try
      ShowModal;
      Result := fPermissionGranted;
    finally
      Free;
    end;
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

end.

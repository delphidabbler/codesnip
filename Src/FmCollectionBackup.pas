{
 * This Source Code Form is subject to the terms of the Mozilla Public License,
 * v. 2.0. If a copy of the MPL was not distributed with this file, You can
 * obtain one at https://mozilla.org/MPL/2.0/
 *
 * Copyright (C) 2024, Peter Johnson (gravatar.com/delphidabbler).
 *
 * Implements a dialogue box that enables the user to choose a collection to
 * backup or restore along with the directory to backup to or restore from.
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
  DB.UCollections,
  FmGenericOKDlg,
  UCollectionListAdapter;

type
  TCollectionBackupDlg = class(TGenericOKDlg)
    lblCollection: TLabel;
    cbCollection: TComboBox;
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
      fCollection: TVault;
      fCollList: TCollectionListAdapter;
    function GetFilePathFromEditCtrl: string;
  strict protected
    procedure ConfigForm; override;
    procedure ArrangeForm; override;
  public
    class function Execute(AOwner: TComponent;
      out AFileName: string; out ACollection: TVault): Boolean;
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

procedure TCollectionBackupDlg.ArrangeForm;
begin
  TCtrlArranger.AlignLefts([lblCollection, cbCollection, lblPath, edPath], 0);
  // row 1
  lblCollection.Top := 0;
  // row 2
  TCtrlArranger.MoveBelow(lblCollection, cbCollection, 6);
  // row 3
  TCtrlArranger.MoveBelow(cbCollection, lblPath, 12);
  // row 4
  TCtrlArranger.MoveToRightOf(edPath, btnBrowse, 6);
  TCtrlArranger.AlignVCentres(
    TCtrlArranger.BottomOf(lblPath, 6), [edPath, btnBrowse]
  );
  pnlBody.ClientWidth := TCtrlArranger.TotalControlWidth(pnlBody);
  pnlBody.ClientHeight := TCtrlArranger.TotalControlHeight(pnlBody) + 8;
  inherited;
end;

procedure TCollectionBackupDlg.btnBrowseClick(Sender: TObject);
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

procedure TCollectionBackupDlg.btnOKClick(Sender: TObject);
begin
  fFileName := GetFilePathFromEditCtrl;
  fCollection := fCollList.Collection(cbCollection.ItemIndex);
end;

procedure TCollectionBackupDlg.ConfigForm;
begin
  inherited;
  fCollList.ToStrings(cbCollection.Items);
  cbCollection.ItemIndex := fCollList.IndexOfUID(TVaultID.Default);
end;

class function TCollectionBackupDlg.Execute(AOwner: TComponent;
  out AFileName: string; out ACollection: TVault): Boolean;
var
  Dlg: TCollectionBackupDlg;
begin
  Dlg := TCollectionBackupDlg.Create(AOwner);
  Result := Dlg.ShowModal = mrOK;
  if Result then
  begin
    AFileName := Dlg.fFileName;
    ACollection := Dlg.fCollection;
  end;
end;

procedure TCollectionBackupDlg.FormCreate(Sender: TObject);
begin
  inherited;
  fCollList := TCollectionListAdapter.Create;
end;

procedure TCollectionBackupDlg.FormDestroy(Sender: TObject);
begin
  fCollList.Free;
  inherited;
end;

function TCollectionBackupDlg.GetFilePathFromEditCtrl: string;
begin
  Result := StrTrim(edPath.Text);
end;

end.

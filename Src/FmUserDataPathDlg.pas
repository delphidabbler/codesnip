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
 * Implements a dialogue box that can be used to move the user database to a
 * different directory.
}


unit FmUserDataPathDlg;


interface


uses
  // Delphi
  SysUtils, Classes, ActnList, StdCtrls, Controls, ExtCtrls,
  // Project
  FmGenericViewDlg, UBaseObjects;


type
  TUserDataPathDlg = class(TGenericViewDlg, INoPublicConstruct)
    actBrowse: TAction;
    actDefaultPath: TAction;
    actMove: TAction;
    alDlg: TActionList;
    btnBrowse: TButton;
    btnMove: TButton;
    btnDefaultPath: TButton;
    edPath: TEdit;
    lblInstructions: TLabel;
    lblPath: TLabel;
    procedure actBrowseExecute(Sender: TObject);
    procedure actDefaultPathExecute(Sender: TObject);
    procedure actDefaultPathUpdate(Sender: TObject);
    procedure actMoveExecute(Sender: TObject);
    procedure actMoveUpdate(Sender: TObject);
  strict private
    function NewDirFromEditCtrl: string;
    procedure DoMove;
    procedure HandleException(const E: Exception);
  strict protected
    procedure ArrangeForm; override;
    procedure InitForm; override;
  public
    class procedure Execute(AOwner: TComponent);
  end;


implementation


uses
  // Delphi
  IOUtils, RTLConsts,
  // Project
  UAppInfo, UBrowseForFolderDlg, UCtrlArranger, UExceptions, UMessageBox,
  UStrUtils;

{$R *.dfm}


{ TUserDataPathDlg }

procedure TUserDataPathDlg.actBrowseExecute(Sender: TObject);
var
  Dlg: TBrowseForFolderDlg; // browse for folder standard dialog box
resourcestring
  sDlgTitle = 'Choose Database Directory';
  sDlgHeading = 'Choose an empty directory or create a new one';
begin
  Dlg := TBrowseForFolderDlg.Create(nil);
  try
    Dlg.Title := sDlgTitle;
    Dlg.Headline := sDlgHeading;
    Dlg.MakeFolderBtnVisible := True;
    Dlg.HelpKeyword := 'ChooseUserDBDirDlg';
    if Dlg.Execute then
      edPath.Text := Dlg.FolderName;
  finally
    Dlg.Free;
  end;
end;

procedure TUserDataPathDlg.actDefaultPathExecute(Sender: TObject);
begin
  edPath.Text := TAppInfo.DefaultUserDataDir;
end;

procedure TUserDataPathDlg.actDefaultPathUpdate(Sender: TObject);
begin
  actDefaultPath.Enabled := (NewDirFromEditCtrl <> '')
    and not StrSameText(NewDirFromEditCtrl, TAppInfo.DefaultUserDataDir);
end;

procedure TUserDataPathDlg.actMoveExecute(Sender: TObject);
resourcestring
  sNonEmptyDir = 'The specified directory is not empty';
  sConfirmMsg = 'Are you sure you want to move the user database';
begin
  if TDirectory.Exists(NewDirFromEditCtrl)
    and not TDirectory.IsEmpty(NewDirFromEditCtrl) then
    raise EDataEntry.Create(sNonEmptyDir, edPath);
  if not TMessageBox.Confirm(Self, sConfirmMsg) then
    Exit;
  try
    DoMove;
    TAppInfo.ChangeUserDataDir(NewDirFromEditCtrl);
  except
    on E: Exception do
      HandleException(E);
  end;
end;

procedure TUserDataPathDlg.actMoveUpdate(Sender: TObject);
begin
  actMove.Enabled := (NewDirFromEditCtrl <> '')
    and not StrSameText(NewDirFromEditCtrl, TAppInfo.UserDataDir);
end;

procedure TUserDataPathDlg.ArrangeForm;
begin
  TCtrlArranger.SetLabelHeight(lblInstructions);
  TCtrlArranger.AlignLefts([lblInstructions, lblPath, edPath], 0);
  TCtrlArranger.MoveToRightOf(edPath, btnBrowse, 4);
  lblInstructions.Width := TCtrlArranger.RightOf(btnBrowse);
  TCtrlArranger.AlignHCentresTo([edPath, btnBrowse], [btnDefaultPath, btnMove]);
  pnlBody.ClientWidth := TCtrlArranger.TotalControlWidth(pnlBody);

  lblInstructions.Top := 0;
  TCtrlArranger.MoveBelow(lblInstructions, lblPath, 12);
  TCtrlArranger.AlignVCentres(
    TCtrlArranger.BottomOf(lblPath, 4), [edPath, btnBrowse]
  );
  TCtrlArranger.MoveBelow([edPath, btnBrowse], btnDefaultPath, 8);
  TCtrlArranger.MoveBelow(btnDefaultPath, btnMove, 18);
  pnlBody.ClientHeight := TCtrlArranger.TotalControlHeight(pnlBody) + 18;

  inherited;
end;

procedure TUserDataPathDlg.DoMove;
var
  SourceDir, DestDir: string;
begin
  SourceDir := TAppInfo.UserDataDir;
  DestDir := NewDirFromEditCtrl;
  if TDirectory.Exists(DestDir) and not TDirectory.IsEmpty(DestDir) then
    raise EInOutError.Create(SDirectoryNotEmpty);
  if not TDirectory.Exists(DestDir) then
    TDirectory.CreateDirectory(DestDir);
  TDirectory.Copy(SourceDir, DestDir);
  TDirectory.Delete(SourceDir, True);
end;

class procedure TUserDataPathDlg.Execute(AOwner: TComponent);
begin
  {$IFDEF PORTABLE}
  raise EBug.Create(ClassName + '.Execute: Call forbidden in portable version');
  {$ENDIF}
  with InternalCreate(AOwner) do
    try
      ShowModal
    finally
      Free;
    end;
end;

procedure TUserDataPathDlg.HandleException(const E: Exception);
begin
  if (E is EInOutError) or (E is ENotSupportedException)
    or (E is EDirectoryNotFoundException) or (E is EPathTooLongException)
    or (E is EArgumentException) then
    raise EDataEntry.Create(E.Message);
  raise E;
end;

procedure TUserDataPathDlg.InitForm;
begin
  inherited;
  edPath.Text := TAppInfo.UserDataDir;
end;

function TUserDataPathDlg.NewDirFromEditCtrl: string;
begin
  Result := StrTrim(edPath.Text);
end;

end.


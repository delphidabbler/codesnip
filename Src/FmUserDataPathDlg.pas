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
    lblInstructions: TLabel;
    gbMove: TGroupBox;
    edPath: TEdit;
    lblPath: TLabel;
    btnBrowse: TButton;
    lblExplainMove: TLabel;
    btnMove: TButton;
    gbRestore: TGroupBox;
    lblExplainDefaultPath: TLabel;
    btnDefaultPath: TButton;
    lblWarning: TLabel;
    procedure actBrowseExecute(Sender: TObject);
    procedure actDefaultPathExecute(Sender: TObject);
    procedure actDefaultPathUpdate(Sender: TObject);
    procedure actMoveExecute(Sender: TObject);
    procedure actMoveUpdate(Sender: TObject);
  strict private
    function NewDirFromEditCtrl: string;
    procedure DoMove(const DestDir: string);
    procedure HandleException(const E: Exception);
  strict protected
    procedure ConfigForm; override;
    procedure ArrangeForm; override;
  public
    class procedure Execute(AOwner: TComponent);
  end;


implementation


uses
  // Delphi
  IOUtils, RTLConsts,
  // Project
  UAppInfo, UBrowseForFolderDlg, UColours, UConsts, UCtrlArranger, UExceptions,
  UFontHelper, UMessageBox, UStrUtils;

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
resourcestring
  sNonEmptyDir = 'Can''t restore the database to its default directory because '
    + 'the directory it is not empty.'
    + EOL2
    + 'Please clear out all the contents of "%s" before re-trying.';
  sConfirmMsg = 'Are you sure you want to restore the user database to its '
    + 'default location?';
begin
  if TDirectory.Exists(TAppInfo.DefaultUserDataDir) and
    not TDirectory.IsEmpty(TAppInfo.DefaultUserDataDir) then
    raise ECodeSnip.CreateFmt(sNonEmptyDir, [TAppInfo.DefaultUserDataDir]);
  if not TMessageBox.Confirm(Self, sConfirmMsg) then
    Exit;
  try
    DoMove(TAppInfo.DefaultUserDataDir);
    TAppInfo.ChangeUserDataDir(TAppInfo.DefaultUserDataDir);
  except
    on E: Exception do
      HandleException(E);
  end;
end;

procedure TUserDataPathDlg.actDefaultPathUpdate(Sender: TObject);
begin
  actDefaultPath.Enabled :=
    not StrSameText(TAppInfo.UserDataDir, TAppInfo.DefaultUserDataDir);
end;

procedure TUserDataPathDlg.actMoveExecute(Sender: TObject);
resourcestring
  sNonEmptyDir = 'The specified directory is not empty';
  sConfirmMsg = 'Are you sure you want to move the user database?';
begin
  if TDirectory.Exists(NewDirFromEditCtrl)
    and not TDirectory.IsEmpty(NewDirFromEditCtrl) then
    raise EDataEntry.Create(sNonEmptyDir, edPath);
  if not TMessageBox.Confirm(Self, sConfirmMsg) then
    Exit;
  try
    DoMove(NewDirFromEditCtrl);
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
  TCtrlArranger.SetLabelHeights(Self);

  pnlBody.ClientWidth := TCtrlArranger.TotalControlWidth(pnlBody);

  TCtrlArranger.AlignVCentres(
    TCtrlArranger.BottomOf(lblPath, 4), [edPath, btnBrowse]
  );
  TCtrlArranger.MoveBelow([edPath, btnBrowse], lblExplainMove, 8);
  TCtrlArranger.MoveBelow(lblExplainMove, btnMove, 8);
  gbMove.ClientHeight := TCtrlArranger.TotalControlHeight(gbMove) + 8;

  TCtrlArranger.MoveBelow(lblExplainDefaultPath, btnDefaultPath, 12);
  gbRestore.ClientHeight := TCtrlArranger.TotalControlHeight(gbRestore) + 12;

  TCtrlArranger.MoveBelow(lblInstructions, lblWarning, 8);
  TCtrlArranger.MoveBelow(lblWarning, gbMove, 12);
  TCtrlArranger.MoveBelow(gbMove, gbRestore, 12);

  pnlBody.ClientHeight := TCtrlArranger.TotalControlHeight(pnlBody) + 8;
  inherited;
end;

procedure TUserDataPathDlg.ConfigForm;
begin
  inherited;
  TFontHelper.SetDefaultBaseFonts([
    gbMove.Font, gbRestore.Font, lblWarning.Font]
  );
  TFontHelper.SetDefaultFonts([
    lblPath.Font, edPath.Font, lblExplainMove.Font,
    btnBrowse.Font, btnMove.Font, lblExplainDefaultPath.Font,
    btnDefaultPath.Font
  ]);
//  lblWarning.Font.Color := clWarningText;
end;

procedure TUserDataPathDlg.DoMove(const DestDir: string);
resourcestring
  sCantMoveToSubDir = 'Can''t move database into a sub-directory of the '
    + 'existing database directory';
  sMustBeRooted = 'A full path to the new database directory must be provided.';
var
  SourceDir: string;
begin
  SourceDir := TAppInfo.UserDataDir;
  if not TPath.IsPathRooted(DestDir) then
    raise EInOutError.Create(sMustBeRooted);
  if TDirectory.Exists(DestDir) and not TDirectory.IsEmpty(DestDir) then
    raise EInOutError.Create(SDirectoryNotEmpty);
  if StrStartsText(
    IncludeTrailingPathDelimiter(TAppInfo.UserDataDir), DestDir
  ) then
    raise EInOutError.Create(sCantMoveToSubDir);
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

function TUserDataPathDlg.NewDirFromEditCtrl: string;
begin
  Result := ExcludeTrailingPathDelimiter(StrTrim(edPath.Text));
end;

end.


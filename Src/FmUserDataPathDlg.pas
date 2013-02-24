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
  ///  <summary>Dialogue box that is used to move the user database to a new
  ///  directory or to restore a previously moved database to its default
  ///  directory.</summary>
  ///  <remarks>IMPORTANT: This dialogue box is for use only with the standard
  ///  edition of CodeSnip. It MUST NOT be displayed from the portable edition.
  ///  </remarks>
  TUserDataPathDlg = class(TGenericViewDlg, INoPublicConstruct)
    actBrowse: TAction;
    actDefaultPath: TAction;
    actMove: TAction;
    alDlg: TActionList;
    btnBrowse: TButton;
    btnDefaultPath: TButton;
    btnMove: TButton;
    gbMove: TGroupBox;
    gbRestore: TGroupBox;
    lblExplainDefaultPath: TLabel;
    lblExplainMove: TLabel;
    lblInstructions: TLabel;
    lblPath: TLabel;
    lblWarning: TLabel;
    edPath: TEdit;
    ///  <summary>Dispays Browse For Folder dialogue box and copies any chosen
    ///  folder to the edPath edit control.</summary>
    procedure actBrowseExecute(Sender: TObject);
    ///  <summary>Moves user database back to default directory and records the
    ///  changed path.</summary>
    ///  <exception>Raises exception if default path can't be used for any
    ///  reason or if there was an error copying the database.</exception>
    procedure actDefaultPathExecute(Sender: TObject);
    ///  <summary>Enables / disables Default Path action according to whether
    ///  database is already in default path.</summary>
    procedure actDefaultPathUpdate(Sender: TObject);
    ///  <summary>Moves user database to path entered by user and records the
    ///  changed path.</summary>
    ///  <exception>Raises exception given path can't be used for any reason or
    ///  if there was an error copying the database.</exception>
    procedure actMoveExecute(Sender: TObject);
    ///  <summary>Enables / disables Move action according to whether a suitable
    ///  path has been entered by user.</summary>
    procedure actMoveUpdate(Sender: TObject);
  strict private
    ///  <summary>Gets directory entered in edPath edit control.</summary>
    ///  <remarks>Edit control contents are trimmed of spaces and any trailing
    ///  path delimiter.</remarks>
    function NewDirFromEditCtrl: string;
    ///  <summary>Move user database from its current location to the given
    ///  directory.</summary>
    ///  <exception>Raises an exception if DestDir is not a full path, if
    ///  DestDir is a sub-directory of the current database directory or if an
    ///  error occurs during the move operation.</exception>
    procedure DoMove(const DestDir: string);
    ///  <summary>Handles given exception, converting expected exceptions into
    ///  EDataEntry and re-raising all other unchanged.</summary>
    ///  <exception>Always raises a new exception.</exception>
    ///  <remarks>This method is designed to handle exception raised when the
    ///  user database is moved.</remarks>
    procedure HandleException(const E: Exception);
  strict protected
    ///  <summary>Sets controls with ParentFont=False to use system default
    ///  fonts, preserving font styles for those fonts that need them.</summary>
    procedure ConfigForm; override;
    ///  <summary>Arranges form's controls and sizes the dialogue box to fit.
    ///  </summary>
    procedure ArrangeForm; override;
  public
    ///  <summary>Displays the dialogue box aligned over the given owner
    ///  control.</summary>
    ///  <exception>Raise EBug if called by the portable edition of CodeSnip.
    ///  </exception>
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


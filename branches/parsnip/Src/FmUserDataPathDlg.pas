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
 * Implements a dialogue box that can be used to move the snippets database to a
 * different directory.
}


unit FmUserDataPathDlg;


// TODO: Rename this unit to relate to form class name


interface


uses
  // Delphi
  SysUtils,
  Forms,
  Classes,
  ActnList,
  StdCtrls,
  Controls,
  ExtCtrls,
  // Project
  FmGenericViewDlg,
  FmUserDataPathDlg.FrProgress,
  UBaseObjects,
  UControlStateMgr,
  UUserDBMove;

type
  ///  <summary>Dialogue box that is used to move the snippets database to a new
  ///  directory or to restore a previously moved database to its default
  ///  directory.</summary>
  ///  <remarks>IMPORTANT: This dialogue box is for use only when running in
  ///  standard mode. It MUST NOT be displayed when in portable mode.</remarks>
  TDBMoveDlg = class(TGenericViewDlg, INoPublicConstruct)
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
    frmProgress: TDBMoveDlgProgressFrame;
    ///  <summary>Dispays Browse For Folder dialogue box and copies any chosen
    ///  folder to the edPath edit control.</summary>
    procedure actBrowseExecute(Sender: TObject);
    ///  <summary>Moves database back to default directory and records the
    ///  changed path.</summary>
    ///  <exception>Raises exception if default path can't be used for any
    ///  reason or if there was an error copying the database.</exception>
    procedure actDefaultPathExecute(Sender: TObject);
    ///  <summary>Enables / disables Default Path action according to whether
    ///  database is already in default path.</summary>
    procedure actDefaultPathUpdate(Sender: TObject);
    ///  <summary>Moves database to path entered by user and records the changed
    ///  path.</summary>
    ///  <exception>Raises exception given path can't be used for any reason or
    ///  if there was an error copying the database.</exception>
    procedure actMoveExecute(Sender: TObject);
    ///  <summary>Enables / disables Move action according to whether a suitable
    ///  path has been entered by user.</summary>
    procedure actMoveUpdate(Sender: TObject);
    ///  <summary>Constructs and initialises form's owned object.</summary>
    procedure FormCreate(Sender: TObject);
    ///  <summary>Destroys form's owned objects.</summary>
    procedure FormDestroy(Sender: TObject);
  strict private
    var
      ///  <summary>Object that moves the snippets database to a new location.
      ///  </summary>
      fMover: TDBMove;
      ///  <summary>Object used to disable and enable all controls on the form.
      ///  </summary>
      fControlStateMgr: TControlStateMgr;
    ///  <summary>Sets visibility of all child controls of a parent control.
    ///  </summary>
    ///  <param name="ParentCtrl">TWinControl [in] Parent of affected controls.
    ///  </param>
    ///  <param name="Show">Boolean [in] Flag indicating required visibility.
    ///  Pass True to show the controls and False to hide them.</param>
    procedure SetVisibility(const ParentCtrl: TWinControl; const Show: Boolean);
    ///  <summary>Performs the database move to the directory given by NewDir,
    ///  displaying a progress base located over the given host window.
    ///  </summary>
    ///  <remarks>The new directory is checked to be empty and the user is asked
    ///  for confirmation.</remarks>
    procedure DoMove(const NewDir: string; const ProgressHostCtrl: TWinControl);
    ///  <summary>Handles the database mover object's OnCopyFile event by
    ///  updating the progress frame.</summary>
    procedure CopyFileHandler(Sender: TObject; const Percent: Byte);
    ///  <summary>Handles the database mover object's OnDeleteFile event by
    ///  updating the progress frame.</summary>
    procedure DeleteFileHandler(Sender: TObject; const Percent: Byte);
    ///  <summary>Gets directory entered in edPath edit control.</summary>
    ///  <remarks>Edit control contents are trimmed of spaces and any trailing
    ///  path delimiter.</remarks>
    function NewDirFromEditCtrl: string;
    ///  <summary>Handles given exception, converting expected exceptions into
    ///  ECodeSnip and re-raising all other unchanged.</summary>
    ///  <exception>Always raises a new exception.</exception>
    ///  <remarks>This method is designed to handle exceptions raised when the
    ///  database is moved.</remarks>
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
    ///  <exception>Raises EBug if called when running in portable mode.
    ///  </exception>
    class procedure Execute(AOwner: TComponent);
  end;


implementation


uses
  // Delphi
  IOUtils, 
  // Project
  CS.Init.CommandLineOpts,
  UAppInfo,
  UBrowseForFolderDlg,
  UCtrlArranger,
  UExceptions,
  UFontHelper,
  UMessageBox,
  UStrUtils,
  UStructs;

{$R *.dfm}

{ TUserDataPathDlg }

procedure TDBMoveDlg.actBrowseExecute(Sender: TObject);
var
  Dlg: TBrowseForFolderDlg; // browse for folder standard dialogue box
resourcestring
  sDlgTitle = 'Choose Database Directory';
  sDlgHeading = 'Choose an empty directory or create a new one';
begin
  Dlg := TBrowseForFolderDlg.Create(nil);
  try
    Dlg.Title := sDlgTitle;
    Dlg.Headline := sDlgHeading;
    Dlg.MakeFolderBtnVisible := True;
    if Dlg.Execute then
      edPath.Text := Dlg.FolderName;
  finally
    Dlg.Free;
  end;
end;

procedure TDBMoveDlg.actDefaultPathExecute(Sender: TObject);
begin
  DoMove(TAppInfo.DefaultUserDataDir, gbRestore);
end;

procedure TDBMoveDlg.actDefaultPathUpdate(Sender: TObject);
begin
  actDefaultPath.Enabled :=
    not StrSameText(TAppInfo.UserDataDir, TAppInfo.DefaultUserDataDir)
    and Self.Enabled;
end;

procedure TDBMoveDlg.actMoveExecute(Sender: TObject);
begin
  DoMove(NewDirFromEditCtrl, gbMove);
end;

procedure TDBMoveDlg.actMoveUpdate(Sender: TObject);
begin
  actMove.Enabled := (NewDirFromEditCtrl <> '')
    and not StrSameText(NewDirFromEditCtrl, TAppInfo.UserDataDir)
    and Self.Enabled;
end;

procedure TDBMoveDlg.ArrangeForm;
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

procedure TDBMoveDlg.ConfigForm;
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
  frmProgress.Visible := False;
  frmProgress.Range := TRange.Create(0, 100);
end;

procedure TDBMoveDlg.CopyFileHandler(Sender: TObject;
  const Percent: Byte);
resourcestring
  sCopying = 'Copying files...';
begin
  if Percent = 0 then
    frmProgress.Description := sCopying;
  frmProgress.Progress := Percent;
  Application.ProcessMessages;
end;

procedure TDBMoveDlg.DeleteFileHandler(Sender: TObject;
  const Percent: Byte);
resourcestring
  sDeleting = 'Deleting files...';
begin
  if Percent = 0 then
    frmProgress.Description := sDeleting;
  frmProgress.Progress := 100 - Percent;
  Application.ProcessMessages;
end;

procedure TDBMoveDlg.DoMove(const NewDir: string;
  const ProgressHostCtrl: TWinControl);
resourcestring
  sNonEmptyDir = 'The specified directory is not empty.';
  sConfirmMsg = 'Are you sure you want to move the database?';
begin
  if TDirectory.Exists(NewDir)
    and not TDirectory.IsEmpty(NewDir) then
    raise ECodeSnip.Create(sNonEmptyDir);
  if not TMessageBox.Confirm(Self, sConfirmMsg) then
    Exit;
  try
    Enabled := False;
    fControlStateMgr.Update;
    SetVisibility(ProgressHostCtrl, False);
    frmProgress.Show(ProgressHostCtrl);
    try
      fMover.MoveTo(NewDir);
    except
      on E: Exception do
        HandleException(E);
    end;
  finally
    frmProgress.Hide;
    SetVisibility(ProgressHostCtrl, True);
    Enabled := True;
    fControlStateMgr.Update;
  end;
end;

class procedure TDBMoveDlg.Execute(AOwner: TComponent);
begin
  Assert(not TCommandLineOpts.IsPortable,
    ClassName + '.Execute: Call forbidden in portable mode');
  with InternalCreate(AOwner) do
    try
      ShowModal
    finally
      Free;
    end;
end;

procedure TDBMoveDlg.FormCreate(Sender: TObject);
begin
  inherited;
  fMover := TDBMove.Create;
  fMover.OnCopyFile := CopyFileHandler;
  fMover.OnDeleteFile := DeleteFileHandler;
  fControlStateMgr := TControlStateMgr.Create(Self);
end;

procedure TDBMoveDlg.FormDestroy(Sender: TObject);
begin
  fControlStateMgr.Free;
  fMover.Free;
  inherited;
end;

procedure TDBMoveDlg.HandleException(const E: Exception);
begin
  if (E is EInOutError) or (E is ENotSupportedException)
    or (E is EDirectoryNotFoundException) or (E is EPathTooLongException)
    or (E is EArgumentException) then
    raise ECodeSnip.Create(E.Message);
  raise E;
end;

function TDBMoveDlg.NewDirFromEditCtrl: string;
begin
  Result := ExcludeTrailingPathDelimiter(StrTrim(edPath.Text));
end;

procedure TDBMoveDlg.SetVisibility(const ParentCtrl: TWinControl;
  const Show: Boolean);
var
  I: Integer;
begin
  for I := 0 to Pred(ParentCtrl.ControlCount) do
    ParentCtrl.Controls[I].Visible := Show;
end;

end.


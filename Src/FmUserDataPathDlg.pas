{
 * This Source Code Form is subject to the terms of the Mozilla Public License,
 * v. 2.0. If a copy of the MPL was not distributed with this file, You can
 * obtain one at https://mozilla.org/MPL/2.0/
 *
 * Copyright (C) 2013-2023, Peter Johnson (gravatar.com/delphidabbler).
 *
 * Implements a dialogue box that can be used to move collection data to a
 * different directory.
}


unit FmUserDataPathDlg;


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
  FrProgress,
  UBaseObjects,
  UCollectionListAdapter,
  UControlStateMgr,
  UUserDBMove;

type
  ///  <summary>Dialogue box that is used to move the collection data to a new
  ///  directory.</summary>
  ///  <remarks>IMPORTANT: This dialogue box is for use only with the standard
  ///  edition of CodeSnip. It MUST NOT be displayed from the portable edition.
  ///  </remarks>
  TUserDataPathDlg = class(TGenericViewDlg, INoPublicConstruct)
    actBrowse: TAction;
    actMove: TAction;
    alDlg: TActionList;
    lblInstructions: TLabel;
    lblWarning: TLabel;
    frmProgress: TProgressFrame;
    lblPath: TLabel;
    lblExplainMove: TLabel;
    btnMove: TButton;
    edPath: TEdit;
    btnBrowse: TButton;
    lblCollection: TLabel;
    cbCollection: TComboBox;
    ///  <summary>Dispays Browse For Folder dialogue box and copies any chosen
    ///  folder to the edPath edit control.</summary>
    procedure actBrowseExecute(Sender: TObject);
    ///  <summary>Moves the chosen collection to the path entered by the user
    ///  and records the changed path.</summary>
    ///  <exception>Raises exception if the given path can't be used for any
    ///  reason or if there was an error copying the collection.</exception>
    procedure actMoveExecute(Sender: TObject);
    ///  <summary>Enables / disables Move action according to whether a suitable
    ///  path has been entered by user.</summary>
    procedure actMoveUpdate(Sender: TObject);
    ///  <summary>Constructs and initialises form's owned objects.</summary>
    procedure FormCreate(Sender: TObject);
    ///  <summary>Destroys form's owned objects.</summary>
    procedure FormDestroy(Sender: TObject);
  strict private
    var
      ///  <summary>Object that moves a collection's data to a new location.
      ///  </summary>
      fMover: TUserDBMove;
      ///  <summary>Object used to disable and enable all controls on the form.
      ///  </summary>
      fControlStateMgr: TControlStateMgr;
      ///  <summary>Object used to provide and interogate a sorted list of
      ///  collection names displated in <c>cbCollection</c>.</summary>
      fCollList: TCollectionListAdapter;
    ///  <summary>Sets visibility of all child controls of a parent control.
    ///  </summary>
    ///  <param name="ParentCtrl">TWinControl [in] Parent of affected controls.
    ///  </param>
    ///  <param name="Show">Boolean [in] Flag indicating required visibility.
    ///  Pass True to show the controls and False to hide them.</param>
    procedure SetVisibility(const ParentCtrl: TWinControl; const Show: Boolean);
    ///  <summary>Move the chosen collection data to the directory given by
    ///  <c>NewDir</c>, displaying a progress base located over the given host
    ///  window.</summary>
    ///  <remarks>The new directory is checked to be empty and the user is asked
    ///  for confirmation.</remarks>
    procedure DoMove(const NewDir: string; const ProgressHostCtrl: TWinControl);
    ///  <summary>Handles the collection data mover object's <c>OnCopyFile</c>
    ///  event by updating the progress frame.</summary>
    procedure CopyFileHandler(Sender: TObject; const Percent: Byte);
    ///  <summary>Handles the collectio data mover object's <c>OnDeleteFile</c>
    ///  event by updating the progress frame.</summary>
    procedure DeleteFileHandler(Sender: TObject; const Percent: Byte);
    ///  <summary>Gets the directory entered in the <c>edPath</c> edit control.
    ///  </summary>
    ///  <remarks>The edit control contents are trimmed of spaces and any
    ///  trailing path delimiter.</remarks>
    function NewDirFromEditCtrl: string;
    ///  <summary>Handles the given exception <c>E</c>, converting expected
    ///  exceptions into <c>ECodeSnip</c> exceptions and re-raising all other
    ///  exceptions unchanged.</summary>
    ///  <exception>Always raises a new exception.</exception>
    ///  <remarks>This method is designed to handle exceptions raised when the
    ///  collection data is moved.</remarks>
    procedure HandleException(const E: Exception);
  strict protected
    ///  <summary>Initialises the form's controls and associated objects.
    ///  </summary>
    procedure ConfigForm; override;
    ///  <summary>Arranges form's controls and sizes the dialogue box to fit.
    ///  </summary>
    procedure ArrangeForm; override;
  public
    ///  <summary>Displays the dialogue box aligned over the given owner
    ///  control.</summary>
    ///  <exception>Raises <c>EBug</c> if called from the portable edition of
    ///  CodeSnip.</exception>
    class procedure Execute(AOwner: TComponent);
  end;


implementation


uses
  // Delphi
  IOUtils,
  // Project
  DB.UCollections,
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

procedure TUserDataPathDlg.actBrowseExecute(Sender: TObject);
var
  Dlg: TBrowseForFolderDlg; // browse for folder standard dialogue box
resourcestring
  sDlgTitle = 'Choose Collection Data Directory';
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

procedure TUserDataPathDlg.actMoveExecute(Sender: TObject);
begin
  DoMove(NewDirFromEditCtrl, Self);
end;

procedure TUserDataPathDlg.actMoveUpdate(Sender: TObject);
begin
  actMove.Enabled := (NewDirFromEditCtrl <> '')
    and not StrSameText(
      NewDirFromEditCtrl,
      fCollList.Collection(cbCollection.ItemIndex).Storage.Directory
    )
    and Self.Enabled;
end;

procedure TUserDataPathDlg.ArrangeForm;
begin
  TCtrlArranger.SetLabelHeights(Self);

  pnlBody.ClientWidth := TCtrlArranger.TotalControlWidth(pnlBody);

  TCtrlArranger.AlignLefts(
    [
      lblInstructions, lblWarning, lblCollection, cbCollection, lblPath, edPath,
      lblExplainMove
    ],
    0
  );
  // Row 1
  lblInstructions.Top := 0;
  lblInstructions.Width := pnlBody.ClientWidth;
  // Row 2
  TCtrlArranger.MoveBelow(lblInstructions, lblWarning, 8);
  lblWarning.Width := pnlBody.ClientWidth;
  // Row 3
  TCtrlArranger.MoveBelow(lblWarning, lblCollection, 12);
  // Row 4
  TCtrlArranger.MoveBelow(lblCollection, cbCollection, 6);
  cbCollection.Width := pnlBody.ClientWidth;
  // Row 5
  TCtrlArranger.MoveBelow(cbCollection, lblPath, 12);
  // Row 6
  TCtrlArranger.AlignRights([btnBrowse], pnlBody.ClientWidth);
  edPath.Width := btnBrowse.Left - 6 - edPath.Left;
  TCtrlArranger.AlignVCentres(
    TCtrlArranger.BottomOf(lblPath, 6), [edPath, btnBrowse]
  );
  // Row 7
  TCtrlArranger.MoveBelow([edPath, btnBrowse], lblExplainMove, 12);
  lblExplainMove.Width := pnlBody.ClientWidth;
  // Row 8
  TCtrlArranger.MoveBelow(lblExplainMove, btnMove, 12);
  btnMove.Left := (pnlBody.Width - btnMove.Width) div 2;

  pnlBody.ClientHeight := TCtrlArranger.TotalControlHeight(pnlBody) + 8;

  inherited;
end;

procedure TUserDataPathDlg.ConfigForm;
begin
  inherited;
  TFontHelper.SetDefaultBaseFonts([lblWarning.Font]);
  TFontHelper.SetDefaultFonts([
    lblPath.Font, edPath.Font, lblExplainMove.Font,
    btnBrowse.Font, btnMove.Font
  ]);
  frmProgress.Visible := False;
  frmProgress.Range := TRange.Create(0, 100);
  fCollList.ToStrings(cbCollection.Items);
  cbCollection.ItemIndex := fCollList.IndexOfUID(TCollectionID.Default);
end;

procedure TUserDataPathDlg.CopyFileHandler(Sender: TObject;
  const Percent: Byte);
resourcestring
  sCopying = 'Copying files...';
begin
  if Percent = 0 then
    frmProgress.Description := sCopying;
  frmProgress.Progress := Percent;
  Application.ProcessMessages;
end;

procedure TUserDataPathDlg.DeleteFileHandler(Sender: TObject;
  const Percent: Byte);
resourcestring
  sDeleting = 'Deleting files...';
begin
  if Percent = 0 then
    frmProgress.Description := sDeleting;
  frmProgress.Progress := 100 - Percent;
  Application.ProcessMessages;
end;

procedure TUserDataPathDlg.DoMove(const NewDir: string;
  const ProgressHostCtrl: TWinControl);
resourcestring
  sNonEmptyDir = 'The specified directory is not empty.';
  sConfirmMsg = 'Are you sure you want to move the collection data?';
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
      fMover.MoveTo(
        fCollList.Collection(cbCollection.ItemIndex), NewDir
      );
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

class procedure TUserDataPathDlg.Execute(AOwner: TComponent);
{$IFNDEF PORTABLE}
var
  Dlg: TUserDataPathDlg;
{$ENDIF}
begin
  {$IFDEF PORTABLE}
  {TODO -cVault: Permit this, but restrict to sub-dirs of install dir}
  raise EBug.Create(ClassName + '.Execute: Call forbidden in portable edition');
  {$ELSE}
  Dlg := InternalCreate(AOwner);
  try
    Dlg.ShowModal
  finally
    Dlg.Free;
  end;
  {$ENDIF}
end;

procedure TUserDataPathDlg.FormCreate(Sender: TObject);
begin
  inherited;
  fMover := TUserDBMove.Create;
  fMover.OnCopyFile := CopyFileHandler;
  fMover.OnDeleteFile := DeleteFileHandler;
  fControlStateMgr := TControlStateMgr.Create(Self);
  fCollList := TCollectionListAdapter.Create;
end;

procedure TUserDataPathDlg.FormDestroy(Sender: TObject);
begin
  fCollList.Free;
  fControlStateMgr.Free;
  fMover.Free;
  inherited;
end;

procedure TUserDataPathDlg.HandleException(const E: Exception);
begin
  if (E is EInOutError) or (E is ENotSupportedException)
    or (E is EDirectoryNotFoundException) or (E is EPathTooLongException)
    or (E is EArgumentException) then
    raise ECodeSnip.Create(E.Message);
  raise E;
end;

function TUserDataPathDlg.NewDirFromEditCtrl: string;
begin
  Result := ExcludeTrailingPathDelimiter(StrTrim(edPath.Text));
end;

procedure TUserDataPathDlg.SetVisibility(const ParentCtrl: TWinControl;
  const Show: Boolean);
var
  I: Integer;
begin
  for I := 0 to Pred(ParentCtrl.ControlCount) do
    ParentCtrl.Controls[I].Visible := Show;
end;

end.


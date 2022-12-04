{
 * This Source Code Form is subject to the terms of the Mozilla Public License,
 * v. 2.0. If a copy of the MPL was not distributed with this file, You can
 * obtain one at https://mozilla.org/MPL/2.0/
 *
 * Copyright (C) 2005-2021, Peter Johnson (gravatar.com/delphidabbler).
 *
 * Implements a wizard dialogue box that handles the updating of the main
 * DelphiDabbler Code Snippets database.
}


unit FmDBUpdateDlg;


interface


uses
  // Project
  SysUtils,
  Windows,
  Classes,
  ActnList,
  StdCtrls,
  Forms,
  ComCtrls,
  Controls,
  ExtCtrls,
  System.Actions,
  // VCL
  FmWizardDlg,
  FrBrowserBase,
  FrFixedHTMLDlg,
  FrHTMLDlg,
  FrHTMLTpltDlg,
  FrProgress,
  UBaseObjects,
  UCodeImportMgr,
  UCSSBuilder,
  UControlStateMgr,
  UDBUpdateMgr;

type
  ///  <summary>Wizard dialogue box that handles the updating of the main
  ///  DelphiDabbler Code Snippets database from disk.</summary>
  TDBUpdateDlg = class(TWizardDlg, INoPublicConstruct)
    tsIntro: TTabSheet;
    tsFolder: TTabSheet;
    frmIntro: THTMLTpltDlgFrame;
    lblFolder: TLabel;
    edPath: TEdit;
    lblFolderPageInfo: TLabel;
    btnBrowse: TButton;
    alMain: TActionList;
    actBrowse: TAction;
    tsLoad: TTabSheet;
    tsFinish: TTabSheet;
    frmLoad: TFixedHTMLDlgFrame;
    frmProgress: TProgressFrame;
    frmFinish: TFixedHTMLDlgFrame;
    ///  <summary>Displays standard Browse For Folder dialogue box to get
    ///  directory containing updated database files.</summary>
    procedure actBrowseExecute(Sender: TObject);
    ///  <summary>Constructs owned object.</summary>
    procedure FormCreate(Sender: TObject);
    ///  <summary>Destroys owned objects.</summary>
    procedure FormDestroy(Sender: TObject);
  strict private
    const
      // Indices of wizard pages
      cIntroPage = 0;
      cSelectFolderPage = 1;
      cLoadDatabasePage = 2;
      cFinishPage = 3;
    var
      ///  <summary>Flag that indicates if local database was updated.</summary>
      fDataUpdated: Boolean;
      ///  <summary>Object used to disable and enable all controls on the form.
      ///  </summary>
      fControlStateMgr: TControlStateMgr;

    ///  <summary>Handles HTML template frame's OnBuildCSS event. Adds
    ///  additional CSS required by HTML in this form.</summary>
    ///  <param name="Sender">TObject [in] Reference to object triggering event.
    ///  </param>
    ///  <param name="CSSBuilder">TCSSBuilder [in] Object used to construct the
    ///  CSS.</param>
    procedure BuildCSS(Sender: TObject; const CSSBuilder: TCSSBuilder);

    ///  <summary>Validates entries on wizard pages indetified by the page
    ///  index.</summary>
    procedure ValidatePage(const PageIdx: Integer);

    ///  <summary>Retrieves import directory name from edit control where it is
    ///  entered.</summary>
    function GetDirNameFromEditCtrl: string;

    ///  <summary>Handles OnFileUpdateProgress event of TDBUpdateMgr by updating
    ///  progress display.</summary>
    procedure CopyProgress(Sender: TObject; const Percentage: Single);

    ///  <summary>Performs database update.</summary>
    procedure DoUpdate;

    ///  <summary>Handles given exception, converting expected exceptions into
    ///  ECodeSnip and re-raising all other unchanged.</summary>
    ///  <exception>Always raises a new exception.</exception>
    ///  <remarks>This method is designed to handle exceptions raised when the
    ///  main database is updated.</remarks>
    procedure HandleException(const E: Exception);

  strict protected

    ///  <summary>Initialises wizard pages that display HTML content.</summary>
    ///  <remarks>Overridden method called from ancestor class.</remarks>
    procedure CustomiseControls; override;

    ///  <summary>Aligns and arranges controls in each tab sheet and sizes
    ///  dialog box to accomodate controls.</summary>
    ///  <remarks>Overridden method called from ancestor class.</remarks>
    procedure ArrangeControls; override;

    ///  <summary>Protected constructor that sets up form.</summary>
    constructor InternalCreate(AOwner: TComponent); override;

    ///  <summary>Returns text of heading on page indexed by PageIdx.</summary>
    ///  <remarks>Overridden method called from ancestor class.</remarks>
    function HeadingText(const PageIdx: Integer): string; override;

    ///  <summary>Validates a specified page then performs any action required
    ///   before next page is displayed.</summary>
    ///  <param name="PageIdx">Integer [in] Index of page to be checked before
    ///  moving to next page.</param>
    ///  <param name="CanMove">Boolean [in/out] Set True to permit next page to
    ///  be displayed or False to inhibit this. Defaults to True.</param>
    ///  <remarks>Overridden method called from ancestor class.</remarks>
    procedure MoveForward(const PageIdx: Integer; var CanMove: Boolean);
      override;

    ///  <summary>Updates state and caption of buttons on page index by
    ///  PageIdx.</summary>
    ///  <remarks>Implementation of abstract method called from ancestor class.
    ///  </remarks>
    procedure UpdateButtons(const PageIdx: Integer); override;

  public

    ///  <summary>Displays the dialogue box and performs any required database
    ///  update.</summary>
    ///  <param name="AOwner">TComponent [in] Component that owns the dialogue
    ///  box. If the component has an associated window the dialogue box is
    ///  aligned to it. May be nil.</param>
    ///  <returns>Boolean. True if the local database was updated or False if no
    ///  update was performed for any reason (i.e. cancelling, loca database is
    ///  up to date or an error occurred.</returns>
    class function Execute(AOwner: TComponent): Boolean;

  end;


implementation

uses
  // VCL
  IOUtils,
  Math,
  // Project
  UAppInfo,
  UBrowseForFolderDlg,
  UColours,
  UCtrlArranger,
  UCSSUtils,
  UExceptions,
  UHTMLTemplate,
  UMessageBox,
  UStructs,
  UStrUtils,
  UUrl;

{$R *.dfm}

{ TGetDDabSnippetsDlg }

procedure TDBUpdateDlg.actBrowseExecute(Sender: TObject);
var
  Dlg: TBrowseForFolderDlg; // browse for folder standard dialogue box
resourcestring
  sDlgTitle = 'Choose Database Download Directory';
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

procedure TDBUpdateDlg.ArrangeControls;
var
  HTMLFrameHeight: Integer;
  HTMLFrameClientWidth: Integer;
begin
  TCtrlArranger.SetLabelHeights(Self);

  // Arrange controls on tab sheets

  HTMLFrameHeight := MaxIntValue([
    frmIntro.DocHeight, frmLoad.DocHeight, frmFinish.DocHeight
  ]);
  HTMLFrameClientWidth := frmIntro.ClientWidth;

  // tsInfo
  frmIntro.Height := HTMLFrameHeight;
  frmIntro.Top := 0;
  frmIntro.Left := 0;

  // tsFolder
  TCtrlArranger.AlignVCentres(
    TCtrlArranger.BottomOf(lblFolder, 6), [edPath, btnBrowse]
  );
  lblFolderPageInfo.Top := TCtrlArranger.BottomOf([edPath, btnBrowse], 12);

  // tsLoad
  frmLoad.Height := HTMLFrameHeight;
  frmLoad.ClientWidth := HTMLFrameClientWidth;
  frmLoad.Top := 0;
  frmLoad.Left := 0;

  // tsFinish
  frmFinish.Height := HTMLFrameHeight;
  frmFinish.ClientWidth := HTMLFrameClientWidth;
  frmFinish.Top := 0;
  frmFinish.Left := 0;

  // Size body
  pnlBody.ClientWidth := TCtrlArranger.MaxContainerWidth(
    [tsIntro, tsFolder, tsLoad, tsFinish]
  ) + pnlBody.ClientWidth - tsIntro.Width;
  pnlBody.ClientHeight := TCtrlArranger.MaxContainerHeight(
    [tsIntro, tsFolder, tsLoad, tsFinish]
  ) + pnlBody.ClientHeight - tsIntro.Height;

  // Arrange inherited controls and size the form
  inherited;
end;

procedure TDBUpdateDlg.BuildCSS(Sender: TObject;
    const CSSBuilder: TCSSBuilder);
begin
  inherited;
  // Create .framed border style
  with CSSBuilder.AddSelector('.framed') do
  begin
    AddProperty(TCSS.BorderProp(cssAll, 1, cbsSolid, clBorder));
    AddProperty(TCSS.PaddingProp(4));
  end;
end;

procedure TDBUpdateDlg.CopyProgress(Sender: TObject;
  const Percentage: Single);
begin
  frmProgress.Progress := Round(Percentage);
  Application.ProcessMessages;
end;

procedure TDBUpdateDlg.CustomiseControls;
resourcestring
  sProgressFrameDesc = 'Copying files...';
begin
  // Initialise Intro HTML frame
  pcWizard.ActivePage := tsIntro;
  frmIntro.OnBuildCSS := BuildCSS;
  frmIntro.Initialise(
    'dlg-dbupdate-intro-tplt.html',
    procedure (Tplt: THTMLTemplate)
    begin
      Tplt.ResolvePlaceholderText(
        'CSDBReleaseURL',
        TURL.CodeSnippetsDBReleases
      );
    end
  );

  // Initialise Load HTML frame
  pcWizard.ActivePage := tsLoad;
  frmLoad.OnBuildCSS := BuildCSS;
  frmLoad.Initialise('dlg-dbupdate-load.html');

  // Initialise Finish HTML frame
  pcWizard.ActivePage := tsFinish;
  frmFinish.OnBuildCSS := BuildCSS;
  frmFinish.Initialise('dlg-dbupdate-finish.html');

  // Initialise progress display frame
  frmProgress.Visible := False;
  frmProgress.Range := TRange.Create(0, 100);
  frmProgress.Description := sProgressFrameDesc;
end;

procedure TDBUpdateDlg.DoUpdate;

  procedure PrepareUIForUpdate;
  begin
    Enabled := False;
    fControlStateMgr.Update;
    frmLoad.Visible := False;
    frmProgress.Show(pnlBody);
  end;

  procedure RestoreUI;
  begin
    frmProgress.Hide;
    frmLoad.Visible := True;
    Enabled := True;
    fControlStateMgr.Update;
  end;

var
  UpdateMgr: TDBUpdateMgr;
begin
  fDataUpdated := False;
  PrepareUIForUpdate;
  try
    UpdateMgr := TDBUpdateMgr.Create(
      TAppInfo.AppDataDir, GetDirNameFromEditCtrl
    );
    UpdateMgr.OnFileUpdateProgress := CopyProgress;
    try
      try
        UpdateMgr.Execute;
        fDataUpdated := True;
      finally
        UpdateMgr.Free;
      end;
    except
      on E: Exception do
        HandleException(E);
    end;
  finally
    RestoreUI;
  end;
end;

class function TDBUpdateDlg.Execute(AOwner: TComponent): Boolean;
begin
  with InternalCreate(AOwner) do
    try
      ShowModal;
      Result := fDataUpdated;
    finally
      Free;
    end;
end;

procedure TDBUpdateDlg.FormCreate(Sender: TObject);
begin
  inherited;
  fControlStateMgr := TControlStateMgr.Create(Self);
end;

procedure TDBUpdateDlg.FormDestroy(Sender: TObject);
begin
  fControlStateMgr.Free;
  inherited;
end;

function TDBUpdateDlg.GetDirNameFromEditCtrl: string;
begin
  Result := StrTrim(edPath.Text);
end;

procedure TDBUpdateDlg.HandleException(const E: Exception);
begin
  if (E is EInOutError)
    or (E is ENotSupportedException)
    or (E is EDirectoryNotFoundException)
    or (E is EPathTooLongException)
    or (E is EArgumentException)
    or (E is EDBUpdateValidationError) then
    raise ECodeSnip.Create(E.Message);
  raise E;
end;

function TDBUpdateDlg.HeadingText(const PageIdx: Integer): string;
resourcestring
  sIntroHeading = 'Download the database';
  sSelectFolderHeading = 'Select database download folder';
  sLoadDataseHeading = 'Install the database';
  sFinishHeading = 'Database installed';
begin
  case PageIdx of
    cIntroPage: Result := sIntroHeading;
    cSelectFolderPage: Result := sSelectFolderHeading;
    cLoadDatabasePage: Result := sLoadDataseHeading;
    cFinishPage: Result := sFinishHeading;
  end;
end;

constructor TDBUpdateDlg.InternalCreate(AOwner: TComponent);
begin
  Assert(Supports(Self, INoPublicConstruct), ClassName + '.InternalCreate: '
    + 'Form''s protected constructor can''t be called');
  inherited InternalCreate(AOwner);
end;

procedure TDBUpdateDlg.MoveForward(const PageIdx: Integer;
  var CanMove: Boolean);
begin
  // NOTE: Will never be called if PageIdx is last page.
  CanMove := False;
  try
    ValidatePage(PageIdx);
    case PageIdx of
      cLoadDatabasePage: DoUpdate;
    end;
    CanMove := True;
  except
    on E: EDataEntry do
    begin
      TMessageBox.Error(Self, E.Message);
      if Assigned(E.Ctrl) then
        E.Ctrl.SetFocus;
    end;
    on E: ECodeImportMgr do
      TMessageBox.Error(Self, E.Message);
  end;
end;

procedure TDBUpdateDlg.UpdateButtons(const PageIdx: Integer);
resourcestring
  sLoadPageBtn = '&Load';
begin
  inherited;
  case PageIdx of
    cLoadDatabasePage: btnNext.Caption := sLoadPageBtn;
    cFinishPage: btnBack.Enabled := False;
  end;
end;

procedure TDBUpdateDlg.ValidatePage(const PageIdx: Integer);

  procedure ValidateSelectFolderPage;
  resourcestring
    sNoDirNameError = 'No update directory specified. Please enter one.';
  begin
    if GetDirNameFromEditCtrl = '' then
      raise EDataEntry.Create(sNoDirNameError, edPath);
    try
      TDBUpdateMgr.ValidateUpdate(GetDirNameFromEditCtrl);
    except
      on E: EDBUpdateValidationError do
        raise EDataEntry.Create(E.Message, edPath);
      else
        raise;
    end;
  end;

begin
  case PageIdx of
    cSelectFolderPage: ValidateSelectFolderPage;
  end;
end;

end.

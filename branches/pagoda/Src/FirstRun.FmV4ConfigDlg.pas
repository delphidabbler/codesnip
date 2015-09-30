{
 * This Source Code Form is subject to the terms of the Mozilla Public License,
 * v. 2.0. If a copy of the MPL was not distributed with this file, You can
 * obtain one at http://mozilla.org/MPL/2.0/
 *
 * Copyright (C) 2012-2014, Peter Johnson (www.delphidabbler.com).
 *
 * $Rev$
 * $Date$
 *
 * Implements a wizard dialogue box that may be displayed on the first run of
 * CodeSnip v5 to offer the user a choice whether to bring forward preferences
 * and/or a snippets database.
}


unit FirstRun.FmV4ConfigDlg;

interface

uses
  // Delphi
  StdCtrls,
  ComCtrls,
  Controls,
  ExtCtrls,
  Classes,
  Forms,
  // Project
  FmWizardDlg,
  FirstRun.UMain,
  UBaseObjects,
  IntfAligner,
  UIStringList;

type
  ///  <summary>Wizard dialogue box for display on first run of program if there
  ///  is a need to offer user a choice whether to bring forward preferences
  ///  and/or a snippets database.</summary>
  TV4ConfigDlg = class(TWizardDlg, INoPublicConstruct)
    chkCopyConfig: TCheckBox;
    chkCopyDB: TCheckBox;
    lblCopyConfig: TLabel;
    lblIntro1: TLabel;
    lblIntro2: TLabel;
    lblIntro4: TLabel;
    lblIntro5: TLabel;
    lblIntro3: TLabel;
    lblSummaryPrefix: TLabel;
    lblSummaryPostfix1: TLabel;
    lblSummaryPostfix2: TLabel;
    lblDatabase1: TLabel;
    lblDatabase2: TLabel;
    tsConfigFile: TTabSheet;
    tsFinish: TTabSheet;
    tsIntro: TTabSheet;
    tsSummary: TTabSheet;
    tsDatabase: TTabSheet;
    sbFinish: TScrollBox;
    lblFinish1: TLabel;
    lblFinish2: TLabel;
    lblFinish3: TLabel;
    ///  <summary>Determines if form can close.</summary>
    ///  <remarks>Permits closure only if wizard has been completed.</remarks>
    procedure FormCloseQuery(Sender: TObject; var CanClose: Boolean);
  strict private
    const
      ///  <summary>Index of introductory page.</summary>
      IntroPageIdx = 0;
      ///  <summary>Index of page dealing with bringing forward user
      ///  preferences.</summary>
      ConfigPageIdx = 1;
      ///  <summary>Index of page dealing with bringing forward an earlier
      ///  snippets database.</summary>
      DatabasePageIdx = 2;
      ///  <summary>Index of page that summarises actions to be taken.</summary>
      SummaryPageIdx = 3;
      ///  <summary>Index of last page.</summary>
      FinishPageIdx = 4;
    type
      ///  <summary>Set of actions to be taken as a result of user input.
      ///  </summary>
      TUpdateActions = set of (uaCopyCfgFile, uaCopyDB);
  strict private
    var
      ///  <summary>Object that provides info about user config file and
      ///  database and performs required actions.</summary>
      fFirstRun: TFirstRun;
      ///  <summary>Set of changes made to brought forward config files that
      ///  result in data loss.</summary>
      fCfgChanges: TFirstRunCfgChangeSet;
    ///  <summary>Gets set of actions to be taken from user input.</summary>
    function GetUpdateActions: TUpdateActions;
    ///  <summary>Checks if an old user config file is available for copying.
    ///  </summary>
    function ConfigFileAvailable: Boolean;
    ///  <summary>Checks if an old snippets database is available for copying.
    ///  </summary>
    function DatabaseAvailable: Boolean;
    ///  <summary>Creates a bullet list on a tab sheet.</summary>
    ///  <param name="ParentCtrl">TWinControl [in] Control where bullet list is
    ///  to be parented.</param>
    ///  <param name="Prefix">array of TLabel [in] List of labels to be
    ///  positioned before bullet list.</param>
    ///  <param name="BulletPoints">IStringList [in] List of bullet item text.
    ///  </param>
    ///  <param name="PostFix">array of TLabel [in] List of labels to be
    ///  positioned after bullet list.</param>
    procedure CreateBulletPage(ParentCtrl: TWinControl;
      const Prefix: array of TLabel; BulletPoints: IStringList;
      const PostFix: array of TLabel);
    ///  <summary>Gets description of choices made and displays them in a bullet
    ///  list on summary page.</summary>
    procedure UpdateChoices;
    ///  <summary>Performs any config file and database updates requested by
    ///  user.</summary>
    procedure UpdateData;
    ///  <summary>Displays message on final page confirming that changes have
    ///  been applied. Also displays a bullet list of any changes that resulted
    ///  in data loss.</summary>
    procedure ListChanges;
  strict private
    type
      ///  <summary>Custom form aligner class for wizard.</summary>
      TAligner = class(TInterfacedObject, IFormAligner)
      public
        ///  <summary>Aligns wizard at centre of primary monitor.</summary>
        procedure AlignForm(const AForm: TCustomForm);
      end;
  strict protected
    ///  <summary>Returns instance of form aligner object.</summary>
    function GetAligner: IFormAligner; override;
    ///  <summary>Arranges controls within each tab sheet.</summary>
    procedure ArrangeForm; override;
    ///  <summary>Sets up wizard ready for display.</summary>
    procedure ConfigForm; override;
    ///  <summary>Returns heading text of given wizard page.</summary>
    function HeadingText(const PageIdx: Integer): string; override;
    ///  <summary>Returns index of page following given page index in wizard.
    ///  </summary>
    ///  <remarks>Preferences or database pages are skipped if not relevant.
    ///  </remarks>
    function NextPage(const PageIdx: Integer): Integer; override;
    ///  <summary>Returns index of page before given page index in wizard.
    ///  </summary>
    ///  <remarks>Preferences or database pages are skipped if not relevant.
    ///  </remarks>
    function PrevPage(const PageIdx: Integer): Integer; override;
    ///  <summary>Initialises wizard page with given index in cases where page
    ///  content depends on user input in other pages.</summary>
    ///  <remarks>Used only for summary page to display a summary of choices
    ///  user made on previous pages.</remarks>
    procedure BeginPage(const PageIdx: Integer); override;
    ///  <summary>Finalises the page with the given index before moving to next
    ///  page. Page is prevented from changing if CanMove is set to False.
    ///  </summary>
    ///  <remarks>Used only to perform any config file and database updates when
    ///  leaving summary page.</remarks>
    procedure MoveForward(const PageIdx: Integer;
      var CanMove: Boolean); override;
    ///  <summary>Updates state and caption of wizard's buttons as displayed for
    ///  page with given index.</summary>
    procedure UpdateButtons(const PageIdx: Integer); override;
  public
    ///  <summary>Displays wizard with given owner. Wizard uses given FirstRun
    ///  object to get info about user config file and snippets database and
    ///  performs any required actions.</summary>
    class procedure Execute(AOwner: TComponent; const FirstRun: TFirstRun);
  end;


implementation


uses
  // Delphi
  Windows,
  // Project
  UConsts,
  UCtrlArranger,
  UMessageBox,
  UStructs;


{$R *.dfm}

{ TFirstRunDlg }

procedure TV4ConfigDlg.ArrangeForm;
begin
  TCtrlArranger.SetLabelHeights(Self);

  // tsIntro
  TCtrlArranger.AlignLefts([lblIntro1, lblIntro2, lblIntro3], 0);
  lblIntro1.Top := 4;
  TCtrlArranger.MoveBelow(lblIntro1, lblIntro2, 6);
  TCtrlArranger.MoveBelow(lblIntro2, lblIntro3, 6);
  TCtrlArranger.MoveBelow(lblIntro3, lblIntro4, 6);
  TCtrlArranger.MoveBelow(lblIntro4, lblIntro5, 6);

  // tsConfigFile
  TCtrlArranger.AlignLefts([lblCopyConfig, chkCopyConfig], 0);
  lblCopyConfig.Top := 4;
  TCtrlArranger.MoveBelow(lblCopyConfig, chkCopyConfig, 12);

  // tsDatabase
  TCtrlArranger.AlignLefts([lblDatabase1, lblDatabase2, chkCopyDB], 0);
  lblDatabase1.Top := 4;
  TCtrlArranger.MoveBelow(lblDatabase1, chkCopyDB, 12);
  TCtrlArranger.MoveBelow(chkCopyDB, lblDatabase2, 12);

  // tsSummary & tsFinish are arranged on the fly when displayed
  inherited;
end;

procedure TV4ConfigDlg.BeginPage(const PageIdx: Integer);
begin
  if PageIdx = SummaryPageIdx then
    UpdateChoices;
end;

function TV4ConfigDlg.ConfigFileAvailable: Boolean;
begin
  Result := fFirstRun.HaveOldUserCfgFile;
end;

procedure TV4ConfigDlg.ConfigForm;
begin
  inherited;
  pcWizard.ActivePage := tsIntro;
  fCfgChanges := [];
end;

procedure TV4ConfigDlg.CreateBulletPage(ParentCtrl: TWinControl;
  const Prefix: array of TLabel; BulletPoints: IStringList;
  const PostFix: array of TLabel);
var
  PrefixLbl: TLabel;
  PostfixLbl: TLabel;
  BulletTextLbl: TLabel;
  BulletLbl: TLabel;
  NextTop: Integer;
  BulletPointText: string;
const
  Spacing = 6;
  Bullet: Char = #$2022;

  // Frees any dynamically created labels
  procedure FreeDynLabels;
  var
    Idx: Integer;
  begin
    for Idx := Pred(ParentCtrl.ControlCount) downto 0 do
    begin
      if (ParentCtrl.Controls[Idx] is TLabel)
        and (ParentCtrl.Controls[Idx].Name = '') then
        ParentCtrl.Controls[Idx].Free;
    end;
  end;

  function CreateBulletLbl(const ATop: Integer): TLabel;
  begin
    Result := TLabel.Create(Self);
    Result.Parent := ParentCtrl;
    Result.Left := 10;
    Result.Top := ATop;
    Result.Caption := Bullet;
  end;

  function CreateBulletTextLbl(const ATop: Integer; const AText: string):
    TLabel;
  begin
    Result := TLabel.Create(Self);
    Result.Parent := ParentCtrl;
    Result.Left := 20;
    Result.WordWrap := True;
    Result.AutoSize := True;
    Result.Width := ParentCtrl.ClientWidth - 22
      - GetSystemMetrics(SM_CXVSCROLL);
    Result.Top := ATop;
    Result.Caption := AText;
    Result.ShowAccelChar := False;
  end;

begin
  FreeDynLabels;
  NextTop := 4;
  for PrefixLbl in Prefix do
  begin
    PrefixLbl.Top := NextTop;
    PrefixLbl.Left := 0;
    PrefixLbl.WordWrap := True;
    PrefixLbl.AutoSize := True;
    PrefixLbl.Width := ParentCtrl.ClientWidth - 2
      - GetSystemMetrics(SM_CXVSCROLL);
    NextTop := TCtrlArranger.BottomOf(PrefixLbl, Spacing);
  end;
  for BulletPointText in BulletPoints do
  begin
    BulletLbl := CreateBulletLbl(NextTop);
    BulletTextLbl := CreateBulletTextLbl(NextTop, BulletPointText);
    NextTop := TCtrlArranger.BottomOf([BulletLbl, BulletTextLbl], Spacing);
  end;
  for PostfixLbl in Postfix do
  begin
    PostfixLbl.Top := NextTop;
    PostfixLbl.Left := 0;
    PostfixLbl.WordWrap := True;
    PostfixLbl.AutoSize := True;
    PostfixLbl.Width := ParentCtrl.ClientWidth - 2
      - GetSystemMetrics(SM_CXVSCROLL);
    NextTop := TCtrlArranger.BottomOf(PostfixLbl, Spacing);
  end;
end;

function TV4ConfigDlg.DatabaseAvailable: Boolean;
begin
  Result := fFirstRun.HaveOldDB;
end;

class procedure TV4ConfigDlg.Execute(AOwner: TComponent;
  const FirstRun: TFirstRun);
begin
  with InternalCreate(AOwner) do
    try
      fFirstRun := FirstRun;
      ShowModal;
    finally
      Free;
    end;
end;

procedure TV4ConfigDlg.FormCloseQuery(Sender: TObject; var CanClose: Boolean);
resourcestring
  sCantCloseMsg = 'Sorry, you can''t close this wizard prematurely.'
    + EOL2
    + 'Please continue until the "Finished" page is reached.';
begin
  inherited;
  CanClose := CurrentPage = LastPage;
  if not CanClose then
    TMessageBox.Error(Self, sCantCloseMsg);
end;

function TV4ConfigDlg.GetAligner: IFormAligner;
begin
  Result := TAligner.Create;
end;

function TV4ConfigDlg.GetUpdateActions: TUpdateActions;
begin
  Result := [];
  if ConfigFileAvailable and chkCopyConfig.Checked then
    Include(Result, uaCopyCfgFile);
  if DatabaseAvailable and chkCopyDB.Checked then
    Include(Result, uaCopyDB);
end;

function TV4ConfigDlg.HeadingText(const PageIdx: Integer): string;
resourcestring
  sIntroHeading   = 'New version of CodeSnip';
  sConfigHeading  = 'Preferences';
  sDBHeading      = 'Snippets database';
  sSummaryHeading = 'Summary';
  sFinishHeading  = 'Finished';
begin
  Result := sIntroHeading;
  case PageIdx of
    IntroPageIdx:
      Result := sIntroHeading;
    ConfigPageIdx:
      Result := sConfigHeading;
    DatabasePageIdx:
      Result := sDBHeading;
    SummaryPageIdx:
      Result := sSummaryHeading;
    FinishPageIdx:
      Result := sFinishHeading;
    else
      Result := '';
  end;
end;

procedure TV4ConfigDlg.ListChanges;
resourcestring
  sHiliter = 'Syntax highlighter customisations have been lost.';
  sProxyPwd = 'Your proxy server password needs to be re-entered.';
  sSourceFormat = 'Source code formatting preferences may have been lost.';
  sCustomDBPath = 'The custom database path used by CodeSnip 4 is being '
    + 'ignored. CodeSnip 5 will use its default path. You can move the '
    + 'database to your desired location later by using the program''s '
    + 'Database | Move Database menu option.';
  sPageStructure = 'Any detail pane layout customisations you may have created '
    + 'have been lost. You can re-create them by using the program''s Tools | '
    + 'Preferences menu option then selecting the Snippet Layout tab of the '
    + 'resulting dialogue box';
var
  Changes: IStringList;
begin
  if fCfgChanges <> [] then
  begin
    // there are changes to config file: show in bullet list
    lblFinish2.Visible := True;
    Changes := TIStringList.Create;
    if frcHiliter in fCfgChanges then
      Changes.Add(sHiliter);
    if frcProxyPwd in fCfgChanges then
      Changes.Add(sProxyPwd);
    if frcSourceFormat in fCfgChanges then
      Changes.Add(sSourceFormat);
    if frcCustomDBPath in fCfgChanges then
      Changes.Add(sCustomDBPath);
    if frcPageStructure in fCfgChanges then
      Changes.Add(sPageStructure);
    CreateBulletPage(
      sbFinish,
      [lblFinish1, lblFinish2],
      Changes,
      [lblFinish3]
    );
  end
  else
  begin
    // no changes to config file: just display "finished" message
    lblFinish2.Visible := False;
    TCtrlArranger.AlignLefts([lblFinish1, lblFinish3], 0);
    lblFinish1.Top := 4;
    TCtrlArranger.MoveBelow(lblFinish1, lblFinish3, 12);
  end;

end;

procedure TV4ConfigDlg.MoveForward(const PageIdx: Integer;
  var CanMove: Boolean);
begin
  if PageIdx = SummaryPageIdx then
  begin
    UpdateData;
    ListChanges;
  end;
end;

function TV4ConfigDlg.NextPage(const PageIdx: Integer): Integer;
begin
  Result := inherited NextPage(PageIdx);
  // Don't display related pages if no config file or no snippets database
  if (Result = ConfigPageIdx) and not ConfigFileAvailable then
    Exit(NextPage(Result));
  if (Result = DatabasePageIdx) and not DatabaseAvailable then
    Exit(NextPage(Result));
end;

function TV4ConfigDlg.PrevPage(const PageIdx: Integer): Integer;
begin
  Result := inherited PrevPage(PageIdx);
  // Don't display related pages if no config file or no snippets database
  if (Result = DatabasePageIdx) and not DatabaseAvailable then
    Exit(PrevPage(Result));
  if (Result = ConfigPageIdx) and not ConfigFileAvailable then
    Exit(PrevPage(Result));
end;

procedure TV4ConfigDlg.UpdateButtons(const PageIdx: Integer);
resourcestring
  sUpdate = '&Confirm';
begin
  inherited;
  btnCancel.Enabled := False;
  btnCancel.Visible := False;
  if PageIdx = SummaryPageIdx then
    btnNext.Caption := sUpdate;
  if PageIdx = FinishPageIdx then
    btnBack.Enabled := False;
end;

procedure TV4ConfigDlg.UpdateChoices;
resourcestring
  sBFConfigYes = 'Bring forward preferences from earlier version';
  sBFConfigNo = 'Use default settings, ignoring earlier preferences';
  sCopyDBYes = 'Copy snippets database from earlier version';
  sCopyDBNo = 'Start program with an empty snippets database';
var
  Actions: TUpdateActions;
  Bullets: IStringList;
begin
  Actions := GetUpdateActions;
  Bullets := TIStringList.Create;
  if ConfigFileAvailable then
  begin
    if uaCopyCfgFile in Actions then
      Bullets.Add(sBFConfigYes)
    else
      Bullets.Add(sBFConfigNo);
  end;
  if DatabaseAvailable then
  begin
    if uaCopyDB in Actions then
      Bullets.Add(sCopyDBYes)
    else
      Bullets.Add(sCopyDBNo);
  end;
  CreateBulletPage(
    tsSummary,
    [lblSummaryPrefix],
    Bullets,
    [lblSummaryPostfix1, lblSummaryPostfix2]
  );
end;

procedure TV4ConfigDlg.UpdateData;
var
  Actions: TUpdateActions;
begin
  Actions := GetUpdateActions;
  if uaCopyCfgFile in Actions then
  begin
    fFirstRun.BringForwardUserCfgFile;
    fFirstRun.UpdateCfgFiles(fCfgChanges);
  end;
  if uaCopyDB in Actions then
    fFirstRun.BringForwardDB;
end;

{ TFirstRunDlg.TAligner }

procedure TV4ConfigDlg.TAligner.AlignForm(const AForm: TCustomForm);
var
  WorkArea: TRectEx;
begin
  // This form is designed for display centred on desktop, so assume it fits
  WorkArea := Screen.WorkAreaRect;
  AForm.Left := WorkArea.Left + (WorkArea.Width - AForm.Width) div 2;
  AForm.Top := WorkArea.Top + (WorkArea.Height - AForm.Height) div 2;
end;

end.


{
 * This Source Code Form is subject to the terms of the Mozilla Public License,
 * v. 2.0. If a copy of the MPL was not distributed with this file, You can
 * obtain one at http://mozilla.org/MPL/2.0/
 *
 * Copyright (C) 2012-2020, Peter Johnson (gravatar.com/delphidabbler).
 *
 * Implements a wizard dialogue box that may be displayed on the first run of
 * CodeSnip v4 to get user to decide whether what data to bring forward from
 * earlier versions of the program.
}


unit FirstRun.FmV4ConfigDlg;

interface

uses
  // Delphi
  StdCtrls, ComCtrls, Controls, ExtCtrls, Classes, Forms,
  // Project
  FmWizardDlg, FirstRun.UMain, UBaseObjects, IntfAligner, UIStringList;

type
  ///  <summary>Wizard dialogue box for display on first run of program if there
  ///  is a need to offer user a choice whether to bring forward preferences
  ///  and/or a user database.</summary>
  TV4ConfigDlg = class(TWizardDlg, INoPublicConstruct)
    chkCopyConfig: TCheckBox;
    chkCopyDB: TCheckBox;
    lblCopyConfig: TLabel;
    lblFinish1: TLabel;
    lblFinish2: TLabel;
    lblFinish3: TLabel;
    lblIntro1: TLabel;
    lblIntro2: TLabel;
    lblIntro4: TLabel;
    lblIntro5: TLabel;
    lblIntro3: TLabel;
    lblSummaryPrefix: TLabel;
    lblSummaryPostfix1: TLabel;
    lblSummaryPostfix2: TLabel;
    lblUserDB1: TLabel;
    lblUserDB2: TLabel;
    tsConfigFile: TTabSheet;
    tsFinish: TTabSheet;
    tsIntro: TTabSheet;
    tsSummary: TTabSheet;
    tsUserDB: TTabSheet;
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
      ///  user database.</summary>
      DBPageIdx = 2;
      ///  <summary>Index of page that summarises actions to be taken.</summary>
      SummaryPageIdx = 3;
      ///  <summary>Index of last page.</summary>
      FinishPageIdx = 4;
    type
      ///  <summary>Set of actions to be taken as a result of user input.
      ///  </summary>
      TUpdateActions = set of (uaCopyCfgFile, uaCopyUserDB);
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
    ///  <summary>Checks if an old user database is available for copying.
    ///  </summary>
    function DatabaseAvailable: Boolean;
    ///  <summary>Creates a bullet list on a tab sheet.</summary>
    ///  <param name="TS">TTabSheet [in] Tab sheet where buller list is to be
    ///  placed.</param>
    ///  <param name="Prefix">array of TLabel [in] List of labels to be
    ///  positioned before bullet list.</param>
    ///  <param name="BulletPoints">IStringList [in] List of bullet item text.
    ///  </param>
    ///  <param name="PostFix">array of TLabel [in] List of labels to be
    ///  positioned after bullet list.</param>
    procedure CreateBulletPage(TS: TTabSheet; const Prefix: array of TLabel;
      BulletPoints: IStringList; const PostFix: array of TLabel);
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
    ///  <summary>Modifies window creation parameters to ensure the dialgue box
    ///  displays a button in the task bar.</summary>
    ///  <remarks>This is necessary because the dialogue box is displayed before
    ///  CodeSnip's main window is shown, so there is no suitable button
    ///  displayed yet.</remarks>
    procedure CreateParams(var Params: TCreateParams); override;
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
    ///  object to get info about user config file and database and performs
    ///  any required actions.</summary>
    class procedure Execute(AOwner: TComponent; const FirstRun: TFirstRun);
  end;


implementation


uses
  // VCL
  Windows,
  // Project
  UConsts, UCtrlArranger, UMessageBox, UStructs;


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
  TCtrlArranger.AlignLefts(
    [lblCopyConfig, chkCopyConfig], 0
  );
  lblCopyConfig.Top := 4;
  TCtrlArranger.MoveBelow(lblCopyConfig, chkCopyConfig, 12);

  // tsUserDB
  TCtrlArranger.AlignLefts(
    [lblUserDB1, lblUserDB2, chkCopyDB], 0
  );
  lblUserDB1.Top := 4;
  TCtrlArranger.MoveBelow(lblUserDB1, chkCopyDB, 12);
  TCtrlArranger.MoveBelow(chkCopyDB, lblUserDB2, 12);

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

procedure TV4ConfigDlg.CreateBulletPage(TS: TTabSheet;
  const Prefix: array of TLabel; BulletPoints: IStringList;
  const PostFix: array of TLabel);
var
  Lbl: TLabel;
  NextTop: Integer;
  BulletPoint: string;
const
  Spacing = 6;
  Bullet: Char = #$2022;

  // Frees any dynamically created labels
  procedure FreeDynLabels;
  var
    Idx: Integer;
  begin
    for Idx := Pred(TS.ControlCount) downto 0 do
    begin
      if (TS.Controls[Idx] is TLabel) and (TS.Controls[Idx].Name = '') then
        TS.Controls[Idx].Free;
    end;
  end;

begin
  FreeDynLabels;
  NextTop := 4;
  for Lbl in Prefix do
  begin
    Lbl.Top := NextTop;
    Lbl.Left := 0;
    NextTop := TCtrlArranger.BottomOf(Lbl, Spacing);
  end;
  for BulletPoint in BulletPoints do
  begin
    Lbl := TLabel.Create(Self);
    // Don't give label a name or FreeDynLabels will not work
    Lbl.Parent := TS;
    Lbl.Left := 12;
    Lbl.Top := NextTop;
    Lbl.Caption := Bullet + '  ' + BulletPoint;
    Lbl.ShowAccelChar := False;
    NextTop := TCtrlArranger.BottomOf(Lbl);
  end;
  Inc(NextTop, Spacing);
  for Lbl in Postfix do
  begin
    Lbl.Top := NextTop;
    Lbl.Left := 0;
    NextTop := TCtrlArranger.BottomOf(Lbl, Spacing);
  end;
end;

procedure TV4ConfigDlg.CreateParams(var Params: TCreateParams);
begin
  inherited;
  Params.ExStyle := Params.ExStyle OR WS_EX_APPWINDOW;
end;

function TV4ConfigDlg.DatabaseAvailable: Boolean;
begin
  Result := fFirstRun.HaveOldUserDB;
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
    Include(Result, uaCopyUserDB);
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
    DBPageIdx:
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
  sSourceFormat = 'Source code formatting preferences may have been lost.';
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
    if frcSourceFormat in fCfgChanges then
      Changes.Add(sSourceFormat);
    CreateBulletPage(
      tsFinish,
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
  // Don't display related pages if no config file or no user database
  if (Result = ConfigPageIdx) and not ConfigFileAvailable then
    Exit(NextPage(Result));
  if (Result = DBPageIdx) and not DatabaseAvailable then
    Exit(NextPage(Result));
end;

function TV4ConfigDlg.PrevPage(const PageIdx: Integer): Integer;
begin
  Result := inherited PrevPage(PageIdx);
  // Don't display related pages if no config file or no user database
  if (Result = DBPageIdx) and not DatabaseAvailable then
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
  sCopyDBYes = 'Copy user snippets database from earlier version';
  sCopyDBNo = 'Start program with an empty user snippets database';
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
    if uaCopyUserDB in Actions then
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
    fFirstRun.UpdateUserCfgFile(fCfgChanges);
  end;
  if uaCopyUserDB in Actions then
    fFirstRun.BringForwardUserDB;
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


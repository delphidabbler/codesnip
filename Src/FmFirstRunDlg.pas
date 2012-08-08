{
 * This Source Code Form is subject to the terms of the Mozilla Public License,
 * v. 2.0. If a copy of the MPL was not distributed with this file, You can
 * obtain one at http://mozilla.org/MPL/2.0/
 *
 * Copyright (C) 2012, Peter Johnson (www.delphidabbler.com).
 *
 * $Rev$
 * $Date$
 *
 * Implements a wizard dialogue box that may be displayed on the first run of
 * CodeSnip v4 to get user to decide whether what data to bring forward from
 * earlier versions of the program.
}


unit FmFirstRunDlg;

interface

uses
  // Delphi
  StdCtrls, ComCtrls, Controls, ExtCtrls, Classes, Forms,
  // Project
  FmWizardDlg, FirstRun.UMain, UBaseObjects, IntfAligner, UIStringList;

type

  TFirstRunDlg = class(TWizardDlg, INoPublicConstruct)
    tsIntro: TTabSheet;
    tsConfigFile: TTabSheet;
    tsUserDB: TTabSheet;
    tsSummary: TTabSheet;
    lblCopyConfig: TLabel;
    chkCopyConfig: TCheckBox;
    lblUserDB1: TLabel;
    chkCopyDB: TCheckBox;
    lblSummaryPrefix: TLabel;
    lblSummaryPostfix1: TLabel;
    tsFinish: TTabSheet;
    lblFinish1: TLabel;
    lblIntro1: TLabel;
    lblIntro2: TLabel;
    lblIntro4: TLabel;
    lblIntro5: TLabel;
    lblIntro3: TLabel;
    lblUserDB2: TLabel;
    lblSummaryPostfix2: TLabel;
    lblFinish2: TLabel;
    lblFinish3: TLabel;
    procedure FormCloseQuery(Sender: TObject; var CanClose: Boolean);
  strict private
    const
      IntroPageIdx = 0;
      ConfigPageIdx = 1;
      DBPageIdx = 2;
      SummaryPageIdx = 3;
      FinishPageIdx = 4;
    type
      TUpdateActions = set of (uaCopyCfgFile, uaCopyUserDB);
  strict private
    var
      fFirstRun: TFirstRun;
      fCfgChanges: TFirstRunCfgChangeSet;
    function GetUpdateActions: TUpdateActions;
    function ConfigFileAvailable: Boolean;
    function DatabaseAvailable: Boolean;
    procedure CreateBulletPage(TS: TTabSheet; const Prefix: array of TLabel;
      BulletPoints: IStringList; const PostFix: array of TLabel);
    procedure UpdateChoices;
    procedure UpdateData;
    procedure ListChanges;
  strict private
    type
      TAligner = class(TInterfacedObject, IFormAligner)
      public
        ///  <summary>Aligns wizard at centre of primary monitor.</summary>
        procedure AlignForm(const AForm: TCustomForm);
          {Aligns splash form over main form.
            @param AForm [in] Form to be aligned.
          }
      end;
  strict protected
    function GetAligner: IFormAligner; override;
    procedure ArrangeForm; override;
    procedure ConfigForm; override;
      {Initialises HTML frame, loads HTML template and inserts HTML
      representation of Extra Text REML.
      }
    function HeadingText(const PageIdx: Integer): string; override;
      {Gets text of heading of a wizard page.
        @param PageIdx [in] Index of page for which heading is required.
        @return Heading text.
      }
    function NextPage(const PageIdx: Integer): Integer; override;
      {Index of next wizard page. Must not be called when at last page. Can be
      overridden to change default ordering of pages.
        @param PageIdx [in] Index of current page.
        @return Index of next page.
      }
    function PrevPage(const PageIdx: Integer): Integer; override;
      {Index of previous wizard page. Must not be called when at first page. Can
      be overridden to change deafult ordering of pages.
        @param PageIdx [in] Index of current page.
        @return Index of previous page.
      }
    procedure BeginPage(const PageIdx: Integer); override;
      {Called when a wizard page is first displayed. Descendants can override to
      perform initialisation.
        @param PageIdx [in] Index page to be initialised.
      }
    procedure MoveForward(const PageIdx: Integer;
      var CanMove: Boolean); override;
      {Called when about to move forward to a new page. Descendants can override
      to tidy up existing page or prevent move.
        @param PageIdx [in] Index of page we are about move to.
        @param CanMove [in/out] Flag indicating whether page change is allowed.
          Defaults to true.
      }
    procedure UpdateButtons(const PageIdx: Integer); override;
      {Updates wizard buttons depending on page and state.
        @param PageIdx [in] Index of current page.
      }
  public
    class procedure Execute(AOwner: TComponent; const FirstRun: TFirstRun);
  end;


implementation


uses
  // Project
  UConsts, UCtrlArranger, UMessageBox, UStructs;


{$R *.dfm}

{ TFirstRunDlg }

procedure TFirstRunDlg.ArrangeForm;
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

procedure TFirstRunDlg.BeginPage(const PageIdx: Integer);
begin
  if PageIdx = SummaryPageIdx then
    UpdateChoices;
end;

function TFirstRunDlg.ConfigFileAvailable: Boolean;
begin
  Result := fFirstRun.HaveOldCfgFile;
end;

procedure TFirstRunDlg.ConfigForm;
resourcestring
  sConfigFiles = 'Configuration file';
  sDatabase = 'User-defined snippets database';
begin
  inherited;
  pcWizard.ActivePage := tsIntro; // ensure HTML frame tab active before loading
  fCfgChanges := [];
end;

procedure TFirstRunDlg.CreateBulletPage(TS: TTabSheet;
  const Prefix: array of TLabel; BulletPoints: IStringList;
  const PostFix: array of TLabel);
var
  Lbl: TLabel;
  NextTop: Integer;
  BulletPoint: string;
const
  Spacing = 6;
  Bullet: Char = #$2022;

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
    // Don't give label a name: required for FreeDynLabels to work
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

function TFirstRunDlg.DatabaseAvailable: Boolean;
begin
  Result := fFirstRun.HaveOldUserDB;
end;

class procedure TFirstRunDlg.Execute(AOwner: TComponent;
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

procedure TFirstRunDlg.FormCloseQuery(Sender: TObject; var CanClose: Boolean);
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

function TFirstRunDlg.GetAligner: IFormAligner;
begin
  Result := TAligner.Create;
end;

function TFirstRunDlg.GetUpdateActions: TUpdateActions;
begin
  Result := [];
  if ConfigFileAvailable and chkCopyConfig.Checked then
    Include(Result, uaCopyCfgFile);
  if DatabaseAvailable and chkCopyDB.Checked then
    Include(Result, uaCopyUserDB);
end;

function TFirstRunDlg.HeadingText(const PageIdx: Integer): string;
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

procedure TFirstRunDlg.ListChanges;
resourcestring
  sRegistration = 'Program registration information has been lost.';
  sHiliter = 'Syntax highlighter customisations have been lost.';
  sProxyPwd = 'Your proxy server password needs to be re-entered.';
  sSourceFormat = 'Source code formatting preferences may have been lost.';
var
  Changes: IStringList;
begin
  if fCfgChanges <> [] then
  begin
    lblFinish2.Visible := True;
    Changes := TIStringList.Create;
    if frcRegistration in fCfgChanges then
      Changes.Add(sRegistration);
    if frcHiliter in fCfgChanges then
      Changes.Add(sHiliter);
    if frcProxyPwd in fCfgChanges then
      Changes.Add(sProxyPwd);
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
    lblFinish2.Visible := False;
    TCtrlArranger.AlignLefts([lblFinish1, lblFinish3], 0);
    lblFinish1.Top := 4;
    TCtrlArranger.MoveBelow(lblFinish1, lblFinish3, 12);
  end;

end;

procedure TFirstRunDlg.MoveForward(const PageIdx: Integer;
  var CanMove: Boolean);
begin
  if PageIdx = SummaryPageIdx then
  begin
    UpdateData;
    ListChanges;
  end;
end;

function TFirstRunDlg.NextPage(const PageIdx: Integer): Integer;
begin
  Result := inherited NextPage(PageIdx);
  // Don't display related pages if no config file or no user database
  if (Result = ConfigPageIdx) and not ConfigFileAvailable then
    Exit(NextPage(Result));
  if (Result = DBPageIdx) and not DatabaseAvailable then
    Exit(NextPage(Result));
end;

function TFirstRunDlg.PrevPage(const PageIdx: Integer): Integer;
begin
  Result := inherited PrevPage(PageIdx);
  // Don't display related pages if no config file or no user database
  if (Result = DBPageIdx) and not DatabaseAvailable then
    Exit(PrevPage(Result));
  if (Result = ConfigPageIdx) and not ConfigFileAvailable then
    Exit(PrevPage(Result));
end;

procedure TFirstRunDlg.UpdateButtons(const PageIdx: Integer);
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

procedure TFirstRunDlg.UpdateChoices;
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

procedure TFirstRunDlg.UpdateData;
var
  Actions: TUpdateActions;
begin
  Actions := GetUpdateActions;
  if uaCopyCfgFile in Actions then
  begin
    fFirstRun.BringForwardCfgFile;
    fFirstRun.UpdateCfgFile(fCfgChanges);
  end;
  if uaCopyUserDB in Actions then
    fFirstRun.BringForwardUserDB;
end;

{ TFirstRunDlg.TAligner }

procedure TFirstRunDlg.TAligner.AlignForm(const AForm: TCustomForm);
var
  WorkArea: TRectEx;
begin
  // This form is designed for display centred on desktop, so assume it fits
  WorkArea := Screen.WorkAreaRect;
  AForm.Left := WorkArea.Left + (WorkArea.Width - AForm.Width) div 2;
  AForm.Top := WorkArea.Top + (WorkArea.Height - AForm.Height) div 2;
end;

end.


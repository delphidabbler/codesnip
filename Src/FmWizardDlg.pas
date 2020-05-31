{
 * This Source Code Form is subject to the terms of the Mozilla Public License,
 * v. 2.0. If a copy of the MPL was not distributed with this file, You can
 * obtain one at http://mozilla.org/MPL/2.0/
 *
 * Copyright (C) 2006-2020, Peter Johnson (gravatar.com/delphidabbler).
 *
 * Base class for multi-page modal "wizard" dialogue boxes.
}


unit FmWizardDlg;


interface


uses
  // Delphi
  ComCtrls, StdCtrls, Controls, ExtCtrls, Classes,
  // Project
  FmGenericModalDlg, FmGenericDlg;


type

  {
  TWizardDlg:
    Base class for multi-page modal "wizard" dialog boxes.
  }
  TWizardDlg = class(TGenericModalDlg)
    btnBack: TButton;
    btnNext: TButton;
    btnCancel: TButton;
    pnlHead: TPanel;
    lblHead: TLabel;
    pcWizard: TPageControl;
    procedure btnNextClick(Sender: TObject);
    procedure btnBackClick(Sender: TObject);
  strict private
    procedure InitPage(const PageIdx: Integer);
      {Performs initialisation of a newly displayed page before calling
      BeginPage.
        @param PageIdx [in] Index of new page.
      }
    procedure GoForward;
      {Attempts to move forward in wizard.
      }
    procedure GoBackward;
      {Attempts to move backward in wizard.
      }
  strict protected
    procedure ArrangeForm; override;
      {Positions controls and sets form size according to body panel dimensions.
      }
    function ModalResultOnEsc: Integer; override;
      {Gets modal result returned from dialog when user presses ESC key.
        @return Required modal result.
      }
    procedure ConfigForm; override;
      {Sets correct UI default font on heading label.
      }
    procedure InitForm; override;
      {Displays and initialises first page of wizard.
      }
    function HeadingText(const PageIdx: Integer): string; virtual; abstract;
      {Gets text of heading of a wizard page.
        @param PageIdx [in] Index of page for which heading is required.
        @return Heading text.
      }
    procedure UpdateButtons(const PageIdx: Integer); virtual;
      {Updates wizard buttons depending on page and state.
        @param PageIdx [in] Index of current page.
      }
    procedure BeginPage(const PageIdx: Integer); virtual;
      {Called when a wizard page is first displayed. Descendants can override to
      perform initialisation.
        @param PageIdx [in] Index page to be initialised.
      }
    procedure MoveForward(const PageIdx: Integer;
      var CanMove: Boolean); virtual;
      {Called when about to move forward to a new page. Descendants can override
      to tidy up existing page or prevent move.
        @param PageIdx [in] Index of page we are about move to.
        @param CanMove [in/out] Flag indicating whether page change is allowed.
          Defaults to true.
      }
    procedure MoveBackward(const PageIdx: Integer;
      var CanMove: Boolean); virtual;
      {Called when about to move back to an earlier page. Descendants can
      override to tidy up existing page or prevent move.
        @param PageIdx [in] Index of page we are about move to.
        @param CanMove [in/out] Flag indicating whether page change is allowed.
          Defaults to true.
      }
    function FirstPage: Integer; virtual;
      {Index of first page in wizard.
        @return 0. Can be overridden to change index of first page.
      }
    function LastPage: Integer; virtual;
      {Index of last page in wizard.
        @return Index of last page in page control. Can be overridden to change
          index.
      }
    function NextPage(const PageIdx: Integer): Integer; virtual;
      {Index of next wizard page. Must not be called when at last page. Can be
      overridden to change default ordering of pages.
        @param PageIdx [in] Index of current page.
        @return Index of next page.
      }
    function PrevPage(const PageIdx: Integer): Integer; virtual;
      {Index of previous wizard page. Must not be called when at first page. Can
      be overridden to change deafult ordering of pages.
        @param PageIdx [in] Index of current page.
        @return Index of previous page.
      }
    function CurrentPage: Integer;
      {Index of currently displayed page.
        @return Required index.
      }
  end;


implementation


uses
  // Project
  UCtrlArranger, UFontHelper;


{$R *.dfm}


resourcestring
  // Captions for btnNext
  sNextBtnCaption = 'Next &>';
  sFinishBtnCaption = '&Finish';


{ TWizardDlg }

procedure TWizardDlg.ArrangeForm;
  {Positions controls and sets form size according to body panel dimensions.
  }
begin
  inherited;
  // Align Cancel, Back and Next buttons
  TCtrlArranger.MoveToLeftOf(btnHelp, btnCancel, 16);
  TCtrlArranger.MoveToLeftOf(btnCancel, btnNext, 4);
  TCtrlArranger.MoveToLeftOf(btnNext, btnBack, 4);
  btnCancel.Top := btnHelp.Top;
  btnNext.Top := btnHelp.Top;
  btnBack.Top := btnHelp.Top;
  // Centre heading label vertically in title
  lblHead.Top := (pnlHead.ClientHeight - lblHead.Height) div 2;
end;

procedure TWizardDlg.BeginPage(const PageIdx: Integer);
  {Called when a wizard page is first displayed. Descendants can override to
  perform initialisation.
    @param PageIdx [in] Index page to be initialised.
  }
begin
  // Do nothing: override in descendants
end;

procedure TWizardDlg.btnBackClick(Sender: TObject);
  {Attempts to move to previous page in wizard unless we are on first page.
    @param Sender [in] Not used.
  }
begin
  if CurrentPage <> FirstPage then
    GoBackward;
end;

procedure TWizardDlg.btnNextClick(Sender: TObject);
  {Attempts to move to next page when Next button clicked. If we are on last
  page the wizard will be closed.
    @param Sender [in] Not used.
  }
begin
  if CurrentPage <> LastPage then
    GoForward
  else
    Close;
end;

procedure TWizardDlg.ConfigForm;
  {Sets correct UI default font on heading label.
  }
begin
  inherited;
  TFontHelper.SetDefaultBaseFont(lblHead.Font);
end;

function TWizardDlg.CurrentPage: Integer;
  {Gets index of currently displayed page.
    @return Required index.
  }
begin
  Result := pcWizard.ActivePageIndex;
end;

function TWizardDlg.FirstPage: Integer;
  {Index of first page in wizard.
    @return 0. Can be overridden to change index of first page.
  }
begin
  Result := 0;
end;

procedure TWizardDlg.GoBackward;
  {Attempts to move backward in wizard.
    @param NewPageIdx [in] Index of page in wizard to display.
  }
var
  CanLeave: Boolean;  // Flag indicating whether we can leave current page
begin
  CanLeave := True;
  MoveBackward(CurrentPage, CanLeave);
  if CanLeave then
    InitPage(PrevPage(CurrentPage));
end;

procedure TWizardDlg.GoForward;
  {Attempts to move forward in wizard.
  }
var
  CanLeave: Boolean;  // Flag indicating whether we can leave current page
begin
  CanLeave := True;
  MoveForward(CurrentPage, CanLeave);
  if CanLeave then
    InitPage(NextPage(CurrentPage));
end;

procedure TWizardDlg.InitForm;
  {Displays and initialises first page of wizard.
  }
begin
  inherited;
  InitPage(FirstPage);
end;

procedure TWizardDlg.InitPage(const PageIdx: Integer);
  {Performs initialisation of a newly displayed page before calling BeginPage.
    @param PageIdx [in] Index of new page.
  }
begin
  pcWizard.ActivePageIndex := PageIdx;
  lblHead.Caption := HeadingText(PageIdx);
  UpdateButtons(PageIdx);
  BeginPage(PageIdx);
end;

function TWizardDlg.LastPage: Integer;
  {Index of last page in wizard.
    @return Index of last page in page control. Can be overridden to change
    index.
  }
begin
  Result := Pred(pcWizard.PageCount);
end;

function TWizardDlg.ModalResultOnEsc: Integer;
  {Gets modal result returned from dialog when user presses ESC key.
    @return Required modal result.
  }
begin
  if btnCancel.Enabled then
    Result := btnCancel.ModalResult
  else
    Result := mrNone;
end;

procedure TWizardDlg.MoveBackward(const PageIdx: Integer;
  var CanMove: Boolean);
  {Called when about to move back to an earlier page. Descendants can override
  to tidy up existing page or prevent move.
    @param PageIdx [in] Index of page we are about move to.
    @param CanMove [in/out] Flag indicating whether page change is allowed.
      Defaults to true.
  }
begin
  // Do nothing: override in descendants
end;

procedure TWizardDlg.MoveForward(const PageIdx: Integer;
  var CanMove: Boolean);
  {Called when about to move forward to a new page. Descendants can override
  to tidy up existing page or prevent move.
    @param PageIdx [in] Index of page we are about move to.
    @param CanMove [in/out] Flag indicating whether page change is allowed.
      Defaults to true.
  }
begin
  // Do nothing: override in descendants
end;

function TWizardDlg.NextPage(const PageIdx: Integer): Integer;
  {Index of next wizard page. Must not be called when at last page. Can be
  overridden to change default ordering of pages.
    @param PageIdx [in] Index of current page.
    @return Index of next page.
  }
begin
  Assert(PageIdx <> LastPage, ClassName + '.NextPage: PageIdx = LastPage');
  Result := Succ(PageIdx)
end;

function TWizardDlg.PrevPage(const PageIdx: Integer): Integer;
  {Index of previous wizard page. Must not be called when at first page. Can
  be overridden to change deafult ordering of pages.
    @param PageIdx [in] Index of current page.
    @return Index of previous page.
  }
begin
  Assert(PageIdx <> FirstPage, ClassName + '.PrevPage: PageIdx = FirstPage');
  Result := Pred(PageIdx)
end;

procedure TWizardDlg.UpdateButtons(const PageIdx: Integer);
  {Updates wizard buttons depending on page and state.
    @param PageIdx [in] Index of current page.
  }
begin
  btnBack.Enabled := PageIdx <> FirstPage;
  btnCancel.Enabled := PageIdx <> LastPage;
  btnNext.Enabled := True;
  if PageIdx = LastPage then
    btnNext.Caption := sFinishBtnCaption
  else
    btnNext.Caption := sNextBtnCaption;
end;

end.


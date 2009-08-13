{
 * FmBugReportDlg.pas
 *
 * Implements a wizard style bug report dialog box.
 *
 * $Rev$
 * $Date$
 *
 * ***** BEGIN LICENSE BLOCK *****
 *
 * Version: MPL 1.1
 *
 * The contents of this file are subject to the Mozilla Public License Version
 * 1.1 (the "License"); you may not use this file except in compliance with the
 * License. You may obtain a copy of the License at http://www.mozilla.org/MPL/
 *
 * Software distributed under the License is distributed on an "AS IS" basis,
 * WITHOUT WARRANTY OF ANY KIND, either express or implied. See the License for
 * the specific language governing rights and limitations under the License.
 *
 * The Original Code is FmBugReportDlg.pas
 *
 * The Initial Developer of the Original Code is Peter Johnson
 * (http://www.delphidabbler.com/).
 *
 * Portions created by the Initial Developer are Copyright (C) 2005-2009 Peter
 * Johnson. All Rights Reserved.
 *
 * Contributor(s)
 *   NONE
 *
 * ***** END LICENSE BLOCK *****
}


unit FmBugReportDlg;


interface


uses
  // Delphi
  SysUtils, StdCtrls, ComCtrls, Graphics, ExtCtrls, Controls, Classes,
  // Project
  FmWizardDlg, Forms, FrBrowserBase, FrHTMLDlg, FrFixedHTMLDlg;


type

  {
  TBugReportDlg:
    Implements a multi-page wizard style dialog box that is displayed when an
    unexpected exception is detected or when a user requests to report a bug.
    The wizard gathers information about the bug and sends it to the
    DelphiDabbler website.
  }
  TBugReportDlg = class(TWizardDlg)
    tsIntroBug: TTabSheet;
    tsIntroUser: TTabSheet;
    tsBugInfo: TTabSheet;
    tsUserInfo: TTabSheet;
    tsSubmit: TTabSheet;
    tsDone: TTabSheet;
    imgBug: TImage;
    lblHeading: TLabel;
    bvlBugDesc: TBevel;
    lblBugMarker: TLabel;
    lblBugInfo: TLabel;
    lblExceptIntro: TLabel;
    lblUserIntro: TLabel;
    lblDesc: TLabel;
    memoDesc: TMemo;
    lblEmailRequest: TLabel;
    lblEmail: TLabel;
    edEmail: TEdit;
    lblOS: TLabel;
    edOS: TEdit;
    lblReport: TLabel;
    edReport: TMemo;
    lblSubmit: TLabel;
    lblDone: TLabel;
    lblUserBegin: TLabel;
    lblBugBegin: TLabel;
    frmPrivacy: TFixedHTMLDlgFrame;
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
  strict private
    fErrorObj: Exception;
      {If dialog box displayed as a result of an exception this field stores a
      reference to the exception. It is nil if user initiated the dialog box}
    fBugData: TStringList;
      {Contains the bug report sent to the web server}
    procedure CreateReport;
      {Creates bug report in fBugData string list.
      }
    procedure DoSubmit;
      {Attempts to submit a bug report to the DelphiDabbler website.
        @except ECodeSnip raised on web service or script errors. Unexpected
          exceptions are also converted to ECodeSnip.
      }
  strict protected
    procedure ConfigForm; override;
      {Configures form. Controls with dynamic text are set. Fonts set to default
      where required and HTML is loaded into HTML frame.
      }
    procedure ArrangeForm; override;
      {Aligns and sizes of controls depending on text sizes.
      }
    function HeadingText(const PageIdx: Integer): string; override;
      {Gets text of heading of a wizard page.
        @param PageIdx [in] Index of page for which heading is required.
        @return Heading text.
      }
    procedure UpdateButtons(const PageIdx: Integer); override;
      {Updates wizard buttons depending on page.
        @param PageIdx [in] Index of current page.
      }
    procedure BeginPage(const PageIdx: Integer); override;
      {Performs any required initialisation when a page is displayed.
        @param PageIdx [in] Index of page to be initialised.
      }
    procedure MoveForward(const PageIdx: Integer;
      var CanMove: Boolean); override;
      {Performs required processing when moving forward from pages.
        @param PageIdx [in] Index of page we are leaving.
        @param CanMove [in/out] Flag indicating whether can leave page. Defaults
          to true.
      }
    function FirstPage: Integer; override;
      {Index of first page in wizard.
        @return Index of either bug intro or user intro pages depending on how
          the dialog box was called.
      }
    function LastPage: Integer; override;
      {Index of last page in wizard.
        @return Required page index.
      }
    function NextPage(const PageIdx: Integer): Integer; override;
      {Index of next wizard page.
        @param PageIdx [in] Index of current page.
        @return Index of next page.
      }
    function PrevPage(const PageIdx: Integer): Integer; override;
      {Index of previous wizard page.
        @param PageIdx [in] Index of current page.
        @return Index of previous page.
      }
  public
    class procedure Execute(Owner: TComponent; const ErrorObj: Exception = nil);
      {Creates and displays bug report wizard dialog box.
        @param Owner [in] Component that owns dialog box. Dialog box is aligned
          over this component if it is a form. If Owner it is nil or not a form
          the dialog is aligned over the active form.
        @param ErrorObj [in] Exception that caused dialog box to be displayed or
          nil if dialog was displayed by user.
      }
  end;


implementation


uses
  // Delphi
  StrUtils, Windows,
  // Project
  UAppInfo, UBugReporter, UConsts, UCtrlArranger, UEmailHelper, UExceptions,
  UFontHelper, UMessageBox, USystemInfo, UWebInfo, UWebService;


{$R *.dfm}


resourcestring
  // Exception type messages
  sBugDetected = 'The following bug has been detected:';
  sErrorDetected = 'The following error has been detetcted: it may be a bug:';

  // Validation error messages
  sUserDescNeeded = 'You need to provide a description of the bug!';
  sBugDescNeeded = 'Please provide a description';
  sBadEmailAddress = 'Invalid email address';

  // Message box queries
  sDescQuery = 'No description has been provided - continue anyway?';

  // Web service / server errors
  sWebServerError = 'Submission failed because web server reported '
    + 'HTTP Error %0:d: %1:s';
  sUnexpectedError = 'UNEXPECTED ERROR: %0:s' + EOL2
    + 'Please report this bug via %1:s';

  // Wizard page titles
  sBugIntroPageTitle = 'CodeSnip has encountered an error';
  sUserIntroPageTitle = 'Bug report wizard';
  sBugInfoPageTitle = 'Step 1: About the bug';
  sUserInfoPageTitle = 'Step 2: About you';
  sSubmitPageTitle = 'Step 3: Send the bug report';
  sDonePageTitle = 'Finished';

  // Special caption used by next button on submit page
  sSubmitBtnCaption = '&Submit';

  // Form caption
  sBug = 'Bug!!';
  sBugReport = 'Bug Report';


const
  // Version number of bug report
  cBugRepVer = '1';

  // Wizard page indexes
  cBugIntroPage = 0;        // first page seen after bug exception detected
  cUserIntroPage = 1;       // first page seen when user reports error
  cBugInfoPage = 2;         // collects info about bug
  cUserInfoPage = 3;        // collects info about user
  cSubmitPage = 4;          // page where user submits info
  cDonePage = 5;            // final, confirmation page
  cLastPageIdx = cDonePage; // index of last page


{ TBugReportDlg }

procedure TBugReportDlg.ArrangeForm;
  {Aligns and sizes of controls depending on text sizes.
  }
var
  ATop: Integer; // top of a control
begin
  inherited;
  TCtrlArranger.SetLabelHeights(Self);
  // tsIntroBug
  bvlBugDesc.Height := lblBugInfo.Height + 10;
  bvlBugDesc.Top := TCtrlArranger.BottomOf(lblHeading, 8);
  lblBugMarker.Top := TCtrlArranger.BottomOf(lblHeading, 12);
  lblBugInfo.Top := lblBugMarker.Top;
  lblExceptIntro.Top := TCtrlArranger.BottomOf(bvlBugDesc, 8);
  lblBugBegin.Top := TCtrlArranger.BottomOf(lblExceptIntro, 8);
  // tsIntroUser
  lblUserBegin.Top := TCtrlArranger.BottomOf(lblUserIntro, 8);
  // tsBugInfo
  memoDesc.Top := TCtrlArranger.BottomOf(lblDesc, 8);
  memoDesc.Height := tsUserInfo.Height - memoDesc.Top;
  // tsBugInfo
  frmPrivacy.Height := frmPrivacy.DocHeight;
  ATop := TCtrlArranger.BottomOf(lblEmailRequest, 8);
  ATop := ATop + TCtrlArranger.AlignVCentres(ATop, [lblEmail, edEmail]) + 8;
  edEmail.Left := lblEmail.Left + lblEmail.Width + 8;
  edEmail.Width := tsUserInfo.Width - edEmail.Left;
  frmPrivacy.Top := ATop;
  lblOS.Top := TCtrlArranger.BottomOf(frmPrivacy, 8);
  edOS.Top := TCtrlArranger.BottomOf(frmPrivacy, 8);
  // tsSubmit
  edReport.Top := TCtrlArranger.BottomOf(lblReport, 8);
  lblSubmit.Top := tsSubmit.Height - lblSubmit.Height;
  edReport.Height := lblSubmit.Top - edReport.Top - 8;
  // tsDone
  { nothing to do }
end;

procedure TBugReportDlg.BeginPage(const PageIdx: Integer);
  {Performs any required initialisation when a page is displayed.
    @param PageIdx [in] Index of page to be initialised.
  }
begin
  case PageIdx of
    cBugInfoPage:
    begin
      memoDesc.SetFocus;
      memoDesc.SelectAll;
    end;
    cUserInfoPage:
      edEmail.SetFocus;
    cSubmitPage:
      CreateReport;
  end;
end;

procedure TBugReportDlg.ConfigForm;
  {Configures form. Controls with dynamic text are set. Fonts set to default
  where required and HTML is loaded into HTML frame.
  }

  // ---------------------------------------------------------------------------
  procedure LoadHTMLFrame(const Frm: TFixedHTMLDlgFrame; const ResName: string);
    {Safely loads HTML into an HTML frame. To do this requires the tab sheet
    containing the frame to be active, so we find and activate the required tab
    before loading the HTML into the frame.
      @param Frm [in] HTML frame to be initialised with loaded HTML.
      @param ResName [in] Name of resource containing HTML.
    }
  var
    Ctrl: TWinControl;  // Scans through frame's parents looking for tab sheet
  begin
    Ctrl := Frm.Parent;
    while Assigned(Ctrl) and not (Ctrl is TTabSheet) do
      Ctrl := Ctrl.Parent;
    Assert(Assigned(Ctrl),
      ClassName + '.ConfigForm:LoadHTMLFrame: HTML Frame not on a tab sheet');
    pcWizard.ActivePage := Ctrl as TTabSheet;
    Frm.Initialise(ResName);
  end;
  // ---------------------------------------------------------------------------

begin
  inherited;
  // Select and set up first page of wizard according to if shown as a result of
  // exception or user request
  if Assigned(fErrorObj) then
  begin
    // Displayed as a result of exception: set up first page
    // show status of exception (bug or other error)
    if (fErrorObj is EBug) or (fErrorObj is EAssertionFailed) then
      lblHeading.Caption := sBugDetected
    else
      lblHeading.Caption := sErrorDetected;
    // display the exception's message
    lblBugInfo.Caption := fErrorObj.Message;
    // beep before displaying dialog
    MessageBeep(MB_ICONEXCLAMATION);
    // set form's caption
    Caption := sBug;
  end
  else
    // Displayed as a result of user request (no exception): set up first page
    // set form's caption
    Caption := sBugReport;
  // Display the operating system info
  edOS.Text := TOSInfo.Description;
  // Set default font where necessary
  TFontHelper.SetDefaultBaseFont(lblBugInfo.Font, False);
  // Load HTML into browser control
  LoadHTMLFrame(frmPrivacy, 'frm-emailprivacy.html');
end;

procedure TBugReportDlg.CreateReport;
  {Creates bug report in fBugData string list.
  }

  // ---------------------------------------------------------------------------
  procedure RecordBugListItem(const Name, Value: string);
    {Records or updates report line in bug list.
      @param Name [in] Name of a bug report item.
      @param Value [in] Value of a bug report item.
    }
  begin
    if fBugData.IndexOfName(Name) = -1 then
      fBugData.Add(Name + '=' + Value)
    else
      fBugData.Values[Name] := Value;
  end;

  function NoEOL(const Str: string): string;
    {Replaces all line breaks in a string with spaces.
      @param Str [in] String to have line breaks converted.
      @return String with line breaks converted to spaced.
    }
  begin
    Result := ReplaceStr(Str, EOL, ' ');
  end;
  // ---------------------------------------------------------------------------

begin
  fBugData.Clear;
  RecordBugListItem('BugRepVer', cBugRepVer);
  RecordBugListItem('ProgId', TAppInfo.ProgramID);
  RecordBugListItem('ProgName', TAppInfo.ProgramName);
  RecordBugListItem('ProgVer', TAppInfo.ProgramReleaseVersion);
  RecordBugListItem('FileVer', TAppInfo.ProgramFileVersion);
  RecordBugListItem('OSDesc', TOSInfo.Description);
  if Assigned(fErrorObj) then
    RecordBugListItem(
      'ExceptionInfo',
      NoEOL(Format('%s: %s', [fErrorObj.ClassName, lblBugInfo.Caption]))
    )
  else
    RecordBugListItem('ExceptionInfo', '');
  RecordBugListItem('UserDesc', NoEOL(Trim(memoDesc.Text)));
  RecordBugListItem('UserEMail', edEmail.Text);
  edReport.Lines.Assign(fBugData);
end;

procedure TBugReportDlg.DoSubmit;
  {Attempts to submit a bug report to the DelphiDabbler website.
    @except ECodeSnip raised on web service or script errors. Unexpected
      exceptions are also converted to ECodeSnip.
  }
var
  BugReporter: TBugReporter;  // communicates with web service
begin
  try
    // Do the submission
    Screen.Cursor := crHourglass;
    BugReporter := TBugReporter.Create;
    try
      // Store user agent name and POST the data
      BugReporter.Submit(fBugData);
    finally
      FreeAndNil(BugReporter);
      Screen.Cursor := crDefault;
    end;
  except
    // handle any exceptions from submission: we convert expected exceptions to
    // ECodeSnip to save triggering this dialog again: others are re-raised
    on E: EHTTPError do
      // error on web server: make more friendly
      raise ECodeSnip.CreateFmt(
        sWebServerError, [E.HTTPErrorCode, Trim(E.Message)]
      );
    on E: ECodeSnip do
      // other expected error (inc script errors): re-raise
      raise;
    on E: Exception do
      // unexpected error: re-raise as ECodeSnip to prevent this dialog getting
      // triggered again
      raise ECodeSnip.CreateFmt(
        sUnexpectedError, [E.Message, TWebInfo.ContactPageURL]);
  end;
end;

class procedure TBugReportDlg.Execute(Owner: TComponent;
  const ErrorObj: Exception);
  {Creates and displays bug report wizard dialog box.
    @param Owner [in] Component that owns dialog box. Dialog box is aligned over
      this component if it is a form. If Owner it is nil or not a form the
      dialog is aligned over the active form.
    @param ErrorObj [in] Exception that caused dialog box to be displayed or nil
      if dialog was displayed by user.
  }
begin
  // Create dialog box
  with Create(Owner) do
    try
      // Record the exception object
      fErrorObj := ErrorObj;
      // Display dialog box
      ShowModal;
    finally
      Free;
    end;
end;

function TBugReportDlg.FirstPage: Integer;
  {Index of first page in wizard.
    @return Index of either bug intro or user intro pages depending on how the
      dialog box was called.
  }
begin
  if Assigned(fErrorObj) then
    // called automatically as a result of an exception
    Result := cBugIntroPage
  else
    // called by user
    Result := cUserIntroPage;
end;

procedure TBugReportDlg.FormCreate(Sender: TObject);
  {Creates owned objects when form is created.
    @param Sender [in] Not used.
  }
begin
  inherited;
  // Create object to store data about bug for sending to website
  fBugData := TStringList.Create;
end;

procedure TBugReportDlg.FormDestroy(Sender: TObject);
  {Disposes of owned objects when form is destroyed.
    @param Sender [in] Not used.
  }
begin
  FreeAndNil(fBugData);
  inherited;
end;

function TBugReportDlg.HeadingText(const PageIdx: Integer): string;
  {Gets text of heading of a wizard page.
    @param PageIdx [in] Index of page for which heading is required.
    @return Heading text.
  }
const
  // Titles of each wizard page
  cPageHeaders: array[0..cLastPageIdx] of string = (
    sBugIntroPageTitle, sUserIntroPageTitle, sBugInfoPageTitle,
    sUserInfoPageTitle, sSubmitPageTitle, sDonePageTitle
  );
begin
  Result := cPageHeaders[PageIdx];
end;

function TBugReportDlg.LastPage: Integer;
  {Index of last page in wizard.
    @return Required page index.
  }
begin
  Result := cLastPageIdx;
end;

procedure TBugReportDlg.MoveForward(const PageIdx: Integer;
  var CanMove: Boolean);
  {Performs required processing when moving forward from pages.
    @param PageIdx [in] Index of page we are leaving.
    @param CanMove [in/out] Flag indicating whether can leave page. Defaults to
      true.
  }
begin
  inherited;
  case PageIdx of
    cBugInfoPage:
    begin
      // Validate data entered on bug information page
      if (memoDesc.Text = '') then
      begin
        // no description entered
        if not Assigned(fErrorObj) then
        begin
          // for user report description is required: no other error info
          TMessageBox.Error(Self, sUserDescNeeded);
          CanMove := False;
        end
        else if not TMessageBox.Confirm(Self, sDescQuery) then
        begin
          // for internally generated report desc is desirable, but we also have
          // info from error object, so we just check with user
          TMessageBox.Error(Self, sBugDescNeeded);
          CanMove := False;
        end;
      end;
    end;
    cUserInfoPage:
    begin
      // Validate data entered on user information page
      // do some basic validation of (optional) email address
      if (edEmail.Text <> '') and not IsValidEmailAddress(edEmail.Text) then
      begin
        TMessageBox.Error(Self, sBadEmailAddress);
        CanMove := False;
      end;
    end;
    cSubmitPage:
      // We submit data to web service on leaving submit page
      // DoSubmit raises exceptions on error, so we don't bother setting CanMove
      DoSubmit;
  end;
end;

function TBugReportDlg.NextPage(const PageIdx: Integer): Integer;
  {Index of next wizard page.
    @param PageIdx [in] Index of current page.
    @return Index of next page.
  }
begin
  case PageIdx of
    cBugIntroPage, cUserIntroPage: Result := cBugInfoPage;
    cBugInfoPage: Result := cUserInfoPage;
    cUserInfoPage: Result := cSubmitPage;
    cSubmitPage: Result := cDonePage;
    else Result := inherited NextPage(PageIdx);
  end;
end;

function TBugReportDlg.PrevPage(const PageIdx: Integer): Integer;
  {Index of previous wizard page.
    @param PageIdx [in] Index of current page.
    @return Index of previous page.
  }
begin
  if PageIdx = cBugInfoPage then
    Result := FirstPage
  else
    Result := inherited PrevPage(PageIdx);
end;

procedure TBugReportDlg.UpdateButtons(const PageIdx: Integer);
  {Updates wizard buttons depending on page.
    @param PageIdx [in] Index of current page.
  }
begin
  inherited;
  if PageIdx = cSubmitPage then
    btnNext.Caption := sSubmitBtnCaption
end;

end.


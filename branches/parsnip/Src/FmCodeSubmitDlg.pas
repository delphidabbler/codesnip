{
 * This Source Code Form is subject to the terms of the Mozilla Public License,
 * v. 2.0. If a copy of the MPL was not distributed with this file, You can
 * obtain one at http://mozilla.org/MPL/2.0/
 *
 * Copyright (C) 2008-2013, Peter Johnson (www.delphidabbler.com).
 *
 * $Rev$
 * $Date$
 *
 * Implements a wizard dialogue box that gathers data about and submits a user's
 * code submission for inclusion in the database.
}


unit FmCodeSubmitDlg;


interface


uses
  // Delphi
  StdCtrls,
  Forms,
  ComCtrls,
  Controls,
  ExtCtrls,
  Classes,
  // Project
  DB.USnippet,
  FmWizardDlg,
  FrBrowserBase,
  FrCheckedTV,
  FrFixedHTMLDlg,
  FrHTMLDlg,
  FrSelectSnippets,
  UBaseObjects,
  UEncodings,
  UExceptions;


type

  {
  TCodeSubmitDlg:
    Implements a wizard dialog that gathers data about and submits a user's
    code submission for inclusion in the database.
  }
  TCodeSubmitDlg = class(TWizardDlg, INoPublicConstruct)
    btnPreview: TButton;
    edComments: TMemo;
    edEMail: TEdit;
    edName: TEdit;
    frmSnippets: TSelectSnippetsFrame;
    lblComments: TLabel;
    lblEmail: TLabel;
    lblFinished: TLabel;
    lblIntro: TLabel;
    lblName: TLabel;
    lblSnippetPrompt: TLabel;
    lblSnippets: TLabel;
    lblSubmit: TLabel;
    tsFinished: TTabSheet;
    tsIntro: TTabSheet;
    tsSnippets: TTabSheet;
    tsUserInfo: TTabSheet;
    tsSubmit: TTabSheet;
    frmPrivacy: TFixedHTMLDlgFrame;
    procedure btnPreviewClick(Sender: TObject);
  strict private
    var
      fData: TEncodedData; // Contains submission as XML document
    procedure SelectSnippet(const Snippet: TSnippet);
      {Selects the specified snippet in the check list of snippets or clears
      selections.
        @param Snippet [in] Snippet to be selected in the list. If Snippet is
          nil then list is cleared of selections.
      }
    procedure SnippetListChange(Sender: TObject);
      {Handles change events in list of snippets. Updates state of button on
      snippet page and display of prompt if there is an error.
        @param Sender [in] Not used.
      }
    procedure FocusFirstControl(const PageIdx: Integer);
      {Focusses first control on page if necessary.
        @param ParamIdx [in] Index of page for which focus is required.
      }
    procedure ValidatePage(PageIdx: Integer);
      {Validates user entries on wizard pages.
        @param PageIdx [in] Index of page to be validated.
        @except Raises EDataEntry exceptions with reference to relevant when an
          error is encountered.
      }
    procedure BuildSubmission;
      {Builds XML document containing details of submission and stores in a
      stream.
      }
    procedure DoSubmit;
      {Attempts to submit the XML code to the DelphiDabbler website.
        @except ECodeSubmitDlg raised on web service or script errors.
      }
    procedure SaveUserData;
      {Saves content of some wizard controls to persistent storage.
      }
  strict protected
    procedure ArrangeForm; override;
      {Aligns controls vertically where necessary to accomodate height of
      controls that depend on UI font.
      }
    procedure ConfigForm; override;
      {Loads required HTML into HTML frame and modified fonts where required.
      }
    procedure InitForm; override;
      {Initialises some wizard controls from persistent data.
      }
    function HeadingText(const PageIdx: Integer): string; override;
      {Gets text to be displayed in a wizard page.
        @param PageIdx [in] Page for which heading is required.
        @return The required heading.
      }
    procedure UpdateButtons(const PageIdx: Integer); override;
      {Updates wizard buttons depending on current page and state.
        @param PageIdx [in] Index of current page.
      }
    procedure MoveForward(const PageIdx: Integer; var CanMove: Boolean);
      override;
      {Called when about to move forward to a new page. Prevents movement if
      there is an error on the current page. Handles entry errors by refocussing
      control where error occured.
        @param PageIdx [in] Index of page we are about move to.
        @param CanMove [in/out] Flag indicating whether page change is allowed.
          Defaults to true.
      }
    procedure BeginPage(const PageIdx: Integer); override;
      {Called when a wizard page is first displayed. Focusses first control and
      performs any action required.
        @param PageIdx [in] Index of page to be initialised.
      }
    constructor InternalCreate(AOwner: TComponent); override;
      {Protected class constructor. Initialise objects required by this wizard.
      }
  public
    class procedure Execute(const AOwner: TComponent; const Snippet: TSnippet);
      {Excutes code submission dialog box. Submits code snippet to DelphiDabbler
      web service if user OKs.
        @param AOwner [in] Component that owns and parent's dialog box.
        @param Snippet [in] Reference to any snippet to be selected in snippets
          list. If nil nothing is selected.
      }
  end;

  {
  ECodeSubmitDlg:
    Class of exception raised by errors in TCodeSubmitDlg.
  }
  ECodeSubmitDlg = class(ECodeSnip);


implementation


uses
  // Delphi
  Graphics,
  // Project
  FmPreviewDlg,
  UCodeImportExport,
  UCtrlArranger,
  UEmailHelper,
  UMessageBox,
  UStrUtils,
  UUserDetails,
  UUserDetailsPersist,
  Web.UCodeSubmitter,
  Web.UExceptions;


{$R *.dfm}

const
  // Indices of wizard pages
  cIntroPageIdx = 0;
  cSnippetsPageIdx = 1;
  cUserInfoPageIdx = 2;
  cSubmitPageIdx = 3;
  cFinishPageIdx = 4;


{ TCodeSubmitDlg }

procedure TCodeSubmitDlg.ArrangeForm;
  {Aligns controls vertically where necessary to accomodate height of controls
  that depend on UI font.
  }
begin
  inherited;
  TCtrlArranger.SetLabelHeights(Self);
  // tsIntro
  { nothing to do }
  // tsSnippets
  lblSnippetPrompt.Top := tsSnippets.Height - lblSnippetPrompt.Height - 0;
  frmSnippets.Top := TCtrlArranger.BottomOf(lblSnippets, 4);
  frmSnippets.Height := lblSnippetPrompt.Top - frmSnippets.Top - 8;
  // tsUserInfo
  frmPrivacy.Top := TCtrlArranger.BottomOf(edEmail, 8);
  frmPrivacy.Height := frmPrivacy.DocHeight;
  lblComments.Top := TCtrlArranger.BottomOf(frmPrivacy, 8);
  edComments.Top := TCtrlArranger.BottomOf(lblComments, 4);
  edComments.Height := tsUserInfo.Height - edComments.Top;
  // tsSubmit
  btnPreview.Top := TCtrlArranger.BottomOf(lblSubmit, 8);
  // tsFinished
  { nothing to do }
end;

procedure TCodeSubmitDlg.BeginPage(const PageIdx: Integer);
  {Called when a wizard page is first displayed. Focusses first control and
  performs any action required.
    @param PageIdx [in] Index of page to be initialised.
  }
begin
  FocusFirstControl(PageIdx);
  case PageIdx of
    cSubmitPageIdx: BuildSubmission;
    cFinishPageIdx: SaveUserData;
  end;
end;

procedure TCodeSubmitDlg.btnPreviewClick(Sender: TObject);
  {Handles Preview button click event. Displays XML data to be submitted in a
  preview dialog box.
    @param Sender [in] Not used.
  }
begin
  TPreviewDlg.Execute(Self, fData, dtPlainText);
end;

procedure TCodeSubmitDlg.BuildSubmission;
  {Builds XML document containing details of submission and stores in a stream.
  }
begin
  Assert(not frmSnippets.SelectedSnippets.IsEmpty,
    ClassName + '.BuildSubmission: No snippets selected');
  Assert(edName.Text <> '',
    ClassName + '.BuildSubmission: No user name provided');
  Assert(IsValidEmailAddress(StrTrim(edEmail.Text)),
    ClassName + '.BuildSubmission: Invalid or no email address specified');
  // Build the document
  fData := TCodeExporter.ExportSnippets(
    TUserInfo.Create(
      TUserDetails.Create(edName.Text, edEmail.Text),
      StrTrim(edComments.Text)
    ),
    frmSnippets.SelectedSnippets
  );
end;

procedure TCodeSubmitDlg.ConfigForm;
  {Loads required HTML into HTML frame and modified fonts where required.
  }
begin
  inherited;
  pcWizard.ActivePage := tsUserInfo;  // show page so that HTML can load
  frmPrivacy.Initialise('frm-emailprivacy.html');
  lblSnippetPrompt.Font.Style := [fsBold];
  frmSnippets.CanCollapse := True;
end;

procedure TCodeSubmitDlg.DoSubmit;
  {Attempts to submit the XML code to the DelphiDabbler website.
    @except ECodeSubmitDlg raised on web service or script errors.
  }
resourcestring
  // Web service / server error meesage
  sWebServerError = 'Submission failed because web server reported '
    + 'HTTP Error %0:d: %1:s';
var
  WebSvc: TCodeSubmitter;  // communicates with web service
begin
  try
    // Do the submission
    WebSvc := TCodeSubmitter.Create;
    try
      Screen.Cursor := crHourglass;
      Enabled := False;
      // POST the data
      WebSvc.SubmitData(fData.Data);
    finally
      WebSvc.Free;
      Enabled := True;
      Screen.Cursor := crDefault;
    end;
  except
    // handle any exceptions from submission: we convert expected exceptions to
    // ECodeSubmitDlg to save triggering this dialog again: others are re-raised
    on E: EHTTPError do
      // error on web server: make more friendly
      raise ECodeSubmitDlg.CreateFmt(
        sWebServerError, [E.HTTPErrorCode, StrTrim(E.Message)]
      );
  end;
end;

class procedure TCodeSubmitDlg.Execute(const AOwner: TComponent;
  const Snippet: TSnippet);
  {Excutes code submission dialog box. Submits code snippet to DelphiDabbler
  web service if user OKs.
    @param AOwner [in] Component that owns and parent's dialog box.
    @param Snippet [in] Reference to any snippet to be selected in snippets
      list. If nil nothing is selected.
  }
begin
  with InternalCreate(AOwner) do
    try
      SelectSnippet(Snippet);
      ShowModal;
    finally
      Free;
    end;
end;

procedure TCodeSubmitDlg.FocusFirstControl(const PageIdx: Integer);
  {Focusses first control on page if necessary.
    @param ParamIdx [in] Index of page for which focus is required.
  }
begin
  case PageIdx of
    cSnippetsPageIdx: frmSnippets.SetFocus;
    cUserInfoPageIdx: edName.SetFocus;
  end;
end;

function TCodeSubmitDlg.HeadingText(const PageIdx: Integer): string;
  {Gets text to be displayed in a wizard page.
    @param PageIdx [in] Page for which heading is required.
    @return The required heading.
  }
resourcestring
  // Pages headings
  sIntroHeading = 'Submit code to the online database';
  sSnippetsHeading = 'Select snippets';
  sUserInfoHeading = 'About you';
  sSubmitHeading = 'Ready to submit';
  sFinishHeading = 'Submission complete';
begin
  case PageIdx of
    cIntroPageIdx:      Result := sIntroHeading;
    cSnippetsPageIdx:   Result := sSnippetsHeading;
    cUserInfoPageIdx:   Result := sUserInfoHeading;
    cSubmitPageIdx:     Result := sSubmitHeading;
    cFinishPageIdx:     Result := sFinishHeading;
  end;
end;

procedure TCodeSubmitDlg.InitForm;
  {Initialises some wizard controls from persistent data.
  }
var
  UserDetails: TUserDetails;  // user details from settings
begin
  inherited;
  UserDetails := TUserDetailsPersist.Load;
  edName.Text := UserDetails.Name;
  edEmail.Text := UserDetails.Email;
end;

constructor TCodeSubmitDlg.InternalCreate(AOwner: TComponent);
  {Protected class constructor. Initialise objects required by this wizard.
  }
begin
  inherited;
  frmSnippets.OnChange := SnippetListChange;
end;

procedure TCodeSubmitDlg.MoveForward(const PageIdx: Integer;
  var CanMove: Boolean);
  {Called when about to move forward to a new page. Prevents movement if there
  is an error on the current page. Handles entry errors by refocussing control
  where error occured.
    @param PageIdx [in] Index of page we are about move to.
    @param CanMove [in/out] Flag indicating whether page change is allowed.
      Defaults to true.
  }
begin
  CanMove := False;
  try
    ValidatePage(PageIdx);
    case PageIdx of
      cSubmitPageIdx: DoSubmit;
    end;
    CanMove := True;
  except
    on E: EDataEntry do
    begin
      // Error occurred in control: refocus it
      TMessageBox.Error(Self, E.Message);
      if Assigned(E.Ctrl) then
        E.Ctrl.SetFocus;
    end;
  end;
end;

procedure TCodeSubmitDlg.SaveUserData;
  {Saves content of some wizard controls to persistent storage.
  }
begin
  TUserDetailsPersist.Update(TUserDetails.Create(edName.Text, edEMail.Text));
end;

procedure TCodeSubmitDlg.SelectSnippet(const Snippet: TSnippet);
  {Selects the specified snippet in the check list of snippets or clears
  selections.
    @param Snippet [in] Snippet to be selected in the list. If Snippet is nil
      then list is cleared of selections.
  }
var
  List: TSnippetList; // list containing only one snippet
begin
  if not Assigned(Snippet) then
    frmSnippets.SelectedSnippets := nil
  else
  begin
    List := TSnippetList.Create;
    try
      List.Add(Snippet);
      frmSnippets.SelectedSnippets := List;
    finally
      List.Free;
    end;
  end;
end;

procedure TCodeSubmitDlg.SnippetListChange(Sender: TObject);
  {Handles change events in list of snippets. Updates state of button on snippet
  page and display of prompt if there is an error.
    @param Sender [in] Not used.
  }
begin
  if CurrentPage = cSnippetsPageIdx then
    UpdateButtons(CurrentPage);
  lblSnippetPrompt.Visible := frmSnippets.SelectedSnippets.IsEmpty;
end;

procedure TCodeSubmitDlg.UpdateButtons(const PageIdx: Integer);
  {Updates wizard buttons depending on current page and state.
    @param PageIdx [in] Index of current page.
  }
resourcestring
  sSubmitBtnCaption = '&Submit';  // submit button caption
begin
  inherited;
  // We change button caption on submit page
  case PageIdx of
    cSubmitPageIdx: btnNext.Caption := sSubmitBtnCaption;
  end;
end;

procedure TCodeSubmitDlg.ValidatePage(PageIdx: Integer);
  {Validates user entries on wizard pages.
    @param PageIdx [in] Index of page to be validated.
    @except Raises EDataEntry exceptions with reference to relevant when an
      error is encountered.
  }
resourcestring
  // Error messages
  sNoSnippets = 'Please select at least one snippet';
  sNoName = 'Please enter your name or nickname';
  sNoEmail = 'Please enter an email address';
  sBadEmail = 'Email address is not valid';
begin
  case PageIdx of
    cSnippetsPageIdx:
      if frmSnippets.SelectedSnippets.Count = 0 then
        raise EDataEntry.Create(sNoSnippets, frmSnippets);
    cUserInfoPageIdx:
    begin
      if edName.Text = '' then
        raise EDataEntry.Create(sNoName, edName);
      if edEmail.Text = '' then
        raise EDataEntry.Create(sNoEmail, edEmail);
      if not IsValidEmailAddress(StrTrim(edEmail.Text)) then
        raise EDataEntry.Create(sBadEmail, edEmail);
    end;
  end;
end;

end.


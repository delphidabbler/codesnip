{
 * FmCodeSubmitDlg.pas
 *
 * Implements a wizard dialog that gathers data about and submits a user's code
 * submission for inclusion in the database.
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
 * The Original Code is FmCodeSubmitDlg.pas
 *
 * The Initial Developer of the Original Code is Peter Johnson
 * (http://www.delphidabbler.com/).
 *
 * Portions created by the Initial Developer are Copyright (C) 2008-2010 Peter
 * Johnson. All Rights Reserved.
 *
 * Contributor(s)
 *   NONE
 *
 * ***** END LICENSE BLOCK *****
}


unit FmCodeSubmitDlg;


interface


uses
  // Delphi
  StdCtrls, Forms, ComCtrls, Controls, ExtCtrls, Classes,
  // Project
  FmWizardDlg, FrBrowserBase, FrCheckedTV, FrFixedHTMLDlg, FrHTMLDlg,
  FrSelectSnippetsBase, FrSelectUserSnippets, UBaseObjects, UExceptions,
  USnippets;


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
    frmRoutines: TSelectUserSnippetsFrame;
    lblComments: TLabel;
    lblEmail: TLabel;
    lblFinished: TLabel;
    lblIntro: TLabel;
    lblName: TLabel;
    lblRoutinePrompt: TLabel;
    lblRoutines: TLabel;
    lblSubmit: TLabel;
    tsFinished: TTabSheet;
    tsIntro: TTabSheet;
    tsRoutines: TTabSheet;
    tsUserInfo: TTabSheet;
    tsSubmit: TTabSheet;
    frmPrivacy: TFixedHTMLDlgFrame;
    procedure btnPreviewClick(Sender: TObject);
  strict private
    fData: TStream; // Stream containing XML document describing submission
    procedure SelectRoutine(const Routine: TRoutine);
      {Selects the specified snippet in the check list of snippets or clears
      selections.
        @param Routine [in] Snippet to be selected in the list. If Routine is
          nil then list is cleared of selections.
      }
    procedure RoutineListChange(Sender: TObject);
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
    destructor Destroy; override;
      {Class destructor. Tears down object.
      }
    class procedure Execute(const AOwner: TComponent; const Routine: TRoutine);
      {Excutes code submission dialog box. Submits code snippet to DelphiDabbler
      web service if user OKs.
        @param AOwner [in] Component that owns and parent's dialog box.
        @param Routine [in] Reference to any snippet to be selected in snippets
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
  SysUtils, Graphics,
  // Project
  FmPreviewDlg, UCodeImportExport, UCtrlArranger,
  UEmailHelper, UMessageBox, UUserDetails, Web.UCodeSubmitter, Web.UExceptions;


{$R *.dfm}

const
  // Indices of wizard pages
  cIntroPageIdx = 0;
  cRoutinesPageIdx = 1;
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
  // tsRoutines
  lblRoutinePrompt.Top := tsRoutines.Height - lblRoutinePrompt.Height - 0;
  frmRoutines.Top := TCtrlArranger.BottomOf(lblRoutines, 4);
  frmRoutines.Height := lblRoutinePrompt.Top - frmRoutines.Top - 8;
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
var
  SS: TStringStream;  // stream used to pass XML data to preview dialog
begin
  SS := TStringStream.Create('');
  try
    SS.CopyFrom(fData, 0);
    TPreviewDlg.Execute(Self, SS.DataString);
    fData.Position := 0;
  finally
    FreeAndNil(SS);
  end;
end;

procedure TCodeSubmitDlg.BuildSubmission;
  {Builds XML document containing details of submission and stores in a stream.
  }
begin
  Assert(frmRoutines.SelectedRoutines.Count > 0,
    ClassName + '.BuildSubmission: No snippets selected');
  Assert(edName.Text <> '',
    ClassName + '.BuildSubmission: No user name provided');
  Assert(IsValidEmailAddress(Trim(edEmail.Text)),
    ClassName + '.BuildSubmission: Invalid or no email address specified');
  // Build the document
  fData.Size := 0;
  TCodeExporter.ExportRoutines(
    TUserInfo.Create(
      TUserDetails.Create(edName.Text, edEmail.Text), Trim(edComments.Text)
    ),
    frmRoutines.SelectedRoutines,
    fData
  );
  fData.Position := 0;
end;

procedure TCodeSubmitDlg.ConfigForm;
  {Loads required HTML into HTML frame and modified fonts where required.
  }
begin
  inherited;
  pcWizard.ActivePage := tsUserInfo;  // show page so that HTML can load
  frmPrivacy.Initialise('frm-emailprivacy.html');
  lblRoutinePrompt.Font.Style := [fsBold];
end;

destructor TCodeSubmitDlg.Destroy;
  {Class destructor. Tears down object.
  }
begin
  FreeAndNil(fData);
  inherited;
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
      fData.Position := 0;
      WebSvc.SubmitData(fData);
    finally
      FreeAndNil(WebSvc);
      Enabled := True;
      Screen.Cursor := crDefault;
    end;
  except
    // handle any exceptions from submission: we convert expected exceptions to
    // ECodeSubmitDlg to save triggering this dialog again: others are re-raised
    on E: EHTTPError do
      // error on web server: make more friendly
      raise ECodeSubmitDlg.CreateFmt(
        sWebServerError, [E.HTTPErrorCode, Trim(E.Message)]
      );
  end;
end;

class procedure TCodeSubmitDlg.Execute(const AOwner: TComponent;
  const Routine: TRoutine);
  {Excutes code submission dialog box. Submits code snippet to DelphiDabbler
  web service if user OKs.
    @param AOwner [in] Component that owns and parent's dialog box.
    @param Routine [in] Reference to any snippet to be selected in snippets
      list. If nil nothing is selected.
  }
begin
  with InternalCreate(AOwner) do
    try
      SelectRoutine(Routine);
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
    cRoutinesPageIdx:   frmRoutines.SetFocus;
    cUserInfoPageIdx:   edName.SetFocus;
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
  sRoutinesHeading = 'Select snippets';
  sUserInfoHeading = 'About you';
  sSubmitHeading = 'Ready to submit';
  sFinishHeading = 'Submission complete';
begin
  case PageIdx of
    cIntroPageIdx:      Result := sIntroHeading;
    cRoutinesPageIdx:   Result := sRoutinesHeading;
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
  frmRoutines.OnChange := RoutineListChange;
  fData := TMemoryStream.Create;
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

procedure TCodeSubmitDlg.RoutineListChange(Sender: TObject);
  {Handles change events in list of snippets. Updates state of button on snippet
  page and display of prompt if there is an error.
    @param Sender [in] Not used.
  }
begin
  if CurrentPage = cRoutinesPageIdx then
    UpdateButtons(CurrentPage);
  lblRoutinePrompt.Visible := frmRoutines.SelectedRoutines.Count = 0;
end;

procedure TCodeSubmitDlg.SaveUserData;
  {Saves content of some wizard controls to persistent storage.
  }
begin
  TUserDetailsPersist.Update(TUserDetails.Create(edName.Text, edEMail.Text));
end;

procedure TCodeSubmitDlg.SelectRoutine(const Routine: TRoutine);
  {Selects the specified routine in the check list of routines or clears
  selections.
    @param Routine [in] Snippet to be selected in the list. If Snippet is nil
      then list is cleared of selections.
  }
var
  List: TRoutineList; // list containing only one snippet
begin
  if not Assigned(Routine) or not Routine.UserDefined then
    frmRoutines.SelectedRoutines := nil
  else
  begin
    List := TRoutineList.Create;
    try
      List.Add(Routine);
      frmRoutines.SelectedRoutines := List;
    finally
      FreeAndNil(List);
    end;
  end;
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
  sNoRoutines = 'Please select at least one snippet';
  sNoName = 'Please enter your name or nickname';
  sNoEmail = 'Please enter an email address';
  sBadEmail = 'Email address is not valid';
begin
  case PageIdx of
    cRoutinesPageIdx:
      if frmRoutines.SelectedRoutines.Count = 0 then
        raise EDataEntry.Create(sNoRoutines, frmRoutines);
    cUserInfoPageIdx:
    begin
      if edName.Text = '' then
        raise EDataEntry.Create(sNoName, edName);
      if edEmail.Text = '' then
        raise EDataEntry.Create(sNoEmail, edEmail);
      if not IsValidEmailAddress(Trim(edEmail.Text)) then
        raise EDataEntry.Create(sBadEmail, edEmail);
    end;
  end;
end;

end.


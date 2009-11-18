{
 * FmRegistrationDlg.pas
 *
 * Wizard style dialog box that collects information and sends application
 * registration to web server.
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
 * The Original Code is FmRegistrationDlg.pas
 *
 * The Initial Developer of the Original Code is Peter Johnson
 * (http://www.delphidabbler.com/).
 *
 * Portions created by the Initial Developer are Copyright (C) 2006-2009 Peter
 * Johnson. All Rights Reserved.
 *
 * Contributor(s)
 *   NONE
 *
 * ***** END LICENSE BLOCK *****
}


unit FmRegistrationDlg;


interface


uses
  // Delphi
  StdCtrls, ComCtrls, Controls, ExtCtrls, Classes, Forms,
  // Project
  FmWizardDlg, FrBrowserBase, FrFixedHTMLDlg, FrHTMLDlg, UBaseObjects,
  UCSSBuilder;


type

  {
  TRegistrationDlg:
    Wizard style dialog box that collects information and sends application
    registration to web server.
  }
  TRegistrationDlg = class(TWizardDlg, INoPublicConstruct)
    chkMailList: TCheckBox;
    edEmail: TEdit;
    edName: TEdit;
    edRegCode: TEdit;
    edReport: TMemo;
    frmMailListIntro: TFixedHTMLDlgFrame;
    frmPrivacy: TFixedHTMLDlgFrame;
    gbRequired: TGroupBox;
    gbMailList: TGroupBox;
    lblEmail: TLabel;
    lblInstructions: TLabel;
    lblIntro: TLabel;
    lblIntroExplain: TLabel;
    lblMailListConfirm: TLabel;
    lblName: TLabel;
    lblRegCode: TLabel;
    lblReport: TLabel;
    lblSubmit: TLabel;
    lblThanks: TLabel;
    tsAboutUser: TTabSheet;
    tsFinish: TTabSheet;
    tsIntro: TTabSheet;
    tsSubmit: TTabSheet;
    procedure chkMailListClick(Sender: TObject);
  strict private
    fRegistered: Boolean; // Flag indicating whether program was registered
    procedure BuildCSS(Sender: TObject; const CSSBuilder: TCSSBuilder);
      {Sets CSS for required font for use in HTML frames.
        @param Sender [in] Not used.
        @param CSSBuilder [in] Object used to create CSS.
      }
    procedure BuildSubmission(const Report: TStrings);
      {Builds registration submission as list of values in name=value format.
        @param Report [in] Stores submission on completion.
      }
    procedure DoRegistration;
      {Registers program and displays registration code.
      }
    function RegisterWithWebServer: string;
      {Gathers required registration data, sends to web server and gets
      registration code from it.
        @return Registration code.
        @except EWebService exception raised if there is a failure in
          interaction with web service.
      }
    function ValidateUserInfo: Boolean;
      {Validates data entered by user. Displays any error messages.
        @return True if data valid, false if not.
      }
  strict protected
    procedure ArrangeForm; override;
      {Vertically arranges controls as required and sizes the tab sheets to be
      able to display the longest page.
      }
    procedure ConfigForm; override;
      {Sets font styles where necessary and initialises HTML frames.
      }
    procedure InitForm; override;
      {Initialises controls.
      }
    procedure BeginPage(const PageIdx: Integer); override;
      {Performs required initialisation when a page is displayed.
        @param PageIdx [in] Index page to be initialised.
      }
    function HeadingText(const PageIdx: Integer): string; override;
      {Gets text of heading of a wizard page.
        @param PageIdx [in] Index of page for which heading is required.
        @return Heading text.
      }
    procedure MoveForward(const PageIdx: Integer;
      var CanMove: Boolean); override;
      {Performs required processing on moving forward from pages.
        @param PageIdx [in] Index of page we are leaving.
        @param CanMove [in/out] Flag set to indicate whether can leave page.
          Defaults to true.
      }
    procedure UpdateButtons(const PageIdx: Integer); override;
      {Updates wizard buttons depending on page and state.
        @param PageIdx [in] Index of current page.
      }
  public
    class function Execute(const Owner: TComponent): Boolean;
      {Displays dialog box.
        @param Owner [in] Component that owns this dialog.
        @return True if program was registered, false otherwise.
      }
  end;


implementation


uses
  // Delphi
  SysUtils, Graphics, Math,
  // Project
  UAppInfo, UCSSUtils, UEmailHelper, UFontHelper, UCtrlArranger, UMessageBox,
  URegistrar, USystemInfo, UUserDetails;


{$R *.dfm}

const
  // Index of wizard pages
  cIntroPageIdx = 0;
  cAboutUserPageIdx = 1;
  cSubmitPageIdx = 2;
  cFinishPageIdx = 3;

resourcestring
  // Pages headings
  sIntroHeading = 'Register CodeSnip';
  sAboutUserHeading = 'About you';
  sSubmitHeading = 'Submit registration';
  sFinishHeading = 'Registration complete';
  // Submit button caption
  sSubmitBtnCaption = '&Submit';
  // Mailing list confirmation message
  sMailListConfirm = 'You chose to subscribe the the CodeSnip mailing list. '
    + 'For your own protection you need to confirm your subscription. '
    + 'An email has been sent to %s that explains how to do this.';
  // Error messages
  sErrNameRequired = 'You need to provide your name or a nickname.';
  sErrEmailRequired = 'Your email address is needed to subscribe to the '
    + 'mailing list.';
  sErrEmailInvalid = 'Your email address does not appear to be valid.';


{ TRegistrationDlg }

procedure TRegistrationDlg.ArrangeForm;
  {Vertically arranges controls as required and sizes the tab sheets to be able
  to display the longest page.
  }
begin
  // set heights of all labels with AutoSize = False
  TCtrlArranger.SetLabelHeights(Self);

  // tsIntro tabsheet
  lblIntroExplain.Top := TCtrlArranger.BottomOf(lblIntro, 8);
  lblInstructions.Top := TCtrlArranger.BottomOf(lblIntroExplain, 8);

  // tsAboutUser tabsheet
  edName.Top := TCtrlArranger.BottomOf(lblName, 8);
  gbRequired.ClientHeight := TCtrlArranger.BottomOf(edName, 12);
  gbMailList.Top := TCtrlArranger.BottomOf(gbRequired, 8);
  frmMailListIntro.Height := frmMailListIntro.DocHeight;
  chkMailList.Top := TCtrlArranger.BottomOf(frmMailListIntro, 8);
  TCtrlArranger.AlignVCentres(
    TCtrlArranger.BottomOf(chkMailList, 8),
    [lblEmail, edEmail]
  );
  frmPrivacy.Top := TCtrlArranger.BottomOf([lblEmail, edEmail], 8);
  frmPrivacy.Height := frmPrivacy.DocHeight;
  gbMailList.ClientHeight := TCtrlArranger.BottomOf(frmPrivacy, 12);

  // tsSubmit tabsheet
  edReport.Top := TCtrlArranger.BottomOf(lblReport, 8);
  lblSubmit.Top := TCtrlArranger.BottomOf(edReport, 8);

  // tsFinish tabsheet
  lblRegCode.Top := TCtrlArranger.BottomOf(lblThanks, 8);
  edRegCode.Top := TCtrlArranger.BottomOf(lblRegCode, 4);
  lblMailListConfirm.Top := TCtrlArranger.BottomOf(edRegCode, 8);

  // set required height
  pnlBody.ClientHeight := TCtrlArranger.MaxContainerHeight(
    [tsAboutUser, tsFinish, tsIntro, tsSubmit]
  ) + pnlBody.ClientHeight - tsAboutUser.Height;

  // Arrange inherited controls and size the form
  inherited;
end;

procedure TRegistrationDlg.BeginPage(const PageIdx: Integer);
  {Performs required initialisation when a page is displayed.
    @param PageIdx [in] Index page to be initialised.
  }
begin
  inherited;
  case PageIdx of
    cSubmitPageIdx:
      // Create data to be sent to website ready for submission
      BuildSubmission(edReport.Lines);
    cAboutUserPageIdx:
      // Focus first control on page
      edName.SetFocus;
    cFinishPageIdx:
    begin
      // Set up confirmation messages
      if chkMailList.Checked then
      begin
        lblMailListConfirm.Caption := Format(
          sMailListConfirm, [Trim(edEmail.Text)]
        );
        lblMailListConfirm.Visible := True;
      end
      else
        lblMailListConfirm.Visible := False;
    end;
  end;
end;

procedure TRegistrationDlg.BuildCSS(Sender: TObject;
  const CSSBuilder: TCSSBuilder);
  {Sets CSS for required font for use in HTML frames.
    @param Sender [in] Not used.
    @param CSSBuilder [in] Object used to create CSS.
  }
var
  DefaultFont: TFont; // default font for OS
begin
  DefaultFont := TFont.Create;
  try
    TFontHelper.SetDefaultFont(DefaultFont, False);
    with CSSBuilder.Selectors['body'] do
      AddProperty(CSSFontProps(DefaultFont));
  finally
    FreeAndNil(DefaultFont);
  end;
end;

procedure TRegistrationDlg.BuildSubmission(const Report: TStrings);
  {Builds registration submission as list of values in name=value format.
    @param Report [in] Stores submission on completion.
  }
begin
  Report.Clear;
  Report.Values['ProgId'] := TAppInfo.ProgramID;
  Report.Values['ProgName'] := TAppInfo.ProgramName;
  Report.Values['ProgVer'] := TAppInfo.ProgramReleaseVersion;
  Report.Values['ProgKey'] := TAppInfo.ProgramKey;
  Report.Values['UserName'] := Trim(edName.Text);
  Report.Values['OSDesc'] := TOSInfo.Description;
  Report.Values['MailList'] := IntToStr(Ord(chkMailList.Checked));
  if chkMailList.Checked then
    Report.Values['UserEmail'] := Trim(edEmail.Text)
  else
    Report.Values['UserEmail'] := '';
end;

procedure TRegistrationDlg.chkMailListClick(Sender: TObject);
  {Updates state and focus of email address edit box as check box is checked and
  cleared.
    @param Sender [in] Not used.
  }
begin
  if chkMailList.Checked then
  begin
    edEmail.Enabled := True;
    edEmail.Color := clWindow;
    edEmail.SetFocus;
  end
  else
  begin
    edEmail.Enabled := False;
    edEmail.ParentColor := True;
  end;
end;

procedure TRegistrationDlg.ConfigForm;
  {Sets font styles where necessary and initialises HTML frames.
  }

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

begin
  inherited;
  gbRequired.Font.Style := [fsBold];
  gbMailList.Font.Style := [fsBold];
  edName.Font.Style := [];
  lblName.Font.Style := [];
  chkMailList.Font.Style := [];
  edEMail.Font.Style := [];
  lblEmail.Font.Style := [];
  TFontHelper.SetDefaultMonoFont(edRegCode.Font, False);
  frmMailListIntro.OnBuildCSS := BuildCSS;
  frmPrivacy.OnBuildCSS := BuildCSS;
  LoadHTMLFrame(frmMailListIntro, 'dlg-registration-maillist.html');
  LoadHTMLFrame(frmPrivacy, 'frm-emailprivacy.html');
end;

procedure TRegistrationDlg.DoRegistration;
  {Registers program and displays registration code.
  }
var
  UserDetails: TUserDetails;  // information about user
begin
  Screen.Cursor := crHourglass;
  try
    // register with server
    edRegCode.Text := RegisterWithWebServer;
    // record registration & user details
    UserDetails := TUserDetails.Create(Trim(edName.Text), Trim(edEmail.Text));
    TAppInfo.RegisterProgram(edRegCode.Text, UserDetails.Name);
    TUserDetailsPersist.Update(UserDetails);
    fRegistered := True;
  finally
    Screen.Cursor := crDefault;
  end;
end;

class function TRegistrationDlg.Execute(const Owner: TComponent): Boolean;
  {Displays dialog box.
    @param Owner [in] Component that owns this dialog.
    @return True if program was registered, false otherwise.
  }
begin
  with InternalCreate(Owner) do
    try
      ShowModal;
      Result := fRegistered;
    finally
      Free;
    end;
end;

function TRegistrationDlg.HeadingText(const PageIdx: Integer): string;
  {Gets text of heading of a wizard page.
    @param PageIdx [in] Index of page for which heading is required.
    @return Heading text.
  }
const
  // Map of page indexes to page heading
  cPageHeadings: array[0..3] of string = (
    sIntroHeading, sAboutUserHeading, sSubmitHeading, sFinishHeading
  );
begin
  Result := cPageHeadings[PageIdx];
end;

procedure TRegistrationDlg.InitForm;
  {Initialises controls.
  }
var
  UserDetails: TUserDetails;  // any known information about user
begin
  inherited;
  // Use user name if known
  UserDetails := TUserDetailsPersist.Load;
  edName.Text := UserDetails.Name;
  edEmail.Text := UserDetails.Email;
end;

procedure TRegistrationDlg.MoveForward(const PageIdx: Integer;
  var CanMove: Boolean);
  {Performs required processing on moving forward from pages.
    @param PageIdx [in] Index of page we are leaving.
    @param CanMove [in/out] Flag set to indicate whether can leave page.
      Defaults to true.
  }
begin
  inherited;
  case PageIdx of
    cAboutUserPageIdx:
      CanMove := ValidateUserInfo;
    cSubmitPageIdx:
      DoRegistration;
  end;
end;

function TRegistrationDlg.RegisterWithWebServer: string;
  {Gathers required registration data, sends to web server and gets registration
  code from it.
    @return Registration code.
    @except EWebService exception raised if there is a failure in interaction
      with web service.
  }
var
  Reg: TRegistrar;        // object that communicates with web server
  PostData: TStringList;  // list of data items to be sent to web server
begin
  PostData := nil;
  Reg := TRegistrar.Create;
  try
    PostData := TStringList.Create;
    BuildSubmission(PostData);
    Result := Reg.Submit(PostData); // raises exception on fail result
  finally
    FreeAndNil(PostData);
    FreeAndNil(Reg);
  end;
end;

procedure TRegistrationDlg.UpdateButtons(const PageIdx: Integer);
  {Updates wizard buttons depending on page and state.
    @param PageIdx [in] Index of current page.
  }
begin
  inherited;
  if PageIdx = cSubmitPageIdx then
    btnNext.Caption := sSubmitBtnCaption;
  btnCancel.Enabled := PageIdx <> cFinishPageIdx;
end;

function TRegistrationDlg.ValidateUserInfo: Boolean;
  {Validates data entered by user. Displays any error messages.
    @return True if data valid, false if not.
  }

  // ---------------------------------------------------------------------------
  function ValidateEmailAddress(const Email: string): Boolean;
    {Checks that email address is valid by doing some basic checks on it.
    Displays any error messages.
      @param Email [in] Email address to check.
      @return True if address if OK, false if not.
    }
  begin
    Result := True;
    if Email = '' then
    begin
      Result := False;
      TMessageBox.Error(Self, sErrEmailRequired);
    end
    else if not IsValidEmailAddress(Email) then
    begin
      Result := False;
      TMessageBox.Error(Self, sErrEmailInvalid);
    end;
  end;
  // ---------------------------------------------------------------------------

begin
  Result := True;
  if Trim(edName.Text) = '' then
  begin
    Result := False;
    TMessageBox.Error(Self, sErrNameRequired);
  end
  else if chkMailList.Checked then
    Result := ValidateEmailAddress(Trim(edEmail.Text));
end;

end.


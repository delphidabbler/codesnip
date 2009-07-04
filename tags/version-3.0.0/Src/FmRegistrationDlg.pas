{
 * FmRegistrationDlg.pas
 *
 * Wizard style dialog box that collects information and sends application
 * registration to web server.
 *
 * v0.1 of 07 Apr 2006  - Original version.
 * v1.0 of 26 May 2006  - Improved and corrected comments.
 *                      - Replaced literal program name and id with constants
 *                        from UGlobals unit.
 * v1.1 of 16 Nov 2006  - Corrected and revised text displayed on final page of
 *                        when user elects to join mailing list.
 * v1.2 of 18 Nov 2006  - Corrected typo on "about user" page of wizard.
 *                      - Now intialises user name edit box if user name is
 *                        already recorded.
 * v1.3 of 08 Feb 2007  - Changed type of Owner parameter of Execute method from
 *                        TForm to TComponent.
 *                      - Removed unused code used to align dialog to active
 *                        form if owner was nil.
 *                      - Moved control initialisation code from FormCreate
 *                        event handler to new overridden InitForm method and
 *                        deleted FormCreate method.
 * v1.4 of 15 Dec 2008  - Replaced custom email address checking code with
 *                        routine from UEmailHelper unit.
 *                      - Made private and protected sections strict.
 * v1.5 of 13 May 2009  - Changed to use revised web service constructor.
 *                      - Removed reference to deleted UParams unit.
 *                      - Now gets program name and ID from TAppInfo instead of
 *                        UGlobals unit. 
 *
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
 * Portions created by the Initial Developer are Copyright (C) 2006-2008 Peter
 * Johnson. All Rights Reserved.
 *
 * ***** END LICENSE BLOCK *****
}


unit FmRegistrationDlg;


interface


uses
  // Delphi
  StdCtrls, ComCtrls, Controls, ExtCtrls, Classes,
  // Project
  FmWizardDlg;


type

  {
  TRegistrationDlg:
    Wizard style dialog box that collects information and sends application
    registration to web server.
  }
  TRegistrationDlg = class(TWizardDlg)
    chkMailList: TCheckBox;
    edEmail: TEdit;
    edName: TEdit;
    edRegCode: TEdit;
    edReport: TMemo;
    gbRequired: TGroupBox;
    gpMailList: TGroupBox;
    lblEmail: TLabel;
    lblInstructions: TLabel;
    lblIntro: TLabel;
    lblIntroExplain: TLabel;
    lblMailListConfirm: TLabel;
    lblMailListHelp: TLabel;
    lblMailListInto: TLabel;
    lblName: TLabel;
    lblPrivacy: TLabel;
    lblPrivacyHelp: TLabel;
    lblRegCode: TLabel;
    lblReport: TLabel;
    lblSubmit: TLabel;
    lblThanks: TLabel;
    tsAboutUser: TTabSheet;
    tsFinish: TTabSheet;
    tsIntro: TTabSheet;
    tsSubmit: TTabSheet;
    procedure chkMailListClick(Sender: TObject);
    procedure lblMailListHelpClick(Sender: TObject);
    procedure lblPrivacyHelpClick(Sender: TObject);
  strict private
    fRegistered: Boolean;
      {Flag indicating whether program was registered}
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
  SysUtils, Graphics, Forms,
  // Project
  UAppInfo, UEmailHelper, UMessageBox, URegistrar;


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
        lblMailListConfirm.Caption := Format(sMailListConfirm, [edEmail.Text]);
        lblMailListConfirm.Visible := True;
      end
      else
        lblMailListConfirm.Visible := False;
    end;
  end;
end;

procedure TRegistrationDlg.BuildSubmission(const Report: TStrings);
  {Builds registration submission as list of values in name=value format.
    @param Report [in] Stores submission on completion.
  }
begin
  // ** do not localise string literals in this method
  Report.Clear;
  Report.Values['ProgId'] := TAppInfo.ProgramID;
  Report.Values['ProgName'] := TAppInfo.ProgramName;
  Report.Values['ProgVer'] := TAppInfo.ProgramReleaseVersion;
  Report.Values['ProgKey'] := TAppInfo.ProgramKey;
  Report.Values['UserName'] := edName.Text;
  Report.Values['MailList'] := IntToStr(Ord(chkMailList.Checked));
  if chkMailList.Checked then
    Report.Values['UserEmail'] := edEmail.Text
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

procedure TRegistrationDlg.DoRegistration;
  {Registers program and displays registration code.
  }
begin
  Screen.Cursor := crHourglass;
  try
    edRegCode.Text := RegisterWithWebServer;
    TAppInfo.RegisterProgram(edRegCode.Text, edName.Text);
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
  with TRegistrationDlg.Create(Owner) do
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
begin
  inherited;
  // Use user name if known
  edName.Text := TAppInfo.RegisteredUser;
end;

procedure TRegistrationDlg.lblMailListHelpClick(Sender: TObject);
  {Displays mailing list information in help file.
    @param Sender [in] Not used.
  }
begin
  DisplayHelp('MailingList');       // ** do not localise
end;

procedure TRegistrationDlg.lblPrivacyHelpClick(Sender: TObject);
  {Displays privacy statement in help file.
    @param Sender [in] Not used.
  }
begin
  DisplayHelp('PrivacyStatement');  // ** do not localise
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
  if edName.Text = '' then
  begin
    Result := False;
    TMessageBox.Error(Self, sErrNameRequired);
  end
  else if chkMailList.Checked then
    Result := ValidateEmailAddress(edEmail.Text);
end;

end.


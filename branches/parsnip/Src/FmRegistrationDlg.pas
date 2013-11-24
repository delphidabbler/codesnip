{
 * This Source Code Form is subject to the terms of the Mozilla Public License,
 * v. 2.0. If a copy of the MPL was not distributed with this file, You can
 * obtain one at http://mozilla.org/MPL/2.0/
 *
 * Copyright (C) 2006-2013, Peter Johnson (www.delphidabbler.com).
 *
 * $Rev$
 * $Date$
 *
 * Implements a wizard style dialogue box that registers the program online.
}


unit FmRegistrationDlg;


interface


uses
  // Delphi
  StdCtrls, ComCtrls, Controls, ExtCtrls, Classes,
  // Project
  FmWizardDlg, UBaseObjects;


type

  {
  TRegistrationDlg:
    Wizard style dialog box that collects information and sends application
    registration to web server.
  }
  TRegistrationDlg = class(TWizardDlg, INoPublicConstruct)
    edName: TEdit;
    edRegCode: TEdit;
    edReport: TMemo;
    lblInstructions: TLabel;
    lblIntro: TLabel;
    lblIntroExplain: TLabel;
    lblName: TLabel;
    lblRegCode: TLabel;
    lblReport: TLabel;
    lblSubmit: TLabel;
    lblThanks: TLabel;
    tsAboutUser: TTabSheet;
    tsFinish: TTabSheet;
    tsIntro: TTabSheet;
    tsSubmit: TTabSheet;
  strict private
    fRegistered: Boolean; // Flag indicating whether program was registered
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
        @except EWebError exception raised if there is a failure in interaction
          with web service.
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
  SysUtils, Forms,
  // Project
  UAppInfo, UFontHelper, UCtrlArranger, UMessageBox, UStrUtils, USystemInfo,
  UUserDetails, UUserDetailsPersist, Web.URegistrar;


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
  // Error messages
  sErrNameRequired = 'You need to provide your name or a nickname.';


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

  // tsSubmit tabsheet
  edReport.Top := TCtrlArranger.BottomOf(lblReport, 8);
  lblSubmit.Top := TCtrlArranger.BottomOf(edReport, 8);

  // tsFinish tabsheet
  lblRegCode.Top := TCtrlArranger.BottomOf(lblThanks, 8);
  edRegCode.Top := TCtrlArranger.BottomOf(lblRegCode, 4);

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
  Report.Values['UserName'] := StrTrim(edName.Text);
  Report.Values['OSDesc'] :=
    Format(
      '%0:s. IE Version %1:d.', [TOSInfo.Description, TIEInfo.MajorVersion]
    );
end;

procedure TRegistrationDlg.ConfigForm;
  {Sets font styles where necessary and initialises HTML frames.
  }
begin
  inherited;
  TFontHelper.SetDefaultMonoFont(edRegCode.Font);
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
    UserDetails := TUserDetails.Create(StrTrim(edName.Text), '');
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
    @except EWebError exception raised if there is a failure in interaction
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
    PostData.Free;
    Reg.Free;
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
begin
  Result := True;
  if StrIsBlank(edName.Text) then
  begin
    Result := False;
    TMessageBox.Error(Self, sErrNameRequired);
  end;
end;

end.


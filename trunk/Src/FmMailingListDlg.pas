{
 * FmMailingListDlg.pas
 *
 * Dialog box that is used to subscribe to the CodeSnip mailing list.
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
 * The Original Code is FmMailingListDlg.pas
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


unit FmMailingListDlg;


interface


uses
  // Delphi
  StdCtrls, Controls, ExtCtrls, Classes, Forms,
  // Project
  FmGenericDlg, FrBrowserBase, FrFixedHTMLDlg, FrHTMLDlg, FrHTMLTpltDlg;


type

  {
  TMailingListDlg:
    Defines a dialog box that is used to subscribe to the CodeSnip mailing list.
  }
  TMailingListDlg = class(TGenericDlg)
    btnCancel: TButton;
    btnSubmit: TButton;
    edEmail: TEdit;
    edName: TEdit;
    frmResult: THTMLTpltDlgFrame;
    lblEmail: TLabel;
    lblName: TLabel;
    lblSubmit: TLabel;
    lblSubscribing: TLabel;
    pnlData: TPanel;
    pnlResult: TPanel;
    frmPrivacy: TFixedHTMLDlgFrame;
    frmMailList: TFixedHTMLDlgFrame;
    procedure btnSubmitClick(Sender: TObject);
  strict private
    procedure Subscribing(const Flag: Boolean);
      {Updates display according whether we are currently using web service to
      perform subscription.
        @param Flag [in] True if subscribing and false if not.
      }
    procedure ShowResultPane(const Msg: string);
      {Displays result pane and shows a message in it.
        @param Msg [in] Message to display in result pane.
      }
    procedure LoadResultAsHTML(const Msg: string);
      {Displays result message as HTML.
        @param Msg [in] Message to convert to HTML.
      }
    function ValidateEmailAddress: Boolean;
      {Checks that entered email address is valid by doing some basic checks on
      it. Displays any error messages.
        @return True if address if OK, false if not.
      }
    function Subscribe: string;
      {Attempts to subscribe user mailing list and handles message returned from
      web service.
        @return Message returned from web service.
      }
  strict protected
    procedure ConfigForm; override;
      {Initialises content of HTML frames and sets fonts as required.
      }
    procedure ArrangeForm; override;
      {Positions controls and sets form size according to body panel dimensions.
      }
    procedure InitForm; override;
      {Populates and initialises controls.
      }
    function ModalResultOnEsc: Integer; override;
      {Gets modal result returned from dialog when user presses ESC key.
        @return mrCancel.
      }
  public
    class procedure Execute(const AOwner: TComponent);
      {Displays dialog box.
        @param AOwner [in] Owning form.
      }
  end;


implementation


uses
  // Delphi
  SysUtils, Math,
  // Project
  UAppInfo, UConsts, UCtrlArranger, UEmailHelper, UFontHelper, UHTMLUtils,
  UMailListSubscriber, UMessageBox, UUtils;


{$R *.dfm}


resourcestring
  // Button caption
  sCloseBtnCaption = '&Finish';
  // Error messages
  sErrEmailRequired = 'Please provide your email address.';
  sErrEmailInvalid = 'Your email address does not appear to be valid.';


{ TMailingListDlg }

procedure TMailingListDlg.ArrangeForm;
  {Positions controls and sets form size according to body panel dimensions.
  }
begin
  // set control heights
  TCtrlArranger.SetLabelHeights(Self);
  frmMailList.Height := frmMailList.DocHeight;
  frmPrivacy.Height := frmPrivacy.DocHeight;
  // align controls
  TCtrlArranger.AlignVCentres(
    TCtrlArranger.BottomOf(frmMailList, 8), [lblEmail, edEmail]
  );
  frmPrivacy.Top := TCtrlArranger.BottomOf([lblEmail, edEmail]);
  TCtrlArranger.AlignVCentres(
    TCtrlArranger.BottomOf(frmPrivacy, 8), [lblName, edName]
  );
  lblSubmit.Top := TCtrlArranger.BottomOf([lblName, edName], 8);
  lblSubscribing.Top := lblSubmit.Top;
  // set body panel height
  pnlBody.ClientHeight := TCtrlArranger.TotalControlHeight(pnlData) + 8;
  // align inherited controls and size form
  inherited;
  // align buttons added in this dialog
  btnCancel.Left := btnHelp.Left - btnCancel.Width - 4;
  btnSubmit.Left := btnCancel.Left - btnSubmit.Width - 4;
  btnCancel.Top := btnHelp.Top;
  btnSubmit.Top := btnHelp.Top;
end;

procedure TMailingListDlg.btnSubmitClick(Sender: TObject);
  {Submit button handler. Validates user input then submits subscription request
  to web service and updates display and controls.
    @param Sender [in] Not used.
  }
var
  Response: string; // message sent in response to request by web service
begin
  try
    // Check user's entries
    if not ValidateEmailAddress then
      Exit;
    // Do subscription
    Subscribing(True);
    Response := Subscribe;
    ShowResultPane(Response);
  finally
    Subscribing(False);
  end;
end;

procedure TMailingListDlg.ConfigForm;
  {Initialises content of HTML frames and sets fonts as required.
  }
begin
  inherited;
  frmPrivacy.Initialise('frm-emailprivacy.html');
  frmMailList.Initialise('dlg-mlist-maillist.html');
  TFontHelper.SetDefaultBaseFont(lblSubscribing.Font, False);
end;

class procedure TMailingListDlg.Execute(const AOwner: TComponent);
  {Displays dialog box.
    @param AOwner [in] Owning form.
  }
begin
  with Create(AOwner) do
    try
      ShowModal;
    finally
      Free;
    end;
end;

procedure TMailingListDlg.InitForm;
  {Populates and initialises controls.
  }
begin
  inherited;
  // Use any registered user name as default name
  edName.Text := TAppInfo.RegisteredUser;
  // Make sure correct pane is displayed
  pnlResult.Hide;
  pnlData.Show;
end;

procedure TMailingListDlg.LoadResultAsHTML(const Msg: string);
  {Displays result message as HTML.
    @param Msg [in] Message to convert to HTML.
  }
var
  Lines: TStringList;   // lines of message
  LineIdx: Integer;     // loops thru lines of message
  Values: TStringList;  // map of HTML placeholders to actual values
begin
  Values := nil;

  // Record message as lines of valid HTML text
  Lines := TStringList.Create;
  try
    Lines.Text := Msg;
    for LineIdx := 0 to Pred(Lines.Count) do
      Lines[LineIdx] := MakeSafeHTMLText(Lines[LineIdx]);

    // Map placeholders to content
    Values := TStringList.Create;
    // heading is in first line of message
    if Lines.Count > 0 then
    begin
      Values.Values['Heading'] := Lines[0];
      Lines.Delete(0);
    end;
    // remainder of lines are paragraphs
    Values.Values['Paragraphs'] :=
      MakeTag('p', ttOpen) +
      JoinStr(
        Lines, MakeTag('p', ttClose) + EOL + MakeTag('p', ttOpen), False
      ) +
      MakeTag('p', ttClose);

    // Resolve and load template:
    frmResult.Initialise('dlg-mlist-result-tplt.html', Values);

  finally
    FreeAndNil(Values);
    FreeAndNil(Lines);
  end;
end;

function TMailingListDlg.ModalResultOnEsc: Integer;
  {Gets modal result returned from dialog when user presses ESC key.
    @return mrCancel.
  }
begin
  Result := btnCancel.ModalResult;
end;

procedure TMailingListDlg.ShowResultPane(const Msg: string);
  {Displays result pane and shows a message in it.
    @param Msg [in] Message to display in result pane.
  }
begin
  // Display result pane
  pnlData.Hide;
  pnlResult.Show;
  // Load HTML
  LoadResultAsHTML(Msg);
  // Update dialog's buttons
  btnCancel.Caption := sCloseBtnCaption;
  btnSubmit.Hide;
end;

function TMailingListDlg.Subscribe: string;
  {Attempts to subscribe user mailing list and handles message returned from
  web service.
    @return Message returned from web service.
  }
var
  MLSubs: TMailListSubscriber;  // mailing list web service
begin
  MLSubs := TMailListSubscriber.Create;
  try
    Result := MLSubs.Subscribe(edEmail.Text, edName.Text);
  finally
    FreeAndNil(MLSubs);
  end;
end;

procedure TMailingListDlg.Subscribing(const Flag: Boolean);
  {Updates display according whether we are currently using web service to
  perform subscription.
    @param Flag [in] True if subscribing and false if not.
  }
begin
  if Flag then
    Screen.Cursor := crHourGlass
  else
    Screen.Cursor := crDefault;
  btnCancel.Enabled := not Flag;
  btnSubmit.Enabled := not Flag;
  lblSubmit.Visible := not Flag;
  lblSubscribing.Visible := Flag;
  edEmail.Enabled := not Flag;
  edName.Enabled := not Flag;
  Application.ProcessMessages;
end;

function TMailingListDlg.ValidateEmailAddress: Boolean;
  {Checks that entered email address is valid by doing some basic checks on it.
  Displays any error messages.
    @return True if address if OK, false if not.
  }
var
  Email: string;  // entered email address
begin
  Email := edEmail.Text;
  // Assume failure
  Result := False;
  // Check if no email address supplied
  if Email = '' then
  begin
    TMessageBox.Error(Self, sErrEmailRequired);
    Exit;
  end;
  // Do some basic checks on address
  if not IsValidEmailAddress(Email) then
  begin
    TMessageBox.Error(Self, sErrEmailInvalid);
    Exit;
  end;
  // If we get here all is OK
  Result := True;
end;

end.


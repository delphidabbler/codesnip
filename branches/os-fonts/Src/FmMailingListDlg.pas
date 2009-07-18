{
 * FmMailingListDlg.pas
 *
 * Dialog box that is used to subscribe to the CodeSnip mailing list.
 *
 * v1.0 of 18 Nov 2006  - Original version.
 * v1.1 of 08 Feb 2007  - Moved control initialisation code from FormCreate
 *                        event handler to new overridden InitForm method and
 *                        deleted FormCreate method.
 * v1.2 of 15 Dec 2008  - Replaced custom email address checking code with
 *                        routine from UEmailHelper unit.
 *                      - Made private and protected sections strict.
 * v1.3 of 13 Jan 2009  - Replaced control char literals with constants.
 * v1.4 of 25 Jan 2009  - Changed to use routines from UHTMLUtils to generate
 *                        HTML tags instead of using literal strings.
 *                      - Removed class reference from call to TMailingListDlg
 *                        constructor.
 * v1.5 of 13 May 2009  - Changed to use revised web service constructor.
 *                      - Removed reference to deleted UParams unit.
 * v1.6 of 19 Jul 2009  - Modified to accommodate the new Vista default font in
 *                        controls. Some labels replaced by HTML frames.
 *                      - Controls now dynamically arranged vertically and
 *                        dialog box sizes itself to tallest tab sheet.
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
 * The Original Code is FmMailingListDlg.pas
 *
 * The Initial Developer of the Original Code is Peter Johnson
 * (http://www.delphidabbler.com/).
 *
 * Portions created by the Initial Developer are Copyright (C) 2006-2009 Peter
 * Johnson. All Rights Reserved.
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
  UAppInfo, UConsts, UGraphicUtils, UEmailHelper, UFontHelper, UHTMLUtils,
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

  procedure SetLabelHeight(const Lbl: TLabel);
    {Sets height of a label to accommodate the text it contains in its font.
      @param Lbl [in] Label whose height is to be set.
    }
  begin
    Lbl.Height := StringExtent(Lbl.Caption, Lbl.Font, Lbl.Width).cy;
  end;

  function BottomOf(const Ctrl: TControl): Integer;
    {Gets position of bottom of a control relative to its parent control in
    pixels.
      @param Ctr [in] Control to check.
      @return Required position.
    }
  begin
    Result := Ctrl.Top + Ctrl.Height;
  end;

  function VCentre(const ATop: Integer;
    const Ctrls: array of TControl): Integer;
    {Vertically centres a list of controls.
      @param ATop [in] Top tallest control to be aligned.
      @param Ctrls [in] Array of controls to be aligned.
      @return Height occupied by controls (= height of tallest control).
    }
  var
    I: Integer; // loops thru all controls to be aligned
  begin
    Result := 0;
    for I := Low(Ctrls) to High(Ctrls) do
      if Ctrls[I].Height > Result then
        Result := Ctrls[I].Height;
    for I := Low(Ctrls) to High(Ctrls) do
      Ctrls[I].Top := ATop + (Result - Ctrls[I].Height) div 2;
  end;

var
  ATop: Integer;  // indicates top of various controls
begin
  frmMailList.Height := frmMailList.DocHeight;
  frmPrivacy.Height := frmPrivacy.DocHeight;
  SetLabelHeight(lblEmail);
  SetLabelHeight(lblName);
  SetLabelHeight(lblSubmit);
  SetLabelHeight(lblSubscribing);
  ATop := BottomOf(frmMailList) + 8;
  frmPrivacy.Top := ATop + VCentre(ATop, [lblEmail, edEmail]);
  ATop := BottomOf(frmPrivacy) + 8;
  lblSubmit.Top := ATop + VCentre(ATop, [lblName, edName]) + 8;
  lblSubscribing.Top := lblSubmit.Top;
  pnlBody.ClientHeight := Max(BottomOf(lblSubmit), BottomOf(lblSubscribing))
    + 8;
  inherited;  // aligns inherited controls and sizes form
  // now align buttons added in this dialog
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
  frmPrivacy.Initialise('dlg-mlist-privacy.html');
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


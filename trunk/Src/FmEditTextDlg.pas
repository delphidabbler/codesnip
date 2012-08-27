{
 * This Source Code Form is subject to the terms of the Mozilla Public License,
 * v. 2.0. If a copy of the MPL was not distributed with this file, You can
 * obtain one at http://mozilla.org/MPL/2.0/
 *
 * Copyright (C) 2008-2012, Peter Johnson (www.delphidabbler.com).
 *
 * $Rev$
 * $Date$
 *
 * Implements a dialogue box that gets a single line string of text from user.
}


unit FmEditTextDlg;


interface


uses
  // Delphi
  StdCtrls, Controls, ExtCtrls, Classes,
  // Project
  FmGenericOKDlg, UBaseObjects;


type

  {
  TEditTextDlg:
    Dialog box that prompts for and gets a single line string of text from user.
  }
  TEditTextDlg = class(TGenericOKDlg, INoPublicConstruct)
    lblPrompt: TLabel;
    edText: TEdit;
    procedure btnOKClick(Sender: TObject);
  public
    type
      // Type of anonymous method used to validate entry in edit box
      TValidator = reference to function(const Name: string;
        out ErrMsg: string): Boolean;
  strict private
    fValidator: TValidator; // Reference to validator anonymous method
  strict protected
    procedure InitForm; override;
      {Sets focus on edit control.
      }
  public
    class function Execute(const AOwner: TComponent;
      const ATitle, APrompt: string; var AText: string;
      const AHelpKeyword: string = ''): Boolean; overload;
      {Displays dialog box to get requested text from user. Text is not
      validated.
        @param AOwner [in] Reference to component that owns the dialog box.
        @param ATitle [in] Title to be displayed in dialog box caption.
        @param APrompt [in] Prompt to be displayed in body of dialog box.
        @param AText [in] Default text displayed in edit control. [out] text
          entered by user if OK pressed, otherwise unchanged.
        @param AHelpKeyword [in] Optional help keyword. If provided a help
          button is displayed.
        @return True if user OKd, False if cancelled.
      }
    class function Execute(const AOwner: TComponent;
      const ATitle, APrompt: string; var AText: string;
      const AValidator: TValidator;
      const AHelpKeyword: string = ''): Boolean; overload;
      {Displays dialog box to get requested text from user. Text is validated
      according to provided validator and dialog box will not close until text
      validates OK. An error message is displayed when text fails to validate.
        @param AOwner [in] Reference to component that owns the dialog box.
        @param ATitle [in] Title to be displayed in dialog box caption.
        @param APrompt [in] Prompt to be displayed in body of dialog box.
        @param AText [in] Default text displayed in edit control. [out] text
          entered by user if OK pressed, otherwise unchanged.
        @param AValidator [in] Anonymous method used to validate entered text.
          Returns true if text is valid. Returns false and provides error
          message if text if not valid.
        @param AHelpKeyword [in] Optional help keyword. If provided a help
          button is displayed.
        @return True if user OKd, False if cancelled.
      }
  end;


implementation


uses
  // Project
  UMessageBox;


{$R *.dfm}

{ TEditTextDlg }

procedure TEditTextDlg.btnOKClick(Sender: TObject);
  {Handles OK button clicks. Validates text. If validation fails error message
  is displayed and dialog box remains open. Dialog box closes if text valid.
    @param Sender [in] Not used.
  }
var
  ErrMsg: string; // Error message if validation fails.
begin
  if fValidator(edText.Text, ErrMsg) then
    ModalResult := mrOK
  else
  begin
    ModalResult := mrNone;
    TMessageBox.Error(Self, ErrMsg);
  end;
end;

class function TEditTextDlg.Execute(const AOwner: TComponent; const ATitle,
  APrompt: string; var AText: string; const AValidator: TValidator;
  const AHelpKeyword: string): Boolean;
  {Displays dialog box to get requested text from user. Text is validated
  according to provided validator and dialog box will not close until text
  validates OK. An error message is displayed when text fails to validate.
    @param AOwner [in] Reference to component that owns the dialog box.
    @param ATitle [in] Title to be displayed in dialog box caption.
    @param APrompt [in] Prompt to be displayed in body of dialog box.
    @param AText [in] Default text displayed in edit control. [out] text entered
      by user if OK pressed, otherwise unchanged.
    @param AValidator [in] Anonymous method used to validate entered text.
      Returns true if text is valid. Returns false and provides error message if
      text if not valid.
    @param AHelpKeyword [in] Optional help keyword. If provided a help button is
      displayed.
    @return True if user OKd, False if cancelled.
  }
begin
  Assert(Assigned(AValidator));
  with InternalCreate(AOwner) do
    try
      Caption := ATitle;
      HelpKeyword := AHelpKeyword;
      btnHelp.Visible := AHelpKeyword <> '';
      lblPrompt.Caption := APrompt;
      edText.Text := AText;
      fValidator := AValidator;
      Result := ShowModal = mrOK;
      if Result then
        AText := edText.Text;
    finally
      Free;
    end;
end;

class function TEditTextDlg.Execute(const AOwner: TComponent; const ATitle,
  APrompt: string; var AText: string; const AHelpKeyword: string): Boolean;
  {Displays dialog box to get requested text from user. Text is not validated.
    @param AOwner [in] Reference to component that owns the dialog box.
    @param ATitle [in] Title to be displayed in dialog box caption.
    @param APrompt [in] Prompt to be displayed in body of dialog box.
    @param AText [in] Default text displayed in edit control. [out] text
      entered by user if OK pressed, otherwise unchanged.
    @param AHelpKeyword [in] Optional help keyword. If provided a help button is
      displayed.
    @return True if user OKd, False if cancelled.
  }
begin
  Result := Execute(
    AOwner,
    ATitle,
    APrompt,
    AText,
    function(const Text: string; out ErrMsg: string): Boolean
    begin
      // nul validator: always returns true
      ErrMsg := '';
      Result := True;
    end,
    AHelpKeyword
  );
end;

procedure TEditTextDlg.InitForm;
  {Sets focus on edit control.
  }
begin
  inherited;
  edText.SetFocus;
end;

end.


{
 * FmEditTextDlg.pas
 *
 * Implements a dialog box that gets a single line string of text from user.
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
 * The Original Code is FmEditTextDlg.pas
 *
 * The Initial Developer of the Original Code is Peter Johnson
 * (http://www.delphidabbler.com/).
 *
 * Portions created by the Initial Developer are Copyright (C) 2008-2009 Peter
 * Johnson. All Rights Reserved.
 *
 * Contributor(s)
 *   NONE
 *
 * ***** END LICENSE BLOCK *****
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
  strict protected
    procedure InitForm; override;
      {Sets focus on edit control.
      }
  public
    class function Execute(const AOwner: TComponent;
      const ATitle, APrompt: string; var AText: string;
      const AHelpKeyword: string = ''): Boolean;
      {Displays dialog box to get requested text from user.
        @param AOwner [in] Reference to component that owns the dialog box.
        @param ATitle [in] Title to be displayed in dialog box caption.
        @param APrompt [in] Prompt to be displayed in body of dialog box.
        @param AText [in] Default text displayed in edit control. [out] text
          entered by user if OK pressed, otherwise unchanged.
        @param AHelpKeyword [in] Optional help keyword. If provided a help
          button is displayed.
        @return True if user OKd, False if cancelled.
      }
  end;


implementation


{$R *.dfm}

{ TEditTextDlg }

class function TEditTextDlg.Execute(const AOwner: TComponent; const ATitle,
  APrompt: string; var AText: string; const AHelpKeyword: string): Boolean;
  {Displays dialog box to get requested text from user.
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
  with InternalCreate(AOwner) do
    try
      Caption := ATitle;
      HelpKeyword := AHelpKeyword;
      btnHelp.Visible := AHelpKeyword <> '';
      lblPrompt.Caption := APrompt;
      edText.Text := AText;
      Result := ShowModal = mrOK;
      if Result then
        AText := edText.Text;
    finally
      Free;
    end;
end;

procedure TEditTextDlg.InitForm;
  {Sets focus on edit control.
  }
begin
  inherited;
  edText.SetFocus;
end;

end.


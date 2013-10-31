{
 * FmDonateDlg.pas
 *
 * Implements a dialog box that displays information about donating to support
 * CodeSnip along with button to access Paypal donation web page in default
 * browser.
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
 * The Original Code is FmDonateDlg.pas
 *
 * The Initial Developer of the Original Code is Peter Johnson
 * (http://www.delphidabbler.com/).
 *
 * Portions created by the Initial Developer are Copyright (C) 2009-2010 Peter
 * Johnson. All Rights Reserved.
 *
 * Contributor(s)
 *   NONE
 *
 * ***** END LICENSE BLOCK *****
}


unit FmDonateDlg;


interface


uses
  // Delphi
  StdCtrls, Controls, ExtCtrls, Classes, Forms,
  // Project
  FmHTMLViewDlg, FrBrowserBase, FrFixedHTMLDlg, FrHTMLDlg, UCSSBuilder;


type
  {
  TDonateDlg:
    Dialog box that displays information about donating to support CodeSnip
    along with a button to display the Paypal donation web page.
  }
  TDonateDlg = class(THTMLViewDlg)
    btnDoDonate: TButton;
    frmContent: TFixedHTMLDlgFrame;
    procedure FormCreate(Sender: TObject);
  strict private
    procedure UpdateCSS(Sender: TObject; const CSSBuilder: TCSSBuilder);
      {Modifies CSS used to display dialog box body to achieve required
      appearance.
        @param Sender [in] Not used.
        @param CSSBuilder [in] Object used to modify CSS.
      }
  strict protected
    procedure ConfigForm; override;
      {Sets UI font for emboldened Donate button.
      }
    procedure ArrangeForm; override;
      {Sizes dialog to fit content and adjusts position of donation button on
      bottom line. Called from ancestor class.
      }
    procedure InitHTMLFrame; override;
      {Initialises HTML frame.
      }
  public
    class procedure Execute(const AOwner: TComponent);
      {Displays dialog box.
        @param AOwner [in] Component that owns this dialog box.
      }
  end;


implementation


uses
  // Delphi
  SysUtils, Graphics,
  // Project
  UCSSUtils, UFontHelper, UPaypalDonateAction;

{$R *.dfm}

{ TDonateDlg }

procedure TDonateDlg.ArrangeForm;
  {Sizes dialog to fit content and adjusts position of donation button on bottom
  line. Called from ancestor class.
  }
begin
  // set body panel height from size of content
  pnlBody.Height := frmContent.DocHeight;
  // size dialog box
  inherited;
  btnDoDonate.Left := pnlBody.Left;
  btnDoDonate.Top := btnClose.Top;
end;

procedure TDonateDlg.ConfigForm;
  {Sets UI font for emboldened Donate button.
  }
begin
  inherited;
  TFontHelper.SetDefaultBaseFont(btnDoDonate.Font, False);
end;

class procedure TDonateDlg.Execute(const AOwner: TComponent);
  {Displays dialog box.
    @param AOwner [in] Component that owns this dialog box.
  }
begin
  with Create(AOwner) do
    try
      ShowModal;
    finally
      Free;
    end;
end;

procedure TDonateDlg.FormCreate(Sender: TObject);
  {Form construction event handler. Assigns donation button's action.
  }
begin
  inherited;
  btnDoDonate.Action := TPaypalDonateAction.Create(Self);
end;

procedure TDonateDlg.InitHTMLFrame;
  {Initialises HTML frame.
  }
begin
  // Set event handler used to customise CSS
  frmContent.OnBuildCSS := UpdateCSS;
  // Load required HTML into frame
  frmContent.Initialise('dlg-donate.html');
end;

procedure TDonateDlg.UpdateCSS(Sender: TObject; const CSSBuilder: TCSSBuilder);
  {Modifies CSS used to display dialog box body to achieve required appearance.
    @param Sender [in] Not used.
    @param CSSBuilder [in] Object used to modify CSS.
  }
var
  ContentFont: TFont; // font used for dialog box content (not controls)
begin
  // Build content font and apply to HTML frame
  ContentFont := TFont.Create;
  try
    TFontHelper.SetContentFont(ContentFont, True);  // font must be true type
    with CSSBuilder.Selectors['body'] do
    begin
      AddProperty(CSSFontProps(ContentFont));
      AddProperty(CSSBackgroundColorProp(clWindow));
      AddProperty(CSSPaddingProp(0, 6, 6, 6));
    end;
  finally
    FreeAndNil(ContentFont);
  end;
end;

end.


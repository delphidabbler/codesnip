{
 * This Source Code Form is subject to the terms of the Mozilla Public License,
 * v. 2.0. If a copy of the MPL was not distributed with this file, You can
 * obtain one at http://mozilla.org/MPL/2.0/
 *
 * Copyright (C) 2009-2014, Peter Johnson (www.delphidabbler.com).
 *
 * $Rev$
 * $Date$
 *
 * Implements a dialogue box that displays information about donating to support
 * CodeSnip along with button to access Paypal donation web page in default
 * browser.
}


// TODO -cwebsvc: remove this form unit

unit FmDonateDlg;


interface


uses
  // Delphi
  StdCtrls, Controls, ExtCtrls, Classes, Forms,
  // Project
  FmGenericViewDlg, FrBrowserBase, FrFixedHTMLDlg, FrHTMLDlg, UCSSBuilder;


type
  {
  TDonateDlg:
    Dialogue box that displays information about donating to support CodeSnip
    along with a button to display the Paypal donation web page.
  }
  TDonateDlg = class(TGenericViewDlg)
    btnDoDonate: TButton;
    frmContent: TFixedHTMLDlgFrame;
  strict private
    procedure UpdateCSS(Sender: TObject; const CSSBuilder: TCSSBuilder);
      {Modifies CSS used to display dialogue box body to achieve required
      appearance.
        @param Sender [in] Not used.
        @param CSSBuilder [in] Object used to modify CSS.
      }
  strict protected
    procedure ConfigForm; override;
      {Initialises HTML frame and sets UI font for emboldened Donate button.
      Called from ancestor class.
      }
    procedure ArrangeForm; override;
      {Sizes dialogue to fit content and adjusts position of donation button on
      bottom line. Called from ancestor class.
      }
  public
    class procedure Execute(const AOwner: TComponent);
      {Displays dialogue box.
        @param AOwner [in] Component that owns this dialogue box.
      }
  end;


implementation


uses
  // Delphi
  SysUtils, Graphics,
  // Project
  UCSSUtils, UFontHelper;

{$R *.dfm}

{ TDonateDlg }

procedure TDonateDlg.ArrangeForm;
  {Sizes dialogue to fit content and adjusts position of donation button on
  bottom line. Called from ancestor class.
  }
begin
  // set body panel height from size of content
  pnlBody.Height := frmContent.DocHeight;
  // size dialogue box
  inherited;
  btnDoDonate.Left := pnlBody.Left;
  btnDoDonate.Top := btnClose.Top;
end;

procedure TDonateDlg.ConfigForm;
  {Initialises HTML frame and sets UI font for emboldened Donate button.
  }
begin
  inherited;
  TFontHelper.SetDefaultBaseFont(btnDoDonate.Font);
  frmContent.OnBuildCSS := UpdateCSS;
  frmContent.Initialise('dlg-donate.html');
end;

class procedure TDonateDlg.Execute(const AOwner: TComponent);
  {Displays dialogue box.
    @param AOwner [in] Component that owns this dialogue box.
  }
begin
  with Create(AOwner) do
    try
      ShowModal;
    finally
      Free;
    end;
end;

procedure TDonateDlg.UpdateCSS(Sender: TObject; const CSSBuilder: TCSSBuilder);
  {Modifies CSS used to display dialogue box body to achieve required
  appearance.
    @param Sender [in] Not used.
    @param CSSBuilder [in] Object used to modify CSS.
  }
var
  ContentFont: TFont; // font used for dialogue box content (not controls)
begin
  // Build content font and apply to HTML frame
  ContentFont := TFont.Create;
  try
    TFontHelper.SetContentFont(ContentFont);
    with CSSBuilder.Selectors['body'] do
    begin
      AddProperty(TCSS.FontProps(ContentFont));
      AddProperty(TCSS.BackgroundColorProp(clWindow));
      AddProperty(TCSS.PaddingProp(0, 6, 6, 6));
    end;
  finally
    ContentFont.Free;
  end;
end;

end.


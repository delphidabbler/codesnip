{
 * This Source Code Form is subject to the terms of the Mozilla Public License,
 * v. 2.0. If a copy of the MPL was not distributed with this file, You can
 * obtain one at https://mozilla.org/MPL/2.0/
 *
 * Copyright (C) 2020-2021, Peter Johnson (gravatar.com/delphidabbler).
 *
 * Frame that displays HTML of "what's new" message in a TWebBrowser control.
}


unit FirstRun.FmWhatsNew.FrHTML;

interface

uses
  // VCL
  OleCtrls,
  SHDocVw,
  Classes,
  Controls,
  ExtCtrls,
  // Project
  FrBrowserBase,
  UCSSBuilder;


type
  TWhatsNewHTMLFrame = class(TBrowserBaseFrame)
  strict protected
    procedure BuildCSS(const CSSBuilder: TCSSBuilder); override;
      {Generates CSS classes specific to this frame. This CSS is added
      to that provided by parent class.
        @param CSSBuilder [in] Object used to build the CSS code.
      }
  public
    constructor Create(AOwner: TComponent); override;
      {Object constructor. Sets up frame and initialises web browser.
        @param AOwner [in] Component that owns the frame (must be a form).
      }
    procedure Initialise(const HTML: string);
  end;


implementation


uses
  // VCL
  Graphics,
  // Project
  Browser.UUIMgr,
  UCSSUtils,
  UFontHelper;

{$R *.dfm}


{ TWhatsNewHTMLFrame }

procedure TWhatsNewHTMLFrame.BuildCSS(const CSSBuilder: TCSSBuilder);
var
  CSSFont: TFont; // font used to set CSS properties
begin
  inherited;
  CSSFont := TFont.Create;
  try
    TFontHelper.SetContentFont(CSSFont);
    CSSFont.Size := CSSFont.Size + 2;
    with CSSBuilder.AddSelector('body') do
    begin
      AddProperty(TCSS.FontProps(CSSFont));
      AddProperty(TCSS.MarginProp(0, 8, 0, 8));
    end;
    with CSSBuilder.AddSelector('.lead') do
    begin
      AddProperty(TCSS.FontSizeProp(CSSFont.Size + 2));
      AddProperty(TCSS.FontWeightProp(cfwBold));
      AddProperty(TCSS.ColorProp($233bc2));
    end;
    // Sets paragraph margins and padding
    with CSSBuilder.AddSelector('p') do
    begin
      AddProperty(TCSS.MarginProp(cssTop, 6));
      AddProperty(TCSS.MarginProp(cssBottom, 0));
      AddProperty(TCSS.PaddingProp(0));
    end;
    with CSSBuilder.AddSelector('ul') do
    begin
      AddProperty(TCSS.MarginProp(cssTop, 6));
      AddProperty(TCSS.MarginProp(cssBottom, 0));
      AddProperty(TCSS.PaddingProp(0));
    end;
    with CSSBuilder.AddSelector('li') do
    begin
      AddProperty(TCSS.MarginProp(cssTop, 6));
    end;
   finally
    CSSFont.Free;
  end;
end;

constructor TWhatsNewHTMLFrame.Create(AOwner: TComponent);
begin
  inherited;
  WBController.UIMgr.ScrollbarStyle := sbsNormal;
  WBController.UIMgr.AllowTextSelection := False;
end;

procedure TWhatsNewHTMLFrame.Initialise(const HTML: string);
begin
  WBController.IOMgr.LoadFromString(HTML);
end;

end.

{
 * FrNewsHTML.pas
 *
 * Frame containing a web browser control that displays news items.
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
 * The Original Code is FrNewsHTML.pas
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


unit FrNewsHTML;


interface


uses
  // Delphi
  OleCtrls, SHDocVw, Classes, Controls, ExtCtrls,
  // Project
  FrBrowserBase, UCSSBuilder;


type

  {
  TNewsHTMLFrame:
    Frame containing a web browser control that displays news items.
  }
  TNewsHTMLFrame = class(TBrowserBaseFrame)
  strict protected
    procedure BuildCSS(const CSSBuilder: TCSSBuilder); override;
      {Generates CSS classes specific to the news pane. This CSS is added to
      that provided by parent class.
        @param CSSBuilder [in] Object used to build the CSS code.
      }
  public
    constructor Create(AOwner: TComponent); override;
      {Class constructor. Sets up object and configures web browser control.
        @param AOwner [in] Component that owns frame.
      }
    procedure LoadPage(const HTML: string);
      {Loads HTML code into browser control.
        @param HTML [in] HTML ode to be displayed.
      }
  end;


implementation


uses
  // Delphi
  SysUtils, Graphics,
  // Project
  UColours, UCSSUtils, UFontHelper, UHTMLTemplate, UWBUIMgr, UWBNulDropTarget;


{
  NOTE:

  The news HTML template is stored in RT_HTML resources as
  "dlg-updt-news-tplt.html". It has the following placeholders that are replaced
  by values in this code:

  <%Content%>       content of a news item
}


{$R *.dfm}


{ TNewsFrame }

procedure TNewsHTMLFrame.BuildCSS(const CSSBuilder: TCSSBuilder);
  {Generates CSS classes specific to the news pane. This CSS is added to that
  provided by parent class.
    @param CSSBuilder [in] Object used to build the CSS code.
  }
var
  ContentFont: TFont; // content font used to set CSS properties
begin
  inherited;
  // Set body style to use window colour and frame's font with no margin
  ContentFont := TFont.Create;
  try
    TFontHelper.SetContentFont(ContentFont, True);
    with CSSBuilder.AddSelector('body') do
    begin
      AddProperty(CSSBackgroundColorProp(clWindow));
      AddProperty(CSSFontProps(ContentFont));
      AddProperty(CSSMarginProp(3));
    end;
    // Sets H1 style
    with CSSBuilder.AddSelector('h1') do
    begin
      AddProperty(CSSFontSizeProp(ContentFont.Size));
      AddProperty(CSSFontStyleProp([fsBold]));
      AddProperty(CSSColorProp(clNewsHeading));
      AddProperty(CSSMarginProp(0, 0, 4, 0));
      AddProperty(CSSPaddingProp(0, 0, 2, 0));
      AddProperty(CSSBorderProp(cssBottom, 1, cbsSolid, clBorder));
    end;
    // Sets paragraph style
    with CSSBuilder.AddSelector('p') do
      AddProperty(CSSMarginProp(4, 0, 0, 0));
    // Setup .dateline style
    with CSSBuilder.AddSelector('.dateline') do
    begin
      AddProperty(CSSFontSizeProp(ContentFont.Size - 1));
      AddProperty(CSSColorProp(clNewsDateText));
      AddProperty(CSSTextAlignProp(ctaRight));
      AddProperty(CSSMarginProp(cssTop, 3));
      AddProperty(CSSFloatProp(cfvRight));
    end;
    // Setup .highlight style
    with CSSBuilder.AddSelector('.highlight') do
      AddProperty(CSSColorProp(clNewsHighlight));
    // Setup .message style
    with CSSBuilder.AddSelector('.message') do
    begin
      AddProperty(CSSTextAlignProp(ctaCenter));
      AddProperty(CSSMarginProp(cssTop, 30));
    end;
  finally
    FreeAndNil(ContentFont);
  end;
end;

constructor TNewsHTMLFrame.Create(AOwner: TComponent);
  {Class constructor. Sets up object and configures web browser control.
    @param AOwner [in] Component that owns frame.
  }
begin
  inherited;
  // Set up browser control
  WBController.UIMgr.ScrollbarStyle := sbsNormal; // no scroll bars
  WBController.UIMgr.Show3dBorder := False;       // we'll handle border
  WBController.UIMgr.AllowTextSelection := False; // can't select text
  WBController.UIMgr.DropTarget :=                // inhibit drag drop in dialog
    TWBNulDropTarget.Create;
end;

procedure TNewsHTMLFrame.LoadPage(const HTML: string);
  {Loads HTML code into browser control.
    @param HTML [in] HTML ode to be displayed.
  }
var
  HTMLTplt: THTMLTemplate;  // object used to create HTML from template
begin
  // Loads HTML template, resolves placeholders and displays HTML
  HTMLTplt := THTMLTemplate.Create(HInstance, 'dlg-updt-news-tplt.html');
  try
    HTMLTplt.ResolvePlaceholderHTML('Content', HTML);
    WBController.IOMgr.LoadFromString(HTMLTplt.HTML);
  finally
    FreeAndNil(HTMLTplt);
  end;
end;

end.


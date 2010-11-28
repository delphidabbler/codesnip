{
 * FrEasterEgg.pas
 *
 * Defines a frame that hosts the HTML, CSS and JavaScript used to display the
 * program's animated easter egg.
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
 * The Original Code is FrEasterEgg.pas
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


unit FrEasterEgg;


interface


uses
  // Delphi
  OleCtrls, SHDocVw, Classes, Controls, ExtCtrls,
  // Project
  FrBrowserBase, UCSSBuilder;


type

  {
  TEasterEggFrame:
    Frame that hosts easter egg HTML and JavaScript that provides animation.
  }
  TEasterEggFrame = class(TBrowserBaseFrame)
  strict protected
    procedure BuildCSS(const CSSBuilder: TCSSBuilder); override;
      {Generates CSS classes specific to the easter egg frame. This CSS is added
      to that provided by parent class.
        @param CSSBuilder [in] Object used to build the CSS code.
      }
  public
    constructor Create(AOwner: TComponent); override;
      {Object constructor. Sets up frame and initialises web browser.
        @param AOwner [in] Component that owns the frame (must be a form).
      }
    procedure Initialise;
      {Initialises display by loading HTML resource.
      }
  end;


implementation


uses
  // Delphi
  SysUtils, Graphics,
  // Project
  Browser.UUIMgr, UColours, UCSSUtils, UFontHelper;

{$R *.dfm}


{ TEasterEggFrame }

procedure TEasterEggFrame.BuildCSS(const CSSBuilder: TCSSBuilder);
  {Generates CSS classes specific to the easter egg frame. This CSS is added
  to that provided by parent class.
    @param CSSBuilder [in] Object used to build the CSS code.
  }
var
  CSSFont: TFont; // font used to set CSS properties
begin
  inherited;
  // Set body style to use window colour and frame's font with no margin
  CSSFont := TFont.Create;
  try
    TFontHelper.SetContentFont(CSSFont, True);
    CSSFont.Size := CSSFont.Size + 1;
    with CSSBuilder.AddSelector('body') do
    begin
      AddProperty(CSSFontProps(CSSFont));
      AddProperty(CSSMarginProp(3));
      // background colour same as parent: used as transparency colour key
      AddProperty(CSSBackgroundColorProp(ParentForm.Color));
    end;
    // Sets paragraph style
    with CSSBuilder.AddSelector('p') do
    begin
      AddProperty(CSSMarginProp(4, 0, 0, 0));
    end;
    // Customises A-link styles
    with CSSBuilder.AddSelector('a:link, a:active, a:visited') do
      AddProperty(CSSColorProp(clEggLinkText));
    // Customises "blurb" id
    with CSSBuilder.AddSelector('#blurb') do
    begin
      AddProperty(CSSBorderProp(cssAll, 2, cbsSolid, clEggBlurbBorder));
      AddProperty(CSSBackgroundColorProp(clEggBlurbBg));
    end;
    // Sets H1 style for "blurb" div
    with CSSBuilder.AddSelector('#blurb h1') do
    begin
      CSSFont.Style := [fsBold];
      AddProperty(CSSFontSizeProp(CSSFont.Size + 2));
      AddProperty(CSSMarginProp(4, 0, 4, 0));
      AddProperty(CSSPaddingProp(4, 0, 4, 0));
      AddProperty(CSSBackgroundColorProp(clEggBlurbHeadingBg));
      AddProperty(CSSBorderProp(cssBottom, 1, cbsSolid, clBorder));
    end;
    with CSSBuilder.AddSelector('#more-info') do
    begin
      AddProperty(CSSBackgroundColorProp(clEggMoreInfoBg));
      AddProperty(CSSBorderProp(cssAll, 1, cbsSolid, clEggMoreInfoBorder));
    end;
  finally
    FreeAndNil(CSSFont);
  end;
end;

constructor TEasterEggFrame.Create(AOwner: TComponent);
  {Object constructor. Sets up frame and initialises web browser.
    @param AOwner [in] Component that owns the frame (must be a form).
  }
begin
  inherited;
  WBController.UIMgr.ScrollbarStyle := sbsHide;
  WBController.UIMgr.AllowTextSelection := False;
end;

procedure TEasterEggFrame.Initialise;
  {Initialises display by loading HTML resource.
  }
begin
  WBController.IOMgr.NavigateToResource(HInstance, 'dlg-easter-egg.html');
end;

end.


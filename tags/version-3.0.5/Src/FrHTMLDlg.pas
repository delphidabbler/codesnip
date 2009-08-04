{
 * FrHTMLDlg.pas
 *
 * Frame containing a web browser control that displays HTML content that takes
 * on the appearance of a dialog box.
 *
 * v0.1 of 30 Jan 2005  - Original version (handled HTML templates).
 * v0.2 of 23 Feb 2005  - Now derived from TBrowserFrame that provides web
 *                        browser controller object.
 *                      - Moved code that handled HTML templates to a new
 *                        descendant class.
 * v0.3 of 05 Mar 2005  - Added new default CSS style for <table> tag.
 * v0.4 of 16 Apr 2006  - Fixed error where user could drop files on dialog box
 *                        and display the file contents, replacing dialog
 *                        contents. Added drag drop handler to browser control
 *                        that inhibits all drag drop.
 * v1.0 of 25 May 2006  - Improved and corrected comments.
 * v1.1 of 07 Nov 2006  - Changed to use UCSSUtils methods to generate CSS
 *                        attributes.
 *                      - Changed to use new TCSSSelector.Extras property to add
 *                        custom CSS.
 * v1.2 of 14 Nov 2006  - Added a:link.help-link, a:visited.help-link and
 *                        a:active.help-link CSS styles to colour links to help
 *                        files appropriately.
 * v1.3 of 18 Nov 2006  - Added new h1 CSS style.
 *                      - Changed p CSS style to specify no padding.
 * v1.4 of 25 Nov 2006  - Changed to set browser's default CSS by overriding
 *                        method of parent class rather than update browser
 *                        control directly.
 *                      - Removed a-link classes from generated CSS since this
 *                        is now provided by parent class.
 * v1.5 of 11 Feb 2007  - Added DocHeight method to retrieve height of current
 *                        HTML document.
 * v1.6 of 17 Feb 2007  - Changed DocHeight method to call same named method in
 *                        TWBUIMgr rather than calculating document height
 *                        directly here.
 * v1.7 of 05 Nov 2007  - Changed to use revised CSS builder classes.
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
 * The Original Code is FrHTMLDlg.pas
 *
 * The Initial Developer of the Original Code is Peter Johnson
 * (http://www.delphidabbler.com/).
 *
 * Portions created by the Initial Developer are Copyright (C) 2005-2007 Peter
 * Johnson. All Rights Reserved.
 *
 * Contributor(s): None
 *
 * ***** END LICENSE BLOCK *****
}


unit FrHTMLDlg;


interface


uses
  // Delphi
  OleCtrls, SHDocVw, Classes, Controls, ExtCtrls, Forms,
  // Project
  FrBrowserBase, UCSSBuilder;


type

  {
  THTMLDlgFrame:
    Frame containing a web browser control that displays HTML content and takes
    on the appearance of a dialog box.
  }
  THTMLDlgFrame = class(TBrowserBaseFrame)
  private
    fOwner: TForm;
      {Form that owns the frame. Used to set font and colours to display in
      frame's HTML }
  protected
    procedure BuildCSS(const CSSBuilder: TCSSBuilder); override;
      {Generates CSS classes specific to HTML dialog boxes. This CSS is added to
      that provided by parent class.
        @param CSSBuilder [in] Object used to build the CSS code.
      }
  public
    constructor Create(AOwner: TComponent); override;
      {Class constructor. Sets up frame and initialises web browser.
        @param AOwner [in] Component that owns the frame (must be a form).
      }
    function DocHeight: Integer;
      {Calculates height of document displayed in browser.
        @return Document height in pixels.
      }
  end;


implementation


uses
  // Delphi
  SysUtils, Graphics,
  // Project
  UCSSUtils, UWBNulDropTarget, UWBUIMgr;


{$R *.dfm}


{ THTMLDlgFrame }

procedure THTMLDlgFrame.BuildCSS(const CSSBuilder: TCSSBuilder);
  {Generates CSS classes specific to HTML dialog boxes. This CSS is added to
  that provided by parent class.
    @param CSSBuilder [in] Object used to build the CSS code.
  }
var
  CSSFont: TFont; // font used to set CSS properties
begin
  inherited;
  // Set body style to use dialog box colour and font with no margin
  CSSFont := TFont.Create;
  try
    with CSSBuilder.AddSelector('body') do
    begin
      AddProperty(CSSBackgroundColorProp(fOwner.Color));
      AddProperty(CSSFontProps(fOwner.Font));
      AddProperty(CSSMarginProp(0));
    end;
    // Sets heading margins, padding and font size
    with CSSBuilder.AddSelector('h1') do
    begin
      CSSFont.Assign(Self.Font);
      CSSFont.Size := CSSFont.Size + 2;
      CSSFont.Style := [fsBold];
      AddProperty(CSSFontProps(CSSFont));
      AddProperty(CSSMarginProp(0));
      AddProperty(CSSPaddingProp(0));
    end;
    // Sets paragraph margins and padding
    with CSSBuilder.AddSelector('p') do
    begin
      AddProperty(CSSMarginProp(cssTop, 6));
      AddProperty(CSSMarginProp(cssBottom, 0));
      AddProperty(CSSPaddingProp(0));
    end;
    // Sets table font info
    with CSSBuilder.AddSelector('table') do
      AddProperty(CSSFontProps(fOwner.Font));
  finally
    FreeAndNil(CSSFont);
  end;
end;

constructor THTMLDlgFrame.Create(AOwner: TComponent);
  {Class constructor. Sets up frame and initialises web browser.
    @param AOwner [in] Component that owns the frame (must be a form).
  }
begin
  inherited;
  // Record parent form instance
  fOwner := (AOwner as TForm);
  // Set up browser control to take on dialog box's appearance
  WBController.UIMgr.ScrollbarStyle := sbsHide;   // no scroll bars
  WBController.UIMgr.AllowTextSelection := False; // can't select text
  WBController.UIMgr.DropTarget :=                // inhibit drag drop in dialog
    TWBNulDropTarget.Create;
end;

function THTMLDlgFrame.DocHeight: Integer;
  {Calculates height of document displayed in browser.
    @return Document height in pixels.
  }
begin
  Result := WBController.UIMgr.DocHeight;
end;

end.


{
 * FrHTMLDlg.pas
 *
 * Frame containing a web browser control that displays HTML content that takes
 * on the appearance of a dialog box.
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
 * The Original Code is FrHTMLDlg.pas
 *
 * The Initial Developer of the Original Code is Peter Johnson
 * (http://www.delphidabbler.com/).
 *
 * Portions created by the Initial Developer are Copyright (C) 2005-2010 Peter
 * Johnson. All Rights Reserved.
 *
 * Contributor(s)
 *   NONE
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
      {Object constructor. Sets up frame and initialises web browser.
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
  Browser.UUIMgr, UCSSUtils, UNulDropTarget;


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
  {Object constructor. Sets up frame and initialises web browser.
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
    TNulDropTarget.Create;
end;

function THTMLDlgFrame.DocHeight: Integer;
  {Calculates height of document displayed in browser.
    @return Document height in pixels.
  }
begin
  Result := WBController.UIMgr.DocHeight;
end;

end.


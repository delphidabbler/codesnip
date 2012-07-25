{
 * FrRSSNews.pas
 *
 * Frame containing a web browser control that is used to display the program's
 * RSS news feed.
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
 * The Original Code is FrRSSNews.pas
 *
 * The Initial Developer of the Original Code is Peter Johnson
 * (http://www.delphidabbler.com/).
 *
 * Portions created by the Initial Developer are Copyright (C) 2010 Peter
 * Johnson. All Rights Reserved.
 *
 * Contributor(s)
 *   NONE
 *
 * ***** END LICENSE BLOCK *****
}


unit FrRSSNews;


interface


uses
  // Delphi
  OleCtrls, SHDocVw, Classes, Controls, ExtCtrls,
  // Project
  FrBrowserBase, UCSSBuilder;

type

  {
  TRSSNewsFrame:
    Frame containing a web browser control that is used to display the program's
    RSS news feed.
  }
  TRSSNewsFrame = class(TBrowserBaseFrame)
  strict protected
    procedure BuildCSS(const CSSBuilder: TCSSBuilder); override;
      {Generates CSS classes specific to this frame. This CSS is added to that
      provided by parent class.
        @param CSSBuilder [in] Object used to build the CSS code.
      }
  public
    constructor Create(AOwner: TComponent); override;
      {Object constructor. Sets up object and configures web browser control.
        @param AOwner [in] Component that owns frame.
      }
    procedure Initialise;
      {Initialises browser control. Loads container HTML document.
      }
    procedure DisplayContent(const HTML: string);
      {Displays HTML content as body of document currently loaded in the browser
      control.
        @param HTML [in] HTML ode to be displayed.
      }
  end;


implementation


uses
  // Delphi
  Graphics,
  // Project
  Browser.UUIMgr, UColours, UCSSUtils, UFontHelper, UNulDropTarget;

{$R *.dfm}


{ TRSSNewsFrame }

procedure TRSSNewsFrame.BuildCSS(const CSSBuilder: TCSSBuilder);
  {Generates CSS classes specific to this frame. This CSS is added to that
  provided by parent class.
    @param CSSBuilder [in] Object used to build the CSS code.
  }
var
  ContentFont: TFont; // content font used to set CSS properties
begin
  inherited;
  ContentFont := TFont.Create;
  try
    TFontHelper.SetContentFont(ContentFont, True);
    with CSSBuilder.AddSelector('body') do
    begin
      AddProperty(CSSBackgroundColorProp(clWindow));
      AddProperty(CSSFontProps(ContentFont));
      AddProperty(CSSMarginProp(3));
    end;
    with CSSBuilder.AddSelector('p') do
    begin
      AddProperty(CSSMarginProp(4, 0, 0, 0));
    end;
    with CSSBuilder.AddSelector('dt') do
    begin
      AddProperty(CSSMarginProp(0, 0, 4, 0));
      AddProperty(CSSPaddingProp(4));
      AddProperty(CSSBackgroundColorProp(clBtnFace));
      AddProperty(CSSColorProp(clBtnText));
    end;
    with CSSBuilder.AddSelector('dd') do
    begin
      AddProperty(CSSMarginProp(0, 0, 8, 20));
    end;
    with CSSBuilder.AddSelector('.message') do
    begin
      AddProperty(CSSTextAlignProp(ctaCenter));
      AddProperty(CSSMarginProp(cssTop, 30));
      AddProperty(CSSFontWeightProp(cfwBold));
    end;
    with CSSBuilder.AddSelector('.error-heading') do
    begin
      AddProperty(CSSColorProp(clWarningText));
      AddProperty(CSSFontWeightProp(cfwBold));
    end;
    with CSSBuilder.AddSelector('.error-message') do
    begin
      AddProperty(CSSMarginProp(cssLeft, 20));
    end;
  finally
    ContentFont.Free;
  end;
end;

constructor TRSSNewsFrame.Create(AOwner: TComponent);
  {Object constructor. Sets up object and configures web browser control.
    @param AOwner [in] Component that owns frame.
  }
begin
  inherited;
  // Set up browser control
  WBController.UIMgr.ScrollbarStyle := sbsNormal; // normal scroll bars
  WBController.UIMgr.Show3dBorder := False;       // we'll handle border
  WBController.UIMgr.AllowTextSelection := False; // can't select text
  WBController.UIMgr.DropTarget :=                // inhibit drag drop in dialog
    TNulDropTarget.Create;
end;

procedure TRSSNewsFrame.DisplayContent(const HTML: string);
  {Displays HTML content as body of document currently loaded in the browser
  control.
    @param HTML [in] HTML ode to be displayed.
  }
begin
  WBController.IOMgr.ReplaceExistingBodyHTML(HTML);
end;

procedure TRSSNewsFrame.Initialise;
  {Initialises browser control. Loads container HTML document.
  }
begin
  WBController.IOMgr.NavigateToResource(HInstance, 'dlg-rssnews.html');
end;

end.

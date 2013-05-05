{
 * This Source Code Form is subject to the terms of the Mozilla Public License,
 * v. 2.0. If a copy of the MPL was not distributed with this file, You can
 * obtain one at http://mozilla.org/MPL/2.0/
 *
 * Copyright (C) 2010-2013, Peter Johnson (www.delphidabbler.com).
 *
 * $Rev$
 * $Date$
 *
 * Implements a frame containing a web browser control that is used to display
 * the program's RSS news feed.
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
    TFontHelper.SetContentFont(ContentFont);
    with CSSBuilder.AddSelector('body') do
    begin
      AddProperty(TCSS.BackgroundColorProp(clWindow));
      AddProperty(TCSS.FontProps(ContentFont));
      AddProperty(TCSS.MarginProp(3));
    end;
    with CSSBuilder.AddSelector('p') do
    begin
      AddProperty(TCSS.MarginProp(4, 0, 0, 0));
    end;
    with CSSBuilder.AddSelector('dt') do
    begin
      AddProperty(TCSS.MarginProp(0, 0, 4, 0));
      AddProperty(TCSS.PaddingProp(4));
      AddProperty(TCSS.BackgroundColorProp(clBtnFace));
      AddProperty(TCSS.ColorProp(clBtnText));
    end;
    with CSSBuilder.AddSelector('dd') do
    begin
      AddProperty(TCSS.MarginProp(0, 0, 8, 20));
    end;
    with CSSBuilder.AddSelector('.message') do
    begin
      AddProperty(TCSS.TextAlignProp(ctaCenter));
      AddProperty(TCSS.MarginProp(cssTop, 30));
      AddProperty(TCSS.FontWeightProp(cfwBold));
    end;
    with CSSBuilder.AddSelector('.error-heading') do
    begin
      AddProperty(TCSS.ColorProp(clWarningText));
      AddProperty(TCSS.FontWeightProp(cfwBold));
    end;
    with CSSBuilder.AddSelector('.error-message') do
    begin
      AddProperty(TCSS.MarginProp(cssLeft, 20));
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

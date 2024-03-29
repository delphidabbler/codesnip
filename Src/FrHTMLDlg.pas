{
 * This Source Code Form is subject to the terms of the Mozilla Public License,
 * v. 2.0. If a copy of the MPL was not distributed with this file, You can
 * obtain one at https://mozilla.org/MPL/2.0/
 *
 * Copyright (C) 2005-2022, Peter Johnson (gravatar.com/delphidabbler).
 *
 * Implements a frame containing a web browser control that displays HTML
 * content that takes on the appearance of a dialogue box.
}


unit FrHTMLDlg;


interface


uses
  // Delphi
  OleCtrls,
  SHDocVw,
  Classes,
  Controls,
  ExtCtrls,
  Forms,
  // Project
  Browser.UUIMgr,
  FrBrowserBase,
  UCSSBuilder;


type

  {
  THTMLDlgFrame:
    Frame containing a web browser control that displays HTML content and takes
    on the appearance of a dialog box.
  }
  THTMLDlgFrame = class(TBrowserBaseFrame)
  strict private
    fOwner: TForm;
      {Form that owns the frame. Used to set font and colours to display in
      frame's HTML }
    function GetScrollStyle: TWBScrollbarStyle;
    procedure SetScrollStyle(const AValue: TWBScrollbarStyle);
  strict protected
    procedure BuildCSS(const CSSBuilder: TCSSBuilder); override;
  published
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
    property ScrollStyle: TWBScrollbarStyle read GetScrollStyle write SetScrollStyle default sbsHide;
  end;


implementation


uses
  // Delphi
  SysUtils,
  Graphics,
  // Project
  UCSSUtils,
  UNulDropTarget;


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
    CSSBuilder.AddSelector('body')
      .AddProperty(TCSS.BackgroundColorProp(fOwner.Color))
      .AddProperty(TCSS.FontProps(fOwner.Font))
      .AddProperty(TCSS.MarginProp(0));
    // Sets heading margins, padding and font size
    CSSFont.Assign(Self.Font);
    CSSFont.Size := CSSFont.Size + 2;
    CSSFont.Style := [fsBold];
    CSSBuilder.AddSelector('h1')
      .AddProperty(TCSS.FontProps(CSSFont))
      .AddProperty(TCSS.MarginProp(0))
      .AddProperty(TCSS.PaddingProp(0));
    // Sets paragraph margins and padding
    CSSBuilder.AddSelector('p')
      .AddProperty(TCSS.MarginProp(cssTop, 6))
      .AddProperty(TCSS.MarginProp(cssBottom, 0))
      .AddProperty(TCSS.PaddingProp(0));
    CSSBuilder.AddSelector('ul')
      .AddProperty(TCSS.MarginProp(cssTop, 6))
      .AddProperty(TCSS.MarginProp(cssBottom, 0))
      .AddProperty(TCSS.PaddingProp(0));
    // Sets table font info
    CSSBuilder.AddSelector('table')
      .AddProperty(TCSS.FontProps(fOwner.Font));
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

function THTMLDlgFrame.GetScrollStyle: TWBScrollbarStyle;
begin
  Result := WBController.UIMgr.ScrollbarStyle;
end;

procedure THTMLDlgFrame.SetScrollStyle(const AValue: TWBScrollbarStyle);
begin
  if AValue <> GetScrollStyle then
    WBController.UIMgr.ScrollbarStyle := AValue;
end;

end.


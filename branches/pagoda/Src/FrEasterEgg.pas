{
 * This Source Code Form is subject to the terms of the Mozilla Public License,
 * v. 2.0. If a copy of the MPL was not distributed with this file, You can
 * obtain one at http://mozilla.org/MPL/2.0/
 *
 * Copyright (C) 2009-2013, Peter Johnson (www.delphidabbler.com).
 *
 * $Rev$
 * $Date$
 *
 * Implements a frame that hosts the HTML, CSS and JavaScript used to display
 * the program's animated easter egg.
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
  Browser.UUIMgr, UCSSUtils, UFontHelper;

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
    TFontHelper.SetContentFont(CSSFont);
    with CSSBuilder.AddSelector('body') do
      AddProperty(TCSS.FontProps(CSSFont));
  finally
    CSSFont.Free;
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


{
 * UGraphicUtils.pas
 *
 * Utility routines used for working with graphics.
 *
 * v1.0 of 21 Nov 2006  - Original version.
 * v1.1 of 25 Feb 2007  - Added new DrawTextOutline routine.
 * v1.2 of 06 Dec 2008  - Added new StringExtent and GetTextRect routines along
 *                        with supporting private routines.
 * v1.3 of 15 Dec 2008  - Modified to use TRectEx record instead of TRect.
 * v1.4 of 10 May 2009  - Made CreateDisplayDC function public.
 * v1.5 of 17 Jun 2009  - Removed redundant DrawTextOutline routine.
 * v1.6 of 10 Jul 2009  - Added an overloaded StringExtent routine that gets
 *                        size of string word-wrapped within a maximum width.
 *                      - Added private support routines to create and free
 *                        display canvases for StringExtent routines.
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
 * The Original Code is UGraphicUtils.pas
 *
 * The Initial Developer of the Original Code is Peter Johnson
 * (http://www.delphidabbler.com/).
 *
 * Portions created by the Initial Developer are Copyright (C) 2006-2009 Peter
 * Johnson. All Rights Reserved.
 *
 * ***** END LICENSE BLOCK *****
}


unit UGraphicUtils;


interface


uses
  // Delphi
  Windows, Graphics,
  // Project
  UStructs;


function CreateDisplayDC: HDC;
  {Creates a display device context.
  }

type
  {
  TGradientDirection:
    Direction of colour change in a colour gradient.
  }
  TGradientDirection = (gdHorizontal, gdVertical);

procedure GradientFill(const Canvas: TCanvas; const Rect: TRectEx;
  const SecondColour: TColor; const Direction: TGradientDirection);
  {Gradient fills an area of a canvas.
    @param Canvas [in] Canvas on which to draw. Canvas's brush colour is used
      for start of gradient.
    @param Rect [in] Area of canvas to be filled.
    @param SecondColour [in] 2ndary colour to be used in gradient.
    @param Direction [in] Direction of gradient: horizontal or vertical.
  }

function StringExtent(const S: string; const Font: TFont;
  const MaxWidth: Integer): TSize; overload;
  {Calculates the width and height of a string when rendered in a specified
  font, word-wrapped within a maximum width.
    @param S [in] String for which size is required.
    @param Font [in] Font used to render string.
    @param MaxWidth [in] Maximum width of rendered string; string word-wraps at
      this width.
    @return Structure containing width and height of string in pixels.
  }

function StringExtent(const S: string; const Font: TFont): TSize; overload;
  {Calculates the width and height of a string when rendered in a specified
  font.
    @param S [in] String for which size is required.
    @param Font [in] Font used to render string.
    @return Structure containing width and height of string in pixels.
  }

function GetTextRect(const Text: string; const Canvas: TCanvas;
  const Rect: TRect; const Flags: Longint): TRect;
  {Gets rectangle of size required to display text in a specified canvas.
    @param Text [in] Text to display.
    @param Canvas [in] Canvas on which text to be displayed.
    @param Rect [in] Rectangle in which to display text. Bottom field ignored.
    @param Flags [in] Flags to use in outputting text.
  }


implementation


uses
  // Project
  SysUtils;


{ Helper routines }

procedure ExtractRGB(const Colour: TColor; out Red, Green, Blue: Byte);
  {Extract constituant RGB values from a colour. System colours are converted to
  their actual values per current Window display settings.
    @param Colour [in] Colour for which RGB values are required.
    @param Red [out] Set to red value of colour.
    @param Green [out] Set to green value of colour.
    @param Blue [out] Set to blue value of colour.
  }
var
  RGB: TColorRef; // RGB triple equivalent of given colour
begin
  RGB := ColorToRGB(Colour);  // this ensures system colours are converted
  Red := GetRValue(RGB);
  Green := GetGValue(RGB);
  Blue := GetBValue(RGB);
end;

function CreateDisplayCanvas(const Font: TFont): TCanvas;
  {Creates a display canvas that uses a specified font.
    @param Font [in] Font to be used by canvas.
    @return Required canvas. Should be freed using FreeDisplayCanvas.
  }
begin
  Assert(Assigned(Font));
  Result := TCanvas.Create;
  Result.Handle := CreateDisplayDC;
  Result.Font := Font;
end;

procedure FreeDisplayCanvas(var Canvas: TCanvas);
  {Releases resources and frees a canvas object created by CreateDisplayCanvas.
    @param Canvas [in/out] Canvas to be freed. Set to nil once freed.
  }
begin
  if Assigned(Canvas) then
    try
      DeleteDC(Canvas.Handle);
      Canvas.Handle := 0;
    finally
      FreeAndNil(Canvas);
    end;
end;

{ Public routines }

function CreateDisplayDC: HDC;
  {Creates a display device context.
  }
begin
  Result := CreateDC('DISPLAY', nil, nil, nil);
end;

procedure GradientFill(const Canvas: TCanvas; const Rect: TRectEx;
  const SecondColour: TColor; const Direction: TGradientDirection);
  {Gradient fills an area of a canvas.
    @param Canvas [in] Canvas on which to draw. Canvas's brush colour is used
      for start of gradient.
    @param Rect [in] Area of canvas to be filled.
    @param SecondColour [in] 2ndary colour to be used in gradient.
    @param Direction [in] Direction of gradient: horizontal or vertical.
  }
var
  GradientSize: Integer;          // number of pixels gradient changes over
  StartR, StartG, StartB: Byte;   // R, G & B values of primary gradient colour
  EndR, EndG, EndB: Byte;         // R, G & B values of 2ndary gradient colour
  R, G, B: Byte;                  // R, G & B values of intermediate colour
  I: Integer;                     // loops thru pixels of gradient
  OldPenStyle: TPenStyle;         // stores canvas's original pen style
  OldPenWidth: Integer;           // stores canvas's original pen width
  OldPenColor: TColor;            // stores canvas's original pen colour
begin
  // Record current pen attributes for restoration later
  OldPenStyle := Canvas.Pen.Style;
  OldPenWidth := Canvas.Pen.Width;
  OldPenColor := Canvas.Pen.Color;
  try
    // Get number of pixels over which gradient extends
    if Direction = gdHorizontal then
      GradientSize := Rect.Width
    else
      GradientSize := Rect.Height;
    // Record RGB values of primary and secondary gradient colours
    ExtractRGB(Canvas.Brush.Color, StartR, StartG, StartB);
    ExtractRGB(SecondColour, EndR, EndG, EndB);
    // Ensure pen is 1px wide and solid
    Canvas.Pen.Width := 1;
    Canvas.Pen.Style := psSolid;
    // Draw the gradient
    // loop thru each pixel across gradient
    for I := 0 to Pred(GradientSize) do
    begin
      // calculate required intermediate colour
      R := StartR + I * (EndR - StartR) div GradientSize;
      G := StartG + I * (EndG - StartG) div GradientSize;
      B := StartB + I * (EndB - StartB) div GradientSize;
      Canvas.Pen.Color := TColor(RGB(R, G, B));
      // draw line of constant colour in correct direction
      if Direction = gdHorizontal then
      begin
        Canvas.MoveTo(Rect.Left + I, Rect.Top);
        Canvas.LineTo(Rect.Left + I, Rect.Bottom);
      end
      else
      begin
        Canvas.MoveTo(Rect.Left, Rect.Top  + I);
        Canvas.LineTo(Rect.Right + I, Rect.Top + I);
      end;
    end;
  finally
    // Restore pen's properties
    Canvas.Pen.Style := OldPenStyle;
    Canvas.Pen.Width := OldPenWidth;
    Canvas.Pen.Color := OldPenColor;
  end;
end;

function StringExtent(const S: string; const Font: TFont;
  const MaxWidth: Integer): TSize; overload;
  {Calculates the width and height of a string when rendered in a specified
  font, word-wrapped within a maximum width.
    @param S [in] String for which size is required.
    @param Font [in] Font used to render string.
    @param MaxWidth [in] Maximum width of rendered string; string word-wraps at
      this width.
    @return Structure containing width and height of string in pixels.
  }
var
  Canvas: TCanvas; // canvas used to measure text extent
  Rect: TRectEx;   // zero based rectangle required to contain rendered string
begin
  Canvas := CreateDisplayCanvas(Font);
  try
    Rect := GetTextRect(
      S, Canvas, TRectEx.Create(0, 0, MaxWidth, 0), DT_WORDBREAK
    );
    Result.cx := Rect.Width;
    Result.cy := Rect.Height;
  finally
    FreeDisplayCanvas(Canvas);
  end;
end;

function StringExtent(const S: string; const Font: TFont): TSize; overload;
  {Calculates the width and height of a string when rendered in a specified
  font.
    @param S [in] String for which size is required.
    @param Font [in] Font used to render string.
    @return Structure containing width and height of string in pixels.
  }
var
  Canvas: TCanvas; // canvas used to measure text extent
begin
  Canvas := CreateDisplayCanvas(Font);
  try
    Result := Canvas.TextExtent(S);
  finally
    FreeDisplayCanvas(Canvas);
  end;
end;

function GetTextRect(const Text: string; const Canvas: TCanvas;
  const Rect: TRect; const Flags: Longint): TRect;
  {Gets rectangle of size required to display text in a specified canvas.
    @param Text [in] Text to display.
    @param Canvas [in] Canvas on which text to be displayed.
    @param Rect [in] Rectangle in which to display text. Bottom field ignored.
    @param Flags [in] Flags to use in outputting text.
  }
begin
  Result := Rect;
  Result.Bottom := 0;
  DrawText(Canvas.Handle, PChar(Text), -1, Result, Flags or DT_CALCRECT);
end;

end.


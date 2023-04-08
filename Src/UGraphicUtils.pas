{
 * This Source Code Form is subject to the terms of the Mozilla Public License,
 * v. 2.0. If a copy of the MPL was not distributed with this file, You can
 * obtain one at https://mozilla.org/MPL/2.0/
 *
 * Copyright (C) 2006-2021, Peter Johnson (gravatar.com/delphidabbler).
 *
 * Utility routines used for working with graphics.
}


unit UGraphicUtils;


interface


uses
  // Delphi
  Windows, Graphics;

function CreateDisplayDC: HDC;
  {Creates a display device context.
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

///  <summary>Returns width, in pixels, of the widest of the given strings when
///  rendered a specified font.</summary>
///  <param name="AStrings"><c>array of string</c> [in] Strings whose rendered
///  width is to be measured.</param>
///  <param name="AFont"><c>TFont</c> [in] Font in which strings are to be
///  rendered.</param>
///  <returns><c>SmallInt</c>. Width of widest string in array in pixels.
///  </returns>
function MaxStringWidthPx(const AStrings: array of string; const AFont: TFont):
  SmallInt;

///  <summary>Returns width, in twips, of the widest of the given strings when
///  rendered a specified font.</summary>
///  <param name="AStrings"><c>array of string</c> [in] Strings whose rendered
///  width is to be measured.</param>
///  <param name="AFont"><c>TFont</c> [in] Font in which strings are to be
///  rendered.</param>
///  <returns><c>SmallInt</c>. Width of widest string in array in twips.
///  </returns>
function MaxStringWidthTwips(const AStrings: array of string;
  const AFont: TFont): SmallInt;

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
  // Delphi
  SysUtils,
  // Project
  UStructs;


{ Helper routines }

function CreateDisplayCanvas(const Font: TFont): TCanvas;
  {Creates a display canvas that uses a specified font.
    @param Font [in] Font to be used by canvas.
    @return Required canvas. Should be freed using FreeDisplayCanvas.
  }
begin
  Assert(Assigned(Font), 'CreateDisplayCanvas: Font is nil');
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

///  <summary>Returns width of the widest of the given strings when rendered a
///  specified font.</summary>
///  <remarks>Width is calculated in pixels, but is converted to returned value
///  by closure passed as a parameter.</remarks>
///  <param name="AStrings"><c>array of string</c> [in] Strings whose rendered
///  width is to be measured.</param>
///  <param name="AFont"><c>TFont</c> [in] Font in which strings are to be
///  rendered.</param>
///  <param name="AConverter"><c>TFunc&lt;HDC, Integer, SmallInt&gt;</c> [in]
///  Converter function used to convert result to required units, using the
///  handle of the font canvas.</param>
///  <returns><c>SmallInt</c>. Width of widest string in array in twips.
///  </returns>
function InternalMaxStringWidth(const AStrings: array of string;
  const AFont: TFont; const AConverter: TFunc<HDC, Integer, SmallInt>):
  SmallInt;
var
  Str: string;
  StrWidth: Integer;
  MaxStrWidth: Integer;
  Canvas: TCanvas; // canvas used to measure text extent
begin
  MaxStrWidth := 0;
  Canvas := CreateDisplayCanvas(AFont);
  try
    for Str in AStrings do
    begin
      StrWidth := Canvas.TextExtent(Str).cx;
      if StrWidth > MaxStrWidth then
        MaxStrWidth := StrWidth;
    end;
    Result := AConverter(Canvas.Handle, MaxStrWidth);
  finally
    FreeDisplayCanvas(Canvas);
  end;
end;

{ Public routines }

function CreateDisplayDC: HDC;
  {Creates a display device context.
  }
begin
  Result := CreateDC('DISPLAY', nil, nil, nil);
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

function MaxStringWidthTwips(const AStrings: array of string;
  const AFont: TFont): SmallInt;
begin
  Result := InternalMaxStringWidth(
    AStrings,
    AFont,
    function (CanvasHandle: HDC; MaxStrWidthPx: Integer): SmallInt
    var
      PxPerInchX: Integer;
    const
      TwipsPerInch = 1440;
    begin
      // convert pixels to twips
      PxPerInchX := GetDeviceCaps(CanvasHandle, LOGPIXELSX);
      Result := SmallInt(Round(MaxStrWidthPx * TwipsPerInch / PxPerInchX));
    end
  );
end;

function MaxStringWidthPx(const AStrings: array of string; const AFont: TFont):
  SmallInt;
begin
  Result := InternalMaxStringWidth(
    AStrings,
    AFont,
    function (CanvasHandle: HDC; StrWidthPx: Integer): SmallInt
    begin
      // no conversion
      Result := SmallInt(StrWidthPx);
    end
  );
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


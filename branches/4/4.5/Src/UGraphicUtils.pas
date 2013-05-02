{
 * This Source Code Form is subject to the terms of the Mozilla Public License,
 * v. 2.0. If a copy of the MPL was not distributed with this file, You can
 * obtain one at http://mozilla.org/MPL/2.0/
 *
 * Copyright (C) 2006-2012, Peter Johnson (www.delphidabbler.com).
 *
 * $Rev$
 * $Date$
 *
 * Utility routines used for working with graphics.
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


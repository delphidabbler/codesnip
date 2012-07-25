{
 * UGraphicUtils.pas
 *
 * Utility routines used for working with graphics.
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
 * The Original Code is UGraphicUtils.pas
 *
 * The Initial Developer of the Original Code is Peter Johnson
 * (http://www.delphidabbler.com/).
 *
 * Portions created by the Initial Developer are Copyright (C) 2006-2010 Peter
 * Johnson. All Rights Reserved.
 *
 * Contributor(s)
 *   NONE
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


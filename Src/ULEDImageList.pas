{
 * ULEDImageList.pas
 *
 * Defines a custom image list that provides a list of LED images. Image list is
 * automatically loaded from resources when class is instantiated.
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
 * The Original Code is ULEDImageList.pas
 *
 * The Initial Developer of the Original Code is Peter Johnson
 * (http://www.delphidabbler.com/).
 *
 * Portions created by the Initial Developer are Copyright (C) 2009-2010 Peter
 * Johnson. All Rights Reserved.
 *
 * Contributor(s)
 *   NONE
 *
 * ***** END LICENSE BLOCK *****
}


unit ULEDImageList;


interface


uses
  // Delphi
  SysUtils, Classes, ImgList, Windows, Graphics,
  // Project
  Compilers.UGlobals;


type
  {
  TLEDImageList:
    Custom image list that provides a list of LED images. Image list is
    automatically loaded from resources when class is instantiated. Image list
    can be accessed by compile result as well as by the normal integer indexing.
  }
  TLEDImageList = class(TCustomImageList)
  strict private
    function CompResToIdx(const CompRes: TCompileResult): Integer;
      {Maps a compile result onto an image index.
        @param CompRes [in] Compile result to be mapped.
        @return Match image index.
      }
  public
    constructor Create(AOwner: TComponent); override;
      {Class constructor. Sets up object and reads images from resources.
      }
    procedure Draw(const Canvas: TCanvas; const TopLeft: TPoint;
      const CompRes: TCompileResult); overload;
      {Overloaded method that draws a LED specified by its compile result.
        @param Canvas [in] Canvas on which to draw LED.
        @param TopLeft [in] Offset of top left corner of drawing in Canvas.
        @param CompRes [in] Compile result that specifies the LED to draw.
      }
  end;


implementation


{
  Note: resources must contain a valid bitmap named "LEDS" that combines all
  four bitmaps, each 18px x 18px. This combined bitmap may be in one of three
  formats:
    18px x 72px => LEDs are in a vertical column
    36px x 36px => LEDs are in two rows of two, varying by row
    72px x 18px => LEDs are in a horizontal row
  Ordering must be
     S    or    SW    or   SWEQ
     W          EQ
     E
     Q
  Where S = success (green), W = warning (yellow), E = error (red) and
  Q = query (grey).
  The mask colour must be clFuchsia.
}


{ TLEDImageList }

function TLEDImageList.CompResToIdx(const CompRes: TCompileResult): Integer;
  {Maps a compile result onto an image index.
    @param CompRes [in] Compile result to be mapped.
    @return Match image index.
  }
begin
  // Assumes images are ordered in same order as TCompileResult enumeration
  Result := Ord(CompRes);
end;

constructor TLEDImageList.Create(AOwner: TComponent);
  {Class constructor. Sets up object and reads images from resources.
  }
var
  BmpStrip: TBitmap;    // strip containg all bitmaps from resources
  LEDBmp: TBitmap;      // a bitmap of a single LED
  RS: TResourceStream;  // stream used to resources
  LeftOffset: Integer;  // left offset of a bitmap in "strip"
  TopOffset: Integer;   // top offset of a bitmap in "strip"
begin
  inherited;
  // Set required size of images in image list
  Width := 18;
  Height := 18;
  // Load "strip" of all LED bitmaps from resources.
  // note that we load from RCDATA since bitmap is 32 bit and resource compiler
  // won't recognise this as valid if placed in a BITMAP resource
  BmpStrip := TBitmap.Create;
  try
    RS := TResourceStream.Create(HInstance, 'LEDS', RT_RCDATA);
    try
      BmpStrip.LoadFromStream(RS);
    finally
      FreeAndNil(RS);
    end;
    // Split strip up into individual bitmaps and load them into image list
    // create bitmap of correct size and bit depth: we re-use it for each bitmap
    LEDBmp := TBitmap.Create;
    try
      LEDBmp.Width := Width;
      LEDBmp.Height := Height;
      LEDBmp.PixelFormat := BmpStrip.PixelFormat;
      // scan across then down bitmaps: works for 1*4 or 2*2 bitmaps
      TopOffset := 0;
      while TopOffset < BmpStrip.Height do
      begin
        LeftOffset := 0;
        while LeftOffset < BmpStrip.Width do
        begin
          // copy the bitmap from the "strip"
          LEDBmp.Canvas.CopyRect(
            Rect(0, 0, Width, Height),
            BmpStrip.Canvas,
            Bounds(LeftOffset, TopOffset, Width, Height)
          );
          Self.AddMasked(LEDBmp, clFuchsia);
          Inc(LeftOffset, Width);
        end;
        Inc(TopOffset, Height);
      end;
    finally
      FreeAndNil(LEDBmp);
    end;
  finally
    FreeAndNil(BmpStrip);
  end;
end;

procedure TLEDImageList.Draw(const Canvas: TCanvas; const TopLeft: TPoint;
  const CompRes: TCompileResult);
  {Overloaded method that draws a LED specified by its compile result.
    @param Canvas [in] Canvas on which to draw LED.
    @param TopLeft [in] Offset of top left corner of drawing in Canvas.
    @param CompRes [in] Compile result that specifies the LED to draw.
  }
begin
  Self.Draw(Canvas, TopLeft.X, TopLeft.Y, CompResToIdx(CompRes));
end;

end.


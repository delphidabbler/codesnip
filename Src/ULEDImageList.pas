{
 * This Source Code Form is subject to the terms of the Mozilla Public License,
 * v. 2.0. If a copy of the MPL was not distributed with this file, You can
 * obtain one at https://mozilla.org/MPL/2.0/
 *
 * Copyright (C) 2009-2020, Peter Johnson (gravatar.com/delphidabbler).
 *
 * Defines a custom image list that provides a list of LED images. Image list is
 * automatically loaded from resources when class is instantiated.
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
      {Constructor. Sets up object and reads images from resources.
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

uses
  // Project
  UClassHelpers;


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
  {Constructor. Sets up object and reads images from resources.
  }
begin
  inherited;
  // Loads images: uses class helper method from TImageListHelper
  LoadFromResource(RT_RCDATA, 'LEDS', 18, clFuchsia);
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


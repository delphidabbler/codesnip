{
 * This Source Code Form is subject to the terms of the Mozilla Public License,
 * v. 2.0. If a copy of the MPL was not distributed with this file, You can
 * obtain one at http://mozilla.org/MPL/2.0/
 *
 * Copyright (C) 2012, Peter Johnson (www.delphidabbler.com).
 *
 * $Rev$
 * $Date$
 *
 * Provides a class helper for TCustomImageList that adds a method to load
 * images from any resource type.
}


unit UImageListHelper;


interface


uses
  // Delphi
  ImgList, Graphics;


type
  ///  <summary>Class helper that adds a method to TCustomImageList that can
  ///  load images from resources of any type into the image list.</summary>
  ///  <remarks>By default TCustomImageList can only load images from Bitmap,
  ///  Cursor or Icon resources. Because of limitations of the resource compiler
  ///  used to build CodeSnip, 32 bit bitmaps can't be added to BITMAP resources
  ///  and so are stored in a format not supported by TCustomImageList.
  ///  </remarks>
  TImageListHelper = class helper for TCustomImageList
  public
    ///  <summary>Loads images from given resource.</summary>
    ///  <param name="ResType">PChar [in] Type of resource.</param>
    ///  <param name="ResName">string [in] Name of resource.</param>
    ///  <param name="Size">Integer [in] Size of individual images in resource
    ///  (must be square).</param>
    ///  <param name="MaskColour">TColor [in] Colour used as transparency mask.
    ///  </param>
    ///  <remarks>Resource is loaded from the current program instance.
    ///  </remarks>
    procedure LoadFromResource(ResType: PChar; const ResName: string;
      Size: Integer; MaskColour: TColor);
  end;


implementation


uses
  // Delphi
  Classes;


{ TImageListHelper }

procedure TImageListHelper.LoadFromResource(ResType: PChar;
  const ResName: string; Size: Integer; MaskColour: TColor);
var
  MasterBmp: TBitmap;   // bitmap containing all images
  Bmp: TBitmap;         // a bitmap of a single image
  RS: TResourceStream;  // stream used to resources
  LeftOffset: Integer;  // left offset of an image in master
  TopOffset: Integer;   // top offset of an image in master

begin
  // Set required size of images in image list
  Width := Size;
  Height := Size;
  // Load master bitmap containing all images from resources.
  MasterBmp := TBitmap.Create;
  try
    RS := TResourceStream.Create(HInstance, ResName, ResType);
    try
      MasterBmp.LoadFromStream(RS);
    finally
      RS.Free;
    end;
    // Split master bitmap into individual bitmaps and load them into image list
    // create bitmap of correct size and bit depth: we re-use it for each bitmap
    Bmp := TBitmap.Create;
    try
      Bmp.Width := Width;
      Bmp.Height := Height;
      Bmp.PixelFormat := MasterBmp.PixelFormat;
      // scan across then down bitmaps: works for strips or rectangular master
      // bitmaps
      TopOffset := 0;
      while TopOffset < MasterBmp.Height do
      begin
        LeftOffset := 0;
        while LeftOffset < MasterBmp.Width do
        begin
          // copy the bitmap from the master into the image list
          Bmp.Canvas.CopyRect(
            Rect(0, 0, Width, Height),
            MasterBmp.Canvas,
            Bounds(LeftOffset, TopOffset, Width, Height)
          );
          Self.AddMasked(Bmp, clFuchsia);
          Inc(LeftOffset, Width);
        end;
        Inc(TopOffset, Height);
      end;
    finally
      Bmp.Free;
    end;
  finally
    MasterBmp.Free;
  end;
end;

end.

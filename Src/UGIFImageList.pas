{
 * This Source Code Form is subject to the terms of the Mozilla Public License,
 * v. 2.0. If a copy of the MPL was not distributed with this file, You can
 * obtain one at https://mozilla.org/MPL/2.0/
 *
 * Copyright (C) 2008-2021, Peter Johnson (gravatar.com/delphidabbler).
 *
 * Image list descendant that enables representations of GIF images loaded from
 * HTML resource to be added. Resource names are mapped to image indices.
}


unit UGIFImageList;


interface


uses
  // Delphi
  Classes, Generics.Collections, Windows, Graphics, ImgList;


type

  {
  TGIFImageList:
    Extension of image list that enables representations of GIF images loaded
    from HTML resource to be added. Resource names are mapped to image indices.
  }
  TGIFImageList = class(TCustomImageList)
  strict private
    type
      // Class that maps GIF resource name to index of image in image list
      TResNamesMap = TDictionary<string,Integer>;
    var
      fGIFResNames: TResNamesMap;
        {Map of GIF resource names to index of matching image in image list}
    function CreateBMPFromGIFRes(const GIFResName: string): TBitmap;
      {Creates bitmap that is a copy of a GIF stored in HTML resources.
        @param GIFResName [in] Name of resource containing GIF.
        @return Bitmap representation of GIF.
      }
  public
    constructor Create(AOwner: TComponent); override;
      {Constructor. Sets up object.
        @param AOwner [in] Reference to any component that owns image list.
      }
    destructor Destroy; override;
      {Destructor. Tears down object.
      }
    function ImageIndex(const GIFResName: string): Integer;
      {Gets image of representation of a GIF resource in image list.
        @param GIFResName [in] Name of GIF resource.
        @return Index of image in image list or -1 if no such image.
      }
    function AddGIFImage(const GIFResName: string): Integer;
      {Adds an image to the list that is obtained by converting a GIF image
      stored loaded from HTML resources.
        @param GIFResName [in] Name of resource storing GIF image.
      }
    procedure Clear;
      {Clears image list and record of loaded resource names.
      }
  end;


implementation


uses
  // Delphi
  GIFImg, UClassHelpers,
  // Project
  UComparers;


{ TGIFImageList }

function TGIFImageList.AddGIFImage(const GIFResName: string): Integer;
  {Adds an image to the list that is obtained by converting a GIF image stored
  loaded from HTML resources.
    @param GIFResName [in] Name of resource storing GIF image.
  }
var
  Bmp: TBitmap;   // bitmap used to store GIF conversion
begin
  // Check if image is already in image list
  Result := ImageIndex(GIFResName);
  if Result >= 0 then
    Exit;
  // Load GIF from resources, converted to bitmap, and add to image list
  Bmp := CreateBMPFromGIFRes(GIFResName);
  try
    Result := Self.AddMasked(Bmp, Bmp.TransparentColor);
  finally
    Bmp.Free;
  end;
  // Record resource name and index of associated image in image list
  fGIFResNames.Add(GIFResName, Result);
end;

procedure TGIFImageList.Clear;
  {Clears image list and record of loaded resource names.
  }
begin
  fGIFResNames.Clear;
  inherited Clear;
end;

constructor TGIFImageList.Create(AOwner: TComponent);
  {Constructor. Sets up object.
    @param AOwner [in] Reference to any component that owns image list.
  }
begin
  inherited Create(AOwner);
  // Set up map: we ignore case of resource names
  fGIFResNames := TResNamesMap.Create(TTextEqualityComparer.Create);
end;

function TGIFImageList.CreateBMPFromGIFRes(const GIFResName: string): TBitmap;
  {Creates bitmap that is a copy of a GIF stored in HTML resources.
    @param GIFResName [in] Name of resource containing GIF.
    @return Bitmap representation of GIF.
  }
var
  GIF: TGIFImage;     // GIF image object
begin
  GIF := TGIFImage.Create;
  try
    // Open stream onto GIF in HTML resources and load into GIF image object
    GIF.LoadFromResource(HInstance, GIFResName, RT_HTML);
    // Make bitmap copy of GIF
    Result := TBitmap.Create;
    Result.Assign(GIF);
    Result.TransparentColor := Result.Canvas.Pixels[0, 0];
  finally
    GIF.Free;
  end;
end;

destructor TGIFImageList.Destroy;
  {Destructor. Tears down object.
  }
begin
  fGIFResNames.Free;
  inherited;
end;

function TGIFImageList.ImageIndex(const GIFResName: string): Integer;
  {Gets image of representation of a GIF resource in image list.
    @param GIFResName [in] Name of GIF resource.
    @return Index of image in image list or -1 if no such image.
  }
begin
  if not fGIFResNames.ContainsKey(GIFResName) then
    Result := -1
  else
    Result := fGIFResNames[GIFResName];
end;

end.


{
 * This Source Code Form is subject to the terms of the Mozilla Public License,
 * v. 2.0. If a copy of the MPL was not distributed with this file, You can
 * obtain one at https://mozilla.org/MPL/2.0/
 *
 * Copyright (C) 2009-2020, Peter Johnson (gravatar.com/delphidabbler).
 *
 * Defines a class that is used to manage image lists containing drop down
 * button images for display in UI widgets.
}


unit UDropDownButtons;


interface


uses
  // Delphi
  Classes, Controls, Windows, Graphics,
  // Project
  UUIWidgetImages;


type
  {
  TDropDownButtons:
    Class used to manage image lists containing drop down button images for
    display in UI widgets. The class also maps drop down button identifiers to
    indexes into image lists. Different images are loaded depending on whether
    themes are active. Update is called to change bitmaps when themes are
    changed.
  }
  TDropDownButtons = class(TUIWidgetImages)
  strict private
    class function ButtonImageIdx(const Hot, Focussed: Boolean): Integer;
      {Gets index of a drop down button in image list.
        @param Hot [in] Flag indicating whether button is to be hot or normal.
        @param Focussed [in] Flag indicating whether button is focussed.
        @return Required image index.
      }
  strict protected
    procedure RecreateImages; override;
      {Recreates image list containing appropriate drop-down buttons images.
      Images depend on whether themes are active.
      }
    function GetImageSize: TSize; override;
      {Gets size of drop down button images. Size of image may differ depending
      on whether themes are active.
        @return Size of button in pixels.
      }
  public
    constructor Create(AOwner: TComponent);
      {Object constructor. Sets up object and loads button images into image
      list.
        @param AOwner [in] Component that owns this object.
      }
    procedure Draw(const Canvas: TCanvas; const TopLeft: TPoint;
      const Hot, Focussed: Boolean);
      {Draws the required drop down button on a canvas.
        @param Canvas [in] Canvas where button is to be drawn.
        @param TopLeft [in] Top left corner of button in canvas.
        @param Hot [in] Whether button to be drawn in hot state.
        @param Focussed [in] Whether button is to display a focus rectangle.
      }
    property Images;
      {Image list containing drop down buttons. Inherited protected property
      made public}
  end;


implementation


uses
  // Delphi
  Themes,
  // Project
  UStructs, UThemesEx;


{ TDropDownButtons }

class function TDropDownButtons.ButtonImageIdx(const Hot,
  Focussed: Boolean): Integer;
  {Gets index of a drop down button in image list.
    @param Hot [in] Flag indicating whether button is to be hot or normal.
    @param Focussed [in] Flag indicating whether button is focussed.
    @return Required image index.
  }
begin
  Result := Ord(Hot) + Ord(Focussed) shl 1;
end;

constructor TDropDownButtons.Create(AOwner: TComponent);
  {Object constructor. Sets up object and loads button images into image list.
    @param AOwner [in] Component that owns this object.
  }
begin
  inherited Create(TImageList.Create(AOwner), True);
end;

procedure TDropDownButtons.Draw(const Canvas: TCanvas; const TopLeft: TPoint;
  const Hot, Focussed: Boolean);
  {Draws the required drop down button on a canvas.
    @param Canvas [in] Canvas where button is to be drawn.
    @param TopLeft [in] Top left corner of button in canvas.
    @param Hot [in] Whether button to be drawn in hot state.
    @param Focussed [in] Whether button is to display a focus rectangle.
  }
begin
  Images.Draw(
    Canvas,
    TopLeft.X,
    TopLeft.Y,
    ButtonImageIdx(Hot, Focussed)
  );
end;

function TDropDownButtons.GetImageSize: TSize;
  {Gets size of drop down button images. Size of image may differ depending on
  whether themes are active.
    @return Size of button in pixels.
  }
var
  BtnBmp: TBitmap; // Bitmap used to load combo box buttons resource
begin
  // We return same size whether themes are active or not, and this is the size
  // of non themed buttons. This is because non-themed buttons don't have a
  // fixed size. We assume all differenr button states are same size.
  BtnBmp := TBitmap.Create;
  try
    BtnBmp.Handle := LoadBitmap(0, MakeIntResource(OBM_DNARROW));
    Result.cx := BtnBmp.Width;
    Result.cy := BtnBmp.Height;
  finally
    BtnBmp.Free;
  end;
end;

procedure TDropDownButtons.RecreateImages;
  {Recreates image list containing appropriate drop-down buttons images. Images
  depend on whether themes are active.
  }

  // ---------------------------------------------------------------------------
  procedure GetNonThemedButton(const Bmp: TBitmap; const Hot: Boolean);
    {Draws a non-themed button on a bitmap.
      @param Bmp [in] Bitmap to receive drawing.
      @param Hot [in] Whether to draw button in hot or normal state.
    }
  const
    // Map of index of drop down buttons in image list to id of standard UI
    // bitmap representing button in normal and hot styles
    cUnThemedComboBoxButtons: array[Boolean]
      of Integer = (OBM_DNARROW, OBM_DNARROW);
  begin
    Bmp.Handle := LoadBitmap(
      0, MakeIntResource(cUnThemedComboBoxButtons[Hot])
    );
  end;

  procedure GetThemedButton(const Bmp: TBitmap; const Hot: Boolean);
    {Draws a themed button on a bitmap.
      @param Bmp [in] Bitmap to receive drawing.
      @param Hot [in] Whether to draw button in hot or normal state.
    }
  const
    // Map of index of drop down buttons in image list to themed element
    // representing button in normal and hot styles
    cThemedComboBoxButtons: array[Boolean]
      of TThemedComboBox = (tcDropDownButtonNormal, tcDropDownButtonHot);
  var
    BtnRect: TRect; // bounds rectangle of bitmap
  begin
    BtnRect := TRectEx.Create(0, 0, Bmp.Width, Bmp.Height);
    Bmp.Canvas.FillRect(BtnRect);
    ThemeServicesEx.DrawElement(
      cThemedComboBoxButtons[Hot], Bmp, BtnRect
    );
  end;
  // ---------------------------------------------------------------------------

const
  cMaskColour: TColor = clFuchsia;  // image list mask colour
var
  BtnBmp: TBitmap;                  // bitmap on which buttons are drawn
  FocusRect: TRectEx;               // bounds of any required focus rectangle
  Hot: Boolean;                     // loops thru not-hot / hot
  Focussed: Boolean;                // loops thru not-focussed / focussed
begin
  // Load check boxes into image list
  // We load standard combo box buttons into a bitmap which is then added to
  // image list.
  BtnBmp := TBitmap.Create;
  try
    // Prepare bitmap
    BtnBmp.Width := Images.Width;
    BtnBmp.Height := Images.Height;
    BtnBmp.Canvas.Brush.Color := cMaskColour;
    // Load normal then hot bitmaps
    for Focussed := Low(Boolean) to High(Boolean) do
    begin
      for Hot := Low(Boolean) to High(Boolean) do
      begin
        if ThemeServicesEx.ThemesEnabled then
          GetThemedButton(BtnBmp, Hot)
        else
          GetNonThemedButton(BtnBmp, Hot);
        if Focussed then
        begin
          FocusRect := TRectEx.CreateBounds(
            0, 0, BtnBmp.Width, BtnBmp.Height
          );
          FocusRect.InflateBy(-2, -2);
          BtnBmp.Canvas.DrawFocusRect(FocusRect);
        end;
        Images.AddMasked(BtnBmp, cMaskColour);
      end;
    end;
  finally
    BtnBmp.Free;
  end;
end;

end.


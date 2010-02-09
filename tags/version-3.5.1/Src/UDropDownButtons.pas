{
 * UDropDownButtons.pas
 *
 * Defines a class that is used to manage image lists containing drop down
 * button images for display in UI widgets.
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
 * The Original Code is UDropDownButtons.pas
 *
 * The Initial Developer of the Original Code is Peter Johnson
 * (http://www.delphidabbler.com/).
 *
 * Portions created by the Initial Developer are Copyright (C) 2009 Peter
 * Johnson. All Rights Reserved.
 *
 * Contributor(s)
 *   NONE
 *
 * ***** END LICENSE BLOCK *****
}


unit UDropDownButtons;


interface


uses
  // Delphi
  Classes, SyncObjs, Controls, Types, Windows, Graphics;


type
  {
  TDropDownButtons:
    Class used to manage image lists containing drop down button images for
    display in UI widgets. The class also maps drop down button identifiers to
    indexes into image lists. Different images are loaded depending on whether
    themes are active. Update is called to change bitmaps when themes are
    changed.
  }
  TDropDownButtons = class(TObject)
  strict private
    const
      cLockTimeout = 10000; {10 secs} // Event signalling timeout
    var
      fLock: TSimpleEvent;      // Signal locks access to Update event
      fImages: TImageList;      // Image list that stores drop down button images
      fOnChange: TNotifyEvent;  // OnChange event handler
    function ButtonSize: TSize;
      {Gets size of drop down button images.
        @return Size of button in pixels.
      }
    procedure SetImgListSize;
      {Sets size of bitmaps expected by image list per dimensions of buttons
      used.
      }
    procedure Update;
      {Updates button images in image list. Buttons used depend on whether
      themes are active.
      }
    procedure ThemeChangeListener(Sender: TObject);
      {Handles theme services change event. Updates button images according to
      whether themes are in use or not.
        @param Sender [in] Not used.
      }
    class function ButtonImageIdx(const Hot, Focussed: Boolean): Integer;
      {Gets index of a drop down button in image list.
        @param Hot [in] Flag indicating whether button is to be hot or normal.
        @param Focussed [in] Flag indicating whether button is focussed.
        @return Required image index.
      }
  public
    constructor Create(AOwner: TComponent);
      {Class constructor. Sets up object and loads button images into image
      list.
      }
    destructor Destroy; override;
      {Class destructor. Tears down object.
      }
    procedure Draw(const Canvas: TCanvas; const TopLeft: TPoint;
      const Hot, Focussed: Boolean);
      {Draws the required drop down button on a canvas.
        @param Canvas [in] Canvas where button is to be drawn.
        @param TopLeft [in] Top left corner of button in canvas.
        @param Hot [in] Whether button to be drawn in hot state.
        @param Focussed [in] Whether button is to display a focus rectangle.
      }
    property Images: TImageList read fImages;
      {Image list containing drop down buttons}
    property OnChange: TNotifyEvent
      read fOnChange write fOnChange;
      {Event triggered when image list changes, i.e. when themes change}
  end;


implementation


uses
  // Delphi
  SysUtils, Themes,
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

function TDropDownButtons.ButtonSize: TSize;
  {Gets size of drop down button images.
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
    FreeAndNil(BtnBmp);
  end;
end;

constructor TDropDownButtons.Create(AOwner: TComponent);
  {Class constructor. Sets up object and loads button images into image
  list.
  }
begin
  inherited Create;
  fImages := TImageList.Create(AOwner);
  fImages.Width := 16;
  fImages.Height := 16;
  fLock := TSimpleEvent.Create; // lock for protected sections: needed by Update
  fLock.SetEvent;
  Update;   // loads check boxes into image list
  ThemeServicesEx.AddChangeEventHandler(ThemeChangeListener);
end;

destructor TDropDownButtons.Destroy;
  {Class destructor. Tears down object.
  }
begin
  ThemeServicesEx.RemoveChangeEventHandler(ThemeChangeListener);
  FreeAndNil(fLock);
  FreeAndNil(fImages);
  inherited;
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
  fImages.Draw(
    Canvas,
    TopLeft.X,
    TopLeft.Y,
    ButtonImageIdx(Hot, Focussed)
  );
end;

procedure TDropDownButtons.SetImgListSize;
  {Sets size of bitmaps expected by image list per dimensions of buttons used.
  }
var
  Size: TSize;  // required size
begin
  Size := ButtonSize;
  fImages.Width := Size.cx;
  fImages.Height := Size.cy;
end;

procedure TDropDownButtons.ThemeChangeListener(Sender: TObject);
  {Handles theme services change event. Updates button images according to
  whether themes are in use or not.
    @param Sender [in] Not used.
  }
begin
  Update;
end;

procedure TDropDownButtons.Update;
  {Updates button images in image list. Buttons used depend on whether
  themes are active.
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
  // Wait for any lock to be opened
  fLock.WaitFor(cLockTimeout);
  // Close lock: this is used to prevent image list from being modified
  // asynchronously
  fLock.ResetEvent;
  try
    // Initialise image list
    fImages.Clear;
    SetImgListSize;
    // Load check boxes into image list
    // We load standard combo box buttons into a bitmap which is then added to
    // image list.
    BtnBmp := TBitmap.Create;
    try
      // Prepare bitmap
      BtnBmp.Width := fImages.Width;
      BtnBmp.Height := fImages.Height;
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
          fImages.AddMasked(BtnBmp, cMaskColour);
        end;
      end;
    finally
      FreeAndNil(BtnBmp);
    end;
  finally
    // Open lock: this permits modification of image list
    fLock.SetEvent;
  end;
  // Trigger event notifying that check boxes have changed
  if Assigned(fOnChange) then
    fOnChange(Self);
end;

end.


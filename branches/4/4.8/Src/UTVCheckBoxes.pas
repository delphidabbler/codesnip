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
 * Implements class that is used to manage image lists containing check box
 * images for display in tree views.
}


unit UTVCheckBoxes;


interface


uses
  // Delphi
  Controls, StdCtrls, Types,
  // Project
  UUIWidgetImages;


type

  {
  TTVCheckBoxes:
    Class used to manage image lists containing check box images for display in
    tree views and to map check box identifiers to indexes into image lists.
    Class loads image list with different images depending on whether themes are
    active. Update method can be called to change bitmaps in themes are changed.
  }
  TTVCheckBoxes = class(TUIWidgetImages)
  strict protected
    procedure RecreateImages; override;
      {Recreates image list containing appropriate check box images depending on
      whether themes are active.
      }
    function GetImageSize: TSize; override;
      {Gets size of check box image. Size may differ depending on whether themes
      are active or not.
        @return Size of check box in pixels.
      }
  public
    constructor Create(const ImgList: TImageList);
      {Object constructor. Sets up object and loads check box images into image
      list.
        @param ImgList [in] Image list to receive check box images.
      }
    class function CheckImageIdx(const CheckState: TCheckBoxState;
      const Hot: Boolean): Integer;
      {Gets image list index of a check box.
        @param CheckState [in] State of check box.
        @param Hot [in] Flag indicating whether check box is to be hot or
          normal.
        @return Required image index.
      }
    class function ImageIdxToCheckState(const ImgIdx: Integer): TCheckBoxState;
      {Finds state of checkbox referenced by an image list index.
        @param ImgIdx [in] Image index to be checked.
        @return Associated check box state.
      }
  end;


implementation


uses
  // Delphi
  Graphics, ImgList, Themes,
  // Project
  UStructs, UThemesEx;


const
  // Map of index of check boxes in image list to theme element representing
  // check box in normal and hot style
  cThemedCheckBoxes: array[TCheckBoxState, Boolean] of TThemedButton =
    (
      (tbCheckBoxUncheckedNormal, tbCheckBoxUncheckedHot),
      (tbCheckBoxCheckedNormal, tbCheckBoxCheckedHot),
      (tbCheckBoxMixedNormal, tbCheckBoxMixedHot)
    );

  // Name of resource storing checkbox bitmap used when themes not active
  cCheckBoxResName = 'TVCHECKBOXES';


{ TTVCheckBoxes }

class function TTVCheckBoxes.CheckImageIdx(const CheckState: TCheckBoxState;
  const Hot: Boolean): Integer;
  {Gets image list index of a check box.
    @param CheckState [in] State of check box.
    @param Hot [in] Flag indicating whether check box is to be hot or normal.
    @return Required image index.
  }
begin
  Result := Ord(CheckState);
  if Hot then
    Result := Result + Ord(High(TCheckBoxState)) - Ord(Low(TCheckBoxState)) + 1;
end;

constructor TTVCheckBoxes.Create(const ImgList: TImageList);
  {Object constructor. Sets up object and loads check box images into image
  list.
    @param ImgList [in] Image list to receive check box images.
  }
begin
  inherited Create(ImgList, False);
end;

function TTVCheckBoxes.GetImageSize: TSize;
  {Gets size of check box image. Size may differ depending on whether themes are
  active or not.
    @return Size of check box in pixels.
  }

  // ---------------------------------------------------------------------------
  function ThemedCheckBoxSize: TSize;
    {Calculates size of a themed check box used when themes are active.
      @return Size of check box.
    }
  begin
    // We get size from theme manager (assume all check boxes same size)
    Result := ThemeServicesEx.GetElementSize(tbCheckBoxUncheckedNormal);
  end;

  function NonThemedCheckBoxSize: TSize;
    {Calculates size of check box used when themes not active.
      @return Size of check box.
    }
  const
    // Dimension of checkbox bitmap resource, i.e. number of checkboxes across
    // and down bitmap
    cBmpResDim: TSize = (
      cx: 6;
      cy: 1
    );
  var
    ChkBmp: TBitmap; // Bitmap used to load checkbox resource
  begin
    // When themes not active we get check boxes from resources.
    // We get size of each bitmap by loading bitmap from resources and checking
    // size of resulting bitmap. We divide each dimension in pixels by number of
    // checkboxes in that dimension
    ChkBmp := TBitmap.Create;
    try
      ChkBmp.LoadFromResourceName(HInstance, cCheckBoxResName);
      Result.cx := ChkBmp.Width div cBmpResDim.cx;
      Result.cy := ChkBmp.Height div cBmpResDim.cy;
    finally
      ChkBmp.Free;
    end;
  end;
  // ---------------------------------------------------------------------------

begin
  inherited;
  if ThemeServicesEx.ThemesEnabled then
    Result := ThemedCheckBoxSize
  else
    Result := NonThemedCheckBoxSize;
end;

class function TTVCheckBoxes.ImageIdxToCheckState(
  const ImgIdx: Integer): TCheckBoxState;
  {Finds state of checkbox referenced by an image list index.
    @param ImgIdx [in] Image index to be checked.
    @return Associated check box state.
  }
var
  TVCheck: TCheckBoxState;  // loops thru all check box states
begin
  Result := cbUnchecked;  // keep compiler happy
  // Loop thru all check boxes looking for image index in hot or normal state
  for TVCheck := Low(TCheckBoxState) to High(TCheckBoxState) do
  begin
    if (CheckImageIdx(TVCheck, False) = ImgIdx) or
      (CheckImageIdx(TVCheck, True) = ImgIdx) then
    begin
      Result := TVCheck;
      Break;
    end;
  end;
end;

procedure TTVCheckBoxes.RecreateImages;
  {Recreates image list containing appropriate check box images depending on
  whether themes are active.
  }
const
  cMaskColour: TColor = clFuchsia;  // image list mask colour

  // ---------------------------------------------------------------------------
  procedure LoadThemedCheckBoxes;
    {Loads image list with check boxes used when themes are active.
    }
  var
    CheckBmp: TBitmap;        // bitmap on which check boxes are drawn
    CheckRect: TRect;         // bounds rectangle in bitmap to receive checkbox
    TVCheck: TCheckBoxState;  // loops thru all check box states
    Hot: Boolean;             // loops thru not-hot / hot
  begin
    // We use theme manager to draw check boxes on a bitmap which is then added
    // to image list.
    CheckBmp := TBitmap.Create;
    try
      // Prepare bitmap
      CheckBmp.Width := Images.Width;
      CheckBmp.Height := Images.Height;
      CheckBmp.Canvas.Brush.Color := cMaskColour;
      CheckRect := TRectEx.Create(0, 0, CheckBmp.Width, CheckBmp.Height);

      // Draw normal then hot bitmaps
      for Hot := Low(Boolean) to High(Boolean) do
      begin
        // Draw each check box state in normal and hot states
        for TVCheck := Low(TCheckBoxState) to High(TCheckBoxState) do
        begin
          // Draw each checkbox to bitmap and add to image list
          CheckBmp.Canvas.FillRect(CheckRect);
          ThemeServicesEx.DrawElement(
            cThemedCheckBoxes[TVCheck, Hot], CheckBmp, CheckRect
          );
          Images.AddMasked(CheckBmp, cMaskColour);
        end;
      end;
    finally
      CheckBmp.Free;
    end;
  end;

  procedure LoadNonThemedCheckBoxes;
    {Loads image list with check boxes used when themes not active.
    }
  begin
    // We load required checkbox bitmaps from resources
    Images.ResourceLoad(rtBitmap, cCheckBoxResName, cMaskColour);
  end;
  // ---------------------------------------------------------------------------

begin
  // Load check boxes into image list
  if ThemeServicesEx.ThemesEnabled then
    LoadThemedCheckBoxes
  else
    LoadNonThemedCheckBoxes;
end;

end.


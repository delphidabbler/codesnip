{
 * UTVCheckBoxes.pas
 *
 * Implements class that is used to manage image lists containing check box
 * images for display in tree views.
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
 * The Original Code is UTVCheckBoxes.pas
 *
 * The Initial Developer of the Original Code is Peter Johnson
 * (http://www.delphidabbler.com/).
 *
 * Portions created by the Initial Developer are Copyright (C) 2006-2009 Peter
 * Johnson. All Rights Reserved.
 *
 * Contributor(s)
 *   NONE
 *
 * ***** END LICENSE BLOCK *****
}


unit UTVCheckBoxes;


interface


uses
  // Delphi
  Controls, Classes, StdCtrls, SyncObjs, Windows;


type

  {
  TTVCheckBoxes:
    Class used to manage image lists containing check box images for display in
    tree views and to map check box identifiers to indexes into image lists.
    Class loads image list with different images depending on whether XP themes
    are active. Update method can be called to change bitmaps in themes are
    changed.
  }
  TTVCheckBoxes = class(TObject)
  strict private
    fLock: TSimpleEvent;
      {Simple signalling event object used to prevent Update method being called
      when themes change if it is already running}
    fImgList: TImageList;
      {Reference to image list used to store checkbox images}
    fOnChange: TNotifyEvent;
      {Handler for OnChange event}
    function CheckboxSize: TSize;
      {Gets size of check box. Size may differ depending on whether XP themes
      are active or not.
        @return Size of check box in pixels.
      }
    procedure SetImgListSize;
      {Sets size of bitmaps expected by image list. Size depends on dimensions
      of check boxes used.
      }
    procedure Update;
      {Updates check boxes in image list. Check boxes used depend on whether XP
      themes are active.
      }
    procedure ThemeChangeListener(Sender: TObject);
      {Handles theme services change event. Updates check boxes according to
      whether themes are in use or not.
        @param Sender [in] Not used.
      }
  public
    constructor Create(const ImgList: TImageList);
      {Class constructor. Sets up object and loads check box images into image
      list.
        @param ImgList [in] Image list to receive check box images.
      }
    destructor Destroy; override;
      {Class destructor. Tears down object.
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
    property OnChange: TNotifyEvent
      read fOnChange write fOnChange;
      {Event triggered when check boxes change, i.e. when themes change}
  end;


implementation


uses
  // Delphi
  SysUtils, Graphics, ImgList, Themes,
  // Project
  UStructs, UThemesEx;


const
  // Map of index of check boxes in image list to XP theme element representing
  // check box in normal and hot style
  cXPCheckBoxes: array[TCheckBoxState, Boolean] of TThemedButton =
    (
      (tbCheckBoxUncheckedNormal, tbCheckBoxUncheckedHot),
      (tbCheckBoxCheckedNormal, tbCheckBoxCheckedHot),
      (tbCheckBoxMixedNormal, tbCheckBoxMixedHot)
    );

  // Name of resource storing checkbox bitmap used when XP themes not active
  cCheckBoxResName = 'TVCHECKBOXES';

  // Timeout used when waiting for an event to signal
  cLockTimeout = 10000; // 10 secs


{ TTVCheckBoxes }

function TTVCheckBoxes.CheckboxSize: TSize;
  {Gets size of check box. Size may differ depending on whether XP themes are
  active or not.
    @return Size of check box in pixels.
  }

  // ---------------------------------------------------------------------------
  function ThemedCheckBoxSize: TSize;
    {Calculates size of a XP themed check box used when themes active.
      @return Size of check box.
    }
  begin
    // We get size from theme manager (assume all check boxes same size)
    Result := ThemeServicesEx.GetElementSize(tbCheckBoxUncheckedNormal);
  end;

  function NonThemedCheckBoxSize: TSize;
    {Calculates size of check box used when XP themes not active.
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
      FreeAndNil(ChkBmp);
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
  {Class constructor. Sets up object and loads check box images into image list.
    @param ImgList [in] Image list to receive check box images.
  }
begin
  Assert(Assigned(ImgList), ClassName + '.Create: ImgList is nil');
  inherited Create;
  fImgList := ImgList;
  fLock := TSimpleEvent.Create; // lock for protected sections: needed by Update
  fLock.SetEvent;
  Update;   // loads check boxes into image list
  ThemeServicesEx.AddChangeEventHandler(ThemeChangeListener);
end;

destructor TTVCheckBoxes.Destroy;
  {Class destructor. Tears down object.
  }
begin
  ThemeServicesEx.RemoveChangeEventHandler(ThemeChangeListener);
  FreeAndNil(fLock);
  inherited;
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

procedure TTVCheckBoxes.SetImgListSize;
  {Sets size of bitmaps expected by image list. Size depends on dimensions of
  check boxes used.
  }
var
  Size: TSize;  // required size
begin
  Size := CheckboxSize;
  fImgList.Width := Size.cx;
  fImgList.Height := Size.cy;
end;

procedure TTVCheckBoxes.ThemeChangeListener(Sender: TObject);
  {Handles theme services change event. Updates check boxes according to whether
  themes are in use or not.
    @param Sender [in] Not used.
  }
begin
  Update;
end;

procedure TTVCheckBoxes.Update;
  {Updates check boxes in image list. Check boxes used depend on whether XP
  themes are active.
  }
const
  cMaskColour: TColor = clFuchsia;  // image list mask colour

  // ---------------------------------------------------------------------------
  procedure LoadThemedCheckBoxes;
    {Loads image list with check boxes used when XP themes are active.
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
      CheckBmp.Width := fImgList.Width;
      CheckBmp.Height := fImgList.Height;
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
            cXPCheckBoxes[TVCheck, Hot], CheckBmp, CheckRect
          );
          fImgList.AddMasked(CheckBmp, cMaskColour);
        end;
      end;

    finally
      FreeAndNil(CheckBmp);
    end;
  end;

  procedure LoadNonThemedCheckBoxes;
    {Loads image list with check boxes used when XP themes not active.
    }
  begin
    // We load required checkbox bitmaps from resources
    fImgList.ResourceLoad(rtBitmap, cCheckBoxResName, cMaskColour);
  end;
  // ---------------------------------------------------------------------------

begin
  // Wair for any lock to be opened
  fLock.WaitFor(cLockTimeout);
  // Close lock: this is used to prevent image list from being modified
  // asynchronously
  fLock.ResetEvent;
  try
    // Initialise image list
    fImgList.Clear;
    SetImgListSize;
    // Load check boxes into image list
    if ThemeServicesEx.ThemesEnabled then
      LoadThemedCheckBoxes
    else
      LoadNonThemedCheckBoxes;
  finally
    // Open lock: this permits modification of image list
    fLock.SetEvent;
  end;
  // Trigger event notifying that check boxes have changed
  if Assigned(fOnChange) then
    fOnChange(Self);
end;

end.


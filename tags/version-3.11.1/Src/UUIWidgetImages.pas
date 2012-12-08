{
 * UUIWidgetImages.pas
 *
 * Implements an abstract base class that provides a framework for classes that
 * manage image lists containing images that are displayed in GUI widgets.
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
 * The Original Code is UUIWidgetImages.pas
 *
 * The Initial Developer of the Original Code is Peter Johnson
 * (http://www.delphidabbler.com/).
 *
 * Portions created by the Initial Developer are Copyright (C) 2010 Peter
 * Johnson. All Rights Reserved.
 *
 * Contributor(s)
 *   NONE
 *
 * ***** END LICENSE BLOCK *****
}


unit UUIWidgetImages;


interface


uses
  // Delphi
  Types, Classes, Controls, SyncObjs;


type

  {
  TUIWidgetImages:
    Abstract base class that provides a framework for classes that manage image
    lists containing images that are displayed in GUI widgets.
  }
  TUIWidgetImages = class abstract(TObject)
  strict private
    var
      fImages: TImageList;      // Value of Images property
      fOwnsImages: Boolean;     // Whether object owns (and frees) fImages
      fLock: TSimpleEvent;      // Signalling event to control access to Update
      fOnChange: TNotifyEvent;  // OnChange event handler
    const
      cLockTimeout = 10000; {10 secs} // Event signalling timeout
    procedure ThemeChangeListener(Sender: TObject);
      {Handles theme services change event. Updates images according to whether
      themes are in use or not.
        @param Sender [in] Not used.
      }
    procedure Update;
      {Updates image list. Images used depend on whether themes are active.
      }
  strict protected
    procedure RecreateImages; virtual; abstract;
      {Descendants recreate the image list in this method. Images should take
      account of whether themes are active.
      }
    function GetImageSize: TSize; virtual; abstract;
      {Gets the size of a single UI widget image.
        @return Required image size.
      }
    property Images: TImageList read fImages;
      {References image list that contains the various UI widget images}
  public
    constructor Create(const AImages: TImageList; const AOwnsImages: Boolean);
      {Object constructor. Sets up object.
        @param AImages [in] Reference to image list that will contain the the UI
          widget images.
        @param AOwnsImages [in] True if this object owns, and frees, AImages and
          False if caller maintains lifetime of AImages.
      }
    destructor Destroy; override;
      {Object destructor. Tears down object and frees Images image list if
      required.
      }
    property OnChange: TNotifyEvent
      read fOnChange write fOnChange;
      {Event triggered when image list changes, i.e. when themes change}
  end;


implementation


uses
  // Project
  UThemesEx;


{ TUIWidgetImages }

constructor TUIWidgetImages.Create(const AImages: TImageList;
  const AOwnsImages: Boolean);
  {Object constructor. Sets up object.
    @param AImages [in] Reference to image list that will contain the the UI
      widget images.
    @param AOwnsImages [in] True if this object owns, and frees, AImages and
      False if caller maintains lifetime of AImages.
  }
begin
  Assert(Assigned(AImages), ClassName + '.Create: AImages is nil');
  inherited Create;
  fImages := AImages;
  fOwnsImages := AOwnsImages;
  fLock := TSimpleEvent.Create; // lock for protected sections: needed by Update
  fLock.SetEvent;
  Update;
  ThemeServicesEx.AddChangeEventHandler(ThemeChangeListener);
end;

destructor TUIWidgetImages.Destroy;
  {Object destructor. Tears down object and frees Images image list if required.
  }
begin
  ThemeServicesEx.RemoveChangeEventHandler(ThemeChangeListener);
  if fOwnsImages then
    fImages.Free;
  fLock.Free;
  inherited;
end;

procedure TUIWidgetImages.ThemeChangeListener(Sender: TObject);
  {Handles theme services change event. Updates images according to whether
  themes are in use or not.
    @param Sender [in] Not used.
  }
begin
  Update;
end;

procedure TUIWidgetImages.Update;
  {Updates image list. Images used depend on whether themes are active.
  }

  // ---------------------------------------------------------------------------
  procedure UpdateImageListSize;
    {Updates width and height of images stored in image list.
    }
  var
    Size: TSize;  // image size
  begin
    Size := GetImageSize;
    Images.Width := Size.cx;
    Images.Height := Size.cy;
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
    Images.Clear;
    UpdateImageListSize;
    RecreateImages;
  finally
    // Open lock: this permits modification of image list
    fLock.SetEvent;
  end;
  // Trigger event notifying that check boxes have changed
  if Assigned(fOnChange) then
    fOnChange(Self);
end;

end.

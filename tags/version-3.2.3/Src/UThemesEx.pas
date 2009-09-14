{
 * UThemesEx.pas
 *
 * Implements class that extends TThemeServices with new methods and adds
 * support for multiple event handlers for the OnThemeChange event.
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
 * The Original Code is UThemesEx.pas
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


unit UThemesEx;

{$WARN UNSAFE_CODE OFF}

interface


uses
  // Delphi
  Classes, Windows, Graphics, Themes, Messages,
  // Project
  UMultiCastEvents, UMessageWindow;


type

  {
  TThemeServicesEx:
    Extends TThemeServices with methods that get size of a theme element and
    that provide easy access to drawing an element.
    }
  TThemeServicesEx = class(TThemeServices)
  strict private
    fMessageWdw: TMessageWindow;
      {Hidden window used to intercept messages and inform of possible theme
      changes when WM_WININICHANGED received}
    fThemeChanges: TMultiCastEvents;
      {Object used to support multi-cast events}
    function InternalGetElementSize(
      const Details: TThemedElementDetails): TSize;
      {Gets size of a theme part in a specified state.
        @param Details [in] Specifies element, part and state.
        @return Size of element.
      }
    procedure InternalDrawElement(const Details: TThemedElementDetails;
      const Bmp: TBitmap; const Rect: TRect); overload;
      {Draw a theme part in specified state on an area of a bitmap.
        @param Details [in] Specifies element, part and state.
        @param Bmp [in] Bitmap on which part is drawn.
        @param Rect [in] Area of bitmap receiving part.
      }
  strict protected
    procedure DoOnThemeChange; override;
      {Triggers multiple OnThemeChange events if themes may have changed.
      }
  public
    constructor Create; override;
      {Class constructor. Sets up object.
      }
    destructor Destroy; override;
      {Class destructor. Tears down object.
      }
    function GetElementSize(const Elem: TThemedButton): TSize; overload;
      {Gets size of a themed element.
        @param Elem [in] Element we want size of.
        @return Size of element.
      }
    procedure DrawElement(const Elem: TThemedButton; const Bmp: TBitmap;
      const Rect: TRect); overload;
      {Draws a themed element on a bitmap.
        @param Elem [in] Element to be drawn.
        @param Bmp [in] Bitmap on which to draw element.
        @param Rect [in] Area of bitmap in which to draw element.
      }
    procedure DrawElement(const Elem: TThemedTab; const Bmp: TBitmap;
      const Rect: TRect); overload;
      {Draws a themed tab element on a bitmap.
        @param Elem [in] Element to be drawn.
        @param Bmp [in] Bitmap on which to draw element.
        @param Rect [in] Area of bitmap in which to draw element.
      }
    function GetTabBodyColour: TColor;
      {Returns a reasonable approximation of the colour of an XP themed tab
      sheets. Tab sheets are gradient filled, so a representative colour is
      returned that looks OK painted on to a tab sheet.
        @return Suggested colour.
      }
    procedure AddChangeEventHandler(const Evt: TNotifyEvent);
      {Adds an event handler to list of handlers that are triggered when theme
      changes.
        @param Evt [in] Event handler to be added to list.
      }
    procedure RemoveChangeEventHandler(const Evt: TNotifyEvent);
      {Removes an event handler from list of handlers that are triggered when
      theme changes. Does nothing if handler not in list.
        @param Evt [in] Event handler to be removed from list.
      }
    procedure HiddenWindowHandler(Sender: TObject; var Msg: TMessage;
      var Handled: Boolean);
      {Handles messages from the hidden window. Responds only to WM_WININICHANGE
      messages by notifying that a possible theme changed has occured.
        @param Sender [in] Not used.
        @param Msg [in/out] Message to be handled. Left unchanged.
        @param Handled [in/out] Set to true if message is WM_WININICHANGE.
      }
  end;


function ThemeServicesEx: TThemeServicesEx;
  {Casts ThemeServices object to its actual type.
    @return ThemeServices object cast to TThemeServicesEx.
  }


implementation


uses
  // Delphi
  SysUtils, UxTheme, ComObj,
  // Project
  UStructs;


{
  Implementation note re getting size of and drawing element types
  ----------------------------------------------------------------

  To extend TThemeServicesEx to get size of and to draw specified element types,
  for example TThemedMenu, create overloaded versions of DrawElement and
  GetElementSize with the Elem parameters of required type. Then implement
  DrawElement using
    InternalDrawElement(GetElementDetails(Elem), Bmp, Rect);
  and GetElementSize using
    Result := InternalGetElementSize(GetElementDetails(Elem));
}


function ThemeServicesEx: TThemeServicesEx;
  {Casts ThemeServices object to its actual type.
    @return ThemeServices object cast to TThemeServicesEx.
  }
begin
  Result := Themes.ThemeServices as TThemeServicesEx;
end;

{ TThemeServicesEx }

procedure TThemeServicesEx.AddChangeEventHandler(const Evt: TNotifyEvent);
  {Adds an event handler to list of handlers that are triggered when theme
  changes.
    @param Evt [in] Event handler to be added to list.
  }
begin
  if not Assigned(fThemeChanges) then
    fThemeChanges := TMultiCastEvents.Create;
  fThemeChanges.AddHandler(Evt);
end;

constructor TThemeServicesEx.Create;
  {Class constructor. Sets up object.
  }
begin
  inherited;
  fMessageWdw := TMessageWindow.Create;
  fMessageWdw.OnMessage := HiddenWindowHandler;
end;

destructor TThemeServicesEx.Destroy;
  {Class destructor. Tears down object.
  }
begin
  FreeAndNil(fThemeChanges);
  FreeAndNil(fMessageWdw);
  inherited;
end;

procedure TThemeServicesEx.DoOnThemeChange;
  {Triggers multiple OnThemeChange events if themes may have changed.
  }
begin
  inherited;  // triggers OnThemeChange event
  if Assigned(fThemeChanges) then
    fThemeChanges.TriggerEvents;
end;

procedure TThemeServicesEx.DrawElement(const Elem: TThemedTab;
  const Bmp: TBitmap; const Rect: TRect);
  {Draws a themed tab element on a bitmap.
    @param Elem [in] Element to be drawn.
    @param Bmp [in] Bitmap on which to draw element.
    @param Rect [in] Area of bitmap in which to draw element.
  }
begin
  Assert(ThemesEnabled,                                    // ** do not localise
    'TThemeServicesEx.DrawElement: Themes not enabled');
  InternalDrawElement(GetElementDetails(Elem), Bmp, Rect);
end;

procedure TThemeServicesEx.DrawElement(const Elem: TThemedButton;
  const Bmp: TBitmap; const Rect: TRect);
  {Draws a themed element on a bitmap.
    @param Elem [in] Element to be drawn.
    @param Bmp [in] Bitmap on which to draw element.
    @param Rect [in] Area of bitmap in which to draw element.
  }
begin
  Assert(ThemesEnabled,                                    // ** do not localise
    'TThemeServicesEx.DrawElement: Themes not enabled');
  InternalDrawElement(GetElementDetails(Elem), Bmp, Rect);
end;

function TThemeServicesEx.GetElementSize(const Elem: TThemedButton): TSize;
  {Gets size of a themed element.
    @param Elem [in] Element we want size of.
    @return Size of element.
  }
begin
  Assert(ThemesEnabled,                                    // ** do not localise
    'TThemeServicesEx.GetElementSize: Themes not enabled');
  Result := InternalGetElementSize(GetElementDetails(Elem));
end;

function TThemeServicesEx.GetTabBodyColour: TColor;
  {Returns a reasonable approximation of the colour of an XP themed tab sheets.
  Tab sheets are gradient filled, so a representative colour is returned that
  looks OK painted on to a tab sheet.
    @return Suggested colour.
  }
var
  Bmp: TBitmap; // bitmap on which a tab sheet is drawn
begin
  Bmp := TBitmap.Create;
  try
    Bmp.Width := 16;
    Bmp.Height := 16;
    // draw the tab pane
    DrawElement(ttPane, Bmp, TRectEx.Create(0, 0, Bmp.Width, Bmp.Height));
    // take a representative colour from drawn pane
    Result := Bmp.Canvas.Pixels[2, 3];
  finally
    FreeAndNil(Bmp);
  end;
end;

procedure TThemeServicesEx.HiddenWindowHandler(Sender: TObject;
  var Msg: TMessage; var Handled: Boolean);
  {Handles messages from the hidden window. Responds only to WM_WININICHANGE
  messages by notifying that a possible theme changed has occured.
    @param Sender [in] Not used.
    @param Msg [in/out] Message to be handled. Left unchanged.
    @param Handled [in/out] Set to true if message is WM_WININICHANGE.
  }
begin
  case Msg.Msg of
    WM_WININICHANGE:
    begin
      ApplyThemeChange;
      Handled := True;
    end;
  end;
end;

procedure TThemeServicesEx.InternalDrawElement(
  const Details: TThemedElementDetails; const Bmp: TBitmap;
  const Rect: TRect);
  {Draw a theme part in specified state on an area of a bitmap.
    @param Details [in] Specifies element, part and state.
    @param Bmp [in] Bitmap on which part is drawn.
    @param Rect [in] Area of bitmap receiving part.
  }
begin
  DrawElement(Bmp.Canvas.Handle, Details, Rect);
end;

function TThemeServicesEx.InternalGetElementSize(
  const Details: TThemedElementDetails): TSize;
  {Gets size of a theme part in a specified state.
    @param Details [in] Specifies element, part and state.
    @return Size of element.
  }
var
  DC: HDC;  // device context required by GetThemePartSize
begin
  // Use desktop device context
  DC := GetDC(0);
  try
    // Get size of element that would be drawn
    OleCheck(
      UxTheme.GetThemePartSize(
        Theme[Details.Element], // handle to window's theme data
        DC,                     // device context to select fonts into
        Details.Part,           // specifies part to calculate size of
        Details.State,          // specifies state of part
        nil,                    // rectangle of part drawing dest (may be nil)
        TS_DRAW,                // type of size to retrieve (drawing size)
        Result                  // dimensions of specified part
      )
    );
  finally
    ReleaseDC(0, DC);
  end;
end;

procedure TThemeServicesEx.RemoveChangeEventHandler(
  const Evt: TNotifyEvent);
  {Removes an event handler from list of handlers that are triggered when theme
  changes. Does nothing if handler not in list.
    @param Evt [in] Event handler to be removed from list.
  }
begin
  if Assigned(fThemeChanges) then
  begin
    fThemeChanges.RemoveHandler(Evt);
    // we free list if it is empty
    if fThemeChanges.Count = 0 then
      FreeAndNil(fThemeChanges);
  end;
end;

initialization

// Ensure Themes unit creates ThemeServices singleton as type TThemeServicesEx
// rather than TThemeServices.
ThemeServicesClass := TThemeServicesEx;

end.


{
 * UWBUIMgr.pas
 *
 * Contains class that implements IDocHostUIHandler interface and allows
 * customisation of IE web browser control's user interface, message
 * translation, pop-up menu, external object and drag-drop handling.
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
 * The Original Code is UWBUIMgr.pas
 *
 * The Initial Developer of the Original Code is Peter Johnson
 * (http://www.delphidabbler.com/).
 *
 * Portions created by the Initial Developer are Copyright (C) 2005-2010 Peter
 * Johnson. All Rights Reserved.
 *
 * Contributor(s)
 *   NONE
 *
 * ***** END LICENSE BLOCK *****
}


unit UWBUIMgr;


interface


uses
  // Delphi
  Classes, Windows, ActiveX, SHDocVw, Menus,
  // Project
  IntfUIHandlers, UNulUIHandler;


type

  {
  TWBTranslateEvent:
    Type of event triggered when a key is pressed in the web browser control.
    The program decides whether to handle (translate) the keypress itself or
    allow the web browser to handle it.
      @param Sender [in] Object triggering event.
      @param Msg [in[ Windows message record identifying the message that
        triggered the event. This may be zeroed if there was no such message.
      @param CmdID [in] Browser command that normally occurs in response to the
        event. This value may be zero if there is no related command.
      @param Handled [in/out] False when the event handler is called. Set to
        True if the handler program handles (translates) the keypress and leave
        False to let the web browser handle it. Keypresses can be supressed by
        setting Handled to true and doing nothing with the key press.
  }
  TWBTranslateEvent = procedure(Sender: TObject;
    const Msg: TMSG; const CmdID: DWORD; var Handled: Boolean) of object;

  {
  TWBMenuPopupEvent:
    Event called when browser wishes to display a popup menu. A popup menu
    should only be displayed in this event handler if Handled is true and the
    web browser's PopupMenu property is nil, otherwise two menus will be
    displayed.
      @param Sender [in] Object triggering event.
      @param PopupPos [in] Position to display the menu.
      @param MenuID [in] Type of menu to be displayed - this is one of the
        CONTEXT_MENU_* values.
      @param Handled [in/out] When called will be true if the program is to
        display the popup menu and false if the browser is intending to display
        the menu. Change Handled's value to change this behaviour and to
        override the manager object's UseDefaultContextMenu property.
  }
  TWBMenuPopupEvent = procedure(Sender: TObject; PopupPos: TPoint;
    const MenuID: DWORD; var Handled: Boolean) of object;

  {
  TWBMenuPopupEventEx:
    Extended version of event called when browser wishes to display a popup
    menu. A popup menu should only be displayed in this event handler if Handled
    is true and the web browser's PopupMenu property is nil, otherwise two menus
    will be displayed.
      @param Sender [in] Object triggering event.
      @param PopupPos [in] Position to display the menu.
      @param MenuID [in] Type of menu to be displayed - this is one of the
        CONTEXT_MENU_* values.
      @param Handled [in/out] When called will be true if the program is to
        display the popup menu and false if the browser is intending to display
        the menu. Change Handled's value to change this behaviour and to
        override the manager object's UseDefaultContextMenu property.
      @param Obj [in] IDispatch interface to the selected object in the current
        document when the menu was summoned. Cast this to IHTMLElement to get
        information about the selected tag.
  }
  TWBMenuPopupEventEx = procedure(Sender: TObject; PopupPos: TPoint;
    const MenuID: DWORD; var Handled: Boolean; const Obj: IDispatch)
    of object;

  {
  TWBUpdateCSSEvent:
    Event called when browser control needs to get default CSS. Handler can
    modify or set default CSS.
      @param Sender [in] Reference to browser UI manager triggering event.
      @param CSS [in/out] CSS code. Current value of CSS property passed in.
        Handler can modify or replace this CSS code.
  }
  TWBUpdateCSSEvent = procedure(Sender: TObject; var CSS: string) of object;

  {
  TWBScrollbarStyle:
    Styles for browser's scroll bars as determined by the manager class's
    ScrollbarStyle property.
  }
  TWBScrollbarStyle = (
    sbsNormal,    // display scroll bars in normal (default) style
    sbsFlat,      // display flat scroll bars (in classic Windows UI)
    sbsHide       // hide the scroll bars
  );

  {
  TWBUIMgr:
    Class that manages a web browser object and modifies its behaviour according
    to this class's properties and events. The class can be constructed either
    as a stand alone class or as an aggregated object of a containing class.
    The class supports the IDocHostUIHandler interface whose methods are called
    by the browser when it needs to customise or update its UI. It also provides
    public methods for modifying or interogating the browser control and the
    currently displayed document.
  }
  TWBUIMgr = class(TNulUIHandler,
    IInterface,
    IDocHostUIHandler
  )
  strict private
    fWebBrowser: TWebBrowser;
      {Reference to managed web browser control}
    fShow3dBorder: Boolean;
      {Determines whether browsers displays 3D border}
    fAllowTextSelection: Boolean;
      {Determines whether text can be selected in browser}
    fCSS: string;
      {Records master cascading style sheet used by browser}
    fUseDefaultContextMenu: Boolean;
      {Records whether browser's default context menus are displayed or whether
      program handles context menus}
    fExternScript: IDispatch;
      {Reference to object that implement's browser "external" object}
    fDropTarget: IDropTarget;
      {Reference to drag drop handler for browser object}
    fScrollbarStyle: TWBScrollbarStyle;
      {Determines style of scroll bars}
    fUseThemes: Boolean;
      {Determines whether browser uses themes}
    fOnTranslateAccel: TWBTranslateEvent;
      {Handler for OnTranslateAccel event}
    fOnMenuPopup: TWBMenuPopupEvent;
      {Handler for OnMenuPopup event}
    fOnMenuPopupEx: TWBMenuPopupEventEx;
      {Handler for OnMenuPopupEx event}
    fOnUpdateCSS: TWBUpdateCSSEvent;
      {Handler for OnUpdateCSS event}
    fOnBrowserDeactivate: TNotifyEvent;
      {Handler for OnBrowserDeactivate event}
    fOnBrowserActivate: TNotifyEvent;
      {Handler for OnBrowserActivate event}
    fOldOnEnter: TNotifyEvent;
      {Original event handler for browser's OnEnter event. Used to pass any such
      event on to original handler before we handle it}
    function GetSelectedText: string;
      {Read accessor for SelectedText property. Gets selected text from
      browser's current document.
        @return Selected text or '' if none.
      }
    procedure BrowserEnter(Sender: TObject);
      {Handler for browser's OnEnter event. Ensures that browser receives focus.
        @param Sender [in] Reference to web browser object that triggered the
          event. Passed to any previous event handler, otherwise not used.
      }
    function GetPopupMenu: TPopupMenu;
      {Read accessor for PopupMenu property.
        @return Reference to web browser's popup menu or nil if none assigned.
      }
    procedure SetPopupMenu(const Value: TPopupMenu);
      {Write accessor for PopupMenu property.
        @param Value [in] Reference to required popup menu or nil if no popup
          menu required.
      }
  protected // do not make strict
    { IDocHostUIHandler overrides }
    function ShowContextMenu(const dwID: DWORD; const ppt: PPOINT;
      const pcmdtReserved: IUnknown; const pdispReserved: IDispatch): HResult;
      stdcall;
      {Called by browser when about to display a context menu. We change
      behaviour as required by relevant property values.
        @param dwID [in] Specifies identifier of the shortcut menu to be
          displayed.
        @param ppt [in] Pointer screen coordinates for the menu.
        @param pcmdtReserved [in] Not used.
        @param pdispReserved [in] IDispatch interface of HTML object under
          mouse.
        @return S_OK to prevent IE displaying its menu or S_FALSE to enable the
          IE menu.
      }
    function GetHostInfo(var pInfo: TDocHostUIInfo): HResult; stdcall;
      {Called by browser to get UI capabilities. We configure the required UI
      appearance per relevant property values.
        @param pInfo [in/out] Reference to structure that we fill in to
          configure appearance of browser.
        @return S_OK to show we handled OK.
      }
    function ShowUI(const dwID: DWORD;
      const pActiveObject: IOleInPlaceActiveObject;
      const pCommandTarget: IOleCommandTarget; const pFrame: IOleInPlaceFrame;
      const pDoc: IOleInPlaceUIWindow): HResult; stdcall;
      {Called by browser when a document activates to enable the host to update
      its UI. We trigger an OnBrowserActivate event.
        @param dwID [in] Not used other than to pass to inherited method.
        @param pActiveObject [in] Not used other than to pass to inherited
          method.
        @param pCommandTarget [in] Not used other than to pass to inherited
          method.
        @pFrame [in] Not used other than to pass to inherited method.
        @param pDoc [in] Not used other than to pass to inherited method.
        @return S_OK to show we displayed our own UI.
      }
    function HideUI: HResult; stdcall;
      {Called by browser when a document deactivates to enable the host to
      update its UI. We trigger an OnBrowserDeactivate event.
        @return S_OK to indicate we handled successfully.
      }
    function TranslateAccelerator(const lpMsg: PMSG; const pguidCmdGroup: PGUID;
      const nCmdID: DWORD): HResult; stdcall;
      {Called by browser when a key press is received. We trigger
      OnTranslateAccel event to filter out key presses not to be handled by
      browser.
        @param lpMsg [in] Pointer to structure that specifies message to be
          translated.
        @param pguidCmdGroup [in] Not used.
        @param nCmdID [in] Command identifier.
        @return S_OK to prevent IE handling message or S_FALSE to allow it.
      }
    function GetExternal(out ppDispatch: IDispatch): HResult; stdcall;
      {Notifies browser control of reference to any object assigned to
      ExternScript property that extends browser's "external" object.
        @param ppDispatch [out] Set to IDispatch interface for "external" COM
          object.
        @return S_OK if successful, or an error value otherwise.
      }
    function GetDropTarget(const pDropTarget: IDropTarget;
      out ppDropTarget: IDropTarget): HResult; stdcall;
      {Notifies browser control of any drag drop handler object assigned to
      DropTarget property.
        @param pDropTarget [in] Not used other than to pass to inherited method.
        @param ppDropTarget [out] Set to object assigned to DropTarget property.
        @return E_FAIL if DropTarget is nil or S_OK if DropTarget is assigned.
      }
  public
    constructor Create(const WebBrowser: TWebBrowser;
      const Controller: IInterface = nil);
      {Class constructor. Sets up the object (optionally as an aggregated
      object).
        @param WebBrowser [in] Reference to managed browser control. Must not be
          nil.
        @param Controller [in] IInterface of containing object if this object is
          to be aggregated or nil if to be stand alone.
      }
    function CanCopy: Boolean;
      {Checks if text can be copied from browser control to clipboard.
        @return True if copying permitted, False otherwise.
      }
    procedure CopyToClipboard;
      {Copies selected text from browser control to clipboard if operation
      permitted.
      }
    function CanSelectAll: Boolean;
      {Checks if all browser control's text can be selected.
        @return True if text can be selected, False otherwise.
      }
    procedure SelectAll;
      {Selects all text in browser control if operation is permitted.
      }
    procedure ClearSelection;
      {Clears any selected text in browser control.
      }
    procedure ScrollTo(const X, Y: Integer);
      {Scrolls document displayed in browser control to given co-ordinates.
        @param X [in] X-coordinate
        @param Y [in] Y-coordinate.
      }
    function DocHeight: Integer;
      {Gets height of currently displayed document.
        @return Required height in pixels.
      }
    property UseDefaultContextMenu: Boolean
      read fUseDefaultContextMenu write fUseDefaultContextMenu default True;
      {Determines if IE displays its default context menu (true) or if program
      handles this (false). The value of this property can be selectively
      overridden if the OnMenuPopup or OnMenuPopupEx events are handled}
    property ScrollbarStyle: TWBScrollbarStyle
      read fScrollbarStyle write fScrollbarStyle default sbsNormal;
      {Determines style of browser control's scroll bars, or hides them. See
      documentation for TWBScrollbarStyle for details}
    property Show3dBorder: Boolean
      read fShow3dBorder write fShow3dBorder default True;
      {Flag that indicates whether browser control displays 3D borders}
    property AllowTextSelection: Boolean
      read fAllowTextSelection write fAllowTextSelection default True;
      {Flag that indicates whether user can select text in browser control}
    property UseThemes: Boolean
      read fUseThemes write fUseThemes;
      {Flag that indicates whether browser control should use UIthemes when it
      is displaying widgets. This property is ignored if not running on Windows
      XP or later or when running in classic style or when running on an IE
      version earlier earlier than v6. The property defaults to true if themes
      are enabled and false if they are not available or not enabled}
    property CSS: string
      read fCSS write fCSS;
      {The default CSS to apply to documents displayed in browser control}
    property SelectedText: string
      read GetSelectedText;
      {Provides the current text selection or '' if no text is selected in
      browser control}
    property ExternScript: IDispatch
      read fExternScript write fExternScript;
      {Object that extends the browser control's "external" object. This
      object's methods can be called by scripts running in the browser's
      current document}
    property DropTarget: IDropTarget
      read fDropTarget write fDropTarget;
      {Object that handles drag/drop on behalf of the browser control. If this
      property is nil browser's default drag drop handling is used}
    property PopupMenu: TPopupMenu
      read GetPopupMenu write SetPopupMenu;
      {Reference to any popup menu assigned to browser control or nil if there
      is no such menu assigned}
    property OnTranslateAccel: TWBTranslateEvent
      read fOnTranslateAccel write fOnTranslateAccel;
      {Event triggered when browser control receives a key press. This enables
      the program to determine whether the key press is handled by the browser
      or by the program. See the documentation of TWBTranslateEvent for more
      details}
    property OnMenuPopup: TWBMenuPopupEvent
      read fOnMenuPopup write fOnMenuPopup;
      {Event triggered when the browser is ready to display a popup menu. Allows
      the program to customise the menu or to display its own. See the
      documentation of TWBMenuPopupEvent for more information}
    property OnMenuPopupEx: TWBMenuPopupEventEx
      read fOnMenuPopupEx write fOnMenuPopupEx;
      {Extended version of OnMenuPopup event triggered when the browser is ready
      to display a popup menu. In addition to information made available in
      OnMenuPopup the IDispatch interface of the object under the cursor when
      the menu was summoned is made available. This event is only triggered if
      OnMenuPopup has no event handler}
    property OnUpdateCSS: TWBUpdateCSSEvent
      read fOnUpdateCSS write fOnUpdateCSS;
      {Event triggered when browser needs default CSS. Provides opportunity to
      modify or replace code per CSS property}
    property OnBrowserActivate: TNotifyEvent
      read fOnBrowserActivate write fOnBrowserActivate;
      {Event triggered when browser control is activated, either by user
      clicking in it or tabbing into it}
    property OnBrowserDeactivate: TNotifyEvent
      read fOnBrowserDeactivate write fOnBrowserDeactivate;
      {Event triggered when browser control is deactivated, either by user
      clicking or tabbing away from it or control being hidden}
  end;


implementation


uses
  // Delphi
  SysUtils,
  // Project
  UHTMLDocHelper, UThemesEx, UWBHelper;


function TaskAllocWideString(const S: string): PWChar;
  {Allocates memory for a wide string using the Shell's task allocator and
  copies a given string into the memory as a wide string. Caller is responsible
  for freeing the buffer and must use the shell's allocator to do this.
    @param S [in] String to convert.
    @return Pointer to buffer containing wide string.
  }
var
  StrLen: Integer;  // length of string in bytes
begin
  // Store length of string, allowing for terminal #0
  StrLen := Length(S) + 1;
  // Allocate buffer for wide string using task allocator
  Result := CoTaskMemAlloc(StrLen * SizeOf(WideChar));
  if not Assigned(Result) then
    raise EOutOfMemory.Create(    // ** do not localise
      'TaskAllocWideString: can''t allocate buffer.'
    );
  // Convert string to wide string and store in buffer
  StringToWideChar(S, Result, StrLen);
end;


{ TWBUIMgr }

procedure TWBUIMgr.BrowserEnter(Sender: TObject);
  {Handler for browser's OnEnter event. Ensures that browser receives focus.
    @param Sender [in] Reference to web browser object that triggered the event.
      Passed to any previous event handler, otherwise not used.
  }
begin
  // Focus the <body> tag if it exists
  THTMLDocHelper.FocusElem(THTMLDocHelper.GetBodyElem(fWebBrowser.Document));
  // Pass event on to any original handler
  if Assigned(fOldOnEnter) then
    fOldOnEnter(Sender);
end;

function TWBUIMgr.CanCopy: Boolean;
  {Checks if text can be copied from browser control to clipboard.
    @return True if copying permitted, False otherwise.
  }
begin
  // Check that browser control supports copying and that some text is selected
  Result := TWBHelper.IsCommandEnabled(fWebBrowser, OLECMDID_COPY)
    and (SelectedText <> '');
end;

function TWBUIMgr.CanSelectAll: Boolean;
  {Checks if all browser control's text can be selected.
    @return True if text can be selected, False otherwise.
  }
begin
  // Check if browser supports text selection and text selection allowed
  Result := TWBHelper.IsCommandEnabled(fWebBrowser, OLECMDID_SELECTALL)
    and AllowTextSelection;
end;

procedure TWBUIMgr.ClearSelection;
  {Clears any selected text in browser control.
  }
begin
  // Check if browser supports clearing selection and clear it if so
  if TWBHelper.IsCommandEnabled(fWebBrowser, OLECMDID_CLEARSELECTION) then
    TWBHelper.ExecCommand(fWebBrowser, OLECMDID_CLEARSELECTION);
end;

procedure TWBUIMgr.CopyToClipboard;
  {Copies selected text from browser control to clipboard if operation
  permitted.
  }
begin
  if CanCopy then
    // Get browser control to copy its content to clipboard
    TWBHelper.ExecCommand(fWebBrowser, OLECMDID_COPY);
end;

constructor TWBUIMgr.Create(const WebBrowser: TWebBrowser;
  const Controller: IInterface = nil);
  {Class constructor. Sets up the object (optionally as an aggregated object).
    @param WebBrowser [in] Reference to managed browser control. Must not be
      nil.
    @param Controller [in] IInterface of containing object if this object is to
      be aggregated or nil if to be stand alone.
  }
begin
  Assert(Assigned(WebBrowser), ClassName + '.Create: WebBrowser is nil');
  inherited Create(Controller);
  fWebBrowser := WebBrowser;
  fUseDefaultContextMenu := True;
  fScrollbarStyle := sbsNormal;
  fShow3dBorder := True;
  fAllowTextSelection := True;
  fUseThemes := ThemeServicesEx.ThemesEnabled;
  // Handler browser ctrl's OnEnter event to focus browser control
  fOldOnEnter := fWebBrowser.OnEnter;
  fWebBrowser.OnEnter := BrowserEnter;
end;

function TWBUIMgr.DocHeight: Integer;
  {Gets height of currently displayed document.
    @return Required height in pixels.
  }
begin
  // Calculate height of HTML in browser control: this is height of <body> tag
  Result := THTMLDocHelper.GetScrollHeight(
    THTMLDocHelper.GetBodyElem(fWebBrowser.Document)
  );
end;

function TWBUIMgr.GetDropTarget(const pDropTarget: IDropTarget;
  out ppDropTarget: IDropTarget): HResult;
  {Notifies browser control of any drag drop handler object assigned to
  DropTarget property.
    @param pDropTarget [in] Not used other than to pass to inherited method.
    @param ppDropTarget [out] Set to object assigned to DropTarget property.
    @return E_FAIL if DropTarget is nil or S_OK if DropTarget is assigned.
  }
begin
  if Assigned(fDropTarget) then
  begin
    // We are handling drag-drop: notify browser of drop target object to use
    ppDropTarget := fDropTarget;
    Result := S_OK;
  end
  else
    // We are not handling drag-drop: use inherited default behaviour
    Result := inherited GetDropTarget(pDropTarget, ppDropTarget);
end;

function TWBUIMgr.GetExternal(out ppDispatch: IDispatch): HResult;
  {Notifies browser control of reference to any object assigned to ExternScript
  property that extends browser's "external" object.
    @param ppDispatch [out] Set to IDispatch interface for "external" COM
      object.
    @return S_OK if successful, or an error value otherwise.
  }
begin
  if Assigned(fExternScript) then
  begin
    // We have an external script object: notify browser
    ppDispatch := fExternScript;
    Result := S_OK; // indicates we've provided script
  end
  else
    // We have no script object: use inherited default behviour
    Result := inherited GetExternal(ppDispatch);
end;

function TWBUIMgr.GetHostInfo(var pInfo: TDocHostUIInfo): HResult;
  {Called by browser to get UI capabilities. We configure the required UI
  appearance per relevant property values.
    @param pInfo [in/out] Reference to structure that we fill in to configure
      appearance of browser.
    @return S_OK to show we handled OK.
  }
var
  CSS: string;  // CSS to pass to browser
begin
  // Update flags depending on property values
  pInfo.dwFlags := 0;
  if fUseThemes and ThemeServicesEx.ThemesEnabled then
    pInfo.dwFlags := pInfo.dwFlags or DOCHOSTUIFLAG_THEME
  else if ThemeServicesEx.ThemesAvailable then
    pInfo.dwFlags := pInfo.dwFlags or DOCHOSTUIFLAG_NOTHEME;
  // scroll bar style
  case fScrollbarStyle of
    sbsHide:
      // hide the scroll bars
      pInfo.dwFlags := pInfo.dwFlags or DOCHOSTUIFLAG_SCROLL_NO;
    sbsFlat:
      // use flat scroll bars (has effect in classic UI only)
      pInfo.dwFlags := pInfo.dwFlags or DOCHOSTUIFLAG_FLAT_SCROLLBAR;
    sbsNormal:
      // use standard scroll bars: this is default
      {Do nothing};
  end;
  // 3d border
  if not fShow3dBorder then
    pInfo.dwFlags := pInfo.dwFlags or DOCHOSTUIFLAG_NO3DBORDER;
  // text selection
  if not fAllowTextSelection then
    pInfo.dwFlags := pInfo.dwFlags or DOCHOSTUIFLAG_DIALOG;

  // Record default style sheet if provided
  CSS := fCSS;
  if Assigned(fOnUpdateCSS) then
    fOnUpdateCSS(Self, CSS);
  if CSS <> '' then
    pInfo.pchHostCss := TaskAllocWideString(CSS);

  Result := S_OK;
end;

function TWBUIMgr.GetPopupMenu: TPopupMenu;
  {Read accessor for PopupMenu property.
    @return Reference to web browser's popup menu or nil if none assigned.
  }
begin
  Result := fWebBrowser.PopupMenu;
end;

function TWBUIMgr.GetSelectedText: string;
  {Read accessor for SelectedText property. Gets selected text from browser's
  current document.
    @return Selected text or '' if none.
  }
begin
  Result := THTMLDocHelper.GetTextSelection(fWebBrowser.Document);
end;

function TWBUIMgr.HideUI: HResult;
  {Called by browser when a document deactivates to enable the host to update
  its UI. We trigger an OnBrowserDeactivate event.
    @return S_OK to indicate we handled successfully.
  }
begin
  Result := inherited HideUI;
  if Assigned(fOnBrowserDeactivate) then
    fOnBrowserDeactivate(Self);
end;

procedure TWBUIMgr.ScrollTo(const X, Y: Integer);
  {Scrolls document displayed in browser control to given co-ordinates.
    @param X [in] X-coordinate
    @param Y [in] Y-coordinate.
  }
begin
  THTMLDocHelper.ScrollTo(fWebBrowser.Document, X, Y);
end;

procedure TWBUIMgr.SelectAll;
  {Selects all text in browser control if operation is permitted.
  }
begin
  if CanSelectAll then
    // Get web browser to select all its text
    TWBHelper.ExecCommand(fWebBrowser, OLECMDID_SELECTALL);
end;

procedure TWBUIMgr.SetPopupMenu(const Value: TPopupMenu);
  {Write accessor for PopupMenu property.
    @param Value [in] Reference to required popup menu or nil if no popup menu
      required.
  }
begin
  fWebBrowser.PopupMenu := Value;
end;

function TWBUIMgr.ShowContextMenu(const dwID: DWORD; const ppt: PPOINT;
  const pcmdtReserved: IInterface;
  const pdispReserved: IDispatch): HResult;
  {Called by browser when about to display a context menu. We change behaviour
  as required by relevant property values.
    @param dwID [in] Specifies identifier of the shortcut menu to be displayed.
    @param ppt [in] Pointer screen coordinates for the menu.
    @param pcmdtReserved [in] Not used.
    @param pdispReserved [in] IDispatch interface of HTML object under mouse.
    @return S_OK to prevent IE displaying its menu or S_FALSE to enable the IE
      menu.
  }
var
  Handled: Boolean; // whether program handled pop-up menu
begin
  // Set default value of Handled: program handles if not using default IE menu
  Handled := not fUseDefaultContextMenu;

  // Try to trigger OnMenuPopup or OnMenuPopupEx events. Only one of these
  // events is triggered: OnMenuPopupEx is only triggered if assigned and
  // OnMenuPopup is not assigned. Handler can change value of Handled hence
  // overriding UseDefaultContextMenu property. This permits handler to display
  // some menu types while leaving IE to handle others, depending on system
  // state or menu id. Note that handler should only display popup menu here if
  // Handled is set true and web browser control has no popup menu assigned.
  // The additional information provided by OnMenuPopupEx is the IDispatch
  // interface of the clicked object
  if Assigned(fOnMenuPopup) then
    fOnMenuPopup(Self, Point(ppt.X, ppt.Y), dwID, Handled)
  else if Assigned(fOnMenuPopupEx) then
    fOnMenuPopupEx(Self, Point(ppt.X, ppt.Y), dwID, Handled, pDispReserved);

  // Determine whether event handler or IE display menu popup
  if not Handled then
    // Tell IE to use default popup menu action
    Result := S_FALSE
  else
  begin
    // Tell IE we're handling the context menu so that it will not display menu.
    // If web browser control has popup menu assigned it is displayed, otherwise
    // no menu shown
    Result := S_OK;
    if Assigned(fWebBrowser.PopupMenu) then
      fWebBrowser.PopupMenu.Popup(ppt.X, ppt.Y);
  end;
end;

function TWBUIMgr.ShowUI(const dwID: DWORD;
  const pActiveObject: IOleInPlaceActiveObject;
  const pCommandTarget: IOleCommandTarget; const pFrame: IOleInPlaceFrame;
  const pDoc: IOleInPlaceUIWindow): HResult;
  {Called by browser when a document activates to enable the host to update its
  UI. We trigger an OnBrowserActivate event.
    @param dwID [in] Not used other than to pass to inherited method.
    @param pActiveObject [in] Not used other than to pass to inherited method.
    @param pCommandTarget [in] Not used other than to pass to inherited method.
    @pFrame [in] Not used other than to pass to inherited method.
    @param pDoc [in] Not used other than to pass to inherited method.
    @return S_OK to show we displayed our own UI.
  }
begin
  Result := inherited ShowUI(dwID, pActiveObject, pCommandTarget, pFrame, pDoc);
  if Assigned(fOnBrowserActivate) then
    fOnBrowserActivate(Self);
end;

function TWBUIMgr.TranslateAccelerator(const lpMsg: PMSG;
  const pguidCmdGroup: PGUID; const nCmdID: DWORD): HResult;
  {Called by browser when a key press is received. We trigger OnTranslateAccel
  event to filter out key presses not to be handled by browser.
    @param lpMsg [in] Pointer to structure that specifies message to be
      translated.
    @param pguidCmdGroup [in] Not used.
    @param nCmdID [in] Command identifier.
    @return S_OK to prevent IE handling message or S_FALSE to allow it.
  }
var
  Handled: Boolean; // flag set true by event handler if it handles key
  Msg: TMsg;        // Windows message record for given message
begin
  // Assume not handled by event handler
  Handled := False;
  // Call event handler if set
  if Assigned(fOnTranslateAccel) then
  begin
    // create copy of message: set all fields zero if lpMsg is nil
    if Assigned(lpMsg) then
      Msg := lpMsg^
    else
      FillChar(Msg, SizeOf(Msg), 0);
    // trigger event handler
    fOnTranslateAccel(Self, Msg, nCmdID, Handled);
  end;
  // If event handler handled accelerator then return S_OK to stop web browser
  // handling it otherwise return S_FALSE so browser will handle it
  if Handled then
    Result := S_OK
  else
    Result := S_FALSE;
end;

end.


{
 * FrBrowserBase.pas
 *
 * Base class for all frames that contain a web browser control.
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
 * The Original Code is FrBrowserBase.pas
 *
 * The Initial Developer of the Original Code is Peter Johnson
 * (http://www.delphidabbler.com/).
 *
 * Portions created by the Initial Developer are Copyright (C) 2005-2012 Peter
 * Johnson. All Rights Reserved.
 *
 * Contributor(s)
 *   NONE
 *
 * ***** END LICENSE BLOCK *****
}


unit FrBrowserBase;


interface


uses
  // Delphi
  OleCtrls, SHDocVw, Classes, Controls, ExtCtrls, Forms, Windows,
  // Project
  Browser.UController, UCSSBuilder, UHTMLEvents, UMenuHelper;


type

  {
  TBrowserBuildCSSEvent:
    Type of event triggered when frame needs CSS. This can be handled by forms
    that own frame to modify default CSS.
      @param Sender [in] Reference to frame triggering event.
      @param CSSBuilder [in] Object containing information about existing CSS.
        Existing CSS can be modified and added to.
  }
  TBrowserBuildCSSEvent = procedure(Sender: TObject;
    const CSSBuilder: TCSSBuilder) of object;

  {
  TBrowserBaseFrame:
    Base class for all frames that contain a web browser control. Creates a
    web browser controller object and sets default characteristics of controlled
    browser object. Also provides methods to handle activation of browser, HTML
    events, access selected links, trigger pop-up menus and provide information
    about the control.
  }
  TBrowserBaseFrame = class(TFrame, IPopupMenu)
    pnlBrowser: TPanel;
    wbBrowser: TWebBrowser;
    procedure FrameEnter(Sender: TObject);
  strict private
    fWBController: TWBController;       // Object used to control web browser
    fOnBuildCSS: TBrowserBuildCSSEvent; // Handler for OnBuildCSS event
    fOnHTMLEvent: THTMLEvent;           // Handler for OnHTMLEvent event
    procedure UpdateCSS(Sender: TObject; var CSS: string);
      {Handles browser UI manager's OnUpdateCSS event. Sets browser control's
      CSS.
        @param Sender [in] Not used.
        @param CSS [in/out] Browser's default CSS. Any CSS passed in is ignored
          and CSS set to required CSS.
      }
    procedure MakeBrowserActiveControl;
      {Makes browser the active control of the parent form.
      }
    procedure TriggerActiveLink;
      {Triggers any selected link in web browser control.
      }
  strict protected
    function ParentForm: TForm;
      {Gets reference to form that hosts the frame.
        @return Reference to host form or nil if no such host.
      }
    procedure BuildCSS(const CSSBuilder: TCSSBuilder); virtual;
      {Builds required default CSS. Can be overridden to add or modify the CSS.
        @param CSSBuilder [in] Object used to construct the CSS.
      }
    procedure BrowserActivate(Sender: TObject); virtual;
      {Handles web browser control's OnEnter event. Makes browser the active
      control of the host form.
        @param Sender [in] Not used.
      }
    procedure TranslateAccelHandler(Sender: TObject; const Msg: TMSG;
      const CmdID: DWORD; var Handled: Boolean);
      {Handles event triggered by web browser controller when a key is pressed
      in the browser control. Browser is prevented from handling any key press
      where we don't want browser's default action or we want an opportunity to
      handle the key press in owning form or main application.
        @param Sender [in] Not used.
        @param Msg [in] Message generating accelerator translation request.
        @param CmdID [in] Not used.
        @param Handled [in/out] False when passed in. Set to true as required to
          prevent web browser handling key press.
      }
    function DoNavigate(const URL: string): Boolean;
      {Navigates to a URL. Action peformed depends on URL's protocol.
        @param URL [in] URL being navigated to.
        @return True if navigation was handled by method, false if not.
      }
    procedure NavigateHandler(Sender: TObject; const URL: string;
      var Cancel: Boolean);
      {Handles web browser navigation events.
        @param Sender [in] Not used.
        @param URL [in] URL to access.
        @param Cancel [in/out] False when passed in. Set to true to cancel
          browser's own navigation if navigation is handled by the application.
          Left false if browser is to handle navigation.
      }
    procedure HTMLEventHandler(Sender: TObject;
      const EventInfo: THTMLEventInfo); virtual;
      {Handles "standard" web browser events. Checks for shift key being pressed
      when link is clicked and inhibits default browser action of opening in IE.
      Instead URL is opened according to protocol handler. All events are passed
      on via this object's OnHTMLEvent event.
        @param Sender [in] Object that triggered event. Passed on to this
          object's OnHTMLEvent handler.
        @param EventInfo [in] Object providing information about the event.
      }
    property WBController: TWBController
      read fWBController;
      {Object used to control web browser}
  protected // do not make strict
    function IsBrowserActive: Boolean;
      {Checks if browser control is currently active. Implements
      IWBInfo.IsBrowserActive for sub-classes.
        @return True if browser control is active, False otherwise.
      }
    function CanCopy: Boolean;
      {Checks whether text can be copied to clipboard from browser control.
      Implements IClipboardMgr.CanCopy for sub-classes.
        @return True if text can be copied.
      }
    procedure CopyToClipboard;
      {Copies selected text from browser control to clipboard. Implements
      IClipboardMgr.CopyToClipboard for sub-classes.
      }
    function CanSelectAll: Boolean;
      {Checks whether text can be selected in browser control. Implements
      ISelectionMgr.CanSelectAll for sub-classes.
        @return True if text can be selected.
      }
    procedure SelectAll;
      {Selects all text in browser control. Implements ISelectionMgr.SelectAll
      for sub-classes.
      }
  public
    constructor Create(AOwner: TComponent); override;
      {Class constructor. Sets up browser frame.
        @param AOwner [in] Component that owns frame.
      }
    destructor Destroy; override;
      {Class destructor. Tears down object.
      }
    function HasPopup: Boolean;
      {Checks if browser control has a pop-up menu. Method of IPopupMenu.
      }
    procedure Popup(const Pt: TPoint);
      {Displays browser control's menu for active element at given point. Method
      of IPopupMenu.
      }
    property OnBuildCSS: TBrowserBuildCSSEvent
      read fOnBuildCSS write fOnBuildCSS;
      {Event triggerd when default CSS is required by browser control}
    property OnHTMLEvent: THTMLEvent
      read fOnHTMLEvent write fOnHTMLEvent;
      {Event used to pass on HTML events triggered by browser control}
  end;


implementation


uses
  // Delphi
  SysUtils, Messages,
  // Project
  UAnchors, UColours, UCSSUtils, UHTMLDocHelper, UProtocols;


{$R *.dfm}


{ TBrowserBaseFrame }

procedure TBrowserBaseFrame.BrowserActivate(Sender: TObject);
  {Handles web browser control's OnEnter event. Makes browser the active control
  of the host form.
    @param Sender [in] Not used.
  }
begin
  MakeBrowserActiveControl;
end;

procedure TBrowserBaseFrame.BuildCSS(const CSSBuilder: TCSSBuilder);
  {Builds required default CSS. Can be overridden to add or modify the CSS.
    @param CSSBuilder [in] Object used to construct the CSS.
  }
begin
  // Ensures images have no borders
  with CSSBuilder.AddSelector('img') do
    AddProperty(CSSBorderProp(cssAll, 0));
  // Sets default link style
  with CSSBuilder.AddSelector('a:link, a:visited, a:active') do
  begin
    AddProperty(CSSColorProp(clLinkText));
    AddProperty(CSSTextDecorationProp([ctdUnderline]));
  end;
  // Sets style for help links
  with CSSBuilder.AddSelector(
    'a:link.help-link, a:visited.help-link, a:active.help-link'
  ) do
  begin
    AddProperty(CSSColorProp(clHelpLinkText));
    AddProperty(CSSTextDecorationProp([ctdUnderline]));
  end;
  // Sets warning text class
  with CSSBuilder.AddSelector('.warning') do
  begin
    AddProperty(CSSColorProp(clWarningText));
    AddProperty(CSSFontWeightProp(cfwBold));
  end;
end;

function TBrowserBaseFrame.CanCopy: Boolean;
  {Checks whether text can be copied to clipboard from browser control.
  Implements IClipboardMgr.CanCopy for sub-classes.
    @return True if text can be copied.
  }
begin
  Result := WBController.UIMgr.CanCopy;
end;

function TBrowserBaseFrame.CanSelectAll: Boolean;
  {Checks whether text can be selected in browser control. Implements
  ISelectionMgr.CanSelectAll for sub-classes.
    @return True if text can be selected.
  }
begin
  Result := WBController.UIMgr.CanSelectAll;
end;

procedure TBrowserBaseFrame.CopyToClipboard;
  {Copies selected text from browser control to clipboard. Implements
  IClipboardMgr.CopyToClipboard for sub-classes.
  }
begin
  WBController.UIMgr.CopyToClipboard;
end;

constructor TBrowserBaseFrame.Create(AOwner: TComponent);
  {Class constructor. Sets up browser frame.
    @param AOwner [in] Component that owns frame.
  }
begin
  inherited;
  Assert(AOwner is TWinControl, ClassName + '.Create: AOwner not a TForm');
  // Create web browser controller object and set defaults and event handlers
  fWBController := TWBController.Create(wbBrowser);
  fWBController.UIMgr.Show3dBorder := False;
  fWBController.UIMgr.UseDefaultContextMenu := False;
  fWBController.UIMgr.OnTranslateAccel := TranslateAccelHandler;
  fWBController.UIMgr.OnUpdateCSS := UpdateCSS;
  fWBController.UIMgr.OnBrowserActivate := BrowserActivate;
  fWBController.IOMgr.OnNavigate := NavigateHandler;
  fWBController.IOMgr.OnHTMLEvent := HTMLEventHandler;
end;

destructor TBrowserBaseFrame.Destroy;
  {Class destructor. Tears down object.
  }
begin
  FreeAndNil(fWBController);
  inherited;
end;

function TBrowserBaseFrame.DoNavigate(const URL: string): Boolean;
  {Navigates to a URL. Action peformed depends on URL's protocol.
    @param URL [in] URL being navigated to.
    @return True if navigation was handled by method, false if not.
  }
var
  Protocol: TProtocol;  // object used to handle certain URL protocols
begin
  // If protocol handler recognises a URL's protocol it deals with it in its
  // execute method and returns true. This return value is also returned by this
  // method. If the protocol handler does not recognise a protocol its Execute
  // method does nothing and returns false and this method returns false.
  Protocol := TProtocolFactory.CreateHandler(URL);
  try
    Result := Protocol.Execute;
  finally
    FreeAndNil(Protocol);
  end;
end;

procedure TBrowserBaseFrame.FrameEnter(Sender: TObject);
  {Handles event triggered when frame is entered. Activates browser.
    @param Sender [in] Not used.
  }
begin
  MakeBrowserActiveControl;
end;

function TBrowserBaseFrame.HasPopup: Boolean;
  {Checks if browser control has a pop-up menu. Method of IPopupMenu.
  }
begin
  Result := WBController.UIMgr.SupportsPopupMenu;
end;

procedure TBrowserBaseFrame.HTMLEventHandler(Sender: TObject;
  const EventInfo: THTMLEventInfo);
  {Handles "standard" web browser events. Checks for shift key being pressed
  when link is clicked and inhibits default browser action of opening in IE.
  Instead URL is opened according to protocol handler. All events are passed on
  via this object's OnHTMLEvent event.
    @param Sender [in] Object that triggered event. Passed on to this object's
      OnHTMLEvent handler.
    @param EventInfo [in] Object providing information about the event.
  }
var
  ALink: IDispatch; // any <a> tag generating (or wrapping tag generating) event
begin
  try
    // Trigger OnHTMLEvent: uses original sender
    if Assigned(fOnHTMLEvent) then
      fOnHTMLEvent(Sender, EventInfo);
    // We only handle onclick events if they've not already been handled and
    // cancelled
    if (EventInfo.DispatchId <> cDocEventOnClick) or (EventInfo.Cancelled) then
      Exit;
    // We only handle clicks on <a> tags or tags within <a> tags (eg <img>)
    ALink := TAnchors.FindEnclosingAnchor(EventInfo.Args.srcElement);
    if not Assigned(ALink) then
      Exit;
    if EventInfo.Args.shiftKey then
    begin
      // User pressed shift key:
      // prevent default browser action by cancelling the event
      EventInfo.Cancelled := True;
      // navigate according to protocol handler
      DoNavigate(TAnchors.GetURL(ALink));
    end;
  except
    Application.HandleException(ExceptObject);
  end;
end;

function TBrowserBaseFrame.IsBrowserActive: Boolean;
  {Checks if browser control is currently active. Implements
  IWBInfo.IsBrowserActive for sub-classes.
    @return True if browser control is active, False otherwise.
  }
begin
  Result := ParentForm.ActiveControl = wbBrowser;
end;

procedure TBrowserBaseFrame.MakeBrowserActiveControl;
  {Makes browser the active control of the parent form.
  }
begin
  if ParentForm <> nil then
    ParentForm.ActiveControl := wbBrowser;
end;

procedure TBrowserBaseFrame.NavigateHandler(Sender: TObject;
  const URL: string; var Cancel: Boolean);
  {Handles web browser navigation events.
    @param Sender [in] Not used.
    @param URL [in] URL to access.
    @param Cancel [in/out] False when passed in. Set to true to cancel browser's
      own navigation if navigation is handled by the application. Left false if
      browser is to handle navigation.
  }
begin
  try
    Cancel := DoNavigate(URL);
  except
    Application.HandleException(ExceptObject);
    Cancel := True;
  end;
end;

function TBrowserBaseFrame.ParentForm: TForm;
  {Gets reference to form that hosts the frame.
    @return Reference to host form or nil if no such host.
  }
var
  ParentCtrl: TWinControl;  // moves up tree of parent controls
begin
  ParentCtrl := Self.Parent;
  while Assigned(ParentCtrl) and not (ParentCtrl is TForm) do
    ParentCtrl := ParentCtrl.Parent;
  if ParentCtrl is TForm then
    Result := ParentCtrl as TForm
  else
    Result := nil;
end;

procedure TBrowserBaseFrame.Popup(const Pt: TPoint);
  {Displays browser control's menu for active element at given point. Method
  of IPopupMenu.
  }
begin
  WBController.UIMgr.ShowPopupMenu(Pt);
end;

procedure TBrowserBaseFrame.SelectAll;
  {Selects all text in browser control. Implements ISelectionMgr.SelectAll for
  sub-classes.
  }
begin
  WBController.UIMgr.SelectAll;
end;

procedure TBrowserBaseFrame.TranslateAccelHandler(Sender: TObject;
  const Msg: TMSG; const CmdID: DWORD; var Handled: Boolean);
  {Handles event triggered by web browser controller when a key is pressed in
  the browser control. Browser is prevented from handling any key press where we
  don't want browser's default action or we want an opportunity to handle the
  key press in owning form or main application.
    @param Sender [in] Not used.
    @param Msg [in] Message generating accelerator translation request.
    @param CmdID [in] Not used.
    @param Handled [in/out] False when passed in. Set to true as required to
      prevent web browser handling key press.
  }

  // ---------------------------------------------------------------------------
  procedure PostKeyPress(const DownMsg, UpMsg: UINT);
    {Posts a key down and key up message to frame's parent window using wParam
    and lParam values of message passed to outer method.
      @param DownMsg [in] Key down message (WM_KEYDOWN or WM_SYSKEYDOWN).
      @param UpMsg [in] key up message (WM_KEYUP or WM_SYSKEYUP).
    }
  begin
    PostMessage(Parent.Handle, DownMsg, Msg.wParam, Msg.lParam);
    PostMessage(Parent.Handle, UpMsg, Msg.wParam, Msg.lParam);
  end;
  // ---------------------------------------------------------------------------

var
  ShiftState: TShiftState;  // state of key modifiers
  PostMsg: Boolean;         // whether to post message to parent
begin
  PostMsg := False;
  // We only handle key down messages
  if ((Msg.message = WM_KEYDOWN) or (Msg.message = WM_SYSKEYDOWN)) then
  begin
    // Record state of modifier keys
    ShiftState := Forms.KeyDataToShiftState(Msg.lParam);
    // Process key pressed (wParam field of Msg)
    case Msg.wParam of
      Ord('A')..Ord('Z'), Ord('0')..Ord('9'):
        // We pass Ctrl key with any alphanumeric keys to parent. This enables
        // any of application's shortcut keys to be handled by associated action
        if (ShiftState = [ssCtrl])
          or (ShiftState = [ssCtrl, ssShift])
          or (ShiftState = [ssCtrl, ssShift, ssAlt]) then
          PostMsg := True;
      VK_F1..VK_F24:
        // We post all function keys with any modifier to parent to allow
        // processing there. It is particularly important for pass on F5 to
        // prevent browser from refreshing and F1 to enable help.
        // NOTE: we assume all function keys have contiguous codes.
        PostMsg := True;
      VK_LEFT, VK_RIGHT, VK_HOME:
        // We post Alt+Left, Alt+Right and Alt+Home keys to parent to enable
        // application to call any associated actions
        if ShiftState = [ssAlt] then
          PostMsg := True;
      VK_INSERT, VK_DELETE:
        // We post Ctrl+Ins and Ctrl+Del to parent to enable application to
        // call any associated actions
        if ShiftState = [ssCtrl] then
          PostMsg := True;
      VK_TAB:
        // We post Ctrl+Tab and Shift+Ctrl+Tab to parent to enable tab switching
        // where browser is hosted by a tab set of some description
        if (ShiftState = [ssCtrl]) or (ShiftState = [ssCtrl, ssShift]) then
          PostMsg := True;
      VK_RETURN:
        // We handle Ctrl+Return to trigger any active link. This message is not
        // posted to parent, but is handled here and browser is prevented from
        // processing the keypress further
        if ShiftState = [ssCtrl] then
        begin
          TriggerActiveLink;
          Handled := True;
        end;
        VK_ESCAPE:
          // We pass on all ESC key presses
          PostMsg := True;
    end;
    if PostMsg then
    begin
      // Any message for which posting is required is posted to window that owns
      // frame, and is therefore passed to main application.
      case Msg.message of
        WM_KEYDOWN:     PostKeyPress(WM_KEYDOWN, WM_KEYUP);
        WM_SYSKEYDOWN:  PostKeyPress(WM_SYSKEYDOWN, WM_SYSKEYUP);
      end;
      // We consider all posted messages as handled to prevent browser
      // processing them further
      Handled := True;
    end;
  end;
end;

procedure TBrowserBaseFrame.TriggerActiveLink;
  {Triggers any selected link in web browser control. Does nothing if there is
  no active link.
  }
begin
  TAnchors.Click(THTMLDocHelper.GetActiveElem(wbBrowser.Document));
end;

procedure TBrowserBaseFrame.UpdateCSS(Sender: TObject; var CSS: string);
  {Handles browser UI manager's OnUpdateCSS event. Sets browser control's CSS.
    @param Sender [in] Not used.
    @param CSS [in/out] Browser's default CSS. Any CSS passed in is ignored and
      CSS set to required CSS.
  }
var
  CSSBuilder: TCSSBuilder;  // object used to build CSS
begin
  CSSBuilder := TCSSBuilder.Create;
  try
    // Build CSS
    BuildCSS(CSSBuilder);
    // Modify / add CSS using OnBuildCSS event if set
    if Assigned(fOnBuildCSS) then
      fOnBuildCSS(Self, CSSBuilder);
    // Render CSS
    CSS := CSSBuilder.AsString;
  finally
    FreeAndNil(CSSBuilder);
  end;
end;

end.


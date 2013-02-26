{
 * This Source Code Form is subject to the terms of the Mozilla Public License,
 * v. 2.0. If a copy of the MPL was not distributed with this file, You can
 * obtain one at http://mozilla.org/MPL/2.0/
 *
 * Copyright (C) 2005-2013, Peter Johnson (www.delphidabbler.com).
 *
 * $Rev$
 * $Date$
 *
 * Implements a base class for all frames that contain a web browser control.
}


unit FrBrowserBase;


interface


uses
  // Delphi
  OleCtrls, SHDocVw, Classes, Controls, ExtCtrls, Forms, Windows,
  // Project
  Browser.UController, Browser.UHTMLEvents, UCSSBuilder, UMenus;


type
  ///  <summary>Type of event triggered when frame that hosts a TWebBrowser
  ///  needs CSS.</summary>
  ///  <remarks>Such events can be handled by forms that own the frame to modify
  ///  default CSS.</remarks>
  ///  <param name="Sender">TObject [in] Reference to frame triggering event.
  ///  </param>
  ///  <param name="CSSBuilder">TCSSBuilder [in] Object containing information
  ///  about existing CSS. This can be modified and/or added to.</param>
  TBrowserBuildCSSEvent = procedure(Sender: TObject;
    const CSSBuilder: TCSSBuilder) of object;

type
  ///  <summary>
  ///  Base class for all frames that contain a web browser control. Creates a
  ///  web browser controller object and sets default characteristics of
  ///  controlled browser object. Also provides methods to handle activation of
  ///  browser, HTML events, access selected links, trigger pop-up menus and
  ///  provide information about the browser control.
  ///  </summary>
  TBrowserBaseFrame = class(TFrame, IPopupMenu)
    pnlBrowser: TPanel;
    wbBrowser: TWebBrowser;
    ///  <summary>Handles event triggered when frame is entered. Activates
    ///  browser.</summary>
    procedure FrameEnter(Sender: TObject);
  strict private
    var
      ///  <summary>Object used to control web browser.</summary>
      fWBController: TWBController;
      ///  <summary>Handler for OnBuildCSS event.</summary>
      fOnBuildCSS: TBrowserBuildCSSEvent;
      ///  <summary>Handler for OnHTMLEvent event.</summary>
      fOnHTMLEvent: THTMLEvent;
    ///  <summary>Handles browser UI manager's OnUpdateCSS event. Passes CSS
    ///  to be used by browser control via CSS parameter.</summary>
    procedure UpdateCSS(Sender: TObject; var CSS: string);
    ///  <summary>Makes browser the active control of the parent form.</summary>
    procedure MakeBrowserActiveControl;
    ///  <summary>Triggers any selected link in web browser control.</summary>
    procedure TriggerActiveLink;
  strict protected
    ///  <summary>Returns reference to form that hosts the frame or nil if there
    ///  is no host form.</summary>
    function ParentForm: TForm;
    ///  <summary>Builds required default CSS using given TCSSBuilder object.
    ///  </summary>
    ///  <remarks>Can be overridden to modify the CSS.</remarks>
    procedure BuildCSS(const CSSBuilder: TCSSBuilder); virtual;
    ///  <summary>Handles web browser control's OnEnter event. Makes browser
    ///  control the active control of the host form.</summary>
    procedure BrowserActivate(Sender: TObject); virtual;
    ///  <summary>Handles event triggered by web browser controller when a key
    ///  is pressed in the browser control.</summary>
    ///  <remarks>Browser is prevented from handling any key press where we
    ///  don't want browser's default action or we want an opportunity to handle
    ///  the key press in owning form or main application.</remarks>
    ///  <param name="Sender">TObject [in] Not used.</param>
    ///  <param name="Msg">TMSG [in] Message generating accelerator translation
    ///  request.</param>
    ///  <param name="CmdID">DWORD [in] Not used.</param>
    ///  <param name="Handled">Boolean [in/out] False when passed in. Set to
    ///  True as required to prevent web browser handling key press.</param>
    procedure TranslateAccelHandler(Sender: TObject; const Msg: TMSG;
      const CmdID: DWORD; var Handled: Boolean);
    ///  <summary>Navigates to a URL. Action peformed depends on URL's protocol.
    ///  </summary>
    ///  <param name="URL">string [in] URL to be navigated to.</param>
    ///  <returns>Boolean. True if navigation was handled by method, False if
    ///  not.</returns>
    function DoNavigate(const URL: string): Boolean;
    ///  <summary>Handler for web browser controller's OnNavigate event.
    ///  Navigates to given URL.</summary>
    ///  <param name="Sender">TObject [in] Not used.</param>
    ///  <param name="URL">string [in] URL to be navigated to.</param>
    ///  <param name="Cancel">Boolean [in/out] False when passed in. Set to True
    ///  to cancel browser's own navigation if navigation is handled by the
    ///  application. Left False if browser is to handle navigation.</param>
    procedure NavigateHandler(Sender: TObject; const URL: string;
      var Cancel: Boolean);
    ///  <summary>Handles "standard" web browser events.</summary>
    ///  <remarks>
    ///  <para>Checks for shift key when link is clicked and inhibits default
    ///  browser action of opening in IE. Instead URL is opened according to
    ///  protocol handler.</para>
    ///  <para>All events are passed to this object's OnHTMLEvent
    ///  event.</para>
    ///  </remarks>
    ///  <param name="Sender">TObject [in] Object that triggered event.</param>
    ///  <param name="EventInfo">THTMLEventInfo [in] Provides information about
    ///  the event.</param>
    procedure HTMLEventHandler(Sender: TObject;
      const EventInfo: THTMLEventInfo); virtual;
    ///  <summary>Handles browser window's onerror event. Generates and handles
    ///  an EBug exception to report the error.</summary>
    ///  <remarks>
    ///  <para>Parameters are used in exception error message.</para>
    ///  <para>Handles parameter is set True to inhibit browser control's script
    ///  error dialog box.</para>
    ///  </remarks>
    procedure HTMLWindowErrorHandler(Sender: TObject; const Desc, URL: string;
      const Line: Integer; var Handled: Boolean); virtual;
    ///  <summary>Web browser controller object.</summary>
    property WBController: TWBController read fWBController;
  public
    ///  <summary>Creates frame with given owner and creates and initialises
    ///  browser controller.</summary>
    constructor Create(AOwner: TComponent); override;
    ///  <summary>Tears down frame.</summary>
    destructor Destroy; override;
    ///  <summary>Checks if browser control has a pop-up menu.</summary>
    ///  <remarks>Method of IPopupMenu.</remarks>
    function HasPopup: Boolean;
    ///  <summary>Displays browser control's menu for active element at given
    ///  point.</summary>
    ///  <remarks>Method of IPopupMenu.</remarks>
    procedure Popup(const Pt: TPoint);
    ///  <summary>Checks if browser control is currently active.</summary>
    ///  <remarks>Method of IWBInfo.</remarks>
    function IsBrowserActive: Boolean;
    ///  <summary>Checks whether text can be copied to clipboard from browser
    ///  control.</summary>
    ///  <remarks>Method of IClipboardMgr</remarks>
    function CanCopy: Boolean;
    ///  <summary>Copies selected text from browser control to clipboard.
    ///  </summary>
    ///  <remarks>Method of IClipboardMgr.</remarks>
    procedure CopyToClipboard;
    ///  <summary>Checks whether text can be selected in browser control.
    ///  </summary>
    ///  <remarks>Method of ISelectionMgr.</remarks>
    function CanSelectAll: Boolean;
    ///  <summary>Selects all text in browser control.</summary>
    ///  <remarks>Method of ISelectionMgr.</remarks>
    procedure SelectAll;
    ///  <summary>Event triggerd when default CSS is required by browser
    ///  control.</summary>
    property OnBuildCSS: TBrowserBuildCSSEvent
      read fOnBuildCSS write fOnBuildCSS;
    ///  <summary>Event triggered when supported HTML events are intercepted.
    ///  </summary>
    property OnHTMLEvent: THTMLEvent
      read fOnHTMLEvent write fOnHTMLEvent;
  end;


implementation


uses
  // Delphi
  SysUtils, Messages, Graphics,
  // Project
  UAnchors, UColours, UCSSUtils, UExceptions, UFontHelper, UProtocols, UUtils;


{$R *.dfm}


{ TBrowserBaseFrame }

procedure TBrowserBaseFrame.BrowserActivate(Sender: TObject);
begin
  MakeBrowserActiveControl;
end;

procedure TBrowserBaseFrame.BuildCSS(const CSSBuilder: TCSSBuilder);
var
  CSSFont: TFont;
begin
  // <img> tag style: no borders
  with CSSBuilder.AddSelector('img') do
    AddProperty(TCSS.HideBorderProp(cssAll));

  // Default <a> tag style: fall back links for unknown link classes.
  // Each link type is expected to define own colour as a minimum.
  with CSSBuilder.AddSelector('a') do
  begin
    AddProperty(TCSS.ColorProp(clDefaultLink));
    AddProperty(TCSS.TextDecorationProp([ctdUnderline]));
  end;
  // <a class="help-link"> override
  with CSSBuilder.AddSelector('a.help-link') do
  begin
    AddProperty(TCSS.ColorProp(clHelpLink));
  end;
  // <a class="snippet-link"> and <a class="category-link"> overrides
  with CSSBuilder.AddSelector('a.snippet-link, a.category-link') do
  begin
    AddProperty(TCSS.ColorProp(clDBLink));
    AddProperty(TCSS.FontStyleProp(cfsItalic));
    AddProperty(TCSS.TextDecorationProp([ctdNone]));
  end;
  with CSSBuilder.AddSelector('a:hover.snippet-link, a:hover.category-link') do
  begin
    AddProperty(TCSS.BorderProp(cssBottom, 1, cbsDotted, clDBLink));
  end;
  // <a class="command-link"> override
  with CSSBuilder.AddSelector('a.command-link') do
  begin
    AddProperty(TCSS.ColorProp(clCommandLink));
    AddProperty(TCSS.FontStyleProp(cfsItalic));
    AddProperty(TCSS.TextDecorationProp([ctdNone]));
  end;
  with CSSBuilder.AddSelector('a:hover.command-link') do
  begin
    AddProperty(TCSS.BorderProp(cssBottom, 1, cbsDotted, clCommandLink));
  end;
  with CSSBuilder.AddSelector('.no-link-decoration a:hover') do
    AddProperty(TCSS.HideBorderProp(cssBottom));
  // <a class="external-link"> override
  with CSSBuilder.AddSelector('a.external-link') do
  begin
    AddProperty(TCSS.ColorProp(clExternalLink));
  end;

  // <var> tag style
  with CSSBuilder.AddSelector('var') do
  begin
    AddProperty(TCSS.ColorProp(clVarText));
    AddProperty(TCSS.FontStyleProp(cfsItalic));
  end;

  // <code> tag style
  with CSSBuilder.AddSelector('code') do
  begin
    CSSFont := TFont.Create;
    try
      TFontHelper.SetDefaultMonoFont(CSSFont);
      AddProperty(TCSS.FontProps(CSSFont));
    finally
      CSSFont.Free;
    end;
  end;

  // .warning class style: mainly for use inline
  with CSSBuilder.AddSelector('.warning') do
  begin
    AddProperty(TCSS.ColorProp(clWarningText));
    AddProperty(TCSS.FontWeightProp(cfwBold));
  end;
end;

function TBrowserBaseFrame.CanCopy: Boolean;
begin
  Result := WBController.UIMgr.CanCopy;
end;

function TBrowserBaseFrame.CanSelectAll: Boolean;
begin
  Result := WBController.UIMgr.CanSelectAll;
end;

procedure TBrowserBaseFrame.CopyToClipboard;
begin
  WBController.UIMgr.CopyToClipboard;
end;

constructor TBrowserBaseFrame.Create(AOwner: TComponent);
begin
  inherited;
  Assert(AOwner is TWinControl, ClassName + '.Create: AOwner not a TForm');
  fWBController := TWBController.Create(wbBrowser);
  fWBController.UIMgr.Show3dBorder := False;
  fWBController.UIMgr.OnTranslateAccel := TranslateAccelHandler;
  fWBController.UIMgr.OnUpdateCSS := UpdateCSS;
  fWBController.UIMgr.OnBrowserActivate := BrowserActivate;
  fWBController.IOMgr.OnNavigate := NavigateHandler;
  fWBController.IOMgr.OnHTMLEvent := HTMLEventHandler;
  fWBController.IOMgr.OnHTMLWindowError := HTMLWindowErrorHandler;
end;

destructor TBrowserBaseFrame.Destroy;
begin
  FreeAndNil(fWBController);
  inherited;
end;

function TBrowserBaseFrame.DoNavigate(const URL: string): Boolean;
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
begin
  MakeBrowserActiveControl;
end;

function TBrowserBaseFrame.HasPopup: Boolean;
begin
  Result := WBController.UIMgr.SupportsPopupMenu;
end;

procedure TBrowserBaseFrame.HTMLEventHandler(Sender: TObject;
  const EventInfo: THTMLEventInfo);
var
  ALink: IDispatch; // any <a> tag generating (or wrapping tag generating) event
begin
  try
    // Trigger OnHTMLEvent: uses original sender
    if Assigned(fOnHTMLEvent) then
      fOnHTMLEvent(Sender, EventInfo);
    // We only handle onclick events if they've not already been handled and
    // cancelled
    if EventInfo.Cancelled
      or not EventInfo.IsEvent(
        THTMLDocumentEvents2Sink.EventIntf,
        THTMLDocumentEvents2Sink.DISPID_OnClick
      ) then
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

procedure TBrowserBaseFrame.HTMLWindowErrorHandler(Sender: TObject; const Desc,
  URL: string; const Line: Integer; var Handled: Boolean);
const
  ErrorMessage = 'An unexpected error occurred loading a document or running a '
    + 'script in the browser control hosted by frame %0:s:'#10#10
    + 'Document: %2:s, line %3:d'#10
    + 'Error: "%1:s"';

begin
  try
    raise EBug.CreateFmt(
      ErrorMessage, [ClassName, Desc, URIBaseName(URL), Line]
    );
  except
    Application.HandleException(ExceptObject);
  end;
  Handled := True;
end;

function TBrowserBaseFrame.IsBrowserActive: Boolean;
begin
  Result := ParentForm.ActiveControl = wbBrowser;
end;

procedure TBrowserBaseFrame.MakeBrowserActiveControl;
begin
  if ParentForm <> nil then
    ParentForm.ActiveControl := wbBrowser;
end;

procedure TBrowserBaseFrame.NavigateHandler(Sender: TObject;
  const URL: string; var Cancel: Boolean);
begin
  try
    Cancel := DoNavigate(URL);
  except
    Application.HandleException(ExceptObject);
    Cancel := True;
  end;
end;

function TBrowserBaseFrame.ParentForm: TForm;
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
begin
  WBController.UIMgr.ShowPopupMenu(Pt);
end;

procedure TBrowserBaseFrame.SelectAll;
begin
  WBController.UIMgr.SelectAll;
end;

procedure TBrowserBaseFrame.TranslateAccelHandler(Sender: TObject;
  const Msg: TMSG; const CmdID: DWORD; var Handled: Boolean);

  // ---------------------------------------------------------------------------
  ///  Posts a key down and key up message to frame's parent window using wParam
  ///  and lParam values of message passed to outer method.
  procedure PostKeyPress(const DownMsg, UpMsg: UINT);
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
        if (ShiftState = [ssCtrl]) or (ShiftState = []) then
        begin
          TriggerActiveLink;  // uses new tab if Ctrl key pressed.
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
begin
  TAnchors.ClickActiveLink(wbBrowser.Document);
end;

procedure TBrowserBaseFrame.UpdateCSS(Sender: TObject; var CSS: string);
var
  CSSBuilder: TCSSBuilder;  // object used to build CSS
begin
  CSSBuilder := TCSSBuilder.Create;
  try
    BuildCSS(CSSBuilder);
    if Assigned(fOnBuildCSS) then
      fOnBuildCSS(Self, CSSBuilder);
    CSS := CSSBuilder.AsString;
  finally
    FreeAndNil(CSSBuilder);
  end;
end;

end.


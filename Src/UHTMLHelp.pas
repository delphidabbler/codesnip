{
 * This Source Code Form is subject to the terms of the Mozilla Public License,
 * v. 2.0. If a copy of the MPL was not distributed with this file, You can
 * obtain one at http://mozilla.org/MPL/2.0/
 *
 * Copyright (C) 2005-2020, Peter Johnson (gravatar.com/delphidabbler).
 *
 * Provides a partial translation of HTMLHelp header files. Dynamically imports
 * HTMLHelp function from hhctrl.ocx.
}


unit UHTMLHelp;


interface


uses
  // Delphi
  Windows;


function HtmlHelp(hwndCaller: HWND; pszFile: LPCTSTR; uCommand: UINT;
  dwData: DWORD): HWND; stdcall;
  {Single function used to control HTML Help. Action varies according to the
  specified command.
    @param hwndCaller [in] Handle of window calling HtmlHelp. HtmlHelp returns
      focus to this window when closed, unless window is desktop. If
      notifcations are enabled HtmlHelp sends notification messages to this
      window.
    @param pszFile [in] Depending on uCommand specifies either a .chm file or a
      topic file within a help file. May also specify a window.
    @param uCommand [in] Specifies command to be executed.
    @param dwData [in] Specifies any data that may be required, based on the
      value of the uCommand parameter.
    @return Depending on uCommand and the result, either the handle of the help
      window or 0 are returned. Zero can indicate either failure or that the
      help window has not yet been created.
    @except ECodeSnip raised if hhctrl.ocx library not available.
  }


const
  //
  // Commands for HtmlHelp API function
  //
  HH_DISPLAY_TOPIC           = $0000;
    {Opens a help topic in a specified help window}
  HH_HELP_FINDER             = $0000;
    {Alias for HH_DISPLAY_TOPIC}
  HH_DISPLAY_TOC             = $0001;
    {Selects the Contents tab in the Navigation pane of the HTML Help Viewer}
  HH_DISPLAY_INDEX           = $0002;
    {Selects the Index tab in the Navigation pane of the HTML Help Viewer and
    searches for the keyword specified in the dwData parameter}
  HH_DISPLAY_SEARCH          = $0003;
    {Selects the Search tab in the Navigation pane of the HTML Help Viewer}
  HH_SET_WIN_TYPE            = $0004;
    {Creates a new help window or modifies an existing help window at run time}
  HH_GET_WIN_TYPE            = $0005;
    {Retrieves a pointer to the HH_WINTYPE structure associated with a specified
    window type}
  HH_GET_WIN_HANDLE          = $0006;
    {Returns the handle of a specified window type}
  HH_ENUM_INFO_TYPE          = $0007;
    {No documentation in htmlhelp.chm}
  HH_SET_INFO_TYPE           = $0008;
    {No documentation in htmlhelp.chm}
  HH_SYNC                    = $0009;
    {Locates and selects the contents entry for the help topic that is open in
    the Topic pane of the HTML Help Viewer}
  HH_RESERVED1               = $000A;
    {Reserved for future use}
  HH_RESERVED2               = $000B;
    {Reserved for future use}
  HH_RESERVED3               = $000C;
    {Reserved for future use}
  HH_KEYWORD_LOOKUP          = $000D;
    {Looks up one or more keywords in a compiled help file}
  HH_DISPLAY_TEXT_POPUP      = $000E;
    {Opens a pop-up window that displays the contents of one of the following:
    an explicit text string; a text string based on a resource ID; a text string
    ID based on a text file contained in a compiled help file}
  HH_HELP_CONTEXT            = $000F;
    {Displays a help topic based on a mapped topic ID}
  HH_TP_HELP_CONTEXTMENU     = $0010;
    {Opens a pop-up context menu. Generally used in response to the Windows
    WM_CONTEXTMENU message. For example, this message is sent when a user
    right-clicks a dialog box control}
  HH_TP_HELP_WM_HELP         = $0011;
    {Opens a pop-up help topic. Generally used in response to the Windows
    WM_HELP message. For example, this message is sent when a user presses F1}
  HH_CLOSE_ALL               = $0012;
    {Closes all windows opened directly or indirectly by the calling program}
  HH_ALINK_LOOKUP            = $0013;
    {Looks up one or more Associative Link (ALink) names in compiled help file}
  HH_GET_LAST_ERROR          = $0014;
    {Returns information about the last error that occurred in hhctrl.ocx}
  HH_ENUM_CATEGORY           = $0015;
    {No documentation in htmlhelp.chm}
  HH_ENUM_CATEGORY_IT        = $0016;
    {No documentation in htmlhelp.chm}
  HH_RESET_IT_FILTER         = $0017;
    {No documentation in htmlhelp.chm}
  HH_SET_INCLUSIVE_FILTER    = $0018;
    {No documentation in htmlhelp.chm}
  HH_SET_EXCLUSIVE_FILTER    = $0019;
    {No documentation in htmlhelp.chm}
  HH_INITIALIZE              = $001C;
    {Initializes the help system for use and must be the first HTML Help command
    called. It returns a cookie which must be used in the HH_UNINITIALIZE call}
  HH_UNINITIALIZE            = $001D;
    {Called to properly shut down HTML Help. This function should be the last
    help command the application calls. HH_UNINITIALIZE should not be called
    during DLL process detach, but during the normal application shutdown
    process}
  HH_PRETRANSLATEMESSAGE     = $00FD;
    {Called in a Window application's message loop to ensure proper handling of
    Windows messages, especially keyboard messages when running HTML Help single
    thread}
  HH_SET_GLOBAL_PROPERTY     = $00FC;
    {No documentation in htmlhelp.chm}


type

  {
  THHAKLink:
    Structure used to specify one or more ALink names or KLink keywords to be
    searched for.
  }
  THHAKLink = packed record
    cbStruct: Integer;    // sizeof this structure
    fReserved: BOOL;      // must be FALSE (really!)
    pszKeywords: LPCTSTR; // semi-colon separated keywords
    pszUrl: LPCTSTR;      // URL to jump to if no keywords found (may be nil)
    pszMsgText: LPCTSTR;  // MessageBox text on failure (used if pszUrl nil)
    pszMsgTitle: LPCTSTR; // Title of any failure MessageBox
    pszWindow: LPCTSTR;   // Window to display pszURL in
    fIndexOnFail: BOOL;   // Displays index if keyword lookup fails.
  end;

  {
  PHHAKLink:
    Pointer to THHAKLink structure.
  }
  PHHAKLink = ^THHAKLink;


implementation


uses
  // Delphi
  SysUtils,
  // Project
  UConsts, UExceptions;


type

  {
  THtmlHelp:
    Prototype for HtmlHelpA API function.
      @param hwndCaller [in] Handle of window calling HtmlHelp. HtmlHelp returns
        focus to this window when closed, unless window is desktop. If
        notifcations are enabled HtmlHelp sends notification messages to this
        window.
      @param pszFile [in] Depending on uCommand specifies either a .chm file or
        a topic file within a help file. May also specify a window.
      @param uCommand [in] Specifies command to be executed.
      @param dwData [in] Specifies any data that may be required, based on the
        value of the uCommand parameter.
      @return Depending on uCommand and the result, either the handle of the
        help window or 0 are returned. Zero can indicate either failure or that
        the help window has not yet been created.
  }
  THtmlHelp = function (hwndCaller: HWND; pszFile: LPCTSTR; uCommand: UINT;
    dwData: DWORD): HWND; stdcall;


var
  pvtHHCtrl: THandle = 0;         // handle to hhctrl.ocx library
  pvtHtmlHelp: THtmlHelp = nil;   // pointer to HtmlHelpA API function


resourcestring
  // Error message used for exception raised when hhctrl.ocx not found
  sNoHelp = 'Sorry, help is not available. ' + EOL2
    + 'This is because the HTML Help library (hhctrl.ocx) cannot be found.';


function HtmlHelp(hwndCaller: HWND; pszFile: LPCTSTR; uCommand: UINT;
  dwData: DWORD): HWND; stdcall;
  {Single function used to control HTML Help. Action varies according to the
  specified command.
    @param hwndCaller [in] Handle of window calling HtmlHelp. HtmlHelp returns
      focus to this window when closed, unless window is desktop. If
      notifcations are enabled HtmlHelp sends notification messages to this
      window.
    @param pszFile [in] Depending on uCommand specifies either a .chm file or a
      topic file within a help file. May also specify a window.
    @param uCommand [in] Specifies command to be executed.
    @param dwData [in] Specifies any data that may be required, based on the
      value of the uCommand parameter.
    @return Depending on uCommand and the result, either the handle of the help
      window or 0 are returned. Zero can indicate either failure or that the
      help window has not yet been created.
    @except ECodeSnip raised if hhctrl.ocx library not available.
  }
begin
  if not Assigned(pvtHtmlHelp) then
    raise ECodeSnip.Create(sNoHelp);
  Result := pvtHtmlHelp(hwndCaller, pszFile, uCommand, dwData);
end;

procedure LoadHtmlHelp;
  {Attempt to load HtmlHelpA function from hhctrl.ocx.
  }
const
  cHTMLHelp = 'HtmlHelpW';
begin
  pvtHHCtrl := SafeLoadLibrary('hhctrl.ocx');
  if pvtHHCtrl <> 0 then
    pvtHtmlHelp := GetProcAddress(pvtHHCtrl, cHTMLHelp);
end;

initialization

LoadHtmlHelp;

finalization

// Free hhctrl.ocx
if pvtHHCtrl <> 0 then
  FreeLibrary(pvtHHCtrl);

end.


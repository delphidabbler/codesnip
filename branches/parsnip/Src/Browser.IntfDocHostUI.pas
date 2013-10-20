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
 * Interfaces, records and constants used when hosting the IE WebBrowser Control
 * or automating IE to replace the menus, toolbars, and context menus. The
 * content of the unit is based on Microsoft UI documentation from MSDN. Not all
 * the structures and constants are used in CodeSnip.
}


unit Browser.IntfDocHostUI;


interface


uses
  // Delphi
  Windows, ActiveX;


type

  ///  <summary>Defines a set of flags that indicate the capabilities of an
  ///  IDocHostUIHandler implementation.</summary>
  ///  <remarks>The meaning of the values is explained on MSDN at
  ///  http://msdn.microsoft.com/en-us/library/aa753277.aspx.</remarks>
  TDocHostUIFlag = record
  public
    const
      DIALOG = $00000001;
      DISABLE_HELP_MENU = $00000002;
      NO3DBORDER = $00000004;
      SCROLL_NO = $00000008;
      DISABLE_SCRIPT_INACTIVE = $00000010;
      OPENNEWWIN = $00000020;
      DISABLE_OFFSCREEN = $00000040;
      FLAT_SCROLLBAR = $00000080;
      DIV_BLOCKDEFAULT = $00000100;
      ACTIVATE_CLIENTHIT_ONLY = $00000200;
      OVERRIDEBEHAVIORFACTORY = $00000400;
      CODEPAGELINKEDFONTS = $00000800;
      URL_ENCODING_DISABLE_UTF8 = $00001000;
      URL_ENCODING_ENABLE_UTF8 = $00002000;
      ENABLE_FORMS_AUTOCOMPLETE = $00004000;
      ENABLE_INPLACE_NAVIGATION = $00010000;
      IME_ENABLE_RECONVERSION = $00020000;
      THEME = $00040000;
      NOTHEME = $00080000;
      NOPICS = $00100000;
      NO3DOUTERBORDER = $00200000;
      DISABLE_EDIT_NS_FIXUP = $00400000;
      LOCAL_MACHINE_ACCESS_CHECK = $00800000;
      DISABLE_UNTRUSTEDPROTOCOL = $01000000;
      HOST_NAVIGATES = $02000000;
      ENABLE_REDIRECT_NOTIFICATION = $04000000;
      USE_WINDOWLESS_SELECTCONTROL = $08000000;
      USE_WINDOWED_SELECTCONTROL = $10000000;
      ENABLE_ACTIVEX_INACTIVATE_MODE = $20000000;
      DPI_AWARE = $40000000;
  end;

  ///  <summary>Defines a set of flags that indicate the required action on a
  ///  double click event in a IDocHostUIHandler implementation.</summary>
  ///  <remarks>The meaning of the values is explained on MSDN at
  ///  http://msdn.microsoft.com/en-us/library/aa753276.aspx.</remarks>
  TDocHostUIDblClk = record
  public
    const
      DEFAULT = 0;
      SHOWPROPERTIES = 1;
      SHOWCODE = 2;
  end;

  ///  <summary>Defines a set of flags that indicate the kind of UI required in
  ///  when in an IDocHostUIHandler implementation.</summary>
  ///  <remarks>The meaning of the values is explained on MSDN at
  ///  http://msdn.microsoft.com/en-us/library/aa753278.</remarks>
  TDocHostUIType = record
  public
    const
      BROWSE = 0;
      AUTHOR = 1;
  end;

  ///  <summary>Defines a set of flags that indicate which kind of shortcut menu
  ///  is to be displayed by the browser control in an IDocHostUIHandler
  ///  implementation.</summary>
  ///  <remarks>The meaning of the values is explained on MSDN at
  ///  http://msdn.microsoft.com/en-us/library/aa753264.</remarks>
  TDocHostUIContextMenu = record
  public
    const
      DEFAULT = $00;
      IMAGE = $01;
      CONTROL = $02;
      TABLE = $03;
      TEXTSELECT = $04;
      ANCHOR = $05;
      UNKNOWN = $06;
      VSCROLL = $09;
      HSCROLL = $10;
      MEDIA = $11;
  end;

  {
  TDocHostUIInfo:
    Record used by the IDocHostUIHandler.GetHostInfo method to allow MSHTML to
    retrieve information about the host's UI requirements.
  }
  TDocHostUIInfo = record
    cbSize: ULONG;
      {Size of structure in bytes}
    dwFlags: DWORD;
      {One or more DOCHOSTUIFLAG_* flags that specify UI capabilitiess of host}
    dwDoubleClick: DWORD;
      {A DOCHOSTUIDBLCLK_* value that specifies operation in response to a
      double-click}
    pchHostCss: PWChar;
      {Pointer to set of CSS rules set by host}
    pchHostNS: PWChar;
      {Pointer to a ';' delimited namespace list that allows declaration of
      namespaces for custom tags on the page}
  end;

  {
  PDocHostUIInfo:
    Pointer to TDocHostUIInfo record.
  }
  PDocHostUIInfo = ^TDocHostUIInfo;

  {
  IDocHostUIHandler:
    This customisation interface enables an application hosting the WebBrowser
    Control or automating IE to replace the menus, toolbars, and context menus
    used by MSHTML.
  }
  IDocHostUIHandler = interface(IUnknown)
    ['{bd3f23c0-d43e-11cf-893b-00aa00bdce1a}']
    function ShowContextMenu(const dwID: DWORD; const ppt: PPOINT;
      const pcmdtReserved: IUnknown; const pdispReserved: IDispatch): HResult;
      stdcall;
      {Called by MSHTML to display a shortcut menu.
        @param dwID [in] Specifies identifier of the shortcut menu to be
          displayed. This identifier is $1 shifted left by the value of one of
          the values defined in TDocHostUIContextMenu.
        @param ppt [in] Pointer to POINT structure containing screen coordinates
          for the menu.
        @param pcmdtReserved [in] IUnknown interface of an IOleCommandTarget
          interface used to query command status and execute commands on this
          object.
        @param pdispReserved [in] IDispatch interface of the object at the
          screen coordinates specified in ppt. This allows a host to
          differentiate particular objects to provide more specific context. In
          IE 4.0 this parameter supplied no information, but in IE5 and later
          the parameter contains an IDispatch interface.
        @return S_OK if host displayed its own user interface (UI) - MSHTML will
          not attempt to display its UI; S_FALSE if host did not display any UI
          - MSHTML will display its UI.
      }
    function GetHostInfo(var pInfo: TDocHostUIInfo): HResult; stdcall;
      {Called by MSHTML to retrieve the user interface (UI) capabilities and
      requirement of the application that is hosting MSHTML. Various aspects of
      MSHTML can be controlled.
        @param pInfo [in, out] Reference to a TDocHostUIInfo structure that
          receives the host's UI capabilities.
        @return S_OK if successful, or an error value otherwise or if we don't
          make any changes to pInfo.
      }
    function ShowUI(const dwID: DWORD;
      const pActiveObject: IOleInPlaceActiveObject;
      const pCommandTarget: IOleCommandTarget; const pFrame: IOleInPlaceFrame;
      const pDoc: IOleInPlaceUIWindow): HResult; stdcall;
      {Called by MSHTML to enable the host to replace MSHTML menus and toolbars
      etc. If the host uses any of the interfaces handed to it by this function,
      the host should call the interface's _AddRef method to save the interface
      for later use. If the host calls the interface's _AddRef method, the host
      must also call the interface's _Release method when the interface is no
      longer required.
        @param dwID [in] Receives a DOCHOSTUITYPE_* value indicating the type of
          user interface (UI).
        @param pActiveObject [in] IOleInPlaceActiveObject interface reference
          for the active object.
        @param pCommandTarget [in] IOleCommandTarget interface for the object.
        @pFrame [in] IOleInPlaceFrame interface for the object. Menus and
          toolbars must use this parameter.
        @param pDoc [in] An IOleInPlaceUIWindow interface for the object.
          Toolbars must use this parameter.
        @return S_OK if host displayed its own UI (MSHTML will not display its
          UI); S_FALSE if host did not display its own UI (MSHTML will display
          its UI) or DOCHOST_E_UNKNOWN if host did not recognize the UI
          identifier. MSHTML will either try an alternative identifier for
          compatibility with a previous version or display its own UI.
      }
    function HideUI: HResult; stdcall;
      {Called when MSHTML removes its menus and toolbars. If a host displayed
      menus and toolbars during the call to ShowUI, it should remove them when
      this method is called. This method is called regardless of the return
      value from ShowUI.
        @return S_OK on success or error value on failure.
      }
    function UpdateUI: HResult; stdcall;
      {Called by MSHTML to notify the host that the command state has changed.
      The host should update the state of toolbar buttons in an implementation
      of this method. This method is called regardless of the return value from
      the IDocHostUIHandler.ShowUI method.
        @return S_OK on success or error value on failure.
      }
    function EnableModeless(const fEnable: BOOL): HResult; stdcall;
      {Called by the MSHTML implementation of IOleInPlaceActiveObject.
      EnableModeless. Also called when MSHTML displays a modal UI.
        @param fEnable [in] Indicates if the host's modeless dialog boxes are
          enabled (true) or disabled (false).
        @return S_OK on success or error value on failure.
      }
    function OnDocWindowActivate(const fActivate: BOOL): HResult; stdcall;
      {Called by the MSHTML implementation of IOleInPlaceActiveObject.
      OnDocWindowActivate when the document window is activated or deactivated.
        @param fActivate [in] Indicates the state of the document window: true
          if the window is being activated and false if the window is being
          deactivated.
        @return S_OK on success or error value on failure.
      }
    function OnFrameWindowActivate(const fActivate: BOOL): HResult; stdcall;
      {Called by the MSHTML implementation of IOleInPlaceActiveObject.
      OnFrameWindowActivate when the top-level frame window is activated or
      deactivated.
        @param fActivate [in] Indicates the state of the container's top-level
          frame window: true if the window is being activated and false if the
          window is being deactivated.
        @return S_OK on success or error value on failure.
      }
    function ResizeBorder(const prcBorder: PRECT;
      const pUIWindow: IOleInPlaceUIWindow; const fFrameWindow: BOOL): HResult;
      stdcall;
      {Called by the MSHTML implementation of IOleInPlaceActiveObject.
      ResizeBorder called when a frame or document's window's border is about to
      be changed.
        @param prcBorder [in] Pointer to a RECT for the new outer rectangle of
          the border.
        @param pUIWindow [in] Reference to an IOleInPlaceUIWindow interface for
          the frame or document window whose border is to be changed.
        @param fFrameWindow [in] Flag True if the frame window is calling
          IDocHostUIHandler.ResizeBorder, or False otherwise.
        @return S_OK on success or error value on failure.
      }
    function TranslateAccelerator(const lpMsg: PMSG; const pguidCmdGroup: PGUID;
      const nCmdID: DWORD): HResult; stdcall;
      {Called by MSHTML when IOleInPlaceActiveObject.TranslateAccelerator or
      IOleControlSite.TranslateAccelerator is called. When accelerator keys such
      as TAB are used, the default host behavior may need to be overridden.
        @param lpMsg [in] Pointer to a MSG structure that specifies the message
          to be translated.
        @param pguidCmdGroup [in] Pointer to a GUID for the command group
          identifier.
        @param nCmdID [in] Specifies a command identifier.
        @return S_OK on success or error value on failure. Return S_FALSE if
          we override behaviour.
      }
    function GetOptionKeyPath(var pchKey: POLESTR; const dw: DWORD ): HResult;
      stdcall;
      {Called by the WebBrowser Control to retrieve a registry subkey path that
      overrides the default IE registry settings. If S_FALSE is returned or if
      the registry key path returned in pchKey is nil or empty, the WebBrowser
      Control reverts to the default Internet Explorer registry settings.
        @param pchKey [out] POLESTR that receives the registry subkey string
          where the host stores its registry settings.
        @param dw [in] Reserved. Always 0.
        @return S_OK if successful, S_FALSE to use default registry setting,
          or an error value otherwise.
      }
    function GetDropTarget(const pDropTarget: IDropTarget;
      out ppDropTarget: IDropTarget): HResult; stdcall;
      {Called by MSHTML when it is used as a drop target and enables the host to
      supply an alternative IDropTarget interface.
        @param pDropTarget [in] Pointer to an IDropTarget interface for the
          current drop target object supplied by MSHTML.
        @param ppDropTarget [out] Address of a pointer variable that receives an
          IDropTarget interface pointer for the alternative drop target object
          supplied by the host. If we don't provide an alternative drop target
          we must return a failure code such as E_NOTIMPL or E_FAIL.
        @return S_OK if successful, or an error value otherwise.
      }
    function GetExternal(out ppDispatch: IDispatch): HResult; stdcall;
      {Called by MSHTML to obtain the host's IDispatch interface. If the host
      exposes an automation interface, it can provide a reference to it to
      MSHTML in this method. Used to enable the browser to call methods in the
      host.
        @param ppDispatch [out] Address of a pointer to a variable that receives
          an IDispatch interface pointer for the host application. Must be set
          to nil if we don't supply an IDispatch interface even if the method
          fails or returns S_FALSE.
        @return S_OK if successful, or an error value otherwise.
      }
    function TranslateUrl(const dwTranslate: DWORD; const pchURLIn: POLESTR;
      var ppchURLOut: POLESTR): HResult; stdcall;
      {Called by MSHTML to give the host an opportunity to modify the URL to be
      loaded.
        @param dwTranslate [in] Reserved. Always 0.
        @param pchURLIn [in] Pointer to OLE string that specifies the current
          URL for navigation.
        @param ppchURLOut [out] Address of a pointer variable that receives an
          OLE string pointer containing the new URL. The buffer pointed to by
          ppchURLOut should be allocated using CoTaskMemAlloc. If the
          implementation of this method does not supply a URL, ppchURLOut should
          be set to nil, even if the method fails or returns S_FALSE.
        @return S_OK if URL was translated, or S_FALSE if not.
      }
    function FilterDataObject(const pDO: IDataObject;
      out ppDORet: IDataObject): HResult; stdcall;
      {Called by MSHTML to allow the host to replace the MSHTML data object. It
      enables the host to block certain clipboard formats or support additional
      clipboard formats.
        @param pDO [in] Pointer to an IDataObject interface supplied by MSHTML.
        @param ppDORet [out] Address of a pointer variable that receives an
          IDataObject interface pointer supplied by the host. Must be set to nil
          if we don't supply a IDataObject, even if we fail or return S_FALSE.
        @return S_OK if the data object is replaced, or S_FALSE if not.
      }
  end;

  {
  ICustomDoc:
    Interface implemented by MSHTML to allow a host to set the MSHTML
    IDocHostUIHandler interface.
  }
  ICustomDoc = interface(IUnknown)
    ['{3050f3f0-98b5-11cf-bb82-00aa00bdce0b}']
    function SetUIHandler(const pUIHandler: IDocHostUIHandler): HResult;
      stdcall;
      {Sets the IDocHostUIHandler interface for MSHTML. MSHTML will release its
      previous IDocHostUIHandler interface (if one is present) and call
      pUIHandler's _AddRef method.
        @param pUIHandler [in] Host's IDocHostUIHandler interface.
        @return S_OK on success or error value on failure.
      }
  end;


implementation

end.


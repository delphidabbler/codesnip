{
 * Browser.UNulUIHandler.pas
 *
 * Defines a class that provides a "do-nothing" implementation of the
 * IDocHostUIHandler interface. All methods are "stubbed out" to return values
 * that will make no changes to the web browser control if this class is
 * assigned as the browser's UI handler.
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
 * The Original Code is Browser.UNulUIHandler.pas, formerly UNulUIHandler.pas
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


unit Browser.UNulUIHandler;


interface


uses
  // Delphi
  Windows, ActiveX,
  // Project
  Browser.IntfDocHostUI, UBaseObjects;


type

  {
  TNulUIHandler:
    "Do nothing" implementation of IDocHostUIHandler that is designed for use
    as a base class for others classes that selectively implement methods for
    specific UI purposes. The effect of this class, when assigned as a web
    browser control's UI handler is neutral.
  }
  TNulUIHandler = class(TAggregatedOrLoneObject,
    IDocHostUIHandler
  )
  protected
    function ShowContextMenu(const dwID: DWORD; const ppt: PPOINT;
      const pcmdtReserved: IUnknown; const pdispReserved: IDispatch): HResult;
      stdcall;
      {Called when browser ready to display its context menu. Allows browser to
      display default menu.
        @param dwID Not used.
        @param ppt Not used.
        @param pcmdtReserved Not used.
        @param pdispReserved Not used.
        @return S_FALSE to indicate we did not display any UI.
      }
    function GetHostInfo(var pInfo: TDocHostUIInfo): HResult; stdcall;
      {Retrieves UI capabilities. No action taken.
        @param pInfo Not used.
        @return S_OK to indicate success.
      }
    function ShowUI(const dwID: DWORD;
      const pActiveObject: IOleInPlaceActiveObject;
      const pCommandTarget: IOleCommandTarget; const pFrame: IOleInPlaceFrame;
      const pDoc: IOleInPlaceUIWindow): HResult; stdcall;
      {Called to enable host to modify IE menus, toolbars etc. No changes made.
        @param dwID Not used.
        @param pActiveObject Not used.
        @param pCommandTarget Not used.
        @param pDoc Not used.
        @return S_OK to indicate we displayed own UI (required since browser is
          not displaying its UI).
      }
    function HideUI: HResult; stdcall;
      {Called when IE menus, toolbars etc are to be removed. No action taken.
        @return S_OK to indicate we handled successfully.
      }
    function UpdateUI: HResult; stdcall;
      {Called when menus, toolbars etc need to be updated. No action taken.
        @return S_OK to indicate we handled successfully.
      }
    function EnableModeless(const fEnable: BOOL): HResult; stdcall;
      {Called when a modal UI is displayed by IE. No action taken.
        @param fEnable Not used.
        @return S_OK to indicate we handled successfully.
      }
    function OnDocWindowActivate(const fActivate: BOOL): HResult; stdcall;
      {Called when the document window is activated/deactivated. No action
      taken.
        @param fActivate Not used.
        @return S_OK to indicate we handled successfully.
      }
    function OnFrameWindowActivate(const fActivate: BOOL): HResult; stdcall;
      {Called when the top level frame window is activated/deactivated. No
      action taken.
        @param fActivate Not used.
        @return S_OK to indicate we handled successfully.
      }
    function ResizeBorder(const prcBorder: PRECT;
      const pUIWindow: IOleInPlaceUIWindow; const fFrameWindow: BOOL): HResult;
      stdcall;
      {Called when frame or document window changing. No action taken.
        @param prcBorder Not used.
        @param pUIWindow Not used.
        @param fFrameWindow Not used.
        @return S_FALSE to indicate we did nothing.
      }
    function TranslateAccelerator(const lpMsg: PMSG; const pguidCmdGroup: PGUID;
      const nCmdID: DWORD): HResult; stdcall;
      {Called when accelerator keys are used to enable behaviour to be changed.
      No changes are made.
        @param lpMsg Not used.
        @param pguidCmdGroup Not used.
        @param nCmdID Not used.
        @return S_FALSE to indicate no translations are made.
      }
    function GetOptionKeyPath(var pchKey: POLESTR; const dw: DWORD ): HResult;
      stdcall;
      {Called when IE retrieves data from registry and enables sub key path to
      be changed. Default registry settings not changed.
        @param pchKey Set to nil to indicate usage of default registry settings.
        @param dw Not used.
        @return S_FALSE to use default registry setting.
      }
    function GetDropTarget(const pDropTarget: IDropTarget;
      out ppDropTarget: IDropTarget): HResult; stdcall;
      {Called when browser is used as drop target to enable alternative drop
      target interface to be displayed. No alternative drop target is supplied.
        @param pDropTarget Not used.
        @param ppDropTarget Set to nil to indicate no alternative drop target.
        @return E_FAIL to indicate to alternative drop target supplied.
      }
    function GetExternal(out ppDispatch: IDispatch): HResult; stdcall;
      {Called to get host's implementation of browser's external object to
      enable browser (e.g. scripts) to call host's methods. No object is
      exposed.
        @param ppDispatch Set to nil to indicate no external object exposed.
        @return E_FAIL to indicate to external object.
      }
    function TranslateUrl(const dwTranslate: DWORD; const pchURLIn: POLESTR;
      var ppchURLOut: POLESTR): HResult; stdcall;
      {Gives an opportunity to modify the URL to be loaded. No changes made.
        @param dwTranslate Not used.
        @param pchURLIn Not used.
        @param ppchURLOut Set to nil to indicate no translation.
        @return S_FALSE to indicate URL not translated.
      }
    function FilterDataObject(const pDO: IDataObject;
      out ppDORet: IDataObject): HResult; stdcall;
      {Called to enable host to block or add additional clipboard objects. No
      changes made.
        @param pDO Not used.
        @param ppDORet Set to nil to indicate no additional formats.
        @return S_FALSE to indicate no additional formats.
      }
  end;


implementation


{ TNulUIHandler }

function TNulUIHandler.EnableModeless(const fEnable: BOOL): HResult;
  {Called when a modal UI is displayed by IE. No action taken.
    @param fEnable Not used.
    @return S_OK to indicate we handled successfully.
  }
begin
  Result := S_OK;
end;

function TNulUIHandler.FilterDataObject(const pDO: IDataObject;
  out ppDORet: IDataObject): HResult;
  {Called to enable host to block or add additional clipboard objects. No
  changes made.
    @param pDO Not used.
    @param ppDORet Set to nil to indicate no additional formats.
    @return S_FALSE to indicate no additional formats.
  }
begin
  ppDORet := nil;
  Result := S_FALSE;
end;

function TNulUIHandler.GetDropTarget(const pDropTarget: IDropTarget;
  out ppDropTarget: IDropTarget): HResult;
  {Called when browser is used as drop target to enable alternative drop target
  interface to be displayed. No alternative drop target is supplied.
    @param pDropTarget Not used.
    @param ppDropTarget Set to nil to indicate no alternative drop target.
    @return E_FAIL to indicate to alternative drop target supplied.
  }
begin
  ppDropTarget := nil;
  Result := E_FAIL;
end;

function TNulUIHandler.GetExternal(out ppDispatch: IDispatch): HResult;
  {Called to get host's implementation of browser's external object to enable
  browser (e.g. scripts) to call host's methods. No object is exposed.
    @param ppDispatch Set to nil to indicate no external object exposed.
    @return E_FAIL to indicate to external object.
  }
begin
  ppDispatch := nil;
  Result := E_FAIL;
end;

function TNulUIHandler.GetHostInfo(var pInfo: TDocHostUIInfo): HResult;
  {Retrieves UI capabilities. No action taken.
    @param pInfo Not used.
    @return S_OK to indicate success.
  }
begin
  Result := S_OK;
end;

function TNulUIHandler.GetOptionKeyPath(var pchKey: POLESTR;
  const dw: DWORD): HResult;
  {Called when IE retrieves data from registry and enables sub key path to be
  changed. Default registry settings not changed.
    @param pchKey Set to nil to indicate usage of default registry settings.
    @param dw Not used.
    @return S_FALSE to use default registry setting.
  }
begin
  pchKey := nil;
  Result := S_FALSE;
end;

function TNulUIHandler.HideUI: HResult;
  {Called when IE menus, toolbars etc are to be removed. No action taken.
    @return S_OK to indicate we handled successfully.
  }
begin
  Result := S_OK;
end;

function TNulUIHandler.OnDocWindowActivate(const fActivate: BOOL): HResult;
  {Called when the document window is activated/deactivated. No action taken.
    @param fActivate Not used.
    @return S_OK to indicate we handled successfully.
  }
begin
  Result := S_OK;
end;

function TNulUIHandler.OnFrameWindowActivate(const fActivate: BOOL): HResult;
  {Called when the top level frame window is activated/deactivated. No action
  taken.
    @param fActivate Not used.
    @return S_OK to indicate we handled successfully.
  }
begin
  Result := S_OK;
end;

function TNulUIHandler.ResizeBorder(const prcBorder: PRECT;
  const pUIWindow: IOleInPlaceUIWindow; const fFrameWindow: BOOL): HResult;
  {Called when frame or document window changing. No action taken.
    @param prcBorder Not used.
    @param pUIWindow Not used.
    @param fFrameWindow Not used.
    @return S_FALSE to indicate we did nothing.
  }
begin
  Result := S_FALSE;
end;

function TNulUIHandler.ShowContextMenu(const dwID: DWORD;
  const ppt: PPOINT; const pcmdtReserved: IInterface;
  const pdispReserved: IDispatch): HResult;
  {Called when browser ready to display its context menu. Allows browser to
  display default menu.
    @param dwID Not used.
    @param ppt Not used.
    @param pcmdtReserved Not used.
    @param pdispReserved Not used.
    @return S_FALSE to indicate we did not display any UI.
  }
begin
  Result := S_FALSE;
end;

function TNulUIHandler.ShowUI(const dwID: DWORD;
  const pActiveObject: IOleInPlaceActiveObject;
  const pCommandTarget: IOleCommandTarget; const pFrame: IOleInPlaceFrame;
  const pDoc: IOleInPlaceUIWindow): HResult;
  {Called to enable host to modify IE menus, toolbars etc. No changes made.
    @param dwID Not used.
    @param pActiveObject Not used.
    @param pCommandTarget Not used.
    @param pDoc Not used.
    @return S_OK to indicate we displayed own UI (required since browser is not
      displaying its UI).
  }
begin
  Result := S_OK;
end;

function TNulUIHandler.TranslateAccelerator(const lpMsg: PMSG;
  const pguidCmdGroup: PGUID; const nCmdID: DWORD): HResult;
  {Called when accelerator keys are used to enable behaviour to be changed. No
  changes are made.
    @param lpMsg Not used.
    @param pguidCmdGroup Not used.
    @param nCmdID Not used.
    @return S_FALSE to indicate no translations are made.
  }
begin
  Result := S_FALSE;
end;

function TNulUIHandler.TranslateUrl(const dwTranslate: DWORD;
  const pchURLIn: POLESTR; var ppchURLOut: POLESTR): HResult;
  {Gives an opportunity to modify the URL to be loaded. No changes made.
    @param dwTranslate Not used.
    @param pchURLIn Not used.
    @param ppchURLOut Set to nil to indicate no translation.
    @return S_FALSE to indicate URL not translated.
  }
begin
  ppchURLOut := nil;
  Result := S_FALSE;
end;

function TNulUIHandler.UpdateUI: HResult;
  {Called when menus, toolbars etc need to be updated. No action taken.
    @return S_OK to indicate we handled successfully.
  }
begin
  Result := S_OK;
end;

end.


{
 * This Source Code Form is subject to the terms of the Mozilla Public License,
 * v. 2.0. If a copy of the MPL was not distributed with this file, You can
 * obtain one at https://mozilla.org/MPL/2.0/
 *
 * Copyright (C) 2005-2020, Peter Johnson (gravatar.com/delphidabbler).
 *
 * Contains a class that provides a do-nothing implementation of the
 * IOleClientSite interface that is sufficient to record the application as a
 * host for the IE web browser control.
}


unit UOleClientSite;


interface


uses
  // Delphi
  Windows, ActiveX,
  // Project
  UBaseObjects;

type

  {
  TOleClientSite:
    Minimal implementation of the IOleClientSite interface that provides a host
    container for the web browser control. This implementation is needed so that
    the web browser control can call QueryInterface on it to get hold of our
    IDocHostUIHandler implementation.
  }
  TOleClientSite = class(TAggregatedOrLoneObject,
    IUnknown, IOleClientSite)
  public
    { IOleClientSite methods }
    function SaveObject: HResult; stdcall;
      {Saves the object associated with the client site. No action taken.
        @return S_OK to inform we have responded.
      }
    function GetMoniker(dwAssign: Longint; dwWhichMoniker: Longint;
      out mk: IMoniker): HResult; stdcall;
      {Returns a moniker to an object's client site. We don't implement
      monikers.                                                          
        @param dwAssign [in] Not used.
        @param dwWhichMoniker [in] Not used.
        @param mk [out] Set to nil.
        @return E_NOTIMPL to indicate method is not implemented.
      }
    function GetContainer(out container: IOleContainer): HResult; stdcall;
      {Returns a pointer to the container's IOleContainer interface if
      supported. We don't support the interface.
        @param container [out] Set to nil.
        @return E_NOINTERFACE to indicate we don't support IOleContainer.
      }
    function ShowObject: HResult; stdcall;
      {Tells the container to position the object so it is visible to the user.
      No action taken.
        @return S_OK to inform we have responded.
      }
    function OnShowWindow(fShow: BOOL): HResult; stdcall;
      {Notifies a container when an embedded object's window is about to become
      visible or invisible. No action taken.
        @param fShow [in] Not used.
        @return S_OK to inform we have responded.
      }
    function RequestNewObjectLayout: HResult; stdcall;
      {Asks container to allocate more or less space for displaying an embedded
      object. We don't support layout requests.
        @return E_NOTIMPL to indicate layout requests not implemented.
      }
  end;


implementation


{ TOleClientSite }

function TOleClientSite.GetContainer(out container: IOleContainer): HResult;
  {Returns a pointer to the container's IOleContainer interface if supported. We
  don't support the interface.
    @param container [out] Set to nil.
    @return E_NOINTERFACE to indicate we don't support IOleContainer.
  }
begin
  container := nil;
  Result := E_NOINTERFACE;
end;

function TOleClientSite.GetMoniker(dwAssign, dwWhichMoniker: Integer;
  out mk: IMoniker): HResult;
  {Returns a moniker to an object's client site. We don't implement monikers.
    @param dwAssign [in] Not used.
    @param dwWhichMoniker [in] Not used.
    @param mk [out] Set to nil.
    @return E_NOTIMPL to indicate method is not implemented.
  }
begin
  mk := nil;
  Result := E_NOTIMPL;
end;

function TOleClientSite.OnShowWindow(fShow: BOOL): HResult;
  {Notifies a container when an embedded object's window is about to become
  visible or invisible. No action taken.
    @param fShow [in] Not used.
    @return S_OK to inform we have responded.
  }
begin
  Result := S_OK;
end;

function TOleClientSite.RequestNewObjectLayout: HResult;
  {Asks container to allocate more or less space for displaying an embedded
  object. We don't support layout requests.
    @return E_NOTIMPL to indicate layout requests not implemented.
  }
begin
  Result := E_NOTIMPL;
end;

function TOleClientSite.SaveObject: HResult;
  {Saves the object associated with the client site. No action taken.
    @return S_OK to inform we have responded.
  }
begin
  Result := S_OK;
end;

function TOleClientSite.ShowObject: HResult;
  {Tells the container to position the object so it is visible to the user. No
  action taken.
    @return S_OK to inform we have responded.
  }
begin
  Result := S_OK;
end;

end.


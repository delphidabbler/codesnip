{
 * This Source Code Form is subject to the terms of the Mozilla Public License,
 * v. 2.0. If a copy of the MPL was not distributed with this file, You can
 * obtain one at https://mozilla.org/MPL/2.0/
 *
 * Copyright (C) 2005-2021, Peter Johnson (gravatar.com/delphidabbler).
 *
 * Class that hosts the IE web browser control and enables both direct loading
 * and saving of browser's HTML and customisation of the user interface. Much of
 * the functionality of the class is handled by owned objects.
}


unit Browser.UController;


interface


uses
  // Delphi
  SHDocVw, ActiveX,
  // Project
  Browser.IntfDocHostUI, Browser.UIOMgr, Browser.UUIMgr, UBaseObjects,
  UOleClientSite;


type

  {
  TWBController:
    Class that hosts the IE web browser control and enables both direct loading
    and saving of browser's HTML and customisation of the user interface. Much
    of the functionality of the class is handled by owned objects.
  }
  TWBController = class(TNonRefCountedObject,
    IOleClientSite,     // notifies web browser that we provide an OLE container
    IDocHostUIHandler   // accessed web browser to customise UI
  )
  strict private
    fWebBrowser: TWebBrowser;
      {Reference to controlled web browser control}
    fIOMgr: TWBIOMgr;
      {Object that controls web browser IO}
    fOleClientSite: TOleClientSite;
      {Object's IOleClientSite implementation}
    fUIMgr: TWBUIMgr;
      {Object that customises web browser UI}
    procedure SetBrowserOleClientSite(const Site: IOleClientSite);
      {Registers this object as web browser's OLE container.
        @param Reference to object providing OLE container. May be Self to
          register this object as OLE container or nil to unregister.
      }
  public
    constructor Create(const WebBrowser: TWebBrowser);
      {Object constructor. Sets up object as container for a browser control.
        @param WebBrowser Contained browser control.
      }
    destructor Destroy; override;
      {Object destructor. Unregisters browser control container and tears down
      object.
      }
    property IOMgr: TWBIOMgr
      read fIOMgr;
      {Object that controls web browser's IO}
    property UIMgr: TWBUIMgr
      read fUIMgr implements IDocHostUIHandler;
      {Object that customises UI and implement's object IDocHostUIHandler
      interface}
    { IOleClientSite method}
    property OleClientSite: TOleClientSite
      read fOleClientSite implements IOleClientSite;
      {References aggregated object implementing IOleClientSite interface}
  end;


implementation


uses
  // Delphi
  SysUtils,
  // Project
  UExceptions;


{ TWBController }

constructor TWBController.Create(const WebBrowser: TWebBrowser);
  {Object constructor. Sets up object as container for a browser control.
    @param WebBrowser Contained browser control.
  }
begin
  Assert(Assigned(WebBrowser), ClassName + '.Create: WebBrowser is nil');
  inherited Create;
  fWebBrowser := WebBrowser;
  fIOMgr := TWBIOMgr.Create(WebBrowser);
  fUIMgr := TWBUIMgr.Create(WebBrowser, Self);
  fOleClientSite := TOleClientSite.Create(Self);
  SetBrowserOleClientSite(Self);  // register as browser's OLE container
end;

destructor TWBController.Destroy;
  {Object destructor. Unregisters browser control container and tears down
  object.
  }
begin
  SetBrowserOleClientSite(nil);
  FreeAndNil(fOleClientSite);   // unregister as browser's OLE container
  FreeAndNil(fUIMgr);
  FreeAndNil(fIOMgr);
  inherited;
end;

procedure TWBController.SetBrowserOleClientSite(
  const Site: IOleClientSite);
  {Registers this object as web browser's OLE container.
    @param Reference to object providing OLE container. May be Self to register
      this object as OLE container or nil to unregister.
  }
var
  OleObj: IOleObject; // web browser's IOleObject interface
begin
  // Get web browser's IOleObject interface
  if not Supports(
    fWebBrowser.DefaultInterface, IOleObject, OleObj
  ) then
    raise EBug.Create(
      ClassName + '.SetBrowserOleClientSite: '
      + 'Browser''s Default interface does not support IOleObject'
    );
  // Register's given client site as web browser's OLE container
  OleObj.SetClientSite(Site);
end;


initialization

// Set up OLE
OleInitialize(nil);

finalization

// Finalise OLE
OleUninitialize;

end.


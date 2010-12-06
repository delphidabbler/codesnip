{
 * UWBPopupMenus.pas
 *
 * Classes that manage and display popup menus associated with web browser
 * controls.
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
 * The Original Code is UWBPopupMenus.pas
 *
 * The Initial Developer of the Original Code is Peter Johnson
 * (http://www.delphidabbler.com/).
 *
 * Portions created by the Initial Developer are Copyright (C) 2007-2010 Peter
 * Johnson. All Rights Reserved.
 *
 * Contributor(s)
 *   NONE
 *
 * ***** END LICENSE BLOCK *****
}


unit UWBPopupMenus;


interface


uses
  // Delphi
  Classes, Generics.Collections, Types, Menus,
  // Project
  UCommandBars;


type
  {
  TWBPopupMenuQueryPopupEvent:
    Type of event handler for TWBPopupMenu.OnQueryPopup event.
      @param Sender [in] Object triggering event.
      @param Cancel [in/out] Whether to cancel menu popup. Cancel is always
        False when called. Handler can set Cancel to true to cancel popup.
  }
  TWBPopupMenuQueryPopupEvent = procedure(Sender: TObject; var Cancel: Boolean)
    of object;

  {
  TWBPopupMenu:
    Class that overrides popup menu. Designed for triggering from a web browser
    control. Records HTML element under mouse cursor when menu triggered. Also
    provides an event that enables popup to be cancelled.
  }
  TWBPopupMenu = class(TPopupMenu)
  strict private
    fHTMLElem: IDispatch;                       // Value of HTMLElem property
    fOnQueryPopup: TWBPopupMenuQueryPopupEvent; // OnQueryPopup event handler
  public
    procedure Popup(X, Y: Integer); override;
      {Override of method that displays popup menu. Triggers OnQueryPopup event
      and cancels display of menu if event handler cancels the popup.
        @param X [in] X coordinate of mouse cursor when menu triggered.
        @param Y [in] Y coordinate of mouse cursor when menu triggered.
      }
    property HTMLElem: IDispatch read fHTMLElem write fHTMLElem;
      {HTML element under mouse cursor when menu activated}
    property OnQueryPopup: TWBPopupMenuQueryPopupEvent
      read fOnQueryPopup write fOnQueryPopup;
      {Event triggered before menu pops up. Handler can prevent display of menu
      by setting event handler's Cancel parameter to true}
  end;

  {
  TWBPopupMenuMgr:
    Class used to manage the various popup menus associated with a web browser
    control. Associates popup menus with various menu command ids supported by
    web browser control.
  }
  TWBPopupMenuMgr = class(TComponent)
  strict private
    type
      // Map of command bar ids to popup menu components
      TMenuMap = TDictionary<TCommandBarID, TWBPopupMenu>;
    var fMenus: TMenuMap; // Maps menu ids to popup menu objects
    function GetMenu(const ID: TCommandBarID): TWBPopupMenu;
      {Gets reference to menu associated with a command ID.
        @param ID [in] ID of required menu.
        @return Reference to required menu or nil if ID is not found.
      }
  public
    constructor Create(AOwner: TComponent); override;
      {Constructor. Sets up object.
        @param AOwner [in] Component that owns this object.
      }
    destructor Destroy; override;
      {Destructor. Tears down object.
      }
    function AddMenu(const ID: TCommandBarID): TWBPopupMenu;
      {Creates a new menu, identifies it with an ID and adds it to the menu
      list.
        @param ID [in] ID of new menu.
        @return Reference to new menu.
      }
    procedure Popup(const ID: TCommandBarID; const Pt: TPoint;
      const Elem: IDispatch);
      {Pops up menu of required kind for a specified HTML element.
        @param ID [in] ID of menu to be displayed.
        @param Pt [in] Point on screen where menu to be displayed.
        @param Elem [in] HTML Element under mouse cursor when menu display
          requested.
      }
  end;

  {
  TWBTempMenuItem:
    Custom menu item type used for temporary menu items added to menus by
    TWBDefaultPopupMenuWrapper. Associated action is freed when menu item is
    freed.
  }
  TWBTempMenuItem = class(TMenuItem)
  public
    destructor Destroy; override;
      {Destructor. Tears down object.
      }
  end;


implementation


{ TWBPopupMenu }

procedure TWBPopupMenu.Popup(X, Y: Integer);
  {Override of method that displays popup menu. Triggers OnQueryPopup event and
  cancels display of menu if event handler cancels the popup.
    @param X [in] X coordinate of mouse cursor when menu triggered.
    @param Y [in] Y coordinate of mouse cursor when menu triggered.
  }
var
  Cancel: Boolean;  // flag indicates whether to cancel popup
begin
  Cancel := False;
  if Assigned(fOnQueryPopup) then
    fOnQueryPopup(Self, Cancel);
  if not Cancel then
    inherited;
end;

{ TWBPopupMenuMgr }

function TWBPopupMenuMgr.AddMenu(const ID: TCommandBarID): TWBPopupMenu;
  {Creates a new menu, identifies it with an ID and adds it to the menu list.
    @param ID [in] ID of new menu.
    @return Reference to new menu.
  }
begin
  Assert(not fMenus.ContainsKey(ID), ClassName + '.AddMenu: ID already exists');
  // new menu item created here gets freed automatically when menu is destoyed
  Result := TWBPopupMenu.Create(Self);
  fMenus.Add(ID, Result);
end;

constructor TWBPopupMenuMgr.Create(AOwner: TComponent);
  {Constructor. Sets up object.
    @param AOwner [in] Component that owns this object.
  }
begin
  inherited;
  // use default integer comparison and hash for TCommandBarID key in map
  fMenus := TMenuMap.Create;
end;

destructor TWBPopupMenuMgr.Destroy;
  {Destructor. Tears down object.
  }
begin
  fMenus.Free;
  inherited;
end;

function TWBPopupMenuMgr.GetMenu(const ID: TCommandBarID): TWBPopupMenu;
  {Gets reference to menu associated with a command ID.
    @param ID [in] ID of required menu.
    @return Reference to required menu or nil if ID is not found.
  }
begin
  Result := fMenus[ID];
end;

procedure TWBPopupMenuMgr.Popup(const ID: TCommandBarID; const Pt: TPoint;
  const Elem: IDispatch);
  {Pops up menu of required kind for a specified HTML element.
    @param ID [in] ID of menu to be displayed.
    @param Pt [in] Point on screen where menu to be displayed.
    @param Elem [in] HTML Element under mouse cursor when menu display
      requested.
  }
var
  Menu: TWBPopupMenu; // menu to popup
begin
  Menu := GetMenu(ID);
  if Assigned(Menu) then
  begin
    Menu.HTMLElem := Elem;
    Menu.Popup(Pt.X, Pt.Y);
  end;
end;

{ TWBTempMenuItem }

destructor TWBTempMenuItem.Destroy;
  {Destructor. Tears down object.
  }
begin
  Action.Free;    // free the associated action
  inherited;
end;

end.


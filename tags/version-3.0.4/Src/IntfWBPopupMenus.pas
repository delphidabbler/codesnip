{
 * IntfWBPopupMenus.pas
 *
 * Defines interfaces and enumerations that support management and display of
 * popup menus over web browser controls.
 *
 * v1.0 of 01 Nov 2007  - Original version.
 *
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
 * The Original Code is IntfWBPopupMenus.pas
 *
 * The Initial Developer of the Original Code is Peter Johnson
 * (http://www.delphidabbler.com/).
 *
 * Portions created by the Initial Developer are Copyright (C) 2007 Peter
 * Johnson. All Rights Reserved.
 *
 * Contributor(s): None
 *
 * ***** END LICENSE BLOCK *****
}


unit IntfWBPopupMenus;


interface


uses
  // Delphi
  ActnList, Controls, Windows,
  // Project
  IntfUIHandlers;


type

  {
  TWBPopupMenuKind:
    Enumeration that identifies various menu type that can be displayed by web
    browser control. Kind depends on HTML element where mouse is clicked.
  }
  TWBPopupMenuKind = (
    pmkDefault    = CONTEXT_MENU_DEFAULT,     // default menu
    pmkImage      = CONTEXT_MENU_IMAGE,       // mouse over image control
    pmkControl    = CONTEXT_MENU_CONTROL,     // mouse over HTML control
    pmkTable      = CONTEXT_MENU_TABLE,       // mouse over HTML table
    pmkTextSelect = CONTEXT_MENU_TEXTSELECT,  // mouse over selected table
    pmkAnchor     = CONTEXT_MENU_ANCHOR,      // mouse over HTML anchor
    pmkUnknown    = CONTEXT_MENU_UNKNOWN      // unknown menu
  );

  {
  IWBPopupMenuConfig:
    Interface to object that can configure browser pop-up menus.
  }
  IWBPopupMenuConfig = interface(IInterface)
    ['{D1DAAA99-44B3-4F5E-A12E-7D2956ED6E4E}']
    procedure AddAction(const Action: TCustomAction;
      const Kind: TWBPopupMenuKind);
      {Adds a menu item with an associated action to a popup menu.
        @param Action [in] Action to be associated with menu item.
        @param Kind [in] Specifies menu to add menu item to.
      }
    procedure AddSpacer(const Kind: TWBPopupMenuKind);
      {Adds a spacer to a pop-up menu.
        @param Kind [in] Specifies menu to add spacer to.
      }
    procedure SetImages(const Images: TImageList);
      {Sets image list to be used by menus.
        @param Images [in] Image list to be used.
      }
  end;

  {
  IWBPopupMenus:
    Interface to object that can pop up a suitable menu over web browser
    control.
  }
  IWBPopupMenus = interface(IInterface)
    ['{9082306C-8585-48FA-8F4B-A85AA08006D7}']
    function Popup(const Pt: TPoint; const Kind: TWBPopupMenuKind;
      const Elem: IDispatch): Boolean;
      {Pops up menu of required kind for a specified HTML element.
        @param Pt [in] Point on screen where menu to be displayed.
        @param Kind [in] Kind of menu to be displayed.
        @param Elem [in] Element under mouse cursor when menu display requested.
      }
  end;

implementation

end.


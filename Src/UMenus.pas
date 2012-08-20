{
 * UMenus.pas
 *
 * Helper routines and interface for working with menus.
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
 * The Original Code is UMenus.pas, formerly UMenuHelper.pas.
 *
 * The Initial Developer of the Original Code is Peter Johnson
 * (http://www.delphidabbler.com/).
 *
 * Portions created by the Initial Developer are Copyright (C) 2009-2012 Peter
 * Johnson. All Rights Reserved.
 *
 * Contributor(s)
 *   NONE
 *
 * ***** END LICENSE BLOCK *****
}


unit UMenus;


interface


uses
  // Delphi
  Classes, Types, Menus;


type
  ///  <summary>Interface that defines operations to be supported by objects
  ///  that can display a pop-up menu by means other than a menu associated with
  ///  a component's PopupMenu property.</summary>
  IPopupMenu = interface(IInterface)
    ['{A01D903C-90B9-4D24-8B0C-D770852C6340}']
    ///  <summary>Inform if a pop-up menu is available.</summary>
    function HasPopup: Boolean;
    ///  <summary>Display pop-up menu at given point on screen.</summary>
    procedure Popup(const Pt: TPoint);
  end;

type
  ///  <summary>Class reference to a TMenuItem.</summary>
  TMenuItemClass = class of TMenuItem;

///  <summary>Creates menu item that has an associated action.</summary>
///  <param name="AOwner">TComponent [in] Menu item owner.</param>
///  <param name="Cls">TMenuItemClass [in] Class of menu item to be created.
///  </param>
///  <param name="Action">TBasicAction [in] Action to be associated with menu
///  item.</param>
///  <returns>TMenuItem. New menu item object.</returns>
function CreateMenuItem(AOwner: TComponent; const Cls: TMenuItemClass;
  const Action: TBasicAction): TMenuItem; overload;

///  <summary>Create a menu item that acts as a menu spacer.</summary>
///  <param name="AOwner">TComponent [in] Menu item owner.</param>
///  <param name="Cls">TMenuItemClass [in] Class of menu item to be created.
///  </param>
///  <returns>TMenuItem. New menu item spacer object.</returns>
function CreateMenuSpacer(AOwner: TComponent;
  const Cls: TMenuItemClass): TMenuItem; overload;


implementation


///  <summary>Creates a menu item object.</summary>
///  <param name="AOwner">TComponent [in] Menu item owner.</param>
///  <param name="Cls">TMenuItemClass [in] Class of menu item to be created.
///  </param>
///  <returns>TMenuItem. New menu item object.</returns>
function CreateMenuItem(AOwner: TComponent;
  const Cls: TMenuItemClass): TMenuItem; overload;
begin
  Result := Cls.Create(AOwner);
end;

function CreateMenuItem(AOwner: TComponent; const Cls: TMenuItemClass;
  const Action: TBasicAction): TMenuItem; overload;
begin
  Result := CreateMenuItem(AOwner, Cls);
  Result.Action := Action;
end;

function CreateMenuSpacer(AOwner: TComponent;
  const Cls: TMenuItemClass): TMenuItem; overload;
begin
  Result := CreateMenuItem(AOwner, Cls);
  Result.Caption := '-';
end;

end.


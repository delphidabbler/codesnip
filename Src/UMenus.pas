{
 * This Source Code Form is subject to the terms of the Mozilla Public License,
 * v. 2.0. If a copy of the MPL was not distributed with this file, You can
 * obtain one at https://mozilla.org/MPL/2.0/
 *
 * Copyright (C) 2009-2020, Peter Johnson (gravatar.com/delphidabbler).
 *
 * Helper routines and interface for working with menus.
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


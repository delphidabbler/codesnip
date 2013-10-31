{
 * UMenuHelper.pas
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
 * The Original Code is UMenuHelper.pas
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


unit UMenuHelper;


interface


uses
  // Delphi
  Classes, Types, Menus;


type
  ///  <summary>Interface supported by objects that can display a pop-up menu
  ///  by means other than a menu associated with a PopupMenu
  ///  property.</summary>
  IPopupMenu = interface(IInterface)
    ['{A01D903C-90B9-4D24-8B0C-D770852C6340}']
    ///  <summary>Informs if the object has a pop-up menu available.</summary>
    function HasPopup: Boolean;
    ///  <summary>Displays pop-up menu at given point on screen.</summary>
    procedure Popup(const Pt: TPoint);
  end;

type
  {TMenuItemClass:
    Class reference to TMenuItem and descendants.
  }
  TMenuItemClass = class of TMenuItem;

function CreateMenuItem(AOwner: TComponent; const Cls: TMenuItemClass;
  const Action: TBasicAction): TMenuItem; overload;
  {Creates a menu item of a specified subclass with an associated action.
    @param AOwner [in] Menu item owner.
    @param Cls [in] Type of menu item to create.
    @param Action [in] Required action.
    @return New menu item instance.
  }

function CreateMenuSpacer(AOwner: TComponent;
  const Cls: TMenuItemClass): TMenuItem; overload;
  {Creates a menu spacer of a specified subclass.
    @param AOwner [in] Menu spacer owner.
    @param Cls [in] Type of menu spacer to create.
    @return New menu spacer instance.
  }


implementation


function CreateMenuItem(AOwner: TComponent;
  const Cls: TMenuItemClass): TMenuItem; overload;
  {Creates a menu item of a specified subclass.
    @param AOwner [in] Menu item owner.
    @param Cls [in] Type of menu item to create.
    @return New menu item instance.
  }
begin
  Result := Cls.Create(AOwner);
end;

function CreateMenuItem(AOwner: TComponent; const Cls: TMenuItemClass;
  const Action: TBasicAction): TMenuItem; overload;
  {Creates a menu item of a specified subclass with an associated action.
    @param AOwner [in] Menu item owner.
    @param Cls [in] Type of menu item to create.
    @param Action [in] Required action.
    @return New menu item instance.
  }
begin
  Result := CreateMenuItem(AOwner, Cls);
  Result.Action := Action;
end;

function CreateMenuSpacer(AOwner: TComponent;
  const Cls: TMenuItemClass): TMenuItem; overload;
  {Creates a menu spacer of a specified subclass.
    @param AOwner [in] Menu spacer owner.
    @param Cls [in] Type of menu spacer to create.
    @return New menu spacer instance.
  }
begin
  Result := CreateMenuItem(AOwner, Cls);
  Result.Caption := '-';
end;

end.

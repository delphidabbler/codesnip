{
 * UHistoryMenus.pas
 *
 * Custom popup menu components that display menu items for recent view items in
 * the history. Separate menu classes deal with "forward" and "back" menus.
 *
 * v0.1 of 30 Jan 2005  - Original version.
 * v0.2 of 18 Feb 2005  - Removed debug code.
 * v0.3 of 18 Feb 2005  - Deleted unused THistoryMenu destructor.
 *                      - Deleted unused THistoryMenuItem constructor and
 *                        destructor.
 * v0.4 of 19 Feb 2005  - Refactoring: moved code that sets history menu item
 *                        caption to describe associated view item into custom
 *                        menu item class.
 * v0.5 of 22 Feb 2005  - Localised literal strings.
 *                      - Moved constant determining max number of menu items to
 *                        UGlobals unit.
 * v1.0 of 24 May 2006  - Improved and corrected comments.
 * v1.1 of 11 Jan 2009  - cMaxHistoryMenuItems const added to private section of
 *                        THistoryMenu, moved back here from UGlobals.
 *                      - Flagged THistoryMenu as abstract and TBackHistoryMenu,
 *                        TForwardHistoryMenu and THistoryMenuItem as sealed.
 *                      - Made private and protected visibility sections strict.
 *                      - Made assertions and buf exceptions use ClassName to
 *                        report name of class.
 * v1.2 of 19 Jun 2009  - Removed support for uncategorised view items.
 *                      - Added support for alphabetic and snippet kind view
 *                        items.
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
 * The Original Code is UHistoryMenus.pas
 *
 * The Initial Developer of the Original Code is Peter Johnson
 * (http://www.delphidabbler.com/).
 *
 * Portions created by the Initial Developer are Copyright (C) 2005-2009 Peter
 * Johnson. All Rights Reserved.
 *
 * ***** END LICENSE BLOCK *****
}


unit UHistoryMenus;


interface


uses
  // Delphi
  Classes, Menus,
  // Project
  UHistory, UView, UViewItemAction;


type

  {
  THistoryMenu:
    Abstract base class for popup menu components that display menu items for
    recent view items in the history.
  }
  THistoryMenu = class abstract(TPopupMenu)
  strict private
    var
      fHistory: THistory;         // Reference to menu's history list
      fAction: TViewItemAction;   // Action triggered when menu items clicked
    const
      cMaxHistoryMenuItems = 10;  // max items displayed in a history menu
  strict protected
    procedure PopulateMenu;
      {Builds menu with a menu item for each view item in history.
      }
    procedure MenuItemClick(Sender: TObject);
      {Handles OnClick events for all menu items by triggering view item action
      for view item associated with menu item.
        @param Sender [in] Menu item that was clicked.
      }
    procedure GetViewItemList(const List: TViewItemList); virtual; abstract;
      {Returns list of view items to be displayed in menu, subject to maximum
      menu size limit. Descendant classes overrride this method to get
      appropriate view items.
        @param List [in] Receives view item list.
      }
    procedure DoPopup(Sender: TObject); override;
      {Populates menu with required items just before it is displayed.
        @param Sender [in] Not used.
      }
    property History: THistory read fHistory;
      {Reference to history list associated with menu}
  public
    constructor Create(AOwner: TComponent; const History: THistory;
      const Action: TViewItemAction); reintroduce;
      {Class constructor. Sets up menu for a history list.
        @param AOwner [in[ Component that owns this menu.
        @param History [in] Reference to history list that stores view items
          displayed in this menu.
        @param Action [in] Action triggered when a menu item is selected.
      }
  end;

  {
  TBackHistoryMenu:
    Popup menu component that displays menu items for recent view items in the
    "back" history - i.e. view items that preceed the currently selected history
    item.
  }
  TBackHistoryMenu = class sealed(THistoryMenu)
  strict protected
    procedure GetViewItemList(const List: TViewItemList); override;
      {Gets list of view items in "back" history.
        @param List [in] Receives relevant view items.
      }
  end;

  {
  TForwardHistoryMenu:
    Popup menu component that displays menu items for recent view items in the
    "forward" history - i.e. view items that follow the currently selected
    history item.
  }
  TForwardHistoryMenu = class sealed(THistoryMenu)
  strict protected
    procedure GetViewItemList(const List: TViewItemList); override;
      {Gets list of view items in "forward" history.
        @param List [in] Receives relevant view items.
      }
  end;


implementation


uses
  // Delphi
  SysUtils,
  // Project
  UExceptions;


type

  {
  THistoryMenuItem:
    Menu item class that extends TMenuItem by adding a property that references
    a TViewItem object.
  }
  THistoryMenuItem = class sealed(TMenuItem)
  strict private
    fViewItem: TViewItem;
      {View item associated with menu item}
    procedure SetViewItem(const Value: TViewItem);
      {Write accessor for ViewItem property. Sets menu item Caption property
      to represent view item.
        @param Value [in] View item associated with menu.
      }
  public
    property ViewItem: TViewItem
      read fViewItem write SetViewItem;
      {View item associated with this menu item. Setting this property also
      stores a description in menu item Caption}
  end;

{ THistoryMenu }

constructor THistoryMenu.Create(AOwner: TComponent;
  const History: THistory; const Action: TViewItemAction);
  {Class constructor. Sets up menu for a history list.
    @param AOwner [in[ Component that owns this menu.
    @param History [in] Reference to history list that stores view items
      displayed in this menu.
    @param Action [in] Action triggered when a menu item is selected.
  }
begin
  Assert(Assigned(History), ClassName + '.Create: History is nil');
  Assert(Assigned(Action), ClassName + '.Create: Action is nil');
  inherited Create(AOwner);
  // Record history list and action
  fHistory := History;
  fAction := Action;
end;

procedure THistoryMenu.DoPopup(Sender: TObject);
  {Populates menu with required items just before it is displayed.
    @param Sender [in] Not used.
  }
begin
  PopulateMenu;
  inherited;  // triggers menu's OnPopup event
end;

procedure THistoryMenu.MenuItemClick(Sender: TObject);
  {Handles OnClick events for all menu items by triggering view item action for
  view item associated with menu item.
    @param Sender [in] Menu item that was clicked.
  }
begin
  fAction.ViewItem := (Sender as THistoryMenuItem).ViewItem;
  fAction.Execute;
end;

procedure THistoryMenu.PopulateMenu;
  {Builds menu with a menu item for each view item in history.
  }
var
  ViewList: TViewItemList;  // list of view items in required section of history
  MI: THistoryMenuItem;     // a menu item instance
  Idx: Integer;             // loops thru ViewList
begin
  // Remove any existing menu items (this frees them)
  Items.Clear;
  // Get a list of view items to be displayed in menu
  ViewList := TViewItemList.Create;
  try
    GetViewItemList(ViewList);
    // Loop thru view items, up to maximum that can be displayed
    for Idx := 0 to Pred(ViewList.Count) do
    begin
      if Idx = cMaxHistoryMenuItems then
        Break;
      // Create menu item
      MI := THistoryMenuItem.Create(Self);         // use custom menu item class
      MI.ViewItem := ViewList[Idx];      // record view item (also sets Caption)
      MI.OnClick := MenuItemClick;       // every item uses same OnClick handler
      MI.Default := (Idx = 0);                     // first menu item is default
      // Add item to menu
      Items.Add(MI);
    end;
  finally
    FreeAndNil(ViewList);
  end;
end;

{ TBackHistoryMenu }

procedure TBackHistoryMenu.GetViewItemList(const List: TViewItemList);
  {Gets list of view items in "back" history.
    @param List [in] Receives relevant view items.
  }
begin
  History.BackList(List);
end;

{ TForwardHistoryMenu }

procedure TForwardHistoryMenu.GetViewItemList(const List: TViewItemList);
  {Gets list of view items in "forward" history.
    @param List [in] Receives relevant view items.
  }
begin
  History.ForwardList(List);
end;

{ THistoryMenuItem }

procedure THistoryMenuItem.SetViewItem(const Value: TViewItem);
  {Write accessor for ViewItem property. Sets menu item Caption property to
  represent view item.
    @param Value [in] View item associated with menu.
  }
resourcestring
  // Menu caption templates
  sRoutineDesc = 'Routine: %s';
  sCategoryDesc = 'Category: %s';
  sSnipKindDesc = 'Snippets type: %s';
  sAlphabetDesc = 'Alphabetic section: %s';
const
  cBadViewItem = '%s.SetViewItem: unknown view item kind';
begin
  // Record view item
  fViewItem := Value;
  // Set menu item caption to describe view item
  case fViewItem.Kind of
    vkWelcome:
      Caption := ViewItem.Description;
    vkRoutine:
      Caption := Format(sRoutineDesc, [ViewItem.Description]);
    vkCategory:
      Caption := Format(sCategoryDesc, [ViewItem.Description]);
    vkSnipKind:
      Caption := Format(sSnipKindDesc, [ViewItem.Description]);
    vkAlphabet:
      Caption := Format(sAlphabetDesc, [ViewItem.Description]);
    else
      raise EBug.CreateFmt(cBadViewItem, [cBadViewItem]);
  end;
end;

end.


{
 * This Source Code Form is subject to the terms of the Mozilla Public License,
 * v. 2.0. If a copy of the MPL was not distributed with this file, You can
 * obtain one at http://mozilla.org/MPL/2.0/
 *
 * Copyright (C) 2005-2012, Peter Johnson (www.delphidabbler.com).
 *
 * $Rev$
 * $Date$
 *
 * Custom popup menu components that display menu items for recent view items in
 * the history. Separate menu classes deal with "forward" and "back" menus.
}


unit UHistoryMenus;


interface


uses
  // Delphi
  Classes, Menus,
  // Project
  UHistory, UView, UViewItemAction;


type
  ///  <summary>
  ///  Abstract base class for popup menu components that display menu items for
  ///  recent view items in the history.
  ///  </summary>
  THistoryMenu = class abstract(TPopupMenu)
  strict private
    var
      ///  <summary>Reference to menu's history list.</summary>
      fHistory: THistory;
      ///  <summary>Action triggered when menu items clicked.</summary>
      fAction: TViewItemAction;
    const
      ///  <summary>Max items displayed in a history menu.</summary>
      cMaxHistoryMenuItems = 10;
  strict protected
    ///  <summary>Builds menu with a menu item for each view item in history.
    ///  </summary>
    procedure PopulateMenu;
    ///  <summary>Handles OnClick events for all menu items in menu by
    ///  executing action for view item associated with menu item.</summary>
    procedure MenuItemClick(Sender: TObject);
    ///  <summary>Builds list of views to be displayed in menu.</summary>
    ///  <param name="List">TViewItemList [in] List that receives views.
    ///  </param>
    procedure GetViewItemList(const List: TViewList); virtual; abstract;
    ///  <summary>Populates menu with required items just before it is
    ///  displayed.</summary>
    procedure DoPopup(Sender: TObject); override;
    ///  <summary>Reference to history list associated with menu.</summary>
    property History: THistory read fHistory;
  public
    ///  <summary>Object constructor. Sets up menu for a history list.</summary>
    ///  <param name="AOwner">TComponent [in] Component that owns this one, or
    ///  nil if not owned.</param>
    ///  <param name="History">THistory [in] Reference to history list
    ///  containing view items displayed in this menu.</param>
    ///  <param name="Action">TViewItemAction [in] Action to be triggered when a
    ///  menu item is selected.</param>
    constructor Create(AOwner: TComponent; const History: THistory;
      const Action: TViewItemAction); reintroduce;
  end;

type
  ///  <summary>
  ///  Popup menu component that displays menu items for recent view items in
  ///  the "back" history - i.e. view items that preceed the currently selected
  ///  history item.
  ///  </summary>
  TBackHistoryMenu = class sealed(THistoryMenu)
  strict protected
    ///  <summary>Builds list of views in "back" history.</summary>
    ///  <param name="List">TViewItemList [in] List that receives views.</param>
    procedure GetViewItemList(const List: TViewList); override;
  end;

type
  ///  <summary>
  ///  Popup menu component that displays menu items for recent view items in
  ///  the "forward" history - i.e. view items that follow the currently
  ///  selected history item.
  ///  </summary>
  TForwardHistoryMenu = class sealed(THistoryMenu)
  strict protected
    ///  <summary>Builds list of views in "forward" history.</summary>
    ///  <param name="List">TViewItemList [in] List that receives views.</param>
    procedure GetViewItemList(const List: TViewList); override;
  end;


implementation


uses
  // Delphi
  SysUtils,
  // Project
  UExceptions, UKeysHelper;


type
  ///  <summary>
  ///  Extension of TMenuItem that adds a property that references a TViewItem
  ///  object.
  ///  </summary>
  THistoryMenuItem = class sealed(TMenuItem)
  strict private
    var
      ///  <summary>View item associated with menu item.</summary>
      fViewItem: IView;
    ///  <summary>Write accessor for ViewItem property.</summary>
    procedure SetViewItem(Value: IView);
  public
    ///  <summary>View item associated with this menu item.</summary>
    ///  <remarks>Setting this property also stores a description in menu item
    ///  Caption.</remarks>
    property ViewItem: IView read fViewItem write SetViewItem;
  end;


{ THistoryMenu }

constructor THistoryMenu.Create(AOwner: TComponent;
  const History: THistory; const Action: TViewItemAction);
begin
  Assert(Assigned(History), ClassName + '.Create: History is nil');
  Assert(Assigned(Action), ClassName + '.Create: Action is nil');
  inherited Create(AOwner);
  // Record history list and action
  fHistory := History;
  fAction := Action;
end;

procedure THistoryMenu.DoPopup(Sender: TObject);
begin
  PopulateMenu;
  inherited;  // triggers menu's OnPopup event
end;

procedure THistoryMenu.MenuItemClick(Sender: TObject);
begin
  fAction.ViewItem := (Sender as THistoryMenuItem).ViewItem;
  fAction.NewTab := ssCtrl in ShiftKeysPressed;
  fAction.Execute;
  fHistory.SelectItem(fAction.ViewItem);
end;

procedure THistoryMenu.PopulateMenu;
var
  ViewList: TViewList;      // list of views in required section of history
  MI: THistoryMenuItem;     // a menu item instance
  Idx: Integer;             // loops thru ViewList
begin
  // Remove any existing menu items (this frees them)
  Items.Clear;
  // Get a list of view items to be displayed in menu
  ViewList := TViewList.Create;
  try
    GetViewItemList(ViewList);
    // Loop thru views, up to maximum that can be displayed
    for Idx := 0 to Pred(ViewList.Count) do
    begin
      if Idx = cMaxHistoryMenuItems then
        Break;
      // Create menu item
      MI := THistoryMenuItem.Create(Self);         // use custom menu item class
      MI.ViewItem := ViewList[Idx];           // record view (also sets Caption)
      MI.OnClick := MenuItemClick;       // every item uses same OnClick handler
      MI.Default := (Idx = 0);                     // first menu item is default
      // Add item to menu
      Items.Add(MI);
    end;
  finally
    ViewList.Free;
  end;
end;

{ TBackHistoryMenu }

procedure TBackHistoryMenu.GetViewItemList(const List: TViewList);
begin
  History.BackList(List);
end;

{ TForwardHistoryMenu }

procedure TForwardHistoryMenu.GetViewItemList(const List: TViewList);
begin
  History.ForwardList(List);
end;

{ THistoryMenuItem }

procedure THistoryMenuItem.SetViewItem(Value: IView);
resourcestring
  // Menu caption templates
  sSnippetDesc = 'Snippet: %s';
  sCategoryDesc = 'Category: %s';
  sSnipKindDesc = 'Snippets type: %s';
  sAlphabetDesc = 'Alphabetic section: %s';
const
  cBadViewItem = '%s.SetViewItem: unknown or unsupported view item';
begin
  // Record view item
  fViewItem := Value;
  // Set menu item caption to describe view item
  if Supports(fViewItem, IStartPageView) then
    Caption := ViewItem.Description
  else if Supports(fViewItem, ISnippetView) then
    Caption := Format(sSnippetDesc, [ViewItem.Description])
  else if Supports(fViewItem, ICategoryView) then
    Caption := Format(sCategoryDesc, [ViewItem.Description])
  else if Supports(fViewItem, ISnippetKindView) then
    Caption := Format(sSnipKindDesc, [ViewItem.Description])
  else if Supports(fViewItem, IInitialLetterView) then
    Caption := Format(sAlphabetDesc, [ViewItem.Description])
  else
    raise EBug.CreateFmt(cBadViewItem, [ClassName]);
end;

end.


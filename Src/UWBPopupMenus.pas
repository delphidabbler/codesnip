{
 * UWBPopupMenus.pas
 *
 * Classes that manage, configure and display popup menu items for web browser
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
 * Portions created by the Initial Developer are Copyright (C) 2007-2009 Peter
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
  Windows, Menus, Controls, ActnList,
  // Project
  IntfWBPopupMenus, UBaseObjects;


type

  {
  TWBPopupMenus:
    Class that manages, configures and displays popup menus for we browser
    controls. Can act as an aggregated object that implements IWBPopupMenus and
    IWBPopupMenuConfig.
  }
  TWBPopupMenus = class(TAggregatedOrLoneObject,
    IWBPopupMenus, IWBPopupMenuConfig
  )
  strict private
    fMenus: array[TWBPopupMenuKind] of TPopupMenu;
      {Array of menus displayed for each kind of popup menu supported by browser
      control}
    function HasVisibleItems(const Kind: TWBPopupMenuKind): Boolean;
      {Checks if a menu has any visible menu items.
        @param Kind [in] Specifies menu to be checked.
        @return True if menu has visible items, False otherwise.
      }
  protected // must not be strict
    { IWBPopupMenus }
    function Popup(const Pt: TPoint; const Kind: TWBPopupMenuKind;
      const Elem: IDispatch): Boolean;
      {Pops up menu of required kind for a specified HTML element.
        @param Pt [in] Point on screen where menu to be displayed.
        @param Kind [in] Kind of menu to be displayed.
        @param Elem [in] Element under mouse cursor when menu display requested.
        @return True if menu has visible items and was displayed, False if no
          visible items and was not displayed.
      }
    { IWBPopupMenuConfig }
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
      {Specifies image list to be used by menus.
        @param Images [in] Image list to be used.
      }
  public
    constructor Create(const Controller: IInterface); override;
      {Class constructor. Creates either an aggregated or stand-alone object and
      sets up object.
        @param Controller [in] IInterface reference to containing object if
          aggregated or nil if not aggregated.
      }
    destructor Destroy; override;
      {Class destructor. Tears down object.
      }
  end;


implementation


uses
  // Delphi
  SysUtils, StrUtils,
  // Project
  UAnchors, UDispatchList, UGIFImageList, UHTMLDocHelper, UImageTags,
  ULinkAction;


var
  // Global image list used by menus
  pvtImages: TGIFImageList = nil;


type

  {
  TWBPopupMenuClass:
    Class reference for custom pop-up menu classes.
  }
  TWBPopupMenuClass = class of TAbstractWBPopupMenu;

  {
  TAbstractWBPopupMenu:
    Abstract base class for browser popup menus.
  }
  TAbstractWBPopupMenu = class abstract(TPopupMenu)
  public
    procedure Initialise(const Elem: IDispatch); virtual; abstract;
      {Initialises menu. To be called just before menu displayed. NOTE: Can't
      override DoPopup since a reference to the selected HTML element is
      required.
        @param Elem [in] IDispatch interface of HTML element under mouse cursor.
      }
  end;

  {
  TWBNulPopupMenu:
    Popup menu class that makes no changed to menu when initialised.
  }
  TWBNulPopupMenu = class(TAbstractWBPopupMenu)
  public
    procedure Initialise(const Elem: IDispatch); override;
      {Initialises menu. Does nothing.
        @param Elem [in] IDispatch interface of HTML element under mouse cursor.
          Not used.
      }
  end;

  {
  TWBPopupMenu:
    Class of popup menu that can be initialised before being displayed. Ensures
    that any menu items that trigger links are updated for selected link.
  }
  TWBPopupMenu = class(TAbstractWBPopupMenu)
  public
    procedure Initialise(const Elem: IDispatch); override;
      {Initialises menu. Stores selected HTML element in any link actions
      associated with menu items.
        @param Elem [in] IDispatch interface of HTML element under mouse cursor.
      }
  end;

  {
  TWBDefaultPopupMenu:
    Class of popup menu used for default popup menu kinds. Initialises menu to
    include item for all links in document that are designated menu items.
  }
  TWBDefaultPopupMenu = class(TWBPopupMenu)
  strict private
    procedure ClearTempMenuItems;
      {Clears temporary menu items from menu.
      }
    procedure GetLinkMenuItems(const Doc: IDispatch; out CommandItems,
      HelpItems: IDispatchList);
      {Gets all command and help links from document that are designated as menu
      items.
        @param Doc [in] IDispatch interface of document containing links.
        @param CommandItems [out] List of command links.
        @param HelpItems [out] List of help links.
      }
    procedure AddLinksToMenu(const Links: IDispatchList);
      {Adds menu items to menu that can trigger links from a link list.
        @param Links [in] List of links to be added to menu.
      }
    function GetImageIndex(const Link: IDispatch): Integer;
      {Gets index of any image associated with a link in image list used by
      menu. If image doesn't exist in list it is added to it.
        @param Link [in] Link for which image needed.
        @return Index of image in image or -1 if there is no associated image.
      }
  public
    procedure Initialise(const Elem: IDispatch); override;
      {Initialises menu. Adds items to menu for any command and help links in
      current HTML document.
        @param Elem [in] IDispatch interface of HTML element under mouse cursor.
      }
  end;

  {
  TWBMenuItemClass:
    Class reference to menu item classes used in popup menus.
  }
  TWBMenuItemClass = class of TMenuItem;

  {
  TWBTempMenuItem:
    Custom menu item type used for temporary menu items added to menus by
    TWBDefaultPopupMenu. Class type is used to identify this type of menu item
    for special processing. Associated action is freed when menu item is freed.
  }
  TWBTempMenuItem = class(TMenuItem)
  public
    destructor Destroy; override;
      {Class destructor. Tears down object.
      }
  end;


const
  // Map of menu kinds to classes implementing menu for each kind
  cPopupMenuClassMap: array[TWBPopupMenuKind] of TWBPopupMenuClass = (
    TWBDefaultPopupMenu,  // pmkDefault
    TWBPopupMenu,         // pmkImage
    TWBNulPopupMenu,      // pmkControl
    TWBNulPopupMenu,      // pmkTable
    TWBPopupMenu,         // pmkTextSelect
    TWBPopupMenu,         // pmkAnchor
    TWBNulPopupMenu       // pmkUnknown
  );


function CreateActionItem(const MIClass: TWBMenuItemClass;
  const Action: TCustomAction): TMenuItem;
  {Creates a menu item that triggers an action.
    @param MIClass [in] Class of required menu item.
    @param Action [in] Action to be associated with menu item.
    @return New menu item.
  }
begin
  Result := MIClass.Create(nil);
  Result.Action := Action;
end;

function CreateSpacer(const MIClass: TWBMenuItemClass): TMenuItem;
  {Creates a spacer menu item.
    @param MIClass [in] Class of required menu item.
    @return New spacer menu item.
  }
begin
  Result := MIClass.Create(nil);
  Result.Caption := '-';
end;

{ TWBPopupMenus }

procedure TWBPopupMenus.AddAction(const Action: TCustomAction;
  const Kind: TWBPopupMenuKind);
  {Adds a menu item with an associated action to a popup menu.
    @param Action [in] Action to be associated with menu item.
    @param Kind [in] Specifies menu to add menu item to.
  }
begin
  fMenus[Kind].Items.Add(CreateActionItem(TMenuItem, Action));
end;

procedure TWBPopupMenus.AddSpacer(const Kind: TWBPopupMenuKind);
  {Adds a spacer to a pop-up menu.
    @param Kind [in] Specifies menu to add spacer to.
  }
begin
  fMenus[Kind].Items.Add(CreateSpacer(TMenuItem));
end;

constructor TWBPopupMenus.Create(const Controller: IInterface);
  {Class constructor. Creates either an aggregated or stand-alone object and
  sets up object.
    @param Controller [in] IInterface reference to containing object if
      aggregated or nil if not aggregated.
  }
var
  MenuId: TWBPopupMenuKind; // loops through popup menu array
begin
  inherited;
  // Create all popup menus
  for MenuId := Low(TWBPopupMenuKind) to High(TWBPopupMenuKind) do
    fMenus[MenuId] := cPopupMenuClassMap[MenuId].Create(nil);
end;

destructor TWBPopupMenus.Destroy;
  {Class destructor. Tears down object.
  }
var
  MenuId: TWBPopupMenuKind; // loops through popup menu array
begin
  // Free all menus
  for MenuId := Low(TWBPopupMenuKind) to High(TWBPopupMenuKind) do
    FreeAndNil(fMenus[MenuId]);
  inherited;
end;

function TWBPopupMenus.HasVisibleItems(const Kind: TWBPopupMenuKind): Boolean;
  {Checks if a menu has any visible menu items.
    @param Kind [in] Specifies menu to be checked.
    @return True if menu has visible items, False otherwise.
  }
var
  VisibleCount: Integer;  // counts visible menu items
  MI: TMenuItem;          // references a menu item
begin
  VisibleCount := 0;
  for MI in fMenus[Kind].Items do
  begin
    // update menu item's action before checking for visibility
    if (MI.Action is TCustomAction) then
      (MI.Action as TCustomAction).Update;
    if MI.Visible then
      Inc(VisibleCount);
  end;
  Result := VisibleCount > 0;
end;

function TWBPopupMenus.Popup(const Pt: TPoint; const Kind: TWBPopupMenuKind;
  const Elem: IDispatch): Boolean;
  {Pops up menu of required kind for a specified HTML element.
    @param Pt [in] Point on screen where menu to be displayed.
    @param Kind [in] Kind of menu to be displayed.
    @param Elem [in] Element under mouse cursor when menu display requested.
    @return True if menu has visible items and was displayed, False if no
      visible items and was not displayed.
  }
begin
  (fMenus[Kind] as TWBPopupMenu).Initialise(Elem);
  Result := HasVisibleItems(Kind);
  if Result then
    fMenus[Kind].Popup(Pt.X, Pt.Y);
end;

procedure TWBPopupMenus.SetImages(const Images: TImageList);
  {Specifies image list to be used by menus.
    @param Images [in] Image list to be used.
  }
var
  MenuId: TWBPopupMenuKind; // loops through different menu kinds
begin
  // Reset internal image list to store only items from Images
  pvtImages.Clear;
  pvtImages.AddImages(Images);
  // Make each menu use the internal image list
  for MenuId := Low(TWBPopupMenuKind) to High(TWBPopupMenuKind) do
    fMenus[MenuId].Images := pvtImages;
end;


{ TWBNulPopupMenu }

procedure TWBNulPopupMenu.Initialise(const Elem: IDispatch);
  {Initialises menu. Does nothing.
    @param Elem [in] IDispatch interface of HTML element under mouse cursor. Not
      used.
  }
begin
  // Do nothing
end;

{ TWBPopupMenu }

procedure TWBPopupMenu.Initialise(const Elem: IDispatch);
  {Initialises menu. Stores selected HTML element in any link actions associated
  with menu items.
    @param Elem [in] IDispatch interface of HTML element under mouse cursor.
  }
var
  MI: TMenuItem;  // references a menu item
begin
  for MI in Items do
    if MI.Action is TLinkAction then
      // menu item associated with a link action: store reference to Elem in it
      (MI.Action as TLinkAction).Link := Elem;
end;

{ TWBDefaultPopupMenu }

procedure TWBDefaultPopupMenu.AddLinksToMenu(const Links: IDispatchList);
  {Adds menu items to menu that can trigger links from a link list.
    @param Links [in] List of links to be added to menu.
  }
var
  Action: TLinkAction;  // action to trigger a link
  Link: IDispatch;      // references all links in Links
begin
  // Do nothing if list is empty
  if Links.Count = 0 then
    Exit;
  // Start list with a spacer
  Items.Add(CreateSpacer(TWBTempMenuItem));
  // Add menu items
  for Link in Links do
  begin
    // Create action for menu item: store link reference and get caption from
    // link text. If link is not visible it is not added to menu.
    if THTMLDocHelper.ElemIsVisible(Link) then
    begin
      Action := TLinkAction.Create(nil);
      Action.Link := Link;
      Action.ImageIndex := GetImageIndex(Link);
      Action.Caption := TAnchors.GetInnerText(Link);
      Items.Add(CreateActionItem(TWBTempMenuItem, Action));
    end;
  end;
end;

procedure TWBDefaultPopupMenu.ClearTempMenuItems;
  {Clears temporary menu items from menu.
  }
var
  MI: TMenuItem;  // references each menu item in menu
begin
  for MI in Items do
    if MI is TWBTempMenuItem then
      MI.Free;    // only free temp menu items
end;

function TWBDefaultPopupMenu.GetImageIndex(const Link: IDispatch): Integer;
  {Gets index of any image associated with a link in image list used by menu.
  If image doesn't exist in list it is added to it.
    @param Link [in] Link for which image needed.
    @return Index of image in image or -1 if there is no associated image.
  }

  // ---------------------------------------------------------------------------
  function URLBaseName(const URL: string): string;
    {Extracts a base resource name from a URL.
      @param URL [in] URL containing resource name.
      @return Required base name.
    }
  var
    Pos: Integer; // position of last path delimiter in URL
  begin
    Pos := LastDelimiter('/', URL);
    if Pos > 0 then
      Result := AnsiRightStr(URL, Length(URL) - Pos)
    else
      Result := URL;
  end;
  // ---------------------------------------------------------------------------

var
  ParentDiv: IDispatch;     // parent <div> or <span> tag that contains Link
  ImgTags: IDispatchList;   // all <img> children of parent
  ImgTag: IDispatch;        // <img> child of parent that contains required GIF
  Src: string;              // resource URL of GIF file
begin
  Result := -1;
  // Check if parent elem is a <div> or <span> with class "option"
  ParentDiv := THTMLDocHelper.ParentElem(Link, 'div', 'option');
  if not Assigned(ParentDiv) then
    ParentDiv := THTMLDocHelper.ParentElem(Link, 'span', 'option');
  if not Assigned(ParentDiv) then
    Exit;
  // So see if there's an child <img> of parent with class "option-img"
  ImgTags := TImageTags.GetAllImageTags(ParentDiv);
  ImgTag := nil;
  for ImgTag in ImgTags do
    if THTMLDocHelper.ElemHasClass(ImgTag, 'option-img') then
      Break;
  if not Assigned(ImgTag) then
    Exit;
  // Get resource name of image from <img> tag's "src" attribute
  Src := URLBaseName(TImageTags.GetSrc(ImgTag));
  // Get matching bitmap from image list: add one from GIF file if not found
  Result := pvtImages.ImageIndex(Src);
  if Result = -1 then
    Result := pvtImages.AddGIFImage(Src);
end;

procedure TWBDefaultPopupMenu.GetLinkMenuItems(const Doc: IDispatch;
  out CommandItems, HelpItems: IDispatchList);
  {Gets all command and help links from document that are designated as menu
  items.
    @param Doc [in] IDispatch interface of document containing links.
    @param CommandItems [out] List of command links.
    @param HelpItems [out] List of help links.
  }
var
  AllLinks: IDispatchList;  // list of all links in document
  Link: IDispatch;          // referenced each link in AllLinks
begin
  CommandItems := TDispatchList.Create;
  HelpItems := TDispatchList.Create;
  // Get all links from document
  AllLinks := TAnchors.GetAllAnchors(Doc);
  // Scan all links
  for Link in AllLinks do
  begin
    // To have a link on menu it must have 'menu-item' class
    if THTMLDocHelper.ElemHasClass(Link, 'menu-item') then
    begin
      case TAnchors.AnchorKind(Link) of
        akCommand: CommandItems.Add(Link);
        akHelp: HelpItems.Add(Link);
      end;
    end;
  end;
end;

procedure TWBDefaultPopupMenu.Initialise(const Elem: IDispatch);
  {Initialises menu. Adds items to menu for any command and help links in
  current HTML document.
    @param Elem [in] IDispatch interface of HTML element under mouse cursor.
  }
var
  CommandLinks: IDispatchList;  // list of command links in document
  HelpLinks: IDispatchList;     // list of help links in document
begin
  // Removes any pre-existing menu items from menu
  ClearTempMenuItems;
  inherited;
  // Get list of command and help links from current document
  GetLinkMenuItems(
    THTMLDocHelper.DocumentFromElem(Elem), CommandLinks, HelpLinks
  );
  // Add the required menu items to the menu
  AddLinksToMenu(CommandLinks);
  AddLinksToMenu(HelpLinks);
end;

{ TWBTempMenuItem }

destructor TWBTempMenuItem.Destroy;
  {Class destructor. Tears down object.
  }
begin
  Action.Free;    // free the associated action
  inherited;
end;

initialization

// Create private image list
pvtImages := TGIFImageList.Create(nil);


finalization

// Dispose of private image list
FreeAndNil(pvtImages);

end.


{
 * UWBCommandBars.pas
 *
 * Defines various classes used to configure one or more command bars owned by
 * a web browser container. Command bars are UI elements used to issue commands,
 * e.g. menus & toolbars.
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
 * The Original Code is UWBCommandBars.pas
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


unit UWBCommandBars;


interface


uses
  // Delphi
  ImgList, Menus,
  // Project
  UCommandBars, UDispatchList, UGIFImageList;


type
  {
  TWBCommandBarMgr:
    Command bar manager for use with command bars associated with web browser
    controls. Uses a fixed image list.
  }
  TWBCommandBarMgr = class(TCommandBarMgr, ICommandBarConfig)
  strict private
    class var fImages: TGIFImageList; // Static image list
    class var fGC: IInterface;        // Garbage collector for staic image list
  strict protected
    class function GetImages: TGIFImageList;
      {Gets reference to static image list, creating it if necessary.
        @return Reference to image list.
      }
    procedure SetImages(const Images: TCustomImageList); override;
      {Specifies image list to be used by all command bars.
        @param Images [in] Image list to be used.
      }
  end;

  {
  TWBPopupMenuWrapper:
    Wrapper for popup menus associated with a web browser control. Records
    reference HTML element under mouse cursor and prevents display of menu if
    there's nothing to display.
  }
  TWBPopupMenuWrapper = class(TPopupMenuWrapper)
  strict private
    function HasVisibleItems: Boolean;
      {Checks if menu has any visible non-spacer menu items with captions.
      Assumes there are no sub-menus.
        @return True if menu has displayable items, False if not.
      }
    procedure QueryPopupHandler(Sender: TObject; var Cancel: Boolean);
      {Handles wrapped menu's OnPopupQuery event. Prevents display of menu if
      there are no displayable items.
        @param Sender [in] Not used.
        @param Cancel [in/out] Set to True if menu is not to be displayed.
      }
  strict protected
    procedure InitMenu; override;
      {Initialises menu. Records details of HTML link element to any link
      related menu items.
      }
  public
    constructor Create(const Menu: TPopupMenu); override;
      {Class constructor. Sets up object.
        @param Menu [in] Menu component being wrapped.
      }
  end;

  {
  TWBDefaultPopupMenuWrapper:
    Wrapper for popup menus associated with a web browser control that extends
    TWBPopupMenuWrapper to add some menu items based on links in underlying
    document in browser control.
  }
  TWBDefaultPopupMenuWrapper = class(TWBPopupMenuWrapper)
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
  strict protected
    procedure InitMenu; override;
      {Initialises menu. Clears temporary menu items and adds any required menu
      items for links in HTML document.
      }
  end;


implementation


uses
  // Delphi
  SysUtils, StrUtils,
  // Project
  UAnchors, UGC, UHTMLDocHelper, UImageTags, ULinkAction, UMenuHelper,
  UWBPopupMenus;


{ TWBCommandBarMgr }

class function TWBCommandBarMgr.GetImages: TGIFImageList;
  {Gets reference to static image list, creating it if necessary.
    @return Reference to image list.
  }
begin
  if not Assigned(fImages) then
  begin
    fImages := TGIFImageList.Create(nil);
    TGC.GCLocalObj(fGC, fImages);
  end;
  Result := fImages;
end;

procedure TWBCommandBarMgr.SetImages(const Images: TCustomImageList);
  {Specifies image list to be used by all command bars.
    @param Images [in] Image list to be used.
  }
begin
  GetImages.Clear;
  GetImages.AddImages(Images);
  inherited SetImages(GetImages);
end;

{ TWBPopupMenuWrapper }

constructor TWBPopupMenuWrapper.Create(const Menu: TPopupMenu);
  {Class constructor. Sets up object.
    @param Menu [in] Menu component being wrapped.
  }
begin
  inherited;
  // Assign popup query handler if possible
  if (Menu is TWBPopupMenu) then
    (Menu as TWBPopupMenu).OnQueryPopup := QueryPopupHandler;
end;

function TWBPopupMenuWrapper.HasVisibleItems: Boolean;
  {Checks if menu has any visible non-spacer menu items with captions. Assumes
  there are no sub-menus.
    @return True if menu has displayable items, False if not.
  }
var
  MI: TMenuItem;  // each menu item in menu
begin
  Result := False;
  for MI in Menu.Items do
    if MI.Visible and (MI.Caption <> '') and (MI.Caption <> '-') then
    begin
      Result := True;
      Exit;
    end;
end;

procedure TWBPopupMenuWrapper.InitMenu;
  {Initialises menu. Records details of HTML link element to any link related
  menu items.
  }
var
  MI: TMenuItem;  // references a menu item
begin
  for MI in Menu.Items do
    if MI.Action is TLinkAction then
      // menu item associated with a link action: store reference to Elem in it
      (MI.Action as TLinkAction).Link := (Menu as TWBPopupMenu).HTMLElem;
end;

procedure TWBPopupMenuWrapper.QueryPopupHandler(Sender: TObject;
  var Cancel: Boolean);
  {Handles wrapped menu's OnPopupQuery event. Prevents display of menu if there
  are no displayable items.
    @param Sender [in] Not used.
    @param Cancel [in/out] Set to True if menu is not to be displayed.
  }
begin
  PrepareMenu;
  Cancel := not HasVisibleItems;
end;

{ TWBDefaultPopupMenuWrapper }

procedure TWBDefaultPopupMenuWrapper.AddLinksToMenu(const Links: IDispatchList);
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
  Menu.Items.Add(CreateMenuSpacer(Menu, TWBTempMenuItem));
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
      Menu.Items.Add(CreateMenuItem(Menu, TWBTempMenuItem, Action));
    end;
  end;
end;

procedure TWBDefaultPopupMenuWrapper.ClearTempMenuItems;
  {Clears temporary menu items from menu.
  }
var
  MI: TMenuItem;  // references each menu item in menu
  Idx: Integer;   // loops through all menu items
begin
  // NOTE: do not use enumerator here: goes horribly wrong since MI is freed.
  for Idx := Pred(Menu.Items.Count) downto 0 do
  begin
    MI := Menu.Items[Idx];
    if MI is TWBTempMenuItem then
      MI.Free;
  end;
end;

function TWBDefaultPopupMenuWrapper.GetImageIndex(
  const Link: IDispatch): Integer;
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
  Result := -1;
  if Menu.Images is TGIFImageList then
    with Menu.Images as TGIFImageList do
    begin
      Result := ImageIndex(Src);
      if Result = -1 then
        Result := AddGIFImage(Src);
    end;
end;

procedure TWBDefaultPopupMenuWrapper.GetLinkMenuItems(const Doc: IDispatch;
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
        akCommand:
          CommandItems.Add(Link);
        akHelp:
          HelpItems.Add(Link);
      end;
    end;
  end;
end;

procedure TWBDefaultPopupMenuWrapper.InitMenu;
  {Initialises menu. Clears temporary menu items and adds any required menu
  items for links in HTML document.
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
    THTMLDocHelper.DocumentFromElem(
      (Menu as TWBPopupMenu).HTMLElem
    ),
    CommandLinks,
    HelpLinks
  );
  // Add the required menu items to the menu
  AddLinksToMenu(CommandLinks);
  AddLinksToMenu(HelpLinks);
end;

end.

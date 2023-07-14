{
 * This Source Code Form is subject to the terms of the Mozilla Public License,
 * v. 2.0. If a copy of the MPL was not distributed with this file, You can
 * obtain one at https://mozilla.org/MPL/2.0/
 *
 * Copyright (C) 2007-2023, Peter Johnson (gravatar.com/delphidabbler).
 *
 * Defines various classes used to configure one or more command bars owned by
 * a web browser container. Command bars are UI elements used to issue commands,
 * e.g. menus & toolbars.
}


unit UWBCommandBars;


interface


uses
  // Delphi
  ImgList, Menus,
  // Project
  UCommandBars, UDispatchList, UGIFImageList;


type
  ///  <summary>
  ///  Command bar manager for use with command bars associated with web browser
  ///  controls.
  ///  </summary>
  ///  <remarks>
  ///  All instances share the same, static, image list.
  ///  </remarks>
  TWBCommandBarMgr = class sealed(TCommandBarMgr, ICommandBarConfig)
  strict private
    class var
      /// <summary>Static image list.</summary>
      fImages: TGIFImageList;
  public
    ///  <summary>Creates empty static image list.</summary>
    class constructor Create;
    ///  <summary>Frees static image list.</summary>
    class destructor Destroy;
    ///  <summary>Sets image list to be used by all command bars to given image
    ///  list.</summary>
    ///  <remarks>
    ///  <para>Assigns images from Images to static image list.</para>
    ///  <para>Method of ICommandBarConfig.</para>
    ///  </remarks>
    procedure SetImages(const Images: TCustomImageList); override;
  end;

type
  ///  <summary>
  ///  Wrapper for popup menus associated with a web browser control. Records
  ///  reference HTML element under mouse cursor and prevents display of menu if
  ///  there's nothing to display.
  ///  </summary>
  TWBPopupMenuWrapper = class(TPopupMenuWrapper)
  strict private
    ///  <summary>Checks if menu has any visible non-spacer menu items with
    ///  captions.</summary>
    ///  <remarks>Assumes there are no sub-menus.</remarks>
    function HasVisibleItems: Boolean;
    ///  <summary>Handles wrapped menu's OnPopupQuery event. Prevents display of
    ///  menu if there are no displayable items.</summary>
    ///  <param name="Sender">TObject [in] Not used.</param>
    ///  <param name="Cancel">Boolean [in/out] False when method called. Set to
    ///  True if menu is not to be displayed.</param>
    procedure QueryPopupHandler(Sender: TObject; var Cancel: Boolean);
  strict protected
    ///  <summary>Initialises menu. Records details of HTML link element in any
    ///  link related menu items.</summary>
    procedure InitMenu; override;
  public
    ///  <summary>Sets up object for a given popup menu.</summary>
    constructor Create(const Menu: TPopupMenu); override;
  end;

type
  ///  <summary>
  ///  Wrapper for popup menus associated with a web browser control. Extends
  ///  TWBPopupMenuWrapper to add some menu items based on links in underlying
  ///  document in browser control.
  ///  </summary>
  TWBDefaultPopupMenuWrapper = class sealed(TWBPopupMenuWrapper)
  strict private
    ///  <summary>Clears temporary menu items from menu.</summary>
    procedure ClearTempMenuItems;
    ///  <summary>Gets all command and help links from browser document that are
    ///  designated as menu items.</summary>
    ///  <param name="Doc">IDispatch [in] IDispatch interface of document
    ///  containing links.</param>
    ///  <param name="CommandItems">IDispatchLIst [out] List of command links.
    ///  </param>
    ///  <param name="HelpItems">IDispatchList [out] List of help links.</param>
    procedure GetLinkMenuItems(const Doc: IDispatch; out CommandItems,
      HelpItems: IDispatchList);
    ///  <summary>Adds menu items to menu that can trigger links from a link
    ///  list.</summary>
    ///  <param name="Links">IDispatchList [in] List of links to be added to
    ///  name.</param>
    procedure AddLinksToMenu(const Links: IDispatchList);
    ///  <summary>Gets index of any image associated with a link in image list
    ///  used by menu. If image doesn't exist in list it is added to it.
    ///  </summary>
    ///  <param name="Link">IDispatch [in] Link for which image needed.</param>
    ///  <returns>Index of image in image list or -1 if there is no associated
    ///  image.</returns>
    function GetImageIndex(const Link: IDispatch): Integer;
  strict protected
    ///  <summary>Initialises menu. Clears temporary menu items and adds any
    ///  required menu items for links in HTML document.</summary>
    procedure InitMenu; override;
  end;


implementation


uses
  // Project
  UAnchors, UHTMLDOMHelper, UImageTags, ULinkAction, UMenus, UStrUtils,
  UWBPopupMenus;


{ TWBCommandBarMgr }

class constructor TWBCommandBarMgr.Create;
begin
  fImages := TGIFImageList.Create(nil);
end;

class destructor TWBCommandBarMgr.Destroy;
begin
  fImages.Free;
end;

procedure TWBCommandBarMgr.SetImages(const Images: TCustomImageList);
begin
  fImages.Clear;
  fImages.AddImages(Images);
  inherited SetImages(fImages);
end;

{ TWBPopupMenuWrapper }

constructor TWBPopupMenuWrapper.Create(const Menu: TPopupMenu);
begin
  inherited;
  // Assign popup query handler if possible
  if (Menu is TWBPopupMenu) then
    (Menu as TWBPopupMenu).OnQueryPopup := QueryPopupHandler;
end;

function TWBPopupMenuWrapper.HasVisibleItems: Boolean;
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
begin
  PrepareMenu;
  Cancel := not HasVisibleItems;
end;

{ TWBDefaultPopupMenuWrapper }

procedure TWBDefaultPopupMenuWrapper.AddLinksToMenu(const Links: IDispatchList);
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
    if THTMLDOMHelper.ElemIsVisible(Link) then
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

  ///  Extracts a base resource name from a URL.
  function URLBaseName(const URL: string): string;
  var
    Pos: Integer; // position of last path delimiter in URL
  begin
    Pos := StrLastDelimiterPos('/', URL);
    if Pos > 0 then
      Result := StrSliceRight(URL, Length(URL) - Pos)
    else
      Result := URL;
  end;

var
  ParentDiv: IDispatch;     // parent <div> or <span> tag that contains Link
  ImgTags: IDispatchList;   // all <img> children of parent
  ImgTag: IDispatch;        // <img> child of parent that contains required GIF
  Src: string;              // resource URL of GIF file
  MenuImages: TGIFImageList;
begin
  Result := -1;
  // Check if parent elem is a <div> or <span> with class "option"
  ParentDiv := THTMLDOMHelper.ParentElem(Link, 'div', 'option');
  if not Assigned(ParentDiv) then
    ParentDiv := THTMLDOMHelper.ParentElem(Link, 'span', 'option');
  if not Assigned(ParentDiv) then
    Exit;
  // So see if there's an child <img> of parent with class "option-img"
  ImgTags := TImageTags.GetAllImageTags(ParentDiv);
  ImgTag := nil;
  for ImgTag in ImgTags do
    if THTMLDOMHelper.ElemHasClass(ImgTag, 'option-img') then
      Break;
  if not Assigned(ImgTag) then
    Exit;
  // Get resource name of image from <img> tag's "src" attribute
  Src := URLBaseName(TImageTags.GetSrc(ImgTag));
  // Get matching bitmap from image list: add one from GIF file if not found
  Result := -1;
  if Menu.Images is TGIFImageList then
  begin
    MenuImages := Menu.Images as TGIFImageList;
    Result := MenuImages.ImageIndex(Src);
    if Result = -1 then
      Result := MenuImages.AddGIFImage(Src);
  end;
end;

procedure TWBDefaultPopupMenuWrapper.GetLinkMenuItems(const Doc: IDispatch;
  out CommandItems, HelpItems: IDispatchList);
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
    if THTMLDOMHelper.ElemHasClass(Link, 'menu-item') then
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
var
  CommandLinks: IDispatchList;  // list of command links in document
  HelpLinks: IDispatchList;     // list of help links in document
begin
  // Removes any pre-existing menu items from menu
  ClearTempMenuItems;
  inherited;
  // Get list of command and help links from current document
  GetLinkMenuItems(
    THTMLDOMHelper.DocumentFromElem(
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


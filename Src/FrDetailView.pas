{
 * FrDetailView.pas
 *
 * Abstract base class for frames that display content in the detail pane.
 * Implements functionality common to all detail frames.
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
 * The Original Code is FrDetailView.pas
 *
 * The Initial Developer of the Original Code is Peter Johnson
 * (http://www.delphidabbler.com/).
 *
 * Portions created by the Initial Developer are Copyright (C) 2005-2012 Peter
 * Johnson. All Rights Reserved.
 *
 * Contributor(s)
 *   NONE
 *
 * ***** END LICENSE BLOCK *****
}


unit FrDetailView;


interface


uses
  // Delphi
  OleCtrls, SHDocVw, Classes, Controls, ExtCtrls, Windows, ActiveX,
  // Project
  FrBrowserBase, IntfFrameMgrs, IntfHTMLDocHostInfo, UCommandBars, UCSSBuilder,
  UDetailPageLoader, UView, UWBPopupMenus;

type

  {
  TDetailViewFrame:
    Abstract base class for frames that display content in the detail pane.
    Implements functionality common to all detail frames. It implements web
    browser display manager, clipboard manager and selection manager interfaces.
    It also manages the contained web browser control and provides an extension
    to the browser's "external" object via its ISetWBExternal interface.
  }
  TDetailViewFrame = class(TBrowserBaseFrame,
    IPaneInfo,                                // provides information about pane
    IHTMLDocHostInfo,                       // info for use in HTML manipulation
    ICommandBarConfig                               // command bar configuration
  )
  strict private
    fCurrentView: TViewItem;        // Value of CurrentView property
    fIsActivated: Boolean;          // Value of Active property
    fPopupMenuMgr: TWBPopupMenuMgr; // Managed browser related popup menus
    fCommandBars: TCommandBarMgr;   // Configures command bars (browser popups)
    procedure MoveToDocTop;
      {Scrolls browser control to top. This method is used to ensure that newly
      loaded documents are not partially scrolled.
      }
    procedure PopupMenuHandler(Sender: TObject; PopupPos: TPoint;
      const MenuID: DWORD; var Handled: Boolean; const Obj: IDispatch);
      {Handles web browser UI manager's OnMenuPopup event. Pops up relevant menu
      if appropriate.
        @param Sender [in] Not used.
        @param PopupPos [in] Point at which menu is to be displayed.
        @param MenuID [in] Identifies kinds of menu required.
        @param Handled [in/out] Set to true to prevent browser control
          displaying own menu.
        @param Obj [in] Reference to HTML element at popup position.
      }
  protected // do not make strict
    { IClipboardMgr: Implemented in base class }
    { ISelectionMgr: Implemented in base class }
    { IPaneInfo }
    function IsInteractive: Boolean;
      {Checks if the pane is currently interactive with user.
        @return True if pane is interactive, False if not.
      }
    { IWBDisplayMgr }
    procedure Activate;
      {Activates the frame (when it is shown).
      }
    procedure Deactivate;
      {Deactivates the frame (when it is hidden).
      }
    { IViewItemDisplayMgr }
    procedure Display(const View: TViewItem; const Force: Boolean = False);
      {Displays compiler support information for a view item.
        @param View [in] Information about view item to be displayed.
        @param Force [in] Forces view item to be re-displayed even if not
          changed.
      }
    { IWBCustomiser }
    procedure SetExternalObj(const Obj: IDispatch);
      {Provides an object to be used to extend a web browser's external object.
        @param Obj [in] External browser object extender.
      }
    procedure SetDragDropHandler(const Obj: IDropTarget);
      {Provides an object to be used by web browser control to handle drag-drop
      operations.
        @param Obj [in] Drag-drop handler.
      }
    { IHTMLDocHostInfo }
    function HTMLDocument: IDispatch;
      {Gets reference to IDispatch interface of HTML document loaded in browser
      control.
        @return Document reference.
      }
    { ICommandBarConfig }
    property CommandBars: TCommandBarMgr
      read fCommandBars implements ICommandBarConfig;
      {References aggregated object implementing ICommandBarConfig}
  strict protected
    function GetPageKind: TDetailPageKind; virtual; abstract;
      {Gets kind of page to be loaded by DisplayCurViewItem.
        @return Page kind.
      }
    procedure DisplayCurViewItem; virtual;
      {Displays current view item. This method should not be called directly
      in descendant classes. They should instead call UpdateDisplay which checks
      if frame is active before calling this method.
      }
    procedure UpdateDisplay;
      {Updates the display if active. Does nothing if display not active.
      }
    procedure BuildCSS(const CSSBuilder: TCSSBuilder); override;
      {Generates CSS classes specific to HTML displayed in detail panes. This
      CSS is added to that provided by parent class.
        @param CSSBuilder [in] Object used to build the CSS code.
      }
    property CurrentView: TViewItem
      read fCurrentView;
      {Information about currently displayed view item}
  public
    constructor Create(AOwner: TComponent); override;
      {Class constructor. Sets up detail view frame.
        @param AOwner [in] Component that owns frame.
      }
    destructor Destroy; override;
      {Class destructor. Tears down object.
      }
  end;


implementation


uses
  // Delphi
  SysUtils, Graphics, Menus,
  // Project
  Hiliter.UAttrs, Hiliter.UCSS, Hiliter.UGlobals, UColours, UCSSUtils,
  UFontHelper, UUtils, UWBCommandBars;

{$R *.dfm}

{ TDetailViewFrame }

procedure TDetailViewFrame.Activate;
  {Activates the frame (when it is shown).
  }
begin
  if not fIsActivated then
  begin
    // We are going from inactive to active: draw display
    fIsActivated := True;
    UpdateDisplay;
  end;
end;

procedure TDetailViewFrame.BuildCSS(const CSSBuilder: TCSSBuilder);
  {Generates CSS classes specific to HTML displayed in detail panes. This CSS is
  added to that provided by parent class.
    @param CSSBuilder [in] Object used to build the CSS code.
  }
var
  HiliteAttrs: IHiliteAttrs;  // syntax highlighter used to build CSS
  CSSFont: TFont;             // font used to set CSS properties
begin
  // NOTE:
  // We only set CSS properties that may need to use system colours or fonts
  // that may be changed by user or changing program defaults. CSS that controls
  // layout remains in a CSS file embedded in resources.
  inherited;
  CSSFont := TFont.Create;
  try
    // Set body style to use program's font and window colour
    with CSSBuilder.AddSelector('body') do
    begin
      TFontHelper.SetContentFont(CSSFont, True);
      AddProperty(CSSFontProps(CSSFont));
      AddProperty(CSSBackgroundColorProp(clWindow));
    end;
    // Set table to use required font
    with CSSBuilder.AddSelector('table') do
    begin
      TFontHelper.SetContentFont(CSSFont, True);
      AddProperty(CSSFontProps(CSSFont));
      AddProperty(CSSBackgroundColorProp(clBorder));
    end;
    // Set default table cell colour (must be different to table to get border)
    with CSSBuilder.AddSelector('td') do
      AddProperty(CSSBackgroundColorProp(clWindow));
    // Sets H1 heading font size and border
    with CSSBuilder.AddSelector('h1') do
    begin
      TFontHelper.SetContentFont(CSSFont, True);
      CSSFont.Size := CSSFont.Size + 2;
      CSSFont.Style := [fsBold];
      AddProperty(CSSFontProps(CSSFont));
      AddProperty(CSSBorderProp(cssBottom, 1, cbsSolid, clBorder));
    end;
    // Sets H2 heading font size and border
    with CSSBuilder.AddSelector('h2') do
    begin
      TFontHelper.SetContentFont(CSSFont, True);
      CSSFont.Assign(CSSFont);
      CSSFont.Style := [fsBold];
      AddProperty(CSSFontProps(CSSFont));
    end;
    // Style of box that appears around clickable options (or actions)
    with CSSBuilder.AddSelector('.optionbox') do
      AddProperty(CSSBorderProp(cssAll, 1, cbsSolid, clBorder));
    with CSSBuilder.AddSelector('.userdb') do
      AddProperty(CSSColorProp(clUserRoutine));
    with CSSBuilder.AddSelector('.maindb') do
      AddProperty(CSSColorProp(Self.Font.Color));
    // Sets text styles and colours used by syntax highlighter
    HiliteAttrs := THiliteAttrsFactory.CreateDisplayAttrs;
    with THiliterCSS.Create(HiliteAttrs) do
      try
        BuildCSS(CSSBuilder);
      finally
        Free;
      end;
    // Adjust .pas-source class to use required background colour
    with CSSBuilder.Selectors['.' + THiliterCSS.GetMainCSSClassName] do
      AddProperty(CSSBackgroundColorProp(clSourceBg));
  finally
    FreeAndNil(CSSFont);
  end;
end;

constructor TDetailViewFrame.Create(AOwner: TComponent);
  {Class constructor. Sets up detail view frame.
    @param AOwner [in] Component that owns frame.
  }
type
  // Range of browser-related popup menu command ids
  TWBMenuID = cDetailPopupMenuFirst..cDetailPopupMenuLast;
const
  // Maps popup menu ids to command bar wrapper classes for assoicated menus. A
  // nil entry indicates no menu or wrapper are required for that menu type.
  cMenuWrapperClassMap: array[TWBMenuID] of TPopupMenuWrapperClass = (
    TWBDefaultPopupMenuWrapper,   // cDetailPopupMenuDefault
    TWBPopupMenuWrapper,          // cDetailPopupMenuImage
    nil,                          // cDetailPopupMenuControl
    nil,                          // cDetailPopupMenuTable
    TWBPopupMenuWrapper,          // cDetailPopupMenuTextSelect
    TWBPopupMenuWrapper,          // cDetailPopupMenuAnchor
    nil                           // cDetailPopupMenuUnknown
  );
var
  CmdBarID: TCommandBarID;            // loops through browser menu command ids
  Menu: TPopupMenu;                   // references each required popup menu
  WrapperCls: TPopupMenuWrapperClass; // class of required command bar wrapper
begin
  inherited;
  // Set up objects that manage browser pop-up menus and command bars
  fPopupMenuMgr := TWBPopupMenuMgr.Create(Self);  // frees itself
  fCommandBars := TWBCommandBarMgr.Create(Self);
  // Create required popup menus for use with browser control and add to command
  // bar manager
  for CmdBarID := cDetailPopupMenuFirst to cDetailPopupMenuLast do
  begin
    WrapperCls := cMenuWrapperClassMap[CmdBarID];
    if Assigned(WrapperCls) then
    begin
      Menu := fPopupMenuMgr.AddMenu(CmdBarID);
      fCommandBars.AddCommandBar(CmdBarID, WrapperCls.Create(Menu));
    end;
  end;
  // Set pop-up menu handler for browser control
  WBController.UIMgr.OnMenuPopup := PopupMenuHandler;
  // Create object to store detailed info about current view item
  fCurrentView := TViewItem.Create;
end;

procedure TDetailViewFrame.Deactivate;
  {Deactivates the frame (when it is hidden).
  }
begin
  // Record that we are inactive
  fIsActivated := False;
end;

destructor TDetailViewFrame.Destroy;
  {Class destructor. Tears down object.
  }
begin
  FreeAndNil(fCurrentView);
  FreeAndNil(fCommandBars);
  inherited;
end;

procedure TDetailViewFrame.Display(const View: TViewItem;
  const Force: Boolean);
  {Displays compiler support information for a view item.
    @param View [in] Information about view item to be displayed.
    @param Force [in] Forces view item to be re-displayed even if not
      changed.
  }
begin
  if not CurrentView.IsEqual(View) or Force then
  begin
    // Record view item and display style
    CurrentView.Assign(View);
    // Redraw the display
    UpdateDisplay;
  end;
end;

procedure TDetailViewFrame.DisplayCurViewItem;
  {Displays current view item. This method should not be called directly in
  descendant classes. They should instead call UpdateDisplay which checks if
  frame is active before calling this method.
  }
begin
  // Load the required page using page loader.
  TDetailPageLoader.LoadPage(GetPageKind, CurrentView, WBController);
  // Cancel any selection in browser control
  WBController.UIMgr.ClearSelection;
end;

function TDetailViewFrame.HTMLDocument: IDispatch;
  {Gets reference to IDispatch interface of HTML document loaded in browser
  control.
    @return Document reference.
  }
begin
  GetIntf(wbBrowser.Document, IDispatch, Result);
end;

function TDetailViewFrame.IsInteractive: Boolean;
  {Checks if the pane is currently interactive with user.
    @return True if pane is interactive, False if not.
  }
begin
  // This frame only contains a browser control. So, if frame is interactive
  // if and only if browser control is active.
  Result := IsBrowserActive;
end;

procedure TDetailViewFrame.MoveToDocTop;
  {Scrolls browser control to top. This method is used to ensure that newly
  loaded documents are not partially scrolled.
  }
begin
  WBController.UIMgr.ScrollTo(0, 0);
end;

procedure TDetailViewFrame.PopupMenuHandler(Sender: TObject;
  PopupPos: TPoint; const MenuID: DWORD; var Handled: Boolean;
  const Obj: IDispatch);
  {Handles web browser UI manager's OnMenuPopupEx event. Pops up relevant menu
  if appropriate.
    @param Sender [in] Not used.
    @param PopupPos [in] Point at which menu is to be displayed.
    @param MenuID [in] Identifies kinds of menu required.
    @param Handled [in/out] Set to true to prevent browser control displaying
      own menu.
    @param Obj [in] Reference to HTML element at popup position.
  }
begin
  fPopupMenuMgr.Popup(MenuID, PopupPos, Obj);
  Handled := True;
end;

procedure TDetailViewFrame.SetDragDropHandler(const Obj: IDropTarget);
  {Provides an object to be used by web browser control to handle drag-drop
  operations.
    @param Obj [in] Drag-drop handler.
  }
begin
  WBController.UIMgr.DropTarget := Obj;
end;

procedure TDetailViewFrame.SetExternalObj(const Obj: IDispatch);
  {Provides an object to be used to extend a web browser's external object.
    @param Obj [in] External browser object extender.
  }
begin
  WBController.UIMgr.ExternScript := Obj;
end;

procedure TDetailViewFrame.UpdateDisplay;
  {Updates the display if active. Does nothing if display not active.
  }
begin
  if fIsActivated then
  begin
    DisplayCurViewItem;
    MoveToDocTop; // ensures top of newly loaded document is displayed
  end;
end;

end.


{
 * FrDetailView.pas
 *
 * Implements a frame that can display detail views.
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
 * Portions created by the Initial Developer are Copyright (C) 2005-2011 Peter
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
  UDetailPageLoader, USearch, UView, UWBPopupMenus;

type

  ///  <summary>
  ///  Implements a frame that can display detail views.
  ///  </summary>
  ///  <remarks>
  ///  Uses a web browser control to display the views.
  ///  </remarks>
  TDetailViewFrame = class {sealed}(TBrowserBaseFrame,
    IPaneInfo,                                // provides information about pane
    ICommandBarConfig,                              // command bar configuration
    IViewItemDisplayMgr,                                 // displays a view item
    IClipboardMgr,                     // clipboard manager (impl in base class)
    ISelectionMgr,                     // selection manager (impl in base class)
    IWBCustomiser,                             // customises web browser control
    IHTMLDocHostInfo                        // info for use in HTML manipulation
  )
  strict private
    var
      ///  <summary>Information about currently displayed view item.</summary>
      fCurrentView: IView;
      ///  <summary>Manager for popup menus that relate to the browser control.
      ///  </summary>
      fPopupMenuMgr: TWBPopupMenuMgr;
      ///  <summary>Configures command bars (i.e. browser popups).</summary>
      fCommandBars: TCommandBarMgr;
    ///  <summary>Scrolls browser control to top.</summary>
    ///  <remarks>This method is used to ensure that newly loaded documents are
    ///  not partially scrolled.</remarks>
    procedure MoveToDocTop;
    ///  <summary>Handles web browser UI manager's OnMenuPopupEx event. Pops up
    ///  relevant menu if appropriate.</summary>
    ///  <param name="Sender">TObject [in] Not used.</param>
    ///  <param name="PopupPos">TPoint [in] Position at which menu is to be
    ///  displayed.</param>
    ///  <param name="MenuID">DWORD [in] Identifies kind of menu required.
    ///  </param>
    ///  <param name="Handled">Boolean [in/out] Determines whether browser
    ///  control displays its default menu. Set to True to inhibit the default
    ///  menu.</param>
    ///  <param name="Obj">IDispatch [in] Reference to HTML element at menu
    ///  popup position.</param>
    procedure PopupMenuHandler(Sender: TObject; PopupPos: TPoint;
      const MenuID: DWORD; var Handled: Boolean; const Obj: IDispatch);
    ///  <summary>Highlights words in current document that match given text
    ///  search criteria.</summary>
    procedure HighlightSearchResults(const Criteria: ITextSearchCriteria);
  strict protected
    ///  <summary>Generates CSS classes specific to HTML displayed in this pane.
    ///  </summary>
    ///  <param name="CSSBuilder">TCSSBuilder [in] Used to build CSS code.
    ///  </param>
    ///  <remarks>The CSS generated in this method is added to that provided by
    ///  base class.</remarks>
    procedure BuildCSS(const CSSBuilder: TCSSBuilder); override;
    ///  <summary>References contained object that implements ICommandBarConfig.
    ///  </summary>
    ///  <remarks>There is no need to access this property: it is here simply to
    ///  implement the interface.</remarks>
    property CommandBars: TCommandBarMgr
      read fCommandBars implements ICommandBarConfig;
  public
    ///  <summary>Object constructor. Initialises frame.</summary>
    ///  <param name="AOwner">TComponent [in] Component that owns this frame.
    ///  </param>
    constructor Create(AOwner: TComponent); override;
    ///  <summary>Object destructor. Tidies up owned object.</summary>
    destructor Destroy; override;
    ///  <summary>Checks if this view is currently interactive with user.
    ///  </summary>
    ///  <remarks>Method of IPaneInfo.</remarks>
    function IsInteractive: Boolean;
    ///  <summary>Displays a view in the frame.</summary>
    ///  <param name="View">IView [in] Information about view to be displayed.
    ///  </param>
    ///  <param name="Force">Boolean [in] When True ensure that view is
    ///  re-displayed even if not changed. When False view is only re-displayed
    ///  if it is different to previous view.</param>
    ///  <remarks>Method of IViewItemDisplayMgr.</remarks>
    procedure Display(View: IView; const Force: Boolean = False);
    ///  <summary>Gets reference to currently displayed view.</summary>
    ///  <remarks>Method of IViewItemDisplayMgr.</remarks>
    function GetCurrentView: IView;
    ///  <summary>Records the object used to extend the web browser control's
    ///  external object.</summary>
    ///  <remarks>Method of IWBCustomiser.</remarks>
    procedure SetExternalObj(Obj: IDispatch);
    ///  <summary>Records the object used to handle web browser control's
    ///  drag-drop operations.</summary>
    ///  <remarks>Method of IWBCustomiser.</remarks>
    procedure SetDragDropHandler(Obj: IDropTarget);
    ///  <summary>Gets a reference to the IDispatch interface of any HTML
    ///  document loaded in browser control.</summary>
    ///  <remarks>Method of IHTMLDocHostInfo.</remarks>
    function HTMLDocument: IDispatch;
  end;


implementation


uses
  // Delphi
  SysUtils, Graphics, Menus,
  // Project
  Browser.UHighlighter, Hiliter.UAttrs, Hiliter.UCSS, Hiliter.UGlobals,
  UActiveTextHTML, UColours, UCSSUtils, UFontHelper, UQuery, UUtils,
  UWBCommandBars;

{$R *.dfm}

{ TDetailViewFrame }

procedure TDetailViewFrame.BuildCSS(const CSSBuilder: TCSSBuilder);
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
      AddProperty(CSSColorProp(clUserSnippet));
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
    with CSSBuilder.AddSelector('.comptable th') do
    begin
      AddProperty(CSSBackgroundColorProp(clCompTblHeadBg));
      AddProperty(CSSFontWeightProp(cfwNormal));
    end;
    // Add CSS relating to active text
    TFontHelper.SetContentFont(CSSFont, True);
    TActiveTextHTML.Styles(CSSFont, CSSBuilder);
  finally
    CSSFont.Free;
  end;
end;

constructor TDetailViewFrame.Create(AOwner: TComponent);
type
  ///  <summary>Range of browser-related popup menu command ids.</summary>
  TWBMenuID = cDetailPopupMenuFirst..cDetailPopupMenuLast;
const
  ///  <summary>Maps popup menu ids to command bar wrapper classes for
  ///  associated popup menus.</summary>
  ///  <remarks>A nil entry indicates no menu or wrapper are required for that
  ///  menu type.</remarks>
  MenuWrapperClassMap: array[TWBMenuID] of TPopupMenuWrapperClass = (
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
    WrapperCls := MenuWrapperClassMap[CmdBarID];
    if Assigned(WrapperCls) then
    begin
      Menu := fPopupMenuMgr.AddMenu(CmdBarID);
      fCommandBars.AddCommandBar(CmdBarID, WrapperCls.Create(Menu));
    end;
  end;
  // Set pop-up menu handler for browser control
  WBController.UIMgr.OnMenuPopupEx := PopupMenuHandler;
  // Create object to store detailed info about current view item
  fCurrentView := TViewItemFactory.CreateNulView;
end;

destructor TDetailViewFrame.Destroy;
begin
  fCurrentView := nil;
  fCommandBars.Free;
  inherited;
end;

procedure TDetailViewFrame.Display(View: IView; const Force: Boolean);
var
  TextSearchCriteria: ITextSearchCriteria;  // criteria for any text search
begin
  if not fCurrentView.IsEqual(View) or Force then
  begin
    fCurrentView := TViewItemFactory.Clone(View);
    // Load view's HTML into browser control
    TDetailPageLoader.LoadPage(fCurrentView, WBController);
    // Clear any existing text selection
    WBController.UIMgr.ClearSelection;
    // If we're viewing a snippet and there's an active text search, highlight
    // text that matches search
    if Supports(fCurrentView, ISnippetView) and
      Supports(
        Query.CurrentSearch.Criteria, ITextSearchCriteria, TextSearchCriteria
      ) then
      HighlightSearchResults(TextSearchCriteria);
    // Ensure top of newly loaded document is displayed
    MoveToDocTop;
  end;
end;

function TDetailViewFrame.GetCurrentView: IView;
begin
  Result := fCurrentView;
end;

procedure TDetailViewFrame.HighlightSearchResults(
  const Criteria: ITextSearchCriteria);
var
  Highlighter: TWBHighlighter;  // object used to perform highlighting
begin
  Assert(Assigned(Criteria),
    ClassName + '.HighlightSearchResults: Criteria is nil');
  Assert(Supports(Criteria, ITextSearchCriteria),
    ClassName + '.HighlightSearchResults: There is no current text search');
  Assert(Supports(fCurrentView, ISnippetView),
    ClassName + '.HighlightSearchResults: View item is not a snippet');
  // Create and configure highlighter object
  Highlighter := TWBHighlighter.Create(wbBrowser);
  try
    // only a snippet's description and source code are included in a text
    // search. These sections are enclosed in tags with ids 'description',
    // 'sourcecode' and 'extra' respectively in the document's HTML so we
    // restrict highlighting to these sections
    Highlighter.SearchSectionIDs.Add('description');
    Highlighter.SearchSectionIDs.Add('sourcecode');
    Highlighter.SearchSectionIDs.Add('extra');
    Highlighter.HighlightSearchResults(Criteria);
  finally
    Highlighter.Free;
  end;
end;

function TDetailViewFrame.HTMLDocument: IDispatch;
begin
  GetIntf(wbBrowser.Document, IDispatch, Result);
end;

function TDetailViewFrame.IsInteractive: Boolean;
begin
  // This frame only contains a browser control. So, if frame is interactive
  // if and only if browser control is active.
  Result := IsBrowserActive;
end;

procedure TDetailViewFrame.MoveToDocTop;
begin
  WBController.UIMgr.ScrollTo(0, 0);
end;

procedure TDetailViewFrame.PopupMenuHandler(Sender: TObject;
  PopupPos: TPoint; const MenuID: DWORD; var Handled: Boolean;
  const Obj: IDispatch);
begin
  fPopupMenuMgr.Popup(MenuID, PopupPos, Obj);
  Handled := True;
end;

procedure TDetailViewFrame.SetDragDropHandler(Obj: IDropTarget);
begin
  WBController.UIMgr.DropTarget := Obj;
end;

procedure TDetailViewFrame.SetExternalObj(Obj: IDispatch);
begin
  WBController.UIMgr.ExternScript := Obj;
end;

end.


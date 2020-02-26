{
 * This Source Code Form is subject to the terms of the Mozilla Public License,
 * v. 2.0. If a copy of the MPL was not distributed with this file, You can
 * obtain one at http://mozilla.org/MPL/2.0/
 *
 * Copyright (C) 2005-2020, Peter Johnson (gravatar.com/delphidabbler).
 *
 * Implements a frame that can display detailed views.
}


unit FrDetailView;


interface


uses
  // Delphi
  OleCtrls, SHDocVw, Classes, Controls, ExtCtrls, Windows, ActiveX,
  // Project
  FrBrowserBase, IntfFrameMgrs, UCommandBars, UCSSBuilder, UDetailPageLoader,
  USearch, UView, UWBPopupMenus;

type

  ///  <summary>Implements a frame that can display detail views.</summary>
  ///  <remarks>Uses a web browser control to display the views.</remarks>
  TDetailViewFrame = class {sealed}(TBrowserBaseFrame,
    IPaneInfo,                                // provides information about pane
    ICommandBarConfig,                              // command bar configuration
    IViewItemDisplayMgr,                                 // displays a view item
    IClipboardMgr,                     // clipboard manager (impl in base class)
    ISelectionMgr,                     // selection manager (impl in base class)
    IWBCustomiser                              // customises web browser control
  )
  strict private
    var
      ///  <summary>Manager for popup menus that relate to the browser control.
      ///  </summary>
      fPopupMenuMgr: TWBPopupMenuMgr;
      ///  <summary>Configures command bars (i.e. browser popups).</summary>
      fCommandBars: TCommandBarMgr;
      ///  <summary>Generates and loads HTML representing a view into browser
      ///  control.</summary>
      fPageLoader: TDetailPageLoader;
    ///  <summary>Handles web browser UI manager's OnMenuPopup event. Pops up
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
    ///  <summary>Highlights words in current document that match criteria of
    ///  given search filter.</summary>
    procedure HighlightSearchResults(const Filter: ITextSearchFilter);
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
    ///  <remarks>Method of IViewItemDisplayMgr.</remarks>
    procedure Display(View: IView);
    ///  <summary>Records the object used to extend the web browser control's
    ///  external object.</summary>
    ///  <remarks>Method of IWBCustomiser.</remarks>
    procedure SetExternalObj(Obj: IDispatch);
    ///  <summary>Records the object used to handle web browser control's
    ///  drag-drop operations.</summary>
    ///  <remarks>Method of IWBCustomiser.</remarks>
    procedure SetDragDropHandler(Obj: IDropTarget);
  end;


implementation


uses
  // Delphi
  SysUtils, Graphics, Menus,
  // Project
  ActiveText.UHTMLRenderer, Browser.UHighlighter, Hiliter.UAttrs, Hiliter.UCSS,
  Hiliter.UGlobals, UColours, UCSSUtils, UFontHelper, UPreferences, UQuery,
  USystemInfo, UUtils, UWBCommandBars;

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
      TFontHelper.SetContentFont(CSSFont);
      AddProperty(TCSS.FontProps(CSSFont));
      AddProperty(TCSS.BackgroundColorProp(clWindow));
    end;
    // Set table to use required font
    with CSSBuilder.AddSelector('table') do
    begin
      TFontHelper.SetContentFont(CSSFont);
      AddProperty(TCSS.FontProps(CSSFont));
      AddProperty(TCSS.BackgroundColorProp(clBorder));
    end;
    // Set default table cell colour (must be different to table to get border)
    with CSSBuilder.AddSelector('td') do
      AddProperty(TCSS.BackgroundColorProp(clWindow));
    // Sets H1 heading font size and border
    with CSSBuilder.AddSelector('h1') do
    begin
      TFontHelper.SetContentFont(CSSFont);
      CSSFont.Size := CSSFont.Size + 2;
      CSSFont.Style := [fsBold];
      AddProperty(TCSS.FontProps(CSSFont));
      AddProperty(TCSS.BorderProp(cssBottom, 1, cbsSolid, clBorder));
    end;
    // Sets H2 heading font size and border
    with CSSBuilder.AddSelector('h2') do
    begin
      TFontHelper.SetContentFont(CSSFont);
      CSSFont.Style := [fsBold];
      AddProperty(TCSS.FontProps(CSSFont));
    end;
    // Set H2 heading font for use in rendered active text
    with CSSBuilder.AddSelector('.active-text h2') do
    begin
      TFontHelper.SetContentFont(CSSFont);
      CSSFont.Style := [fsBold];
      CSSFont.Size := CSSFont.Size + 1;
      AddProperty(TCSS.FontProps(CSSFont));
    end;
    // Set H2 heading font for use in rendered active text in snippet list table
    with CSSBuilder.AddSelector('.snippet-list .active-text h2') do
    begin
      TFontHelper.SetContentFont(CSSFont);
      CSSFont.Style := [fsBold];
      AddProperty(TCSS.FontProps(CSSFont));
    end;
    // Style of box that appears around clickable options (or actions)
    with CSSBuilder.AddSelector('.optionbox') do
      AddProperty(TCSS.BorderProp(cssAll, 1, cbsSolid, clBorder));
    with CSSBuilder.AddSelector('.userdb') do
      AddProperty(TCSS.ColorProp(Preferences.DBHeadingColours[True]));
    with CSSBuilder.AddSelector('.maindb') do
      AddProperty(TCSS.ColorProp(Preferences.DBHeadingColours[False]));
    // Sets CSS for style of New Tab text
    with CSSBuilder.AddSelector('#newtab') do
    begin
      TFontHelper.SetContentFont(CSSFont);
      CSSFont.Size := 36;
      CSSFont.Color := clNewTabText;
      AddProperty(TCSS.FontProps(CSSFont));
    end;
    // Sets text styles and colours used by syntax highlighter
    HiliteAttrs := THiliteAttrsFactory.CreateUserAttrs;
    with THiliterCSS.Create(HiliteAttrs) do
      try
        BuildCSS(CSSBuilder);
      finally
        Free;
      end;
    // Adjust .pas-source class to use required background colour
    with CSSBuilder.Selectors['.' + THiliterCSS.GetMainCSSClassName] do
    begin
      AddProperty(TCSS.BackgroundColorProp(Preferences.SourceCodeBGcolour));
      if TIEInfo.SupportsCSSOverflowX then
        AddProperty(TCSS.OverflowProp(covAuto, codX));
    end;
    if TIEInfo.SupportsCSSOverflowX then
    begin
      with CSSBuilder.AddSelector('#compile-results') do
        AddProperty(TCSS.OverflowProp(covAuto, codX));
    end;
    with CSSBuilder.AddSelector('.comptable th') do
    begin
      AddProperty(TCSS.BackgroundColorProp(clCompTblHeadBg));
      AddProperty(TCSS.FontWeightProp(cfwNormal));
    end;
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
  ///  <remarks>Must never have a nil entry.</remarks>
  MenuWrapperClassMap: array[TWBMenuID] of TPopupMenuWrapperClass = (
    TWBDefaultPopupMenuWrapper,   // cDetailPopupMenuDefault
    TWBPopupMenuWrapper,          // cDetailPopupMenuImage
    TWBDefaultPopupMenuWrapper,   // cDetailPopupMenuControl
    TWBDefaultPopupMenuWrapper,   // cDetailPopupMenuTable
    TWBDefaultPopupMenuWrapper,   // cDetailPopupMenuTextSelect
    TWBPopupMenuWrapper,          // cDetailPopupMenuAnchor
    TWBPopupMenuWrapper           // cDetailPopupMenuUnknown
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
  WBController.UIMgr.OnMenuPopup := PopupMenuHandler;
  // Set up HTML page loader
  fPageLoader := TDetailPageLoader.Create(WBController);
end;

destructor TDetailViewFrame.Destroy;
begin
  fPageLoader.Free;
  fCommandBars.Free;
  inherited;
end;

procedure TDetailViewFrame.Display(View: IView);
var
  Filter: ITextSearchFilter;  // text search filter containing criteria
begin
  // Load view's HTML into browser control
  fPageLoader.LoadPage(View);
  // Clear any existing text selection
  WBController.UIMgr.ClearSelection;
  // If we're viewing a snippet and there's an active text search, highlight
  // text that matches search
  if Supports(View, ISnippetView) and
    Supports(Query.LatestSearch.Filter, ITextSearchFilter, Filter)
    then
    HighlightSearchResults(Filter);
end;

procedure TDetailViewFrame.HighlightSearchResults(
  const Filter: ITextSearchFilter);
var
  Highlighter: TWBHighlighter;  // object used to perform highlighting
begin
  Assert(Assigned(Filter),
    ClassName + '.HighlightSearchResults: Filter is nil');
  Assert(Supports(Filter, ITextSearchFilter),
    ClassName + '.HighlightSearchResults: There is no current text search');
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
    Highlighter.HighlightSearchResults(Filter);
  finally
    Highlighter.Free;
  end;
end;

function TDetailViewFrame.IsInteractive: Boolean;
begin
  // This frame only contains a browser control. So, if frame is interactive
  // if and only if browser control is active.
  Result := IsBrowserActive;
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


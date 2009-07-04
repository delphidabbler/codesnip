{
 * FrInfo.pas
 *
 * Frame that displays and manages user interaction with the pane that displays
 * details of routines and other selected view items.
 *
 * v0.1 of 30 Jan 2005  - Original version.
 * v0.2 of 18 Feb 2005  - Deleted unused units from uses clauses.
 * v0.3 of 19 Feb 2005  - Refactoring: changed frame to implement the renamed
 *                        IFrameActionMgr and IInfoActionMgr interfaces.
 * v0.4 of 20 Feb 2005  - Significantly revised method to generating and
 *                        displaying HTML pages by using new TDetailPageLoader
 *                        classes.
 * v0.5 of 22 Feb 2005  - Modified to use new browser external object extender.
 *                      - Single extender object now supplied to frame (by
 *                        calling its new ISetWBExternal interface) rather than
 *                        frame owning its own custom extender object.
 *                      - All support for IFrameActionMgr interfaces deleted
 *                        since the new extender object handles notification of
 *                        user-initiated events to application.
 * v0.6 of 23 Feb 2005  - Significantly revised to descend from other frames
 *                        that implement functionality that is common to all
 *                        detail view frames. This frame now only implements
 *                        functions specific to the information frame.
 * v0.7 of 28 Feb 2005  - Added support for highlighting found text in current
 *                        document. Uses object in new UWBHighlighter unit to
 *                        perform highlighting.
 * v0.8 of 16 Apr 2006  - Removed ISetWBExternal interface and replaced with new
 *                        IWBCustomiser interface to perform web browser
 *                        customisation. IWBCustomiser methods implemented in
 *                        ancestor class.
 * v1.0 of 25 May 2006  - Improved and corrected comments.
 * v1.1 of 03 Dec 2006  - Added support for dynamically updating display via
 *                        DHTML.
 *                      - Added GetPageKind override to return page kind to base
 *                        class to enable it to display correct view.
 *                      - Changed DisplayCurViewItem to override new version in
 *                        base class to perform any required text search
 *                        highlighting.
 *                      - Added code to set CSS specific to information pane.
 * v1.2 of 04 Feb 2007  - Now uses global query object to find details of
 *                        current search query rather than use TDetailView
 *                        object.
 *                      - Added new parameter to TInfoFrame.Display to force
 *                        redisplay of view item even if already displayed. This
 *                        was done to enable pane to highlight or unhilight text
 *                        search results. We previously used a TDetailView
 *                        object to do this.
 *                      - Replaced now redundant TDetailView class references
 *                        with TViewItem.
 * v1.3 of 16 Feb 2007  - Revised to work with redefined display interfaces from
 *                        IntfFrameMgrs.
 *                      - Pushed Display method back into base class.
 * v1.4 of 15 Oct 2007  - Added "extras" division to list of divs that are
 *                        highlighted by text search highlighter.
 * v1.5 of 05 Nov 2007  - Changed to use revised CSS builder classes.
 * v1.6 of 19 Jan 2009  - Changed to support interfaces from IntfHTMLDocHostInfo
 *                        to replace those deleted from UDHTML.
 *                      - Made a protected section strict.
 *                      - ClassName method now provides class name in asserts.
 * v1.7 of 20 Jun 2009  - Removed frame's support for IInfoRoutineHostInfo and
 *                        IDetailViewHostInfo interfaces.
 *                      - Added CSS relating to HTML used to render REML tags
 *                        used when displaying Extra information.
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
 * The Original Code is FrInfo.pas
 *
 * The Initial Developer of the Original Code is Peter Johnson
 * (http://www.delphidabbler.com/).
 *
 * Portions created by the Initial Developer are Copyright (C) 2005-2009 Peter
 * Johnson. All Rights Reserved.
 *
 * ***** END LICENSE BLOCK *****
}


unit FrInfo;


interface


uses
  // Delphi
  OleCtrls, SHDocVw, Classes, Controls, ExtCtrls,
  // Project
  FrDetailView, IntfFrameMgrs, IntfHTMLDocHostInfo, UCSSBuilder,
  UDetailPageLoader, USearch;


type

  {
  TInfoFrame:
    Frame that displays and manages user interaction with the pane that displays
    details of snippets and other selected view items. It implements display
    manager, clipboard manager and selection manager interfaces. It also manages
    the contained web browser control and provides an extension to the browser's
    "external" object via its ISetWBExternal interface.
  }
  TInfoFrame = class(TDetailViewFrame,
    IWBDisplayMgr,                         // support for hosted browser control
    IViewItemDisplayMgr,                                 // displays a view item
    IPaneInfo,                                // provides information about pane
    IWBCustomiser,                             // customises web browser control
    IClipboardMgr,                                          // clipboard manager
    ISelectionMgr,                                          // selection manager
    IHTMLDocHostInfo                        // info for use in HTML manipulation
    )
  protected // do not make strict
    { IWBDisplayMgr: Implemented in ancestor class }
    { IViewItemDisplayMgr: Implemented in ancestor class }
    { IPaneInfo: Implemented in ancestor class }
    { IWBCustomiser: Implemented in ancestor class }
    { IClipboardMgr: Implemented in ancestor class }
    { ISelectionMgr: Implemented in ancestor class }                                          // selection manager
    { IHTMLDocHostInfo: Implemented in ancestor class }
  strict protected
    procedure BuildCSS(const CSSBuilder: TCSSBuilder); override;
      {Generates CSS classes specific to HTML displayed in information pane.
      This CSS is added to that provided by parent class.
        @param CSSBuilder [in] Object used to build the CSS code.
      }
    procedure HighlightSearchResults(const Criteria: ITextSearchCriteria);
      {Highlights words in current snippet document that match text search
      criteria.
        @param Criteria [in] Text search criteria.
      }
    function GetPageKind: TDetailPageKind; override;
      {Gets page kind and provides to base class.
        @return Page kind.
      }
    procedure DisplayCurViewItem; override;
      {Displays current view item. This method should not be called directly
      from this class. Instead call inherited UpdateDisplay method which first
      checks if frame is active before calling this method polymorphically.
      }
  end;


implementation


uses
  // Delphi
  SysUtils, Graphics,
  // Project
  UColours, UCSSUtils, UFontHelper, UQuery, UWBHighlighter, UView;


{$R *.dfm}


{ TInfoFrame }

procedure TInfoFrame.BuildCSS(const CSSBuilder: TCSSBuilder);
  {Generates CSS classes specific to HTML displayed in information pane. This
  CSS is added to that provided by parent class.
    @param CSSBuilder [in] Object used to build the CSS code.
  }
var
  ContentFont: TFont;   // default content font for OS
  MonoFont: TFont;      // default mono spaced font for OS
begin
  // NOTE:
  // We only set CSS properties that may need to use system colours or fonts
  // that may be changed by user or changing program defaults. CSS that controls
  // layout remains in a CSS file embedded in resources.
  inherited;
  // Add CSS relating to compiler table
  with CSSBuilder.AddSelector('.comptable th') do
  begin
    AddProperty(CSSBackgroundColorProp(clCompTblHeadBg));
    AddProperty(CSSFontWeightProp(cfwNormal));
  end;
  // Add CSS relating to Extra REML code
  // -- heading tag
  with CSSBuilder.AddSelector('h2.extra') do
  begin
    ContentFont := TFont.Create;
    try
      TFontHelper.SetContentFont(Font, True);
      Font.Size := Font.Size + 1;
      AddProperty(CSSFontSizeProp(Font.Size));
    finally
      FreeAndNil(ContentFont);
    end;
  end;
  // -- warning tag
  with CSSBuilder.AddSelector('span.extra-warning') do
  begin
    AddProperty(CSSFontWeightProp(cfwBold));
    AddProperty(CSSColorProp(clWarningText));
  end;
  // -- mono tag
  with CSSBuilder.AddSelector('span.extra-mono') do
  begin
    MonoFont := TFont.Create;
    try
      TFontHelper.SetDefaultMonoFont(MonoFont, True);
      AddProperty(CSSFontProps(MonoFont));
    finally
      FreeAndNil(MonoFont);
    end;
  end;
  // -- var tag
  with CSSBuilder.AddSelector('var.extra') do
  begin
    AddProperty(CSSColorProp(clVarText));
    AddProperty(CSSFontStyleProp(cfsItalic));
  end;
end;

procedure TInfoFrame.DisplayCurViewItem;
  {Displays current view item. This method should not be called directly from
  this class. Instead call inherited UpdateDisplay method which first checks if
  frame is active before calling this method polymorphically.
  }
var
  TextSearchCriteria: ITextSearchCriteria;  // criteria for any text search
begin
  inherited;  // Load page and update dynamically as required
  // If we're viewing a snippet and there's an active text search, highlight
  // text that matches search
  if (CurrentView.Kind = vkRoutine) and
    Supports(
      Query.CurrentSearch.Criteria, ITextSearchCriteria, TextSearchCriteria
    ) then
    HighlightSearchResults(TextSearchCriteria);
end;

function TInfoFrame.GetPageKind: TDetailPageKind;
  {Gets page kind and provides to base class.
    @return Page kind.
  }
begin
  Result := pkInfo;
end;

procedure TInfoFrame.HighlightSearchResults(
  const Criteria: ITextSearchCriteria);
  {Highlights words in current snippet document that match text search criteria.
    @param Criteria [in] Text search criteria.
  }
var
  Highlighter: TWBHighlighter;  // object used to perform highlighting
begin
  Assert(Assigned(Criteria),
    ClassName + '.HighlightSearchResults: Criteria is nil');
  Assert(Supports(Criteria, ITextSearchCriteria),
    ClassName + '.HighlightSearchResults: There is no current text search');
  Assert(CurrentView.Kind = vkRoutine,
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
    FreeAndNil(Highlighter);
  end;
end;

end.


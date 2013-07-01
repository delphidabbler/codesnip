{
 * FrInfo.pas
 *
 * Frame that displays and manages user interaction with the pane that displays
 * details of routines and other selected view items.
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
 * The Original Code is FrInfo.pas
 *
 * The Initial Developer of the Original Code is Peter Johnson
 * (http://www.delphidabbler.com/).
 *
 * Portions created by the Initial Developer are Copyright (C) 2005-2010 Peter
 * Johnson. All Rights Reserved.
 *
 * Contributors:
 *   NONE
 *
 * ***** END LICENSE BLOCK *****
}


unit FrInfo;


interface


uses
  // Delphi
  OleCtrls, SHDocVw, Classes, Controls, ExtCtrls,
  // Project
  FrDetailView, IntfFrameMgrs, IntfHTMLDocHostInfo, UActiveTextHTML,
  UCSSBuilder, UDetailPageLoader, USearch;


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
  Browser.UHighlighter, UColours, UCSSUtils, UFontHelper, UQuery, UView;


{$R *.dfm}


{ TInfoFrame }

procedure TInfoFrame.BuildCSS(const CSSBuilder: TCSSBuilder);
  {Generates CSS classes specific to HTML displayed in information pane. This
  CSS is added to that provided by parent class.
    @param CSSBuilder [in] Object used to build the CSS code.
  }
var
  ContentFont: TFont;   // default content font for OS
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
  ContentFont := TFont.Create;
  try
    // Add CSS relating to Extra REML code
    TFontHelper.SetContentFont(ContentFont, True);
    TActiveTextHTML.Styles(ContentFont, CSSBuilder);
  finally
    FreeAndNil(ContentFont);
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


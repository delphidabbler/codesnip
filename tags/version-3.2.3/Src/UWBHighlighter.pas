{
 * UWBHighlighter.pas
 *
 * Class that highlights text in web browser that match a search criteria.
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
 * The Original Code is UWBHighlighter.pas
 *
 * The Initial Developer of the Original Code is Peter Johnson
 * (http://www.delphidabbler.com/).
 *
 * Portions created by the Initial Developer are Copyright (C) 2005-2009 Peter
 * Johnson. All Rights Reserved.
 *
 * Contributor(s)
 *   NONE
 *
 * ***** END LICENSE BLOCK *****
}


unit UWBHighlighter;


interface


uses
  // Delphi
  Classes, SHDocVw, MSHTML, Graphics,
  // Project
  USearch;


type

  {
  TWBHighlighter:
    Highlights text in web browser's current document that matches text search
    criteria. The document may be divided into named search sections, and if so
    only the text in the defined sections is highlighted. For example, if we
    have a document containing these HTML fragments:
      <div id="one">Division one text</div>
      <div id="two">Division two text</div>
      <div id="three">Division three text</div>
    and we want only text in division's one and three to be highlighted we would
    add the IDs 'one' and 'three' to the SearchSectionIDs property. If we want
    all text in the document to highlighted we ensure the SearchSectionIDs
    property is emnpty.
  }
  TWBHighlighter = class(TObject)
  strict private
    fWebBrowser: TWebBrowser;
      {Browser control containing document being highlighted}
    fSearchSectionIDs: TStrings;
      {List of IDs of HTML tags enclosing text that is to be highlighted}
    fHighlightBackColor: TColor;
      {Background colour of highlighted text}
    fHighlightTextColor: TColor;
      {Colour of highlighted text}
    fHighLightStyle: string;
      {Current style used to highlight text}
    function IsRangeInSearchSection(const Range: IHTMLTxtRange): Boolean;
      {Checks if an HTML text range is within one of the identifies sections of
      the document that are to be searched. If no sections are defined then all
      ranges are searched.
        @param Range [in] Text range to be searched.
        @return True if text range is included in a search section.
      }
    function HighlightWord(const Word: string;
      const FindFlags: Integer): Integer;
      {Searches document for a word. Any words found are highlighted only if
      they fall within one of the designated search sections per the
      SearchSectionIDs property.
        @param Word [in] Word to be highlighted.
        @param FindFlags [in] Flags that customise search.
        @return Number of words highlighted.
      }
    procedure SetSearchSectionIDs(const Value: TStrings);
      {Write accessor for SearchSectionIDs property. Copies strings from new
      value to property.
        @param Value [in] String list containing IDs. If nil the property is
          cleared.
      }
    procedure SetHighlightBackColor(const Value: TColor);
      {Write accessor for HighlightBackColor property. Updates highlight colour.
        @param Value [in] New background colour.
      }
    procedure SetHighlightTextColor(const Value: TColor);
      {Write accessor for HighlightTextColor property. Updates highlight colour.
        @param Value [in] New text colour.
      }
    procedure UpdateHighlightStyle;
      {Updates CSS code that defines style used for highlighted text using
      values of HighlightTextColor and HighlightBackColor properties.
      }
    function SearchOptionsToFlags(const Options: TTextSearchOptions): Integer;
      {Converts text search options into equivalent bit mask used by web browser
      control to modify how search is performed.
        @param Options [in] Set of text search options to convert.
        @return Equivalent bit mask.
      }
  public
    constructor Create(const WebBrowser: TWebBrowser);
      {Class constructor. Sets up the object.
        @param WebBrowser [in] Browser control containing document to be
          highlighted.
        @except EBug raised if browser control does not contain a HTML document.
      }
    destructor Destroy; override;
      {Class destructor. Tears down object.
      }
    function HighlightSearchResults(
      const Criteria: ITextSearchCriteria): Integer;
      {Searches for and highlights all text in document within defined search
      sections that matches search criteria.
        @param Criteria [in] Provides information about a text search.
        @return Number of words highlighted.
      }
    property SearchSectionIDs: TStrings
      read fSearchSectionIDs write SetSearchSectionIDs;
      {List of IDs of HTML tags that enclose text which is to be highlighted. If
      the list is non-empty only the text enclosed by the identified tags is
      highlighted. If the list is empty then found text in the whole document is
      highlighted}
    property HighlightBackColor: TColor
      read fHighlightBackColor write SetHighlightBackColor default clYellow;
      {Background colour of highlight text}
    property HighlightTextColor: TColor
      read fHighlightTextColor write SetHighlightTextColor default clNone;
      {Colour of highlighted text}
  end;


implementation


uses
  // Delphi
  SysUtils,
  // Project
  UColours, UCSSUtils, UHTMLDocHelper, UHTMLUtils, UWBHelper;


{ TWBHighlighter }

constructor TWBHighlighter.Create(const WebBrowser: TWebBrowser);
  {Class constructor. Sets up the object.
    @param WebBrowser [in] Browser control containing document to be
      highlighted.
    @except EBug raised if browser control does not contain a valid HTML
      document.
  }
begin
  Assert(Assigned(WebBrowser), ClassName + '.Create: WebBrowser is nil');
  inherited Create;
  // Record reference to web browser control
  fWebBrowser := WebBrowser;
  // Set up properties
  fSearchSectionIDs := TStringList.Create;
  fHighlightBackColor := clTextSearchHighlight;
  fHighlightTextColor := clTextSearchText;
  UpdateHighlightStyle;
  // Ensure browser has loaded document before continuing
  TWBHelper.WaitForValidDocToLoad(fWebBrowser);                // can raise EBug
end;

destructor TWBHighlighter.Destroy;
  {Class destructor. Tears down object.
  }
begin
  fSearchSectionIDs.Free;
  inherited;
end;

function TWBHighlighter.HighlightSearchResults(
  const Criteria: ITextSearchCriteria): Integer;
  {Searches for and highlights all text in document within defined search
  sections that matches search criteria.
    @param Criteria [in] Provides information about a text search.
    @return Number of words highlighted.
  }
var
  Idx: Integer;             // loops thru all words in text search criteria
  FindFlags: Integer;       // flags that control how browser searches for text
begin
  // Initialise count
  Result := 0;
  if Assigned(Criteria) then
  begin
    // Convert search options into bit mask that controls text search in web
    // browser control
    FindFlags := SearchOptionsToFlags(Criteria.Options);
    // Highlight each search word
    for Idx := 0 to Pred(Criteria.Words.Count) do
      Result := Result + HighlightWord(Criteria.Words[Idx], FindFlags);
  end;
end;

function TWBHighlighter.HighlightWord(const Word: string;
  const FindFlags: Integer): Integer;
  {Searches document for a word. Any words found are highlighted only if they
  fall within one of the designated search sections per the SearchSectionIDs
  property.
    @param Word [in] Word to be highlighted.
    @param FindFlags [in] Flags that customise search.
    @return Number of words highlighted.
  }
var
  Range: IHTMLTxtRange;       // HTML range to be searched for the word
  SpanAttrs: IHTMLAttributes; // attributes of generated span tag
begin
  Range := THTMLDocHelper.CreateBodyTextRange(fWebBrowser.Document);
  Assert(Assigned(Range), ClassName + '.HighlightWord: Range is nil');
  // Set up counter of highlighted items
  Result := 0;
  // Find each word according to find flags
  while Range.findText(Word, 1, FindFlags) do
  begin
    // Check that found text is in a search section
    if IsRangeInSearchSection(Range) then
    begin
      // Apply highlight to found text by spanning it with highlight style
      SpanAttrs := THTMLAttributes.Create;
      SpanAttrs.Add('style', fHighLightStyle);
      Range.pasteHTML(MakeCompoundTag('span', SpanAttrs, Range.htmlText));
      // Count the found text item
      Inc(Result);
    end
    else
      // Ignoring this word - not in search section: skip over it
      Range.move('character', 1);
  end;
end;

function TWBHighlighter.IsRangeInSearchSection(
  const Range: IHTMLTxtRange): Boolean;
  {Checks if an HTML text range is within one of the identifies sections of the
  document that are to be searched. If no sections are defined then all ranges
  are searched.
    @param Range [in] Text range to be searched.
    @return True if text range is included in a search section.
  }
var
  Elem: IHTMLElement; // an HTML element in range
begin
  Assert(Assigned(Range), ClassName + '.IsRangeInSearchSection: Range is nil');
  // Check if we have any specified search sections to search - if none provided
  // then we class whole document as a single search section
  if fSearchSectionIDs.Count > 0 then
  begin
    // Assume failure
    Result := False;
    // Loop thru all HTML elements from current range up to <body> tag, looking
    // for enclosing tag with one of IDs that define a search section. Returns
    // true if element found
    Elem := Range.parentElement;
    while Assigned(Elem) and not AnsiSameText(Elem.tagName, 'body') do
    begin
      if fSearchSectionIDs.IndexOf(Elem.id) >= 0 then
      begin
        Result := True;
        Break;
      end;
      Elem := Elem.parentElement;
    end;
  end
  else
    // There are no specified search sections => all elements are searched
    Result := True;
end;

function TWBHighlighter.SearchOptionsToFlags(
  const Options: TTextSearchOptions): Integer;
  {Converts text search options into equivalent bit mask used by web browser
  control to modify how search is performed.
    @param Options [in] Set of text search options to convert.
    @return Equivalent bit mask.
  }
var
  Option: TTextSearchOption;  // loops thru all search options
const
  // Map of search options to find flags
  cFlags: array[TTextSearchOption] of Integer = (
    FINDTEXT_MATCHCASE, FINDTEXT_WHOLEWORD
  );
begin
  Result := 0;
  for Option := Low(TTextSearchOption) to High(TTextSearchOption) do
    if Option in Options then
      Result := Result or cFlags[Option];
end;

procedure TWBHighlighter.SetHighlightBackColor(const Value: TColor);
  {Write accessor for HighlightBackColor property. Updates highlight colour.
    @param Value [in] New background colour.
  }
begin
  if fHighlightBackColor <> Value then
  begin
    fHighlightBackColor := Value;
    UpdateHighlightStyle;
  end;
end;

procedure TWBHighlighter.SetHighlightTextColor(const Value: TColor);
  {Write accessor for HighlightTextColor property. Updates highlight colour.
    @param Value [in] New text colour.
  }
begin
  if fHighlightTextColor <> Value then
  begin
    fHighlightTextColor := Value;
    UpdateHighlightStyle;
  end;
end;

procedure TWBHighlighter.SetSearchSectionIDs(const Value: TStrings);
  {Write accessor for SearchSectionIDs property. Copies strings from new value
    to property.
    @param Value [in] String list containing IDs. If nil the property is
      cleared.
  }
begin
  if Assigned(Value) then
    fSearchSectionIDs.Assign(Value)
  else
    fSearchSectionIDs.Clear;
end;

procedure TWBHighlighter.UpdateHighlightStyle;
  {Updates CSS code that defines style used for highlighted text using values of
  HighlightTextColor and HighlightBackColor properties.
  }
begin
  fHighlightStyle := '';
  if fHighlightBackColor <> clNone then
    fHighlightStyle := fHighlightStyle +
      CSSBackgroundColorProp(fHighlightBackColor);
  if fHighlightTextColor <> clNone then
    fHighlightStyle := fHighlightStyle +
      CSSColorProp(fHighlightTextColor);
end;

end.


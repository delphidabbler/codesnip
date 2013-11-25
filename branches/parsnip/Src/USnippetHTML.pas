{
 * This Source Code Form is subject to the terms of the Mozilla Public License,
 * v. 2.0. If a copy of the MPL was not distributed with this file, You can
 * obtain one at http://mozilla.org/MPL/2.0/
 *
 * Copyright (C) 2006-2013, Peter Johnson (www.delphidabbler.com).
 *
 * $Rev$
 * $Date$
 *
 * Classes that generates HTML used to display snippets in detail pane.
}


unit USnippetHTML;


interface


uses
  // Project
  CS.ActiveText,
  DB.UCategory,
  DB.USnippet,
  USnippetIDs;


type
  ///  <summary>
  ///  Provides HTML used to display snippet details in information pane.
  ///  </summary>
  TSnippetHTML = class sealed(TObject)
  strict private
    var
      ///  <summary>Reference to snippet for which HTML is being generated.
      ///  </summary>
      fSnippet: TSnippet;
    ///  <summary>Generates HTML of a comma separated list of snippets, where
    ///  each snippet name is a link to the snippet.</summary>
    ///  <param name="Snippets">TSnippetList [in] List of snippets.</param>
    ///  <returns>string. HTML of snippet list or 'None' if list empty.
    ///  </returns>
    function SnippetList(SnippetIDs: ISnippetIDList): string;
    ///  <summary>Returns HTML text indicating a list is empty.</summary>
    function EmptyListSentence: string;
    ///  <summary>Renders given active text as HTML and returns it.</summary>
    function RenderActiveText(ActiveText: IActiveText): string;
    ///  <summary>Returns HTML of link to given snippet.</summary>
    class function SnippetALink(const Snippet: TSnippet): string; overload;
    ///  <summary>Returns HTML of a link that performs its action using
    ///  JavaScript.</summary>
    ///  <param name="JSFn">string [in] Javascript function to be called when
    ///  link is clicked.</param>
    ///  <param name="CSSClass">string [in] CSS class of link.</param>
    ///  <param name="Text">string [in] Text to be displayed in link.</param>
    ///  <returns>string. Required HTML.</returns>
    class function JSALink(const JSFn, CSSClass, Text: string): string;
  public
    ///  <summary>Object constructor. Sets up object to provide HTML for given
    ///  snippet.</summary>
    constructor Create(const Snippet: TSnippet);
    ///  <summary>Returns snippet display name as HTML.</summary>
    function SnippetName: string;
    ///  <summary>Returns snippet description as HTML.</summary>
    function Description: string;
    ///  <summary>Returns description of snippet's kind as HTML.</summary>
    function SnippetKind: string;
    ///  <summary>Returns HTML of a link to category containing snippet.
    ///  </summary>
    function Category: string;
    ///  <summary>Highlights snippet's source code and returns it as HTML.
    ///  </summary>
    function SourceCode: string;
    ///  <summary>Returns HTML of a comma separated list of links to snippets on
    ///  which the snippet depends, or a message if list is empty.</summary>
    function Depends: string;
    ///  <summary>Returns HTML of a comma separated list of links to snippet's
    ///  cross-referenced snippets, or a message if list is empty.</summary>
    function XRefs: string;
    ///  <summary>Returns HTML of a comma separated list of units required by
    ///  snippet, or a messafe if list is empty.</summary>
    function Units: string;
    ///  <summary>Returns HTML representation of active text from snippet's
    ///  Notes property.</summary>
    function Notes: string;
    ///  <summary>Returns HTML containing rows of a table representing snippet's
    ///  compilation results for each supported compiler.</summary>
    function CompileResults: string;
    ///  <summary>Returns HTML of link to snippet.</summary>
    function SnippetALink: string; overload;
    ///  <summary>Returns an image tag referenceing the image used to display
    ///  the snippet's test information.</summary>
    function TestingImage: string;
  end;


implementation


uses
  // Delphi
  SysUtils,
  // Project
  CS.ActiveText.Renderers.HTML,
  CS.Config,
  CS.SourceCode.Languages,
  CS.SourceCode.Hiliter.Brushes,
  CS.SourceCode.Hiliter.Renderers,
  DB.UMain,
  DB.USnippetKind,
  UCompResHTML,
  UHTMLBuilder,
  UHTMLUtils,
  UIStringList,
  UJavaScriptUtils,
  UPreferences,
  UResourceUtils,
  UStrUtils;


{ TSnippetHTML }

function TSnippetHTML.Category: string;
var
  Cat: TCategory; // category that snippet belongs to
begin
  Cat := Database.Categories.Find(fSnippet.Category);
  Assert(Assigned(Cat), ClassName + '.Category: Category not found');
  Result := StrMakeSentence(
    JSALink(
      TJavaScript.LiteralFunc('displayCategory', [Cat.ID]),
      'category-link',
      Cat.Description
    )
  );
end;

function TSnippetHTML.CompileResults: string;
begin
  Result := TCompResHTML.TableRows(fSnippet.Compatibility);
end;

constructor TSnippetHTML.Create(const Snippet: TSnippet);
begin
  inherited Create;
  fSnippet := Snippet;
end;

function TSnippetHTML.Depends: string;
begin
  Result := SnippetList(fSnippet.RequiredSnippets);
end;

function TSnippetHTML.Description: string;
begin
  Result := RenderActiveText(fSnippet.Description);
end;

function TSnippetHTML.EmptyListSentence: string;
resourcestring
  sEmpty = 'None';
begin
  Result := THTML.Entities(StrMakeSentence(sEmpty));
end;

class function TSnippetHTML.JSALink(const JSFn, CSSClass, Text: string):
  string;
var
  Attrs: IHTMLAttributes;
begin
  Attrs := THTMLAttributes.Create([
    THTMLAttribute.Create('href', '#'),
    THTMLAttribute.Create('onclick', JSFn + '; return false;'),
    THTMLAttribute.Create('class', CSSClass)
  ]);
  Result := THTML.CompoundTag('a', Attrs, THTML.Entities(Text));
end;

function TSnippetHTML.Notes: string;
begin
  Result := RenderActiveText(fSnippet.Notes);
end;

function TSnippetHTML.RenderActiveText(ActiveText: IActiveText): string;
begin
  Result := TActiveTextHTMLRenderer.Render(ActiveText);
end;

function TSnippetHTML.SnippetList(SnippetIDs: ISnippetIDList): string;
var
  SnippetID: TSnippetID;
begin
  if SnippetIDs.IsEmpty then
    // There are no snippets: say so
    Result := EmptyListSentence
  else
  begin
    // Build comma separated list of snippet links
    Result := '';
    for SnippetID in SnippetIDs do
    begin
      if Result <> '' then
        Result := Result + ', ';
      Result := Result + SnippetALink(Database.Lookup(SnippetID));
    end;
    Result := StrMakeSentence(Result);
  end;
end;

function TSnippetHTML.SnippetName: string;
begin
  Result := THTML.Entities(fSnippet.DisplayName);
end;

class function TSnippetHTML.SnippetALink(const Snippet: TSnippet): string;
begin
  // Create javascript link enclosing snippet name
  Result := JSALink(
    TJavaScript.LiteralFunc('displaySnippet', [Snippet.ID.ToString]),
    'snippet-link',
    Snippet.DisplayName
  );
end;

function TSnippetHTML.SnippetALink: string;
begin
  Result := SnippetALink(fSnippet);
end;

function TSnippetHTML.SnippetKind: string;
begin
  Result := THTML.Entities(
    StrMakeSentence(TSnippetKindInfoList.Items[fSnippet.Kind].DisplayName)
  );
end;

function TSnippetHTML.SourceCode: string;
var
  Builder: THTMLBuilder;      // object that assembles HTML
  Renderer: IHiliteRenderer;  // object that renders highlighted code as HTML
  Lang: TSourceCodeLanguage;  // source code language
  Brush: TSyntaxHiliterBrush; // brush used to perform highlighting
begin
  Lang := TConfig.Instance.SourceCodeLanguages[fSnippet.Language];
  Builder := THTMLBuilder.Create;
  try
    Brush := TSyntaxHiliterBrushes.CreateBrush(Lang.HiliterBrushID);
    try
      Renderer := THTMLHiliteRenderer.Create(
        Builder,
        Brush,
        TConfig.Instance.HiliterThemes[Preferences.CurrentHiliteThemeIds[htkUI]]
      );
      TSyntaxHiliter.Hilite(
        fSnippet.SourceCode,
        Brush,
        Renderer
      );
    finally
      Brush.Free;
    end;
    Result := Builder.HTMLFragment;
  finally
    Builder.Free;
  end;
end;

function TSnippetHTML.TestingImage: string;
resourcestring
  sTestingNone = 'Untested'#13#10'Use with care';
  sTestingBasic = 'Passed simple tests';
  sTestingAdvanced = 'Passed advanced / unit testing';
const
  ImgWidth = 16;
  ImgHeight = 16;
  ImgSrcs: array[TSnippetTestInfo] of record
    ResName: string;    // name of image resource
    Title: string;      // value of image tag title attribute
  end =(
    (ResName: 'testing-none.png';     Title: sTestingNone),
    (ResName: 'testing-basic.png';    Title: sTestingBasic),
    (ResName: 'testing-advanced.png'; Title: sTestingAdvanced)
  );
var
  Attrs: IHTMLAttributes; // image's attributes
begin
  Attrs := THTMLAttributes.Create;
  Attrs.Add('src', MakeResourceURL(ImgSrcs[fSnippet.TestInfo].ResName));
  Attrs.Add('title', THTML.Entities(ImgSrcs[fSnippet.TestInfo].Title));
  Attrs.Add('class', 'testing-img');
  Result := THTML.SimpleTag('img', Attrs);
end;

function TSnippetHTML.Units: string;
begin
  if fSnippet.RequiredModules.Count = 0 then
    Result := EmptyListSentence
  else
    Result := THTML.Entities(fSnippet.RequiredModules.GetText(', ', False));
end;

function TSnippetHTML.XRefs: string;
begin
  Result := SnippetList(fSnippet.XRefs);
end;

end.



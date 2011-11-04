{
 * UDetailPageHTML.pas
 *
 * Heirachy of classes that render views as HTML. The HTML is used to display
 * the view item in a tab in the detail pane. A factory is provided that can
 * create the correct object for any type of view item.
 *
 * Originally named UHTMLGenerators.pas. Renamed as UDetailPageHTML.pas as of
 * v2.0
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
 * The Original Code is UDetailPageHTML.pas
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


unit UDetailPageHTML;


interface


uses
  // Project
  UView;


type
  ///  <summary>
  ///  Abstract base class for all classes that render a view item as HTML for
  ///  inclusion in the body of an HTML document displayed in the detail pane.
  ///  </summary>
  TDetailPageHTML = class abstract(TObject)
  strict private
    var
      ///  <summary>Value of View property.</summary>
      fView: IView;
  strict protected
    ///  <summary>Reference to view item that is to be rendered in HTML.
    ///  </summary>
    ///  <remarks>Provided for use by sub-classes.</remarks>
    property View: IView read fView;
  public
    ///  <summary>Object constructor. Sets up object to render given view in
    ///  HTML.</summary>
    constructor Create(View: IView); virtual;
    ///  <summary>Generates and returns HTML representing view passed to
    ///  constructor.</summary>
    function Generate: string; virtual; abstract;
  end;

type
  ///  <summary>
  ///  Factory for creation of TDetailPageHTML objects for use in rendering a
  ///  view as HTML for display in detail pane.
  ///  </summary>
  TDetailPageHTMLFactory = record
  public
    ///  <summary>Creates and returns object that can render given view as HTML
    ///  for display in detail pane.</summary>
    class function CreateGenerator(View: IView): TDetailPageHTML; static;
  end;


implementation


uses
  // Delphi
  SysUtils,
  // Project
  Compilers.UCompilers, Compilers.UGlobals, DB.UMain, DB.USnippet, UCompResHTML,
  UConsts, UCSSUtils, UHTMLDetailUtils, UHTMLTemplate, UHTMLUtils,
  UJavaScriptUtils, UQuery, USnippetHTML, UStrUtils;


type
  ///  <summary>
  ///  Abstract base class for classes that generate a view's HTML by updating a
  ///  template stored in resources.
  ///  </summary>
  TDetailPageTpltHTML = class abstract(TDetailPageHTML)
  strict protected
    ///  <summary>Returns name of resource containing template.</summary>
    ///  <remarks>Resource must be stored as HTML resource.</remarks>
    function GetTemplateResName: string; virtual; abstract;
    ///  <summary>Replaces place-holders in a template with suitable values,
    ///  depending on view.</summary>
    ///  <param name="Tplt">THTMLTemplate [in] Object containing template to
    ///  be updated.</param>
    ///  <remarks>Implementors must replace every placeholder in template with
    ///  required values. This is done by manipulating Tplt object.</remarks>
    procedure ResolvePlaceholders(const Tplt: THTMLTemplate); virtual; abstract;
  public
    ///  <summary>Generates and returns HTML representing view passed to
    ///  constructor.</summary>
    function Generate: string; override;
  end;

type
  ///  <summary>
  ///  Generates no HTML.
  ///  </summary>
  ///  <remarks>
  ///  Used whenever a blank HTML page is required.
  ///  </remarks>
  TNulPageHTML = class sealed(TDetailPageHTML)
  public
    ///  <summary>Returns empty string, representing an empty document body.
    ///  </summary>
    function Generate: string; override;
  end;

type
  ///  <summary>
  ///  Generates HTML body for page displayed for a new, empty, detail pane tab.
  ///  </summary>
  TNewTabPageHTML = class sealed(TDetailPageHTML)
  public
    ///  <summary>Returns fixed HTML informing of a new, empty tab.</summary>
    function Generate: string; override;
  end;

type
  ///  <summary>
  ///  Generates HTML body of welcome page.
  ///  </summary>
  TWelcomePageHTML = class sealed(TDetailPageTpltHTML)
  strict protected
    ///  <summary>Returns name of welcome page template resource.</summary>
    function GetTemplateResName: string; override;
    ///  <summary>Replaces place-holders in welcome page template with suitable
    ///  values.</summary>
    procedure ResolvePlaceholders(const Tplt: THTMLTemplate); override;
  end;

type
  ///  <summary>
  ///  Generates HTML body of page displayed after database has been updated.
  ///  </summary>
  TDBUpdatedPageHTML = class sealed(TDetailPageHTML)
  public
    ///  <summary>Returns fixed HTML informaing that database has been updated.
    ///  </summary>
    function Generate: string; override;
  end;

type
  ///  <summary>
  ///  Abstract base class for classes that generate HTML bodies for pages that
  ///  describe a snippet.
  ///  </summary>
  ///  <remarks>
  ///  View passed to constructor must be a valid snippet view.
  ///  </remarks>
  TSnippetPageHTML = class abstract(TDetailPageTpltHTML)
  strict private
    var
      ///  <summary>Value of CompilerInfo property.</summary>
      fCompilersInfo: ICompilers;
  strict protected
    ///  <summary>Returns name of resource containing template used to render
    ///  snippet.</summary>
    function GetTemplateResName: string; override; abstract;
    ///  <summary>Replaces place-holders in given template with suitable values.
    ///  </summary>
    procedure ResolvePlaceholders(const Tplt: THTMLTemplate); override;
    ///  <summary>Returns reference to snippet that is being rendered.</summary>
    ///  <remarks>Snippet is recorded in View property.</remarks>
    function GetSnippet: TSnippet;
    ///  <summary>Provides information about available compilers.</summary>
    ///  <remarks>Provided for use by sub-classes.</remarks>
    property CompilersInfo: ICompilers read fCompilersInfo;
  public
    ///  <summary>Object constructor. Sets up object to render snippet recorded
    ///  in given view.</summary>
    ///  <remarks>View must represent a snippet.</remarks>
    constructor Create(View: IView); override;
  end;

type
  // TODO: Collapse this class into base class, which has no other descendants
  ///  <summary>
  ///  Generates HTML body of a page that provides information about a snippet.
  ///  </summary>
  TSnippetInfoPageHTML = class sealed(TSnippetPageHTML)
  strict protected
    ///  <summary>Returns name of snippet information template resource.
    ///  </summary>
    function GetTemplateResName: string; override;
    ///  <summary>Replaces place-holders in snippet information template with
    ///  suitable values.</summary>
    procedure ResolvePlaceholders(const Tplt: THTMLTemplate); override;
  end;

type
  ///  <summary>
  ///  Abstract base class for classes that generate HTML bodies for pages that
  ///  display a list of snippets.
  ///  </summary>
  ///  <remarks>
  ///  Snippet list may be empty.
  ///  </remarks>
  TSnippetListPageHTML = class abstract(TDetailPageTpltHTML)
  strict private
    var
      ///  <summary>Value of Snippets property.</summary>
      fSnippets: TSnippetList;
  strict protected
    ///  <summary>Creates and returns a sequence of HTML table rows each
    ///  containing a link to one of the snippets in the list, along with its
    ///  description.</summary>
    function SnippetTableInner: string;
    ///  <summary>Creates and returns an HTML table row containing one cell that
    ///  links to given snippet and another containing snippet's description.
    ///  </summary>
    function SnippetTableRow(const Snippet: TSnippet): string;
    ///  <summary>Constructs a list of all snippets to be displayed and stores
    ///  in in Snippets property.</summary>
    procedure BuildSnippetList; virtual; abstract;
    ///  <summary>List of all snippets to be displayed.</summary>
    property Snippets: TSnippetList read fSnippets;
    ///  <summary>Returns name of template resource for either an empty or none-
    ///  empty list of snippets.</summary>
    function GetTemplateResName: string; override;
    ///  <summary>Replaces place-holders in chosen template with suitable
    ///  values.</summary>
    procedure ResolvePlaceholders(const Tplt: THTMLTemplate); override;
      abstract;
    ///  <summary>Checks if there are snippets in snippets list.</summary>
    function HaveSnippets: Boolean;
  public
    ///  <summary>Object constructor. Sets up object to render snippet list
    ///  represented by given view.</summary>
    ///  <remarks>View must contain a list of snippets which may be empty.
    ///  </remarks>
    constructor Create(View: IView); override;
    ///  <summary>Object destructor. Tears down object.</summary>
    destructor Destroy; override;
  end;

type
  ///  <summary>
  ///  Generates HTML body of a page that displays information about a category
  ///  grouping of snippets.
  ///  </summary>
  ///  <remarks>
  ///  List of snippets contained in grouping may be empty.
  ///  </remarks>
  TCategoryPageHTML = class sealed(TSnippetListPageHTML)
  strict protected
    ///  <summary>Replaces place-holders in chosen template with suitable
    ///  values.</summary>
    procedure ResolvePlaceholders(const Tplt: THTMLTemplate); override;
    ///  <summary>Stores all snippets in category grouping in Snippets
    ///  property.</summary>
    procedure BuildSnippetList; override;
  end;

type
  ///  <summary>
  ///  Generates HTML body of a page that displays information about an
  ///  alphabetical grouping of snippets.
  ///  </summary>
  ///  <remarks>
  ///  List of snippets contained in grouping may be empty.
  ///  </remarks>
  TAlphaListPageHTML = class sealed(TSnippetListPageHTML)
  strict protected
    ///  <summary>Replaces place-holders in chosen template with suitable
    ///  values.</summary>
    procedure ResolvePlaceholders(const Tplt: THTMLTemplate); override;
    ///  <summary>Stores all snippets in alphabetical grouping in Snippets
    ///  property.</summary>
    procedure BuildSnippetList; override;
  end;

type
  ///  <summary>
  ///  Generates HTML body of a page that displays information about a grouping
  ///  of snippets by snippet kind.
  ///  </summary>
  ///  <remarks>
  ///  List of snippets contained in grouping may be empty.
  ///  </remarks>
  TSnipKindPageHTML = class sealed(TSnippetListPageHTML)
  strict protected
    ///  <summary>Replaces place-holders in chosen template with suitable
    ///  values.</summary>
    procedure ResolvePlaceholders(const Tplt: THTMLTemplate); override;
    ///  <summary>Stores all snippets in snippet kind grouping in Snippets
    ///  property.</summary>
    procedure BuildSnippetList; override;
  end;

{ TDetailPageHTMLFactory }

class function TDetailPageHTMLFactory.CreateGenerator(
  View: IView): TDetailPageHTML;
begin
  Result := nil;
  if Supports(View, INulView) then
    Result := TNulPageHTML.Create(View)
  else if Supports(View, IStartPageView) then
    Result := TWelcomePageHTML.Create(View)
  else if Supports(View, ISnippetView) then
    Result := TSnippetInfoPageHTML.Create(View)
  else if Supports(View, ICategoryView) then
    Result := TCategoryPageHTML.Create(View)
  else if Supports(View, ISnippetKindView) then
    Result := TSnipKindPageHTML.Create(View)
  else if Supports(View, IInitialLetterView) then
    Result := TAlphaListPageHTML.Create(View)
  else if Supports(View, INewTabView) then
    Result := TNewTabPageHTML.Create(View)
  else if Supports(View, IDBUpdateInfoView) then
    Result := TDBUpdatedPageHTML.Create(View);
  Assert(Assigned(Result),
    'TDetailPageHTMLFactory.CreateGenerator: No HTML generator');
end;

{ TDetailPageHTML }

constructor TDetailPageHTML.Create(View: IView);
begin
  Assert(Assigned(View), ClassName + '.Create: View is nil');
  inherited Create;
  fView := View;
end;

{ TDetailPageTpltHTML }

function TDetailPageTpltHTML.Generate: string;
var
  Tplt: THTMLTemplate;  // encapsulates HTML template
begin
  // Create template object from required HTML template resource
  Tplt := THTMLTemplate.Create(HInstance, GetTemplateResName);
  try
    // Resolve all placeholders and write resulting HTML to stream
    ResolvePlaceholders(Tplt);
    Result := Tplt.HTML;
  finally
    Tplt.Free;
  end;
end;

{ TNulPageHTML }

function TNulPageHTML.Generate: string;
begin
  Result := '';
end;

{ TNewTabPageHTML }

function TNewTabPageHTML.Generate: string;
begin
  Result := MakeCompoundTag(
    'div',
    THTMLAttributes.Create('id', 'newtab'),
    MakeSafeHTMLText(View.Description)
  );
end;

{ TWelcomePageHTML }

function TWelcomePageHTML.GetTemplateResName: string;
begin
  Result := 'welcome-tplt.html';
end;

procedure TWelcomePageHTML.ResolvePlaceholders(const Tplt: THTMLTemplate);
var
  HaveMainDB: Boolean;  // flag indicating if main database is available
  HaveUserDB: Boolean;  // flag indicating if user database has entries
begin
  HaveMainDB := Database.Snippets.Count(False) > 0;
  HaveUserDB := Database.Snippets.Count(True) > 0;
  Tplt.ResolvePlaceholderHTML(
    'NoUserDB', CSSBlockDisplayProp(not HaveUserDB)
  );
  Tplt.ResolvePlaceholderHTML(
    'NoMainDB', CSSBlockDisplayProp(not HaveMainDB)
  );
  Tplt.ResolvePlaceholderHTML(
    'Intro', CSSBlockDisplayProp(HaveMainDB or HaveUserDB)
  );
  Tplt.ResolvePlaceholderHTML(
    'Disclaimer', CSSBlockDisplayProp(HaveMainDB)
  );
  Tplt.ResolvePlaceholderHTML(
    'UpdateDB', CSSBlockDisplayProp(HaveMainDB)
  );
  Tplt.ResolvePlaceholderHTML(
    'DownloadDB', CSSBlockDisplayProp(not HaveMainDB)
  );
end;

{ TDBUpdatedPageHTML }

function TDBUpdatedPageHTML.Generate: string;
resourcestring
  sBody = 'The database has been updated successfully.';
begin
  Result :=
    MakeCompoundTag('h1', View.Description)
    +
    MakeCompoundTag('p', sBody);
end;

{ TSnippetPageHTML }

constructor TSnippetPageHTML.Create(View: IView);
begin
  inherited;
  // create compilers info object
  fCompilersInfo := TCompilersFactory.CreateAndLoadCompilers;
end;

function TSnippetPageHTML.GetSnippet: TSnippet;
begin
  Assert(Supports(View, ISnippetView),
    ClassName + '.GetSnippet: View is not snippet');
  Result := (View as ISnippetView).Snippet;
end;

procedure TSnippetPageHTML.ResolvePlaceholders(const Tplt: THTMLTemplate);
var
  SnippetHTML: TSnippetHTML;  // object used to generate HTML
begin
  // Resolve placeholders common to all snippet templates:
  // snippet name and class
  if GetSnippet.UserDefined then
    Tplt.ResolvePlaceholderHTML('SnippetCSSClass', 'userdb')
  else
    Tplt.ResolvePlaceholderHTML('SnippetCSSClass', 'maindb');
  SnippetHTML := TSnippetHTML.Create(GetSnippet);
  try
    Tplt.ResolvePlaceholderHTML('SnippetName', SnippetHTML.SnippetName);
  finally
    SnippetHTML.Free;
  end;
  // "edit snippet" link for user-defined snippets
  Tplt.ResolvePlaceholderHTML(
    'EditLink', CSSBlockDisplayProp(GetSnippet.UserDefined)
  );
  Tplt.ResolvePlaceholderText(
    'EditEventHandler', JSLiteralFunc('editSnippet', [GetSnippet.Name])
  );
end;

{ TSnippetInfoPageHTML }

function TSnippetInfoPageHTML.GetTemplateResName: string;
begin
  Result := 'info-snippet-tplt.html';
end;

procedure TSnippetInfoPageHTML.ResolvePlaceholders(const Tplt: THTMLTemplate);

  ///  Generates and returns inner HTML (rows) of compiler table.
  function CompilerTableInner: string;
  var
    Compiler: ICompiler;    // reference to each compiler
    Row1, Row2: string;     // HTML for two rows in HTML table
  begin
    // Initialise HTML for two rows of table and resulting table HTML
    Row1 := MakeTag('tr', ttOpen);
    Row2 := MakeTag('tr', ttOpen);
    Result := '';

    // Loop thru each supported compiler
    for Compiler in CompilersInfo do
    begin
      // Add table cell for compiler name to 1st row of table
      Row1 := Row1 + TInfoCompResHTML.NameCell(Compiler) + EOL;
      // Add table cell containing required LED image to 2nd row of table
      Row2 := Row2
        + TInfoCompResHTML.ResultCell(GetSnippet.Compatibility[Compiler.GetID])
        + EOL;
    end;

    // Close the two rows
    Row1 := Row1 + MakeTag('tr', ttClose);
    Row2 := Row2 + MakeTag('tr', ttClose);

    // Return HTML of two rows
    Result := Row1 + Row2;
  end;

var
  InfoHTML: TInfoHTML;  // object used to generate HTML
begin
  inherited;
  InfoHTML := TInfoHTML.Create(GetSnippet);
  try
    Tplt.ResolvePlaceholderHTML('Kind', InfoHTML.SnippetKind);
    Tplt.ResolvePlaceholderHTML('Category', InfoHTML.Category);
    Tplt.ResolvePlaceholderHTML('Description', InfoHTML.Description);
    Tplt.ResolvePlaceholderHTML('SourceCode', InfoHTML.SourceCode);
    Tplt.ResolvePlaceholderHTML('Units', InfoHTML.Units);
    Tplt.ResolvePlaceholderHTML('Depends', InfoHTML.Depends);
    Tplt.ResolvePlaceholderHTML('XRefs', InfoHTML.XRefs);
    Tplt.ResolvePlaceholderHTML('CompilerTableRows', CompilerTableInner);
    Tplt.ResolvePlaceholderHTML('Extra', InfoHTML.Extra);
    Tplt.ResolvePlaceholderHTML(
      'ShowCompilations', CSSBlockDisplayProp(GetSnippet.CanCompile)
    );
  finally
    InfoHTML.Free;
  end;
end;

{ TSnippetListPageHTML }

constructor TSnippetListPageHTML.Create(View: IView);
begin
  inherited;
  fSnippets := TSnippetList.Create;
  BuildSnippetList;
end;

destructor TSnippetListPageHTML.Destroy;
begin
  fSnippets.Free;
  inherited;
end;

function TSnippetListPageHTML.GetTemplateResName: string;
begin
  if HaveSnippets then
    Result := 'info-snippet-list-tplt.html'
  else
    Result := 'info-empty-selection-tplt.html';
end;

function TSnippetListPageHTML.HaveSnippets: Boolean;
begin
  Result := Snippets.Count > 0;
end;

function TSnippetListPageHTML.SnippetTableInner: string;
var
  Snippet: TSnippet;  // each snippet in list
begin
  Result := '';
  for Snippet in Snippets do
    Result := Result + SnippetTableRow(Snippet);
end;

function TSnippetListPageHTML.SnippetTableRow(const Snippet: TSnippet): string;
begin
  Result := MakeCompoundTag(
    'tr',
    MakeCompoundTag(
      'td', SnippetALink(Snippet.Name, Snippet.UserDefined)
    )
    + MakeCompoundTag(
      'td', MakeSafeHTMLText(Snippet.Description)
    )
  )
end;

{ TCategoryPageHTML }

procedure TCategoryPageHTML.BuildSnippetList;
begin
  Query.GetCatSelection((View as ICategoryView).Category, Snippets);
end;

procedure TCategoryPageHTML.ResolvePlaceholders(const Tplt: THTMLTemplate);

  ///  Returns name of CSS class used in H1 heading.
  function H1ClassName: string;
  begin
    if (View as ICategoryView).Category.UserDefined then
      Result := 'userdb'
    else
      Result := 'maindb';
  end;

resourcestring
  sNarrative = 'List of selected snippets in this category.';
  sNote = 'The current selection contains no snippets in this category.';
begin
  Tplt.ResolvePlaceholderHTML('H1Class', H1ClassName);
  Tplt.ResolvePlaceholderText('Heading', View.Description);
  if HaveSnippets then
  begin
    Tplt.ResolvePlaceholderText('Narrative', sNarrative);
    Tplt.ResolvePlaceholderHTML('SnippetList', SnippetTableInner);
  end
  else
    Tplt.ResolvePlaceholderText('Note', sNote);
end;

{ TAlphaListPageHTML }

procedure TAlphaListPageHTML.BuildSnippetList;
var
  Snippet: TSnippet;  // each snippet in current query
begin
  Snippets.Clear;
  for Snippet in Query.Selection do
  begin
    if Snippet.Name[1] = (View as IInitialLetterView).InitialLetter then
      Snippets.Add(Snippet);
  end;
end;

procedure TAlphaListPageHTML.ResolvePlaceholders(const Tplt: THTMLTemplate);
resourcestring
  sNarrative = 'List of selected snippets beginning with the letter %s.';
  sNote = 'The are no snippets in the current selection that begin with the '
    + 'letter %s.';
begin
  Tplt.ResolvePlaceholderHTML('H1Class', 'maindb');
  Tplt.ResolvePlaceholderText('Heading', View.Description);
  if HaveSnippets then
  begin
    Tplt.ResolvePlaceholderText(
      'Narrative',
      Format(sNarrative, [(View as IInitialLetterView).InitialLetter.Letter])
    );
    Tplt.ResolvePlaceholderHTML('SnippetList', SnippetTableInner);
  end
  else
    Tplt.ResolvePlaceholderText(
      'Note', Format(sNote, [(View as IInitialLetterView).InitialLetter.Letter])
    );
end;

{ TSnipKindPageHTML }

procedure TSnipKindPageHTML.BuildSnippetList;
var
  Snippet: TSnippet;  // each snippet in current query
begin
  Snippets.Clear;
  for Snippet in Query.Selection do
  begin
    if Snippet.Kind = (View as ISnippetKindView).KindInfo.Kind then
      Snippets.Add(Snippet);
  end;
end;

procedure TSnipKindPageHTML.ResolvePlaceholders(const Tplt: THTMLTemplate);
resourcestring
  sHeading = '%s Snippets';
  sNarrative = 'List of all %s snippets in the current selection.';
  sNote = 'There are no %s snippets in the current selection.';
begin
  Tplt.ResolvePlaceholderHTML('H1Class', 'maindb');
  Tplt.ResolvePlaceholderText(
    'Heading', Format(sHeading, [View .Description])
  );
  if HaveSnippets then
  begin
    Tplt.ResolvePlaceholderText(
      'Narrative',
      Format(sNarrative, [StrToLower(View.Description)])
    );
    Tplt.ResolvePlaceholderHTML('SnippetList', SnippetTableInner);
  end
  else
    Tplt.ResolvePlaceholderText(
      'Note', Format(sNote, [StrToLower(View.Description)])
    );
end;

end.


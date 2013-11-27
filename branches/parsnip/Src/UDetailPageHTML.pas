{
 * This Source Code Form is subject to the terms of the Mozilla Public License,
 * v. 2.0. If a copy of the MPL was not distributed with this file, You can
 * obtain one at http://mozilla.org/MPL/2.0/
 *
 * Copyright (C) 2005-2013, Peter Johnson (www.delphidabbler.com).
 *
 * $Rev$
 * $Date$
 *
 * Heirachy of classes that render views as HTML. The HTML is used to display
 * the view item in a tab in the detail pane. A factory is provided that can
 * create the correct object for any type of view item.
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
  // 3rd party
  Collections.Base,
  Collections.Lists,
  // Project
  Compilers.UGlobals, Compilers.UCompilers, DB.UMain, DB.USnippet, UConsts,
  UCSSUtils, UEncodings, UHTMLTemplate, UHTMLUtils, UJavaScriptUtils,
  UPreferences, UQuery, UResourceUtils, USnippetHTML, USnippetPageHTML,
  UStrUtils, USystemInfo;


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
    ///  <remarks>This method resolves place-holders common to all templates.
    ///  Descendants must replace every remaining place-holder in their
    ///  templates with required values.</remarks>
    procedure ResolvePlaceholders(const Tplt: THTMLTemplate); virtual;
  public
    ///  <summary>Generates and returns HTML representing view passed to
    ///  constructor.</summary>
    function Generate: string; override; final;
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
  ///  <summary>Abstract base class for HTML pages that rely on a simple HTML
  ///  template that has no scripts.</summary>
  ///  <remarks>Subclasses must provide the whole content of the HTML document
  ///  body.</remarks>
  TBasicPageTpltHTML = class abstract(TDetailPageTpltHTML)
  strict protected
    ///  <summary>Returns name of HTML resource containing the basic template.
    ///  </summary>
    function GetTemplateResName: string; override;
    ///  <summary>Replaces place-holders in basic with suitable values.
    ///  </summary>
    ///  <param name="Tplt">THTMLTemplate [in] Object containing template to
    ///  be updated.</param>
    ///  <remarks>Sub classes must not override this method, but should instead
    ///  provide the body HTML by overriding GetBodyHTML.</remarks>
    procedure ResolvePlaceholders(const Tplt: THTMLTemplate); override; final;
    ///  <summary>Returns HTML to be included within the body of the HTML
    ///  document.</summary>
    ///  <remarks>Implementors must ensure the returned HTML is valid. The body
    ///  tag must not be included in the HTML.</remarks>
    function GetBodyHTML: string; virtual; abstract;
  end;

type
  ///  <summary>
  ///  Generates HTML body for page displayed for a new, empty, detail pane tab.
  ///  </summary>
  TNewTabPageHTML = class sealed(TBasicPageTpltHTML)
  strict protected
    ///  <summary>Returns HTML that informs the user of a new, empty, tab.
    ///  </summary>
    function GetBodyHTML: string; override;
  end;

type
  ///  <summary>
  ///  Generates HTML body of page displayed after database has been updated.
  ///  </summary>
  TDBUpdatedPageHTML = class sealed(TBasicPageTpltHTML)
  strict protected
    ///  <summary>Returns HTML that informs the user that the database has been
    ///  updated.</summary>
    function GetBodyHTML: string; override;
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
  ///  Generates HTML body of a page that provides information about a snippet.
  ///  </summary>
  TSnippetInfoPageHTML = class sealed(TDetailPageTpltHTML)
  strict private
    ///  <summary>Returns reference to snippet that is being rendered.</summary>
    ///  <remarks>Snippet is recorded in View property.</remarks>
    function GetSnippet: TSnippet;
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
      ///  <summary>Sorted list of snippets to be displayed.</summary>
      fSnippetList: TObjectSortedList<TSnippet>;
    ///  <summary>Constructs sorted list of snippets to be displayed.</summary>
    procedure BuildSnippetList;
  strict protected
    ///  <summary>Creates and returns a sequence of HTML table rows each
    ///  containing a link to one of the snippets in the list, along with its
    ///  description.</summary>
    function SnippetTableInner: string;
    ///  <summary>Creates and returns an HTML table row containing one cell that
    ///  links to given snippet and another containing snippet's description.
    ///  </summary>
    function SnippetTableRow(const Snippet: TSnippet): string;
    ///  <summary>Returns name of template resource for either an empty or none-
    ///  empty list of snippets.</summary>
    function GetTemplateResName: string; override;
    ///  <summary>Replaces place-holders in chosen template with suitable
    ///  values.</summary>
    procedure ResolvePlaceholders(const Tplt: THTMLTemplate); override; final;
    ///  <summary>Checks if there are snippets in snippets list.</summary>
    function HaveSnippets: Boolean;
    ///  <summary>Checks if the given snippet should be included in the list of
    ///  snippets to be displayed.</summary>
    function IsSnippetRequired(const Snippet: TSnippet): Boolean; virtual;
      abstract;
      // TODO: change Snippet param of IsSnippetRequired to TSnippetID
    ///  <summary>Returns name of CSS class to be used for page heading.
    ///  </summary>
    ///  <remarks>Provides default class name. Descendant classes should
    ///  override as necessary.</remarks>
    function GetH1ClassName: string; virtual;
    ///  <summary>Returns page's heading text.</summary>
    ///  <remarks>Returns view's description by default. Descendants can
    ///  override if different behaviour is required.</remarks>
    function GetHeading: string; virtual;
    ///  <summary>Returns narrative to be used at top of any page that displays
    ///  a snippet list.</summary>
    function GetNarrative: string; virtual; abstract;
    ///  <summary>Returns text to be displayed on a page that has no snippets to
    ///  display.</summary>
    function GetEmptyListNote: string; virtual; abstract;
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
    ///  <summary>Checks if the given snippet should be included in the list of
    ///  snippets to be displayed.</summary>
    ///  <remarks>The snippet is to be displayed if it is in the category being
    ///  displayed.</remarks>
    function IsSnippetRequired(const Snippet: TSnippet): Boolean; override;
    ///  <summary>Returns name of CSS class to be used for page heading.
    ///  </summary>
    function GetH1ClassName: string; override;
    ///  <summary>Returns narrative to be used at top of any page that displays
    ///  a snippet list.</summary>
    function GetNarrative: string; override;
    ///  <summary>Returns text to be displayed on a page that has no snippets to
    ///  display.</summary>
    function GetEmptyListNote: string; override;
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
    ///  <summary>Checks if the given snippet should be included in the list of
    ///  snippets to be displayed.</summary>
    ///  <remarks>The snippet is to be displayed if its display name starts with
    ///  the initial letter being displayed.</remarks>
    function IsSnippetRequired(const Snippet: TSnippet): Boolean; override;
    ///  <summary>Returns narrative to be used at top of any page that displays
    ///  a snippet list.</summary>
    function GetNarrative: string; override;
    ///  <summary>Returns text to be displayed on a page that has no snippets to
    ///  display.</summary>
    function GetEmptyListNote: string; override;
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
    ///  <summary>Checks if the given snippet should be included in the list of
    ///  snippets to be displayed.</summary>
    ///  <remarks>The snippet is to be displayed if its kind is the same as that
    ///  being displayed.</remarks>
    function IsSnippetRequired(const Snippet: TSnippet): Boolean; override;
    ///  <summary>Returns page's heading text.</summary>
    function GetHeading: string; override;
    ///  <summary>Returns narrative to be used at top of any page that displays
    ///  a snippet list.</summary>
    function GetNarrative: string; override;
    ///  <summary>Returns text to be displayed on a page that has no snippets to
    ///  display.</summary>
    function GetEmptyListNote: string; override;
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

procedure TDetailPageTpltHTML.ResolvePlaceholders(const Tplt: THTMLTemplate);
begin
  // This placeholder is common to all templates
  Tplt.ResolvePlaceholderHTML('ResourcePath', MakeResourcePath(HInstance));
end;

{ TNulPageHTML }

function TNulPageHTML.Generate: string;
begin
  Result := '';
end;

{ TNewTabPageHTML }

function TNewTabPageHTML.GetBodyHTML: string;
begin
  Result := THTML.CompoundTag(
    'div',
    THTMLAttributes.Create('id', 'newtab'),
    THTML.Entities(View.Description)
  );
end;

{ TWelcomePageHTML }

function TWelcomePageHTML.GetTemplateResName: string;
begin
  Result := 'welcome-tplt.html';
end;

procedure TWelcomePageHTML.ResolvePlaceholders(const Tplt: THTMLTemplate);
var
  DBSize: Integer;
  Compilers: ICompilers;
  Compiler: ICompiler;
  CompilerList: TStringBuilder;

  ///  <summary>Returns the text of a statement that describes how often am
  ///  automatic update checked is performed.</summary>
  ///  <param name="Frequency">Word [in] Days between checks or zero to
  ///  indicated that no checks are made.</param>
  ///  <returns>string. Required text.</returns>
  function UpdateFrequencyText(const Frequency: Word): string;
  resourcestring
    sNeverChecked = 'never checked';
    sCheckedEveryNDays = 'checked every %d days';
    sCheckedEveryDay = 'checked every day';
  begin
    if Frequency = 0 then
      Result := sNeverChecked
    else if Frequency = 1 then
      Result := sCheckedEveryDay
    else
      Result := Format(sCheckedEveryNDays, [Frequency]);
  end;

begin
  inherited;
  Tplt.ResolvePlaceholderHTML(
    'externalScript', TJavaScript.LoadScript('external.js', etWindows1252)
  );

  DBSize := Database.SnippetCount;
  Tplt.ResolvePlaceholderHTML(
    'HaveSnippets', TCSS.BlockDisplayProp(DBSize > 0)
  );
  Tplt.ResolvePlaceholderHTML(
    'NoSnippets', TCSS.BlockDisplayProp(DBSize <= 0)
  );
  Tplt.ResolvePlaceholderText('SnippetCount', IntToStr(DBSize));


  Compilers := TCompilersFactory.CreateAndLoadCompilers;
  Tplt.ResolvePlaceholderHTML(
    'HaveCompilers', TCSS.BlockDisplayProp(Compilers.AvailableCount > 0)
  );
  Tplt.ResolvePlaceholderHTML(
    'NoCompilers', TCSS.BlockDisplayProp(Compilers.AvailableCount <= 0)
  );
  CompilerList := TStringBuilder.Create;
  try
    for Compiler in Compilers do
      if Compiler.IsAvailable then
        CompilerList.AppendLine(
          THTML.CompoundTag(
            'li',
            THTML.Entities(Compiler.GetName)
          )
        );
    Tplt.ResolvePlaceholderHTML('CompilerList', CompilerList.ToString);
  finally
    CompilerList.Free;
  end;
  Tplt.ResolvePlaceholderText(
    'ProgramAutoCheckFrequency',
    UpdateFrequencyText(Preferences.AutoCheckProgramFrequency)
  );
  // TODO: Restore line similar to following when synch spaces can auto-update
//  Tplt.ResolvePlaceholderText(
//    'DatabaseAutoCheckFrequency',
//    UpdateFrequencyText(Preferences.AutoCheckDatabaseFrequency)
//  );
end;

{ TDBUpdatedPageHTML }

function TDBUpdatedPageHTML.GetBodyHTML: string;
resourcestring
  sBody = 'The database has been updated successfully.';
begin
  Result :=
    THTML.CompoundTag('h1', View.Description)
    +
    THTML.CompoundTag('p', sBody);
end;

{ TSnippetInfoPageHTML }

function TSnippetInfoPageHTML.GetSnippet: TSnippet;
begin
  Assert(Supports(View, ISnippetView),
    ClassName + '.Create: View is not snippet');
  Result := (View as ISnippetView).Snippet;
end;

function TSnippetInfoPageHTML.GetTemplateResName: string;
begin
  Result := 'info-snippet-tplt.html';
end;

procedure TSnippetInfoPageHTML.ResolvePlaceholders(const Tplt: THTMLTemplate);
var
  SnippetHTML: TSnippetHTML;  // object used to generate HTML for snippet
begin
  inherited;
  Tplt.ResolvePlaceholderHTML(
    'externalScript',  TJavaScript.LoadScript('external.js', etWindows1252)
  );
  if TIEInfo.RequiresCSSOverflowXFix then
    Tplt.ResolvePlaceholderHTML(
      'overflowXFixScript',
      TJavaScript.LoadScript('overflowXFix.js', etWindows1252)
    )
  else
    Tplt.ResolvePlaceholderHTML(
      'overflowXFixScript',
      'window.onload = null;'
    );
  { TODO -cSynch Spaces: rethink following placeholder re synch-spaces: may need
                         style attrib to set required colour. }
  Tplt.ResolvePlaceholderHTML('SnippetCSSClass', 'userdb');
  SnippetHTML := TSnippetHTML.Create(GetSnippet);
  try
    Tplt.ResolvePlaceholderHTML('TestingInfoImg', SnippetHTML.TestingImage);
    Tplt.ResolvePlaceholderHTML('SnippetName', SnippetHTML.SnippetName);
  finally
    SnippetHTML.Free;
  end;
  Tplt.ResolvePlaceholderHTML(
    'SnippetPageFragments', TSnippetPageHTML.Render(GetSnippet)
  );
end;

{ TSnippetListPageHTML }

procedure TSnippetListPageHTML.BuildSnippetList;
var
  Snippet: TSnippet;  // each snippet in current query
begin
  fSnippetList.Clear;
  for Snippet in Query.Selection do
  begin
    if IsSnippetRequired(Snippet) then
      fSnippetList.Add(Snippet);
  end;
end;

constructor TSnippetListPageHTML.Create(View: IView);
begin
  inherited;
  fSnippetList := TObjectSortedList<TSnippet>.Create(
    TRules<TSnippet>.Create(
      TSnippet.TTitleComparer.Create, TSnippet.TTitleEqualityComparer.Create
    )
  );
  fSnippetList.OwnsObjects := False;
  BuildSnippetList;
end;

destructor TSnippetListPageHTML.Destroy;
begin
  fSnippetList.Free;
  inherited;
end;

function TSnippetListPageHTML.GetH1ClassName: string;
begin
  Result := 'maindb';
end;

function TSnippetListPageHTML.GetHeading: string;
begin
  Result := View.Description;
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
  Result := not fSnippetList.Empty;
end;

procedure TSnippetListPageHTML.ResolvePlaceholders(const Tplt: THTMLTemplate);
begin
  inherited;
  Tplt.ResolvePlaceholderHTML('H1Class', GetH1ClassName);
  Tplt.ResolvePlaceholderText('Heading', GetHeading);
  if HaveSnippets then
  begin
    Tplt.ResolvePlaceholderHTML(
      'externalScript', TJavaScript.LoadScript('external.js', etWindows1252)
    );
    Tplt.ResolvePlaceholderText('Narrative', GetNarrative);
    Tplt.ResolvePlaceholderHTML('SnippetList', SnippetTableInner);
  end
  else
    Tplt.ResolvePlaceholderText('Note', GetEmptyListNote);
end;

function TSnippetListPageHTML.SnippetTableInner: string;
var
  Snippet: TSnippet;  // each snippet in list
begin
  Result := '';
  for Snippet in fSnippetList do
    Result := Result + SnippetTableRow(Snippet);
end;

function TSnippetListPageHTML.SnippetTableRow(const Snippet: TSnippet): string;
var
  SnippetHTML: TSnippetHTML;
  NameCellAttrs: IHTMLAttributes;
  DescCellAttrs: IHTMLAttributes;
begin
  NameCellAttrs := THTMLAttributes.Create('class', 'name');
  DescCellAttrs := THTMLAttributes.Create('class', 'desc');
  SnippetHTML := TSnippetHTML.Create(Snippet);
  try
    Result := THTML.CompoundTag(
      'tr',
      THTML.CompoundTag(
        'td',
        NameCellAttrs,
        SnippetHTML.SnippetALink
      )
      + THTML.CompoundTag('td', DescCellAttrs, SnippetHTML.Description)
    );
  finally
    SnippetHTML.Free;
  end;
end;

{ TCategoryPageHTML }

function TCategoryPageHTML.GetEmptyListNote: string;
resourcestring
  sNote = 'The current selection contains no snippets in this category.';
begin
  Result := sNote;
end;

function TCategoryPageHTML.GetH1ClassName: string;
begin
  if (View as ICategoryView).Category.UserDefined then
    Result := 'userdb'
  else
    Result := inherited GetH1ClassName;
end;

function TCategoryPageHTML.GetNarrative: string;
resourcestring
  sNarrative = 'List of selected snippets in this category.';
begin
  Result := sNarrative;
end;

function TCategoryPageHTML.IsSnippetRequired(const Snippet: TSnippet): Boolean;
begin
  Result := (View as ICategoryView).Category.SnippetIDs.Contains(Snippet.ID);
end;

{ TAlphaListPageHTML }

function TAlphaListPageHTML.GetEmptyListNote: string;
resourcestring
  sNote = 'The are no snippets in the current selection that begin with the '
    + 'letter %s.';
begin
  Result := Format(sNote, [(View as IInitialLetterView).InitialLetter.Letter]);
end;

function TAlphaListPageHTML.GetNarrative: string;
resourcestring
  sNarrative = 'List of selected snippets beginning with the letter %s.';
begin
  Result := Format(
    sNarrative, [(View as IInitialLetterView).InitialLetter.Letter]
  );
end;

function TAlphaListPageHTML.IsSnippetRequired(const Snippet: TSnippet): Boolean;
begin
  Result := StrStartsText(
    (View as IInitialLetterView).InitialLetter, Snippet.Title
  );
end;

{ TSnipKindPageHTML }

function TSnipKindPageHTML.GetEmptyListNote: string;
resourcestring
  sNote = 'There are no %s snippets in the current selection.';
begin
  Result := Format(sNote, [StrToLower(View.Description)]);
end;

function TSnipKindPageHTML.GetHeading: string;
resourcestring
  sHeading = '%s Snippets';
begin
  Result := Format(sHeading, [View.Description]);
end;

function TSnipKindPageHTML.GetNarrative: string;
resourcestring
  sNarrative = 'List of all %s snippets in the current selection.';
begin
  Result := Format(sNarrative, [StrToLower(View.Description)])
end;

function TSnipKindPageHTML.IsSnippetRequired(const Snippet: TSnippet): Boolean;
begin
  Result := (View as ISnippetKindView).KindInfo.Kind = Snippet.Kind;
end;

{ TBasicPageTpltHTML }

function TBasicPageTpltHTML.GetTemplateResName: string;
begin
  Result := 'info-basic-tplt.html';
end;

procedure TBasicPageTpltHTML.ResolvePlaceholders(const Tplt: THTMLTemplate);
begin
  inherited;
  Tplt.ResolvePlaceholderHTML('BodyContent', GetBodyHTML);
end;

end.


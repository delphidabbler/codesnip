{
 * UDetailPageHTML.pas
 *
 * Heirachy of classes that generate dynamic HTML pages from HTML templates, for
 * use in displaying detail view items. Also includes a factory class to create
 * HTML generator objects.
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
  // Delphi
  Classes,
  // Project
  Compilers.UGlobals, DB.USnippet, UHTMLTemplate, UView;


type

  {
  TDetailPageHTML:
    Abstract base class for classes that generate body HTML displayed in detail
    views.
  }
  TDetailPageHTML = class abstract(TObject)
  strict private
    var fView: IView; // Value of View property
  strict protected
    property View: IView read fView;
      {Reference to object providing information about item to be viewed}
  public
    constructor Create(View: IView); virtual;
      {Object constructor. Sets up object for a view item.
        @param View [in] Provides information about item to be displayed.
      }
    function Generate: string; virtual; abstract;
      {Generates required body HTML and writes to stream.
        @return Body HTML.
      }
  end;

  {
  TDetailPageTpltHTML:
    Abstract base class for classes that generate HTML by updating a template
    stored in resources.
  }
  TDetailPageTpltHTML = class abstract(TDetailPageHTML)
  strict protected
    function GetTemplateResName: string; virtual; abstract;
      {Gets the name of the HTML template resource.
        @return Name of template resource.
      }
    procedure ResolvePlaceholders(const Tplt: THTMLTemplate); virtual; abstract;
      {Resolves the placeholders in the HTML template.
        @param Tplt [in] Reference to HTML template object that encapsulates the
          template.
      }
  public
    function Generate: string; override;
      {Generates the required body HTML from a HTML template and writes to a
      stream.
        @return Body HTML.
      }
  end;

  {
  TNulPageHTML:
    Nul class called if blank HTML pages is required. Does nothing since blank
    HTML page has no body contents.
  }
  TNulPageHTML = class sealed(TDetailPageHTML)
  public
    function Generate: string; override;
      {Generates body content for nul document.
        @return Empty string.
      }
  end;

  // TODO: Comment this class
  TNewTabPageHTML = class sealed(TDetailPageHTML)
  public
    function Generate: string; override;
  end;

  {
  TWelcomePageHTML:
    Class that generates the welcome page from a template stored in resources.
  }
  TWelcomePageHTML = class sealed(TDetailPageTpltHTML)
  strict protected
    function GetTemplateResName: string; override;
      {Gets the name of the HTML template resource.
        @return Name of template resource.
      }
    procedure ResolvePlaceholders(const Tplt: THTMLTemplate); override;
      {Resolves the placeholders in the HTML template.
        @param Tplt [in] Reference to HTML template object that encapsulates the
          template.
      }
  end;

  {
  TSnippetPageHTML:
    Abstract base class for classes that generate HTML for pages that describe
    a snippet.
  }
  TSnippetPageHTML = class abstract(TDetailPageTpltHTML)
  strict private
    fCompilersInfo: ICompilers; // Provides information about compilers
  strict protected
    function GetTemplateResName: string; override; abstract;
      {Gets the name of the HTML template resource.
        @return Name of template resource.
      }
    procedure ResolvePlaceholders(const Tplt: THTMLTemplate); override;
      {Resolves the placeholders in the HTML template.
        @param Tplt [in] Reference to HTML template object that encapsulates the
          template.
      }
    function GetSnippet: TSnippet;
      {Gets reference to snippet from View property.
        @return Required snippet reference.
      }
    property CompilersInfo: ICompilers read fCompilersInfo;
      {Provides information to sub classes about compilers}
  public
    constructor Create(View: IView); override;
      {Object constructor. Sets up object for a view item.
        @param View [in] Provides information about snippet to be displayed.
      }
  end;

  {
  TSnippetInfoPageHTML:
    Class that generates information about a snippet displayed in information
    pane. Uses a template stored in resources.
  }
  TSnippetInfoPageHTML = class sealed(TSnippetPageHTML)
  strict protected
    function GetTemplateResName: string; override;
      {Gets the name of the HTML template resource.
        @return Name of template resource.
      }
    procedure ResolvePlaceholders(const Tplt: THTMLTemplate); override;
      {Resolves the placeholders in the HTML template.
        @param Tplt [in] Reference to HTML template object that encapsulates the
          template.
      }
  end;

  {
  TSnippetPageHTML:
    Abstract base class for classes that generate HTML for pages that display a
    list of snippets.
  }
  TSnippetListPageHTML = class abstract(TDetailPageTpltHTML)
  strict private
    fSnippets: TSnippetList;  // List of snippets to be displayed
  strict protected
    function SnippetTableInner: string;
      {Builds a sequence of table rows each containing a link to the snippet
      along with its description.
        @return Required table rows.
      }
    function SnippetTableRow(const Snippet: TSnippet): string;
      {Builds a table row containing cells with a link to a snippet and a
      description of the snippet.
        @param Snippet [in] Snippet to be included in row.
        @return Required table row.
      }
    procedure BuildSnippetList; virtual; abstract;
      {Stores all snippets to be displayed in Snippets property.
      }
    property Snippets: TSnippetList read fSnippets;
      {List of all snippets to be displayed}
    function GetTemplateResName: string; override;
      {Gets the name of the HTML template resource.
        @return Name of template resource.
      }
    procedure ResolvePlaceholders(const Tplt: THTMLTemplate); override;
      abstract;
      {Resolves the placeholders in the HTML template.
        @param Tplt [in] Reference to HTML template object that encapsulates the
          template.
      }
    function HaveSnippets: Boolean;
      {Checks if there are snippets in list.
        @return True if there are snippets in list, False if not.
      }
  public
    constructor Create(View: IView); override;
      {Object constructor. Sets up object for a view item.
        @param View [in] Provides information about item to be displayed.
      }
    destructor Destroy; override;
      {Object destructor. Tidies up object.
      }
  end;

  {
  TCategoryPageHTML:
    Class that displays snippets contained in a category. Uses a template stored
    in resources.
  }
  TCategoryPageHTML = class sealed(TSnippetListPageHTML)
  strict protected
    procedure ResolvePlaceholders(const Tplt: THTMLTemplate); override;
      {Resolves the placeholders in the HTML template.
        @param Tplt [in] Reference to HTML template object that encapsulates the
          template.
      }
    procedure BuildSnippetList; override;
      {Stores all snippets to be displayed in Snippets property.
      }
  end;

  {
  TAlphaListPageHTML:
    Class that displays all snippets that have same initial letter. Uses a
    template stored in resources.
  }
  TAlphaListPageHTML = class sealed(TSnippetListPageHTML)
  strict protected
    procedure ResolvePlaceholders(const Tplt: THTMLTemplate); override;
      {Resolves the placeholders in the HTML template.
        @param Tplt [in] Reference to HTML template object that encapsulates the
          template.
      }
    procedure BuildSnippetList; override;
      {Stores all snippets to be displayed in Snippets property.
      }
  end;

  {
  TSnipKindPageHTML:
    Class that displays all snippets that are of same kind. Uses a template
    stored in resources.
  }
  TSnipKindPageHTML = class sealed(TSnippetListPageHTML)
  strict protected
    procedure ResolvePlaceholders(const Tplt: THTMLTemplate); override;
      {Resolves the placeholders in the HTML template.
        @param Tplt [in] Reference to HTML template object that encapsulates the
          template.
      }
    procedure BuildSnippetList; override;
      {Stores all snippets to be displayed in Snippets property.
      }
  end;


implementation


uses
  // Delphi
  SysUtils, Character,
  // Project
  Compilers.UCompilers, DB.UMain, UCompResHTML, UConsts, UCSSUtils, UEncodings,
  UHTMLUtils, UHTMLDetailUtils, UJavaScriptUtils, UQuery, UResourceUtils,
  USnippetHTML, UStrUtils, UUtils;


{ TDetailPageHTML }

constructor TDetailPageHTML.Create(View: IView);
  {Object constructor. Sets up object for a view item.
    @return Body HTML.
  }
begin
  Assert(Assigned(View), ClassName + '.Create: View is nil');
  inherited Create;
  fView := View;
end;

{ TDetailPageTpltHTML }

function TDetailPageTpltHTML.Generate: string;
  {Generates the required body HTML from a HTML template and writes to a stream.
    @return Body HTML.
  }
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
  {Generates body content for nul document.
    @return Empty string.
  }
begin
  Result := '';
end;

{ TWelcomePageHTML }

function TWelcomePageHTML.GetTemplateResName: string;
  {Gets the name of the HTML template resource.
    @return Name of template resource.
  }
begin
  Result := 'welcome-tplt.html';
end;

procedure TWelcomePageHTML.ResolvePlaceholders(const Tplt: THTMLTemplate);
  {Resolves the placeholders in the HTML template.
    @param Tplt [in] Reference to HTML template object that encapsulates the
      template.
  }
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

{ TSnippetPageHTML }

constructor TSnippetPageHTML.Create(View: IView);
  {Object constructor. Sets up object for a view item.
    @param View [in] Provides information about snippet to be displayed.
  }
begin
  inherited;
  // create compilers info object
  fCompilersInfo := TCompilersFactory.CreateAndLoadCompilers;
end;

function TSnippetPageHTML.GetSnippet: TSnippet;
  {Gets reference to snippet from View property.
    @return Required snippet reference.
  }
begin
  Assert(Supports(View, ISnippetView),
    ClassName + '.GetSnippet: View is not snippet');
  Result := (View as ISnippetView).Snippet;
end;

procedure TSnippetPageHTML.ResolvePlaceholders(const Tplt: THTMLTemplate);
  {Resolves the placeholders in the HTML template.
    @param Tplt [in] Reference to HTML template object that encapsulates the
      template.
  }
var
  SnippetHTML: TSnippetHTML;  // object used to generate HTML
begin
  // Resolve placeholders common to all snippet templates
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
  {Gets the name of the HTML template resource.
    @return Name of template resource.
  }
begin
  Result := 'info-snippet-tplt.html';
end;

procedure TSnippetInfoPageHTML.ResolvePlaceholders(const Tplt: THTMLTemplate);
  {Resolves the placeholders in the HTML template.
    @param Tplt [in] Reference to HTML template object that encapsulates the
      template.
  }

  // ---------------------------------------------------------------------------
  function CompilerTableInner: string;
    {Generates inner HTML (rows) of compiler table.
      @return Required HTML.
    }
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
  // ---------------------------------------------------------------------------

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
  {Object constructor. Sets up object for a view item.
    @param View [in] Provides information about item to be displayed.
  }
begin
  inherited;
  // Create list of all snippets to be displayed
  fSnippets := TSnippetList.Create;
  BuildSnippetList;
end;

destructor TSnippetListPageHTML.Destroy;
  {Object destructor. Tidies up object.
  }
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
  {Checks if there are snippets in list.
    @return True if there are snippets in list, False if not.
  }
begin
  Result := Snippets.Count > 0;
end;

function TSnippetListPageHTML.SnippetTableInner: string;
  {Builds a sequence of table rows each containing a link to the snippet
  along with its description.
    @return Required table rows.
  }
var
  Snippet: TSnippet;  // references each snippet in list
begin
  Result := '';
  for Snippet in Snippets do
    Result := Result + SnippetTableRow(Snippet);
end;

function TSnippetListPageHTML.SnippetTableRow(const Snippet: TSnippet): string;
  {Builds a table row containing cells with a link to a snippet and a
  description of the snippet.
    @param Snippet [in] Snippet to be included in row.
    @return Required table row.
  }
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
  {Stores all snippets to be displayed in Snippets property.
  }
begin
  Query.GetCatSelection((View as ICategoryView).Category, Snippets);
end;

procedure TCategoryPageHTML.ResolvePlaceholders(const Tplt: THTMLTemplate);
  {Resolves the placeholders in the HTML template.
    @param Tplt [in] Reference to HTML template object that encapsulates the
      template.
  }

  // ---------------------------------------------------------------------------
  function H1ClassName: string;
    {Gets CSS class name used in H1 heading.
      @return Required name. Depends on whether category is user defined.
    }
  begin
    if (View as ICategoryView).Category.UserDefined then
      Result := 'userdb'
    else
      Result := 'maindb';
  end;
  // ---------------------------------------------------------------------------

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
  {Stores all snippets to be displayed in Snippets property.
  }
var
  Snippet: TSnippet;
begin
  Snippets.Clear;
  for Snippet in Query.Selection do
  begin
    if Snippet.Name[1] = (View as IInitialLetterView).InitialLetter then
      Snippets.Add(Snippet);
  end;
end;

procedure TAlphaListPageHTML.ResolvePlaceholders(const Tplt: THTMLTemplate);
  {Resolves the placeholders in the HTML template.
    @param Tplt [in] Reference to HTML template object that encapsulates the
      template.
  }
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
  {Stores all snippets to be displayed in Snippets property.
  }
var
  Snippet: TSnippet;
begin
  Snippets.Clear;
  for Snippet in Query.Selection do
  begin
    if Snippet.Kind = (View as ISnippetKindView).KindInfo.Kind then
      Snippets.Add(Snippet);
  end;
end;

procedure TSnipKindPageHTML.ResolvePlaceholders(const Tplt: THTMLTemplate);
  {Resolves the placeholders in the HTML template.
    @param Tplt [in] Reference to HTML template object that encapsulates the
      template.
  }
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

{ TNewTabPageHTML }

function TNewTabPageHTML.Generate: string;
begin
  Result := MakeCompoundTag(
    'div',
    THTMLAttributes.Create('id', 'newtab'),
    MakeSafeHTMLText(View.Description)
  );
end;

end.


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
 * v0.1 of 21 Feb 2005  - Original version, named UHTMLGenerators based on some
 *                        code from now deleted UCompCheckHTML.pas and
 *                        UHTMLProducers.pas units.
 * v0.2 of 24 Feb 2005  - Changed to compiler names from Compilers global object
 *                        now that name removed from UCompilerTypes.
 * v0.3 of 04 Mar 2005  - Changed to use TCompilerID that replaces former
 *                        TDelphiVersion and changed some identifiers to suit.
 *                      - Now uses revised version of Compilers object.
 * v0.4 of 10 Mar 2005  - Added ability to syntax highlight the source code
 *                        displayed on info and detail pages. A helper routine
 *                        was added to interface with highlighter classes.
 * v0.5 of 17 Mar 2005  - Changed to use new detail highlighter object in
 *                        USyntaxHiliters instead of previous obsolete
 *                        highlighter.
 * v0.6 of 21 Apr 2005  - Changed to use renamed USourceGen unit and associated
 *                        renamed class and method. Changed to use renamed
 *                        IntfCompilers unit.
 *                      - Added new IntfHiliter unit that contains type
 *                        declarations formerly in other units.
 * v0.7 of 03 Jun 2005  - Changed TDbaseCompRoutineGenerator to enable / disable
 *                        test compile button depending on if compilers
 *                        available. Also inserts HTML display warning message
 *                        if no compilers available.
 *                      - Replaced redundant method in TDetailViewGenerator with
 *                        abstract override.
 * v1.0 of 24 May 2006  - Improved and corrected comments.
 * v1.1 of 25 Oct 2006  - Removed target="_blank" attribute from links to
 *                        external URLs. Display of URLs is now handled by
 *                        program rather than relying on default behaviour of
 *                        web browser control.
 * v1.2 of 07 Nov 2006  - Changed to use UCSSUtils methods to generate CSS
 *                        attributes.
 * v1.3 of 25 Nov 2006  - Changed to use default syntax highlighter when
 *                        highlighting source code.
 *                      - Changed references to fixed colours to use special
 *                        program colour constants.
 *                      - Replaced custom style used to display "no compiler"
 *                        warning note with warning CSS class.
 *                      - Changed to use new .comptable CSS class to format
 *                        compiler table.
 * v1.4 of 26 Nov 2006  - Changed code that generates JavaScript call that
 *                        displays a compiler log to use renamed JavaScript
 *                        function.
 *                      - Now generate above JavaScript call using JSLiteralFunc
 *                        routine.
 * v1.5 of 02 Dec 2006  - Modified to handle compiler check pane change to
 *                        display a single view rather than different views for
 *                        database and test results.
 *                      - Changed TDetailHTMLGeneratorKind enumeration re
 *                        compiler check view changes.
 *                      - Replaced TCompRoutineGeneratorBase,
 *                        TTestCompRoutineGenerator and
 *                        TDbaseCompRoutineGenerator with single
 *                        TCompRoutineGenerator class that resolves only one
 *                        placeholder. Remainder of details are updated
 *                        dynamically.
 *                      - Updated THTMLGeneratorFactory re changes.
 *                      - Modified TInfoRoutineGenerator to get compiler image
 *                        from TCompResHTML.ImageTag().
 * v1.6 of 03 Dec 2006  - Removed HiliteSource helper routine.
 *                      - Changed TInfoRoutineGenerator.ResolvePlaceholders to
 *                        remove all HTML generation code except for nul
 *                        compiler table. Also changed to work with revised
 *                        template document. The only placeholder is now the nul
 *                        compiler table.
 * v1.7 of 04 Feb 2007  - Replaced use of redundant TDetailView objects with
 *                        calls to new global query object and TViewItem
 *                        objects.
 *                      - Removed some redundant code.
 * v1.8 of 10 Sep 2008  - Added new UserDefined param to call RoutineALink
 *                        routine.
 *                      - Added code to resolve new <%H1Class%> placeholder when
 *                        generating category details on information pane.
 * v1.9 of 04 Oct 2008  - Changed THTMLGeneratorFactory to derive from
 *                        TNoConstructObject and hence prevented it from being
 *                        constructed.
 *                      - Made various class' private and protected sections
 *                        strict.
 *                      - Now use ClassName method in all assert statements.
 * v1.10 of 12 Dec 2008 - Changed iterations of Compilers and routine lists to
 *                        use for..in loops.
 * v1.11 of 09 Jan 2009 - Added new TRoutineGeneratorBase base class for classes
 *                        that generate HTML for routines: TInfoRoutineGenerator
 *                        and TCompRoutineGenerator.
 * v1.12 of 13 Jan 2009 - Replaced control char literals with constants.
 * v2.0 of 25 Jan 2009  - Renamed as UDetailPageHTML.pas.
 *                      - Major rewrite to generate all required code as body
 *                        HTML to be included in a blank HTML body, without
 *                        reliance on dynamic updating of the generated code.
 *                      - Now uses routines from UHTMLUtils to generate HTML
 *                        tags instead of generating from literal strings.
 *                      - Class structure completely revised.
 * v2.1 of 16 Jun 2009  - Now calls new TRoutine.CanCompile method instead of
 *                        defunct TRoutine.StandardFormat property to check if
 *                        a snippet is compilable.
 *                      - Added TAlphListPageHTML and TSnipKindPageHTML classes,
 *                        descended from TRoutineListPageHTML to display
 *                        snippets of same initial letter or snippet kind.
 *                      - Removed redundant TUncategorisedPageHTML.
 *                      - Modified TRoutineListPageHTML to provide single pair
 *                        of templates for descendants.
 *                      - Changed some of of template "file" names.
 *                      - Removed all support for IDetailViewHostInfo: method
 *                        parameters and local variables removed.
 *                      - Removed support for displaying test units from
 *                        TRoutineCompCheckPageHTML.
 *                      - Provided new TRoutineListPageHTML.HaveSnippets method.
 *                      - Modified TWelcomePageHTML.ResolvePlaceholders to work
 *                        with revised template.
 *                      - Added support for new <%Kind%> placeholder in snippet
 *                        information HTML.
 *                      - Moved edit link code from TRoutineInfoPageHTML down to
 *                        TRoutinePageHTML since now common with
 *                        TRoutineCompCheckPageHTML.
 *                      - Changed TRoutineCompCheckPageHTML to use different
 *                        templates for when snippet is not compilable or there
 *                        are no compilers.
 *                      - Modified TRoutineInfoPageHTML to hide compiler table
 *                        for non-compilable snippets.
 * v2.2 of 12 Jul 2009  - Added support for new <%Category%> placeholder in
 *                        snippet information HTML. Creates link to a category.
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
 * The Original Code is UDetailPageHTML.pas
 *
 * The Initial Developer of the Original Code is Peter Johnson
 * (http://www.delphidabbler.com/).
 *
 * Portions created by the Initial Developer are Copyright (C) 2005-2009 Peter
 * Johnson. All Rights Reserved.
 *
 * ***** END LICENSE BLOCK *****
}


unit UDetailPageHTML;


interface


uses
  // Delphi
  Classes,
  // Project
  IntfCompilers, UHTMLTemplate, USnippets, UView;


type

  {
  TDetailPageHTML:
    Abstract base class for classes that generate body HTML displayed in detail
    views.
  }
  TDetailPageHTML = class abstract(TObject)
  strict private
    fView: TViewItem; // Value of View property
  strict protected
    property View: TViewItem read fView;
      {Reference to object providing information about item to be viewed}
  public
    constructor Create(const View: TViewItem); virtual;
      {Class constructor. Sets up object for a view item.
        @param View [in] Provides information about item to be displayed.
      }
    procedure Generate(const Stm: TStream); virtual; abstract;
      {Generates required body HTML and writes to stream.
        @param Stm [in] Stream that received HTML body.
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
    procedure Generate(const Stm: TStream); override;
      {Generates the required body HTML from a HTML template and writes to a
      stream.
        @param Stm [in] Stream to receive generated HTML.
      }
  end;

  {
  TNulPageHTML:
    Nul class called if blank HTML pages is required. Does nothing since blank
    HTML page has no body contents.
  }
  TNulPageHTML = class sealed(TDetailPageHTML)
  public
    procedure Generate(const Stm: TStream); override;
      {Generates body content for nul document. Does nothing.
        @param Stm [in] Not used.
      }
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
  TRoutinePageHTML:
    Abstract base class for classes that generate HTML for pages that describe
    a snippet.
  }
  TRoutinePageHTML = class abstract(TDetailPageTpltHTML)
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
    function GetRoutine: TRoutine;
      {Gets reference to snippet from View property.
        @return Required snippet reference.
      }
    property CompilersInfo: ICompilers read fCompilersInfo;
      {Provides information to sub classes about compilers}
  public
    constructor Create(const View: TViewItem); override;
      {Class constructor. Sets up object for a view item.
        @param View [in] Provides information about snippet to be displayed.
      }
  end;

  {
  TRoutineInfoPageHTML:
    Class that generates information about a snippet displayed in information
    pane. Uses a template stored in resources.
  }
  TRoutineInfoPageHTML = class sealed(TRoutinePageHTML)
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
  TRoutineCompCheckPageHTML:
    Class that generates information about a snippet displayed in compiler check
    pane. Uses a template stored in resources.
  }
  TRoutineCompCheckPageHTML = class sealed(TRoutinePageHTML)
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
  TRoutinePageHTML:
    Abstract base class for classes that generate HTML for pages that display a
    list of snippets.
  }
  TRoutineListPageHTML = class abstract(TDetailPageTpltHTML)
  strict private
    fRoutines: TRoutineList;  // List of snippets to be displayed
  strict protected
    function RoutineTableInner: string;
      {Builds a sequence of table rows each containing a link to the snippet
      along with its description.
        @return Required table rows.
      }
    function RoutineTableRow(const Routine: TRoutine): string;
      {Builds a table row containing cells with a link to a snippet and a
      description of the snippet.
        @param Routine [in] Snippet to be included in row.
        @return Required table row.
      }
    procedure BuildRoutineList; virtual; abstract;
      {Stores all snippets to be displayed in Routines property.
      }
    property Routines: TRoutineList read fRoutines;
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
    constructor Create(const View: TViewItem); override;
      {Class constructor. Sets up object for a view item.
        @param View [in] Provides information about item to be displayed.
      }
    destructor Destroy; override;
      {Class destructor. Tidies up object.
      }
  end;

  {
  TCategoryPageHTML:
    Class that displays routines contained in a category. Uses a template stored
    in resources.
  }
  TCategoryPageHTML = class sealed(TRoutineListPageHTML)
  strict protected
    procedure ResolvePlaceholders(const Tplt: THTMLTemplate); override;
      {Resolves the placeholders in the HTML template.
        @param Tplt [in] Reference to HTML template object that encapsulates the
          template.
      }
    procedure BuildRoutineList; override;
      {Stores all snippets to be displayed in Routines property.
      }
  end;

  {
  TAlphaListPageHTML:
    Class that displays all snippets that have same initial letter. Uses a
    template stored in resources.
  }
  TAlphaListPageHTML = class sealed(TRoutineListPageHTML)
  strict protected
    procedure ResolvePlaceholders(const Tplt: THTMLTemplate); override;
      {Resolves the placeholders in the HTML template.
        @param Tplt [in] Reference to HTML template object that encapsulates the
          template.
      }
    procedure BuildRoutineList; override;
      {Stores all snippets to be displayed in Routines property.
      }
  end;

  {
  TSnipKindPageHTML:
    Class that displays all snippets that are of same kind. Uses a template
    stored in resources.
  }
  TSnipKindPageHTML = class sealed(TRoutineListPageHTML)
  strict protected
    procedure ResolvePlaceholders(const Tplt: THTMLTemplate); override;
      {Resolves the placeholders in the HTML template.
        @param Tplt [in] Reference to HTML template object that encapsulates the
          template.
      }
    procedure BuildRoutineList; override;
      {Stores all snippets to be displayed in Routines property.
      }
  end;

  {
  TNoCompCheckPageHTML:
    Class that generates HTML that indicates that compiler checks are not
    available for selected view item. Displays body HTML stored in resources.
  }
  TNoCompCheckPageHTML = class sealed(TDetailPageHTML)
  public
    procedure Generate(const Stm: TStream); override;
      {Generates HTML body content for "No compiler check available" pages.
        @param Stm [in] Stream that receives body HTML.
      }
  end;


implementation


uses
  // Delphi
  SysUtils,
  // Project
  UCompilers, UCompResHTML, UConsts, UCSSUtils, UHTMLUtils, UHTMLDetailUtils,
  UJavaScriptUtils, UQuery, URoutineHTML, UUtils;


{ TDetailPageHTML }

constructor TDetailPageHTML.Create(const View: TViewItem);
  {Class constructor. Sets up object for a view item.
    @param View [in] Provides information about item to be displayed.
  }
begin
  Assert(Assigned(View), ClassName + '.Create: View is nil');
  inherited Create;
  fView := View;
end;

{ TDetailPageTpltHTML }

procedure TDetailPageTpltHTML.Generate(const Stm: TStream);
  {Generates the required body HTML from a HTML template and writes to a stream.
    @param Stm [in] Stream to receive generated HTML.
  }
var
  Tplt: THTMLTemplate;  // encapsulates HTML template
begin
  // Create template object from required HTML template resource
  Tplt := THTMLTemplate.Create(HInstance, GetTemplateResName);
  try
    // Resolve all placeholders and write resulting HTML to stream
    ResolvePlaceholders(Tplt);
    Tplt.SaveToStream(Stm);
  finally
    Tplt.Free;
  end;
end;

{ TNulPageHTML }

procedure TNulPageHTML.Generate(const Stm: TStream);
  {Generates body content for nul document. Does nothing.
    @param Stm [in] Not used.
  }
begin
  // do nothing
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
  HaveMainDB := Snippets.Routines.Count(False) > 0;
  HaveUserDB := Snippets.Routines.Count(True) > 0;
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

{ TRoutinePageHTML }

constructor TRoutinePageHTML.Create(const View: TViewItem);
  {Class constructor. Sets up object for a view item.
    @param View [in] Provides information about snippet to be displayed.
  }
begin
  inherited;
  // create compilers info object
  fCompilersInfo := TCompilersFactory.CreateAndLoadCompilers;
end;

function TRoutinePageHTML.GetRoutine: TRoutine;
  {Gets reference to snippet from View property.
    @return Required snippet reference.
  }
begin
  Assert(View.Kind = vkRoutine, ClassName + '.GetRoutine: View is not snippet');
  Result := View.Routine;
end;

procedure TRoutinePageHTML.ResolvePlaceholders(const Tplt: THTMLTemplate);
  {Resolves the placeholders in the HTML template.
    @param Tplt [in] Reference to HTML template object that encapsulates the
      template.
  }
var
  RoutineHTML: TRoutineHTML;  // object used to generate HTML
begin
  // Resolve placeholders common to all snippet templates
  // snippet name and class
  if GetRoutine.UserDefined then
    Tplt.ResolvePlaceholderHTML('RoutineCSSClass', 'userdb')
  else
    Tplt.ResolvePlaceholderHTML('RoutineCSSClass', 'maindb');
  RoutineHTML := TRoutineHTML.Create(GetRoutine);
  try
    Tplt.ResolvePlaceholderHTML('RoutineName', RoutineHTML.RoutineName);
  finally
    FreeAndNil(RoutineHTML);
  end;
  // "edit snippet" link for user-defined snippets
  Tplt.ResolvePlaceholderHTML(
    'EditLink', CSSBlockDisplayProp(GetRoutine.UserDefined)
  );
  Tplt.ResolvePlaceholderText(
    'EditEventHandler', JSLiteralFunc('editRoutine', [GetRoutine.Name])
  );
end;

{ TRoutineInfoPageHTML }

function TRoutineInfoPageHTML.GetTemplateResName: string;
  {Gets the name of the HTML template resource.
    @return Name of template resource.
  }
begin
  Result := 'info-snippet-tplt.html';
end;

procedure TRoutineInfoPageHTML.ResolvePlaceholders(const Tplt: THTMLTemplate);
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
        + TInfoCompResHTML.ResultCell(GetRoutine.Compatibility[Compiler.GetID])
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
  InfoHTML := TInfoHTML.Create(GetRoutine);
  try
    Tplt.ResolvePlaceholderHTML('Kind', InfoHTML.SnippetKind);
    Tplt.ResolvePlaceholderHTML('Category', InfoHTML.Category);
    Tplt.ResolvePlaceholderHTML('Description', InfoHTML.RoutineDesc);
    Tplt.ResolvePlaceholderHTML('SourceCode', InfoHTML.SourceCode);
    Tplt.ResolvePlaceholderHTML('Units', InfoHTML.Units);
    Tplt.ResolvePlaceholderHTML('Depends', InfoHTML.Depends);
    Tplt.ResolvePlaceholderHTML('XRefs', InfoHTML.XRefs);
    Tplt.ResolvePlaceholderHTML('CompilerTableRows', CompilerTableInner);
    Tplt.ResolvePlaceholderHTML('Extra', InfoHTML.Extra);
    Tplt.ResolvePlaceholderHTML(
      'ShowCompilations', CSSBlockDisplayProp(GetRoutine.CanCompile)
    );
  finally
    FreeAndNil(InfoHTML);
  end;
end;

{ TRoutineCompCheckPageHTML }

function TRoutineCompCheckPageHTML.GetTemplateResName: string;
  {Gets the name of the HTML template resource.
    @return Name of template resource.
  }
begin
  if CompilersInfo.AvailableCount = 0 then
    Result := 'comp-nocompilers-tplt.html'
  else if GetRoutine.CanCompile then
    Result := 'comp-snippet-tplt.html'
  else
    Result := 'comp-freeform-tplt.html';
end;

procedure TRoutineCompCheckPageHTML.ResolvePlaceholders(
  const Tplt: THTMLTemplate);
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
    Compiler: ICompiler;  // reference to each compiler
  begin
    Result := '';
    for Compiler in CompilersInfo do
    begin
      // Add table row for each supported compiler
      Result := Result
        + MakeTag('tr', ttOpen)
        + EOL
        + TCompCheckResHTML.NameCell(Compiler)
        + EOL
        + TCompCheckResHTML.ResultCell(GetRoutine.Compatibility[Compiler.GetID])
        + EOL
        + TCompCheckResHTML.TestCellPlaceholder(Compiler)
        + EOL
        + TCompCheckResHTML.ErrCellPlaceholder(Compiler)
        + EOL
        + MakeTag('tr', ttClose)
        + EOL;
    end;
  end;
  // ---------------------------------------------------------------------------

begin
  inherited;
  if GetRoutine.CanCompile and (CompilersInfo.AvailableCount > 0) then
    // This placeholder occurs only in the template used for compilable snippets
    Tplt.ResolvePlaceholderHTML(
      'CompilerInfo', CompilerTableInner
    );
end;

{ TRoutineListPageHTML }

constructor TRoutineListPageHTML.Create(const View: TViewItem);
  {Class constructor. Sets up object for a view item.
    @param View [in] Provides information about item to be displayed.
  }
begin
  inherited;
  // Create list of all snippets to be displayed
  fRoutines := TRoutineList.Create;
  BuildRoutineList;
end;

destructor TRoutineListPageHTML.Destroy;
  {Class destructor. Tidies up object.
  }
begin
  FreeAndNil(fRoutines);
  inherited;
end;

function TRoutineListPageHTML.GetTemplateResName: string;
begin
  if HaveSnippets then
    Result := 'info-snippet-list-tplt.html'
  else
    Result := 'info-empty-selection-tplt.html';
end;

function TRoutineListPageHTML.HaveSnippets: Boolean;
  {Checks if there are snippets in list.
    @return True if there are snippets in list, False if not.
  }
begin
  Result := Routines.Count > 0;
end;

function TRoutineListPageHTML.RoutineTableInner: string;
  {Builds a sequence of table rows each containing a link to the snippet
  along with its description.
    @return Required table rows.
  }
var
  Snippet: TRoutine;  // references each snippet in list
begin
  Result := '';
  for Snippet in Routines do
    Result := Result + RoutineTableRow(Snippet);
end;

function TRoutineListPageHTML.RoutineTableRow(const Routine: TRoutine): string;
  {Builds a table row containing cells with a link to a snippet and a
  description of the snippet.
    @param Routine [in] Snippet to be included in row.
    @return Required table row.
  }
begin
  Result := MakeCompoundTag(
    'tr',
    MakeCompoundTag(
      'td', RoutineALink(Routine.Name, Routine.UserDefined)
    )
    + MakeCompoundTag(
      'td', MakeSafeHTMLText(Routine.Description)
    )
  )
end;

{ TCategoryPageHTML }

procedure TCategoryPageHTML.BuildRoutineList;
  {Stores all snippets to be displayed in Routines property.
  }
begin
  Query.GetCatSelection(View.Category, Routines);
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
    if View.Category.UserDefined then
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
  Tplt.ResolvePlaceholderText('Heading', View.Category.Description);
  if HaveSnippets then
  begin
    Tplt.ResolvePlaceholderText('Narrative', sNarrative);
    Tplt.ResolvePlaceholderHTML('Routines', RoutineTableInner);
  end
  else
    Tplt.ResolvePlaceholderText('Note', sNote);
end;

{ TAlphaListPageHTML }

procedure TAlphaListPageHTML.BuildRoutineList;
  {Stores all snippets to be displayed in Routines property.
  }
var
  Snippet: TRoutine;
begin
  Routines.Clear;
  for Snippet in Query.Selection do
  begin
    if Snippet.Name[1] = View.AlphaChar.Letter then
      Routines.Add(Snippet);
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
  Tplt.ResolvePlaceholderText('Heading', View.AlphaChar.Letter);
  if HaveSnippets then
  begin
    Tplt.ResolvePlaceholderText(
      'Narrative', Format(sNarrative, [View.AlphaChar.Letter])
    );
    Tplt.ResolvePlaceholderHTML('Routines', RoutineTableInner);
  end
  else
    Tplt.ResolvePlaceholderText('Note', Format(sNote, [View.AlphaChar.Letter]));
end;

{ TSnipKindPageHTML }

procedure TSnipKindPageHTML.BuildRoutineList;
  {Stores all snippets to be displayed in Routines property.
  }
var
  Snippet: TRoutine;
begin
  Routines.Clear;
  for Snippet in Query.Selection do
  begin
    if Snippet.Kind = View.SnippetKind.Kind then
      Routines.Add(Snippet);
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
    'Heading', Format(sHeading, [View.SnippetKind.Description])
  );
  if HaveSnippets then
  begin
    Tplt.ResolvePlaceholderText(
      'Narrative',
      Format(sNarrative, [AnsiLowerCase(View.SnippetKind.Description)])
    );
    Tplt.ResolvePlaceholderHTML('Routines', RoutineTableInner);
  end
  else
    Tplt.ResolvePlaceholderText(
      'Note', Format(sNote, [AnsiLowerCase(View.SnippetKind.Description)])
    );
end;

{ TNoCompCheckPageHTML }

procedure TNoCompCheckPageHTML.Generate(const Stm: TStream);
  {Generates HTML body content for "No compiler check available" pages.
    @param Stm [in] Stream that receives body HTML.
  }
var
  RS: TResourceStream;  // stream used to access HTML template resource
begin
  RS := TResourceStream.Create(HInstance, 'nocompcheck-body.html', RT_HTML);
  try
    Stm.CopyFrom(RS, 0);  // copy whole stream
  finally
    RS.Free;
  end;
end;

end.


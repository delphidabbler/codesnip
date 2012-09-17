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
 * Portions created by the Initial Developer are Copyright (C) 2005-2010 Peter
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
  Compilers.UGlobals, UHTMLTemplate, USnippets, UView;


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
      {Object constructor. Sets up object for a view item.
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
      {Object constructor. Sets up object for a view item.
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
      {Object constructor. Sets up object for a view item.
        @param View [in] Provides information about item to be displayed.
      }
    destructor Destroy; override;
      {Object destructor. Tidies up object.
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
  SysUtils, Character,
  // Project
  Compilers.UCompilers, UCompResHTML, UConsts, UCSSUtils, UHTMLUtils,
  UHTMLDetailUtils, UJavaScriptUtils, UQuery, URoutineHTML, UUtils;


{ TDetailPageHTML }

constructor TDetailPageHTML.Create(const View: TViewItem);
  {Object constructor. Sets up object for a view item.
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
  {Object constructor. Sets up object for a view item.
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
    Tplt.ResolvePlaceholderHTML('RoutineName', RoutineHTML.SnippetName);
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
    Tplt.ResolvePlaceholderHTML('Description', InfoHTML.Description);
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
  {Object constructor. Sets up object for a view item.
    @param View [in] Provides information about item to be displayed.
  }
begin
  inherited;
  // Create list of all snippets to be displayed
  fRoutines := TRoutineList.Create;
  BuildRoutineList;
end;

destructor TRoutineListPageHTML.Destroy;
  {Object destructor. Tidies up object.
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
    if TCharacter.ToUpper(Snippet.Name[1])
      = TCharacter.ToUpper(View.AlphaChar.Letter) then
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


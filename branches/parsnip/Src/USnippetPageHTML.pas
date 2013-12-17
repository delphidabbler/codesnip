{
 * This Source Code Form is subject to the terms of the Mozilla Public License,
 * v. 2.0. If a copy of the MPL was not distributed with this file, You can
 * obtain one at http://mozilla.org/MPL/2.0/
 *
 * Copyright (C) 2012-2013, Peter Johnson (www.delphidabbler.com).
 *
 * $Rev$
 * $Date$
 *
 * Defines classes etc that render different fragments of information about a
 * snippet as HTML for display in the detail pane. Page content is flexible and
 * user configurable.
}


unit USnippetPageHTML;


interface


uses
  // Project
  CS.Database.Types,
  USnippetHTML,
  USnippetPageStructure;


type
  ///  <summary>Advanced record use to render a "page" of HTML containing all
  ///  required fragments of a snippet.</summary>
  ///  <remarks>This is not strictly a complete HTML page, just the user-
  ///  configured fragments of page. Constant page elements are rendered
  ///  elsewhere.</remarks>
  TSnippetPageHTML = record
  public
    ///  <summary>Renders HTML description of given snippet, using the fragments
    ///  specified in preferences.</summary>
    class function Render(Snippet: ISnippet): string; static;
  end;

type
  ///  <summary>Base class for classes that render a snippet fragment as HTML.
  ///  </summary>
  ///  <remarks>Each sub class renders a different fragment of snippet
  ///  information and must return it via overridden ToString method.</remarks>
  TSnippetHTMLFragment = class abstract(TObject)
  strict private
    var
      ///  <summary>Value of SnippetHTML property.</summary>
      fSnippetHTML: TSnippetHTML;
  strict protected
    ///  <summary>Object that renders snippet information in HTML.</summary>
    ///  <remarks>For use in sub-classes.</remarks>
    property SnippetHTML: TSnippetHTML read fSnippetHTML;
  public
    ///  <summary>Constructs object to render fragment of given snippet.
    ///  </summary>
    constructor Create(Snippet: ISnippet);
    ///  <summary>Destroys fragement object.</summary>
    destructor Destroy; override;
  end;

type
  ///  <summary>Factory used to instantiate objects used to generate HTML
  ///  fragments describing a snippet.</summary>
  TSnippetHTMLFragmentFactory = record
  public
    ///  <summary>Generates a TSnippetHTMLFragment instance used to specify a
    ///  specified fragment of a specified snippet.</summary>
    ///  <param name="FragKind">TSnippetPagePartId [in] Id of required fragment.
    ///  </param>
    ///  <param name="Snippet">TSnippet [in] Snippet for which fragment is to be
    ///  rendered.</param>
    ///  <returns>TSnippetHTMLFragment. Required renderer object.</returns>
    ///  <remarks>Caller is responsible for freeing returned object.</remarks>
    class function Create(FragKind: TSnippetPagePartId; Snippet: ISnippet):
      TSnippetHTMLFragment; static;
  end;


implementation


uses
  // Delphi
  SysUtils,
  // Project
  UHTMLUtils, UPreferences;


type
  TSnippetHTMLFragmentClass = class of TSnippetHTMLFragment;

type
  ///  <summary>Base class for classes that render HTML describing a snippet
  ///  fragment comprising fixed prefix text following by some snippet
  ///  information.</summary>
  TPrefixedSnippetHTMLFragment = class abstract(TSnippetHTMLFragment)
  strict protected
    ///  <summary>Renders required prefixed HTML.</summary>
    ///  <param name="Prefix">string [in] Prefix as valid HTML text.</param>
    ///  <param name="Id">string [in] Unique id of HTML that spans Content.
    ///  </param>
    ///  <param name="Content">string [in] Snippet dependant content, as valid
    ///  HTML text.</param>
    ///  <returns>string. Required HTML.</returns>
    class function Render(const Prefix, Id, Content: string): string;
  end;

type
  ///  <summary>Class that renders "Description" HTML fragment for a snippet.
  ///  </summary>
  TSnippetDescHTMLFragment = class(TSnippetHTMLFragment)
  public
    ///  <summary>Renders "Description" fragment as HTML.</summary>
    function ToString: string; override;
  end;

type
  ///  <summary>Class that renders "Source Code" HTML fragment for a snippet.
  ///  </summary>
  TSnippetSourceCodeHTMLFragment = class(TSnippetHTMLFragment)
  public
    ///  <summary>Renders "Source Code" fragment as HTML.</summary>
    function ToString: string; override;
  end;

type
  ///  <summary>Class that renders "Type" HTML fragment for a snippet.
  ///  </summary>
  TSnippetKindHTMLFragment = class(TPrefixedSnippetHTMLFragment)
  public
    ///  <summary>Renders "Type" fragment as HTML.</summary>
    function ToString: string; override;
  end;

type
  ///  <summary>Class that renders "Tags" HTML fragment for a snippet.</summary>
  TSnippetTagsHTMLFragment = class(TPrefixedSnippetHTMLFragment)
  public
    ///  <summary>Renders "Tags" fragment as HTML.</summary>
    function ToString: string; override;
  end;

type
  ///  <summary>Class that renders "Required Units List" HTML fragment for a
  ///  snippet.</summary>
  TSnippetUnitsHTMLFragment = class(TPrefixedSnippetHTMLFragment)
  public
    ///  <summary>Renders "Required Units List" fragment as HTML.</summary>
    function ToString: string; override;
  end;

type
  ///  <summary>Class that renders "Required Snippets List" HTML fragment for a
  ///  snippet.</summary>
  TSnippetDependsHTMLFragment = class(TPrefixedSnippetHTMLFragment)
  public
    ///  <summary>Renders "Required Snippets List" fragment as HTML.</summary>
    function ToString: string; override;
  end;

type
  ///  <summary>Class that renders "Cross Reference List" HTML fragment for a
  ///  snippet.</summary>
  TSnippetXRefsHTMLFragment = class(TPrefixedSnippetHTMLFragment)
  public
    ///  <summary>Renders "Cross Reference List" fragment as HTML.</summary>
    function ToString: string; override;
  end;

type
  ///  <summary>Class that renders "Compile Results Table" HTML fragment for a
  ///  snippet.</summary>
  TSnippetCompileResultsHTMLFragment = class(TSnippetHTMLFragment)
  public
    ///  <summary>Renders "Compile Results Table" fragment as HTML.</summary>
    function ToString: string; override;
  end;

type
  ///  <summary>Class that renders "Extra Information" HTML fragment for a
  ///  snippet.</summary>
  TSnippetNotesHTMLFragment = class(TSnippetHTMLFragment)
  public
    ///  <summary>Renders "Notes" fragment as HTML.</summary>
    function ToString: string; override;
  end;

{ TSnippetHTMLFragment }

constructor TSnippetHTMLFragment.Create(Snippet: ISnippet);
begin
  Assert(Assigned(Snippet), ClassName + '.Create: Snippet is nil');
  inherited Create;
  fSnippetHTML := TSnippetHTML.Create(Snippet);
end;

destructor TSnippetHTMLFragment.Destroy;
begin
  fSnippetHTML.Free;
  inherited;
end;

{ TPrefixedSnippetHTMLFragment }

class function TPrefixedSnippetHTMLFragment.Render(const Prefix, Id,
  Content: string): string;
begin
  Result := THTML.CompoundTag(
    'p',
    THTML.CompoundTag('strong', Prefix) + ' ' +
      THTML.CompoundTag('span', THTMLAttributes.Create('id', Id), Content)
  );
end;

{ TSnippetDescHTMLFragment }

function TSnippetDescHTMLFragment.ToString: string;
begin
  Result := THTML.CompoundTag(
    'div', THTMLAttributes.Create('id', 'description'), SnippetHTML.Description
  );
end;

{ TSnippetSourceCodeHTMLFragment }

function TSnippetSourceCodeHTMLFragment.ToString: string;
begin
  Result := THTML.CompoundTag(
    'div', THTMLAttributes.Create('id', 'sourcecode'), SnippetHTML.SourceCode
  );
end;

{ TSnippetKindHTMLFragment }

function TSnippetKindHTMLFragment.ToString: string;
resourcestring
  sPrefix = 'Snippet type:';
begin
  Result := Render(sPrefix, 'kind', SnippetHTML.SnippetKind);
end;

{ TSnippetTagsHTMLFragment }

function TSnippetTagsHTMLFragment.ToString: string;
resourcestring
  sPrefix = 'Tags:';
begin
  Result := Render(sPrefix, 'tags', SnippetHTML.Tags);
end;

{ TSnippetUnitsHTMLFragment }

function TSnippetUnitsHTMLFragment.ToString: string;
resourcestring
  sPrefix = 'Required units:';
begin
  Result := Render(sPrefix, 'units', SnippetHTML.Units);
end;

{ TSnippetDependsHTMLFragment }

function TSnippetDependsHTMLFragment.ToString: string;
resourcestring
  sPrefix = 'Required snippets:';
begin
  Result := Render(sPrefix, 'depends', SnippetHTML.Depends);
end;

{ TSnippetXRefsHTMLFragment }

function TSnippetXRefsHTMLFragment.ToString: string;
resourcestring
  sPrefix = 'See also:';
begin
  Result := Render(sPrefix, 'xrefs', SnippetHTML.XRefs);
end;

{ TSnippetCompileResultsHTMLFragment }

function TSnippetCompileResultsHTMLFragment.ToString: string;
begin
  Result := THTML.CompoundTag(
    'div',
    THTMLAttributes.Create('id', 'compile-results'),
    THTML.CompoundTag(
      'table',
      THTMLAttributes.Create(
        [
          THTMLAttribute.Create('class', 'comptable'),
          THTMLAttribute.Create('cellspacing', '1'),
          THTMLAttribute.Create('cellpadding', '4')
        ]
      ),
      SnippetHTML.CompileResults
    )
  );
end;

{ TSnippetNotesHTMLFragment }

function TSnippetNotesHTMLFragment.ToString: string;
begin
  Result := THTML.CompoundTag(
    'div', THTMLAttributes.Create('id', 'notes'), SnippetHTML.Notes
  );
end;

{ TSnippetHTMLFragmentFactory }

class function TSnippetHTMLFragmentFactory.Create(FragKind: TSnippetPagePartId;
  Snippet: ISnippet): TSnippetHTMLFragment;
const
  Map: array[TSnippetPagePartId] of TSnippetHTMLFragmentClass = (
    TSnippetDescHTMLFragment,           // sppDescription,
    TSnippetSourceCodeHTMLFragment,     // sppSourceCode,
    TSnippetKindHTMLFragment,           // sppKind,
    TSnippetTagsHTMLFragment,           // sppTags,
    TSnippetUnitsHTMLFragment,          // sppUnits,
    TSnippetDependsHTMLFragment,        // sppDepends,
    TSnippetXRefsHTMLFragment,          // sppXRefs,
    TSnippetCompileResultsHTMLFragment, // sppCompileResults,
    TSnippetNotesHTMLFragment           // sppNotes
  );
begin
  Result := Map[FragKind].Create(Snippet);
end;

{ TSnippetPageHTML }

class function TSnippetPageHTML.Render(Snippet: ISnippet): string;
var
  SB: TStringBuilder;
  PageStruct: TSnippetPageStructure;
  Part: TSnippetPagePart;
  Fragment: TSnippetHTMLFragment;

begin
  PageStruct := Preferences.PageStructures[Snippet.KindID];
  SB := TStringBuilder.Create;
  try
    for Part in PageStruct.Parts do
    begin
      Fragment := TSnippetHTMLFragmentFactory.Create(Part.Id, Snippet);
      try
        SB.AppendLine(Fragment.ToString);
      finally
        Fragment.Free;
      end;
    end;
    Result := SB.ToString;
  finally
    SB.Free;
  end;
end;

end.


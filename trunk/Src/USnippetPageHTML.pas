{
 * This Source Code Form is subject to the terms of the Mozilla Public License,
 * v. 2.0. If a copy of the MPL was not distributed with this file, You can
 * obtain one at http://mozilla.org/MPL/2.0/
 *
 * Copyright (C) 2012, Peter Johnson (www.delphidabbler.com).
 *
 * $Rev$
 * $Date$
 *
 * Defines classes etc that render different fragments of information about a
 * snippet as HTML for display in the detail pane. Page content is flexible and
 * user defined.
}


unit USnippetPageHTML;

// TODO: Rationalise this page with USnippetHTML and UDetailPageHTML

interface

uses
  DB.USnippet, USnippetHTML, USnippetPageStructure;

type
  TSnippetPageHTML = record
  public
    class function Render(const Snippet: TSnippet): string; static;
  end;

type
  TSnippetHTMLFragment = class abstract(TObject)
  strict private
    var
      fSnippetHTML: TSnippetHTML;
  strict protected
    property SnippetHTML: TSnippetHTML read fSnippetHTML;
  public
    constructor Create(Snippet: TSnippet);
    destructor Destroy; override;
  end;

type
  TSnippetHTMLFragmentFactory = record
  public
    class function Create(FragKind: TSnippetPagePartId; Snippet: TSnippet):
      TSnippetHTMLFragment; static;
  end;

implementation

uses
  SysUtils,
  UHTMLUtils, UPreferences;

type
  TSnippetHTMLFragmentClass = class of TSnippetHTMLFragment;

type
  TPrefixedSnippetHTMLFragment = class abstract(TSnippetHTMLFragment)
  strict protected
    class function Render(const Prefix, Id, Content: string): string;
  end;

type
  TSnippetDescHTMLFragment = class(TSnippetHTMLFragment)
  public
    function ToString: string; override;
  end;

type
  TSnippetSourceCodeHTMLFragment = class(TSnippetHTMLFragment)
  public
    function ToString: string; override;
  end;

type
  TSnippetKindHTMLFragment = class(TPrefixedSnippetHTMLFragment)
  public
    function ToString: string; override;
  end;

type
  TSnippetCategoryHTMLFragment = class(TPrefixedSnippetHTMLFragment)
  public
    function ToString: string; override;
  end;

type
  TSnippetUnitsHTMLFragment = class(TPrefixedSnippetHTMLFragment)
  public
    function ToString: string; override;
  end;

type
  TSnippetDependsHTMLFragment = class(TPrefixedSnippetHTMLFragment)
  public
    function ToString: string; override;
  end;

type
  TSnippetXRefsHTMLFragment = class(TPrefixedSnippetHTMLFragment)
  public
    function ToString: string; override;
  end;

type
  TSnippetCompileResultsHTMLFragment = class(TSnippetHTMLFragment)
  public
    function ToString: string; override;
  end;

type
  TSnippetExtraHTMLFragment = class(TSnippetHTMLFragment)
  public
    function ToString: string; override;
  end;

{ TSnippetHTMLFragment }

constructor TSnippetHTMLFragment.Create(Snippet: TSnippet);
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

{ TSnippetCategoryHTMLFragment }

function TSnippetCategoryHTMLFragment.ToString: string;
resourcestring
  sPrefix = 'Category:';
begin
  Result := Render(sPrefix, 'category', SnippetHTML.Category);
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
    'table',
    THTMLAttributes.Create(
      [
        THTMLAttribute.Create('class', 'comptable'),
        THTMLAttribute.Create('cellspacing', '1'),
        THTMLAttribute.Create('cellpadding', '4')
      ]
    ),
    SnippetHTML.CompileResults
  );
end;

{ TSnippetExtraHTMLFragment }

function TSnippetExtraHTMLFragment.ToString: string;
begin
  Result := THTML.CompoundTag(
    'div', THTMLAttributes.Create('id', 'extra'), SnippetHTML.Extra
  );
end;

{ TSnippetHTMLFragmentFactory }

class function TSnippetHTMLFragmentFactory.Create(FragKind: TSnippetPagePartId;
  Snippet: TSnippet): TSnippetHTMLFragment;
const
  Map: array[TSnippetPagePartId] of TSnippetHTMLFragmentClass = (
    TSnippetDescHTMLFragment,           // sppDescription,
    TSnippetSourceCodeHTMLFragment,     // sppSourceCode,
    TSnippetKindHTMLFragment,           // sppKind,
    TSnippetCategoryHTMLFragment,       // sppCategory,
    TSnippetUnitsHTMLFragment,          // sppUnits,
    TSnippetDependsHTMLFragment,        // sppDepends,
    TSnippetXRefsHTMLFragment,          // sppXRefs,
    TSnippetCompileResultsHTMLFragment, // sppCompileResults,
    TSnippetExtraHTMLFragment           // sppExtra
  );
begin
  Result := Map[FragKind].Create(Snippet);
end;

{ TSnippetPageHTML }

class function TSnippetPageHTML.Render(const Snippet: TSnippet):
  string;
var
  SB: TStringBuilder;
  PageStruct: TSnippetPageStructure;
  Part: TSnippetPagePart;
  Fragment: TSnippetHTMLFragment;

begin
  PageStruct := Preferences.PageStructures[Snippet.Kind];
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


{
 * USnippetHTML.pas
 *
 * Classes that generates HTML used to display snippets in detail pane.
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
 * The Original Code is USnippetHTML.pas, formerly URoutineHTML.pas.
 *
 * The Initial Developer of the Original Code is Peter Johnson
 * (http://www.delphidabbler.com/).
 *
 * Portions created by the Initial Developer are Copyright (C) 2006-2011 Peter
 * Johnson. All Rights Reserved.
 *
 * Contributors:
 *   NONE
 *
 * ***** END LICENSE BLOCK *****
}


unit USnippetHTML;


interface


uses
  // Project
  DB.USnippet;


type
  {
  TSnippetHTML:
    Class that provides HTML used to display snippet details in information
    pane.
  }
  TSnippetHTML = class sealed(TObject)
  strict private
    // Reference to snippet for which HTML is being generated.
    fSnippet: TSnippet;
    function HiliteSource(const SourceCode: string): string;
      {Highlights source code in a style suitable for display in UI.
        @param SourceCode [in] Source code to be highlighted.
        @return Highlighted source code.
      }
    function SnippetList(const Snippets: TSnippetList): string;
      {Generates HTML of a comma separated list of snippets, where each snippet
      name is a link to the snippet.
        @param Snippets [in] List of snippets in list.
        @return HTML of snippet list or 'None' if list empty.
      }
    function EmptyListSentence: string;
      {Generates an HTML safe sentence that indicates a list is empty.
        @return Required sentenct.
      }
  public
    constructor Create(const Snippet: TSnippet);
      {Object constructor. Sets up object to provide HTML for a snippet.
        @param Snippet [in] Snippet for which to generate HTML.
      }
    function SnippetName: string;
      {Provides snippet name as valid HTML text.
        @return Required HTML.
      }
    function Description: string;
      {Provides description of snippet as valid HTML text.
        @return Required HTML.
      }
    function SnippetKind: string;
      {Provides HTML containing a description of snippet's kind.
        @return Required HTML.
      }
    function Category: string;
      {Provides HTML containing the category that the snippet belongs to.
        @return Required HTML.
      }
    function SourceCode: string;
      {Provides HTML containing snippet's source code, syntax highlighted in a
      style suitable for display in the UI.
        @return Required HTML.
      }
    function Depends: string;
      {Provides list of links to snippets on which the snippet depends.
        @return Required HTML containing either a list of links to snippets or
          text informing there are no dependencies.
      }
    function XRefs: string;
      {Provides list of links to the snippets with which this snippet is cross-
      referenced.
        @return Required HTML containing either a list of links to snippets or
          text informing there are no cross references.
      }
    function Units: string;
      {Provides comma separated list of units required by the snippet as valid
      HTML text.
        @return Required HTML of list or text informing if no units are
          required.
      }
    function Extra: string;
      {Builds valid HTML containing information from snippet's Extra property.
      May contain links and some formatting.
        @return Required HTML.
      }
  end;


implementation


uses
  // Project
  DB.UCategory, DB.UMain, DB.USnippetKind, Hiliter.UAttrs, Hiliter.UGlobals,
  Hiliter.UHiliters, UActiveTextHTML, UHTMLBuilder, UHTMLDetailUtils,
  UHTMLUtils, UStrUtils;

{ TSnippetHTML }

function TSnippetHTML.Category: string;
  {Provides HTML containing the category that the snippet belongs to.
    @return Required HTML.
  }
var
  Cat: TCategory; // category that snippet belongs to
begin
  Cat := Database.Categories.Find(fSnippet.Category);
  Assert(Assigned(Cat), ClassName + '.Category: Category not found');
  Result := StrMakeSentence(
    CategoryALink(Cat.ID, Cat.Description)
  );
end;

constructor TSnippetHTML.Create(const Snippet: TSnippet);
begin
  inherited Create;
  fSnippet := Snippet;
end;

function TSnippetHTML.Depends: string;
  {Provides list of links to snippets on which the snippet depends.
    @return Required HTML containing either a list of links to snippets or text
      informing there are no dependencies.
  }
begin
  Result := SnippetList(fSnippet.Depends);
end;

function TSnippetHTML.Description: string;
  {Provides description of snippet as valid HTML text.
    @return Required HTML.
  }
begin
  Result := MakeSafeHTMLText(StrMakeSentence(fSnippet.Description));
end;

function TSnippetHTML.EmptyListSentence: string;
  {Generates an HTML safe sentence that indicates a list is empty.
    @return Required sentenct.
  }
resourcestring
  sEmpty = 'None';  // word that indicates list is empty
begin
  Result := MakeSafeHTMLText(StrMakeSentence(sEmpty));
end;

function TSnippetHTML.Extra: string;
  {Builds valid HTML containing information from snippet's Extra property. May
  contain links and some formatting.
    @return Required HTML.
  }
begin
  Result := TActiveTextHTML.Render(fSnippet.Extra);
end;

function TSnippetHTML.HiliteSource(const SourceCode: string): string;
  {Highlights source code in a style suitable for display in UI.
    @param SourceCode [in] Source code to be highlighted.
    @return Highlighted source code.
  }
var
  Builder: THTMLBuilder;
  Renderer: IHiliteRenderer;
begin
  Builder := THTMLBuilder.Create;
  try
    Renderer := THTMLHiliteRenderer.Create(
      Builder, THiliteAttrsFactory.CreateDisplayAttrs
    );
    TSyntaxHiliter.Hilite(SourceCode, Renderer);
    Result := Builder.HTMLFragment;
  finally
    Builder.Free;
  end;
end;

function TSnippetHTML.SnippetList(const Snippets: TSnippetList): string;
  {Generates HTML of a comma separated list of snippets, where each snippet name
  is a link to the snippet.
    @param Snippets [in] List of snippets in list.
    @return HTML of snippet list or 'None' if list empty.
  }
var
  Snippet: TSnippet;  // refers to each snippet in list
begin
  if Snippets.Count = 0 then
    // There are no snippets: say so
    Result := EmptyListSentence
  else
  begin
    // Build comma separated list of snippet links
    Result := '';
    for Snippet in Snippets do
    begin
      if Result <> '' then
        Result := Result + ', ';
      Result := Result + SnippetALink(Snippet.Name, Snippet.UserDefined);
    end;
    Result := StrMakeSentence(Result);
  end;
end;

function TSnippetHTML.SnippetName: string;
begin
  Result := MakeSafeHTMLText(fSnippet.Name);
end;

function TSnippetHTML.SnippetKind: string;
  {Provides HTML containing a description of snippet's kind.
    @return Required HTML.
  }
begin
  Result := MakeSafeHTMLText(
    StrMakeSentence(TSnippetKindInfoList.Items[fSnippet.Kind].DisplayName)
  );
end;

function TSnippetHTML.SourceCode: string;
  {Provides HTML containing snippet's source code, syntax highlighted in a style
  suitable for display in the UI.
    @return Required HTML.
  }
begin
  Result := HiliteSource(fSnippet.SourceCode);
end;

function TSnippetHTML.Units: string;
  {Provides comma separated list of units required by the snippet as valid HTML
  text.
    @return Required HTML of list or text informing if no units are
      required.
  }
begin
  if fSnippet.Units.Count = 0 then
    Result := EmptyListSentence
  else
    Result := MakeSafeHTMLText(StrJoin(fSnippet.Units, ', ', False) + '.');
end;

function TSnippetHTML.XRefs: string;
  {Provides list of links to the snippets with which this snippet is cross
  referenced.
    @return Required HTML containing either a list of links to snippets or text
      informing there are no cross references.
  }
begin
  Result := SnippetList(fSnippet.XRef);
end;

end.


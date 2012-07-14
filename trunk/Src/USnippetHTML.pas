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
 * Portions created by the Initial Developer are Copyright (C) 2006-2012 Peter
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
  ActiveText.UMain, DB.USnippet;


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
    function SnippetList(const Snippets: TSnippetList): string;
    ///  <summary>Returns HTML text indicating a list is empty.</summary>
    function EmptyListSentence: string;
    ///  <summary>Renders given active text as HTML and returns it.</summary>
    function RenderActiveText(ActiveText: IActiveText): string;
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
    ///  Extra property.</summary>
    function Extra: string;
    ///  <summary>Returns HTML containing rows of a table representing snippet's
    ///  compilation results for each supported compiler.</summary>
    function CompileResults: string;
  end;


implementation


uses
  // Project
  ActiveText.UHTMLRenderer, DB.UCategory, DB.UMain, DB.USnippetKind,
  Hiliter.UAttrs, Hiliter.UGlobals, Hiliter.UHiliters, UCompResHTML,
  UHTMLBuilder, UHTMLDetailUtils, UHTMLUtils, UStrUtils;


{ TSnippetHTML }

function TSnippetHTML.Category: string;
var
  Cat: TCategory; // category that snippet belongs to
begin
  Cat := Database.Categories.Find(fSnippet.Category);
  Assert(Assigned(Cat), ClassName + '.Category: Category not found');
  Result := StrMakeSentence(
    CategoryALink(Cat.ID, Cat.Description)
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
  Result := SnippetList(fSnippet.Depends);
end;

function TSnippetHTML.Description: string;
begin
  Result := RenderActiveText(fSnippet.Description);
end;

function TSnippetHTML.EmptyListSentence: string;
resourcestring
  sEmpty = 'None';
begin
  Result := MakeSafeHTMLText(StrMakeSentence(sEmpty));
end;

function TSnippetHTML.Extra: string;
begin
  Result := RenderActiveText(fSnippet.Extra);
end;

function TSnippetHTML.RenderActiveText(ActiveText: IActiveText): string;
var
  Renderer: TActiveTextHTML;
begin
  Renderer := TActiveTextHTML.Create;
  try
    Result := Renderer.Render(ActiveText);
  finally
    Renderer.Free;
  end;
end;

function TSnippetHTML.SnippetList(const Snippets: TSnippetList): string;
var
  Snippet: TSnippet;  // refers to each snippet in list
begin
  if Snippets.IsEmpty then
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
      Result := Result + SnippetALink(
        Snippet.Name, Snippet.DisplayName, Snippet.UserDefined
      );
    end;
    Result := StrMakeSentence(Result);
  end;
end;

function TSnippetHTML.SnippetName: string;
begin
  Result := MakeSafeHTMLText(fSnippet.DisplayName);
end;

function TSnippetHTML.SnippetKind: string;
begin
  Result := MakeSafeHTMLText(
    StrMakeSentence(TSnippetKindInfoList.Items[fSnippet.Kind].DisplayName)
  );
end;

function TSnippetHTML.SourceCode: string;
var
  Builder: THTMLBuilder;      // object that assembles HTML
  Renderer: IHiliteRenderer;  // object that renders highlighted code as HTML
begin
  Builder := THTMLBuilder.Create;
  try
    Renderer := THTMLHiliteRenderer.Create(
      Builder, THiliteAttrsFactory.CreateDisplayAttrs
    );
    TSyntaxHiliter.Hilite(fSnippet.SourceCode, Renderer);
    Result := Builder.HTMLFragment;
  finally
    Builder.Free;
  end;
end;

function TSnippetHTML.Units: string;
begin
  if fSnippet.Units.Count = 0 then
    Result := EmptyListSentence
  else
    Result := MakeSafeHTMLText(StrJoin(fSnippet.Units, ', ', False) + '.');
end;

function TSnippetHTML.XRefs: string;
begin
  Result := SnippetList(fSnippet.XRef);
end;

end.


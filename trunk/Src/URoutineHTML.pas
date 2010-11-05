{
 * URoutineHTML.pas
 *
 * Set of classes that generate HTML used to display snippets in information and
 * compiler check panes.
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
 * The Original Code is URoutineHTML.pas
 *
 * The Initial Developer of the Original Code is Peter Johnson
 * (http://www.delphidabbler.com/).
 *
 * Portions created by the Initial Developer are Copyright (C) 2006-2009 Peter
 * Johnson. All Rights Reserved.
 *
 * Contributors:
 *   NONE
 *
 * ***** END LICENSE BLOCK *****
}


unit URoutineHTML;


interface


uses
  // Project
  USnippets;


type

  {
  TRoutineHTML:
    Base for classes that provide HTML fragments used in pages that describe
    snippets. Provides helper and common methods.
  }
  TRoutineHTML = class(TObject)
  strict private
    fRoutine: TRoutine; // Value of Routine property
  strict protected
    function HiliteSource(const SourceCode: string): string;
      {Highlights source code in a style suitable for display in UI.
        @param SourceCode [in] Source code to be highlighted.
        @return Highlighted source code.
      }
    property Routine: TRoutine
      read fRoutine;
      {Reference to snippet for which we're generating HTML}
  public
    constructor Create(const Routine: TRoutine);
      {Class constructor. Sets up object to provide HTML for a snippet.
        @param Routine [in] Snippet for which to generate HTML.
      }
    function RoutineName: string;
      {Provides snippet name as valid HTML text.
        @return Required HTML.
      }
  end;

  {
  TInfoHTML:
    Class that provides HTML used to display snippet details in information
    pane.
  }
  TInfoHTML = class(TRoutineHTML)
  strict private
    function RoutineList(const Routines: TRoutineList): string;
      {Generates HTML of a comma separated list of snippets, where each snippet
      name is a link to the snippet.
        @param Routines [in] List of snippets in list.
        @return HTML of snippet list or 'None' if list empty.
      }
    function EmptyListSentence: string;
      {Generates an HTML safe sentence that indicates a list is empty.
        @return Required sentenct.
      }
  public
    function RoutineDesc: string;
      {Provides description of snippet as valid HTML text.
        @return Required HTML.
      }
    function SnippetKind: string;
      {Provides HTML containing a description of snippet kind.
        @return Required HTML.
      }
    function Category: string;
      {Provides HTML containing the category that a snippet belongs to.
        @return Required HTML.
      }
    function SourceCode: string;
      {Provides HTML containing snippet's source code, syntax highlighted in a
      style suitable for display in UI.
        @return Required HTML.
      }
    function Depends: string;
      {Provides list of links to snippets on which snippet depends.
        @return Required HTML containing either a list of links to snippets or
          text informing there are no dependencies.
      }
    function XRefs: string;
      {Provides list of links to snippets with which snippet is cross-
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
  // Delphi
  SysUtils, StrUtils,
  // Project
  IntfHiliter, UActiveTextHTML, UHiliteAttrs, UHTMLDetailUtils, UHTMLUtils,
  USnippetKindInfo, USyntaxHiliters, UUtils;


{ TRoutineHTML }

constructor TRoutineHTML.Create(const Routine: TRoutine);
  {Class constructor. Sets up object to provide HTML for a snippet.
    @param Routine [in] Snippet for which to generate HTML.
  }
begin
  inherited Create;
  fRoutine := Routine;
end;

function TRoutineHTML.HiliteSource(const SourceCode: string): string;
  {Highlights source code in a style suitable for display in UI.
    @param SourceCode [in] Source code to be highlighted.
    @return Highlighted source code.
  }
var
  Hiliter: ISyntaxHiliter;  // highlighter object
begin
  Hiliter := TSyntaxHiliterFactory.CreateHiliter(hkDetailHTML);
  Result := Hiliter.Hilite(SourceCode, THiliteAttrsFactory.CreateDisplayAttrs);
end;

function TRoutineHTML.RoutineName: string;
  {Provides snippet name as valid HTML text.
    @return Required HTML.
  }
begin
  Result := MakeSafeHTMLText(Routine.Name);
end;

{ TInfoHTML }

function TInfoHTML.Category: string;
  {Provides HTML containing the category that a snippet belongs to.
    @return Required HTML.
  }
var
  Cat: TCategory; // category that snippet belongs to
begin
  Cat := Snippets.Categories.Find(Routine.Category);
  Assert(Assigned(Cat), ClassName + '.Category: Category not found');
  Result := MakeSentence(
    CategoryALink(Cat.Category, Cat.Description)
  );
end;

function TInfoHTML.Depends: string;
  {Provides list of links to snippets on which snippet depends.
    @return Required HTML containing either a list of links to snippets or text
      informing there are no dependencies.
  }
begin
  Result := RoutineList(Routine.Depends);
end;

function TInfoHTML.EmptyListSentence: string;
  {Generates an HTML safe sentence that indicates a list is empty.
    @return Required sentenct.
  }
resourcestring
  sEmpty = 'None';  // word that indicates list is empty
begin
  Result := MakeSafeHTMLText(MakeSentence(sEmpty));
end;

function TInfoHTML.Extra: string;
  {Builds valid HTML containing information from snippet's Extra property. May
  contain links and some formatting.
    @return Required HTML.
  }
begin
  Result := TActiveTextHTML.Render(Routine.Extra);
end;

function TInfoHTML.RoutineDesc: string;
  {Provides description of snippet as valid HTML text.
    @return Required HTML.
  }
begin
  Result := MakeSafeHTMLText(MakeSentence(Routine.Description));
end;

function TInfoHTML.RoutineList(const Routines: TRoutineList): string;
  {Generates HTML of a comma separated list of snippets, where each snippet name
  is a link to the snippet.
    @param Routines [in] List of snippets in list.
    @return HTML of snippet list or 'None' if list empty.
  }
var
  Routine: TRoutine;  // refers to each snippet in list
begin
  if Routines.Count = 0 then
    // There are no snippets: say so
    Result := EmptyListSentence
  else
  begin
    // Build comma separated list of snippet links
    Result := '';
    for Routine in Routines do
    begin
      if Result <> '' then
        Result := Result + ', ';
      Result := Result + RoutineALink(Routine.Name, Routine.UserDefined);
    end;
    Result := MakeSentence(Result);
  end;
end;

function TInfoHTML.SnippetKind: string;
  {Provides HTML containing a description of snippet kind.
    @return Required HTML.
  }
begin
  Result := MakeSafeHTMLText(
    MakeSentence(TSnippetKindInfoList.Instance[Routine.Kind].Description)
  );
end;

function TInfoHTML.SourceCode: string;
  {Provides HTML containing snippet's source code, syntax highlighted in a style
  suitable for display in UI.
    @return Required HTML.
  }
begin
  Result := HiliteSource(Routine.SourceCode);
end;

function TInfoHTML.Units: string;
  {Provides comma separated list of units required by the snippet as valid HTML
  text.
    @return Required HTML of list or text informing if no units are
      required.
  }
begin
  if Routine.Units.Count = 0 then
    Result := EmptyListSentence
  else
    Result := MakeSafeHTMLText(JoinStr(Routine.Units, ', ', False) + '.');
end;

function TInfoHTML.XRefs: string;
  {Provides list of links to snippets with which snippet is cross-referenced.
    @return Required HTML containing either a list of links to snippets or text
      informing there are no cross references.
  }
begin
  Result := RoutineList(Routine.XRef);
end;

end.


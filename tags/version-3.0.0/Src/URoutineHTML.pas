{
 * URoutineHTML.pas
 *
 * Set of classes that generate HTML used to display snippets in information and
 * compiler check panes.
 *
 * v1.0 of 03 Dec 2006  - Original version.
 * v1.1 of 04 Dec 2006  - Changed to use TTestUnit to generate test unit. This
 *                        causes name of test unit to change from 'TestUnit' to
 *                        a name based on routine name.
 * v1.2 of 12 Feb 2007  - Removed MakeSentence routine. Now use version in
 *                        UUtils.
 *                      - Rationalised use of MakeSentence.
 *                      - Improved generated HTML to reduce likelihood of
 *                        illegal characters being included.
 * v1.3 of 31 Oct 2007  - Added 'external-link' class to credits web-links.
 *                      - Modified to use IStringList instead of TStringList.
 * v1.4 of 26 Aug 2008  - Modified to work with revised signature of
 *                        RoutineALink routine.
 * v1.5 of 30 Dec 2008  - Made public and private sections strict.
 *                      - Changed TInfoHTML to generate a routines' extra info
 *                        from new active text TRoutine.Extra property rather
 *                        than from Credits, CreditsURL and Comments properties.
 *                      - Now access all routines in routine list using for..do.
 *                      - Added method to TInfoHTML to generate a sentence
 *                        indicating an empty list.
 * v1.6 of 20 Jun 2009  - Removed TCompHTML class.
 *                      - Added support for new REML tags in TRoutine.Extra
 *                        property.
 *                      - Added new TInfoHTML.SnippetKind method.
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
 * The Original Code is URoutineHTML.pas
 *
 * The Initial Developer of the Original Code is Peter Johnson
 * (http://www.delphidabbler.com/).
 *
 * Portions created by the Initial Developer are Copyright (C) 2006-2009 Peter
 * Johnson. All Rights Reserved.
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
  IntfHiliter, UActiveText, UHiliteAttrs, UHTMLDetailUtils, UHTMLUtils,
  USnippetKindInfo, UIStringList, USyntaxHiliters, UTestUnit, UUtils;


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
  Result := Hiliter.Hilite(SourceCode, THiliteAttrsFactory.CreateDefaultAttrs);
end;

function TRoutineHTML.RoutineName: string;
  {Provides snippet name as valid HTML text.
    @return Required HTML.
  }
begin
  Result := MakeSafeHTMLText(Routine.Name);
end;

{ TInfoHTML }

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
const
  // maps state of an active text element to equivalent HTML tag type
  cTagTypeMap: array[TActiveTextElemState] of THTMLTagType = (
    ttClose,  // fsClose: closing tag e.g. </tagname>
    ttOpen    // fsOpen: opening tag e.g. <tagname [params]>
  );
resourcestring
  sCreditsURLHint = 'Visit %s';       // hint used in <a> tag title attribute
var
  Elem: IActiveTextElem;              // each active text element
  TextElem: IActiveTextTextElem;      // a text active text element
  ActionElem: IActiveTextActionElem;  // an action active text element
  EncloseInDiv: Boolean;

  // ---------------------------------------------------------------------------
  function ClassAttr(const ClassName: string): IHTMLAttributes;
    {Creates an HTML attributes object containing a class attribute.
      @param ClassName [in] Name of class.
      @return Required HTML attributes object.
    }
  begin
    Result := THTMLAttributes.Create;
    Result.Add('class', ClassName);
  end;
  // ---------------------------------------------------------------------------

begin
  Result := '';
  // Process each active text element
  for Elem in Routine.Extra do
  begin
    if Supports(Elem, IActiveTextTextElem, TextElem) then
      // A text element: write it literally as safe HTML text
      Result := Result + MakeSafeHTMLText(TextElem.Text)
    else if Supports(Elem, IActiveTextActionElem, ActionElem) then
    begin
      // An action element: if supported output it in HTML
      case ActionElem.Kind of
        ekLink:
        begin
          // REML <a> => HTML <a class="external-link">
          //   REML href => HTML href
          if ActionElem.State = fsOpen then
            // opening tag: element's Param property is HTML href attribute
            Result := Result + AOpenTag(
              ActionElem.Param,
              '',
              '|' + Format(sCreditsURLHint, [ActionElem.Param]),
              TIStringList.Create('external-link')
            )
          else
            // an </a> tag
            Result := Result + MakeTag('a', ttClose);
        end;
        ekStrong:
          // REML <strong> => HTML <strong>
          Result := Result + MakeTag('strong', cTagTypeMap[ActionElem.State]);
        ekEm:
          // REML <em> => HTML <em>
          Result := Result + MakeTag('em', cTagTypeMap[ActionElem.State]);
        ekVar:
          // REML <var> => HTML <var class="extra">
          Result := Result + MakeTag(
            'var', cTagTypeMap[ActionElem.State], ClassAttr('extra')
          );
        ekPara:
          // REML <p> => HTML <p>
          Result := Result + MakeTag('p', cTagTypeMap[ActionElem.State]);
        ekWarning:
          // REML <warning> => HTML <span class="extra-warning">
          Result := Result + MakeTag(
            'span', cTagTypeMap[ActionElem.State], ClassAttr('extra-warning')
          );
        ekMono:
          // REML <mono> => HTML <span class="extra-mono">
          Result := Result + MakeTag(
            'span', cTagTypeMap[ActionElem.State], ClassAttr('extra-mono')
          );
        ekHeading:
          // REML <heading> => HTML <h2 class="extra">
          Result := Result + MakeTag(
            'h2', cTagTypeMap[ActionElem.State], ClassAttr('extra')
          );
        else
          {Unsupported action element type: do nothing};
      end;
    end;
  end;
  // Extra property may have "p" or "heading" tags, but may not have. So we
  // check and add enclosing "div" tags if necessary with required properties.
  // paragraph tags if
  EncloseInDiv := not Routine.Extra.IsEmpty and
    not ((Routine.Extra[0].Kind in [ekPara, ekHeading]));
  if EncloseInDiv then
    Result := MakeTag('div', ttOpen, ClassAttr('extra-wrapper')) +
      Result +
      MakeTag('div', ttClose);
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
    TSnippetKindInfoList.Instance[Routine.Kind].Description
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


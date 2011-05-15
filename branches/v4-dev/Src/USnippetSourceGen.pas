{
 * USnippetSourceGen.pas
 *
 * Implements a static class that generates source code for code snippet(s)
 * contained in a routine snippet or category view.
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
 * The Original Code is USnippetSourceGen.pas
 *
 * The Initial Developer of the Original Code is Peter Johnson
 * (http://www.delphidabbler.com/).
 *
 * Portions created by the Initial Developer are Copyright (C) 2006-2011 Peter
 * Johnson. All Rights Reserved.
 *
 * Contributor(s)
 *   NONE
 *
 * ***** END LICENSE BLOCK *****
}


unit USnippetSourceGen;


interface


uses
  // Project
  UBaseObjects, UIStringList, USourceGen, UView;


type

  {
  TSnippetSourceGen:
    Static class that generates source code for code snippet(s) contained in a
    routine snippet or category view.
  }
  TSnippetSourceGen = class sealed(TNoPublicConstructObject)
  strict private
    fContainsMainDBSnippets: Boolean;
      {Flag true if source code contains at least one snippet from main
      database, False only if source code is completely user defined}
    fGenerator: TSourceGen;
      {Object used to generate the source code}
    procedure Initialize(View: IView);
      {Initializes source code generator using information from a snippet or
      category view.
        @param View [in] View from which to retrieve source code.
      }
    function BuildHeaderComments: IStringList;
      {Creates and stores header comments to be written to head of snippet.
        @return String list containing comments.
      }
    function DoGenerate(const CommentStyle: TCommentStyle): string;
      {Generates source code for included snippets.
        @param CommentStyle [in] Style of commenting to use in source code.
        @return Required source code.
      }
  strict protected
    constructor InternalCreate(View: IView);
      {Class constructor. Sets up object to record and generate source code for
      a view.
        @param View [in] View for which we are generating source code.
      }
  public
    destructor Destroy; override;
      {Class destructor. Tears down object.
      }
    class function CanGenerate(View: IView): Boolean;
      {Checks if a valid source code snippet can be generated from a view.
        @param View [in] View to be checked.
        @return True if View is a routine snippet or a category that contains
          routine snippets for current query.
      }
    class function Generate(View: IView; const CommentStyle: TCommentStyle):
      string;
      {Generates source code of all routine snippets or categories in a view.
        @param View [in] View containing required snippet(s).
        @param CommentStyle [in] Style of commenting to use in source code.
        @return Required source code.
      }
  end;


implementation


uses
  // Delphi
  SysUtils,
  // Project
  DB.USnippet, UAppInfo, USnippetKindInfo, UQuery, UUtils;


{ TSnippetSourceGen }

function TSnippetSourceGen.BuildHeaderComments: IStringList;
  {Creates and stores header comments to be written to head of snippet.
    @return String list containing comments.
  }
var
  Header: string; // format string for header comment
resourcestring
  // Comment to be included at top of snippet
  // header used if snippet includes code from main database
  sMainHeader = 'This code snippet was generated by %0:s %1:s on %2:s. It is '
    + 'made available on an "AS IS" basis, WITHOUT WARRANTY OF ANY KIND, '
    + 'either express or implied. The code is used entirely at your own risk.';
  // header used if snippet contains only user defined snippets
  sUserHeader = 'This user defined code snippet was generated by '
    + '%0:s %1:s on %2:s.';
begin
  // Header depends on whether snippets contain any from main database or are
  // all user defined
  if fContainsMainDBSnippets then
    Header := sMainHeader
  else
    Header := sUserHeader;
  // Add the comments
  Result := TIStringList.Create;
  Result.Add(
    Format(
      Header,
      [TAppInfo.FullProgramName, TAppInfo.ProgramReleaseInfo, DateStamp]
    )
  );
end;

class function TSnippetSourceGen.CanGenerate(View: IView): Boolean;
  {Checks if a valid source code snippet can be generated from a view.
    @param View [in] View to be checked.
    @return True if View is a routine snippet or a category that contains
      routine snippets for current query.
  }
var
  CatSnippets: TSnippetList;  // list of snippets in a category
  CatView: ICategoryView;     // category view if supported
  SnipView: ISnippetView;     // snippets view if supported
begin
  Result := False;
  if Supports(View, ISnippetView, SnipView) then
    Result := SnipView.Snippet.Kind = skRoutine
  else if Supports(View, ICategoryView, CatView) then
  begin
    CatSnippets := TSnippetList.Create;
    try
      Query.GetCatSelection(CatView.Category, CatSnippets);
      Result := CatSnippets.ContainsKinds([skRoutine]);
    finally
      CatSnippets.Free;
    end;
  end;
end;

destructor TSnippetSourceGen.Destroy;
  {Class destructor. Tears down object.
  }
begin
  fGenerator.Free;
  inherited;
end;

function TSnippetSourceGen.DoGenerate(
  const CommentStyle: TCommentStyle): string;
  {Generates source code for included snippets.
    @param CommentStyle [in] Style of commenting to use in source code.
    @return Required source code.
  }
begin
  Result := fGenerator.IncFileAsString(CommentStyle, BuildHeaderComments);
end;

class function TSnippetSourceGen.Generate(View: IView;
  const CommentStyle: TCommentStyle): string;
  {Generates source code of all routine snippets or categories in a view.
    @param View [in] View containing required snippet(s).
    @param CommentStyle [in] Style of commenting to use in source code.
    @return Required source code.
  }
begin
  with InternalCreate(View) do
    try
      Result := DoGenerate(CommentStyle);
    finally
      Free;
    end;
end;

procedure TSnippetSourceGen.Initialize(View: IView);
  {Initializes source code generator using information from a snippet or
  category view.
    @param View [in] View from which to retrieve source code.
  }
var
  Snips: TSnippetList;  // list of snippets in a category to display
  Snippet: TSnippet;    // a snippet in Snips list
begin
  fContainsMainDBSnippets := False;
  // Record required snippet(s)
  if Supports(View, ISnippetView) then
  begin
    // view is single snippet: just record that
    Snippet := (View as ISnippetView).Snippet;
    fGenerator.IncludeSnippet(Snippet);
    fContainsMainDBSnippets := not Snippet.UserDefined;
  end
  else
  begin
    // view is category: record all selected snippets in category
    Snips := TSnippetList.Create;
    try
      Query.GetCatSelection((View as ICategoryView).Category, Snips);
      fGenerator.IncludeSnippets(Snips);  // ignores freeform snippets
      for Snippet in Snips do
      begin
        if not Snippet.UserDefined then
        begin
          fContainsMainDBSnippets := True;
          Break;
        end;
      end;
    finally
      Snips.Free;
    end;
  end;
end;

constructor TSnippetSourceGen.InternalCreate(View: IView);
  {Class constructor. Sets up object to record and generate source code for a
  view.
    @param View [in] View for which we are generating source code.
  }
begin
  Assert(Assigned(View), ClassName + '.InternalCreate: View is nil');
  Assert(CanGenerate(View), ClassName + '.InternalCreate: View not supported');
  inherited InternalCreate;
  fGenerator := TSourceGen.Create;
  Initialize(View);
end;

end.


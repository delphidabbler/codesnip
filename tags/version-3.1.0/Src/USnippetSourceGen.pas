{
 * USnippetSourceGen.pas
 *
 * Implements a static class that generates source code for code snippet(s)
 * contained in a routine or category view.
 *
 * v0.1 of 06 Jan 2006  - Original version.
 * v1.0 of 24 May 2006  - Improved and corrected comments.
 * v1.1 of 04 Feb 2007  - Replaced use of redundant TDetailView objects with
 *                        calls to new global query object and TViewItem
 *                        objects.
 *                      - Refactored some code to take advantage of changes.
 * v1.2 of 09 Feb 2007  - Changed to add multiple routines to a code snippet
 *                        using TSourceGen.IncludeRoutines rather than multiple
 *                        calls to IncludeRoutine.
 *                      - Changed to provide header comments to the snippet,
 *                        including disclaimer.
 * v1.3 of 02 Jul 2007  - Corrected typo in source code header comment.
 * v1.4 of 13 Sep 2008  - Changed to use a different header comment if snippet
 *                        contains only user defined routines.
 * v1.5 of 13 May 2009  - Now gets full program name from TAppInfo instead of
 *                        UGlobals unit.
 * v1.6 of 16 May 2009  - Modified to use renamed TSourceGen methods.
 *                      - Asserts now get class name from ClassName method.
 *                      - Renamed some variables and fields.
 * v2.0 of 11 Jul 2009  - Re-implemented TSnippetSourceGen as a static class:
 *                        - Now descends from TNoPublicConstructObject.
 *                        - Changed TSnippetSourceGen.Generate into a static
 *                          method.
 *                      - Added new static TSnippetSourceGen.CanGenerate method
 *                        that checks if a source code snippet can be generated
 *                        for a view.
 *                      - Changed to generate and use comments as IStringList
 *                        rather than TStringList.
 *                      - Made private section strict.
 *                      - Class sealed.
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
 * The Original Code is USnippetSourceGen.pas
 *
 * The Initial Developer of the Original Code is Peter Johnson
 * (http://www.delphidabbler.com/).
 *
 * Portions created by the Initial Developer are Copyright (C) 2006-2009 Peter
 * Johnson. All Rights Reserved.
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
    routine or category view.
  }
  TSnippetSourceGen = class sealed(TNoPublicConstructObject)
  strict private
    fContainsMainDBSnippets: Boolean;
      {Flag true if source code contains at least one snippet from main
      database, False only if source code is completely user defined}
    fGenerator: TSourceGen;
      {Object used to generate the source code}
    procedure Initialize(const View: TViewItem);
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
    constructor InternalCreate(const View: TViewItem);
      {Class constructor. Sets up object to record and generate source code for
      a view.
        @param View [in] View for which we are generating source code.
      }
  public
    destructor Destroy; override;
      {Class destructor. Tears down object.
      }
    class function CanGenerate(const View: TViewItem): Boolean;
      {Checks if a valid source code snippet can be generated from a view.
        @param View [in] View to be checked.
        @return True if View is a routine snippet or a category that contains
          routine snippets for current query.
      }
    class function Generate(const View: TViewItem;
      const CommentStyle: TCommentStyle): string;
      {Generates source code of all routine snippets in a view.
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
  UAppInfo, UQuery, USnippets, UUtils;


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
  // header used if snippet contains only user defined routines
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

class function TSnippetSourceGen.CanGenerate(const View: TViewItem): Boolean;
  {Checks if a valid source code snippet can be generated from a view.
    @param View [in] View to be checked.
    @return True if View is a routine snippet or a category that contains
      routine snippets for current query.
  }
var
  CatSnippets: TRoutineList;  // list of snippets in a category
begin
  Result := False;
  case View.Kind of
    vkRoutine:
      Result := View.Routine.Kind = skRoutine;
    vkCategory:
    begin
      CatSnippets := TRoutineList.Create;
      try
        Query.GetCatSelection(View.Category, CatSnippets);
        Result := CatSnippets.ContainsKinds([skRoutine]);
      finally
        FreeAndNil(CatSnippets);
      end;
    end;
  end;
end;

destructor TSnippetSourceGen.Destroy;
  {Class destructor. Tears down object.
  }
begin
  FreeAndNil(fGenerator);
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

class function TSnippetSourceGen.Generate(const View: TViewItem;
  const CommentStyle: TCommentStyle): string;
  {Generates source code of all routine snippets in a view.
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

procedure TSnippetSourceGen.Initialize(const View: TViewItem);
  {Initializes source code generator using information from a snippet or
  category view.
    @param View [in] View from which to retrieve source code.
  }
var
  Snips: TRoutineList; // list of snippets in a category to display
  Snippet: TRoutine;   // a snippet in Snips list
begin
  fContainsMainDBSnippets := False;
  // Record required snippet(s)
  if View.Kind = vkRoutine then
  begin
    // view is single snippet: just record that
    fGenerator.IncludeSnippet(View.Routine);
    fContainsMainDBSnippets := not View.Routine.UserDefined;
  end
  else
  begin
    // view is category: record all selected snippets in category
    Snips := TRoutineList.Create;
    try
      Query.GetCatSelection(View.Category, Snips);
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
      FreeAndNil(Snips);
    end;
  end;
end;

constructor TSnippetSourceGen.InternalCreate(const View: TViewItem);
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


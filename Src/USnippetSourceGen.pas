﻿{
 * This Source Code Form is subject to the terms of the Mozilla Public License,
 * v. 2.0. If a copy of the MPL was not distributed with this file, You can
 * obtain one at https://mozilla.org/MPL/2.0/
 *
 * Copyright (C) 2006-2023, Peter Johnson (gravatar.com/delphidabbler).
 *
 * Implements a static class that generates source code for code snippet(s)
 * contained in a routine snippet or category view.
}


unit USnippetSourceGen;


interface


uses
  // Delphi
  Generics.Collections,
  // Project
  DB.MetaData,
  DB.Vaults,
  UBaseObjects,
  UIStringList,
  USourceGen,
  UView;


type

  {
  TSnippetSourceGen:
    Static class that generates source code for code snippet(s) contained in a
    routine snippet or category view.
  }
  TSnippetSourceGen = class sealed(TNoPublicConstructObject)
  strict private
    ///  <summary>List of vaults that have contributed snippets to the source
    ///  code being generated.</summary>
    fCollections: TList<TVault>;
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
    function DoGenerate(const CommentStyle: TCommentStyle;
      const TruncateComments: Boolean): string;
      {Generates source code for included snippets.
        @param CommentStyle [in] Style of commenting to use in source code.
        @param TruncateComments [in] Whether to truncate multi paragraph
          snippet description to first paragraph in comments.
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
    class function Generate(View: IView; const CommentStyle: TCommentStyle;
      const TruncateComments: Boolean): string;
      {Generates source code of all routine snippets or categories in a view.
        @param View [in] View containing required snippet(s).
        @param CommentStyle [in] Style of commenting to use in source code.
        @param TruncateComments [in] Whether to truncate multi paragraph
          snippet description to first paragraph in comments.
        @return Required source code.
      }
  end;


implementation


uses
  // Delphi
  SysUtils,
  // Project
  DB.DataFormats,
  DB.USnippet,
  DB.USnippetKind,
  UConsts,
  UAppInfo,
  UQuery,
  UStrUtils,
  UUtils;


{ TSnippetSourceGen }

function TSnippetSourceGen.BuildHeaderComments: IStringList;
  {Creates and stores header comments to be written to head of snippet.
    @return String list containing comments.
  }
var
  MetaData: TMetaData;
  Collection: TVault;
  Credits: string;
resourcestring
  // Comment to be included at top of snippet
  // when snippets include those from main database
  sMainGenerator = 'This code snippet was generated by %0:s %1:s on %2:s.';
  sCollection = 'The code was sourced from the %s collection.';
  sCollectionList = 'The code was sourced from the following collections:';
  sCollectionCredit = 'Collection "%0:s" is licensed under the %1:s';

  function CreditsLine(const ACollection: TVault): string;
  begin
    MetaData := ACollection.MetaData;
    Result := '';
    if TMetaDataCap.License in MetaData.Capabilities then
      Result := Result + StrMakeSentence(MetaData.LicenseInfo.NameWithURL);
    if TMetaDataCap.Copyright in MetaData.Capabilities then
    begin
      if not StrIsEmpty(Result) then
        Result := Result + ' ';
      Result := Result + StrMakeSentence(MetaData.CopyrightInfo.ToString);
    end;
  end;

begin
  Result := TIStringList.Create;

  Result.Add(
    Format(
      sMainGenerator,
      [TAppInfo.FullProgramName, TAppInfo.ProgramReleaseInfo, RFC1123DateStamp]
    )
  );

  Result.Add('');
  if fCollections.Count = 1 then
    Result.Add(Format(sCollection, [fCollections[0].Name]))
  else
  begin
    Result.Add(sCollectionList);
    for Collection in fCollections do
    begin
      Result.Add('  - ' + Collection.Name);
    end;
  end;

  for Collection in fCollections do
  begin
    Credits := CreditsLine(Collection);
    if Credits <> '' then
    begin
      Result.Add('');
      Result.Add(Format(sCollectionCredit, [Collection.Name, Credits]));
    end;
  end;

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
  fCollections.Free;
  fGenerator.Free;
  inherited;
end;

function TSnippetSourceGen.DoGenerate(const CommentStyle: TCommentStyle;
  const TruncateComments: Boolean): string;
  {Generates source code for included snippets.
    @param CommentStyle [in] Style of commenting to use in source code.
    @param TruncateComments [in] Whether to truncate multi paragraph snippet
      description to first paragraph in comments.
    @return Required source code.
  }
begin
  Result := fGenerator.IncFileAsString(
    CommentStyle, TruncateComments, BuildHeaderComments
  );
end;

class function TSnippetSourceGen.Generate(View: IView;
  const CommentStyle: TCommentStyle; const TruncateComments: Boolean): string;
  {Generates source code of all routine snippets or categories in a view.
    @param View [in] View containing required snippet(s).
    @param CommentStyle [in] Style of commenting to use in source code.
    @param TruncateComments [in] Whether to truncate multi paragraph snippet
      description to first paragraph in comments.
    @return Required source code.
  }
var
  Instance: TSnippetSourceGen;
begin
  Instance := InternalCreate(View);
  try
    Result := Instance.DoGenerate(CommentStyle, TruncateComments);
  finally
    Instance.Free;
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
  Collection: TVault;
begin
  // Record required snippet(s)
  if Supports(View, ISnippetView) then
  begin
    // view is single snippet: just record that
    Snippet := (View as ISnippetView).Snippet;
    fGenerator.IncludeSnippet(Snippet);
    fCollections.Add(TVaults.Instance.GetVault(Snippet.VaultID));
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
        Collection := TVaults.Instance.GetVault(Snippet.VaultID);
        if not fCollections.Contains(Collection) then
          fCollections.Add(Collection);
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
  fCollections := TList<TVault>.Create(TVault.TComparer.Create);
  Initialize(View);
end;

end.


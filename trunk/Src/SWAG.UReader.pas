{
 * This Source Code Form is subject to the terms of the Mozilla Public License,
 * v. 2.0. If a copy of the MPL was not distributed with this file, You can
 * obtain one at http://mozilla.org/MPL/2.0/
 *
 * Copyright (C) 2013, Peter Johnson (www.delphidabbler.com).
 *
 * $Rev$
 * $Date$
 *
 * Implements a class that provides cached access to the SWAG database.
}


unit SWAG.UReader;


interface


uses
  // Delphi
  SysUtils,
  Generics.Collections,
  // Project
  SWAG.UCommon,
  SWAG.USnippetCache,
  Web.USWAGRESTMgr;


type
  TSWAGRESTCallWrapper = reference to procedure(CallProc: TProc);

type
  TSWAGReader = class(TObject)
  strict private
    const
      MaxSnippetCacheSize = 50;
    var
      fCategories: TList<TSWAGCategory>;
      fSnippetsByCategory: TDictionary<string,TList<TSWAGSnippet>>;
      fSWAGRESTMgr: TSWAGRESTMgr;
      fDefaultCallWrapper: TSWAGRESTCallWrapper;
      fSnippetCache: TSWAGSnippetCache;
    function DoFetchFullSnippet(const SnipID: Cardinal): TSWAGSnippet;
    procedure FetchCategories(CallWrapper: TSWAGRESTCallWrapper);
    procedure FetchSnippets(const CatID: string;
      CallWrapper: TSWAGRESTCallWrapper);
    function FetchSnippet(const SnippetID: Cardinal;
      CallWrapper: TSWAGRESTCallWrapper): TSWAGSnippet;
    procedure FetchFullSnippets(const SnipIDs: TList<Cardinal>;
      Snippets: TList<TSWAGSnippet>; CallWrapper: TSWAGRESTCallWrapper);
  public
    constructor Create(const DefaultRESTCallWrapper: TSWAGRESTCallWrapper);
    destructor Destroy; override;
    procedure GetCategories(const Cats: TList<TSWAGCategory>;
      CallWrapper: TSWAGRESTCallWrapper = nil);
    function GetCategory(const CatID: string;
      CallWrapper: TSWAGRESTCallWrapper = nil): TSWAGCategory;
    procedure GetSnippets(const CatID: string;
      const Snippets: TList<TSWAGSnippet>;
      CallWrapper: TSWAGRESTCallWrapper = nil);
    function GetSnippet(const ID: Cardinal;
      CallWrapper: TSWAGRESTCallWrapper = nil): TSWAGSnippet;
    procedure GetFullSnippets(SnipIDs: TList<Cardinal>;
      Snippets: TList<TSWAGSnippet>; CallWrapper: TSWAGRESTCallWrapper = nil);
  end;


implementation


uses
  // Project
  UComparers;


{ TSWAGReader }

constructor TSWAGReader.Create(
  const DefaultRESTCallWrapper: TSWAGRESTCallWrapper);
begin
  inherited Create;
  fDefaultCallWrapper := DefaultRESTCallWrapper;
  fCategories := TList<TSWAGCategory>.Create;
  fSnippetsByCategory := TObjectDictionary<string,TList<TSWAGSnippet>>.Create(
    [doOwnsValues],
    TStringEqualityComparer.Create
  );
  fSnippetCache := TSWAGSnippetCache.Create(MaxSnippetCacheSize);
  fSWAGRESTMgr := TSWAGRESTMgr.Create;
end;

destructor TSWAGReader.Destroy;
begin
  fSWAGRESTMgr.Free;
  fSnippetCache.Free;
  fSnippetsByCategory.Free;
  fCategories.Free;
  inherited;
end;

function TSWAGReader.DoFetchFullSnippet(const SnipID: Cardinal): TSWAGSnippet;
begin
  fSWAGRESTMgr.GetSnippet(
    SnipID,
    [
      'id', 'category', 'file_name', 'datestamp', 'title', 'author',
      'source_code', 'is_document'
    ],
    Result
  );
end;

procedure TSWAGReader.FetchCategories(CallWrapper: TSWAGRESTCallWrapper);
begin
  fCategories.Clear;
  if not Assigned(CallWrapper) then
    CallWrapper := fDefaultCallWrapper;
  CallWrapper(
    procedure
    var
      Cat: TSWAGCategory;
      CatList: TList<TSWAGCategory>;
    begin
      CatList := TList<TSWAGCategory>.Create;
      try
        fSWAGRESTMgr.GetCategories(CatList);
        for Cat in CatList do
          fCategories.Add(Cat);
      finally
        CatList.Free;
      end
    end
  );
end;

procedure TSWAGReader.FetchFullSnippets(const SnipIDs: TList<Cardinal>;
  Snippets: TList<TSWAGSnippet>; CallWrapper: TSWAGRESTCallWrapper);
begin
  if not Assigned(CallWrapper) then
    CallWrapper := fDefaultCallWrapper;
  CallWrapper(
    procedure
    var
      SnipID: Cardinal;
      Snippet: TSWAGSnippet;
    begin
      for SnipID in SnipIDs do
      begin
        Snippet := DoFetchFullSnippet(SnipID);
        Snippets.Add(Snippet);
      end;
    end
  );
end;

function TSWAGReader.FetchSnippet(const SnippetID: Cardinal;
  CallWrapper: TSWAGRESTCallWrapper): TSWAGSnippet;
var
  Snippet: TSWAGSnippet;
begin
  if not Assigned(CallWrapper) then
    CallWrapper := fDefaultCallWrapper;
  CallWrapper(
    procedure
    begin
      Snippet := DoFetchFullSnippet(SnippetID);
    end
  );
  Result := Snippet;
end;

procedure TSWAGReader.FetchSnippets(const CatID: string;
  CallWrapper: TSWAGRESTCallWrapper);
begin
  if not Assigned(CallWrapper) then
    CallWrapper := fDefaultCallWrapper;
  CallWrapper(
    procedure
    var
      SnipList: TList<TSWAGSnippet>;
    begin
      SnipList := TList<TSWAGSnippet>.Create;
      fSWAGRESTMgr.GetSnippets(CatID, ['id', 'title'], SnipList);
      fSnippetsByCategory.Add(CatID, SnipList);
    end
  );
end;

procedure TSWAGReader.GetCategories(const Cats: TList<TSWAGCategory>;
  CallWrapper: TSWAGRESTCallWrapper);
var
  Cat: TSWAGCategory;
begin
  if fCategories.Count = 0 then
    FetchCategories(CallWrapper);
  for Cat in fCategories do
    Cats.Add(Cat);
end;

function TSWAGReader.GetCategory(const CatID: string;
  CallWrapper: TSWAGRESTCallWrapper): TSWAGCategory;
var
  Cat: TSWAGCategory;
begin
  if fCategories.Count = 0 then
    FetchCategories(CallWrapper);
  for Cat in fCategories do
    if Cat.ID = CatID then
      Exit(Cat);
  Result.ID := '';
  Result.Title := '';
end;

procedure TSWAGReader.GetFullSnippets(SnipIDs: TList<Cardinal>;
  Snippets: TList<TSWAGSnippet>; CallWrapper: TSWAGRESTCallWrapper);
var
  RemoteSnippetIDs: TList<Cardinal>;
  DownloadedSnippets: TList<TSWAGSnippet>;
  Snippet: TSWAGSnippet;
  SnipID: Cardinal;
begin
  RemoteSnippetIDs := TList<Cardinal>.Create;
  try
    for SnipID in SnipIDs do
    begin
      if fSnippetCache.Retrieve(SnipID, Snippet) then
        Snippets.Add(Snippet)
      else
        RemoteSnippetIDs.Add(SnipID);
    end;
    if RemoteSnippetIDs.Count > 0 then
    begin
      DownloadedSnippets := TList<TSWAGSnippet>.Create;
      try
        FetchFullSnippets(RemoteSnippetIDs, DownloadedSnippets, CallWrapper);
        for Snippet in DownloadedSnippets do
        begin
          fSnippetCache.Add(Snippet);
          Snippets.Add(Snippet);
        end;
      finally
        DownloadedSnippets.Free;
      end;
    end;
  finally
    RemoteSnippetIDs.Free;
  end;
end;

function TSWAGReader.GetSnippet(const ID: Cardinal;
  CallWrapper: TSWAGRESTCallWrapper): TSWAGSnippet;
begin
  if not fSnippetCache.Retrieve(ID, Result) then
  begin
    Result := FetchSnippet(ID, CallWrapper);
    fSnippetCache.Add(Result);
  end;
end;

procedure TSWAGReader.GetSnippets(const CatID: string;
  const Snippets: TList<TSWAGSnippet>; CallWrapper: TSWAGRESTCallWrapper);
var
  Snippet: TSWAGSnippet;
begin
  if not fSnippetsByCategory.ContainsKey(CatID) then
    FetchSnippets(CatID, CallWrapper);
  for Snippet in fSnippetsByCategory[CatID] do
    Snippets.Add(Snippet);
end;

end.


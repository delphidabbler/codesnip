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
  UExceptions,
  Web.USWAGRESTMgr;


type
  TSWAGReader = class(TObject)
  public
    type
      TRESTCallWrapper = reference to procedure (CallProc: TProc);
  strict private
    const
      MaxSnippetCacheSize = 50;
    var
      fCategories: TList<TSWAGCategory>;
      fSnippetsByCategory: TDictionary<string,TList<TSWAGSnippet>>;
      fSWAGRESTMgr: TSWAGRESTMgr;
      fDefaultCallWrapper: TRESTCallWrapper;
      fSnippetCache: TSWAGSnippetCache;
    procedure HandleException(E: Exception);
    procedure DownloadCategories(const Cats: TList<TSWAGCategory>);
    procedure DownloadPartialSnippets(const CatID: string;
      const SnipList: TList<TSWAGSnippet>);
    function DownloadCompleteSnippet(const SnipID: Cardinal): TSWAGSnippet;
    procedure FetchCategories(CallWrapper: TRESTCallWrapper);
    procedure FetchPartialSnippets(const CatID: string;
      CallWrapper: TRESTCallWrapper);
    function FetchCompleteSnippet(const SnippetID: Cardinal;
      CallWrapper: TRESTCallWrapper): TSWAGSnippet;
    procedure FetchCompleteSnippets(const SnipIDs: TList<Cardinal>;
      Snippets: TList<TSWAGSnippet>; CallWrapper: TRESTCallWrapper);
  public
    constructor Create(const DefaultRESTCallWrapper: TRESTCallWrapper);
    destructor Destroy; override;
    procedure GetCategories(const Cats: TList<TSWAGCategory>;
      CallWrapper: TRESTCallWrapper = nil);
    procedure GetPartialSnippets(const CatID: string;
      const Snippets: TList<TSWAGSnippet>; CallWrapper: TRESTCallWrapper = nil);
    function GetCompleteSnippet(const ID: Cardinal;
      CallWrapper: TRESTCallWrapper = nil): TSWAGSnippet;
    procedure GetCompleteSnippets(SnipIDs: TList<Cardinal>;
      Snippets: TList<TSWAGSnippet>; CallWrapper: TRESTCallWrapper = nil);
  end;

type
  ESWAGReader = class(ECodeSnip);


implementation


uses
  // Project
  UComparers,
  UConsts,
  Web.UExceptions;


{ TSWAGReader }

constructor TSWAGReader.Create(
  const DefaultRESTCallWrapper: TRESTCallWrapper);
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

procedure TSWAGReader.DownloadCategories(const Cats: TList<TSWAGCategory>);
begin
  try
    fSWAGRESTMgr.GetCategories(Cats);
  except
    HandleException(ExceptObject as Exception);
  end;
end;

function TSWAGReader.DownloadCompleteSnippet(const SnipID: Cardinal):
  TSWAGSnippet;
begin
  try
    fSWAGRESTMgr.GetSnippet(
      SnipID,
      [
        'id', 'category', 'file_name', 'datestamp', 'title', 'author',
        'source_code', 'is_document'
      ],
      Result
    );
  except
    HandleException(ExceptObject as Exception);
  end;
end;

procedure TSWAGReader.DownloadPartialSnippets(const CatID: string;
  const SnipList: TList<TSWAGSnippet>);
begin
  try
    fSWAGRESTMgr.GetSnippets(CatID, ['id', 'title'], SnipList);
  except
    HandleException(ExceptObject as Exception);
  end;
end;

procedure TSWAGReader.FetchCategories(CallWrapper: TRESTCallWrapper);
begin
  fCategories.Clear;
  if not Assigned(CallWrapper) then
    CallWrapper := fDefaultCallWrapper;
  CallWrapper(
    procedure
    var
      CatList: TList<TSWAGCategory>;
    begin
      CatList := TList<TSWAGCategory>.Create;
      try
        DownloadCategories(CatList);
        fCategories.AddRange(CatList);
      finally
        CatList.Free;
      end
    end
  );
end;

function TSWAGReader.FetchCompleteSnippet(const SnippetID: Cardinal;
  CallWrapper: TRESTCallWrapper): TSWAGSnippet;
var
  Snippet: TSWAGSnippet;
begin
  if not Assigned(CallWrapper) then
    CallWrapper := fDefaultCallWrapper;
  CallWrapper(
    procedure
    begin
      Snippet := DownloadCompleteSnippet(SnippetID);
    end
  );
  Result := Snippet;
end;

procedure TSWAGReader.FetchCompleteSnippets(const SnipIDs: TList<Cardinal>;
  Snippets: TList<TSWAGSnippet>; CallWrapper: TRESTCallWrapper);
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
        Snippet := DownloadCompleteSnippet(SnipID);
        Snippets.Add(Snippet);
      end;
    end
  );
end;

procedure TSWAGReader.FetchPartialSnippets(const CatID: string;
  CallWrapper: TRESTCallWrapper);
begin
  if not Assigned(CallWrapper) then
    CallWrapper := fDefaultCallWrapper;
  CallWrapper(
    procedure
    var
      SnipList: TList<TSWAGSnippet>;
    begin
      SnipList := TList<TSWAGSnippet>.Create;
      DownloadPartialSnippets(CatID, SnipList);
      fSnippetsByCategory.Add(CatID, SnipList);
    end
  );
end;

procedure TSWAGReader.GetCategories(const Cats: TList<TSWAGCategory>;
  CallWrapper: TRESTCallWrapper);
begin
  if fCategories.Count = 0 then
    FetchCategories(CallWrapper);
  Cats.AddRange(fCategories);
end;

function TSWAGReader.GetCompleteSnippet(const ID: Cardinal;
  CallWrapper: TRESTCallWrapper): TSWAGSnippet;
begin
  if not fSnippetCache.Retrieve(ID, Result) then
  begin
    Result := FetchCompleteSnippet(ID, CallWrapper);
    fSnippetCache.Add(Result);
  end;
end;

procedure TSWAGReader.GetCompleteSnippets(SnipIDs: TList<Cardinal>;
  Snippets: TList<TSWAGSnippet>; CallWrapper: TRESTCallWrapper);
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
        FetchCompleteSnippets(
          RemoteSnippetIDs, DownloadedSnippets, CallWrapper
        );
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

procedure TSWAGReader.GetPartialSnippets(const CatID: string;
  const Snippets: TList<TSWAGSnippet>; CallWrapper: TRESTCallWrapper);
begin
  if not fSnippetsByCategory.ContainsKey(CatID) then
    FetchPartialSnippets(CatID, CallWrapper);
  Snippets.AddRange(fSnippetsByCategory[CatID]);
end;

procedure TSWAGReader.HandleException(E: Exception);
resourcestring
  sHTTPError = 'The SWAG web server returned the following error:'
    + EOL2
    + '%s';
  sWebTransmissionError = 'The data received from the SWAG web service was '
    + 'corrupt';
  sWebServiceFailure = 'The following error was detected in the response '
    + 'received from the SWAG web service:'
    + EOL2
    + '%s';
  sWebServiceError = 'The SWAG web service reported the following error:'
    + EOL2
    + '%s';
begin
  if E is EHTTPError then
    raise ESWAGReader.CreateFmt(sHTTPError, [E.Message]);
  if E is EWebConnectionError then
    raise ESWAGReader.Create(E);
  if E is EWebTransmissionError then
    raise ESWAGReader.Create(sWebTransmissionError);
  if E is EWebServiceFailure then
    raise ESWAGReader.CreateFmt(sWebServiceFailure, [E.Message]);
  if E is EWebServiceError then
    raise ESWAGReader.CreateFmt(sWebServiceError, [E.Message]);
  raise E;
end;

end.


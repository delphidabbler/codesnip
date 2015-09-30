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
  ///  <summary>Class that provides cached access to the online SWAG database.
  ///  </summary>
  ///  <remarks>Whenever this class needs to get data from the SWAG database it
  ///  passes a closure back to the caller that it uses to get the required
  ///  data. This enables the calling code to display a suitable UI while
  ///  database accesses take place without having to know how to retrieve the
  ///  required data.</remarks>
  TSWAGReader = class(TObject)
  public
    type
      ///  <summary>Type of closure passed back to calling code whenever the
      ///  SWAG database is to be accessed.</summary>
      ///  <param name="CallProc">TProc [in] Closure that calling code must call
      ///  to perform the required database access.</param>
      TRESTCallWrapper = reference to procedure (CallProc: TProc);
  strict private
    const
      ///  <summary>Maximum size of cache of complete snippets downloaded from
      ///  the SWAG database.</summary>
      MaxSnippetCacheSize = 50;
    var
      ///  <summary>List of all categories in the SWAG database.</summary>
      fCategories: TList<TSWAGCategory>;
      ///  <summary>Map of category IDs onto the snippets that belong to the
      ///  category.</summary>
      ///  <remarks>
      ///  <para>This object is used to cache the snippets for each category to
      ///  avoid having to download them more than once.</para>
      ///  <para>Only partial information that summarises each snippet is
      ///  stored.</para>
      ///  </remarks>
      fSnippetsByCategory: TDictionary<string,TList<TSWAGSnippet>>;
      ///  <summary>Object that interfaces with the SWAG database REST API.
      ///  </summary>
      fSWAGRESTMgr: TSWAGRESTMgr;
      ///  <summary>Default wrapper for calls to the online SWAG database.
      ///  </summary>
      ///  <remarks>This wrapper is supplied in the constructor and is used
      ///  unless a different wrapper is specified by the caller in certain
      ///  method calls.</remarks>
      fDefaultCallWrapper: TRESTCallWrapper;
      ///  <summary>Cache of complete snippets downloaded from the SWAG
      ///  database.</summary>
      fSnippetCache: TSWAGSnippetCache;
    ///  <summary>Handles the given exception by converting HTTP and web service
    ///  related exceptions into for informative ESWAGReader exceptions.
    ///  </summary>
    ///  <exception>Always raises an exception. ESWAGReader is raised if E is an
    ///  HTTP or web service exception. Otherwise E is re-raised.</exception>
    procedure HandleException(E: Exception);
    ///  <summary>Downloads all categories in the SWAG database and stores their
    ///  data in the list Cats.</summary>
    procedure DownloadCategories(const Cats: TList<TSWAGCategory>);
    ///  <summary>Downloads partial (i.e. summary) snippet information for all
    ///  snippets in the category with ID CatID and stores them in the list
    ///  SnipList.</summary>
    procedure DownloadPartialSnippets(const CatID: string;
      const SnipList: TList<TSWAGSnippet>);
    ///  <summary>Downloads and returns all the data for the snippet with the
    ///  given ID.</summary>
    function DownloadCompleteSnippet(const SnipID: Cardinal): TSWAGSnippet;
    ///  <summary>Passes a closure back to the caller that it must call when a
    ///  list of all categories must be downloaded from the SWAG database.
    ///  </summary>
    ///  <param name="CallWrapper">TRESTCallWrapper [in] Callback function
    ///  provided by caller to which the category download closure is passed. If
    ///  nil then the default callback function is used.</param>
    ///  <remarks>The fetched categories are cached in fCategories.</remarks>
    procedure FetchCategories(CallWrapper: TRESTCallWrapper);
    ///  <summary>Passes a closure back to the caller that it must call when a
    ///  the summaries of all snippets in a category is to be downloaded from
    ///  the SWAG database.</summary>
    ///  <param name="CatID">string [in] ID of category for which snippet
    ///  summaries are required.</param>
    ///  <param name="CallWrapper">TRESTCallWrapper [in] Callback function
    ///  provided by caller to which the snippets download closure is passed. If
    ///  nil then the default callback function is used.</param>
    ///  <remarks>The fetched snippet summaries are cached in
    ///  fSnippetsByCategory.</remarks>
    procedure FetchPartialSnippets(const CatID: string;
      CallWrapper: TRESTCallWrapper);
    ///  <summary>Passes a closure back to the caller that it must call when a
    ///  complete snippet must be downloaded from the SWAG database.</summary>
    ///  <param name="SnippetID">Cardinal [in] ID of required snippet.</param>
    ///  <param name="CallWrapper">TRESTCallWrapper [in] Callback function
    ///  provided by caller to which the snippet download closure is passed. If
    ///  nil then the default callback function is used.</param>
    ///  <returns>TSWAGSnippet. The downloaded snippet.</returns>
    function FetchCompleteSnippet(const SnippetID: Cardinal;
      CallWrapper: TRESTCallWrapper): TSWAGSnippet;
    ///  <summary>Passes a closure back to the caller that it must call when a
    ///  list of all complete snippets must be downloaded from the SWAG
    ///  database.</summary>
    ///  <param name="SnipIDs">TList&lt;Cardinal&gt; [in] List of IDs of the
    ///  required snippets.</param>
    ///  <param name="Snippets">TList&lt;TSWAGSnippet&gt; [in] Receives the list
    ///  of downloaded snippets.</param>
    ///  <param name="CallWrapper">TRESTCallWrapper [in] Callback function
    ///  provided by caller to which the snippets download closure is passed. If
    ///  nil then the default callback function is used.</param>
    procedure FetchCompleteSnippets(const SnipIDs: TList<Cardinal>;
      Snippets: TList<TSWAGSnippet>; CallWrapper: TRESTCallWrapper);
  public
    ///  <summary>Creates a new object instance.</summary>
    ///  <param name="DefaultRESTCallWrapper">TRESTCallWrapper [in] Callback to
    ///  be passed a database access closure in all database operations where no
    ///  specific callback is provided.</param>
    constructor Create(const DefaultRESTCallWrapper: TRESTCallWrapper);
    ///  <summary>Destroys object instance.</summary>
    destructor Destroy; override;
    ///  <summary>Gets all the categories in the SWAG database.</summary>
    ///  <param name="Cats">TList&lt;TSWAGCategory&gt; [in] Receives the
    ///  required list of categories.</param>
    ///  <param name="CallWrapper">TRESTCallWrapper [in] Callback that is called
    ///  and passed a closure which can download categories from the database.
    ///  If nil then the default callback passed to the constructor is used.
    ///  </param>
    ///  <remarks>The first time this method is called CallWrapper is called to
    ///  download the categories from the SWAG database. On subsequent calls
    ///  the categories are read from a cache and CallWrapper is not called.
    ///  </remarks>
    procedure GetCategories(const Cats: TList<TSWAGCategory>;
      CallWrapper: TRESTCallWrapper = nil);
    ///  <summary>Gets summaries of all the snippets containing in a given
    ///  SWAG database category.</summary>
    ///  <param name="CatID">string [in] ID of the required category.</param>
    ///  <param name="Snippets">TList&lt;TSWAGSnippet&gt; [in] Receives the
    ///  required list of snippet summaries.</param>
    ///  <param name="CallWrapper">TRESTCallWrapper [in] Callback that is called
    ///  and passed a closure which can download the required snippet summaries
    ///  from the database. If nil then the default callback passed to the
    ///  constructor is used.</param>
    ///  <remarks>The first time this method is called for any category
    ///  CallWrapper is be called to download the required snippets. On
    ///  subsequent calls for that category the snippets summaries are read from
    ///  a cache and CallWrapper is not called.</remarks>
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
    [doOwnsValues], TStringComparator.Create
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


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
  SWAG.UXMLProcessor,
  UExceptions;


type
  ///  <summary>Class that provides cached access to a local copy of the SWAG
  ///  database.</summary>
  ///  <remarks>Whenever this class needs to get data from the SWAG database it
  ///  passes a closure back to the caller that it uses to get the required
  ///  data. This enables the calling code to display a suitable UI while
  ///  database accesses take place without having to know how to retrieve the
  ///  required data.</remarks>
  TSWAGReader = class(TObject)
  public
    type
      ///  <summary>Type of closure passed back to calling code whenever SWAG
      ///  data is begin read.</summary>
      ///  <param name="CallProc">TProc [in] Closure that calling code must call
      ///  to perform the required database access.</param>
      TSWAGCallbackWrapper = reference to procedure (CallProc: TProc);
  strict private
    const
      ///  <summary>Maximum size of cache of complete snippets read from the
      ///  SWAG database.</summary>
      MaxSnippetCacheSize = 50;
    var
      ///  <summary>List of all categories in the SWAG database.</summary>
      fCategories: TList<TSWAGCategory>;
      ///  <summary>Map of category IDs onto the snippets that belong to the
      ///  category.</summary>
      ///  <remarks>
      ///  <para>This object is used to cache the snippets for each category, to
      ///  avoid having to read from the database more than once.</para>
      ///  <para>Only partial information that summarises each snippet is
      ///  stored.</para>
      ///  </remarks>
      fSnippetsByCategory: TDictionary<string,TList<TSWAGSnippet>>;
      ///  <summary>Object used to interogate SWAG XML file.</summary>
      fXMLProcessor: TSWAGXMLProcessor;
      ///  <summary>Default wrapper for calls to the local SWAG database.
      ///  </summary>
      ///  <remarks>This wrapper is supplied in the constructor and is used
      ///  unless a different wrapper is specified by the caller in certain
      ///  method calls.</remarks>
      fDefaultCallWrapper: TSWAGCallbackWrapper;
      ///  <summary>Cache of complete snippets read from the SWAG database.
      ///  </summary>
      fSnippetCache: TSWAGSnippetCache;
    ///  <summary>Handles the given exception by converting expected exceptions
    ///  into ESWAGReader exceptions.</summary>
    ///  <exception>Always raises an exception. ESWAGReader is raised if E is
    ///  expected. Otherwise E is re-raised.</exception>
    procedure HandleException(E: Exception);
    ///  <summary>Gets a list of all categories from the SWAG database.
    ///  </summary>
    ///  <param name="CallWrapper">TSWAGCallbackWrapper [in] Callback that is
    ///  called and passed a closure which can retrive the required category
    ///  information from the database. If nil then the default callback passed
    ///  to the constructor is used.</param>
    ///  <remarks>The fetched categories are cached in fCategories.</remarks>
    procedure FetchCategories(CallWrapper: TSWAGCallbackWrapper);
    ///  <summary>Fetches summaries of all the snippets contained in a given
    ///  SWAG category from the SWAG database.</summary>
    ///  <param name="CatID">string [in] ID of category for which snippet
    ///  summaries are required.</param>
    ///  <param name="CallWrapper">TSWAGCallbackWrapper [in] Callback that is
    ///  called and passed a closure which can retrive the required category
    ///  information from the database. If nil then the default callback passed
    ///  to the constructor is used.</param>
    ///  <remarks>The fetched snippet summaries are cached in
    ///  fSnippetsByCategory.</remarks>
    procedure FetchPartialSnippets(const CatID: string;
      CallWrapper: TSWAGCallbackWrapper);
    ///  <summary>Fetches full details a snippet from the SWAG database.
    ///  </summary>
    ///  <param name="SnippetID">Cardinal [in] ID of required snippet.</param>
    ///  <param name="CallWrapper">TSWAGCallbackWrapper [in] Callback that is
    ///  called and passed a closure which can retrive the required snippet
    ///  from the database. If nil then the default callback passed to the
    ///  constructor is used.</param>
    ///  <returns>TSWAGSnippet. The retrieved snippet.</returns>
    function FetchCompleteSnippet(const SnippetID: Cardinal;
      CallWrapper: TSWAGCallbackWrapper): TSWAGSnippet;
    ///  <summary>Fetches multiple complete snippets from the SWAG database.
    ///  </summary>
    ///  <param name="SnipIDs">TList&lt;Cardinal&gt; [in] List of IDs of the
    ///  required snippets.</param>
    ///  <param name="Snippets">TList&lt;TSWAGSnippet&gt; [in] Receives the list
    ///  of retrieved snippets.</param>
    ///  <param name="CallWrapper">TSWAGCallbackWrapper [in] Callback that is
    ///  called and passed a closure which can retrive the required snippets
    ///  from the database. If nil then the default callback passed to the
    ///  constructor is used.</param>
    ///  <param name="BreatherProc">TProc [in] Callback procedure called between
    ///  reading each snippet.</param>
    procedure FetchCompleteSnippets(const SnipIDs: TList<Cardinal>;
      Snippets: TList<TSWAGSnippet>; BreatherProc: TProc;
      CallWrapper: TSWAGCallbackWrapper);
  public
    ///  <summary>Creates a new object instance.</summary>
    ///  <param name="SWAGDBDir">string [in] Directory where SWAG database is
    ///  located on the local system.</param>
    ///  <param name="DefaultSWAGCallbackWrapper">TSWAGCallbackWrapper [in]
    ///  Callback to be passed a database access closure in all database
    ///  operations where no specific callback is provided.</param>
    constructor Create(const SWAGDBDir: string;
      const DefaultSWAGCallbackWrapper: TSWAGCallbackWrapper);
    ///  <summary>Destroys object instance.</summary>
    destructor Destroy; override;
    ///  <summary>Gets a list of all the categories in the SWAG database.
    ///  </summary>
    ///  <param name="Cats">TList&lt;TSWAGCategory&gt; [in] Receives the
    ///  required list of categories.</param>
    ///  <param name="CallWrapper">TSWAGCallbackWrapper [in] Callback that is
    ///  called and passed a closure which can retrive the required category
    ///  information from the database. If nil then the default callback passed
    ///  to the constructor is used.</param>
    ///  <remarks>The first time this method is called CallWrapper is called to
    ///  retrieve the categories from the SWAG database. On subsequent calls
    ///  the categories are read from a cache and CallWrapper is not called.
    ///  </remarks>
    procedure GetCategories(const Cats: TList<TSWAGCategory>;
      CallWrapper: TSWAGCallbackWrapper = nil);
    ///  <summary>Gets summaries of all the snippets contained in a given SWAG
    ///  category.</summary>
    ///  <param name="CatID">string [in] ID of the required category.</param>
    ///  <param name="Snippets">TList&lt;TSWAGSnippet&gt; [in] Receives the
    ///  required list of snippet summaries.</param>
    ///  <param name="CallWrapper">TSWAGCallbackWrapper [in] Callback that is
    ///  called and passed a closure which can retrive the required snippet
    ///  summaries from the database. If nil then the default callback passed to
    ///  the constructor is used.</param>
    ///  <remarks>The first time this method is called for any category
    ///  CallWrapper is called to retrieve the required snippets. On subsequent
    ///  calls for that category the snippets summaries are read from a cache
    ///  and CallWrapper is not called.</remarks>
    procedure GetPartialSnippets(const CatID: string;
      const Snippets: TList<TSWAGSnippet>;
      CallWrapper: TSWAGCallbackWrapper = nil);
    ///  <summary>Gets full details a snippet from the SWAG database.</summary>
    ///  <param name="ID">Cardinal [in] Unique ID of the required snippet.
    ///  </param>
    ///  <param name="CallWrapper">TSWAGCallbackWrapper [in] Callback that is
    ///  called and passed a closure which can retrive the required snippet
    ///  from the database. If nil then the default callback passed to the
    ///  constructor is used.</param>
    ///  <returns>TSWAGSnippet. The required snippet.</returns>
    ///  <remarks>The first time this method is called for any snippet
    ///  CallWrapper is called to retrieve the snippet. On subsequent calls for
    ///  calls for that snippet the snippet is read from a cache and CallWrapper
    ///  is not called.</remarks>
    function GetCompleteSnippet(const ID: Cardinal;
      CallWrapper: TSWAGCallbackWrapper = nil): TSWAGSnippet;
    ///  <summary>Gets full details of multiple snippets from the SWAG database.
    ///  </summary>
    ///  <param name="SnipIDs">TList&lt;Cardinal&gt; [in] List of IDs of the
    ///  required snippets.</param>
    ///  <param name="Snippets">TList&lt;TSWAGSnippet&gt; [in] Receives the
    ///  required list of snippets.</param>
    ///  <param name="CallWrapper">TSWAGCallbackWrapper [in] Callback that is
    ///  called and passed a closure which can retrive the required snippets
    ///  from the database. If nil then the default callback passed to the
    ///  constructor is used.</param>
    ///  <param name="BreatherProc">TProc [in] Callback procedure called between
    ///  reading each snippet.</param>
    ///  <remarks>The first time this method is called for any category
    ///  CallWrapper is called to retrieve the required snippets. On subsequent
    ///  calls for that category the snippets are read from a cache and
    ///  CallWrapper is not called.</remarks>
    procedure GetCompleteSnippets(SnipIDs: TList<Cardinal>;
      Snippets: TList<TSWAGSnippet>; BreatherProc: TProc = nil;
      CallWrapper: TSWAGCallbackWrapper = nil);
  end;


type
  ///  <summary>Class of exception raised by TSWAGReader.</summary>
  ESWAGReader = class(ECodeSnip);


implementation


uses
  // VCL
  XMLDom,
  XMLIntf,
  // Project
  UComparers,
  UConsts,
  Web.UExceptions;


{ TSWAGReader }

constructor TSWAGReader.Create(const SWAGDBDir: string;
  const DefaultSWAGCallbackWrapper: TSWAGCallbackWrapper);
begin
  inherited Create;
  fDefaultCallWrapper := DefaultSWAGCallbackWrapper;
  fCategories := TList<TSWAGCategory>.Create;
  fSnippetsByCategory := TObjectDictionary<string,TList<TSWAGSnippet>>.Create(
    [doOwnsValues],
    TStringEqualityComparer.Create
  );
  fSnippetCache := TSWAGSnippetCache.Create(MaxSnippetCacheSize);
  fXMLProcessor := TSWAGXMLProcessor.Create;
  fXMLProcessor.Initialise(SWAGDBDir);
end;

destructor TSWAGReader.Destroy;
begin
  fXMLProcessor.Free;
  fSnippetCache.Free;
  fSnippetsByCategory.Free;
  fCategories.Free;
  inherited;
end;

procedure TSWAGReader.FetchCategories(CallWrapper: TSWAGCallbackWrapper);
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
        fXMLProcessor.GetCategories(CatList);
        fCategories.AddRange(CatList);
      finally
        CatList.Free;
      end
    end
  );
end;

function TSWAGReader.FetchCompleteSnippet(const SnippetID: Cardinal;
  CallWrapper: TSWAGCallbackWrapper): TSWAGSnippet;
var
  Snippet: TSWAGSnippet;
begin
  if not Assigned(CallWrapper) then
    CallWrapper := fDefaultCallWrapper;
  CallWrapper(
    procedure
    begin
      Snippet := fXMLProcessor.GetSnippet(SnippetID);
    end
  );
  Result := Snippet;
end;

procedure TSWAGReader.FetchCompleteSnippets(const SnipIDs: TList<Cardinal>;
  Snippets: TList<TSWAGSnippet>;
  BreatherProc: TProc;
  CallWrapper: TSWAGCallbackWrapper);
begin
  if not Assigned(CallWrapper) then
    CallWrapper := fDefaultCallWrapper;
  CallWrapper(
    procedure
    var
      SnipID: Cardinal;
      Snippet: TSWAGSnippet;
    begin
      BreatherProc;
      for SnipID in SnipIDs do
      begin
        Snippet := fXMLProcessor.GetSnippet(SnipID);
        Snippets.Add(Snippet);
        BreatherProc;
      end;
    end
  );
end;

procedure TSWAGReader.FetchPartialSnippets(const CatID: string;
  CallWrapper: TSWAGCallbackWrapper);
begin
  if not Assigned(CallWrapper) then
    CallWrapper := fDefaultCallWrapper;
  CallWrapper(
    procedure
    var
      SnipList: TList<TSWAGSnippet>;
    begin
      SnipList := TList<TSWAGSnippet>.Create;
      fXMLProcessor.GetPartialSnippets(CatID, SnipList);
      fSnippetsByCategory.Add(CatID, SnipList);
    end
  );
end;

procedure TSWAGReader.GetCategories(const Cats: TList<TSWAGCategory>;
  CallWrapper: TSWAGCallbackWrapper);
begin
  if fCategories.Count = 0 then
    try
      FetchCategories(CallWrapper);
    except
      on E: Exception do
        HandleException(E);
    end;
  Cats.AddRange(fCategories);
end;

function TSWAGReader.GetCompleteSnippet(const ID: Cardinal;
  CallWrapper: TSWAGCallbackWrapper): TSWAGSnippet;
begin
  if not fSnippetCache.Retrieve(ID, Result) then
  begin
    Result := FetchCompleteSnippet(ID, CallWrapper);
    fSnippetCache.Add(Result);
  end;
end;

procedure TSWAGReader.GetCompleteSnippets(SnipIDs: TList<Cardinal>;
  Snippets: TList<TSWAGSnippet>; BreatherProc: TProc;
  CallWrapper: TSWAGCallbackWrapper);
var
  RemoteSnippetIDs: TList<Cardinal>;
  FetchedSnippets: TList<TSWAGSnippet>;
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
      FetchedSnippets := TList<TSWAGSnippet>.Create;
      try
        if not Assigned(BreatherProc) then
          BreatherProc := procedure begin end;
        FetchCompleteSnippets(
          RemoteSnippetIDs, FetchedSnippets, BreatherProc, CallWrapper
        );
        for Snippet in FetchedSnippets do
        begin
          fSnippetCache.Add(Snippet);
          Snippets.Add(Snippet);
        end;
      finally
        FetchedSnippets.Free;
      end;
    end;
  finally
    RemoteSnippetIDs.Free;
  end;
end;

procedure TSWAGReader.GetPartialSnippets(const CatID: string;
  const Snippets: TList<TSWAGSnippet>; CallWrapper: TSWAGCallbackWrapper);
begin
  if not fSnippetsByCategory.ContainsKey(CatID) then
    FetchPartialSnippets(CatID, CallWrapper);
  Snippets.AddRange(fSnippetsByCategory[CatID]);
end;

procedure TSWAGReader.HandleException(E: Exception);
begin
  if E is EXMLDocError then
    raise ESWAGReader.Create(E);
  if E is EDOMParseError then
    raise ESWAGReader.Create(E);
  raise E;
end;

end.


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
 * Provides a class that implements a cache of SWAG snippets.
}


unit SWAG.USnippetCache;


interface


uses
  // Delphi
  Generics.Collections,
  // Project
  SWAG.UCommon;


type
  ///  <summary>Class that implements a cache of SWAG snippets.</summary>
  ///  <remarks>When the cache is full a snippet must be removed from the cache
  ///  for each one added. The cache is implemented so that the snippets
  ///  referenced the most are the least likely to be removed from the cache.
  ///  </remarks>
  TSWAGSnippetCache = class(TObject)
  strict private
    const
      ///  <summary>Minimum permitted size of cache.</summary>
      MinCacheSize = 10;
      ///  <summary>Maximum permitted size of cache.</summary>
      MaxCacheSize = 200;
    var
      ///  <summary>Stores the cached snippets.</summary>
      fCache: TList<TSWAGSnippet>;
      ///  <summary>Records size of cache.</summary>
      fCacheSize: Integer;
    ///  <summary>Returns index of SWAG snippet with given ID in cache, or -1 if
    ///  snippet not in cache.</summary>
    function IndexOfID(const SnippetID: Cardinal): Integer;
  public
    ///  <summary>Creates a new cache instance with given size.</summary>
    ///  <remarks>CacheSize may be adjusted to fall within valid range of cache
    ///  sizes.</remarks>
    constructor Create(CacheSize: Integer);
    ///  <summary>Destroys current object instance.</summary>
    destructor Destroy; override;
    ///  <summary>Retrieves the SWAG snippet with the given ID from the cache.
    ///  If the snippet is present it is stored in Snippet and True is returned.
    ///  If the snippet is not present False is returned and Snippet is
    ///  undefined.</summary>
    function Retrieve(const SnippetID: Cardinal; out Snippet: TSWAGSnippet):
      Boolean;
    ///  <summary>Adds the given SWAG snippet to the cache.</summary>
    ///  <remarks>If the cache is full a snippet will be removed from the cache
    ///  to make way for the newly added snippet.</remarks>
    procedure Add(const Snippet: TSWAGSnippet);
  end;


implementation


uses
  // Project
  UStructs;


{ TSWAGSnippetCache }

procedure TSWAGSnippetCache.Add(const Snippet: TSWAGSnippet);
begin
  if fCache.Count = fCacheSize then
    fCache.Delete(0);
  fCache.Add(Snippet);
end;

constructor TSWAGSnippetCache.Create(CacheSize: Integer);
var
  CacheSizeRange: TRange;
begin
  inherited Create;
  CacheSizeRange := TRange.Create(MinCacheSize, MaxCacheSize);
  fCacheSize := CacheSizeRange.Constrain(CacheSize);
  fCache := TList<TSWAGSnippet>.Create;
end;

destructor TSWAGSnippetCache.Destroy;
begin
  fCache.Free;
  inherited;
end;

function TSWAGSnippetCache.IndexOfID(const SnippetID: Cardinal): Integer;
var
  Idx: Integer;
begin
  for Idx := 0 to Pred(fCache.Count) do
    if fCache[Idx].ID = SnippetID then
      Exit(Idx);
  Result := -1;
end;

function TSWAGSnippetCache.Retrieve(const SnippetID: Cardinal;
  out Snippet: TSWAGSnippet): Boolean;
var
  Idx: Integer;
begin
  Idx := IndexOfID(SnippetID);
  if Idx = -1 then
    Exit(False);
  fCache.Move(Idx, Pred(fCache.Count));
  Snippet := fCache.Last;
  Result := True;
end;

end.


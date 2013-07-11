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
  TSWAGSnippetCache = class(TObject)
  strict private
    const
      MinCacheSize = 10;
      MaxCachSize = 200;
    var
      fCache: TList<TSWAGSnippet>;
      fCacheSize: Integer;
    function IndexOfID(const SnippetID: Cardinal): Integer;
  public
    constructor Create(CacheSize: Integer);
    destructor Destroy; override;
    function Retrieve(const SnippetID: Cardinal; out Snippet: TSWAGSnippet):
      Boolean;
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
  CacheSizeRange := TRange.Create(MinCacheSize, MaxCachSize);
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


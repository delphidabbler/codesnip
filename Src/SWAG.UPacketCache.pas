{
 * This Source Code Form is subject to the terms of the Mozilla Public License,
 * v. 2.0. If a copy of the MPL was not distributed with this file, You can
 * obtain one at https://mozilla.org/MPL/2.0/
 *
 * Copyright (C) 2013-2021, Peter Johnson (gravatar.com/delphidabbler).
 *
 * Provides a class that implements a cache of SWAG packets.
}


unit SWAG.UPacketCache;


interface


uses
  // Delphi
  Generics.Collections,
  // Project
  SWAG.UCommon;


type
  ///  <summary>Class that implements a cache of SWAG packets.</summary>
  ///  <remarks>When the cache is full a packet must be removed from the cache
  ///  for each one added. The cache is implemented so that the packets
  ///  referenced the most are the least likely to be removed from the cache.
  ///  </remarks>
  TSWAGPacketCache = class(TObject)
  strict private
    const
      ///  <summary>Minimum permitted size of cache.</summary>
      MinCacheSize = 10;
      ///  <summary>Maximum permitted size of cache.</summary>
      MaxCacheSize = 200;
    var
      ///  <summary>Stores the cached packets.</summary>
      fCache: TList<TSWAGPacket>;
      ///  <summary>Records size of cache.</summary>
      fCacheSize: Integer;
    ///  <summary>Returns index of SWAG packet with given ID in cache, or -1 if
    ///  packet not in cache.</summary>
    function IndexOfID(const PacketID: Cardinal): Integer;
  public
    ///  <summary>Creates a new cache instance with given size.</summary>
    ///  <remarks>CacheSize may be adjusted to fall within valid range of cache
    ///  sizes.</remarks>
    constructor Create(CacheSize: Integer);
    ///  <summary>Destroys current object instance.</summary>
    destructor Destroy; override;
    ///  <summary>Retrieves the SWAG packet with the given ID from the cache.
    ///  If the packet is present it is stored in Packet and True is returned.
    ///  If the packet is not present False is returned and Packet is
    ///  undefined.</summary>
    function Retrieve(const PacketID: Cardinal; out Packet: TSWAGPacket):
      Boolean;
    ///  <summary>Adds the given SWAG packet to the cache.</summary>
    ///  <remarks>If the cache is full a packet will be removed from the cache
    ///  to make way for the newly added packet.</remarks>
    procedure Add(const Packet: TSWAGPacket);
  end;


implementation


uses
  // Project
  UStructs;


{ TSWAGPacketCache }

procedure TSWAGPacketCache.Add(const Packet: TSWAGPacket);
begin
  if fCache.Count = fCacheSize then
    fCache.Delete(0);
  fCache.Add(Packet);
end;

constructor TSWAGPacketCache.Create(CacheSize: Integer);
var
  CacheSizeRange: TRange;
begin
  inherited Create;
  CacheSizeRange := TRange.Create(MinCacheSize, MaxCacheSize);
  fCacheSize := CacheSizeRange.Constrain(CacheSize);
  fCache := TList<TSWAGPacket>.Create;
end;

destructor TSWAGPacketCache.Destroy;
begin
  fCache.Free;
  inherited;
end;

function TSWAGPacketCache.IndexOfID(const PacketID: Cardinal): Integer;
var
  Idx: Integer;
begin
  for Idx := 0 to Pred(fCache.Count) do
    if fCache[Idx].ID = PacketID then
      Exit(Idx);
  Result := -1;
end;

function TSWAGPacketCache.Retrieve(const PacketID: Cardinal;
  out Packet: TSWAGPacket): Boolean;
var
  Idx: Integer;
begin
  Idx := IndexOfID(PacketID);
  if Idx = -1 then
    Exit(False);
  fCache.Move(Idx, Pred(fCache.Count));
  Packet := fCache.Last;
  Result := True;
end;

end.


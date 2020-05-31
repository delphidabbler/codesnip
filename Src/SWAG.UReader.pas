{
 * This Source Code Form is subject to the terms of the Mozilla Public License,
 * v. 2.0. If a copy of the MPL was not distributed with this file, You can
 * obtain one at http://mozilla.org/MPL/2.0/
 *
 * Copyright (C) 2013-2020, Peter Johnson (gravatar.com/delphidabbler).
 *
 * Implements a class that provides cached access to the SWAG database.
}


unit SWAG.UReader;


interface


uses
  // Delphi
  SysUtils,
  Generics.Defaults,
  Generics.Collections,
  // Project
  SWAG.UCommon,
  SWAG.UPacketCache,
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
      ///  <summary>Maximum size of cache of complete packets read from the
      ///  SWAG database.</summary>
      MaxPacketCacheSize = 50;
    var
      ///  <summary>List of all categories in the SWAG database.</summary>
      fCategories: TList<TSWAGCategory>;
      ///  <summary>Map of category IDs onto the packets that belong to the
      ///  category.</summary>
      ///  <remarks>
      ///  <para>This object is used to cache the packets for each category, to
      ///  avoid having to read from the database more than once.</para>
      ///  <para>Only partial information that summarises each packet is
      ///  stored.</para>
      ///  </remarks>
      fPacketsByCategory: TDictionary<Cardinal,TList<TSWAGPacket>>;
      ///  <summary>Object used to interogate SWAG XML file.</summary>
      fXMLProcessor: TSWAGXMLProcessor;
      ///  <summary>Default wrapper for calls to the local SWAG database.
      ///  </summary>
      ///  <remarks>This wrapper is supplied in the constructor and is used
      ///  unless a different wrapper is specified by the caller in certain
      ///  method calls.</remarks>
      fDefaultCallWrapper: TSWAGCallbackWrapper;
      ///  <summary>Cache of complete packets read from the SWAG database.
      ///  </summary>
      fPacketCache: TSWAGPacketCache;
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
    ///  <summary>Fetches summaries of all the packets contained in a given
    ///  SWAG category from the SWAG database.</summary>
    ///  <param name="CatID">Cardinal [in] ID of category for which packet
    ///  summaries are required.</param>
    ///  <param name="CallWrapper">TSWAGCallbackWrapper [in] Callback that is
    ///  called and passed a closure which can retrive the required category
    ///  information from the database. If nil then the default callback passed
    ///  to the constructor is used.</param>
    ///  <remarks>The fetched packet summaries are cached in
    ///  fPacketsByCategory.</remarks>
    procedure FetchPartialPackets(const CatID: Cardinal;
      CallWrapper: TSWAGCallbackWrapper);
    ///  <summary>Fetches full details a packet from the SWAG database.
    ///  </summary>
    ///  <param name="PacketID">Cardinal [in] ID of required packet.</param>
    ///  <param name="CallWrapper">TSWAGCallbackWrapper [in] Callback that is
    ///  called and passed a closure which can retrive the required packet
    ///  from the database. If nil then the default callback passed to the
    ///  constructor is used.</param>
    ///  <returns>TSWAGPacket. The retrieved packet.</returns>
    function FetchCompletePacket(const PacketID: Cardinal;
      CallWrapper: TSWAGCallbackWrapper): TSWAGPacket;
    ///  <summary>Fetches multiple complete packets from the SWAG database.
    ///  </summary>
    ///  <param name="PacketIDs">TList&lt;Cardinal&gt; [in] List of IDs of the
    ///  required packets.</param>
    ///  <param name="Packets">TList&lt;TSWAGPacket&gt; [in] Receives the list
    ///  of retrieved packets.</param>
    ///  <param name="CallWrapper">TSWAGCallbackWrapper [in] Callback that is
    ///  called and passed a closure which can retrive the required packets
    ///  from the database. If nil then the default callback passed to the
    ///  constructor is used.</param>
    ///  <param name="BreatherProc">TProc [in] Callback procedure called between
    ///  reading each packet.</param>
    procedure FetchCompletePackets(const PacketIDs: TList<Cardinal>;
      Packets: TList<TSWAGPacket>; BreatherProc: TProc;
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
    ///  <summary>Gets summaries of all the packets contained in a given SWAG
    ///  category.</summary>
    ///  <param name="CatID">Cardinal [in] ID of the required category.</param>
    ///  <param name="Packets">TList&lt;TSWAGPacket&gt; [in] Receives the
    ///  required list of packet summaries.</param>
    ///  <param name="CallWrapper">TSWAGCallbackWrapper [in] Callback that is
    ///  called and passed a closure which can retrive the required packet
    ///  summaries from the database. If nil then the default callback passed to
    ///  the constructor is used.</param>
    ///  <remarks>The first time this method is called for any category
    ///  CallWrapper is called to retrieve the required packets. On subsequent
    ///  calls for that category the packets summaries are read from a cache
    ///  and CallWrapper is not called.</remarks>
    procedure GetPartialPackets(const CatID: Cardinal;
      const Packets: TList<TSWAGPacket>;
      CallWrapper: TSWAGCallbackWrapper = nil);
    ///  <summary>Gets full details a packet from the SWAG database.</summary>
    ///  <param name="ID">Cardinal [in] Unique ID of the required packet.
    ///  </param>
    ///  <param name="CallWrapper">TSWAGCallbackWrapper [in] Callback that is
    ///  called and passed a closure which can retrive the required packet
    ///  from the database. If nil then the default callback passed to the
    ///  constructor is used.</param>
    ///  <returns>TSWAGPacket. The required packet.</returns>
    ///  <remarks>The first time this method is called for any packet
    ///  CallWrapper is called to retrieve the packet. On subsequent calls for
    ///  that packet the packet is read from a cache and CallWrapper is not
    ///  called.</remarks>
    function GetCompletePacket(const ID: Cardinal;
      CallWrapper: TSWAGCallbackWrapper = nil): TSWAGPacket;
    ///  <summary>Gets full details of multiple packets from the SWAG database.
    ///  </summary>
    ///  <param name="PacketIDs">TList&lt;Cardinal&gt; [in] List of IDs of the
    ///  required packets.</param>
    ///  <param name="Packets">TList&lt;TSWAGPacket&gt; [in] Receives the
    ///  required list of packets.</param>
    ///  <param name="CallWrapper">TSWAGCallbackWrapper [in] Callback that is
    ///  called and passed a closure which can retrive the required packets
    ///  from the database. If nil then the default callback passed to the
    ///  constructor is used.</param>
    ///  <param name="BreatherProc">TProc [in] Callback procedure called between
    ///  reading each packet.</param>
    ///  <remarks>The first time this method is called for any category
    ///  CallWrapper is called to retrieve the required packets. On subsequent
    ///  calls for that category the packets are read from a cache and
    ///  CallWrapper is not called.</remarks>
    procedure GetCompletePackets(PacketIDs: TList<Cardinal>;
      Packets: TList<TSWAGPacket>; BreatherProc: TProc = nil;
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
  UConsts;


{ TSWAGReader }

constructor TSWAGReader.Create(const SWAGDBDir: string;
  const DefaultSWAGCallbackWrapper: TSWAGCallbackWrapper);
begin
  inherited Create;
  fDefaultCallWrapper := DefaultSWAGCallbackWrapper;
  fCategories := TList<TSWAGCategory>.Create;
  fPacketsByCategory := TObjectDictionary<Cardinal,TList<TSWAGPacket>>.Create(
    [doOwnsValues],
    TDelegatedEqualityComparer<Cardinal>.Create(
      function (const Left, Right: Cardinal): Boolean
      begin
        Result := Left = Right;
      end,
      function (const Value: Cardinal): Integer
      begin
        Result := Integer(Value);
      end
    )
  );
  fPacketCache := TSWAGPacketCache.Create(MaxPacketCacheSize);
  fXMLProcessor := TSWAGXMLProcessor.Create;
  fXMLProcessor.Initialise(SWAGDBDir);
end;

destructor TSWAGReader.Destroy;
begin
  fXMLProcessor.Free;
  fPacketCache.Free;
  fPacketsByCategory.Free;
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

function TSWAGReader.FetchCompletePacket(const PacketID: Cardinal;
  CallWrapper: TSWAGCallbackWrapper): TSWAGPacket;
var
  Packet: TSWAGPacket;
begin
  if not Assigned(CallWrapper) then
    CallWrapper := fDefaultCallWrapper;
  CallWrapper(
    procedure
    begin
      Packet := fXMLProcessor.GetPacket(PacketID);
    end
  );
  Result := Packet;
end;

procedure TSWAGReader.FetchCompletePackets(const PacketIDs: TList<Cardinal>;
  Packets: TList<TSWAGPacket>;
  BreatherProc: TProc;
  CallWrapper: TSWAGCallbackWrapper);
begin
  if not Assigned(CallWrapper) then
    CallWrapper := fDefaultCallWrapper;
  CallWrapper(
    procedure
    var
      PacketID: Cardinal;
      Packet: TSWAGPacket;
    begin
      BreatherProc;
      for PacketID in PacketIDs do
      begin
        Packet := fXMLProcessor.GetPacket(PacketID);
        Packets.Add(Packet);
        BreatherProc;
      end;
    end
  );
end;

procedure TSWAGReader.FetchPartialPackets(const CatID: Cardinal;
  CallWrapper: TSWAGCallbackWrapper);
begin
  if not Assigned(CallWrapper) then
    CallWrapper := fDefaultCallWrapper;
  CallWrapper(
    procedure
    var
      PacketList: TList<TSWAGPacket>;
    begin
      PacketList := TList<TSWAGPacket>.Create;
      fXMLProcessor.GetPartialPackets(CatID, PacketList);
      fPacketsByCategory.Add(CatID, PacketList);
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

function TSWAGReader.GetCompletePacket(const ID: Cardinal;
  CallWrapper: TSWAGCallbackWrapper): TSWAGPacket;
begin
  if not fPacketCache.Retrieve(ID, Result) then
  begin
    Result := FetchCompletePacket(ID, CallWrapper);
    fPacketCache.Add(Result);
  end;
end;

procedure TSWAGReader.GetCompletePackets(PacketIDs: TList<Cardinal>;
  Packets: TList<TSWAGPacket>; BreatherProc: TProc;
  CallWrapper: TSWAGCallbackWrapper);
var
  RemotePacketIDs: TList<Cardinal>;
  FetchedPackets: TList<TSWAGPacket>;
  Packet: TSWAGPacket;
  PacketID: Cardinal;
begin
  RemotePacketIDs := TList<Cardinal>.Create;
  try
    for PacketID in PacketIDs do
    begin
      if fPacketCache.Retrieve(PacketID, Packet) then
        Packets.Add(Packet)
      else
        RemotePacketIDs.Add(PacketID);
    end;
    if RemotePacketIDs.Count > 0 then
    begin
      FetchedPackets := TList<TSWAGPacket>.Create;
      try
        if not Assigned(BreatherProc) then
          BreatherProc := procedure begin end;
        FetchCompletePackets(
          RemotePacketIDs, FetchedPackets, BreatherProc, CallWrapper
        );
        for Packet in FetchedPackets do
        begin
          fPacketCache.Add(Packet);
          Packets.Add(Packet);
        end;
      finally
        FetchedPackets.Free;
      end;
    end;
  finally
    RemotePacketIDs.Free;
  end;
end;

procedure TSWAGReader.GetPartialPackets(const CatID: Cardinal;
  const Packets: TList<TSWAGPacket>; CallWrapper: TSWAGCallbackWrapper);
begin
  if not fPacketsByCategory.ContainsKey(CatID) then
    FetchPartialPackets(CatID, CallWrapper);
  Packets.AddRange(fPacketsByCategory[CatID]);
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


{
 * This Source Code Form is subject to the terms of the Mozilla Public License,
 * v. 2.0. If a copy of the MPL was not distributed with this file, You can
 * obtain one at https://mozilla.org/MPL/2.0/
 *
 * Copyright (C) 2024, Peter Johnson (gravatar.com/delphidabbler).
 *
 * Implements support for multiple snippet collections.
}


unit DB.UCollections;

{$ScopedEnums ON}

interface

uses
  SysUtils,
  Generics.Collections,
  Generics.Defaults,

  DB.DataFormats,
  UEncodings,
  UExceptions,
  USettings,
  USingleton;

type

  TCollectionID = record
  strict private
    var
      fID: TBytes;
  public
    type
      TComparer = class(TInterfacedObject,
        IComparer<TCollectionID>, IEqualityComparer<TCollectionID>
      )
      public
        function Compare(const Left, Right: TCollectionID): Integer;
        function Equals(const Left, Right: TCollectionID): Boolean;
          reintroduce;
        function GetHashCode(const Value: TCollectionID): Integer;
          reintroduce;
      end;
    constructor Create(const ABytes: TBytes); overload;
    constructor Create(const AStr: string); overload;
    constructor Create(const AGUID: TGUID); overload;
    class function CreateFromHexString(const AHexStr: string): TCollectionID;
      static;
    class function CreateNull: TCollectionID; static;
    class function Default: TCollectionID; static;
    function Clone: TCollectionID;
    function ToArray: TBytes;
    function ToHexString: string;
    function IsNull: Boolean;
    function Hash: Integer;
    class function Compare(Left, Right: TCollectionID): Integer; static;
    class operator Equal(Left, Right: TCollectionID): Boolean;
    class operator NotEqual(Left, Right: TCollectionID): Boolean;
  end;

  ECollectionID = class(ECodeSnip);

  TCollection = record
  strict private
    var
      fUID: TCollectionID;
      fName: string;
      fStorage: TDataStorageDetails;
  public
    type
      TComparer = class(TInterfacedObject,
        IComparer<TCollection>, IEqualityComparer<TCollection>
      )
      public
        function Compare(const Left, Right: TCollection): Integer;
        function Equals(const Left, Right: TCollection): Boolean;
          reintroduce;
        function GetHashCode(const Value: TCollection): Integer;
          reintroduce;
      end;
    ///  <summary>Creates a collection record.</summary>
    ///  <param name="AUID"><c>TCollectionID</c> [in] Unique ID of the
    ///  collection. Must not be null.</param>
    ///  <param name="AName"><c>string</c> [in] Name of collection. Should be
    ///  unique. Must not be empty or only whitespace.</param>
    constructor Create(const AUID: TCollectionID; const AName: string;
      const AStorage: TDataStorageDetails);
    ///  <summary>Collection identifier. Must be unique.</summary>
    property UID: TCollectionID
      read fUID;
    ///  <summary>Collection name. Must be unique.</summary>
    property Name: string read
      fName;
    ///  <summary>Collection storage information.</summary>
    property Storage: TDataStorageDetails
      read fStorage;
    ///  <summary>Checks if this record's fields are valid.</summary>
    function IsValid: Boolean;
    ///  <summary>Checks if this record is the default collection.</summary>
    function IsDefault: Boolean;
  end;

  TCollections = class sealed(TSingleton)
  strict private
    var
      fItems: TList<TCollection>;
    function GetItem(const Idx: Integer): TCollection;
    class function GetInstance: TCollections; static;
  strict protected
    procedure Initialize; override;
    procedure Finalize; override;
  public
    class property Instance: TCollections read GetInstance;
    function GetEnumerator: TEnumerator<TCollection>;
    function IndexOfID(const AUID: TCollectionID): Integer;
    function ContainsID(const AUID: TCollectionID): Boolean;
    function ContainsName(const AName: string): Boolean;
    function GetCollection(const AUID: TCollectionID): TCollection;
    function Default: TCollection;
    procedure Add(const ACollection: TCollection);
    procedure Update(const ACollection: TCollection);
    procedure AddOrUpdate(const ACollection: TCollection);
    procedure Delete(const AUID: TCollectionID);
    procedure Clear;
    procedure Save;
    function ToArray: TArray<TCollection>;
    function GetAllIDs: TArray<TCollectionID>;
    function Count: Integer;
    property Items[const Idx: Integer]: TCollection read GetItem; default;
  end;

  TCollectionsPersist = record
  strict private
    const
      CountKey = 'Count';
      UIDKey = 'UID';
      NameKey = 'Name';
      StorageFormatKey = 'Storage.Format';
      StorageDirectoryKey = 'Storage.Directory';
    class procedure SaveCollection(const AOrdinal: Cardinal;
      const ACollection: TCollection); static;
    class procedure LoadCollection(const AOrdinal: Cardinal;
      const ACollections: TCollections); static;
  public
    class procedure Save(const ACollections: TCollections); static;
    class procedure Load(const ACollections: TCollections); static;
  end;

implementation

uses
  // Delphi
  RTLConsts,
  IOUtils,
  Math,
  // Project
  UAppInfo,
  UStrUtils,
  UUtils;

resourcestring
  SBadHexString = 'Invalid Hex String.';

{ TCollection }

constructor TCollection.Create(const AUID: TCollectionID; const AName: string;
  const AStorage: TDataStorageDetails);
var
  TrimmedName: string;
begin
  TrimmedName := StrTrim(AName);
  Assert(not AUID.IsNull, 'TCollection.Create: AUID is null');
  Assert(TrimmedName <> '',
    'TCollection.Create: AName is empty or only whitespace');
  {TODO -cRefactor: move following into IsValid method of TDataDetails}
  Assert(AStorage.Format <> TDataFormatKind.Error,
    'TCollection.Create: ADataDetails.Kind = TCollectionFormatKind.Error');
  fUID := AUID.Clone;
  fName := TrimmedName;
  fStorage := AStorage;
end;

function TCollection.IsDefault: Boolean;
begin
  Result := UID = TCollectionID.Default;
end;

function TCollection.IsValid: Boolean;
begin
  {TODO: Constructor enforces all these requirements, so #TCollection.IsValid
  may not be needed.}
  Result := not fUID.IsNull
    and (fName <> '')
    and (fStorage.Format <> TDataFormatKind.Error);
end;

{ TCollections }

procedure TCollections.Add(const ACollection: TCollection);
begin
  if not ContainsID(ACollection.UID) then
    fItems.Add(ACollection);
end;

procedure TCollections.AddOrUpdate(const ACollection: TCollection);
var
  Idx: Integer;
begin
  Idx := IndexOfID(ACollection.UID);
  if Idx < 0 then
    fItems.Add(ACollection)
  else
    fItems[Idx] := ACollection;
end;

procedure TCollections.Clear;
begin
  fItems.Clear;
end;

function TCollections.ContainsID(const AUID: TCollectionID):
  Boolean;
begin
  Result := IndexOfID(AUID) >= 0;
end;

function TCollections.ContainsName(const AName: string): Boolean;
var
  Collection: TCollection;
begin
  Result := False;
  for Collection in fItems do
    if StrSameText(AName, Collection.Name) then
      Exit(True);
end;

function TCollections.Count: Integer;
begin
  Result := fItems.Count;
end;

function TCollections.Default: TCollection;
begin
  Result := GetCollection(TCollectionID.Default);
end;

procedure TCollections.Delete(const AUID: TCollectionID);
resourcestring
  sCantDelete = 'Cannot delete the default collection';
var
  Idx: Integer;
begin
  if TCollectionID.Default = AUID then
    raise EArgumentException.Create(sCantDelete);
  Idx := IndexOfID(AUID);
  if Idx >= 0 then
    fItems.Delete(Idx);
end;

procedure TCollections.Finalize;
begin
  Save;
  fItems.Free;
end;

function TCollections.GetAllIDs: TArray<TCollectionID>;
var
  Idx: Integer;
begin
  SetLength(Result, fItems.Count);
  for Idx := 0 to Pred(fItems.Count) do
    Result[Idx] := fItems[Idx].UID;
end;

function TCollections.GetCollection(const AUID: TCollectionID): TCollection;
var
  Idx: Integer;
begin
  Idx := IndexOfID(AUID);
  if Idx < 0 then
    raise EArgumentException.CreateRes(@SGenericItemNotFound);
  Result := fItems[Idx];
end;

function TCollections.GetEnumerator: TEnumerator<TCollection>;
begin
  Result := fItems.GetEnumerator;
end;

class function TCollections.GetInstance: TCollections;
begin
  Result := TCollections.Create;
end;

function TCollections.GetItem(const Idx: Integer): TCollection;
begin
  Result := fItems[Idx];
end;

function TCollections.IndexOfID(const AUID: TCollectionID): Integer;
var
  Idx: Integer;
begin
  Result := -1;
  for Idx := 0 to Pred(fItems.Count) do
    if AUID = fItems[Idx].UID then
      Exit(Idx);
end;

procedure TCollections.Initialize;
begin
  fItems := TList<TCollection>.Create;
  TCollectionsPersist.Load(Self);
  // Ensure there is always at least the default collection present
  if not ContainsID(TCollectionID.Default) then
    Add(
      TCollection.Create(
        TCollectionID.Default,
        'Default',
        TDataStorageDetails.Create(
          TDataFormatInfo.DefaultFormat,
          TAppInfo.UserDefaultCollectionDir
        )
      )
    );
end;

procedure TCollections.Save;
begin
  TCollectionsPersist.Save(Self);
end;

function TCollections.ToArray: TArray<TCollection>;
begin
  Result := fItems.ToArray;
end;

procedure TCollections.Update(const ACollection: TCollection);
var
  Idx: Integer;
begin
  Idx := IndexOfID(ACollection.UID);
  if Idx >= 0 then
    fItems[Idx] := ACollection;
end;

{ TCollectionID }

constructor TCollectionID.Create(const ABytes: TBytes);
begin
  fID := System.Copy(ABytes);
end;

constructor TCollectionID.Create(const AStr: string);
begin
  fID := TEncoding.UTF8.GetBytes(AStr);
end;

function TCollectionID.Clone: TCollectionID;
begin
  Result := TCollectionID.Create(fID);
end;

class function TCollectionID.Compare(Left, Right: TCollectionID): Integer;
var
  CompareLength: Integer;
  Idx: Integer;
begin
  CompareLength := Min(Length(Left.fID), Length(Right.fID));
  Result := 0;
  for Idx := 0 to Pred(CompareLength) do
  begin
    Result := Left.fID[Idx] - Right.fID[Idx];
    if Result <> 0 then
      Exit;
  end;
  if Length(Left.fID) < Length(Right.fID) then
    Exit(-1)
  else if Length(Left.fID) > Length(Right.fID) then
    Exit(1);
end;

constructor TCollectionID.Create(const AGUID: TGUID);
begin
  fID := System.Copy(GUIDToBytes(AGUID));
end;

class function TCollectionID.CreateFromHexString(
  const AHexStr: string): TCollectionID;
var
  ConvertedBytes: TBytes;
begin
  if not TryHexStringToBytes(AHexStr, ConvertedBytes) then
    raise ECollectionID.Create(SBadHexString);
  Result := TCollectionID.Create(ConvertedBytes);
end;

class function TCollectionID.CreateNull: TCollectionID;
var
  NullID: TBytes;
begin
  SetLength(NullID, 0);
  Result := TCollectionID.Create(NullID);
end;

class function TCollectionID.Default: TCollectionID;
begin
  // Default collection is an empty GUID = 16 zero bytes
  Result := TCollectionID.Create(TGUID.Empty);
end;

class operator TCollectionID.Equal(Left, Right: TCollectionID):
  Boolean;
begin
  Result := IsEqualBytes(Left.fID, Right.fID);
end;

function TCollectionID.Hash: Integer;
begin
  Result := BobJenkinsHash(fID[0], Length(fID), 0);
end;

function TCollectionID.IsNull: Boolean;
begin
  Result := Length(fID) = 0;
end;

class operator TCollectionID.NotEqual(Left, Right: TCollectionID):
  Boolean;
begin
  Result := not IsEqualBytes(Left.fID, Right.fID);
end;

function TCollectionID.ToArray: TBytes;
begin
  Result := System.Copy(fID);
end;

function TCollectionID.ToHexString: string;
begin
  Result := BytesToHexString(fID);
end;

{ TCollectionID.TComparer }

function TCollectionID.TComparer.Compare(const Left,
  Right: TCollectionID): Integer;
begin
  Result := TCollectionID.Compare(Left, Right);
end;

function TCollectionID.TComparer.Equals(const Left,
  Right: TCollectionID): Boolean;
begin
  Result := Left = Right;
end;

function TCollectionID.TComparer.GetHashCode(
  const Value: TCollectionID): Integer;
begin
  Result := Value.Hash;
end;

{ TCollectionsPersist }

class procedure TCollectionsPersist.Load(
  const ACollections: TCollections);
var
  ConfigSection: ISettingsSection;
  Count: Integer;
  Idx: Integer;
begin
  ConfigSection := Settings.ReadSection(ssCollections);
  Count := ConfigSection.GetInteger(CountKey, 0);
  for Idx := 0 to Pred(Count) do
    LoadCollection(Idx, ACollections);
end;

class procedure TCollectionsPersist.LoadCollection(const AOrdinal: Cardinal;
  const ACollections: TCollections);
var
  ConfigSection: ISettingsSection;
  UID: TCollectionID;
  Name: string;
  Collection: TCollection;
  StorageDetails: TDataStorageDetails;
begin
  ConfigSection := Settings.ReadSection(ssCollection, IntToStr(AOrdinal));
  UID := TCollectionID.Create(ConfigSection.GetBytes(UIDKey));
  if ACollections.ContainsID(UID) then
    // Don't load a duplicate collection
    Exit;
  Name := ConfigSection.GetString(NameKey, '');

  StorageDetails := TDataStorageDetails.Create(
    TDataFormatKind(
      ConfigSection.GetInteger(StorageFormatKey, Ord(TDataFormatKind.Error))
    ),
    ConfigSection.GetString(StorageDirectoryKey, '')
  );
  Collection := TCollection.Create(UID, Name, StorageDetails);
  ACollections.Add(Collection);
end;

class procedure TCollectionsPersist.Save(const
  ACollections: TCollections);
var
  ConfigSection: ISettingsSection;
  Idx: Integer;
begin
  // Save number of collections
  ConfigSection := Settings.EmptySection(ssCollections);
  ConfigSection.SetInteger(CountKey, ACollections.Count);
  ConfigSection.Save;
  // Save each collection's properties in its own section
  for Idx := 0 to Pred(ACollections.Count) do
    SaveCollection(Idx, ACollections[Idx]);
end;

class procedure TCollectionsPersist.SaveCollection(const AOrdinal: Cardinal;
  const ACollection: TCollection);
var
  ConfigSection: ISettingsSection;
begin
  // Save info about collection format in its own section
  ConfigSection := Settings.EmptySection(ssCollection, IntToStr(AOrdinal));
  ConfigSection.SetBytes(UIDKey, ACollection.UID.ToArray);
  ConfigSection.SetString(NameKey, ACollection.Name);
  ConfigSection.SetInteger(StorageFormatKey, Ord(ACollection.Storage.Format));
  ConfigSection.SetString(StorageDirectoryKey, ACollection.Storage.Directory);
  ConfigSection.Save;
end;

{ TCollection.TComparer }

function TCollection.TComparer.Compare(const Left, Right: TCollection): Integer;
begin
  Result := TCollectionID.Compare(Left.UID, Right.UID);
end;

function TCollection.TComparer.Equals(const Left, Right: TCollection): Boolean;
begin
  Result := Left.UID = Right.UID;
end;

function TCollection.TComparer.GetHashCode(const Value: TCollection): Integer;
begin
  Result := Value.UID.Hash;
end;

end.


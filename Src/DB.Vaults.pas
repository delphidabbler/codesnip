{
 * This Source Code Form is subject to the terms of the Mozilla Public License,
 * v. 2.0. If a copy of the MPL was not distributed with this file, You can
 * obtain one at https://mozilla.org/MPL/2.0/
 *
 * Copyright (C) 2024, Peter Johnson (gravatar.com/delphidabbler).
 *
 * Implements support for multiple snippet vaults.
}


unit DB.Vaults;

{$ScopedEnums ON}

interface

uses
  SysUtils,
  Generics.Collections,
  Generics.Defaults,

  DB.DataFormats,
  DB.MetaData,
  UEncodings,
  UExceptions,
  USettings,
  USingleton;

type

  TVaultID = record
  strict private
    var
      fID: TBytes;
  public
    type
      TComparer = class(TInterfacedObject,
        IComparer<TVaultID>, IEqualityComparer<TVaultID>
      )
      public
        function Compare(const Left, Right: TVaultID): Integer;
        function Equals(const Left, Right: TVaultID): Boolean; reintroduce;
        function GetHashCode(const Value: TVaultID): Integer; reintroduce;
      end;
    constructor Create(const ABytes: TBytes); overload;
    constructor Create(const AStr: string); overload;
    constructor Create(const AGUID: TGUID); overload;
    class function CreateFromHexString(const AHexStr: string): TVaultID;
      static;
    class function CreateNull: TVaultID; static;
    class function Default: TVaultID; static;
    function Clone: TVaultID;
    function ToArray: TBytes;
    function ToHexString: string;
    function IsNull: Boolean;
    function Hash: Integer;
    class function Compare(Left, Right: TVaultID): Integer; static;
    class operator Equal(Left, Right: TVaultID): Boolean;
    class operator NotEqual(Left, Right: TVaultID): Boolean;
  end;

  EVaultID = class(ECodeSnip);

  TVault = class
  strict private
    var
      fUID: TVaultID;
      fName: string;
      fStorage: TDataStorageDetails;
      fMetaData: TMetaData;
      procedure SetMetaData(const AValue: TMetaData);
  public
    type
      TComparer = class(TInterfacedObject,
        IComparer<TVault>, IEqualityComparer<TVault>
      )
      public
        function Compare(const Left, Right: TVault): Integer;
        function Equals(const Left, Right: TVault): Boolean; reintroduce;
        function GetHashCode(const Value: TVault): Integer; reintroduce;
      end;
    ///  <summary>Creates a vault object.</summary>
    ///  <param name="AUID"><c>TVaultID</c> [in] Unique ID of the vault. Must
    ///  not be null.</param>
    ///  <param name="AName"><c>string</c> [in] Name of vault. Should be unique.
    ///  Must not be empty or only whitespace.</param>
    constructor Create(const AUID: TVaultID; const AName: string;
      const AStorage: TDataStorageDetails);
    ///  <summary>Vault identifier. Must be unique.</summary>
    property UID: TVaultID read fUID;
    ///  <summary>Vault name. Must be unique.</summary>
    property Name: string read fName;
    ///  <summary>Vault storage information.</summary>
    property Storage: TDataStorageDetails read fStorage;
    ///  <summary>Meta data associated with the vault.</summary>
    ///  <remarks>Meta data is read from and written to the associated storage.
    ///  </remarks>
    property MetaData: TMetaData read fMetaData write SetMetaData;
    ///  <summary>Checks if this object's fields are valid.</summary>
    function IsValid: Boolean;
    ///  <summary>Checks if this object is the default vault.</summary>
    function IsDefault: Boolean;
  end;

  TVaults = class sealed(TSingleton)
  strict private
    var
      fItems: TList<TVault>;
    function GetItem(const Idx: Integer): TVault;
    procedure DoUpdate(const Idx: Integer; const AVault: TVault);
    class function GetInstance: TVaults; static;
  strict protected
    procedure Initialize; override;
    procedure Finalize; override;
  public
    class property Instance: TVaults read GetInstance;
    function GetEnumerator: TEnumerator<TVault>;
    function IndexOfID(const AUID: TVaultID): Integer;
    function ContainsID(const AUID: TVaultID): Boolean;
    function ContainsName(const AName: string): Boolean;
    function GetVault(const AUID: TVaultID): TVault;
    function Default: TVault;
    procedure Add(const AVault: TVault);
    procedure Update(const AVault: TVault);
    procedure AddOrUpdate(const AVault: TVault);
    procedure Delete(const AUID: TVaultID);
    procedure Clear;
    procedure Save;
    function ToArray: TArray<TVault>;
    function GetAllIDs: TArray<TVaultID>;
    function Count: Integer;
    property Items[const Idx: Integer]: TVault read GetItem; default;
  end;

  TVaultsPersist = record
  strict private
    const
      CountKey = 'Count';
      UIDKey = 'UID';
      NameKey = 'Name';
      StorageFormatKey = 'Storage.Format';
      StorageDirectoryKey = 'Storage.Directory';
    class procedure SaveVault(const AOrdinal: Cardinal; const AVault: TVault); 
      static;
    class procedure LoadVault(const AOrdinal: Cardinal; const AVaults: TVaults); 
      static;
  public
    class procedure Save(const AVaults: TVaults); static;
    class procedure Load(const AVaults: TVaults); static;
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

{ TVault }

constructor TVault.Create(const AUID: TVaultID; const AName: string;
  const AStorage: TDataStorageDetails);
var
  TrimmedName: string;
begin
  TrimmedName := StrTrim(AName);
  Assert(not AUID.IsNull, 'TVault.Create: AUID is null');
  Assert(TrimmedName <> '',
    'TVault.Create: AName is empty or only whitespace');
  {TODO -cRefactor: move following into IsValid method of TDataDetails}
  Assert(AStorage.Format <> TDataFormatKind.Error,
    'TVault.Create: ADataDetails.Kind = TDataFormatKind.Error');
  fUID := AUID.Clone;
  fName := TrimmedName;
  fStorage := AStorage;
end;

function TVault.IsDefault: Boolean;
begin
  Result := UID = TVaultID.Default;
end;

function TVault.IsValid: Boolean;
begin
  {TODO: Constructor enforces all these requirements, so #TVault.IsValid
  may not be needed.}
  Result := not fUID.IsNull
    and (fName <> '')
    and (fStorage.Format <> TDataFormatKind.Error);
end;

procedure TVault.SetMetaData(const AValue: TMetaData);
begin
  fMetaData := AValue.Clone;
end;

{ TVaults }

procedure TVaults.Add(const AVault: TVault);
begin
  if not ContainsID(AVault.UID) then
    fItems.Add(AVault);
end;

procedure TVaults.AddOrUpdate(const AVault: TVault);
var
  Idx: Integer;
begin
  Idx := IndexOfID(AVault.UID);
  if Idx < 0 then
    fItems.Add(AVault)
  else
    DoUpdate(Idx, AVault);
end;

procedure TVaults.Clear;
var
  Idx: Integer;
begin
  for Idx := Pred(fItems.Count) downto 0 do
    DoUpdate(Idx, nil); // frees and nils item with given index
  fItems.Clear;
end;

function TVaults.ContainsID(const AUID: TVaultID):
  Boolean;
begin
  Result := IndexOfID(AUID) >= 0;
end;

function TVaults.ContainsName(const AName: string): Boolean;
var
  Vault: TVault;
begin
  Result := False;
  for Vault in fItems do
    if StrSameText(AName, Vault.Name) then
      Exit(True);
end;

function TVaults.Count: Integer;
begin
  Result := fItems.Count;
end;

function TVaults.Default: TVault;
begin
  Result := GetVault(TVaultID.Default);
end;

procedure TVaults.Delete(const AUID: TVaultID);
resourcestring
  sCantDelete = 'Cannot delete the default vault';
var
  Idx: Integer;
begin
  if TVaultID.Default = AUID then
    raise EArgumentException.Create(sCantDelete);
  Idx := IndexOfID(AUID);
  if Idx >= 0 then
  begin
    DoUpdate(Idx, nil); // frees and nils item with given index
    fItems.Delete(Idx);
  end;
end;

procedure TVaults.DoUpdate(const Idx: Integer; const AVault: TVault);
var
  OldEntry: TVault;
begin
  OldEntry := fItems[Idx];
  fItems[Idx] := AVault;
  OldEntry.Free;
end;

procedure TVaults.Finalize;
begin
  Save;
  Clear;
  fItems.Free;
end;

function TVaults.GetAllIDs: TArray<TVaultID>;
var
  Idx: Integer;
begin
  SetLength(Result, fItems.Count);
  for Idx := 0 to Pred(fItems.Count) do
    Result[Idx] := fItems[Idx].UID;
end;

function TVaults.GetVault(const AUID: TVaultID): TVault;
var
  Idx: Integer;
begin
  Idx := IndexOfID(AUID);
  if Idx < 0 then
    raise EArgumentException.CreateRes(@SGenericItemNotFound);
  Result := fItems[Idx];
end;

function TVaults.GetEnumerator: TEnumerator<TVault>;
begin
  Result := fItems.GetEnumerator;
end;

class function TVaults.GetInstance: TVaults;
begin
  Result := TVaults.Create;
end;

function TVaults.GetItem(const Idx: Integer): TVault;
begin
  Result := fItems[Idx];
end;

function TVaults.IndexOfID(const AUID: TVaultID): Integer;
var
  Idx: Integer;
begin
  Result := -1;
  for Idx := 0 to Pred(fItems.Count) do
    if AUID = fItems[Idx].UID then
      Exit(Idx);
end;

procedure TVaults.Initialize;
begin
  fItems := TList<TVault>.Create;
  TVaultsPersist.Load(Self);
  // Ensure there is always at least the default vault present
  if not ContainsID(TVaultID.Default) then
    Add(
      TVault.Create(
        TVaultID.Default,
        'Default',
        TDataStorageDetails.Create(
          TDataFormatInfo.DefaultFormat,
          TAppInfo.UserDefaultCollectionDir
        )
      )
    );
end;

procedure TVaults.Save;
begin
  TVaultsPersist.Save(Self);
end;

function TVaults.ToArray: TArray<TVault>;
begin
  Result := fItems.ToArray;
end;

procedure TVaults.Update(const AVault: TVault);
var
  Idx: Integer;
begin
  Idx := IndexOfID(AVault.UID);
  if Idx >= 0 then
    DoUpdate(Idx, AVault);
end;

{ TVaultID }

constructor TVaultID.Create(const ABytes: TBytes);
begin
  fID := System.Copy(ABytes);
end;

constructor TVaultID.Create(const AStr: string);
begin
  fID := TEncoding.UTF8.GetBytes(AStr);
end;

function TVaultID.Clone: TVaultID;
begin
  Result := TVaultID.Create(fID);
end;

class function TVaultID.Compare(Left, Right: TVaultID): Integer;
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

constructor TVaultID.Create(const AGUID: TGUID);
begin
  fID := System.Copy(GUIDToBytes(AGUID));
end;

class function TVaultID.CreateFromHexString(
  const AHexStr: string): TVaultID;
var
  ConvertedBytes: TBytes;
begin
  if not TryHexStringToBytes(AHexStr, ConvertedBytes) then
    raise EVaultID.Create(SBadHexString);
  Result := TVaultID.Create(ConvertedBytes);
end;

class function TVaultID.CreateNull: TVaultID;
var
  NullID: TBytes;
begin
  SetLength(NullID, 0);
  Result := TVaultID.Create(NullID);
end;

class function TVaultID.Default: TVaultID;
begin
  // Default vault is an empty GUID = 16 zero bytes
  Result := TVaultID.Create(TGUID.Empty);
end;

class operator TVaultID.Equal(Left, Right: TVaultID):
  Boolean;
begin
  Result := IsEqualBytes(Left.fID, Right.fID);
end;

function TVaultID.Hash: Integer;
begin
  Result := BobJenkinsHash(fID[0], Length(fID), 0);
end;

function TVaultID.IsNull: Boolean;
begin
  Result := Length(fID) = 0;
end;

class operator TVaultID.NotEqual(Left, Right: TVaultID):
  Boolean;
begin
  Result := not IsEqualBytes(Left.fID, Right.fID);
end;

function TVaultID.ToArray: TBytes;
begin
  Result := System.Copy(fID);
end;

function TVaultID.ToHexString: string;
begin
  Result := BytesToHexString(fID);
end;

{ TVaultID.TComparer }

function TVaultID.TComparer.Compare(const Left,
  Right: TVaultID): Integer;
begin
  Result := TVaultID.Compare(Left, Right);
end;

function TVaultID.TComparer.Equals(const Left,
  Right: TVaultID): Boolean;
begin
  Result := Left = Right;
end;

function TVaultID.TComparer.GetHashCode(
  const Value: TVaultID): Integer;
begin
  Result := Value.Hash;
end;

{ TVaultsPersist }

class procedure TVaultsPersist.Load(const AVaults: TVaults);
var
  ConfigSection: ISettingsSection;
  Count: Integer;
  Idx: Integer;
begin
  ConfigSection := Settings.ReadSection(ssVaults);
  Count := ConfigSection.GetInteger(CountKey, 0);
  for Idx := 0 to Pred(Count) do
    LoadVault(Idx, AVaults);
end;

class procedure TVaultsPersist.LoadVault(const AOrdinal: Cardinal;
  const AVaults: TVaults);
var
  ConfigSection: ISettingsSection;
  UID: TVaultID;
  Name: string;
  Vault: TVault;
  StorageDetails: TDataStorageDetails;
begin
  ConfigSection := Settings.ReadSection(ssVault, IntToStr(AOrdinal));
  UID := TVaultID.Create(ConfigSection.GetBytes(UIDKey));
  if AVaults.ContainsID(UID) then
    // Don't load a duplicate vault
    Exit;
  Name := ConfigSection.GetString(NameKey, '');

  StorageDetails := TDataStorageDetails.Create(
    TDataFormatKind(
      ConfigSection.GetInteger(StorageFormatKey, Ord(TDataFormatKind.Error))
    ),
    ConfigSection.GetString(StorageDirectoryKey, '')
  );
  Vault := TVault.Create(UID, Name, StorageDetails);
  AVaults.Add(Vault);
end;

class procedure TVaultsPersist.Save(const AVaults: TVaults);
var
  ConfigSection: ISettingsSection;
  Idx: Integer;
begin
  // Save number of vaults
  ConfigSection := Settings.EmptySection(ssVaults);
  ConfigSection.SetInteger(CountKey, AVaults.Count);
  ConfigSection.Save;
  // Save each vault's properties in its own section
  for Idx := 0 to Pred(AVaults.Count) do
    SaveVault(Idx, AVaults[Idx]);
end;

class procedure TVaultsPersist.SaveVault(const AOrdinal: Cardinal;
  const AVault: TVault);
var
  ConfigSection: ISettingsSection;
begin
  // Save info about vault format in its own section
  ConfigSection := Settings.EmptySection(ssVault, IntToStr(AOrdinal));
  ConfigSection.SetBytes(UIDKey, AVault.UID.ToArray);
  ConfigSection.SetString(NameKey, AVault.Name);
  ConfigSection.SetInteger(StorageFormatKey, Ord(AVault.Storage.Format));
  ConfigSection.SetString(StorageDirectoryKey, AVault.Storage.Directory);
  ConfigSection.Save;
end;

{ TVault.TComparer }

function TVault.TComparer.Compare(const Left, Right: TVault): Integer;
begin
  Result := TVaultID.Compare(Left.UID, Right.UID);
end;

function TVault.TComparer.Equals(const Left, Right: TVault): Boolean;
begin
  Result := Left.UID = Right.UID;
end;

function TVault.TComparer.GetHashCode(const Value: TVault): Integer;
begin
  Result := Value.UID.Hash;
end;

end.


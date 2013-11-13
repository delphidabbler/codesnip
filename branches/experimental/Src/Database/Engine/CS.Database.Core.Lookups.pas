unit CS.Database.Core.Lookups;

interface

uses
  Generics.Defaults,
  Generics.Collections,
  CS.Database.Types,
  CS.SourceCode.Languages;

type

  TDBLookup<TKey> = class(TObject)
  strict private
    var
      fLookup: TDictionary<TKey,IDBSnippetIDList>;
      fComparer: IEqualityComparer<TKey>;
    function DoAddKey(const AKey: TKey): IDBSnippetIDList;
  public
    constructor Create(AComparer: IEqualityComparer<TKey>);
    destructor Destroy; override;
    procedure Add(const AKey: TKey); overload;
    procedure Add(const AKey: TKey; const ASnippetID: TDBSnippetID); overload;
    procedure Delete(const AKey: TKey); overload;
    procedure Delete(const AKey: TKey; const ASnippetID: TDBSnippetID);
      overload;
    procedure Update(const AOldKey, ANewKey: TKey;
      const ASnippetID: TDBSnippetID);
    function GetIDs(const AKey: TKey): IDBSnippetIDList;
    function GetKeys: TArray<TKey>;
    function Contains(const AKey: TKey): Boolean; overload;
    function Contains(const AKey: TKey; const ASnippetID: TDBSnippetID):
      Boolean; overload;
  end;

  TDBTagLookup = class(TDBLookup<TDBTag>)
  public
    constructor Create;
  end;

  TSourceCodeLanguageIDLookup = class(TDBLookup<TSourceCodeLanguageID>)
    constructor Create;
  end;

implementation

uses
  Classes,
  RTLConsts,
  Contnrs,
  CS.Database.Exceptions,
  CS.Database.Snippets,
  CS.Database.Tags;

{ TDBLookup<TKey> }

procedure TDBLookup<TKey>.Add(const AKey: TKey;
  const ASnippetID: TDBSnippetID);
var
  IDList: IDBSnippetIDList;
begin
  if fLookup.ContainsKey(AKey) then
  begin
    fLookup[AKey].Add(ASnippetID)
  end
  else
  begin
    IDList := DoAddKey(AKey);
    IDList.Add(ASnippetID);
  end;
end;

procedure TDBLookup<TKey>.Add(const AKey: TKey);
begin
  DoAddKey(AKey);
end;

function TDBLookup<TKey>.Contains(const AKey: TKey): Boolean;
begin
  Result := fLookup.ContainsKey(AKey);
end;

function TDBLookup<TKey>.Contains(const AKey: TKey;
  const ASnippetID: TDBSnippetID): Boolean;
begin
  if not fLookup.ContainsKey(AKey) then
    Exit(False);
  Result := fLookup[AKey].Contains(ASnippetID);
end;

constructor TDBLookup<TKey>.Create(AComparer: IEqualityComparer<TKey>);
begin
  Assert(Assigned(AComparer), ClassName + '.Create: AComparer is nil');
  inherited Create;
  fComparer := AComparer;
  fLookup := TDictionary<TKey,IDBSnippetIDList>.Create(AComparer);
end;

procedure TDBLookup<TKey>.Delete(const AKey: TKey;
  const ASnippetID: TDBSnippetID);
begin
  if not fLookup.ContainsKey(AKey) then
    raise EListError.Create(SGenericItemNotFound);
  fLookup[AKey].Delete(ASnippetID);
end;

procedure TDBLookup<TKey>.Delete(const AKey: TKey);
begin
  if not flookup.ContainsKey(AKey) then
    raise EListError.Create(SGenericItemNotFound);
  fLookup.Remove(AKey);
end;

destructor TDBLookup<TKey>.Destroy;
begin
  fLookup.Free;
  inherited;
end;

function TDBLookup<TKey>.DoAddKey(const AKey: TKey): IDBSnippetIDList;
begin
  Result := TDBSnippetIDList.Create;
  fLookup.Add(AKey, Result);
end;

function TDBLookup<TKey>.GetIDs(const AKey: TKey): IDBSnippetIDList;
begin
  Result := fLookup[AKey];
end;

function TDBLookup<TKey>.GetKeys: TArray<TKey>;
begin
  Result := fLookup.Keys.ToArray;
end;

procedure TDBLookup<TKey>.Update(const AOldKey, ANewKey: TKey;
  const ASnippetID: TDBSnippetID);
begin
  if fComparer.Equals(AOldKey, ANewKey) then
    Exit;
  Delete(AOldKey, ASnippetID);
  Add(ANewKey, ASnippetID);
end;

{ TDBTagLookup }

constructor TDBTagLookup.Create;
begin
  inherited Create(TDBTag.TEqualityComparer.Create);
end;

{ TSourceCodeLanguageIDLookup }

constructor TSourceCodeLanguageIDLookup.Create;
begin
  inherited Create(TSourceCodeLanguageID.TEqualityComparer.Create);
end;

end.


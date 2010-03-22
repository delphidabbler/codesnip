unit UOrderedDictionary;

interface

uses
  Generics.Defaults, Generics.Collections,
  UOrderedList;

type

  TOrderedDictionary<TKey,TValue> = class(TObject)
  private
    fList: TOrderedList<TPair<TKey,TValue>>;
    fComparer: IComparer<TKey>;
    function GetValue(const Key: TKey): TValue;
    function GetCount: Integer;
    function GetKey(const Idx: Integer): TKey;
    function GetValueByIndex(const Idx: Integer): TValue;
  public
    constructor Create; overload;
    constructor Create(const AComparer: IComparer<TKey>); overload;
    destructor Destroy; override;
    function Add(const Key: TKey; const Value: TValue): Integer; overload;
    function Add(const Item: TPair<TKey,TValue>): Integer; overload;
    procedure Clear;
    function Find(const Key: TKey; out Index: Integer): Boolean;
    function IndexOf(const Key: TKey): Integer;
    function Contains(const Key: TKey): Boolean;
    function IsEmpty: Boolean;
    // TODO: Delete
    // TODO: Remove
    // TODO: Notification stuff: hand off to fList
    function GetEnumerator: TEnumerator<TPair<TKey,TValue>>;
    property Count: Integer read GetCount;
// TODO:   property Items[Idx: Integer]: <TPair<TKey,TValue>> read GetItem;
    property Keys[const Idx: Integer]: TKey read GetKey;
    property Values[const Key: TKey]: TValue read GetValue; default;
    property ValuesByIndex[const Idx: Integer]: TValue read GetValueByIndex;
  end;

resourcestring
  sKeyNotFound = 'Key not found';

implementation

uses
  Classes;

{ TOrderedDictionary<TKey, TValue> }

function TOrderedDictionary<TKey, TValue>.Add(const Key: TKey;
  const Value: TValue): Integer;
begin
  Result := Add(TPair<TKey,TValue>.Create(Key, Value));
end;

function TOrderedDictionary<TKey, TValue>.Add(
  const Item: TPair<TKey, TValue>): Integer;
begin
  Result := fList.Add(Item);
end;

procedure TOrderedDictionary<TKey, TValue>.Clear;
begin
  fList.Clear;
end;

function TOrderedDictionary<TKey, TValue>.Contains(const Key: TKey): Boolean;
var
  Dummy: Integer;
begin
  Result := Find(Key, Dummy);
end;

constructor TOrderedDictionary<TKey, TValue>.Create(
  const AComparer: IComparer<TKey>);
begin
  inherited Create;
  // determine comparer
  if Assigned(AComparer) then
    fComparer := AComparer
  else
    fComparer := TComparer<TKey>.Default;
  // created ordered list for key value pair that compares only on key
  fList := TOrderedList<TPair<TKey,TValue>>.Create(
    TComparer<TPair<TKey,TValue>>.Construct(
      function(const Left, Right: TPair<TKey,TValue>): Integer
      begin
        Result := fComparer.Compare(Left.Key, Right.Key);
      end
    )
  );
  // initialize properties
  fList.PermitDuplicates := False;
end;

constructor TOrderedDictionary<TKey, TValue>.Create;
begin
  Create(nil);
end;

destructor TOrderedDictionary<TKey, TValue>.Destroy;
begin
  fList.Free;
  inherited;
end;

function TOrderedDictionary<TKey, TValue>.Find(const Key: TKey;
  out Index: Integer): Boolean;
begin
  Result := fList.Find(TPair<TKey,TValue>.Create(Key, Default(TValue)), Index);
end;

function TOrderedDictionary<TKey, TValue>.GetCount: Integer;
begin
  Result := fList.Count;
end;

function TOrderedDictionary<TKey, TValue>.GetEnumerator:
  TEnumerator<TPair<TKey, TValue>>;
begin
  Result := fList.GetEnumerator;
end;

function TOrderedDictionary<TKey, TValue>.GetKey(const Idx: Integer): TKey;
begin
  Result := fList[Idx].Key;
end;

function TOrderedDictionary<TKey, TValue>.GetValue(const Key: TKey): TValue;
var
  Idx: Integer;
begin
  if not Find(Key, Idx) then
    raise EListError.Create(sKeyNotFound);
  Result := fList[Idx].Value;
end;

function TOrderedDictionary<TKey, TValue>.GetValueByIndex(
  const Idx: Integer): TValue;
begin
  Result := fList[Idx].Value;
end;

function TOrderedDictionary<TKey, TValue>.IndexOf(const Key: TKey): Integer;
begin
  if not Find(Key, Result) then
    Exit(-1);
end;

function TOrderedDictionary<TKey, TValue>.IsEmpty: Boolean;
begin
  Result := Count = 0;
end;

end.

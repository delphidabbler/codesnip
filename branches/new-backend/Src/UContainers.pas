{
 * UContainers.pas
 *
 * Provides various generic container classes.
 *
 * $Rev$
 * $Date$
 *
 * ***** BEGIN LICENSE BLOCK *****
 *
 * Version: MPL 1.1
 *
 * The contents of this file are subject to the Mozilla Public License Version
 * 1.1 (the "License"); you may not use this file except in compliance with the
 * License. You may obtain a copy of the License at http://www.mozilla.org/MPL/
 *
 * Software distributed under the License is distributed on an "AS IS" basis,
 * WITHOUT WARRANTY OF ANY KIND, either express or implied. See the License for
 * the specific language governing rights and limitations under the License.
 *
 * The Original Code is UContainers.pas
 *
 * The Initial Developer of the Original Code is Peter Johnson
 * (http://www.delphidabbler.com/).
 *
 * Portions created by the Initial Developer are Copyright (C) 2010 Peter
 * Johnson. All Rights Reserved.
 *
 * Contributor(s)
 *   NONE
 *
 * ***** END LICENSE BLOCK *****
}


unit UContainers;

interface

uses
  // Delphi
  Classes,  // declare before Generics.Collections
  Generics.Defaults, Generics.Collections;

type

  // TODO: make TOrderedList<T> descend from TEnumerable<T> like TList<T>
  // TODO: Notification stuff: hand off to fList
  TOrderedList<T> = class(TObject)
  strict private
    fList: TList<T>;
    fComparer: IComparer<T>;
    fPermitDuplicates: Boolean;
    fOnNotify: TCollectionNotifyEvent<T>;
    function GetCount: Integer;
    function GetItem(Idx: Integer): T;
    procedure SetPermitDuplicates(const Value: Boolean);
    procedure NotificationHandler(Sender: TObject; const Item: T;
      Action: TCollectionNotification);
  strict protected
    procedure Notify(const Item: T; Action: TCollectionNotification); virtual;
  public
    constructor Create; overload;
    constructor Create(const AComparer: IComparer<T>); overload;
    destructor Destroy; override;
    function Add(const Item: T): Integer;
    procedure Clear;
    function Find(const Item: T; out Index: Integer): Boolean;
    function IndexOf(const Item: T): Integer;
    function Contains(const Item: T): Boolean;
    function IsEmpty: Boolean;
    function Remove(const Item: T): Integer;
    procedure Delete(Index: Integer);
    function ContainsDuplicates: Boolean;
    function GetEnumerator: TEnumerator<T>;
    property Count: Integer read GetCount;
    property PermitDuplicates: Boolean read fPermitDuplicates write
      SetPermitDuplicates;
    property Items[Idx: Integer]: T read GetItem; default;
    property OnNotify: TCollectionNotifyEvent<T> read fOnNotify write fOnNotify;
  end;

  // TODO: make TOrderedDictionary<TKey,TValue> descend from TEnumerable<T>
  // TODO: Notification stuff: hand off to fList
  TOrderedDictionary<TKey,TValue> = class(TObject)
  private
    fList: TOrderedList<TPair<TKey,TValue>>;
    fComparer: IComparer<TKey>;
    fOnKeyNotify: TCollectionNotifyEvent<TKey>;
    fOnValueNotify: TCollectionNotifyEvent<TValue>;
    function GetValue(const Key: TKey): TValue;
    function GetCount: Integer;
    function GetKey(const Idx: Integer): TKey;
    function GetValueByIndex(const Idx: Integer): TValue;
    procedure NotificationHandler(Sender: TObject;
      const Item: TPair<TKey,TValue>; Action: TCollectionNotification);
  strict protected
    procedure KeyNotify(const Key: TKey;
       Action: TCollectionNotification); virtual;
    procedure ValueNotify(const Value: TValue;
      Action: TCollectionNotification); virtual;
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
    function Remove(const Key: TKey): Integer;
    procedure Delete(Index: Integer);
    function GetEnumerator: TEnumerator<TPair<TKey,TValue>>;
    property Count: Integer read GetCount;
// TODO:   property Items[Idx: Integer]: <TPair<TKey,TValue>> read GetItem;
    property Keys[const Idx: Integer]: TKey read GetKey;
    property Values[const Key: TKey]: TValue read GetValue; default;
    property ValuesByIndex[const Idx: Integer]: TValue read GetValueByIndex;
    property OnKeyNotify: TCollectionNotifyEvent<TKey> read fOnKeyNotify
      write fOnKeyNotify;
    property OnValueNotify: TCollectionNotifyEvent<TValue> read fOnValueNotify
      write fOnValueNotify;
  end;

resourcestring // must be in interface for parametised types
  sOrderedListDuplicateErr = 'Duplicate item not permitted';
  sOrderedListPermitDuplicatesError = 'List contains duplicates';
  sKeyNotFound = 'Key not found';

implementation

{ TOrderedList<T> }

function TOrderedList<T>.Add(const Item: T): Integer;
begin
  if Find(Item, Result) and not PermitDuplicates then
    raise EListError.Create(sOrderedListDuplicateErr);
  fList.Insert(Result, Item);
end;

procedure TOrderedList<T>.Clear;
begin
  fList.Clear;
end;

function TOrderedList<T>.Contains(const Item: T): Boolean;
var
  Dummy: Integer;
begin
  Result := Find(Item, Dummy);
end;

function TOrderedList<T>.ContainsDuplicates: Boolean;
var
  Idx: Integer;
begin
  Result := False;
  for Idx := 1 to Pred(fList.Count) do
    if fComparer.Compare(fList[Idx], fList[Idx - 1]) = 0 then
      Exit(True);
end;

constructor TOrderedList<T>.Create;
begin
  Create(nil);
end;

constructor TOrderedList<T>.Create(const AComparer: IComparer<T>);
begin
  inherited Create;
  if Assigned(AComparer) then
    fComparer := AComparer
  else
    fComparer := TComparer<T>.Default;
  fList := TList<T>.Create(fComparer);
  fList.OnNotify := NotificationHandler;
end;

procedure TOrderedList<T>.Delete(Index: Integer);
begin
  fList.Delete(Index);
end;

destructor TOrderedList<T>.Destroy;
begin
  fList.Free;
  inherited;
end;

function TOrderedList<T>.Find(const Item: T; out Index: Integer): Boolean;
begin
  Result := fList.BinarySearch(Item, Index);
end;

function TOrderedList<T>.GetCount: Integer;
begin
  Result := fList.Count;
end;

function TOrderedList<T>.GetEnumerator: TEnumerator<T>;
begin
  Result := fList.GetEnumerator;
end;

function TOrderedList<T>.GetItem(Idx: Integer): T;
begin
  Result := fList[Idx];
end;

function TOrderedList<T>.IndexOf(const Item: T): Integer;
begin
  if not Find(Item, Result) then
    Result := -1;
end;

function TOrderedList<T>.IsEmpty: Boolean;
begin
  Result := Count = 0;
end;

procedure TOrderedList<T>.NotificationHandler(Sender: TObject; const Item: T;
  Action: TCollectionNotification);
begin
  Notify(Item, Action);
end;

procedure TOrderedList<T>.Notify(const Item: T;
  Action: TCollectionNotification);
begin
  if Assigned(fOnNotify) then
    fOnNotify(Self, Item, Action);
end;

function TOrderedList<T>.Remove(const Item: T): Integer;
begin
  Result := fList.Remove(Item);
end;

procedure TOrderedList<T>.SetPermitDuplicates(const Value: Boolean);
begin
  if not Value and ContainsDuplicates then
    raise EListError.Create(sOrderedListPermitDuplicatesError);
  fPermitDuplicates := Value;
end;

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
  fList.OnNotify := NotificationHandler;
end;

constructor TOrderedDictionary<TKey, TValue>.Create;
begin
  Create(nil);
end;

procedure TOrderedDictionary<TKey, TValue>.Delete(Index: Integer);
begin
  fList.Delete(Index);
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

procedure TOrderedDictionary<TKey, TValue>.KeyNotify(const Key: TKey;
  Action: TCollectionNotification);
begin
  if Assigned(fOnKeyNotify) then
    fOnKeyNotify(Self, Key, Action);
end;

procedure TOrderedDictionary<TKey, TValue>.NotificationHandler(Sender: TObject;
  const Item: TPair<TKey,TValue>; Action: TCollectionNotification);
begin
  KeyNotify(Item.Key, Action);
  ValueNotify(Item.Value, Action);
end;

function TOrderedDictionary<TKey, TValue>.Remove(const Key: TKey): Integer;
begin
  Result := fList.Remove(TPair<TKey,TValue>.Create(Key, Default(TValue)));
end;

procedure TOrderedDictionary<TKey, TValue>.ValueNotify(const Value: TValue;
  Action: TCollectionNotification);
begin
  if Assigned(fOnValueNotify) then
    fOnValueNotify(Self, Value, Action);
end;

end.


{
 * UOrderedList.pas
 *
 * Provides a generic ordered list class.
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
 * The Original Code is UOrderedList.pas
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


unit UOrderedList;

interface

uses
  Generics.Defaults, Generics.Collections;

type

  // TODO: make TOrderedList<T> descend from TEnumerable<T> like TList<T>
  TOrderedList<T> = class(TObject)
  strict private
    fList: TList<T>;
    fComparer: IComparer<T>;
    fPermitDuplicates: Boolean;
    function GetCount: Integer;
    function GetItem(Idx: Integer): T;
    procedure SetPermitDuplicates(const Value: Boolean);
  public
    constructor Create; overload;
    constructor Create(const AComparer: IComparer<T>); overload;
    destructor Destroy; override;
    function Add(const Item: T): Integer;
    procedure Clear;
    function Find(const Item: T; out Index: Integer): Boolean;
    // TODO: IndexOf
    // TODO: Contains
    // TODO: IsEmpty
    // TODO: Delete
    // TODO: Remove
    // TODO: Notification stuff: hand off to fList
    function ContainsDuplicates: Boolean;
    function GetEnumerator: TEnumerator<T>;
    property Count: Integer read GetCount;
    property PermitDuplicates: Boolean
      read fPermitDuplicates write SetPermitDuplicates;
    property Items[Idx: Integer]: T read GetItem; default;
  end;

resourcestring // must be in interface for parametised types
  sOrderedListDuplicateErr = 'Duplicate item not permitted';
  sOrderedListPermitDuplicatesError = 'List contains duplicates';

implementation

uses
  Classes;

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

procedure TOrderedList<T>.SetPermitDuplicates(const Value: Boolean);
begin
  if not Value and ContainsDuplicates then
    raise EListError.Create(sOrderedListPermitDuplicatesError);
  fPermitDuplicates := Value;
end;

end.


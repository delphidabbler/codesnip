{
 * This Source Code Form is subject to the terms of the Mozilla Public License,
 * v. 2.0. If a copy of the MPL was not distributed with this file, You can
 * obtain one at https://mozilla.org/MPL/2.0/
 *
 * Copyright (C) 2009-2021, Peter Johnson (gravatar.com/delphidabbler).
 *
 * Defines a record that uniquely indentifies a snippet in the database, along
 * with an object that maintains a list of snippet ID records.
}


unit USnippetIDs;


interface


uses
  // Delphi
  Generics.Collections,
  // Project
  IntfCommon,
  DB.UCollections;


type

  ///  <summary>Record that uniquely identifies a code snippet.</summary>
  ///  <remarks>Comprises the snippet's key and collection.</remarks>
  TSnippetID = record
  strict private
    var
      ///  <summary>Value of Key property.</summary>
      fKey: string;
      fCollectionID: TCollectionID;
    procedure SetCollectionID(const AValue: TCollectionID);
  public
    ///  <summary>Snippet's key.</summary>
    property Key: string read fKey write fKey;

    ///  <summary>ID of the collection to which a snippet with this ID belongs.
    ///  </summary>
    ///  <remarks>ID must not be null.</remarks>
    property CollectionID: TCollectionID
      read fCollectionID write SetCollectionID;

    ///  <summary>Creates a record with given property values.</summary>
    ///  <remarks><c>ACollectionID</c> must not be null.</remarks>
    constructor Create(const AKey: string; const ACollectionID: TCollectionID);

    ///  <summary>Creates copy of given snippet ID</summary>
    constructor Clone(const Src: TSnippetID);

    ///  <summary>Compares this record with another.</summary>
    ///  <param name="SID">TSnippetID [in] Snippet ID to compare with.</param>
    ///  <returns>Integer. 0 if records are same, -ve if this record less than
    ///  SID or +ve if this record greater than SID.</returns>
    function CompareTo(const SID: TSnippetID): Integer;

    ///  <summary>Compares two snippet keys.</summary>
    ///  <param name="Left">string [in] First key.</param>
    ///  <param name="Right">string [in] Second key.</param>
    ///  <result>Integer. 0 if keys are same, -ve if Left is less than Right or
    ///  +ve Left is greater than Right.</result>
    class function CompareKeys(const Left, Right: string): Integer; static;

    ///  <summary>Overload of equality operator for two TSnippetIDs.</summary>
    class operator Equal(const SID1, SID2: TSnippetID): Boolean;

    ///  <summary>Overload of inequality operator for two TSnippetIDs.</summary>
    class operator NotEqual(const SID1, SID2: TSnippetID): Boolean;
  end;

type
  ///  <summary>Interface supported by objects that implement a list of
  ///  TSnippetID records.</summary>
  ISnippetIDList = interface(IInterface)
    ['{238CFDCC-E84E-4D29-9BC6-10FBCECBC4FA}']
    ///  <summary>Gets new list enumerator.</summary>
    function GetEnumerator: TEnumerator<TSnippetID>;
    ///  <summary>Clears the list.</summary>
    procedure Clear;
    ///  <summary>Adds given snippet ID to list and returns its index in list.
    ///  </summary>
    function Add(const SnippetID: TSnippetID): Integer;
    ///  <summary>Checks if list contains given snippet ID.</summary>
    function Contains(const SnippetID: TSnippetID): Boolean;
    ///  <summary>Returns number of snippet ID records in list.</summary>
    function Count: Integer;
    ///  <summary>Gets snippet ID record from list by index.</summary>
    function GetItem(Idx: Integer): TSnippetID;
    ///  <summary>Stores snippet ID record in list at specified index.</summary>
    procedure SetItem(Idx: Integer; const Value: TSnippetID);
    ///  <summary>Provides read/write access to snippet IDs by index.</summary>
    property Items[Idx: Integer]: TSnippetID
      read GetItem write SetItem; default;
  end;

type
  ///  <summary>Implements a list of snippet identification records.</summary>
  TSnippetIDList = class(TInterfacedObject, ISnippetIDList, IAssignable)
  strict private
    var
      ///  <summary>Internal list if snippet ID records.</summary>
      fList: TList<TSnippetID>;
  public
    ///  <summary>Constructs empty list object.</summary>
    constructor Create;

    ///  <summary>Destroys object.</summary>
    destructor Destroy; override;

    ///  <summary>Gets new list enumerator.</summary>
    ///  <remarks>Method of ISnippetIDList.</remarks>
    function GetEnumerator: TEnumerator<TSnippetID>;

    ///  <summary>Clears the list.</summary>
    ///  <remarks>Method of ISnippetIDList.</remarks>
    procedure Clear;

    ///  <summary>Adds given snippet ID to list and returns its index in list.
    ///  </summary>
    ///  <remarks>Method of ISnippetIDList.</remarks>
    function Add(const SnippetID: TSnippetID): Integer;

    ///  <summary>Checks if list contains given snippet ID.</summary>
    ///  <remarks>Method of ISnippetIDList.</remarks>
    function Contains(const SnippetID: TSnippetID): Boolean;

    ///  <summary>Returns number of snippet ID records in list.</summary>
    ///  <remarks>Method of ISnippetIDList.</remarks>
    function Count: Integer;

    ///  <summary>Gets snippet ID record from list by index.</summary>
    ///  <remarks>Method of ISnippetIDList.</remarks>
    function GetItem(Idx: Integer): TSnippetID;

    ///  <summary>Stores snippet ID record in list at specified index.</summary>
    ///  <remarks>Method of ISnippetIDList.</remarks>
    procedure SetItem(Idx: Integer; const Value: TSnippetID);

    ///  <summary>Copies properties of given list to this one.</summary>
    ///  <param name="Src">IInterface [in] List whose properties are to be
    ///  copied. Src must support ISnippetIDList.</param>
    ///  <remarks>Method of IAssignable.</remarks>
    procedure Assign(const Src: IInterface);
  end;


implementation


uses
  // Delphi
  SysUtils, Generics.Defaults,
  // Project
  UStrUtils;


{ TSnippetID }

constructor TSnippetID.Clone(const Src: TSnippetID);
begin
  Create(Src.Key, Src.CollectionID);
end;

class function TSnippetID.CompareKeys(const Left, Right: string): Integer;
begin
  Result := StrCompareText(Left, Right);
end;

function TSnippetID.CompareTo(const SID: TSnippetID): Integer;
begin
  Result := CompareKeys(Key, SID.Key);
  if Result = 0 then
    // TODO -cNote: New comparison changes ordering (no problem tho!)
    Result := TCollectionID.Compare(CollectionID, SID.CollectionID);
end;

constructor TSnippetID.Create(const AKey: string;
  const ACollectionID: TCollectionID);
begin
  fKey := AKey;
  SetCollectionID(ACollectionID);
end;

class operator TSnippetID.Equal(const SID1, SID2: TSnippetID): Boolean;
begin
  Result := SID1.CompareTo(SID2) = 0;
end;

class operator TSnippetID.NotEqual(const SID1, SID2: TSnippetID): Boolean;
begin
  Result := not (SID1 = SID2);
end;

procedure TSnippetID.SetCollectionID(const AValue: TCollectionID);
begin
  Assert(not AValue.IsNull, 'TSnippetID.SetCollectionID: Value is null');
  fCollectionID := AValue.Clone;
end;

{ TSnippetIDList }

function TSnippetIDList.Add(const SnippetID: TSnippetID): Integer;
begin
  Result := fList.Add(SnippetID);
end;

procedure TSnippetIDList.Assign(const Src: IInterface);
var
  SrcID: TSnippetID;  // references each ID in source
begin
  Assert(Supports(Src, ISnippetIDList),
    ClassName + '.Assign: Src must support ISnippetIDList');
  Clear;
  for SrcID in (Src as ISnippetIDList) do
    Add(TSnippetID.Clone(SrcID));
end;

procedure TSnippetIDList.Clear;
begin
  fList.Clear;
end;

function TSnippetIDList.Contains(const SnippetID: TSnippetID): Boolean;
begin
  Result := fList.Contains(SnippetID);
end;

function TSnippetIDList.Count: Integer;
begin
  Result := fList.Count;
end;

constructor TSnippetIDList.Create;
begin
  inherited;
  fList := TList<TSnippetID>.Create(
    TDelegatedComparer<TSnippetID>.Create(
      function(const Left, Right: TSnippetID): Integer
      begin
        Result := Left.CompareTo(Right);
      end
    )
  );
end;

destructor TSnippetIDList.Destroy;
begin
  fList.Free;
  inherited;
end;

function TSnippetIDList.GetEnumerator: TEnumerator<TSnippetID>;
begin
  Result := fList.GetEnumerator;
end;

function TSnippetIDList.GetItem(Idx: Integer): TSnippetID;
begin
  Result := fList[Idx];
end;

procedure TSnippetIDList.SetItem(Idx: Integer; const Value: TSnippetID);
begin
  fList[Idx] := Value;
end;

end.


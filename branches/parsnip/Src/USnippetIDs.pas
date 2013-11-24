{
 * This Source Code Form is subject to the terms of the Mozilla Public License,
 * v. 2.0. If a copy of the MPL was not distributed with this file, You can
 * obtain one at http://mozilla.org/MPL/2.0/
 *
 * Copyright (C) 2009-2013, Peter Johnson (www.delphidabbler.com).
 *
 * $Rev$
 * $Date$
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
  IntfCommon;


type

  ///  <summary>Record that uniquely identifies a code snippet. Specifies name
  ///  and flag indicating whether snippet is user-defined.</summary>
  TSnippetID = record
  strict private
    var
      ///  <summary>Internal value of snippet ID.</summary>
      fID: string;
  public
    ///  <summary>Creates a record with given ID string.</summary>
    constructor Create(const AIDStr: string);

    ///  <summary>Returns hash of snippet ID.</summary>
    function Hash: Integer;

    ///  <summary>Returns string representation of ID.</summary>
    function ToString: string;

    ///  <summary>Compares two snippet IDs.</summary>
    ///  <param name="Left">string [in] First ID to compare.</param>
    ///  <param name="Right">string [in] Second ID to compare.</param>
    ///  <returns>Integer. 0 if names are same, -ve if Left is less than Right
    ///  or +ve Left is greater than Right.</returns>
    class function Compare(const Left, Right: TSnippetID): Integer; static;

    ///  <summary>Overload of equality operator for two TSnippetIDs.</summary>
    class operator Equal(const Left, Right: TSnippetID): Boolean;

    ///  <summary>Overload of inequality operator for two TSnippetIDs.</summary>
    class operator NotEqual(const Left, Right: TSnippetID): Boolean;
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
    ///  <summary>Removed the given snippet ID from the list.</summary>
    ///  <remarks>Does nothing if SnippetID is not in the list.</remarks>
    procedure Remove(const SnippetID: TSnippetID);
    ///  <summary>Checks if list contains given snippet ID.</summary>
    function Contains(const SnippetID: TSnippetID): Boolean;
    ///  <summary>Checks if list is empty.</summary>
    function IsEmpty: Boolean;
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
  TSnippetIDList = class(
    TInterfacedObject, ISnippetIDList, IAssignable, IClonable
  )
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

    ///  <summary>Removed the given snippet ID from the list.</summary>
    ///  <remarks>
    ///  <para>Does nothing if SnippetID is not in the list.</para>
    ///  <para>Method of ISnippetIDList.</para>
    ///  </remarks>
    procedure Remove(const SnippetID: TSnippetID);

    ///  <summary>Checks if list contains given snippet ID.</summary>
    ///  <remarks>Method of ISnippetIDList.</remarks>
    function Contains(const SnippetID: TSnippetID): Boolean;

    ///  <summary>Checks if list is empty.</summary>
    ///  <remarks>Method of ISnippetIDList.</remarks>
    function IsEmpty: Boolean;

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

    ///  <summary>Creates and returns a new list that is an exact copy of the
    ///  current one.</summary>
    ///  <returns>IInterface. Reference to cloned object.</returns>
    ///  <remarks>Method of IClonable</remarks>
    function Clone: IInterface;
  end;


implementation


uses
  // Delphi
  SysUtils, Generics.Defaults,
  // Project
  CS.Utils.Hashes,
  UExceptions,
  UStrUtils;


{ TSnippetID }

class function TSnippetID.Compare(const Left, Right: TSnippetID): Integer;
begin
  Result := StrCompareText(Left.fID, Right.fID);
end;

constructor TSnippetID.Create(const AIDStr: string);
begin
  fID := AIDStr;
end;

class operator TSnippetID.Equal(const Left, Right: TSnippetID): Boolean;
begin
  Result := Compare(Left, Right) = 0;
end;

function TSnippetID.Hash: Integer;
begin
  Result := TextHash(fID);
end;

class operator TSnippetID.NotEqual(const Left, Right: TSnippetID): Boolean;
begin
  Result := Compare(Left, Right) <> 0;
end;

function TSnippetID.ToString: string;
begin
  Result := fID;
end;

{ TSnippetIDList }

function TSnippetIDList.Add(const SnippetID: TSnippetID): Integer;
begin
  Result := fList.Add(SnippetID);
end;

procedure TSnippetIDList.Assign(const Src: IInterface);
var
  SrcID: TSnippetID;  // references each ID in source
  SrcList: ISnippetIDList;
begin
  if not Supports(Src, ISnippetIDList, Srclist) then
    raise EBug.Create(ClassName + '.Assign: Src must support ISnippetIDList');
  Clear;
  for SrcID in SrcList do
    Add(SrcID);
end;

procedure TSnippetIDList.Clear;
begin
  fList.Clear;
end;

function TSnippetIDList.Clone: IInterface;
begin
  Result := TSnippetIDList.Create;
  (Result as IAssignable).Assign(Self);
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
        Result := TSnippetID.Compare(Left, Right);
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

function TSnippetIDList.IsEmpty: Boolean;
begin
  Result := fList.Count < 0;
end;

procedure TSnippetIDList.Remove(const SnippetID: TSnippetID);
begin
  fList.Remove(SnippetID);
end;

procedure TSnippetIDList.SetItem(Idx: Integer; const Value: TSnippetID);
begin
  fList[Idx] := Value;
end;

end.


{
 * This Source Code Form is subject to the terms of the Mozilla Public License,
 * v. 2.0. If a copy of the MPL was not distributed with this file, You can
 * obtain one at http://mozilla.org/MPL/2.0/
 *
 * Copyright (C) 2010-2013, Peter Johnson (www.delphidabbler.com).
 *
 * $Rev$
 * $Date$
 *
 * Provides various generic container classes and enumerators.
}


// TODO: Rename UContainers to something more appropriate to its reduced state

unit UContainers;


interface


uses
  // Delphi
  Generics.Defaults,
  Generics.Collections;


type
  ///  <summary>Generic enumerator for dynamic arrays.</summary>
  TArrayEnumerator<T> = class(TEnumerator<T>)
  strict private
    var
      ///  <summary>Array being enumerated.</summary>
      fArray: TArray<T>;
      ///  <summary>Index of current array element in enumeration.</summary>
      fIndex: Integer;
  strict protected
    ///  <summary>Gets current array element in enumeration.</summary>
    ///  <returns>T. Content of current array element.</returns>
    function DoGetCurrent: T; override;
    ///  <summary>Moves to next item in enumeration.</summary>
    ///  <returns>Boolean. True if there is a next item, False if at end of
    ///  enumeration.</returns>
    function DoMoveNext: Boolean; override;
  public
    ///  <summary>Creates enumerator for given dynamic array.</summary>
    ///  <param name="A">array of T [in] Array to be enumerated.</param>
    ///  <remarks>Constructor makes a shallow copy of the given array: value
    ///  type elements are copied but reference type elements are simply
    ///  referenced.</remarks>
    constructor Create(const A: array of T);
  end;

type
  ///  <summary>Container of functions that help in manipulating arrays.
  ///  </summary>
  TArrayHelper = record
  public
    ///  <summary>Creates and returns a new dynamic array that is an exact copy
    ///  of the given open array.</summary>
    class function Copy<T>(const A: array of T): TArray<T>; static;
    ///  <summary>Finds the index of the first array element in the given array
    ///  that has the given value, using the given comparer to check for
    ///  equality of values. Returns -1 if the value is not in the array.
    ///  </summary>
    class function IndexOf<T>(const A: array of T; const Value: T;
      const EqualsFn: TEqualityComparison<T>): Integer; static;
    ///  <summary>Checks if the value exists as an element of the given array,
    ///  using the given comparer to check for equality of values.</summary>
    class function Contains<T>(const A: array of T; const Value: T;
      const EqualsFn: TEqualityComparison<T>): Boolean; static;
  end;


implementation


{ TArrayEnumerator<T> }

constructor TArrayEnumerator<T>.Create(const A: array of T);
var
  Idx: Integer;
begin
  inherited Create;
  fArray := TArrayHelper.Copy<T>(A);
  fIndex := -1;
end;

function TArrayEnumerator<T>.DoGetCurrent: T;
begin
  Result := fArray[fIndex];
end;

function TArrayEnumerator<T>.DoMoveNext: Boolean;
begin
  if fIndex >= Length(fArray) then
    Exit(False);
  Inc(fIndex);
  Result := fIndex < Length(fArray);
end;

{ TArrayHelper }

class function TArrayHelper.Contains<T>(const A: array of T; const Value: T;
  const EqualsFn: TEqualityComparison<T>): Boolean;
begin
  Result := TArrayHelper.IndexOf<T>(A, Value, EqualsFn) >= 0;
end;

class function TArrayHelper.Copy<T>(const A: array of T): TArray<T>;
var
  Idx: Integer;
begin
  SetLength(Result, Length(A));
  for Idx := 0 to High(A) do
    Result[Idx] := A[Idx];
end;

class function TArrayHelper.IndexOf<T>(const A: array of T; const Value: T;
  const EqualsFn: TEqualityComparison<T>): Integer;
var
  Idx: Integer;
begin
  for Idx := 0 to High(A) do
    if EqualsFn(A[Idx], Value) then
      Exit(Idx);
  Result := -1;
end;

end.


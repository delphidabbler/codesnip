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
 * Contains comparer classes, hashes etc.
}


unit UComparers;


interface


uses
  // Delphi
  Generics.Defaults,
  // 3rd party
  Collections.Base;


type
  ///  <summary>Generic interface use to compare and check equality of two
  ///  values of the get the hash of a value.</summary>
  ///  <remarks>IComparator is a union of the methods of IComparer and
  ///  IEqualityComparer.</remarks>
  IComparator<T> = interface
    ///  <summary>Generic method to compare two values, Left and Right. Returns
    ///  -ve if Left is less than Right, 0 if the values are equal or +ve if
    ///  Left is greater than Right.</summary>
    function Compare(const Left, Right: T): Integer;
    ///  <summary>Generic method used to check the equality of two values, Left
    ///  and Right.</summary>
    function Equals(const Left, Right: T): Boolean;
    ///  <summary>Generic method used to generate a hash code for the given
    ///  value.</summary>
    function GetHashCode(const Value: T): Integer;
  end;

  ///  <summary>Abstract base class for all comparator classes that can
  ///  construct a TRules record that uses this class to provide comparison
  ///  rules for use with collections from the Delphi Collections library.
  ///  </summary>
  ///  <remarks>In order for classes to be used by TRules instances, the classes
  ///  must support IComparer and IEqualityComparer in addition to IComparator.
  ///  </remarks>
  TBaseComparator<T> = class abstract(TInterfacedObject,
    IComparator<T>, IComparer<T>, IEqualityComparer<T>)
  public
    ///  <summary>Creates a TRules wrapper around an instance of the comparator
    ///  that provides the IComparer and IEqualityComparer objects required by
    ///  collection classes in the Delphi Collections library.</summary>
    class function ConstructRules: TRules<T>; virtual;
    ///  <summary>Abstract generic method to compare two values, Left and Right.
    ///  Returns -ve if Left is less than Right, 0 if the values are equal or
    ///  +ve if Left is greater than Right.</summary>
    function Compare(const Left, Right: T): Integer;
      virtual; abstract;
    ///  <summary>Abstract generic method used to check the equality of two
    ///  values, Left and Right.</summary>
    function Equals(const Left, Right: T): Boolean;
      reintroduce; overload; virtual; abstract;
    ///  <summary>Abstract generic method used to generate a hash code for the
    ///  given value.</summary>
    function GetHashCode(const Value: T): Integer;
      reintroduce; overload; virtual; abstract;
  end;

  ///  <summary>Abstract base class for IComparator implementations and a
  ///  provider of default IComparator implementations.</summary>
  ///  <remarks>Since the IComparator is a union of the methods of IComparer and
  ///  IEqualityComparer, TComparator also supports those interfaces.</remarks>
  TComparator<T> = class abstract(TBaseComparator<T>,
    IComparator<T>, IComparer<T>, IEqualityComparer<T>)
  public
    ///  <summary>Returns an instance of TComparator for the required type.
    ///  </summary>
    ///  <remarks>RTTI is used to determine the type information and to select
    ///  the most appropriate implementation of TComparator.</remarks>
    class function Default: IComparator<T>;
    ///  <summary>Constructs a concrete instance of TComparator for the required
    ///  type.</summary>
    ///  <param name="ACompareFn">TComparison [in] Reference to a function that
    ///  will handle comparision requests.</param>
    ///  <param name="AEqualsFn">TEqualityComparison [in] Reference to a
    ///  function that will handle equality checks.</param>
    ///  <param name="AHasherFn">THasher [in] Reference to a function that will
    ///  handle hash generation.</param>
    ///  <returns>IComparer. Required concrete object instance.</returns>
    ///  <remarks>This method creates a new instance of TDelegatedComparator,
    ///  passing the user-supplied routines as parameters to the constructor of
    ///  TDelegatedEqualityComparer.</remarks>
    class function Construct(const ACompareFn: TComparison<T>;
      const AEqualsFn: TEqualityComparison<T>; const AHasherFn: THasher<T>):
      IComparer<T>; overload;
    ///  <summary>Constructs a concrete instance of TComparator for the required
    ///  type from the given IComparer and IEqualityComparer instances.
    ///  </summary>
    ///  <remarks>This method creates a new instance of TDelegatedComparator,
    ///  passing the user-supplied comparers as parameters to the constructor of
    ///  TDelegatedEqualityComparer.</remarks>
    class function Construct(AComparer: IComparer<T>;
      AEqualityComparer: IEqualityComparer<T>): IComparer<T>; overload;
  end;

  ///  <summary>Concrete implementation of TComparator that delegates all method
  ///  calls to user-provide callback functions or to user-provided instances of
  ///  TComparer and TEqualityComparer.</summary>
  ///  <remarks>Note TDelegatedComparator is used by TComparator to provide a
  ///  concrete implementation of TComparator as return from it Construct
  ///  methods.</remarks>
  TDelegatedComparator<T> = class(TComparator<T>)
  private
    var
      ///  <summary>Object that provides implementation of Compare method.
      ///  </summary>
      fComparer: IComparer<T>;
      ///  <summary>Object that provides implementation of Equals and
      ///  GetHashCode methods.</summary>
      fEqualityComparer: IEqualityComparer<T>;
  public
    ///  <summary>Constructs a new instance for the required type from given.
    ///  </summary>
    ///  <param name="ACompareFn">TComparison [in] Reference to a function that
    ///  will handle comparision requests.</param>
    ///  <param name="AEqualsFn">TEqualityComparison [in] Reference to a
    ///  function that will handle equality checks.</param>
    ///  <param name="AHasherFn">THasher [in] Reference to a function that will
    ///  handle hash generation.</param>
    constructor Create(const ACompareFn: TComparison<T>;
      const AEqualsFn: TEqualityComparison<T>; const AHasherFn: THasher<T>);
      overload;
    ///  <summary>Constructs a new instance of for the required type from the
    ///  given IComparer and IEqualityComparer instances.</summary>
    constructor Create(AComparer: IComparer<T>;
      AEqualityComparer: IEqualityComparer<T>); overload;
    ///  <summary>Generic method to compare two values, Left and Right. Returns
    ///  -ve if Left is less than Right, 0 if the values are equal or +ve if
    ///  Left is greater than Right.</summary>
    ///  <remarks>Method of IComparator and IComparer</remarks>
    function Compare(const Left, Right: T): Integer; override;
    ///  <summary>Generic method used to check the equality of two values, Left
    ///  and Right.</summary>
    ///  <remarks>Method of IComparator and IEqualityComparer.</remarks>
    function Equals(const Left, Right: T): Boolean; override;
    ///  <summary>Generic method used to generate a hash code for the given
    ///  value.</summary>
    ///  <remarks>Method of IComparator and IEqualityComparer.</remarks>
    function GetHashCode(const Value: T): Integer; override;
  end;

type
  ///  <summary>Case sensitive string comparator.</summary>
  TStringComparator = class(TBaseComparator<string>,
    IComparator<string>, IComparer<string>, IEqualityComparer<string>
  )
  public
    ///  <summary>Compares strings Left and Right, taking account of case.
    ///  Returns -ve if Left less than Right, 0 if equal or +ve if Left greater
    ///  than Right.</summary>
    ///  <remarks>Method of IComparator and IComparer.</remarks>
    function Compare(const Left, Right: string): Integer; override;
    ///  <summary>Checks if two strings, Left and Right, are equal, taking
    ///  account of case.</summary>
    ///  <remarks>Method of IComparator and IEqualityComparer.</remarks>
    function Equals(const Left, Right: string): Boolean; override;
    ///  <summary>Gets hash of given string, taking account of case.</summary>
    ///  <remarks>Method of IComparator and IEqualityComparer.</remarks>
    function GetHashCode(const Value: string): Integer; override;
  end;

type
  ///  <summary>Case insensitive string comparator.</summary>
  TTextComparator = class(TBaseComparator<string>,
    IComparator<string>, IComparer<string>, IEqualityComparer<string>
  )
  public
    ///  <summary>Compares strings Left and Right, ignoring case. Returns -ve if
    ///  Left less than Right, 0 if equal or +ve if Left greater than Right.
    ///  </summary>
    ///  <remarks>Method of IComparator and IComparer.</remarks>
    function Compare(const Left, Right: string): Integer; override;
    ///  <summary>Checks if two strings, Left and Right, are equal, ignoring
    ///  case.</summary>
    ///  <remarks>Method of IComparator and IEqualityComparer.</remarks>
    function Equals(const Left, Right: string): Boolean; override;
    ///  <summary>Gets hash of given string, ignoring case.</summary>
    ///  <remarks>
    ///  <para>To strings that contain the same text but differ in case will
    ///  always hash to the same value.</para>
    ///  <para>Method of IComparator and IEqualityComparer.</para>
    ///  </remarks>
    function GetHashCode(const Value: string): Integer; override;
  end;


implementation


uses
  // Project
  CS.Utils.Hashes,
  UStrUtils;


{ TBaseComparator<T> }

class function TBaseComparator<T>.ConstructRules: TRules<T>;
var
  Inst: TBaseComparator<T>;
begin
  Inst := Self.Create;
  Result := TRules<T>.Create(Inst, Inst);
end;

{ TComparator<T> }

class function TComparator<T>.Construct(const ACompareFn: TComparison<T>;
  const AEqualsFn: TEqualityComparison<T>; const AHasherFn: THasher<T>):
  IComparer<T>;
begin
  Result := TDelegatedComparator<T>.Create(ACompareFn, AEqualsFn, AHasherFn);
end;

class function TComparator<T>.Construct(AComparer: IComparer<T>;
  AEqualityComparer: IEqualityComparer<T>): IComparer<T>;
begin
  Result := TDelegatedComparator<T>.Create(AComparer, AEqualityComparer);
end;

class function TComparator<T>.Default: IComparator<T>;
var
  DefComparer: IComparer<T>;
  DefEqualityComparer: IEqualityComparer<T>;
begin
  DefComparer := TComparer<T>.Default;
  DefEqualityComparer := TEqualityComparer<T>.Default;
  Result := TDelegatedComparator<T>.Create(DefComparer, DefEqualityComparer);
end;

{ TDelegatedComparator<T> }

function TDelegatedComparator<T>.Compare(const Left, Right: T): Integer;
begin
  Result := fComparer.Compare(Left, Right);
end;

constructor TDelegatedComparator<T>.Create(const ACompareFn: TComparison<T>;
  const AEqualsFn: TEqualityComparison<T>; const AHasherFn: THasher<T>);
begin
  inherited Create;
  fComparer := TComparer<T>.Construct(ACompareFn);
  fEqualityComparer := TEqualityComparer<T>.Construct(AEqualsFn, AHasherFn);
end;

constructor TDelegatedComparator<T>.Create(AComparer: IComparer<T>;
  AEqualityComparer: IEqualityComparer<T>);
begin
  if Assigned(AComparer) then
    fComparer := AComparer
  else
    fComparer := TComparer<T>.Default;
  if Assigned(AEqualityComparer) then
    fEqualityComparer := AEqualityComparer
  else
    fEqualityComparer := TEqualityComparer<T>.Default;
end;

function TDelegatedComparator<T>.Equals(const Left, Right: T): Boolean;
begin
  Result := fEqualityComparer.Equals(Left, Right);
end;

function TDelegatedComparator<T>.GetHashCode(const Value: T): Integer;
begin
  Result := fEqualityComparer.GetHashCode(Value);
end;

{ TStringComparator }

function TStringComparator.Compare(const Left, Right: string): Integer;
begin
  Result := StrCompareStr(Left, Right);
end;

function TStringComparator.Equals(const Left, Right: string): Boolean;
begin
  Result := StrSameStr(Left, Right);
end;

function TStringComparator.GetHashCode(const Value: string): Integer;
begin
  Result := StrHash(Value);
end;

{ TTextComparator }

function TTextComparator.Compare(const Left, Right: string): Integer;
begin
  Result := StrCompareText(Left, Right);
end;

function TTextComparator.Equals(const Left, Right: string): Boolean;
begin
  Result := StrSameText(Left, Right);
end;

function TTextComparator.GetHashCode(const Value: string): Integer;
begin
  // In principle, two values that are considered equal should hash to the same
  // value. Using TextHash() ensures this for strings that test equal but vary
  // in case.
  Result := TextHash(Value);
end;

end.


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

  ///  <summary>Abstract base class for implementations of IComparator,
  ///  IComparer and IEqualityComparer that can be used with TRulesFactory to
  ///  create TRules records.</summary>
  ///  <remarks>Since the IComparator is a union of the methods of IComparer and
  ///  IEqualityComparer, TBaseComparator also supports those interfaces.
  ///  </remarks>
  TBaseComparator<T> = class abstract(TInterfacedObject,
    IComparator<T>, IComparer<T>, IEqualityComparer<T>)
  public
    ///  <summary>Abstract generic method to compare two values, Left and Right.
    ///  Returns -ve if Left is less than Right, 0 if the values are equal or
    ///  +ve if Left is greater than Right.</summary>
    ///  <remarks>Method of IComparator.</remarks>
    function Compare(const Left, Right: T): Integer;
      virtual; abstract;
    ///  <summary>Abstract generic method used to check the equality of two
    ///  values, Left and Right.</summary>
    ///  <remarks>Method of IComparator.</remarks>
    function Equals(const Left, Right: T): Boolean;
      reintroduce; overload; virtual; abstract;
    ///  <summary>Abstract generic method used to generate a hash code for the
    ///  given value.</summary>
    ///  <remarks>Method of IComparator.</remarks>
    function GetHashCode(const Value: T): Integer;
      reintroduce; overload; virtual; abstract;
  end;

  ///  <summary>Abstract base class for IComparator implementations that also
  ///  provide a both delegated and default IComparator implementations.
  ///  </summary>
  TComparator<T> = class abstract(TBaseComparator<T>)
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
    ///  TDelegatedComparator.</remarks>
    class function Construct(const ACompareFn: TComparison<T>;
      const AEqualsFn: TEqualityComparison<T>; const AHasherFn: THasher<T>):
      IComparator<T>; overload;
    ///  <summary>Constructs a concrete instance of TComparator for the required
    ///  type from the given IComparer and IEqualityComparer instances.
    ///  </summary>
    ///  <remarks>This method creates a new instance of TDelegatedComparator,
    ///  passing the user-supplied comparers as parameters to the constructor of
    ///  TDelegatedComparator.</remarks>
    class function Construct(AComparer: IComparer<T>;
      AEqualityComparer: IEqualityComparer<T>): IComparator<T>; overload;
  end;

  ///  <summary>Concrete implementation of TComparator that delegates all method
  ///  calls to user-provided callback functions or to user-provided instances
  ///  of TComparer and TEqualityComparer.</summary>
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
    ///  <summary>Constructs a new instance for the required type from the given
    ///  closures.</summary>
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
  TStringComparator = class(TBaseComparator<string>)
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
  TTextComparator = class(TBaseComparator<string>)
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

type
  ///  <summary>Collection of static methods that can create the TRules records
  ///  from closures or from comparator objects.</summary>
  ///  <remarks>TRules records are required by the constructors of collection
  ///  classes from the Delphi Collections library. This helper make it possible
  ///  to write less verbose code in the collection constructors.</remarks>
  TRulesFactory<T> = record
  public
    ///  <summary>Constructs a TRules record using the given closures.</summary>
    ///  <param name="ACompareFn">TComparison [in] Reference to a function that
    ///  will handle comparision requests.</param>
    ///  <param name="AEqualsFn">TEqualityComparison [in] Reference to a
    ///  function that will handle equality checks.</param>
    ///  <param name="AHasherFn">THasher [in] Reference to a function that will
    ///  handle hash generation.</param>
    ///  <returns>TRules. Required rules record.</returns>
    class function Construct(const ACompareFn: TComparison<T>;
      const AEqualsFn: TEqualityComparison<T>; const AHasherFn: THasher<T>):
      TRules<T>; overload; static;
    ///  <summary>Constructs a TRules record using the given closures.</summary>
    ///  <param name="ACompareFn">TComparison [in] Reference to a function that
    ///  will handle both comparision and equality requests.</param>
    ///  <param name="AHasherFn">THasher [in] Reference to a function that will
    ///  handle hash generation.</param>
    ///  <remarks>When ACompareFn is used in equality tests the assumption is
    ///  made that for two equal items ACompareFn will return zero and for two
    ///  unequal items the function will return non-zero.</remarks>
    ///  <returns>TRules. Required rules record.</returns>
    class function Construct(const ACompareFn: TComparison<T>;
      const AHasherFn: THasher<T>): TRules<T>; overload; static;
    ///  <summary>Constructs a TRules record from the given Comparator.
    ///  </summary>
    ///  <remarks>
    ///  <para>This method has to have a unique name within the class because
    ///  the compile can fail to find the corrected overload when an IComparator
    ///  object is created on the fly in the method call.</para>
    ///  <para>NOTE: Comparator in parameter list must be declared "const" or
    ///  else a memory leak can occur when Comparer is created on the fly in the
    ///  method call.</para>
    ///  </remarks>
    class function CreateFromComparator(const Comparator: IComparator<T>):
      TRules<T>; static;
  end;


implementation


uses
  // Project
  CS.Utils.Hashes,
  UStrUtils;


{ TComparator<T> }

class function TComparator<T>.Construct(const ACompareFn: TComparison<T>;
  const AEqualsFn: TEqualityComparison<T>; const AHasherFn: THasher<T>):
  IComparator<T>;
begin
  Result := TDelegatedComparator<T>.Create(ACompareFn, AEqualsFn, AHasherFn);
end;

class function TComparator<T>.Construct(AComparer: IComparer<T>;
  AEqualityComparer: IEqualityComparer<T>): IComparator<T>;
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

{ TRulesFactory<T> }

class function TRulesFactory<T>.Construct(const ACompareFn: TComparison<T>;
  const AEqualsFn: TEqualityComparison<T>; const AHasherFn: THasher<T>):
  TRules<T>;
var
  Comparator: TComparator<T>;
begin
  Comparator := TDelegatedComparator<T>.Create(
    ACompareFn, AEqualsFn, AHasherFn
  );
  Result := TRules<T>.Create(Comparator, Comparator);
end;

class function TRulesFactory<T>.Construct(const ACompareFn: TComparison<T>;
  const AHasherFn: THasher<T>): TRules<T>;
var
  Comparator: TComparator<T>;
begin
  Result := Construct(
    ACompareFn,
    function (const Left, Right: T): Boolean
    begin
      Result := ACompareFn(Left, Right) = 0;
    end,
    AHasherFn
  );
end;

class function TRulesFactory<T>.CreateFromComparator(
  const Comparator: IComparator<T>): TRules<T>;
begin
  Result := TRules<T>.Create(
    TComparer<T>.Construct(
      function (const Left, Right: T): Integer
      begin
        Result := Comparator.Compare(Left, Right);
      end
    ),
    TEqualityComparer<T>.Construct(
      function (const Left, Right: T): Boolean
      begin
        Result := Comparator.Equals(Left, Right);
      end,
      function (const Value: T): Integer
      begin
        Result := Comparator.GetHashCode(Value)
      end
    )
  );
end;

end.


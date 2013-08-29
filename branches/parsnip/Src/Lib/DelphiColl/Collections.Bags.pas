(*
* Copyright (c) 2009-2011, Ciobanu Alexandru
* All rights reserved.
*
* Redistribution and use in source and binary forms, with or without
* modification, are permitted provided that the following conditions are met:
*     * Redistributions of source code must retain the above copyright
*       notice, this list of conditions and the following disclaimer.
*     * Redistributions in binary form must reproduce the above copyright
*       notice, this list of conditions and the following disclaimer in the
*       documentation and/or other materials provided with the distribution.
*     * Neither the name of the <organization> nor the
*       names of its contributors may be used to endorse or promote products
*       derived from this software without specific prior written permission.
*
* THIS SOFTWARE IS PROVIDED BY THE AUTHOR ''AS IS'' AND ANY
* EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE IMPLIED
* WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE ARE
* DISCLAIMED. IN NO EVENT SHALL THE AUTHOR BE LIABLE FOR ANY
* DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES
* (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES;
* LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND
* ON ANY THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT
* (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE OF THIS
* SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.
*)

unit Collections.Bags;
interface
uses SysUtils,
     Generics.Defaults,
     Generics.Collections,
     Collections.Base,
     Collections.Dictionaries;

type
  ///  <summary>The base abstract class for all <c>bag collections</c> in this package.</summary>
  TAbstractBag<T> = class(TEnexCollection<T>, IBag<T>)
  private type
    {$REGION 'Internal Types'}
    { Enumerator }
    TEnumerator = class(TEnumerator<T>)
    private
      FVer: NativeInt;
      FDict: TAbstractBag<T>;
      FCurrentKV: IEnumerator<TPair<T, NativeUInt>>;
      FCurrentCount: NativeInt;
      FValue: T;

    public
      { Constructor }
      constructor Create(const ADict: TAbstractBag<T>);

      { Destructor }
      destructor Destroy(); override;

      function GetCurrent(): T; override;
      function MoveNext(): Boolean; override;
    end;
    {$ENDREGION}

  private var
    FDictionary: IDictionary<T, NativeUInt>;
    FVer: NativeInt;
    FKnownCount: NativeInt;

  protected
    ///  <summary>Specifies the internal dictionary used as back-end.</summary>
    ///  <returns>A dictionary of lists used as back-end.</summary>
    property Dictionary: IDictionary<T, NativeUInt> read FDictionary;

    ///  <summary>Returns the number of elements in the bag.</summary>
    ///  <returns>A positive value specifying the number of elements in the bag.</returns>
    ///  <remarks>The returned value is calculated by taking each key and multiplying it to its weight in
    ///  the bag. For example, an item that has a weight of <c>20</c> will increse the count with <c>20</c>.</remarks>
    function GetCount(): NativeInt; override;

    ///  <summary>Returns the weight of an element.</param>
    ///  <param name="AValue">The value to check.</param>
    ///  <returns>The weight of the value.</returns>
    ///  <remarks>If the value is not found in the bag, zero is returned.</remarks>
    function GetWeight(const AValue: T): NativeUInt;

    ///  <summary>Sets the weight of an element.</param>
    ///  <param name="AValue">The value to set the weight for.</param>
    ///  <param name="AWeight">The new weight.</param>
    ///  <remarks>If the value is not found in the bag, this method acts like an <c>Add</c> operation; otherwise
    ///  the weight of the stored item is adjusted.</remarks>
    procedure SetWeight(const AValue: T; const AWeight: NativeUInt);

    ///  <summary>Called when the map needs to initialize its internal dictionary.</summary>
    ///  <param name="ARules">The rule set describing the elements.</param>
    ///  <remarks>This method creates a hash-based dictionary used as the underlying back-end for the bag.</remarks>
    function CreateDictionary(const ARules: TRules<T>): IDictionary<T, NativeUInt>; virtual; abstract;
  public
    ///  <summary>Creates a new instance of this class.</summary>
    ///  <remarks>The default rule set is requested.</remarks>
    constructor Create(); overload;

    ///  <summary>Creates a new instance of this class.</summary>
    ///  <param name="ACollection">A collection to copy elements from.</param>
    ///  <exception cref="SysUtils|EArgumentNilException"><paramref name="ACollection"/> is <c>nil</c>.</exception>
    ///  <remarks>The default rule set is requested.</remarks>
    constructor Create(const ACollection: IEnumerable<T>); overload;

    ///  <summary>Creates a new instance of this class.</summary>
    ///  <param name="AArray">An array to copy elements from.</param>
    ///  <remarks>The default rule set is requested.</remarks>
    constructor Create(const AArray: array of T); overload;

    ///  <summary>Creates a new instance of this class.</summary>
    ///  <param name="ARules">A rule set describing the elements in the bag.</param>
    constructor Create(const ARules: TRules<T>); overload;

    ///  <summary>Creates a new instance of this class.</summary>
    ///  <param name="ARules">A rule set describing the elements in the bag.</param>
    ///  <param name="ACollection">A collection to copy elements from.</param>
    ///  <exception cref="SysUtils|EArgumentNilException"><paramref name="ACollection"/> is <c>nil</c>.</exception>
    constructor Create(const ARules: TRules<T>; const ACollection: IEnumerable<T>); overload;

    ///  <summary>Creates a new instance of this class.</summary>
    ///  <param name="ARules">A rule set describing the elements in the bag.</param>
    ///  <param name="AArray">An array to copy elements from.</param>
    constructor Create(const ARules: TRules<T>; const AArray: array of T); overload;

    ///  <summary>Destroys this instance.</summary>
    ///  <remarks>Do not call this method directly; call <c>Free</c> instead.</remarks>
    destructor Destroy(); override;

    ///  <summary>Clears the contents of the bag.</summary>
    procedure Clear();

    ///  <summary>Adds an element to the bag.</summary>
    ///  <param name="AValue">The element to add.</param>
    ///  <param name="AWeight">The weight of the element.</param>
    ///  <remarks>If the bag already contains the given value, its stored weight is incremented to by <paramref name="AWeight"/>.
    ///  If the value of <paramref name="AWeight"/> is zero, nothing happens.</remarks>
    procedure Add(const AValue: T; const AWeight: NativeUInt = 1);

    ///  <summary>Removes an element from the bag.</summary>
    ///  <param name="AValue">The value to remove.</param>
    ///  <param name="AWeight">The weight to remove.</param>
    ///  <remarks>This method decreses the weight of the stored item by <paramref name="AWeight"/>. If the resulting weight is less
    ///  than zero or zero, the element is removed from the bag. If <paramref name="AWeight"/> is zero, nothing happens.</remarks>
    procedure Remove(const AValue: T; const AWeight: NativeUInt = 1);

    ///  <summary>Removes an element from the bag.</summary>
    ///  <param name="AValue">The value to remove.</param>
    ///  <remarks>This method completely removes an item from the bag ignoring its stored weight. Nothing happens if the given value
    ///  is not in the bag to begin with.</remarks>
    procedure RemoveAll(const AValue: T);

    ///  <summary>Checks whether the bag contains an element with at least the required weight.</summary>
    ///  <param name="AValue">The value to check.</param>
    ///  <param name="AWeight">The smallest allowed weight.</param>
    ///  <returns><c>True</c> if the condition is met; <c>False</c> otherwise.</returns>
    ///  <remarks>This method checks whether the bag contains the given value and that the contained value has at least the
    ///  given weight.</remarks>
    function Contains(const AValue: T; const AWeight: NativeUInt = 1): Boolean;

    ///  <summary>Sets or gets the weight of an item in the bag.</summary>
    ///  <param name="AValue">The value.</param>
    ///  <remarks>If the value is not found in the bag, this method acts like an <c>Add</c> operation; otherwise
    ///  the weight of the stored item is adjusted.</remarks>
    property Weights[const AValue: T]: NativeUInt read GetWeight write SetWeight; default;

    ///  <summary>Returns the number of elements in the bag.</summary>
    ///  <returns>A positive value specifying the number of elements in the bag.</returns>
    ///  <remarks>The returned value is calculated by taking each key and multiplying it to its weight in
    ///  the bag. For example, an item that has a weight of <c>20</c> will increase the count with <c>20</c>.</remarks>
    property Count: NativeInt read FKnownCount;

    ///  <summary>Returns a new enumerator object used to enumerate this bag.</summary>
    ///  <remarks>This method is usually called by compiler-generated code. Its purpose is to create an enumerator
    ///  object that is used to actually traverse the bag.</remarks>
    ///  <returns>An enumerator object.</returns>
    function GetEnumerator(): IEnumerator<T>; override;

    ///  <summary>Copies the values stored in the bag to a given array.</summary>
    ///  <param name="AArray">An array where to copy the contents of the bag.</param>
    ///  <param name="AStartIndex">The index into the array at which the copying begins.</param>
    ///  <remarks>This method assumes that <paramref name="AArray"/> has enough space to hold the contents of the bag.</remarks>
    ///  <exception cref="SysUtils|EArgumentOutOfRangeException"><paramref name="AStartIndex"/> is out of bounds.</exception>
    ///  <exception cref="Collections.Base|EArgumentOutOfSpaceException">The array is not long enough.</exception>
    procedure CopyTo(var AArray: array of T; const AStartIndex: NativeInt); overload; override;

    ///  <summary>Checks whether the bag is empty.</summary>
    ///  <returns><c>True</c> if the bag is empty; <c>False</c> otherwise.</returns>
    ///  <remarks>This method is the recommended way of detecting if the bag is empty.</remarks>
    function Empty(): Boolean; override;

    ///  <summary>Returns the biggest element.</summary>
    ///  <returns>An element from the bag considered to have the biggest value.</returns>
    ///  <exception cref="Collections.Base|ECollectionEmptyException">The bag is empty.</exception>
    function Max(): T; override;

    ///  <summary>Returns the smallest element.</summary>
    ///  <returns>An element from the bag considered to have the smallest value.</returns>
    ///  <exception cref="Collections.Base|ECollectionEmptyException">The bag is empty.</exception>
    function Min(): T; override;

    ///  <summary>Returns the first element.</summary>
    ///  <returns>The first element in the bag.</returns>
    ///  <exception cref="Collections.Base|ECollectionEmptyException">The bag is empty.</exception>
    function First(): T; override;

    ///  <summary>Returns the first element or a default, if the bag is empty.</summary>
    ///  <param name="ADefault">The default value returned if the bag is empty.</param>
    ///  <returns>The first element in the bag if the bag is not empty; otherwise <paramref name="ADefault"/> is returned.</returns>
    function FirstOrDefault(const ADefault: T): T; override;

    ///  <summary>Returns the last element.</summary>
    ///  <returns>The last element in the bag.</returns>
    ///  <exception cref="Collections.Base|ECollectionEmptyException">The bag is empty.</exception>
    function Last(): T; override;

    ///  <summary>Returns the last element or a default, if the bag is empty.</summary>
    ///  <param name="ADefault">The default value returned if the bag is empty.</param>
    ///  <returns>The last element in the bag if the bag is not empty; otherwise <paramref name="ADefault"/> is returned.</returns>
    function LastOrDefault(const ADefault: T): T; override;

    ///  <summary>Returns the single element stored in the bag.</summary>
    ///  <returns>The element in the bag.</returns>
    ///  <remarks>This method checks whether the bag contains just one element, in which case it is returned.</remarks>
    ///  <exception cref="Collections.Base|ECollectionEmptyException">The bag is empty.</exception>
    ///  <exception cref="Collections.Base|ECollectionNotOneException">There is more than one element in the bag.</exception>
    function Single(): T; override;

    ///  <summary>Returns the single element stored in the bag, or a default value.</summary>
    ///  <param name="ADefault">The default value returned if there are less or more elements in the bag.</param>
    ///  <returns>The element in the bag if the condition is satisfied; <paramref name="ADefault"/> is returned otherwise.</returns>
    ///  <remarks>This method checks whether the bag contains just one element, in which case it is returned. Otherwise
    ///  the value in <paramref name="ADefault"/> is returned.</remarks>
    function SingleOrDefault(const ADefault: T): T; override;

    ///  <summary>Checks whether at least one element in the bag satisfies a given predicate.</summary>
    ///  <param name="APredicate">The predicate to check for each element.</param>
    ///  <returns><c>True</c> if at least one element satisfies a given predicate; <c>False</c> otherwise.</returns>
    ///  <remarks>This method traverses the whole bag and checks the value of the predicate for each element. This method
    ///  stops on the first element for which the predicate returns <c>True</c>. The logical equivalent of this operation is "OR".</remarks>
    ///  <exception cref="SysUtils|EArgumentNilException"><paramref name="APredicate"/> is <c>nil</c>.</exception>
    function Any(const APredicate: TFunc<T, Boolean>): Boolean; override;

    ///  <summary>Checks that all elements in the bag satisfy a given predicate.</summary>
    ///  <param name="APredicate">The predicate to check for each element.</param>
    ///  <returns><c>True</c> if all elements satisfy a given predicate; <c>False</c> otherwise.</returns>
    ///  <remarks>This method traverses the whole bag and checks the value of the predicate for each element. This method
    ///  stops on the first element for which the predicate returns <c>False</c>. The logical equivalent of this operation is "AND".</remarks>
    ///  <exception cref="SysUtils|EArgumentNilException"><paramref name="APredicate"/> is <c>nil</c>.</exception>
    function All(const APredicate: TFunc<T, Boolean>): Boolean; override;
  end;

type
  ///  <summary>The generic <c>bag</c> collection.</summary>
  ///  <remarks>This type uses hashing techniques to store its values.</remarks>
  TBag<T> = class(TAbstractBag<T>)
  private var
    FInitialCapacity: NativeInt;

  protected
    ///  <summary>Called when the bag needs to initialize its internal dictionary.</summary>
    ///  <param name="ARules">The rule set describing the bag's elements.</param>
    ///  <remarks>This method creates a hash-based dictionary used as the underlying back-end for the bag.</remarks>
    function CreateDictionary(const ARules: TRules<T>): IDictionary<T, NativeUInt>; override;
  public
    ///  <summary>Creates a new instance of this class.</summary>
    ///  <param name="AInitialCapacity">The bag's initial capacity.</param>
    ///  <remarks>The default rule set is requested.</remarks>
    constructor Create(const AInitialCapacity: NativeInt); overload;

    ///  <summary>Creates a new instance of this class.</summary>
    ///  <param name="ARules">The rule set describing the bag's elements.</param>
    ///  <param name="AInitialCapacity">The bag's initial capacity.</param>
    ///  <exception cref="SysUtils|EArgumentNilException"><paramref name="ARules"/> is <c>nil</c>.</exception>
    constructor Create(const ARules: TRules<T>; const AInitialCapacity: NativeInt); overload;
  end;

  ///  <summary>The generic <c>bag</c> collection designed to store objects.</summary>
  ///  <remarks>This type uses hashing techniques to store its objects.</remarks>
  TObjectBag<T: class> = class(TBag<T>)
  private
    FOwnsObjects: Boolean;

  protected
    ///  <summary>Frees the object that was removed from the collection.</summary>
    ///  <param name="AElement">The object that was removed from the collection.</param>
    procedure HandleElementRemoved(const AElement: T); override;

  public
    ///  <summary>Specifies whether this bag owns the objects stored in it.</summary>
    ///  <returns><c>True</c> if the bag owns its objects; <c>False</c> otherwise.</returns>
    ///  <remarks>This property controls the way the bag controls the life-time of the stored objects.</remarks>
    property OwnsObjects: Boolean read FOwnsObjects write FOwnsObjects;
  end;

type
  ///  <summary>The generic <c>bag</c> collection.</summary>
  ///  <remarks>This type uses an AVL tree to store its values.</remarks>
  TSortedBag<T> = class(TAbstractBag<T>)
  private var
    FAscSort: Boolean;

  protected
    ///  <summary>Called when the bag needs to initialize its internal dictionary.</summary>
    ///  <param name="ARules">The rule set describing the bag's elements.</param>
    ///  <remarks>This method creates an AVL-based dictionary used as the underlying back-end for the bag.</remarks>
    function CreateDictionary(const ARules: TRules<T>): IDictionary<T, NativeUInt>; override;
  public
    ///  <summary>Creates a new instance of this class.</summary>
    ///  <param name="AAscending">Specifies whether the elements are kept sorted in ascending order. The default is <c>True</c>.</param>
    ///  <remarks>The default rule set is requested.</remarks>
    constructor Create(const AAscending: Boolean = true); overload;

    ///  <summary>Creates a new instance of this class.</summary>
    ///  <param name="ACollection">A collection to copy elements from.</param>
    ///  <param name="AAscending">Specifies whether the elements are kept sorted in ascending order. The default is <c>True</c>.</param>
    ///  <exception cref="SysUtils|EArgumentNilException"><paramref name="ACollection"/> is <c>nil</c>.</exception>
    ///  <remarks>The default rule set is requested.</remarks>
    constructor Create(const ACollection: IEnumerable<T>; const AAscending: Boolean = true); overload;

    ///  <summary>Creates a new instance of this class.</summary>
    ///  <param name="AArray">An array to copy elements from.</param>
    ///  <param name="AAscending">Specifies whether the elements are kept sorted in ascending order. The default is <c>True</c>.</param>
    ///  <remarks>The default rule set is requested.</remarks>
    constructor Create(const AArray: array of T; const AAscending: Boolean = true); overload;

    ///  <summary>Creates a new instance of this class.</summary>
    ///  <param name="ARules">A rule set describing the elements in the bag.</param>
    ///  <param name="AAscending">Specifies whether the elements are kept sorted in ascending order. The default is <c>True</c>.</param>
    constructor Create(const ARules: TRules<T>; const AAscending: Boolean = true); overload;

    ///  <summary>Creates a new instance of this class.</summary>
    ///  <param name="ARules">A rule set describing the elements in the bag.</param>
    ///  <param name="ACollection">A collection to copy elements from.</param>
    ///  <param name="AAscending">Specifies whether the elements are kept sorted in ascending order. The default is <c>True</c>.</param>
    ///  <exception cref="SysUtils|EArgumentNilException"><paramref name="ACollection"/> is <c>nil</c>.</exception>
    constructor Create(const ARules: TRules<T>; const ACollection: IEnumerable<T>; const AAscending: Boolean = true); overload;

    ///  <summary>Creates a new instance of this class.</summary>
    ///  <param name="ARules">A rule set describing the elements in the bag.</param>
    ///  <param name="AArray">An array to copy elements from.</param>
    ///  <param name="AAscending">Specifies whether the elements are kept sorted in ascending order. The default is <c>True</c>.</param>
    constructor Create(const ARules: TRules<T>; const AArray: array of T; const AAscending: Boolean = true); overload;
  end;

  ///  <summary>The generic <c>bag</c> collection designed to store objects.</summary>
  ///  <remarks>This type uses an AVL tree to store its objects.</remarks>
  TObjectSortedBag<T: class> = class(TSortedBag<T>)
  private
    FOwnsObjects: Boolean;

  protected
    ///  <summary>Frees the object that was removed from the collection.</summary>
    ///  <param name="AElement">The object that was removed from the collection.</param>
    procedure HandleElementRemoved(const AElement: T); override;

  public
    ///  <summary>Specifies whether this bag owns the objects stored in it.</summary>
    ///  <returns><c>True</c> if the bag owns its objects; <c>False</c> otherwise.</returns>
    ///  <remarks>This property specifies the way the bag controls the life-time of the stored objects.</remarks>
    property OwnsObjects: Boolean read FOwnsObjects write FOwnsObjects;
  end;

implementation

{ TAbstractBag<T> }

procedure TAbstractBag<T>.Add(const AValue: T; const AWeight: NativeUInt);
var
  LOldCount: NativeUInt;
begin
  { Check count > 0 }
  if AWeight = 0 then
    Exit;

  { Add or update count }
  if FDictionary.TryGetValue(AValue, LOldCount) then
    FDictionary[AValue] := LOldCount + AWeight
  else
    FDictionary.Add(AValue, AWeight);

  Inc(FKnownCount, AWeight);
  Inc(FVer);
end;

function TAbstractBag<T>.All(const APredicate: TFunc<T, Boolean>): Boolean;
begin
  { Use TDictionary's Keys }
  Result := FDictionary.Keys.All(APredicate);
end;

function TAbstractBag<T>.Any(const APredicate: TFunc<T, Boolean>): Boolean;
begin
  { Use TDictionary's Keys }
  Result := FDictionary.Keys.Any(APredicate);
end;

procedure TAbstractBag<T>.Clear;
begin
  if Assigned(FDictionary) then
  begin
    { Simply clear the dictionary }
    FDictionary.Clear();

    FKnownCount := 0;
    Inc(FVer);
  end;
end;

function TAbstractBag<T>.Contains(const AValue: T; const AWeight: NativeUInt): Boolean;
var
  LInCount: NativeUInt;
begin
  { Check count > 0 }
  if AWeight = 0 then
    Exit(true);

  { Check the counts in the bag }
  Result := (FDictionary.TryGetValue(AValue, LInCount)) and (LInCount >= AWeight);
end;

procedure TAbstractBag<T>.CopyTo(var AArray: array of T; const AStartIndex: NativeInt);
var
  LTempArray: array of TPair<T, NativeUInt>;
  I, X, Y: NativeInt;
begin
  if (AStartIndex >= Length(AArray)) or (AStartIndex < 0) then
    ExceptionHelper.Throw_ArgumentOutOfRangeError('AStartIndex');

  { Check for indexes }
  if (Length(AArray) - AStartIndex) < Count then
    ExceptionHelper.Throw_ArgumentOutOfSpaceError('AArray');

  { Nothing to do? }
  if Count = 0 then
    Exit;

  { Initialize the temporary array }
  SetLength(LTempArray, FDictionary.Count);
  FDictionary.CopyTo(LTempArray);

  X := AStartIndex;

  { OK! Now let's simply copy }
  for I := 0 to Length(LTempArray) - 1 do
  begin
    { Copy one value for a number of counts }
    for Y := 0 to LTempArray[I].Value - 1 do
    begin
      AArray[X] := LTempArray[I].Key;
      Inc(X);
    end;
  end;
end;

constructor TAbstractBag<T>.Create();
begin
  { Call upper constructor }
  Create(TRules<T>.Default);
end;

constructor TAbstractBag<T>.Create(const ACollection: IEnumerable<T>);
begin
  { Call upper constructor }
  Create(TRules<T>.Default, ACollection);
end;

constructor TAbstractBag<T>.Create(const ARules: TRules<T>; const ACollection: IEnumerable<T>);
var
  LValue: T;
begin
  if not Assigned(ACollection) then
     ExceptionHelper.Throw_ArgumentNilError('ACollection');

  { Call upper constructor }
  Create(ARules);

  { Iterate and add }
  for LValue in ACollection do
    Add(LValue);
end;

constructor TAbstractBag<T>.Create(const ARules: TRules<T>; const AArray: array of T);
var
  I: NativeInt;
begin
  { Call upper constructor }
  Create(ARules);

  { Copy all items in }
  for I := 0 to Length(AArray) - 1 do
  begin
    Add(AArray[I]);
  end;
end;

constructor TAbstractBag<T>.Create(const ARules: TRules<T>);
begin
  inherited Create(ARules);

  FDictionary := CreateDictionary(ElementRules);
  FVer := 0;
  FKnownCount := 0;
end;

constructor TAbstractBag<T>.Create(const AArray: array of T);
begin
  { Call upper constructor }
  Create(TRules<T>.Default, AArray);
end;

destructor TAbstractBag<T>.Destroy;
begin
  { Clear the bag first }
  Clear();

  inherited;
end;

function TAbstractBag<T>.Empty: Boolean;
begin
  Result := (FKnownCount = 0);
end;

function TAbstractBag<T>.First: T;
begin
  { Use TDictionary's Keys }
  Result := FDictionary.Keys.First();
end;

function TAbstractBag<T>.FirstOrDefault(const ADefault: T): T;
begin
  { Use TDictionary's Keys }
  Result := FDictionary.Keys.FirstOrDefault(ADefault);
end;

function TAbstractBag<T>.Last: T;
begin
  { Use TDictionary's Keys }
  Result := FDictionary.Keys.Last();
end;

function TAbstractBag<T>.LastOrDefault(const ADefault: T): T;
begin
  { Use TDictionary's Keys }
  Result := FDictionary.Keys.LastOrDefault(ADefault);
end;

function TAbstractBag<T>.Max: T;
begin
  { Use TDictionary's Keys }
  Result := FDictionary.Keys.Max();
end;

function TAbstractBag<T>.Min: T;
begin
  { Use TDictionary's Keys }
  Result := FDictionary.Keys.Min();
end;

function TAbstractBag<T>.GetCount: NativeInt;
begin
  { Dictionary knows the real count }
  Result := FKnownCount;
end;

function TAbstractBag<T>.GetWeight(const AValue: T): NativeUInt;
begin
  { Get the count }
  if not FDictionary.TryGetValue(AValue, Result) then
    Result := 0;
end;

function TAbstractBag<T>.GetEnumerator: IEnumerator<T>;
begin
  Result := TEnumerator.Create(Self);
end;

procedure TAbstractBag<T>.Remove(const AValue: T; const AWeight: NativeUInt);
var
  LOldCount: NativeUInt;
begin
  { Check count > 0 }
  if AWeight = 0 then
    Exit;

  { Check that the key os present in the dictionary first }
  if not FDictionary.TryGetValue(AValue, LOldCount) then
    Exit;

  if LOldCount < AWeight then
    LOldCount := 0
  else
    LOldCount := LOldCount - AWeight;

  { Update the counts }
  if LOldCount = 0 then
    FDictionary.Remove(AValue)
  else
    FDictionary[AValue] := LOldCount;

  Dec(FKnownCount, AWeight);
  Inc(FVer);
end;

procedure TAbstractBag<T>.RemoveAll(const AValue: T);
var
  LOldCount: NativeUInt;
begin
  { Check that the key is present in the dictionary first }
  if not FDictionary.TryGetValue(AValue, LOldCount) then
    Exit;

  FDictionary.Remove(AValue);

  Dec(FKnownCount, LOldCount);
  Inc(FVer);
end;

procedure TAbstractBag<T>.SetWeight(const AValue: T; const AWeight: NativeUInt);
var
  LOldValue: NativeUInt;
begin
  { Check count > 0 }
  if Count = 0 then
    Exit;

  if FDictionary.ContainsKey(AValue) then
  begin
    LOldValue := FDictionary[AValue];
    FDictionary[AValue] := AWeight;
  end else
  begin
    LOldValue := 0;
    FDictionary.Add(AValue, AWeight);
  end;

  { Change the counts }
  FKnownCount := FKnownCount - NativeInt(LOldValue + AWeight);
  Inc(FVer);
end;

function TAbstractBag<T>.Single: T;
begin
  { Use TDictionary's Keys }
  Result := FDictionary.Keys.Single();
end;

function TAbstractBag<T>.SingleOrDefault(const ADefault: T): T;
begin
  { Use TDictionary's Keys }
  Result := FDictionary.Keys.SingleOrDefault(ADefault);
end;

{ TAbstractBag<T>.TEnumerator }

constructor TAbstractBag<T>.TEnumerator.Create(const ADict: TAbstractBag<T>);
begin
  { Initialize }
  FDict := ADict;
  KeepObjectAlive(FDict);

  FCurrentKV := FDict.FDictionary.GetEnumerator();

  FCurrentCount := 0;
  FValue := Default(T);

  FVer := ADict.FVer;
end;

destructor TAbstractBag<T>.TEnumerator.Destroy;
begin
  ReleaseObject(FDict);

  inherited;
end;

function TAbstractBag<T>.TEnumerator.GetCurrent: T;
begin
  if FVer <> FDict.FVer then
    ExceptionHelper.Throw_CollectionChangedError();

  Result := FValue;
end;

function TAbstractBag<T>.TEnumerator.MoveNext: Boolean;
begin
  { Repeat until something happens }
  while True do
  begin
    if FVer <> FDict.FVer then
      ExceptionHelper.Throw_CollectionChangedError();

    { We're still in the same KV? }
    if FCurrentCount <> 0 then
    begin
      { Decrease the count of the bag item }
      Dec(FCurrentCount);
      Result := true;

      Exit;
    end;

    { Get the next KV pair from the dictionary }
    Result := FCurrentKV.MoveNext();
    if not Result then
      Exit;

    { Copy the key/value }
    FCurrentCount := FCurrentKV.Current.Value;
    FValue := FCurrentKV.Current.Key;
  end;
end;

{ TBag<T> }

constructor TBag<T>.Create(const AInitialCapacity: NativeInt);
begin
  FInitialCapacity := AInitialCapacity;
  inherited Create();
end;

constructor TBag<T>.Create(const ARules: TRules<T>; const AInitialCapacity: NativeInt);
begin
  FInitialCapacity := AInitialCapacity;
  inherited Create(ARules);
end;

function TBag<T>.CreateDictionary(const ARules: TRules<T>): IDictionary<T, NativeUInt>;
var
  LNewCapacity: NativeInt;
  LDictionary: TDictionary<T, NativeUInt>;
begin
  { Create a simple dictionary }
  if FInitialCapacity <= 0 then
    LNewCapacity := CDefaultSize
  else
    LNewCapacity := FInitialCapacity;

  LDictionary := TDictionary<T, NativeUInt>.Create(ARules, TRules<NativeUInt>.Default, LNewCapacity);
  LDictionary.KeyRemoveNotification := NotifyElementRemoved;

  Result := LDictionary;
end;

{ TObjectBag<T> }

procedure TObjectBag<T>.HandleElementRemoved(const AElement: T);
begin
  if FOwnsObjects then
    TObject(AElement).Free;
end;

{ TSortedBag<T> }

function TSortedBag<T>.CreateDictionary(const ARules: TRules<T>): IDictionary<T, NativeUInt>;
var
  LDictionary: TSortedDictionary<T, NativeUInt>;
begin
  { Create a sorted dictionary }
  LDictionary := TSortedDictionary<T, NativeUInt>.Create(ARules, TRules<NativeUInt>.Default, FAscSort);
  LDictionary.KeyRemoveNotification := NotifyElementRemoved;

  Result := LDictionary;
end;

constructor TSortedBag<T>.Create(const AAscending: Boolean);
begin
  { Call upper constructor }
  FAscSort := AAscending;
  inherited Create();
end;

constructor TSortedBag<T>.Create(const ACollection: IEnumerable<T>; const AAscending: Boolean);
begin
  { Call upper constructor }
  FAscSort := AAscending;
  inherited Create(ACollection);
end;

constructor TSortedBag<T>.Create(const ARules: TRules<T>; const ACollection: IEnumerable<T>; const AAscending: Boolean);
begin
  { Call upper constructor }
  FAscSort := AAscending;
  inherited Create(ARules, ACollection);
end;

constructor TSortedBag<T>.Create(const ARules: TRules<T>; const AArray: array of T; const AAscending: Boolean);
begin
  { Call upper constructor }
  FAscSort := AAscending;
  inherited Create(ARules, AArray);
end;

constructor TSortedBag<T>.Create(const ARules: TRules<T>; const AAscending: Boolean);
begin
  { Call upper constructor }
  FAscSort := AAscending;
  inherited Create(ARules);
end;

constructor TSortedBag<T>.Create(const AArray: array of T; const AAscending: Boolean);
begin
  { Call upper constructor }
  FAscSort := AAscending;
  inherited Create(AArray);
end;

{ TObjectSortedBag<T> }

procedure TObjectSortedBag<T>.HandleElementRemoved(const AElement: T);
begin
  if FOwnsObjects then
    TObject(AElement).Free;
end;

end.


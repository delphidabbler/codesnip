(*
* Copyright (c) 2008-2011, Ciobanu Alexandru
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

unit Collections.Base;
interface
uses
  SysUtils,
  Rtti,
  Collections.Dynamic,
  Generics.Collections,
  Generics.Defaults;

{$REGION 'Base Collection Interfaces'}
type
  ///  <summary>Base interface describing all enumerators in this package.</summary>
  ///  <remarks><see cref="Collections.Base|IEnumerator&lt;T&gt;">Collections.Base.IEnumerator&lt;T&gt;</see> is implemented by
  ///  all enumerator objects in this package.</remarks>
  IEnumerator<T> = interface
    ///  <summary>Returns the current element of the enumerated collection.</summary>
    ///  <remarks><see cref="Collections.Base|IEnumerator&lt;T&gt;.GetCurrent">Collections.Base.IEnumerator&lt;T&gt;.GetCurrent</see> is the
    ///  getter method for the <see cref="Collections.Base|IEnumerator&lt;T&gt;.Current">Collections.Base.IEnumerator&lt;T&gt;.Current</see>
    ///  property. Use the property to obtain the element instead.</remarks>
    ///  <returns>The current element of the enumerated collection.</returns>
    function GetCurrent(): T;

    ///  <summary>Moves the enumerator to the next element of the collection.</summary>
    ///  <remarks><see cref="Collections.Base|IEnumerator&lt;T&gt;.MoveNext">Collections.Base.IEnumerator&lt;T&gt;.MoveNext</see> is usually
    ///  called by compiler-generated code. Its purpose is to move the "pointer" to the next element in the collection
    ///  (if there are elements left). Also note that many enumerator implementations may throw various exceptions if the
    ///  enumerated collections were changed in the meantime.</remarks>
    ///  <returns><c>True</c> if the enumerator successfully selected the next element; <c>False</c> if there are
    ///  no more elements to be enumerated.</returns>
    function MoveNext(): Boolean;

    ///  <summary>Returns the current element of the traversed collection.</summary>
    ///  <remarks><see cref="Collections.Base|IEnumerator&lt;T&gt;.Current">Collections.Base.IEnumerator&lt;T&gt;.Current</see> can only return a
    ///  valid element if <see cref="Collections.Base|IEnumerator&lt;T&gt;.MoveNext">Collections.Base.IEnumerator&lt;T&gt;.MoveNext</see> was
    ///  priorly called and returned <c>True</c>; otherwise the behavior of this property is undefined. Note that many enumerator implementations
    ///  may throw exceptions if the collection was changed in the meantime.
    ///  </remarks>
    ///  <returns>The current element of the enumerator collection.</returns>
    property Current: T read GetCurrent;
  end;

  ///  <summary>Base interface describing all enumerable collections in this package.</summary>
  ///  <remarks><see cref="Collections.Base|IEnumerable&lt;T&gt;">Collections.Base.IEnumerable&lt;T&gt;</see> is implemented by all
  ///  enumerable collections in this package.</remarks>
  IEnumerable<T> = interface
    ///  <summary>Returns a <see cref="Collections.Base|IEnumerator&lt;T&gt;">Collections.Base.IEnumerator&lt;T&gt;</see> interface that is used
    ///  to enumerate the collection.</summary>
    ///  <remarks><see cref="Collections.Base|IEnumerable&lt;T&gt;.MoveNext">Collections.Base.IEnumerable&lt;T&gt;.MoveNext</see> is usually
    ///  called by compiler-generated code. Its purpose is to create an enumerator object that is used to actually traverse
    ///  the collections.
    ///  Note that many collections generate enumerators that depend on the state of the collection. If the collection is changed
    ///  after the <see cref="Collections.Base|IEnumerator&lt;T&gt;">Collections.Base.IEnumerator&lt;T&gt;</see> had been obtained,
    ///  <see cref="Collections.Base|ECollectionChangedException">Collections.Base.ECollectionChangedException</see> is thrown.</remarks>
    ///  <returns>The <see cref="Collections.Base|IEnumerator&lt;T&gt;">Collections.Base.IEnumerator&lt;T&gt;</see> interface.</returns>
    function GetEnumerator(): IEnumerator<T>;
  end;

  ///  <summary>A special record designed to hold both a comparer and an equality
  ///  comparer. All collections require this type in order to function properly.</summary>
  ///  <remarks>The collection provided in this package provides extended functionality (Enex), which
  ///  implies comparing values in many circumstances, which requires the presence of the comparer.
  ///  Some collections need an additional equality comparer. This type is meant to provide both
  ///  on the need basis.</remarks>
  TRules<T> = record
  private
    FComparer: IComparer<T>;
    FEqComparer: IEqualityComparer<T>;

  public
    ///  <summary>Initializes a rule set with the given comparers.</summary>
    ///  <param name="AComparer">The comparer.</param>
    ///  <param name="AEqualityComparer">The equality comparer.</param>
    ///  <returns>A rule set initialized with the provided comparers.</returns>
    ///  <exception cref="SysUtils|EArgumentNilException"> if <paramref name="AComparer"/> is <c>nil</c>.</exception>
    ///  <exception cref="SysUtils|EArgumentNilException"> if <paramref name="AEqualityComparer"/> is <c>nil</c>.</exception>
    class function Create(const AComparer: IComparer<T>; const AEqualityComparer: IEqualityComparer<T>): TRules<T>; static;

    ///  <summary>Initializes a rule set with a given custom comparer.</summary>
    ///  <param name="AComparer">The custom comparer.</param>
    ///  <returns>A rule set initialized with the custom comparer.</returns>
    ///  <exception cref="SysUtils|EArgumentNilException"> if <paramref name="AComparer"/> is <c>nil</c>.</exception>
    class function Custom(const AComparer: TCustomComparer<T>): TRules<T>; static;

    ///  <summary>Initializes a rule set using default comparers.</summary>
    ///  <returns>A rule set initialized with the default comparers.</returns>
    class function Default: TRules<T>; static;
  end;

  ///  <summary>Base interface inherited by all specific collection interfaces.</summary>
  ///  <remarks>This interface defines a set of traits common to all collections implemented in this package.</remarks>
  ICollection<T> = interface(IEnumerable<T>)
    ///  <summary>Returns the number of elements in the collection.</summary>
    ///  <returns>A positive value specifying the number of elements in the collection.</returns>
    ///  <remarks>For associative collections such as dictionaries or multimaps, this value represents the
    ///  number of key-value pairs stored in the collection. A call to this method can be costly because some
    ///  collections cannot detect the number of stored elements directly, resorting to enumerating themselves.</remarks>
    function GetCount(): NativeInt;

    ///  <summary>Checks whether the collection is empty.</summary>
    ///  <returns><c>True</c> if the collection is empty; <c>False</c> otherwise.</returns>
    ///  <remarks>This method is the recommended way of detecting if the collection is empty. It is optimized
    ///  in most collections to offer a fast response.</remarks>
    function Empty(): Boolean;

    ///  <summary>Returns the single element stored in the collection.</summary>
    ///  <returns>The element in the collection.</returns>
    ///  <remarks>This method checks whether the collection contains just one element, in which case it is returned.</remarks>
    ///  <exception cref="Collections.Base|ECollectionEmptyException">The collection is empty.</exception>
    ///  <exception cref="Collections.Base|ECollectionNotOneException">There is more than one element in the collection.</exception>
    function Single(): T;

    ///  <summary>Returns the single element stored in the collection, or a default value.</summary>
    ///  <param name="ADefault">The default value returned if there is less or more elements in the collection.</param>
    ///  <returns>The element in the collection if the condition is satisfied; <paramref name="ADefault"/> is returned otherwise.</returns>
    ///  <remarks>This method checks whether the collection contains just one element, in which case it is returned. Otherwise
    ///  the value in <paramref name="ADefault"/> is returned.</remarks>
    function SingleOrDefault(const ADefault: T): T;

    ///  <summary>Copies the values stored in the collection to a given array.</summary>
    ///  <param name="AArray">An array where to copy the contents of the collection.</param>
    ///  <remarks>This method assumes that <paramref name="AArray"/> has enough space to hold the contents of the collection.</remarks>
    ///  <exception cref="Collections.Base|EArgumentOutOfSpaceException">The array is not long enough.</exception>
    procedure CopyTo(var AArray: array of T); overload;

    ///  <summary>Copies the values stored in the collection to a given array.</summary>
    ///  <param name="AArray">An array where to copy the contents of the collection.</param>
    ///  <param name="AStartIndex">The index into the array at which the copying begins.</param>
    ///  <remarks>This method assumes that <paramref name="AArray"/> has enough space to hold the contents of the collection.</remarks>
    ///  <exception cref="SysUtils|EArgumentOutOfRangeException"><paramref name="AStartIndex"/> is out of bounds.</exception>
    ///  <exception cref="Collections.Base|EArgumentOutOfSpaceException">The array is not long enough.</exception>
    procedure CopyTo(var AArray: array of T; const AStartIndex: NativeInt); overload;

    ///  <summary>Creates a new Delphi array with the contents of the collection.</summary>
    ///  <remarks>The length of the new array is equal to the value of the <c>Count</c> property.</remarks>
    function ToArray(): TArray<T>;

    ///  <summary>Specifies the number of elements in the collection.</summary>
    ///  <returns>A positive value specifying the number of elements in the collection.</returns>
    ///  <remarks>For associative collections such as dictionaries or multimaps, this value represents the
    ///  number of key-value pairs stored in the collection. Accesing this property can be costly because some
    ///  collections cannot detect the number of stored elements directly, resorting to enumerating themselves.</remarks>
    property Count: NativeInt read GetCount;
  end;

  { Pre-declarations }
  IList<T> = interface;
  ISet<T> = interface;
  IDictionary<TKey, TValue> = interface;
  IEnexCollection<T> = interface;
  IEnexGroupingCollection<TKey, T> = interface;

  ///  <summary>Offers an extended set of Enex operations.</summary>
  ///  <remarks>This type is exposed by Enex collections, and serves simply as a bridge between the interfaces
  ///  and some advanced operations that require parameterized methods. For example, expressions such as
  ///  <c>List.Op.Select&lt;Integer&gt;</c> are based on this type.</remarks>
  TEnexExtOps<T> = record
  private
    FRules: TRules<T>;
    FInstance: Pointer;
    FKeepAlive: IInterface;

  public
    ///  <summary>Represents a "select" operation.</summary>
    ///  <param name="ASelector">A selector method invoked for each element in the collection.</param>
    ///  <param name="ARules">A rule set representing the elements in the output collection.</param>
    ///  <returns>A new collection containing the selected values.</returns>
    ///  <remarks>This method is used when it is required to select values related to the ones in the operated collection.
    ///  For example, you can select a collection of integers where each integer is a field of a class in the original collection.</remarks>
    ///  <exception cref="SysUtils|EArgumentNilException"><paramref name="ASelector"/> is <c>nil</c>.</exception>
    ///  <exception cref="SysUtils|EArgumentNilException"><paramref name="ARules"/> is <c>nil</c>.</exception>
    function Select<TOut>(const ASelector: TFunc<T, TOut>; const ARules: TRules<TOut>): IEnexCollection<TOut>; overload;

    ///  <summary>Represents a "select" operation.</summary>
    ///  <param name="ASelector">A selector method invoked for each element in the collection.</param>
    ///  <returns>A new collection containing the selected values.</returns>
    ///  <remarks>This method is used when it is required to select values related to the ones in the operated collection.
    ///  For example, you can select a collection of integers where each integer is a field of a class in the original collection.</remarks>
    ///  <exception cref="SysUtils|EArgumentNilException"><paramref name="ASelector"/> is <c>nil</c>.</exception>
    function Select<TOut>(const ASelector: TFunc<T, TOut>): IEnexCollection<TOut>; overload;

    ///  <summary>Represents a "select" operation.</summary>
    ///  <param name="AMemberName">A record or class field/property name that will be selected.</param>
    ///  <returns>A new collection containing the selected values.</returns>
    ///  <remarks>This method will only work for classes and record types!</remarks>
    ///  <exception cref="Generics.Collections|ENotSupportedException"><paramref name="AMemberName"/> is not a real member of record or class.</exception>
    ///  <exception cref="Generics.Collections|ENotSupportedException">The collection's elements are not objects ore records.</exception>
    function Select<TOut>(const AMemberName: string): IEnexCollection<TOut>; overload;

    ///  <summary>Represents a "select" operation.</summary>
    ///  <param name="AMemberName">A record or class field/property name that will be selected.</param>
    ///  <returns>A new collection containing the selected values represented as Rtti <c>TValue</c>s.</returns>
    ///  <remarks>This method will only work for classes and record types!</remarks>
    ///  <exception cref="Generics.Collections|ENotSupportedException"><paramref name="AMemberName"/> is not a real member of record or class.</exception>
    ///  <exception cref="Generics.Collections|ENotSupportedException">The collection's elements are not objects ore records.</exception>
    function Select(const AMemberName: string): IEnexCollection<TAny>; overload;

    ///  <summary>Represents a "select" operation.</summary>
    ///  <param name="AMemberNames">A record or class field/property names that will be selected.</param>
    ///  <returns>A new collection containing the selected values represented as a view.</returns>
    ///  <remarks>This method will only work for classes and record types! The resulting view contains the selected members.</remarks>
    ///  <exception cref="Generics.Collections|ENotSupportedException"><paramref name="AMemberName"/> is not a real member of record or class.</exception>
    ///  <exception cref="Generics.Collections|ENotSupportedException">The collection's elements are not objects ore records.</exception>
    function Select(const AMemberNames: array of string): IEnexCollection<TView>; overload;

    ///  <summary>Represents a "where, select object" operation.</summary>
    ///  <returns>A new collection containing the selected values.</returns>
    ///  <remarks>This method can be used on a collection containing objects. The operation involves two steps,
    ///  where and select. First, each object is checked to be derived from <c>TOut</c>. If that is true, it is then
    ///  cast to <c>TOut</c>. The result of the operation is a new collection that contains only the objects of a given
    ///  class. For example, <c>AList.Op.Select&lt;TMyObject&gt;</c> results in a new collection that only contains
    ///  "TMyObject" instances.</remarks>
    ///  <exception cref="Generics.Collections|ENotSupportedException">The collection's elements are not objects.</exception>
    function Select<TOut: class>(): IEnexCollection<TOut>; overload;

    ///  <summary>Groups all elements in the collection by a given key.</summary>
    ///  <param name="ASelector">The selector function. Returns the key (based on each collection element) that serves for grouping purposes.</param>
    ///  <returns>A collection of grouping collections.</returns>
    ///  <exception cref="SysUtils|EArgumentNilException"><paramref name="ASelector"/> is <c>nil</c>.</exception>
    ///  <remarks>This operation will call <paramref name="ASelector"/> for each element in the collection and retrieve a "key". Using this key,
    ///  the elements are grouped into new collections called groupings. The result of this operation is a collection of groupings. Each grouping
    ///  contains the elements from the original collection that have the same group and a key (which is the group value used).</remarks>
    function GroupBy<TKey>(const ASelector: TFunc<T, TKey>): IEnexCollection<IEnexGroupingCollection<TKey, T>>; overload;
  end;

  ///  <summary>Base Enex (Extended enumerable) interface inherited by all specific collection interfaces.</summary>
  ///  <remarks>This interface defines a set of traits common to all collections implemented in this package. It also introduces
  ///  a large set of extended operations that can be performed on any collection that supports enumerability.</remarks>
  IEnexCollection<T> = interface(ICollection<T>)
    ///  <summary>Checks whether the elements in this collection are equal to the elements in another collection.</summary>
    ///  <param name="ACollection">The collection to compare to.</param>
    ///  <returns><c>True</c> if the collections are equal; <c>False</c> if the collections are different.</returns>
    ///  <remarks>This method checks that each element at position X in this collection is equal to an element at position X in
    ///  the provided collection. If the number of elements in both collections is different, then the collections are considered different.
    ///  Note that comparison of element is done using the rule set used by this collection. This means that comparing this collection
    ///  to another one might yield a different result than comparing the other collection to this one.</remarks>
    ///  <exception cref="SysUtils|EArgumentNilException"><paramref name="ACollection"/> is <c>nil</c>.</exception>
    function EqualsTo(const ACollection: IEnumerable<T>): Boolean;

    ///  <summary>Creates a new list containing the elements of this collection.</summary>
    ///  <returns>A list containing the elements copied from this collection.</returns>
    ///  <remarks>This method also copies the rule set of this collection. Be careful if the rule set
    ///  performs cleanup on the elements.</remarks>
    function ToList(): IList<T>;

    ///  <summary>Creates a new set containing the elements of this collection.</summary>
    ///  <returns>A set containing the elements copied from this collection.</returns>
    ///  <remarks>This method also copies the rule set of this collection. Be careful if the rule set
    ///  performs cleanup on the elements.</remarks>
    function ToSet(): ISet<T>;

    ///  <summary>Returns the biggest element.</summary>
    ///  <returns>An element from the collection considered to have the biggest value.</returns>
    ///  <exception cref="Collections.Base|ECollectionEmptyException">The collection is empty.</exception>
    function Max(): T;

    ///  <summary>Returns the smallest element.</summary>
    ///  <returns>An element from the collection considered to have the smallest value.</returns>
    ///  <exception cref="Collections.Base|ECollectionEmptyException">The collection is empty.</exception>
    function Min(): T;

    ///  <summary>Returns the first element.</summary>
    ///  <returns>The first element in the collection.</returns>
    ///  <exception cref="Collections.Base|ECollectionEmptyException">The collection is empty.</exception>
    function First(): T;

    ///  <summary>Returns the first element or a default if the collection is empty.</summary>
    ///  <param name="ADefault">The default value returned if the collection is empty.</param>
    ///  <returns>The first element in the collection if the collection is not empty; otherwise <paramref name="ADefault"/> is returned.</returns>
    function FirstOrDefault(const ADefault: T): T;

    ///  <summary>Returns the first element that satisfies the given predicate.</summary>
    ///  <param name="APredicate">The predicate to use.</param>
    ///  <returns>The first element that satisfies the given predicate.</returns>
    ///  <exception cref="Collections.Base|ECollectionEmptyException">The collection is empty.</exception>
    ///  <exception cref="Collections.Base|ECollectionFilteredEmptyException">No elements satisfy the predicate.</exception>
    ///  <exception cref="SysUtils|EArgumentNilException"><paramref name="APredicate"/> is <c>nil</c>.</exception>
    function FirstWhere(const APredicate: TFunc<T, Boolean>): T;

    ///  <summary>Returns the first element that satisfies the given predicate or a default value.</summary>
    ///  <param name="APredicate">The predicate to use.</param>
    ///  <param name="ADefault">The default value.</param>
    ///  <returns>The first element that satisfies the given predicate; <paramref name="ADefault"/> otherwise.</returns>
    ///  <exception cref="SysUtils|EArgumentNilException"><paramref name="APredicate"/> is <c>nil</c>.</exception>
    function FirstWhereOrDefault(const APredicate: TFunc<T, Boolean>; const ADefault: T): T;

    ///  <summary>Returns the first element that does not satisfy the given predicate.</summary>
    ///  <param name="APredicate">The predicate to use.</param>
    ///  <returns>The first element that does not satisfy the given predicate.</returns>
    ///  <exception cref="Collections.Base|ECollectionEmptyException">The collection is empty.</exception>
    ///  <exception cref="Collections.Base|ECollectionFilteredEmptyException">No elements that do not satisfy the predicate.</exception>
    ///  <exception cref="SysUtils|EArgumentNilException"><paramref name="APredicate"/> is <c>nil</c>.</exception>
    function FirstWhereNot(const APredicate: TFunc<T, Boolean>): T;

    ///  <summary>Returns the first element that does not satisfy the given predicate or a default value.</summary>
    ///  <param name="APredicate">The predicate to use.</param>
    ///  <param name="ADefault">The default value.</param>
    ///  <returns>The first element that does not satisfy the given predicate; <paramref name="ADefault"/> otherwise.</returns>
    ///  <exception cref="SysUtils|EArgumentNilException"><paramref name="APredicate"/> is <c>nil</c>.</exception>
    function FirstWhereNotOrDefault(const APredicate: TFunc<T, Boolean>; const ADefault: T): T;

    ///  <summary>Returns the first element lower than a given value.</summary>
    ///  <param name="ABound">The value to compare against.</param>
    ///  <returns>The first element that satisfies the given condition.</returns>
    ///  <exception cref="Collections.Base|ECollectionEmptyException">The collection is empty.</exception>
    ///  <exception cref="Collections.Base|ECollectionFilteredEmptyException">No elements satisfy the condition.</exception>
    function FirstWhereLower(const ABound: T): T;

    ///  <summary>Returns the first element lower than a given value or a default.</summary>
    ///  <param name="ABound">The value to compare against.</param>
    ///  <param name="ADefault">The default value.</param>
    ///  <returns>The first element that satisfies the given condition; <paramref name="ADefault"/> otherwise.</returns>
    ///  <exception cref="Collections.Base|ECollectionEmptyException">The collection is empty.</exception>
    ///  <exception cref="Collections.Base|ECollectionFilteredEmptyException">No elements satisfy the condition.</exception>
    function FirstWhereLowerOrDefault(const ABound: T; const ADefault: T): T;

    ///  <summary>Returns the first element lower than or equal to a given value.</summary>
    ///  <param name="ABound">The value to compare against.</param>
    ///  <returns>The first element that satisfies the given condition.</returns>
    ///  <exception cref="Collections.Base|ECollectionEmptyException">The collection is empty.</exception>
    ///  <exception cref="Collections.Base|ECollectionFilteredEmptyException">No elements satisfy the condition.</exception>
    function FirstWhereLowerOrEqual(const ABound: T): T;

    ///  <summary>Returns the first element lower than or equal to a given value or a default.</summary>
    ///  <param name="ABound">The value to compare against.</param>
    ///  <param name="ADefault">The default value.</param>
    ///  <returns>The first element that satisfies the given condition; <paramref name="ADefault"/> otherwise.</returns>
    ///  <exception cref="Collections.Base|ECollectionEmptyException">The collection is empty.</exception>
    ///  <exception cref="Collections.Base|ECollectionFilteredEmptyException">No elements satisfy the condition.</exception>
    function FirstWhereLowerOrEqualOrDefault(const ABound: T; const ADefault: T): T;

    ///  <summary>Returns the first element greater than a given value.</summary>
    ///  <param name="ABound">The value to compare against.</param>
    ///  <returns>The first element that satisfies the given condition.</returns>
    ///  <exception cref="Collections.Base|ECollectionEmptyException">The collection is empty.</exception>
    ///  <exception cref="Collections.Base|ECollectionFilteredEmptyException">No elements satisfy the condition.</exception>
    function FirstWhereGreater(const ABound: T): T;

    ///  <summary>Returns the first element greater than a given value or a default.</summary>
    ///  <param name="ABound">The value to compare against.</param>
    ///  <param name="ADefault">The default value.</param>
    ///  <returns>The first element that satisfies the given condition; <paramref name="ADefault"/> otherwise.</returns>
    ///  <exception cref="Collections.Base|ECollectionEmptyException">The collection is empty.</exception>
    ///  <exception cref="Collections.Base|ECollectionFilteredEmptyException">No elements satisfy the condition.</exception>
    function FirstWhereGreaterOrDefault(const ABound: T; const ADefault: T): T;

    ///  <summary>Returns the first element greater than or equal to a given value.</summary>
    ///  <param name="ABound">The value to compare against.</param>
    ///  <returns>The first element that satisfies the given condition.</returns>
    ///  <exception cref="Collections.Base|ECollectionEmptyException">The collection is empty.</exception>
    ///  <exception cref="Collections.Base|ECollectionFilteredEmptyException">No elements satisfy the condition.</exception>
    function FirstWhereGreaterOrEqual(const ABound: T): T;

    ///  <summary>Returns the first element greater than or equal to a given value or a default.</summary>
    ///  <param name="ABound">The value to compare against.</param>
    ///  <param name="ADefault">The default value.</param>
    ///  <returns>The first element that satisfies the given condition; <paramref name="ADefault"/> otherwise.</returns>
    ///  <exception cref="Collections.Base|ECollectionEmptyException">The collection is empty.</exception>
    ///  <exception cref="Collections.Base|ECollectionFilteredEmptyException">No elements satisfy the condition.</exception>
    function FirstWhereGreaterOrEqualOrDefault(const ABound: T; const ADefault: T): T;

    ///  <summary>Returns the first element situated within the given bounds.</summary>
    ///  <param name="ALower">The lower bound.</param>
    ///  <param name="AHigher">The higher bound.</param>
    ///  <returns>The first element that satisfies the given condition.</returns>
    ///  <exception cref="Collections.Base|ECollectionEmptyException">The collection is empty.</exception>
    ///  <exception cref="Collections.Base|ECollectionFilteredEmptyException">No elements satisfy the condition.</exception>
    function FirstWhereBetween(const ALower, AHigher: T): T;

    ///  <summary>Returns the first element situated within the given bounds or a default.</summary>
    ///  <param name="ALower">The lower bound.</param>
    ///  <param name="AHigher">The higher bound.</param>
    ///  <param name="ADefault">The default value.</param>
    ///  <returns>The first element that satisfies the given condition; <paramref name="ADefault"/> otherwise.</returns>
    ///  <exception cref="Collections.Base|ECollectionEmptyException">The collection is empty.</exception>
    ///  <exception cref="Collections.Base|ECollectionFilteredEmptyException">No elements satisfy the condition.</exception>
    function FirstWhereBetweenOrDefault(const ALower, AHigher: T; const ADefault: T): T;

    ///  <summary>Returns the last element.</summary>
    ///  <returns>The last element in the collection.</returns>
    ///  <exception cref="Collections.Base|ECollectionEmptyException">The collection is empty.</exception>
    function Last(): T;

    ///  <summary>Returns the last element or a default if the collection is empty.</summary>
    ///  <param name="ADefault">The default value returned if the collection is empty.</param>
    ///  <returns>The last element in the collection if the collection is not empty; otherwise <paramref name="ADefault"/> is returned.</returns>
    function LastOrDefault(const ADefault: T): T;

    ///  <summary>Aggregates a value based on the collection's elements.</summary>
    ///  <param name="AAggregator">The aggregator method.</param>
    ///  <returns>A value that contains the collection's aggregated value.</returns>
    ///  <remarks>This method returns the first element if the collection only has one element. Otherwise,
    ///  <paramref name="AAggregator"/> is invoked for each two elements (first and second; then the result of the first two
    ///  and the third, and so on). The simplest example of aggregation is the "sum" operation where you can obtain the sum of all
    ///  elements in the value.</remarks>
    ///  <exception cref="SysUtils|EArgumentNilException"><paramref name="AAggregator"/> is <c>nil</c>.</exception>
    ///  <exception cref="Collections.Base|ECollectionEmptyException">The collection is empty.</exception>
    function Aggregate(const AAggregator: TFunc<T, T, T>): T;

    ///  <summary>Aggregates a value based on the collection's elements.</summary>
    ///  <param name="AAggregator">The aggregator method.</param>
    ///  <param name="ADefault">The default value returned if the collection is empty.</param>
    ///  <returns>A value that contains the collection's aggregated value. If the collection is empty, <paramref name="ADefault"/> is returned.</returns>
    ///  <remarks>This method returns the first element if the collection only has one element. Otherwise,
    ///  <paramref name="AAggregator"/> is invoked for each two elements (first and second; then the result of the first two
    ///  and the third, and so on). The simplest example of aggregation is the "sum" operation where you can obtain the sum of all
    ///  elements in the value.</remarks>
    ///  <exception cref="SysUtils|EArgumentNilException"><paramref name="AAggregator"/> is <c>nil</c>.</exception>
    function AggregateOrDefault(const AAggregator: TFunc<T, T, T>; const ADefault: T): T;

    ///  <summary>Returns the element at a given position.</summary>
    ///  <param name="AIndex">The index from which to return the element.</param>
    ///  <returns>The element at the specified position.</returns>
    ///  <remarks>This method is slow for collections that cannot reference their elements by indexes; for example: linked lists</remarks>
    ///  <exception cref="Collections.Base|ECollectionEmptyException">The collection is empty.</exception>
    ///  <exception cref="SysUtils|EArgumentOutOfRangeException"><paramref name="AIndex"/> is out of bounds.</exception>
    function ElementAt(const AIndex: NativeInt): T;

    ///  <summary>Returns the element at a given position.</summary>
    ///  <param name="AIndex">The index from which to return the element.</param>
    ///  <param name="ADefault">The default value returned if the collection is empty.</param>
    ///  <returns>The element at the specified position if the collection is not empty and the position is not out of bounds; otherwise
    ///  the value of <paramref name="ADefault"/> is returned.</returns>
    ///  <remarks>This method is slow for collections that cannot reference their elements by indexes; for example: linked lists</remarks>
    function ElementAtOrDefault(const AIndex: NativeInt; const ADefault: T): T;

    ///  <summary>Check whether at least one element in the collection satisfies a given predicate.</summary>
    ///  <param name="APredicate">The predicate to check for each element.</param>
    ///  <returns><c>True</c> if the at least one element satisfies a given predicate; <c>False</c> otherwise.</returns>
    ///  <remarks>This method traverses the whole collection and checks the value of the predicate for each element. This method
    ///  stops on the first element for which the predicate returns <c>True</c>. The logical equivalent of this operation is "OR".</remarks>
    ///  <exception cref="SysUtils|EArgumentNilException"><paramref name="APredicate"/> is <c>nil</c>.</exception>
    function Any(const APredicate: TFunc<T, Boolean>): Boolean;

    ///  <summary>Checks that all elements in the collection satisfies a given predicate.</summary>
    ///  <param name="APredicate">The predicate to check for each element.</param>
    ///  <returns><c>True</c> if all elements satisfy a given predicate; <c>False</c> otherwise.</returns>
    ///  <remarks>This method traverses the whole collection and checks the value of the predicate for each element. This method
    ///  stops on the first element for which the predicate returns <c>False</c>. The logical equivalent of this operation is "AND".</remarks>
    ///  <exception cref="SysUtils|EArgumentNilException"><paramref name="APredicate"/> is <c>nil</c>.</exception>
    function All(const APredicate: TFunc<T, Boolean>): Boolean;

    ///  <summary>Selects only the elements that satisfy a given rule.</summary>
    ///  <param name="APredicate">The predicate that represents the rule.</param>
    ///  <returns>A new collection that contains only the elements that satisfy the given rule.</returns>
    ///  <exception cref="SysUtils|EArgumentNilException"><paramref name="APredicate"/> is <c>nil</c>.</exception>
    function Where(const APredicate: TFunc<T, Boolean>): IEnexCollection<T>;

    ///  <summary>Selects only the elements that do not satisfy a given rule.</summary>
    ///  <param name="APredicate">The predicate that represents the rule.</param>
    ///  <returns>A new collection that contains only the elements that do not satisfy the given rule.</returns>
    ///  <exception cref="SysUtils|EArgumentNilException"><paramref name="APredicate"/> is <c>nil</c>.</exception>
    function WhereNot(const APredicate: TFunc<T, Boolean>): IEnexCollection<T>;

    ///  <summary>Selects only the elements that are less than a given value.</summary>
    ///  <param name="ABound">The element to compare against.</param>
    ///  <returns>A new collection that contains only the elements that satisfy the relationship.</returns>
    function WhereLower(const ABound: T): IEnexCollection<T>;

    ///  <summary>Selects only the elements that are less than or equal to a given value.</summary>
    ///  <param name="ABound">The element to compare against.</param>
    ///  <returns>A new collection that contains only the elements that satisfy the relationship.</returns>
    function WhereLowerOrEqual(const ABound: T): IEnexCollection<T>;

    ///  <summary>Selects only the elements that are greater than a given value.</summary>
    ///  <param name="ABound">The element to compare against.</param>
    ///  <returns>A new collection that contains only the elements that satisfy the relationship.</returns>
    function WhereGreater(const ABound: T): IEnexCollection<T>;

    ///  <summary>Selects only the elements that are greater than or equal to a given value.</summary>
    ///  <param name="ABound">The element to compare against.</param>
    ///  <returns>A new collection that contains only the elements that satisfy the relationship.</returns>
    function WhereGreaterOrEqual(const ABound: T): IEnexCollection<T>;

    ///  <summary>Selects only the elements whose values are contained whithin a given interval.</summary>
    ///  <param name="ALower">The lower bound.</param>
    ///  <param name="AHigher">The upper bound.</param>
    ///  <returns>A new collection that contains only the elements that satisfy the relationship.</returns>
    ///  <remarks>The elements that are equal to the lower or upper bounds, are also included.</remarks>
    function WhereBetween(const ALower, AHigher: T): IEnexCollection<T>;

    ///  <summary>Selects all the elements from the collection excluding duplicates.</summary>
    ///  <returns>A new collection that contains the distinct elements.</returns>
    function Distinct(): IEnexCollection<T>;

    ///  <summary>Returns a new ordered collection that contains the elements from this collection.</summary>
    ///  <param name="AAscending">Specifies whether the elements are ordered ascending or descending.</param>
    ///  <returns>A new ordered collection.</returns>
    function Ordered(const AAscending: Boolean = true): IEnexCollection<T>; overload;

    ///  <summary>Returns a new ordered collection that contains the elements from this collection.</summary>
    ///  <param name="ASortProc">The comparison method.</param>
    ///  <returns>A new ordered collection.</returns>
    ///  <exception cref="SysUtils|EArgumentNilException"><paramref name="ASortProc"/> is <c>nil</c>.</exception>
    function Ordered(const ASortProc: TComparison<T>): IEnexCollection<T>; overload;

    ///  <summary>Revereses the contents of the collection.</summary>
    ///  <returns>A new collection that contains the elements from this collection but in reverse order.</returns>
    function Reversed(): IEnexCollection<T>;

    ///  <summary>Concatenates this collection with another collection.</summary>
    ///  <param name="ACollection">A collection to concatenate.</param>
    ///  <returns>A new collection that contains the elements from this collection followed by elements
    ///  from the given collection.</returns>
    ///  <exception cref="SysUtils|EArgumentNilException"><paramref name="ACollection"/> is <c>nil</c>.</exception>
    function Concat(const ACollection: IEnexCollection<T>): IEnexCollection<T>;

    ///  <summary>Creates a new collection that contains the elements from both collections taken a single time.</summary>
    ///  <param name="ACollection">The collection to unify with.</param>
    ///  <returns>A new collection that contains the elements from this collection followed by elements
    ///  from the given collection except the elements that already are present in this collection. This operation can be seen as
    ///  a "concat" operation followed by a "distinct" operation. </returns>
    ///  <exception cref="SysUtils|EArgumentNilException"><paramref name="ACollection"/> is <c>nil</c>.</exception>
    function Union(const ACollection: IEnexCollection<T>): IEnexCollection<T>;

    ///  <summary>Creates a new collection that contains the elements from this collection minus the ones in the given collection.</summary>
    ///  <param name="ACollection">The collection to exclude.</param>
    ///  <returns>A new collection that contains the elements from this collection minus the those elements that are common between
    ///  this and the given collection.</returns>
    ///  <exception cref="SysUtils|EArgumentNilException"><paramref name="ACollection"/> is <c>nil</c>.</exception>
    function Exclude(const ACollection: IEnexCollection<T>): IEnexCollection<T>;

    ///  <summary>Creates a new collection that contains the elements that are present in both collections.</summary>
    ///  <param name="ACollection">The collection to interset with.</param>
    ///  <returns>A new collection that contains the elements that are common to both collections.</returns>
    ///  <exception cref="SysUtils|EArgumentNilException"><paramref name="ACollection"/> is <c>nil</c>.</exception>
    function Intersect(const ACollection: IEnexCollection<T>): IEnexCollection<T>;

    ///  <summary>Select the elements that whose indexes are located in the given range.</summary>
    ///  <param name="AStart">The lower bound.</param>
    ///  <param name="AEnd">The upper bound.</param>
    ///  <returns>A new collection that contains the elements whose indexes in this collection are locate between <paramref name="AStart"/>
    ///  and <paramref name="AEnd"/>. Note that this method does not check the indexes. This means that a bad combination of parameters will
    ///  simply result in an empty or incorrect result.</returns>
    function Range(const AStart, AEnd: NativeInt): IEnexCollection<T>;

    ///  <summary>Selects only a given amount of elements.</summary>
    ///  <param name="ACount">The number of elements to select.</param>
    ///  <returns>A new collection that contains only the first <paramref name="ACount"/> elements.</returns>
    ///  <exception cref="SysUtils|EArgumentOutOfRangeException"><paramref name="ACount"/> is zero.</exception>
    function Take(const ACount: NativeInt): IEnexCollection<T>;

    ///  <summary>Selects all the elements from the collection while a given rule is satisfied.</summary>
    ///  <param name="APredicate">The rule to satisfy.</param>
    ///  <returns>A new collection that contains the selected elements.</returns>
    ///  <remarks>This method selects all elements from the collection while the given rule is satisfied.</remarks>
    ///  <exception cref="SysUtils|EArgumentNilException"><paramref name="APredicate"/> is <c>nil</c>.</exception>
    function TakeWhile(const APredicate: TFunc<T, Boolean>): IEnexCollection<T>;

    ///  <summary>Selects all the elements from the collection while elements are lower than a given value.</summary>
    ///  <param name="ABound">The value to check against.</param>
    ///  <returns>A new collection that contains the selected elements.</returns>
    ///  <remarks>This method selects all elements from the collection while the given rule is satisfied.</remarks>
    function TakeWhileLower(const ABound: T): IEnexCollection<T>;

    ///  <summary>Selects all the elements from the collection while elements are lower than
    ///  or equals to a given value.</summary>
    ///  <param name="ABound">The value to check against.</param>
    ///  <returns>A new collection that contains the selected elements.</returns>
    ///  <remarks>This method selects all elements from the collection while the given rule is satisfied.</remarks>
    function TakeWhileLowerOrEqual(const ABound: T): IEnexCollection<T>;

    ///  <summary>Selects all the elements from the collection while elements are greater than
    ///  a given value.</summary>
    ///  <param name="ABound">The value to check against.</param>
    ///  <returns>A new collection that contains the selected elements.</returns>
    ///  <remarks>This method selects all elements from the collection while the given rule is satisfied.</remarks>
    function TakeWhileGreater(const ABound: T): IEnexCollection<T>;

    ///  <summary>Selects all the elements from the collection while elements are greater than
    ///  or equals to a given value.</summary>
    ///  <param name="ABound">The value to check against.</param>
    ///  <returns>A new collection that contains the selected elements.</returns>
    ///  <remarks>This method selects all elements from the collection while the given rule is satisfied.</remarks>
    function TakeWhileGreaterOrEqual(const ABound: T): IEnexCollection<T>;

    ///  <summary>Selects all the elements from the collection while elements are between a given range of values.</summary>
    ///  <param name="ALower">The lower bound.</param>
    ///  <param name="AHigher">The higher bound.</param>
    ///  <returns>A new collection that contains the selected elements.</returns>
    ///  <remarks>This method selects all elements from the collection while the given rule is satisfied.</remarks>
    function TakeWhileBetween(const ALower, AHigher: T): IEnexCollection<T>;

    ///  <summary>Skips a given amount of elements.</summary>
    ///  <param name="ACount">The number of elements to skip.</param>
    ///  <returns>A new collection that contains the elements that were not skipped.</returns>
    ///  <exception cref="SysUtils|EArgumentOutOfRangeException"><paramref name="ACount"/> is zero.</exception>
    function Skip(const ACount: NativeInt): IEnexCollection<T>;

    ///  <summary>Skips all the elements from the collection while a given rule is satisfied.</summary>
    ///  <param name="APredicate">The rule to satisfy.</param>
    ///  <returns>A new collection that contains the elements that were not skipped.</returns>
    ///  <exception cref="SysUtils|EArgumentNilException"><paramref name="APredicate"/> is <c>nil</c>.</exception>
    function SkipWhile(const APredicate: TFunc<T, Boolean>): IEnexCollection<T>;

    ///  <summary>Skips all the elements from the collection while elements are lower than a given value.</summary>
    ///  <param name="ABound">The value to check.</param>
    ///  <returns>A new collection that contains the elements that were not skipped.</returns>
    function SkipWhileLower(const ABound: T): IEnexCollection<T>;

    ///  <summary>Skips all the elements from the collection while elements are lower than or equal to a given value.</summary>
    ///  <param name="ABound">The value to check.</param>
    ///  <returns>A new collection that contains the elements that were not skipped.</returns>
    function SkipWhileLowerOrEqual(const ABound: T): IEnexCollection<T>;

    ///  <summary>Skips all the elements from the collection while elements are greater than a given value.</summary>
    ///  <param name="ABound">The value to check.</param>
    ///  <returns>A new collection that contains the elements that were not skipped.</returns>
    function SkipWhileGreater(const ABound: T): IEnexCollection<T>;

    ///  <summary>Skips all the elements from the collection while elements are greater than or equal to a given value.</summary>
    ///  <param name="ABound">The value to check.</param>
    ///  <returns>A new collection that contains the elements that were not skipped.</returns>
    function SkipWhileGreaterOrEqual(const ABound: T): IEnexCollection<T>;

    ///  <summary>Skips all the elements from the collection while elements are between a given range of values.</summary>
    ///  <param name="ALower">The lower bound.</param>
    ///  <param name="AHigher">The higher bound.</param>
    ///  <returns>A new collection that contains the elements that were not skipped.</returns>
    function SkipWhileBetween(const ALower, AHigher: T): IEnexCollection<T>;

    ///  <summary>Exposes a type that provides extended Enex operations such as "select".</summary>
    ///  <returns>A record that exposes more Enex operations that otherwise would be impossible.</returns>
    function Op: TEnexExtOps<T>;
  end;

  ///  <summary>Enex collection that is presumed to be grouped by a certain key.</summary>
  IEnexGroupingCollection<TKey, T> = interface(IEnexCollection<T>)
    ///  <summary>Returns the key under which all elements in this collection are grouped.</summary>
    ///  <returns>The key of this grouping.</returns>
    function GetKey(): TKey;

    ///  <summary>Returns the key under which all elements in this collection are grouped.</summary>
    ///  <returns>The key of this grouping.</returns>
    property Key: TKey read GetKey;
  end;

  ///  <summary>The Enex interface implemented in collections that allow indexed element access.</summary>
  ///  <remarks>This interface is inherited by other more specific interfaces such as lists. Indexed collections
  ///  allow their elements to be accesed given a numeric index.</remarks>
  IEnexIndexedCollection<T> = interface(IEnexCollection<T>)
    ///  <summary>Returns the item from a given index.</summary>
    ///  <param name="AIndex">The index in the collection.</param>
    ///  <returns>The element at the specified position.</returns>
    ///  <remarks>This method is similar to <c>ElementAt</c>. The only difference is that this method is guaranteed
    ///  to provide the fastest lookup (normally <c>ElementAt</c> should also use the same method in indexed collections).</remarks>
    ///  <exception cref="SysUtils|EArgumentOutOfRangeException"><paramref name="AIndex"/> is out of bounds.</exception>
    function GetItem(const AIndex: NativeInt): T;

    ///  <summary>Returns the item from a given index.</summary>
    ///  <param name="AIndex">The index in the collection.</param>
    ///  <returns>The element at the specified position.</returns>
    ///  <exception cref="SysUtils|EArgumentOutOfRangeException"><paramref name="AIndex"/> is out of bounds.</exception>
    property Items[const AIndex: NativeInt]: T read GetItem; default;
  end;

  ///  <summary>Base Enex (Extended enumerable) interface inherited by all specific associative collection interfaces.</summary>
  ///  <remarks>This interface defines a set of traits common to all associative collections implemented in this package. It also introduces
  ///  a large se of extended operations that can pe performed on any collection that supports enumerability.</remarks>
  IEnexAssociativeCollection<TKey, TValue> = interface(ICollection<TPair<TKey, TValue>>)
    ///  <summary>Creates a new dictionary containing the elements of this collection.</summary>
    ///  <returns>A dictionary containing the elements copied from this collection.</returns>
    ///  <remarks>This method also copies the rule sets of this collection. Be careful if the rule set
    ///  performs cleanup on the elements.</remarks>
    ///  <exception cref="Collections.Base|EDuplicateKeyException">The collection contains more than
    ///  one key-value pair with the same key.</exception>
    function ToDictionary(): IDictionary<TKey, TValue>;

    ///  <summary>Returns the value associated with the given key.</summary>
    ///  <param name="AKey">The key for which to return the associated value.</param>
    ///  <returns>The value associated with the given key.</returns>
    ///  <exception cref="Collections.Base|EKeyNotFoundException">No such key in the collection.</exception>
    function ValueForKey(const AKey: TKey): TValue;

    ///  <summary>Checks whether the collection contains a given key-value pair.</summary>
    ///  <param name="AKey">The key part of the pair.</param>
    ///  <param name="AValue">The value part of the pair.</param>
    ///  <returns><c>True</c> if the given key-value pair exists; <c>False</c> otherwise.</returns>
    function KeyHasValue(const AKey: TKey; const AValue: TValue): Boolean;

    ///  <summary>Returns the biggest key.</summary>
    ///  <returns>The biggest key stored in the collection.</returns>
    ///  <exception cref="Collections.Base|ECollectionEmptyException">The collection is empty.</exception>
    function MaxKey(): TKey;

    ///  <summary>Returns the smallest key.</summary>
    ///  <returns>The smallest key stored in the collection.</returns>
    ///  <exception cref="Collections.Base|ECollectionEmptyException">The collection is empty.</exception>
    function MinKey(): TKey;

    ///  <summary>Returns the biggest value.</summary>
    ///  <returns>The biggest value stored in the collection.</returns>
    ///  <exception cref="Collections.Base|ECollectionEmptyException">The collection is empty.</exception>
    function MaxValue(): TValue;

    ///  <summary>Returns the smallest value.</summary>
    ///  <returns>The smallest value stored in the collection.</returns>
    ///  <exception cref="Collections.Base|ECollectionEmptyException">The collection is empty.</exception>
    function MinValue(): TValue;

    ///  <summary>Returns an Enex collection that contains only the keys.</summary>
    ///  <returns>An Enex collection that contains all the keys stored in the collection.</returns>
    function SelectKeys(): IEnexCollection<TKey>;

    ///  <summary>Returns a Enex collection that contains only the values.</summary>
    ///  <returns>An Enex collection that contains all the values stored in the collection.</returns>
    function SelectValues(): IEnexCollection<TValue>;

    ///  <summary>Specifies the collection that contains only the keys.</summary>
    ///  <returns>An Enex collection that contains all the keys stored in the collection.</returns>
    property Keys: IEnexCollection<TKey> read SelectKeys;

    ///  <summary>Specifies the collection that contains only the values.</summary>
    ///  <returns>An Enex collection that contains all the values stored in the collection.</returns>
    property Values: IEnexCollection<TValue> read SelectValues;

    ///  <summary>Selects all the key-value pairs from the collection excluding the duplicates by key.</summary>
    ///  <returns>A new collection that contains the distinct pairs.</returns>
    function DistinctByKeys(): IEnexAssociativeCollection<TKey, TValue>;

    ///  <summary>Selects all the key-value pairs from the collection excluding the duplicates by value.</summary>
    ///  <returns>A new collection that contains the distinct pairs.</returns>
    function DistinctByValues(): IEnexAssociativeCollection<TKey, TValue>;

    ///  <summary>Checks whether this collection includes the key-value pairs in another collection.</summary>
    ///  <param name="ACollection">The collection to check against.</param>
    ///  <returns><c>True</c> if this collection includes the elements in another; <c>False</c> otherwise.</returns>
    function Includes(const ACollection: IEnumerable<TPair<TKey, TValue>>): Boolean;

    ///  <summary>Selects only the key-value pairs that satisfy a given rule.</summary>
    ///  <param name="APredicate">The predicate that represents the rule.</param>
    ///  <returns>A new collection that contains only the pairs that satisfy the given rule.</returns>
    ///  <exception cref="SysUtils|EArgumentNilException"><paramref name="APredicate"/> is <c>nil</c>.</exception>
    function Where(const APredicate: TFunc<TKey, TValue, Boolean>): IEnexAssociativeCollection<TKey, TValue>;

    ///  <summary>Selects only the key-value pairs that do not satisfy a given rule.</summary>
    ///  <param name="APredicate">The predicate that represents the rule.</param>
    ///  <returns>A new collection that contains only the pairs that do not satisfy the given rule.</returns>
    ///  <exception cref="SysUtils|EArgumentNilException"><paramref name="APredicate"/> is <c>nil</c>.</exception>
    function WhereNot(const APredicate: TFunc<TKey, TValue, Boolean>): IEnexAssociativeCollection<TKey, TValue>;

    ///  <summary>Selects only the key-value pairs whose keys are less than a given value.</summary>
    ///  <param name="ABound">The value to compare against.</param>
    ///  <returns>A new collection that contains only the pairs that satisfy the relationship.</returns>
    function WhereKeyLower(const ABound: TKey): IEnexAssociativeCollection<TKey, TValue>;

    ///  <summary>Selects only the key-value pairs whose keys are less than or equal to a given value.</summary>
    ///  <param name="ABound">The value to compare against.</param>
    ///  <returns>A new collection that contains only the pairs that satisfy the relationship.</returns>
    function WhereKeyLowerOrEqual(const ABound: TKey): IEnexAssociativeCollection<TKey, TValue>;

    ///  <summary>Selects only the key-value pairs whose keys are greater than a given value.</summary>
    ///  <param name="ABound">The value to compare against.</param>
    ///  <returns>A new collection that contains only the pairs that satisfy the relationship.</returns>
    function WhereKeyGreater(const ABound: TKey): IEnexAssociativeCollection<TKey, TValue>;

    ///  <summary>Selects only the key-value pairs whose keys are greater than or equal to a given value.</summary>
    ///  <param name="ABound">The value to compare against.</param>
    ///  <returns>A new collection that contains only the pairs that satisfy the relationship.</returns>
    function WhereKeyGreaterOrEqual(const ABound: TKey): IEnexAssociativeCollection<TKey, TValue>;

    ///  <summary>Selects only the key-value pairs whose keys are are contained whithin a given interval.</summary>
    ///  <param name="ALower">The lower bound.</param>
    ///  <param name="AHigher">The upper bound.</param>
    ///  <returns>A new collection that contains only the pairs that satisfy the relationship.</returns>
    function WhereKeyBetween(const ALower, AHigher: TKey): IEnexAssociativeCollection<TKey, TValue>;

    ///  <summary>Selects only the key-value pairs whose values are less than a given value.</summary>
    ///  <param name="ABound">The value to compare against.</param>
    ///  <returns>A new collection that contains only the pairs that satisfy the relationship.</returns>
    function WhereValueLower(const ABound: TValue): IEnexAssociativeCollection<TKey, TValue>;

    ///  <summary>Selects only the key-value pairs whose values are less than or equal to a given value.</summary>
    ///  <param name="ABound">The value to compare against.</param>
    ///  <returns>A new collection that contains only the pairs that satisfy the relationship.</returns>
    function WhereValueLowerOrEqual(const ABound: TValue): IEnexAssociativeCollection<TKey, TValue>;

    ///  <summary>Selects only the key-value pairs whose values are greater than a given value.</summary>
    ///  <param name="ABound">The value to compare against.</param>
    ///  <returns>A new collection that contains only the pairs that satisfy the relationship.</returns>
    function WhereValueGreater(const ABound: TValue): IEnexAssociativeCollection<TKey, TValue>;

    ///  <summary>Selects only the key-value pairs whose values are greater than or equal to a given value.</summary>
    ///  <param name="ABound">The value to compare against.</param>
    ///  <returns>A new collection that contains only the pairs that satisfy the relationship.</returns>
    function WhereValueGreaterOrEqual(const ABound: TValue): IEnexAssociativeCollection<TKey, TValue>;

    ///  <summary>Selects only the key-value pairs whose values are are contained whithin a given interval.</summary>
    ///  <param name="ALower">The lower bound.</param>
    ///  <param name="AHigher">The upper bound.</param>
    ///  <returns>A new collection that contains only the pairs that satisfy the relationship.</returns>
    function WhereValueBetween(const ALower, AHigher: TValue): IEnexAssociativeCollection<TKey, TValue>;
  end;

  ///  <summary>The Enex interface that defines the behavior of a <c>stack</c>.</summary>
  ///  <remarks>This interface is implemented by all collections that provide the functionality of a <c>stack</c>.</remarks>
  IStack<T> = interface(IEnexCollection<T>)
    ///  <summary>Clears the contents of the stack.</summary>
    procedure Clear();

    ///  <summary>Pushes an element to the top of the stack.</summary>
    ///  <param name="AValue">The value to push.</param>
    procedure Push(const AValue: T);

    ///  <summary>Retrieves the element from the top of the stack.</summary>
    ///  <returns>The value at the top of the stack.</returns>
    ///  <remarks>This method removes the element from the top of the stack.</remarks>
    ///  <exception cref="Collections.Base|ECollectionEmptyException">The stack is empty.</exception>
    function Pop(): T;

    ///  <summary>Reads the element from the top of the stack.</summary>
    ///  <returns>The value at the top of the stack.</returns>
    ///  <remarks>This method does not remove the element from the top of the stack. It merely reads it's value.</remarks>
    ///  <exception cref="Collections.Base|ECollectionEmptyException">The stack is empty.</exception>
    function Peek(): T;

    ///  <summary>Removes an element from the stack.</summary>
    ///  <param name="AValue">The value to remove. If there is no such element in the stack, nothing happens.</param>
    procedure Remove(const AValue: T);

    ///  <summary>Checks whether the stack contains a given value.</summary>
    ///  <param name="AValue">The value to check.</param>
    ///  <returns><c>True</c> if the value was found in the stack; <c>False</c> otherwise.</returns>
    function Contains(const AValue: T): Boolean;
  end;

  ///  <summary>The Enex interface that defines the behavior of a <c>queue</c>.</summary>
  ///  <remarks>This interface is implemented by all collections that provide the functionality of a <c>queue</c>.</remarks>
  IQueue<T> = interface(IEnexCollection<T>)
    ///  <summary>Clears the contents of the queue.</summary>
    procedure Clear();

    ///  <summary>Appends an element to the top of the queue.</summary>
    ///  <param name="AValue">The value to append.</param>
    procedure Enqueue(const AValue: T);

    ///  <summary>Retrieves the element from the bottom of the queue.</summary>
    ///  <returns>The value at the bottom of the queue.</returns>
    ///  <remarks>This method removes the element from the bottom of the queue.</remarks>
    ///  <exception cref="Collections.Base|ECollectionEmptyException">The queue is empty.</exception>
    function Dequeue(): T;

    ///  <summary>Reads the element from the bottom of the queue.</summary>
    ///  <returns>The value at the bottom of the queue.</returns>
    ///  <remarks>This method does not remove the element from the bottom of the queue. It merely reads it's value.</remarks>
    ///  <exception cref="Collections.Base|ECollectionEmptyException">The queue is empty.</exception>
    function Peek(): T;

    ///  <summary>Checks whether the queue contains a given value.</summary>
    ///  <param name="AValue">The value to check.</param>
    ///  <returns><c>True</c> if the value was found in the queue; <c>False</c> otherwise.</returns>
    function Contains(const AValue: T): Boolean;
  end;

  ///  <summary>The Enex interface that defines the behavior of a <c>priority queue</c>.</summary>
  ///  <remarks>This interface is implemented by all collections that provide the functionality of a <c>priority queue</c>.</remarks>
  IPriorityQueue<TPriority, TValue> = interface(IEnexAssociativeCollection<TPriority, TValue>)
    ///  <summary>Clears the contents of the priority queue.</summary>
    procedure Clear();

    ///  <summary>Adds an element to the priority queue.</summary>
    ///  <param name="AValue">The value to append.</param>
    ///  <remarks>The lowest possible priority of the element is assumed. This means that the element is appended to the top of the queue.</remarks>
    procedure Enqueue(const AValue: TValue); overload;

    ///  <summary>Adds an element to the priority queue.</summary>
    ///  <param name="AValue">The value to add.</param>
    ///  <param name="APriority">The priority of the value.</param>
    ///  <remarks>The given priority is used to calculate the position of the value in the queue. Based on the priority the element might occupy any
    ///  given position (for example it might even end up at the bottom position).</remarks>
    procedure Enqueue(const AValue: TValue; const APriority: TPriority); overload;

    ///  <summary>Retrieves the element from the bottom of the priority queue.</summary>
    ///  <returns>The value at the bottom of the priority queue.</returns>
    ///  <remarks>This method removes the element from the bottom of the priority queue.</remarks>
    ///  <exception cref="Collections.Base|ECollectionEmptyException">The queue is empty.</exception>
    function Dequeue(): TValue;

    ///  <summary>Reads the element from the bottom of the priority queue.</summary>
    ///  <returns>The value at the bottom of the priority queue.</returns>
    ///  <remarks>This method does not remove the element from the bottom of the priority queue. It merely reads it's value.</remarks>
    ///  <exception cref="Collections.Base|ECollectionEmptyException">The queue is empty.</exception>
    function Peek(): TValue;

    ///  <summary>Checks whether the priority queue contains a given value.</summary>
    ///  <param name="AValue">The value to check.</param>
    ///  <returns><c>True</c> if the value was found in the queue; <c>False</c> otherwise.</returns>
    function Contains(const AValue: TValue): Boolean;
  end;

  ///  <summary>The Enex interface that defines the behavior of a <c>set</c>.</summary>
  ///  <remarks>This interface is implemented by all collections that provide the functionality of a <c>set</c>.</remarks>
  ISet<T> = interface(IEnexCollection<T>)
    ///  <summary>Clears the contents of the set.</summary>
    procedure Clear();

    ///  <summary>Adds an element to the set.</summary>
    ///  <param name="AValue">The value to add.</param>
    ///  <remarks>If the set already contains the given value, nothing happens.</remarks>
    procedure Add(const AValue: T);

    ///  <summary>Removes a given value from the set.</summary>
    ///  <param name="AValue">The value to remove.</param>
    ///  <remarks>If the set does not contain the given value, nothing happens.</remarks>
    procedure Remove(const AValue: T);

    ///  <summary>Checks whether the set contains a given value.</summary>
    ///  <param name="AValue">The value to check.</param>
    ///  <returns><c>True</c> if the value was found in the set; <c>False</c> otherwise.</returns>
    function Contains(const AValue: T): Boolean;
  end;

  ///  <summary>The Enex interface that defines the behavior of a <c>sorted set</c>.</summary>
  ///  <remarks>This interface is implemented by all collections that provide the functionality of a <c>sorted set</c>.</remarks>
  ISortedSet<T> = interface(ISet<T>)
    ///  <summary>Returns the biggest set element.</summary>
    ///  <returns>An element from the set considered to have the biggest value.</returns>
    ///  <exception cref="Collections.Base|ECollectionEmptyException">The set is empty.</exception>
    function Max(): T;

    ///  <summary>Returns the smallest set element.</summary>
    ///  <returns>An element from the set considered to have the smallest value.</returns>
    ///  <exception cref="Collections.Base|ECollectionEmptyException">The set is empty.</exception>
    function Min(): T;
  end;

  ///  <summary>The Enex interface that defines the behavior of a <c>bag</c>.</summary>
  ///  <remarks>This interface is implemented by all collections that provide the functionality of a <c>bag</c>.</remarks>
  IBag<T> = interface(IEnexCollection<T>)
    ///  <summary>Clears the contents of the bag.</summary>
    procedure Clear();

    ///  <summary>Adds an element to the bag.</summary>
    ///  <param name="AValue">The element to add.</param>
    ///  <param name="AWeight">The weight of the element.</param>
    ///  <remarks>If the bag already contains the given value, it's stored weight is incremented to by <paramref name="AWeight"/>.
    ///  If the value of <paramref name="AWeight"/> is zero, nothing happens.</remarks>
    procedure Add(const AValue: T; const AWeight: NativeUInt = 1);

    ///  <summary>Removes an element from the bag.</summary>
    ///  <param name="AValue">The value to remove.</param>
    ///  <param name="AWeight">The weight to remove.</param>
    ///  <remarks>This method decreses the weight of the stored item by <paramref name="AWeight"/>. If the resulting weight is less
    ///  than zero or zero, the element is removed for the bag. If <paramref name="AWeight"/> is zero, nothing happens.</remarks>
    procedure Remove(const AValue: T; const AWeight: NativeUInt = 1);

    ///  <summary>Removes an element from the bag.</summary>
    ///  <param name="AValue">The value to remove.</param>
    ///  <remarks>This method completely removes an item from the bag ignoring it's stored weight. Nothing happens if the given value
    ///  is not in the bag to begin with.</remarks>
    procedure RemoveAll(const AValue: T);

    ///  <summary>Checks whether the bag contains an element with at least the required weight.</summary>
    ///  <param name="AValue">The value to check.</param>
    ///  <param name="AWeight">The smallest allowed weight.</param>
    ///  <returns><c>True</c> if the condition is met; <c>False</c> otherwise.</returns>
    ///  <remarks>This method checks whether the bag contains the given value and that the contained value has at least the
    ///  given weight.</remarks>
    function Contains(const AValue: T; const AWeight: NativeUInt = 1): Boolean;

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

    ///  <summary>Sets or gets the weight of an item in the bag.</summary>
    ///  <param name="AValue">The value.</param>
    ///  <remarks>If the value is not found in the bag, this method acts like an <c>Add</c> operation; otherwise
    ///  the weight of the stored item is adjusted.</remarks>
    property Weights[const AValue: T]: NativeUInt read GetWeight write SetWeight; default;
  end;

  ///  <summary>The Enex interface that defines the basic behavior of all <c>map</c>-like collections.</summary>
  ///  <remarks>This interface is inherited by all interfaces that provide <c>map</c>-like functionality.</remarks>
  IMap<TKey, TValue> = interface(IEnexAssociativeCollection<TKey, TValue>)
    ///  <summary>Clears the contents of the map.</summary>
    procedure Clear();

{$IF CompilerVersion < 22}
    ///  <summary>Adds a key-value pair to the map.</summary>
    ///  <param name="APair">The key-value pair to add.</param>
    ///  <exception cref="Collections.Base|EDuplicateKeyException">The map already contains a pair with the given key.</exception>
    procedure Add(const APair: TPair<TKey, TValue>); overload;
{$IFEND}

    ///  <summary>Adds a key-value pair to the map.</summary>
    ///  <param name="AKey">The key of pair.</param>
    ///  <param name="AValue">The value associated with the key.</param>
    ///  <exception cref="Collections.Base|EDuplicateKeyException">The map already contains a pair with the given key.</exception>
    procedure Add(const AKey: TKey; const AValue: TValue); overload;

    ///  <summary>Removes a key-value pair using a given key.</summary>
    ///  <param name="AKey">The key of pair.</param>
    ///  <remarks>If the specified key was not found in the map, nothing happens.</remarks>
    procedure Remove(const AKey: TKey);

    ///  <summary>Checks whether the map contains a key-value pair identified by the given key.</summary>
    ///  <param name="AKey">The key to check for.</param>
    ///  <returns><c>True</c> if the map contains a pair identified by the given key; <c>False</c> otherwise.</returns>
    function ContainsKey(const AKey: TKey): Boolean;

    ///  <summary>Checks whether the map contains a key-value pair that contains a given value.</summary>
    ///  <param name="AValue">The value to check for.</param>
    ///  <returns><c>True</c> if the map contains a pair containing the given value; <c>False</c> otherwise.</returns>
    ///  <remarks>This operation should be avoided. Its perfomance is poor is most map implementations.</remarks>
    function ContainsValue(const AValue: TValue): Boolean;
  end;

  ///  <summary>The Enex interface that defines the behavior of a <c>dictionary</c>.</summary>
  ///  <remarks>This interface is implemented by all collections that provide the functionality of a <c>dictionary</c>.</remarks>
  IDictionary<TKey, TValue> = interface(IMap<TKey, TValue>)
    ///  <summary>Tries to obtain the value associated with a given key.</summary>
    ///  <param name="AKey">The key for which to try to retreive the value.</param>
    ///  <param name="AFoundValue">The found value (if the result is <c>True</c>).</param>
    ///  <returns><c>True</c> if the dictionary contains a value for the given key; <c>False</c> otherwise.</returns>
    function TryGetValue(const AKey: TKey; out AFoundValue: TValue): Boolean;

    ///  <summary>Returns the value associated with the given key.</summary>
    ///  <param name="AKey">The key for which to try to retreive the value.</param>
    ///  <returns>The value associated with the key.</returns>
    ///  <exception cref="Collections.Base|EKeyNotFoundException">The key is not found in the dictionary.</exception>
    function GetItem(const AKey: TKey): TValue;

    ///  <summary>Sets the value for a given key.</summary>
    ///  <param name="AKey">The key for which to set the value.</param>
    ///  <param name="AValue">The value to set.</param>
    ///  <remarks>If the dictionary does not contain the key, this method acts like <c>Add</c>; otherwise the
    ///  value of the specified key is modified.</remarks>
    procedure SetItem(const AKey: TKey; const AValue: TValue);

    ///  <summary>Gets or sets the value for a given key.</summary>
    ///  <param name="AKey">The key for to operate on.</param>
    ///  <returns>The value associated with the key.</returns>
    ///  <remarks>If the dictionary does not contain the key, this method acts like <c>Add</c> if assignment is done to this property;
    ///  otherwise the value of the specified key is modified.</remarks>
    ///  <exception cref="Collections.Base|EKeyNotFoundException">The trying to read the value of a key that is
    ///  not found in the dictionary.</exception>
    property Items[const AKey: TKey]: TValue read GetItem write SetItem; default;
  end;

  ///  <summary>The Enex interface that defines the behavior of a <c>bidirectional dictionary</c>.</summary>
  ///  <remarks>This interface is implemented by all collections that provide the functionality of a <c>bidirectional dictionary</c>. In a
  ///  <c>bidirectional dictionary</c>, both the key and the value are treated as "keys".</remarks>
  IBidiDictionary<TKey, TValue> = interface(IMap<TKey, TValue>)

    ///  <summary>Removes a key-value pair using a given key.</summary>
    ///  <param name="AKey">The key (and its associated value) to remove.</param>
    procedure RemoveKey(const AKey: TKey);

    ///  <summary>Removes a key-value pair using a given value.</summary>
    ///  <param name="AValue">The value (and its associated key) to remove.</param>
    procedure RemoveValue(const AValue: TValue);

    ///  <summary>Removes a specific key-value combination.</summary>
    ///  <param name="AKey">The key to remove.</param>
    ///  <param name="AValue">The value to remove.</param>
    ///  <remarks>This method only remove a key-value combination if that combination actually exists in the dictionary.
    ///  If the key is associated with another value, nothing happens.</remarks>
    procedure Remove(const AKey: TKey; const AValue: TValue); overload;

{$IF CompilerVersion < 22}
    ///  <summary>Removes a key-value combination.</summary>
    ///  <param name="APair">The pair to remove.</param>
    ///  <remarks>This method only remove a key-value combination if that combination actually exists in the dictionary.
    ///  If the key is associated with another value, nothing happens.</remarks>
    procedure Remove(const APair: TPair<TKey, TValue>); overload;
{$IFEND}

    ///  <summary>Checks whether the map contains the given key-value combination.</summary>
    ///  <param name="AKey">The key associated with the value.</param>
    ///  <param name="AValue">The value associated with the key.</param>
    ///  <returns><c>True</c> if the dictionary contains the given association; <c>False</c> otherwise.</returns>
    function ContainsPair(const AKey: TKey; const AValue: TValue): Boolean; overload;

{$IF CompilerVersion < 22}
    ///  <summary>Checks whether the map contains a given key-value combination.</summary>
    ///  <param name="APair">The key-value pair combination.</param>
    ///  <returns><c>True</c> if the dictionary contains the given association; <c>False</c> otherwise.</returns>
    function ContainsPair(const APair: TPair<TKey, TValue>): Boolean; overload;
{$IFEND}

    ///  <summary>Tries to obtain the value associated with a given key.</summary>
    ///  <param name="AKey">The key for which to try to retreive the value.</param>
    ///  <param name="AFoundValue">The found value (if the result is <c>True</c>).</param>
    ///  <returns><c>True</c> if the dictionary contains a value for the given key; <c>False</c> otherwise.</returns>
    function TryGetValue(const AKey: TKey; out AFoundValue: TValue): Boolean;

    ///  <summary>Returns the value associated with a key.</summary>
    ///  <param name="AKey">The key for which to obtain the associated value.</param>
    ///  <returns>The associated value.</returns>
    ///  <exception cref="Collections.Base|EKeyNotFoundException">The key is not found in the collection.</exception>
    function GetValue(const AKey: TKey): TValue;

    ///  <summary>Sets the value for a given key.</summary>
    ///  <param name="AKey">The key for which to set the value.</param>
    ///  <param name="AValue">The value to set.</param>
    ///  <remarks>If the dictionary does not contain the key, this method acts like <c>Add</c>; otherwise the
    ///  value of the specified key is modified.</remarks>
    ///  <exception cref="Collections.Base|EDuplicateKeyException">The new value is already used by another key.</exception>
    procedure SetValue(const AKey: TKey; const AValue: TValue);

    ///  <summary>Returns the value associated with a key.</summary>
    ///  <param name="AKey">The key for which to obtain the associated value.</param>
    ///  <returns>The associated value.</returns>
    ///  <exception cref="Collections.Base|EKeyNotFoundException">The key is not found in the collection.</exception>
    property ByKey[const AKey: TKey]: TValue read GetValue write SetValue;

    ///  <summary>Tries to obtain the key associated with a given value.</summary>
    ///  <param name="AValue">The value for which to try to retreive the key.</param>
    ///  <param name="AFoundKey">The found key (if the result is <c>True</c>).</param>
    ///  <returns><c>True</c> if the dictionary contains a key for the given value; <c>False</c> otherwise.</returns>
    function TryGetKey(const AValue: TValue; out AFoundKey: TKey): Boolean;

    ///  <summary>Returns the key associated with a value.</summary>
    ///  <param name="AValue">The value for which to obtain the associated key.</param>
    ///  <returns>The associated key.</returns>
    ///  <exception cref="Collections.Base|EKeyNotFoundException">The value is not found in the collection.</exception>
    function GetKey(const AValue: TValue): TKey;

    ///  <summary>Sets the key for a given value.</summary>
    ///  <param name="AValue">The value for which to set the key.</param>
    ///  <param name="AKey">The key to set.</param>
    ///  <remarks>If the dictionary does not contain the value, this method acts like <c>Add</c>; otherwise the
    ///  key of the specified value is modified.</remarks>
    ///  <exception cref="Collections.Base|EDuplicateKeyException">The new key is already used by another value.</exception>
    procedure SetKey(const AValue: TValue; const AKey: TKey);

    ///  <summary>Returns the key associated with a value.</summary>
    ///  <param name="AValue">The value for which to obtain the associated key.</param>
    ///  <returns>The associated key.</returns>
    ///  <exception cref="Collections.Base|EKeyNotFoundException">The value is not found in the collection.</exception>
    property ByValue[const AValue: TValue]: TKey read GetKey write SetKey;
  end;

  ///  <summary>The Enex interface that defines the basic behavior of all <c>map</c>-like collections that associate a
  ///  key with multiple values.</summary>
  ///  <remarks>This interface is inherited by all interfaces that provide <c>multi-map</c>-like functionality.</remarks>
  ICollectionMap<TKey, TValue> = interface(IMap<TKey, TValue>)

    ///  <summary>Removes a key-value pair using a given key and value.</summary>
    ///  <param name="AKey">The key associated with the value.</param>
    ///  <param name="AValue">The value to remove.</param>
    ///  <remarks>A multi-map allows storing multiple values for a given key. This method allows removing only the
    ///  specified value from the collection of values associated with the given key.</remarks>
    procedure Remove(const AKey: TKey; const AValue: TValue); overload;

{$IF CompilerVersion < 22}
    ///  <summary>Removes a key-value pair using a given key and value.</summary>
    ///  <param name="APair">The key and its associated value to remove.</param>
    ///  <remarks>A multi-map allows storing multiple values for a given key. This method allows removing only the
    ///  specified value from the collection of values associated with the given key.</remarks>
    procedure Remove(const APair: TPair<TKey, TValue>); overload;
{$IFEND}

    ///  <summary>Checks whether the multi-map contains a given key-value combination.</summary>
    ///  <param name="AKey">The key associated with the value.</param>
    ///  <param name="AValue">The value associated with the key.</param>
    ///  <returns><c>True</c> if the map contains the given association; <c>False</c> otherwise.</returns>
    function ContainsValue(const AKey: TKey; const AValue: TValue): Boolean; overload;

{$IF CompilerVersion < 22}
    ///  <summary>Checks whether the multi-map contains a given key-value combination.</summary>
    ///  <param name="APair">The key-value pair to check for.</param>
    ///  <returns><c>True</c> if the map contains the given association; <c>False</c> otherwise.</returns>
    function ContainsValue(const APair: TPair<TKey, TValue>): Boolean; overload;
{$IFEND}
  end;

  ///  <summary>The Enex interface that defines the behavior of a <c>bidirectional multi-map</c>.</summary>
  ///  <remarks>This interface is implemented by all collections that provide the functionality of a <c>bidirectional multi-map</c>. In a
  ///  <c>bidirectional multi-map</c>, both the key and the value are treated as "keys".</remarks>
  IBidiMap<TKey, TValue> = interface(IMap<TKey, TValue>)
    ///  <summary>Removes a key-value pair using a given key.</summary>
    ///  <param name="AKey">The key (and its associated values) to remove.</param>
    ///  <remarks>This method removes all the values that are associated with the given key. The rule set's cleanup
    ///  routines are used to cleanup the values that are dropped from the map.</remarks>
    procedure RemoveKey(const AKey: TKey);

    ///  <summary>Removes a key-value pair using a given value.</summary>
    ///  <param name="AValue">The value (and its associated keys) to remove.</param>
    ///  <remarks>This method removes all the keys that are associated with the given value. The rule set's cleanup
    ///  routines are used to cleanup the keys that are dropped from the map.</remarks>
    procedure RemoveValue(const AValue: TValue);

    ///  <summary>Removes a specific key-value combination.</summary>
    ///  <param name="AKey">The key to remove.</param>
    ///  <param name="AValue">The value to remove.</param>
    ///  <remarks>This method only remove a key-value combination if that combination actually exists in the dictionary.
    ///  If the key is associated with another value, nothing happens.</remarks>
    procedure Remove(const AKey: TKey; const AValue: TValue); overload;

{$IF CompilerVersion < 22}
    ///  <summary>Removes a key-value combination.</summary>
    ///  <param name="APair">The pair to remove.</param>
    ///  <remarks>This method only remove a key-value combination if that combination actually exists in the dictionary.
    ///  If the key is associated with another value, nothing happens.</remarks>
    procedure Remove(const APair: TPair<TKey, TValue>); overload;
{$IFEND}

    ///  <summary>Checks whether the map contains the given key-value combination.</summary>
    ///  <param name="AKey">The key associated with the value.</param>
    ///  <param name="AValue">The value associated with the key.</param>
    ///  <returns><c>True</c> if the map contains the given association; <c>False</c> otherwise.</returns>
    function ContainsPair(const AKey: TKey; const AValue: TValue): Boolean; overload;

{$IF CompilerVersion < 22}
    ///  <summary>Checks whether the map contains a given key-value combination.</summary>
    ///  <param name="APair">The key-value pair combination.</param>
    ///  <returns><c>True</c> if the map contains the given association; <c>False</c> otherwise.</returns>
    function ContainsPair(const APair: TPair<TKey, TValue>): Boolean; overload;
{$IFEND}

    ///  <summary>Returns the collection of values associated with a key.</summary>
    ///  <param name="AKey">The key for which to obtain the associated values.</param>
    ///  <returns>An Enex collection that contains the values associated with this key.</returns>
    ///  <exception cref="Collections.Base|EKeyNotFoundException">The key is not found in the collection.</exception>
    function GetValueList(const AKey: TKey): IEnexCollection<TValue>;

    ///  <summary>Returns the collection of values associated with a key.</summary>
    ///  <param name="AKey">The key for which to obtain the associated values.</param>
    ///  <returns>An Enex collection that contains the values associated with this key.</returns>
    ///  <exception cref="Collections.Base|EKeyNotFoundException">The key is not found in the collection.</exception>
    property ByKey[const AKey: TKey]: IEnexCollection<TValue> read GetValueList;

    ///  <summary>Returns the collection of keys associated with a value.</summary>
    ///  <param name="AValue">The value for which to obtain the associated keys.</param>
    ///  <returns>An Enex collection that contains the values associated with this key.</returns>
    ///  <exception cref="Collections.Base|EKeyNotFoundException">The value is not found in the collection.</exception>
    function GetKeyList(const AValue: TValue): IEnexCollection<TKey>;

    ///  <summary>Returns the collection of keys associated with a value.</summary>
    ///  <param name="AValue">The value for which to obtain the associated keys.</param>
    ///  <returns>An Enex collection that contains the values associated with this key.</returns>
    ///  <exception cref="Collections.Base|EKeyNotFoundException">The value is not found in the collection.</exception>
    property ByValue[const AValue: TValue]: IEnexCollection<TKey> read GetKeyList;
  end;

  ///  <summary>The Enex interface that defines the behavior of a <c>multi-map</c>.</summary>
  ///  <remarks>This interface is implemented by all collections that provide the functionality of a <c>multi-map</c>. In a
  ///  <c>multi-map</c>, a key is associated with multiple values, not just one.</remarks>
  IMultiMap<TKey, TValue> = interface(ICollectionMap<TKey, TValue>)
    ///  <summary>Returns the collection of values associated with a key.</summary>
    ///  <param name="AKey">The key for which to obtain the associated values.</param>
    ///  <returns>An Enex collection that contains the values associated with this key.</returns>
    ///  <exception cref="Collections.Base|EKeyNotFoundException">The key is not found in the collection.</exception>
    function GetItemList(const AKey: TKey): IEnexIndexedCollection<TValue>;

    ///  <summary>Returns the collection of values associated with a key.</summary>
    ///  <param name="AKey">The key for which to obtain the associated values.</param>
    ///  <returns>An Enex collection that contains the values associated with this key.</returns>
    ///  <exception cref="Collections.Base|EKeyNotFoundException">The key is not found in the collection.</exception>
    property Items[const AKey: TKey]: IEnexIndexedCollection<TValue> read GetItemList; default;

    ///  <summary>Tries to extract the collection of values associated with a key.</summary>
    ///  <param name="AKey">The key for which to obtain the associated values.</param>
    ///  <param name="AValues">The Enex collection that stores the associated values.</param>
    ///  <returns><c>True</c> if the key exists in the collection; <c>False</c> otherwise;</returns>
    function TryGetValues(const AKey: TKey; out AValues: IEnexIndexedCollection<TValue>): Boolean; overload;

    ///  <summary>Tries to extract the collection of values associated with a key.</summary>
    ///  <param name="AKey">The key for which to obtain the associated values.</param>
    ///  <returns>The associated collection if the key if valid; an empty collection otherwise.</returns>
    function TryGetValues(const AKey: TKey): IEnexIndexedCollection<TValue>; overload;
  end;

  ///  <summary>The Enex interface that defines the behavior of a <c>distinct multi-map</c>.</summary>
  ///  <remarks>This interface is implemented by all collections that provide the functionality of a <c>distinct multi-map</c>. In a
  ///  <c>dictinct multi-map</c>, a key is associated with multiple distinct values.</remarks>
  IDistinctMultiMap<TKey, TValue> = interface(ICollectionMap<TKey, TValue>)
    ///  <summary>Returns the collection of values associated with a key.</summary>
    ///  <param name="AKey">The key for which to obtain the associated values.</param>
    ///  <returns>An Enex collection that contains the values associated with this key.</returns>
    ///  <exception cref="Collections.Base|EKeyNotFoundException">The key is not found in the collection.</exception>
    function GetItemList(const Key: TKey): IEnexCollection<TValue>;

    ///  <summary>Returns the collection of values associated with a key.</summary>
    ///  <param name="AKey">The key for which to obtain the associated values.</param>
    ///  <returns>An Enex collection that contains the values associated with this key.</returns>
    ///  <exception cref="Collections.Base|EKeyNotFoundException">The key is not found in the collection.</exception>
    property Items[const Key: TKey]: IEnexCollection<TValue> read GetItemList; default;

    ///  <summary>Tries to extract the collection of values associated with a key.</summary>
    ///  <param name="AKey">The key for which to obtain the associated values.</param>
    ///  <param name="AValues">The Enex collection that stores the associated values.</param>
    ///  <returns><c>True</c> if the key exists in the collection; <c>False</c> otherwise;</returns>
    function TryGetValues(const AKey: TKey; out AValues: IEnexCollection<TValue>): Boolean; overload;

    ///  <summary>Tries to extract the collection of values associated with a key.</summary>
    ///  <param name="AKey">The key for which to obtain the associated values.</param>
    ///  <returns>The associated collection if the key if valid; an empty collection otherwise.</returns>
    function TryGetValues(const AKey: TKey): IEnexCollection<TValue>; overload;
  end;

  ///  <summary>The Enex interface that defines the behavior of a <c>list</c>.</summary>
  ///  <remarks>This interface is implemented by all collections that provide the functionality of a <c>list</c>.</remarks>
  IList<T> = interface(IEnexIndexedCollection<T>)
    ///  <summary>Clears the contents of the list.</summary>
    procedure Clear();

    ///  <summary>Appends an element to the list.</summary>
    ///  <param name="AValue">The value to append.</param>
    procedure Add(const AValue: T); overload;

    ///  <summary>Appends the elements from a collection to the list.</summary>
    ///  <param name="ACollection">The values to append.</param>
    ///  <exception cref="SysUtils|EArgumentNilException"><paramref name="ACollection"/> is <c>nil</c>.</exception>
    procedure Add(const ACollection: IEnumerable<T>); overload;

    ///  <summary>Inserts an element into the list.</summary>
    ///  <param name="AIndex">The index to insert to.</param>
    ///  <param name="AValue">The value to insert.</param>
    ///  <remarks>All elements starting with <paramref name="AIndex"/> are moved to the right by one and then
    ///  <paramref name="AValue"/> is placed at position <paramref name="AIndex"/>.</remarks>
    ///  <exception cref="SysUtils|EArgumentOutOfRangeException"><paramref name="AIndex"/> is out of bounds.</exception>
    procedure Insert(const AIndex: NativeInt; const AValue: T); overload;

    ///  <summary>Inserts the elements of a collection into the list.</summary>
    ///  <param name="AIndex">The index to insert to.</param>
    ///  <param name="ACollection">The values to insert.</param>
    ///  <remarks>All elements starting with <paramref name="AIndex"/> are moved to the right by the length of
    ///  <paramref name="ACollection"/> and then <paramref name="AValue"/> is placed at position <paramref name="AIndex"/>.</remarks>
    ///  <exception cref="SysUtils|EArgumentOutOfRangeException"><paramref name="AIndex"/> is out of bounds.</exception>
    ///  <exception cref="SysUtils|EArgumentNilException"><paramref name="ACollection"/> is <c>nil</c>.</exception>
    procedure Insert(const AIndex: NativeInt; const ACollection: IEnumerable<T>); overload;

    ///  <summary>Checks whether the list contains a given value.</summary>
    ///  <param name="AValue">The value to check.</param>
    ///  <returns><c>True</c> if the value was found in the list; <c>False</c> otherwise.</returns>
    function Contains(const AValue: T): Boolean;

    ///  <summary>Removes an element from the list at a given index.</summary>
    ///  <param name="AIndex">The index from which to remove the element.</param>
    ///  <remarks>This method removes the specified element and moves all following elements to the left by one.</remarks>
    ///  <exception cref="SysUtils|EArgumentOutOfRangeException"><paramref name="AIndex"/> is out of bounds.</exception>
    procedure RemoveAt(const AIndex: NativeInt);

    ///  <summary>Removes a given value from the list.</summary>
    ///  <param name="AValue">The value to remove.</param>
    ///  <remarks>If the list does not contain the given value, nothing happens.</remarks>
    procedure Remove(const AValue: T);

    ///  <summary>Searches for the first appearance of a given element in this list.</summary>
    ///  <param name="AValue">The value to search for.</param>
    ///  <param name="AStartIndex">The index to from which the search starts.</param>
    ///  <param name="ACount">The number of elements after the starting one to check against.</param>
    ///  <returns><c>-1</c> if the value was not found; otherwise a positive value indicating the index of the value.</returns>
    ///  <exception cref="SysUtils|EArgumentOutOfRangeException">Parameter combination is incorrect.</exception>
    function IndexOf(const AValue: T; const AStartIndex, ACount: NativeInt): NativeInt; overload;

    ///  <summary>Searches for the first appearance of a given element in this list.</summary>
    ///  <param name="AValue">The value to search for.</param>
    ///  <param name="AStartIndex">The index to from which the search starts.</param>
    ///  <returns><c>-1</c> if the value was not found; otherwise a positive value indicating the index of the value.</returns>
    ///  <exception cref="SysUtils|EArgumentOutOfRangeException"><paramref name="AStartIndex"/> is out of bounds.</exception>
    function IndexOf(const AValue: T; const AStartIndex: NativeInt): NativeInt; overload;

    ///  <summary>Searches for the first appearance of a given element in this list.</summary>
    ///  <param name="AValue">The value to search for.</param>
    ///  <returns><c>-1</c> if the value was not found; otherwise a positive value indicating the index of the value.</returns>
    function IndexOf(const AValue: T): NativeInt; overload;

    ///  <summary>Searches for the last appearance of a given element in this list.</summary>
    ///  <param name="AValue">The value to search for.</param>
    ///  <param name="AStartIndex">The index to from which the search starts.</param>
    ///  <param name="ACount">The number of elements after the starting one to check against.</param>
    ///  <returns><c>-1</c> if the value was not found; otherwise a positive value indicating the index of the value.</returns>
    ///  <exception cref="SysUtils|EArgumentOutOfRangeException">Parameter combination is incorrect.</exception>
    function LastIndexOf(const AValue: T; const AStartIndex, ACount: NativeInt): NativeInt; overload;

    ///  <summary>Searches for the last appearance of a given element in this list.</summary>
    ///  <param name="AValue">The value to search for.</param>
    ///  <param name="AStartIndex">The index to from which the search starts.</param>
    ///  <returns><c>-1</c> if the value was not found; otherwise a positive value indicating the index of the value.</returns>
    ///  <exception cref="SysUtils|EArgumentOutOfRangeException"><paramref name="AStartIndex"/> is out of bounds.</exception>
    function LastIndexOf(const AValue: T; const AStartIndex: NativeInt): NativeInt; overload;

    ///  <summary>Searches for the last appearance of a given element in this list.</summary>
    ///  <param name="AValue">The value to search for.</param>
    ///  <returns><c>-1</c> if the value was not found; otherwise a positive value indicating the index of the value.</returns>
    function LastIndexOf(const AValue: T): NativeInt; overload;
  end;

  ///  <summary>The Enex interface that defines the behavior of a <c>sorted list</c>.</summary>
  ///  <remarks>This interface is implemented by all collections that provide the functionality of a <c>sorted list</c>.
  ///  A <c>sorted list</c> maintains its elements in an ordered fashion at all times. Whenever a new element is added, it is
  ///  automatically inserted in the right position.</remarks>
  ISortedList<T> = interface(IList<T>)
    ///  <summary>Returns the biggest element.</summary>
    ///  <returns>An element from the list considered to have the biggest value. This is either the
    ///  last or the first element (depending on the sorting order).</returns>
    ///  <exception cref="Collections.Base|ECollectionEmptyException">The collection is empty.</exception>
    function Max(): T;

    ///  <summary>Returns the smallest element.</summary>
    ///  <returns>An element from the list considered to have the smallest value. This is either the
    ///  last or the first element (depending on the sorting order).</returns>
    ///  <exception cref="Collections.Base|ECollectionEmptyException">The collection is empty.</exception>
    function Min(): T;
  end;

  ///  <summary>A special interface implemented by collections that support the concept of capacity.</summary>
  ///  <remarks>This interface specifies a set of method that allow controlling the capactity of a collection.</remarks>
  IDynamic = interface
    ///  <summary>Returns the current capacity.</summary>
    ///  <returns>A positive number that specifies the number of elements that the collection can hold before it
    ///  needs to grow again.</returns>
    ///  <remarks>The value of this method is greater or equal to the amount of elements in the collection. If this value
    ///  if greater then the number of elements, it means that the collection has some extra capacity to operate upon.</remarks>
    function GetCapacity(): NativeInt;

    ///  <summary>Removes the excess capacity from the collection.</summary>
    ///  <remarks>This method can be called manually to force the collection to drop the extra capacity it might hold. For example,
    ///  after performing some massive operations on a big list, call this method to ensure that all extra memory held by the
    ///  collection is released.</remarks>
    procedure Shrink();

    ///  <summary>Forces the collection to increase its capacity.</summary>
    ///  <remarks>Call this method to force the collection to increase its capacity ahead of time. Manually adjusting the capacity
    ///  can be useful in certain situations. Each collection specifies its "growing" strategy. Most collections grow by a factor of two
    ///  <c>(New Capacity = Old Capacity * 2)</c>.</remarks>
    procedure Grow();

    ///  <summary>Specifies the current capacity.</summary>
    ///  <returns>A positive number that specifies the number of elements that the collection can hold before it
    ///  needs to grow again.</returns>
    ///  <remarks>The value of this property is greater or equal to the amount of elements in the collection. If this value
    ///  if greater then the number of elements, it means that the collection has some extra capacity to operate upon.</remarks>
    property Capacity: NativeInt read GetCapacity;
  end;
{$ENDREGION}

{$REGION 'Base Collection Classes'}
type
{$HINTS OFF}
  ///  <summary>Base for all reference counted objects in this package.</summary>
  ///  <remarks><see cref="Collections.Base|TRefCountedObject">Collections.Base.TRefCountedObject</see> is designed to be used as a base class for all
  ///  objects that implement interfaces and require reference counting.</remarks>
  TRefCountedObject = class abstract(TInterfacedObject, IInterface)
  private
    FKeepAliveList: TArray<IInterface>;
    FInConstruction: Boolean;

  protected
    ///  <summary>Registers a reference counted object as as keep-alive for this object.</summary>
    ///  <param name="AObject">The object to keep alive.</param>
    ///  <remarks>If <paramref name="AObject"/> is <c>nil</c> nothing happens. Otherwise, this object is
    ///  checked to have a positive reference count. If that is the case, a new interface reference is requested
    ///  and registered internally, preventing the object from being destroyed prematurely.</remarks>
    ///  <exception cref="Collections.Base|ECannotSelfReferenceException"> if trying to keep alive self.</exception>
    procedure KeepObjectAlive(const AObject: TRefCountedObject);

    ///  <summary>Unregisters a reference counted object from the keep-alive list.</summary>
    ///  <param name="AObject">The object to unregister.</param>
    ///  <param name="AFreeObject">Specifies whether to free the object if its reference reaches is zero.</param>
    ///  <remarks>If <paramref name="AObject"/> is <c>nil</c> nothing happens. Otherwise, this object is
    ///  checked to have a positive reference count. If that is the case, the help reference is released.</remarks>
    ///  <exception cref="Collections.Base|ECannotSelfReferenceException"> if trying to release self.</exception>
    procedure ReleaseObject(const AObject: TRefCountedObject;
      const AFreeObject: Boolean = false);

    ///  <summary>Extract an interafce reference for this object.</summary>
    ///  <remarks>If the reference count is zero, then no reference is extracted.</remarks>
    ///  <returns>An interface reference or <c>nil</c>.</returns>
    function ExtractReference(): IInterface;

    ///  <summary>Specifies whether the object is currently being constructed.</summary>
    ///  <returns><c>True</c> if the object is in construction; <c>False</c> otherwise.</returns>
    property Constructing: Boolean read FInConstruction;
  public
    ///  <summary>Initializes the internals of the <see cref="Collections.Base|TRefCountedObject">Collections.Base.TRefCountedObject</see> objects.</summary>
    ///  <remarks>Do not call this method directly. It is part of the object creation process.</remarks>
    class function NewInstance: TObject; override;

    ///  <summary>Initializes the internals of the <see cref="Collections.Base|TRefCountedObject">Collections.Base.TRefCountedObject</see> objects.</summary>
    ///  <remarks>Do not call this method directly. It is part of the object creation process.</remarks>
    procedure AfterConstruction; override;
  end;
{$HINTS ON}

  ///  <summary>Base class for all Enex enumerator objects.</summary>
  ///  <remarks>All Enex collection are expected to provide enumerators that derive from
  ///  this class.</remarks>
  TEnumerator<T> = class abstract(TRefCountedObject, IEnumerator<T>)
    ///  <summary>Returns the current element of the enumerated collection.</summary>
    ///  <remarks>This method is the getter for <c>Current</c> property. Use the property to obtain the element instead.</remarks>
    ///  <returns>The current element of the enumerated collection.</returns>
    function GetCurrent(): T; virtual; abstract;

    ///  <summary>Moves the enumerator to the next element of collection.</summary>
    ///  <remarks>This method is usually called by compiler generated code. Its purpose is to move the "pointer" to the next element in
    ///  the collection (if there are elements left). Also note that many specific enumerator implementations may throw various
    ///  exceptions if the enumerated collection was changed while enumerating.</remarks>
    ///  <returns><c>True</c> if the enumerator succesefully selected the next element; <c>False</c> is there are
    ///  no more elements to be enumerated.</returns>
    function MoveNext(): Boolean; virtual; abstract;

    ///  <summary>Returns the current element of the enumerated collection.</summary>
    ///  <remarks>This property can only return a valid element if <c>MoveNext</c> was priorly called and returned <c>True</c>;
    ///  otherwise the behavior of this property is undefined.
    ///  </remarks>
    ///  <returns>The current element of the enumerated collection.</returns>
    property Current: T read GetCurrent;
  end;

  ///  <summary>Procedural type used by collections to insert custom remove notification code
  ///  into inner collections.</summary>
  ///  <param name="AValue">The value being removed.</param>
  TRemoveNotification<T> = reference to procedure(const AValue: T);

  ///  <summary>Base class for all collections.</summary>
  ///  <remarks>All collections are derived from this base class. It implements most Enex operations based on
  ///  enumerability and introduces serialization support.</remarks>
  TCollection<T> = class abstract(TRefCountedObject, ICollection<T>, IEnumerable<T>)
  protected
    const CDefaultSize = 32;

    ///  <summary>Returns the number of elements in the collection.</summary>
    ///  <returns>A positive value specifying the number of elements in the collection.</returns>
    ///  <remarks>A call to this method can be costly because some
    ///  collections cannot detect the number of stored elements directly, resorting to enumerating themselves.</remarks>
    function GetCount(): NativeInt; virtual;
  public
    ///  <summary>Checks whether the collection is empty.</summary>
    ///  <returns><c>True</c> if the collection is empty; <c>False</c> otherwise.</returns>
    ///  <remarks>This method is the recommended way of detecting if the collection is empty. It is optimized
    ///  in most collections to offer a fast response.</remarks>
    function Empty(): Boolean; virtual;

    ///  <summary>Returns the single element stored in the collection.</summary>
    ///  <returns>The element in collection.</returns>
    ///  <remarks>This method checks if the collection contains just one element, in which case it is returned.</remarks>
    ///  <exception cref="Collections.Base|ECollectionEmptyException">The collection is empty.</exception>
    ///  <exception cref="Collections.Base|ECollectionNotOneException">There is more than one element in the collection.</exception>
    function Single(): T; virtual;

    ///  <summary>Returns the single element stored in the collection, or a default value.</summary>
    ///  <param name="ADefault">The default value returned if there is less or more elements in the collection.</param>
    ///  <returns>The element in the collection if the condition is satisfied; <paramref name="ADefault"/> is returned otherwise.</returns>
    ///  <remarks>This method checks if the collection contains just one element, in which case it is returned. Otherwise
    ///  the value in <paramref name="ADefault"/> is returned.</remarks>
    function SingleOrDefault(const ADefault: T): T; virtual;

    ///  <summary>Copies the values stored in the collection to a given array.</summary>
    ///  <param name="AArray">An array where to copy the contents of the collection.</param>
    ///  <remarks>This method assumes that <paramref name="AArray"/> has enough space to hold the contents of the collection.</remarks>
    ///  <exception cref="Collections.Base|EArgumentOutOfSpaceException">There array is not long enough.</exception>
    procedure CopyTo(var AArray: array of T); overload;

    ///  <summary>Copies the values stored in the collection to a given array.</summary>
    ///  <param name="AArray">An array where to copy the contents of the collection.</param>
    ///  <param name="AStartIndex">The index into the array at which the copying begins.</param>
    ///  <remarks>This method assumes that <paramref name="AArray"/> has enough space to hold the contents of the collection.</remarks>
    ///  <exception cref="SysUtils|EArgumentOutOfRangeException"><paramref name="AStartIndex"/> is out of bounds.</exception>
    ///  <exception cref="Collections.Base|EArgumentOutOfSpaceException">There array is not long enough.</exception>
    procedure CopyTo(var AArray: array of T; const AStartIndex: NativeInt); overload; virtual;

    ///  <summary>Creates a new Delphi array with the contents of the collection.</summary>
    ///  <remarks>The length of the new array is equal to the value of <c>Count</c> property.</remarks>
    function ToArray(): TArray<T>; virtual;

    ///  <summary>Returns a new enumerator object used to enumerate the collection.</summary>
    ///  <remarks>This method is usually called by compiler generated code. It's purpose is to create an enumerator
    ///  object that is used to actually traverse the collection.
    ///  Note that many collections generate enumerators that depend on the state of the collection. If the collection is changed
    ///  after the enumerator has been obtained, the enumerator is considered invalid. All subsequent operations on that enumerator
    ///  will throw exceptions.</remarks>
    ///  <returns>An enumerator object.</returns>
    function GetEnumerator(): IEnumerator<T>; virtual; abstract;

    ///  <summary>Specifies the number of elements in the collection.</summary>
    ///  <returns>A positive value specifying the number of elements in the collection.</returns>
    ///  <remarks>Accesing this property can be costly because some
    ///  collections cannot detect the number of stored elements directly, resorting to enumerating themselves.</remarks>
    property Count: NativeInt read GetCount;
  end;

  ///  <summary>Base class for all non-associative Enex collections.</summary>
  ///  <remarks>All normal Enex collections (ex. list or stack) are derived from this base class.
  ///  It implements the extended Enex operations based on enumerability and introduces functional
  ///  serialization support.</remarks>
  TEnexCollection<T> = class abstract(TCollection<T>, IComparable, IEnexCollection<T>)
  private
    FElementRules: TRules<T>;
    FRemoveNotification: TRemoveNotification<T>;

  protected
    ///  <summary>Specifies a custom remove notification method that will be called by this
    ///  collection when elements are removed.</summary>
    ///  <returns>The notification method.</returns>
    property RemoveNotification: TRemoveNotification<T> read FRemoveNotification write FRemoveNotification;

    ///  <summary>Compares two values for equality.</summary>
    ///  <param name="ALeft">The first value.</param>
    ///  <param name="ARight">The second value.</param>
    ///  <returns><c>True</c> if the values are equal; <c>False</c> otherwise.</returns>
    ///  <remarks>This method uses the equality comparer. If such a comparer was not provided
    ///  a default one is requested.</remarks>
    function ElementsAreEqual(const ALeft, ARight: T): Boolean;

    ///  <summary>Compares two values.</summary>
    ///  <param name="ALeft">The first value.</param>
    ///  <param name="ARight">The second value.</param>
    ///  <returns>A value less than zero if <paramref name="ALeft"/> is less than <paramref name="ARight"/>.
    ///  A value greater than zero if <paramref name="ALeft"/> is greater than <paramref name="ARight"/>. Zero if
    ///  <paramref name="ALeft"/> is equal to <paramref name="ARight"/>.</returns>
    ///  <remarks>This method uses the comparer. If such a comparer was not provided
    ///  a default one is requested.</remarks>
    function CompareElements(const ALeft, ARight: T): NativeInt;

    ///  <summary>Generates a hash code for the given value.</summary>
    ///  <param name="AValue">The value.</param>
    ///  <returns>The calculated hash code.</returns>
    ///  <remarks>This method uses the equality comparer. If such a comparer was not provided
    ///  a default one is requested.</remarks>
    function GetElementHashCode(const AValue: T): NativeInt; overload;

    ///  <summary>Specifies the rule set that describes the stored elements.</summary>
    ///  <returns>A rule set describing the stored elements.</returns>
    property ElementRules: TRules<T> read FElementRules;

    ///  <summary>Override in descendant classed to properly handle elements that are removed from
    ///  the collection.</summary>
    ///  <param name="AElement">The element being removed.</param>
    ///  <remarks>This method is called by the collection when an element is removed and the caller has
    ///  no possibility of obtaining it. For example, a call to <c>Clear</c> calls this method for each element
    ///  of the collection.</remarks>
    procedure HandleElementRemoved(const AElement: T); virtual;

    ///  <summary>Call this method in descendant collections to properly invoke the removal mechanism.</summary>
    ///  <param name="AElement">The element being removed.</param>
    ///  <remarks>This method verifies if a custom removal notification is registered and calls it. Otherwise the normal
    ///  removal mechanisms are involved.</remarks>
    procedure NotifyElementRemoved(const AElement: T);
  public
    ///  <summary>Instantiates this class.</summary>
    ///  <remarks>The default comparer and equality comparer are requested if this constructor is used. Do not call this method if
    ///  you don't know what you are doing.</remarks>
    constructor Create(); overload;

    ///  <summary>Instantiates this class.</summary>
    ///  <param name="ARules">The rules set used by the collection.</param>
    ///  <remarks>The provided rules set is used by this collection. This constructor must be called from descendent collections.</remarks>
    constructor Create(const ARules: TRules<T>); overload;

    ///  <summary>Returns the biggest element.</summary>
    ///  <returns>An element from the collection considered to have the biggest value.</returns>
    ///  <exception cref="Collections.Base|ECollectionEmptyException">The collection is empty.</exception>
    function Max(): T; virtual;

    ///  <summary>Returns the smallest element.</summary>
    ///  <returns>An element from the collection considered to have the smallest value.</returns>
    ///  <exception cref="Collections.Base|ECollectionEmptyException">The collection is empty.</exception>
    function Min(): T; virtual;

    ///  <summary>Returns the first element.</summary>
    ///  <returns>The first element in collection.</returns>
    ///  <exception cref="Collections.Base|ECollectionEmptyException">The collection is empty.</exception>
    function First(): T; virtual;

    ///  <summary>Returns the first element or a default if the collection is empty.</summary>
    ///  <param name="ADefault">The default value returned if the collection is empty.</param>
    ///  <returns>The first element in collection if the collection is not empty; otherwise <paramref name="ADefault"/> is returned.</returns>
    function FirstOrDefault(const ADefault: T): T; virtual;

    ///  <summary>Returns the first element that satisfies the given predicate.</summary>
    ///  <param name="APredicate">The predicate to use.</param>
    ///  <returns>The first element that satisfies the given predicate.</returns>
    ///  <exception cref="Collections.Base|ECollectionEmptyException">The collection is empty.</exception>
    ///  <exception cref="Collections.Base|ECollectionFilteredEmptyException">No elements satisfy the predicate.</exception>
    ///  <exception cref="SysUtils|EArgumentNilException"><paramref name="APredicate"/> is <c>nil</c>.</exception>
    function FirstWhere(const APredicate: TFunc<T, Boolean>): T; virtual;

    ///  <summary>Returns the first element that satisfies the given predicate or a default value.</summary>
    ///  <param name="APredicate">The predicate to use.</param>
    ///  <param name="ADefault">The default value.</param>
    ///  <returns>The first element that satisfies the given predicate; or <paramref name="ADefault"/> otherwise.</returns>
    ///  <exception cref="SysUtils|EArgumentNilException"><paramref name="APredicate"/> is <c>nil</c>.</exception>
    function FirstWhereOrDefault(const APredicate: TFunc<T, Boolean>; const ADefault: T): T; virtual;

    ///  <summary>Returns the first element that does not satisfy the given predicate.</summary>
    ///  <param name="APredicate">The predicate to use.</param>
    ///  <returns>The first element that does not satisfy the given predicate.</returns>
    ///  <exception cref="Collections.Base|ECollectionEmptyException">The collection is empty.</exception>
    ///  <exception cref="Collections.Base|ECollectionFilteredEmptyException">No elements that do not satisfy the predicate.</exception>
    ///  <exception cref="SysUtils|EArgumentNilException"><paramref name="APredicate"/> is <c>nil</c>.</exception>
    function FirstWhereNot(const APredicate: TFunc<T, Boolean>): T;

    ///  <summary>Returns the first element that does not satisfy the given predicate or a default value.</summary>
    ///  <param name="APredicate">The predicate to use.</param>
    ///  <param name="ADefault">The default value.</param>
    ///  <returns>The first element that does not satisfy the given predicate; or <paramref name="ADefault"/> otherwise.</returns>
    ///  <exception cref="SysUtils|EArgumentNilException"><paramref name="APredicate"/> is <c>nil</c>.</exception>
    function FirstWhereNotOrDefault(const APredicate: TFunc<T, Boolean>; const ADefault: T): T;

    ///  <summary>Returns the first element lower than a given value.</summary>
    ///  <param name="ABound">The value to compare against.</param>
    ///  <returns>The first element that satisfies the given condition.</returns>
    ///  <exception cref="Collections.Base|ECollectionEmptyException">The collection is empty.</exception>
    ///  <exception cref="Collections.Base|ECollectionFilteredEmptyException">No elements satisfy the condition.</exception>
    function FirstWhereLower(const ABound: T): T;

    ///  <summary>Returns the first element lower than a given value or a default.</summary>
    ///  <param name="ABound">The value to compare against.</param>
    ///  <param name="ADefault">The default value.</param>
    ///  <returns>The first element that satisfies the given condition; or <paramref name="ADefault"/> otherwise.</returns>
    ///  <exception cref="Collections.Base|ECollectionEmptyException">The collection is empty.</exception>
    ///  <exception cref="Collections.Base|ECollectionFilteredEmptyException">No elements satisfy the condition.</exception>
    function FirstWhereLowerOrDefault(const ABound: T; const ADefault: T): T;

    ///  <summary>Returns the first element lower than or equal to a given value.</summary>
    ///  <param name="ABound">The value to compare against.</param>
    ///  <returns>The first element that satisfies the given condition.</returns>
    ///  <exception cref="Collections.Base|ECollectionEmptyException">The collection is empty.</exception>
    ///  <exception cref="Collections.Base|ECollectionFilteredEmptyException">No elements satisfy the condition.</exception>
    function FirstWhereLowerOrEqual(const ABound: T): T;

    ///  <summary>Returns the first element lower than or equal to a given value or a default.</summary>
    ///  <param name="ABound">The value to compare against.</param>
    ///  <param name="ADefault">The default value.</param>
    ///  <returns>The first element that satisfies the given condition; or <paramref name="ADefault"/> otherwise.</returns>
    ///  <exception cref="Collections.Base|ECollectionEmptyException">The collection is empty.</exception>
    ///  <exception cref="Collections.Base|ECollectionFilteredEmptyException">No elements satisfy the condition.</exception>
    function FirstWhereLowerOrEqualOrDefault(const ABound: T; const ADefault: T): T;

    ///  <summary>Returns the first element greater than a given value.</summary>
    ///  <param name="ABound">The value to compare against.</param>
    ///  <returns>The first element that satisfies the given condition.</returns>
    ///  <exception cref="Collections.Base|ECollectionEmptyException">The collection is empty.</exception>
    ///  <exception cref="Collections.Base|ECollectionFilteredEmptyException">No elements satisfy the condition.</exception>
    function FirstWhereGreater(const ABound: T): T;

    ///  <summary>Returns the first element greater than a given value or a default.</summary>
    ///  <param name="ABound">The value to compare against.</param>
    ///  <param name="ADefault">The default value.</param>
    ///  <returns>The first element that satisfies the given condition; or <paramref name="ADefault"/> otherwise.</returns>
    ///  <exception cref="Collections.Base|ECollectionEmptyException">The collection is empty.</exception>
    ///  <exception cref="Collections.Base|ECollectionFilteredEmptyException">No elements satisfy the condition.</exception>
    function FirstWhereGreaterOrDefault(const ABound: T; const ADefault: T): T;

    ///  <summary>Returns the first element greater than or equal to a given value.</summary>
    ///  <param name="ABound">The value to compare against.</param>
    ///  <returns>The first element that satisfies the given condition.</returns>
    ///  <exception cref="Collections.Base|ECollectionEmptyException">The collection is empty.</exception>
    ///  <exception cref="Collections.Base|ECollectionFilteredEmptyException">No elements satisfy the condition.</exception>
    function FirstWhereGreaterOrEqual(const ABound: T): T;

    ///  <summary>Returns the first element greater than or equal to a given value or a default.</summary>
    ///  <param name="ABound">The value to compare against.</param>
    ///  <param name="ADefault">The default value.</param>
    ///  <returns>The first element that satisfies the given condition; or <paramref name="ADefault"/> otherwise.</returns>
    ///  <exception cref="Collections.Base|ECollectionEmptyException">The collection is empty.</exception>
    ///  <exception cref="Collections.Base|ECollectionFilteredEmptyException">No elements satisfy the condition.</exception>
    function FirstWhereGreaterOrEqualOrDefault(const ABound: T; const ADefault: T): T;

    ///  <summary>Returns the first element situated within the given bounds.</summary>
    ///  <param name="ALower">The lower bound.</param>
    ///  <param name="AHigher">The higher bound.</param>
    ///  <returns>The first element that satisfies the given condition.</returns>
    ///  <exception cref="Collections.Base|ECollectionEmptyException">The collection is empty.</exception>
    ///  <exception cref="Collections.Base|ECollectionFilteredEmptyException">No elements satisfy the condition.</exception>
    function FirstWhereBetween(const ALower, AHigher: T): T;

    ///  <summary>Returns the first element situated within the given bounds or a default.</summary>
    ///  <param name="ALower">The lower bound.</param>
    ///  <param name="AHigher">The higher bound.</param>
    ///  <param name="ADefault">The default value.</param>
    ///  <returns>The first element that satisfies the given condition; or <paramref name="ADefault"/> otherwise.</returns>
    ///  <exception cref="Collections.Base|ECollectionEmptyException">The collection is empty.</exception>
    ///  <exception cref="Collections.Base|ECollectionFilteredEmptyException">No elements satisfy the condition.</exception>
    function FirstWhereBetweenOrDefault(const ALower, AHigher: T; const ADefault: T): T;

    ///  <summary>Returns the last element.</summary>
    ///  <returns>The last element in collection.</returns>
    ///  <exception cref="Collections.Base|ECollectionEmptyException">The collection is empty.</exception>
    function Last(): T; virtual;

    ///  <summary>Returns the last element or a default if the collection is empty.</summary>
    ///  <param name="ADefault">The default value returned if the collection is empty.</param>
    ///  <returns>The last element in collection if the collection is not empty; otherwise <paramref name="ADefault"/> is returned.</returns>
    function LastOrDefault(const ADefault: T): T; virtual;

    ///  <summary>Aggregates a value based on the collection's elements.</summary>
    ///  <param name="AAggregator">The aggregator method.</param>
    ///  <returns>A value that contains the collection's aggregated value.</returns>
    ///  <remarks>This method returns the first element if the collection only has one element. Otherwise,
    ///  <paramref name="AAggregator"/> is invoked for each two elements (first and second; then the result of the first two
    ///  and the third, and so on). The simplest example of aggregation is the "sum" operation where you can obtain the sum of all
    ///  elements in the value.</remarks>
    ///  <exception cref="SysUtils|EArgumentNilException"><paramref name="AAggregator"/> is <c>nil</c>.</exception>
    ///  <exception cref="Collections.Base|ECollectionEmptyException">The collection is empty.</exception>
    function Aggregate(const AAggregator: TFunc<T, T, T>): T; virtual;

    ///  <summary>Aggregates a value based on the collection's elements.</summary>
    ///  <param name="AAggregator">The aggregator method.</param>
    ///  <param name="ADefault">The default value returned if the collection is empty.</param>
    ///  <returns>A value that contains the collection's aggregated value. If the collection is empty, <paramref name="ADefault"/> is returned.</returns>
    ///  <remarks>This method returns the first element if the collection only has one element. Otherwise,
    ///  <paramref name="AAggregator"/> is invoked for each two elements (first and second; then the result of the first two
    ///  and the third, and so on). The simplest example of aggregation is the "sum" operation where you can obtain the sum of all
    ///  elements in the value.</remarks>
    ///  <exception cref="SysUtils|EArgumentNilException"><paramref name="AAggregator"/> is <c>nil</c>.</exception>
    function AggregateOrDefault(const AAggregator: TFunc<T, T, T>; const ADefault: T): T; virtual;

    ///  <summary>Returns the element at a given position.</summary>
    ///  <param name="AIndex">The index from which to return the element.</param>
    ///  <returns>The element at the specified position.</returns>
    ///  <remarks>This method is slow for collections that cannot reference their elements by indexes, for example linked lists.</remarks>
    ///  <exception cref="Collections.Base|ECollectionEmptyException">The collection is empty.</exception>
    ///  <exception cref="SysUtils|EArgumentOutOfRangeException"><paramref name="AIndex"/> is out of bounds.</exception>
    function ElementAt(const AIndex: NativeInt): T; virtual;

    ///  <summary>Returns the element at a given position.</summary>
    ///  <param name="AIndex">The index from which to return the element.</param>
    ///  <param name="ADefault">The default value returned if the collection is empty.</param>
    ///  <returns>The element at the specified position if the collection is not empty and the position is not out of bounds; otherwise
    ///  the value of <paramref name="ADefault"/> is returned.</returns>
    ///  <remarks>This method is slow for collections that cannot reference their elements by indexes, for example linked lists.</remarks>
    function ElementAtOrDefault(const AIndex: NativeInt; const ADefault: T): T; virtual;

    ///  <summary>Check whether at least one element in the collection satisfies a given predicate.</summary>
    ///  <param name="APredicate">The predicate to check for each element.</param>
    ///  <returns><c>True</c> if at least one element satisfies a given predicate; <c>False</c> otherwise.</returns>
    ///  <remarks>This method traverses the whole collection and checks the value of the predicate for each element. This method
    ///  stops on the first element for which the predicate returns <c>True</c>. The logical equivalent of this operation is "OR".</remarks>
    ///  <exception cref="SysUtils|EArgumentNilException"><paramref name="APredicate"/> is <c>nil</c>.</exception>
    function Any(const APredicate: TFunc<T, Boolean>): Boolean; virtual;

    ///  <summary>Checks that all elements in the collection satisfies a given predicate.</summary>
    ///  <param name="APredicate">The predicate to check for each element.</param>
    ///  <returns><c>True</c> if all elements satisfy a given predicate; <c>False</c> otherwise.</returns>
    ///  <remarks>This method traverses the whole collection and checks the value of the predicate for each element. This method
    ///  stops on the first element for which the predicate returns <c>False</c>. The logical equivalent of this operation is "AND".</remarks>
    ///  <exception cref="SysUtils|EArgumentNilException"><paramref name="APredicate"/> is <c>nil</c>.</exception>
    function All(const APredicate: TFunc<T, Boolean>): Boolean; virtual;

    ///  <summary>Checks whether the elements in this collection are equal to the elements in another collection.</summary>
    ///  <param name="ACollection">The collection to compare to.</param>
    ///  <returns><c>True</c> if the collections are equal; <c>False</c> if the collections are different.</returns>
    ///  <remarks>This method checks that each element at position X in this collection is equal to an element at position X in
    ///  the provided collection. If the number of elements in both collections is different, then the collections are considered different.
    ///  Note that comparison of element is done using the rule set used by this collection. This means that comparing this collection
    ///  to another one might yield a different result than comparing the other collection to this one.</remarks>
    ///  <exception cref="SysUtils|EArgumentNilException"><paramref name="ACollection"/> is <c>nil</c>.</exception>
    function EqualsTo(const ACollection: IEnumerable<T>): Boolean; virtual;

    ///  <summary>Selects only the elements that satisfy a given rule.</summary>
    ///  <param name="APredicate">The predicate that represents the rule.</param>
    ///  <returns>A new collection that contains only the elements that satisfy the given rule.</returns>
    ///  <exception cref="SysUtils|EArgumentNilException"><paramref name="APredicate"/> is <c>nil</c>.</exception>
    function Where(const APredicate: TFunc<T, Boolean>): IEnexCollection<T>;

    ///  <summary>Selects only the elements that do not satisfy a given rule.</summary>
    ///  <param name="APredicate">The predicate that represents the rule.</param>
    ///  <returns>A new collection that contains only the elements that do not satisfy the given rule.</returns>
    ///  <exception cref="SysUtils|EArgumentNilException"><paramref name="APredicate"/> is <c>nil</c>.</exception>
    function WhereNot(const APredicate: TFunc<T, Boolean>): IEnexCollection<T>;

    ///  <summary>Selects only the elements that are less than a given value.</summary>
    ///  <param name="ABound">The element to compare against.</param>
    ///  <returns>A new collection that contains only the elements that satisfy the relationship.</returns>
    function WhereLower(const ABound: T): IEnexCollection<T>;

    ///  <summary>Selects only the elements that are less than or equal to a given value.</summary>
    ///  <param name="ABound">The element to compare against.</param>
    ///  <returns>A new collection that contains only the elements that satisfy the relationship.</returns>
    function WhereLowerOrEqual(const ABound: T): IEnexCollection<T>;

    ///  <summary>Selects only the elements that are greater than a given value.</summary>
    ///  <param name="ABound">The element to compare against.</param>
    ///  <returns>A new collection that contains only the elements that satisfy the relationship.</returns>
    function WhereGreater(const ABound: T): IEnexCollection<T>;

    ///  <summary>Selects only the elements that are greater than or equal to a given value.</summary>
    ///  <param name="ABound">The element to compare against.</param>
    ///  <returns>A new collection that contains only the elements that satisfy the relationship.</returns>
    function WhereGreaterOrEqual(const ABound: T): IEnexCollection<T>;

    ///  <summary>Selects only the elements whose values are contained whithin a given interval.</summary>
    ///  <param name="ALower">The lower bound.</param>
    ///  <param name="AHigher">The upper bound.</param>
    ///  <returns>A new collection that contains only the elements that satisfy the relationship.</returns>
    ///  <remarks>The elements that are equal to the lower or upper bound are also included.</remarks>
    function WhereBetween(const ALower, AHigher: T): IEnexCollection<T>;

    ///  <summary>Selects all the elements from the collection excluding duplicates.</summary>
    ///  <returns>A new collection that contains the distinct elements.</returns>
    function Distinct(): IEnexCollection<T>; virtual;

    ///  <summary>Returns a new ordered collection that contains the elements from this collection.</summary>
    ///  <param name="AAscending">Specifies whether the elements are ordered in an ascending or descending way.</param>
    ///  <returns>A new ordered collection.</returns>
    function Ordered(const AAscending: Boolean = true): IEnexCollection<T>; overload; virtual;

    ///  <summary>Returns a new ordered collection that contains the elements from this collection.</summary>
    ///  <param name="ASortProc">The comparison method.</param>
    ///  <returns>A new ordered collection.</returns>
    ///  <exception cref="SysUtils|EArgumentNilException"><paramref name="ASortProc"/> is <c>nil</c>.</exception>
    function Ordered(const ASortProc: TComparison<T>): IEnexCollection<T>; overload; virtual;

    ///  <summary>Revereses the contents of the collection.</summary>
    ///  <returns>A new collection that contains the elements from this collection but in reverse order.</returns>
    function Reversed(): IEnexCollection<T>; virtual;

    ///  <summary>Concatenates this collection with another collection.</summary>
    ///  <param name="ACollection">A collection to concatenate.</param>
    ///  <returns>A new collection that contains the elements from this collection followed by elements
    ///  from the given collection.</returns>
    ///  <exception cref="SysUtils|EArgumentNilException"><paramref name="ACollection"/> is <c>nil</c>.</exception>
    function Concat(const ACollection: IEnexCollection<T>): IEnexCollection<T>;

    ///  <summary>Creates a new collection that contains the elements from both collections, taken a single time.</summary>
    ///  <param name="ACollection">The collection to unify with.</param>
    ///  <returns>A new collection that contains the elements from this collection followed by elements
    ///  from the given collection except the elements that already are present in this collection. This operation can be seen as
    ///  a "concat" operation followed by a "distinct" operation. </returns>
    ///  <exception cref="SysUtils|EArgumentNilException"><paramref name="ACollection"/> is <c>nil</c>.</exception>
    function Union(const ACollection: IEnexCollection<T>): IEnexCollection<T>;

    ///  <summary>Creates a new collection that contains the elements from this collection minus the ones in the given collection.</summary>
    ///  <param name="ACollection">The collection to exclude.</param>
    ///  <returns>A new collection that contains the elements from this collection minus those elements that are common between
    ///  this and the given collection.</returns>
    ///  <exception cref="SysUtils|EArgumentNilException"><paramref name="ACollection"/> is <c>nil</c>.</exception>
    function Exclude(const ACollection: IEnexCollection<T>): IEnexCollection<T>;

    ///  <summary>Creates a new collection that contains the elements that are present in both collections.</summary>
    ///  <param name="ACollection">The collection to interset with.</param>
    ///  <returns>A new collection that contains the elements that are common to both collections.</returns>
    ///  <exception cref="SysUtils|EArgumentNilException"><paramref name="ACollection"/> is <c>nil</c>.</exception>
    function Intersect(const ACollection: IEnexCollection<T>): IEnexCollection<T>;

    ///  <summary>Select the elements whose indexes are located in the given range.</summary>
    ///  <param name="AStart">The lower bound.</param>
    ///  <param name="AEnd">The upper bound.</param>
    ///  <returns>A new collection that contains the elements whose indexes in this collection are located between <paramref name="AStart"/>
    ///  and <paramref name="AEnd"/>. Note that this method does not check the indexes. This means that a bad combination of parameters will
    ///  simply result in an empty or incorrect result.</returns>
    function Range(const AStart, AEnd: NativeInt): IEnexCollection<T>;

    ///  <summary>Selects only a given amount of elements.</summary>
    ///  <param name="ACount">The number of elements to select.</param>
    ///  <returns>A new collection that contains only the first <paramref name="ACount"/> elements.</returns>
    ///  <exception cref="SysUtils|EArgumentOutOfRangeException"><paramref name="ACount"/> is zero.</exception>
    function Take(const ACount: NativeInt): IEnexCollection<T>;

    ///  <summary>Selects all the elements from the collection while a given rule is satisfied.</summary>
    ///  <param name="APredicate">The rule to satisfy.</param>
    ///  <returns>A new collection that contains the selected elements.</returns>
    ///  <remarks>This method selects all elements from the collection while the given rule is satisfied.</remarks>
    ///  <exception cref="SysUtils|EArgumentNilException"><paramref name="APredicate"/> is <c>nil</c>.</exception>
    function TakeWhile(const APredicate: TFunc<T, Boolean>): IEnexCollection<T>;

    ///  <summary>Selects all the elements from the collection while elements are lower than a given value.</summary>
    ///  <param name="ABound">The value to check against.</param>
    ///  <returns>A new collection that contains the selected elements.</returns>
    ///  <remarks>This method selects all elements from the collection while the given rule is satisfied.</remarks>
    function TakeWhileLower(const ABound: T): IEnexCollection<T>;

    ///  <summary>Selects all the elements from the collection while elements are lower than
    ///  or equal to a given value.</summary>
    ///  <param name="ABound">The value to check against.</param>
    ///  <returns>A new collection that contains the selected elements.</returns>
    ///  <remarks>This method selects all elements from the collection while the given rule is satisfied.</remarks>
    function TakeWhileLowerOrEqual(const ABound: T): IEnexCollection<T>;

    ///  <summary>Selects all the elements from the collection while elements are greater than
    ///  a given value.</summary>
    ///  <param name="ABound">The value to check against.</param>
    ///  <returns>A new collection that contains the selected elements.</returns>
    ///  <remarks>This method selects all elements from the collection while the given rule is satisfied.</remarks>
    function TakeWhileGreater(const ABound: T): IEnexCollection<T>;

    ///  <summary>Selects all the elements from the collection while elements are greater than
    ///  or equal to a given value.</summary>
    ///  <param name="ABound">The value to check against.</param>
    ///  <returns>A new collection that contains the selected elements.</returns>
    ///  <remarks>This method selects all elements from the collection while the given rule is satisfied.</remarks>
    function TakeWhileGreaterOrEqual(const ABound: T): IEnexCollection<T>;

    ///  <summary>Selects all the elements from the collection while elements are between a given range of values.</summary>
    ///  <param name="ALower">The lower bound.</param>
    ///  <param name="AHigher">The higher bound.</param>
    ///  <returns>A new collection that contains the selected elements.</returns>
    ///  <remarks>This method selects all elements from the collection while the given rule is satisfied.</remarks>
    function TakeWhileBetween(const ALower, AHigher: T): IEnexCollection<T>;

    ///  <summary>Skips a given amount of elements.</summary>
    ///  <param name="ACount">The number of elements to skip.</param>
    ///  <returns>A new collection that contains the elements that were not skipped.</returns>
    ///  <exception cref="SysUtils|EArgumentOutOfRangeException"><paramref name="ACount"/> is zero.</exception>
    function Skip(const ACount: NativeInt): IEnexCollection<T>;

    ///  <summary>Skips all the elements from the collection while a given rule is satisfied.</summary>
    ///  <param name="APredicate">The rule to satisfy.</param>
    ///  <returns>A new collection that contains the elements that were not skipped.</returns>
    ///  <exception cref="SysUtils|EArgumentNilException"><paramref name="APredicate"/> is <c>nil</c>.</exception>
    function SkipWhile(const APredicate: TFunc<T, Boolean>): IEnexCollection<T>;

    ///  <summary>Skips all the elements from the collection while elements are lower than a given value.</summary>
    ///  <param name="ABound">The value to check.</param>
    ///  <returns>A new collection that contains the elements that were not skipped.</returns>
    function SkipWhileLower(const ABound: T): IEnexCollection<T>;

    ///  <summary>Skips all the elements from the collection while elements are lower than or equal to a given value.</summary>
    ///  <param name="ABound">The value to check.</param>
    ///  <returns>A new collection that contains the elements that were not skipped.</returns>
    function SkipWhileLowerOrEqual(const ABound: T): IEnexCollection<T>;

    ///  <summary>Skips all the elements from the collection while elements are greater than a given value.</summary>
    ///  <param name="ABound">The value to check.</param>
    ///  <returns>A new collection that contains the elements that were not skipped.</returns>
    function SkipWhileGreater(const ABound: T): IEnexCollection<T>;

    ///  <summary>Skips all the elements from the collection while elements are greater than or equal to a given value.</summary>
    ///  <param name="ABound">The value to check.</param>
    ///  <returns>A new collection that contains the elements that were not skipped.</returns>
    function SkipWhileGreaterOrEqual(const ABound: T): IEnexCollection<T>;

    ///  <summary>Skips all the elements from the collection while elements are between a given range of values.</summary>
    ///  <param name="ALower">The lower bound.</param>
    ///  <param name="AHigher">The higher bound.</param>
    ///  <returns>A new collection that contains the elements that were not skipped.</returns>
    function SkipWhileBetween(const ALower, AHigher: T): IEnexCollection<T>;

    ///  <summary>Exposes a type that provides extended Enex operations such as "select".</summary>
    ///  <returns>A record that exposes more Enex operations that otherwise would be impossible.</returns>
    function Op: TEnexExtOps<T>;

    ///  <summary>Creates a new list containing the elements of this collection.</summary>
    ///  <returns>A list containing the elements copied from this collection.</returns>
    ///  <remarks>This method also copies the rule set of this collection. Be careful if the rule set
    ///  performs cleanup on the elements.</remarks>
    function ToList(): IList<T>;

    ///  <summary>Creates a new set containing the elements of this collection.</summary>
    ///  <returns>A set containing the elements copied from this collection.</returns>
    ///  <remarks>This method also copies the rule set of this collection. Be careful if the rule set
    ///  performs cleanup on the elements.</remarks>
    function ToSet(): ISet<T>;

    ///  <summary>Compares the elements in this collection to another collection.</summary>
    ///  <param name="AObject">The instance to compare against.</param>
    ///  <returns>An integer value depicting the result of the comparison operation.
    ///  If the result is less than zero, <c>Self</c> is less than <paramref name="AObject"/>. If the result is zero,
    ///  <c>Self</c> is equal to <paramref name="AObject"/>. And finally, if the result is greater than zero, <c>Self</c> is greater
    ///  than <paramref name="AObject"/>.</returns>
    function CompareTo(AObject: TObject): Integer;

    ///  <summary>Generates the hash code of all the elements in the collection.</summary>
    ///  <returns>An integer value representing the hash codes of all the elements in the collection.</returns>
    function GetHashCode(): Integer; override;

    ///  <summary>Checks whether this collection is equal to another collection.</summary>
    ///  <param name="Obj">The collection to check against.</param>
    ///  <returns><c>True</c> if the collections are equal; <c>False</c> otherwise.</returns>
    ///  <remarks>This method checks whether <paramref name="Obj"/> is not <c>nil</c>, and that
    ///  <paramref name="Obj"/> is an Enex collection. Then, elements are checked for equality one by one.</remarks>
    function Equals(Obj: TObject): Boolean; override;

    ///  <summary>Generates a new collection that contains a given value for a given number of times.</summary>
    ///  <param name="AElement">The element to fill the collection with.</param>
    ///  <param name="ACount">The number of times the element is present in the collection (the length of the collection).</param>
    ///  <param name="ARules">The rule set describing the elements in the new collection.</param>
    ///  <returns>A new collection containing the <paramref name="AElement"/>, <paramref name="ACount"/> times.</returns>
    ///  <exception cref="SysUtils|EArgumentNilException"><paramref name="AElement"/> is <c>nil</c>.</exception>
    ///  <exception cref="SysUtils|EArgumentOutOfRangeException"><paramref name="ACount"/> is zero or less.</exception>
    class function Fill(const AElement: T; const ACount: NativeInt; const ARules: TRules<T>): IEnexCollection<T>; overload; static;

    ///  <summary>Generates a new collection that contains a given value for a given number of times.</summary>
    ///  <param name="AElement">The element to fill the collection with.</param>
    ///  <param name="ACount">The number of times the element is present in the collection (the length of the collection).</param>
    ///  <returns>A new collection containing the <paramref name="AElement"/>, <paramref name="ACount"/> times.</returns>
    ///  <exception cref="SysUtils|EArgumentOutOfRangeException"><paramref name="ACount"/> is zero or less.</exception>
    class function Fill(const AElement: T; const ACount: NativeInt): IEnexCollection<T>; overload; static;
  end;

  ///  <summary>Base class for all associative Enex collections.</summary>
  ///  <remarks>All associative Enex collections (ex. dictionary or multi-map) are derived from this base class.
  ///  It implements the extended Enex operations based on enumerability and introduces functional
  ///  serialization support.</remarks>
  TEnexAssociativeCollection<TKey, TValue> = class abstract(TCollection<TPair<TKey, TValue>>,
      IEnexAssociativeCollection<TKey, TValue>)
  private
    FKeyRules: TRules<TKey>;
    FValueRules: TRules<TValue>;
    FKeyRemoveNotification: TRemoveNotification<TKey>;
    FValueRemoveNotification: TRemoveNotification<TValue>;

  protected
    ///  <summary>Specifies a custom remove notification method that will be called by this
    ///  collection when keys are removed.</summary>
    ///  <returns>The notification method.</returns>
    property KeyRemoveNotification: TRemoveNotification<TKey> read FKeyRemoveNotification write FKeyRemoveNotification;

    ///  <summary>Specifies a custom remove notification method that will be called by this
    ///  collection when values are removed.</summary>
    ///  <returns>The notification method.</returns>
    property ValueRemoveNotification: TRemoveNotification<TValue> read FValueRemoveNotification write FValueRemoveNotification;

    ///  <summary>Compares two keys for equality.</summary>
    ///  <param name="ALeft">The first key.</param>
    ///  <param name="ARight">The second key.</param>
    ///  <returns><c>True</c> if the keys are equal; <c>False</c> otherwise.</returns>
    ///  <remarks>This method uses the equality comparer. If such a comparer was not provided
    ///  a default one is requested.</remarks>
    function KeysAreEqual(const ALeft, ARight: TKey): Boolean;

    ///  <summary>Compares two keys.</summary>
    ///  <param name="ALeft">The first key.</param>
    ///  <param name="ARight">The second key.</param>
    ///  <returns>A value less than zero if <paramref name="ALeft"/> is less than <paramref name="ARight"/>.
    ///  A value greater than zero if <paramref name="ALeft"/> is greater than <paramref name="ARight"/>. Zero if
    ///  <paramref name="ALeft"/> is equal to <paramref name="ARight"/>.</returns>
    ///  <remarks>This method uses the comparer. If such a comparer was not provided
    ///  a default one is requested.</remarks>
    function CompareKeys(const ALeft, ARight: TKey): NativeInt;

    ///  <summary>Generates a hash code for the given key.</summary>
    ///  <param name="AValue">The key.</param>
    ///  <returns>The calculated hash code.</returns>
    ///  <remarks>This method uses the equality comparer. If such a comparer was not provided
    ///  a default one is requested.</remarks>
    function GetKeyHashCode(const AValue: TKey): NativeInt; overload;

    ///  <summary>Compares two values for equality.</summary>
    ///  <param name="ALeft">The first value.</param>
    ///  <param name="ARight">The second value.</param>
    ///  <returns><c>True</c> if the keys are equal; <c>False</c> otherwise.</returns>
    ///  <remarks>This method uses the equality comparer. If such a comparer was not provided
    ///  a default one is requested.</remarks>
    function ValuesAreEqual(const ALeft, ARight: TValue): Boolean;

    ///  <summary>Compares two values.</summary>
    ///  <param name="ALeft">The first value.</param>
    ///  <param name="ARight">The second value.</param>
    ///  <returns>A value less than zero if <paramref name="ALeft"/> is less than <paramref name="ARight"/>.
    ///  A value greater than zero if <paramref name="ALeft"/> is greater than <paramref name="ARight"/>. Zero if
    ///  <paramref name="ALeft"/> is equal to <paramref name="ARight"/>.</returns>
    ///  <remarks>This method uses the comparer. If such a comparer was not provided
    ///  a default one is requested.</remarks>
    function CompareValues(const ALeft, ARight: TValue): NativeInt;

    ///  <summary>Generates a hash code for the given value.</summary>
    ///  <param name="AValue">The value.</param>
    ///  <returns>The calculated hash code.</returns>
    ///  <remarks>This method uses the equality comparer. If such a comparer was not provided
    ///  a default one is requested.</remarks>
    function GetValueHashCode(const AValue: TValue): NativeInt; overload;

    ///  <summary>Specifies the rule set that describes the keys of the stored pairs.</summary>
    ///  <returns>A rule set describing the keys.</returns>
    property KeyRules: TRules<TKey> read FKeyRules;

    ///  <summary>Specifies the rule set that describes the values of the stored pairs.</summary>
    ///  <returns>A rule set describing the values.</returns>
    property ValueRules: TRules<TValue> read FValueRules;

    ///  <summary>Override in descendent classed to properly handle keys that are removed from
    ///  the collection.</summary>
    ///  <param name="AKey">The key being removed.</param>
    ///  <remarks>This method is called by the collection when a key is removed and the caller has
    ///  no possibility of obtaining it. For example, a call to <c>Clear</c> calls this method for each key
    ///  of the collection.</remarks>
    procedure HandleKeyRemoved(const AKey: TKey); virtual;

    ///  <summary>Override in descendaet classed to properly handle values that are removed from
    ///  the collection.</summary>
    ///  <param name="AValue">The key being removed.</param>
    ///  <remarks>This method is called by the collection when a value is removed and the caller has
    ///  no possibility of obtaining it. For example, a call to <c>Clear</c> calls this method for each value
    ///  of the collection.</remarks>
    procedure HandleValueRemoved(const AValue: TValue); virtual;

    ///  <summary>Call this method in descendent collections to properly invoke the removal mechanism.</summary>
    ///  <param name="AKey">The key being removed.</param>
    ///  <remarks>This method verifies whether a custom removal notification is registered and calls it. Otherwise the normal
    ///  removal mechanisms are involved.</remarks>
    procedure NotifyKeyRemoved(const AKey: TKey);

    ///  <summary>Call this method in descendent collections to properly invoke the removal mechanism.</summary>
    ///  <param name="AValue">The key being removed.</param>
    ///  <remarks>This method verifies whether a custom removal notification is registered and calls it. Otherwise the normal
    ///  removal mechanisms are involved.</remarks>
    procedure NotifyValueRemoved(const AValue: TValue);
  public
    ///  <summary>Instantiates this class.</summary>
    ///  <remarks>The default comparer and equality comparer are requested if this constructor is used. Do not call this method if
    ///  you don't know what you are doing.</remarks>
    constructor Create(); overload;

    ///  <summary>Instantiates this class.</summary>
    ///  <param name="AKeyRules">The rules set used by the collection for its keys.</param>
    ///  <param name="AValueRules">The rules set used by the collection for its values.</param>
    ///  <remarks>The provided rules set is used by this collection. This constructor must be called from descendent collections.</remarks>
    constructor Create(const AKeyRules: TRules<TKey>; const AValueRules: TRules<TValue>); overload;

    ///  <summary>Returns the value associated with the given key.</summary>
    ///  <param name="AKey">The key for which to return the associated value.</param>
    ///  <returns>The value associated with the given key.</returns>
    ///  <exception cref="Collections.Base|EKeyNotFoundException">No such key in the collection.</exception>
    function ValueForKey(const AKey: TKey): TValue; virtual;

    ///  <summary>Checks whether the collection contains a given key-value pair.</summary>
    ///  <param name="AKey">The key part of the pair.</param>
    ///  <param name="AValue">The value part of the pair.</param>
    ///  <returns><c>True</c> if the given key-value pair exists; <c>False</c> otherwise.</returns>
    function KeyHasValue(const AKey: TKey; const AValue: TValue): Boolean; virtual;

    ///  <summary>Returns the biggest key.</summary>
    ///  <returns>The biggest key stored in the collection.</returns>
    ///  <exception cref="Collections.Base|ECollectionEmptyException">The collection is empty.</exception>
    function MaxKey(): TKey; virtual;

    ///  <summary>Returns the smallest key.</summary>
    ///  <returns>The smallest key stored in the collection.</returns>
    ///  <exception cref="Collections.Base|ECollectionEmptyException">The collection is empty.</exception>
    function MinKey(): TKey; virtual;

    ///  <summary>Returns the biggest value.</summary>
    ///  <returns>The biggest value stored in the collection.</returns>
    ///  <exception cref="Collections.Base|ECollectionEmptyException">The collection is empty.</exception>
    function MaxValue(): TValue; virtual;

    ///  <summary>Returns the smallest value.</summary>
    ///  <returns>The smallest value stored in the collection.</returns>
    ///  <exception cref="Collections.Base|ECollectionEmptyException">The collection is empty.</exception>
    function MinValue(): TValue; virtual;

    ///  <summary>Checks whether this collection includes the key-value pairs in another collection.</summary>
    ///  <param name="ACollection">The collection to check against.</param>
    ///  <returns><c>True</c> if this collection includes the elements in another; <c>False</c> otherwise.</returns>
    function Includes(const ACollection: IEnumerable<TPair<TKey, TValue>>): Boolean; virtual;

    ///  <summary>Returns an Enex collection that contains only the keys.</summary>
    ///  <returns>An Enex collection that contains all the keys stored in the collection.</returns>
    function SelectKeys(): IEnexCollection<TKey>; virtual;

    ///  <summary>Returns an Enex collection that contains only the values.</summary>
    ///  <returns>An Enex collection that contains all the values stored in the collection.</returns>
    function SelectValues(): IEnexCollection<TValue>; virtual;

    ///  <summary>Selects all the key-value pairs from the collection excluding the duplicates by key.</summary>
    ///  <returns>A new collection that contains the distinct pairs.</returns>
    function DistinctByKeys(): IEnexAssociativeCollection<TKey, TValue>;

    ///  <summary>Selects all the key-value pairs from the collection excluding the duplicates by value.</summary>
    ///  <returns>A new collection that contains the distinct pairs.</returns>
    function DistinctByValues(): IEnexAssociativeCollection<TKey, TValue>;

    ///  <summary>Selects only the key-value pairs that satisfy a given rule.</summary>
    ///  <param name="APredicate">The predicate that represents the rule.</param>
    ///  <returns>A new collection that contains only the pairs that satisfy the given rule.</returns>
    ///  <exception cref="SysUtils|EArgumentNilException"><paramref name="APredicate"/> is <c>nil</c>.</exception>
    function Where(const APredicate: TFunc<TKey, TValue, Boolean>): IEnexAssociativeCollection<TKey, TValue>;

    ///  <summary>Selects only the key-value pairs that do not satisfy a given rule.</summary>
    ///  <param name="APredicate">The predicate that represents the rule.</param>
    ///  <returns>A new collection that contains only the pairs that do not satisfy the given rule.</returns>
    ///  <exception cref="SysUtils|EArgumentNilException"><paramref name="APredicate"/> is <c>nil</c>.</exception>
    function WhereNot(const APredicate: TFunc<TKey, TValue, Boolean>): IEnexAssociativeCollection<TKey, TValue>;

    ///  <summary>Selects only the key-value pairs whose keys are less than a given value.</summary>
    ///  <param name="ABound">The value to compare against.</param>
    ///  <returns>A new collection that contains only the pairs that satisfy the relationship.</returns>
    function WhereKeyLower(const ABound: TKey): IEnexAssociativeCollection<TKey, TValue>;

    ///  <summary>Selects only the key-value pairs whose keys are less than or equal to a given value.</summary>
    ///  <param name="ABound">The value to compare against.</param>
    ///  <returns>A new collection that contains only the pairs that satisfy the relationship.</returns>
    function WhereKeyLowerOrEqual(const ABound: TKey): IEnexAssociativeCollection<TKey, TValue>;

    ///  <summary>Selects only the key-value pairs whose keys are greater than a given value.</summary>
    ///  <param name="ABound">The value to compare against.</param>
    ///  <returns>A new collection that contains only the pairs that satisfy the relationship.</returns>
    function WhereKeyGreater(const ABound: TKey): IEnexAssociativeCollection<TKey, TValue>;

    ///  <summary>Selects only the key-value pairs whose keys are greater than or equal to a given value.</summary>
    ///  <param name="ABound">The value to compare against.</param>
    ///  <returns>A new collection that contains only the pairs that satisfy the relationship.</returns>
    function WhereKeyGreaterOrEqual(const ABound: TKey): IEnexAssociativeCollection<TKey, TValue>;

    ///  <summary>Selects only the key-value pairs whose keys are contained whithin a given interval.</summary>
    ///  <param name="ALower">The lower bound.</param>
    ///  <param name="AHigher">The upper bound.</param>
    ///  <returns>A new collection that contains only the pairs that satisfy the relationship.</returns>
    function WhereKeyBetween(const ALower, AHigher: TKey): IEnexAssociativeCollection<TKey, TValue>;

    ///  <summary>Selects only the key-value pairs whose values are less than a given value.</summary>
    ///  <param name="ABound">The value to compare against.</param>
    ///  <returns>A new collection that contains only the pairs that satisfy the relationship.</returns>
    function WhereValueLower(const ABound: TValue): IEnexAssociativeCollection<TKey, TValue>;

    ///  <summary>Selects only the key-value pairs whose values are less than or equal to a given value.</summary>
    ///  <param name="ABound">The value to compare against.</param>
    ///  <returns>A new collection that contains only the pairs that satisfy the relationship.</returns>
    function WhereValueLowerOrEqual(const ABound: TValue): IEnexAssociativeCollection<TKey, TValue>;

    ///  <summary>Selects only the key-value pairs whose values are greater than a given value.</summary>
    ///  <param name="ABound">The value to compare against.</param>
    ///  <returns>A new collection that contains only the pairs that satisfy the relationship.</returns>
    function WhereValueGreater(const ABound: TValue): IEnexAssociativeCollection<TKey, TValue>;

    ///  <summary>Selects only the key-value pairs whose values are greater than or equal to a given value.</summary>
    ///  <param name="ABound">The value to compare against.</param>
    ///  <returns>A new collection that contains only the pairs that satisfy the relationship.</returns>
    function WhereValueGreaterOrEqual(const ABound: TValue): IEnexAssociativeCollection<TKey, TValue>;

    ///  <summary>Selects only the key-value pairs whose values are contained whithin a given interval.</summary>
    ///  <param name="ALower">The lower bound.</param>
    ///  <param name="AHigher">The upper bound.</param>
    ///  <returns>A new collection that contains only the pairs that satisfy the relationship.</returns>
    function WhereValueBetween(const ALower, AHigher: TValue): IEnexAssociativeCollection<TKey, TValue>;

    ///  <summary>Creates a new dictionary containing the elements of this collection.</summary>
    ///  <returns>A dictionary containing the elements copied from this collection.</returns>
    ///  <remarks>This method also copies the rule set of this collection. Be careful if the rule set
    ///  performs cleanup on the elements.</remarks>
    ///  <exception cref="Collections.Base|EDuplicateKeyException">The collection contains more than
    ///  one key-value pair with the same key.</exception>
    function ToDictionary(): IDictionary<TKey, TValue>;
  end;
{$ENDREGION}

{$REGION 'Exception Support'}
type
  ///  <summary>Thrown when an attempt to call an unsupported default parameterless constructor is made.</summary>
  EDefaultConstructorNotAllowed = class(Exception);

  ///  <summary>Thrown when a <see cref="Collections.Base|TRefCountedObject">Collections.Base.TRefCountedObject</see> tries to keep itself alive.</summary>
  ECannotSelfReferenceException = class(Exception);

{$IF RTLVersion < 22}
  ///  <summary>Thrown when a given argument is <c>nil</c>.</summary>
  ///  <remarks>This exception is normally provided by Delphi XE's SysUtils.pas.</remarks>
  EArgumentNilException = class(EArgumentException);
{$IFEND}

  ///  <summary>Thrown when a given argument combination specifies a smaller range than required.</summary>
  ///  <remarks>This exception is usually used by collections. The exception is thrown when there is not enough
  ///  space in an array to copy the values to.</remarks>
  EArgumentOutOfSpaceException = class(EArgumentOutOfRangeException);

  ///  <summary>Represents all exceptions that are thrown when collections are involved.</summary>
  ECollectionException = class(Exception);

  ///  <summary>Thrown when an enumerator detects that the enumerated collection was changed.</summary>
  ECollectionChangedException = class(ECollectionException);

  ///  <summary>Thrown when a collection was identified to be empty (and it shouldn't have been).</summary>
  ECollectionEmptyException = class(ECollectionException);

  ///  <summary>Thrown when a collection was expected to have only one exception.</summary>
  ECollectionNotOneException = class(ECollectionException);

  ///  <summary>Thrown when a predicated applied to a collection generates a void collection.</summary>
  ECollectionFilteredEmptyException = class(ECollectionException);

  ///  <summary>Thrown when trying to add a key-value pair into a collection that already has that key
  ///  in it.</summary>
  EDuplicateKeyException = class(ECollectionException);

  ///  <summary>Thrown when the key (of a pair) is not found in the collection.</summary>
  EKeyNotFoundException = class(ECollectionException);

  ///  <summary>A static class that offers methods for throwing exceptions.</summary>
  ///  <remarks><see cref="Collections.Base|ExceptionHelper">Collections.Base.ExceptionHelper</see> is used internally in this package to
  ///  throw all kinds of exceptions. This class is useful because it separates the exceptions
  ///  (including the messages) from the rest of the code.</remarks>
  ExceptionHelper = class sealed
  public
    ///  <summary>Internal method. Do not call directly!</summary>
    ///  <remarks>The interface of this function may change in the future.</remarks>
    class procedure Throw_CannotSelfReferenceError();

    ///  <summary>Internal method. Do not call directly!</summary>
    ///  <remarks>The interface of this function may change in the future.</remarks>
    class procedure Throw_ArgumentNilError(const ArgName: String);

    ///  <summary>Internal method. Do not call directly!</summary>
    ///  <remarks>The interface of this function may change in the future.</remarks>
    class procedure Throw_ArgumentOutOfRangeError(const ArgName: String);

    ///  <summary>Internal method. Do not call directly!</summary>
    ///  <remarks>The interface of this function may change in the future.</remarks>
    class procedure Throw_ArgumentOutOfSpaceError(const ArgName: String);

    ///  <summary>Internal method. Do not call directly!</summary>
    ///  <remarks>The interface of this function may change in the future.</remarks>
    class procedure Throw_CollectionChangedError();

    ///  <summary>Internal method. Do not call directly!</summary>
    ///  <remarks>The interface of this function may change in the future.</remarks>
    class procedure Throw_CollectionEmptyError();

    ///  <summary>Internal method. Do not call directly!</summary>
    ///  <remarks>The interface of this function may change in the future.</remarks>
    class procedure Throw_CollectionHasMoreThanOneElement();

    ///  <summary>Internal method. Do not call directly!</summary>
    ///  <remarks>The interface of this function may change in the future.</remarks>
    class procedure Throw_CollectionHasNoFilteredElements();

    ///  <summary>Internal method. Do not call directly!</summary>
    ///  <remarks>The interface of this function may change in the future.</remarks>
    class procedure Throw_DuplicateKeyError(const ArgName: String);

    ///  <summary>Internal method. Do not call directly!</summary>
    ///  <remarks>The interface of this function may change in the future.</remarks>
    class procedure Throw_KeyNotFoundError(const ArgName: String);

    ///  <summary>Internal method. Do not call directly!</summary>
    ///  <remarks>The interface of this function may change in the future.</remarks>
    class procedure Throw_TypeNotAClassError(const TypeName: String);

    ///  <summary>Internal method. Do not call directly!</summary>
    ///  <remarks>The interface of this function may change in the future.</remarks>
    class procedure Throw_TypeDoesNotExposeMember(const MemberName: String);
  end;

resourcestring
  SDefaultParameterlessCtorNotAllowed = 'Default parameterless constructor not allowed!';
  SCannotSelfReference = 'The object cannot self-reference!';
  SNilArgument = 'Argument "%s" is nil. Expected a normal non-disposed object!';
  SOutOfRangeArgument = 'Argument "%s" is out of range. An argument that falls into the required range of values is expected!';
  SOutOfSpaceArgument = 'Argument "%s" does not have enough space to hold the result!';
  SParentCollectionChanged = 'Parent collection has changed. Cannot continue the operation!';
  SKeyNotFound = 'The key given by the "%s" argument was not found in the collection!';
  SDuplicateKey = 'The key given by the "%s" argument was already registered in the collection!';
  SEmptyCollection = 'The collection is empty! The operation cannot be performed!';
  SCollectionHasMoreThanOneElements = 'The collection has more than one element!';
  SCollectionHasNoFilteredElements = 'The applied predicate generates a void collection.';
  STypeNotAClass = 'The type "%s" on which the operation was invoked is not a class!';
  STypeDoesNotExposeMember = 'The type the collection operates on does not expose member "%s"!';
{$ENDREGION}

{$REGION 'Enex Internal Enumerables'}
  //TODO: doc all these classes :(
type
  { The "Where" collection }
  TEnexWhereCollection<T> = class sealed(TEnexCollection<T>)
  private
  type
    { The "Where" enumerator }
    TEnumerator = class(TEnumerator<T>)
    private
      FCollection: TEnexWhereCollection<T>;
      FEnumerator: IEnumerator<T>;

    public
      { Constructor }
      constructor Create(const ACollection: TEnexWhereCollection<T>);

      { Destructor }
      destructor Destroy(); override;

      function GetCurrent(): T; override;
      function MoveNext(): Boolean; override;
    end;

  var
    FCollection: TEnexCollection<T>;
    FPredicate: TFunc<T, Boolean>;
    FInvertResult: Boolean;
  public
    { Constructors }
    constructor Create(const ACollection: TEnexCollection<T>;
      const APredicate: TFunc<T, Boolean>; const AInvertResult: Boolean); overload;

    { Destructor }
    destructor Destroy(); override;

    { IEnumerable<T> }
    function GetEnumerator(): IEnumerator<T>; override;
  end;

  { The "Select" collection }
  TEnexSelectCollection<T, TOut> = class sealed(TEnexCollection<TOut>, IEnexCollection<TOut>)
  private
  type
    { The "Select" enumerator }
    TEnumerator = class(TEnumerator<TOut>)
    private
      FCollection: TEnexSelectCollection<T, TOut>;
      FEnumerator: IEnumerator<T>;
      FCurrent: TOut;

    public
      { Constructor }
      constructor Create(const ACollection: TEnexSelectCollection<T, TOut>);

      { Destructor }
      destructor Destroy(); override;

      function GetCurrent(): TOut; override;
      function MoveNext(): Boolean; override;
    end;

  var
    FCollection: TEnexCollection<T>;
    FSelector: TFunc<T, TOut>;

  protected
    { Enex: Defaults }
    function GetCount(): NativeInt; override;

  public
    { Constructors }
    constructor Create(const ACollection: TEnexCollection<T>; const ASelector: TFunc<T, TOut>; const ARules: TRules<TOut>); overload;

    { Destructor }
    destructor Destroy(); override;

    { IEnumerable<T> }
    function GetEnumerator(): IEnumerator<TOut>; override;

    { Enex Overrides }
    function Empty(): Boolean; override;
    function First(): TOut; override;
    function Last(): TOut; override;
    function Single(): TOut; override;
    function ElementAt(const AIndex: NativeInt): TOut; override;
  end;

  { The "Select Class" collection }
  TEnexSelectClassCollection<T, TOut: class> = class sealed(TEnexCollection<TOut>, IEnexCollection<TOut>)
  private
  type
    { The "Select Class" enumerator }
    TEnumerator = class(TEnumerator<TOut>)
    private
      FCollection: TEnexSelectClassCollection<T, TOut>;
      FEnumerator: IEnumerator<T>;
      FCurrent: TOut;

    public
      { Constructor }
      constructor Create(const ACollection: TEnexSelectClassCollection<T, TOut>);

      { Destructor }
      destructor Destroy(); override;

      function GetCurrent(): TOut; override;
      function MoveNext(): Boolean; override;
    end;

  var
    FCollection: TEnexCollection<T>;

  public
    { Constructors }
    constructor Create(const ACollection: TEnexCollection<T>; const ARules: TRules<TOut>); overload;

    { Destructor }
    destructor Destroy(); override;

    { IEnumerable<T> }
    function GetEnumerator(): IEnumerator<TOut>; override;
  end;

  { The "Concatenation" collection }
  TEnexConcatCollection<T> = class sealed(TEnexCollection<T>)
  private
  type
    { The "Concatenation" enumerator }
    TEnumerator = class(TEnumerator<T>)
    private
      FCollection: TEnexConcatCollection<T>;
      FEnumerator1, FEnumerator2: IEnumerator<T>;

    public
      { Constructor }
      constructor Create(const ACollection: TEnexConcatCollection<T>);

      { Destructor }
      destructor Destroy(); override;

      function GetCurrent(): T; override;
      function MoveNext(): Boolean; override;
    end;

  var
    FCollection1: TEnexCollection<T>;
    FCollection2: IEnexCollection<T>;
  protected
    { ICollection support/hidden }
    function GetCount(): NativeInt; override;

  public
    { Constructors }
    constructor Create(const ACollection1: TEnexCollection<T>; const ACollection2: IEnexCollection<T>); overload;

    { Destructor }
    destructor Destroy(); override;

    { IEnumerable<T> }
    function GetEnumerator(): IEnumerator<T>; override;

    { Enex Overrides }
    function Empty(): Boolean; override;
    function Any(const APredicate: TFunc<T, Boolean>): Boolean; override;
    function All(const APredicate: TFunc<T, Boolean>): Boolean; override;
  end;

  { The "Union" collection }
  TEnexUnionCollection<T> = class sealed(TEnexCollection<T>)
  private
  type
    { The "Union" enumerator }
    TEnumerator = class(TEnumerator<T>)
    private
      FCollection: TEnexUnionCollection<T>;
      FEnumerator1, FEnumerator2: IEnumerator<T>;
      FSet: ISet<T>;

    public
      { Constructor }
      constructor Create(const ACollection: TEnexUnionCollection<T>);

      { Destructor }
      destructor Destroy(); override;

      function GetCurrent(): T; override;
      function MoveNext(): Boolean; override;
    end;

  var
    FCollection1: TEnexCollection<T>;
    FCollection2: IEnexCollection<T>;
  public
    { Constructors }
    constructor Create(const ACollection1: TEnexCollection<T>; const ACollection2: IEnexCollection<T>); overload;

    { Destructor }
    destructor Destroy(); override;

    { IEnumerable<T> }
    function GetEnumerator(): IEnumerator<T>; override;
  end;

  { The "Exclusion" collection }
  TEnexExclusionCollection<T> = class sealed(TEnexCollection<T>)
  private
  type
    { The "Exclusion" enumerator }
    TEnumerator = class(TEnumerator<T>)
    private
      FCollection: TEnexExclusionCollection<T>;
      FEnumerator: IEnumerator<T>;
      FSet: ISet<T>;

    public
      { Constructor }
      constructor Create(const ACollection: TEnexExclusionCollection<T>);

      { Destructor }
      destructor Destroy(); override;

      function GetCurrent(): T; override;
      function MoveNext(): Boolean; override;
    end;

  var
    FCollection1: TEnexCollection<T>;
    FCollection2: IEnexCollection<T>;
  public
    { Constructors }
    constructor Create(const ACollection1: TEnexCollection<T>; const ACollection2: IEnexCollection<T>); overload;

    { Destructor }
    destructor Destroy(); override;

    { IEnumerable<T> }
    function GetEnumerator(): IEnumerator<T>; override;
  end;

  { The "Intersection" collection }
  TEnexIntersectionCollection<T> = class sealed(TEnexCollection<T>)
  private
  type
    { The "Intersection" enumerator }
    TEnumerator = class(TEnumerator<T>)
    private
      FCollection: TEnexIntersectionCollection<T>;
      FEnumerator: IEnumerator<T>;
      FSet: ISet<T>;

    public
      { Constructor }
      constructor Create(const ACollection: TEnexIntersectionCollection<T>);

      { Destructor }
      destructor Destroy(); override;

      function GetCurrent(): T; override;
      function MoveNext(): Boolean; override;
    end;

  var
    FCollection1: TEnexCollection<T>;
    FCollection2: IEnexCollection<T>;
  public
    { Constructors }
    constructor Create(const ACollection1: TEnexCollection<T>; const ACollection2: IEnexCollection<T>); overload;

    { Destructor }
    destructor Destroy(); override;

    { IEnumerable<T> }
    function GetEnumerator(): IEnumerator<T>; override;
  end;

  { The "Distinct" collection }
  TEnexDistinctCollection<T> = class sealed(TEnexCollection<T>)
  private
  type
    { The "Distinct" enumerator }
    TEnumerator = class(TEnumerator<T>)
    private
      FCollection: TEnexDistinctCollection<T>;
      FEnumerator: IEnumerator<T>;
      FSet: ISet<T>;

    public
      { Constructor }
      constructor Create(const ACollection: TEnexDistinctCollection<T>);

      { Destructor }
      destructor Destroy(); override;

      function GetCurrent(): T; override;
      function MoveNext(): Boolean; override;
    end;

  var
    FCollection: TEnexCollection<T>;

  public
    { Constructors }
    constructor Create(const ACollection: TEnexCollection<T>); overload;

    { Destructor }
    destructor Destroy(); override;

    { IEnumerable<T> }
    function GetEnumerator(): IEnumerator<T>; override;
  end;

  { The "Range" collection }
  TEnexRangeCollection<T> = class sealed(TEnexCollection<T>)
  private
  type
    { The "Range" enumerator }
    TEnumerator = class(TEnumerator<T>)
    private
      FCollection: TEnexRangeCollection<T>;
      FEnumerator: IEnumerator<T>;
      FIdx: NativeInt;
    public
      { Constructor }
      constructor Create(const ACollection: TEnexRangeCollection<T>);

      { Destructor }
      destructor Destroy(); override;

      function GetCurrent(): T; override;
      function MoveNext(): Boolean; override;
    end;

  var
    FStart, FEnd: NativeInt;
    FCollection: TEnexCollection<T>;

  public
    { Constructors }
    constructor Create(const ACollection: TEnexCollection<T>; const AStart, AEnd: NativeInt); overload;

    { Destructor }
    destructor Destroy(); override;

    { IEnumerable<T> }
    function GetEnumerator(): IEnumerator<T>; override;
  end;

  { The "Skip" collection }
  TEnexSkipCollection<T> = class sealed(TEnexCollection<T>)
  private
  type
    { The "Skip" enumerator }
    TEnumerator = class(TEnumerator<T>)
    private
      FCollection: TEnexSkipCollection<T>;
      FEnumerator: IEnumerator<T>;
      FIdx: NativeInt;
    public
      { Constructor }
      constructor Create(const ACollection: TEnexSkipCollection<T>);

      { Destructor }
      destructor Destroy(); override;

      function GetCurrent(): T; override;
      function MoveNext(): Boolean; override;
    end;

  var
    FCount: NativeInt;
    FCollection: TEnexCollection<T>;

  public
    { Constructors }
    constructor Create(const ACollection: TEnexCollection<T>; const ACount: NativeInt); overload;

    { Destructor }
    destructor Destroy(); override;

    { IEnumerable<T> }
    function GetEnumerator(): IEnumerator<T>; override;
  end;

  { The "Take" collection }
  TEnexTakeCollection<T> = class sealed(TEnexCollection<T>)
  private
  type
    { The "Take" enumerator }
    TEnumerator = class(TEnumerator<T>)
    private
      FCollection: TEnexTakeCollection<T>;
      FEnumerator: IEnumerator<T>;
      FIdx: NativeInt;

    public
      { Constructor }
      constructor Create(const ACollection: TEnexTakeCollection<T>);

      { Destructor }
      destructor Destroy(); override;

      function GetCurrent(): T; override;
      function MoveNext(): Boolean; override;
    end;

  var
    FCount: NativeInt;
    FCollection: TEnexCollection<T>;

  public
    { Constructors }
    constructor Create(const ACollection: TEnexCollection<T>; const ACount: NativeInt); overload;

    { Destructor }
    destructor Destroy(); override;

    { IEnumerable<T> }
    function GetEnumerator(): IEnumerator<T>; override;
  end;

  { The "Fill" collection }
  TEnexFillCollection<T> = class sealed(TEnexCollection<T>)
  private
  type
    { The "Fill" enumerator }
    TEnumerator = class(TEnumerator<T>)
    private
      FCollection: TEnexFillCollection<T>;
      FCount: NativeInt;
    public
      { Constructor }
      constructor Create(const ACollection: TEnexFillCollection<T>);

      { Destructor }
      destructor Destroy(); override;

      function GetCurrent(): T; override;
      function MoveNext(): Boolean; override;
    end;

  var
    FElement: T;
    FCount: NativeInt;

  protected
    { Enex: Defaults }
    function GetCount(): NativeInt; override;

  public
    { Constructors }
    constructor Create(const AElement: T; const ACount: NativeInt; const ARules: TRules<T>);

    { IEnumerable<T> }
    function GetEnumerator(): IEnumerator<T>; override;

    { Enex Overrides }
    function Empty(): Boolean; override;
    function Max(): T; override;
    function Min(): T; override;
    function First(): T; override;
    function FirstOrDefault(const ADefault: T): T; override;
    function Last(): T; override;
    function LastOrDefault(const ADefault: T): T; override;
    function Single(): T; override;
    function SingleOrDefault(const ADefault: T): T; override;
    function Aggregate(const AAggregator: TFunc<T, T, T>): T; override;
    function AggregateOrDefault(const AAggregator: TFunc<T, T, T>; const ADefault: T): T; override;
    function ElementAt(const AIndex: NativeInt): T; override;
    function ElementAtOrDefault(const AIndex: NativeInt; const ADefault: T): T; override;
    function Any(const APredicate: TFunc<T, Boolean>): Boolean; override;
    function All(const APredicate: TFunc<T, Boolean>): Boolean; override;
    function EqualsTo(const ACollection: IEnumerable<T>): Boolean; override;
  end;

  { The "Take While" collection }
  TEnexTakeWhileCollection<T> = class sealed(TEnexCollection<T>)
  private
  type
    { The "Take While" enumerator }
    TEnumerator = class(TEnumerator<T>)
    private
      FCollection: TEnexTakeWhileCollection<T>;
      FEnumerator: IEnumerator<T>;

    public
      { Constructor }
      constructor Create(const ACollection: TEnexTakeWhileCollection<T>);

      { Destructor }
      destructor Destroy(); override;

      function GetCurrent(): T; override;
      function MoveNext(): Boolean; override;
    end;

  var
    FCollection: TEnexCollection<T>;
    FPredicate: TFunc<T, Boolean>;

  public
    { Constructors }
    constructor Create(const ACollection: TEnexCollection<T>; const APredicate: TFunc<T, Boolean>); overload;

    { Destructor }
    destructor Destroy(); override;

    { IEnumerable<T> }
    function GetEnumerator(): IEnumerator<T>; override;
  end;

  { The "Skip While" collection }
  TEnexSkipWhileCollection<T> = class sealed(TEnexCollection<T>)
  private
  type
    { The "Skip While" enumerator }
    TEnumerator = class(TEnumerator<T>)
    private
      FCollection: TEnexSkipWhileCollection<T>;
      FEnumerator: IEnumerator<T>;
      FStop: Boolean;
    public
      { Constructor }
      constructor Create(const ACollection: TEnexSkipWhileCollection<T>);

      { Destructor }
      destructor Destroy(); override;

      function GetCurrent(): T; override;
      function MoveNext(): Boolean; override;
    end;

  var
    FCollection: TEnexCollection<T>;
    FPredicate: TFunc<T, Boolean>;

  public
    { Constructors }
    constructor Create(const ACollection: TEnexCollection<T>; const APredicate: TFunc<T, Boolean>); overload;

    { Destructor }
    destructor Destroy(); override;

    { IEnumerable<T> }
    function GetEnumerator(): IEnumerator<T>; override;
  end;

  { The "Group By" collection }
  TEnexGroupByCollection<T, TBy> = class sealed(TEnexCollection<IEnexGroupingCollection<TBy, T>>)
  private type
    TEnexGroupingCollection = class(TEnexCollection<T>, IEnexGroupingCollection<TBy, T>)
    private
      FBy: TBy;
      FList: IList<T>;
    public
      function GetKey(): TBy;
      function GetCount(): NativeInt; override;
      function GetEnumerator(): IEnumerator<T>; override;
      procedure CopyTo(var AArray: array of T; const AStartIndex: NativeInt); overload; override;
      function Empty(): Boolean; override;
      function Max(): T; override;
      function Min(): T; override;
      function First(): T; override;
      function FirstOrDefault(const ADefault: T): T; override;
      function Last(): T; override;
      function LastOrDefault(const ADefault: T): T; override;
      function Single(): T; override;
      function SingleOrDefault(const ADefault: T): T; override;
      function Aggregate(const AAggregator: TFunc<T, T, T>): T; override;
      function AggregateOrDefault(const AAggregator: TFunc<T, T, T>; const ADefault: T): T; override;
      function ElementAt(const AIndex: NativeInt): T; override;
      function ElementAtOrDefault(const AIndex: NativeInt; const ADefault: T): T; override;
      function Any(const APredicate: TFunc<T, Boolean>): Boolean; override;
      function All(const APredicate: TFunc<T, Boolean>): Boolean; override;
      function EqualsTo(const ACollection: IEnumerable<T>): Boolean; override;
    end;

  private var
    FCollection: TEnexCollection<T>;
    FSelector: TFunc<T, TBy>;

  public
    { Constructors }
    constructor Create(const ACollection: TEnexCollection<T>; const ASelector: TFunc<T, TBy>);

    { Destructor }
    destructor Destroy(); override;

    { IEnumerable<T> }
    function GetEnumerator(): IEnumerator<IEnexGroupingCollection<TBy, T>>; override;
  end;

  { The "Select Keys" collection }
  TEnexSelectKeysCollection<TKey, TValue> = class sealed(TEnexCollection<TKey>)
  private
  type
    { The "Select Keys" enumerator }
    TEnumerator = class(TEnumerator<TKey>)
    private
      FCollection: TEnexSelectKeysCollection<TKey, TValue>;
      FEnumerator: IEnumerator<TPair<TKey, TValue>>;
      FCurrent: TKey;

    public
      { Constructor }
      constructor Create(const ACollection: TEnexSelectKeysCollection<TKey, TValue>);

      { Destructor }
      destructor Destroy(); override;

      function GetCurrent(): TKey; override;
      function MoveNext(): Boolean; override;
    end;

  var
    FCollection: TEnexAssociativeCollection<TKey, TValue>;

  protected
    { Enex: Defaults }
    function GetCount(): NativeInt; override;
  public
    { Constructors }
    constructor Create(const ACollection: TEnexAssociativeCollection<TKey, TValue>); overload;

    { Destructor }
    destructor Destroy(); override;

    { IEnumerable<T> }
    function GetEnumerator(): IEnumerator<TKey>; override;
  end;

  { The "Select Values" collection }
  TEnexSelectValuesCollection<TKey, TValue> = class sealed(TEnexCollection<TValue>)
  private
  type
    { The "Select Keys" enumerator }
    TEnumerator = class(TEnumerator<TValue>)
    private
      FCollection: TEnexSelectValuesCollection<TKey, TValue>;
      FEnumerator: IEnumerator<TPair<TKey, TValue>>;
      FCurrent: TValue;

    public
      { Constructor }
      constructor Create(const ACollection: TEnexSelectValuesCollection<TKey, TValue>);

      { Destructor }
      destructor Destroy(); override;

      function GetCurrent(): TValue; override;
      function MoveNext(): Boolean; override;
    end;

  var
    FCollection: TEnexAssociativeCollection<TKey, TValue>;

  protected
    { Enex: Defaults }
    function GetCount(): NativeInt; override;
  public
    { Constructors }
    constructor Create(const ACollection: TEnexAssociativeCollection<TKey, TValue>); overload;

    { Destructor }
    destructor Destroy(); override;

    { IEnumerable<T> }
    function GetEnumerator(): IEnumerator<TValue>; override;
  end;

  { The "Where" associative collection }
  TEnexAssociativeWhereCollection<TKey, TValue> = class sealed(TEnexAssociativeCollection<TKey, TValue>,
      IEnexAssociativeCollection<TKey, TValue>)
  private
  type
    { The "Where" associative enumerator }
    TEnumerator = class(TEnumerator<TPair<TKey, TValue>>)
    private
      FCollection: TEnexAssociativeWhereCollection<TKey, TValue>;
      FEnumerator: IEnumerator<TPair<TKey, TValue>>;

    public
      { Constructor }
      constructor Create(const ACollection: TEnexAssociativeWhereCollection<TKey, TValue>);

      { Destructor }
      destructor Destroy(); override;

      function GetCurrent(): TPair<TKey, TValue>; override;
      function MoveNext(): Boolean; override;
    end;

  var
    FCollection: TEnexAssociativeCollection<TKey, TValue>;
    FPredicate: TFunc<TKey, TValue, Boolean>;
    FInvertResult: Boolean;
  public
    { Constructors }
    constructor Create(const ACollection: TEnexAssociativeCollection<TKey, TValue>;
        const APredicate: TFunc<TKey, TValue, Boolean>; const AInvertResult: Boolean); overload;

    { Destructor }
    destructor Destroy(); override;

    { IEnumerable<T> }
    function GetEnumerator(): IEnumerator<TPair<TKey, TValue>>; override;
  end;

  { The "Distinct By Keys" associative collection }
  TEnexAssociativeDistinctByKeysCollection<TKey, TValue> = class sealed(TEnexAssociativeCollection<TKey, TValue>)
  private
  type
    { The "Distinct By Keys" associative enumerator }
    TEnumerator = class(TEnumerator<TPair<TKey, TValue>>)
    private
      FCollection: TEnexAssociativeDistinctByKeysCollection<TKey, TValue>;
      FEnumerator: IEnumerator<TPair<TKey, TValue>>;
      FSet: ISet<TKey>;

    public
      { Constructor }
      constructor Create(const ACollection: TEnexAssociativeDistinctByKeysCollection<TKey, TValue>);

      { Destructor }
      destructor Destroy(); override;

      function GetCurrent(): TPair<TKey, TValue>; override;
      function MoveNext(): Boolean; override;
    end;

  var
    FCollection: TEnexAssociativeCollection<TKey, TValue>;

  public
    { Constructors }
    constructor Create(const ACollection: TEnexAssociativeCollection<TKey, TValue>); overload;

    { Destructor }
    destructor Destroy(); override;

    { IEnumerable<T> }
    function GetEnumerator(): IEnumerator<TPair<TKey, TValue>>; override;
  end;

  { The "Distinct By Values" associative collection }
  TEnexAssociativeDistinctByValuesCollection<TKey, TValue> = class sealed(TEnexAssociativeCollection<TKey, TValue>)
  private
  type
    { The "Distinct By Keys" associative enumerator }
    TEnumerator = class(TEnumerator<TPair<TKey, TValue>>)
    private
      FCollection: TEnexAssociativeDistinctByValuesCollection<TKey, TValue>;
      FEnumerator: IEnumerator<TPair<TKey, TValue>>;
      FSet: ISet<TValue>;

    public
      { Constructor }
      constructor Create(const ACollection: TEnexAssociativeDistinctByValuesCollection<TKey, TValue>);

      { Destructor }
      destructor Destroy(); override;

      function GetCurrent(): TPair<TKey, TValue>; override;
      function MoveNext(): Boolean; override;
    end;

  var
    FCollection: TEnexAssociativeCollection<TKey, TValue>;

  public
    { Constructors }
    constructor Create(const ACollection: TEnexAssociativeCollection<TKey, TValue>); overload;

    { Destructor }
    destructor Destroy(); override;

    { IEnumerable<T> }
    function GetEnumerator(): IEnumerator<TPair<TKey, TValue>>; override;
  end;
{$ENDREGION}

implementation
uses
  TypInfo,
  Collections.Sets,
  Collections.Lists,
  Collections.Dictionaries;

{ TEnexExtOps<T> }

function TEnexExtOps<T>.GroupBy<TKey>(const ASelector: TFunc<T, TKey>): IEnexCollection<IEnexGroupingCollection<TKey, T>>;
begin
  { Check arguments }
  if not Assigned(ASelector) then
    ExceptionHelper.Throw_ArgumentNilError('ASelector');

  { Create an intermediate collection that will lazy-create the actual stuff }
  Result := TEnexGroupByCollection<T, TKey>.Create(FInstance, ASelector);
end;

function TEnexExtOps<T>.Select(const AMemberName: string): IEnexCollection<TAny>;
var
  LSelector: TFunc<T, TAny>;
begin
  { Get selector }
  LSelector := Member.Name<T>(AMemberName);

  if not Assigned(LSelector) then
    ExceptionHelper.Throw_TypeDoesNotExposeMember('AMemberName');

  { Select the member by a name, as out type }
  Result := Select<TAny>(LSelector);
end;

function TEnexExtOps<T>.Select<TOut>(const AMemberName: string): IEnexCollection<TOut>;
var
  LSelector: TFunc<T, TOut>;
begin
  { Get selector }
  LSelector := Member.Name<T, TOut>(AMemberName);

  if not Assigned(LSelector) then
    ExceptionHelper.Throw_TypeDoesNotExposeMember(AMemberName);

  { Select the member by a name, as out type }
  Result := Select<TOut>(LSelector);
end;

function TEnexExtOps<T>.Select(const AMemberNames: array of string): IEnexCollection<TView>;
var
  LSelector: TFunc<T, TView>;
begin
  { Get selector }
  LSelector := Member.Name<T>(AMemberNames);

  if not Assigned(LSelector) then
    ExceptionHelper.Throw_TypeDoesNotExposeMember('...');

  { Select the member by a name, as out type }
  Result := Select<TView>(LSelector);
end;

function TEnexExtOps<T>.Select<TOut>: IEnexCollection<TOut>;
var
  LTypeInfo: PTypeInfo;
begin
  { Make sure that T is a class }
  LTypeInfo := TypeInfo(T);

  { TADA! }
  if (not Assigned(LTypeInfo)) or (LTypeInfo^.Kind <> tkClass) then
    ExceptionHelper.Throw_TypeNotAClassError(GetTypeName(LTypeInfo));

  Result := TEnexSelectClassCollection<TObject, TOut>.Create(FInstance, TRules<TOut>.Default);
end;

function TEnexExtOps<T>.Select<TOut>(const ASelector: TFunc<T, TOut>): IEnexCollection<TOut>;
begin
  { With default type support }
  Result := Select<TOut>(ASelector, TRules<TOut>.Default);
end;

function TEnexExtOps<T>.Select<TOut>(const ASelector: TFunc<T, TOut>; const ARules: TRules<TOut>): IEnexCollection<TOut>;
begin
  { Check arguments }
  if not Assigned(ASelector) then
    ExceptionHelper.Throw_ArgumentNilError('ASelector');

  { Create a new Enex collection }
  Result := TEnexSelectCollection<T, TOut>.Create(FInstance, ASelector, ARules);
end;

{ TCollection<T> }

procedure TCollection<T>.CopyTo(var AArray: array of T);
begin
  { Call upper version }
  CopyTo(AArray, 0);
end;

procedure TCollection<T>.CopyTo(var AArray: array of T; const AStartIndex: NativeInt);
var
  LEnumerator: IEnumerator<T>;
  L, I: NativeInt;
begin
  if (AStartIndex >= Length(AArray)) or (AStartIndex < 0) then
    ExceptionHelper.Throw_ArgumentOutOfRangeError('AStartIndex');

  { Retrieve the enumerator object }
  LEnumerator := GetEnumerator();
  L := Length(AArray);
  I := AStartIndex;

  { Iterate until ANY element supports the predicate }
  while LEnumerator.MoveNext() do
  begin
    if I >= L then
      ExceptionHelper.Throw_ArgumentOutOfSpaceError('AArray/AStartIndex');

    AArray[I] := LEnumerator.Current;
    Inc(I);
  end;
end;

function TCollection<T>.Empty: Boolean;
var
  LEnumerator: IEnumerator<T>;
begin
  { Retrieve the enumerator object }
  LEnumerator := GetEnumerator();

  { Check if empty }
  Result := (not LEnumerator.MoveNext());
end;

function TCollection<T>.GetCount: NativeInt;
var
  LEnumerator: IEnumerator<T>;
begin
  { Retrieve the enumerator object }
  LEnumerator := GetEnumerator();

  { Iterate till the end }
  Result := 0;
  while LEnumerator.MoveNext() do Inc(Result);
end;

function TCollection<T>.Single: T;
var
  LEnumerator: IEnumerator<T>;
begin
  { Retrieve the enumerator object }
  LEnumerator := GetEnumerator();

  { Get the first object in the enumeration, otherwise fail! }
  if LEnumerator.MoveNext() then
    Result := LEnumerator.Current
  else
    ExceptionHelper.Throw_CollectionEmptyError();

  { Fail if more than one elements are there }
  if LEnumerator.MoveNext() then
    ExceptionHelper.Throw_CollectionHasMoreThanOneElement();
end;

function TCollection<T>.SingleOrDefault(const ADefault: T): T;
var
  LEnumerator: IEnumerator<T>;
begin
  { Retrieve the enumerator object }
  LEnumerator := GetEnumerator();

  { Get the first object in the enumeration, otherwise fail! }
  if LEnumerator.MoveNext() then
    Result := LEnumerator.Current
  else
    Exit(ADefault);

  { Fail if more than one elements are there }
  if LEnumerator.MoveNext() then
    ExceptionHelper.Throw_CollectionHasMoreThanOneElement();
end;

function TCollection<T>.ToArray: TArray<T>;
var
  LCount: NativeInt;
  LResult: TArray<T>;
begin
  LCount := Count;

  if LCount > 0 then
  begin
    { Set the length of array }
    SetLength(LResult, LCount);

    { Copy all elements to array }
    CopyTo(LResult);
  end else
    SetLength(LResult, 0);

  Result := LResult;
end;

{ TEnexCollection<T> }

function TEnexCollection<T>.Aggregate(const AAggregator: TFunc<T, T, T>): T;
var
  LEnumerator: IEnumerator<T>;
begin
  if not Assigned(AAggregator) then
    ExceptionHelper.Throw_ArgumentNilError('AAggregator');

  { Retrieve the enumerator object and type }
  LEnumerator := GetEnumerator();

  { Get the first object in the enumeration, otherwise fail! }
  if not LEnumerator.MoveNext() then
    ExceptionHelper.Throw_CollectionEmptyError();

  { Select the first element as comparison base }
  Result := LEnumerator.Current;

  { Iterate over the last N - 1 elements }
  while LEnumerator.MoveNext() do
  begin
    { Aggregate a value }
    Result := AAggregator(Result, LEnumerator.Current);
  end;
end;

function TEnexCollection<T>.AggregateOrDefault(const AAggregator: TFunc<T, T, T>; const ADefault: T): T;
var
  LEnumerator: IEnumerator<T>;
begin
  if not Assigned(AAggregator) then
    ExceptionHelper.Throw_ArgumentNilError('AAggregator');

  { Retrieve the enumerator object and type }
  LEnumerator := GetEnumerator();

  { Get the first object in the enumeration, otherwise fail! }
  if not LEnumerator.MoveNext() then
    Exit(ADefault);

  { Select the first element as comparison base }
  Result := LEnumerator.Current;

  { Iterate over the last N - 1 elements }
  while LEnumerator.MoveNext() do
  begin
    { Aggregate a value }
    Result := AAggregator(Result, LEnumerator.Current);
  end;
end;

function TEnexCollection<T>.All(const APredicate: TFunc<T, Boolean>): Boolean;
var
  LEnumerator: IEnumerator<T>;
begin
  if not Assigned(APredicate) then
    ExceptionHelper.Throw_ArgumentNilError('APredicate');

  { Retrieve the enumerator object }
  LEnumerator := GetEnumerator();

  { Iterate while ALL elements support the predicate }
  while LEnumerator.MoveNext() do
  begin
    if not APredicate(LEnumerator.Current) then
      Exit(false);
  end;

  Result := true;
end;

function TEnexCollection<T>.Any(const APredicate: TFunc<T, Boolean>): Boolean;
var
  LEnumerator: IEnumerator<T>;
begin
  if not Assigned(APredicate) then
    ExceptionHelper.Throw_ArgumentNilError('APredicate');

  { Retrieve the enumerator object }
  LEnumerator := GetEnumerator();

  { Iterate until ANY element supports the predicate }
  while LEnumerator.MoveNext() do
  begin
    if APredicate(LEnumerator.Current) then
      Exit(true);
  end;

  Result := false;
end;

function TEnexCollection<T>.CompareElements(const ALeft, ARight: T): NativeInt;
begin
  { Lazy init }
  if not Assigned(FElementRules.FComparer) then
    FElementRules.FComparer := TComparer<T>.Default;

  Result := FElementRules.FComparer.Compare(ALeft, ARight);
end;

function TEnexCollection<T>.CompareTo(AObject: TObject): Integer;
var
  LIterSelf, LIterTo: IEnumerator<T>;
  LMovSelf, LMovTo: Boolean;
begin
  { Check if we can continue }
  if (not Assigned(AObject)) or (not AObject.InheritsFrom(TEnexCollection<T>)) then
    Result := 1
  else begin
    { Assume equality }
    Result := 0;

    { Get enumerators }
    LIterSelf := GetEnumerator();
    LIterTo := TEnexCollection<T>(AObject).GetEnumerator();

    while true do
    begin
      { Iterate and verify that both enumerators moved }
      LMovSelf := LIterSelf.MoveNext();
      LMovTo := LIterTo.MoveNext();

      { If one moved but the other did not - error }
      if LMovSelf <> LMovTo then
      begin
        { Decide on the return value }
        if LMovSelf then
          Result := 1
        else
          Result := -1;

        Break;
      end;

      { If neither moved, we've reached the end }
      if not LMovSelf then
        Break;

      { Verify both values are identical }
      Result := CompareElements(LIterSelf.Current, LIterTo.Current);
      if Result <> 0 then
        Break;
    end;
  end;
end;

function TEnexCollection<T>.Concat(const ACollection: IEnexCollection<T>): IEnexCollection<T>;
begin
  { Check arguments }
  if not Assigned(ACollection) then
    ExceptionHelper.Throw_ArgumentNilError('ACollection');

  { Create concatenation iterator }
  Result := TEnexConcatCollection<T>.Create(Self, ACollection);
end;

constructor TEnexCollection<T>.Create(const ARules: TRules<T>);
begin
  FElementRules := ARules;
end;

constructor TEnexCollection<T>.Create;
begin
  Create(TRules<T>.Default);
end;

function TEnexCollection<T>.Distinct: IEnexCollection<T>;
begin
  { Create a new enumerator }
  Result := TEnexDistinctCollection<T>.Create(Self);
end;

function TEnexCollection<T>.ElementAt(const AIndex: NativeInt): T;
var
  LEnumerator: IEnumerator<T>;
  LCount: NativeInt;
begin
  { Retrieve the enumerator object }
  LEnumerator := GetEnumerator();
  LCount := 0;

  while LEnumerator.MoveNext() do
  begin
    { If we reached thge element, exit }
    if LCount = AIndex then
      Exit(LEnumerator.Current);

    Inc(LCount);
  end;

  { Fail! }
  ExceptionHelper.Throw_ArgumentOutOfRangeError('AIndex');
end;

function TEnexCollection<T>.ElementAtOrDefault(const AIndex: NativeInt; const ADefault: T): T;
var
  LEnumerator: IEnumerator<T>;
  LCount: NativeInt;
begin
  { Retrieve the enumerator object }
  LEnumerator := GetEnumerator();
  LCount := 0;

  while LEnumerator.MoveNext() do
  begin
    { If we reached thge element, exit }
    if LCount = AIndex then
      Exit(LEnumerator.Current);

    Inc(LCount);
  end;

  { Return default value }
  Result := ADefault;
end;

function TEnexCollection<T>.ElementsAreEqual(const ALeft, ARight: T): Boolean;
begin
  { Lazy init }
  if not Assigned(FElementRules.FEqComparer) then
    FElementRules.FEqComparer := TEqualityComparer<T>.Default;

  Result := FElementRules.FEqComparer.Equals(ALeft, ARight);
end;

function TEnexCollection<T>.Equals(Obj: TObject): Boolean;
begin
  { Call comparison }
  Result := (CompareTo(Obj) = 0);
end;

function TEnexCollection<T>.EqualsTo(const ACollection: IEnumerable<T>): Boolean;
var
  LEnumerator1, LEnumerator2: IEnumerator<T>;
  LMoved1, LMoved2: Boolean;
begin
  { Check arguments }
  if not Assigned(ACollection) then
    ExceptionHelper.Throw_ArgumentNilError('ACollection');

  { Get enumerators }
  LEnumerator1 := GetEnumerator();
  LEnumerator2 := ACollection.GetEnumerator();

  while true do
  begin
    { Iterate and verify that both enumerators moved }
    LMoved1 := LEnumerator1.MoveNext();
    LMoved2 := LEnumerator2.MoveNext();

    { If one moved but the other did not - error }
    if LMoved1 <> LMoved2 then
      Exit(false);

    { If neither moved, we've reached the end }
    if not LMoved1 then
      break;

    { Verify both values are identical }
    if not ElementsAreEqual(LEnumerator1.Current, LEnumerator2.Current) then
      Exit(false);
  end;

  { It worked! }
  Result := true;
end;

function TEnexCollection<T>.Exclude(const ACollection: IEnexCollection<T>): IEnexCollection<T>;
begin
  { Check arguments }
  if not Assigned(ACollection) then
    ExceptionHelper.Throw_ArgumentNilError('ACollection');

  { Create concatenation iterator }
  Result := TEnexExclusionCollection<T>.Create(Self, ACollection);
end;

class function TEnexCollection<T>.Fill(const AElement: T; const ACount: NativeInt; const ARules: TRules<T>): IEnexCollection<T>;
begin
  { Check arguments }
  if ACount <= 0 then
    ExceptionHelper.Throw_ArgumentOutOfRangeError('ACount');

  { Create an collection }
  Result := TEnexFillCollection<T>.Create(AElement, ACount, ARules);
end;

class function TEnexCollection<T>.Fill(const AElement: T; const ACount: NativeInt): IEnexCollection<T>;
begin
  { Call upper function }
  Result := Fill(AElement, ACount, TRules<T>.Default);
end;

function TEnexCollection<T>.First: T;
var
  LEnumerator: IEnumerator<T>;
begin
  { Retrieve the enumerator object }
  LEnumerator := GetEnumerator();

  { Get the first object in the enumeration, otherwise fail! }
  if LEnumerator.MoveNext() then
    Result := LEnumerator.Current
  else
    ExceptionHelper.Throw_CollectionEmptyError();
end;

function TEnexCollection<T>.FirstOrDefault(const ADefault: T): T;
var
  LEnumerator: IEnumerator<T>;
begin
  { Retrieve the enumerator object }
  LEnumerator := GetEnumerator();

  { Get the first object in the enumeration, otherwise return default! }
  if LEnumerator.MoveNext() then
    Result := LEnumerator.Current
  else
    Result := ADefault;
end;

function TEnexCollection<T>.FirstWhere(const APredicate: TFunc<T, Boolean>): T;
var
  LEnumerator: IEnumerator<T>;
  LWasOne: Boolean;
begin
  if not Assigned(APredicate) then
    ExceptionHelper.Throw_ArgumentNilError('APredicate');

  { Retrieve the enumerator object }
  LEnumerator := GetEnumerator();
  LWasOne := false;

  { Do the funky stuff already }
  while LEnumerator.MoveNext do
  begin
    LWasOne := true;

    if APredicate(LEnumerator.Current) then
      Exit(LEnumerator.Current);
  end;

  { Failure to find what we need }
  if LWasOne then
    ExceptionHelper.Throw_CollectionHasNoFilteredElements()
  else
    ExceptionHelper.Throw_CollectionEmptyError();
end;

function TEnexCollection<T>.FirstWhereBetween(const ALower, AHigher: T): T;
begin
  Result := FirstWhere(
    function(Arg1: T): Boolean
    begin
      Result := (CompareElements(Arg1, ALower) >= 0) and
                (CompareElements(Arg1, AHigher) <= 0)
    end
  );
end;

function TEnexCollection<T>.FirstWhereBetweenOrDefault(const ALower, AHigher, ADefault: T): T;
begin
  Result := FirstWhereOrDefault(
    function(Arg1: T): Boolean
    begin
      Result := (CompareElements(Arg1, ALower) >= 0) and
                (CompareElements(Arg1, AHigher) <= 0)
    end,
    ADefault
  );
end;

function TEnexCollection<T>.FirstWhereGreater(const ABound: T): T;
begin
  Result := FirstWhere(
    function(Arg1: T): Boolean
    begin
      Result := CompareElements(Arg1, ABound) > 0;
    end
  );
end;

function TEnexCollection<T>.FirstWhereGreaterOrDefault(const ABound, ADefault: T): T;
begin
  Result := FirstWhereOrDefault(
    function(Arg1: T): Boolean
    begin
      Result := CompareElements(Arg1, ABound) > 0;
    end,
    ADefault
  );
end;

function TEnexCollection<T>.FirstWhereGreaterOrEqual(const ABound: T): T;
begin
  Result := FirstWhere(
    function(Arg1: T): Boolean
    begin
      Result := CompareElements(Arg1, ABound) >= 0;
    end
  );
end;

function TEnexCollection<T>.FirstWhereGreaterOrEqualOrDefault(const ABound, ADefault: T): T;
begin
  Result := FirstWhereOrDefault(
    function(Arg1: T): Boolean
    begin
      Result := CompareElements(Arg1, ABound) >= 0;
    end,
    ADefault
  );
end;

function TEnexCollection<T>.FirstWhereLower(const ABound: T): T;
begin
  Result := FirstWhere(
    function(Arg1: T): Boolean
    begin
      Result := CompareElements(Arg1, ABound) < 0;
    end
  );
end;

function TEnexCollection<T>.FirstWhereLowerOrDefault(const ABound, ADefault: T): T;
begin
  Result := FirstWhereOrDefault(
    function(Arg1: T): Boolean
    begin
      Result := CompareElements(Arg1, ABound) < 0;
    end,
    ADefault
  );
end;

function TEnexCollection<T>.FirstWhereLowerOrEqual(const ABound: T): T;
begin
  Result := FirstWhere(
    function(Arg1: T): Boolean
    begin
      Result := CompareElements(Arg1, ABound) <= 0;
    end
  );
end;

function TEnexCollection<T>.FirstWhereLowerOrEqualOrDefault(const ABound, ADefault: T): T;
begin
  Result := FirstWhereOrDefault(
    function(Arg1: T): Boolean
    begin
      Result := CompareElements(Arg1, ABound) <= 0;
    end,
    ADefault
  );
end;

function TEnexCollection<T>.FirstWhereNot(const APredicate: TFunc<T, Boolean>): T;
begin
  if not Assigned(APredicate) then
    ExceptionHelper.Throw_ArgumentNilError('APredicate');

  Result := FirstWhere(
    function(Arg1: T): Boolean
    begin
      Result := not APredicate(Arg1);
    end
  );
end;

function TEnexCollection<T>.FirstWhereNotOrDefault(
  const APredicate: TFunc<T, Boolean>; const ADefault: T): T;
begin
  if not Assigned(APredicate) then
    ExceptionHelper.Throw_ArgumentNilError('APredicate');

  Result := FirstWhereOrDefault(
    function(Arg1: T): Boolean
    begin
      Result := not APredicate(Arg1);
    end,
    ADefault
  );
end;

function TEnexCollection<T>.FirstWhereOrDefault(const APredicate: TFunc<T, Boolean>; const ADefault: T): T;
var
  LEnumerator: IEnumerator<T>;
begin
  if not Assigned(APredicate) then
    ExceptionHelper.Throw_ArgumentNilError('APredicate');

  { Retrieve the enumerator object }
  LEnumerator := GetEnumerator();

  { Do the funky stuff already }
  while LEnumerator.MoveNext do
    if APredicate(LEnumerator.Current) then
      Exit(LEnumerator.Current);

  { Failure to find what we need }
  Result := ADefault;
end;

function TEnexCollection<T>.GetElementHashCode(const AValue: T): NativeInt;
begin
  { Lazy init }
  if not Assigned(FElementRules.FEqComparer) then
    FElementRules.FEqComparer := TEqualityComparer<T>.Default;

  Result := FElementRules.FEqComparer.GetHashCode(AValue);
end;

function TEnexCollection<T>.GetHashCode: Integer;
const
  CMagic = $0F;

var
  LEnumerator: IEnumerator<T>;
begin
  { Obtain the enumerator }
  LEnumerator := GetEnumerator();

  { Start at 0 }
  Result := 0;

  { ... }
  while LEnumerator.MoveNext() do
    Result := CMagic * Result + GetElementHashCode(LEnumerator.Current);
end;

procedure TEnexCollection<T>.HandleElementRemoved(const AElement: T);
begin
 // Nothing
end;

function TEnexCollection<T>.Intersect(const ACollection: IEnexCollection<T>): IEnexCollection<T>;
begin
  { Check arguments }
  if not Assigned(ACollection) then
    ExceptionHelper.Throw_ArgumentNilError('ACollection');

  { Create concatenation iterator }
  Result := TEnexIntersectionCollection<T>.Create(Self, ACollection);
end;

function TEnexCollection<T>.Last: T;
var
  LEnumerator: IEnumerator<T>;
begin
  { Retrieve the enumerator object }
  LEnumerator := GetEnumerator();

  { Get the first object in the enumeration, otherwise fail! }
  if not LEnumerator.MoveNext() then
    ExceptionHelper.Throw_CollectionEmptyError();

  { Iterate till the last element in the LEnumerator }
  while true do
  begin
    Result := LEnumerator.Current;

    { Exit if we hit the last element }
    if not LEnumerator.MoveNext() then
      Exit;
  end;
end;

function TEnexCollection<T>.LastOrDefault(const ADefault: T): T;
var
  LEnumerator: IEnumerator<T>;
begin
  { Retrieve the enumerator object }
  LEnumerator := GetEnumerator();

  { Get the first object in the enumeration, otherwise return default! }
  if not LEnumerator.MoveNext() then
    Exit(ADefault);

  { Iterate till the last element in the LEnumerator }
  while true do
  begin
    Result := LEnumerator.Current;

    { Exit if we hit the last element }
    if not LEnumerator.MoveNext() then
      Exit;
  end;
end;

function TEnexCollection<T>.Max: T;
var
  LEnumerator: IEnumerator<T>;
begin
  { Retrieve the enumerator object and type }
  LEnumerator := GetEnumerator();

  { Get the first object in the enumeration, otherwise fail! }
  if not LEnumerator.MoveNext() then
    ExceptionHelper.Throw_CollectionEmptyError();

  { Select the first element as comparison base }
  Result := LEnumerator.Current;

  { Iterate till the last element in the LEnumerator }
  while true do
  begin
    if CompareElements(LEnumerator.Current, Result) > 0 then
      Result := LEnumerator.Current;

    { Exit if we hit the last element }
    if not LEnumerator.MoveNext() then
      Exit;
  end;
end;

function TEnexCollection<T>.Min: T;
var
  LEnumerator: IEnumerator<T>;
begin
  { Retrieve the enumerator object and type }
  LEnumerator := GetEnumerator();

  { Get the first object in the enumeration, otherwise fail! }
  if not LEnumerator.MoveNext() then
    ExceptionHelper.Throw_CollectionEmptyError();

  { Select the first element as comparison base }
  Result := LEnumerator.Current;

  { Iterate till the last element in the LEnumerator }
  while true do
  begin
    if CompareElements(LEnumerator.Current, Result) < 0 then
      Result := LEnumerator.Current;

    { Exit if we hit the last element }
    if not LEnumerator.MoveNext() then
      Exit;
  end;
end;

procedure TEnexCollection<T>.NotifyElementRemoved(const AElement: T);
begin
  { Handle removal }
  if Assigned(FRemoveNotification) then
    FRemoveNotification(AElement)
  else
    HandleElementRemoved(AElement);
end;

function TEnexCollection<T>.Op: TEnexExtOps<T>;
begin
  { Build up the record + keep an optional reference to the object }
  Result.FInstance := Self;
  Result.FKeepAlive := Self.ExtractReference;
  Result.FRules := FElementRules;
end;

function TEnexCollection<T>.Range(const AStart, AEnd: NativeInt): IEnexCollection<T>;
begin
  { Create a new Enex collection }
  Result := TEnexRangeCollection<T>.Create(Self, AStart, AEnd);
end;

function TEnexCollection<T>.Reversed: IEnexCollection<T>;
var
  LList: TList<T>;
begin
  { Create an itermediary LList }
  LList := TList<T>.Create(Self);
  LList.Reverse();

  { Pass the LList further }
  Result := LList;
end;

function TEnexCollection<T>.Skip(const ACount: NativeInt): IEnexCollection<T>;
begin
  { Check parameters }
  if ACount = 0 then
    ExceptionHelper.Throw_ArgumentOutOfRangeError('ACount');

  { Create a new Enex collection }
  Result := TEnexSkipCollection<T>.Create(Self, ACount);
end;

function TEnexCollection<T>.SkipWhile(const APredicate: TFunc<T, Boolean>): IEnexCollection<T>;
begin
  { Check arguments }
  if not Assigned(APredicate) then
    ExceptionHelper.Throw_ArgumentNilError('APredicate');

  { Create a new Enex collection }
  Result := TEnexSkipWhileCollection<T>.Create(Self, APredicate);
end;

function TEnexCollection<T>.SkipWhileBetween(const ALower, AHigher: T): IEnexCollection<T>;
var
  LLower, LHigher: T;
begin
  { Locals }
  LLower := ALower;
  LHigher := AHigher;

  { Use SkipWhile() and pass an anonymous function }
  Result := SkipWhile(
    function(Arg1: T): Boolean
    begin
      Exit((CompareElements(Arg1, LLower) >= 0) and (CompareElements(Arg1, LHigher) <= 0));
    end
  );
end;

function TEnexCollection<T>.SkipWhileGreater(const ABound: T): IEnexCollection<T>;
var
  LBound: T;
begin
  { Locals }
  LBound := ABound;

  { Use SkipWhile() and pass an anonymous function }
  Result := SkipWhile(
    function(Arg1: T): Boolean
    begin
      Exit(CompareElements(Arg1, LBound) > 0);
    end
  );
end;

function TEnexCollection<T>.SkipWhileGreaterOrEqual(const ABound: T): IEnexCollection<T>;
var
  LBound: T;
begin
  { Locals }
  LBound := ABound;

  { Use SkipWhile() and pass an anonymous function }
  Result := SkipWhile(
    function(Arg1: T): Boolean
    begin
      Exit(CompareElements(Arg1, LBound) >= 0);
    end
  );
end;

function TEnexCollection<T>.SkipWhileLower(const ABound: T): IEnexCollection<T>;
var
  LBound: T;
begin
  { Locals }
  LBound := ABound;

  { Use SkipWhile() and pass an anonymous function }
  Result := SkipWhile(
    function(Arg1: T): Boolean
    begin
      Exit(CompareElements(Arg1, LBound) < 0);
    end
  );
end;

function TEnexCollection<T>.SkipWhileLowerOrEqual(const ABound: T): IEnexCollection<T>;
var
  LBound: T;
begin
  { Locals }
  LBound := ABound;

  { Use SkipWhile() and pass an anonymous function }
  Result := SkipWhile(
    function(Arg1: T): Boolean
    begin
      Exit(CompareElements(Arg1, LBound) <= 0);
    end
  );
end;

function TEnexCollection<T>.Ordered(const ASortProc: TComparison<T>): IEnexCollection<T>;
var
  LList: TList<T>;
begin
  { Create an itermediary LList }
  LList := TList<T>.Create(Self);
  LList.Sort(ASortProc);

  { Pass the LList further }
  Result := LList;
end;

function TEnexCollection<T>.Ordered(const AAscending: Boolean = true): IEnexCollection<T>;
var
  LList: TList<T>;
begin
  { Create an itermediary LList }
  LList := TList<T>.Create(Self);
  LList.Sort(AAscending);

  { Pass the LList further }
  Result := LList;
end;

function TEnexCollection<T>.Take(const ACount: NativeInt): IEnexCollection<T>;
begin
  { Check parameters }
  if ACount = 0 then
    ExceptionHelper.Throw_ArgumentOutOfRangeError('ACount');

  { Create a new Enex collection }
  Result := TEnexTakeCollection<T>.Create(Self, ACount);
end;

function TEnexCollection<T>.TakeWhile(const APredicate: TFunc<T, Boolean>): IEnexCollection<T>;
begin
  { Check arguments }
  if not Assigned(APredicate) then
    ExceptionHelper.Throw_ArgumentNilError('APredicate');

  { Create a new Enex collection }
  Result := TEnexTakeWhileCollection<T>.Create(Self, APredicate);
end;

function TEnexCollection<T>.TakeWhileBetween(const ALower, AHigher: T): IEnexCollection<T>;
var
  LLower, LHigher: T;
begin
  { Locals }
  LLower := ALower;
  LHigher := AHigher;

  { Use TakeWhile() and pass an anonymous function }
  Result := TakeWhile(
    function(Arg1: T): Boolean
    begin
      Exit((CompareElements(Arg1, LLower) >= 0) and (CompareElements(Arg1, LHigher) <= 0));
    end
  );
end;

function TEnexCollection<T>.TakeWhileGreater(const ABound: T): IEnexCollection<T>;
var
  LBound: T;
begin
  { Locals }
  LBound := ABound;

  { Use TakeWhile() and pass an anonymous function }
  Result := TakeWhile(
    function(Arg1: T): Boolean
    begin
      Exit(CompareElements(Arg1, LBound) > 0);
    end
  );
end;

function TEnexCollection<T>.TakeWhileGreaterOrEqual(const ABound: T): IEnexCollection<T>;
var
  LBound: T;
begin
  { Locals }
  LBound := ABound;

  { Use TakeWhile() and pass an anonymous function }
  Result := TakeWhile(
    function(Arg1: T): Boolean
    begin
      Exit(CompareElements(Arg1, LBound) >= 0);
    end
  );
end;

function TEnexCollection<T>.TakeWhileLower(const ABound: T): IEnexCollection<T>;
var
  LBound: T;
begin
  { Locals }
  LBound := ABound;

  { Use TakeWhile() and pass an anonymous function }
  Result := TakeWhile(
    function(Arg1: T): Boolean
    begin
      Exit(CompareElements(Arg1, LBound) < 0);
    end
  );
end;

function TEnexCollection<T>.TakeWhileLowerOrEqual(const ABound: T): IEnexCollection<T>;
var
  LBound: T;
begin
  { Locals }
  LBound := ABound;

  { Use TakeWhile() and pass an anonymous function }
  Result := TakeWhile(
    function(Arg1: T): Boolean
    begin
      Exit(CompareElements(Arg1, LBound) <= 0);
    end
  );
end;

function TEnexCollection<T>.ToList: IList<T>;
begin
  { Simply make up a list }
  Result := TList<T>.Create(Self);
end;

function TEnexCollection<T>.ToSet: ISet<T>;
begin
  { Simply make up a bag }
  Result := THashSet<T>.Create(Self);
end;

function TEnexCollection<T>.Union(const ACollection: IEnexCollection<T>): IEnexCollection<T>;
begin
  { Check arguments }
  if not Assigned(ACollection) then
    ExceptionHelper.Throw_ArgumentNilError('ACollection');

  { Create concatenation iterator }
  Result := TEnexUnionCollection<T>.Create(Self, ACollection);
end;

function TEnexCollection<T>.Where(const APredicate: TFunc<T, Boolean>): IEnexCollection<T>;
begin
  { Check arguments }
  if not Assigned(APredicate) then
    ExceptionHelper.Throw_ArgumentNilError('APredicate');

  { Create a new Enex collection }
  Result := TEnexWhereCollection<T>.Create(Self, APredicate, False); // Don't invert the result
end;

function TEnexCollection<T>.WhereBetween(const ALower, AHigher: T): IEnexCollection<T>;
var
  LLower, LHigher: T;
begin
  { Locals }
  LLower := ALower;
  LHigher := AHigher;

  { Use Where() and pass an anonymous function }
  Result := Where(
    function(Arg1: T): Boolean
    begin
      Exit((CompareElements(Arg1, LLower) >= 0) and (CompareElements(Arg1, LHigher) <= 0));
    end
  );
end;

function TEnexCollection<T>.WhereGreater(const ABound: T): IEnexCollection<T>;
var
  LBound: T;
begin
  { Locals }
  LBound := ABound;

  { Use Where() and pass an anonymous function }
  Result := Where(
    function(Arg1: T): Boolean
    begin
      Exit(CompareElements(Arg1, LBound) > 0);
    end
  );
end;

function TEnexCollection<T>.WhereGreaterOrEqual(const ABound: T): IEnexCollection<T>;
var
  LBound: T;
begin
  { Locals }
  LBound := ABound;

  { Use Where() and pass an anonymous function }
  Result := Where(
    function(Arg1: T): Boolean
    begin
      Exit(CompareElements(Arg1, LBound) >= 0);
    end
  );
end;

function TEnexCollection<T>.WhereLower(const ABound: T): IEnexCollection<T>;
var
  LBound: T;
begin
  { Locals }
  LBound := ABound;

  { Use Where() and pass an anonymous function }
  Result := Where(
    function(Arg1: T): Boolean
    begin
      Exit(CompareElements(Arg1, LBound) < 0);
    end
  );
end;

function TEnexCollection<T>.WhereLowerOrEqual(const ABound: T): IEnexCollection<T>;
var
  LBound: T;
begin
  { Locals }
  LBound := ABound;

  { Use Where() and pass an anonymous function }
  Result := Where(
    function(Arg1: T): Boolean
    begin
      Exit(CompareElements(Arg1, LBound) <= 0);
    end
  );
end;

function TEnexCollection<T>.WhereNot(
  const APredicate: TFunc<T, Boolean>): IEnexCollection<T>;
begin
  { Check arguments }
  if not Assigned(APredicate) then
    ExceptionHelper.Throw_ArgumentNilError('APredicate');

  { Create a new Enex collection }
  Result := TEnexWhereCollection<T>.Create(Self, APredicate, True); // Invert the result
end;

{ TEnexAssociativeCollection<TKey, TValue> }

constructor TEnexAssociativeCollection<TKey, TValue>.Create;
begin
  Create(TRules<TKey>.Default, TRules<TValue>.Default);
end;

function TEnexAssociativeCollection<TKey, TValue>.CompareKeys(const ALeft, ARight: TKey): NativeInt;
begin
  { Lazy init }
  if not Assigned(FKeyRules.FComparer) then
    FKeyRules.FComparer := TComparer<TKey>.Default;

  Result := FKeyRules.FComparer.Compare(ALeft, ARight);
end;

function TEnexAssociativeCollection<TKey, TValue>.CompareValues(const ALeft, ARight: TValue): NativeInt;
begin
  { Lazy init }
  if not Assigned(FValueRules.FComparer) then
    FValueRules.FComparer := TComparer<TValue>.Default;

  Result := FValueRules.FComparer.Compare(ALeft, ARight);
end;

constructor TEnexAssociativeCollection<TKey, TValue>.Create(const AKeyRules: TRules<TKey>; const AValueRules: TRules<TValue>);
begin
  FKeyRules := AKeyRules;
  FValueRules := AValueRules;
end;

function TEnexAssociativeCollection<TKey, TValue>.DistinctByKeys: IEnexAssociativeCollection<TKey, TValue>;
begin
  Result := TEnexAssociativeDistinctByKeysCollection<TKey, TValue>.Create(Self);
end;

function TEnexAssociativeCollection<TKey, TValue>.DistinctByValues: IEnexAssociativeCollection<TKey, TValue>;
begin
  Result := TEnexAssociativeDistinctByValuesCollection<TKey, TValue>.Create(Self);
end;

function TEnexAssociativeCollection<TKey, TValue>.GetKeyHashCode(const AValue: TKey): NativeInt;
begin
  { Lazy init }
  if not Assigned(FKeyRules.FEqComparer) then
    FKeyRules.FEqComparer := TEqualityComparer<TKey>.Default;

  Result := FKeyRules.FEqComparer.GetHashCode(AValue);
end;

function TEnexAssociativeCollection<TKey, TValue>.GetValueHashCode(const AValue: TValue): NativeInt;
begin
  { Lazy init }
  if not Assigned(FValueRules.FEqComparer) then
    FValueRules.FEqComparer := TEqualityComparer<TValue>.Default;

  Result := FValueRules.FEqComparer.GetHashCode(AValue);
end;

procedure TEnexAssociativeCollection<TKey, TValue>.HandleKeyRemoved(const AKey: TKey);
begin
  // Nothing!
end;

procedure TEnexAssociativeCollection<TKey, TValue>.HandleValueRemoved(const AValue: TValue);
begin
  // Nothing!
end;

function TEnexAssociativeCollection<TKey, TValue>.Includes(const ACollection: IEnumerable<TPair<TKey, TValue>>): Boolean;
var
  LEnumerator: IEnumerator<TPair<TKey, TValue>>;
begin
  { Retrieve the enumerator object }
  LEnumerator := ACollection.GetEnumerator();

  { Iterate till the last element in the LEnumerator }
  while LEnumerator.MoveNext do
  begin
    if not KeyHasValue(LEnumerator.Current.Key, LEnumerator.Current.Value) then
      Exit(false);
  end;

  { We got here, it means all is OK }
  Result := true;
end;

function TEnexAssociativeCollection<TKey, TValue>.KeyHasValue(const AKey: TKey; const AValue: TValue): Boolean;
var
  LEnumerator: IEnumerator<TPair<TKey, TValue>>;
begin
  { Retrieve the enumerator object and type }
  LEnumerator := GetEnumerator();

  { Iterate till the last element in the LEnumerator }
  while LEnumerator.MoveNext do
  begin
    if KeysAreEqual(LEnumerator.Current.Key, AKey) and
       ValuesAreEqual(LEnumerator.Current.Value, AValue) then
      Exit(true);
  end;

  { No found! }
  Result := false;
end;

function TEnexAssociativeCollection<TKey, TValue>.KeysAreEqual(const ALeft, ARight: TKey): Boolean;
begin
  { Lazy init }
  if not Assigned(FKeyRules.FEqComparer) then
    FKeyRules.FEqComparer := TEqualityComparer<TKey>.Default;

  Result := FKeyRules.FEqComparer.Equals(ALeft, ARight);
end;

function TEnexAssociativeCollection<TKey, TValue>.MaxKey: TKey;
var
  LEnumerator: IEnumerator<TPair<TKey, TValue>>;
begin
  { Retrieve the enumerator object and type }
  LEnumerator := GetEnumerator();

  { Get the first object in the enumeration, otherwise fail! }
  if not LEnumerator.MoveNext() then
    ExceptionHelper.Throw_CollectionEmptyError();

  { Select the first element as comparison base }
  Result := LEnumerator.Current.Key;

  { Iterate till the last element in the LEnumerator }
  while true do
  begin
    if CompareKeys(LEnumerator.Current.Key, Result) > 0 then
      Result := LEnumerator.Current.Key;

    { Exit if we hit the last element }
    if not LEnumerator.MoveNext() then
      Exit;
  end;
end;

function TEnexAssociativeCollection<TKey, TValue>.MaxValue: TValue;
var
  LEnumerator: IEnumerator<TPair<TKey, TValue>>;
begin
  { Retrieve the enumerator object and type }
  LEnumerator := GetEnumerator();

  { Get the first object in the enumeration, otherwise fail! }
  if not LEnumerator.MoveNext() then
    ExceptionHelper.Throw_CollectionEmptyError();

  { Select the first element as comparison base }
  Result := LEnumerator.Current.Value;

  { Iterate till the last element in the LEnumerator }
  while true do
  begin
    if CompareValues(LEnumerator.Current.Value, Result) > 0 then
      Result := LEnumerator.Current.Value;

    { Exit if we hit the last element }
    if not LEnumerator.MoveNext() then
      Exit;
  end;
end;

function TEnexAssociativeCollection<TKey, TValue>.MinKey: TKey;
var
  LEnumerator: IEnumerator<TPair<TKey, TValue>>;
begin
  { Retrieve the enumerator object and type }
  LEnumerator := GetEnumerator();

  { Get the first object in the enumeration, otherwise fail! }
  if not LEnumerator.MoveNext() then
    ExceptionHelper.Throw_CollectionEmptyError();

  { Select the first element as comparison base }
  Result := LEnumerator.Current.Key;

  { Iterate till the last element in the LEnumerator }
  while true do
  begin
    if CompareKeys(LEnumerator.Current.Key, Result) < 0 then
      Result := LEnumerator.Current.Key;

    { Exit if we hit the last element }
    if not LEnumerator.MoveNext() then
      Exit;
  end;
end;

function TEnexAssociativeCollection<TKey, TValue>.MinValue: TValue;
var
  LEnumerator: IEnumerator<TPair<TKey, TValue>>;
begin
  { Retrieve the enumerator object and type }
  LEnumerator := GetEnumerator();

  { Get the first object in the enumeration, otherwise fail! }
  if not LEnumerator.MoveNext() then
    ExceptionHelper.Throw_CollectionEmptyError();

  { Select the first element as comparison base }
  Result := LEnumerator.Current.Value;

  { Iterate till the last element in the LEnumerator }
  while true do
  begin
    if CompareValues(LEnumerator.Current.Value, Result) < 0 then
      Result := LEnumerator.Current.Value;

    { Exit if we hit the last element }
    if not LEnumerator.MoveNext() then
      Exit;
  end;
end;

procedure TEnexAssociativeCollection<TKey, TValue>.NotifyKeyRemoved(const AKey: TKey);
begin
  { Handle stuff }
  if Assigned(FKeyRemoveNotification) then
    FKeyRemoveNotification(AKey)
  else
    HandleKeyRemoved(AKey);
end;

procedure TEnexAssociativeCollection<TKey, TValue>.NotifyValueRemoved(const AValue: TValue);
begin
  { Handle stuff }
  if Assigned(FValueRemoveNotification) then
    FValueRemoveNotification(AValue)
  else
    HandleValueRemoved(AValue);
end;

function TEnexAssociativeCollection<TKey, TValue>.SelectKeys: IEnexCollection<TKey>;
begin
  { Create a selector }
  Result := TEnexSelectKeysCollection<TKey, TValue>.Create(Self);
end;

function TEnexAssociativeCollection<TKey, TValue>.SelectValues: IEnexCollection<TValue>;
begin
  { Create a selector }
  Result := TEnexSelectValuesCollection<TKey, TValue>.Create(Self);
end;

function TEnexAssociativeCollection<TKey, TValue>.ToDictionary: IDictionary<TKey, TValue>;
begin
  Result := TDictionary<TKey, TValue>.Create(Self);
end;

function TEnexAssociativeCollection<TKey, TValue>.ValueForKey(const AKey: TKey): TValue;
var
  LEnumerator: IEnumerator<TPair<TKey, TValue>>;
begin
  { Retrieve the enumerator object and type }
  LEnumerator := GetEnumerator();

  { Iterate till the last element in the LEnumerator }
  while LEnumerator.MoveNext do
  begin
    if KeysAreEqual(LEnumerator.Current.Key, AKey) then
      Exit(LEnumerator.Current.Value);
  end;

  { If nothing found, simply raise an exception }
  ExceptionHelper.Throw_KeyNotFoundError('AKey');
end;

function TEnexAssociativeCollection<TKey, TValue>.ValuesAreEqual(const ALeft, ARight: TValue): Boolean;
begin
  { Lazy init }
  if not Assigned(FValueRules.FEqComparer) then
    FValueRules.FEqComparer := TEqualityComparer<TValue>.Default;

  Result := FValueRules.FEqComparer.Equals(ALeft, ARight);
end;

function TEnexAssociativeCollection<TKey, TValue>.Where(
  const APredicate: TFunc<TKey, TValue, Boolean>): IEnexAssociativeCollection<TKey, TValue>;
begin
  { Check arguments }
  if not Assigned(APredicate) then
    ExceptionHelper.Throw_ArgumentNilError('APredicate');

  { Create a new Enex collection }
  Result := TEnexAssociativeWhereCollection<TKey, TValue>.Create(Self, APredicate, False); // Don't invert the result
end;

function TEnexAssociativeCollection<TKey, TValue>.WhereKeyBetween(const ALower,
  AHigher: TKey): IEnexAssociativeCollection<TKey, TValue>;
var
  LLower, LHigher: TKey;
begin
  { Locals }
  LLower := ALower;
  LHigher := AHigher;

  { Use Where() and pass an anonymous function }
  Result := Where(
    function(Arg1: TKey; Arg2: TValue): Boolean
    begin
      Exit((CompareKeys(Arg1, LLower) >= 0) and (CompareKeys(Arg1, LHigher) <= 0));
    end
  );
end;

function TEnexAssociativeCollection<TKey, TValue>.WhereKeyGreater(
  const ABound: TKey): IEnexAssociativeCollection<TKey, TValue>;
var
  LBound: TKey;
begin
  { Locals }
  LBound := ABound;

  { Use Where() and pass an anonymous function }
  Result := Where(
    function(Arg1: TKey; Arg2: TValue): Boolean
    begin
      Exit(CompareKeys(Arg1, LBound) > 0);
    end
  );
end;

function TEnexAssociativeCollection<TKey, TValue>.WhereKeyGreaterOrEqual(
  const ABound: TKey): IEnexAssociativeCollection<TKey, TValue>;
var
  LBound: TKey;
begin
  { Locals }
  LBound := ABound;

  { Use Where() and pass an anonymous function }
  Result := Where(
    function(Arg1: TKey; Arg2: TValue): Boolean
    begin
      Exit(CompareKeys(Arg1, LBound) >= 0);
    end
  );
end;

function TEnexAssociativeCollection<TKey, TValue>.WhereKeyLower(
  const ABound: TKey): IEnexAssociativeCollection<TKey, TValue>;
var
  LBound: TKey;
  LRules: TRules<TKey>;
begin
  { Locals }
  LBound := ABound;

  { Use Where() and pass an anonymous function }
  Result := Where(
    function(Arg1: TKey; Arg2: TValue): Boolean
    begin
      Exit(CompareKeys(Arg1, LBound) < 0);
    end
  );
end;

function TEnexAssociativeCollection<TKey, TValue>.WhereKeyLowerOrEqual(
  const ABound: TKey): IEnexAssociativeCollection<TKey, TValue>;
var
  LBound: TKey;
begin
  { Locals }
  LBound := ABound;

  { Use Where() and pass an anonymous function }
  Result := Where(
    function(Arg1: TKey; Arg2: TValue): Boolean
    begin
      Exit(CompareKeys(Arg1, LBound) <= 0);
    end
  );
end;

function TEnexAssociativeCollection<TKey, TValue>.WhereNot(
  const APredicate: TFunc<TKey, TValue, Boolean>): IEnexAssociativeCollection<TKey, TValue>;
begin
  { Check arguments }
  if not Assigned(APredicate) then
    ExceptionHelper.Throw_ArgumentNilError('APredicate');

  { Create a new Enex collection }
  Result := TEnexAssociativeWhereCollection<TKey, TValue>.Create(Self, APredicate, True); // Invert the result
end;

function TEnexAssociativeCollection<TKey, TValue>.WhereValueBetween(
  const ALower, AHigher: TValue): IEnexAssociativeCollection<TKey, TValue>;
var
  LLower, LHigher: TValue;
begin
  { Locals }
  LLower := ALower;
  LHigher := AHigher;

  { Use Where() and pass an anonymous function }
  Result := Where(
    function(Arg1: TKey; Arg2: TValue): Boolean
    begin
      Exit((CompareValues(Arg2, LLower) >= 0) and (CompareValues(Arg2, LHigher) <= 0));
    end
  );
end;

function TEnexAssociativeCollection<TKey, TValue>.WhereValueGreater(
  const ABound: TValue): IEnexAssociativeCollection<TKey, TValue>;
var
  LBound: TValue;
begin
  { Locals }
  LBound := ABound;

  { Use Where() and pass an anonymous function }
  Result := Where(
    function(Arg1: TKey; Arg2: TValue): Boolean
    begin
      Exit(CompareValues(Arg2, LBound) > 0);
    end
  );
end;

function TEnexAssociativeCollection<TKey, TValue>.WhereValueGreaterOrEqual(
  const ABound: TValue): IEnexAssociativeCollection<TKey, TValue>;
var
  LBound: TValue;
begin
  { Locals }
  LBound := ABound;

  { Use Where() and pass an anonymous function }
  Result := Where(
    function(Arg1: TKey; Arg2: TValue): Boolean
    begin
      Exit(CompareValues(Arg2, LBound) >= 0);
    end
  );
end;

function TEnexAssociativeCollection<TKey, TValue>.WhereValueLower(
  const ABound: TValue): IEnexAssociativeCollection<TKey, TValue>;
var
  LBound: TValue;
begin
  { Locals }
  LBound := ABound;

  { Use Where() and pass an anonymous function }
  Result := Where(
    function(Arg1: TKey; Arg2: TValue): Boolean
    begin
      Exit(CompareValues(Arg2, LBound) < 0);
    end
  );
end;

function TEnexAssociativeCollection<TKey, TValue>.WhereValueLowerOrEqual(
  const ABound: TValue): IEnexAssociativeCollection<TKey, TValue>;
var
  LBound: TValue;
begin
  { Locals }
  LBound := ABound;

  { Use Where() and pass an anonymous function }
  Result := Where(
    function(Arg1: TKey; Arg2: TValue): Boolean
    begin
      Exit(CompareValues(Arg2, LBound) <= 0);
    end
  );
end;

{ TEnexWhereCollection<T> }

constructor TEnexWhereCollection<T>.Create(const ACollection: TEnexCollection<T>;
  const APredicate: TFunc<T, Boolean>; const AInvertResult: Boolean);
begin
  { Check arguments }
  if not Assigned(APredicate) then
    ExceptionHelper.Throw_ArgumentNilError('APredicate');

  if not Assigned(ACollection) then
    ExceptionHelper.Throw_ArgumentNilError('ACollection');

  inherited Create(ACollection.ElementRules);

  { Assign internals }
  FCollection := ACollection;
  KeepObjectAlive(FCollection);

  FPredicate := APredicate;
  FInvertResult := AInvertResult;
end;

destructor TEnexWhereCollection<T>.Destroy;
begin
  { Delete the enumerable if required }
  ReleaseObject(FCollection, false);

  inherited;
end;

function TEnexWhereCollection<T>.GetEnumerator: IEnumerator<T>;
begin
  { Generate an enumerator }
  Result := TEnumerator.Create(Self);
end;

{ TEnexWhereCollection<T>.TEnumerator }

constructor TEnexWhereCollection<T>.TEnumerator.Create(const ACollection: TEnexWhereCollection<T>);
begin
  { Initialize }
  FCollection := ACollection;
  KeepObjectAlive(FCollection);

  FEnumerator:= ACollection.FCollection.GetEnumerator();
end;

destructor TEnexWhereCollection<T>.TEnumerator.Destroy;
begin
  ReleaseObject(FCollection);
  inherited;
end;

function TEnexWhereCollection<T>.TEnumerator.GetCurrent: T;
begin
  { Get current element of the "sub-enumerable" object }
  Result := FEnumerator.Current;
end;

function TEnexWhereCollection<T>.TEnumerator.MoveNext: Boolean;
begin
  { Iterate until given condition is met on an element }
  while True do
  begin
    Result := FEnumerator.MoveNext;

    { Terminate on sub-enum termination }
    if not Result then
      Exit;

    { Check whether the current element meets the condition and exit }
    { ... otherwise continue to the next iteration }
    if FCollection.FPredicate(FEnumerator.Current) xor FCollection.FInvertResult then
      Exit;
  end;
end;

{ TEnexSelectCollection<T, TOut> }

constructor TEnexSelectCollection<T, TOut>.Create(const ACollection: TEnexCollection<T>;
  const ASelector: TFunc<T, TOut>; const ARules: TRules<TOut>);
begin
  { Check arguments }
  if not Assigned(ASelector) then
    ExceptionHelper.Throw_ArgumentNilError('ASelector');

  if not Assigned(ACollection) then
    ExceptionHelper.Throw_ArgumentNilError('ACollection');

  { Rules ... }
  inherited Create(ARules);

  { Assign internals }
  FCollection := ACollection;
  KeepObjectAlive(FCollection);

  FSelector := ASelector;
end;

destructor TEnexSelectCollection<T, TOut>.Destroy;
begin
  { Delete the enumerable if required }
  ReleaseObject(FCollection, false);

  inherited;
end;

function TEnexSelectCollection<T, TOut>.ElementAt(const AIndex: NativeInt): TOut;
begin
  Result := FSelector(FCollection.ElementAt(AIndex));
end;

function TEnexSelectCollection<T, TOut>.Empty: Boolean;
begin
  Result := FCollection.Empty;
end;

function TEnexSelectCollection<T, TOut>.First: TOut;
begin
  Result := FSelector(FCollection.First);
end;

function TEnexSelectCollection<T, TOut>.GetCount: NativeInt;
begin
  Result := FCollection.GetCount();
end;

function TEnexSelectCollection<T, TOut>.GetEnumerator: IEnumerator<TOut>;
begin
  { Generate an enumerator }
  Result := TEnumerator.Create(Self);
end;

function TEnexSelectCollection<T, TOut>.Last: TOut;
begin
  Result := FSelector(FCollection.Last);
end;

function TEnexSelectCollection<T, TOut>.Single: TOut;
begin
  Result := FSelector(FCollection.Single);
end;

{ TEnexSelectCollection<T, TOut>.TEnumerator }

constructor TEnexSelectCollection<T, TOut>.TEnumerator.Create(const ACollection: TEnexSelectCollection<T, TOut>);
begin
  { Initialize }
  FCollection := ACollection;
  KeepObjectAlive(FCollection);

  FEnumerator := ACollection.FCollection.GetEnumerator();
  FCurrent := default(TOut);
end;

destructor TEnexSelectCollection<T, TOut>.TEnumerator.Destroy;
begin
  ReleaseObject(FCollection);
  inherited;
end;

function TEnexSelectCollection<T, TOut>.TEnumerator.GetCurrent: TOut;
begin
  { Get current element of the "sub-enumerable" object }
  Result := FCurrent;
end;

function TEnexSelectCollection<T, TOut>.TEnumerator.MoveNext: Boolean;
begin
  { Next iteration }
  Result := FEnumerator.MoveNext;

  { Terminate on sub-enum termination }
  if not Result then
    Exit;

  { Return the next "selected" element }
  FCurrent := FCollection.FSelector(FEnumerator.Current);
end;

{ TEnexConcatCollection<T> }

function TEnexConcatCollection<T>.All(const APredicate: TFunc<T, Boolean>): Boolean;
begin
  Result := FCollection1.All(APredicate) and FCollection2.All(APredicate);
end;

function TEnexConcatCollection<T>.Any(const APredicate: TFunc<T, Boolean>): Boolean;
begin
  Result := FCollection1.Any(APredicate) or FCollection2.Any(APredicate);
end;

constructor TEnexConcatCollection<T>.Create(
  const ACollection1: TEnexCollection<T>; const ACollection2: IEnexCollection<T>);
begin
  { Check arguments }
  if not Assigned(ACollection1) then
    ExceptionHelper.Throw_ArgumentNilError('ACollection1');

  if not Assigned(ACollection2) then
    ExceptionHelper.Throw_ArgumentNilError('ACollection2');

  { Rules ... }
  inherited Create(ACollection1.ElementRules);

  { Assign internals }
  FCollection1 := ACollection1;
  KeepObjectAlive(FCollection1);

  FCollection2 := ACollection2;
end;

destructor TEnexConcatCollection<T>.Destroy;
begin
  { Delete the enumerable if required }
  ReleaseObject(FCollection1, false);

  inherited;
end;

function TEnexConcatCollection<T>.Empty: Boolean;
begin
  Result := (GetCount = 0);
end;

function TEnexConcatCollection<T>.GetCount: NativeInt;
begin
  Result := FCollection1.GetCount() + FCollection2.GetCount();
end;

function TEnexConcatCollection<T>.GetEnumerator: IEnumerator<T>;
begin
  { Create enumerator }
  Result := TEnumerator.Create(Self);
end;

{ TEnexConcatCollection<T>.TEnumerator }

constructor TEnexConcatCollection<T>.TEnumerator.Create(const ACollection: TEnexConcatCollection<T>);
begin
  { Initialize }
  FCollection := ACollection;
  KeepObjectAlive(FCollection);

  FEnumerator1 := ACollection.FCollection1.GetEnumerator();
  FEnumerator2 := ACollection.FCollection2.GetEnumerator();
end;

destructor TEnexConcatCollection<T>.TEnumerator.Destroy;
begin
  ReleaseObject(FCollection);
  inherited;
end;

function TEnexConcatCollection<T>.TEnumerator.GetCurrent: T;
begin
  { Pass the first and then the last }
  if Assigned(FEnumerator1) then
    Result := FEnumerator1.Current
  else
    Result := FEnumerator2.Current;
end;

function TEnexConcatCollection<T>.TEnumerator.MoveNext: Boolean;
begin
  if Assigned(FEnumerator1) then
  begin
    { Iterate over 1 }
    Result := FEnumerator1.MoveNext();

    { Succesefully iterated collection 1 }
    if Result then
      Exit;

    { We've reached the bottom of 1 }
    FEnumerator1 := nil;
  end;

  { Iterate over 2 now }
  Result := FEnumerator2.MoveNext();
end;

{ TEnexUnionCollection<T> }

constructor TEnexUnionCollection<T>.Create(
  const ACollection1: TEnexCollection<T>; const ACollection2: IEnexCollection<T>);
begin
  { Check arguments }
  if not Assigned(ACollection1) then
    ExceptionHelper.Throw_ArgumentNilError('ACollection1');

  if not Assigned(ACollection2) then
    ExceptionHelper.Throw_ArgumentNilError('ACollection2');

  { Rules ... }
  inherited Create(ACollection1.ElementRules);

  { Assign internals }
  FCollection1 := ACollection1;
  KeepObjectAlive(FCollection1);

  FCollection2 := ACollection2;
end;

destructor TEnexUnionCollection<T>.Destroy;
begin
  { Delete the enumerable if required }
  ReleaseObject(FCollection1, false);

  inherited;
end;

function TEnexUnionCollection<T>.GetEnumerator: IEnumerator<T>;
begin
  { Create enumerator }
  Result := TEnumerator.Create(Self);
end;

{ TEnexUnionCollection<T>.TEnumerator }

constructor TEnexUnionCollection<T>.TEnumerator.Create(const ACollection: TEnexUnionCollection<T>);
begin
  { Initialize }
  FCollection := ACollection;
  KeepObjectAlive(FCollection);

  FEnumerator1 := ACollection.FCollection1.GetEnumerator();
  FEnumerator2 := ACollection.FCollection2.GetEnumerator();

  { Create an internal set }
  FSet := THashSet<T>.Create(ACollection.FCollection1.ElementRules);
end;

destructor TEnexUnionCollection<T>.TEnumerator.Destroy;
begin
  ReleaseObject(FCollection);
  inherited;
end;

function TEnexUnionCollection<T>.TEnumerator.GetCurrent: T;
begin
  { Pass the first and then the last }
  if Assigned(FEnumerator1) then
    Result := FEnumerator1.Current
  else
    Result := FEnumerator2.Current;
end;

function TEnexUnionCollection<T>.TEnumerator.MoveNext: Boolean;
begin
  if Assigned(FEnumerator1) then
  begin
    { Iterate over 1 }
    Result := FEnumerator1.MoveNext();

    { Succesefully iterated collection 1 }
    if Result then
    begin
      { Add the element to the set }
      FSet.Add(FEnumerator1.Current);
      Exit;
    end;

    { We've reached the bottom of 1 }
    FEnumerator1 := nil;
  end;

  { Continue until we find what we need or we get to the bottom }
  while True do
  begin
    { Iterate over 2 now }
    Result := FEnumerator2.MoveNext();

    { Exit on bad result }
    if not Result then
      Exit;

    { Exit if the element is good }
    if not FSet.Contains(FEnumerator2.Current) then
    begin
      FSet.Add(FEnumerator2.Current);
      Exit;
    end;
  end;
end;

{ TEnexExclusionCollection<T> }

constructor TEnexExclusionCollection<T>.Create(
  const ACollection1: TEnexCollection<T>; const ACollection2: IEnexCollection<T>);
begin
  { Check arguments }
  if not Assigned(ACollection1) then
    ExceptionHelper.Throw_ArgumentNilError('ACollection1');

  if not Assigned(ACollection2) then
    ExceptionHelper.Throw_ArgumentNilError('ACollection2');

  { Rules ... }
  inherited Create(ACollection1.ElementRules);

  { Assign internals }
  FCollection1 := ACollection1;
  KeepObjectAlive(FCollection1);

  FCollection2 := ACollection2;
end;

destructor TEnexExclusionCollection<T>.Destroy;
begin
  { Delete the enumerable if required }
  ReleaseObject(FCollection1, false);

  inherited;
end;

function TEnexExclusionCollection<T>.GetEnumerator: IEnumerator<T>;
begin
  { Create enumerator }
  Result := TEnumerator.Create(Self);
end;

{ TEnexExclusionCollection<T>.TEnumerator }

constructor TEnexExclusionCollection<T>.TEnumerator.Create(const ACollection: TEnexExclusionCollection<T>);
begin
  { Initialize }
  FCollection := ACollection;
  KeepObjectAlive(FCollection);

  FEnumerator := ACollection.FCollection1.GetEnumerator();

  { Create an internal set }
  FSet := THashSet<T>.Create(ACollection.FCollection1.ElementRules, ACollection.FCollection2);
end;

destructor TEnexExclusionCollection<T>.TEnumerator.Destroy;
begin
  ReleaseObject(FCollection);
  inherited;
end;

function TEnexExclusionCollection<T>.TEnumerator.GetCurrent: T;
begin
  { Pass 1's enumerator }
  Result := FEnumerator.Current;
end;

function TEnexExclusionCollection<T>.TEnumerator.MoveNext: Boolean;
begin
  { Continue until we find what we need or we get to the bottom }
  while True do
  begin
    { Iterate over 1 }
    Result := FEnumerator.MoveNext();

    { Exit on bad result }
    if not Result then
      Exit;

    { Exit if the element is good }
    if not FSet.Contains(FEnumerator.Current) then
      Exit;
  end;
end;


{ TEnexIntersectionCollection<T> }

constructor TEnexIntersectionCollection<T>.Create(
  const ACollection1: TEnexCollection<T>; const ACollection2: IEnexCollection<T>);
begin
  { Check arguments }
  if not Assigned(ACollection1) then
    ExceptionHelper.Throw_ArgumentNilError('ACollection1');

  if not Assigned(ACollection2) then
    ExceptionHelper.Throw_ArgumentNilError('ACollection2');

  { Rules ... }
  inherited Create(ACollection1.ElementRules);

  { Assign internals }
  FCollection1 := ACollection1;
  KeepObjectAlive(FCollection1);

  FCollection2 := ACollection2;
end;

destructor TEnexIntersectionCollection<T>.Destroy;
begin
  { Delete the enumerable if required }
  ReleaseObject(FCollection1, false);

  inherited;
end;

function TEnexIntersectionCollection<T>.GetEnumerator: IEnumerator<T>;
begin
  { Create enumerator }
  Result := TEnumerator.Create(Self);
end;

{ Collection.EnexIntersectionCollection<T>.TEnumerator }

constructor TEnexIntersectionCollection<T>.TEnumerator .Create(const ACollection: TEnexIntersectionCollection<T>);
begin
  { Initialize }
  FCollection := ACollection;
  KeepObjectAlive(FCollection);

  FEnumerator := ACollection.FCollection1.GetEnumerator();

  { Create an internal set }
  FSet := THashSet<T>.Create(ACollection.FCollection1.ElementRules, ACollection.FCollection2);
end;

destructor TEnexIntersectionCollection<T>.TEnumerator .Destroy;
begin
  ReleaseObject(FCollection);
  inherited;
end;

function TEnexIntersectionCollection<T>.TEnumerator .GetCurrent: T;
begin
  { Pass 1's enumerator }
  Result := FEnumerator.Current;
end;

function TEnexIntersectionCollection<T>.TEnumerator .MoveNext: Boolean;
begin
  { Continue until we find what we need or we get to the bottom }
  while True do
  begin
    { Iterate over 1 }
    Result := FEnumerator.MoveNext();

    { Exit on bad result }
    if not Result then
      Exit;

    { Exit if the element is good }
    if FSet.Contains(FEnumerator.Current) then
      Exit;
  end;
end;

{ TEnexRangeCollection<T> }

constructor TEnexRangeCollection<T>.Create(const ACollection: TEnexCollection<T>; const AStart, AEnd: NativeInt);
begin
  if AStart < 0 then
    ExceptionHelper.Throw_ArgumentOutOfRangeError('AStart');

  if AEnd < 0 then
    ExceptionHelper.Throw_ArgumentOutOfRangeError('AEnd');

  { Check arguments }
  if not Assigned(ACollection) then
    ExceptionHelper.Throw_ArgumentNilError('ACollection');

  { Rules ... }
  inherited Create(ACollection.ElementRules);

  { Assign internals }
  FCollection := ACollection;
  KeepObjectAlive(FCollection);

  FStart := AStart;
  FEnd := AEnd;
end;

destructor TEnexRangeCollection<T>.Destroy;
begin
  { Delete the enumerable if required }
  ReleaseObject(FCollection, false);

  inherited;
end;

function TEnexRangeCollection<T>.GetEnumerator: IEnumerator<T>;
begin
  { Create the enumerator }
  Result := TEnumerator.Create(Self);
end;

{ TEnexRangeCollection<T>.TEnumerator }

constructor TEnexRangeCollection<T>.TEnumerator.Create(const ACollection: TEnexRangeCollection<T>);
begin
  { Initialize }
  FCollection := ACollection;
  KeepObjectAlive(FCollection);

  FEnumerator := ACollection.FCollection.GetEnumerator();
  FIdx  := 0;
end;

destructor TEnexRangeCollection<T>.TEnumerator.Destroy;
begin
  ReleaseObject(FCollection);
  inherited;
end;

function TEnexRangeCollection<T>.TEnumerator.GetCurrent: T;
begin
  { PAss the current in the sub-enum }
  Result := FEnumerator.Current;
end;

function TEnexRangeCollection<T>.TEnumerator.MoveNext: Boolean;
begin
  { Skip the required amount of elements }
  if (FIdx <= FCollection.FStart) then
  begin
    while (FIdx <= FCollection.FStart) do
    begin
      { Move cursor }
      Result := FEnumerator.MoveNext();

      if not Result then
        Exit;

      Inc(FIdx);
    end;
  end else
  begin
    { Check if we're finished }
    if (FIdx > FCollection.FEnd) then
      Exit(false);

    { Move the cursor next in the sub-enum, and increase index }
    Result := FEnumerator.MoveNext();
    Inc(FIdx);
  end;
end;

{ TEnexDistinctCollection<T> }

constructor TEnexDistinctCollection<T>.Create(const ACollection: TEnexCollection<T>);
begin
  { Check arguments }
  if not Assigned(ACollection) then
    ExceptionHelper.Throw_ArgumentNilError('ACollection');

  inherited Create(ACollection.ElementRules);

  { Assign internals }
  FCollection := ACollection;
  KeepObjectAlive(FCollection);
end;

destructor TEnexDistinctCollection<T>.Destroy;
begin
  { Delete the enumerable if required }
  ReleaseObject(FCollection, false);

  inherited;
end;

function TEnexDistinctCollection<T>.GetEnumerator: IEnumerator<T>;
begin
  { Create an enumerator }
  Result := TEnumerator.Create(Self);
end;

{ TEnexDistinctCollection<T>.TEnumerator }

constructor TEnexDistinctCollection<T>.TEnumerator.Create(const ACollection: TEnexDistinctCollection<T>);
begin
  { Initialize }
  FCollection := ACollection;
  KeepObjectAlive(FCollection);

  FEnumerator := ACollection.FCollection.GetEnumerator();

  { Create an internal set }
  FSet := THashSet<T>.Create(ACollection.FCollection.ElementRules);
end;

destructor TEnexDistinctCollection<T>.TEnumerator.Destroy;
begin
  ReleaseObject(FCollection);
  inherited;
end;

function TEnexDistinctCollection<T>.TEnumerator.GetCurrent: T;
begin
  { Get from sub-enum }
  Result := FEnumerator.Current;
end;

function TEnexDistinctCollection<T>.TEnumerator.MoveNext: Boolean;
begin
  while True do
  begin
    { Iterate }
    Result := FEnumerator.MoveNext;

    if not Result then
      Exit;

    { If the item is distinct, add it to set and continue }
    if not FSet.Contains(FEnumerator.Current) then
    begin
      FSet.Add(FEnumerator.Current);
      Exit;
    end;
  end;
end;

{ TEnexFillCollection<T> }

function TEnexFillCollection<T>.Aggregate(const AAggregator: TFunc<T, T, T>): T;
var
  I: NativeInt;
begin
  { Check arguments }
  if not Assigned(AAggregator) then
    ExceptionHelper.Throw_ArgumentNilError('AAggregator');

  if FCount = 0 then
    ExceptionHelper.Throw_CollectionEmptyError();

  { Select the first element as comparison base }
  Result := FElement;

  { Iterate over the last N - 1 elements }
  for I := 1 to FCount - 1 do
  begin
    { Aggregate a value }
    Result := AAggregator(Result, FElement);
  end;
end;

function TEnexFillCollection<T>.AggregateOrDefault(const AAggregator: TFunc<T, T, T>; const ADefault: T): T;
var
  I: NativeInt;
begin
  { Check arguments }
  if not Assigned(AAggregator) then
    ExceptionHelper.Throw_ArgumentNilError('AAggregator');

  if FCount = 0 then
    Exit(ADefault);

  { Select the first element as comparison base }
  Result := FElement;

  { Iterate over the last N - 1 elements }
  for I := 1 to FCount - 1 do
  begin
    { Aggregate a value }
    Result := AAggregator(Result, FElement);
  end;
end;

function TEnexFillCollection<T>.All(const APredicate: TFunc<T, Boolean>): Boolean;
begin
  if not Assigned(APredicate) then
    ExceptionHelper.Throw_ArgumentNilError('APredicate');

  if not APredicate(FElement) then
    Result := false
  else
    Result := true;
end;

function TEnexFillCollection<T>.Any(const APredicate: TFunc<T, Boolean>): Boolean;
begin
  if not Assigned(APredicate) then
    ExceptionHelper.Throw_ArgumentNilError('APredicate');

  if APredicate(FElement) then
    Result := true
  else
    Result := false;
end;

constructor TEnexFillCollection<T>.Create(const AElement: T; const ACount: NativeInt; const ARules: TRules<T>);
begin
  if ACount <= 0 then
    ExceptionHelper.Throw_ArgumentOutOfRangeError('ACount');

  { Install the type }
  inherited Create(ARules);

  { Copy values in }
  FCount := ACount;
  FElement := AElement;
end;

function TEnexFillCollection<T>.ElementAt(const AIndex: NativeInt): T;
begin
  if (AIndex = FCount) or (AIndex < 0) then
    ExceptionHelper.Throw_ArgumentOutOfRangeError('AIndex');

  Result := FElement;
end;

function TEnexFillCollection<T>.ElementAtOrDefault(const AIndex: NativeInt; const ADefault: T): T;
begin
  if (AIndex = FCount) or (AIndex < 0) then
    Result := ADefault
  else
    Result := FElement;
end;

function TEnexFillCollection<T>.Empty: Boolean;
begin
  Result := (FCount = 0);
end;

function TEnexFillCollection<T>.EqualsTo(const ACollection: IEnumerable<T>): Boolean;
var
  LValue: T;
  I: NativeInt;
begin
  I := 0;

  for LValue in ACollection do
  begin
    if I >= FCount then
      Exit(false);

    if not ElementsAreEqual(FElement, LValue) then
      Exit(false);

    Inc(I);
  end;

  if I < FCount then
    Exit(false);

  Result := true;
end;

function TEnexFillCollection<T>.First: T;
begin
  if FCount = 0 then
    ExceptionHelper.Throw_CollectionEmptyError();

  Result := FElement;
end;

function TEnexFillCollection<T>.FirstOrDefault(const ADefault: T): T;
begin
  if FCount = 0 then
    Result := ADefault
  else
    Result := FElement;
end;

function TEnexFillCollection<T>.GetCount: NativeInt;
begin
  Result := FCount;
end;

function TEnexFillCollection<T>.GetEnumerator: IEnumerator<T>;
begin
  { Create an enumerator }
  Result := TEnumerator.Create(Self);
end;

function TEnexFillCollection<T>.Last: T;
begin
  if FCount = 0 then
    ExceptionHelper.Throw_CollectionEmptyError();

  Result := FElement;
end;

function TEnexFillCollection<T>.LastOrDefault(const ADefault: T): T;
begin
  if FCount = 0 then
    Result := ADefault
  else
    Result := FElement;
end;

function TEnexFillCollection<T>.Max: T;
begin
  if FCount = 0 then
    ExceptionHelper.Throw_CollectionEmptyError();

  Result := FElement;
end;

function TEnexFillCollection<T>.Min: T;
begin
  if FCount = 0 then
    ExceptionHelper.Throw_CollectionEmptyError();

  Result := FElement;
end;

function TEnexFillCollection<T>.Single: T;
begin
  if FCount = 0 then
    ExceptionHelper.Throw_CollectionEmptyError()
  else if FCount = 1 then
    Result := FElement
  else
    ExceptionHelper.Throw_CollectionHasMoreThanOneElement();
end;

function TEnexFillCollection<T>.SingleOrDefault(const ADefault: T): T;
begin
  if FCount = 0 then
    Result := ADefault
  else if FCount = 1 then
    Result := FElement
  else
    ExceptionHelper.Throw_CollectionHasMoreThanOneElement();
end;


{ TEnexFillCollection<T>.TEnumerator }

constructor TEnexFillCollection<T>.TEnumerator.Create(const ACollection: TEnexFillCollection<T>);
begin
  FCollection := ACollection;
  KeepObjectAlive(FCollection);

  FCount := 0;
end;

destructor TEnexFillCollection<T>.TEnumerator.Destroy;
begin
  ReleaseObject(FCollection);
  inherited;
end;

function TEnexFillCollection<T>.TEnumerator.GetCurrent: T;
begin
  { Pass the element }
  Result := FCollection.FElement;
end;

function TEnexFillCollection<T>.TEnumerator.MoveNext: Boolean;
begin
  { Check for end }
  Result := (FCount < FCollection.FCount);

  if not Result then
    Exit;

  Inc(FCount);
end;

{ TEnexSkipCollection<T> }

constructor TEnexSkipCollection<T>.Create(const ACollection: TEnexCollection<T>; const ACount: NativeInt);
begin
  { Check parameters }
  if ACount <= 0 then
    ExceptionHelper.Throw_ArgumentOutOfRangeError('ACount');

  { Check arguments }
  if not Assigned(ACollection) then
    ExceptionHelper.Throw_ArgumentNilError('ACollection');

  { Installing the element type }
  inherited Create(ACollection.ElementRules);

  { Assign internals }
  FCollection := ACollection;
  KeepObjectAlive(FCollection);

  FCount := ACount;
end;

destructor TEnexSkipCollection<T>.Destroy;
begin
  { Delete the enumerable if required }
  ReleaseObject(FCollection, false);

  inherited;
end;

function TEnexSkipCollection<T>.GetEnumerator: IEnumerator<T>;
begin
  { Create the enumerator }
  Result := TEnumerator.Create(Self);
end;

{ TEnexSkipCollection<T>.TEnumerator }

constructor TEnexSkipCollection<T>.TEnumerator.Create(const ACollection: TEnexSkipCollection<T>);
begin
  { Initialize }
  FCollection := ACollection;
  KeepObjectAlive(FCollection);

  FEnumerator := ACollection.FCollection.GetEnumerator();
  FIdx  := 0;
end;

destructor TEnexSkipCollection<T>.TEnumerator.Destroy;
begin
  ReleaseObject(FCollection);
  inherited;
end;

function TEnexSkipCollection<T>.TEnumerator.GetCurrent: T;
begin
  { PAss the current in the sub-enum }
  Result := FEnumerator.Current;
end;

function TEnexSkipCollection<T>.TEnumerator.MoveNext: Boolean;
begin
  { Skip the required amount of elements }
  if (FIdx < FCollection.FCount) then
  begin
    while (FIdx < FCollection.FCount) do
    begin
      { Move cursor }
      Result := FEnumerator.MoveNext();

      if not Result then
        Exit;

      Inc(FIdx);
    end;
  end;

  Result := FEnumerator.MoveNext(); { Move the cursor next in the sub-enum }
end;

{ TEnexTakeCollection<T> }

constructor TEnexTakeCollection<T>.Create(const ACollection: TEnexCollection<T>; const ACount: NativeInt);
begin
  { Check parameters }
  if ACount <= 0 then
    ExceptionHelper.Throw_ArgumentOutOfRangeError('ACount');

  { Check arguments }
  if not Assigned(ACollection) then
    ExceptionHelper.Throw_ArgumentNilError('ACollection');

  { Installing the element type }
  inherited Create(ACollection.ElementRules);

  { Assign internals }
  FCollection := ACollection;
  KeepObjectAlive(FCollection);

  FCount := ACount;
end;

destructor TEnexTakeCollection<T>.Destroy;
begin
  { Delete the enumerable if required }
  ReleaseObject(FCollection, false);

  inherited;
end;

function TEnexTakeCollection<T>.GetEnumerator: IEnumerator<T>;
begin
  { Create the enumerator }
  Result := TEnumerator.Create(Self);
end;

{ TEnexTakeCollection<T>.TEnumerator }

constructor TEnexTakeCollection<T>.TEnumerator.Create(const ACollection: TEnexTakeCollection<T>);
begin
  { Initialize }
  FCollection := ACollection;
  KeepObjectAlive(FCollection);

  FEnumerator := ACollection.FCollection.GetEnumerator();
  FIdx  := 0;
end;

destructor TEnexTakeCollection<T>.TEnumerator.Destroy;
begin
  ReleaseObject(FCollection);
  inherited;
end;

function TEnexTakeCollection<T>.TEnumerator.GetCurrent: T;
begin
  { Pass the current in the sub-enum }
  Result := FEnumerator.Current;
end;

function TEnexTakeCollection<T>.TEnumerator.MoveNext: Boolean;
begin
  { Check if we're finished}
  if (FIdx >= FCollection.FCount) then
    Exit(false);

  { Move the cursor next in the sub-enum, and increase index }
  Result := FEnumerator.MoveNext();
  Inc(FIdx);
end;

{ TEnexTakeWhileCollection<T> }

constructor TEnexTakeWhileCollection<T>.Create(const ACollection: TEnexCollection<T>; const APredicate: TFunc<T, Boolean>);
begin
  { Check arguments }
  if not Assigned(APredicate) then
    ExceptionHelper.Throw_ArgumentNilError('APredicate');

  if not Assigned(ACollection) then
    ExceptionHelper.Throw_ArgumentNilError('ACollection');

  { Install the type }
  inherited Create(ACollection.ElementRules);

  { Assign internals }
  FCollection := ACollection;
  KeepObjectAlive(FCollection);

  FPredicate := APredicate;
end;

destructor TEnexTakeWhileCollection<T>.Destroy;
begin
  { Delete the enumerable if required }
  ReleaseObject(FCollection, false);

  inherited;
end;

function TEnexTakeWhileCollection<T>.GetEnumerator: IEnumerator<T>;
begin
  { Generate an enumerator }
  Result := TEnumerator.Create(Self);
end;

{ TEnexTakeWhileCollection<T>.TEnumerator }

constructor TEnexTakeWhileCollection<T>.TEnumerator.Create(const ACollection: TEnexTakeWhileCollection<T>);
begin
  { Initialize }
  FCollection := ACollection;
  KeepObjectAlive(FCollection);

  FEnumerator:= ACollection.FCollection.GetEnumerator();
end;

destructor TEnexTakeWhileCollection<T>.TEnumerator.Destroy;
begin
  ReleaseObject(FCollection);
  inherited;
end;

function TEnexTakeWhileCollection<T>.TEnumerator.GetCurrent: T;
begin
  { Get current element of the "sub-enumerable" object }
  Result := FEnumerator.Current;
end;

function TEnexTakeWhileCollection<T>.TEnumerator.MoveNext: Boolean;
begin
  Result := FEnumerator.MoveNext;

  { Terminate on sub-enum termination }
  if not Result then
    Exit;

  { When the condition is not met, stop iterating! }
  if not FCollection.FPredicate(FEnumerator.Current) then
    Exit(false);
end;

{ TEnexSkipWhileCollection<T> }

constructor TEnexSkipWhileCollection<T>.Create(const ACollection: TEnexCollection<T>; const APredicate: TFunc<T, Boolean>);
begin
  { Check arguments }
  if not Assigned(APredicate) then
    ExceptionHelper.Throw_ArgumentNilError('APredicate');

  if not Assigned(ACollection) then
    ExceptionHelper.Throw_ArgumentNilError('ACollection');

  { Install the type }
  inherited Create(ACollection.ElementRules);

  { Assign internals }
  FCollection := ACollection;
  KeepObjectAlive(FCollection);

  FPredicate := APredicate;
end;

destructor TEnexSkipWhileCollection<T>.Destroy;
begin
  { Delete the enumerable if required }
  ReleaseObject(FCollection, false);

  inherited;
end;

function TEnexSkipWhileCollection<T>.GetEnumerator: IEnumerator<T>;
begin
  { Generate an enumerator }
  Result := TEnumerator.Create(Self);
end;

{ TEnexSkipWhileCollection<T>.TEnumerator }

constructor TEnexSkipWhileCollection<T>.TEnumerator.Create(const ACollection: TEnexSkipWhileCollection<T>);
begin
  { Initialize }
  FCollection := ACollection;
  KeepObjectAlive(FCollection);

  FEnumerator := ACollection.FCollection.GetEnumerator();
  FStop := false;
end;

destructor TEnexSkipWhileCollection<T>.TEnumerator.Destroy;
begin
  ReleaseObject(FCollection);
  inherited;
end;

function TEnexSkipWhileCollection<T>.TEnumerator.GetCurrent: T;
begin
  { Get current element of the "sub-enumerable" object }
  Result := FEnumerator.Current;
end;

function TEnexSkipWhileCollection<T>.TEnumerator.MoveNext: Boolean;
begin
  { Iterate until given condition is met on an element }
  if not FStop then
  begin
    while not FStop do
    begin
      Result := FEnumerator.MoveNext;

      { Terminate on sub-enum termination }
      if not Result then
        Exit;

      { When condition is met, move next }
      if FCollection.FPredicate(FEnumerator.Current) then
        Continue;

      { Mark as skipped }
      FStop := true;
    end;
  end else
    Result := FEnumerator.MoveNext;
end;

{ TEnexGroupByCollection<T, TGroup> }

constructor TEnexGroupByCollection<T, TBy>.Create(
  const ACollection: TEnexCollection<T>; const ASelector: TFunc<T, TBy>);
begin
  { Check arguments }
  if not Assigned(ASelector) then
    ExceptionHelper.Throw_ArgumentNilError('ASelector');

  if not Assigned(ACollection) then
    ExceptionHelper.Throw_ArgumentNilError('ACollection');

  { Install the type (some default type) }
  inherited Create();

  { Assign internals }
  FCollection := ACollection;
  KeepObjectAlive(FCollection);

  FSelector := ASelector;
end;

destructor TEnexGroupByCollection<T, TBy>.Destroy;
begin
  { Delete the enumerable if required }
  ReleaseObject(FCollection, false);

  inherited;
end;

function TEnexGroupByCollection<T, TBy>.GetEnumerator: IEnumerator<IEnexGroupingCollection<TBy, T>>;
var
  LDictionary: IDictionary<TBy, IList<T>>;
  LList: IList<T>;
  LSrcEnumerator: IEnumerator<T>;
  LDictEnumerator: IEnumerator<TPair<TBy, IList<T>>>;
  LGroup: TBy;
  LOutList: IList<IEnexGroupingCollection<TBy, T>>;
  LGrouping: TEnexGroupingCollection;
  LGroupingIntf: IEnexGroupingCollection<TBy, T>;
begin
  { Initialize the dictionary (need one that preserves the input order) }
  LDictionary := TLinkedDictionary<TBy, IList<T>>.Create();

  { Obtain the source enumerator }
  LSrcEnumerator := FCollection.GetEnumerator();
  while LSrcEnumerator.MoveNext() do
  begin
    LGroup := FSelector(LSrcEnumerator.Current);

    { Try to get the list of groupet input elements }
    if not LDictionary.TryGetValue(LGroup, LList) then
    begin
      LList := TList<T>.Create();
      LDictionary.Add(LGroup, LList);
    end;

    { Add the element that was grouped into the list, and move on ... }
    LList.Add(LSrcEnumerator.Current);
  end;

  { Build result and such things }
  LOutList := TList<IEnexGroupingCollection<TBy, T>>.Create();

  { Get the dictionary enumerator and build output }
  LDictEnumerator := LDictionary.GetEnumerator();
  while LDictEnumerator.MoveNext() do
  begin
    { Initialize the grouping structure }
    LGrouping := TEnexGroupingCollection.Create;
    LGrouping.FBy := LDictEnumerator.Current.Key;
    LGrouping.FList := LDictEnumerator.Current.Value;
    LGroupingIntf := LGrouping;

    { Place it into output }
    LOutList.Add(LGroupingIntf);
  end;

  LDictEnumerator := nil;
  LDictionary := nil;

  { Finally, provide the enumerator }
  Result := LOutList.GetEnumerator();
end;


{ TEnexGroupByCollection<T, TKey>.TEnexGroupingCollection }

function TEnexGroupByCollection<T, TBy>.TEnexGroupingCollection.Aggregate(const AAggregator: TFunc<T, T, T>): T;
begin
  Result := FList.Aggregate(AAggregator);
end;

function TEnexGroupByCollection<T, TBy>.TEnexGroupingCollection.AggregateOrDefault(const AAggregator: TFunc<T, T, T>; const ADefault: T): T;
begin
  Result := FList.AggregateOrDefault(AAggregator, ADefault);
end;

function TEnexGroupByCollection<T, TBy>.TEnexGroupingCollection.All(const APredicate: TFunc<T, Boolean>): Boolean;
begin
  Result := FList.All(APredicate);
end;

function TEnexGroupByCollection<T, TBy>.TEnexGroupingCollection.Any(const APredicate: TFunc<T, Boolean>): Boolean;
begin
  Result := FList.Any(APredicate);
end;

procedure TEnexGroupByCollection<T, TBy>.TEnexGroupingCollection.CopyTo(var AArray: array of T; const AStartIndex: NativeInt);
begin
  FList.CopyTo(AArray, AStartIndex);
end;

function TEnexGroupByCollection<T, TBy>.TEnexGroupingCollection.ElementAt(const AIndex: NativeInt): T;
begin
  Result := FList.ElementAt(AIndex);
end;

function TEnexGroupByCollection<T, TBy>.TEnexGroupingCollection.ElementAtOrDefault(const AIndex: NativeInt; const ADefault: T): T;
begin
  Result := FList.ElementAtOrDefault(AIndex, ADefault);
end;

function TEnexGroupByCollection<T, TBy>.TEnexGroupingCollection.Empty: Boolean;
begin
  Result := FList.Empty;
end;

function TEnexGroupByCollection<T, TBy>.TEnexGroupingCollection.EqualsTo(const ACollection: IEnumerable<T>): Boolean;
begin
  Result := FList.EqualsTo(ACollection);
end;

function TEnexGroupByCollection<T, TBy>.TEnexGroupingCollection.First: T;
begin
  Result := FList.First;
end;

function TEnexGroupByCollection<T, TBy>.TEnexGroupingCollection.FirstOrDefault(const ADefault: T): T;
begin
  Result := FList.FirstOrDefault(ADefault);
end;

function TEnexGroupByCollection<T, TBy>.TEnexGroupingCollection.GetCount: NativeInt;
begin
  Result := FList.Count;
end;

function TEnexGroupByCollection<T, TBy>.TEnexGroupingCollection.GetEnumerator: IEnumerator<T>;
begin
  Result := FList.GetEnumerator();
end;

function TEnexGroupByCollection<T, TBy>.TEnexGroupingCollection.GetKey: TBy;
begin
  Result := FBy;
end;

function TEnexGroupByCollection<T, TBy>.TEnexGroupingCollection.Last: T;
begin
  Result := FList.Last;
end;

function TEnexGroupByCollection<T, TBy>.TEnexGroupingCollection.LastOrDefault(const ADefault: T): T;
begin
  Result := FList.LastOrDefault(ADefault);
end;

function TEnexGroupByCollection<T, TBy>.TEnexGroupingCollection.Max: T;
begin
  Result := FList.Max;
end;

function TEnexGroupByCollection<T, TBy>.TEnexGroupingCollection.Min: T;
begin
  Result := FList.Min;
end;

function TEnexGroupByCollection<T, TBy>.TEnexGroupingCollection.Single: T;
begin
  Result := FList.Single;
end;

function TEnexGroupByCollection<T, TBy>.TEnexGroupingCollection.SingleOrDefault(const ADefault: T): T;
begin
  Result := FList.SingleOrDefault(ADefault);
end;

{ TEnexSelectKeysCollection<TKey, TValue> }

constructor TEnexSelectKeysCollection<TKey, TValue>.Create(const ACollection: TEnexAssociativeCollection<TKey, TValue>);
begin
  { Check arguments }
  if not Assigned(ACollection) then
    ExceptionHelper.Throw_ArgumentNilError('ACollection');

  { Install the type }
  inherited Create(ACollection.KeyRules);

  { Assign internals }
  FCollection := ACollection;
  KeepObjectAlive(FCollection);
end;

destructor TEnexSelectKeysCollection<TKey, TValue>.Destroy;
begin
  { Delete the enumerable if required }
  ReleaseObject(FCollection, false);

  inherited;
end;

function TEnexSelectKeysCollection<TKey, TValue>.GetCount: NativeInt;
begin
  Result := FCollection.GetCount();
end;

function TEnexSelectKeysCollection<TKey, TValue>.GetEnumerator: IEnumerator<TKey>;
begin
  { Generate an enumerator }
  Result := TEnumerator.Create(Self);
end;

{ TEnexSelectKeysCollection<TKey, TValue>.TEnumerator }

constructor TEnexSelectKeysCollection<TKey, TValue>.TEnumerator.Create(
  const ACollection: TEnexSelectKeysCollection<TKey, TValue>);
begin
  { Initialize }
  FCollection := ACollection;
  KeepObjectAlive(FCollection);

  FEnumerator:= ACollection.FCollection.GetEnumerator();
  FCurrent := default(TKey);
end;

destructor TEnexSelectKeysCollection<TKey, TValue>.TEnumerator.Destroy;
begin
  ReleaseObject(FCollection);
  inherited;
end;

function TEnexSelectKeysCollection<TKey, TValue>.TEnumerator.GetCurrent: TKey;
begin
  { Get current element of the "sub-enumerable" object }
  Result := FCurrent;
end;

function TEnexSelectKeysCollection<TKey, TValue>.TEnumerator.MoveNext: Boolean;
begin
  { Next iteration }
  Result := FEnumerator.MoveNext;

  { Terminate on sub-enum termination }
  if not Result then
    Exit;

  { Return the next "selected" key }
  FCurrent := FEnumerator.Current.Key;
end;

{ TEnexSelectValuesCollection<TKey, TValue> }

constructor TEnexSelectValuesCollection<TKey, TValue>.Create(
  const ACollection: TEnexAssociativeCollection<TKey, TValue>);
begin
  { Check arguments }
  if not Assigned(ACollection) then
    ExceptionHelper.Throw_ArgumentNilError('ACollection');

  { Install the type }
  inherited Create(ACollection.ValueRules);

  { Assign internals }
  FCollection := ACollection;

  KeepObjectAlive(FCollection);
end;

destructor TEnexSelectValuesCollection<TKey, TValue>.Destroy;
begin
  { Delete the enumerable if required }
  ReleaseObject(FCollection, false);

  inherited;
end;

function TEnexSelectValuesCollection<TKey, TValue>.GetCount: NativeInt;
begin
  Result := FCollection.GetCount();
end;

function TEnexSelectValuesCollection<TKey, TValue>.GetEnumerator: IEnumerator<TValue>;
begin
  { Generate an enumerator }
  Result := TEnumerator.Create(Self);
end;

{ TEnexSelectValuesCollection<TKey, TValue>.TEnumerator }

constructor TEnexSelectValuesCollection<TKey, TValue>.TEnumerator.Create(
  const ACollection: TEnexSelectValuesCollection<TKey, TValue>);
begin
  { Initialize }
  FCollection := ACollection;
  KeepObjectAlive(FCollection);

  FEnumerator:= ACollection.FCollection.GetEnumerator();
  FCurrent := default(TValue);
end;

destructor TEnexSelectValuesCollection<TKey, TValue>.TEnumerator.Destroy;
begin
  ReleaseObject(FCollection);
  inherited;
end;

function TEnexSelectValuesCollection<TKey, TValue>.TEnumerator.GetCurrent: TValue;
begin
  { Get current element of the "sub-enumerable" object }
  Result := FCurrent;
end;

function TEnexSelectValuesCollection<TKey, TValue>.TEnumerator.MoveNext: Boolean;
begin
  { Next iteration }
  Result := FEnumerator.MoveNext;

  { Terminate on sub-enum termination }
  if not Result then
    Exit;

  { Return the next "selected" key }
  FCurrent := FEnumerator.Current.Value;
end;

{ TEnexAssociativeWhereCollection<TKey, TValue> }

constructor TEnexAssociativeWhereCollection<TKey, TValue>.Create(
  const ACollection: TEnexAssociativeCollection<TKey, TValue>;
  const APredicate: TFunc<TKey, TValue, Boolean>;
  const AInvertResult: Boolean);
begin
  { Check arguments }
  if not Assigned(APredicate) then
    ExceptionHelper.Throw_ArgumentNilError('APredicate');

  if not Assigned(ACollection) then
    ExceptionHelper.Throw_ArgumentNilError('ACollection');

  { Install types }
  inherited Create(ACollection.KeyRules, ACollection.ValueRules);

  { Assign internals }
  FCollection := ACollection;
  KeepObjectAlive(FCollection);

  FPredicate := APredicate;

  FInvertResult := AInvertResult;
end;

destructor TEnexAssociativeWhereCollection<TKey, TValue>.Destroy;
begin
  { Delete the enumerable if required }
  ReleaseObject(FCollection, false);

  inherited;
end;

function TEnexAssociativeWhereCollection<TKey, TValue>.GetEnumerator: IEnumerator<TPair<TKey, TValue>>;
begin
  { Generate an enumerator }
  Result := TEnumerator.Create(Self);
end;

{ TEnexAssociativeWhereCollection<TKey, TValue>.TEnumerator }

constructor TEnexAssociativeWhereCollection<TKey, TValue>.TEnumerator.Create(
  const ACollection: TEnexAssociativeWhereCollection<TKey, TValue>);
begin
  { Initialize }
  FCollection := ACollection;
  KeepObjectAlive(FCollection);

  FEnumerator := ACollection.FCollection.GetEnumerator();
end;

destructor TEnexAssociativeWhereCollection<TKey, TValue>.TEnumerator.Destroy;
begin
  ReleaseObject(FCollection);
  inherited;
end;

function TEnexAssociativeWhereCollection<TKey, TValue>.TEnumerator.GetCurrent: TPair<TKey, TValue>;
begin
  { Get current element of the "sub-enumerable" object }
  Result := FEnumerator.Current;
end;

function TEnexAssociativeWhereCollection<TKey, TValue>.TEnumerator.MoveNext: Boolean;
begin
  { Iterate until given condition is met on an element }
  while True do
  begin
    Result := FEnumerator.MoveNext;

    { Terminate on sub-enum termination }
    if not Result then
      Exit;

    { Check whether the current element meets the condition and exit }
    { ... otherwise continue to the next iteration }
    if FCollection.FPredicate(FEnumerator.Current.Key, FEnumerator.Current.Value) xor FCollection.FInvertResult then
      Exit;
  end;
end;

{ TCollection.EnexAssociativeDistinctByKeysCollection<TKey, TValue> }

constructor TEnexAssociativeDistinctByKeysCollection<TKey, TValue>.Create(
  const ACollection: TEnexAssociativeCollection<TKey, TValue>);
begin
  { Check arguments }
  if not Assigned(ACollection) then
    ExceptionHelper.Throw_ArgumentNilError('ACollection');

  { Install types }
  inherited Create(ACollection.KeyRules, ACollection.ValueRules);

  { Assign internals }
  FCollection := ACollection;
  KeepObjectAlive(FCollection);
end;

destructor TEnexAssociativeDistinctByKeysCollection<TKey, TValue>.Destroy;
begin
  { Delete the enumerable if required }
  ReleaseObject(FCollection, false);

  inherited;
end;

function TEnexAssociativeDistinctByKeysCollection<TKey, TValue>.GetEnumerator: IEnumerator<TPair<TKey, TValue>>;
begin
  { Create an enumerator }
  Result := TEnumerator.Create(Self);
end;

{ TEnexAssociativeDistinctByKeysCollection<TKey, TValue>.TEnumerator }

constructor TEnexAssociativeDistinctByKeysCollection<TKey, TValue>.TEnumerator.Create(
  const ACollection: TEnexAssociativeDistinctByKeysCollection<TKey, TValue>);
begin
  { Initialize }
  FCollection := ACollection;
  KeepObjectAlive(FCollection);

  FEnumerator := ACollection.FCollection.GetEnumerator();

  { Create an internal set }
  FSet := THashSet<TKey>.Create(ACollection.FCollection.KeyRules);
end;

destructor TEnexAssociativeDistinctByKeysCollection<TKey, TValue>.TEnumerator.Destroy;
begin
  ReleaseObject(FCollection);
  inherited;
end;

function TEnexAssociativeDistinctByKeysCollection<TKey, TValue>.TEnumerator.GetCurrent: TPair<TKey, TValue>;
begin
  { Get from sub-enum }
  Result := FEnumerator.Current;
end;

function TEnexAssociativeDistinctByKeysCollection<TKey, TValue>.TEnumerator.MoveNext: Boolean;
begin
  while True do
  begin
    { Iterate }
    Result := FEnumerator.MoveNext;

    if not Result then
      Exit;

    { If the item is distinct, add it to set and continue }
    if not FSet.Contains(FEnumerator.Current.Key) then
    begin
      FSet.Add(FEnumerator.Current.Key);
      Exit;
    end;
  end;
end;


{ TEnexAssociativeDistinctByValuesCollection<TKey, TValue> }

constructor TEnexAssociativeDistinctByValuesCollection<TKey, TValue>.Create(
  const ACollection: TEnexAssociativeCollection<TKey, TValue>);
begin
  { Check arguments }
  if not Assigned(ACollection) then
    ExceptionHelper.Throw_ArgumentNilError('ACollection');

  { Install types }
  inherited Create(ACollection.KeyRules, ACollection.ValueRules);

  { Assign internals }
  FCollection := ACollection;
  KeepObjectAlive(FCollection);
end;

destructor TEnexAssociativeDistinctByValuesCollection<TKey, TValue>.Destroy;
begin
  { Delete the enumerable if required }
  ReleaseObject(FCollection, false);

  inherited;
end;

function TEnexAssociativeDistinctByValuesCollection<TKey, TValue>.GetEnumerator: IEnumerator<TPair<TKey, TValue>>;
begin
  { Create an enumerator }
  Result := TEnumerator.Create(Self);
end;

{ TEnexAssociativeDistinctByValuesCollection<TKey, TValue>.TEnumerator }

constructor TEnexAssociativeDistinctByValuesCollection<TKey, TValue>.TEnumerator.Create(
  const ACollection: TEnexAssociativeDistinctByValuesCollection<TKey, TValue>);
begin
  { Initialize }
  FCollection := ACollection;
  KeepObjectAlive(FCollection);

  FEnumerator := ACollection.FCollection.GetEnumerator();

  { Create an internal set }
  FSet := THashSet<TValue>.Create(ACollection.FCollection.ValueRules);
end;

destructor TEnexAssociativeDistinctByValuesCollection<TKey, TValue>.TEnumerator.Destroy;
begin
  ReleaseObject(FCollection);
  inherited;
end;

function TEnexAssociativeDistinctByValuesCollection<TKey, TValue>.TEnumerator.GetCurrent: TPair<TKey, TValue>;
begin
  { Get from sub-enum }
  Result := FEnumerator.Current;
end;

function TEnexAssociativeDistinctByValuesCollection<TKey, TValue>.TEnumerator.MoveNext: Boolean;
begin
  while True do
  begin
    { Iterate }
    Result := FEnumerator.MoveNext;

    if not Result then
      Exit;

    { If the item is distinct, add it to set and continue }
    if not FSet.Contains(FEnumerator.Current.Value) then
    begin
      FSet.Add(FEnumerator.Current.Value);
      Exit;
    end;
  end;
end;

{ TEnexSelectClassCollection<T, TOut> }

constructor TEnexSelectClassCollection<T, TOut>.Create(const ACollection: TEnexCollection<T>; const ARules: TRules<TOut>);
begin
  { Check arguments }
  if not Assigned(ACollection) then
    ExceptionHelper.Throw_ArgumentNilError('ACollection');

  { Installing the element type }
  inherited Create(ARules);

  { Assign internals }
  FCollection := ACollection;
  KeepObjectAlive(FCollection);
end;

destructor TEnexSelectClassCollection<T, TOut>.Destroy;
begin
  { Delete the enumerable if required }
  ReleaseObject(FCollection, false);

  inherited;
end;

function TEnexSelectClassCollection<T, TOut>.GetEnumerator: IEnumerator<TOut>;
begin
  { Generate an enumerator }
  Result := TEnumerator.Create(Self);
end;

{ TEnexSelectClassCollection<T, TOut>.TEnumerator }

constructor TEnexSelectClassCollection<T, TOut>.TEnumerator.Create(const ACollection: TEnexSelectClassCollection<T, TOut>);
begin
  { Initialize }
  FCollection := ACollection;
  KeepObjectAlive(FCollection);

  FEnumerator := ACollection.FCollection.GetEnumerator();
  FCurrent := default(TOut);
end;

destructor TEnexSelectClassCollection<T, TOut>.TEnumerator.Destroy;
begin
  ReleaseObject(FCollection);
  inherited;
end;

function TEnexSelectClassCollection<T, TOut>.TEnumerator.GetCurrent: TOut;
begin
  { Get current element of the "sub-enumerable" object }
  Result := FCurrent;
end;

function TEnexSelectClassCollection<T, TOut>.TEnumerator.MoveNext: Boolean;
begin
  { Iterate until given condition is met on an element }
  while True do
  begin
    Result := FEnumerator.MoveNext;

    { Terminate on sub-enum termination }
    if not Result then
      Exit;

    { Check if T is TOut. Exit if yes}
    if Assigned(FEnumerator.Current) and FEnumerator.Current.InheritsFrom(TOut) then
    begin
      FCurrent := TOut(TObject(FEnumerator.Current));
      Exit;
    end;
  end;
end;

{ TRules<T> }

class function TRules<T>.Create(const AComparer: IComparer<T>;
  const AEqualityComparer: IEqualityComparer<T>): TRules<T>;
begin
  if not Assigned(AComparer) then
    ExceptionHelper.Throw_ArgumentNilError('AComparer');

  if not Assigned(AEqualityComparer) then
    ExceptionHelper.Throw_ArgumentNilError('AEqualityComparer');

  { Initialize }
  Result.FComparer := AComparer;
  Result.FEqComparer := AEqualityComparer;
end;

class function TRules<T>.Custom(const AComparer: TCustomComparer<T>): TRules<T>;
begin
  if not Assigned(AComparer) then
    ExceptionHelper.Throw_ArgumentNilError('AComparer');

  { Init with proper stuff }
  Result.FComparer := AComparer;
  Result.FEqComparer := AComparer;
end;

class function TRules<T>.Default: TRules<T>;
begin
  { Init with proper stuff }
  Result.FComparer := TComparer<T>.Default;
  Result.FEqComparer := TEqualityComparer<T>.Default;
end;

{ TRefCountedObject }

procedure TRefCountedObject.AfterConstruction;
begin
  FInConstruction := false;
  inherited AfterConstruction();
end;

function TRefCountedObject.ExtractReference: IInterface;
var
  LRefCount: NativeInt;
begin
  { While constructing, an object has an implicit LRefCount count of 1 }
  if FInConstruction then
    LRefCount := 1
  else
    LRefCount := 0;

  {
      If the object is referenced in other places as an
      interface, get a new one, otherwise return nil
   }
  if RefCount > LRefCount then
    Result := Self
  else
    Result := nil;
end;

procedure TRefCountedObject.KeepObjectAlive(const AObject: TRefCountedObject);
var
  I, LKALen: NativeInt;
  LIntfRef: IInterface;
begin
  { Skip nil references }
  if not Assigned(AObject) then
    Exit;

  { Cannot self-ref! }
  if AObject = Self then
    ExceptionHelper.Throw_CannotSelfReferenceError();

  { Extract an optional reference, do not continue if failed }
  LIntfRef := AObject.ExtractReference();
  if not Assigned(LIntfRef) then
    Exit;

  LKALen := Length(FKeepAliveList);

  { Find a free spot }
  if LKALen > 0 then
    for I := 0 to LKALen - 1 do
      if not Assigned(FKeepAliveList[I]) then
      begin
        FKeepAliveList[I] := LIntfRef;
        Exit;
      end;

  { No free spots, extend array and insert the ref there }
  SetLength(FKeepAliveList, LKALen + 1);
  FKeepAliveList[LKALen] := LIntfRef;
end;

class function TRefCountedObject.NewInstance: TObject;
begin
  Result := inherited NewInstance();

  { Set in construction! }
  TRefCountedObject(Result).FInConstruction := true;
end;

procedure TRefCountedObject.ReleaseObject(const AObject: TRefCountedObject; const AFreeObject: Boolean);
var
  I, LKALen: NativeInt;
  LIntfRef: IInterface;
begin
  { Do nothing on nil references, since it may be calle din destructors }
  if not Assigned(AObject) then
    Exit;

  { Cannot self-ref! }
  if AObject = Self then
    ExceptionHelper.Throw_CannotSelfReferenceError();

  { Extract an optional reference, if none received, exit }
  LIntfRef := AObject.ExtractReference();
  if not Assigned(LIntfRef) then
  begin
    if AFreeObject then
      AObject.Free;

    Exit;
  end;

  LKALen := Length(FKeepAliveList);

  { Find a free spot }
  if LKALen > 0 then
    for I := 0 to LKALen - 1 do
      if FKeepAliveList[I] = LIntfRef then
      begin
        { Release the spot and kill references to the interface }
        FKeepAliveList[I] := nil;
        LIntfRef := nil;
        Exit;
      end;
end;

{ ExceptionHelper }

class procedure ExceptionHelper.Throw_ArgumentNilError(const ArgName: String);
begin
  raise EArgumentNilException.CreateFmt(SNilArgument, [ArgName]);
end;

class procedure ExceptionHelper.Throw_ArgumentOutOfRangeError(const ArgName: String);
begin
  raise EArgumentOutOfRangeException.CreateFmt(SOutOfRangeArgument, [ArgName]);
end;

class procedure ExceptionHelper.Throw_ArgumentOutOfSpaceError(const ArgName: String);
begin
  raise EArgumentOutOfSpaceException.CreateFmt(SOutOfSpaceArgument, [ArgName]);
end;

class procedure ExceptionHelper.Throw_CannotSelfReferenceError;
begin
  raise ECannotSelfReferenceException.Create(SCannotSelfReference);
end;

class procedure ExceptionHelper.Throw_CollectionChangedError;
begin
  raise ECollectionChangedException.Create(SParentCollectionChanged);
end;

class procedure ExceptionHelper.Throw_CollectionEmptyError;
begin
  raise ECollectionEmptyException.Create(SEmptyCollection);
end;

class procedure ExceptionHelper.Throw_CollectionHasMoreThanOneElement;
begin
  raise ECollectionNotOneException.Create(SCollectionHasMoreThanOneElements);
end;

class procedure ExceptionHelper.Throw_CollectionHasNoFilteredElements;
begin
  raise ECollectionFilteredEmptyException.Create(SCollectionHasNoFilteredElements);
end;

class procedure ExceptionHelper.Throw_DuplicateKeyError(const ArgName: String);
begin
  raise EDuplicateKeyException.CreateFmt(SDuplicateKey, [ArgName]);
end;

class procedure ExceptionHelper.Throw_KeyNotFoundError(const ArgName: String);
begin
  raise EKeyNotFoundException.CreateFmt(SKeyNotFound, [ArgName]);
end;

class procedure ExceptionHelper.Throw_TypeDoesNotExposeMember(const MemberName: String);
begin
  raise ENotSupportedException.CreateFmt(STypeDoesNotExposeMember, [MemberName]);
end;

class procedure ExceptionHelper.Throw_TypeNotAClassError(const TypeName: String);
begin
  raise ENotSupportedException.CreateFmt(STypeNotAClass, [TypeName]);
end;

end.

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

unit Collections.BidiMaps;
interface
uses SysUtils,
     Generics.Defaults,
     Generics.Collections,
     Collections.Base,
     Collections.MultiMaps;

type
  ///  <summary>The base abstract class for all <c>bidi-maps</c> in this package.</summary>
  TAbstractBidiMap<TKey, TValue> = class abstract(TEnexAssociativeCollection<TKey, TValue>, IBidiMap<TKey, TValue>)
  private
    FByKeyMap: IDistinctMultiMap<TKey, TValue>;
    FByValueMap: IDistinctMultiMap<TValue, TKey>;

    { Got from the underlying collections }
    FValueCollection: IEnexCollection<TValue>;
    FKeyCollection: IEnexCollection<TKey>;

  protected
    ///  <summary>Specifies the internal map used as back-end to store key relations.</summary>
    ///  <returns>A map used as back-end.</summary>
    property ByKeyMap: IDistinctMultiMap<TKey, TValue> read FByKeyMap;

    ///  <summary>Specifies the internal map used as back-end to store value relations.</summary>
    ///  <returns>A map used as back-end.</summary>
    property ByValueMap: IDistinctMultiMap<TValue, TKey> read FByValueMap;

    ///  <summary>Called when the map needs to initialize its internal key map.</summary>
    ///  <param name="AKeyRules">The rule set describing the keys.</param>
    function CreateKeyMap(const AKeyRules: TRules<TKey>;
      const AValueRules: TRules<TValue>): IDistinctMultiMap<TKey, TValue>; virtual; abstract;

    ///  <summary>Called when the map needs to initialize its internal value map.</summary>
    ///  <param name="AValueRules">The rule set describing the values.</param>
    function CreateValueMap(const AValueRules: TRules<TValue>;
      const AKeyRules: TRules<TKey>): IDistinctMultiMap<TValue, TKey>; virtual; abstract;

    ///  <summary>Returns the number of pairs in the bidi-map.</summary>
    ///  <returns>A positive value specifying the total number of pairs in the bidi-map.</returns>
    function GetCount(): NativeInt; override;

    ///  <summary>Returns the collection of keys associated with a value.</summary>
    ///  <param name="AValue">The value for which to obtain the associated keys.</param>
    ///  <returns>An Enex collection that contains the values associated with this key.</returns>
    ///  <exception cref="Collections.Base|EKeyNotFoundException">The value is not found in the bidi-map.</exception>
    function GetKeyList(const AValue: TValue): IEnexCollection<TKey>;

    ///  <summary>Returns the collection of values associated with a key.</summary>
    ///  <param name="AKey">The key for which to obtain the associated values.</param>
    ///  <returns>An Enex collection that contains the values associated with this key.</returns>
    ///  <exception cref="Collections.Base|EKeyNotFoundException">The key is not found in the bidi-map.</exception>
    function GetValueList(const AKey: TKey): IEnexCollection<TValue>;
  public
    ///  <summary>Creates a new instance of this class.</summary>
    ///  <remarks>The default rule set is requested.</remarks>
    constructor Create(); overload;

    ///  <summary>Creates a new instance of this class.</summary>
    ///  <param name="ACollection">A collection to copy pairs from.</param>
    ///  <exception cref="SysUtils|EArgumentNilException"><paramref name="ACollection"/> is <c>nil</c>.</exception>
    ///  <remarks>The default rule set is requested.</remarks>
    constructor Create(const ACollection: IEnumerable<TPair<TKey,TValue>>); overload;

    ///  <summary>Creates a new instance of this class.</summary>
    ///  <param name="AArray">An array to copy pairs from.</param>
    ///  <remarks>The default rule set is requested.</remarks>
    constructor Create(const AArray: array of TPair<TKey,TValue>); overload;

    ///  <summary>Creates a new instance of this class.</summary>
    ///  <param name="AKeyRules">A rule set describing the keys in the bidi-map.</param>
    ///  <param name="AValueRules">A rule set describing the values in the bidi-map.</param>
    constructor Create(const AKeyRules: TRules<TKey>; const AValueRules: TRules<TValue>); overload;

    ///  <summary>Creates a new instance of this class.</summary>
    ///  <param name="AKeyRules">A rule set describing the keys in the bidi-map.</param>
    ///  <param name="AValueRules">A rule set describing the values in the bidi-map.</param>
    ///  <param name="ACollection">A collection to copy pairs from.</param>
    ///  <exception cref="SysUtils|EArgumentNilException"><paramref name="ACollection"/> is <c>nil</c>.</exception>
    constructor Create(const AKeyRules: TRules<TKey>; const AValueRules: TRules<TValue>;
          const ACollection: IEnumerable<TPair<TKey,TValue>>); overload;

    ///  <summary>Creates a new instance of this class.</summary>
    ///  <param name="AKeyRules">A rule set describing the keys in the bidi-map.</param>
    ///  <param name="AValueRules">A rule set describing the values in the bidi-map.</param>
    ///  <param name="AArray">An array to copy pairs from.</param>
    constructor Create(const AKeyRules: TRules<TKey>; const AValueRules: TRules<TValue>;
          const AArray: array of TPair<TKey,TValue>); overload;

    ///  <summary>Destroys this instance.</summary>
    ///  <remarks>Do not call this method directly; call <c>Free</c> instead.</remarks>
    destructor Destroy(); override;

    ///  <summary>Clears the contents of the bidi-map.</summary>
    procedure Clear();

    ///  <summary>Adds a key-value pair to the bidi-map.</summary>
    ///  <param name="APair">The key-value pair to add.</param>
    ///  <exception cref="Collections.Base|EDuplicateKeyException">The map already contains a pair with the given key.</exception>
    procedure Add(const APair: TPair<TKey, TValue>); overload;

    ///  <summary>Adds a key-value pair to the bidi-map.</summary>
    ///  <param name="AKey">The key of the pair.</param>
    ///  <param name="AValue">The value associated with the key.</param>
    ///  <exception cref="Collections.Base|EDuplicateKeyException">The map already contains a pair with the given key.</exception>
    procedure Add(const AKey: TKey; const AValue: TValue); overload;

    ///  <summary>Removes a key-value pair using a given key.</summary>
    ///  <param name="AKey">The key (and its associated values) to remove.</param>
    ///  <remarks>This method removes all the values that are associated with the given key. The rule set's cleanup
    ///  routines are used to clean up the values that are dropped from the bidi-map.</remarks>
    procedure RemoveKey(const AKey: TKey);

    ///  <summary>Removes a key-value pair using a given key.</summary>
    ///  <param name="AKey">The key of the pair.</param>
    ///  <remarks>If the specified key was not found in the bidi-map, nothing happens.</remarks>
    procedure Remove(const AKey: TKey); overload;

    ///  <summary>Removes a key-value pair using a given value.</summary>
    ///  <param name="AValue">The value (and its associated keys) to remove.</param>
    ///  <remarks>This method removes all the keys that are associated with the given value. The rule set's cleanup
    ///  routines are used to clean up the keys that are dropped from the bidi-map.</remarks>
    procedure RemoveValue(const AValue: TValue);

    ///  <summary>Removes a specific key-value combination.</summary>
    ///  <param name="AKey">The key to remove.</param>
    ///  <param name="AValue">The value to remove.</param>
    ///  <remarks>This method only removes a key-value combination if that combination actually exists in the bidi-map.
    ///  If the key is associated with another value, nothing happens.</remarks>
    procedure Remove(const AKey: TKey; const AValue: TValue); overload;

    ///  <summary>Removes a key-value combination.</summary>
    ///  <param name="APair">The pair to remove.</param>
    ///  <remarks>This method only removes a key-value combination if that combination actually exists in the bidi-map.
    ///  If the key is associated with another value, nothing happens.</remarks>
    procedure Remove(const APair: TPair<TKey, TValue>); overload;

    ///  <summary>Checks whether the map contains a key-value pair identified by the given key.</summary>
    ///  <param name="AKey">The key to check for.</param>
    ///  <returns><c>True</c> if the map contains a pair identified by the given key; <c>False</c> otherwise.</returns>
    function ContainsKey(const AKey: TKey): Boolean;

    ///  <summary>Checks whether the map contains a key-value pair that contains a given value.</summary>
    ///  <param name="AValue">The value to check for.</param>
    ///  <returns><c>True</c> if the map contains a pair containing the given value; <c>False</c> otherwise.</returns>
    function ContainsValue(const AValue: TValue): Boolean;

    ///  <summary>Checks whether the map contains the given key-value combination.</summary>
    ///  <param name="AKey">The key associated with the value.</param>
    ///  <param name="AValue">The value associated with the key.</param>
    ///  <returns><c>True</c> if the map contains the given association; <c>False</c> otherwise.</returns>
    function ContainsPair(const AKey: TKey; const AValue: TValue): Boolean; overload;

    ///  <summary>Checks whether the map contains a given key-value combination.</summary>
    ///  <param name="APair">The key-value pair combination.</param>
    ///  <returns><c>True</c> if the map contains the given association; <c>False</c> otherwise.</returns>
    function ContainsPair(const APair: TPair<TKey, TValue>): Boolean; overload;

    ///  <summary>Returns the collection of values associated with a key.</summary>
    ///  <param name="AKey">The key for which to obtain the associated values.</param>
    ///  <returns>An Enex collection that contains the values associated with this key.</returns>
    ///  <exception cref="Collections.Base|EKeyNotFoundException">The key is not found in the bidi-map.</exception>
    property ByKey[const AKey: TKey]: IEnexCollection<TValue> read GetValueList;

    ///  <summary>Returns the collection of keys associated with a value.</summary>
    ///  <param name="AValue">The value for which to obtain the associated keys.</param>
    ///  <returns>An Enex collection that contains the values associated with this key.</returns>
    ///  <exception cref="Collections.Base|EKeyNotFoundException">The value is not found in the bidi-map.</exception>
    property ByValue[const AValue: TValue]: IEnexCollection<TKey> read GetKeyList;

    ///  <summary>Specifies the collection that contains only the keys.</summary>
    ///  <returns>An Enex collection that contains all the keys stored in the bidi-map.</returns>
    property Keys: IEnexCollection<TKey> read FKeyCollection;

    ///  <summary>Specifies the collection that contains only the values.</summary>
    ///  <returns>An Enex collection that contains all the values stored in the bidi-map.</returns>
    property Values: IEnexCollection<TValue> read FValueCollection;

    ///  <summary>Returns the number of pairs in the bidi-map.</summary>
    ///  <returns>A positive value specifying the total number of pairs in the bidi-map.</returns>
    property Count: NativeInt read GetCount;

    ///  <summary>Returns a new enumerator object used to enumerate this bidi-map.</summary>
    ///  <remarks>This method is usually called by compiler-generated code. Its purpose is to create an enumerator
    ///  object that is used to actually traverse the bidi-map.</remarks>
    ///  <returns>An enumerator object.</returns>
    function GetEnumerator(): IEnumerator<TPair<TKey, TValue>>; override;

    ///  <summary>Copies the values stored in the bidi-map to a given array.</summary>
    ///  <param name="AArray">An array where to copy the contents of the bidi-map.</param>
    ///  <param name="AStartIndex">The index into the array at which the copying begins.</param>
    ///  <remarks>This method assumes that <paramref name="AArray"/> has enough space to hold the contents of the bidi-map.</remarks>
    ///  <exception cref="SysUtils|EArgumentOutOfRangeException"><paramref name="AStartIndex"/> is out of bounds.</exception>
    ///  <exception cref="Collections.Base|EArgumentOutOfSpaceException">The array is not long enough.</exception>
    procedure CopyTo(var AArray: array of TPair<TKey,TValue>; const AStartIndex: NativeInt); overload; override;

    ///  <summary>Returns the value associated with the given key.</summary>
    ///  <param name="AKey">The key for which to return the associated value.</param>
    ///  <returns>The value associated with the given key.</returns>
    ///  <exception cref="Collections.Base|EKeyNotFoundException">No such key in the bidi-map.</exception>
    function ValueForKey(const AKey: TKey): TValue; override;

    ///  <summary>Checks whether the bidi-map contains a given key-value pair.</summary>
    ///  <param name="AKey">The key part of the pair.</param>
    ///  <param name="AValue">The value part of the pair.</param>
    ///  <returns><c>True</c> if the given key-value pair exists; <c>False</c> otherwise.</returns>
    function KeyHasValue(const AKey: TKey; const AValue: TValue): Boolean; override;

    ///  <summary>Returns an Enex collection that contains only the keys.</summary>
    ///  <returns>An Enex collection that contains all the keys stored in the bidi-map.</returns>
    function SelectKeys(): IEnexCollection<TKey>; override;

    ///  <summary>Returns an Enex collection that contains only the values.</summary>
    ///  <returns>An Enex collection that contains all the values stored in the bidi-map.</returns>
    function SelectValues(): IEnexCollection<TValue>; override;
  end;

type
  ///  <summary>The generic <c>bidirectional map</c> collection.</summary>
  ///  <remarks>This type uses <c>distinct multimaps</c> to store its keys and values.</remarks>
  TBidiMap<TKey, TValue> = class(TAbstractBidiMap<TKey, TValue>)
  private
    FInitialCapacity: NativeInt;

  protected
    ///  <summary>Called when the map needs to initialize the key multimap.</summary>
    ///  <param name="AKeyRules">The rule set describing the keys.</param>
    ///  <param name="AValueRules">The rule set describing the values.</param>
    ///  <remarks>This method creates a distinct multimap used as the underlying back-end for the map.</remarks>
    function CreateKeyMap(const AKeyRules: TRules<TKey>;
      const AValueRules: TRules<TValue>): IDistinctMultiMap<TKey, TValue>; override;

    ///  <summary>Called when the map needs to initialize the value multimap.</summary>
    ///  <param name="AKeyRules">The rule set describing the keys.</param>
    ///  <param name="AValueRules">The rule set describing the values.</param>
    ///  <remarks>This method creates a distinct multimap used as the underlying back-end for the map.</remarks>
    function CreateValueMap(const AValueRules: TRules<TValue>;
      const AKeyRules: TRules<TKey>): IDistinctMultiMap<TValue, TKey>; override;

  public
    ///  <summary>Creates a new instance of this class.</summary>
    ///  <param name="AInitialCapacity">The map's initial capacity.</param>
    ///  <remarks>The default rule set is requested.</remarks>
    constructor Create(const AInitialCapacity: NativeInt); overload;

    ///  <summary>Creates a new instance of this class.</summary>
    ///  <param name="AKeyRules">The rule set describing the keys.</param>
    ///  <param name="AValueRules">The rule set describing the values.</param>
    ///  <param name="AInitialCapacity">The map's initial capacity.</param>
    constructor Create(const AKeyRules: TRules<TKey>; const AValueRules: TRules<TValue>; const AInitialCapacity: NativeInt); overload;
  end;

  ///  <summary>The generic <c>bidirectional map</c> collection designed to store objects.</summary>
  ///  <remarks>This type uses <c>distinct multimaps</c> to store its keys and values.</remarks>
  TObjectBidiMap<TKey, TValue> = class(TBidiMap<TKey, TValue>)
  private
    FOwnsKeys, FOwnsValues: Boolean;

  protected
    ///  <summary>Frees the key (object) that was removed from the collection.</summary>
    ///  <param name="AKey">The key that was removed from the collection.</param>
    procedure HandleKeyRemoved(const AKey: TKey); override;

    ///  <summary>Frees the value (object) that was removed from the collection.</summary>
    ///  <param name="AKey">The value that was removed from the collection.</param>
    procedure HandleValueRemoved(const AValue: TValue); override;
  public
    ///  <summary>Specifies whether this map owns the keys.</summary>
    ///  <returns><c>True</c> if the map owns the keys; <c>False</c> otherwise.</returns>
    ///  <remarks>This property specififies the way the map controls the life-time of the stored keys. The value of this property has effect only
    ///  if the keys are objects, otherwise it is ignored.</remarks>
    property OwnsKeys: Boolean read FOwnsKeys write FOwnsKeys;

    ///  <summary>Specifies whether this map owns the values.</summary>
    ///  <returns><c>True</c> if the map owns the values; <c>False</c> otherwise.</returns>
    ///  <remarks>This property specififes the way the map controls the life-time of the stored values. The value of this property has effect only
    ///  if the values are objects, otherwise it is ignored.</remarks>
    property OwnsValues: Boolean read FOwnsValues write FOwnsValues;
  end;

type
  ///  <summary>The generic <c>bidirectional map</c> collection.</summary>
  ///  <remarks>This type uses <c>sorted distinct multimaps</c> to store its keys and values.</remarks>
  TSortedBidiMap<TKey, TValue> = class(TAbstractBidiMap<TKey, TValue>)
  private
    FAscSort: Boolean;

  protected
    ///  <summary>Called when the map needs to initialize the key multimap.</summary>
    ///  <param name="AKeyRules">The rule set describing the keys.</param>
    ///  <param name="AValueRules">The rule set describing the values.</param>
    ///  <remarks>This method creates a sorted distinct multimap used as the underlying back-end for the map.</remarks>
    function CreateKeyMap(const AKeyRules: TRules<TKey>;
      const AValueRules: TRules<TValue>): IDistinctMultiMap<TKey, TValue>; override;

    ///  <summary>Called when the map needs to initialize the value multimap.</summary>
    ///  <param name="AKeyRules">The rule set describing the keys.</param>
    ///  <param name="AValueRules">The rule set describing the values.</param>
    ///  <remarks>This method creates a sorted distinct multimap used as the underlying back-end for the map.</remarks>
    function CreateValueMap(const AValueRules: TRules<TValue>;
      const AKeyRules: TRules<TKey>): IDistinctMultiMap<TValue, TKey>; override;
  public
    ///  <summary>Creates a new instance of this class.</summary>
    ///  <param name="AAscending">A value specifying whether the keys are sorted in ascending order. The default is <c>True</c>.</param>
    ///  <remarks>The default rule set is requested.</remarks>
    constructor Create(const AAscending: Boolean = true); overload;

    ///  <summary>Creates a new instance of this class.</summary>
    ///  <param name="ACollection">A collection to copy the key-value pairs from.</param>
    ///  <param name="AAscending">A value specifying whether the keys are sorted in ascending order. The default is <c>True</c>.</param>
    ///  <remarks>The default rule set is requested.</remarks>
    ///  <exception cref="SysUtils|EArgumentNilException"><paramref name="ACollection"/> is <c>nil</c>.</exception>
    constructor Create(const ACollection: IEnumerable<TPair<TKey,TValue>>; const AAscending: Boolean = true); overload;

    ///  <summary>Creates a new instance of this class.</summary>
    ///  <param name="AArray">An array to copy the key-value pairs from.</param>
    ///  <param name="AAscending">A value specifying whether the keys are sorted in ascending order. The default is <c>True</c>.</param>
    ///  <remarks>The default rule set is requested.</remarks>
    constructor Create(const AArray: array of TPair<TKey,TValue>; const AAscending: Boolean = true); overload;

    ///  <summary>Creates a new instance of this class.</summary>
    ///  <param name="AKeyRules">The rule set describing the keys.</param>
    ///  <param name="AValueRules">The rule set describing the values.</param>
    ///  <param name="AAscending">A value specifying whether the keys are sorted in ascending order. The default is <c>True</c>.</param>
    constructor Create(const AKeyRules: TRules<TKey>; const AValueRules: TRules<TValue>;
      const AAscending: Boolean = true); overload;

    ///  <summary>Creates a new instance of this class.</summary>
    ///  <param name="AKeyRules">The rule set describing the keys.</param>
    ///  <param name="AValueRules">The rule set describing the values.</param>
    ///  <param name="ACollection">A collection to copy the key-value pairs from.</param>
    ///  <param name="AAscending">A value specifying whether the keys are sorted in ascending order. The default is <c>True</c>.</param>
    ///  <exception cref="SysUtils|EArgumentNilException"><paramref name="ACollection"/> is <c>nil</c>.</exception>
    constructor Create(const AKeyRules: TRules<TKey>; const AValueRules: TRules<TValue>;
      const ACollection: IEnumerable<TPair<TKey,TValue>>; const AAscending: Boolean = true); overload;

    ///  <summary>Creates a new instance of this class.</summary>
    ///  <param name="AKeyRules">The rule set describing the keys.</param>
    ///  <param name="AValueRules">The rule set describing the values.</param>
    ///  <param name="AArray">An array to copy the key-value pairs from.</param>
    ///  <param name="AAscending">A value specifying whether the keys are sorted in ascending order. The default is <c>True</c>.</param>
    constructor Create(const AKeyRules: TRules<TKey>; const AValueRules: TRules<TValue>;
      const AArray: array of TPair<TKey,TValue>; const AAscending: Boolean = true); overload;

    ///  <summary>Returns the biggest key.</summary>
    ///  <returns>The biggest key stored in the map.</returns>
    ///  <exception cref="Collections.Base|ECollectionEmptyException">The map is empty.</exception>
    function MaxKey(): TKey; override;

    ///  <summary>Returns the smallest key.</summary>
    ///  <returns>The smallest key stored in the map.</returns>
    ///  <exception cref="Collections.Base|ECollectionEmptyException">The map is empty.</exception>
    function MinKey(): TKey; override;
  end;

  ///  <summary>The generic <c>bidirectional map</c> collection designed to store objects.</summary>
  ///  <remarks>This type uses <c>sorted distinct multimaps</c> to store its keys and values.</remarks>
  TObjectSortedBidiMap<TKey, TValue> = class(TSortedBidiMap<TKey, TValue>)
  private
    FOwnsKeys, FOwnsValues: Boolean;

  protected
    ///  <summary>Frees the key (object) that was removed from the collection.</summary>
    ///  <param name="AKey">The key that was removed from the collection.</param>
    procedure HandleKeyRemoved(const AKey: TKey); override;

    ///  <summary>Frees the value (object) that was removed from the collection.</summary>
    ///  <param name="AKey">The value that was removed from the collection.</param>
    procedure HandleValueRemoved(const AValue: TValue); override;
  public
    ///  <summary>Specifies whether this map owns the keys.</summary>
    ///  <returns><c>True</c> if the map owns the keys; <c>False</c> otherwise.</returns>
    ///  <remarks>This propertyspecififes the way the map controls the life-time of the stored keys. The value of this property has effect only
    ///  if the keys are objects, otherwise it is ignored.</remarks>
    property OwnsKeys: Boolean read FOwnsKeys write FOwnsKeys;

    ///  <summary>Specifies whether this map owns the values.</summary>
    ///  <returns><c>True</c> if the map owns the values; <c>False</c> otherwise.</returns>
    ///  <remarks>This property specifies the way the map controls the life-time of the stored values. The value of this property has effect only
    ///  if the values are objects, otherwise it is ignored.</remarks>
    property OwnsValues: Boolean read FOwnsValues write FOwnsValues;
  end;

type
  ///  <summary>The generic <c>bidirectional map</c> collection.</summary>
  ///  <remarks>This type uses <c>double sorted distinct multimaps</c> to store its keys and values.</remarks>
  TDoubleSortedBidiMap<TKey, TValue> = class(TAbstractBidiMap<TKey, TValue>)
  private
    FAscKeys, FAscValues: Boolean;

  protected
    ///  <summary>Called when the map needs to initialize the key multimap.</summary>
    ///  <param name="AKeyRules">The rule set describing the keys.</param>
    ///  <param name="AValueRules">The rule set describing the values.</param>
    ///  <remarks>This method creates a double sorted distinct multimap used as the underlying back-end for the map.</remarks>
    function CreateKeyMap(const AKeyRules: TRules<TKey>;
      const AValueRules: TRules<TValue>): IDistinctMultiMap<TKey, TValue>; override;

    ///  <summary>Called when the map needs to initialize the value multimap.</summary>
    ///  <param name="AKeyRules">The rule set describing the keys.</param>
    ///  <param name="AValueRules">The rule set describing the values.</param>
    ///  <remarks>This method creates a double sorted distinct multimap used as the underlying back-end for the map.</remarks>
    function CreateValueMap(const AValueRules: TRules<TValue>;
      const AKeyRules: TRules<TKey>): IDistinctMultiMap<TValue, TKey>; override;

  public
    ///  <summary>Creates a new instance of this class.</summary>
    ///  <param name="AAscendingKeys">A value specifying whether the keys are sorted in ascending order. The default is <c>True</c>.</param>
    ///  <param name="AAscendingValues">A value specifying whether the values are sorted in ascending order. The default is <c>True</c>.</param>
    ///  <remarks>The default rule set is requested.</remarks>
    constructor Create(const AAscendingKeys: Boolean = true; const AAscendingValues: Boolean = true); overload;

    ///  <summary>Creates a new instance of this class.</summary>
    ///  <param name="ACollection">A collection to copy the key-value pairs from.</param>
    ///  <param name="AAscendingKeys">A value specifying whether the keys are sorted in ascending order. The default is <c>True</c>.</param>
    ///  <param name="AAscendingValues">A value specifying whether the values are sorted in ascending order. The default is <c>True</c>.</param>
    ///  <remarks>The default rule set is requested.</remarks>
    ///  <exception cref="SysUtils|EArgumentNilException"><paramref name="ACollection"/> is <c>nil</c>.</exception>
    constructor Create(const ACollection: IEnumerable<TPair<TKey,TValue>>;
      const AAscendingKeys: Boolean = true; const AAscendingValues: Boolean = true); overload;

    ///  <summary>Creates a new instance of this class.</summary>
    ///  <param name="AArray">An array to copy the key-value pairs from.</param>
    ///  <param name="AAscendingKeys">A value specifying whether the keys are sorted in ascending order. The default is <c>True</c>.</param>
    ///  <param name="AAscendingValues">A value specifying whether the values are sorted in ascending order. The default is <c>True</c>.</param>
    ///  <remarks>The default rule set is requested.</remarks>
    constructor Create(const AArray: array of TPair<TKey,TValue>;
      const AAscendingKeys: Boolean = true; const AAscendingValues: Boolean = true); overload;

    ///  <summary>Creates a new instance of this class.</summary>
    ///  <param name="AKeyRules">The rule set describing the keys.</param>
    ///  <param name="AValueRules">The rule set describing the values.</param>
    ///  <param name="AAscendingKeys">A value specifying whether the keys are sorted in ascending order. The default is <c>True</c>.</param>
    ///  <param name="AAscendingValues">A value specifying whether the values are sorted in ascending order. The default is <c>True</c>.</param>
    constructor Create(const AKeyRules: TRules<TKey>; const AValueRules: TRules<TValue>;
      const AAscendingKeys: Boolean = true; const AAscendingValues: Boolean = true); overload;

    ///  <summary>Creates a new instance of this class.</summary>
    ///  <param name="AKeyRules">The rule set describing the keys.</param>
    ///  <param name="AValueRules">The rule set describing the values.</param>
    ///  <param name="ACollection">A collection to copy the key-value pairs from.</param>
    ///  <param name="AAscendingKeys">A value specifying whether the keys are sorted in ascending order. The default is <c>True</c>.</param>
    ///  <param name="AAscendingValues">A value specifying whether the values are sorted in ascending order. The default is <c>True</c>.</param>
    ///  <exception cref="SysUtils|EArgumentNilException"><paramref name="ACollection"/> is <c>nil</c>.</exception>
    constructor Create(const AKeyRules: TRules<TKey>; const AValueRules: TRules<TValue>;
      const ACollection: IEnumerable<TPair<TKey,TValue>>; const AAscendingKeys: Boolean = true;
      const AAscendingValues: Boolean = true); overload;

    ///  <summary>Creates a new instance of this class.</summary>
    ///  <param name="AKeyRules">The rule set describing the keys.</param>
    ///  <param name="AValueRules">The rule set describing the values.</param>
    ///  <param name="AArray">An array to copy the key-value pairs from.</param>
    ///  <param name="AAscendingKeys">A value specifying whether the keys are sorted in ascending order. The default is <c>True</c>.</param>
    ///  <param name="AAscendingValues">A value specifying whether the values are sorted in ascending order. The default is <c>True</c>.</param>
    constructor Create(const AKeyRules: TRules<TKey>; const AValueRules: TRules<TValue>;
      const AArray: array of TPair<TKey,TValue>; const AAscendingKeys: Boolean = true;
      const AAscendingValues: Boolean = true); overload;

    ///  <summary>Returns the biggest key.</summary>
    ///  <returns>The biggest key stored in the map.</returns>
    ///  <exception cref="Collections.Base|ECollectionEmptyException">The map is empty.</exception>
    function MaxKey(): TKey; override;

    ///  <summary>Returns the smallest key.</summary>
    ///  <returns>The smallest key stored in the map.</returns>
    ///  <exception cref="Collections.Base|ECollectionEmptyException">The map is empty.</exception>
    function MinKey(): TKey; override;
  end;

  ///  <summary>The generic <c>bidirectional map</c> collection designed to store objects.</summary>
  ///  <remarks>This type uses <c>double sorted distinct multimaps</c> to store its keys and values.</remarks>
  TObjectDoubleSortedBidiMap<TKey, TValue> = class(TDoubleSortedBidiMap<TKey, TValue>)
  private
    FOwnsKeys, FOwnsValues: Boolean;

  protected
    ///  <summary>Frees the key (object) that was removed from the collection.</summary>
    ///  <param name="AKey">The key that was removed from the collection.</param>
    procedure HandleKeyRemoved(const AKey: TKey); override;

    ///  <summary>Frees the value (object) that was removed from the collection.</summary>
    ///  <param name="AKey">The value that was removed from the collection.</param>
    procedure HandleValueRemoved(const AValue: TValue); override;
  public
    ///  <summary>Specifies whether this map owns the keys.</summary>
    ///  <returns><c>True</c> if the map owns the keys; <c>False</c> otherwise.</returns>
    ///  <remarks>This property specififes the way the map controls the life-time of the stored keys. The value of this property has effect only
    ///  if the keys are objects, otherwise it is ignored.</remarks>
    property OwnsKeys: Boolean read FOwnsKeys write FOwnsKeys;

    ///  <summary>Specifies whether this map owns the values.</summary>
    ///  <returns><c>True</c> if the map owns the values; <c>False</c> otherwise.</returns>
    ///  <remarks>This property specifies the way the map controls the life-time of the stored values. The value of this property has effect only
    ///  if the values are objects, otherwise it is ignored.</remarks>
    property OwnsValues: Boolean read FOwnsValues write FOwnsValues;
  end;

implementation


{ TAbstractBidiMap<TKey, TValue> }

constructor TAbstractBidiMap<TKey, TValue>.Create(const AArray: array of TPair<TKey, TValue>);
begin
  Create(TRules<TKey>.Default, TRules<TValue>.Default, AArray);
end;

constructor TAbstractBidiMap<TKey, TValue>.Create;
begin
  Create(TRules<TKey>.Default, TRules<TValue>.Default);
end;

constructor TAbstractBidiMap<TKey, TValue>.Create(const ACollection: IEnumerable<TPair<TKey, TValue>>);
begin
  Create(TRules<TKey>.Default, TRules<TValue>.Default, ACollection);
end;

procedure TAbstractBidiMap<TKey, TValue>.Add(const AKey: TKey; const AValue: TValue);
begin
  { Add the K/V pair to the maps }
  FByKeyMap.Add(AKey, AValue);
  FByValueMap.Add(AValue, AKey);
end;

procedure TAbstractBidiMap<TKey, TValue>.Add(const APair: TPair<TKey, TValue>);
begin
  Add(APair.Key, APair.Value);
end;

procedure TAbstractBidiMap<TKey, TValue>.Clear;
begin
  if Assigned(FByKeyMap) then
    FByKeyMap.Clear;

  if Assigned(FByValueMap) then
    FByValueMap.Clear;
end;

function TAbstractBidiMap<TKey, TValue>.ContainsKey(const AKey: TKey): Boolean;
begin
  Result := FByKeyMap.ContainsKey(AKey);
end;

function TAbstractBidiMap<TKey, TValue>.ContainsPair(const APair: TPair<TKey, TValue>): Boolean;
begin
  { The by-key relation since it is always correct }
  Result := FByKeyMap.ContainsValue(APair.Key, APair.Value);
end;

function TAbstractBidiMap<TKey, TValue>.ContainsPair(const AKey: TKey; const AValue: TValue): Boolean;
begin
  { The by-key relation since it is always correct }
  Result := FByKeyMap.ContainsValue(AKey, AValue);
end;

function TAbstractBidiMap<TKey, TValue>.ContainsValue(const AValue: TValue): Boolean;
begin
  Result := FByValueMap.ContainsKey(AValue);
end;

procedure TAbstractBidiMap<TKey, TValue>.CopyTo(var AArray: array of TPair<TKey, TValue>; const AStartIndex: NativeInt);
begin
  { Check for indexes }
  if (AStartIndex >= Length(AArray)) or (AStartIndex < 0) then
    ExceptionHelper.Throw_ArgumentOutOfRangeError('AStartIndex');

  if (Length(AArray) - AStartIndex) < Count then
     ExceptionHelper.Throw_ArgumentOutOfSpaceError('AArray');

  { Call the underlying collection }
  FByKeyMap.CopyTo(AArray, AStartIndex);
end;

destructor TAbstractBidiMap<TKey, TValue>.Destroy;
begin
  { Clear out the instance }
  Clear();

  inherited;
end;

function TAbstractBidiMap<TKey, TValue>.GetCount: NativeInt;
begin
  { The count follows the map properties }
  Result := FByKeyMap.Count;
end;

function TAbstractBidiMap<TKey, TValue>.GetEnumerator: IEnumerator<TPair<TKey, TValue>>;
begin
  { Pass the enumerator from the key map }
  Result := FByKeyMap.GetEnumerator();
end;

function TAbstractBidiMap<TKey, TValue>.GetKeyList(const AValue: TValue): IEnexCollection<TKey>;
begin
  Result := FByValueMap[AValue];
end;

function TAbstractBidiMap<TKey, TValue>.GetValueList(const AKey: TKey): IEnexCollection<TValue>;
begin
  Result := FByKeyMap[AKey];
end;

function TAbstractBidiMap<TKey, TValue>.KeyHasValue(const AKey: TKey; const AValue: TValue): Boolean;
begin
  Result := ContainsPair(AKey, AValue);
end;

procedure TAbstractBidiMap<TKey, TValue>.Remove(const AKey: TKey; const AValue: TValue);
var
  LValues: IEnexCollection<TValue>;
  LValue: TValue;
begin
  { Check whether there is such a key }
  if not FByKeyMap.ContainsValue(AKey, AValue) then
    Exit;

  { Remove the stuff }
  FByKeyMap.Remove(AKey, AValue);
  FByValueMap.Remove(AValue, AKey);
end;

procedure TAbstractBidiMap<TKey, TValue>.Remove(const APair: TPair<TKey, TValue>);
begin
  Remove(APair.Key, APair.Value);
end;

procedure TAbstractBidiMap<TKey, TValue>.Remove(const AKey: TKey);
begin
  RemoveKey(AKey);
end;

procedure TAbstractBidiMap<TKey, TValue>.RemoveKey(const AKey: TKey);
var
  LValues: IEnexCollection<TValue>;
  LValue: TValue;
begin
  { Check whether there is such a key }
  if not FByKeyMap.TryGetValues(AKey, LValues) then
    Exit;

  { Exclude the key for all values too }
  for LValue in LValues do
    FByValueMap.Remove(LValue, AKey);

  { And finally remove the key }
  FByKeyMap.Remove(AKey);
end;

procedure TAbstractBidiMap<TKey, TValue>.RemoveValue(const AValue: TValue);
var
  LKeys: IEnexCollection<TKey>;
  LValue: TKey;
begin
  { Check whether there is such a key }
  if not FByValueMap.TryGetValues(AValue, LKeys) then
    Exit;

  { Exclude the key for all values too}
  for LValue in LKeys do
    FByKeyMap.Remove(LValue, AValue);

  { And finally remove the key }
  FByValueMap.Remove(AValue);

//  { Clean up the value if necessary }
//  if ValueRules.Management = tmManual then
//    ValueRules.Cleanup(LValue);
end;

function TAbstractBidiMap<TKey, TValue>.SelectKeys: IEnexCollection<TKey>;
begin
  { Pass the values on }
  Result := Keys;
end;

function TAbstractBidiMap<TKey, TValue>.SelectValues: IEnexCollection<TValue>;
begin
  { Pass the value on }
  Result := Values;
end;

function TAbstractBidiMap<TKey, TValue>.ValueForKey(const AKey: TKey): TValue;
begin
  Result := FByKeyMap[AKey].First;
end;

constructor TAbstractBidiMap<TKey, TValue>.Create(const AKeyRules: TRules<TKey>;
  const AValueRules: TRules<TValue>;
  const AArray: array of TPair<TKey, TValue>);
var
  I: NativeInt;
begin
  { Call upper constructor }
  Create(AKeyRules, AValueRules);

  { Copy all items in }
  for I := 0 to Length(AArray) - 1 do
  begin
    Add(AArray[I]);
  end;
end;

constructor TAbstractBidiMap<TKey, TValue>.Create(const AKeyRules: TRules<TKey>; const AValueRules: TRules<TValue>);
begin
  { Install the types }
  inherited Create(AKeyRules, AValueRules);

  { Create the maps }
  FByKeyMap := CreateKeyMap(AKeyRules, ValueRules);
  FByValueMap := CreateValueMap(AValueRules, KeyRules);

  { The collections }
  FValueCollection := FByValueMap.Keys;
  FKeyCollection := FByKeyMap.Keys;
end;

constructor TAbstractBidiMap<TKey, TValue>.Create(const AKeyRules: TRules<TKey>;
  const AValueRules: TRules<TValue>;
  const ACollection: IEnumerable<TPair<TKey, TValue>>);
var
  LValue: TPair<TKey, TValue>;
begin
  { Call upper constructor }
  Create(AKeyRules, AValueRules);

  if not Assigned(ACollection) then
     ExceptionHelper.Throw_ArgumentNilError('ACollection');

  { Pump in all items }
  for LValue in ACollection do
  begin
{$IF CompilerVersion < 22}
    Add(LValue);
{$ELSE}
    Add(LValue.Key, LValue.Value);
{$IFEND}
  end;
end;

{ TBidiMap<TKey, TValue> }

constructor TBidiMap<TKey, TValue>.Create(const AInitialCapacity: NativeInt);
begin
  FInitialCapacity := AInitialCapacity;
  inherited Create();
end;

constructor TBidiMap<TKey, TValue>.Create(const AKeyRules: TRules<TKey>; const AValueRules: TRules<TValue>;
  const AInitialCapacity: NativeInt);
begin
  FInitialCapacity := AInitialCapacity;
  inherited Create(AKeyRules, AValueRules);
end;

function TBidiMap<TKey, TValue>.CreateKeyMap(const AKeyRules: TRules<TKey>;
  const AValueRules: TRules<TValue>): IDistinctMultiMap<TKey, TValue>;
var
  LNewCapacity: NativeInt;
  LMap: TDistinctMultiMap<TKey, TValue>;
begin
  { Create a simple dictionary }
  if FInitialCapacity <= 0 then
    LNewCapacity := CDefaultSize
  else
    LNewCapacity := FInitialCapacity;

  { Use a simple non-sorted map }
  LMap := TDistinctMultiMap<TKey, TValue>.Create(AKeyRules, AValueRules, LNewCapacity);
  LMap.KeyRemoveNotification := NotifyKeyRemoved;

  Result := LMap;
end;

function TBidiMap<TKey, TValue>.CreateValueMap(const AValueRules: TRules<TValue>;
  const AKeyRules: TRules<TKey>): IDistinctMultiMap<TValue, TKey>;
var
  LNewCapacity: NativeInt;
  LMap: TDistinctMultiMap<TValue, TKey>;
begin
  { Create a simple dictionary }
  if FInitialCapacity <= 0 then
    LNewCapacity := CDefaultSize
  else
    LNewCapacity := FInitialCapacity;

  { Use a simple non-sorted map }
  LMap := TDistinctMultiMap<TValue, TKey>.Create(AValueRules, AKeyRules, LNewCapacity);
  LMap.KeyRemoveNotification := NotifyValueRemoved;

  Result := LMap;
end;

{ TObjectBidiMap<TKey, TValue> }

procedure TObjectBidiMap<TKey, TValue>.HandleKeyRemoved(const AKey: TKey);
begin
  if FOwnsKeys then
    TObject(AKey).Free;
end;

procedure TObjectBidiMap<TKey, TValue>.HandleValueRemoved(const AValue: TValue);
begin
  if FOwnsValues then
    TObject(AValue).Free;
end;

{ TSortedBidiMap<TKey, TValue> }

constructor TSortedBidiMap<TKey, TValue>.Create(
  const AArray: array of TPair<TKey, TValue>; const AAscending: Boolean);
begin
  { Do the dew and continue }
  FAscSort := AAscending;
  inherited Create(AArray);
end;

constructor TSortedBidiMap<TKey, TValue>.Create(const AAscending: Boolean);
begin
  { Do the dew and continue }
  FAscSort := AAscending;
  inherited Create();
end;

constructor TSortedBidiMap<TKey, TValue>.Create(
  const ACollection: IEnumerable<TPair<TKey, TValue>>;
  const AAscending: Boolean);
begin
  { Do the dew and continue }
  FAscSort := AAscending;
  inherited Create(ACollection);
end;

constructor TSortedBidiMap<TKey, TValue>.Create(const AKeyRules: TRules<TKey>;
  const AValueRules: TRules<TValue>;
  const AArray: array of TPair<TKey, TValue>; const AAscending: Boolean);
begin
  { Do the dew and continue }
  FAscSort := AAscending;
  inherited Create(AKeyRules, AValueRules, AArray);
end;

constructor TSortedBidiMap<TKey, TValue>.Create(const AKeyRules: TRules<TKey>;
  const AValueRules: TRules<TValue>; const AAscending: Boolean);
begin
  { Do the dew and continue }
  FAscSort := AAscending;
  inherited Create(AKeyRules, AValueRules);
end;

constructor TSortedBidiMap<TKey, TValue>.Create(const AKeyRules: TRules<TKey>;
  const AValueRules: TRules<TValue>;
  const ACollection: IEnumerable<TPair<TKey, TValue>>;
  const AAscending: Boolean);
begin
  { Do the dew and continue }
  FAscSort := AAscending;
  inherited Create(AKeyRules, AValueRules, ACollection);
end;

function TSortedBidiMap<TKey, TValue>.CreateKeyMap(const AKeyRules: TRules<TKey>;
  const AValueRules: TRules<TValue>): IDistinctMultiMap<TKey, TValue>;
var
  LMap: TSortedDistinctMultiMap<TKey, TValue>;
begin
  { Use a simple sorted map }
  LMap := TSortedDistinctMultiMap<TKey, TValue>.Create(AKeyRules, AValueRules, FAscSort);
  LMap.KeyRemoveNotification := NotifyKeyRemoved;

  Result := LMap;
end;

function TSortedBidiMap<TKey, TValue>.CreateValueMap(const AValueRules: TRules<TValue>;
  const AKeyRules: TRules<TKey>): IDistinctMultiMap<TValue, TKey>;
var
  LMap: TSortedDistinctMultiMap<TValue, TKey>;
begin
  { Use a simple sorted map }
  LMap := TSortedDistinctMultiMap<TValue, TKey>.Create(AValueRules, AKeyRules, FAscSort);
  LMap.KeyRemoveNotification := NotifyValueRemoved;
  Result := LMap;
end;

function TSortedBidiMap<TKey, TValue>.MaxKey: TKey;
begin
  Result := ByKeyMap.MaxKey;
end;

function TSortedBidiMap<TKey, TValue>.MinKey: TKey;
begin
  Result := ByKeyMap.MinKey;
end;

{ TObjectSortedBidiMap<TKey, TValue> }

procedure TObjectSortedBidiMap<TKey, TValue>.HandleKeyRemoved(const AKey: TKey);
begin
  if FOwnsKeys then
    TObject(AKey).Free;
end;

procedure TObjectSortedBidiMap<TKey, TValue>.HandleValueRemoved(const AValue: TValue);
begin
  if FOwnsValues then
    TObject(AValue).Free;
end;

{ TDoubleSortedBidiMap<TKey, TValue> }

constructor TDoubleSortedBidiMap<TKey, TValue>.Create(
  const AArray: array of TPair<TKey, TValue>; const AAscendingKeys,
  AAscendingValues: Boolean);
begin
  { Do the dew and continue! }
  FAscKeys := AAscendingKeys;
  FAscValues := AAscendingValues;

  inherited Create(AArray);
end;

constructor TDoubleSortedBidiMap<TKey, TValue>.Create(const AAscendingKeys, AAscendingValues: Boolean);
begin
  { Do the dew and continue! }
  FAscKeys := AAscendingKeys;
  FAscValues := AAscendingValues;

  inherited Create();
end;

constructor TDoubleSortedBidiMap<TKey, TValue>.Create(
  const ACollection: IEnumerable<TPair<TKey, TValue>>;
  const AAscendingKeys, AAscendingValues: Boolean);
begin
  { Do the dew and continue! }
  FAscKeys := AAscendingKeys;
  FAscValues := AAscendingValues;

  inherited Create(ACollection);
end;

constructor TDoubleSortedBidiMap<TKey, TValue>.Create(
  const AKeyRules: TRules<TKey>; const AValueRules: TRules<TValue>;
  const AArray: array of TPair<TKey, TValue>; const AAscendingKeys,
  AAscendingValues: Boolean);
begin
  { Do the dew and continue! }
  FAscKeys := AAscendingKeys;
  FAscValues := AAscendingValues;

  inherited Create(AKeyRules, AValueRules, AArray);
end;

constructor TDoubleSortedBidiMap<TKey, TValue>.Create(
  const AKeyRules: TRules<TKey>; const AValueRules: TRules<TValue>;
  const AAscendingKeys, AAscendingValues: Boolean);
begin
  { Do the dew and continue! }
  FAscKeys := AAscendingKeys;
  FAscValues := AAscendingValues;

  inherited Create(AKeyRules, AValueRules);
end;

constructor TDoubleSortedBidiMap<TKey, TValue>.Create(
  const AKeyRules: TRules<TKey>; const AValueRules: TRules<TValue>;
  const ACollection: IEnumerable<TPair<TKey, TValue>>;
  const AAscendingKeys, AAscendingValues: Boolean);
begin
  { Do the dew and continue! }
  FAscKeys := AAscendingKeys;
  FAscValues := AAscendingValues;

  inherited Create(AKeyRules, AValueRules, ACollection);
end;

function TDoubleSortedBidiMap<TKey, TValue>.CreateKeyMap(const AKeyRules: TRules<TKey>;
  const AValueRules: TRules<TValue>): IDistinctMultiMap<TKey, TValue>;
var
  LMap: TDoubleSortedDistinctMultiMap<TKey, TValue>;
begin
  { Use a double sorted map }
  LMap := TDoubleSortedDistinctMultiMap<TKey, TValue>.Create(AKeyRules, AValueRules, FAscKeys, FAscValues);
  LMap.KeyRemoveNotification := NotifyKeyRemoved;

  Result := LMap;
end;

function TDoubleSortedBidiMap<TKey, TValue>.CreateValueMap(const AValueRules: TRules<TValue>;
  const AKeyRules: TRules<TKey>): IDistinctMultiMap<TValue, TKey>;
var
  LMap: TDoubleSortedDistinctMultiMap<TValue, TKey>;
begin
  { Use a double sorted map }
  LMap := TDoubleSortedDistinctMultiMap<TValue, TKey>.Create(AValueRules, AKeyRules, FAscKeys, FAscValues);
  LMap.KeyRemoveNotification := NotifyValueRemoved;
  Result := LMap;
end;

function TDoubleSortedBidiMap<TKey, TValue>.MaxKey: TKey;
begin
  Result := ByKeyMap.MaxKey;
end;

function TDoubleSortedBidiMap<TKey, TValue>.MinKey: TKey;
begin
  Result := ByKeyMap.MinKey;
end;

{ TObjectDoubleSortedBidiMap<TKey, TValue> }

procedure TObjectDoubleSortedBidiMap<TKey, TValue>.HandleKeyRemoved(const AKey: TKey);
begin
  if FOwnsKeys then
    TObject(AKey).Free;
end;

procedure TObjectDoubleSortedBidiMap<TKey, TValue>.HandleValueRemoved(const AValue: TValue);
begin
  if FOwnsValues then
    TObject(AValue).Free;
end;

end.

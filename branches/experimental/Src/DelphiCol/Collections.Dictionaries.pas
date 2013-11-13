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

unit Collections.Dictionaries;
interface
uses SysUtils,
     Generics.Defaults,
     Generics.Collections,
     Collections.Base;

type
  ///  <summary>The generic <c>dictionary</c> collection.</summary>
  ///  <remarks>This type uses hashing mechanisms to store its key-value pairs.</remarks>
  TDictionary<TKey, TValue> = class(TEnexAssociativeCollection<TKey, TValue>, IDictionary<TKey, TValue>)
  private type
    {$REGION 'Internal Types'}
    { Generic Dictionary Pairs Enumerator }
    TPairEnumerator = class(TEnumerator<TPair<TKey,TValue>>)
    private
      FVer: NativeInt;
      FDict: TDictionary<TKey, TValue>;
      FCurrentIndex: NativeInt;
      FValue: TPair<TKey,TValue>;

    public
      { Constructor }
      constructor Create(const ADict: TDictionary<TKey, TValue>);

      { Destructor }
      destructor Destroy(); override;

      function GetCurrent(): TPair<TKey,TValue>; override;
      function MoveNext(): Boolean; override;
    end;

    { Generic Dictionary Keys Enumerator }
    TKeyEnumerator = class(TEnumerator<TKey>)
    private
      FVer: NativeInt;
      FDict: TDictionary<TKey, TValue>;
      FCurrentIndex: NativeInt;
      FValue: TKey;
    public
      { Constructor }
      constructor Create(const ADict: TDictionary<TKey, TValue>);

      { Destructor }
      destructor Destroy(); override;

      function GetCurrent(): TKey; override;
      function MoveNext(): Boolean; override;
    end;

    { Generic Dictionary Values Enumerator }
    TValueEnumerator = class(TEnumerator<TValue>)
    private
      FVer: NativeInt;
      FDict: TDictionary<TKey, TValue>;
      FCurrentIndex: NativeInt;
      FValue: TValue;
    public
      { Constructor }
      constructor Create(const ADict: TDictionary<TKey, TValue>);

      { Destructor }
      destructor Destroy(); override;

      function GetCurrent(): TValue; override;
      function MoveNext(): Boolean; override;
    end;

    TEntry = record
      FHashCode: NativeInt;
      FNext: NativeInt;
      FKey: TKey;
      FValue: TValue;
    end;

    TBucketArray = array of NativeInt;
    TEntryArray = TArray<TEntry>;

    { Generic Dictionary Keys Collection }
    TKeyCollection = class(TEnexCollection<TKey>)
    private
      FDict: TDictionary<TKey, TValue>;

    protected
      { Hidden }
      function GetCount(): NativeInt; override;

    public
      { Constructor }
      constructor Create(const ADict: TDictionary<TKey, TValue>);

      { Property }
      property Count: NativeInt read GetCount;

      { IEnumerable/ ICollection support }
      function GetEnumerator(): IEnumerator<TKey>; override;

      { Copy-To }
      procedure CopyTo(var AArray: array of TKey; const AStartIndex: NativeInt); overload; override;
    end;

    { Generic Dictionary Values Collection }
    TValueCollection = class(TEnexCollection<TValue>)
    private
      FDict: TDictionary<TKey, TValue>;

    protected
      { Hidden }
      function GetCount: NativeInt; override;

    public
      { Constructor }
      constructor Create(const ADict: TDictionary<TKey, TValue>);

      { Property }
      property Count: NativeInt read GetCount;

      { IEnumerable/ ICollection support }
      function GetEnumerator(): IEnumerator<TValue>; override;

      { Copy-To }
      procedure CopyTo(var AArray: array of TValue; const AStartIndex: NativeInt); overload; override;
    end;
    {$ENDREGION}

  private var
    FBucketArray: TBucketArray;
    FEntryArray: TEntryArray;
    FKeyCollection: IEnexCollection<TKey>;
    FValueCollection: IEnexCollection<TValue>;
    FCount: NativeInt;
    FFreeCount: NativeInt;
    FFreeList: NativeInt;
    FVer: NativeInt;

    { Internal }
    procedure InitializeInternals(const ACapacity: NativeInt);
    procedure Insert(const AKey: TKey; const AValue: TValue; const ShouldAdd: Boolean = true);
    function FindEntry(const AKey: TKey): NativeInt;
    procedure Resize();
    function Hash(const AKey: TKey): NativeInt;

  protected
    ///  <summary>Returns the number of key-value pairs in the dictionary.</summary>
    ///  <returns>A positive value specifying the number of pairs in the dictionary.</returns>
    function GetCount(): NativeInt; override;

    ///  <summary>Returns the value associated with the given key.</summary>
    ///  <param name="AKey">The key for which to try to retrieve the value.</param>
    ///  <returns>The value associated with the key.</returns>
    ///  <exception cref="Collections.Base|EKeyNotFoundException">The key is not found in the dictionary.</exception>
    function GetItem(const AKey: TKey): TValue;

    ///  <summary>Sets the value for a given key.</summary>
    ///  <param name="AKey">The key for which to set the value.</param>
    ///  <param name="AValue">The value to set.</param>
    ///  <remarks>If the dictionary does not contain the key, this method acts like <c>Add</c>; otherwise the
    ///  value of the specified key is modified.</remarks>
    procedure SetItem(const AKey: TKey; const Value: TValue);

    ///  <summary>Replaces a given value with a new one.</summary>
    ///  <param name="ACurrent">The value to be replaced.</param>
    ///  <param name="ANew">The value to be replaced with.</param>
    ///  <remarks>This method is called by the dictioanry when a value needs to be replaced with another.
    ///  The default implementation will compare the values, if those are equal nothing is done. Otherwise the old value is
    ///  "disposed of" and the new one is copied over. Descendant classes my want another behaviour.</remarks>
    procedure ReplaceValue(var ACurrent: TValue; const ANew: TValue); virtual;
  public
    ///  <summary>Creates a new instance of this class.</summary>
    ///  <remarks>The default rule set is requested.</remarks>
    constructor Create(); overload;

    ///  <summary>Creates a new instance of this class.</summary>
    ///  <param name="AInitialCapacity">The dictionary's initial capacity.</param>
    ///  <remarks>The default rule set is requested.</remarks>
    constructor Create(const AInitialCapacity: NativeInt); overload;

    ///  <summary>Creates a new instance of this class.</summary>
    ///  <param name="ACollection">A collection to copy pairs from.</param>
    ///  <remarks>The default rule set is requested.</remarks>
    ///  <exception cref="SysUtils|EArgumentNilException"><paramref name="ACollection"/> is <c>nil</c>.</exception>
    ///  <exception cref="Collections.Base|EDuplicateKeyException"><paramref name="ACollection"/> contains pairs with equal keys.</exception>
    constructor Create(const ACollection: IEnumerable<TPair<TKey, TValue>>); overload;

    ///  <summary>Creates a new instance of this class.</summary>
    ///  <param name="AArray">An array to copy pairs from.</param>
    ///  <remarks>The default rule set is requested.</remarks>
    ///  <exception cref="Collections.Base|EDuplicateKeyException"><paramref name="AArray"/> contains pairs with equal keys.</exception>
    constructor Create(const AArray: array of TPair<TKey, TValue>); overload;

    ///  <summary>Creates a new instance of this class.</summary>
    ///  <param name="AKeyRules">A rule set describing the keys in the dictionary.</param>
    ///  <param name="AValueRules">A rule set describing the values in the dictionary.</param>
    constructor Create(const AKeyRules: TRules<TKey>; const AValueRules: TRules<TValue>); overload;

    ///  <summary>Creates a new instance of this class.</summary>
    ///  <param name="AKeyRules">A rule set describing the keys in the dictionary.</param>
    ///  <param name="AValueRules">A rule set describing the values in the dictionary.</param>
    ///  <param name="AInitialCapacity">The dictionary's initial capacity.</param>
    constructor Create(const AKeyRules: TRules<TKey>; const AValueRules: TRules<TValue>;
      const AInitialCapacity: NativeInt); overload;

    ///  <summary>Creates a new instance of this class.</summary>
    ///  <param name="ACollection">A collection to copy pairs from.</param>
    ///  <param name="AKeyRules">A rule set describing the keys in the dictionary.</param>
    ///  <param name="AValueRules">A rule set describing the values in the dictionary.</param>
    ///  <exception cref="SysUtils|EArgumentNilException"><paramref name="ACollection"/> is <c>nil</c>.</exception>
    ///  <exception cref="SysUtils|EArgumentNilException"><paramref name="ARules"/> is <c>nil</c>.</exception>
    ///  <exception cref="Collections.Base|EDuplicateKeyException"><paramref name="ACollection"/> contains pairs with equal keys.</exception>
    constructor Create(const AKeyRules: TRules<TKey>; const AValueRules: TRules<TValue>;
      const ACollection: IEnumerable<TPair<TKey, TValue>>); overload;

    ///  <summary>Creates a new instance of this class.</summary>
    ///  <param name="AArray">An array to copy pairs from.</param>
    ///  <param name="AKeyRules">A rule set describing the keys in the dictionary.</param>
    ///  <param name="AValueRules">A rule set describing the values in the dictionary.</param>
    ///  <exception cref="Collections.Base|EDuplicateKeyException"><paramref name="AArray"/> contains pairs with equal keys.</exception>
    constructor Create(const AKeyRules: TRules<TKey>; const AValueRules: TRules<TValue>;
      const AArray: array of TPair<TKey,TValue>); overload;

    ///  <summary>Destroys this instance.</summary>
    ///  <remarks>Do not call this method directly; call <c>Free</c> instead.</remarks>
    destructor Destroy(); override;

    ///  <summary>Clears the contents of the dictionary.</summary>
    procedure Clear();

    ///  <summary>Adds a key-value pair to the dictionary.</summary>
    ///  <param name="APair">The key-value pair to add.</param>
    ///  <exception cref="Collections.Base|EDuplicateKeyException">The dictionary already contains a pair with the given key.</exception>
    procedure Add(const APair: TPair<TKey,TValue>); overload;

    ///  <summary>Adds a key-value pair to the dictionary.</summary>
    ///  <param name="AKey">The key of pair.</param>
    ///  <param name="AValue">The value associated with the key.</param>
    ///  <exception cref="Collections.Base|EDuplicateKeyException">The dictionary already contains a pair with the given key.</exception>
    procedure Add(const AKey: TKey; const AValue: TValue); overload;

    ///  <summary>Removes a key-value pair using a given key.</summary>
    ///  <param name="AKey">The key of the pair to remove.</param>
    ///  <remarks>If the specified key was not found in the dictionary, nothing happens.</remarks>
    procedure Remove(const AKey: TKey); overload;

    ///  <summary>Checks whether the dictionary contains a key-value pair identified by the given key.</summary>
    ///  <param name="AKey">The key to check for.</param>
    ///  <returns><c>True</c> if the dictionary contains a pair identified by the given key; <c>False</c> otherwise.</returns>
    function ContainsKey(const AKey: TKey): Boolean;

    ///  <summary>Checks whether the dictionary contains a key-value pair that contains a given value.</summary>
    ///  <param name="AValue">The value to check for.</param>
    ///  <returns><c>True</c> if the dictionary contains a pair containing the given value; <c>False</c> otherwise.</returns>
    function ContainsValue(const AValue: TValue): Boolean;

    ///  <summary>Tries to obtain the value associated with a given key.</summary>
    ///  <param name="AKey">The key for which to try to retrieve the value.</param>
    ///  <param name="AFoundValue">The found value (if the result is <c>True</c>).</param>
    ///  <returns><c>True</c> if the dictionary contains a value for the given key; <c>False</c> otherwise.</returns>
    function TryGetValue(const AKey: TKey; out AFoundValue: TValue): Boolean;

    ///  <summary>Gets or sets the value for a given key.</summary>
    ///  <param name="AKey">The key to operate on.</param>
    ///  <returns>The value associated with the key.</returns>
    ///  <remarks>If the dictionary does not contain the key, this method acts like <c>Add</c> if assignment is done to this property;
    ///  otherwise the value of the specified key is modified.</remarks>
    ///  <exception cref="Collections.Base|EKeyNotFoundException">The trying to read the value of a key that is
    ///  not found in the dictionary.</exception>
    property Items[const AKey: TKey]: TValue read GetItem write SetItem; default;

    ///  <summary>Specifies the number of key-value pairs in the dictionary.</summary>
    ///  <returns>A positive value specifying the number of pairs in the dictionary.</returns>
    property Count: NativeInt read GetCount;

    ///  <summary>Specifies the collection that contains only the keys.</summary>
    ///  <returns>An Enex collection that contains all the keys stored in the dictionary.</returns>
    property Keys: IEnexCollection<TKey> read FKeyCollection;

    ///  <summary>Specifies the collection that contains only the values.</summary>
    ///  <returns>An Enex collection that contains all the values stored in the dictionary.</returns>
    property Values: IEnexCollection<TValue> read FValueCollection;

    ///  <summary>Returns a new enumerator object used to enumerate this dictionary.</summary>
    ///  <remarks>This method is usually called by compiler-generated code. Its purpose is to create an enumerator
    ///  object that is used to actually traverse the dictionary.</remarks>
    ///  <returns>An enumerator object.</returns>
    function GetEnumerator(): IEnumerator<TPair<TKey,TValue>>; override;

    ///  <summary>Copies the values stored in the dictionary to a given array.</summary>
    ///  <param name="AArray">An array where to copy the contents of the dictionary.</param>
    ///  <param name="AStartIndex">The index into the array at which the copying begins.</param>
    ///  <remarks>This method assumes that <paramref name="AArray"/> has enough space to hold the contents of the dictionary.</remarks>
    ///  <exception cref="SysUtils|EArgumentOutOfRangeException"><paramref name="AStartIndex"/> is out of bounds.</exception>
    ///  <exception cref="Collections.Base|EArgumentOutOfSpaceException">The array is not long enough.</exception>
    procedure CopyTo(var AArray: array of TPair<TKey,TValue>; const AStartIndex: NativeInt); overload; override;

    ///  <summary>Returns the value associated with the given key.</summary>
    ///  <param name="AKey">The key for which to return the associated value.</param>
    ///  <returns>The value associated with the given key.</returns>
    ///  <exception cref="Collections.Base|EKeyNotFoundException">No such key in the dictionary.</exception>
    function ValueForKey(const AKey: TKey): TValue; override;

    ///  <summary>Checks whether the dictionary contains a given key-value pair.</summary>
    ///  <param name="AKey">The key part of the pair.</param>
    ///  <param name="AValue">The value part of the pair.</param>
    ///  <returns><c>True</c> if the given key-value pair exists; <c>False</c> otherwise.</returns>
    function KeyHasValue(const AKey: TKey; const AValue: TValue): Boolean; override;

    ///  <summary>Returns an Enex collection that contains only the keys.</summary>
    ///  <returns>An Enex collection that contains all the keys stored in the dictionary.</returns>
    function SelectKeys(): IEnexCollection<TKey>; override;

    ///  <summary>Returns an Enex collection that contains only the values.</summary>
    ///  <returns>An Enex collection that contains all the values stored in the dictionary.</returns>
    function SelectValues(): IEnexCollection<TValue>; override;
  end;

  ///  <summary>The generic <c>dictionary</c> collection designed to store objects.</summary>
  ///  <remarks>This type uses hashing mechanisms to store its key-value pairs.</remarks>
  TObjectDictionary<TKey, TValue> = class(TDictionary<TKey, TValue>)
  private
    FOwnsKeys, FOwnsValues: Boolean;

  protected
    ///  <summary>Frees the key (object) that was removed from the collection.</summary>
    ///  <param name="AKey">The key that was removed from the collection.</param>
    procedure HandleKeyRemoved(const AKey: TKey); override;

    ///  <summary>Frees the value (object) that was removed from the collection.</summary>
    ///  <param name="AKey">The value that was removed from the collection.</param>
    procedure HandleValueRemoved(const AValue: TValue); override;

    ///  <summary>Replaces a given value with a new one.</summary>
    ///  <param name="ACurrent">The value to be replaced.</param>
    ///  <param name="ANew">The value to be replaced with.</param>
    ///  <remarks>This implementation will check the objects by reference and free the current one if needed.</remarks>
    procedure ReplaceValue(var ACurrent: TValue; const ANew: TValue); override;
  public
    ///  <summary>Specifies whether this dictionary owns the keys.</summary>
    ///  <returns><c>True</c> if the dictionary owns the keys; <c>False</c> otherwise.</returns>
    ///  <remarks>This property specifies the way the dictionary controls the life-time of the stored keys. The value of
    ///  this property has effect only if the keys are objects, otherwise it is ignored.</remarks>
    property OwnsKeys: Boolean read FOwnsKeys write FOwnsKeys;

    ///  <summary>Specifies whether this dictionary owns the values.</summary>
    ///  <returns><c>True</c> if the dictionary owns the values; <c>False</c> otherwise.</returns>
    ///  <remarks>This property specifies the way the dictionary controls the life-time of the stored values. The value of
    ///  this property has effect only if the values are objects, otherwise it is ignored.</remarks>
    property OwnsValues: Boolean read FOwnsValues write FOwnsValues;
  end;

type
  ///  <summary>The generic <c>dictionary</c> collection.</summary>
  ///  <remarks>This type uses hashing mechanisms and linked lists to store its key-value pairs.</remarks>
  TLinkedDictionary<TKey, TValue> = class(TEnexAssociativeCollection<TKey, TValue>, IDictionary<TKey, TValue>)
  private type
    {$REGION 'Internal Types'}

    PEntry = ^TEntry;
    TEntry = record
      FHashCode: NativeInt;
      FNext, FPrev: PEntry;
      FKey: TKey;
      FValue: TValue;
    end;

    TBucketArray = TArray<PEntry>;

    { Generic Dictionary Pairs Enumerator }
    TPairEnumerator = class(TEnumerator<TPair<TKey,TValue>>)
    private
      FVer: NativeInt;
      FDict: TLinkedDictionary<TKey, TValue>;
      FCurrentEntry: PEntry;
      FValue: TPair<TKey, TValue>;
    public
      { Constructor }
      constructor Create(const ADict: TLinkedDictionary<TKey, TValue>);

      { Destructor }
      destructor Destroy(); override;

      function GetCurrent(): TPair<TKey,TValue>; override;
      function MoveNext(): Boolean; override;
    end;

    { Generic Dictionary Keys Enumerator }
    TKeyEnumerator = class(TEnumerator<TKey>)
    private
      FVer: NativeInt;
      FDict: TLinkedDictionary<TKey, TValue>;
      FCurrentEntry: PEntry;
      FValue: TKey;
    public
      { Constructor }
      constructor Create(const ADict: TLinkedDictionary<TKey, TValue>);

      { Destructor }
      destructor Destroy(); override;

      function GetCurrent(): TKey; override;
      function MoveNext(): Boolean; override;
    end;

    { Generic Dictionary Values Enumerator }
    TValueEnumerator = class(TEnumerator<TValue>)
    private
      FVer: NativeInt;
      FDict: TLinkedDictionary<TKey, TValue>;
      FCurrentEntry: PEntry;
      FValue: TValue;
    public
      { Constructor }
      constructor Create(const ADict: TLinkedDictionary<TKey, TValue>);

      { Destructor }
      destructor Destroy(); override;

      function GetCurrent(): TValue; override;
      function MoveNext(): Boolean; override;
    end;

    { Generic Dictionary Keys Collection }
    TKeyCollection = class(TEnexCollection<TKey>)
    private
      FDict: TLinkedDictionary<TKey, TValue>;

    protected
      { Hidden }
      function GetCount(): NativeInt; override;

    public
      { Constructor }
      constructor Create(const ADict: TLinkedDictionary<TKey, TValue>);

      { Property }
      property Count: NativeInt read GetCount;

      { IEnumerable/ ICollection support }
      function GetEnumerator(): IEnumerator<TKey>; override;

      { Copy-To }
      procedure CopyTo(var AArray: array of TKey; const AStartIndex: NativeInt); overload; override;
    end;

    { Generic Dictionary Values Collection }
    TValueCollection = class(TEnexCollection<TValue>)
    private
      FDict: TLinkedDictionary<TKey, TValue>;

    protected
      { Hidden }
      function GetCount: NativeInt; override;

    public
      { Constructor }
      constructor Create(const ADict: TLinkedDictionary<TKey, TValue>);

      { Property }
      property Count: NativeInt read GetCount;

      { IEnumerable/ ICollection support }
      function GetEnumerator(): IEnumerator<TValue>; override;

      { Copy-To }
      procedure CopyTo(var AArray: array of TValue; const AStartIndex: NativeInt); overload; override;
    end;
    {$ENDREGION}

  private var
    FBucketArray: TBucketArray;
    FKeyCollection: IEnexCollection<TKey>;
    FValueCollection: IEnexCollection<TValue>;
    FCount, FFreeCount: NativeInt;
    FVer: NativeInt;
    FHead, FTail, FFirstFree: PEntry;

    { Internal }
    procedure InitializeInternals(const ACapacity: NativeInt);
    procedure Insert(const AKey: TKey; const AValue: TValue; const AShouldAdd: Boolean = true);
    procedure ReInsert(const AEntry: PEntry; const ACapacity: NativeInt);

    function FindEntry(const AKey: TKey): PEntry;
    function Hash(const AKey: TKey): NativeInt;

    { Caching }
    function NeedEntry(const AKey: TKey; const AValue: TValue; const AHash: NativeInt): PEntry;
    procedure ReleaseEntry(const AEntry: PEntry);

  protected
    ///  <summary>Returns the number of key-value pairs in the dictionary.</summary>
    ///  <returns>A positive value specifying the number of pairs in the dictionary.</returns>
    function GetCount(): NativeInt; override;

    ///  <summary>Returns the value associated with the given key.</summary>
    ///  <param name="AKey">The key for which to try to retrieve the value.</param>
    ///  <returns>The value associated with the key.</returns>
    ///  <exception cref="Collections.Base|EKeyNotFoundException">The key is not found in the dictionary.</exception>
    function GetItem(const AKey: TKey): TValue;

    ///  <summary>Sets the value for a given key.</summary>
    ///  <param name="AKey">The key for which to set the value.</param>
    ///  <param name="AValue">The value to set.</param>
    ///  <remarks>If the dictionary does not contain the key, this method acts like <c>Add</c>; otherwise the
    ///  value of the specified key is modified.</remarks>
    procedure SetItem(const AKey: TKey; const Value: TValue);

    ///  <summary>Replaces a given value with a new one.</summary>
    ///  <param name="ACurrent">The value to be replaced.</param>
    ///  <param name="ANew">The value to be replaced with.</param>
    ///  <remarks>This method is called by the dictioanry when a value needs to be replaced with another.
    ///  The default implementation will compare the values, if those are equal nothing is done. Otherwise the old value is
    ///  "disposed of" and the new one is copied over. Descendant classes my want another behaviour.</remarks>
    procedure ReplaceValue(var ACurrent: TValue; const ANew: TValue); virtual;
  public
    ///  <summary>Creates a new instance of this class.</summary>
    ///  <remarks>The default rule set is requested.</remarks>
    constructor Create(); overload;

    ///  <summary>Creates a new instance of this class.</summary>
    ///  <param name="AInitialCapacity">The dictionary's initial capacity.</param>
    ///  <remarks>The default rule set is requested.</remarks>
    constructor Create(const AInitialCapacity: NativeInt); overload;

    ///  <summary>Creates a new instance of this class.</summary>
    ///  <param name="ACollection">A collection to copy pairs from.</param>
    ///  <remarks>The default rule set is requested.</remarks>
    ///  <exception cref="SysUtils|EArgumentNilException"><paramref name="ACollection"/> is <c>nil</c>.</exception>
    ///  <exception cref="Collections.Base|EDuplicateKeyException"><paramref name="ACollection"/> contains pairs with equal keys.</exception>
    constructor Create(const ACollection: IEnumerable<TPair<TKey, TValue>>); overload;

    ///  <summary>Creates a new instance of this class.</summary>
    ///  <param name="AArray">An array to copy pairs from.</param>
    ///  <remarks>The default rule set is requested.</remarks>
    ///  <exception cref="Collections.Base|EDuplicateKeyException"><paramref name="AArray"/> contains pairs with equal keys.</exception>
    constructor Create(const AArray: array of TPair<TKey, TValue>); overload;

    ///  <summary>Creates a new instance of this class.</summary>
    ///  <param name="AKeyRules">A rule set describing the keys in the dictionary.</param>
    ///  <param name="AValueRules">A rule set describing the values in the dictionary.</param>
    constructor Create(const AKeyRules: TRules<TKey>; const AValueRules: TRules<TValue>); overload;

    ///  <summary>Creates a new instance of this class.</summary>
    ///  <param name="AKeyRules">A rule set describing the keys in the dictionary.</param>
    ///  <param name="AValueRules">A rule set describing the values in the dictionary.</param>
    ///  <param name="AInitialCapacity">The dictionary's initial capacity.</param>
    constructor Create(const AKeyRules: TRules<TKey>; const AValueRules: TRules<TValue>;
      const AInitialCapacity: NativeInt); overload;

    ///  <summary>Creates a new instance of this class.</summary>
    ///  <param name="ACollection">A collection to copy pairs from.</param>
    ///  <param name="AKeyRules">A rule set describing the keys in the dictionary.</param>
    ///  <param name="AValueRules">A rule set describing the values in the dictionary.</param>
    ///  <exception cref="SysUtils|EArgumentNilException"><paramref name="ACollection"/> is <c>nil</c>.</exception>
    ///  <exception cref="SysUtils|EArgumentNilException"><paramref name="ARules"/> is <c>nil</c>.</exception>
    ///  <exception cref="Collections.Base|EDuplicateKeyException"><paramref name="ACollection"/> contains pairs with equal keys.</exception>
    constructor Create(const AKeyRules: TRules<TKey>; const AValueRules: TRules<TValue>;
      const ACollection: IEnumerable<TPair<TKey, TValue>>); overload;

    ///  <summary>Creates a new instance of this class.</summary>
    ///  <param name="AArray">An array to copy pairs from.</param>
    ///  <param name="AKeyRules">A rule set describing the keys in the dictionary.</param>
    ///  <param name="AValueRules">A rule set describing the values in the dictionary.</param>
    ///  <exception cref="Collections.Base|EDuplicateKeyException"><paramref name="AArray"/> contains pairs with equal keys.</exception>
    constructor Create(const AKeyRules: TRules<TKey>; const AValueRules: TRules<TValue>;
      const AArray: array of TPair<TKey,TValue>); overload;

    ///  <summary>Destroys this instance.</summary>
    ///  <remarks>Do not call this method directly; call <c>Free</c> instead.</remarks>
    destructor Destroy(); override;

    ///  <summary>Clears the contents of the dictionary.</summary>
    procedure Clear();

    ///  <summary>Adds a key-value pair to the dictionary.</summary>
    ///  <param name="APair">The key-value pair to add.</param>
    ///  <exception cref="Collections.Base|EDuplicateKeyException">The dictionary already contains a pair with the given key.</exception>
    procedure Add(const APair: TPair<TKey,TValue>); overload;

    ///  <summary>Adds a key-value pair to the dictionary.</summary>
    ///  <param name="AKey">The key of pair.</param>
    ///  <param name="AValue">The value associated with the key.</param>
    ///  <exception cref="Collections.Base|EDuplicateKeyException">The dictionary already contains a pair with the given key.</exception>
    procedure Add(const AKey: TKey; const AValue: TValue); overload;

    ///  <summary>Removes a key-value pair using a given key.</summary>
    ///  <param name="AKey">The key of the pair to remove.</param>
    ///  <remarks>If the specified key was not found in the dictionary, nothing happens.</remarks>
    procedure Remove(const AKey: TKey); overload;

    ///  <summary>Checks whether the dictionary contains a key-value pair identified by the given key.</summary>
    ///  <param name="AKey">The key to check for.</param>
    ///  <returns><c>True</c> if the dictionary contains a pair identified by the given key; <c>False</c> otherwise.</returns>
    function ContainsKey(const AKey: TKey): Boolean;

    ///  <summary>Checks whether the dictionary contains a key-value pair that contains a given value.</summary>
    ///  <param name="AValue">The value to check for.</param>
    ///  <returns><c>True</c> if the dictionary contains a pair containing the given value; <c>False</c> otherwise.</returns>
    function ContainsValue(const AValue: TValue): Boolean;

    ///  <summary>Tries to obtain the value associated with a given key.</summary>
    ///  <param name="AKey">The key for which to try to retrieve the value.</param>
    ///  <param name="AFoundValue">The found value (if the result is <c>True</c>).</param>
    ///  <returns><c>True</c> if the dictionary contains a value for the given key; <c>False</c> otherwise.</returns>
    function TryGetValue(const AKey: TKey; out AFoundValue: TValue): Boolean;

    ///  <summary>Gets or sets the value for a given key.</summary>
    ///  <param name="AKey">The key to operate on.</param>
    ///  <returns>The value associated with the key.</returns>
    ///  <remarks>If the dictionary does not contain the key, this method acts like <c>Add</c> if assignment is done to this property;
    ///  otherwise the value of the specified key is modified.</remarks>
    ///  <exception cref="Collections.Base|EKeyNotFoundException">The trying to read the value of a key that is
    ///  not found in the dictionary.</exception>
    property Items[const AKey: TKey]: TValue read GetItem write SetItem; default;

    ///  <summary>Specifies the number of key-value pairs in the dictionary.</summary>
    ///  <returns>A positive value specifying the number of pairs in the dictionary.</returns>
    property Count: NativeInt read GetCount;

    ///  <summary>Specifies the collection that contains only the keys.</summary>
    ///  <returns>An Enex collection that contains all the keys stored in the dictionary.</returns>
    property Keys: IEnexCollection<TKey> read FKeyCollection;

    ///  <summary>Specifies the collection that contains only the values.</summary>
    ///  <returns>An Enex collection that contains all the values stored in the dictionary.</returns>
    property Values: IEnexCollection<TValue> read FValueCollection;

    ///  <summary>Returns a new enumerator object used to enumerate this dictionary.</summary>
    ///  <remarks>This method is usually called by compiler-generated code. Its purpose is to create an enumerator
    ///  object that is used to actually traverse the dictionary.</remarks>
    ///  <returns>An enumerator object.</returns>
    function GetEnumerator(): IEnumerator<TPair<TKey,TValue>>; override;

    ///  <summary>Copies the values stored in the dictionary to a given array.</summary>
    ///  <param name="AArray">An array where to copy the contents of the dictionary.</param>
    ///  <param name="AStartIndex">The index into the array at which the copying begins.</param>
    ///  <remarks>This method assumes that <paramref name="AArray"/> has enough space to hold the contents of the dictionary.</remarks>
    ///  <exception cref="SysUtils|EArgumentOutOfRangeException"><paramref name="AStartIndex"/> is out of bounds.</exception>
    ///  <exception cref="Collections.Base|EArgumentOutOfSpaceException">The array is not long enough.</exception>
    procedure CopyTo(var AArray: array of TPair<TKey,TValue>; const AStartIndex: NativeInt); overload; override;

    ///  <summary>Returns the value associated with the given key.</summary>
    ///  <param name="AKey">The key for which to return the associated value.</param>
    ///  <returns>The value associated with the given key.</returns>
    ///  <exception cref="Collections.Base|EKeyNotFoundException">No such key in the dictionary.</exception>
    function ValueForKey(const AKey: TKey): TValue; override;

    ///  <summary>Checks whether the dictionary contains a given key-value pair.</summary>
    ///  <param name="AKey">The key part of the pair.</param>
    ///  <param name="AValue">The value part of the pair.</param>
    ///  <returns><c>True</c> if the given key-value pair exists; <c>False</c> otherwise.</returns>
    function KeyHasValue(const AKey: TKey; const AValue: TValue): Boolean; override;

    ///  <summary>Returns an Enex collection that contains only the keys.</summary>
    ///  <returns>An Enex collection that contains all the keys stored in the dictionary.</returns>
    function SelectKeys(): IEnexCollection<TKey>; override;

    ///  <summary>Returns an Enex collection that contains only the values.</summary>
    ///  <returns>An Enex collection that contains all the values stored in the dictionary.</returns>
    function SelectValues(): IEnexCollection<TValue>; override;
  end;

  ///  <summary>The generic <c>dictionary</c> collection designed to store objects.</summary>
  ///  <remarks>This type uses hashing mechanisms and linked lists to store its key-value pairs.</remarks>
  TObjectLinkedDictionary<TKey, TValue> = class(TLinkedDictionary<TKey, TValue>)
  private
    FOwnsKeys, FOwnsValues: Boolean;

  protected
    ///  <summary>Frees the key (object) that was removed from the collection.</summary>
    ///  <param name="AKey">The key that was removed from the collection.</param>
    procedure HandleKeyRemoved(const AKey: TKey); override;

    ///  <summary>Frees the value (object) that was removed from the collection.</summary>
    ///  <param name="AKey">The value that was removed from the collection.</param>
    procedure HandleValueRemoved(const AValue: TValue); override;

    ///  <summary>Replaces a given value with a new one.</summary>
    ///  <param name="ACurrent">The value to be replaced.</param>
    ///  <param name="ANew">The value to be replaced with.</param>
    ///  <remarks>This implementation will check the objects by reference and free the current one if needed.</remarks>
    procedure ReplaceValue(var ACurrent: TValue; const ANew: TValue); override;
  public
    ///  <summary>Specifies whether this dictionary owns the keys.</summary>
    ///  <returns><c>True</c> if the dictionary owns the keys; <c>False</c> otherwise.</returns>
    ///  <remarks>This property specifies the way the dictionary controls the life-time of the stored keys. The value of
    ///  this property has effect only if the keys are objects, otherwise it is ignored.</remarks>
    property OwnsKeys: Boolean read FOwnsKeys write FOwnsKeys;

    ///  <summary>Specifies whether this dictionary owns the values.</summary>
    ///  <returns><c>True</c> if the dictionary owns the values; <c>False</c> otherwise.</returns>
    ///  <remarks>This property specifies the way the dictionary controls the life-time of the stored values. The value of
    ///  this property has effect only if the values are objects, otherwise it is ignored.</remarks>
    property OwnsValues: Boolean read FOwnsValues write FOwnsValues;
  end;

type
  ///  <summary>The generic <c>sorted dictionary</c> collection.</summary>
  ///  <remarks>This type uses an AVL tree to store its key-value pairs.</remarks>
  TSortedDictionary<TKey, TValue> = class(TEnexAssociativeCollection<TKey, TValue>, IDictionary<TKey, TValue>)
  private type
    {$REGION 'Internal Types'}
    TBalanceAct = (baStart, baLeft, baRight, baLoop, baEnd);

    { An internal node class }
    TNode = class
    private
      FKey: TKey;
      FValue: TValue;

      FParent,
       FLeft, FRight: TNode;

      FBalance: ShortInt;
    end;

    { Generic Dictionary Pairs Enumerator }
    TPairEnumerator = class(TEnumerator<TPair<TKey,TValue>>)
    private
      FVer: NativeInt;
      FDict: TSortedDictionary<TKey, TValue>;
      FNext: TNode;
      FValue: TPair<TKey,TValue>;

    public
      { Constructor }
      constructor Create(const ADict: TSortedDictionary<TKey, TValue>);

      { Destructor }
      destructor Destroy(); override;

      function GetCurrent(): TPair<TKey,TValue>; override;
      function MoveNext(): Boolean; override;
    end;

    { Generic Dictionary Keys Enumerator }
    TKeyEnumerator = class(TEnumerator<TKey>)
    private
      FVer: NativeInt;
      FDict: TSortedDictionary<TKey, TValue>;
      FNext: TNode;
      FValue: TKey;

    public
      { Constructor }
      constructor Create(const ADict: TSortedDictionary<TKey, TValue>);

      { Destructor }
      destructor Destroy(); override;

      function GetCurrent(): TKey; override;
      function MoveNext(): Boolean; override;
    end;

    { Generic Dictionary Values Enumerator }
    TValueEnumerator = class(TEnumerator<TValue>)
    private
      FVer: NativeInt;
      FDict: TSortedDictionary<TKey, TValue>;
      FNext: TNode;
      FValue: TValue;

    public
      { Constructor }
      constructor Create(const ADict: TSortedDictionary<TKey, TValue>);

      { Destructor }
      destructor Destroy(); override;

      function GetCurrent(): TValue; override;
      function MoveNext(): Boolean; override;
    end;

    { Generic Dictionary Keys Collection }
    TKeyCollection = class(TEnexCollection<TKey>)
    private
      FDict: TSortedDictionary<TKey, TValue>;

    protected
      { Hidden }
      function GetCount(): NativeInt; override;

    public
      { Constructor }
      constructor Create(const ADict: TSortedDictionary<TKey, TValue>);

      { Property }
      property Count: NativeInt read GetCount;

      { IEnumerable/ ICollection support }
      function GetEnumerator(): IEnumerator<TKey>; override;

      { Copy-To }
      procedure CopyTo(var AArray: array of TKey; const AStartIndex: NativeInt); overload; override;
    end;

    { Generic Dictionary Values Collection }
    TValueCollection = class(TEnexCollection<TValue>)
    private
      FDict: TSortedDictionary<TKey, TValue>;

    protected
      { Hidden }
      function GetCount: NativeInt; override;

    public
      { Constructor }
      constructor Create(const ADict: TSortedDictionary<TKey, TValue>);

      { Property }
      property Count: NativeInt read GetCount;

      { IEnumerable/ ICollection support }
      function GetEnumerator(): IEnumerator<TValue>; override;

      { Copy-To }
      procedure CopyTo(var AArray: array of TValue; const AStartIndex: NativeInt); overload; override;
    end;
    {$ENDREGION}

  private var
    FCount: NativeInt;
    FVer: NativeInt;
    FRoot: TNode;
    FSignFix: NativeInt;
    FKeyCollection: IEnexCollection<TKey>;
    FValueCollection: IEnexCollection<TValue>;

    { Some internals }
    function FindNodeWithKey(const AKey: TKey): TNode;
    function FindLeftMostNode(): TNode;
    function FindRightMostNode(): TNode;
    function WalkToTheRight(const ANode: TNode): TNode;

    { ... }
    function MakeNode(const AKey: TKey; const AValue: TValue; const ARoot: TNode): TNode;
    procedure RecursiveClear(const ANode: TNode);
    procedure ReBalanceSubTreeOnInsert(const ANode: TNode);
    function Insert(const AKey: TKey; const AValue: TValue; const ChangeOrFail: Boolean): Boolean;

    { Removal }
    procedure BalanceTreesAfterRemoval(const ANode: TNode);
  protected
    ///  <summary>Returns the number of key-value pairs in the dictionary.</summary>
    ///  <returns>A positive value specifying the number of pairs in the dictionary.</returns>
    function GetCount(): NativeInt; override;

    ///  <summary>Returns the value associated with the given key.</summary>
    ///  <param name="AKey">The key for which to try to retrieve the value.</param>
    ///  <returns>The value associated with the key.</returns>
    ///  <exception cref="Collections.Base|EKeyNotFoundException">The key is not found in the dictionary.</exception>
    function GetItem(const AKey: TKey): TValue;

    ///  <summary>Sets the value for a given key.</summary>
    ///  <param name="AKey">The key for which to set the value.</param>
    ///  <param name="AValue">The value to set.</param>
    ///  <remarks>If the dictionary does not contain the key, this method acts like <c>Add</c>; otherwise the
    ///  value of the specified key is modified.</remarks>
    procedure SetItem(const AKey: TKey; const Value: TValue);

    ///  <summary>Replaces a given value with a new one.</summary>
    ///  <param name="ACurrent">The value to be replaced.</param>
    ///  <param name="ANew">The value to be replaced with.</param>
    ///  <remarks>This method is called by the dictioanry when a value needs to be replaced with another.
    ///  The default implementation will compare the values, if those are equal nothing is done. Otherwise the old value is
    ///  "disposed of" and the new one is copied over. Descendant classes my want another behaviour.</remarks>
    procedure ReplaceValue(var ACurrent: TValue; const ANew: TValue); virtual;
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
    ///  <exception cref="Collections.Base|EDuplicateKeyException"><paramref name="ACollection"/> contains pairs with equal keys.</exception>
    constructor Create(const ACollection: IEnumerable<TPair<TKey,TValue>>; const AAscending: Boolean = true); overload;

    ///  <summary>Creates a new instance of this class.</summary>
    ///  <param name="AArray">An array to copy the key-value pairs from.</param>
    ///  <param name="AAscending">A value specifying whether the keys are sorted in ascending order. The default is <c>True</c>.</param>
    ///  <remarks>The default rule set is requested.</remarks>
    ///  <exception cref="Collections.Base|EDuplicateKeyException"><paramref name="AArray"/> contains pairs with equal keys.</exception>
    constructor Create(const AArray: array of TPair<TKey,TValue>; const AAscending: Boolean = true); overload;

    ///  <summary>Creates a new instance of this class.</summary>
    ///  <param name="AKeyRules">A rule set describing the keys in the dictionary.</param>
    ///  <param name="AValueRules">A rule set describing the values in the dictionary.</param>
    ///  <param name="AAscending">A value specifying whether the keys are sorted in ascending order. The default is <c>True</c>.</param>
    constructor Create(const AKeyRules: TRules<TKey>; const AValueRules: TRules<TValue>;
      const AAscending: Boolean = true); overload;

    ///  <summary>Creates a new instance of this class.</summary>
    ///  <param name="AKeyRules">A rule set describing the keys in the dictionary.</param>
    ///  <param name="AValueRules">A rule set describing the values in the dictionary.</param>    
    ///  <param name="ACollection">A collection to copy the key-value pairs from.</param>
    ///  <param name="AAscending">A value specifying whether the keys are sorted in ascending order. The default is <c>True</c>.</param>
    ///  <exception cref="SysUtils|EArgumentNilException"><paramref name="ACollection"/> is <c>nil</c>.</exception>
    ///  <exception cref="Collections.Base|EDuplicateKeyException"><paramref name="ACollection"/> contains pairs with equal keys.</exception>
    constructor Create(const AKeyRules: TRules<TKey>; const AValueRules: TRules<TValue>;
      const ACollection: IEnumerable<TPair<TKey,TValue>>; const AAscending: Boolean = true); overload;

    ///  <summary>Creates a new instance of this class.</summary>
    ///  <param name="AKeyRules">A rule set describing the keys in the dictionary.</param>
    ///  <param name="AValueRules">A rule set describing the values in the dictionary.</param>
    ///  <param name="AArray">An array to copy the key-value pairs from.</param>
    ///  <param name="AAscending">A value specifying whether the keys are sorted in ascending order. The default is <c>True</c>.</param>
    ///  <exception cref="Collections.Base|EDuplicateKeyException"><paramref name="AArray"/> contains pairs with equal keys.</exception>
    constructor Create(const AKeyRules: TRules<TKey>; const AValueRules: TRules<TValue>;
      const AArray: array of TPair<TKey,TValue>; const AAscending: Boolean = true); overload;

    ///  <summary>Destroys this instance.</summary>
    ///  <remarks>Do not call this method directly; call <c>Free</c> instead.</remarks>
    destructor Destroy(); override;

    ///  <summary>Clears the contents of the dictionary.</summary>
    procedure Clear();

    ///  <summary>Adds a key-value pair to the dictionary.</summary>
    ///  <param name="APair">The key-value pair to add.</param>
    ///  <exception cref="Collections.Base|EDuplicateKeyException">The dictionary already contains a pair with the given key.</exception>
    procedure Add(const APair: TPair<TKey,TValue>); overload;

    ///  <summary>Adds a key-value pair to the dictionary.</summary>
    ///  <param name="AKey">The key of pair.</param>
    ///  <param name="AValue">The value associated with the key.</param>
    ///  <exception cref="Collections.Base|EDuplicateKeyException">The dictionary already contains a pair with the given key.</exception>
    procedure Add(const AKey: TKey; const AValue: TValue); overload;

    ///  <summary>Removes a key-value pair using a given key.</summary>
    ///  <param name="AKey">The key of the pair to remove.</param>
    ///  <remarks>If the specified key was not found in the dictionary, nothing happens.</remarks>
    procedure Remove(const AKey: TKey); overload;

    ///  <summary>Checks whether the dictionary contains a key-value pair identified by the given key.</summary>
    ///  <param name="AKey">The key to check for.</param>
    ///  <returns><c>True</c> if the dictionary contains a pair identified by the given key; <c>False</c> otherwise.</returns>
    function ContainsKey(const AKey: TKey): Boolean;

    ///  <summary>Checks whether the dictionary contains a key-value pair that contains a given value.</summary>
    ///  <param name="AValue">The value to check for.</param>
    ///  <returns><c>True</c> if the dictionary contains a pair containing the given value; <c>False</c> otherwise.</returns>
    function ContainsValue(const AValue: TValue): Boolean;

    ///  <summary>Tries to obtain the value associated with a given key.</summary>
    ///  <param name="AKey">The key for which to try to retrieve the value.</param>
    ///  <param name="AFoundValue">The found value (if the result is <c>True</c>).</param>
    ///  <returns><c>True</c> if the dictionary contains a value for the given key; <c>False</c> otherwise.</returns>
    function TryGetValue(const AKey: TKey; out AFoundValue: TValue): Boolean;

    ///  <summary>Gets or sets the value for a given key.</summary>
    ///  <param name="AKey">The key to operate on.</param>
    ///  <returns>The value associated with the key.</returns>
    ///  <remarks>If the dictionary does not contain the key, this method acts like <c>Add</c> if assignment is done to this property;
    ///  otherwise the value of the specified key is modified.</remarks>
    ///  <exception cref="Collections.Base|EKeyNotFoundException">The trying to read the value of a key that is
    ///  not found in the dictionary.</exception>
    property Items[const AKey: TKey]: TValue read GetItem write SetItem; default;

    ///  <summary>Specifies the number of key-value pairs in the dictionary.</summary>
    ///  <returns>A positive value specifying the number of pairs in the dictionary.</returns>
    property Count: NativeInt read GetCount;

    ///  <summary>Specifies the collection that contains only the keys.</summary>
    ///  <returns>An Enex collection that contains all the keys stored in the dictionary.</returns>
    property Keys: IEnexCollection<TKey> read FKeyCollection;

    ///  <summary>Specifies the collection that contains only the values.</summary>
    ///  <returns>An Enex collection that contains all the values stored in the dictionary.</returns>
    property Values: IEnexCollection<TValue> read FValueCollection;

    ///  <summary>Returns a new enumerator object used to enumerate this dictionary.</summary>
    ///  <remarks>This method is usually called by compiler-generated code. Its purpose is to create an enumerator
    ///  object that is used to actually traverse the dictionary.</remarks>
    ///  <returns>An enumerator object.</returns>
    function GetEnumerator(): IEnumerator<TPair<TKey,TValue>>; override;

    ///  <summary>Copies the values stored in the dictionary to a given array.</summary>
    ///  <param name="AArray">An array where to copy the contents of the dictionary.</param>
    ///  <param name="AStartIndex">The index into the array at which the copying begins.</param>
    ///  <remarks>This method assumes that <paramref name="AArray"/> has enough space to hold the contents of the dictionary.</remarks>
    ///  <exception cref="SysUtils|EArgumentOutOfRangeException"><paramref name="AStartIndex"/> is out of bounds.</exception>
    ///  <exception cref="Collections.Base|EArgumentOutOfSpaceException">The array is not long enough.</exception>
    procedure CopyTo(var AArray: array of TPair<TKey,TValue>; const AStartIndex: NativeInt); overload; override;

    ///  <summary>Returns the value associated with the given key.</summary>
    ///  <param name="AKey">The key for which to return the associated value.</param>
    ///  <returns>The value associated with the given key.</returns>
    ///  <exception cref="Collections.Base|EKeyNotFoundException">No such key in the dictionary.</exception>
    function ValueForKey(const AKey: TKey): TValue; override;

    ///  <summary>Checks whether the dictionary contains a given key-value pair.</summary>
    ///  <param name="AKey">The key part of the pair.</param>
    ///  <param name="AValue">The value part of the pair.</param>
    ///  <returns><c>True</c> if the given key-value pair exists; <c>False</c> otherwise.</returns>
    function KeyHasValue(const AKey: TKey; const AValue: TValue): Boolean; override;

    ///  <summary>Returns the biggest key.</summary>
    ///  <returns>The biggest key stored in the dictionary.</returns>
    ///  <exception cref="Collections.Base|ECollectionEmptyException">The dictionary is empty.</exception>
    function MaxKey(): TKey; override;

    ///  <summary>Returns the smallest key.</summary>
    ///  <returns>The smallest key stored in the dictionary.</returns>
    ///  <exception cref="Collections.Base|ECollectionEmptyException">The dictionary is empty.</exception>
    function MinKey(): TKey; override;

    ///  <summary>Returns an Enex collection that contains only the keys.</summary>
    ///  <returns>An Enex collection that contains all the keys stored in the dictionary.</returns>
    function SelectKeys(): IEnexCollection<TKey>; override;

    ///  <summary>Returns an Enex collection that contains only the values.</summary>
    ///  <returns>An Enex collection that contains all the values stored in the dictionary.</returns>
    function SelectValues(): IEnexCollection<TValue>; override;
  end;

  ///  <summary>The generic <c>sorted dictionary</c> collection designed to store objects.</summary>
  ///  <remarks>This type uses an AVL tree to store its key-value pairs.</remarks>
  TObjectSortedDictionary<TKey, TValue> = class(TSortedDictionary<TKey, TValue>)
  private
    FOwnsKeys, FOwnsValues: Boolean;

  protected
    ///  <summary>Frees the key (object) that was removed from the collection.</summary>
    ///  <param name="AKey">The key that was removed from the collection.</param>
    procedure HandleKeyRemoved(const AKey: TKey); override;

    ///  <summary>Frees the value (object) that was removed from the collection.</summary>
    ///  <param name="AKey">The value that was removed from the collection.</param>
    procedure HandleValueRemoved(const AValue: TValue); override;

    ///  <summary>Replaces a given value with a new one.</summary>
    ///  <param name="ACurrent">The value to be replaced.</param>
    ///  <param name="ANew">The value to be replaced with.</param>
    ///  <remarks>This implementation will check the objects by reference and free the current one if needed.</remarks>
    procedure ReplaceValue(var ACurrent: TValue; const ANew: TValue); override;
  public
    ///  <summary>Specifies whether this dictionary owns the keys.</summary>
    ///  <returns><c>True</c> if the dictionary owns the keys; <c>False</c> otherwise.</returns>
    ///  <remarks>This property specifies the way the dictionary controls the life-time of the stored keys. The value of
    ///  this property has effect only if the keys are objects, otherwise it is ignored.</remarks>
    property OwnsKeys: Boolean read FOwnsKeys write FOwnsKeys;

    ///  <summary>Specifies whether this dictionary owns the values.</summary>
    ///  <returns><c>True</c> if the dictionary owns the values; <c>False</c> otherwise.</returns>
    ///  <remarks>This property specifies the way the dictionary controls the life-time of the stored values. The value of
    ///  this property has effect only if the values are objects, otherwise it is ignored.</remarks>
    property OwnsValues: Boolean read FOwnsValues write FOwnsValues;
  end;

implementation

{ TDictionary<TKey, TValue> }

procedure TDictionary<TKey, TValue>.Add(const APair: TPair<TKey, TValue>);
begin
 { Call insert }
 Insert(APair.Key, APair.Value);
end;

procedure TDictionary<TKey, TValue>.Add(const AKey: TKey; const AValue: TValue);
begin
 { Call insert }
 Insert(AKey, AValue);
end;

procedure TDictionary<TKey, TValue>.Clear;
var
  I, K: NativeInt;
begin
  if FCount > 0 then
    for I := 0 to Length(FBucketArray) - 1 do
      FBucketArray[I] := -1;

  for I := 0 to Length(FEntryArray) - 1 do
  begin
    if FEntryArray[I].FHashCode >= 0 then
    begin
      NotifyKeyRemoved(FEntryArray[I].FKey);
      FEntryArray[I].FKey := default(TKey);

      NotifyValueRemoved(FEntryArray[I].FValue);
      FEntryArray[I].FValue := default(TValue);
    end;
  end;

  FillChar(FEntryArray[0], Length(FEntryArray) * SizeOf(TEntry), 0);

  FFreeList := -1;
  FCount := 0;
  FFreeCount := 0;

  Inc(FVer);
end;

function TDictionary<TKey, TValue>.ContainsKey(const AKey: TKey): Boolean;
begin
  Result := (FindEntry(AKey) >= 0);
end;

function TDictionary<TKey, TValue>.ContainsValue(const AValue: TValue): Boolean;
var
  I: NativeInt;
begin
  Result := False;

  for I := 0 to FCount - 1 do
  begin
    if (FEntryArray[I].FHashCode >= 0) and (ValuesAreEqual(FEntryArray[I].FValue, AValue)) then
       begin Result := True; Exit; end;

  end;
end;

procedure TDictionary<TKey, TValue>.CopyTo(var AArray: array of TPair<TKey, TValue>; const AStartIndex: NativeInt);
var
  I, X: NativeInt;
begin
  { Check for indexes }
  if (AStartIndex >= Length(AArray)) or (AStartIndex < 0) then
    ExceptionHelper.Throw_ArgumentOutOfRangeError('AStartIndex');

  if (Length(AArray) - AStartIndex) < Count then
     ExceptionHelper.Throw_ArgumentOutOfSpaceError('AArray');

  X := AStartIndex;

  for I := 0 to FCount - 1 do
  begin
    if (FEntryArray[I].FHashCode >= 0) then
    begin
       AArray[X].Key := FEntryArray[I].FKey;
       AArray[X].Value := FEntryArray[I].FValue;

       Inc(X);
    end;
  end;
end;

constructor TDictionary<TKey, TValue>.Create;
begin
  Create(TRules<TKey>.Default, TRules<TValue>.Default);
end;

constructor TDictionary<TKey, TValue>.Create(const AInitialCapacity: NativeInt);
begin
  Create(TRules<TKey>.Default, TRules<TValue>.Default, AInitialCapacity);
end;

constructor TDictionary<TKey, TValue>.Create(
  const ACollection: IEnumerable<TPair<TKey, TValue>>);
begin
  Create(TRules<TKey>.Default, TRules<TValue>.Default, ACollection);
end;

constructor TDictionary<TKey, TValue>.Create(
  const AKeyRules: TRules<TKey>;
  const AValueRules: TRules<TValue>; const AInitialCapacity: NativeInt);
begin
  { Call the upper constructor }
  inherited Create(AKeyRules, AValueRules);

  FKeyCollection := TKeyCollection.Create(Self);
  FValueCollection := TValueCollection.Create(Self);

  FVer := 0;
  FCount := 0;
  FFreeCount := 0;
  FFreeList := 0;

  InitializeInternals(AInitialCapacity);
end;

constructor TDictionary<TKey, TValue>.Create(const AKeyRules: TRules<TKey>;
  const AValueRules: TRules<TValue>;
  const ACollection: IEnumerable<TPair<TKey, TValue>>);
var
  LValue: TPair<TKey, TValue>;
begin
  { Call upper constructor }
  Create(AKeyRules, AValueRules, CDefaultSize);

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

constructor TDictionary<TKey, TValue>.Create(
  const AKeyRules: TRules<TKey>;
  const AValueRules: TRules<TValue>);
begin
  { Call upper constructor }
  Create(AKeyRules, AValueRules, CDefaultSize);
end;

destructor TDictionary<TKey, TValue>.Destroy;
begin
  { Clear first }
  Clear();

  inherited;
end;

function TDictionary<TKey, TValue>.FindEntry(const AKey: TKey): NativeInt;
var
  LHashCode, I: NativeInt;
begin
  Result := -1;

  if Length(FBucketArray) > 0 then
  begin
    { Generate the hash code }
    LHashCode := Hash(AKey);

    I := FBucketArray[LHashCode mod Length(FBucketArray)];

    while I >= 0 do
    begin
      if (FEntryArray[I].FHashCode = LHashCode) and KeysAreEqual(FEntryArray[I].FKey, AKey) then
         begin Result := I; Exit; end;

      I := FEntryArray[I].FNext;
    end;
  end;
end;

function TDictionary<TKey, TValue>.GetCount: NativeInt;
begin
  Result := (FCount - FFreeCount);
end;

function TDictionary<TKey, TValue>.GetEnumerator: IEnumerator<TPair<TKey, TValue>>;
begin
  Result := TDictionary<TKey, TValue>.TPairEnumerator.Create(Self);
end;

function TDictionary<TKey, TValue>.GetItem(const AKey: TKey): TValue;
begin
  if not TryGetValue(AKey, Result) then
    ExceptionHelper.Throw_KeyNotFoundError('AKey');
end;

function TDictionary<TKey, TValue>.Hash(const AKey: TKey): NativeInt;
const
  PositiveMask = not NativeInt(1 shl (SizeOf(NativeInt) * 8 - 1));
begin
  Result := PositiveMask and ((PositiveMask and GetKeyHashCode(AKey)) + 1);
end;

procedure TDictionary<TKey, TValue>.InitializeInternals(
  const ACapacity: NativeInt);
var
  I: NativeInt;
begin
  SetLength(FBucketArray, ACapacity);
  SetLength(FEntryArray, ACapacity);

  for I := 0 to ACapacity - 1 do
  begin
    FBucketArray[I] := -1;
    FEntryArray[I].FHashCode := -1;
  end;

  FFreeList := -1;
end;

procedure TDictionary<TKey, TValue>.Insert(const AKey: TKey;
  const AValue: TValue; const ShouldAdd: Boolean);
var
  LFreeList, LIndex,
    LHashCode, I: NativeInt;
begin
  LFreeList := 0;

  if Length(FBucketArray) = 0 then
     InitializeInternals(CDefaultSize);

  { Generate the hash code }
  LHashCode := Hash(AKey);
  LIndex := LHashCode mod Length(FBucketArray);

  I := FBucketArray[LIndex];

  while I >= 0 do
  begin
    if (FEntryArray[I].FHashCode = LHashCode) and KeysAreEqual(FEntryArray[I].FKey, AKey) then
    begin
      if (ShouldAdd) then
        ExceptionHelper.Throw_DuplicateKeyError('AKey');

      ReplaceValue(FEntryArray[I].FValue, AValue);
      Inc(FVer);
      Exit;
    end;

    { Move to next }
    I := FEntryArray[I].FNext;
  end;

  { Adjust free spaces }
  if FFreeCount > 0 then
  begin
    LFreeList := FFreeList;
    FFreeList := FEntryArray[LFreeList].FNext;

    Dec(FFreeCount);
  end else
  begin
    { Adjust LIndex if there is not enough free space }
    if FCount = Length(FEntryArray) then
    begin
      Resize();
      LIndex := LHashCode mod Length(FBucketArray);
    end;

    LFreeList := FCount;
    Inc(FCount);
  end;

  { Insert the element at the right position and adjust arrays }
  FEntryArray[LFreeList].FHashCode := LHashCode;
  FEntryArray[LFreeList].FKey := AKey;
  FEntryArray[LFreeList].FValue := AValue;
  FEntryArray[LFreeList].FNext := FBucketArray[LIndex];

  FBucketArray[LIndex] := LFreeList;
  Inc(FVer);
end;

function TDictionary<TKey, TValue>.KeyHasValue(const AKey: TKey; const AValue: TValue): Boolean;
var
  LValue: TValue;
begin
  Result := TryGetValue(AKey, LValue) and ValuesAreEqual(LValue, AValue);
end;

procedure TDictionary<TKey, TValue>.Remove(const AKey: TKey);
var
  LHashCode, LIndex,
    LRemIndex, I: NativeInt;
begin
  if Length(FBucketArray) > 0 then
  begin
    { Generate the hash code }
    LHashCode := Hash(AKey);

    LIndex := LHashCode mod Length(FBucketArray);
    LRemIndex := -1;

    I := FBucketArray[LIndex];

    while I >= 0 do
    begin
      if (FEntryArray[I].FHashCode = LHashCode) and KeysAreEqual(FEntryArray[I].FKey, AKey) then
      begin

        if LRemIndex < 0 then
          FBucketArray[LIndex] := FEntryArray[I].FNext
        else
          FEntryArray[LRemIndex].FNext := FEntryArray[I].FNext;

        { Cleanup required? }
        NotifyValueRemoved(FEntryArray[I].FValue);

        FEntryArray[I].FHashCode := -1;
        FEntryArray[I].FNext := FFreeList;
        FEntryArray[I].FKey := default(TKey);
        FEntryArray[I].FValue := default(TValue);

        FFreeList := I;
        Inc(FFreeCount);
        Inc(FVer);

        Exit;
      end;

      LRemIndex := I;
      I := FEntryArray[I].FNext;
    end;

  end;
end;

procedure TDictionary<TKey, TValue>.ReplaceValue(var ACurrent: TValue; const ANew: TValue);
begin
  if not ValuesAreEqual(ACurrent, ANew) then
  begin
    { Notify that an element is removed. }
    NotifyValueRemoved(ACurrent);

    { Replace it. }
    ACurrent := ANew;
  end;
end;

procedure TDictionary<TKey, TValue>.Resize;
var
  LNewLength, I, LIndex: NativeInt;
begin
  LNewLength := FCount * 2;

  SetLength(FBucketArray, LNewLength);
  SetLength(FEntryArray, LNewLength);

  for I := 0 to LNewLength - 1 do
    FBucketArray[I] := -1;

  for I := 0 to FCount - 1 do
  begin
    LIndex := FEntryArray[I].FHashCode mod LNewLength;
    FEntryArray[I].FNext := FBucketArray[LIndex];
    FBucketArray[LIndex] := I;
  end;
end;

function TDictionary<TKey, TValue>.SelectKeys: IEnexCollection<TKey>;
begin
  Result := Keys;
end;

function TDictionary<TKey, TValue>.SelectValues: IEnexCollection<TValue>;
begin
  Result := Values;
end;

procedure TDictionary<TKey, TValue>.SetItem(const AKey: TKey; const Value: TValue);
begin
  { Simply call insert }
  Insert(AKey, Value, false);
end;

function TDictionary<TKey, TValue>.TryGetValue(const AKey: TKey; out AFoundValue: TValue): Boolean;
var
  LIndex: NativeInt;
begin
  LIndex := FindEntry(AKey);

  if LIndex >= 0 then
     begin
       AFoundValue := FEntryArray[LIndex].FValue;
       Exit(True);
     end;

  { Key not found, simply fail }
  AFoundValue := Default(TValue);
  Result := False;
end;

function TDictionary<TKey, TValue>.ValueForKey(const AKey: TKey): TValue;
begin
  Result := GetItem(AKey);
end;

constructor TDictionary<TKey, TValue>.Create(
  const AArray: array of TPair<TKey, TValue>);
begin
  Create(TRules<TKey>.Default, TRules<TValue>.Default, AArray);
end;

constructor TDictionary<TKey, TValue>.Create(
  const AKeyRules: TRules<TKey>;
  const AValueRules: TRules<TValue>;
  const AArray: array of TPair<TKey, TValue>);
var
  I: NativeInt;
begin
  { Call upper constructor }
  Create(AKeyRules, AValueRules, CDefaultSize);

  { Copy all items in }
  for I := 0 to Length(AArray) - 1 do
    Add(AArray[I]);
end;

{ TDictionary<TKey, TValue>.TPairEnumerator }

constructor TDictionary<TKey, TValue>.TPairEnumerator.Create(const ADict: TDictionary<TKey, TValue>);
begin
  { Initialize }
  FDict := ADict;
  KeepObjectAlive(FDict);

  FCurrentIndex := 0;
  FVer := ADict.FVer;
end;

destructor TDictionary<TKey, TValue>.TPairEnumerator.Destroy;
begin
  ReleaseObject(FDict);
  inherited;
end;

function TDictionary<TKey, TValue>.TPairEnumerator.GetCurrent: TPair<TKey,TValue>;
begin
  if FVer <> FDict.FVer then
    ExceptionHelper.Throw_CollectionChangedError();

  Result := FValue;
end;

function TDictionary<TKey, TValue>.TPairEnumerator.MoveNext: Boolean;
begin
  if FVer <> FDict.FVer then
     ExceptionHelper.Throw_CollectionChangedError();

  while FCurrentIndex < FDict.FCount do
  begin
    if FDict.FEntryArray[FCurrentIndex].FHashCode >= 0 then
    begin
      FValue.Key := FDict.FEntryArray[FCurrentIndex].FKey;
      FValue.Value := FDict.FEntryArray[FCurrentIndex].FValue;

      Inc(FCurrentIndex);
      Result := True;
      Exit;
    end;

    Inc(FCurrentIndex);
  end;

  FCurrentIndex := FDict.FCount + 1;
  Result := False;
end;

{ TDictionary<TKey, TValue>.TKeyEnumerator }

constructor TDictionary<TKey, TValue>.TKeyEnumerator.Create(const ADict: TDictionary<TKey, TValue>);
begin
  { Initialize }
  FDict := ADict;
  KeepObjectAlive(FDict);
  
  FCurrentIndex := 0;
  FVer := ADict.FVer;
  FValue := default(TKey);
end;

destructor TDictionary<TKey, TValue>.TKeyEnumerator.Destroy;
begin
  ReleaseObject(FDict);
  inherited;
end;

function TDictionary<TKey, TValue>.TKeyEnumerator.GetCurrent: TKey;
begin
  if FVer <> FDict.FVer then
    ExceptionHelper.Throw_CollectionChangedError();

  Result := FValue;
end;

function TDictionary<TKey, TValue>.TKeyEnumerator.MoveNext: Boolean;
begin
  if FVer <> FDict.FVer then
    ExceptionHelper.Throw_CollectionChangedError();

  while FCurrentIndex < FDict.FCount do
  begin
    if FDict.FEntryArray[FCurrentIndex].FHashCode >= 0 then
    begin
      FValue := FDict.FEntryArray[FCurrentIndex].FKey;

      Inc(FCurrentIndex);
      Result := True;
      Exit;
    end;

    Inc(FCurrentIndex);
  end;

  FCurrentIndex := FDict.FCount + 1;
  Result := False;
end;


{ TDictionary<TKey, TValue>.TValueEnumerator }

constructor TDictionary<TKey, TValue>.TValueEnumerator.Create(const ADict: TDictionary<TKey, TValue>);
begin
  { Initialize }
  FDict := ADict;
  KeepObjectAlive(FDict);

  FCurrentIndex := 0;
  FVer := ADict.FVer;
end;

destructor TDictionary<TKey, TValue>.TValueEnumerator.Destroy;
begin
  ReleaseObject(FDict);
  inherited;
end;

function TDictionary<TKey, TValue>.TValueEnumerator.GetCurrent: TValue;
begin
  if FVer <> FDict.FVer then
    ExceptionHelper.Throw_CollectionChangedError();

  Result := FValue;
end;

function TDictionary<TKey, TValue>.TValueEnumerator.MoveNext: Boolean;
begin
  if FVer <> FDict.FVer then
    ExceptionHelper.Throw_CollectionChangedError();

  while FCurrentIndex < FDict.FCount do
  begin
    if FDict.FEntryArray[FCurrentIndex].FHashCode >= 0 then
    begin
      FValue := FDict.FEntryArray[FCurrentIndex].FValue;

      Inc(FCurrentIndex);
      Result := True;
      Exit;
    end;

    Inc(FCurrentIndex);
  end;

  FCurrentIndex := FDict.FCount + 1;
  Result := False;
end;

{ TDictionary<TKey, TValue>.TKeyCollection }

constructor TDictionary<TKey, TValue>.TKeyCollection.Create(const ADict: TDictionary<TKey, TValue>);
begin
  { Call the upper constructor }
  inherited Create(ADict.KeyRules);

  { Initialize }
  FDict := ADict;
end;

function TDictionary<TKey, TValue>.TKeyCollection.GetCount: NativeInt;
begin
  { Number of elements is the same as key }
  Result := FDict.Count;
end;

function TDictionary<TKey, TValue>.TKeyCollection.GetEnumerator: IEnumerator<TKey>;
begin
  Result := TKeyEnumerator.Create(Self.FDict);
end;

procedure TDictionary<TKey, TValue>.TKeyCollection.CopyTo(var AArray: array of TKey; const AStartIndex: NativeInt);
var
  I, X: NativeInt;
begin
  { Check for indexes }
  if (AStartIndex >= Length(AArray)) or (AStartIndex < 0) then
    ExceptionHelper.Throw_ArgumentOutOfRangeError('AStartIndex');

  if (Length(AArray) - AStartIndex) < FDict.Count then
     ExceptionHelper.Throw_ArgumentOutOfSpaceError('AArray');

  X := AStartIndex;

  for I := 0 to FDict.FCount - 1 do
  begin
    if (FDict.FEntryArray[I].FHashCode >= 0) then
    begin
       AArray[X] := FDict.FEntryArray[I].FKey;
       Inc(X);
    end;
  end;
end;

{ TDictionary<TKey, TValue>.TValueCollection }

constructor TDictionary<TKey, TValue>.TValueCollection.Create(const ADict: TDictionary<TKey, TValue>);
begin
  { Call the upper constructor }
  inherited Create(ADict.ValueRules);

  { Initialize }
  FDict := ADict;
end;

function TDictionary<TKey, TValue>.TValueCollection.GetCount: NativeInt;
begin
  { Number of elements is the same as key }
  Result := FDict.Count;
end;

function TDictionary<TKey, TValue>.TValueCollection.GetEnumerator: IEnumerator<TValue>;
begin
  Result := TValueEnumerator.Create(Self.FDict);
end;

procedure TDictionary<TKey, TValue>.TValueCollection.CopyTo(var AArray: array of TValue; const AStartIndex: NativeInt);
var
  I, X: NativeInt;
begin
  if (AStartIndex >= Length(AArray)) or (AStartIndex < 0) then
    ExceptionHelper.Throw_ArgumentOutOfRangeError('AStartIndex');

  { Check for indexes }
  if (Length(AArray) - AStartIndex) < FDict.Count then
     ExceptionHelper.Throw_ArgumentOutOfSpaceError('AArray');

  X := AStartIndex;

  for I := 0 to FDict.FCount - 1 do
  begin
    if (FDict.FEntryArray[I].FHashCode >= 0) then
    begin
       AArray[X] := FDict.FEntryArray[I].FValue;
       Inc(X);
    end;
  end;
end;

{ TObjectDictionary<TKey, TValue> }

procedure TObjectDictionary<TKey, TValue>.HandleKeyRemoved(const AKey: TKey);
begin
  if FOwnsKeys then
    TObject(AKey).Free;
end;

procedure TObjectDictionary<TKey, TValue>.HandleValueRemoved(const AValue: TValue);
begin
  if FOwnsValues then
    TObject(AValue).Free;
end;


procedure TObjectDictionary<TKey, TValue>.ReplaceValue(var ACurrent: TValue; const ANew: TValue);
begin
  { Only act if owns objects is set. Otherwise fallback to default. }
  if (FOwnsValues) and (TObject(ACurrent) <> TObject(ANew)) then
  begin
    NotifyValueRemoved(ACurrent);
    ACurrent := ANew;
  end else
    inherited;
end;

{ TLinkedDictionary<TKey, TValue> }

procedure TLinkedDictionary<TKey, TValue>.Add(const APair: TPair<TKey, TValue>);
begin
 { Call insert }
 Insert(APair.Key, APair.Value);
end;

procedure TLinkedDictionary<TKey, TValue>.Add(const AKey: TKey; const AValue: TValue);
begin
 { Call insert }
 Insert(AKey, AValue);
end;

procedure TLinkedDictionary<TKey, TValue>.Clear;
var
  LEntry, LCurr: PEntry;
begin
  LEntry := FHead;

  while Assigned(LEntry) do
  begin
    NotifyKeyRemoved(LEntry.FKey);
    NotifyValueRemoved(LEntry.FValue);

    { Next and kill }
    LCurr := LEntry;
    LEntry := LEntry^.FNext;

    ReleaseEntry(LCurr);
  end;

  { Clear nodes }
  FHead := nil;
  FTail := nil;

  { Clear array }
  FillChar(FBucketArray[0], Length(FBucketArray) * SizeOf(PEntry), 0);
  FCount := 0;

  Inc(FVer);
end;

function TLinkedDictionary<TKey, TValue>.ContainsKey(const AKey: TKey): Boolean;
begin
  Result := Assigned(FindEntry(AKey));
end;

function TLinkedDictionary<TKey, TValue>.ContainsValue(const AValue: TValue): Boolean;
var
  LEntry: PEntry;
begin
  Result := False;
  LEntry := FHead;

  while Assigned(LEntry) do
  begin
    if ValuesAreEqual(LEntry^.FValue, AValue) then
      Exit(true);

    { Go to next }
    LEntry := LEntry^.FNext;
  end;
end;

procedure TLinkedDictionary<TKey, TValue>.CopyTo(var AArray: array of TPair<TKey, TValue>; const AStartIndex: NativeInt);
var
  I, X: NativeInt;
  LEntry: PEntry;
begin
  { Check for indexes }
  if (AStartIndex >= Length(AArray)) or (AStartIndex < 0) then
    ExceptionHelper.Throw_ArgumentOutOfRangeError('AStartIndex');

  if (Length(AArray) - AStartIndex) < Count then
     ExceptionHelper.Throw_ArgumentOutOfSpaceError('AArray');

  X := AStartIndex;
  LEntry := FHead;

  while Assigned(LEntry) do
  begin
    { Copy it }
    AArray[X].Key := LEntry^.FKey;
    AArray[X].Value := LEntry^.FValue;

    { Go to next }
    Inc(X);
    LEntry := LEntry^.FNext;
  end;
end;

constructor TLinkedDictionary<TKey, TValue>.Create;
begin
  Create(TRules<TKey>.Default, TRules<TValue>.Default);
end;

constructor TLinkedDictionary<TKey, TValue>.Create(const AInitialCapacity: NativeInt);
begin
  Create(TRules<TKey>.Default, TRules<TValue>.Default, AInitialCapacity);
end;

constructor TLinkedDictionary<TKey, TValue>.Create(
  const ACollection: IEnumerable<TPair<TKey, TValue>>);
begin
  Create(TRules<TKey>.Default, TRules<TValue>.Default, ACollection);
end;

constructor TLinkedDictionary<TKey, TValue>.Create(
  const AKeyRules: TRules<TKey>;
  const AValueRules: TRules<TValue>; const AInitialCapacity: NativeInt);
begin
  { Call the upper constructor }
  inherited Create(AKeyRules, AValueRules);

  FKeyCollection := TKeyCollection.Create(Self);
  FValueCollection := TValueCollection.Create(Self);

  FVer := 0;
  FCount := 0;
  FFreeCount := 0;

  FHead := nil;
  FTail := nil;
  FFirstFree := nil;

  if AInitialCapacity <= 0 then
    InitializeInternals(CDefaultSize)
  else
    InitializeInternals(AInitialCapacity)
end;

constructor TLinkedDictionary<TKey, TValue>.Create(const AKeyRules: TRules<TKey>;
  const AValueRules: TRules<TValue>;
  const ACollection: IEnumerable<TPair<TKey, TValue>>);
var
  LValue: TPair<TKey, TValue>;
begin
  { Call upper constructor }
  Create(AKeyRules, AValueRules, CDefaultSize);

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

constructor TLinkedDictionary<TKey, TValue>.Create(
  const AKeyRules: TRules<TKey>;
  const AValueRules: TRules<TValue>);
begin
  { Call upper constructor }
  Create(AKeyRules, AValueRules, CDefaultSize);
end;

destructor TLinkedDictionary<TKey, TValue>.Destroy;
var
  LNext: PEntry;
begin
  { Clear first }
  Clear();

  { Clear the cached entries too }
  if FFreeCount > 0 then
    while Assigned(FFirstFree) do
    begin
      LNext := FFirstFree^.FNext;

      { Delphi doesn't seem to finalize these }
      FFirstFree^.FValue := default(TValue);
      FFirstFree^.FKey := default(TKey);

      FreeMem(FFirstFree);
      FFirstFree := LNext;
    end;

  inherited;
end;

function TLinkedDictionary<TKey, TValue>.FindEntry(const AKey: TKey): PEntry;
var
  LHashCode, LCapacity: NativeInt;
  LEntry: PEntry;
begin
  { Init }
  Result := nil;
  LHashCode := Hash(AKey);
  LCapacity := Length(FBucketArray);
  LEntry := FBucketArray[LHashCode mod LCapacity];

  while Assigned(LEntry) and
    ((LEntry^.FHashCode mod LCapacity) = (LHashCode mod LCapacity)) do
  begin
    { Check the key }
    if KeysAreEqual(LEntry^.FKey, AKey) then
      Exit(LEntry);

    { Go to next }
    LEntry := LEntry^.FNext;
  end;
end;

function TLinkedDictionary<TKey, TValue>.GetCount: NativeInt;
begin
  Result := FCount;
end;

function TLinkedDictionary<TKey, TValue>.GetEnumerator: IEnumerator<TPair<TKey, TValue>>;
begin
  Result := TLinkedDictionary<TKey, TValue>.TPairEnumerator.Create(Self);
end;

function TLinkedDictionary<TKey, TValue>.GetItem(const AKey: TKey): TValue;
begin
  if not TryGetValue(AKey, Result) then
    ExceptionHelper.Throw_KeyNotFoundError('AKey');
end;

function TLinkedDictionary<TKey, TValue>.Hash(const AKey: TKey): NativeInt;
const
  PositiveMask = not NativeInt(1 shl (SizeOf(NativeInt) * 8 - 1));
begin
  Result := PositiveMask and ((PositiveMask and GetKeyHashCode(AKey)) + 1);
end;

procedure TLinkedDictionary<TKey, TValue>.InitializeInternals(const ACapacity: NativeInt);
begin
  { Initialize and clear the dictionary }
  SetLength(FBucketArray, ACapacity);
  FillChar(FBucketArray[0], ACapacity * SizeOf(PEntry), 0);
end;

procedure TLinkedDictionary<TKey, TValue>.Insert(const AKey: TKey; const AValue: TValue; const AShouldAdd: Boolean);
var
  LHashCode, LNewLength, LCapacity: NativeInt;
  LEntry, LNewEntry: PEntry;
begin
  { Initialize stuff }
  LHashCode := Hash(AKey);

  while True do
  begin
    LCapacity := Length(FBucketArray);
    LEntry := FBucketArray[LHashCode mod LCapacity];

    { Case 1: we have a free spot and can insert directly }
    if not Assigned(LEntry) then
    begin
      { Insert the entry }
      LNewEntry := NeedEntry(AKey, AValue, LHashCode);
      LNewEntry^.FPrev := FTail;
      LNewEntry^.FNext := nil;

      if Assigned(FTail) then
        FTail^.FNext := LNewEntry;

      FTail := LNewEntry;

      if not Assigned(FHead) then
        FHead := LNewEntry;

      FBucketArray[LHashCode mod LCapacity] := LNewEntry;

      Inc(FVer);
      Inc(FCount);

      Exit;
    end;

    { Case 2: The spot is filled but capacity is sufficient }
    if FCount < LCapacity then
    begin
      { Search for a place to insert the node into }

      while True do
      begin
        { Check the key }
        if KeysAreEqual(LEntry^.FKey, AKey) then
        begin
          if AShouldAdd then
            ExceptionHelper.Throw_DuplicateKeyError('AKey');

          ReplaceValue(LEntry^.FValue, AValue);

          Inc(FVer);
          Exit;
        end;

        if not Assigned(LEntry^.FNext) or
           ((LEntry^.FNext^.FHashCode mod LCapacity) <> (LHashCode mod LCapacity)) then Break;

        { Go to next }
        LEntry := LEntry^.FNext;
      end;

      { Insert the entry }
      LNewEntry := NeedEntry(AKey, AValue, LHashCode);

      { Get our entry in }
      LNewEntry^.FNext := LEntry^.FNext;
      LNewEntry^.FPrev := LEntry;

      if Assigned(LEntry^.FNext) then
        LEntry^.FNext^.FPrev := LNewEntry;

      LEntry^.FNext := LNewEntry;

      if LEntry = FTail then
        FTail := LNewEntry;

      Inc(FVer);
      Inc(FCount);

      Exit;
    end;

    { Case 3: The spot is filled but capacity is not sufficient }
    if FCount >= LCapacity then
    begin
      { Reset the bucket list }
      LNewLength := FCount * 2;
      SetLength(FBucketArray, LNewLength);
      FillChar(FBucketArray[0], LNewLength * SizeOf(PEntry), 0);

      { Rehash! }
      LEntry := FHead;
      FHead := nil;
      FTail := nil;

      { Rehash the whole list using new capacity }
      while Assigned(LEntry) do
      begin
        LNewEntry := LEntry^.FNext;
        ReInsert(LEntry, LNewLength);

        LEntry := LNewEntry;
      end;
    end;
  end;
end;

function TLinkedDictionary<TKey, TValue>.KeyHasValue(const AKey: TKey; const AValue: TValue): Boolean;
var
  LValue: TValue;
begin
  Result := TryGetValue(AKey, LValue) and ValuesAreEqual(LValue, AValue);
end;

function TLinkedDictionary<TKey, TValue>.NeedEntry(const AKey: TKey;
  const AValue: TValue; const AHash: NativeInt): PEntry;
begin
  if FFreeCount > 0 then
  begin
    Result := FFirstFree;
    FFirstFree := FFirstFree^.FNext;

    Dec(FFreeCount);
  end else
    Result := AllocMem(SizeOf(TEntry));

  { Initialize the node }
  Result^.FHashCode := AHash;
  Result^.FKey := AKey;
  Result^.FValue := AValue;
end;

procedure TLinkedDictionary<TKey, TValue>.ReInsert(const AEntry: PEntry; const ACapacity: NativeInt);
var
  LEntry: PEntry;
begin
  { Initialize stuff }
  LEntry := FBucketArray[AEntry^.FHashCode mod ACapacity];

  { Case 1: we have a free spot and can insert directly }
  if not Assigned(LEntry) then
  begin
    AEntry^.FPrev := FTail;
    AEntry^.FNext := nil;

    if Assigned(FTail) then
      FTail^.FNext := AEntry;

    FTail := AEntry;

    if not Assigned(FHead) then
      FHead := AEntry;

    FBucketArray[AEntry^.FHashCode mod ACapacity] := AEntry;

    Exit;
  end;

  { Case 2: The spot is filled but capacity is sufficient }
  while True do
  begin
    { Check the key }
    ASSERT(not KeysAreEqual(LEntry^.FKey, AEntry^.FKey));

    if not Assigned(LEntry^.FNext) or
       ((LEntry^.FNext^.FHashCode mod ACapacity) <> (AEntry^.FHashCode mod ACapacity)) then Break;

    { Go to next }
    LEntry := LEntry^.FNext;
  end;

  { Get our entry in }
  AEntry^.FNext := LEntry^.FNext;
  AEntry^.FPrev := LEntry;

  if Assigned(LEntry^.FNext) then
    LEntry^.FNext^.FPrev := AEntry;

  LEntry^.FNext := AEntry;

  if LEntry = FTail then
    FTail := AEntry;
end;

procedure TLinkedDictionary<TKey, TValue>.ReleaseEntry(const AEntry: PEntry);
begin
  if FFreeCount = CDefaultSize then
  begin
    { Delphi doesn't seem to finalize these }
    AEntry^.FValue := default(TValue);
    AEntry^.FKey := default(TKey);

    FreeMem(AEntry);
  end else begin
    { Place the entry into the cache }
    AEntry^.FNext := FFirstFree;
    FFirstFree := AEntry;

    Inc(FFreeCount);
  end;
end;

procedure TLinkedDictionary<TKey, TValue>.Remove(const AKey: TKey);
var
  LHashCode, LCapacity: NativeInt;
  LEntry: PEntry;
begin
  { Generate the hash code }
  LHashCode := Hash(AKey);
  LCapacity := Length(FBucketArray);
  LEntry := FBucketArray[LHashCode mod LCapacity];

  while Assigned(LEntry) and
       ((LEntry^.FHashCode mod LCapacity) = (LHashCode mod LCapacity)) do
  begin
    { Check the key }
    if KeysAreEqual(LEntry^.FKey, AKey) then
    begin
      { Remove self from the linked list }
      if Assigned(LEntry^.FPrev) then
        LEntry^.FPrev^.FNext := LEntry^.FNext;

      if Assigned(LEntry^.FNext) then
        LEntry^.FNext^.FPrev := LEntry^.FPrev;

      if LEntry = FBucketArray[LHashCode mod LCapacity] then
      begin
        { This entry is the first for the given hash code. Set the next if it has same hash. }
        if Assigned(LEntry^.FNext) and ((LEntry^.FNext^.FHashCode mod LCapacity) = (LEntry^.FHashCode mod LCapacity)) then
          FBucketArray[LHashCode mod LCapacity] := LEntry^.FNext
        else
          FBucketArray[LHashCode mod LCapacity] := nil;
      end;

      if FTail = LEntry then
        FTail := LEntry^.FPrev;

      if FHead = LEntry then
        FHead := LEntry^.FNext;


      { Kill this entry }
      NotifyValueRemoved(LEntry^.FValue);
      ReleaseEntry(LEntry);
      Dec(FCount);
      Inc(FVer);

      { All done, let's exit }
      Exit;
    end;

    { Go to next }
    LEntry := LEntry^.FNext;
  end;
end;

procedure TLinkedDictionary<TKey, TValue>.ReplaceValue(var ACurrent: TValue; const ANew: TValue);
begin
  if not ValuesAreEqual(ACurrent, ANew) then
  begin
    { Notify that an element is removed. }
    NotifyValueRemoved(ACurrent);

    { Replace it. }
    ACurrent := ANew;
  end;
end;

function TLinkedDictionary<TKey, TValue>.SelectKeys: IEnexCollection<TKey>;
begin
  Result := Keys;
end;

function TLinkedDictionary<TKey, TValue>.SelectValues: IEnexCollection<TValue>;
begin
  Result := Values;
end;

procedure TLinkedDictionary<TKey, TValue>.SetItem(const AKey: TKey; const Value: TValue);
begin
  { Simply call insert }
  Insert(AKey, Value, false);
end;

function TLinkedDictionary<TKey, TValue>.TryGetValue(const AKey: TKey; out AFoundValue: TValue): Boolean;
var
  LEntry: PEntry;
begin
  LEntry := FindEntry(AKey);

  if Assigned(LEntry) then
  begin
    AFoundValue := LEntry^.FValue;
    Exit(True);
  end;

  { Key not found, simply fail }
  AFoundValue := default(TValue);
  Result := False;
end;

function TLinkedDictionary<TKey, TValue>.ValueForKey(const AKey: TKey): TValue;
begin
  Result := GetItem(AKey);
end;

constructor TLinkedDictionary<TKey, TValue>.Create(
  const AArray: array of TPair<TKey, TValue>);
begin
  Create(TRules<TKey>.Default, TRules<TValue>.Default, AArray);
end;

constructor TLinkedDictionary<TKey, TValue>.Create(
  const AKeyRules: TRules<TKey>;
  const AValueRules: TRules<TValue>;
  const AArray: array of TPair<TKey, TValue>);
var
  I: NativeInt;
begin
  { Call upper constructor }
  Create(AKeyRules, AValueRules, CDefaultSize);

  { Copy all items in }
  for I := 0 to Length(AArray) - 1 do
    Add(AArray[I]);
end;

{ TLinkedDictionary<TKey, TValue>.TPairEnumerator }

constructor TLinkedDictionary<TKey, TValue>.TPairEnumerator.Create(const ADict: TLinkedDictionary<TKey, TValue>);
begin
  { Initialize }
  FDict := ADict;
  KeepObjectAlive(FDict);
  FCurrentEntry := FDict.FHead;
  FVer := ADict.FVer;
end;

destructor TLinkedDictionary<TKey, TValue>.TPairEnumerator.Destroy;
begin
  ReleaseObject(FDict);
  inherited;
end;

function TLinkedDictionary<TKey, TValue>.TPairEnumerator.GetCurrent: TPair<TKey,TValue>;
begin
  if FVer <> FDict.FVer then
    ExceptionHelper.Throw_CollectionChangedError();

  Result := FValue;
end;

function TLinkedDictionary<TKey, TValue>.TPairEnumerator.MoveNext: Boolean;
begin
  if FVer <> FDict.FVer then
     ExceptionHelper.Throw_CollectionChangedError();

  Result := Assigned(FCurrentEntry);

  if Result then
  begin
    FValue.Key := FCurrentEntry^.FKey;
    FValue.Value := FCurrentEntry^.FValue;

    FCurrentEntry := FCurrentEntry^.FNext;
  end;
end;

{ TLinkedDictionary<TKey, TValue>.TKeyEnumerator }

constructor TLinkedDictionary<TKey, TValue>.TKeyEnumerator.Create(const ADict: TLinkedDictionary<TKey, TValue>);
begin
  { Initialize }
  FDict := ADict;
  KeepObjectAlive(FDict);
  FCurrentEntry := FDict.FHead;
  FVer := ADict.FVer;
end;

destructor TLinkedDictionary<TKey, TValue>.TKeyEnumerator.Destroy;
begin
  ReleaseObject(FDict);
  inherited;
end;

function TLinkedDictionary<TKey, TValue>.TKeyEnumerator.GetCurrent: TKey;
begin
  if FVer <> FDict.FVer then
    ExceptionHelper.Throw_CollectionChangedError();

  Result := FValue;
end;

function TLinkedDictionary<TKey, TValue>.TKeyEnumerator.MoveNext: Boolean;
begin
  if FVer <> FDict.FVer then
    ExceptionHelper.Throw_CollectionChangedError();

  Result := Assigned(FCurrentEntry);

  if Result then
  begin
    FValue := FCurrentEntry^.FKey;
    FCurrentEntry := FCurrentEntry^.FNext;
  end;
end;

{ TLinkedDictionary<TKey, TValue>.TValueEnumerator }

constructor TLinkedDictionary<TKey, TValue>.TValueEnumerator.Create(const ADict: TLinkedDictionary<TKey, TValue>);
begin
  { Initialize }
  FDict := ADict;
  KeepObjectAlive(FDict);
  FCurrentEntry := FDict.FHead;
  FVer := ADict.FVer;
end;

destructor TLinkedDictionary<TKey, TValue>.TValueEnumerator.Destroy;
begin
  ReleaseObject(FDict);
  inherited;
end;

function TLinkedDictionary<TKey, TValue>.TValueEnumerator.GetCurrent: TValue;
begin
  if FVer <> FDict.FVer then
    ExceptionHelper.Throw_CollectionChangedError();

  Result := FValue;
end;

function TLinkedDictionary<TKey, TValue>.TValueEnumerator.MoveNext: Boolean;
begin
  if FVer <> FDict.FVer then
    ExceptionHelper.Throw_CollectionChangedError();

  Result := Assigned(FCurrentEntry);

  if Result then
  begin
    FValue := FCurrentEntry^.FValue;
    FCurrentEntry := FCurrentEntry^.FNext;
  end;
end;

{ TLinkedDictionary<TKey, TValue>.TKeyCollection }

constructor TLinkedDictionary<TKey, TValue>.TKeyCollection.Create(const ADict: TLinkedDictionary<TKey, TValue>);
begin
  { Call the upper constructor }
  inherited Create(ADict.KeyRules);

  { Initialize }
  FDict := ADict;
end;

function TLinkedDictionary<TKey, TValue>.TKeyCollection.GetCount: NativeInt;
begin
  { Number of elements is the same as key }
  Result := FDict.Count;
end;

function TLinkedDictionary<TKey, TValue>.TKeyCollection.GetEnumerator: IEnumerator<TKey>;
begin
  Result := TKeyEnumerator.Create(Self.FDict);
end;

procedure TLinkedDictionary<TKey, TValue>.TKeyCollection.CopyTo(var AArray: array of TKey; const AStartIndex: NativeInt);
var
  X: NativeInt;
  LEntry: PEntry;
begin
  { Check for indexes }
  if (AStartIndex >= Length(AArray)) or (AStartIndex < 0) then
    ExceptionHelper.Throw_ArgumentOutOfRangeError('AStartIndex');

  if (Length(AArray) - AStartIndex) < FDict.Count then
     ExceptionHelper.Throw_ArgumentOutOfSpaceError('AArray');

  X := AStartIndex;
  LEntry := FDict.FHead;

  while Assigned(LEntry) do
  begin
    { Copy it }
    AArray[X] := LEntry^.FKey;

    { Go to next }
    Inc(X);
    LEntry := LEntry^.FNext;
  end;
end;

{ TLinkedDictionary<TKey, TValue>.TValueCollection }

constructor TLinkedDictionary<TKey, TValue>.TValueCollection.Create(const ADict: TLinkedDictionary<TKey, TValue>);
begin
  { Call the upper constructor }
  inherited Create(ADict.ValueRules);

  { Initialize }
  FDict := ADict;
end;

function TLinkedDictionary<TKey, TValue>.TValueCollection.GetCount: NativeInt;
begin
  { Number of elements is the same as key }
  Result := FDict.Count;
end;

function TLinkedDictionary<TKey, TValue>.TValueCollection.GetEnumerator: IEnumerator<TValue>;
begin
  Result := TValueEnumerator.Create(Self.FDict);
end;

procedure TLinkedDictionary<TKey, TValue>.TValueCollection.CopyTo(var AArray: array of TValue; const AStartIndex: NativeInt);
var
  X: NativeInt;
  LEntry: PEntry;
begin
  if (AStartIndex >= Length(AArray)) or (AStartIndex < 0) then
    ExceptionHelper.Throw_ArgumentOutOfRangeError('AStartIndex');

  { Check for indexes }
  if (Length(AArray) - AStartIndex) < FDict.Count then
     ExceptionHelper.Throw_ArgumentOutOfSpaceError('AArray');

  X := AStartIndex;
  LEntry := FDict.FHead;

  while Assigned(LEntry) do
  begin
    { Copy it }
    AArray[X] := LEntry^.FValue;

    { Go to next }
    Inc(X);
    LEntry := LEntry^.FNext;
  end;
end;

{ TObjectLinkedDictionary<TKey, TValue> }

procedure TObjectLinkedDictionary<TKey, TValue>.HandleKeyRemoved(const AKey: TKey);
begin
  if FOwnsKeys then
    TObject(AKey).Free;
end;

procedure TObjectLinkedDictionary<TKey, TValue>.HandleValueRemoved(const AValue: TValue);
begin
  if FOwnsValues then
    TObject(AValue).Free;
end;

procedure TObjectLinkedDictionary<TKey, TValue>.ReplaceValue(var ACurrent: TValue; const ANew: TValue);
begin
  { Only act if owns objects is set. Otherwise fallback to default. }
  if (FOwnsValues) and (TObject(ACurrent) <> TObject(ANew)) then
  begin
    NotifyValueRemoved(ACurrent);
    ACurrent := ANew;
  end else
    inherited;
end;

{ TSortedDictionary<TKey, TValue> }

procedure TSortedDictionary<TKey, TValue>.Add(const APair: TPair<TKey, TValue>);
begin
  { Insert the pair }
  if not Insert(APair.Key, APair.Value, false) then
    ExceptionHelper.Throw_DuplicateKeyError('AKey');
end;

procedure TSortedDictionary<TKey, TValue>.Add(const AKey: TKey; const AValue: TValue);
begin
  { Insert the pair }
  if not Insert(AKey, AValue, false) then
    ExceptionHelper.Throw_DuplicateKeyError('AKey');
end;

procedure TSortedDictionary<TKey, TValue>.BalanceTreesAfterRemoval(const ANode: TNode);
var
  LCurrentAct: TBalanceAct;
  LLNode, LXNode, LSNode,
    LWNode, LYNode: TNode;
begin
  { Initiliaze ... }
  LCurrentAct := TBalanceAct.baStart;
  LLNode := ANode;

  { Continue looping until end is declared }
  while LCurrentAct <> TBalanceAct.baEnd do
  begin
    case LCurrentAct of

      { START MODE }
      TBalanceAct.baStart:
      begin
        if not Assigned(LLNode.FRight) then
        begin
          { Exclude myself! }
          if Assigned(LLNode.FLeft) then
            LLNode.FLeft.FParent := LLNode.FParent;

          { I'm root! nothing to do here }
          if not Assigned(LLNode.FParent) then
          begin
            FRoot := LLNode.FLeft;

            { DONE! }
            LCurrentAct := TBalanceAct.baEnd;
            continue;
          end;

          { ... }
          if LLNode = LLNode.FParent.FLeft then
          begin
            LLNode.FParent.FLeft := LLNode.FLeft;
            LYNode := LLNode.FParent;
          end else
          begin
            LLNode.FParent.FRight := LLNode.FLeft;
            LYNode := LLNode.FParent;

            { RIGHT! }
            LCurrentAct := TBalanceAct.baRight;
            continue;
          end;
        end else if not Assigned(LLNode.FRight.FLeft) then
        begin
          { Case 1, RIGHT, NO LEFT }
          if Assigned(LLNode.FLeft) then
          begin
            LLNode.FLeft.FParent := LLNode.FRight;
            LLNode.FRight.FLeft := LLNode.FLeft;
          end;

          LLNode.FRight.FBalance := LLNode.FBalance;
          LLNode.FRight.FParent := LLNode.FParent;

          if not Assigned(LLNode.FParent) then
            FRoot := LLNode.FRight
          else
          begin
            if LLNode = LLNode.FParent.FLeft then
              LLNode.FParent.FLeft := LLNode.FRight
            else
              LLNode.FParent.FRight := LLNode.FRight;
          end;

          LYNode := LLNode.FRight;

          { RIGHT! }
          LCurrentAct := TBalanceAct.baRight;
          continue;
        end else
        begin
          { Case 3: RIGHT+LEFT }
          LSNode := LLNode.FRight.FLeft;

          while Assigned(LSNode.FLeft) do
            LSNode := LSNode.FLeft;

          if Assigned(LLNode.FLeft) then
          begin
            LLNode.FLeft.FParent := LSNode;
            LSNode.FLeft := LLNode.FLeft;
          end;

          LSNode.FParent.FLeft := LSNode.FRight;

          if Assigned(LSNode.FRight) then
            LSNode.FRight.FParent := LSNode.FParent;

          LLNode.FRight.FParent := LSNode;
          LSNode.FRight := LLNode.FRight;

          LYNode := LSNode.FParent;

          LSNode.FBalance := LLNode.FBalance;
          LSNode.FParent := LLNode.FParent;

          if not Assigned(LLNode.FParent) then
            FRoot := LSNode
          else
          begin
            if LLNode = LLNode.FParent.FLeft then
              LLNode.FParent.FLeft := LSNode
            else
              LLNode.FParent.FRight := LSNode;
          end;
        end;

        { LEFT! }
        LCurrentAct := TBalanceAct.baLeft;
        continue;
      end; { baStart }

      { LEFT BALANCING MODE }
      TBalanceAct.baLeft:
      begin
        Inc(LYNode.FBalance);

        if LYNode.FBalance = 1 then
        begin
          { DONE! }
          LCurrentAct := TBalanceAct.baEnd;
          continue;
        end
        else if LYNode.FBalance = 2 then
        begin
          LXNode := LYNode.FRight;

          if LXNode.FBalance = -1 then
          begin
            LWNode := LXNode.FLeft;
            LWNode.FParent := LYNode.FParent;

            if not Assigned(LYNode.FParent) then
              FRoot := LWNode
            else
            begin
              if LYNode.FParent.FLeft = LYNode then
                LYNode.FParent.FLeft := LWNode
              else
                LYNode.FParent.FRight := LWNode;
            end;

            LXNode.FLeft := LWNode.FRight;

            if Assigned(LXNode.FLeft) then
              LXNode.FLeft.FParent := LXNode;

            LYNode.FRight := LWNode.FLeft;

            if Assigned(LYNode.FRight) then
              LYNode.FRight.FParent := LYNode;

            LWNode.FRight := LXNode;
            LWNode.FLeft := LYNode;

            LXNode.FParent := LWNode;
            LYNode.FParent := LWNode;

            if LWNode.FBalance = 1 then
            begin
              LXNode.FBalance := 0;
              LYNode.FBalance := -1;
            end else if LWNode.FBalance = 0 then
            begin
              LXNode.FBalance := 0;
              LYNode.FBalance := 0;
            end else
            begin
              LXNode.FBalance := 1;
              LYNode.FBalance := 0;
            end;

            LWNode.FBalance := 0;
            LYNode := LWNode;
          end else
          begin
            LXNode.FParent := LYNode.FParent;

            if Assigned(LYNode.FParent) then
            begin
              if LYNode.FParent.FLeft = LYNode then
                LYNode.FParent.FLeft := LXNode
              else
                LYNode.FParent.FRight := LXNode;
            end else
              FRoot := LXNode;

            LYNode.FRight := LXNode.FLeft;

            if Assigned(LYNode.FRight) then
              LYNode.FRight.FParent := LYNode;

            LXNode.FLeft := LYNode;
            LYNode.FParent := LXNode;

            if LXNode.FBalance = 0 then
            begin
              LXNode.FBalance := -1;
              LYNode.FBalance := 1;

              { DONE! }
              LCurrentAct := TBalanceAct.baEnd;
              continue;
            end else
            begin
              LXNode.FBalance := 0;
              LYNode.FBalance := 0;

              LYNode := LXNode;
            end;
          end;
        end;

        { LOOP! }
        LCurrentAct := TBalanceAct.baLoop;
        continue;
      end; { baLeft }

      { RIGHT BALANCING MODE }
      TBalanceAct.baRight:
      begin
        Dec(LYNode.FBalance);

        if LYNode.FBalance = -1 then
        begin
          { DONE! }
          LCurrentAct := TBalanceAct.baEnd;
          continue;
        end
        else if LYNode.FBalance = -2 then
        begin
          LXNode := LYNode.FLeft;

          if LXNode.FBalance = 1 then
          begin
            LWNode := LXNode.FRight;
            LWNode.FParent := LYNode.FParent;

            if not Assigned(LYNode.FParent) then
              FRoot := LWNode
            else
            begin
              if LYNode.FParent.FLeft = LYNode then
                LYNode.FParent.FLeft := LWNode
              else
                LYNode.FParent.FRight := LWNode;
            end;

            LXNode.FRight := LWNode.FLeft;

            if Assigned(LXNode.FRight) then
              LXNode.FRight.FParent := LXNode;

            LYNode.FLeft := LWNode.FRight;

            if Assigned(LYNode.FLeft) then
              LYNode.FLeft.FParent := LYNode;

            LWNode.FLeft := LXNode;
            LWNode.FRight := LYNode;

            LXNode.FParent := LWNode;
            LYNode.FParent := LWNode;

            if LWNode.FBalance = -1 then
            begin
              LXNode.FBalance := 0;
              LYNode.FBalance := 1;
            end else if LWNode.FBalance = 0 then
            begin
              LXNode.FBalance := 0;
              LYNode.FBalance := 0;
            end else
            begin
              LXNode.FBalance := -1;
              LYNode.FBalance := 0;
            end;

            LWNode.FBalance := 0;
            LYNode := LWNode;
          end else
          begin
            LXNode.FParent := LYNode.FParent;

            if Assigned(LYNode.FParent) then
            begin
              if LYNode.FParent.FLeft = LYNode then
                LYNode.FParent.FLeft := LXNode
              else
                LYNode.FParent.FRight := LXNode
            end else
              FRoot := LXNode;

            LYNode.FLeft := LXNode.FRight;

            if Assigned(LYNode.FLeft) then
              LYNode.FLeft.FParent := LYNode;

            LXNode.FRight := LYNode;
            LYNode.FParent := LXNode;

            if LXNode.FBalance = 0 then
            begin
              LXNode.FBalance := 1;
              LYNode.FBalance := -1;

              { END! }
              LCurrentAct := TBalanceAct.baEnd;
              continue;
            end else
            begin
              LXNode.FBalance := 0;
              LYNode.FBalance := 0;

              LYNode := LXNode;
            end;
          end;
        end;

        { LOOP! }
        LCurrentAct := TBalanceAct.baLoop;
        continue;
      end; { baRight }

      TBalanceAct.baLoop:
      begin
        { Verify continuation }
        if Assigned(LYNode.FParent) then
        begin
          if LYNode = LYNode.FParent.FLeft then
          begin
            LYNode := LYNode.FParent;

            { LEFT! }
            LCurrentAct := TBalanceAct.baLeft;
            continue;
          end;

          LYNode := LYNode.FParent;

          { RIGHT! }
          LCurrentAct := TBalanceAct.baRight;
          continue;
        end;

        { END! }
        LCurrentAct := TBalanceAct.baEnd;
        continue;
      end;
    end; { Case }
  end; { While }
end;

procedure TSortedDictionary<TKey, TValue>.Clear;
begin
  if Assigned(FRoot) then
  begin
    RecursiveClear(FRoot);
    FRoot := nil;

    { Update markers }
    Inc(FVer);
    FCount := 0;
  end;
end;

function TSortedDictionary<TKey, TValue>.ContainsKey(const AKey: TKey): Boolean;
begin
  Result := Assigned(FindNodeWithKey(AKey));
end;

function TSortedDictionary<TKey, TValue>.ContainsValue(const AValue: TValue): Boolean;
var
  LNode: TNode;
begin
  { Find the left-most node }
  LNode := FindLeftMostNode();

  while Assigned(LNode) do
  begin
    { Verify existance }
    if ValuesAreEqual(LNode.FValue, AValue) then
      Exit(true);

    { Navigate further in the tree }
    LNode := WalkToTheRight(LNode);
  end;

  Exit(false);
end;

procedure TSortedDictionary<TKey, TValue>.CopyTo(var AArray: array of TPair<TKey, TValue>; const AStartIndex: NativeInt);
var
  X: NativeInt;
  LNode: TNode;
begin
  { Check for indexes }
  if (AStartIndex >= Length(AArray)) or (AStartIndex < 0) then
    ExceptionHelper.Throw_ArgumentOutOfRangeError('AStartIndex');

  if (Length(AArray) - AStartIndex) < FCount then
     ExceptionHelper.Throw_ArgumentOutOfSpaceError('AArray');

  X := AStartIndex;

  { Find the left-most node }
  LNode := FindLeftMostNode();

  while Assigned(LNode) do
  begin
    { Get the key }
    AArray[X].Key := LNode.FKey;
    AArray[X].Value := LNode.FValue;

    { Navigate further in the tree }
    LNode := WalkToTheRight(LNode);

    { Increment the index }
    Inc(X);
  end;
end;

constructor TSortedDictionary<TKey, TValue>.Create(const AAscending: Boolean);
begin
  Create(TRules<TKey>.Default, TRules<TValue>.Default, AAscending);
end;

constructor TSortedDictionary<TKey, TValue>.Create(const ACollection: IEnumerable<TPair<TKey, TValue>>;
  const AAscending: Boolean);
begin
  Create(TRules<TKey>.Default, TRules<TValue>.Default, ACollection, AAscending);
end;

constructor TSortedDictionary<TKey, TValue>.Create(const AKeyRules: TRules<TKey>; const AValueRules: TRules<TValue>;
  const ACollection: IEnumerable<TPair<TKey, TValue>>; const AAscending: Boolean);
var
  LValue: TPair<TKey, TValue>;
begin
  { Call upper constructor }
  Create(AKeyRules, AValueRules, AAscending);

  if not Assigned(ACollection) then
     ExceptionHelper.Throw_ArgumentNilError('ACollection');

  { Pump in all items }
  for LValue in ACollection do
  begin
{$IFNDEF BUG_GENERIC_INCOMPAT_TYPES}
    Add(LValue);
{$ELSE}
    Add(LValue.Key, LValue.Value);
{$ENDIF}
  end;
end;

constructor TSortedDictionary<TKey, TValue>.Create(const AKeyRules: TRules<TKey>;
  const AValueRules: TRules<TValue>; const AAscending: Boolean);
begin
  { Call the upper constructor }
  inherited Create(AKeyRules, AValueRules);

  FKeyCollection := TKeyCollection.Create(Self);
  FValueCollection := TValueCollection.Create(Self);

  FVer := 0;
  FCount := 0;

  if AAscending then
    FSignFix := 1
  else
    FSignFix := -1;
end;

destructor TSortedDictionary<TKey, TValue>.Destroy;
begin
  { Clear first }
  Clear();

  inherited;
end;

function TSortedDictionary<TKey, TValue>.FindLeftMostNode: TNode;
begin
  { Start with root }
  Result := FRoot;

  { And go to maximum left }
  if Assigned(Result) then
  begin
    while Assigned(Result.FLeft) do
      Result := Result.FLeft;
  end;
end;

function TSortedDictionary<TKey, TValue>.FindNodeWithKey(const AKey: TKey): TNode;
var
  LNode: TNode;
  LCompareResult: NativeInt;
begin
  { Get root }
  LNode := FRoot;

  while Assigned(LNode) do
  begin
	  LCompareResult := CompareKeys(AKey, LNode.FKey) * FSignFix;

    { Navigate left, right or find! }
    if LCompareResult < 0 then
      LNode := LNode.FLeft
    else if LCompareResult > 0 then
      LNode := LNode.FRight
    else
      Exit(LNode);
  end;

  { Did not find anything ... }
  Result := nil;
end;

function TSortedDictionary<TKey, TValue>.FindRightMostNode: TNode;
begin
  { Start with root }
  Result := FRoot;

  { And go to maximum left }
  if Assigned(Result) then
  begin
    while Assigned(Result.FRight) do
      Result := Result.FRight;
  end;
end;

function TSortedDictionary<TKey, TValue>.GetCount: NativeInt;
begin
  Result := FCount;
end;

function TSortedDictionary<TKey, TValue>.GetEnumerator: IEnumerator<TPair<TKey, TValue>>;
begin
  Result := TPairEnumerator.Create(Self);
end;

function TSortedDictionary<TKey, TValue>.GetItem(const AKey: TKey): TValue;
begin
  if not TryGetValue(AKey, Result) then
    ExceptionHelper.Throw_KeyNotFoundError('AKey');
end;

function TSortedDictionary<TKey, TValue>.Insert(const AKey: TKey; const AValue: TValue; const ChangeOrFail: Boolean): Boolean;
var
  LNode: TNode;
  LCompareResult: NativeInt;
begin
  { First one get special treatment! }
  if not Assigned(FRoot) then
  begin
    FRoot := MakeNode(AKey, AValue, nil);

    { Increase markers }
    Inc(FCount);
    Inc(FVer);

    { [ADDED NEW] Exit function }
    Exit(true);
  end;

  { Get root }
  LNode := FRoot;

  while true do
  begin
	  LCompareResult := CompareKeys(AKey, LNode.FKey) * FSignFix;

    if LCompareResult < 0 then
    begin
      if Assigned(LNode.FLeft) then
        LNode := LNode.FLeft
      else
      begin
        { Create  new node }
        LNode.FLeft := MakeNode(AKey, AValue, LNode);
        Dec(LNode.FBalance);

        { [ADDED NEW] Exit function! }
        break;
      end;
    end else if LCompareResult > 0 then
    begin
      if Assigned(LNode.FRight) then
        LNode := LNode.FRight
      else
      begin
        LNode.FRight := MakeNode(AKey, AValue, LNode);
        Inc(LNode.FBalance);

        { [ADDED NEW] Exit function! }
        break;
      end;
    end else
    begin
      { Found  node with the same AKey. Check what to do next }
      if not ChangeOrFail then
        Exit(false);

      ReplaceValue(LNode.FValue, AValue);

      { Increase markers }
      Inc(FVer);

      { [CHANGED OLD] Exit function }
      Exit(true);
    end;
  end;

  { Rebalance the tree }
  ReBalanceSubTreeOnInsert(LNode);

  Inc(FCount);
  Inc(FVer);

  Result := true;
end;

function TSortedDictionary<TKey, TValue>.KeyHasValue(const AKey: TKey; const AValue: TValue): Boolean;
var
  LValue: TValue;
begin
  Result := TryGetValue(AKey, LValue) and ValuesAreEqual(LValue, AValue);
end;

function TSortedDictionary<TKey, TValue>.MakeNode(const AKey: TKey; const AValue: TValue; const ARoot: TNode): TNode;
begin
  Result := TNode.Create();
  Result.FKey := AKey;
  Result.FValue := AValue;
  Result.FParent := ARoot;
end;

function TSortedDictionary<TKey, TValue>.MaxKey: TKey;
begin
  { Check there are elements in the set }
  if not Assigned(FRoot) then
    ExceptionHelper.Throw_CollectionEmptyError();

  if FSignFix = 1 then
    Result := FindRightMostNode().FKey
  else
    Result := FindLeftMostNode().FKey;
end;

function TSortedDictionary<TKey, TValue>.MinKey: TKey;
begin
  { Check there are elements in the set }
  if not Assigned(FRoot) then
    ExceptionHelper.Throw_CollectionEmptyError();

  if FSignFix = 1 then
    Result := FindLeftMostNode().FKey
  else
    Result := FindRightMostNode().FKey;
end;

procedure TSortedDictionary<TKey, TValue>.ReBalanceSubTreeOnInsert(const ANode: TNode);
var
  LLNode, LXNode, LWNode: TNode;
begin
  LLNode := ANode;

  { Re-balancing the tree! }
  while (LLNode.FBalance <> 0) and Assigned(LLNode.FParent) do
  begin
    if (LLNode.FParent.FLeft = LLNode) then
      Dec(LLNode.FParent.FBalance)
    else
      Inc(LLNode.FParent.FBalance);

    { Move up }
    LLNode := LLNode.FParent;

    if (LLNode.FBalance = -2) then
    begin
      LXNode := LLNode.FLeft;

      if (LXNode.FBalance = -1) then
      begin
        LXNode.FParent := LLNode.FParent;

        if not Assigned(LLNode.FParent) then
          FRoot := LXNode
        else
        begin
          if (LLNode.FParent.FLeft = LLNode) then
            LLNode.FParent.FLeft := LXNode
          else
            LLNode.FParent.FRight := LXNode;
        end;

        LLNode.FLeft := LXNode.FRight;

        if Assigned(LLNode.FLeft) then
          LLNode.FLeft.FParent := LLNode;

        LXNode.FRight := LLNode;
        LLNode.FParent := LXNode;

        LXNode.FBalance := 0;
        LLNode.FBalance := 0;
      end else
      begin
        LWNode := LXNode.FRight;
        LWNode.FParent := LLNode.FParent;

        if not Assigned(LLNode.FParent) then
          FRoot := LWNode
        else
        begin
          if LLNode.FParent.FLeft = LLNode then
            LLNode.FParent.FLeft := LWNode
          else
            LLNode.FParent.FRight := LWNode;
        end;

        LXNode.FRight := LWNode.FLeft;

        if Assigned(LXNode.FRight) then
          LXNode.FRight.FParent := LXNode;

        LLNode.FLeft := LWNode.FRight;

        if Assigned(LLNode.FLeft) then
          LLNode.FLeft.FParent := LLNode;

        LWNode.FLeft := LXNode;
        LWNode.FRight := LLNode;

        LXNode.FParent := LWNode;
        LLNode.FParent := LWNode;

        { Apply proper balancing }
        if LWNode.FBalance = -1 then
        begin
          LXNode.FBalance := 0;
          LLNode.FBalance := 1;
        end else if LWNode.FBalance = 0 then
        begin
          LXNode.FBalance := 0;
          LLNode.FBalance := 0;
        end else
        begin
          LXNode.FBalance := -1;
          LLNode.FBalance := 0;
        end;

        LWNode.FBalance := 0;
      end;

      break;
    end else if LLNode.FBalance = 2 then
    begin
      LXNode := LLNode.FRight;

      if LXNode.FBalance = 1 then
      begin
        LXNode.FParent := LLNode.FParent;

        if not Assigned(LLNode.FParent) then
          FRoot := LXNode
        else
        begin
          if LLNode.FParent.FLeft = LLNode then
            LLNode.FParent.FLeft := LXNode
          else
            LLNode.FParent.FRight := LXNode;
        end;

        LLNode.FRight := LXNode.FLeft;

        if Assigned(LLNode.FRight) then
          LLNode.FRight.FParent := LLNode;

        LXNode.FLeft := LLNode;
        LLNode.FParent := LXNode;

        LXNode.FBalance := 0;
        LLNode.FBalance := 0;
      end else
      begin
        LWNode := LXNode.FLeft;
        LWNode.FParent := LLNode.FParent;

        if not Assigned(LLNode.FParent) then
          FRoot := LWNode
        else
        begin
          if LLNode.FParent.FLeft = LLNode then
            LLNode.FParent.FLeft := LWNode
          else
            LLNode.FParent.FRight := LWNode;
        end;

        LXNode.FLeft := LWNode.FRight;

        if Assigned(LXNode.FLeft) then
          LXNode.FLeft.FParent := LXNode;

        LLNode.FRight := LWNode.FLeft;

        if Assigned(LLNode.FRight) then
          LLNode.FRight.FParent := LLNode;

        LWNode.FRight := LXNode;
        LWNode.FLeft := LLNode;

        LXNode.FParent := LWNode;
        LLNode.FParent := LWNode;

        if LWNode.FBalance = 1 then
        begin
          LXNode.FBalance := 0;
          LLNode.FBalance := -1;
        end else if LWNode.FBalance = 0 then
        begin
          LXNode.FBalance := 0;
          LLNode.FBalance := 0;
        end else
        begin
          LXNode.FBalance := 1;
          LLNode.FBalance := 0;
        end;

        LWNode.FBalance := 0;
      end;

      break;
    end;
  end;

end;

procedure TSortedDictionary<TKey, TValue>.Remove(const AKey: TKey);
var
  LNode: TNode;

begin
  { Get root }
  LNode := FindNodeWithKey(AKey);

  { Remove and rebalance the tree accordingly }
  if not Assigned(LNode) then
    Exit;

  { .. Do da dew! }
  BalanceTreesAfterRemoval(LNode);

  { Kill the stored value }
  NotifyValueRemoved(LNode.FValue);

  { Kill the node }
  LNode.Free;

  Dec(FCount);
  Inc(FVer);
end;

procedure TSortedDictionary<TKey, TValue>.ReplaceValue(var ACurrent: TValue; const ANew: TValue);
begin
  if not ValuesAreEqual(ACurrent, ANew) then
  begin
    { Notify that an element is removed. }
    NotifyValueRemoved(ACurrent);

    { Replace it. }
    ACurrent := ANew;
  end;
end;


procedure TSortedDictionary<TKey, TValue>.RecursiveClear(const ANode: TNode);
begin
  if Assigned(ANode.FLeft) then
    RecursiveClear(ANode.FLeft);

  if Assigned(ANode.FRight) then
    RecursiveClear(ANode.FRight);

  { Cleanup for AKey/Value }
  NotifyKeyRemoved(ANode.FKey);
  NotifyValueRemoved(ANode.FValue);

  { Finally, free the node itself }
  ANode.Free;
end;

function TSortedDictionary<TKey, TValue>.SelectKeys: IEnexCollection<TKey>;
begin
  Result := Keys;
end;

function TSortedDictionary<TKey, TValue>.SelectValues: IEnexCollection<TValue>;
begin
  Result := Values;
end;

procedure TSortedDictionary<TKey, TValue>.SetItem(const AKey: TKey; const Value: TValue);
begin
  { Allow inserting and adding values }
  Insert(AKey, Value, true);
end;

function TSortedDictionary<TKey, TValue>.TryGetValue(const AKey: TKey; out AFoundValue: TValue): Boolean;
var
  LResultNode: TNode;
begin
  LResultNode := FindNodeWithKey(AKey);

  if Assigned(LResultNode) then
  begin
    AFoundValue := LResultNode.FValue;
    Exit(true);
  end;

  { Default }
  AFoundValue := default(TValue);
  Exit(false);
end;

function TSortedDictionary<TKey, TValue>.ValueForKey(const AKey: TKey): TValue;
begin
  Result := GetItem(AKey);
end;

function TSortedDictionary<TKey, TValue>.WalkToTheRight(const ANode: TNode): TNode;
begin
  Result := ANode;

  if not Assigned(Result) then
    Exit;

  { Navigate further in the tree }
  if not Assigned(Result.FRight) then
  begin
    while (Assigned(Result.FParent) and (Result = Result.FParent.FRight)) do
      Result := Result.FParent;

    Result := Result.FParent;
  end else
  begin
    Result := Result.FRight;

    while Assigned(Result.FLeft) do
      Result := Result.FLeft;
  end;
end;

constructor TSortedDictionary<TKey, TValue>.Create(const AArray: array of TPair<TKey, TValue>;
  const AAscending: Boolean);
begin
  Create(TRules<TKey>.Default, TRules<TValue>.Default, AArray, AAscending);
end;

constructor TSortedDictionary<TKey, TValue>.Create(
  const AKeyRules: TRules<TKey>;
  const AValueRules: TRules<TValue>;
  const AArray: array of TPair<TKey, TValue>;
  const AAscending: Boolean);
var
  I: NativeInt;
begin
  { Call upper constructor }
  Create(AKeyRules, AValueRules, AAscending);

  { Copy all items in }
  for I := 0 to Length(AArray) - 1 do
  begin
    Add(AArray[I]);
  end;
end;

{ TSortedDictionary<TKey, TValue>.TPairEnumerator }

constructor TSortedDictionary<TKey, TValue>.TPairEnumerator.Create(const ADict: TSortedDictionary<TKey, TValue>);
begin
  { Initialize }
  FDict := ADict;
  KeepObjectAlive(FDict);

  FNext := ADict.FindLeftMostNode();
  FVer := ADict.FVer;
end;

destructor TSortedDictionary<TKey, TValue>.TPairEnumerator.Destroy;
begin
  ReleaseObject(FDict);
  inherited;
end;

function TSortedDictionary<TKey, TValue>.TPairEnumerator.GetCurrent: TPair<TKey,TValue>;
begin
  if FVer <> FDict.FVer then
     ExceptionHelper.Throw_CollectionChangedError();

  Result := FValue;
end;

function TSortedDictionary<TKey, TValue>.TPairEnumerator.MoveNext: Boolean;
begin
  if FVer <> FDict.FVer then
     ExceptionHelper.Throw_CollectionChangedError();

  { Do not continue on last node }
  if not Assigned(FNext) then
    Exit(false);

  { Get the current value }
  FValue.Key := FNext.FKey;
  FValue.Value := FNext.FValue;

  { Navigate further in the tree }
  FNext := FDict.WalkToTheRight(FNext);

  Result := true;
end;

{ TSortedDictionary<TKey, TValue>.TKeyEnumerator }

constructor TSortedDictionary<TKey, TValue>.TKeyEnumerator.Create(const ADict: TSortedDictionary<TKey, TValue>);
begin
  { Initialize }
  FDict := ADict;
  KeepObjectAlive(FDict);

  FNext := ADict.FindLeftMostNode();

  FVer := ADict.FVer;
end;

destructor TSortedDictionary<TKey, TValue>.TKeyEnumerator.Destroy;
begin
  ReleaseObject(FDict);
  inherited;
end;

function TSortedDictionary<TKey, TValue>.TKeyEnumerator.GetCurrent: TKey;
begin
  if FVer <> FDict.FVer then
     ExceptionHelper.Throw_CollectionChangedError();

  Result := FValue;
end;

function TSortedDictionary<TKey, TValue>.TKeyEnumerator.MoveNext: Boolean;
begin
  if FVer <> FDict.FVer then
     ExceptionHelper.Throw_CollectionChangedError();

  { Do not continue on last node }
  if not Assigned(FNext) then
    Exit(false);

  { Get the current value }
  FValue := FNext.FKey;

  { Navigate further in the tree }
  FNext := FDict.WalkToTheRight(FNext);

  Result := true;
end;


{ TSortedDictionary<TKey, TValue>.TValueEnumerator }

constructor TSortedDictionary<TKey, TValue>.TValueEnumerator.Create(const ADict: TSortedDictionary<TKey, TValue>);
begin
  { Initialize }
  FDict := ADict;
  KeepObjectAlive(FDict);

  FNext := ADict.FindLeftMostNode();

  FVer := ADict.FVer;
end;

destructor TSortedDictionary<TKey, TValue>.TValueEnumerator.Destroy;
begin
  ReleaseObject(FDict);
  inherited;
end;

function TSortedDictionary<TKey, TValue>.TValueEnumerator.GetCurrent: TValue;
begin
  if FVer <> FDict.FVer then
     ExceptionHelper.Throw_CollectionChangedError();

  Result := FValue;
end;

function TSortedDictionary<TKey, TValue>.TValueEnumerator.MoveNext: Boolean;
begin
  if FVer <> FDict.FVer then
     ExceptionHelper.Throw_CollectionChangedError();

  { Do not continue on last node }
  if not Assigned(FNext) then
    Exit(false);

  { Get the current value }
  FValue := FNext.FValue;

  { Navigate further in the tree }
  FNext := FDict.WalkToTheRight(FNext);

  Result := true;
end;

{ TSortedDictionary<TKey, TValue>.TKeyCollection }

constructor TSortedDictionary<TKey, TValue>.TKeyCollection.Create(const ADict: TSortedDictionary<TKey, TValue>);
begin
  { Call the upper constructor }
  inherited Create(ADict.KeyRules);

  { Initialize }
  FDict := ADict;
end;

function TSortedDictionary<TKey, TValue>.TKeyCollection.GetCount: NativeInt;
begin
  { Number of elements is the same as AKey }
  Result := FDict.Count;
end;

function TSortedDictionary<TKey, TValue>.TKeyCollection.GetEnumerator: IEnumerator<TKey>;
begin
  Result := TKeyEnumerator.Create(Self.FDict);
end;

procedure TSortedDictionary<TKey, TValue>.TKeyCollection.CopyTo(var AArray: array of TKey; const AStartIndex: NativeInt);
var
  I, X: NativeInt;
  LNode: TNode;
begin
  { Check for indexes }
  if (AStartIndex >= Length(AArray)) or (AStartIndex < 0) then
    ExceptionHelper.Throw_ArgumentOutOfRangeError('AStartIndex');

  if (Length(AArray) - AStartIndex) < FDict.Count then
     ExceptionHelper.Throw_ArgumentOutOfSpaceError('AArray');

  X := AStartIndex;

  { Find the left-most node }
  LNode := FDict.FindLeftMostNode();

  while Assigned(LNode) do
  begin
    { Get the AKey }
    AArray[X] := LNode.FKey;

    { Navigate further in the tree }
    LNode := FDict.WalkToTheRight(LNode);

    { Increment the index }
    Inc(X);
  end;
end;

{ TSortedDictionary<TKey, TValue>.TValueCollection }

constructor TSortedDictionary<TKey, TValue>.TValueCollection.Create(const ADict: TSortedDictionary<TKey, TValue>);
begin
  { Call the upper constructor }
  inherited Create(ADict.ValueRules);

  { Initialize }
  FDict := ADict;
end;

function TSortedDictionary<TKey, TValue>.TValueCollection.GetCount: NativeInt;
begin
  { Number of elements is the same as AKey }
  Result := FDict.Count;
end;

function TSortedDictionary<TKey, TValue>.TValueCollection.GetEnumerator: IEnumerator<TValue>;
begin
  Result := TValueEnumerator.Create(Self.FDict);
end;

procedure TSortedDictionary<TKey, TValue>.TValueCollection.CopyTo(var AArray: array of TValue; const AStartIndex: NativeInt);
var
  X: NativeInt;
  LNode: TNode;
begin
  { Check for indexes }
  if (AStartIndex >= Length(AArray)) or (AStartIndex < 0) then
    ExceptionHelper.Throw_ArgumentOutOfRangeError('AStartIndex');

  if (Length(AArray) - AStartIndex) < FDict.Count then
     ExceptionHelper.Throw_ArgumentOutOfSpaceError('AArray');

  X := AStartIndex;

  { Find the left-most node }
  LNode := FDict.FindLeftMostNode();

  while Assigned(LNode) do
  begin
    { Get the AKey }
    AArray[X] := LNode.FValue;

    { Navigate further in the tree }
    LNode := FDict.WalkToTheRight(LNode);

    { Increment the index }
    Inc(X);
  end;
end;

{ TObjectSortedDictionary<TKey, TValue> }

procedure TObjectSortedDictionary<TKey, TValue>.HandleKeyRemoved(const AKey: TKey);
begin
  if FOwnsKeys then
    TObject(AKey).Free;
end;

procedure TObjectSortedDictionary<TKey, TValue>.HandleValueRemoved(const AValue: TValue);
begin
  if FOwnsValues then
    TObject(AValue).Free;
end;

procedure TObjectSortedDictionary<TKey, TValue>.ReplaceValue(var ACurrent: TValue; const ANew: TValue);
begin
  { Only act if owns objects is set. Otherwise fallback to default. }
  if (FOwnsValues) and (TObject(ACurrent) <> TObject(ANew)) then
  begin
    NotifyValueRemoved(ACurrent);
    ACurrent := ANew;
  end else
    inherited;
end;

end.

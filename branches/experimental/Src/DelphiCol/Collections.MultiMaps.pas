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

unit Collections.MultiMaps;
interface
uses SysUtils,
     Generics.Defaults,
     Generics.Collections,
     Collections.Base,
     Collections.Lists,
     Collections.Sets,
     Collections.Dictionaries;

type
  ///  <summary>The base abstract class for all <c>multi-maps</c> in this package.</summary>
  TAbstractMultiMap<TKey, TValue> = class abstract(TEnexAssociativeCollection<TKey, TValue>, IMultiMap<TKey, TValue>)
  private type
    {$REGION 'Internal Types'}
    { Generic MultiMap Pairs Enumerator }
    TPairEnumerator = class(TEnumerator<TPair<TKey, TValue>>)
    private
      FVer: NativeInt;
      FDict: TAbstractMultiMap<TKey, TValue>;
      FValue: TPair<TKey, TValue>;

      FListIndex: NativeInt;
      FDictEnum: IEnumerator<TPair<TKey, IList<TValue>>>;
      FList: IList<TValue>;

    public
      { Constructor }
      constructor Create(const ADict: TAbstractMultiMap<TKey, TValue>);

      { Destructor }
      destructor Destroy(); override;

      function GetCurrent(): TPair<TKey,TValue>; override;
      function MoveNext(): Boolean; override;
    end;

    { Generic MultiMap Keys Enumerator }
    TKeyEnumerator = class(TEnumerator<TKey>)
    private
      FVer: NativeInt;
      FDict: TAbstractMultiMap<TKey, TValue>;
      FValue: TKey;
      FDictEnum: IEnumerator<TKey>;

    public
      { Constructor }
      constructor Create(const ADict: TAbstractMultiMap<TKey, TValue>);

      { Destructor }
      destructor Destroy(); override;

      function GetCurrent(): TKey; override;
      function MoveNext(): Boolean; override;
    end;

    { Generic MultiMap Values Enumerator }
    TValueEnumerator = class(TEnumerator<TValue>)
    private
      FVer: NativeInt;
      FDict: TAbstractMultiMap<TKey, TValue>;
      FValue: TValue;

      FListIndex: NativeInt;
      FDictEnum: IEnumerator<IList<TValue>>;
      FList: IList<TValue>;

    public
      { Constructor }
      constructor Create(const ADict: TAbstractMultiMap<TKey, TValue>);

      { Destructor }
      destructor Destroy(); override;

      function GetCurrent(): TValue; override;
      function MoveNext(): Boolean; override;
    end;

    { Generic MultiMap Keys Collection }
    TKeyCollection = class(TEnexCollection<TKey>)
    private
      FDict: TAbstractMultiMap<TKey, TValue>;

    protected
      { Hidden }
      function GetCount(): NativeInt; override;

    public
      { Constructor }
      constructor Create(const ADict: TAbstractMultiMap<TKey, TValue>);

      { Destructor }
      destructor Destroy(); override;

      { Property }
      property Count: NativeInt read GetCount;

      { IEnumerable/ ICollection support }
      function GetEnumerator(): IEnumerator<TKey>; override;

      { Copy-To }
      procedure CopyTo(var AArray: array of TKey; const AStartIndex: NativeInt); overload; override;

      { Enex Overrides }
      function Empty(): Boolean; override;
    end;

    { Generic MultiMap Values Collection }
    TValueCollection = class(TEnexCollection<TValue>)
    private
      FDict: TAbstractMultiMap<TKey, TValue>;

    protected

      { Hidden }
      function GetCount: NativeInt; override;
    public
      { Constructor }
      constructor Create(const ADict: TAbstractMultiMap<TKey, TValue>);

      { Destructor }
      destructor Destroy(); override;

      { Property }
      property Count: NativeInt read GetCount;

      { IEnumerable/ ICollection support }
      function GetEnumerator(): IEnumerator<TValue>; override;

      { Copy-To }
      procedure CopyTo(var AArray: array of TValue; const AStartIndex: NativeInt); overload; override;

      { Enex Overrides }
      function Empty(): Boolean; override;
    end;
    {$ENDREGION}

  private
    FVer: NativeInt;
    FKnownCount: NativeInt;
    FEmptyList: IEnexIndexedCollection<TValue>;
    FKeyCollection: IEnexCollection<TKey>;
    FValueCollection: IEnexCollection<TValue>;
    FDictionary: IDictionary<TKey, IList<TValue>>;

  protected
    ///  <summary>Specifies the internal dictionary used as back-end.</summary>
    ///  <returns>A dictionary of lists used as back-end.</summary>
    property Dictionary: IDictionary<TKey, IList<TValue>> read FDictionary;

    ///  <summary>Returns the number of pairs in the multi-map.</summary>
    ///  <returns>A positive value specifying the total number of pairs in the multi-map.</returns>
    ///  <remarks>The value returned by this method represents the total number of key-value pairs
    ///  stored in the dictionary. In a multi-map, this means that each value associated with a key
    ///  is calculated as a pair. If a key has multiple values associated with it, each key-value
    ///  combination is calculated as one.</remarks>
    function GetCount(): NativeInt; override;

    ///  <summary>Returns the collection of values associated with a key.</summary>
    ///  <param name="AKey">The key for which to obtain the associated values.</param>
    ///  <returns>An Enex collection that contains the values associated with this key.</returns>
    ///  <exception cref="Collections.Base|EKeyNotFoundException">The key is not found in the collection.</exception>
    function GetItemList(const AKey: TKey): IEnexIndexedCollection<TValue>;

    ///  <summary>Called when the map needs to initialize its internal dictionary.</summary>
    ///  <param name="AKeyRules">The rule set describing the keys.</param>
    function CreateDictionary(const AKeyRules: TRules<TKey>): IDictionary<TKey, IList<TValue>>; virtual; abstract;

    ///  <summary>Called when the map needs to initialize a list associated with a key.</summary>
    ///  <param name="AValueRules">The rule set describing the values.</param>
    function CreateList(const AValueRules: TRules<TValue>): IList<TValue>; virtual; abstract;

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
    ///  <param name="AKeyRules">A rule set describing the keys in the multi-map.</param>
    ///  <param name="AValueRules">A rule set describing the values in the multi-map.</param>
    constructor Create(const AKeyRules: TRules<TKey>; const AValueRules: TRules<TValue>); overload;

    ///  <summary>Creates a new instance of this class.</summary>
    ///  <param name="AKeyRules">A rule set describing the keys in the multi-map.</param>
    ///  <param name="AValueRules">A rule set describing the values in the multi-map.</param>
    ///  <param name="ACollection">A collection to copy pairs from.</param>
    ///  <exception cref="SysUtils|EArgumentNilException"><paramref name="ACollection"/> is <c>nil</c>.</exception>
    constructor Create(const AKeyRules: TRules<TKey>; const AValueRules: TRules<TValue>;
          const ACollection: IEnumerable<TPair<TKey,TValue>>); overload;

    ///  <summary>Creates a new instance of this class.</summary>
    ///  <param name="AKeyRules">A rule set describing the keys in the multi-map.</param>
    ///  <param name="AValueRules">A rule set describing the values in the multi-map.</param>
    ///  <param name="AArray">An array to copy pairs from.</param>
    constructor Create(const AKeyRules: TRules<TKey>; const AValueRules: TRules<TValue>;
          const AArray: array of TPair<TKey,TValue>); overload;

    ///  <summary>Destroys this instance.</summary>
    ///  <remarks>Do not call this method directly; call <c>Free</c> instead.</remarks>
    destructor Destroy(); override;

    ///  <summary>Clears the contents of the multi-map.</summary>
    procedure Clear();

    ///  <summary>Adds a key-value pair to the multi-map.</summary>
    ///  <param name="APair">The key-value pair to add.</param>
    ///  <exception cref="Collections.Base|EDuplicateKeyException">The multi-map already contains a pair with the given key.</exception>
    procedure Add(const APair: TPair<TKey, TValue>); overload;

    ///  <summary>Adds a key-value pair to the multi-map.</summary>
    ///  <param name="AKey">The key of the pair.</param>
    ///  <param name="AValue">The value associated with the key.</param>
    ///  <exception cref="Collections.Base|EDuplicateKeyException">The multi-map already contains a pair with the given key.</exception>
    procedure Add(const AKey: TKey; const AValue: TValue); overload;

    ///  <summary>Removes a key-value pair using a given key.</summary>
    ///  <param name="AKey">The key of pair.</param>
    ///  <remarks>If the specified key was not found in the multi-map, nothing happens.</remarks>
    procedure Remove(const AKey: TKey); overload;

    ///  <summary>Removes a key-value pair using a given key and value.</summary>
    ///  <param name="AKey">The key associated with the value.</param>
    ///  <param name="AValue">The value to remove.</param>
    ///  <remarks>A multi-map allows storing multiple values for a given key. This method allows removing only the
    ///  specified value from the collection of values associated with the given key.</remarks>
    procedure Remove(const AKey: TKey; const AValue: TValue); overload;

    ///  <summary>Removes a key-value pair using a given key and value.</summary>
    ///  <param name="APair">The key and its associated value to remove.</param>
    ///  <remarks>A multi-map allows storing multiple values for a given key. This method allows removing only the
    ///  specified value from the collection of values associated with the given key.</remarks>
    procedure Remove(const APair: TPair<TKey, TValue>); overload;

    ///  <summary>Checks whether the multi-map contains a key-value pair identified by the given key.</summary>
    ///  <param name="AKey">The key to check for.</param>
    ///  <returns><c>True</c> if the map contains a pair identified by the given key; <c>False</c> otherwise.</returns>
    function ContainsKey(const AKey: TKey): Boolean;

    ///  <summary>Checks whether the multi-map contains a key-value pair that contains a given value.</summary>
    ///  <param name="AValue">The value to check for.</param>
    ///  <returns><c>True</c> if the multi-map contains a pair containing the given value; <c>False</c> otherwise.</returns>
    function ContainsValue(const AValue: TValue): Boolean; overload;

    ///  <summary>Checks whether the multi-map contains a given key-value combination.</summary>
    ///  <param name="AKey">The key associated with the value.</param>
    ///  <param name="AValue">The value associated with the key.</param>
    ///  <returns><c>True</c> if the map contains the given association; <c>False</c> otherwise.</returns>
    function ContainsValue(const AKey: TKey; const AValue: TValue): Boolean; overload;

    ///  <summary>Checks whether the multi-map contains a given key-value combination.</summary>
    ///  <param name="APair">The key-value pair to check for.</param>
    ///  <returns><c>True</c> if the map contains the given association; <c>False</c> otherwise.</returns>
    function ContainsValue(const APair: TPair<TKey, TValue>): Boolean; overload;

    ///  <summary>Tries to extract the collection of values associated with a key.</summary>
    ///  <param name="AKey">The key for which to obtain the associated values.</param>
    ///  <param name="AValues">The Enex collection that stores the associated values.</param>
    ///  <returns><c>True</c> if the key exists in the collection; <c>False</c> otherwise.</returns>
    function TryGetValues(const AKey: TKey; out AValues: IEnexIndexedCollection<TValue>): Boolean; overload;

    ///  <summary>Tries to extract the collection of values associated with a key.</summary>
    ///  <param name="AKey">The key for which to obtain the associated values.</param>
    ///  <returns>The associated collection if the key is valid; an empty collection otherwise.</returns>
    function TryGetValues(const AKey: TKey): IEnexIndexedCollection<TValue>; overload;

    ///  <summary>Returns the collection of values associated with a key.</summary>
    ///  <param name="AKey">The key for which to obtain the associated values.</param>
    ///  <returns>An Enex collection that contains the values associated with this key.</returns>
    ///  <exception cref="Collections.Base|EKeyNotFoundException">The key is not found in the multi-map.</exception>
    property Items[const AKey: TKey]: IEnexIndexedCollection<TValue> read GetItemList; default;

    ///  <summary>Returns the number of pairs in the multi-map.</summary>
    ///  <returns>A positive value specifying the total number of pairs in the multi-map.</returns>
    ///  <remarks>The value returned by this method represents the total number of key-value pairs
    ///  stored in the dictionary. In a multi-map, this means that each value associated with a key
    ///  is calculated as a pair. If a key has multiple values associated with it, each key-value
    ///  combination is calculated as one.</remarks>
    property Count: NativeInt read FKnownCount;

    ///  <summary>Specifies the collection that contains only the keys.</summary>
    ///  <returns>An Enex collection that contains all the keys stored in the multi-map.</returns>
    property Keys: IEnexCollection<TKey> read FKeyCollection;

    ///  <summary>Specifies the collection that contains only the values.</summary>
    ///  <returns>An Enex collection that contains all the values stored in the multi-map.</returns>
    property Values: IEnexCollection<TValue> read FValueCollection;

    ///  <summary>Returns a new enumerator object used to enumerate this multi-map.</summary>
    ///  <remarks>This method is usually called by compiler-generated code. Its purpose is to create an enumerator
    ///  object that is used to actually traverse the multi-map.</remarks>
    ///  <returns>An enumerator object.</returns>
    function GetEnumerator(): IEnumerator<TPair<TKey,TValue>>; override;

    ///  <summary>Copies the values stored in the multi-map to a given array.</summary>
    ///  <param name="AArray">An array where to copy the contents of the multi-map.</param>
    ///  <param name="AStartIndex">The index into the array at which the copying begins.</param>
    ///  <remarks>This method assumes that <paramref name="AArray"/> has enough space to hold the contents of the multi-map.</remarks>
    ///  <exception cref="SysUtils|EArgumentOutOfRangeException"><paramref name="AStartIndex"/> is out of bounds.</exception>
    ///  <exception cref="Collections.Base|EArgumentOutOfSpaceException">The array is not long enough.</exception>
    procedure CopyTo(var AArray: array of TPair<TKey,TValue>; const AStartIndex: NativeInt); overload; override;

    ///  <summary>Returns the value associated with the given key.</summary>
    ///  <param name="AKey">The key for which to return the associated value.</param>
    ///  <returns>The value associated with the given key.</returns>
    ///  <exception cref="Collections.Base|EKeyNotFoundException">No such key in the multi-map.</exception>
    function ValueForKey(const AKey: TKey): TValue; override;

    ///  <summary>Checks whether the multi-map contains a given key-value pair.</summary>
    ///  <param name="AKey">The key part of the pair.</param>
    ///  <param name="AValue">The value part of the pair.</param>
    ///  <returns><c>True</c> if the given key-value pair exists; <c>False</c> otherwise.</returns>
    function KeyHasValue(const AKey: TKey; const AValue: TValue): Boolean; override;

    ///  <summary>Returns an Enex collection that contains only the keys.</summary>
    ///  <returns>An Enex collection that contains all the keys stored in the multi-map.</returns>
    function SelectKeys(): IEnexCollection<TKey>; override;

    ///  <summary>Returns an Enex collection that contains only the values.</summary>
    ///  <returns>An Enex collection that contains all the values stored in the multi-map.</returns>
    function SelectValues(): IEnexCollection<TValue>; override;
  end;

type
  ///  <summary>The base abstract class for all <c>distinct multi-maps</c> in this package.</summary>
  TAbstractDistinctMultiMap<TKey, TValue> = class abstract(TEnexAssociativeCollection<TKey, TValue>, IDistinctMultiMap<TKey, TValue>)
  private type
    {$REGION 'Internal Types'}
    { Generic MultiMap Pairs Enumerator }
    TPairEnumerator = class(TEnumerator<TPair<TKey,TValue>>)
    private
      FVer: NativeInt;
      FDict: TAbstractDistinctMultiMap<TKey, TValue>;
      FValue: TPair<TKey, TValue>;

      FSetEnum: IEnumerator<TValue>;
      FDictEnum: IEnumerator<TPair<TKey, ISet<TValue>>>;
      FSet: ISet<TValue>;

    public
      { Constructor }
      constructor Create(const ADict: TAbstractDistinctMultiMap<TKey, TValue>);

      { Destructor }
      destructor Destroy(); override;

      function GetCurrent(): TPair<TKey,TValue>; override;
      function MoveNext(): Boolean; override;
    end;

    { Generic MultiMap Keys Enumerator }
    TKeyEnumerator = class(TEnumerator<TKey>)
    private
      FVer: NativeInt;
      FDict: TAbstractDistinctMultiMap<TKey, TValue>;
      FValue: TKey;
      FDictEnum: IEnumerator<TKey>;

    public
      { Constructor }
      constructor Create(const ADict: TAbstractDistinctMultiMap<TKey, TValue>);

      { Destructor }
      destructor Destroy(); override;

      function GetCurrent(): TKey; override;
      function MoveNext(): Boolean; override;
    end;

    { Generic MultiMap Values Enumerator }
    TValueEnumerator = class(TEnumerator<TValue>)
    private
      FVer: NativeInt;
      FDict: TAbstractDistinctMultiMap<TKey, TValue>;
      FValue: TValue;

      FDictEnum: IEnumerator<ISet<TValue>>;
      FSetEnum: IEnumerator<TValue>;
      FSet: ISet<TValue>;

    public
      { Constructor }
      constructor Create(const ADict: TAbstractDistinctMultiMap<TKey, TValue>);

      { Destructor }
      destructor Destroy(); override;

      function GetCurrent(): TValue; override;
      function MoveNext(): Boolean; override;
    end;

    { Generic MultiMap Keys Collection }
    TKeyCollection = class(TEnexCollection<TKey>)
    private
      FDict: TAbstractDistinctMultiMap<TKey, TValue>;

    protected
      { Hidden }
      function GetCount(): NativeInt; override;
    public
      { Constructor }
      constructor Create(const ADict: TAbstractDistinctMultiMap<TKey, TValue>);

      { Property }
      property Count: NativeInt read GetCount;

      { IEnumerable/ ICollection support }
      function GetEnumerator(): IEnumerator<TKey>; override;

      { Copy-To }
      procedure CopyTo(var AArray: array of TKey; const AStartIndex: NativeInt); overload; override;

      { Enex Overrides }
      function Empty(): Boolean; override;
    end;

    { Generic MultiMap Values Collection }
    TValueCollection = class(TEnexCollection<TValue>)
    private
      FDict: TAbstractDistinctMultiMap<TKey, TValue>;

    protected
      { Hidden }
      function GetCount: NativeInt; override;

    public
      { Constructor }
      constructor Create(const ADict: TAbstractDistinctMultiMap<TKey, TValue>);

      { Property }
      property Count: NativeInt read GetCount;

      { IEnumerable/ ICollection support }
      function GetEnumerator(): IEnumerator<TValue>; override;

      { Copy-To }
      procedure CopyTo(var AArray: array of TValue; const AStartIndex: NativeInt); overload; override;

      { Enex Overrides }
      function Empty(): Boolean; override;
    end;
    {$ENDREGION}

  private var
    FVer: NativeInt;
    FKnownCount: NativeInt;
    FEmptySet: IEnexCollection<TValue>;
    FKeyCollection: IEnexCollection<TKey>;
    FValueCollection: IEnexCollection<TValue>;
    FDictionary: IDictionary<TKey, ISet<TValue>>;

  protected
    ///  <summary>Specifies the internal dictionary used as back-end.</summary>
    ///  <returns>A dictionary of lists used as back-end.</summary>
    property Dictionary: IDictionary<TKey, ISet<TValue>> read FDictionary;

    ///  <summary>Returns the number of pairs in the multi-map.</summary>
    ///  <returns>A positive value specifying the total number of pairs in the multi-map.</returns>
    ///  <remarks>The value returned by this method represents the total number of key-value pairs
    ///  stored in the dictionary. In a multi-map, this means that each value associated with a key
    ///  is calculated as a pair. If a key has multiple values associated with it, each key-value
    ///  combination is calculated as one.</remarks>
    function GetCount(): NativeInt; override;

    ///  <summary>Returns the collection of values associated with a key.</summary>
    ///  <param name="AKey">The key for which to obtain the associated values.</param>
    ///  <returns>An Enex collection that contains the values associated with this key.</returns>
    ///  <exception cref="Collections.Base|EKeyNotFoundException">The key is not found in the collection.</exception>
    function GetItemList(const AKey: TKey): IEnexCollection<TValue>;

    ///  <summary>Called when the map needs to initialize its internal dictionary.</summary>
    ///  <param name="AKeyRules">The rule set describing the keys.</param>
    function CreateDictionary(const AKeyRules: TRules<TKey>): IDictionary<TKey, ISet<TValue>>; virtual; abstract;

    ///  <summary>Called when the map needs to initialize a set associated with a key.</summary>
    ///  <param name="AValueRules">The rule set describing the values.</param>
    function CreateSet(const AValueRules: TRules<TValue>): ISet<TValue>; virtual; abstract;

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
    ///  <param name="AKeyRules">A rule set describing the keys in the multi-map.</param>
    ///  <param name="AValueRules">A rule set describing the values in the multi-map.</param>
    constructor Create(const AKeyRules: TRules<TKey>; const AValueRules: TRules<TValue>); overload;

    ///  <summary>Creates a new instance of this class.</summary>
    ///  <param name="AKeyRules">A rule set describing the keys in the multi-map.</param>
    ///  <param name="AValueRules">A rule set describing the values in the multi-map.</param>
    ///  <param name="ACollection">A collection to copy pairs from.</param>
    ///  <exception cref="SysUtils|EArgumentNilException"><paramref name="ACollection"/> is <c>nil</c>.</exception>
    constructor Create(const AKeyRules: TRules<TKey>; const AValueRules: TRules<TValue>;
          const ACollection: IEnumerable<TPair<TKey,TValue>>); overload;

    ///  <summary>Creates a new instance of this class.</summary>
    ///  <param name="AKeyRules">A rule set describing the keys in the multi-map.</param>
    ///  <param name="AValueRules">A rule set describing the values in the multi-map.</param>
    ///  <param name="AArray">An array to copy pairs from.</param>
    constructor Create(const AKeyRules: TRules<TKey>; const AValueRules: TRules<TValue>;
          const AArray: array of TPair<TKey,TValue>); overload;

    ///  <summary>Destroys this instance.</summary>
    ///  <remarks>Do not call this method directly; call <c>Free</c> instead.</remarks>
    destructor Destroy(); override;

    ///  <summary>Clears the contents of the multi-map.</summary>
    procedure Clear();

    ///  <summary>Adds a key-value pair to the multi-map.</summary>
    ///  <param name="APair">The key-value pair to add.</param>
    ///  <exception cref="Collections.Base|EDuplicateKeyException">The multi-map already contains a pair with the given key.</exception>
    procedure Add(const APair: TPair<TKey, TValue>); overload;

    ///  <summary>Adds a key-value pair to the multi-map.</summary>
    ///  <param name="AKey">The key of the pair.</param>
    ///  <param name="AValue">The value associated with the key.</param>
    ///  <exception cref="Collections.Base|EDuplicateKeyException">The multi-map already contains a pair with the given key.</exception>
    procedure Add(const AKey: TKey; const AValue: TValue); overload;

    ///  <summary>Removes a key-value pair using a given key.</summary>
    ///  <param name="AKey">The key of the pair.</param>
    ///  <remarks>If the specified key was not found in the multi-map, nothing happens.</remarks>
    procedure Remove(const AKey: TKey); overload;

    ///  <summary>Removes a key-value pair using a given key and value.</summary>
    ///  <param name="AKey">The key associated with the value.</param>
    ///  <param name="AValue">The value to remove.</param>
    ///  <remarks>A multi-map allows storing multiple values for a given key. This method allows removing only the
    ///  specified value from the collection of values associated with the given key.</remarks>
    procedure Remove(const AKey: TKey; const AValue: TValue); overload;

    ///  <summary>Removes a key-value pair using a given key and value.</summary>
    ///  <param name="APair">The key and its associated value to remove.</param>
    ///  <remarks>A multi-map allows storing multiple values for a given key. This method allows removing only the
    ///  specified value from the collection of values associated with the given key.</remarks>
    procedure Remove(const APair: TPair<TKey, TValue>); overload;

    ///  <summary>Checks whether the multi-map contains a key-value pair identified by the given key.</summary>
    ///  <param name="AKey">The key to check for.</param>
    ///  <returns><c>True</c> if the map contains a pair identified by the given key; <c>False</c> otherwise.</returns>
    function ContainsKey(const AKey: TKey): Boolean;

    ///  <summary>Checks whether the multi-map contains a key-value pair that contains a given value.</summary>
    ///  <param name="AValue">The value to check for.</param>
    ///  <returns><c>True</c> if the multi-map contains a pair containing the given value; <c>False</c> otherwise.</returns>
    function ContainsValue(const AValue: TValue): Boolean; overload;

    ///  <summary>Checks whether the multi-map contains a given key-value combination.</summary>
    ///  <param name="AKey">The key associated with the value.</param>
    ///  <param name="AValue">The value associated with the key.</param>
    ///  <returns><c>True</c> if the map contains the given association; <c>False</c> otherwise.</returns>
    function ContainsValue(const AKey: TKey; const AValue: TValue): Boolean; overload;

    ///  <summary>Checks whether the multi-map contains a given key-value combination.</summary>
    ///  <param name="APair">The key-value pair to check for.</param>
    ///  <returns><c>True</c> if the map contains the given association; <c>False</c> otherwise.</returns>
    function ContainsValue(const APair: TPair<TKey, TValue>): Boolean; overload;

    ///  <summary>Tries to extract the collection of values associated with a key.</summary>
    ///  <param name="AKey">The key for which to obtain the associated values.</param>
    ///  <param name="AValues">The Enex collection that stores the associated values.</param>
    ///  <returns><c>True</c> if the key exists in the collection; <c>False</c> otherwise.</returns>
    function TryGetValues(const AKey: TKey; out AValues: IEnexCollection<TValue>): Boolean; overload;

    ///  <summary>Tries to extract the collection of values associated with a key.</summary>
    ///  <param name="AKey">The key for which to obtain the associated values.</param>
    ///  <returns>The associated collection if the key is valid; an empty collection otherwise.</returns>
    function TryGetValues(const AKey: TKey): IEnexCollection<TValue>; overload;

    ///  <summary>Returns the collection of values associated with a key.</summary>
    ///  <param name="AKey">The key for which to obtain the associated values.</param>
    ///  <returns>An Enex collection that contains the values associated with this key.</returns>
    ///  <exception cref="Collections.Base|EKeyNotFoundException">The key is not found in the multi-map.</exception>
    property Items[const AKey: TKey]: IEnexCollection<TValue> read GetItemList; default;

    ///  <summary>Returns the number of pairs in the multi-map.</summary>
    ///  <returns>A positive value specifying the total number of pairs in the multi-map.</returns>
    ///  <remarks>The value returned by this method represents the total number of key-value pairs
    ///  stored in the dictionary. In a multi-map, this means that each value associated with a key
    ///  is calculated as a pair. If a key has multiple values associated with it, each key-value
    ///  combination is calculated as one.</remarks>
    property Count: NativeInt read FKnownCount;

    ///  <summary>Specifies the collection that contains only the keys.</summary>
    ///  <returns>An Enex collection that contains all the keys stored in the multi-map.</returns>
    property Keys: IEnexCollection<TKey> read FKeyCollection;

    ///  <summary>Specifies the collection that contains only the values.</summary>
    ///  <returns>An Enex collection that contains all the values stored in the multi-map.</returns>
    property Values: IEnexCollection<TValue> read FValueCollection;

    ///  <summary>Returns a new enumerator object used to enumerate this multi-map.</summary>
    ///  <remarks>This method is usually called by compiler-generated code. Its purpose is to create an enumerator
    ///  object that is used to actually traverse the multi-map.</remarks>
    ///  <returns>An enumerator object.</returns>
    function GetEnumerator(): IEnumerator<TPair<TKey,TValue>>; override;

    ///  <summary>Copies the values stored in the multi-map to a given array.</summary>
    ///  <param name="AArray">An array where to copy the contents of the multi-map.</param>
    ///  <param name="AStartIndex">The index into the array at which the copying begins.</param>
    ///  <remarks>This method assumes that <paramref name="AArray"/> has enough space to hold the contents of the multi-map.</remarks>
    ///  <exception cref="SysUtils|EArgumentOutOfRangeException"><paramref name="AStartIndex"/> is out of bounds.</exception>
    ///  <exception cref="Collections.Base|EArgumentOutOfSpaceException">The array is not long enough.</exception>
    procedure CopyTo(var AArray: array of TPair<TKey,TValue>; const AStartIndex: NativeInt); overload; override;

    ///  <summary>Returns the value associated with the given key.</summary>
    ///  <param name="AKey">The key for which to return the associated value.</param>
    ///  <returns>The value associated with the given key.</returns>
    ///  <exception cref="Collections.Base|EKeyNotFoundException">No such key in the multi-map.</exception>
    function ValueForKey(const AKey: TKey): TValue; override;

    ///  <summary>Checks whether the multi-map contains a given key-value pair.</summary>
    ///  <param name="AKey">The key part of the pair.</param>
    ///  <param name="AValue">The value part of the pair.</param>
    ///  <returns><c>True</c> if the given key-value pair exists; <c>False</c> otherwise.</returns>
    function KeyHasValue(const AKey: TKey; const AValue: TValue): Boolean; override;

    ///  <summary>Returns an Enex collection that contains only the keys.</summary>
    ///  <returns>An Enex collection that contains all the keys stored in the multi-map.</returns>
    function SelectKeys(): IEnexCollection<TKey>; override;

    ///  <summary>Returns an Enex collection that contains only the values.</summary>
    ///  <returns>An Enex collection that contains all the values stored in the multi-map.</returns>
    function SelectValues(): IEnexCollection<TValue>; override;
  end;

type
  ///  <summary>The generic <c>multi-map</c> collection.</summary>
  ///  <remarks>This type uses a <c>dictionary</c> and a number of <c>lists</c> to store its
  ///  keys and values.</remarks>
  TMultiMap<TKey, TValue> = class(TAbstractMultiMap<TKey, TValue>)
  private
    FInitialCapacity: NativeInt;

  protected
    ///  <summary>Called when the map needs to initialize its internal dictionary.</summary>
    ///  <param name="AKeyRules">The rule set describing the keys.</param>
    ///  <remarks>This method creates a hash-based dictionary used as the underlying back-end for the map.</remarks>
    function CreateDictionary(const AKeyRules: TRules<TKey>): IDictionary<TKey, IList<TValue>>; override;

    ///  <summary>Called when the map needs to initialize a list associated with a key.</summary>
    ///  <param name="AValueRules">The rule set describing the values.</param>
    ///  <remarks>This method creates a simple array-based list. This list is associated with a key and stores the map's
    ///  values for that key.</remarks>
    function CreateList(const AValueRules: TRules<TValue>): IList<TValue>; override;

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

  ///  <summary>The generic <c>multi-map</c> collection designed to store objects.</summary>
  ///  <remarks>This type uses a <c>dictionary</c> and a number of <c>lists</c> to store its
  ///  keys and values.</remarks>
  TObjectMultiMap<TKey, TValue: class> = class(TMultiMap<TKey, TValue>)
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
    ///  <remarks>This property specifies the way the map controls the life-time of the stored keys. The value of this property has effect only
    ///  if the keys are objects, otherwise it is ignored.</remarks>
    property OwnsKeys: Boolean read FOwnsKeys write FOwnsKeys;

    ///  <summary>Specifies whether this map owns the values.</summary>
    ///  <returns><c>True</c> if the map owns the values; <c>False</c> otherwise.</returns>
    ///  <remarks>This property specifies the way the map controls the life-time of the stored values. The value of this property has effect only
    ///  if the values are objects, otherwise it is ignored.</remarks>
    property OwnsValues: Boolean read FOwnsValues write FOwnsValues;
  end;

type
  ///  <summary>The generic <c>multi-map</c> collection.</summary>
  ///  <remarks>This type uses a <c>sorted dictionary</c> and a number of <c>lists</c> to store its
  ///  keys and values.</remarks>
  TSortedMultiMap<TKey, TValue> = class(TAbstractMultiMap<TKey, TValue>)
  private
    FAscSort: Boolean;

  protected
    ///  <summary>Called when the map needs to initialize its internal dictionary.</summary>
    ///  <param name="AKeyRules">The rule set describing the keys.</param>
    ///  <remarks>This method creates an AVL dictionary used as the underlying back-end for the map.</remarks>
    function CreateDictionary(const AKeyRules: TRules<TKey>): IDictionary<TKey, IList<TValue>>; override;

    ///  <summary>Called when the map needs to initialize a list associated with a key.</summary>
    ///  <param name="AValueRules">The rule set describing the values.</param>
    ///  <remarks>This method creates a simple array-based list. This list is associated with a key and store the map's
    ///  values for that key.</remarks>
    function CreateList(const AValueRules: TRules<TValue>): IList<TValue>; override;
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

  ///  <summary>The generic <c>multi-map</c> collection designed to store objects.</summary>
  ///  <remarks>This type uses a <c>sorted dictionary</c> and a number of <c>lists</c> to store its
  ///  keys and values.</remarks>
  TObjectSortedMultiMap<TKey, TValue> = class(TSortedMultiMap<TKey, TValue>)
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
    ///  <remarks>This property specifies the way the map controls the life-time of the stored keys. The value of this property has effect only
    ///  if the keys are objects, otherwise it is ignored.</remarks>
    property OwnsKeys: Boolean read FOwnsKeys write FOwnsKeys;

    ///  <summary>Specifies whether this map owns the values.</summary>
    ///  <returns><c>True</c> if the map owns the values; <c>False</c> otherwise.</returns>
    ///  <remarks>This property specifies the way the map controls the life-time of the stored values. The value of this property has effect only
    ///  if the values are objects, otherwise it is ignored.</remarks>
    property OwnsValues: Boolean read FOwnsValues write FOwnsValues;
  end;

type
  ///  <summary>The generic <c>multi-map</c> collection.</summary>
  ///  <remarks>This type uses a <c>dictionary</c> and a number of <c>sets</c> to store its
  ///  keys and values.</remarks>
  TDistinctMultiMap<TKey, TValue> = class(TAbstractDistinctMultiMap<TKey, TValue>)
  private
    FInitialCapacity: NativeInt;

  protected
    ///  <summary>Called when the map needs to initialize its internal dictionary.</summary>
    ///  <param name="AKeyRules">The rule set describing the keys.</param>
    ///  <remarks>This method creates a hash-based dictionary used as the underlying back-end for the map.</remarks>
    function CreateDictionary(const AKeyRules: TRules<TKey>): IDictionary<TKey, ISet<TValue>>; override;

    ///  <summary>Called when the map needs to initialize a set associated with a key.</summary>
    ///  <param name="AValueRules">The rule set describing the values.</param>
    ///  <remarks>This method creates a hash-based set. This set is associated with a key and stores the map's
    ///  values for that key.</remarks>
    function CreateSet(const AValueRules: TRules<TValue>): ISet<TValue>; override;
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

  ///  <summary>The generic <c>multi-map</c> collection designed to store objects.</summary>
  ///  <remarks>This type uses a <c>dictionary</c> and a number of <c>sets</c> to store its
  ///  keys and values.</remarks>
  TObjectDistinctMultiMap<TKey, TValue> = class(TDistinctMultiMap<TKey, TValue>)
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
    ///  <remarks>This property specifies the way the map controls the life-time of the stored keys. The value of this property has effect only
    ///  if the keys are objects, otherwise it is ignored.</remarks>
    property OwnsKeys: Boolean read FOwnsKeys write FOwnsKeys;

    ///  <summary>Specifies whether this map owns the values.</summary>
    ///  <returns><c>True</c> if the map owns the values; <c>False</c> otherwise.</returns>
    ///  <remarks>This property specifies the way the map controls the life-time of the stored values. The value of this property has effect only
    ///  if the values are objects, otherwise it is ignored.</remarks>
    property OwnsValues: Boolean read FOwnsValues write FOwnsValues;
  end;

type
  ///  <summary>The generic <c>distinct multi-map</c> collection.</summary>
  ///  <remarks>This type uses a <c>sorted dictionary</c> and a number of <c>sorted sets</c> to store its
  ///  keys and values.</remarks>
  TSortedDistinctMultiMap<TKey, TValue> = class(TAbstractDistinctMultiMap<TKey, TValue>)
  private
    FAscSort: Boolean;

  protected
    ///  <summary>Called when the map needs to initialize its internal dictionary.</summary>
    ///  <param name="AKeyRules">The rule set describing the keys.</param>
    ///  <remarks>This method creates an AVL dictionary used as the underlying back-end for the map.</remarks>
    function CreateDictionary(const AKeyRules: TRules<TKey>): IDictionary<TKey, ISet<TValue>>; override;

    ///  <summary>Called when the map needs to initialize a set associated with a key.</summary>
    ///  <param name="AValueRules">The rule set describing the values.</param>
    ///  <remarks>This method creates an AVL-based set. This set is associated with a key and stores the map's
    ///  values for that key.</remarks>
    function CreateSet(const AValueRules: TRules<TValue>): ISet<TValue>; override;
  public
    ///  <summary>Creates a new instance of this class.</summary>
    ///  <param name="AAscending">A value specifying whether the keys and values are sorted in ascending order. The default is <c>True</c>.</param>
    ///  <remarks>The default rule set is requested.</remarks>
    constructor Create(const AAscending: Boolean = True); overload;

    ///  <summary>Creates a new instance of this class.</summary>
    ///  <param name="ACollection">A collection to copy the key-value pairs from.</param>
    ///  <param name="AAscending">A value specifying whether the keys and values are sorted in ascending order. The default is <c>True</c>.</param>
    ///  <remarks>The default rule set is requested.</remarks>
    ///  <exception cref="SysUtils|EArgumentNilException"><paramref name="ACollection"/> is <c>nil</c>.</exception>
    constructor Create(const ACollection: IEnumerable<TPair<TKey,TValue>>; const AAscending: Boolean = True); overload;

    ///  <summary>Creates a new instance of this class.</summary>
    ///  <param name="AArray">An array to copy the key-value pairs from.</param>
    ///  <param name="AAscending">A value specifying whether the keys and values are sorted in ascending order. The default is <c>True</c>.</param>
    ///  <remarks>The default rule set is requested.</remarks>
    constructor Create(const AArray: array of TPair<TKey,TValue>; const AAscending: Boolean = True); overload;

    ///  <summary>Creates a new instance of this class.</summary>
    ///  <param name="AKeyRules">The rule set describing the keys.</param>
    ///  <param name="AValueRules">The rule set describing the values.</param>
    ///  <param name="AAscending">A value specifying whether the keys and values are sorted in ascending order. The default is <c>True</c>.</param>
    constructor Create(const AKeyRules: TRules<TKey>; const AValueRules: TRules<TValue>; const AAscending: Boolean = True); overload;

    ///  <summary>Creates a new instance of this class.</summary>
    ///  <param name="AKeyRules">The rule set describing the keys.</param>
    ///  <param name="AValueRules">The rule set describing the values.</param>
    ///  <param name="ACollection">A collection to copy the key-value pairs from.</param>
    ///  <param name="AAscending">A value specifying whether the keys and values are sorted in ascending order. The default is <c>True</c>.</param>
    ///  <exception cref="SysUtils|EArgumentNilException"><paramref name="ACollection"/> is <c>nil</c>.</exception>
    constructor Create(const AKeyRules: TRules<TKey>; const AValueRules: TRules<TValue>;
      const ACollection: IEnumerable<TPair<TKey,TValue>>; const AAscending: Boolean = True); overload;

    ///  <summary>Creates a new instance of this class.</summary>
    ///  <param name="AKeyRules">The rule set describing the keys.</param>
    ///  <param name="AValueRules">The rule set describing the values.</param>
    ///  <param name="AArray">An array to copy the key-value pairs from.</param>
    ///  <param name="AAscending">A value specifying whether the keys and values are sorted in ascending order. The default is <c>True</c>.</param>
    constructor Create(const AKeyRules: TRules<TKey>; const AValueRules: TRules<TValue>;
      const AArray: array of TPair<TKey,TValue>; const AAscending: Boolean = True); overload;

    ///  <summary>Returns the biggest key.</summary>
    ///  <returns>The biggest key stored in the map.</returns>
    ///  <exception cref="Collections.Base|ECollectionEmptyException">The map is empty.</exception>
    function MaxKey(): TKey; override;

    ///  <summary>Returns the smallest key.</summary>
    ///  <returns>The smallest key stored in the map.</returns>
    ///  <exception cref="Collections.Base|ECollectionEmptyException">The map is empty.</exception>
    function MinKey(): TKey; override;
  end;

  ///  <summary>The generic <c>distinct multi-map</c> collection designed to store objects.</summary>
  ///  <remarks>This type uses a <c>sorted dictionary</c> and a number of <c>sorted sets</c> to store its
  ///  keys and values.</remarks>
  TObjectSortedDistinctMultiMap<TKey, TValue> = class(TSortedDistinctMultiMap<TKey, TValue>)
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
    ///  <remarks>This property specifies the way the map controls the life-time of the stored keys. The value of this property has effect only
    ///  if the keys are objects, otherwise it is ignored.</remarks>
    property OwnsKeys: Boolean read FOwnsKeys write FOwnsKeys;

    ///  <summary>Specifies whether this map owns the values.</summary>
    ///  <returns><c>True</c> if the map owns the values; <c>False</c> otherwise.</returns>
    ///  <remarks>This property specifies the way the map controls the life-time of the stored values. The value of this property has effect only
    ///  if the values are objects, otherwise it is ignored.</remarks>
    property OwnsValues: Boolean read FOwnsValues write FOwnsValues;
  end;

type
  ///  <summary>The generic <c>multi-map</c> collection.</summary>
  ///  <remarks>This type uses a <c>sorted dictionary</c> and a number of <c>sorted lists</c> to store its
  ///  keys and values.</remarks>
  TDoubleSortedMultiMap<TKey, TValue> = class(TSortedMultiMap<TKey, TValue>)
  private
    FAscValues: Boolean;

  protected
    ///  <summary>Called when the map needs to initialize a list associated with a key.</summary>
    ///  <param name="AValueRules">The rule set describing the values.</param>
    ///  <remarks>This method creates a simple array-based sorted list. This list is associated with a key and stores the map's
    ///  values for that key.</remarks>
    function CreateList(const AValueRules: TRules<TValue>): IList<TValue>; override;
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
  end;

  ///  <summary>The generic <c>multi-map</c> collection designed to store objects.</summary>
  ///  <remarks>This type uses a <c>sorted dictionary</c> and a number of <c>sorted lists</c> to store its
  ///  keys and values.</remarks>
  TObjectDoubleSortedMultiMap<TKey, TValue> = class(TDoubleSortedMultiMap<TKey, TValue>)
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
    ///  <remarks>This property specifies the way the map controls the life-time of the stored keys. The value of this property has effect only
    ///  if the keys are objects, otherwise it is ignored.</remarks>
    property OwnsKeys: Boolean read FOwnsKeys write FOwnsKeys;

    ///  <summary>Specifies whether this map owns the values.</summary>
    ///  <returns><c>True</c> if the map owns the values; <c>False</c> otherwise.</returns>
    ///  <remarks>This property specifies the way the map controls the life-time of the stored values. The value of this property has effect only
    ///  if the values are objects, otherwise it is ignored.</remarks>
    property OwnsValues: Boolean read FOwnsValues write FOwnsValues;
  end;

type
  ///  <summary>The generic <c>multi-map</c> collection.</summary>
  ///  <remarks>This type uses a <c>sorted dictionary</c> and a number of <c>sorted sets</c> to store its
  ///  keys and values.</remarks>
  TDoubleSortedDistinctMultiMap<TKey, TValue> = class(TSortedDistinctMultiMap<TKey, TValue>)
  private
    FAscValues: Boolean;

  protected
    ///  <summary>Called when the map needs to initialize a set associated with a key.</summary>
    ///  <param name="AValueRules">The rule set describing the values.</param>
    ///  <remarks>This method creates an AVL-based set. This set is associated with a key and stores the map's
    ///  values for that key.</remarks>
    function CreateSet(const AValueRules: TRules<TValue>): ISet<TValue>; override;
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
  end;

  ///  <summary>The generic <c>multi-map</c> collection designed to store objects.</summary>
  ///  <remarks>This type uses a <c>sorted dictionary</c> and a number of <c>sorted sets</c> to store its
  ///  keys and values.</remarks>
  TObjectDoubleSortedDistinctMultiMap<TKey, TValue> = class(TDoubleSortedDistinctMultiMap<TKey, TValue>)
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
    ///  <remarks>This property specifies the way the map controls the life-time of the stored keys. The value of this property has effect only
    ///  if the keys are objects, otherwise it is ignored.</remarks>
    property OwnsKeys: Boolean read FOwnsKeys write FOwnsKeys;

    ///  <summary>Specifies whether this map owns the values.</summary>
    ///  <returns><c>True</c> if the map owns the values; <c>False</c> otherwise.</returns>
    ///  <remarks>This property specifies the way the map controls the life-time of the stored values. The value of this property has effect only
    ///  if the values are objects, otherwise it is ignored.</remarks>
    property OwnsValues: Boolean read FOwnsValues write FOwnsValues;
  end;

implementation


{ TAbstractMultiMap<TKey, TValue> }

procedure TAbstractMultiMap<TKey, TValue>.Add(const APair: TPair<TKey, TValue>);
begin
  { Call the other add }
  Add(APair.Key, APair.Value);
end;

procedure TAbstractMultiMap<TKey, TValue>.Add(const AKey: TKey; const AValue: TValue);
var
  LList: IList<TValue>;
begin
  { Try to look-up what we need. Create a new LList and add it if required. }
  if not FDictionary.TryGetValue(AKey, LList) then
  begin
    LList := CreateList(ValueRules);
    FDictionary[AKey] := LList;
  end;

  { Add the new element to the LList }
  LList.Add(AValue);

  { Increase the version }
  Inc(FKnownCount);
  Inc(FVer);
end;

procedure TAbstractMultiMap<TKey, TValue>.Clear;
begin
  { Simply clear out the dictionary }
  if Assigned(FDictionary) then
    FDictionary.Clear();

  { Increase the version }
  FKnownCount := 0;
  Inc(FVer);
end;

function TAbstractMultiMap<TKey, TValue>.ContainsKey(const AKey: TKey): Boolean;
begin
  { Delegate to the dictionary object }
  Result := FDictionary.ContainsKey(AKey);
end;

function TAbstractMultiMap<TKey, TValue>.ContainsValue(const AKey: TKey; const AValue: TValue): Boolean;
var
  LList: IList<TValue>;
begin
  { Try to find .. otherwise fail! }
  if FDictionary.TryGetValue(AKey, LList) then
    Result := LList.Contains(AValue)
  else
    Result := false;
end;

function TAbstractMultiMap<TKey, TValue>.ContainsValue(const APair: TPair<TKey, TValue>): Boolean;
begin
  { Call upper function }
  Result := ContainsValue(APair.Key, APair.Value);
end;

function TAbstractMultiMap<TKey, TValue>.ContainsValue(const AValue: TValue): Boolean;
var
  LList: IList<TValue>;
begin
  { Iterate over the dictionary }
  for LList in FDictionary.Values do
  begin
    { Is there anything there? }
    if LList.Contains(AValue) then
      Exit(true);
  end;

  { Nothing found }
  Result := false;
end;

procedure TAbstractMultiMap<TKey, TValue>.CopyTo(var AArray: array of TPair<TKey, TValue>; const AStartIndex: NativeInt);
var
  LKey: TKey;
  LList: IList<TValue>;
  X, I: NativeInt;
begin
  { Check for indexes }
  if (AStartIndex >= Length(AArray)) or (AStartIndex < 0) then
    ExceptionHelper.Throw_ArgumentOutOfRangeError('AStartIndex');

  if (Length(AArray) - AStartIndex) < Count then
     ExceptionHelper.Throw_ArgumentOutOfSpaceError('AArray');

  X := AStartIndex;

  { Iterate over all lists and copy thtm to array }
  for LKey in FDictionary.Keys do
  begin
    LList := FDictionary[LKey];

    if LList.Count > 0 then
      for I := 0 to LList.Count - 1 do
      begin
        AArray[X + I].Key := LKey;
        AArray[X + I].Value := LList[I];
      end;

    Inc(X, LList.Count);
  end;
end;

constructor TAbstractMultiMap<TKey, TValue>.Create;
begin
  Create(TRules<TKey>.Default, TRules<TValue>.Default);
end;

constructor TAbstractMultiMap<TKey, TValue>.Create(
  const ACollection: IEnumerable<TPair<TKey, TValue>>);
begin
  Create(TRules<TKey>.Default, TRules<TValue>.Default, ACollection);
end;

constructor TAbstractMultiMap<TKey, TValue>.Create(
  const AKeyRules: TRules<TKey>;
  const AValueRules: TRules<TValue>);
begin
  { Install the types }
  inherited Create(AKeyRules, AValueRules);

  { Create the dictionary }
  FDictionary := CreateDictionary(KeyRules);

  FKeyCollection := TKeyCollection.Create(Self);
  FValueCollection := TValueCollection.Create(Self);

  { Create an internal empty list }
  FEmptyList := CreateList(ValueRules);

  FKnownCount := 0;
  FVer := 0;
end;

constructor TAbstractMultiMap<TKey, TValue>.Create(const AKeyRules: TRules<TKey>;
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

constructor TAbstractMultiMap<TKey, TValue>.Create(
  const AArray: array of TPair<TKey, TValue>);
begin
  Create(TRules<TKey>.Default, TRules<TValue>.Default, AArray);
end;

constructor TAbstractMultiMap<TKey, TValue>.Create(
  const AKeyRules: TRules<TKey>;
  const AValueRules: TRules<TValue>;
  const AArray: array of TPair<TKey, TValue>);
var
  I: NativeInt;
begin
  { Call upper constructor }
  Create(AKeyRules, AValueRules);

  { Copy all items in }
  for I := 0 to Length(AArray) - 1 do
    Add(AArray[I]);
end;

destructor TAbstractMultiMap<TKey, TValue>.Destroy;
begin
  { Clear first }
  Clear();

  inherited;
end;

function TAbstractMultiMap<TKey, TValue>.GetCount: NativeInt;
begin
  Result := FKnownCount;
end;

function TAbstractMultiMap<TKey, TValue>.GetEnumerator: IEnumerator<TPair<TKey, TValue>>;
begin
  Result := TPairEnumerator.Create(Self);
end;

function TAbstractMultiMap<TKey, TValue>.GetItemList(const AKey: TKey): IEnexIndexedCollection<TValue>;
var
  LList: IList<TValue>;
begin
  if not FDictionary.TryGetValue(AKey, LList) then
    ExceptionHelper.Throw_KeyNotFoundError('AKey');

  Result := LList;
end;

function TAbstractMultiMap<TKey, TValue>.KeyHasValue(const AKey: TKey; const AValue: TValue): Boolean;
begin
  Result := ContainsValue(AKey, AValue);
end;

procedure TAbstractMultiMap<TKey, TValue>.Remove(const AKey: TKey; const AValue: TValue);
var
  LList: IList<TValue>;
begin
  { Simply remove the value from the LList at key }
  if FDictionary.TryGetValue(AKey, LList) then
  begin
    if LList.Contains(AValue) then
    begin
      LList.Remove(AValue);

      { Kill the LList for one element }
      if LList.Count = 0 then
        FDictionary.Remove(AKey);

      Dec(FKnownCount, 1);

      { Increase the version }
      Inc(FVer);
    end;
  end;
end;

procedure TAbstractMultiMap<TKey, TValue>.Remove(const APair: TPair<TKey, TValue>);
begin
  { Call upper function }
  Remove(APair.Key, APair.Value);
end;

function TAbstractMultiMap<TKey, TValue>.SelectKeys: IEnexCollection<TKey>;
begin
  Result := Keys;
end;

function TAbstractMultiMap<TKey, TValue>.SelectValues: IEnexCollection<TValue>;
begin
  Result := Values;
end;

function TAbstractMultiMap<TKey, TValue>.TryGetValues(const AKey: TKey): IEnexIndexedCollection<TValue>;
begin
  if not TryGetValues(AKey, Result) then
    Result := FEmptyList;
end;

function TAbstractMultiMap<TKey, TValue>.TryGetValues(const AKey: TKey;
  out AValues: IEnexIndexedCollection<TValue>): Boolean;
var
  LList: IList<TValue>;
begin
  { Use the internal stuff }
  Result := FDictionary.TryGetValue(AKey, LList);

  if Result then
    AValues := LList;
end;

function TAbstractMultiMap<TKey, TValue>.ValueForKey(const AKey: TKey): TValue;
begin
  Result := GetItemList(AKey)[0];
end;

procedure TAbstractMultiMap<TKey, TValue>.Remove(const AKey: TKey);
var
  LList: IList<TValue>;
begin
  if FDictionary.TryGetValue(AKey, LList) then
    Dec(FKnownCount, LList.Count);

  { Simply remove the element. The LList should be auto-magically collected also }
  FDictionary.Remove(AKey);

  { Increase the version }
  Inc(FVer);
end;

{ TAbstractMultiMap<TKey, TValue>.TPairEnumerator }

constructor TAbstractMultiMap<TKey, TValue>.TPairEnumerator.Create(const ADict: TAbstractMultiMap<TKey, TValue>);
begin
  { Initialize }
  FDict := ADict;
  KeepObjectAlive(FDict);

  FVer := ADict.FVer;

  { Get the enumerator }
  FListIndex := 0;
  FDictEnum := FDict.FDictionary.GetEnumerator();
  FList := nil;
end;

destructor TAbstractMultiMap<TKey, TValue>.TPairEnumerator.Destroy;
begin
  ReleaseObject(FDict);
  inherited;
end;

function TAbstractMultiMap<TKey, TValue>.TPairEnumerator.GetCurrent: TPair<TKey,TValue>;
begin
  if FVer <> FDict.FVer then
     ExceptionHelper.Throw_CollectionChangedError();

  Result := FValue;
end;

function TAbstractMultiMap<TKey, TValue>.TPairEnumerator.MoveNext: Boolean;
begin
  { Repeat until something happens }
  while True do
  begin
    if FVer <> FDict.FVer then
       ExceptionHelper.Throw_CollectionChangedError();

    { We're still in the same KV? }
    if Assigned(FList) and (FListIndex < FList.Count) then
    begin
      { Next element }
      FValue := TPair<TKey, TValue>.Create(FDictEnum.Current.Key, FList[FListIndex]);

      Inc(FListIndex);
      Result := true;

      Exit;
    end;

    { Get the next KV pair from the dictionary }
    Result := FDictEnum.MoveNext();
    if not Result then
    begin
      FList := nil;
      Exit;
    end;

    { Reset the list }
    FListIndex := 0;
    FList := FDictEnum.Current.Value;
  end;
end;

{ TAbstractMultiMap<TKey, TValue>.TKeyEnumerator }

constructor TAbstractMultiMap<TKey, TValue>.TKeyEnumerator.Create(const ADict: TAbstractMultiMap<TKey, TValue>);
begin
  { Initialize }
  FDict := ADict;
  KeepObjectAlive(FDict);

  FVer := ADict.FVer;
  FValue := default(TKey);

  { Create enumerator }
  FDictEnum := FDict.FDictionary.Keys.GetEnumerator();
end;

destructor TAbstractMultiMap<TKey, TValue>.TKeyEnumerator.Destroy;
begin
  ReleaseObject(FDict);
  inherited;
end;

function TAbstractMultiMap<TKey, TValue>.TKeyEnumerator.GetCurrent: TKey;
begin
  if FVer <> FDict.FVer then
     ExceptionHelper.Throw_CollectionChangedError();

  Result := FValue;
end;

function TAbstractMultiMap<TKey, TValue>.TKeyEnumerator.MoveNext: Boolean;
begin
  if FVer <> FDict.FVer then
     ExceptionHelper.Throw_CollectionChangedError();

  { Move next and get the value }
  Result := FDictEnum.MoveNext();
  if Result then
    FValue := FDictEnum.Current;
end;


{ TAbstractMultiMap<TKey, TValue>.TValueEnumerator }

constructor TAbstractMultiMap<TKey, TValue>.TValueEnumerator.Create(const ADict: TAbstractMultiMap<TKey, TValue>);
begin
  { Initialize }
  FDict := ADict;
  KeepObjectAlive(FDict);

  FVer := ADict.FVer;

  { Get the enumerator }
  FListIndex := 0;
  FDictEnum := FDict.FDictionary.Values.GetEnumerator();
  FList := nil;
end;

destructor TAbstractMultiMap<TKey, TValue>.TValueEnumerator.Destroy;
begin
  ReleaseObject(FDict);
  inherited;
end;

function TAbstractMultiMap<TKey, TValue>.TValueEnumerator.GetCurrent: TValue;
begin
  if FVer <> FDict.FVer then
     ExceptionHelper.Throw_CollectionChangedError();

  Result := FValue;
end;

function TAbstractMultiMap<TKey, TValue>.TValueEnumerator.MoveNext: Boolean;
begin
  { Repeat until something happens }
  while True do
  begin
    if FVer <> FDict.FVer then
       ExceptionHelper.Throw_CollectionChangedError();

    { We're still in the same KV? }
    if Assigned(FList) and (FListIndex < FList.Count) then
    begin
      { Next element }
      FValue := FList[FListIndex];

      Inc(FListIndex);
      Result := true;

      Exit;
    end;

    { Get the next KV pair from the dictionary }
    Result := FDictEnum.MoveNext();
    if not Result then
    begin
      FList := nil;
      Exit;
    end;

    { Reset the list }
    FListIndex := 0;
    FList := FDictEnum.Current;
  end;
end;

{ TAbstractMultiMap<TKey, TValue>.TKeyCollection }

constructor TAbstractMultiMap<TKey, TValue>.TKeyCollection.Create(const ADict: TAbstractMultiMap<TKey, TValue>);
begin
  inherited Create(ADict.KeyRules);

  { Initialize }
  FDict := ADict;
end;

destructor TAbstractMultiMap<TKey, TValue>.TKeyCollection.Destroy;
begin
  inherited;
end;

function TAbstractMultiMap<TKey, TValue>.TKeyCollection.Empty: Boolean;
begin
  Result := (FDict.FDictionary.Count = 0);
end;

function TAbstractMultiMap<TKey, TValue>.TKeyCollection.GetCount: NativeInt;
begin
  { Number of elements is the same as key }
  Result := FDict.FDictionary.Count;
end;

function TAbstractMultiMap<TKey, TValue>.TKeyCollection.GetEnumerator: IEnumerator<TKey>;
begin
  Result := TKeyEnumerator.Create(Self.FDict);
end;

procedure TAbstractMultiMap<TKey, TValue>.TKeyCollection.CopyTo(var AArray: array of TKey; const AStartIndex: NativeInt);
begin
  { Check for indexes }
  if (AStartIndex >= Length(AArray)) or (AStartIndex < 0) then
    ExceptionHelper.Throw_ArgumentOutOfRangeError('AStartIndex');

  if (Length(AArray) - AStartIndex) < FDict.FDictionary.Count then
     ExceptionHelper.Throw_ArgumentOutOfSpaceError('AArray');

  { Simply copy using the dictionary provided methods }
  FDict.FDictionary.Keys.CopyTo(AArray, AStartIndex);
end;

{ TAbstractMultiMap<TKey, TValue>.TValueCollection }

constructor TAbstractMultiMap<TKey, TValue>.TValueCollection.Create(const ADict: TAbstractMultiMap<TKey, TValue>);
begin
  inherited Create(ADict.ValueRules);

  { Initialize }
  FDict := ADict;
end;

destructor TAbstractMultiMap<TKey, TValue>.TValueCollection.Destroy;
begin
  inherited;
end;

function TAbstractMultiMap<TKey, TValue>.TValueCollection.Empty: Boolean;
begin
  Result := (FDict.FDictionary.Count = 0);
end;

function TAbstractMultiMap<TKey, TValue>.TValueCollection.GetCount: NativeInt;
begin
  { Number of elements is different; use the count provided by the dictionary }
  Result := FDict.Count;
end;

function TAbstractMultiMap<TKey, TValue>.TValueCollection.GetEnumerator: IEnumerator<TValue>;
begin
  Result := TValueEnumerator.Create(Self.FDict);
end;

procedure TAbstractMultiMap<TKey, TValue>.TValueCollection.CopyTo(var AArray: array of TValue; const AStartIndex: NativeInt);
var
  LList: IList<TValue>;
  X, I: NativeInt;
begin
  { Check for indexes }
  if (AStartIndex >= Length(AArray)) or (AStartIndex < 0) then
    ExceptionHelper.Throw_ArgumentOutOfRangeError('AStartIndex');

  if (Length(AArray) - AStartIndex) < FDict.Count then
     ExceptionHelper.Throw_ArgumentOutOfSpaceError('AArray');

  X := AStartIndex;

  { Iterate over all lists and copy them to array }
  for LList in FDict.FDictionary.Values do
  begin
    if LList.Count > 0 then
      for I := 0 to LList.Count - 1 do
        AArray[X + I] := LList[I];

    Inc(X, LList.Count);
  end;
end;

{ TAbstractDistinctMultiMap<TKey, TValue> }

procedure TAbstractDistinctMultiMap<TKey, TValue>.Add(const APair: TPair<TKey, TValue>);
begin
  { Call the other add }
  Add(APair.Key, APair.Value);
end;

procedure TAbstractDistinctMultiMap<TKey, TValue>.Add(const AKey: TKey; const AValue: TValue);
var
  LSet: ISet<TValue>;
begin
  { Try to look up what we need. Create a new list and add it if required. }
  if not FDictionary.TryGetValue(AKey, LSet) then
  begin
    LSet := CreateSet(ValueRules);
    FDictionary[AKey] := LSet;
  end;

  { Add the new element to the list }
  if not LSet.Contains(AValue) then
  begin
    LSet.Add(AValue);

    { Increase the version }
    Inc(FKnownCount);
    Inc(FVer);
  end;
end;

procedure TAbstractDistinctMultiMap<TKey, TValue>.Clear;
begin
  if Assigned(FDictionary) then
    FDictionary.Clear();

  { Increase the version }
  FKnownCount := 0;
  Inc(FVer);
end;

function TAbstractDistinctMultiMap<TKey, TValue>.ContainsKey(const AKey: TKey): Boolean;
begin
  { Delegate to the dictionary object }
  Result := FDictionary.ContainsKey(AKey);
end;

function TAbstractDistinctMultiMap<TKey, TValue>.ContainsValue(const AKey: TKey; const AValue: TValue): Boolean;
var
  LSet: ISet<TValue>;
begin
  { Try to find .. otherwise fail! }
  if FDictionary.TryGetValue(AKey, LSet) then
    Result := LSet.Contains(AValue)
  else
    Result := false;
end;

function TAbstractDistinctMultiMap<TKey, TValue>.ContainsValue(const APair: TPair<TKey, TValue>): Boolean;
begin
  { Call upper function }
  Result := ContainsValue(APair.Key, APair.Value);
end;

function TAbstractDistinctMultiMap<TKey, TValue>.ContainsValue(const AValue: TValue): Boolean;
var
  LSet: ISet<TValue>;
begin
  { Iterate over the dictionary }
  for LSet in FDictionary.Values do
  begin
    { Is there anything there? }
    if LSet.Contains(AValue) then
      Exit(true);
  end;

  { Nothing found }
  Result := false;
end;

procedure TAbstractDistinctMultiMap<TKey, TValue>.CopyTo(
  var AArray: array of TPair<TKey, TValue>; const AStartIndex: NativeInt);
var
  LKey: TKey;
  LValue: TValue;
  LSet: ISet<TValue>;
  X: NativeInt;
begin
  { Check for indexes }
  if (AStartIndex >= Length(AArray)) or (AStartIndex < 0) then
    ExceptionHelper.Throw_ArgumentOutOfRangeError('AStartIndex');

  if (Length(AArray) - AStartIndex) < Count then
     ExceptionHelper.Throw_ArgumentOutOfSpaceError('AArray');

  X := AStartIndex;

  { Iterate over all lists and copy them to array }
  for LKey in FDictionary.Keys do
  begin
    LSet := FDictionary[LKey];

    for LValue in LSet do
    begin
      AArray[X].Key := LKey;
      AArray[X].Value := LValue;

      Inc(X);
    end;
  end;
end;

constructor TAbstractDistinctMultiMap<TKey, TValue>.Create;
begin
  Create(TRules<TKey>.Default, TRules<TValue>.Default);
end;

constructor TAbstractDistinctMultiMap<TKey, TValue>.Create(
  const ACollection: IEnumerable<TPair<TKey, TValue>>);
begin
  Create(TRules<TKey>.Default, TRules<TValue>.Default, ACollection);
end;

constructor TAbstractDistinctMultiMap<TKey, TValue>.Create(
  const AKeyRules: TRules<TKey>;
  const AValueRules: TRules<TValue>);
begin
  { Install the types }
  inherited Create(AKeyRules, AValueRules);

  { Create the dictionary }
  FDictionary := CreateDictionary(KeyRules);

  FKeyCollection := TKeyCollection.Create(Self);
  FValueCollection := TValueCollection.Create(Self);

  FEmptySet := CreateSet(ValueRules);

  FKnownCount := 0;
  FVer := 0;
end;

constructor TAbstractDistinctMultiMap<TKey, TValue>.Create(const AKeyRules: TRules<TKey>;
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

constructor TAbstractDistinctMultiMap<TKey, TValue>.Create(
  const AArray: array of TPair<TKey, TValue>);
begin
  Create(TRules<TKey>.Default, TRules<TValue>.Default, AArray);
end;

constructor TAbstractDistinctMultiMap<TKey, TValue>.Create(
  const AKeyRules: TRules<TKey>;
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

destructor TAbstractDistinctMultiMap<TKey, TValue>.Destroy;
begin
  { Clear first }
  Clear();

  inherited;
end;

function TAbstractDistinctMultiMap<TKey, TValue>.GetCount: NativeInt;
begin
  Result := FKnownCount;
end;

function TAbstractDistinctMultiMap<TKey, TValue>.GetEnumerator: IEnumerator<TPair<TKey, TValue>>;
begin
  Result := TPairEnumerator.Create(Self);
end;

function TAbstractDistinctMultiMap<TKey, TValue>.GetItemList(const AKey: TKey): IEnexCollection<TValue>;
var
  LSet: ISet<TValue>;
begin
  if not FDictionary.TryGetValue(AKey, LSet) then
    ExceptionHelper.Throw_KeyNotFoundError('AKey');

  Result := LSet;
end;

function TAbstractDistinctMultiMap<TKey, TValue>.KeyHasValue(const AKey: TKey; const AValue: TValue): Boolean;
begin
  Result := ContainsValue(AKey, AValue);
end;

procedure TAbstractDistinctMultiMap<TKey, TValue>.Remove(const AKey: TKey; const AValue: TValue);
var
  LSet: ISet<TValue>;
begin
  { Simply remove the value from the list at key }
  if FDictionary.TryGetValue(AKey, LSet) then
  begin
    if LSet.Contains(AValue) then
    begin
      LSet.Remove(AValue);

      { Kill the list for one element }
      if LSet.Count = 0 then
        FDictionary.Remove(AKey);

      Dec(FKnownCount, 1);
    end;
  end;

  { Increase th version }
  Inc(FVer);
end;

procedure TAbstractDistinctMultiMap<TKey, TValue>.Remove(const APair: TPair<TKey, TValue>);
begin
  { Call upper function }
  Remove(APair.Key, APair.Value);
end;

function TAbstractDistinctMultiMap<TKey, TValue>.SelectKeys: IEnexCollection<TKey>;
begin
  Result := Keys;
end;

function TAbstractDistinctMultiMap<TKey, TValue>.SelectValues: IEnexCollection<TValue>;
begin
  Result := Values;
end;

function TAbstractDistinctMultiMap<TKey, TValue>.TryGetValues(
  const AKey: TKey): IEnexCollection<TValue>;
begin
  if not TryGetValues(AKey, Result) then
    Result := FEmptySet;
end;

function TAbstractDistinctMultiMap<TKey, TValue>.TryGetValues(const AKey: TKey;
  out AValues: IEnexCollection<TValue>): Boolean;
var
  LSet: ISet<TValue>;
begin
  { Use the internal stuff }
  Result := FDictionary.TryGetValue(AKey, LSet);

  if Result then
    AValues := LSet;
end;

function TAbstractDistinctMultiMap<TKey, TValue>.ValueForKey(const AKey: TKey): TValue;
begin
  Result := GetItemList(AKey).First;
end;

procedure TAbstractDistinctMultiMap<TKey, TValue>.Remove(const AKey: TKey);
var
  LSet: ISet<TValue>;
begin
  if FDictionary.TryGetValue(AKey, LSet) then
    Dec(FKnownCount, LSet.Count);

  { Simply remove the element. The list should be auto-magically collected also }
  FDictionary.Remove(AKey);

  { Increase th version }
  Inc(FVer);
end;


{ TAbstractDistinctMultiMap<TKey, TValue>.TPairEnumerator }

constructor TAbstractDistinctMultiMap<TKey, TValue>.TPairEnumerator.Create(const ADict: TAbstractDistinctMultiMap<TKey, TValue>);
begin
  { Initialize }
  FDict := ADict;
  KeepObjectAlive(FDict);

  FVer := ADict.FVer;

  { Get the enumerator }
  FDictEnum := FDict.FDictionary.GetEnumerator();
end;

destructor TAbstractDistinctMultiMap<TKey, TValue>.TPairEnumerator.Destroy;
begin
  ReleaseObject(FDict);
  inherited;
end;

function TAbstractDistinctMultiMap<TKey, TValue>.TPairEnumerator.GetCurrent: TPair<TKey,TValue>;
begin
  if FVer <> FDict.FVer then
     ExceptionHelper.Throw_CollectionChangedError();

  Result := FValue;
end;

function TAbstractDistinctMultiMap<TKey, TValue>.TPairEnumerator.MoveNext: Boolean;
begin
  { Repeat until something happens }
  while True do
  begin
    if FVer <> FDict.FVer then
       ExceptionHelper.Throw_CollectionChangedError();

    { We're still in the same KV? }
    if Assigned(FSetEnum) and FSetEnum.MoveNext() then
    begin
      { Next element }
      FValue.Key := FDictEnum.Current.Key;
      FValue.Value := FSetEnum.Current;

      Result := true;
      Exit;
    end;

    { Get the next KV pair from the dictionary }
    Result := FDictEnum.MoveNext();
    if not Result then
      Exit;

    { Reset the list }
    FSet := FDictEnum.Current.Value;
    FSetEnum := FSet.GetEnumerator();
  end;
end;

{ TAbstractDistinctMultiMap<TKey, TValue>.TKeyEnumerator }

constructor TAbstractDistinctMultiMap<TKey, TValue>.TKeyEnumerator.Create(const ADict: TAbstractDistinctMultiMap<TKey, TValue>);
begin
  { Initialize }
  FDict := ADict;
  KeepObjectAlive(FDict);

  FVer := ADict.FVer;
  FValue := default(TKey);

  { Create enumerator }
  FDictEnum := FDict.FDictionary.Keys.GetEnumerator();
end;

destructor TAbstractDistinctMultiMap<TKey, TValue>.TKeyEnumerator.Destroy;
begin
  ReleaseObject(FDict);
  inherited;
end;

function TAbstractDistinctMultiMap<TKey, TValue>.TKeyEnumerator.GetCurrent: TKey;
begin
  if FVer <> FDict.FVer then
     ExceptionHelper.Throw_CollectionChangedError();

  Result := FValue;
end;

function TAbstractDistinctMultiMap<TKey, TValue>.TKeyEnumerator.MoveNext: Boolean;
begin
  if FVer <> FDict.FVer then
     ExceptionHelper.Throw_CollectionChangedError();

  { Move next and get the value }
  Result := FDictEnum.MoveNext();
  if Result then
    FValue := FDictEnum.Current;
end;


{ TAbstractDistinctMultiMap<TKey, TValue>.TValueEnumerator }

constructor TAbstractDistinctMultiMap<TKey, TValue>.TValueEnumerator.Create(const ADict: TAbstractDistinctMultiMap<TKey, TValue>);
begin
  { Initialize }
  FDict := ADict;
  KeepObjectAlive(FDict);
  FVer := ADict.FVer;

  { Get the enumerator }
  FDictEnum := FDict.FDictionary.Values.GetEnumerator();
end;

destructor TAbstractDistinctMultiMap<TKey, TValue>.TValueEnumerator.Destroy;
begin
  ReleaseObject(FDict);
  inherited;
end;

function TAbstractDistinctMultiMap<TKey, TValue>.TValueEnumerator.GetCurrent: TValue;
begin
  if FVer <> FDict.FVer then
     ExceptionHelper.Throw_CollectionChangedError();

  Result := FValue;
end;

function TAbstractDistinctMultiMap<TKey, TValue>.TValueEnumerator.MoveNext: Boolean;
begin
  { Repeat until something happens }
  while True do
  begin
    if FVer <> FDict.FVer then
       ExceptionHelper.Throw_CollectionChangedError();

    { We're still in the same KV? }
    if Assigned(FSetEnum) and FSetEnum.MoveNext() then
    begin
      { Next element }
      FValue := FSetEnum.Current;

      Result := true;
      Exit;
    end;

    { Get the next KV pair from the dictionary }
    Result := FDictEnum.MoveNext();
    if not Result then
      Exit;

    { Reset the list }
    FSet := FDictEnum.Current;
    FSetEnum := FSet.GetEnumerator();
  end;
end;

{ TAbstractDistinctMultiMap<TKey, TValue>.TKeyCollection }

constructor TAbstractDistinctMultiMap<TKey, TValue>.TKeyCollection.Create(const ADict: TAbstractDistinctMultiMap<TKey, TValue>);
begin
  inherited Create(ADict.KeyRules);

  { Initialize }
  FDict := ADict;
end;

function TAbstractDistinctMultiMap<TKey, TValue>.TKeyCollection.Empty: Boolean;
begin
  Result := (FDict.FDictionary.Count = 0);
end;

function TAbstractDistinctMultiMap<TKey, TValue>.TKeyCollection.GetCount: NativeInt;
begin
  { Number of elements is the same as key }
  Result := FDict.FDictionary.Count;
end;

function TAbstractDistinctMultiMap<TKey, TValue>.TKeyCollection.GetEnumerator: IEnumerator<TKey>;
begin
  Result := TKeyEnumerator.Create(Self.FDict);
end;

procedure TAbstractDistinctMultiMap<TKey, TValue>.TKeyCollection.CopyTo(var AArray: array of TKey; const AStartIndex: NativeInt);
begin
  { Check for indexes }
  if (AStartIndex >= Length(AArray)) or (AStartIndex < 0) then
    ExceptionHelper.Throw_ArgumentOutOfRangeError('AStartIndex');

  if (Length(AArray) - AStartIndex) < FDict.FDictionary.Count then
     ExceptionHelper.Throw_ArgumentOutOfSpaceError('AArray');

  { Simply copy using the dictionary provided methods }
  FDict.FDictionary.Keys.CopyTo(AArray, AStartIndex);
end;

{ TAbstractDistinctMultiMap<TKey, TValue>.TValueCollection }

constructor TAbstractDistinctMultiMap<TKey, TValue>.TValueCollection.Create(const ADict: TAbstractDistinctMultiMap<TKey, TValue>);
begin
  inherited Create(ADict.ValueRules);

  { Initialize }
  FDict := ADict;
end;

function TAbstractDistinctMultiMap<TKey, TValue>.TValueCollection.Empty: Boolean;
begin
  Result := (FDict.FDictionary.Count = 0);
end;

function TAbstractDistinctMultiMap<TKey, TValue>.TValueCollection.GetCount: NativeInt;
begin
  { Number of elements is different; use the count provided by the dictionary }
  Result := FDict.Count;
end;

function TAbstractDistinctMultiMap<TKey, TValue>.TValueCollection.GetEnumerator: IEnumerator<TValue>;
begin
  Result := TValueEnumerator.Create(Self.FDict);
end;

procedure TAbstractDistinctMultiMap<TKey, TValue>.TValueCollection.CopyTo(var AArray: array of TValue; const AStartIndex: NativeInt);
var
  LSet: ISet<TValue>;
  LValue: TValue;
  X: NativeInt;
begin
  { Check for indexes }
  if (AStartIndex >= Length(AArray)) or (AStartIndex < 0) then
    ExceptionHelper.Throw_ArgumentOutOfRangeError('AStartIndex');

  if (Length(AArray) - AStartIndex) < FDict.Count then
     ExceptionHelper.Throw_ArgumentOutOfSpaceError('AArray');

  X := AStartIndex;

  { Iterate over all lists and copy thtm to array }
  for LSet in FDict.FDictionary.Values do
  begin
    for LValue in LSet do
    begin
      AArray[X] := LValue;
      Inc(X);
    end;
  end;
end;

{ TMultiMap<TKey, TValue> }

constructor TMultiMap<TKey, TValue>.Create(const AInitialCapacity: NativeInt);
begin
  FInitialCapacity := AInitialCapacity;
  inherited Create();
end;

constructor TMultiMap<TKey, TValue>.Create(
  const AKeyRules: TRules<TKey>; const AValueRules: TRules<TValue>; const AInitialCapacity: NativeInt);
begin
  FInitialCapacity := AInitialCapacity;
  inherited Create(AKeyRules, AValueRules);
end;

function TMultiMap<TKey, TValue>.CreateDictionary(const AKeyRules: TRules<TKey>): IDictionary<TKey, IList<TValue>>;
var
  LNewCapacity: NativeInt;
  LDictionary: TDictionary<TKey, IList<TValue>>;
begin
  { Create a simple dictionary }
  if FInitialCapacity <= 0 then
    LNewCapacity := CDefaultSize
  else
    LNewCapacity := FInitialCapacity;

  LDictionary := TDictionary<TKey, IList<TValue>>.Create(AKeyRules, TRules<IList<TValue>>.Default, LNewCapacity);
  LDictionary.KeyRemoveNotification := NotifyKeyRemoved;

  Result := LDictionary;
end;

function TMultiMap<TKey, TValue>.CreateList(const AValueRules: TRules<TValue>): IList<TValue>;
var
  LList: TList<TValue>;
begin
  { Create a simple list }
  LList := TList<TValue>.Create(AValueRules);
  LList.RemoveNotification := NotifyValueRemoved;

  Result := LList;
end;

{ TObjectMultiMap<TKey, TValue> }

procedure TObjectMultiMap<TKey, TValue>.HandleKeyRemoved(const AKey: TKey);
begin
  if FOwnsKeys then
    TObject(AKey).Free;
end;

procedure TObjectMultiMap<TKey, TValue>.HandleValueRemoved(const AValue: TValue);
begin
  if FOwnsValues then
    TObject(AValue).Free;
end;

{ TSortedMultiMap<TKey, TValue> }

constructor TSortedMultiMap<TKey, TValue>.Create(
  const AArray: array of TPair<TKey, TValue>; const AAscending: Boolean);
begin
  { Do the dew and continue }
  FAscSort := AAscending;
  inherited Create(AArray);
end;

constructor TSortedMultiMap<TKey, TValue>.Create(const AAscending: Boolean);
begin
  { Do the dew and continue }
  FAscSort := AAscending;
  inherited Create();
end;

constructor TSortedMultiMap<TKey, TValue>.Create(
  const ACollection: IEnumerable<TPair<TKey, TValue>>;
  const AAscending: Boolean);
begin
  { Do the dew and continue }
  FAscSort := AAscending;
  inherited Create(ACollection);
end;

constructor TSortedMultiMap<TKey, TValue>.Create(const AKeyRules: TRules<TKey>;
  const AValueRules: TRules<TValue>;
  const AArray: array of TPair<TKey, TValue>; const AAscending: Boolean);
begin
  { Do the dew and continue }
  FAscSort := AAscending;
  inherited Create(AKeyRules, AValueRules, AArray);
end;

constructor TSortedMultiMap<TKey, TValue>.Create(const AKeyRules: TRules<TKey>;
  const AValueRules: TRules<TValue>; const AAscending: Boolean);
begin
  { Do the dew and continue }
  FAscSort := AAscending;
  inherited Create(AKeyRules, AValueRules);
end;

constructor TSortedMultiMap<TKey, TValue>.Create(const AKeyRules: TRules<TKey>;
  const AValueRules: TRules<TValue>;
  const ACollection: IEnumerable<TPair<TKey, TValue>>;
  const AAscending: Boolean);
begin
  { Do the dew and continue }
  FAscSort := AAscending;
  inherited Create(AKeyRules, AValueRules, ACollection);
end;

function TSortedMultiMap<TKey, TValue>.CreateDictionary(const AKeyRules: TRules<TKey>): IDictionary<TKey, IList<TValue>>;
var
  LDictionary: TSortedDictionary<TKey, IList<TValue>>;
begin
  { Create a simple dictionary }
  LDictionary := TSortedDictionary<TKey, IList<TValue>>.Create(AKeyRules, TRules<IList<TValue>>.Default, FAscSort);
  LDictionary.KeyRemoveNotification := NotifyKeyRemoved;

  Result := LDictionary;
end;

function TSortedMultiMap<TKey, TValue>.CreateList(const AValueRules: TRules<TValue>): IList<TValue>;
var
  LList: TList<TValue>;
begin
  { Create a simple list }
  LList := TList<TValue>.Create(AValueRules);
  LList.RemoveNotification := NotifyValueRemoved;

  Result := LList;
end;

function TSortedMultiMap<TKey, TValue>.MaxKey: TKey;
begin
  Result := Dictionary.MaxKey;
end;

function TSortedMultiMap<TKey, TValue>.MinKey: TKey;
begin
  Result := Dictionary.MinKey;
end;

{ TObjectSortedMultiMap<TKey, TValue> }

procedure TObjectSortedMultiMap<TKey, TValue>.HandleKeyRemoved(const AKey: TKey);
begin
  if FOwnsKeys then
    TObject(AKey).Free;
end;

procedure TObjectSortedMultiMap<TKey, TValue>.HandleValueRemoved(const AValue: TValue);
begin
  if FOwnsValues then
    TObject(AValue).Free;
end;

{ TDistinctMultiMap<TKey, TValue> }

constructor TDistinctMultiMap<TKey, TValue>.Create(const AInitialCapacity: NativeInt);
begin
  FInitialCapacity := AInitialCapacity;
  inherited Create();
end;

constructor TDistinctMultiMap<TKey, TValue>.Create(
  const AKeyRules: TRules<TKey>; const AValueRules: TRules<TValue>; const AInitialCapacity: NativeInt);
begin
  FInitialCapacity := AInitialCapacity;
  inherited Create(AKeyRules, AValueRules);
end;

function TDistinctMultiMap<TKey, TValue>.CreateDictionary(const AKeyRules: TRules<TKey>): IDictionary<TKey, ISet<TValue>>;
var
  LNewCapacity: NativeInt;
  LDictionary: TDictionary<TKey, ISet<TValue>>;
begin
  { Create a simple dictionary }
  if FInitialCapacity <= 0 then
    LNewCapacity := CDefaultSize
  else
    LNewCapacity := FInitialCapacity;

  LDictionary := TDictionary<TKey, ISet<TValue>>.Create(AKeyRules, TRules<ISet<TValue>>.Default, LNewCapacity);
  LDictionary.KeyRemoveNotification := NotifyKeyRemoved;

  Result := LDictionary;
end;

function TDistinctMultiMap<TKey, TValue>.CreateSet(const AValueRules: TRules<TValue>): ISet<TValue>;
var
  LSet: THashSet<TValue>;
begin
  { Create a simple list }
  LSet := THashSet<TValue>.Create(AValueRules);
  LSet.RemoveNotification := NotifyValueRemoved;

  Result := LSet;
end;

{ TObjectDistinctMultiMap<TKey, TValue> }

procedure TObjectDistinctMultiMap<TKey, TValue>.HandleKeyRemoved(const AKey: TKey);
begin
  if FOwnsKeys then
    TObject(AKey).Free;
end;

procedure TObjectDistinctMultiMap<TKey, TValue>.HandleValueRemoved(const AValue: TValue);
begin
  if FOwnsValues then
    TObject(AValue).Free;
end;

{ TSortedDistinctMultiMap<TKey, TValue> }

constructor TSortedDistinctMultiMap<TKey, TValue>.Create(
  const AArray: array of TPair<TKey, TValue>; const AAscending: Boolean);
begin
  { Do the dew and continue }
  FAscSort := AAscending;
  inherited Create(AArray);
end;

constructor TSortedDistinctMultiMap<TKey, TValue>.Create(const AAscending: Boolean);
begin
  { Do the dew and continue }
  FAscSort := AAscending;
  inherited Create();
end;

constructor TSortedDistinctMultiMap<TKey, TValue>.Create(
  const ACollection: IEnumerable<TPair<TKey, TValue>>;
  const AAscending: Boolean);
begin
  { Do the dew and continue }
  FAscSort := AAscending;
  inherited Create(ACollection);
end;

constructor TSortedDistinctMultiMap<TKey, TValue>.Create(const AKeyRules: TRules<TKey>;
  const AValueRules: TRules<TValue>;
  const AArray: array of TPair<TKey, TValue>; const AAscending: Boolean);
begin
  { Do the dew and continue }
  FAscSort := AAscending;
  inherited Create(AKeyRules, AValueRules, AArray);
end;

constructor TSortedDistinctMultiMap<TKey, TValue>.Create(const AKeyRules: TRules<TKey>;
  const AValueRules: TRules<TValue>; const AAscending: Boolean);
begin
  { Do the dew and continue }
  FAscSort := AAscending;
  inherited Create(AKeyRules, AValueRules);
end;

constructor TSortedDistinctMultiMap<TKey, TValue>.Create(const AKeyRules: TRules<TKey>;
  const AValueRules: TRules<TValue>;
  const ACollection: IEnumerable<TPair<TKey, TValue>>;
  const AAscending: Boolean);
begin
  { Do the dew and continue }
  FAscSort := AAscending;
  inherited Create(AKeyRules, AValueRules, ACollection);
end;

function TSortedDistinctMultiMap<TKey, TValue>.CreateDictionary(const AKeyRules: TRules<TKey>): IDictionary<TKey, ISet<TValue>>;
var
  LDictionary: TSortedDictionary<TKey, ISet<TValue>>;
begin
  { Create a simple dictionary }
  LDictionary := TSortedDictionary<TKey, ISet<TValue>>.Create(AKeyRules, TRules<ISet<TValue>>.Default, FAscSort);
  LDictionary.KeyRemoveNotification := NotifyKeyRemoved;

  Result := LDictionary;
end;

function TSortedDistinctMultiMap<TKey, TValue>.CreateSet(const AValueRules: TRules<TValue>): ISet<TValue>;
var
  LSet: THashSet<TValue>;
begin
  { Create a simple list }
  LSet := THashSet<TValue>.Create(AValueRules);
  LSet.RemoveNotification := NotifyValueRemoved;

  Result := LSet;
end;

function TSortedDistinctMultiMap<TKey, TValue>.MaxKey: TKey;
begin
  Result := Dictionary.MaxKey;
end;

function TSortedDistinctMultiMap<TKey, TValue>.MinKey: TKey;
begin
  Result := Dictionary.MinKey;
end;

{ TObjectSortedDistinctMultiMap<TKey, TValue> }

procedure TObjectSortedDistinctMultiMap<TKey, TValue>.HandleKeyRemoved(const AKey: TKey);
begin
  if FOwnsKeys then
    TObject(AKey).Free;
end;

procedure TObjectSortedDistinctMultiMap<TKey, TValue>.HandleValueRemoved(const AValue: TValue);
begin
  if FOwnsValues then
    TObject(AValue).Free;
end;

{ TDoubleSortedMultiMap<TKey, TValue> }

constructor TDoubleSortedMultiMap<TKey, TValue>.Create(
  const AArray: array of TPair<TKey, TValue>; const AAscendingKeys,
  AAscendingValues: Boolean);
begin
  { Do da dew and continue! }
  FAscValues := AAscendingValues;
  inherited Create(AArray, AAscendingKeys);
end;

constructor TDoubleSortedMultiMap<TKey, TValue>.Create(const AAscendingKeys, AAscendingValues: Boolean);
begin
  { Do da dew and continue! }
  FAscValues := AAscendingValues;
  inherited Create(AAscendingKeys);
end;

constructor TDoubleSortedMultiMap<TKey, TValue>.Create(
  const ACollection: IEnumerable<TPair<TKey, TValue>>;
  const AAscendingKeys, AAscendingValues: Boolean);
begin
  { Do da dew and continue! }
  FAscValues := AAscendingValues;
  inherited Create(ACollection, AAscendingKeys);
end;

constructor TDoubleSortedMultiMap<TKey, TValue>.Create(
  const AKeyRules: TRules<TKey>; const AValueRules: TRules<TValue>;
  const AArray: array of TPair<TKey, TValue>; const AAscendingKeys,
  AAscendingValues: Boolean);
begin
  { Do da dew and continue! }
  FAscValues := AAscendingValues;
  inherited Create(AKeyRules, AValueRules, AArray, AAscendingKeys);
end;

constructor TDoubleSortedMultiMap<TKey, TValue>.Create(
  const AKeyRules: TRules<TKey>; const AValueRules: TRules<TValue>;
  const AAscendingKeys, AAscendingValues: Boolean);
begin
  { Do da dew and continue! }
  FAscValues := AAscendingValues;
  inherited Create(AKeyRules, AValueRules, AAscendingKeys);
end;

constructor TDoubleSortedMultiMap<TKey, TValue>.Create(
  const AKeyRules: TRules<TKey>; const AValueRules: TRules<TValue>;
  const ACollection: IEnumerable<TPair<TKey, TValue>>;
  const AAscendingKeys, AAscendingValues: Boolean);
begin
  { Do da dew and continue! }
  FAscValues := AAscendingValues;
  inherited Create(AKeyRules, AValueRules, ACollection, AAscendingKeys);
end;

function TDoubleSortedMultiMap<TKey, TValue>.CreateList(const AValueRules: TRules<TValue>): IList<TValue>;
var
  LList: TSortedList<TValue>;
begin
  { Create a simple list }
  LList := TSortedList<TValue>.Create(AValueRules, FAscValues);
  LList.RemoveNotification := NotifyValueRemoved;

  Result := LList;
end;

{ TObjectDoubleSortedMultiMap<TKey, TValue> }

procedure TObjectDoubleSortedMultiMap<TKey, TValue>.HandleKeyRemoved(const AKey: TKey);
begin
  if FOwnsKeys then
    TObject(AKey).Free;
end;

procedure TObjectDoubleSortedMultiMap<TKey, TValue>.HandleValueRemoved(const AValue: TValue);
begin
  if FOwnsValues then
    TObject(AValue).Free;
end;

{ TDoubleSortedDistinctMultiMap<TKey, TValue> }

constructor TDoubleSortedDistinctMultiMap<TKey, TValue>.Create(
  const AArray: array of TPair<TKey, TValue>; const AAscendingKeys,
  AAscendingValues: Boolean);
begin
  { Do da dew and continue! }
  FAscValues := AAscendingValues;
  inherited Create(AArray, AAscendingKeys);
end;

constructor TDoubleSortedDistinctMultiMap<TKey, TValue>.Create(const AAscendingKeys, AAscendingValues: Boolean);
begin
  { Do da dew and continue! }
  FAscValues := AAscendingValues;
  inherited Create(AAscendingKeys);
end;

constructor TDoubleSortedDistinctMultiMap<TKey, TValue>.Create(
  const ACollection: IEnumerable<TPair<TKey, TValue>>;
  const AAscendingKeys, AAscendingValues: Boolean);
begin
  { Do da dew and continue! }
  FAscValues := AAscendingValues;
  inherited Create(ACollection, AAscendingKeys);
end;

constructor TDoubleSortedDistinctMultiMap<TKey, TValue>.Create(
  const AKeyRules: TRules<TKey>; const AValueRules: TRules<TValue>;
  const AArray: array of TPair<TKey, TValue>; const AAscendingKeys,
  AAscendingValues: Boolean);
begin
  { Do da dew and continue! }
  FAscValues := AAscendingValues;
  inherited Create(AKeyRules, AValueRules, AArray, AAscendingKeys);
end;

constructor TDoubleSortedDistinctMultiMap<TKey, TValue>.Create(
  const AKeyRules: TRules<TKey>; const AValueRules: TRules<TValue>;
  const AAscendingKeys, AAscendingValues: Boolean);
begin
  { Do da dew and continue! }
  FAscValues := AAscendingValues;
  inherited Create(AKeyRules, AValueRules, AAscendingKeys);
end;

constructor TDoubleSortedDistinctMultiMap<TKey, TValue>.Create(
  const AKeyRules: TRules<TKey>; const AValueRules: TRules<TValue>;
  const ACollection: IEnumerable<TPair<TKey, TValue>>;
  const AAscendingKeys, AAscendingValues: Boolean);
begin
  { Do da dew and continue! }
  FAscValues := AAscendingValues;
  inherited Create(AKeyRules, AValueRules, ACollection, AAscendingKeys);
end;

function TDoubleSortedDistinctMultiMap<TKey, TValue>.CreateSet(const AValueRules: TRules<TValue>): ISet<TValue>;
var
  LSet: TSortedSet<TValue>;
begin
  { Create a simple list }
  LSet := TSortedSet<TValue>.Create(AValueRules, FAscValues);
  LSet.RemoveNotification := NotifyValueRemoved;

  Result := LSet;
end;

{ TObjectDoubleSortedDistinctMultiMap<TKey, TValue> }

procedure TObjectDoubleSortedDistinctMultiMap<TKey, TValue>.HandleKeyRemoved(const AKey: TKey);
begin
  if FOwnsKeys then
    TObject(AKey).Free;
end;

procedure TObjectDoubleSortedDistinctMultiMap<TKey, TValue>.HandleValueRemoved(const AValue: TValue);
begin
  if FOwnsValues then
    TObject(AValue).Free;
end;

end.

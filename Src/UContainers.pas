{
 * This Source Code Form is subject to the terms of the Mozilla Public License,
 * v. 2.0. If a copy of the MPL was not distributed with this file, You can
 * obtain one at https://mozilla.org/MPL/2.0/
 *
 * Copyright (C) 2010-2021, Peter Johnson (gravatar.com/delphidabbler).
 *
 * Provides various generic container classes and enumerators.
}


unit UContainers;


interface


uses
  // Delphi
  Classes,  // declare before Generics.Collections
  Generics.Defaults, Generics.Collections;

type

  {
  TSortedList:
    Represents an enumerable sorted list, with sorting defined by user. List
    items are accessible by index.
  }
  TSortedList<T> = class(TEnumerable<T>)
  strict private
    fList: TList<T>;                      // Maintains list
    fComparer: IComparer<T>;              // Object that compares list items
    fPermitDuplicates: Boolean;           // Value of PermitDuplicates property
    fOnNotify: TCollectionNotifyEvent<T>; // OnNotify event handler
    function GetCount: Integer;
      {Read accessor for Count property. Gets number of items in list.
        @return Number of items in list.
      }
    function GetItem(Idx: Integer): T;
      {Read accessor for Items[] property. Gets list item at a specified index
      in list.
        @param Idx [in] Index of required item.
        @return Item at given index in list.
      }
    procedure SetPermitDuplicates(const Value: Boolean);
      {Write accessor for PermitDuplicates property.
        @param Value [in] New property value.
        @except EListError raised if new value is False and list contains
          duplicate entries.
      }
    procedure NotificationHandler(Sender: TObject; const Item: T;
      Action: TCollectionNotification);
      {Handles underlying list's OnNotify event. Triggered when something in
      list changes. Passes event on to this class' own notification system.
        @param Sender [in] Instance of list object that triggers event. Ignored.
        @param Item [in] Item for which change has been detected.
        @param Action [in] Kind of action: add, remove or extract.
      }
  strict protected
    procedure Notify(const Item: T; Action: TCollectionNotification); virtual;
      {Triggers list's OnNotify event.
        @param Item [in] List item for which notification triggered.
        @param Action [in] Kind of notification: add, remove or extract.
      }
    function DoGetEnumerator: TEnumerator<T>; override;
      {Gets a new instance of list's enumerator object.
        @return Enumerator instance.
      }
  public
    constructor Create; overload;
      {Constructs new list object that uses default comparer.
      }
    constructor Create(const AComparer: IComparer<T>); overload;
      {Constructs new list object that uses a specified comparer.
        @param AComparer [in] Object used to compare list items. If nil, default
          comparer is used.
      }
    constructor Create(Collection: TEnumerable<T>); overload;
      {Constructs a new list object that uses the default comparer and adds all
      the items from an enumerable collection to it.
        @param Collection [in] Collection to be added.
      }
    constructor Create(const AComparer: IComparer<T>;
      Collection: TEnumerable<T>); overload;
      {Constructs a new list object that uses a specified comparer and adds all
      the items from an enumerable collection to it.
        @param AComparer [in] Object used to compare list items. If nil, default
          comparer is used.
        @param Collection [in] Collection to be added.
      }
    destructor Destroy; override;
      {Destroys list object.
      }
    function Add(const Item: T): Integer;
      {Adds a new item to the list.
        @param Item [in] Item to be added.
        @return Index of new item in list.
        @except EListError raised if Item is already in list when duplicates are
          not permitted.
      }
    procedure AddRange(const Values: array of T); overload;
      {Adds an array of values to the list.
        @param Values [in] Array to be added.
    }
    procedure AddRange(const Collection: IEnumerable<T>); overload;
      {Adds an enumerable collection to the list.
        @param Collection [in] Interface to collection to be added.
      }
    procedure AddRange(Collection: TEnumerable<T>); overload;
      {Adds an enumerable collection to the list.
        @param Collection [in] Collection to be added.
      }
    procedure Clear;
      {Clears all items from list.
      }
    function Find(const Item: T; out Index: Integer): Boolean;
      {Finds an item in list.
        @param Item [in] Item to find.
        @param Index [out] Index of Item in list if present.
        @return True if Item found in list, False if not.
      }
    function IndexOf(const Item: T): Integer;
      {Gets index of an item in list.
        @param Item [in] List item to find.
        @return Index of first occurence of Item in list or -1 if Item not in
          list.
      }
    function Contains(const Item: T): Boolean;
      {Checks if an item is in the list.
        @param Item [in] Item to be checked.
        @return True if at least one copy of Item is in list, False if not.
      }
    function IsEmpty: Boolean;
      {Checks if list is empty.
        @return True if list is empty, False if not.
      }
    function Remove(const Item: T): Integer;
      {Removes an item from list.
        @param Item [in] Item to be removed from list.
        @return Former index of removed item.
      }
    procedure Delete(Index: Integer);
      {Deletes item at a specified index from list.
        @param Index [in] Index of item to be deleted.
      }
    function ContainsDuplicates: Boolean;
      {Checks if list contains any duplicate items.
        @return True if list contains duplicates, False if not.
      }
    property Count: Integer read GetCount;
      {Number of items in list}
    property PermitDuplicates: Boolean read fPermitDuplicates
      write SetPermitDuplicates;
      {Determines whether duplicate list items are permitted}
    property Items[Idx: Integer]: T read GetItem; default;
      {Provides access to list items by index}
    property OnNotify: TCollectionNotifyEvent<T> read fOnNotify write fOnNotify;
      {Event triggered when something changes in list}
  end;

  {
  TSortedObjectList:
    Represents an enumerable sorted list of objects, with sorting defined by
    user. List can own the objects it contains, in which case it frees objects
    removed from the list.
  }
  TSortedObjectList<T: class> = class(TSortedList<T>)
  strict private
    fOwnsObjects: Boolean;  // Value of OwnsObjects property
  strict protected
    procedure Notify(const Item: T; Action: TCollectionNotification); override;
      {Triggers OnNotify event for item and frees item if OwnsObjects is true
      and item is being removed from list.
        @param Item [in] Object affected by notification.
        @param Action [in] Action that applies to Object.
      }
  public
    constructor Create(AOwnsObjects: Boolean); overload;
      {Constructs object list that owns objects in list and uses default
      comparer.
        @param AOwnsObjects [in] Whether list owns contained objects.
      }
    constructor Create(const AComparer: IComparer<T>;
      AOwnsObjects: Boolean); overload;
      {Constructs object list that owns objects in list and has user-specified
      comparer.
        @param AComparer [in] Object used to compare objects in list.
        @param AOwnsObjects [in] Whether list owns contained objects.
      }
    constructor Create(const AComparer: IComparer<T>;
      Collection: TEnumerable<T>; AOwnsObjects: Boolean); overload;
      {Constructs object list that owns objects in list, has user-specified
      comparer and adds all the items of an enumerable collection to the list.
        @param AComparer [in] Object used to compare objects in list. Pass nil
          to use default comparer
        @param Collection [in] Collection of objects to be added.
        @param AOwnsObjects [in] Whether list owns contained objects.
      }
    property OwnsObjects: Boolean read fOwnsObjects;
      {Specifies whether the list owns the objects it contains or not}
  end;

  {
  TSortedDictionary:
    Represents an enumerable dictionary that is sorted on the key according to a
    user defined sorting. Keys, values and key/value pairs are also accessible
    by index.
  }
  TSortedDictionary<TKey,TValue> = class(TEnumerable<TPair<TKey,TValue>>)
  public
    type
      {
      TKeyEnumerator:
        Enumerator for the dictionary's keys collection.
      }
      TKeyEnumerator = class(TEnumerator<TKey>)
      strict private
        fEnum: TEnumerator<TPair<TKey,TValue>>; // Underlying list enumerator
      strict protected
        function DoGetCurrent: TKey; override;
          {Gets current key in enumeration.
            @return Current key.
          }
        function DoMoveNext: Boolean; override;
          {Moves to next key in enumeration.
            @return True if there is a next key, False if at end of enumeration.
          }
      public
        constructor Create(AItems: TSortedList<TPair<TKey,TValue>>);
          {Constructs a new keys enumerator.
            @param AItems [in] Ordered list of key / value pairs in dictionary.
          }
        destructor Destroy; override;
          {Destroys enumerator object.
          }
      end;
      {
      TKeyCollection:
        Provides access to dictionary's keys as an indexable and enumerable
        collection.
      }
      TKeyCollection = class(TEnumerable<TKey>)
      strict private
        fItems: TSortedList<TPair<TKey,TValue>>; // List of key/value pairs
        function GetCount: Integer;
          {Read accessor for Count property.
            @return Number of keys in collection.
          }
        function GetItem(Idx: Integer): TKey;
          {Read accessor for Items[] property.
            @param Idx [in] Index of required key.
            @return Key at given index.
          }
      strict protected
        function DoGetEnumerator: TEnumerator<TKey>; override;
          {Gets a new instance of the collection's enumerator.
            @return Enumerator instance.
          }
      public
        constructor Create(AItems: TSortedList<TPair<TKey,TValue>>);
          {Constructs a new collection object for a given list of keys.
            @param AItems [in] List of key / value pairs.
          }
        function GetEnumerator: TKeyEnumerator; reintroduce;
          {Gets a new instance of the collection's enumerator.
            @return Enumerator instance.
          }
        property Items[Idx: Integer]: TKey read GetItem; default;
          {Array of keys in collection accessed by zero based index}
        property Count: Integer read GetCount;
          {Number of items in collection}
      end;
      {
      TValueEnumerator:
        Enumerator for the dictionary's values collection.
      }
      TValueEnumerator = class(TEnumerator<TValue>)
      strict private
        fEnum: TEnumerator<TPair<TKey,TValue>>; // Underlying list enumerator
      strict protected
        function DoGetCurrent: TValue; override;
          {Gets current value in enumeration.
            @return Current value.
          }
        function DoMoveNext: Boolean; override;
          {Moves to next value in enumeration.
            @return True if there is a next value, False if at end of
              enumeration.
          }
      public
        constructor Create(AItems: TSortedList<TPair<TKey,TValue>>);
          {Constructs a new values enumerator.
            @param AItems [in] Ordered list of key / value pairs in dictionary.
          }
        destructor Destroy; override;
          {Destroys enumerator object.
          }
      end;
      {
      TValueCollection:
        Provides access to dictionary's values as an indexable and enumerable
        collection.
      }
      TValueCollection = class(TEnumerable<TValue>)
      strict private
        fItems: TSortedList<TPair<TKey,TValue>>; // List of key/value pairs
        function GetCount: Integer;
          {Read accessor for Count property.
            @return Number of values in collection.
          }
        function GetItem(Idx: Integer): TValue;
          {Read accessor for Items[] property.
            @param Idx [in] Index of required value.
            @return Value at given index.
          }
      strict protected
        function DoGetEnumerator: TEnumerator<TValue>; override;
          {Gets a new instance of the collection's enumerator.
            @return Enumerator instance.
          }
      public
        constructor Create(AItems: TSortedList<TPair<TKey,TValue>>);
          {Constructs a new collection object for a given list of values.
            @param AItems [in] List of key / value pairs.
          }
        function GetEnumerator: TValueEnumerator; reintroduce;
          {Gets a new instance of the collection's enumerator.
            @return Enumerator instance.
          }
        property Items[Idx: Integer]: TValue read GetItem; default;
          {Array of values in collection accessed by zero based index}
        property Count: Integer read GetCount;
          {Number of items in collection}
      end;
      {
      TPairEnumerator:
        Helper type for the dictionary's enumerator.
      }
      TPairEnumerator = TEnumerator<TPair<TKey,TValue>>;
  strict private
    fList: TSortedList<TPair<TKey,TValue>>; // Maintains dictionary's entries
    fKeysCollection: TKeyCollection;        // Dictionary's Keys collection
    fValuesCollection: TValueCollection;    // Dictionary's Values collection
    fComparer: IComparer<TKey>;             // Compares dictionary keys
    fOnKeyNotify:                           // OnKeyNotify event handler
      TCollectionNotifyEvent<TKey>;
    fOnValueNotify:                         // OnValueNotify event handler
      TCollectionNotifyEvent<TValue>;
    function GetKeys: TKeyCollection;
      {Read accessor for Keys collection property. Creates collection if
      necessary.
        @return Keys collection object.
      }
    function GetValues: TValueCollection;
      {Read accessor for Values collection property. Creates collection if
      necessary.
        @return Values collection object.
      }
    function GetCount: Integer;
      {Read accessor for Count property.
        @return Number of items in dictionary.
      }
    function GetItem(const Key: TKey): TValue;
      {Read accessor for Items[] property.
        @param Key [in] Key for which value is required.
        @return Required value.
        @except EListError raised if Key is not in dictionary.
      }
    function GetItemByIndex(const Idx: Integer): TPair<TKey, TValue>;
      {Read accessor for ItemsByIndex[] property.
        @param Idx [in] Index of required key/value pair in dictionary.
        @return Key/value pair at given index.
      }
    procedure NotificationHandler(Sender: TObject;
      const Item: TPair<TKey,TValue>; Action: TCollectionNotification);
      {Handles underlying list's OnNotify event. Triggered when something in
      list changes. Passes event on to this class' own notification system.
        @param Sender [in] Instance of list object that triggers event. Ignored.
        @param Item [in] Item for which change has been detected.
        @param Action [in] Kind of action: add, remove or extract.
      }
  strict protected
    procedure KeyNotify(const Key: TKey;
       Action: TCollectionNotification); virtual;
      {Triggers OnKeyNotify event for key.
        @param Key [in] Key for which notification triggered.
        @param Action [in] Kind of notification: add, remove or extract.
      }
    procedure ValueNotify(const Value: TValue;
      Action: TCollectionNotification); virtual;
      {Triggers OnValueNotify event for value.
        @param Value [in] Value for which notification triggered.
        @param Action [in] Kind of notification: add, remove or extract.
      }
    function DoGetEnumerator: TEnumerator<TPair<TKey,TValue>>; override;
      {Gets a new instance of dictionary's enumerator object.
        @return Enumerator instance.
      }
  public
    constructor Create; overload;
      {Constructs new dictionary instance that uses default key comparisons.
      }
    constructor Create(const AComparer: IComparer<TKey>); overload;
      {Constructs new dictionary instance that uses a specified comparer.
        @param AComparer [in] Object to use to compare dictionary keys. If nil
          default comparer is used.
      }
    destructor Destroy; override;
      {Destroys dictionary object.
      }
    function Add(const Key: TKey; const Value: TValue): Integer; overload;
      {Adds a new key and value to dictionary.
        @param Key [in] New key.
        @param Value [in] New value to associated with key.
        @return Index at which new entry added to dictionary.
      }
    function Add(const Item: TPair<TKey,TValue>): Integer; overload;
      {Added a new key/value pair to dictionary.
        @return Index at which new entry added to dictionary.
      }
    procedure Clear;
      {Clears all entries from dictionary.
      }
    function Find(const Key: TKey; out Index: Integer): Boolean;
      {Finds index of a key in dictionary.
        @param Key [in] Key to find.
        @param Index [out] Set to index of Key if found.
        @return True if key is in dictionary, False if not.
      }
    function IndexOf(const Key: TKey): Integer;
      {Gets index of a key in dictionary.
        @param Key [in] Key to be found.
        @return Index of Key in dictionary or -1 if Key not in dictionary.
      }
    function Contains(const Key: TKey): Boolean;
      {Checks if dictionary contains a key.
        @param Key [in] Key to be checked.
        @return True if Key is in dictionary, False if not.
      }
    function IsEmpty: Boolean;
      {Checks if dictionary is empty.
        @return True if dictionary is empty, False if not.
      }
    function Remove(const Key: TKey): Integer;
      {Removes entry with a specified key from dictionary.
        @param Key [in] Key of entry to be removed.
        @return Index of item removed.
      }
    procedure Delete(Index: Integer);
      {Deletes dictionary entry at a specified index.
        @param Index [in] Index of item to be deleted.
      }
    function GetEnumerator: TPairEnumerator; reintroduce;
      {Gets a new instance of dictionary's enumerator.
        @return Enumerator instance.
      }
    property Count: Integer read GetCount;
      {Number of items in dictionary}
    property Items[const Key: TKey]: TValue read GetItem; default;
      {Dictionary's values accessed by key}
    property ItemsByIndex[const Idx: Integer]: TPair<TKey,TValue>
      read GetItemByIndex;
      {Dictionary's key/value pairs accessed by zero based index}
    property Keys: TKeyCollection read GetKeys;
      {Indexable and enumerable list of all dictionary keys}
    property Values: TValueCollection read GetValues;
      {Indexable and enumerable list of all dictionary values}
    property OnKeyNotify: TCollectionNotifyEvent<TKey> read fOnKeyNotify
      write fOnKeyNotify;
      {Event triggered when something changes in dictionary. Provides key of
      item that changed along with details of change}
    property OnValueNotify: TCollectionNotifyEvent<TValue> read fOnValueNotify
      write fOnValueNotify;
      {Event triggered when something changes in dictionary. Provides value of
      item that changed along with details of change}
  end;

  {
  TSortedObjectDictionary:
    Represents an enumerable dictionary that is sorted on the key according to a
    user defined sorting. Keys, values and key/value pairs are also accessible
    by index. If keys and / or values are objects they may optionally be owned
    by the dictionary and freed when removed.
    NOTE: If this class is instantiated with Ownerships = [] then its
    functionality is identical to TSortedDictionary. In particular the class
    can be instantiated with both TKey and TValue as non-objects.
  }
  TSortedObjectDictionary<TKey,TValue> = class(TSortedDictionary<TKey,TValue>)
  strict private
    fOwnerships: TDictionaryOwnerships; // Value of Ownerships property
  strict protected
    procedure KeyNotify(const Key: TKey;
       Action: TCollectionNotification); override;
      {Triggers OnKeyNotify event for key and frees key if Ownerships property
      specifies it and key is being removed from list.
        @param Key [in] Key for which notification triggered.
        @param Action [in] Kind of notification: add, remove or extract.
      }
    procedure ValueNotify(const Value: TValue;
      Action: TCollectionNotification); override;
      {Triggers OnValueNotify event for value and frees value if Ownerships
      property specifies it and value is being removed from list.
        @param Value [in] Value for which notification triggered.
        @param Action [in] Kind of notification: add, remove or extract.
      }
  public
    constructor Create(AOwnerships: TDictionaryOwnerships); overload;
      {Constructs dictionary where either keys or values or both are objects
      that are optionally owned by dictionary and uses default comparer.
        @param AOwnerships [in] Whether keys or values are owned by dictionary.
        @except Raises EInvalidCast if ownership is specified for a key or value
          that is not an object.
      }
    constructor Create(const AComparer: IComparer<TKey>;
      AOwnerships: TDictionaryOwnerships); overload;
      {Constructs dictionary where either keys or values or both are objects
      that are optionally owned by dictionary and has user-specified comparer.
        @param AComparer [in] Object used to compare objects in list.
        @param AOwnerships [in] Whether keys or values are owned by dictionary.
        @except Raises EInvalidCast if ownership is specified for a key or value
          that is not an object.
      }
    property Ownerships: TDictionaryOwnerships read fOwnerships;
      {Specifies whether the dictionary owns the keys and/or values if they are
      objects}
  end;

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


resourcestring
  // Error messages: must be in interface for parametised types
  sOrderedListDuplicateErr = 'Duplicate item not permitted';
  sOrderedListPermitDuplicatesError = 'List contains duplicates';
  sKeyNotFound = 'Key not found';
  sKeyNotObject = 'Key must be a TObject to be owned';
  sValueNotObject = 'Value must be a TObject to be owned';


implementation


uses
  // Project
  SysUtils, RTTI, TypInfo;


{ TSortedList<T> }

function TSortedList<T>.Add(const Item: T): Integer;
  {Adds a new item to the list.
    @param Item [in] Item to be added.
    @return Index of new item in list.
    @except EListError raised if Item is already in list when duplicates are
      not permitted.
  }
begin
  if Find(Item, Result) and not PermitDuplicates then
    raise EListError.Create(sOrderedListDuplicateErr);
  fList.Insert(Result, Item);
end;

procedure TSortedList<T>.AddRange(const Values: array of T);
var
  Item: T;
begin
  for Item in Values do
    Add(Item);
end;

procedure TSortedList<T>.AddRange(const Collection: IEnumerable<T>);
var
  Item: T;
begin
  for Item in Collection do
    Add(Item);
end;

procedure TSortedList<T>.AddRange(Collection: TEnumerable<T>);
var
  Item: T;
begin
  for Item in Collection do
    Add(Item);
end;

procedure TSortedList<T>.Clear;
  {Clears all items from list.
  }
begin
  fList.Clear;
end;

function TSortedList<T>.Contains(const Item: T): Boolean;
  {Checks if an item is in the list.
    @param Item [in] Item to be checked.
    @return True if at least one copy of Item is in list, False if not.
  }
var
  Dummy: Integer; // unused index of Item in list
begin
  Result := Find(Item, Dummy);
end;

function TSortedList<T>.ContainsDuplicates: Boolean;
  {Checks if list contains any duplicate items.
    @return True if list contains duplicates, False if not.
  }
var
  Idx: Integer; // loops thru list
begin
  Result := False;
  for Idx := 1 to Pred(fList.Count) do
    if fComparer.Compare(fList[Idx], fList[Idx - 1]) = 0 then
      Exit(True);
end;

constructor TSortedList<T>.Create(Collection: TEnumerable<T>);
begin
  Create(nil, Collection);
end;

constructor TSortedList<T>.Create(const AComparer: IComparer<T>;
  Collection: TEnumerable<T>);
begin
  Create(AComparer);
  if Assigned(Collection) then
    AddRange(Collection);
end;

constructor TSortedList<T>.Create;
  {Constructs new list object that uses default comparer.
  }
begin
  Create(nil, nil);
end;

constructor TSortedList<T>.Create(const AComparer: IComparer<T>);
  {Constructs new list object that uses a specified comparer.
    @param AComparer [in] Object used to compare list items. If nil, default
      comparer is used.
  }
begin
  inherited Create;
  if Assigned(AComparer) then
    fComparer := AComparer
  else
    fComparer := TComparer<T>.Default;
  fList := TList<T>.Create(fComparer);
  fList.OnNotify := NotificationHandler;
end;

procedure TSortedList<T>.Delete(Index: Integer);
  {Deletes item at a specified index from list.
    @param Index [in] Index of item to be deleted.
  }
begin
  fList.Delete(Index);
end;

destructor TSortedList<T>.Destroy;
  {Destroys list object.
  }
begin
  fList.Free;
  inherited;
end;

function TSortedList<T>.DoGetEnumerator: TEnumerator<T>;
  {Gets a new instance of list's enumerator object.
    @return Enumerator instance.
  }
begin
  Result := fList.GetEnumerator;
end;

function TSortedList<T>.Find(const Item: T; out Index: Integer): Boolean;
  {Finds an item in list.
    @param Item [in] Item to find.
    @param Index [out] Index of Item in list if present.
    @return True if Item found in list, False if not.
  }
begin
  Result := fList.BinarySearch(Item, Index);
end;

function TSortedList<T>.GetCount: Integer;
  {Read accessor for Count property. Gets number of items in list.
    @return Number of items in list.
  }
begin
  Result := fList.Count;
end;

function TSortedList<T>.GetItem(Idx: Integer): T;
  {Read accessor for Items[] property. Gets list item at a specified index in
  list.
    @param Idx [in] Index of required item.
    @return Item at given index in list.
  }
begin
  Result := fList[Idx];
end;

function TSortedList<T>.IndexOf(const Item: T): Integer;
  {Gets index of an item in list.
    @param Item [in] List item to find.
    @return Index of first occurence of Item in list or -1 if Item not in list.
  }
begin
  if not Find(Item, Result) then
    Result := -1;
end;

function TSortedList<T>.IsEmpty: Boolean;
  {Checks if list is empty.
    @return True if list is empty, False if not.
  }
begin
  Result := Count = 0;
end;

procedure TSortedList<T>.NotificationHandler(Sender: TObject; const Item: T;
  Action: TCollectionNotification);
  {Handles underlying list's OnNotify event. Triggered when something in list
  changes. Passes event on to this class' own notification system.
    @param Sender [in] Instance of list object that triggers event. Ignored.
    @param Item [in] Item for which change has been detected.
    @param Action [in] Kind of action: add, remove or extract.
  }
begin
  Notify(Item, Action);
end;

procedure TSortedList<T>.Notify(const Item: T;
  Action: TCollectionNotification);
  {Triggers list's OnNotify event.
    @param Item [in] List item for which notification triggered.
    @param Action [in] Kind of notification: add, remove or extract.
  }
begin
  if Assigned(fOnNotify) then
    fOnNotify(Self, Item, Action);
end;

function TSortedList<T>.Remove(const Item: T): Integer;
  {Removes an item from list.
    @param Item [in] Item to be removed from list.
    @return Former index of removed item.
  }
begin
  Result := fList.Remove(Item);
end;

procedure TSortedList<T>.SetPermitDuplicates(const Value: Boolean);
  {Write accessor for PermitDuplicates property.
    @param Value [in] New property value.
    @except EListError raised if new value is False and list contains
      duplicate entries.
  }
begin
  if not Value and ContainsDuplicates then
    raise EListError.Create(sOrderedListPermitDuplicatesError);
  fPermitDuplicates := Value;
end;

{ TSortedObjectList<T> }

constructor TSortedObjectList<T>.Create(AOwnsObjects: Boolean);
  {Constructs object list that owns objects in list and uses default comparer.
    @param AOwnsObjects [in] Whether list owns contained objects.
  }
begin
  Create(nil, AOwnsObjects);
end;

constructor TSortedObjectList<T>.Create(const AComparer: IComparer<T>;
  AOwnsObjects: Boolean);
  {Constructs object list that owns objects in list and has user-specified
  comparer.
    @param AComparer [in] Object used to compare objects in list.
    @param AOwnsObjects [in] Whether list owns contained objects.
  }
begin
  fOwnsObjects := AOwnsObjects;
  Create(AComparer);
end;

constructor TSortedObjectList<T>.Create(const AComparer: IComparer<T>;
  Collection: TEnumerable<T>; AOwnsObjects: Boolean);
begin
  Create(AComparer, AOwnsObjects);
  Add(Collection);
end;

procedure TSortedObjectList<T>.Notify(const Item: T;
  Action: TCollectionNotification);
  {Triggers OnNotify event for item and frees item if OwnsObjects is true and
  item is being removed from list.
    @param Item [in] Object affected by notification.
    @param Action [in] Action that applies to Object.
  }
begin
  inherited;
  if OwnsObjects and (Action = cnRemoved) then
    Item.Free;
end;

{ TSortedDictionary<TKey, TValue> }

function TSortedDictionary<TKey, TValue>.Add(const Key: TKey;
  const Value: TValue): Integer;
  {Adds a new key and value to dictionary.
    @param Key [in] New key.
    @param Value [in] New value to associated with key.
    @return Index at which new entry added to dictionary.
  }
begin
  Result := Add(TPair<TKey,TValue>.Create(Key, Value));
end;

function TSortedDictionary<TKey, TValue>.Add(
  const Item: TPair<TKey, TValue>): Integer;
  {Added a new key/value pair to dictionary.
    @return Index at which new entry added to dictionary.
  }
begin
  Result := fList.Add(Item);
end;

procedure TSortedDictionary<TKey, TValue>.Clear;
  {Clears all entries from dictionary.
  }
begin
  fList.Clear;
end;

function TSortedDictionary<TKey, TValue>.Contains(const Key: TKey): Boolean;
  {Checks if dictionary contains a key.
    @param Key [in] Key to be checked.
    @return True if Key is in dictionary, False if not.
  }
var
  Dummy: Integer; // Index of key in dictionary. Not used.
begin
  Result := Find(Key, Dummy);
end;

constructor TSortedDictionary<TKey, TValue>.Create(
  const AComparer: IComparer<TKey>);
  {Constructs new dictionary instance that uses a specified comparer.
    @param AComparer [in] Object to use to compare dictionary keys. If nil
      default comparer is used.
  }
begin
  inherited Create;
  // determine comparer
  if Assigned(AComparer) then
    fComparer := AComparer
  else
    fComparer := TComparer<TKey>.Default;
  // created ordered list for key value pair that compares only on key
  fList := TSortedList<TPair<TKey,TValue>>.Create(
    TComparer<TPair<TKey,TValue>>.Construct(
      function(const Left, Right: TPair<TKey,TValue>): Integer
      begin
        Result := fComparer.Compare(Left.Key, Right.Key);
      end
    )
  );
  // initialize properties
  fList.PermitDuplicates := False;
  fList.OnNotify := NotificationHandler;
end;

constructor TSortedDictionary<TKey, TValue>.Create;
  {Constructs new dictionary instance that uses default key comparisons.
  }
begin
  Create(nil);
end;

procedure TSortedDictionary<TKey, TValue>.Delete(Index: Integer);
  {Deletes dictionary entry at a specified index.
    @param Index [in] Index of item to be deleted.
  }
begin
  fList.Delete(Index);
end;

destructor TSortedDictionary<TKey, TValue>.Destroy;
  {Destroys dictionary object.
  }
begin
  fValuesCollection.Free;
  fKeysCollection.Free;
  fList.Free;
  inherited;
end;

function TSortedDictionary<TKey, TValue>.DoGetEnumerator:
  TEnumerator<TPair<TKey,TValue>>;
  {Gets a new instance of dictionary's enumerator object.
    @return Enumerator instance.
  }
begin
  Result := GetEnumerator;
end;

function TSortedDictionary<TKey, TValue>.Find(const Key: TKey;
  out Index: Integer): Boolean;
  {Finds index of a key in dictionary.
    @param Key [in] Key to find.
    @param Index [out] Set to index of Key if found.
    @return True if key is in dictionary, False if not.
  }
begin
  Result := fList.Find(TPair<TKey,TValue>.Create(Key, Default(TValue)), Index);
end;

function TSortedDictionary<TKey, TValue>.GetCount: Integer;
  {Read accessor for Count property.
    @return Number of items in dictionary.
  }
begin
  Result := fList.Count;
end;

function TSortedDictionary<TKey, TValue>.GetEnumerator: TPairEnumerator;
  {Gets a new instance of dictionary's enumerator.
    @return Enumerator instance.
  }
begin
  Result := fList.GetEnumerator;
end;

function TSortedDictionary<TKey, TValue>.GetItem(const Key: TKey): TValue;
  {Read accessor for Items[] property.
    @param Key [in] Key for which value is required.
    @return Required value.
    @except EListError raised if Key is not in dictionary.
  }
var
  Index: Integer; // index of Key in dictionary
begin
  if not Find(Key, Index) then
    raise EListError.Create(sKeyNotFound);
  Result := fList[Index].Value;
end;

function TSortedDictionary<TKey, TValue>.GetItemByIndex(
  const Idx: Integer): TPair<TKey, TValue>;
  {Read accessor for ItemsByIndex[] property.
    @param Idx [in] Index of required key/value pair in dictionary.
    @return Key/value pair at given index.
  }
begin
  Result := fList[Idx];
end;

function TSortedDictionary<TKey, TValue>.GetKeys: TKeyCollection;
  {Read accessor for Keys collection property. Creates collection if necessary.
    @return Keys collection object.
  }
begin
  if not Assigned(fKeysCollection) then
    fKeysCollection := TKeyCollection.Create(fList);
  Result := fKeysCollection;
end;

function TSortedDictionary<TKey, TValue>.GetValues: TValueCollection;
  {Read accessor for Values collection property. Creates collection if
  necessary.
    @return Values collection object.
  }
begin
  if not Assigned(fValuesCollection) then
    fValuesCollection := TValueCollection.Create(fList);
  Result := fValuesCollection;
end;

function TSortedDictionary<TKey, TValue>.IndexOf(const Key: TKey): Integer;
  {Gets index of a key in dictionary.
    @param Key [in] Key to be found.
    @return Index of Key in dictionary or -1 if Key not in dictionary.
  }
begin
  if not Find(Key, Result) then
    Exit(-1);
end;

function TSortedDictionary<TKey, TValue>.IsEmpty: Boolean;
  {Checks if dictionary is empty.
    @return True if dictionary is empty, False if not.
  }
begin
  Result := Count = 0;
end;

procedure TSortedDictionary<TKey, TValue>.KeyNotify(const Key: TKey;
  Action: TCollectionNotification);
  {Triggers OnKeyNotify event for key.
    @param Key [in] Key for which notification triggered.
    @param Action [in] Kind of notification: add, remove or extract.
  }
begin
  if Assigned(fOnKeyNotify) then
    fOnKeyNotify(Self, Key, Action);
end;

procedure TSortedDictionary<TKey, TValue>.NotificationHandler(Sender: TObject;
  const Item: TPair<TKey,TValue>; Action: TCollectionNotification);
  {Handles underlying list's OnNotify event. Triggered when something in list
  changes. Passes event on to this class' own notification system.
    @param Sender [in] Instance of list object that triggers event. Ignored.
    @param Item [in] Item for which change has been detected.
    @param Action [in] Kind of action: add, remove or extract.
  }
begin
  KeyNotify(Item.Key, Action);
  ValueNotify(Item.Value, Action);
end;

function TSortedDictionary<TKey, TValue>.Remove(const Key: TKey): Integer;
  {Removes entry with a specified key from dictionary.
    @param Key [in] Key of entry to be removed.
    @return Index of item removed.
  }
begin
  Result := fList.Remove(TPair<TKey,TValue>.Create(Key, Default(TValue)));
end;

procedure TSortedDictionary<TKey, TValue>.ValueNotify(const Value: TValue;
  Action: TCollectionNotification);
  {Triggers OnValueNotify event for value.
    @param Value [in] Value for which notification triggered.
    @param Action [in] Kind of notification: add, remove or extract.
  }
begin
  if Assigned(fOnValueNotify) then
    fOnValueNotify(Self, Value, Action);
end;

{ TSortedDictionary<TKey, TValue>.TKeyEnumerator }

constructor TSortedDictionary<TKey, TValue>.TKeyEnumerator.Create(
  AItems: TSortedList<TPair<TKey, TValue>>);
  {Constructs a new keys enumerator.
    @param AItems [in] Ordered list of key / value pairs in dictionary.
  }
begin
  inherited Create;
  // we simply use enumerator of list used to store key/value pairs and return
  // key component of pair
  fEnum := AItems.GetEnumerator;
end;

destructor TSortedDictionary<TKey, TValue>.TKeyEnumerator.Destroy;
  {Destroys enumerator object.
  }
begin
  fEnum.Free;
  inherited;
end;

function TSortedDictionary<TKey, TValue>.TKeyEnumerator.DoGetCurrent: TKey;
  {Gets current key in enumeration.
    @return Current key.
  }
begin
  Result := fEnum.Current.Key;
end;

function TSortedDictionary<TKey, TValue>.TKeyEnumerator.DoMoveNext: Boolean;
  {Moves to next key in enumeration.
    @return True if there is a next key, False if at end of enumeration.
  }
begin
  Result := fEnum.MoveNext;
end;

{ TSortedDictionary<TKey, TValue>.TKeyCollection }

constructor TSortedDictionary<TKey, TValue>.TKeyCollection.Create(
  AItems: TSortedList<TPair<TKey, TValue>>);
  {Constructs a new collection object for a given list of keys.
    @param AItems [in] List of key / value pairs.
  }
begin
  inherited Create;
  // We use the keys from the list of key/value pairs
  fItems := AItems;
end;

function TSortedDictionary<TKey, TValue>.TKeyCollection.DoGetEnumerator:
  TEnumerator<TKey>;
  {Gets a new instance of the collection's enumerator.
    @return Enumerator instance.
  }
begin
  Result := GetEnumerator;
end;

function TSortedDictionary<TKey, TValue>.TKeyCollection.GetCount: Integer;
  {Read accessor for Count property.
    @return Number of keys in collection.
  }
begin
  Result := fItems.Count;
end;

function TSortedDictionary<TKey, TValue>.TKeyCollection.GetEnumerator:
  TKeyEnumerator;
  {Gets a new instance of the collection's enumerator.
    @return Enumerator instance.
  }
begin
  Result := TKeyEnumerator.Create(fItems);
end;

function TSortedDictionary<TKey, TValue>.TKeyCollection.GetItem(
  Idx: Integer): TKey;
  {Read accessor for Items[] property.
    @param Idx [in] Index of required key.
    @return Key at given index.
  }
begin
  Result := fItems[Idx].Key;
end;

{ TSortedDictionary<TKey, TValue>.TValueEnumerator }

constructor TSortedDictionary<TKey, TValue>.TValueEnumerator.Create(
  AItems: TSortedList<TPair<TKey, TValue>>);
  {Constructs a new values enumerator.
    @param AItems [in] Ordered list of key / value pairs in dictionary.
  }
begin
  inherited Create;
  // we simply use enumerator of list used to store key/value pairs and return
  // value component of pair
  fEnum := AItems.GetEnumerator;
end;

destructor TSortedDictionary<TKey, TValue>.TValueEnumerator.Destroy;
  {Destroys enumerator object.
  }
begin
  fEnum.Free;
  inherited;
end;

function TSortedDictionary<TKey, TValue>.TValueEnumerator.DoGetCurrent: TValue;
  {Gets current value in enumeration.
    @return Current value.
  }
begin
  Result := fEnum.Current.Value;
end;

function TSortedDictionary<TKey, TValue>.TValueEnumerator.DoMoveNext: Boolean;
  {Moves to next value in enumeration.
    @return True if there is a next value, False if at end of enumeration.
  }
begin
  Result := fEnum.MoveNext;
end;

{ TSortedDictionary<TKey, TValue>.TValueCollection }

constructor TSortedDictionary<TKey, TValue>.TValueCollection.Create(
  AItems: TSortedList<TPair<TKey, TValue>>);
  {Constructs a new collection object for a given list of values.
    @param AItems [in] List of key / value pairs.
  }
begin
  inherited Create;
  //  We use the values from the list of key/value pairs
  fItems := AItems;
end;

function TSortedDictionary<TKey, TValue>.TValueCollection.DoGetEnumerator:
  TEnumerator<TValue>;
  {Gets a new instance of the collection's enumerator.
    @return Enumerator instance.
  }
begin
  Result := GetEnumerator;
end;

function TSortedDictionary<TKey, TValue>.TValueCollection.GetCount: Integer;
  {Read accessor for Count property.
    @return Number of values in collection.
  }
begin
  Result := fItems.Count;
end;

function TSortedDictionary<TKey, TValue>.TValueCollection.GetEnumerator:
  TValueEnumerator;
  {Gets a new instance of the collection's enumerator.
    @return Enumerator instance.
  }
begin
  Result := TValueEnumerator.Create(fItems);
end;

function TSortedDictionary<TKey, TValue>.TValueCollection.GetItem(
  Idx: Integer): TValue;
  {Read accessor for Items[] property.
    @param Idx [in] Index of required value.
    @return Value at given index.
  }
begin
  Result := fItems[Idx].Value;
end;

{ TSortedObjectDictionary<TKey, TValue> }

constructor TSortedObjectDictionary<TKey, TValue>.Create(
  AOwnerships: TDictionaryOwnerships);
  {Constructs dictionary where either keys or values or both are objects that
  are optionally owned by dictionary and uses default comparer.
    @param AOwnerships [in] Whether keys or values are owned by dictionary.
    @except Raises EInvalidCast if ownership is specified for a key or value
      that is not an object.
  }
begin
  Create(nil, AOwnerships);
end;

constructor TSortedObjectDictionary<TKey, TValue>.Create(
  const AComparer: IComparer<TKey>; AOwnerships: TDictionaryOwnerships);
  {Constructs dictionary where either keys or values or both are objects that
  are optionally owned by dictionary and has user-specified comparer.
    @param AComparer [in] Object used to compare objects in list.
    @param AOwnerships [in] Whether keys or values are owned by dictionary.
    @except Raises EInvalidCast if ownership is specified for a key or value
      that is not an object.
  }
var
  RTTICtx: TRTTIContext;  // context for accessing RTTI
begin
  Create(AComparer);
  // Check that any key or value specified in AOwnership is actually a class
  // type
  RTTICtx := TRttiContext.Create;
  try
    if (doOwnsKeys in AOwnerships) and
      (RTTICtx.GetType(TypeInfo(TKey)).TypeKind <> tkClass) then
      raise EInvalidCast.Create(sKeyNotObject);
    if (doOwnsValues in AOwnerships) and
      (RTTICtx.GetType(TypeInfo(TValue)).TypeKind <> tkClass) then
      raise EInvalidCast.Create(sValueNotObject);
  finally
    RTTICtx.Free;
  end;
  // Ownerships validated: record in property
  fOwnerships := AOwnerships;
end;

procedure TSortedObjectDictionary<TKey, TValue>.KeyNotify(const Key: TKey;
  Action: TCollectionNotification);
  {Triggers OnKeyNotify event for key and frees key if Ownerships property
  specifies it and key is being removed from list.
    @param Key [in] Key for which notification triggered.
    @param Action [in] Kind of notification: add, remove or extract.
  }
begin
  inherited;
  if (Action = cnRemoved) and (doOwnsKeys in fOwnerships) then
    TObject(Key).Free;
end;

procedure TSortedObjectDictionary<TKey, TValue>.ValueNotify(const Value: TValue;
  Action: TCollectionNotification);
  {Triggers OnValueNotify event for value and frees value if Ownerships property
  specifies it and value is being removed from list.
    @param Value [in] Value for which notification triggered.
    @param Action [in] Kind of notification: add, remove or extract.
  }
begin
  inherited;
  if (Action = cnRemoved) and (doOwnsValues in fOwnerships) then
    TObject(Value).Free;
end;

{ TArrayEnumerator<T> }

constructor TArrayEnumerator<T>.Create(const A: array of T);
var
  Idx: Integer;
begin
  inherited Create;
  SetLength(fArray, Length(A));
  for Idx := Low(A) to High(A) do
    fArray[Idx] := A[Idx];
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

end.


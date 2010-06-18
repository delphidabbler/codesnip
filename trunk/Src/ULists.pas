{
 * ULists.pas
 *
 * Defines various classes that maintain lists of data of various types.
 *
 * $Rev$
 * $Date$
 *
 * ***** BEGIN LICENSE BLOCK *****
 *
 * Version: MPL 1.1
 *
 * The contents of this file are subject to the Mozilla Public License Version
 * 1.1 (the "License"); you may not use this file except in compliance with the
 * License. You may obtain a copy of the License at http://www.mozilla.org/MPL/
 *
 * Software distributed under the License is distributed on an "AS IS" basis,
 * WITHOUT WARRANTY OF ANY KIND, either express or implied. See the License for
 * the specific language governing rights and limitations under the License.
 *
 * The Original Code is ULists.pas
 *
 * The Initial Developer of the Original Code is Peter Johnson
 * (http://www.delphidabbler.com/).
 *
 * Portions created by the Initial Developer are Copyright (C) 2009 Peter
 * Johnson. All Rights Reserved.
 *
 * Contributor(s)
 *   NONE
 *
 * ***** END LICENSE BLOCK *****
}


unit ULists;


interface


uses
  // Delphi
  Generics.Collections;


type

  {
  TObjectCompare:
    Type of comparison method used to order objects in TSortedObjectList
    objects.
      @param Obj1 [in] First object to be compared.
      @param Obj2 [in] Second object to be compared.
      @return -ve if Obj1 < Obj2, 0 if Obj1 = Obj2 or +ve if Obj1 > Obj2.
  }
  TObjectCompare = function(const Obj1, Obj2: TObject): Integer of object;

  {
  TSortedObjectList:
    Class that maintains a sorted list of objects. Caller must provide a
    comparison method that determines sort order of objects. Objects must be
    unique (i.e. comparison method must not evaluate to zero for two objects in
    the list which means that two distinct objects can be determined to be
    equal).
  }
  TSortedObjectList = class(TObject)
  strict private
    var
      fItems: TObjectList<TObject>; // Maintains list of objects
      fCompare: TObjectCompare;     // Comparison method.
    function GetItem(Idx: Integer): TObject;
      {Read accessor for Items property.
        @param Idx [in] Index of required item.
        @return Required object.
      }
    function GetCount: Integer;
      {Read accessor for Count property.
        @return Number of objects in list.
      }
    function GetOwnsObjects: Boolean;
      {Read accessor for OwnsObjects property.
        @return True list owns list item objects, False if not.
      }
  strict protected
    function Find(const SearchObj: TObject; out Index: Integer): Boolean;
      overload;
      {Finds an object in list.
        @param SearchObj [in] Object to be searched for.
        @param Index [out] Index of object in list if found, undefined if not
          found.
        @return True if object found, False if not.
      }
  public
    constructor Create(const OwnsObjects: Boolean;
      const CompareFn: TObjectCompare);
      {Constructor. Sets up object.
        @param OwnsObjects [in] Flag indicating if list owns the objects added
          to it. When True objects are freed when list is freed. When False
          objects are not freed.
        @param CompareFn [in] Method used to compare objects in list.
      }
    destructor Destroy; override;
      {Destructor. Tears down object. Frees object in list if list owns objects.
      }
    function Add(const Obj: TObject): Integer;
      {Adds an object to the list, maintaining list's sort order.
        @param Obj [in] Object to be added to list.
        @return Index where object was inserted in list.
        @except EBug raised if attempt made to add duplicate to list.
      }
    function Contains(const Obj: TObject): Boolean;
      {Checks if list contains an object.
        @param Obj [in] Object to be checked.
        @return True if Obj is in the list, False if not.
      }
    function Find(const SearchObj: TObject): TObject; overload;
      {Finds a matching object in list.
        @param SearchObj [in] Object to be searched for.
        @return Reference to found object or nil if not found. Returned object
          may or may not not be the same instance as SearchObj, may be a
          different object that compares the same.
      }
    function IndexOf(const SearchObj: TObject): Integer;
      {Gets index of a matching object in list.
        @param SearchObj [in] Object to be searched for.
        @return Index of matching object in list or -1 if not in list.
      }
    function GetEnumerator: TEnumerator<TObject>;
      {Creates an enumerator for this object.
        @return Reference to new enumerator. Caller is repsonsible for freeing
          this object.
      }
    property Items[Idx: Integer]: TObject read GetItem; default;
      {Indexed array of objects in list. Maintained in sort order}
    property Count: Integer read GetCount;
      {Number of objects in list}
    property OwnsObjects: Boolean read GetOwnsObjects;
      {Flag indicates whether list owns, and frees, objects in list}
  end;

  {
  TSortedObjectDictionary:
    Dictionary mapping key objects onto associated value objects. Keys are
    maintained in an order determined by compare method passed to constructor.
  }
  TSortedObjectDictionary = class(TObject)
  strict private
    var
      fKeys: TSortedObjectList;       // List of key objects
      fValues: TObjectList<TObject>;  // List of value objects
    function GetKey(Idx: Integer): TObject;
      {Read accessor for Keys[] property.
        @param Idx [in] Index of required key.
        @return Requested key object.
      }
    function GetValue(Key: TObject): TObject;
      {Read accessor for Values[] property. Retrieves value associated with a
      key.
        @param Key [in] Key associated with required value.
        @return Associated value or nil if Key is not found.
      }
    function GetCount: Integer;
      {Read accessor for Count property.
        @return Number of key / value pairs in dictionary.
      }
  public
    constructor Create(const KeyCompare: TObjectCompare;
      const OwnsKeys: Boolean = False; const OwnsValues: Boolean = False);
      {Constructor. Sets up object.
        @param KeyCompare [in] Method used to compare key objects in list.
        @param OwnsKeys [in] Whether dictionary owns key objects and frees them
          when deleted. If True no object may be used more than once, either as
          a key or a value, otherwise an access violation may occur.
        @param OwnsValues [in] Whether dictionary owns value objects and frees
          them when deleted. If True no object may be used more than once, either
          as a key or a value, otherwise an access violation may occur.
      }
    destructor Destroy; override;
      {Destructor. Tears down object and frees keys and / or values if required.
      }
    function Add(const Key, Value: TObject): Integer;
      {Adds a key/value pair to the dictionary, maintaining key sort order.
        @param Key [in] Object representing key.
        @param Value [in] Object value associated with key.
      }
    function IndexOf(const Key: TObject): Integer;
      {Gets index of a key in dictionary.
        @param Key [in] Key to locate in dictionary.
        @return Index of Key in dictionary or -1 if not found.
      }
    function KeyExists(const Key: TObject): Boolean;
      {Checks whether a key is present in dictionary.
        @param Key [in] Key to be checked.
        @return True if a matching key is present in dictionary, False if not.
      }
    function GetEnumerator: TEnumerator<TObject>;
      {Gets enumerator for dictionary keys.
        @return Required enumerator.
      }
    property Keys[Idx: Integer]: TObject read GetKey;
      {Indexed ordered array of dictionary keys}
    property Values[Key: TObject]: TObject read GetValue;
      {List of values indexed by keys}
    property Count: Integer read GetCount;
      {Number of entries in dictionary}
  end;


implementation


uses
  // Project
  UExceptions;


{ TSortedObjectList }

function TSortedObjectList.Add(const Obj: TObject): Integer;
  {Adds an object to the list, maintaining list's sort order.
    @param Obj [in] Object to be added to list.
    @return Index where object was inserted in list.
    @except EBug raised if attempt made to add duplicate to list.
  }
var
  Idx: Integer; // loops thru list
  Cmp: Integer; // result of comparison of snippet names
begin
  // Assume adding to end of list
  Result := Count;
  // Loop thru existing items searching for location to insert
  for Idx := 0 to Pred(Count) do
  begin
    // compare new item to current item
    Cmp := fCompare(Obj, Items[Idx]);
    if Cmp = 0 then
      // don't allow duplicates
      raise EBug.CreateFmt('%s.Add: Duplicate object', [ClassName]);
    if Cmp < 0 then
    begin
      // current item is greater than new one: insert before it
      Result := Idx;
      Break;
    end;
  end;
  // add the new item
  fItems.Insert(Result, Obj);
end;

function TSortedObjectList.Contains(const Obj: TObject): Boolean;
  {Checks if list contains an object.
    @param Obj [in] Object to be checked.
    @return True if Obj is in the list, False if not.
  }
var
  Dummy: Integer; // index of object in list: unused
begin
  Result := Find(Obj, Dummy);
end;

constructor TSortedObjectList.Create(const OwnsObjects: Boolean;
  const CompareFn: TObjectCompare);
  {Constructor. Sets up object.
    @param OwnsObjects [in] Flag indicating if list owns the objects added to
      it. When True objects are freed when list is freed. When False objects are
      not freed.
    @param CompareFn [in] Method used to compare objects in list.
  }
begin
  Assert(Assigned(CompareFn), ClassName + '.Create: CompareFn not assigned');
  inherited Create;
  fItems := TObjectList<TObject>.Create(OwnsObjects);
  fCompare := CompareFn;
end;

destructor TSortedObjectList.Destroy;
  {Destructor. Tears down object. Frees object in list if list owns objects.
  }
begin
  fItems.Free;  // frees contained objects if specified
  inherited;
end;

function TSortedObjectList.Find(const SearchObj: TObject): TObject;
  {Finds a matching object in list.
    @param SearchObj [in] Object to be searched for.
    @return Reference to found object or nil if not found. Returned object may
      or may not not be the same instance as SearchObj, may be a different
      object that compares the same.
  }
var
  Idx: Integer; // index of match of SearchObj in list if present
begin
  if Find(SearchObj, Idx) then
    Result := fItems[Idx]
  else
    Result := nil;
end;

function TSortedObjectList.Find(const SearchObj: TObject;
  out Index: Integer): Boolean;
  {Finds an object in list.
    @param SearchObj [in] Object to be searched for.
    @param Index [out] Index of object in list if found, undefined if not found.
    @return True if object found, False if not.
  }
var
  Low, High: Integer;   // low and high bounds of search
  Cur: Integer;         // current item to be tested
  Cmp: Integer;         // result of comparing two items
begin
  // Uses binary search: based on TStringList.Find from Delphi Classes unit
  // Initialise
  Result := False;
  Low := 0;
  High := Count - 1;
  // Perform binary search
  while Low <= High do
  begin
    // Choose item to be compared: mid point of range
    Cur := (Low + High) shr 1;
    // Compare chosen
    Cmp := fCompare(Items[Cur], SearchObj);
    if Cmp < 0 then
      // Current item < required: search above current item
      Low := Cur + 1
    else
    begin
      // Current item >= required: search below current item
      High := Cur - 1;
      if Cmp = 0 then
      begin
        // We found it: set Low to Cur because Cur > High => loop will terminate
        Result := True;
        Low := Cur;
      end;
    end;
  end;
  // Set index (Low = Cur if found)
  Index := Low;
end;

function TSortedObjectList.GetCount: Integer;
  {Read accessor for Count property.
    @return Number of objects in list.
  }
begin
  Result := fItems.Count;
end;

function TSortedObjectList.GetEnumerator: TEnumerator<TObject>;
  {Creates an enumerator for this object.
    @return Reference to new enumerator. Caller is repsonsible for freeing
      this object.
  }
begin
  Result := fItems.GetEnumerator;
end;

function TSortedObjectList.GetItem(Idx: Integer): TObject;
  {Read accessor for Items property.
    @param Idx [in] Index of required item.
    @return Required object.
  }
begin
  Result := fItems[Idx];
end;

function TSortedObjectList.GetOwnsObjects: Boolean;
  {Read accessor for OwnsObjects property.
    @return True list owns list item objects, False if not.
  }
begin
  Result := fItems.OwnsObjects;
end;

function TSortedObjectList.IndexOf(const SearchObj: TObject): Integer;
  {Gets index of a matching object in list.
    @param SearchObj [in] Object to be searched for.
    @return Index of matching object in list or -1 if not in list.
  }
begin
  if not Find(SearchObj, Result) then
    Result := -1;
end;

{ TSortedObjectDictionary }

function TSortedObjectDictionary.Add(const Key, Value: TObject): Integer;
  {Adds a key/value pair to the dictionary, maintaining key sort order.
    @param Key [in] Object representing key.
    @param Value [in] Object value associated with key.
  }
begin
  Result := fKeys.Add(Key);
  fValues.Insert(Result, Value);
end;

constructor TSortedObjectDictionary.Create(const KeyCompare: TObjectCompare;
  const OwnsKeys, OwnsValues: Boolean);
  {Constructor. Sets up object.
    @param KeyCompare [in] Method used to compare key objects in list.
    @param OwnsKeys [in] Whether dictionary owns key objects and frees them
      when deleted. If True no object may be used more than once, either as
      a key or a value, otherwise an access violation may occur.
    @param OwnsValues [in] Whether dictionary owns value objects and frees
      them when deleted. If True no object may be used more than once, either
      as a key or a value, otherwise an access violation may occur.
  }
begin
  inherited Create;
  fKeys := TSortedObjectList.Create(OwnsKeys, KeyCompare);
  fValues := TObjectList<TObject>.Create(OwnsValues);
end;

destructor TSortedObjectDictionary.Destroy;
  {Destructor. Tears down object and frees keys and / or values if required.
  }
begin
  fValues.Free;
  fKeys.Free;
  inherited;
end;

function TSortedObjectDictionary.GetCount: Integer;
  {Read accessor for Count property.
    @return Number of key / value pairs in dictionary.
  }
begin
  Result := fKeys.Count;
end;

function TSortedObjectDictionary.GetEnumerator: TEnumerator<TObject>;
  {Gets enumerator for dictionary keys.
    @return Required enumerator.
  }
begin
  Result := fKeys.GetEnumerator;
end;

function TSortedObjectDictionary.GetKey(Idx: Integer): TObject;
  {Read accessor for Keys[] property.
    @param Idx [in] Index of required key.
    @return Requested key object.
  }
begin
  Result := fKeys[Idx];
end;

function TSortedObjectDictionary.GetValue(Key: TObject): TObject;
  {Read accessor for Values[] property. Retrieves value associated with a key.
    @param Key [in] Key associated with required value.
    @return Associated value or nil if Key is not found.
  }
var
  Index: Integer; // index of Key in list
begin
  Index := fKeys.IndexOf(Key);
  if Index >= 0 then
    Result := fValues[Index]
  else
    Result := nil;
end;

function TSortedObjectDictionary.IndexOf(const Key: TObject): Integer;
  {Gets index of a key in dictionary.
    @param Key [in] Key to locate in dictionary.
    @return Index of Key in dictionary or -1 if not found.
  }
begin
  Result := fKeys.IndexOf(Key);
end;

function TSortedObjectDictionary.KeyExists(const Key: TObject): Boolean;
  {Checks whether a key is present in dictionary.
    @param Key [in] Key to be checked.
    @return True if a matching key is present in dictionary, False if not.
  }
begin
  Result := fKeys.Contains(Key);
end;

end.


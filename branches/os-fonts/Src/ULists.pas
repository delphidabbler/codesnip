{
 * ULists.pas
 *
 * Defines various classes that maintain lists of data of various types.
 *
 * v1.0 of 25 May 2009  - Original version, containing TIntegerList only.
 *
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
 * ***** END LICENSE BLOCK *****
}


unit ULists;


interface


uses
  // Delphi
  Contnrs;


type

  {
  TIntegerList:
    Maintains a list of integers and associated objects.
  }
  TIntegerList = class(TObject)
  strict private
    type
      {
      TListItem:
        Wrapper object for items stored in list.
      }
      TListItem = class(TObject)
      strict private
        fValue: Integer;  // value of Value property
        fObj: TObject;    // value of Obj property
      public
        constructor Create(const Value: Integer; const Obj: TObject = nil);
          {Class constructor. Sets up object represent an integer and any
          associated object.
          }
        property Value: Integer read fValue;
          {Integer value of list item}
        property Obj: TObject read fObj;
          {Object associated with Value, or nil if no such object}
      end;
    type
      {
      TEnumerator:
        List enumerator.
      }
      TEnumerator = class(TObject)
      strict private
        fIndex: Integer;      // index of current item in enumeration
        fList: TIntegerList;  // list being eumerated
      public
        function GetCurrent: Integer;
          {Gets current integer in enumeration.
            @return Current integer.
          }
        function MoveNext: Boolean;
          {Moves to next item in enumeration.
            @return True if there is a next item, False if enumeration
              completed.
          }
        property Current: Integer read GetCurrent;
          {Current item in enumeration}
        constructor Create(const List: TIntegerList);
          {Class constructor. Sets up enumerator for a list.
            @param List [in] List to be enumerated.
          }
      end;
    var
      fItems: TObjectList;  // stores lost items
    function GetItem(Idx: Integer): Integer;
      {Read accessor for Items[] property.
        @param Idx [in] Index of required item.
        @return Requested integer.
      }
    function GetCount: Integer;
      {Read accessor for Count property.
        @return Number of items in list.
      }
    function GetObject(Idx: Integer): TObject;
      {Read accessor for Objects[] property.
        @param Idx [in] Index of required object.
        @return Requested object reference.
      }
  public
    constructor Create;
      {Class constructor. Sets up object.
      }
    destructor Destroy; override;
      {Class destructor Tears down object.
      }
    function Add(const Value: Integer; const Obj: TObject = nil): Integer;
      {Adds an integer with optional associated object to the list.
        @param Value [in] Integer to be added.
        @param Obj [in] Object associated with integer or nil.
        @return Index of new list item.
      }
    function IndexOf(const Value: Integer): Integer;
      {Finds index of an integer in list.
        @param Value [in] Value to be found.
        @return Index of Value in list, or -1 if not found.
      }
    function FindObject(const Value: Integer): TObject;
      {Finds object associated with a integer in list.
        @param Value [in] Integer value assoicated with object.
        @return Required object reference or nil if not found or no object
          associated with Value.
      }
    function GetEnumerator: TEnumerator;
      {Gets an enumerator for the list.
        @return Enumerator instance.
      }
    property Items[Idx: Integer]: Integer read GetItem; default;
      {Accesses all integers in list by index}
    property Objects[Idx: Integer]: TObject read GetObject;
      {Accesses all associated objects in list by index}
    property Count: Integer read GetCount;
      {Number of items in list}
  end;


implementation


uses
  // Delphi
  SysUtils;


{ TIntegerList }

function TIntegerList.Add(const Value: Integer; const Obj: TObject): Integer;
  {Adds an integer with optional associated object to the list.
    @param Value [in] Integer to be added.
    @param Obj [in] Object associated with integer or nil.
    @return Index of new list item.
  }
begin
  Result := fItems.Add(TListItem.Create(Value, Obj));
end;

constructor TIntegerList.Create;
  {Class constructor. Sets up object.
  }
begin
  inherited Create;
  fItems := TObjectList.Create(True);
end;

destructor TIntegerList.Destroy;
  {Class destructor Tears down object.
  }
begin
  FreeAndNil(fItems); // frees owned TListItem objects
  inherited;
end;

function TIntegerList.FindObject(const Value: Integer): TObject;
  {Finds object associated with a integer in list.
    @param Value [in] Integer value assoicated with object.
    @return Required object reference or nil if not found or no object
      associated with Value.
  }
var
  Idx: Integer; // loops thru list items
begin
  Idx := IndexOf(Value);
  if Idx >= 0 then
    Result := Objects[Idx]
  else
    Result := nil;
end;

function TIntegerList.GetCount: Integer;
  {Read accessor for Count property.
    @return Number of items in list.
  }
begin
  Result := fItems.Count;
end;

function TIntegerList.GetEnumerator: TEnumerator;
  {Gets an enumerator for the list.
    @return Enumerator instance.
  }
begin
  Result := TEnumerator.Create(Self);
end;

function TIntegerList.GetItem(Idx: Integer): Integer;
  {Read accessor for Items[] property.
    @param Idx [in] Index of required item.
    @return Requested integer.
  }
begin
  Result := (fItems[Idx] as TListItem).Value;
end;

function TIntegerList.GetObject(Idx: Integer): TObject;
  {Read accessor for Objects[] property.
    @param Idx [in] Index of required object.
    @return Requested object reference.
  }
begin
  Result := (fItems[Idx] as TListItem).Obj;
end;

function TIntegerList.IndexOf(const Value: Integer): Integer;
  {Finds index of an integer in list.
    @param Value [in] Value to be found.
    @return Index of Value in list, or -1 if not found.
  }
var
  Idx: Integer; // loops all items in list
begin
  Result := -1;
  for Idx := 0 to Pred(Count) do
  begin
    if Items[Idx] = Value then
    begin
      Result := Idx;
      Break;
    end;
  end;
end;

{ TIntegerList.TListItem }

constructor TIntegerList.TListItem.Create(const Value: Integer;
  const Obj: TObject);
  {Class constructor. Sets up object represent an integer and any associated
  object.
  }
begin
  inherited Create;
  fValue := Value;
  fObj := Obj;
end;

{ TIntegerList.TEnumerator }

constructor TIntegerList.TEnumerator.Create(const List: TIntegerList);
  {Class constructor. Sets up enumerator for a list.
    @param List [in] List to be enumerated.
  }
begin
  inherited Create;
  fList := List;
  fIndex := -1;
end;

function TIntegerList.TEnumerator.GetCurrent: Integer;
  {Gets current integer in enumeration.
    @return Current integer.
  }
begin
  Result := fList[fIndex];
end;

function TIntegerList.TEnumerator.MoveNext: Boolean;
  {Moves to next item in enumeration.
    @return True if there is a next item, False if enumeration completed.
  }
begin
  Result := fIndex < Pred(fList.Count);
  if Result then
    Inc(fIndex);
end;

end.


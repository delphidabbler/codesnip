{
 * USnippetIDs.pas
 *
 * Defines a record that uniquely indentifies a snippet in the database, along
 * with an object that maintains a list of snippet ID records.
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
 * The Original Code is USnippetIDs.pas
 *
 * The Initial Developer of the Original Code is Peter Johnson
 * (http://www.delphidabbler.com/).
 *
 * Portions created by the Initial Developer are Copyright (C) 2009-2010 Peter
 * Johnson. All Rights Reserved.
 *
 * Contributor(s)
 *   NONE
 *
 * ***** END LICENSE BLOCK *****
}


unit USnippetIDs;


interface


uses
  // Project
  IntfCommon, ULists;


type

  {
  TSnippetID:
    Record that uniquely identifies a code snippet. Specifies name and flag
    indicating if snippet is user-defined.
  }
  TSnippetID = record
    Name: string;           // Name of snippet
    UserDefined: Boolean;   // Whether snippet is user-defined
    constructor Create(const AName: string; const AUserDefined: Boolean);
      {Record constructor. Sets initial field values.
        @param AName [in] Required snippet name.
        @paran AUserDefined [in] Indicates if snippet is user defined.
      }
    constructor Clone(const Src: TSnippetID);
      {Clone constructor. Creates a snippet that is a copy of another record.
        @param Src [in] Record to copy.
      }
    function Compare(const SID: TSnippetID): Integer;
      {Compares this record to another one.
        @param SID [in] Record to be compared to this one.
        @return 0 if records are the same, -ve if this record is "less" than
          than SID or +ve if this record is greater than SID.
      }
    class operator Equal(const SID1, SID2: TSnippetID): Boolean;
      {Overload for = operator.
        @param SID1 [in] First record to be compared.
        @param SID2 [in] Second record to be compared.
        @return True if SID1 and SID2 are equal, False if not.
      }
    class operator NotEqual(const SID1, SID2: TSnippetID): Boolean;
      {Overload for <> operator.
        @param SID1 [in] First record to be compared.
        @param SID2 [in] Second record to be compared.
        @return True if SID1 and SID2 are not equal, False if equal.
      }
  end;

  {
  ISnippetIDListEnum:
    Interface supported by enumerator for ISnippetIDList.
  }
  ISnippetIDListEnum = interface(IInterface)
    ['{46B9CBA9-47BA-4A4C-8FAC-A5C56DA1EFE5}']
    function GetCurrent: TSnippetID;
      {Retrieves current snippet ID in the enumeration. Error if at end of
      enumeration.
        @return Current snippet ID.
      }
    function MoveNext: Boolean;
      {Moves to next item in enumeration if available.
        @return True if a next item is available, False if at end of enueration.
      }
    property Current: TSnippetID read GetCurrent;
      {Current snippet ID in the enumeration. Error if at end of enumeration}
  end;

  {
  ISnippetIDList:
    Interface supported by objects that implement a list of TSnippetID records.
  }
  ISnippetIDList = interface(IInterface)
    ['{238CFDCC-E84E-4D29-9BC6-10FBCECBC4FA}']
    function GetEnumerator: ISnippetIDListEnum;
      {Gets a new enumerator for the list.
        @return Reference to initialised enumerator.
      }
    procedure Clear;
      {Clears the list.
      }
    function Add(const SnippetID: TSnippetID): Integer;
      {Adds a snippet ID record to the list.
        @param SnippetID [in] Snippet ID to be added to the list.
        @return Index of added ID in list.
      }
    function Count: Integer;
      {Gets number of snippet ID records in the list.
        @return Number of items in list.
      }
    function GetItem(Idx: Integer): TSnippetID;
      {Gets a snippet ID from list by index.
        @param Idx [in] Index of required ID in list.
        @return Required item.
      }
    procedure SetItem(Idx: Integer; const Value: TSnippetID);
      {Stores a snippet ID in list at a specified index.
        @param Idx [in] Index where ID is to be stored.
        @param Value [in] Snippet ID to be stored.
      }
    property Items[Idx: Integer]: TSnippetID
      read GetItem write SetItem; default;
      {Provides access to IDs in list by index}
  end;

  {
  TSnippetIDList:
    Implements a list of snippet identification records.
  }
  TSnippetIDList = class(TInterfacedObject, ISnippetIDList, IAssignable)
  strict private
    var
      fList: TObjectListEx; // List of objects that wrap the routine ID records
    type
      {
      TSnippetIDWrapper:
        Provides a wrapper around TSnipperID records.
      }
      TSnippetIDWrapper = class(TObject)
      strict private
        var
          fSnippetID: TSnippetID; // Wrapped record
      public
        constructor Create(const SnippetID: TSnippetID);
          {Class constructor. Sets up the object to wrap a snippet ID record.
            @param SnippetID [in] Record to be wrapped.
          }
        property SnippetID: TSnippetID read fSnippetID write fSnippetID;
          {The wrapped snippet ID}
      end;
      {
      TEnumerator:
        Implements an enumerater for the snippet ID list.
      }
      TEnumerator = class(TInterfacedObject, ISnippetIDListEnum)
      strict private
        var
          fIndex: Integer;        // Index of current item in enumeration
          fList: ISnippetIDList;  // Reference to object being enumerated
      public
        constructor Create(const List: ISnippetIDList);
          {Class constructor. Sets up and initialises enumeration.
            @param List [in] Reference to object to be enumerated.
          }
        function GetCurrent: TSnippetID;
          {Gets current snippet ID in enumeration.
            @return Current ID.
          }
        function MoveNext: Boolean;
          {Moves to next item in enumeration.
            @return True if there is a next item, false if enumeration
              completed.
          }
      end;
  protected
    { ISnippetIDList methods }
    function GetEnumerator: ISnippetIDListEnum;
      {Gets a new enumerator for the list.
        @return Reference to initialised enumerator.
      }
    procedure Clear;
      {Clears the list.
      }
    function Add(const SnippetID: TSnippetID): Integer;
      {Adds a snippet ID to the list.
        @param SnippetID [in] Snippet ID to be added to the list.
        @return Index of added ID in list.
      }
    function Count: Integer;
      {Gets number of snippet IDs in the list.
        @return Number of items in list.
      }
    function GetItem(Idx: Integer): TSnippetID;
      {Gets a snippet ID from list by index.
        @param Idx [in] Index of required ID in list.
        @return Required item.
      }
    procedure SetItem(Idx: Integer; const Value: TSnippetID);
      {Stores a snippet ID in list at a specified index.
        @param Idx [in] Index where ID is to be stored.
        @param Value [in] Snippet ID to be stored.
      }
    { IAssignable methods }
    procedure Assign(const Src: IInterface);
      {Sets this list to be a copy of another list.
        @param Src [in] List to be assigned to this one. Must support
          ISnippetIDList.
      }
  public
    constructor Create;
      {Class constructor. Sets up empty list object.
      }
    destructor Destroy; override;
      {Class destructor. Tears down object.
      }
  end;


implementation


uses
  SysUtils, Classes {for inlining}, Windows {for inlining};


{ TSnippetID }

constructor TSnippetID.Clone(const Src: TSnippetID);
  {Clone constructor. Creates a snippet that is a copy of another record.
    @param Src [in] Record to copy.
  }
begin
  Create(Src.Name, Src.UserDefined);
end;

function TSnippetID.Compare(const SID: TSnippetID): Integer;
  {Compares this record to another one.
    @param SID [in] Record to be compared to this one.
    @return 0 if records are the same, -ve if this record is "less" than
      than SID or +ve if this record is greater than SID.
  }
begin
  Result := AnsiCompareText(Name, SID.Name);
  if Result = 0 then
    Result := Ord(UserDefined) - Ord(SID.UserDefined);
end;

constructor TSnippetID.Create(const AName: string; const AUserDefined: Boolean);
  {Record constructor. Sets initial field values.
    @param AName [in] Required snippet name.
    @paran AUserDefined [in] Indicates if snippet is user defined.
  }
begin
  Name := AName;
  UserDefined := AUserDefined;
end;

class operator TSnippetID.Equal(const SID1, SID2: TSnippetID): Boolean;
  {Overload for = operator.
    @param SID1 [in] First record to be compared.
    @param SID2 [in] Second record to be compared.
    @return True if SID1 and SID2 are equal, False if not.
  }
begin
  Result := SID1.Compare(SID2) = 0;
end;

class operator TSnippetID.NotEqual(const SID1, SID2: TSnippetID): Boolean;
  {Overload for <> operator.
    @param SID1 [in] First record to be compared.
    @param SID2 [in] Second record to be compared.
    @return True if SID1 and SID2 are not equal, False if equal.
  }
begin
  Result := not (SID1 = SID2);
end;

{ TSnippetIDList }

function TSnippetIDList.Add(const SnippetID: TSnippetID): Integer;
  {Adds a snippet ID to the list.
    @param SnippetID [in] Snippet ID to be added to the list.
    @return Index of added ID in list.
  }
begin
  Result := fList.Add(TSnippetIDWrapper.Create(SnippetID));
end;

procedure TSnippetIDList.Assign(const Src: IInterface);
  {Sets this list to be a copy of another list.
    @param Src [in] List to be assigned to this one. Must support
      ISnippetIDList.
  }
var
  SrcID: TSnippetID;  // references each ID in source
begin
  Assert(Supports(Src, ISnippetIDList),
    ClassName + '.Assign: Src must support ISnippetIDList');
  Clear;
  for SrcID in (Src as ISnippetIDList) do
    Add(TSnippetID.Clone(SrcID));
end;

procedure TSnippetIDList.Clear;
  {Clears the list.
  }
begin
  fList.Clear;
end;

function TSnippetIDList.Count: Integer;
  {Gets number of snippet IDs in the list.
    @return Number of items in list.
  }
begin
  Result := fList.Count;
end;

constructor TSnippetIDList.Create;
  {Class constructor. Sets up empty list object.
  }
begin
  inherited;
  fList := TObjectListEx.Create(True);
end;

destructor TSnippetIDList.Destroy;
  {Class destructor. Tears down object.
  }
begin
  FreeAndNil(fList);  // destroys contained objects
  inherited;
end;

function TSnippetIDList.GetEnumerator: ISnippetIDListEnum;
  {Gets a new enumerator for the list.
    @return Reference to initialised enumerator.
  }
begin
  Result := TEnumerator.Create(Self);
end;

function TSnippetIDList.GetItem(Idx: Integer): TSnippetID;
  {Gets a snippet ID from list by index.
    @param Idx [in] Index of required ID in list.
    @return Required item.
  }
begin
  Result := (fList[Idx] as TSnippetIDWrapper).SnippetID;
end;

procedure TSnippetIDList.SetItem(Idx: Integer; const Value: TSnippetID);
  {Stores a snippet ID in list at a specified index.
    @param Idx [in] Index where ID is to be stored.
    @param Value [in] Snippet ID to be stored.
  }
begin
  (fList[Idx] as TSnippetIDWrapper).SnippetID := Value;
end;

{ TSnippetIDList.TSnippetIDWrapper }

constructor TSnippetIDList.TSnippetIDWrapper.Create(
  const SnippetID: TSnippetID);
  {Class constructor. Sets up the object to wrap a snippet ID record.
    @param SnippetID [in] Record to be wrapped.
  }
begin
  inherited Create;
  fSnippetID := SnippetID;
end;

{ TSnippetIDList.TEnumerator }

constructor TSnippetIDList.TEnumerator.Create(const List: ISnippetIDList);
  {Class constructor. Sets up and initialises enumeration.
    @param List [in] Reference to object to be enumerated.
  }
begin
  inherited Create;
  fIndex := -1;
  fList := List;
end;

function TSnippetIDList.TEnumerator.GetCurrent: TSnippetID;
  {Gets current snippet ID in enumeration.
    @return Current ID.
  }
begin
  Result := fList[fIndex];
end;

function TSnippetIDList.TEnumerator.MoveNext: Boolean;
  {Moves to next item in enumeration.
    @return True if there is a next item, false if enumeration completed.
  }
begin
  Result := fIndex < Pred(fList.Count);
  if Result then
    Inc(fIndex);
end;

end.

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
  // Delphi
  Generics.Collections,
  // Project
  IntfCommon;


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
      {Clone constructor. Sets this snippet ID to be a copy of another record.
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
  ISnippetIDList:
    Interface supported by objects that implement a list of TSnippetID records.
  }
  ISnippetIDList = interface(IInterface)
    ['{238CFDCC-E84E-4D29-9BC6-10FBCECBC4FA}']
    function GetEnumerator: TEnumerator<TSnippetID>;
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
      fList: TList<TSnippetID>; // Internal list of snippet ID records
  protected
    { ISnippetIDList methods }
    function GetEnumerator: TEnumerator<TSnippetID>;
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
      {Constructor. Sets up empty list object.
      }
    destructor Destroy; override;
      {Destructor. Tears down object.
      }
  end;


implementation


uses
  // Delphi
  SysUtils, Generics.Defaults;


{ TSnippetID }

constructor TSnippetID.Clone(const Src: TSnippetID);
  {Clone constructor. Sets this snippet ID to be a copy of another record.
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
  Result := fList.Add(SnippetID);
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
  {Constructor. Sets up empty list object.
  }
begin
  inherited;
  fList := TList<TSnippetID>.Create(
    TDelegatedComparer<TSnippetID>.Create(
      function(const Left, Right: TSnippetID): Integer
      begin
        Result := Left.Compare(Right);
      end
    )
  );
end;

destructor TSnippetIDList.Destroy;
  {Destructor. Tears down object.
  }
begin
  fList.Free;
  inherited;
end;

function TSnippetIDList.GetEnumerator: TEnumerator<TSnippetID>;
  {Gets a new enumerator for the list.
    @return Reference to initialised enumerator.
  }
begin
  Result := fList.GetEnumerator;
end;

function TSnippetIDList.GetItem(Idx: Integer): TSnippetID;
  {Gets a snippet ID from list by index.
    @param Idx [in] Index of required ID in list.
    @return Required item.
  }
begin
  Result := fList[Idx];
end;

procedure TSnippetIDList.SetItem(Idx: Integer; const Value: TSnippetID);
  {Stores a snippet ID in list at a specified index.
    @param Idx [in] Index where ID is to be stored.
    @param Value [in] Snippet ID to be stored.
  }
begin
  fList[Idx] := Value;
end;

end.


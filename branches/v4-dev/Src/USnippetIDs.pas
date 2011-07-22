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
 * Portions created by the Initial Developer are Copyright (C) 2009-2011 Peter
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

  ///  <summary>
  ///  Record that uniquely identifies a code snippet. Specifies name and flag
  ///  indicating if snippet is user-defined.
  ///  </summary>
  TSnippetID = record
  strict private
    fName: string;           // Name of snippet
    fUserDefined: Boolean;   // Whether snippet is user-defined
  public
    ///  <summary>Name of snippet.</summary>
    property Name: string read fName write fName;
    ///  <summary>Whether snippet is user defined.</summary>
    property UserDefined: Boolean read fUserDefined write fUserDefined;
    ///  <summary>Record constructor. Sets initial field values from parameters.
    ///  </summary>
    constructor Create(const AName: string; const AUserDefined: Boolean);
    ///  <summary>Clone constructor. Makes this record a copy of another snippet
    ///  ID</summary>
    constructor Clone(const Src: TSnippetID);
    ///  <summary>Compares this record to another.</summary>
    ///  <remarks>Returns 0 if records are same, -ve if this record less than
    ///  other record or +ve if this record greater than other.</remarks>
    function Compare(const SID: TSnippetID): Integer;
    ///  <summary>Compares two snippet names, Left and Right.</summary>
    ///  <remarks>Returns 0 if names are same, -ve if Left is less than Right or
    ///  +ve Left is greater than Right.</remarks>
    class function CompareNames(const Left, Right: string): Integer; static;
    ///  <summary>Overload of equality operator.</summary>
    class operator Equal(const SID1, SID2: TSnippetID): Boolean;
    ///  <summary>Overload of inequality operator.</summary>
    class operator NotEqual(const SID1, SID2: TSnippetID): Boolean;
  end;

type
  ///  <summary>
  ///  Interface supported by objects that implement a list of TSnippetID
  ///  records.
  ///  </summary>
  ISnippetIDList = interface(IInterface)
    ['{238CFDCC-E84E-4D29-9BC6-10FBCECBC4FA}']
    ///  <summary>Gets new list enumerator.</summary>
    function GetEnumerator: TEnumerator<TSnippetID>;
    ///  <summary>Clears the list.</summary>
    procedure Clear;
    ///  <summary>Adds given snippet ID record to list.</summary>
    ///  <remarks>Returns index of new record in list.</remarks>
    function Add(const SnippetID: TSnippetID): Integer;
    ///  <summary>Returns number of snippet ID records in list.</summary>
    function Count: Integer;
    ///  <summary>Gets snippet ID record from list by index.</summary>
    function GetItem(Idx: Integer): TSnippetID;
    ///  <summary>Stores snippet ID record in list at specified index.</summary>
    procedure SetItem(Idx: Integer; const Value: TSnippetID);
    ///  <summary>Provides read/write access to snippet IDs by index.</summary>
    property Items[Idx: Integer]: TSnippetID
      read GetItem write SetItem; default;
  end;

type
  ///  <summary>
  ///  Implements a list of snippet identification records.
  ///  </summary>
  TSnippetIDList = class(TInterfacedObject, ISnippetIDList, IAssignable)
  strict private
    var
      ///  <summary>Internal list if snippet ID records.</summary>
      fList: TList<TSnippetID>;
  protected
    { ISnippetIDList methods }
    ///  <summary>Gets new list enumerator.</summary>
    function GetEnumerator: TEnumerator<TSnippetID>;
    ///  <summary>Clears the list.</summary>
    procedure Clear;
    ///  <summary>Adds given snippet ID record to list.</summary>
    ///  <remarks>Returns index of new record in list.</remarks>
    function Add(const SnippetID: TSnippetID): Integer;
    ///  <summary>Returns number of snippet ID records in list.</summary>
    function Count: Integer;
    ///  <summary>Gets snippet ID record from list by index.</summary>
    function GetItem(Idx: Integer): TSnippetID;
    ///  <summary>Stores snippet ID record in list at specified index.</summary>
    procedure SetItem(Idx: Integer; const Value: TSnippetID);
    { IAssignable methods }
    ///  <summary>Sets this list to be a copy of given list Src.</summary>
    ///  <remarks>Src must support ISnippetIDList.</remarks>
    procedure Assign(const Src: IInterface);
  public
    ///  <summary>Object constructor. Sets up empty list object.</summary>
    constructor Create;
    ///  <summary>Destructor. Tears down object.</summary>
    destructor Destroy; override;
  end;


implementation


uses
  // Delphi
  SysUtils, Generics.Defaults,
  // Project
  UStrUtils;


{ TSnippetID }

constructor TSnippetID.Clone(const Src: TSnippetID);
begin
  Create(Src.Name, Src.UserDefined);
end;

function TSnippetID.Compare(const SID: TSnippetID): Integer;
begin
  Result := CompareNames(Name, SID.Name);
  if Result = 0 then
    Result := Ord(UserDefined) - Ord(SID.UserDefined);
end;

class function TSnippetID.CompareNames(const Left, Right: string): Integer;
begin
  Result := StrCompareText(Left, Right);
end;

constructor TSnippetID.Create(const AName: string; const AUserDefined: Boolean);
begin
  fName := AName;
  fUserDefined := AUserDefined;
end;

class operator TSnippetID.Equal(const SID1, SID2: TSnippetID): Boolean;
begin
  Result := SID1.Compare(SID2) = 0;
end;

class operator TSnippetID.NotEqual(const SID1, SID2: TSnippetID): Boolean;
begin
  Result := not (SID1 = SID2);
end;

{ TSnippetIDList }

function TSnippetIDList.Add(const SnippetID: TSnippetID): Integer;
begin
  Result := fList.Add(SnippetID);
end;

procedure TSnippetIDList.Assign(const Src: IInterface);
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
begin
  fList.Clear;
end;

function TSnippetIDList.Count: Integer;
begin
  Result := fList.Count;
end;

constructor TSnippetIDList.Create;
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
begin
  fList.Free;
  inherited;
end;

function TSnippetIDList.GetEnumerator: TEnumerator<TSnippetID>;
begin
  Result := fList.GetEnumerator;
end;

function TSnippetIDList.GetItem(Idx: Integer): TSnippetID;
begin
  Result := fList[Idx];
end;

procedure TSnippetIDList.SetItem(Idx: Integer; const Value: TSnippetID);
begin
  fList[Idx] := Value;
end;

end.


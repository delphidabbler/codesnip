{
 * This Source Code Form is subject to the terms of the Mozilla Public License,
 * v. 2.0. If a copy of the MPL was not distributed with this file, You can
 * obtain one at http://mozilla.org/MPL/2.0/
 *
 * Copyright (C) 2009-2013, Peter Johnson (www.delphidabbler.com).
 *
 * $Rev$
 * $Date$
 *
 * Defines a record that uniquely indentifies a snippet in the database, along
 * with an object that maintains a list of snippet ID records.
}


unit USnippetIDs;


interface


uses
  // Delphi
  Generics.Collections,
  // Project
  CS.Database.Types,
  IntfCommon;


type
  // TODO: Add new ToList<TSnippetID> method
  ///  <summary>Implements a list of snippet identification records.</summary>
  TSnippetIDList = class(
    TInterfacedObject, ISnippetIDList, IAssignable, IClonable
  )
  strict private
    var
      // TODO: Change implementation to use DelphiColl - use TLinkedSet
      ///  <summary>Internal list if snippet ID records.</summary>
      fList: TList<TSnippetID>;
  public
    ///  <summary>Constructs empty list object.</summary>
    constructor Create; overload;

    ///  <summary>Constructs empty list object with the given capacity.
    ///  </summary>
    constructor Create(const ACapacity: Integer); overload;

    ///  <summary>Destroys object.</summary>
    destructor Destroy; override;

    ///  <summary>Gets new list enumerator.</summary>
    ///  <remarks>Method of ISnippetIDList.</remarks>
    function GetEnumerator: TEnumerator<TSnippetID>;

    ///  <summary>Clears the list.</summary>
    ///  <remarks>Method of ISnippetIDList.</remarks>
    procedure Clear;

    ///  <summary>Adds given snippet ID to list and returns its index in list.
    ///  </summary>
    ///  <remarks>Method of ISnippetIDList.</remarks>
    function Add(const SnippetID: TSnippetID): Integer;

    ///  <summary>Removed the given snippet ID from the list.</summary>
    ///  <remarks>
    ///  <para>Does nothing if SnippetID is not in the list.</para>
    ///  <para>Method of ISnippetIDList.</para>
    ///  </remarks>
    procedure Remove(const SnippetID: TSnippetID);

    ///  <summary>Checks if list contains given snippet ID.</summary>
    ///  <remarks>Method of ISnippetIDList.</remarks>
    function Contains(const SnippetID: TSnippetID): Boolean;

    ///  <summary>Checks if list is empty.</summary>
    ///  <remarks>Method of ISnippetIDList.</remarks>
    function IsEmpty: Boolean;

    ///  <summary>Returns number of snippet ID records in list.</summary>
    ///  <remarks>Method of ISnippetIDList.</remarks>
    function Count: Integer;

    ///  <summary>Copies properties of given list to this one.</summary>
    ///  <param name="Src">IInterface [in] List whose properties are to be
    ///  copied. Src must support ISnippetIDList.</param>
    ///  <remarks>Method of IAssignable.</remarks>
    procedure Assign(const Src: IInterface);

    ///  <summary>Creates and returns a new list that is an exact copy of the
    ///  current one.</summary>
    ///  <returns>IInterface. Reference to cloned object.</returns>
    ///  <remarks>Method of IClonable</remarks>
    function Clone: IInterface;
  end;


implementation


uses
  // Delphi
  SysUtils, Generics.Defaults,
  // Project
  CS.Utils.Hashes,
  UExceptions,
  UStrUtils;


{ TSnippetIDList }

function TSnippetIDList.Add(const SnippetID: TSnippetID): Integer;
begin
  Result := fList.Add(SnippetID);
end;

procedure TSnippetIDList.Assign(const Src: IInterface);
var
  SrcID: TSnippetID;  // references each ID in source
  SrcList: ISnippetIDList;
begin
  if not Supports(Src, ISnippetIDList, SrcList) then
    raise EBug.Create(ClassName + '.Assign: Src must support ISnippetIDList');
  Clear;
  fList.Capacity := SrcList.Count;
  for SrcID in SrcList do
    Add(SrcID);
end;

procedure TSnippetIDList.Clear;
begin
  fList.Clear;
end;

function TSnippetIDList.Clone: IInterface;
begin
  Result := TSnippetIDList.Create;
  (Result as IAssignable).Assign(Self);
end;

function TSnippetIDList.Contains(const SnippetID: TSnippetID): Boolean;
begin
  Result := fList.Contains(SnippetID);
end;

function TSnippetIDList.Count: Integer;
begin
  Result := fList.Count;
end;

constructor TSnippetIDList.Create(const ACapacity: Integer);
begin
  Create;
  fList.Capacity := ACapacity;
end;

constructor TSnippetIDList.Create;
begin
  inherited;
  fList := TList<TSnippetID>.Create(
    TDelegatedComparer<TSnippetID>.Create(
      function(const Left, Right: TSnippetID): Integer
      begin
        Result := TSnippetID.Compare(Left, Right);
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

function TSnippetIDList.IsEmpty: Boolean;
begin
  Result := fList.Count = 0;
end;

procedure TSnippetIDList.Remove(const SnippetID: TSnippetID);
begin
  fList.Remove(SnippetID);
end;

end.


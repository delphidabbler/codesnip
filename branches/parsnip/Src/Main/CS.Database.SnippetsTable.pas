{
 * This Source Code Form is subject to the terms of the Mozilla Public License,
 * v. 2.0. If a copy of the MPL was not distributed with this file, You can
 * obtain one at http://mozilla.org/MPL/2.0/
 *
 * Copyright (C) 2013, Peter Johnson (www.delphidabbler.com).
 *
 * $Rev$
 * $Date$
 *
 * Implements the snippets table at the core of the snippets database.
}


unit CS.Database.SnippetsTable;

interface

uses
  Collections.Dictionaries,
  Collections.Base,
  CS.Database.Types,
  CS.Database.Snippets,
  UExceptions;

type

  TDBSnippet = class(TSnippetBase)
  public
    class function CreateFrom(ASnippet: IEditableSnippet): TDBSnippet;
    function CloneAsEditable: IEditableSnippet;
    function CloseAsReadOnly(const RequiredProps: TDBSnippetProps):
      IReadOnlySnippet;
    function IsEqual(const AOther: TDBSnippet): Boolean;
  end;

  TDBSnippetsTable = class(TObject)
  strict private
    var
      fTable: TObjectDictionary<TSnippetID, TDBSnippet>;
  public
    constructor Create;
    destructor Destroy; override;
    function GetEnumerator: IEnumerator<TDBSnippet>;
    function Contains(const ASnippetID: TSnippetID): Boolean;
    function Get(const ASnippetID: TSnippetID): TDBSnippet;
    procedure Add(const ASnippet: TDBSnippet);
    procedure Update(const ASnippet: TDBSnippet);
    procedure Delete(const ASnippetID: TSnippetID);
    procedure Clear;
    function Size: Integer;
    function IsEmpty: Boolean;
  end;

  EDBSnippetsTable = class(EBug);

implementation

uses
  SysUtils,
  Generics.Defaults;

{ TDBSnippet }

function TDBSnippet.CloneAsEditable: IEditableSnippet;
begin
  Result := TEditableSnippet.Create(Self);
end;

function TDBSnippet.CloseAsReadOnly(const RequiredProps: TDBSnippetProps):
  IReadOnlySnippet;
begin
  Result := TReadOnlySnippet.Create(Self, RequiredProps);
end;

class function TDBSnippet.CreateFrom(ASnippet: IEditableSnippet): TDBSnippet;
begin
  Result := TDBSnippet.Create(ASnippet as TEditableSnippet);
end;

function TDBSnippet.IsEqual(const AOther: TDBSnippet): Boolean;
begin
  Result := Self.GetID = AOther.GetID;
end;

{ TDBSnippetsTable }

procedure TDBSnippetsTable.Add(const ASnippet: TDBSnippet);
begin
  fTable.Add(ASnippet.GetID, ASnippet);
end;

procedure TDBSnippetsTable.Clear;
begin
  fTable.Clear;
end;

function TDBSnippetsTable.Contains(const ASnippetID: TSnippetID): Boolean;
begin
  Result := fTable.ContainsKey(ASnippetID);
end;

constructor TDBSnippetsTable.Create;
begin
  inherited Create;
  fTable := TObjectDictionary<TSnippetID,TDBSnippet>.Create(
    TRules<TSnippetID>.Create(
      TSnippetID.TComparator.Create, TSnippetID.TComparator.Create
    ),
    TRules<TDBSnippet>.Create(
      TDelegatedComparer<TDBSnippet>.Create(
        function (const Left, Right: TDBSnippet): Integer
        begin
          Result := TSnippetID.Compare(Left.GetID, Right.GetID);
        end
      ),
      TDelegatedEqualityComparer<TDBSnippet>.Create(
        function (const Left, Right: TDBSnippet): Boolean
        begin
          Result := Left.GetID = Right.GetID;
        end,
        function (const Snippet: TDBSnippet): Integer
        begin
          Result := Snippet.GetID.Hash;
        end
      )
    )
  );
  fTable.OwnsKeys := False;
  fTable.OwnsValues := True;
end;

procedure TDBSnippetsTable.Delete(const ASnippetID: TSnippetID);
begin
  fTable.Remove(ASnippetID);
end;

destructor TDBSnippetsTable.Destroy;
begin
  fTable.Free;
  inherited;
end;

function TDBSnippetsTable.Get(const ASnippetID: TSnippetID): TDBSnippet;
begin
  Result := fTable[ASnippetID];
end;

function TDBSnippetsTable.GetEnumerator: IEnumerator<TDBSnippet>;
begin
  Result := fTable.Values.GetEnumerator;
end;

function TDBSnippetsTable.IsEmpty: Boolean;
begin
  Result := fTable.Count = 0;
end;

function TDBSnippetsTable.Size: Integer;
begin
  Result := fTable.Count;
end;

procedure TDBSnippetsTable.Update(const ASnippet: TDBSnippet);
begin
  fTable[ASnippet.GetID].UpdateFrom(ASnippet);
end;

end.



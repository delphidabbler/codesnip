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
  CS.Markup,
  CS.Utils.Dates,
  UExceptions;

type

  TDBSnippet = class(TSnippetBase)
  public
    class function CreateFrom(ASnippet: ISnippet): TDBSnippet;
    function Copy: ISnippet;
    function CopyPartial(const RequiredProps: TDBSnippetProps):
      IReadOnlySnippet;
  end;

  TDBSnippetsTable = class(TObject)
  strict private
    var
      fTable: TObjectDictionary<TDBSnippetID, TDBSnippet>;
  public
    constructor Create;
    destructor Destroy; override;
    function GetEnumerator: IEnumerator<TDBSnippet>;
    function Contains(const ASnippetID: TDBSnippetID): Boolean;
    function Get(const ASnippetID: TDBSnippetID): TDBSnippet;
    procedure Add(const ASnippet: TDBSnippet);
    procedure Update(const ASnippet: TDBSnippet);
    procedure Delete(const ASnippetID: TDBSnippetID);
    procedure Clear;
    function Size: Integer;
  end;

  EDBSnippetsTable = class(EBug);

implementation

uses
  SysUtils,
  Generics.Defaults;

{ TDBSnippet }

function TDBSnippet.Copy: ISnippet;
begin
  Result := TSnippet.Create(Self);
end;

function TDBSnippet.CopyPartial(const RequiredProps: TDBSnippetProps):
  IReadOnlySnippet;
begin
  Result := TPartialSnippet.Create(Self, RequiredProps);
end;

class function TDBSnippet.CreateFrom(ASnippet: ISnippet): TDBSnippet;
begin
  Result := TDBSnippet.Create(ASnippet as TSnippet);
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

function TDBSnippetsTable.Contains(const ASnippetID: TDBSnippetID): Boolean;
begin
  Result := fTable.ContainsKey(ASnippetID);
end;

constructor TDBSnippetsTable.Create;
begin
  inherited Create;
  fTable := TObjectDictionary<TDBSnippetID,TDBSnippet>.Create(
    TRules<TDBSnippetID>.Create(
      TDBSnippetID.TComparer.Create,
      TDBSnippetID.TEqualityComparer.Create
    ),
    TRules<TDBSnippet>.Create(
      TDelegatedComparer<TDBSnippet>.Create(
        function (const Left, Right: TDBSnippet): Integer
        begin
          Result := TDBSnippetID.Compare(Left.GetID, Right.GetID);
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

procedure TDBSnippetsTable.Delete(const ASnippetID: TDBSnippetID);
begin
  fTable.Remove(ASnippetID);
end;

destructor TDBSnippetsTable.Destroy;
begin
  fTable.Free;
  inherited;
end;

function TDBSnippetsTable.Get(const ASnippetID: TDBSnippetID): TDBSnippet;
begin
  Result := fTable[ASnippetID];
end;

function TDBSnippetsTable.GetEnumerator: IEnumerator<TDBSnippet>;
begin
  Result := fTable.Values.GetEnumerator;
end;

function TDBSnippetsTable.Size: Integer;
begin
  Result := fTable.Count;
end;

procedure TDBSnippetsTable.Update(const ASnippet: TDBSnippet);
begin
  fTable[ASnippet.GetID] := ASnippet;   // original snippet object will be freed
end;

end.



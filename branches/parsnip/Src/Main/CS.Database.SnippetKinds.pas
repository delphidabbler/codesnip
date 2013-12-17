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
 * Defines a record that provides information about the different snippet kinds
 * enumerated by TSnippetKind along with a static class that provides an
 * enumerable list of snippet kind information records.
}


unit CS.Database.SnippetKinds;


interface


uses
  // 3rd Party
  Collections.Base,
  // Project
  CS.Database.Types,
  UBaseObjects;


type
  TSnippetKindList = class(TInterfacedObject, ISnippetKindList)
  strict private
    type
      TEnumerator = class(TInterfacedObject, IEnumerator<TSnippetKind>)
      strict private
        var
          fAtStart: Boolean;
          fCurrent: TSnippetKindID;
          fMap: TSnippetKindList;
      public
        constructor Create(const AMap: TSnippetKindList);
        function GetCurrent: TSnippetKind;
        function MoveNext: Boolean;
      end;
  strict private
    var
      fMap: array[TSnippetKindID] of TSnippetKind;
  public
    constructor Create;
    destructor Destroy; override;
    function GetEnumerator: IEnumerator<TSnippetKind>;
    function GetItem(const KindID: TSnippetKindID): TSnippetKind;
    function First: TSnippetKind;
    function Last: TSnippetKind;
  end;


implementation


{ TSnippetKindList }

constructor TSnippetKindList.Create;
resourcestring
  // Snippet kind descriptions
  sFreeForm         = 'Freeform';
  sRoutine          = 'Routine';
  sConstant         = 'Constant';
  sTypeDef          = 'Type Definition';
  sUnit             = 'Unit';
  sClass            = 'Class / Advanced Record';
const
  // Map of snippet kinds onto their descriptions
  Descriptions: array[TSnippetKindID] of string = (
    sFreeform, sRoutine, sConstant, sTypeDef, sUnit, sClass
  );
var
  Kind: TSnippetKindID;
begin
  inherited Create;
  for Kind := Low(TSnippetKindID) to High(TSnippetKindID) do
    fMap[Kind] := TSnippetKind.Create(Kind, Descriptions[Kind]);
end;

destructor TSnippetKindList.Destroy;
begin
  inherited;
end;

function TSnippetKindList.First: TSnippetKind;
begin
  Result := fMap[Low(TSnippetKindID)];
end;

function TSnippetKindList.GetEnumerator: IEnumerator<TSnippetKind>;
begin
  Result := TEnumerator.Create(Self);
end;

function TSnippetKindList.GetItem(const KindID: TSnippetKindID):
  TSnippetKind;
begin
  Result := fMap[KindID];
end;

function TSnippetKindList.Last: TSnippetKind;
begin
  Result := fMap[High(TSnippetKindID)];
end;

{ TSnippetKindList.TEnumerator }

constructor TSnippetKindList.TEnumerator.Create(const AMap: TSnippetKindList);
begin
  inherited Create;
  fMap := AMap;
  fAtStart := True;
  fCurrent := AMap.First.ID;
end;

function TSnippetKindList.TEnumerator.GetCurrent: TSnippetKind;
begin
  Result := fMap.GetItem(fCurrent);
end;

function TSnippetKindList.TEnumerator.MoveNext: Boolean;
begin
  if fCurrent = fMap.Last.ID then
    Exit(False);
  if fAtStart then
  begin
    fCurrent := fMap.First.ID;
    fAtStart := False;
  end
  else
    Inc(fCurrent);
  Result := True;
end;

end.


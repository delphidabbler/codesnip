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


unit DB.USnippetKind;


interface


uses
  // 3rd Party
  Collections.Base,
  // Project
  CS.Database.Types,
  UBaseObjects;


type
  ///  <summary>
  ///  Static class that provides a read-only, enumerable, list of
  ///  TSnippetKindInfo records, one for each snippet kind.
  ///  </summary>
  TSnippetKindInfoList = class(TNoConstructObject)
  strict private
    type
      ///  <summary>Array of snippet kind information records. Has element for
      ///  each snippet kind.</summary>
      TSnippetKindInfoArray = array[TSnippetKind] of TSnippetKindInfo;
    class var
      ///  <summary>Array of snippet kind info records.</summary>
      fItems: TSnippetKindInfoArray;
      ///  <summary>Flag indicating if fItems has been initialised.</summary>
      fInitialised: Boolean;
  strict private
    ///  <summary>Initialises fItems array.</summary>
    class procedure Init;
    ///  <summary>Read accessor for Items property.</summary>
    class function GetItems: TSnippetKindInfoArray; static;
  public
    ///  <summary>Enumerable array of snippet kind info records, one element for
    ///  each snippet kind.</summary>
    class property Items: TSnippetKindInfoArray read GetItems;
  end;

  TSnippetKindList = class(TInterfacedObject, ISnippetKindList)
  strict private
    type
      TEnumerator = class(TInterfacedObject, IEnumerator<TSnippetKindInfo>)
      strict private
        var
          fAtStart: Boolean;
          fCurrent: TSnippetKind;
          fMap: TSnippetKindList;
      public
        constructor Create(const AMap: TSnippetKindList);
        function GetCurrent: TSnippetKindInfo;
        function MoveNext: Boolean;
      end;
  strict private
    var
      fMap: array[TSnippetKind] of TSnippetKindInfo;
  public
    constructor Create;
    destructor Destroy; override;
    function GetEnumerator: IEnumerator<TSnippetKindInfo>;
    function GetItem(const KindID: TSnippetKind): TSnippetKindInfo;
    function First: TSnippetKindInfo;
    function Last: TSnippetKindInfo;
  end;


implementation


{ TSnippetKindInfoList }

class function TSnippetKindInfoList.GetItems: TSnippetKindInfoArray;
begin
  if not fInitialised then
  begin
    // The obvious thing to do is to initialise the Items[] array in a class
    // constructor. But the required resource strings are read as '' when
    // initialisation is done in a class constructor, so we need to jump thru
    // these hoops.
    Init;
    fInitialised := True;
  end;
  Result := fItems;
end;

class procedure TSnippetKindInfoList.Init;
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
  Descriptions: array[TSnippetKind] of string = (
    sFreeform, sRoutine, sConstant, sTypeDef, sUnit, sClass
  );
var
  Kind: TSnippetKind;
begin
  for Kind := Low(TSnippetKind) to High(TSnippetKind) do
    fItems[Kind] := TSnippetKindInfo.Create(Kind, Descriptions[Kind]);
end;

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
  Descriptions: array[TSnippetKind] of string = (
    sFreeform, sRoutine, sConstant, sTypeDef, sUnit, sClass
  );
var
  Kind: TSnippetKind;
begin
  inherited Create;
  for Kind := Low(TSnippetKind) to High(TSnippetKind) do
    fMap[Kind] := TSnippetKindInfo.Create(Kind, Descriptions[Kind]);
end;

destructor TSnippetKindList.Destroy;
begin
  inherited;
end;

function TSnippetKindList.First: TSnippetKindInfo;
begin
  Result := fMap[Low(TSnippetKind)];
end;

function TSnippetKindList.GetEnumerator: IEnumerator<TSnippetKindInfo>;
begin
  Result := TEnumerator.Create(Self);
end;

function TSnippetKindList.GetItem(const KindID: TSnippetKind): TSnippetKindInfo;
begin
  Result := fMap[KindID];
end;

function TSnippetKindList.Last: TSnippetKindInfo;
begin
  Result := fMap[High(TSnippetKind)];
end;

{ TSnippetKindList.TEnumerator }

constructor TSnippetKindList.TEnumerator.Create(const AMap: TSnippetKindList);
begin
  inherited Create;
  fMap := AMap;
  fAtStart := True;
  fCurrent := AMap.First.Kind;
end;

function TSnippetKindList.TEnumerator.GetCurrent: TSnippetKindInfo;
begin
  Result := fMap.GetItem(fCurrent);
end;

function TSnippetKindList.TEnumerator.MoveNext: Boolean;
begin
  if fCurrent = fMap.Last.Kind then
    Exit(False);
  if fAtStart then
  begin
    fCurrent := fMap.First.Kind;
    fAtStart := False;
  end
  else
    Inc(fCurrent);
  Result := True;
end;

end.


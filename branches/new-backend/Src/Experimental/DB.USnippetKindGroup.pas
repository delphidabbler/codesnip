{
 * DB.UKindGroup.pas
 *
 * Implements classes, records etc. that represent snippet kind groups.
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
 * The Original Code is DB.UKindGroup.pas
 *
 * The Initial Developer of the Original Code is Peter Johnson
 * (http://www.delphidabbler.com/).
 *
 * Portions created by the Initial Developer are Copyright (C) 2010 Peter
 * Johnson. All Rights Reserved.
 *
 * Contributor(s)
 *   NONE
 *
 * ***** END LICENSE BLOCK *****
}


unit DB.USnippetKindGroup;

interface

uses
  DB.UBaseGroup, DB.UConsts;

type

  TDBSnippetKindGroupKey = record
  strict private
    fKind: TDBSnippetKind;
    class function Compare(Key1, Key2: TDBSnippetKindGroupKey): Integer; static;
  public
    constructor Create(AKind: TDBSnippetKind);
    property Kind: TDBSnippetKind read fKind;
    class operator Equal(Key1, Key2: TDBSnippetKindGroupKey): Boolean;
    class operator NotEqual(Key1, Key2: TDBSnippetKindGroupKey): Boolean;
    // cast to TDBSnippetKind
    class operator Implicit(Key: TDBSnippetKindGroupKey): TDBSnippetKind;
  end;

  TDBSnippetKindGroupKeys = TArray<TDBSnippetKindGroupKey>;

  TDBSnippetKindGroup = class(TDBBaseGroup)
  strict private
    fKey: TDBSnippetKindGroupKey;
  strict protected
    function GetDisplayName: string; override;
  public
    constructor Create(Kind: TDBSnippetKind);
    property Key: TDBSnippetKindGroupKey read fKey;
  end;

implementation

resourcestring
  sRoutine = 'Routine';
  sConstant = 'Constant';
  sTypeDef = 'Type Definition';
  sFreeform = 'Freeform';
  sUnit = 'Unit';
  sClass = 'Class / Advanced Record';


const
  DBSnippetKindDisplayNames: array[TDBSnippetKind] of string = (
    sFreeform, sRoutine, sConstant, sTypeDef, sUnit, sClass
  );

{ TDBSnippetKindGroup }

constructor TDBSnippetKindGroup.Create(Kind: TDBSnippetKind);
begin
  inherited Create;
  fKey := TDBSnippetKindGroupKey.Create(Kind);
end;

function TDBSnippetKindGroup.GetDisplayName: string;
begin
  Result := DBSnippetKindDisplayNames[Key.Kind];
end;

{ TDBSnippetKindGroupKey }

class function TDBSnippetKindGroupKey.Compare(Key1,
  Key2: TDBSnippetKindGroupKey): Integer;
begin
  Result := Ord(Key1.Kind) - Ord(Key2.Kind);
end;

constructor TDBSnippetKindGroupKey.Create(AKind: TDBSnippetKind);
begin
  fKind := AKind;
end;

class operator TDBSnippetKindGroupKey.Equal(Key1,
  Key2: TDBSnippetKindGroupKey): Boolean;
begin
  Result := Key1.Kind = Key2.Kind;
end;

class operator TDBSnippetKindGroupKey.Implicit(
  Key: TDBSnippetKindGroupKey): TDBSnippetKind;
begin
  Result := Key.Kind;
end;

class operator TDBSnippetKindGroupKey.NotEqual(Key1,
  Key2: TDBSnippetKindGroupKey): Boolean;
begin
  Result := Key1.Kind <> Key2.Kind;
end;

end.

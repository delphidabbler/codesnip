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
    function CompareTo(const AKey: TDBSnippetKindGroupKey): Integer;
    class operator Equal(Key1, Key2: TDBSnippetKindGroupKey): Boolean;
    class operator NotEqual(Key1, Key2: TDBSnippetKindGroupKey): Boolean;
    class operator GreaterThan(Key1, Key2: TDBSnippetKindGroupKey): Boolean;
    class operator GreaterThanOrEqual(Key1, Key2: TDBSnippetKindGroupKey):
      Boolean;
    class operator LessThan(Key1, Key2: TDBSnippetKindGroupKey): Boolean;
    class operator LessThanOrEqual(Key1, Key2: TDBSnippetKindGroupKey): Boolean;
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
  sType = 'Type';
  sFreeform = 'Freeform';

const
  DBSnippetKindDisplayNames: array[TDBSnippetKind] of string = (
    sRoutine, sConstant, sType, sFreeform
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

function TDBSnippetKindGroupKey.CompareTo(
  const AKey: TDBSnippetKindGroupKey): Integer;
begin
  Result := Compare(Self, AKey);
end;

constructor TDBSnippetKindGroupKey.Create(AKind: TDBSnippetKind);
begin
  fKind := AKind;
end;

class operator TDBSnippetKindGroupKey.Equal(Key1,
  Key2: TDBSnippetKindGroupKey): Boolean;
begin
  Result := Compare(Key1, Key2) = 0;
end;

class operator TDBSnippetKindGroupKey.GreaterThan(Key1,
  Key2: TDBSnippetKindGroupKey): Boolean;
begin
  Result := Compare(Key1, Key2) > 0;
end;

class operator TDBSnippetKindGroupKey.GreaterThanOrEqual(Key1,
  Key2: TDBSnippetKindGroupKey): Boolean;
begin
  Result := Compare(Key1, Key2) >= 0;
end;

class operator TDBSnippetKindGroupKey.Implicit(
  Key: TDBSnippetKindGroupKey): TDBSnippetKind;
begin
  Result := Key.Kind;
end;

class operator TDBSnippetKindGroupKey.LessThan(Key1,
  Key2: TDBSnippetKindGroupKey): Boolean;
begin
  Result := Compare(Key1, Key2) < 0;
end;

class operator TDBSnippetKindGroupKey.LessThanOrEqual(Key1,
  Key2: TDBSnippetKindGroupKey): Boolean;
begin
  Result := Compare(Key1, Key2) <= 0;
end;

class operator TDBSnippetKindGroupKey.NotEqual(Key1,
  Key2: TDBSnippetKindGroupKey): Boolean;
begin
  Result := Compare(Key1, Key2) <> 0;
end;

end.

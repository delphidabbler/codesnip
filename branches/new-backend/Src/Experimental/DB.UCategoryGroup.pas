{
 * DB.UCategoryGroup.pas
 *
 * Implements classes, records etc. that represent category groups.
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
 * The Original Code is DB.UCategoryGroup.pas
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


unit DB.UCategoryGroup;

interface

uses
  DB.UBaseGroup;

type

  TDBCategoryGroupKey = record
  strict private
    fID: string;
    class function Compare(Key1, Key2: TDBCategoryGroupKey): Integer; static;
  public
    constructor Create(const AID: string);
    property ID: string read fID;
    function CompareTo(const AKey: TDBCategoryGroupKey): Integer;
    class operator Equal(Key1, Key2: TDBCategoryGroupKey): Boolean;
    class operator NotEqual(Key1, Key2: TDBCategoryGroupKey): Boolean;
    class operator GreaterThan(Key1, Key2: TDBCategoryGroupKey): Boolean;
    class operator GreaterThanOrEqual(Key1, Key2: TDBCategoryGroupKey): Boolean;
    class operator LessThan(Key1, Key2: TDBCategoryGroupKey): Boolean;
    class operator LessThanOrEqual(Key1, Key2: TDBCategoryGroupKey): Boolean;
    // cast to a string
    class operator Implicit(const Key: TDBCategoryGroupKey): string;
  end;

  TDBCategoryGroupKeys = TArray<TDBCategoryGroupKey>;

  TDBCategoryGroupData = record
    ID: string;
    DisplayName: string;
    // NOTE: use properties for assigning objects rather than copying references
    // ...
    function Clone: TDBCategoryGroupData;
  end;

  TDBCategoryGroup = class(TDBBaseGroup)
  strict private
    fKey: TDBCategoryGroupKey;
    fDisplayName: string;
  strict protected
    function GetDisplayName: string; override;
    procedure UpdateData(Data: TDBCategoryGroupData);
  public
    constructor Create(const Data: TDBCategoryGroupData);
    property Key: TDBCategoryGroupKey read fKey;
    property DisplayName; // defined in parent class
    // ...
  end;

implementation

uses
  SysUtils, Types,
  DB.UCookies;

{ TDBCategoryGroup }

constructor TDBCategoryGroup.Create(const Data: TDBCategoryGroupData);
begin
  inherited Create;
  UpdateData(Data);
end;

function TDBCategoryGroup.GetDisplayName: string;
begin
  Result := fDisplayName;
end;

procedure TDBCategoryGroup.UpdateData(Data: TDBCategoryGroupData);
begin
  fKey := TDBCategoryGroupKey.Create(Data.ID);
  fDisplayName := Data.DisplayName;
  // ...
end;

{ TDBCategoryGroupKey }

class function TDBCategoryGroupKey.Compare(Key1, Key2: TDBCategoryGroupKey):
  Integer;
begin
  Result := AnsiCompareText(Key1.ID, Key2.ID);
end;

function TDBCategoryGroupKey.CompareTo(
  const AKey: TDBCategoryGroupKey): Integer;
begin
  Result := Compare(Self, AKey);
end;

constructor TDBCategoryGroupKey.Create(const AID: string);
begin
  fID := AID;
end;

class operator TDBCategoryGroupKey.Equal(Key1, Key2: TDBCategoryGroupKey):
  Boolean;
begin
  Result := Compare(Key1, Key2) = 0;
end;

class operator TDBCategoryGroupKey.GreaterThan(Key1, Key2: TDBCategoryGroupKey):
  Boolean;
begin
  Result := Compare(Key1, Key2) > 0;
end;

class operator TDBCategoryGroupKey.GreaterThanOrEqual(Key1,
  Key2: TDBCategoryGroupKey): Boolean;
begin
  Result := Compare(Key1, Key2) >= 0;
end;

class operator TDBCategoryGroupKey.Implicit(const Key: TDBCategoryGroupKey):
  string;
begin
  Result := Key.ID;
end;

class operator TDBCategoryGroupKey.LessThan(Key1, Key2: TDBCategoryGroupKey):
  Boolean;
begin
  Result := Compare(Key1, Key2) < 0;
end;

class operator TDBCategoryGroupKey.LessThanOrEqual(Key1,
  Key2: TDBCategoryGroupKey): Boolean;
begin
  Result := Compare(Key1, Key2) <= 0;
end;

class operator TDBCategoryGroupKey.NotEqual(Key1, Key2: TDBCategoryGroupKey):
  Boolean;
begin
  Result := Compare(Key1, Key2) <> 0;
end;

{ TDBCategoryGroupData }

function TDBCategoryGroupData.Clone: TDBCategoryGroupData;
begin
  Result.ID := ID;
  Result.DisplayName := DisplayName;
  // ...
end;

end.


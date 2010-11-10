{
 * DB.UInitialLetterGroup.pas
 *
 * Implements classes, records etc. that represent initial letter groups.
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
 * The Original Code is DB.UInitialLetterGroup.pas
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


unit DB.UInitialLetterGroup;

interface

uses
  DB.UBaseGroup;

type

  TDBInitialLetterGroupKey = record
  strict private
    fLetter: Char;
    class function Compare(Key1, Key2: TDBInitialLetterGroupKey): Integer;
      static;
  public
    constructor Create(const ALetter: Char);
    property Letter: Char read fLetter;
    function CompareTo(const AKey: TDBInitialLetterGroupKey): Integer;
    class operator Equal(Key1, Key2: TDBInitialLetterGroupKey): Boolean;
    class operator NotEqual(Key1, Key2: TDBInitialLetterGroupKey): Boolean;
    class operator GreaterThan(Key1, Key2: TDBInitialLetterGroupKey): Boolean;
    class operator GreaterThanOrEqual(Key1, Key2: TDBInitialLetterGroupKey):
      Boolean;
    class operator LessThan(Key1, Key2: TDBInitialLetterGroupKey): Boolean;
    class operator LessThanOrEqual(Key1, Key2: TDBInitialLetterGroupKey):
      Boolean;
    // cast to string
    class operator Implicit(Key: TDBInitialLetterGroupKey): Char;
  end;

  TDBInitialLetterGroupKeys = TArray<TDBInitialLetterGroupKey>;

  TDBInitialLetterGroup = class(TDBBaseGroup)
  strict private
    fKey: TDBInitialLetterGroupKey;
  strict protected
    function GetDisplayName: string; override;
  public
    constructor Create(Letter: Char);
    property Key: TDBInitialLetterGroupKey read fKey;
  end;

implementation

uses
  SysUtils, Character;


{ TDBInitialLetterGroup }

constructor TDBInitialLetterGroup.Create(Letter: Char);
begin
  inherited Create;
  fKey := TDBInitialLetterGroupKey.Create(Letter);
end;

function TDBInitialLetterGroup.GetDisplayName: string;
begin
  Result := Key.Letter;
end;

{ TDBInitialLetterGroupKey }

class function TDBInitialLetterGroupKey.Compare(Key1,
  Key2: TDBInitialLetterGroupKey): Integer;
begin
  Result := AnsiCompareStr(Key1.Letter, Key2.Letter);
end;

function TDBInitialLetterGroupKey.CompareTo(
  const AKey: TDBInitialLetterGroupKey): Integer;
begin
  Result := Compare(Self, AKey);
end;

constructor TDBInitialLetterGroupKey.Create(const ALetter: Char);
begin
  fLetter := TCharacter.ToUpper(ALetter);
end;

class operator TDBInitialLetterGroupKey.Equal(Key1,
  Key2: TDBInitialLetterGroupKey): Boolean;
begin
  Result := Compare(Key1, Key2) = 0;
end;

class operator TDBInitialLetterGroupKey.GreaterThan(Key1,
  Key2: TDBInitialLetterGroupKey): Boolean;
begin
  Result := Compare(Key1, Key2) > 0;
end;

class operator TDBInitialLetterGroupKey.GreaterThanOrEqual(Key1,
  Key2: TDBInitialLetterGroupKey): Boolean;
begin
  Result := Compare(Key1, Key2) >= 0;
end;

class operator TDBInitialLetterGroupKey.Implicit(
  Key: TDBInitialLetterGroupKey): Char;
begin
  Result := Key.Letter;
end;

class operator TDBInitialLetterGroupKey.LessThan(Key1,
  Key2: TDBInitialLetterGroupKey): Boolean;
begin
  Result := Compare(Key1, Key2) < 0;
end;

class operator TDBInitialLetterGroupKey.LessThanOrEqual(Key1,
  Key2: TDBInitialLetterGroupKey): Boolean;
begin
  Result := Compare(Key1, Key2) <= 0;
end;

class operator TDBInitialLetterGroupKey.NotEqual(Key1,
  Key2: TDBInitialLetterGroupKey): Boolean;
begin
  Result := Compare(Key1, Key2) <> 0;
end;

end.


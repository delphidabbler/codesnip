{
 * This Source Code Form is subject to the terms of the Mozilla Public License,
 * v. 2.0. If a copy of the MPL was not distributed with this file, You can
 * obtain one at http://mozilla.org/MPL/2.0/
 *
 * Copyright (C) 2010-2013, Peter Johnson (www.delphidabbler.com).
 *
 * $Rev$
 * $Date$
 *
 * Contains comparer classes, hashes etc.
}


unit UComparers;


interface


uses
  // Delphi
  Generics.Defaults;


type

  ///  <summary>
  ///  Case insenstive string comparer.
  ///  </summary>
  TTextComparer = class(TComparer<string>, IComparer<string>)
  public
    ///  <summary>Compares strings Left and Right. Returns -ve if Left less than
    ///  Right, 0 if equal or +ve if Left greater than Right.</summary>
    function Compare(const Left, Right: string): Integer; override;
  end;

type
  ///  <summary>
  ///  Case insenstive string equality comparer.
  ///  </summary>
  TTextEqualityComparer = class(TEqualityComparer<string>,
    IEqualityComparer<string>
  )
  public
    ///  <summary>Checks if two strings are equal, ignoring case.</summary>
    function Equals(const Left, Right: string): Boolean; override;
    ///  <summary>Gets hash of lower case version of given string.</summary>
    function GetHashCode(const Value: string): Integer; override;
  end;

type
  ///  <summary>
  ///  Case senstive string equality comparer.
  ///  </summary>
  TStringEqualityComparer = class(TEqualityComparer<string>,
    IEqualityComparer<string>
  )
  public
    ///  <summary>Checks if two strings are equal, taking account of case.
    ///  </summary>
    function Equals(const Left, Right: string): Boolean; override;
    ///  <summary>Gets hash of given string.</summary>
    function GetHashCode(const Value: string): Integer; override;
  end;


implementation


uses
  // Project
  CS.Utils.Hashes,
  UStrUtils;


{ TTextComparer }

function TTextComparer.Compare(const Left, Right: string): Integer;
begin
  Result := StrCompareText(Left, Right);
end;

{ TTextEqualityComparer }

function TTextEqualityComparer.Equals(const Left, Right: string): Boolean;
begin
  Result := StrSameText(Left, Right);
end;

function TTextEqualityComparer.GetHashCode(const Value: string): Integer;
begin
  // Comparison takes place (i.e. Equals gets called) only if hashes are same.
  // So we must ignore case in hash if two strings that differ only in case are
  // to be considered same.
  Result := PaulLarsonHash(StrToLower(Value));
end;

{ TStringEqualityComparer }

function TStringEqualityComparer.Equals(const Left, Right: string): Boolean;
begin
  Result := StrSameStr(Left, Right);
end;

function TStringEqualityComparer.GetHashCode(const Value: string): Integer;
begin
  Result := PaulLarsonHash(Value);
end;

end.


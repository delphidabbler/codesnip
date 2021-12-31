{
 * This Source Code Form is subject to the terms of the Mozilla Public License,
 * v. 2.0. If a copy of the MPL was not distributed with this file, You can
 * obtain one at https://mozilla.org/MPL/2.0/
 *
 * Copyright (C) 2009-2021, Peter Johnson (gravatar.com/delphidabbler).
 *
 * Defines a record that encapsulates an upper case Unicode character.
}


unit UInitialLetter;


interface


type

  ///  <summary>
  ///  Record that encapsulates an upper case Unicode character.
  ///  </summary>
  TInitialLetter = record
  strict private
    var fLetter: Char;  // Value of letter property
  public
    ///  Initialises record with given character, which is forced to upper case.
    constructor Create(ALetter: Char);
    ///  Upper case letter
    property Letter: Char read fLetter;
    ///  Compares two letters. Returns -ve if Left < Right, 0 if equal and +ve
    ///  if Left > Right
    class function Compare(const Left, Right: TInitialLetter): Integer; static;
    ///  Casts a TInitialLetter to a character.
    class operator Implicit(const R: TInitialLetter): Char;
    ///  Test for equality of two records
    class operator Equal(const L1, L2: TInitialLetter): Boolean;
    ///  Test for equality of a record and a char. Case of char is ignored.
    class operator Equal(const L: TInitialLetter; const C: Char): Boolean;
    ///  Test for equality of a char and a record. Case of char is ignored.
    class operator Equal(const C: Char; const L: TInitialLetter): Boolean;
  end;

implementation


uses
  // Delphi
  Character,
  // Project
  UStrUtils;


{ TInitialLetter }

class function TInitialLetter.Compare(const Left, Right: TInitialLetter):
  Integer;
begin
  Result := StrCompareText(Left.Letter, Right.Letter);
end;

constructor TInitialLetter.Create(ALetter: Char);
begin
  fLetter := TCharacter.ToUpper(ALetter);
end;

class operator TInitialLetter.Equal(const L1, L2: TInitialLetter): Boolean;
begin
  Result := Compare(L1, L2) = 0;
end;

class operator TInitialLetter.Equal(const L: TInitialLetter;
  const C: Char): Boolean;
begin
  Result := L = TInitialLetter.Create(C);
end;

class operator TInitialLetter.Equal(const C: Char;
  const L: TInitialLetter): Boolean;
begin
  Result := TInitialLetter.Create(C) = L;
end;

class operator TInitialLetter.Implicit(const R: TInitialLetter): Char;
begin
  Result := R.Letter;
end;

end.


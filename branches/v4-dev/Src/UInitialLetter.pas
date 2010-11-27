{
 * UInitialLetter.pas
 *
 * Defines a record that encapsulates an upper case Unicode character.
 *
 * $Rev$
 * $Date$
 *
 * This unit was originally named UAlphabet.pas.
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
 * The Original Code is UInitialLetter.pas
 *
 * The Initial Developer of the Original Code is Peter Johnson
 * (http://www.delphidabbler.com/).
 *
 * Portions created by the Initial Developer are Copyright (C) 2009-2010 Peter
 * Johnson. All Rights Reserved.
 *
 * Contributor(s)
 *   NONE
 *
 * ***** END LICENSE BLOCK *****
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
  end;

implementation


uses
  // Delphi
  SysUtils, Character;


{ TInitialLetter }

class function TInitialLetter.Compare(const Left, Right: TInitialLetter):
  Integer;
begin
  Result := AnsiCompareText(Left.Letter, Right.Letter);
end;

constructor TInitialLetter.Create(ALetter: Char);
begin
  fLetter := TCharacter.ToUpper(ALetter);
end;

class operator TInitialLetter.Implicit(const R: TInitialLetter): Char;
begin
  Result := R.Letter;
end;

end.


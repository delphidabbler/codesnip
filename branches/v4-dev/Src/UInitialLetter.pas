{
 * UInitialLetter.pas
 *
 * Defines a record that encapsulates an upper case character along with an
 * enumerable list of the records.
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


uses
  // Delphi
  Generics.Collections,
  // Project
  UContainers;


type

  ///  <summary>
  ///  Record that encapsulates an upper case Unicode letter.
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

  ///  <summary>
  ///  Maintains a sorted list of TInitialLetter records
  ///  </summary>
  TInitialLetterList = class(TObject)
  strict private
    var fList: TSortedList<TInitialLetter>; // Sorted list
  public
    ///  Object constructor. Creates list.
    constructor Create;
    ///  Object destructor. Tears down object.
    destructor Destroy; override;
    ///  Creates list enumerator.
    function GetEnumerator: TEnumerator<TInitialLetter>;
    ///  Checks if list contains the upper case version of a given letter.
    function Contains(const Letter: TInitialLetter): Boolean;
    ///  Adds a letter to the list.
    procedure Add(const Letter: TInitialLetter);
  end;


implementation


uses
  // Delphi
  SysUtils, Character, Generics.Defaults;


{ TInitialLetter }

class function TInitialLetter.Compare(const Left, Right: TInitialLetter):
  Integer;
begin
  Result := AnsiCompareText(Left.Letter, Right.Letter);
end;

constructor TInitialLetter.Create(ALetter: Char);
begin
  fLetter := Character.ToUpper(ALetter);
end;

class operator TInitialLetter.Implicit(const R: TInitialLetter): Char;
begin
  Result := R.Letter;
end;

{ TInitialLetterList }

procedure TInitialLetterList.Add(const Letter: TInitialLetter);
begin
  fList.Add(Letter);
end;

function TInitialLetterList.Contains(const Letter: TInitialLetter): Boolean;
begin
  Result := fList.Contains(Letter);
end;

constructor TInitialLetterList.Create;
begin
  inherited Create;
  fList := TSortedList<TInitialLetter>.Create(
    TDelegatedComparer<TInitialLetter>.Create(
      function (const Left, Right: TInitialLetter): Integer
      begin
        Result := TInitialLetter.Compare(Left, Right);
      end
    )
  );
  fList.PermitDuplicates := False;
end;

destructor TInitialLetterList.Destroy;
begin
  fList.Free;
  inherited;
end;

function TInitialLetterList.GetEnumerator: TEnumerator<TInitialLetter>;
begin
  Result := fList.GetEnumerator;
end;

end.


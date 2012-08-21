{
 * UComparers.pas
 *
 * Contains comparer classes, hashes etc.
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
 * The Original Code is UComparers.pas
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


unit UComparers;


interface


uses
  // Delphi
  Generics.Defaults;


type

  {
  TSameTextEqualityComparer:
    Case insensitive string comparer.
  }
  TSameTextEqualityComparer = class(TEqualityComparer<string>,
    IEqualityComparer<string>
  )
  public
    function Equals(const Left, Right: string): Boolean; override;
      {Checks if two strings are equal.
        @param Left [in] First string to compare.
        @param Right [in] Second string to compare.
        @return True if both strings are the same, ignoring case.
      }
    function GetHashCode(const Value: string): Integer; override;
      {Gets hash of a lower case version of a string.
        @param Value [in] String for which hash is needed.
        @return Required hash.
      }
  end;

  {
  TSameSttringEqualityComparer:
    Case sensitive string comparer.
  }
  TSameStringEqualityComparer = class(TEqualityComparer<string>,
    IEqualityComparer<string>
  )
  public
    function Equals(const Left, Right: string): Boolean; override;
      {Checks if two strings are equal.
        @param Left [in] First string to compare.
        @param Right [in] Second string to compare.
        @return True if both strings are the same, taking case into account.
      }
    function GetHashCode(const Value: string): Integer; override;
      {Gets hash of a string.
        @param Value [in] String for which hash is needed.
        @return Required hash.
      }
  end;


implementation


uses
  // Delphi
  SysUtils;


function ElfHash(const Value: string): Integer;
  {String hash function. Sourced from http://www.scalabium.com/faq/dct0136.htm.
    @param Value [in] String to be hashed.
    @return Required hash.
  }
var
  I: Integer; // loops thru string
  X: Integer; // stores interim results
begin
  Result := 0;
  for I := 1 to Length(Value) do
  begin
    Result := (Result shl 4) + Ord(Value[I]);
    X := Result and $F0000000;
    if (X <> 0) then
      Result := Result xor (X shr 24);
    Result := Result and (not X);
  end;
end;

{ TSameTextEqualityComparer }

function TSameTextEqualityComparer.Equals(const Left, Right: string): Boolean;
  {Checks if two strings are equal.
    @param Left [in] First string to compare.
    @param Right [in] Second string to compare.
    @return True if both strings are the same, ignoring case.
  }
begin
  Result := AnsiSameText(Left, Right);
end;

function TSameTextEqualityComparer.GetHashCode(const Value: string): Integer;
  {Gets hash of a lower case version of a string.
    @param Value [in] String for which hash is needed.
    @return Required hash.
  }
begin
  // Comparison takes place (i.e. Equals gets called) only if hashes are same.
  // So we must ignore case in hash if two strings that differ only in case are
  // to be considered same.
  Result := ElfHash(AnsiLowerCase(Value));
end;

{ TSameStringEqualityComparer }

function TSameStringEqualityComparer.Equals(const Left, Right: string): Boolean;
  {Checks if two strings are equal.
    @param Left [in] First string to compare.
    @param Right [in] Second string to compare.
    @return True if both strings are the same, taking case into account.
  }
begin
  Result := AnsiSameStr(Left, Right);
end;

function TSameStringEqualityComparer.GetHashCode(const Value: string): Integer;
  {Gets hash of a string.
    @param Value [in] String for which hash is needed.
    @return Required hash.
  }
begin
  Result := ElfHash(Value);
end;

end.


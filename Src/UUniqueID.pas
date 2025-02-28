{
 * This Source Code Form is subject to the terms of the Mozilla Public License,
 * v. 2.0. If a copy of the MPL was not distributed with this file, You can
 * obtain one at https://mozilla.org/MPL/2.0/
 *
 * Copyright (C) 2009-2021, Peter Johnson (gravatar.com/delphidabbler).
 *
 * Implements a generator of globally unique ids.
}


unit UUniqueID;


interface


uses
  // Delphi
  SysUtils;


type

  ///  <summary>Generator of globally unique ids.</summary>
  TUniqueID = record
  strict private

    ///  <summary>Generates a new GUID and returns it as an array of bytes.
    ///  </summary>
    class function NewUID: TBytes; static;

  public

    ///  <summary>Generates a globally unique string of 32 hexadecimal
    ///  characters.</summary>
    class function Generate: string; static;
    {TODO -cClarity: Rename above method as GenerateHex}

    ///  <summary>Generates a globally unique string of 32 alphabetical
    ///  characters.</summary>
    ///  <remarks>
    ///  <para>The returned string comprises the letters 'A' to 'S', omitting
    ///  'I' and 'O'.</para>
    ///  <para>The string is a valid Pascal identifier.</para>
    ///  </remarks>
    class function GenerateAlpha: string; static;

  end;


implementation


uses
  // Project
  UUtils;


{ TUniqueID }

class function TUniqueID.Generate: string;
var
  Bytes: TBytes;
  B: Byte;
begin
  Bytes := NewUID;
  Result := '';
  for B in Bytes do
    Result := Result + IntToHex(B, 2);
end;

class function TUniqueID.GenerateAlpha: string;
type
  TNybble = 0..15;
const
  AlphaMap: array[TNybble] of Char = (
    'A', 'B', 'C', 'D', 'E', 'F', 'G', 'H',
    'J', 'K', 'L', 'M', 'P', 'Q', 'R', 'S'
  );
var
  Bytes: TBytes;
  B: Byte;
begin
  Bytes := NewUID;
  Result := '';
  for B in Bytes do
  begin
    Result := Result
      + AlphaMap[TNybble(B shr 4)]
      + AlphaMap[TNybble(B and $0F)];
  end;
end;

class function TUniqueID.NewUID: TBytes;
var
  GUID: TGUID;
begin
  CreateGUID(GUID);
  Result := GUIDToBytes(GUID);
end;

end.


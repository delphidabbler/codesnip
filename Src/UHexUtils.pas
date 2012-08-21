{
 * UHexUtils.pas
 *
 * Routines that support conversion of hexadecimal strings to and from the
 * corresponding binary data.
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
 * The Original Code is UHexUtils.pas
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


unit UHexUtils;


interface


uses
  // Delphi
  SysUtils;


function BytesToHex(const Bytes: TBytes): string;
  {Creates a hexadecimal representation of the bytes in an array.
    @param Bytes [in] Byte array.
    return Hexadecimal representation of bytes in array.
  }

function TryHexToBytes(HexStr: string; out Bytes: TBytes): Boolean;
  {Attempts to convert a hexadecimal string into binary data that is stored in a
  byte array.
    @param HexStr [in] String of hex digits optionally prefixed by hex prefix.
    @param Buf [out] Byte array that receives the converted data. Undefined if
      function returns False.
    @return True if HexStr successfully converted, False if not.
  }


implementation


function StripHexPrefix(const HexStr: string): string;
  forward;
function TryHexToInt(const HexStr: string; out Value: Integer): Boolean;
  forward;


function AddHexPrefix(const HexStr: string): string;
  {Ensures a string of hex digits is prefixed by hex prefix string.
    @param HexStr [in] String of hex characters.
    @return HexStr prefixed by correct prefix. Unchanged if HexStr already has
      correct prefix.
  }
begin
  Result := SysUtils.HexDisplayPrefix + StripHexPrefix(HexStr);
end;

function BufToHex(const Buf; const Size: Cardinal): string;
  {Creates a hexadecimal representation of the bytes in a buffer.
    @param Buf [in] Buffer containing data.
    @param Size [in] Size of buffer in bytes.
    @return Hexadecimal representation of bytes in buffer.
  }
const
  // maps nibbles to hex digits
  cHexDigits: array[$0..$F] of Char = (
    '0', '1', '2', '3', '4', '5', '6', '7',
    '8', '9', 'A', 'B', 'C', 'D', 'E', 'F'
  );
var
  I: Cardinal;  // loops thru output string
  PB: ^Byte;    // addresses each byte in buffer
begin
  PB := @Buf;
  SetLength(Result, 2 * Size);
  I := 1;
  while I <= 2 * Size do
  begin
    Result[I] := cHexDigits[PB^ shr 4];
    Result[I + 1] := cHexDigits[PB^ and $0F];
    Inc(PB);
    Inc(I, 2);
  end;
end;

function BytesToHex(const Bytes: TBytes): string;
  {Creates a hexadecimal representation of the bytes in an array.
    @param Bytes [in] Byte array.
    return Hexadecimal representation of bytes in array.
  }
begin
  Result := BufToHex(Bytes[0], Length(Bytes));
end;

function HexByteSize(HexStr: string): Cardinal;
  {Get the number of bytes represented by a hexadecimal string.
    @param HexStr [in] Hexadecimal string with option hex prefix string.
    @return Number of bytes represented by HexStr.
  }
begin
  HexStr := StripHexPrefix(HexStr);
  Result := (Length(HexStr) div 2) + (Length(HexStr) mod 2);
end;

function StripHexPrefix(const HexStr: string): string;
  {Removes any hex prefix string from a string of hexadecimal digits.
    @param HexStr [in] String of hex digits optionally prefixed by hex prefix.
    @return HexStr without any hex prefix. Unchanged if HexStr has no prefix.
  }
begin
  if Pos('$', HexStr) = 1 then
    Result := Copy(HexStr, 2, Length(HexStr) - 1)
  else if Pos('0x', SysUtils.LowerCase(HexStr)) = 1 then
    Result := Copy(HexStr, 3, Length(HexStr) - 2)
  else
    Result := HexStr;
end;

function TryHexToBuf(HexStr: string; var Buf): Boolean;
  {Attempts to convert a hexadecimal string into binary data that is stored in a
  buffer, which must be large enough to receive the data.
    @param HexStr [in] String of hex digits optionally prefixed by hex prefix.
    @param Buf [in/out] Buffer that receives the converted data. Undefined if
      function returns False.
    @return True if HexStr successfully converted, False if not.
  }
var
  I: Integer;       // loops through characters of string
  PB: ^Byte;        // references each byte in buffer
  ByteVal: Integer; // a byte value from hex string
begin
  Result := False;
  HexStr := StripHexPrefix(HexStr);
  if HexStr = '' then
    Exit;
  if Odd(Length(HexStr)) then
    HexStr := '0' + HexStr;
  I := 1;
  PB := @Buf;
  while I <= Length(HexStr) do
  begin
    if not TryHexToInt(HexStr[I] + HexStr[I + 1], ByteVal) then
      Exit;
    PB^ := Byte(ByteVal);
    Inc(I, 2);
    Inc(PB);
  end;
  Result := True;
end;

function TryHexToBytes(HexStr: string; out Bytes: TBytes): Boolean;
  {Attempts to convert a hexadecimal string into binary data that is stored in a
  byte array.
    @param HexStr [in] String of hex digits optionally prefixed by hex prefix.
    @param Buf [out] Byte array that receives the converted data. Undefined if
      function returns False.
    @return True if HexStr successfully converted, False if not.
  }
begin
  SetLength(Bytes, HexByteSize(HexStr));
  Result := TryHexToBuf(HexStr, Bytes[0]);
end;

function TryHexToInt(const HexStr: string; out Value: Integer): Boolean;
  {Attempts to convert a hexadecimal string into an integer.
    @param HexStr [in] String of hex digits optionally prefixed by hex prefix.
    @param Value [out] Set to integer value represented by HexStr. Undefined if
      function returns False.
    @return True if HexStr successfully converted, False if not.
  }
var
  E: Integer; // error code
begin
  Val(AddHexPrefix(HexStr), Value, E);
  Result := E = 0;
end;

end.


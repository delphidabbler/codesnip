{
 * UEncryptor.pas
 *
 * Static class that encrypts and decrypts binary data. Acts as a facade to the
 * actual encryption engine.
 *
 * All code that requires encryption should use this class rather than making
 * calls the the actual the encryption engine.
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
 * The Original Code is UEncryptor.pas
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


unit UEncryptor;


interface


uses
  // Delphi
  SysUtils,
  // Project
  UBaseObjects;


type

  {
  TEncryptor:
    Static class that encrypts and decrypts binary data. Acts as a facade to the
    actual encryption engine.
  }
  TEncryptor = class(TNoConstructObject)
  strict private
    const cEncryptKey = 46723;    // Key used for encryption / decryption
    class function BytesToRawString(const Bytes: TBytes): RawByteString;
      {Copies the bytes from a byte array into a raw byte string.
        @param Bytes [in] Bytes to be copied.
        @return Raw byte string containing bytes.
      }
    class function RawStringToBytes(const Str: RawByteString): TBytes;
      {Copies the bytes from a raw byte string into a byte array.
        @param Str [in] Raw byte string to be copied.
        @return Byte array containing bytes from string.
      }
  public
    class function Encrypt(const Bytes: TBytes): TBytes;
      {Encrypts data from a byte array.
        @param Bytes [in] Array of bytes to be encrypted.
        @return Byte array containing encrypted data.
      }
    class function Decrypt(const Bytes: TBytes): TBytes;
      {Decrypts data from a byte array.
        @param Bytes [in] Array of encrypted bytes to be decrypted.
        @return Byte array containing decrypted data.
      }
  end;


implementation


uses
  // 3rd party
  UEncrypt;


{ TEncryptor }

class function TEncryptor.BytesToRawString(const Bytes: TBytes): RawByteString;
  {Copies the bytes from a byte array into a raw byte string.
    @param Bytes [in] Bytes to be copied.
    @return Raw byte string containing bytes.
  }
begin
  SetLength(Result, Length(Bytes));
  if Length(Bytes) > 0 then
    Move(Bytes[0], Result[1], Length(Bytes));
end;

class function TEncryptor.Decrypt(const Bytes: TBytes): TBytes;
  {Decrypts data from a byte array.
    @param Bytes [in] Array of encrypted bytes to be decrypted.
    @return Byte array containing decrypted data.
  }
begin
  Result := RawStringToBytes(
    UEncrypt.Decrypt(
      BytesToRawString(Bytes), cEncryptKey
    )
  );
end;

class function TEncryptor.Encrypt(const Bytes: TBytes): TBytes;
  {Encrypts data from a byte array.
    @param Bytes [in] Array of bytes to be encrypted.
    @return Byte array containing encrypted data.
  }
begin
  Result := RawStringToBytes(
    UEncrypt.Encrypt(
      BytesToRawString(Bytes), cEncryptKey
    )
  );
end;

class function TEncryptor.RawStringToBytes(const Str: RawByteString): TBytes;
  {Copies the bytes from a raw byte string into a byte array.
    @param Str [in] Raw byte string to be copied.
    @return Byte array containing bytes from string.
  }
begin
  SetLength(Result, Length(Str));
  if Length(Str) > 0 then
    Move(Str[1], Result[0], Length(Str));
end;

end.


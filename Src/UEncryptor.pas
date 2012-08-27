{
 * This Source Code Form is subject to the terms of the Mozilla Public License,
 * v. 2.0. If a copy of the MPL was not distributed with this file, You can
 * obtain one at http://mozilla.org/MPL/2.0/
 *
 * Copyright (C) 2010-2012, Peter Johnson (www.delphidabbler.com).
 *
 * $Rev$
 * $Date$
 *
 * Static class that encrypts and decrypts binary data. Acts as a facade to the
 * actual encryption engine.
 *
 * All code that requires encryption should use this class rather than making
 * calls the the actual the encryption engine.
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


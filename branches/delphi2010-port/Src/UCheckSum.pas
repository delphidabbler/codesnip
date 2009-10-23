{
 * UCheckSum.pas
 *
 * Static class that gets and checks checksums of data. Uses the MD5 algorithm.
 *
 * Uses MD5.pas by Matthias Fichtner (modified by Peter Johnson 12 Jan 2006).
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
 * The Original Code is UCheckSum.pas
 *
 * The Initial Developer of the Original Code is Peter Johnson
 * (http://www.delphidabbler.com/).
 *
 * Portions created by the Initial Developer are Copyright (C) 2006-2009 Peter
 * Johnson. All Rights Reserved.
 *
 * Contributor(s)
 *   NONE
 *
 * ***** END LICENSE BLOCK *****
}


unit UCheckSum;


interface


uses
  // Delphi
  SysUtils, Classes,
  // Project
  UBaseObjects;


type

  {
  TCheckSum:
    Static class that gets and checks checksums of data. Uses the MD5 algorithm.
  }
  TCheckSum = class(TNoConstructObject)
  public
    class function Calculate(const Stm: TStream): AnsiString; overload;
      {Calculates MD5 checksum of whole contents of a stream. Position in stream
      is preserved.
        @param Stm [in] Stream to check.
        @return Checksum as Ansi string.
      }
    class function Calculate(const S: AnsiString): AnsiString; overload;
      {Calculates MD5 checksum of an Ansi string.
        @param S [in] String to check.
        @return Checksum as Ansi string.
      }
    {$IFDEF UNICODE}
    class function Calculate(const Bytes: TBytes): AnsiString; overload;
      {Calculates MD5 checksum of a byte array.
        @param Bytes [in] Byte array to check.
        @return Checksum as Ansi string.
      }
    {$ENDIF}
    class function Compare(const Stm: TStream;
      const CheckSum: AnsiString): Boolean; overload;
      {Compares MD5 checksum of content of a stream against a known checksum.
        @param Stm [in] Stream whose checksum is to be compared.
        @param CheckSum [in] Known checksum string to compare to stream's
          checksum.
        @return True if stream's checksum is same as CheckSum, False otherwise.
      }
    class function Compare(const S, CheckSum: AnsiString): Boolean; overload;
      {Compares MD5 checksum of an Ansi string against a known checksum.
        @param S [in] String whose checksum is to be compared.
        @param CheckSum [in] Known checksum string to compare to string's
          checksum.
        @return True if string's checksum is same as CheckSum, False otherwise.
      }
  end;


implementation


uses
  // 3rd Party
  MD5;


{ TCheckSum }

class function TCheckSum.Calculate(const Stm: TStream): AnsiString;
  {Calculates MD5 checksum of whole contents of a stream. Position in stream is
  preserved.
    @param Stm [in] Stream to check.
    @return Checksum as Ansi string.
  }
var
  StmCopy: TMemoryStream;     // copy of stream in memory
  StmPos: Int64;              // preserves stream position
  Context: MD5Context;        // MD5 context used in calculating checksum
  Digest: MD5Digest;          // MD5 digest stores checksum
begin
  StmCopy := TMemoryStream.Create;
  try
    // Copy stream to memory stream preserving position
    StmPos := Stm.Position;
    Stm.Position := 0;
    StmCopy.CopyFrom(Stm, 0);
    Stm.Position := StmPos;
    // Calculate MD5 checksum of copied stream as string
    MD5Init(Context);
    MD5Update(Context, StmCopy.Memory, StmCopy.Size);
    MD5Final(Context, Digest);
    Result := MD5Print(Digest);
  finally
    FreeAndNil(StmCopy);
  end;
end;

class function TCheckSum.Calculate(const S: AnsiString): AnsiString;
  {Calculates MD5 checksum of an Ansi string.
    @param S [in] String to check.
    @return Checksum as Ansi string.
  }
begin
  Result := MD5Print(MD5String(S));
end;

{$IFDEF UNICODE}
class function TCheckSum.Calculate(const Bytes: TBytes): AnsiString;
  {Calculates MD5 checksum of a byte array.
    @param Bytes [in] Byte array to check.
    @return Checksum as Ansi string.
  }
var
  Context: MD5Context;        // MD5 context used in calculating checksum
  Digest: MD5Digest;          // MD5 digest stores checksum
begin
  MD5Init(Context);
  MD5Update(Context, PByteArray(Bytes), Length(Bytes));
  MD5Final(Context, Digest);
  Result := MD5Print(Digest);
end;
{$ENDIF}

class function TCheckSum.Compare(const S, CheckSum: AnsiString): Boolean;
  {Compares MD5 checksum of an Ansi string against a known checksum.
    @param S [in] String whose checksum is to be compared.
    @param CheckSum [in] Known checksum string to compare to string's checksum.
    @return True if string's checksum is same as CheckSum, False otherwise.
  }
begin
  Result := Calculate(S) = CheckSum;
end;

class function TCheckSum.Compare(const Stm: TStream;
  const CheckSum: AnsiString): Boolean;
  {Compares MD5 checksum of content of a stream against a known checksum.
    @param Stm [in] Stream whose checksum is to be compared.
    @param CheckSum [in] Known checksum string to compare to stream's checksum.
    @return True if stream's checksum is same as CheckSum, False otherwise.
  }
begin
  Result := Calculate(Stm) = CheckSum;
end;

end.


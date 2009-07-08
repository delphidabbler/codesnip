{
 * UDataStreamReader.pas
 *
 * Stream wrapper that can read numeric and string data from a stream. Assumes
 * integers of various sizes are encoded as ASCII hex digits in stream.
 *
 * v0.1 of 30 Apr 2006  - Original version.
 * v1.0 of 24 May 2006  - Improved and corrected comments.
 *                      - Relocated and rationalised $WARN directives.
 * v1.1 of 22 May 2009  - Added new TDataStreamWriter.ReadSizedLongString
 *                        method.
 *
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
 * The Original Code is UDataStreamReader.pas
 *
 * The Initial Developer of the Original Code is Peter Johnson
 * (http://www.delphidabbler.com/).
 *
 * Portions created by the Initial Developer are Copyright (C) 2006-2009 Peter
 * Johnson. All Rights Reserved.
 *
 * ***** END LICENSE BLOCK *****
}


unit UDataStreamReader;

{$WARN UNSAFE_CAST OFF}
{$WARN UNSAFE_CODE OFF}

interface


uses
  // DelphiDabbler library
  PJStreamWrapper;


type

  {
  TDataStreamReader:
    Stream wrapper that can read numeric and string data from a stream. Assumes
    integers of various sizes are encoded as ASCII hex digits in stream.
  }
  TDataStreamReader = class(TPJStreamWrapper)
  private
    function ReadHexDigits(const Count: Integer): LongInt;
      {Reads hex digits from stream.
        @param Count [in] Number of hex digits to read.
        @return Value read from stream.
      }
  public
    function ReadSmallInt: SmallInt;
      {Reads small integer from stream, encoded as 4 hex digits.
        @return Value read from stream.
      }
    function ReadLongInt: LongInt;
      {Reads long integer from stream, encoded as 8 char hex digits.
        @return Value read from stream.
      }
    function ReadInt64: Int64;
      {Reads an Int64 value from stream, encoded as 16 char hex digit.
        @return Value read from stream.
      }
    function ReadString(const Length: Integer): string;
      {Reads a string of specified size from stream.
        @param Length [in] Length of string to be read.
        @return String read from stream.
      }
    function ReadSizedString: string;
      {Reads string from stream that is preceded by a small int length
      descriptor.
        @return String read from stream.
      }
    function ReadSizedLongString: string;
      {Reads string from stream that is preceded by a long int length
      descriptor.
        @return String read from stream.
      }
  end;


implementation


uses
  // Delphi
  SysUtils;


{ TDataStreamReader }

function TDataStreamReader.ReadHexDigits(
  const Count: Integer): LongInt;
  {Reads hex digits from stream.
    @param Count [in] Number of hex digits to read.
    @return Value read from stream.
  }
begin
  Result := StrToInt('$' + ReadString(Count));
end;

function TDataStreamReader.ReadInt64: Int64;
  {Reads an Int64 value from stream, encoded as 16 char hex digit.
    @return Value read from stream.
  }
begin
  Int64Rec(Result).Hi := ReadLongInt;
  Int64Rec(Result).Lo := ReadLongInt;
end;

function TDataStreamReader.ReadLongInt: LongInt;
  {Reads long integer from stream, encoded as 8 char hex digits.
    @return Value read from stream.
  }
begin
  Result := ReadHexDigits(8);
end;

function TDataStreamReader.ReadSizedLongString: string;
  {Reads string from stream that is preceded by a long int length descriptor.
    @return String read from stream.
  }
var
  Length: LongInt; // length of string
begin
  Length := ReadLongInt;
  Result := ReadString(Length);
end;

function TDataStreamReader.ReadSizedString: string;
  {Reads string from stream that is preceded by a small int length descriptor.
    @return String read from stream.
  }
var
  Length: SmallInt; // length of string
begin
  Length := ReadSmallInt;
  Result := ReadString(Length);
end;

function TDataStreamReader.ReadSmallInt: SmallInt;
  {Reads small integer from stream, encoded as 4 hex digits.
    @return Value read from stream.
  }
begin
  Result := ReadHexDigits(4);
end;

function TDataStreamReader.ReadString(const Length: Integer): string;
  {Reads a string of specified size from stream.
    @param Length [in] length of string to be read.
    @return String read from stream.
  }
begin
  SetLength(Result, Length);
  BaseStream.ReadBuffer(Result[1], Length);
end;

end.


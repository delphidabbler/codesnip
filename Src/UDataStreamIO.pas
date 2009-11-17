{
 * UDataStreamIO.pas
 *
 * Stream wrapper that can read numeric and string data from a stream. Assumes
 * integers of various sizes are encoded as ASCII hex digits in stream.
 *
 * Originally named UDataStreamReader.pas. Renamed as UDataStreamIO.pas at v2.0.
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
 * The Original Code is UDataStreamIO.pas
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


unit UDataStreamIO;


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
  strict private
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

  {
  TDataStreamWriter:
    Stream wrapper that can write numeric and string data to a stream. Integers
    of various sizes are encoded as ASCII hex digits in stream.
  }
  TDataStreamWriter = class(TPJStreamWrapper)
  strict private
    procedure WriteHex(const Value: LongInt; const Count: Integer);
      {Writes hex digits to stream.
        @param Value [in] Value to be written.
        @param Count [in] Number of hex digits required.
      }
  public
    procedure WriteSmallInt(const Value: SmallInt);
      {Writes a 16 bit integer to the stream as hex digits.
        @param Value [in] Value to be written.
      }
    procedure WriteLongInt(const Value: LongInt);
      {Write a 32 bit integer to the stream as hex digits.
        @param Value [in] Value to be written.
      }
    procedure WriteString(const Str: string; const Length: Integer); overload;
      {Writes a fixed number of characters from a string to the stream.
        @param Str [in] String to be written.
        @param Length [in] Number of characters from string to write.
      }
    procedure WriteString(const Str: string); overload;
      {Writes a string to stream.
        @param Str [in] String to be written.
      }
    procedure WriteSizedString(const Str: string);
      {Writes a string to stream preceded by a 16 bit length as hex digits.
        @param Str [in] String to be written.
      }
    procedure WriteSizedLongString(const Str: string);
      {Writes a string to stream preceded by a 32 bit length as hex digits.
        @param Str [in] String to be written.
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

{ TDataStreamWriter }

procedure TDataStreamWriter.WriteHex(const Value: LongInt;
  const Count: Integer);
  {Writes hex digits to stream.
    @param Value [in] Value to be written.
    @param Count [in] Number of hex digits required.
  }
begin
  WriteString(IntToHex(Value, Count), Count);
end;

procedure TDataStreamWriter.WriteLongInt(const Value: Integer);
  {Write a 32 bit integer to the stream as hex digits.
    @param Value [in] Value to be written.
  }
begin
  WriteHex(Value, 8);
end;

procedure TDataStreamWriter.WriteSizedLongString(const Str: string);
  {Writes a string to stream preceded by a 32 bit length as hex digits.
    @param Str [in] String to be written.
  }
begin
  WriteLongInt(Length(Str));
  WriteString(Str, Length(Str));
end;

procedure TDataStreamWriter.WriteSizedString(const Str: string);
  {Writes a string to stream preceded by a 16 bit length as hex digits.
    @param Str [in] String to be written.
  }
begin
  WriteSmallInt(Length(Str));
  WriteString(Str, Length(Str));
end;

procedure TDataStreamWriter.WriteSmallInt(const Value: SmallInt);
  {Writes a 16 bit integer to the stream as hex digits.
    @param Value [in] Value to be written.
  }
begin
  WriteHex(Word(Value), 4);
end;

procedure TDataStreamWriter.WriteString(const Str: string);
  {Writes a string to stream.
    @param Str [in] String to be written.
  }
begin
  WriteString(Str, Length(Str));
end;

procedure TDataStreamWriter.WriteString(const Str: string;
  const Length: Integer);
  {Writes a fixed number of characters from a string to the stream.
    @param Str [in] String to be written.
    @param Length [in] Number of characters from string to write.
  }
begin
  BaseStream.WriteBuffer(Str[1], Length);
end;

end.


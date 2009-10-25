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

{ TODO -cDocs : Update documentation for new and changed methods. }
{ TODO -cRefactor : Rationalise code: use WriteBytes for all stream output
  regardless of Unicode or not. }
{ TODO -cRefactor : Refactor out the TBytes overrides of the WriteXXXString
  methods. Not used publicly. }

uses
  // Delphi
  SysUtils,
  // DelphiDabbler library
  PJStreamWrapper,
  // Project
  UUnicodeHelper;


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
    function ReadString(const Length: Integer): Latin1String;
      {Reads a string of specified size from stream.
        @param Length [in] Length of string to be read.
        @return String read from stream.
      }
    function ReadSizedString: Latin1String;
      {Reads string from stream that is preceded by a small int length
      descriptor.
        @return String read from stream.
      }
    function ReadSizedLongString: Latin1String;
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
    {$IFDEF UNICODE}
    procedure WriteBytes(const Bytes: TBytes);
      {Writes an array of bytes to a stream. Bytes must represent valid Latin 1
      characters and must not be 0.
        @param Bytes [in] Bytes to be written.
      }
    {$ENDIF}
    procedure WriteHex(const Value: LongInt; const Count: Integer);
      {Writes a hex representation of a number to stream.
        @param Value [in] Value to be written.
        @param Count [in] Number of hex digits required.
      }
  public
    procedure WriteSmallInt(const Value: SmallInt);
      {Writes a 16 bit integer to the stream as hex digits.
        @param Value [in] Value to be written.
      }
    procedure WriteLongInt(const Value: LongInt);
      {Writes a 32 bit integer to the stream as hex digits.
        @param Value [in] Value to be written.
      }

    procedure WriteString(const Str: Latin1String;
      const Length: Integer); overload;
      {Writes a fixed number of characters from a string to the stream.
        @param Str [in] String to be written.
        @param Length [in] Number of characters from string to write.
      }
    procedure WriteString(const Str: Latin1String); overload;
      {Writes a string to stream.
        @param Str [in] String to be written.
      }
    {$IFDEF UNICODE}
    procedure WriteString(const Str: string; const Length: Integer); overload;
    procedure WriteString(const Str: string); overload;
    {$ENDIF}

    procedure WriteSizedString(const Str: Latin1String); overload;
      {Writes a string to stream preceded by a 16 bit length as hex digits.
        @param Str [in] String to be written.
      }
    {$IFDEF UNICODE}
    procedure WriteSizedString(const Bytes: TBytes); overload;
      {Writes an array of bytes to stream as a sized string preceeded by a 16
      bit length as hex digits. Bytes must represent valid Latin 1 characters
      and must not be zero.
        @param Bytes [in] Bytes to be written.
      }
    procedure WriteSizedString(const Str: string); overload;
    {$ENDIF}

    procedure WriteSizedLongString(const Str: Latin1String); overload;
      {Writes a string to stream preceded by a 32 bit length as hex digits.
        @param Str [in] String to be written.
      }
    {$IFDEF UNICODE}
    procedure WriteSizedLongString(const Str: string); overload;
    procedure WriteSizedLongString(const Bytes: TBytes); overload;
      {Writes an array of bytes to stream as a sized string preceeded by a 32
      bit length as hex digits. Bytes must represent valid Latin 1 characters
      and must not be zero.
        @param Bytes [in] Bytes to be written.
      }
    {$ENDIF}
  end;


implementation

{
  About data streams

  Data streams are always composed of text strings. Numbers are encoded as
  strings of hex digits.
  Text contained in the streams should be encoded in the Latin-1 code page.
  Characters occupy 1 byte.

  There are several "data types":
  + SmallInt          = 16 bit integer encoded as 4 hex digits
  + LongInt           = 32 bit integer encoded as 8 hex digits
  + Int64             = 64 bit integer encoded as 16 hex digits
  + SizedString       = variable length string of 1 byte chars preceeded by
                        a SmallInt that gives the number of characters
  + SizedLongString   = variable length string of 1 byte chars preceeded by
                        a LongInt that gives the number of characters
  + Character         = a single 1 byte character

  There is a pseudo-data type - the fixed size string that is composed of a
  known number of Characters.

  There is no meta-data in the stream to describe the data type of an element of
  the file. The reader / writer must have knowledge of the file format being
  read / written.
}


{ TDataStreamReader }

function TDataStreamReader.ReadHexDigits(const Count: Integer): LongInt;
  {Reads hex digits from stream.
    @param Count [in] Number of hex digits to read.
    @return Value read from stream.
  }
begin
  Result := StrToInt('$' + string(ReadString(Count)));
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

function TDataStreamReader.ReadSizedLongString: Latin1String;
  {Reads string from stream that is preceded by a long int length descriptor.
    @return String read from stream.
  }
var
  Length: LongInt; // length of string
begin
  Length := ReadLongInt;
  Result := ReadString(Length);
end;

function TDataStreamReader.ReadSizedString: Latin1String;
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

function TDataStreamReader.ReadString(const Length: Integer): Latin1String;
  {Reads a string of specified size from stream.
    @param Length [in] length of string to be read.
    @return String read from stream.
  }
begin
  SetLength(Result, Length);
  BaseStream.ReadBuffer(Result[1], Length);
end;

{ TDataStreamWriter }

{$IFDEF UNICODE}
procedure TDataStreamWriter.WriteBytes(const Bytes: TBytes);
  {Writes an array of bytes to a stream. Bytes must represent valid Latin 1
  characters and must not be 0.
    @param Bytes [in] Bytes to be written.
  }
begin
  BaseStream.WriteBuffer(Pointer(Bytes)^, Length(Bytes));
end;
{$ENDIF}

procedure TDataStreamWriter.WriteHex(const Value: LongInt;
  const Count: Integer);
  {Writes a hex representation of a number to stream.
    @param Value [in] Value to be written.
    @param Count [in] Number of hex digits required.
  }
begin
  WriteString(IntToHex(Value, Count), Count);
end;

procedure TDataStreamWriter.WriteLongInt(const Value: Integer);
  {Writes a 32 bit integer to the stream as hex digits.
    @param Value [in] Value to be written.
  }
begin
  WriteHex(Value, 8);
end;

procedure TDataStreamWriter.WriteSizedLongString(const Str: Latin1String);
  {Writes a string to stream preceded by a 32 bit length as hex digits.
    @param Str [in] String to be written.
  }
begin
  WriteLongInt(Length(Str));
  WriteString(Str, Length(Str));
end;

{$IFDEF UNICODE}
procedure TDataStreamWriter.WriteSizedLongString(const Bytes: TBytes);
  {Writes an array of bytes to stream as a sized string preceeded by a 32 bit
  length as hex digits. Bytes must represent valid Latin 1 characters and must
  not be zero.
    @param Bytes [in] Bytes to be written.
  }
begin
  WriteLongInt(Length(Bytes));
  WriteBytes(Bytes);
end;

procedure TDataStreamWriter.WriteSizedLongString(const Str: string);
begin
  WriteSizedLongString(Latin1BytesOf(Str));
end;
{$ENDIF}

procedure TDataStreamWriter.WriteSizedString(const Str: Latin1String);
  {Writes a string to stream preceded by a 16 bit length as hex digits.
    @param Str [in] String to be written.
  }
begin
  WriteSmallInt(Length(Str));
  WriteString(Str, Length(Str));
end;

{$IFDEF UNICODE}
procedure TDataStreamWriter.WriteSizedString(const Bytes: TBytes);
  {Writes an array of bytes to stream as a sized string preceeded by a 16 bit
  length as hex digits. Bytes must represent valid Latin 1 characters and must
  not be zero.
    @param Bytes [in] Bytes to be written.
  }
begin
  WriteSmallInt(Length(Bytes));
  WriteBytes(Bytes);
end;

procedure TDataStreamWriter.WriteSizedString(const Str: string);
begin
  WriteSizedString(Latin1BytesOf(Str));
end;
{$ENDIF}

procedure TDataStreamWriter.WriteSmallInt(const Value: SmallInt);
  {Writes a 16 bit integer to the stream as hex digits.
    @param Value [in] Value to be written.
  }
begin
  WriteHex(Value, 4);
end;

procedure TDataStreamWriter.WriteString(const Str: Latin1String);
  {Writes a string to stream.
    @param Str [in] String to be written.
  }
begin
  WriteString(Str, Length(Str));
end;

procedure TDataStreamWriter.WriteString(const Str: Latin1String;
  const Length: Integer);
  {Writes a fixed number of characters from a string to the stream.
    @param Str [in] String to be written.
    @param Length [in] Number of characters from string to write.
  }
begin
  Assert(SizeOf(AnsiChar) = 1,
    ClassName + '.WriteString: SizeOf(AnsiChar) <> 1');
  BaseStream.WriteBuffer(Pointer(Str)^, Length);
end;

{$IFDEF UNICODE}
procedure TDataStreamWriter.WriteString(const Str: string);
begin
  WriteString(Str, Length(Str));
end;

procedure TDataStreamWriter.WriteString(const Str: string;
  const Length: Integer);
var
  Bytes: TBytes;
begin
  Bytes := Latin1BytesOf(Str);
  BaseStream.WriteBuffer(Pointer(Bytes)^, Length);
end;
{$ENDIF}

end.


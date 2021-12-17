{
 * This Source Code Form is subject to the terms of the Mozilla Public License,
 * v. 2.0. If a copy of the MPL was not distributed with this file, You can
 * obtain one at https://mozilla.org/MPL/2.0/
 *
 * Copyright (C) 2006-2020, Peter Johnson (gravatar.com/delphidabbler).
 *
 * Stream wrappers read / write data from / to a formatted data stream. Classes
 * are provided to read from text or binary streams that conform to the data
 * formatting rules. Another class writes data to formatted binary streams.
}


unit UDataStreamIO;


interface


uses
  // Delphi
  SysUtils, Classes,
  // DelphiDabbler library
  PJStreamWrapper;


type

  ///  Set of values indicating whether a stream wrapper object owns, and hence
  ///  frees, its stream and encoding objects.
  TDataStreamOwnerships = set of (
    dsOwnsStream,   // object owns stream
    dsOwnsEncoding  // object owns encoding: ignored for standard encodings
  );

  ///  <summary>
  ///  Base class of all classes that read and write a formatted data stream.
  ///  Deals with encoding used for stream.
  ///  </summary>
  TDataStreamIO = class(TPJStreamWrapper)
  strict private
    ///  Value of Encoding property.
    var fEncoding: TEncoding;
    ///  Indicates if object owns encoding.
    var fOwnsEncoding: Boolean;
  strict protected
    ///  Encoding used when reading / writing strings.
    property Encoding: TEncoding read fEncoding;
  public
    ///  Creates object that wraps a specified stream. If OwnsObject is true
    ///  object will free the stream on destruction. Uses UTF-8 encoding.
    constructor Create(const Stream: TStream;
      const OwnsStream: Boolean = False); overload; override;
    ///  Creates object that wraps a specified stream and uses a specified
    ///  encoding to read or write strings. Ownerships indicate if object owns
    ///  either the stream or the encoding: owned objects will be freed on
    ///  destruction. If Encoding is nil UTF-8 will be used.
    constructor Create(const Stream: TStream; const Encoding: TEncoding;
      const OwnerShips: TDataStreamOwnerships); reintroduce; overload; virtual;
    ///  Tears down object, freeing any owned objects. NOTE: standard encodings
    ///  are never freed regardless of ownership.
    destructor Destroy; override;
  end;

  ///  <summary>
  ///  Abstract base class for all object that read formatted data from a
  ///  stream. Defines a standard interface and supplies common functionality.
  ///  </summary>
  TDataStreamReader = class abstract(TDataStreamIO)
  strict protected
    ///  Reads CharCount bytes and converts them to a Unicode string.
    function ReadString(CharCount: Integer): UnicodeString;
  public
    ///  Reads a single byte.
    function ReadByte: Byte; virtual; abstract;
    ///  Reads a 16 bit integer value.
    function ReadInt16: SmallInt; virtual; abstract;
    ///  Reads a 32 bit integer value.
    function ReadInt32: LongInt; virtual; abstract;
    ///  Reads a 64 bit integer value.
    function ReadInt64: Int64; virtual; abstract;
    ///  Reads ByteCount bytes and returns them unprocessed.
    function ReadRawData(ByteCount: Integer): TBytes;
    ///  Reads a number of bytes specified by a preceding 16 bit integer and
    ///  returns them unprocessed.
    function ReadSizedRawData16: TBytes;
    ///  Reads a number of bytes specified by a preceding 32 bit integer and
    ///  returns them unprocessed.
    function ReadSizedRawData32: TBytes;
    ///  Reads ByteCount formatted bytes.
    function ReadBytes(ByteCount: Integer): TBytes; virtual; abstract;
    ///  Reads a number of formatted bytes specified by a preceding 16 bit
    ///  integer.
    function ReadSizedBytes16: TBytes;
    ///  Reads a number of formatted bytes specified by a preceding 32 bit
    ///  integer.
    function ReadSizedBytes32: TBytes;
    ///  Reads a number of bytes specified by a preceding 16 bit integer and
    ///  converts them into a Unicode string using the encoding specified by
    ///  the Encoding property.
    function ReadSizedString16: UnicodeString;
    ///  Reads a number of bytes specified by a preceding 32 bit integer and
    ///  converts them into a Unicode string using the encoding specified by
    ///  the Encoding property.
    function ReadSizedString32: UnicodeString;
  end;

  ///  <summary>
  ///  Class that reads binary data from a stream.
  ///  </summary>
  ///  <remarks>
  ///  Byte array and string data may be preceded by integer values that provide
  ///  the length of the following data. String data is converted from its
  ///  binary representation using the encoding specified by the Encoding
  ///  property.
  ///  </remarks>
  TBinaryStreamReader = class(TDataStreamReader)
  public
    ///  Reads a single byte from its raw binary representation
    function ReadByte: Byte; override;
    ///  Reads a 16 bit integer value from its raw binary representation.
    function ReadInt16: SmallInt; override;
    ///  Reads a 32 bit integer value from its raw binary representation.
    function ReadInt32: LongInt; override;
    ///  Reads a 64 bit integer value from its raw binary representation.
    function ReadInt64: Int64; override;
    ///  Reads ByteCount bytes from their raw binary representation. In this
    ///  class this method is the same as ReadRawData.
    function ReadBytes(ByteCount: Integer): TBytes; override;
  end;

  ///  <summary>
  ///  Class that reads text formatted data from a stream.
  ///  </summary>
  ///  <remarks>
  ///  <para>Numeric data is represented by hex characters that are stored as
  ///  ASCII characters. Therefore only encodings in which the hex characters
  ///  are the same as the ASCII characters can be used.</para>
  ///  <para>Byte array and string data may be preceded by hex encoded integer
  ///  values that provide the length of the following data. String data is
  ///  converted from its binary representation using the encoding specified
  ///  by the Encoding property.</para>
  ///  </remarks>
  TTextStreamReader = class(TDataStreamReader)
  strict private
    function ReadHexDigits(const Count: Integer): LongInt;
  public
    ///  Creates object that wraps a specified stream and uses a specified
    ///  encoding to read or write strings. Ownerships indicate if object owns
    ///  either the stream or the encoding: owned objects will be freed on
    ///  destruction. NOTE: The encoding must be a TMBCSEncoding where hex
    ///  digits each encode as single bytes.
    constructor Create(const Stream: TStream; const Encoding: TEncoding;
      const OwnerShips: TDataStreamOwnerships); overload; override;
    ///  Reads a single byte encoded as 2 hex characters.
    function ReadByte: Byte; override;
    ///  Reads a 16 bit integer encoded as 4 hex characters.
    function ReadInt16: SmallInt; override;
    ///  Reads a 32 bit integer encoded as 8 hex characters.
    function ReadInt32: LongInt; override;
    ///  Reads a 64 bit integer encoded as 4 hex characters.
    function ReadInt64: Int64; override;
    ///  Reads ByteCount bytes. Each byte is encoded as 2 hex characters. In
    ///  this class this method is *not* the same as ReadRawData.
    function ReadBytes(ByteCount: Integer): TBytes; override;
  end;

  ///  <summary>
  ///  Class that writes data to a stream using its internal binary
  ///  representation.
  ///  </summary>
  ///  <remarks>
  ///  Byte array and string data may have size information prepended. Strings
  ///  are converted to bytes arrays before being written using the encoding
  ///  specified by the Encoding property.
  ///  </remarks>
  TBinaryStreamWriter = class(TDataStreamIO)
  public
    ///  Writes a 16 bit integer in binary.
    procedure WriteInt16(const I: SmallInt);
    ///  Writes a 32 bit integer in binary.
    procedure WriteInt32(const I: LongInt);
    ///  Writes an array of bytes in binary. The size of the array is not
    ///  recorded.
    procedure WriteBytes(const B: TBytes);
    ///  Writes an array of bytes in binary, prepended by a 16 bit integer
    ///  specifying the number of bytes in the array.
    procedure WriteSizedBytes16(const B: TBytes);
    ///  Writes an array of bytes in binary, prepended by a 32 bit integer
    ///  specifying the number of bytes in the array.
    procedure WriteSizedBytes32(const B: TBytes);
    ///  Encodes the string into an array of bytes and writes it, prepended by
    ///  a 16 bit integer specifying the number of bytes written.
    procedure WriteSizedString16(const Str: UnicodeString);
    ///  Encodes the string into an array of bytes and writes it, prepended by
    ///  a 32 bit integer specifying the number of bytes written.
    procedure WriteSizedString32(const Str: UnicodeString);
  end;


implementation


uses
  // Project
  UEncodings;


{ TDataStreamIO }

constructor TDataStreamIO.Create(const Stream: TStream;
  const OwnsStream: Boolean);
var
  Ownerships: TDataStreamOwnerships;  // object ownership requirements
begin
  Ownerships := [];
  if OwnsStream then
    Include(Ownerships, dsOwnsStream);
  Create(Stream, nil, Ownerships);
end;

constructor TDataStreamIO.Create(const Stream: TStream;
  const Encoding: TEncoding; const Ownerships: TDataStreamOwnerships);
begin
  inherited Create(Stream, dsOwnsStream in  Ownerships);
  if Assigned(Encoding) then
  begin
    fEncoding := Encoding;
    fOwnsEncoding := (dsOwnsEncoding in OwnerShips);
  end
  else
  begin
    fEncoding := TEncoding.UTF8;
    fOwnsEncoding := True;
  end;
end;

destructor TDataStreamIO.Destroy;
begin
  inherited;
  if fOwnsEncoding then
    TEncodingHelper.FreeEncoding(fEncoding);
end;

{ TDataStreamReader }

function TDataStreamReader.ReadRawData(ByteCount: Integer): TBytes;
begin
  if (ByteCount < 0) then
    ByteCount := 0;
  SetLength(Result, ByteCount);
  if ByteCount = 0 then
    Exit;
  BaseStream.ReadBuffer(Pointer(Result)^, ByteCount);
end;

function TDataStreamReader.ReadSizedBytes16: TBytes;
begin
  Result := ReadBytes(ReadInt16);
end;

function TDataStreamReader.ReadSizedBytes32: TBytes;
begin
  Result := ReadBytes(ReadInt32);
end;

function TDataStreamReader.ReadSizedRawData16: TBytes;
begin
  Result := ReadRawData(ReadInt16);
end;

function TDataStreamReader.ReadSizedRawData32: TBytes;
begin
  Result := ReadRawData(ReadInt32);
end;

function TDataStreamReader.ReadSizedString16: UnicodeString;
begin
  Result := ReadString(ReadInt16);
end;

function TDataStreamReader.ReadSizedString32: UnicodeString;
begin
  Result := ReadString(ReadInt32);
end;

function TDataStreamReader.ReadString(CharCount: Integer): UnicodeString;
var
  Bytes: TBytes;
begin
  if CharCount <= 0 then
    Exit('');
  Bytes := ReadRawData(CharCount);
  Result := Encoding.GetString(Bytes);
end;

{ TTextStreamReader }

constructor TTextStreamReader.Create(const Stream: TStream;
  const Encoding: TEncoding; const OwnerShips: TDataStreamOwnerships);
begin
  Assert(not Assigned(Encoding) or (Encoding is TMBCSEncoding),
    ClassName + '.Create: Encoding must be TMBCSEncoding descendant');
  inherited Create(Stream, Encoding, OwnerShips);
end;

function TTextStreamReader.ReadByte: Byte;
begin
  Result := ReadHexDigits(2);
end;

function TTextStreamReader.ReadBytes(ByteCount: Integer): TBytes;
var
  I: Integer;
begin
  if ByteCount < 0 then
    ByteCount := 0;
  SetLength(Result, ByteCount);
  if ByteCount = 0 then
    Exit;
  for I := 0 to Pred(ByteCount) do
    Result[I] := ReadByte;
end;

function TTextStreamReader.ReadHexDigits(const Count: Integer): LongInt;
begin
  Result := StrToInt('$' + ReadString(Count));
end;

function TTextStreamReader.ReadInt16: SmallInt;
begin
  Result := ReadHexDigits(4);
end;

function TTextStreamReader.ReadInt32: LongInt;
begin
  Result := ReadHexDigits(8);
end;

function TTextStreamReader.ReadInt64: Int64;
begin
  Int64Rec(Result).Hi := ReadInt32;
  Int64Rec(Result).Lo := ReadInt32;
end;

{ TBinaryStreamReader }

function TBinaryStreamReader.ReadByte: Byte;
begin
  BaseStream.ReadBuffer(Result, SizeOf(Result));
end;

function TBinaryStreamReader.ReadBytes(ByteCount: Integer): TBytes;
begin
  Result := ReadRawData(ByteCount);
end;

function TBinaryStreamReader.ReadInt16: SmallInt;
begin
  BaseStream.ReadBuffer(Result, SizeOf(Result));
end;

function TBinaryStreamReader.ReadInt32: LongInt;
begin
  BaseStream.ReadBuffer(Result, SizeOf(Result));
end;

function TBinaryStreamReader.ReadInt64: Int64;
begin
  BaseStream.ReadBuffer(Result, SizeOf(Result));
end;

{ TBinaryStreamWriter }

procedure TBinaryStreamWriter.WriteBytes(const B: TBytes);
begin
  if Length(B) = 0 then
    Exit;
  BaseStream.WriteBuffer(Pointer(B)^, Length(B));
end;

procedure TBinaryStreamWriter.WriteInt16(const I: SmallInt);
begin
  BaseStream.WriteBuffer(I, SizeOf(I));
end;

procedure TBinaryStreamWriter.WriteInt32(const I: Integer);
begin
  BaseStream.WriteBuffer(I, SizeOf(I));
end;

procedure TBinaryStreamWriter.WriteSizedBytes16(const B: TBytes);
begin
  WriteInt16(Length(B));
  WriteBytes(B);
end;

procedure TBinaryStreamWriter.WriteSizedBytes32(const B: TBytes);
begin
  WriteInt32(Length(B));
  WriteBytes(B);
end;

procedure TBinaryStreamWriter.WriteSizedString16(const Str: UnicodeString);
begin
  WriteSizedBytes16(Encoding.GetBytes(Str));
end;

procedure TBinaryStreamWriter.WriteSizedString32(const Str: UnicodeString);
begin
  WriteSizedBytes32(Encoding.GetBytes(Str));
end;

end.


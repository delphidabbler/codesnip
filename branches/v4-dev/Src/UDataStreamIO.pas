{
 * UDataStreamIO.pas
 *
 * Stream wrappers read / write data from / to a formatted stream. Classes are
 * provided to read from text or binary streams that conform to the data
 * formatting rules. Another class writes data to formatted binary streams.
 *
 * $Rev$
 * $Date$
 *
 * Originally named UDataStreamReader.pas.
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
 * Portions created by the Initial Developer are Copyright (C) 2006-2010 Peter
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
  // Delphi
  SysUtils, Classes,
  // DelphiDabbler library
  PJStreamWrapper,
  // Project
  UEncodings;


type

  ///  Set of values indicating whether a stream wrapper object owns, and hence
  ///  frees, its stream and encoding objects
  TDataStreamOwnerships = set of (dsOwnsStream, dsOwnsEncoding);

  { TODO: Revise unit's documentation. }

  TDataStreamIO = class(TPJStreamWrapper)
  strict private
    var fEncoding: TEncoding;
    var fOwnsEncoding: Boolean;
  strict protected
    property Encoding: TEncoding read fEncoding;
  public
    constructor Create(const Stream: TStream;
      const OwnsStream: Boolean = False); overload; override;
    constructor Create(const Stream: TStream; const Encoding: TEncoding;
      const OwnerShips: TDataStreamOwnerships); reintroduce; overload; virtual;
    destructor Destroy; override;
  end;

  TDataStreamReader = class(TDataStreamIO)
  strict protected
    function ReadString(CharCount: Integer): UnicodeString;
  public
    function ReadByte: Byte; virtual; abstract;
    function ReadInt16: SmallInt; virtual; abstract;
    function ReadInt32: LongInt; virtual; abstract;
    function ReadInt64: Int64; virtual; abstract;

    function ReadRawData(ByteCount: Integer): TBytes;
    function ReadSizedRawData16: TBytes;
    function ReadSizedRawData32: TBytes;

    function ReadBytes(ByteCount: Integer): TBytes; virtual; abstract;
    function ReadSizedBytes16: TBytes;
    function ReadSizedBytes32: TBytes;

    function ReadSizedString16: UnicodeString;
    function ReadSizedString32: UnicodeString;
  end;

  TBinaryStreamReader = class(TDataStreamReader)
  public
    function ReadByte: Byte; override;
    function ReadInt16: SmallInt; override;
    function ReadInt32: LongInt; override;
    function ReadInt64: Int64; override;
    function ReadBytes(ByteCount: Integer): TBytes; override;
  end;

  TTextStreamReader = class(TDataStreamReader)
  strict private
    function ReadHexDigits(const Count: Integer): LongInt;
  public
    constructor Create(const Stream: TStream; const Encoding: TEncoding;
      const OwnerShips: TDataStreamOwnerships); overload; override;
    function ReadByte: Byte; override;
    function ReadInt16: SmallInt; override;
    function ReadInt32: LongInt; override;
    function ReadInt64: Int64; override;
    function ReadBytes(ByteCount: Integer): TBytes; override;
  end;

  TBinaryStreamWriter = class(TDataStreamIO)
  public
    procedure WriteInt16(const I: SmallInt);
    procedure WriteInt32(const I: LongInt);
    procedure WriteBytes(const B: TBytes);
    procedure WriteSizedBytes16(const B: TBytes);
    procedure WriteSizedBytes32(const B: TBytes);
    procedure WriteSizedString16(const Str: UnicodeString);
    procedure WriteSizedString32(const Str: UnicodeString);
  end;

implementation

{ TDataStreamIO }

constructor TDataStreamIO.Create(const Stream: TStream;
  const OwnsStream: Boolean);
var
  Ownerships: TDataStreamOwnerships;
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
    fEncoding := TMBCSEncoding.Create(Windows1252CodePage);
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


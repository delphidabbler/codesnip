{
  This Source Code Form is subject to the terms of the Mozilla Public License,
  v. 2.0. If a copy of the MPL was not distributed with this file, You can
  obtain one at https://mozilla.org/MPL/2.0/

  Copyright (C) 2024, Peter Johnson (gravatar.com/delphidabbler).

  Data types that encapsulate text data in different encodings.
}

unit CSLE.TextData;

{$SCOPEDENUMS ON}

interface

uses
  System.SysUtils,
  System.Classes;

type
  ASCIIString = type AnsiString(20127);

  TTextDataType = (
    ASCII = 0,    // data bytes represent ASCII string
    ANSI = 1,     // default ANSI encoding for local system
    UTF8 = 2      // data bytes represent UTF-8 string
  );

  TTextData = record
  strict private
    var
      fData: TBytes;
      fDataType: TTextDataType;
    class var
      fEncodingMap: array[TTextDataType] of TEncoding;
    class function CopyBytes(const ABytes: TBytes): TBytes; static;
    class function BytesToRawByteString(const ABytes: TBytes; const CP: UInt16):
      RawByteString; static;
    class function RawByteStringToBytes(const AStr: RawByteString): TBytes;
      static;
    function ToRawByteString(const AWantedType: TTextDataType): RawByteString;
  public
    class constructor Create;
    constructor Create(const AData: TBytes; const ADataType: TTextDataType);
      overload;
    constructor Create(const AStr: string; const ADataType: TTextDataType);
      overload;
    constructor Create(const AStr: RawByteString); overload;
    // If ACount <= 0 then whole of remainder of stream is read
    constructor Create(const AStream: TStream;
      const ADataType: TTextDataType; const ACount: Int64 = 0); overload;
    ///  <summary>Initialises new record instance to null ID.</summary>
    class operator Initialize(out Dest: TTextData);
    ///  <summary>Assigns a copy of the value of record <c>Src</c> to
    ///  <c>Dest</c>.</summary>
    class operator Assign(var Dest: TTextData;
      const [ref] Src: TTextData);

    function DataLength: NativeUInt; inline;
    function Encoding: TEncoding; inline;
    function ToString: string; inline;
    function ToANSIString: AnsiString;
    function ToASCIIString: ASCIIString;
    function ToUTF8String: UTF8String;

    class function SupportsString(const ADataType: TTextDataType;
      const AStr: string): Boolean; static;

    property Data: TBytes read fData;
    property DataType: TTextDataType read fDataType;

    ///  <summary>Compares two text data records for equality.</summary>
    class operator Equal(const Left, Right: TTextData): Boolean;
    ///  <summary>Compares two text data records for inequality.</summary>
    class operator NotEqual(const Left, Right: TTextData): Boolean; inline;

  end;

implementation

{ TTextData }

class operator TTextData.Assign(var Dest: TTextData;
  const [ref] Src: TTextData);
begin
  // Don't do: Dest := TTextData.Create(Src.fData, Src.fDataType);
  // It causes stack overflow, presumably because Dest := XXX causes recursion
  Dest.fData := CopyBytes(Src.fData);
  Dest.fDataType := Src.fDataType;
end;

class function TTextData.BytesToRawByteString(const ABytes: TBytes;
  const CP: UInt16): RawByteString;
begin
  Assert(Assigned(ABytes));

  var StrLen := System.Length(ABytes);
  SetLength(Result, StrLen);
  if StrLen > 0 then
  begin
    Move(ABytes[0], Result[1], StrLen);
    if Result[StrLen] = #0 then
      SetLength(Result, StrLen - 1);
  end;
  SetCodePage(Result, CP, False);
end;

class function TTextData.CopyBytes(const ABytes: TBytes): TBytes;
begin
  if System.Length(ABytes) > 0 then
    Result := Copy(ABytes, 0, System.Length(ABytes))
  else
    System.SetLength(Result, 0);
end;

class constructor TTextData.Create;
begin
  fEncodingMap[TTextDataType.ASCII] := TEncoding.ASCII;
  fEncodingMap[TTextDataType.ANSI] := TEncoding.ANSI;
  fEncodingMap[TTextDataType.UTF8] := TEncoding.UTF8;
end;

constructor TTextData.Create(const AData: TBytes;
  const ADataType: TTextDataType);
begin
  fData := CopyBytes(AData);
  fDataType := ADataType;
end;

constructor TTextData.Create(const AStr: string;
  const ADataType: TTextDataType);
begin
  fDataType := ADataType;
  fData := CopyBytes(fEncodingMap[ADataType].GetBytes(AStr));
end;

constructor TTextData.Create(const AStream: TStream;
  const ADataType: TTextDataType; const ACount: Int64);
begin
  // assume reading all of stream from current position to end
  var BytesToRead := AStream.Size - AStream.Position;
  if (ACount > 0) and (ACount < BytesToRead) then
    // Adjust number of bytes to read down to ACount
    BytesToRead := ACount;
  SetLength(fData, BytesToRead);
  AStream.Read(fData, BytesToRead);
  fDataType := ADataType;
end;

constructor TTextData.Create(const AStr: RawByteString);
begin
  if AStr <> '' then
  begin
    fData := RawByteStringToBytes(AStr);
    var CodePage := StringCodePage(AStr);
    if CodePage = TEncoding.ASCII.CodePage then
      fDataType := TTextDataType.ASCII
    else if CodePage = TEncoding.UTF8.CodePage then
      fDataType := TTextDataType.UTF8
    else if CodePage = TEncoding.ANSI.CodePage then
      fDataType := TTextDataType.ANSI
    else
      raise Exception.CreateFmt('Unsupported code page for string "%s"', [AStr]);
  end
  else
  begin
    SetLength(fData, 0);
    fDataType := TTextDataType.UTF8;
  end;
end;

function TTextData.DataLength: NativeUInt;
begin
  Result := System.Length(fData);
end;

function TTextData.Encoding: TEncoding;
begin
  Result := fEncodingMap[fDataType];
end;

class operator TTextData.Equal(const Left, Right: TTextData): Boolean;
begin
  Result := False;
  if Left.fDataType <> Right.fDataType then
    Exit;
  if Left.DataLength <> Right.DataLength then
    Exit;
  for var I := Low(Left.fData) to High(Left.fData) do
    if Left.fData[I] <> Right.fData[I] then
      Exit;
  Result := True;
end;

class operator TTextData.Initialize(out Dest: TTextData);
begin
  SetLength(Dest.fData, 0);
  Dest.fDataType := TTextDataType.UTF8;
end;

class operator TTextData.NotEqual(const Left, Right: TTextData): Boolean;
begin
  Result := not (Left = Right);
end;

class function TTextData.RawByteStringToBytes(
  const AStr: RawByteString): TBytes;
begin
  var BufLen := System.Length(AStr);
  SetLength(Result, BufLen);
  if BufLen > 0 then
    Move(AStr[1], Result[0], BufLen);
end;

class function TTextData.SupportsString(const ADataType: TTextDataType;
  const AStr: string): Boolean;
begin
  var Bytes := fEncodingMap[ADataType].GetBytes(AStr);
  var TestStr := fEncodingMap[ADataType].GetString(Bytes);
  Result := AStr = TestStr;
end;

function TTextData.ToANSIString: AnsiString;
begin
  Result := ToRawByteString(TTextDataType.ANSI);

  Assert(StringCodePage(Result) = fEncodingMap[TTextDataType.ANSI].CodePage);
end;

function TTextData.ToASCIIString: ASCIIString;
begin
  Result := ToRawByteString(TTextDataType.ASCII);

  Assert(StringCodePage(Result) = fEncodingMap[TTextDataType.ASCII].CodePage);
end;

function TTextData.ToRawByteString(const AWantedType: TTextDataType):
  RawByteString;
begin
  var Bytes: TBytes;
  if AWantedType = fDataType then
    Bytes := fData
  else
    Bytes := fEncodingMap[AWantedType].GetBytes(ToString);
  Result := BytesToRawByteString(Bytes, fEncodingMap[AWantedType].CodePage);
end;

function TTextData.ToString: string;
begin
  Result := fEncodingMap[fDataType].GetString(fData);
end;

function TTextData.ToUTF8String: UTF8String;
begin
  Result := ToRawByteString(TTextDataType.UTF8);

  Assert(StringCodePage(Result) = fEncodingMap[TTextDataType.UTF8].CodePage);
end;

end.


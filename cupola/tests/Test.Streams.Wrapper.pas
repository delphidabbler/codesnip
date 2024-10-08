{
  This unit is dedicated to public domain under the CC0 license.
  See https://creativecommons.org/public-domain/cc0/
}

unit Test.Streams.Wrapper;

interface

uses
  DUnitX.TestFramework,

  System.SysUtils,
  System.Classes,

  CSLE.Streams.Wrapper;

type

  TInstanceCountedStream = class(TBytesStream)
  strict private
    class var
      fInstanceCount: Integer;
  public
    class constructor Create;
    constructor Create(const ABytes: TBytes);
    destructor Destroy; override;
    class property InstanceCount: Integer read fInstanceCount;
  end;

  [TestFixture]
  TTestStreamsWrapper = class
  strict private
    const
      T2 = 'Lorem ipsum dolor sit amet, consectetur adipiscing elit. '
         + 'Proin et erat a mi aliquam maximus. Sed aliquam sodales dapibus. '
         + 'Sed vehicula pretium nulla, sed varius sapien tempus sed. '
         + 'Duis justo nisi, efficitur a sagittis non, dignissim id arcu. '
         + 'Sed cursus tincidunt turpis a cursus. '
         + 'Donec sit amet imperdiet felis.';
      B1: TBytes = [42,56,72,96,1,99,5,128,120,255];  // length 10
    var
      B2: TBytes; // will contain 0..255, 256 elements
      fICStream: TInstanceCountedStream;
      fEmptyStream: TBytesStream;
      fB1Stream, fB2Stream: TBytesStream;
      fTextStream: TBytesStream;
      fSWEmpty, fSWB1, fSWB2, fSWText: TStreamWrapper;
    class function GetStreamBytes(const Stm: TBytesStream): TBytes; static;
  public
    [Setup]
    procedure Setup;
    [TearDown]
    procedure TearDown;

    [Test]
    procedure ctor_without_wrapped_stream_ownership_doesnt_free_stream_on_destruction;
    [Test]
    procedure ctor_with_wrapped_stream_ownership_frees_stream_on_destruction;

    [Test]
    procedure BaseStream_echos_underlying_stream;

    // Position prop tests indirectly test the protected SetSize 64 bit method.
    // We don't test the SetSize 32 method overload method since (a) it is
    // deprecated and (b) it simply calls the 64 bit version.
    [Test]
    procedure Position_set_reflected_in_underlying_stream;
    [Test]
    procedure Position_get_reflects_underlying_stream;

    [Test]
    procedure Seek_32_seek_to_start_of_stream;
    [Test]
    procedure Seek_32_seek_from_start_of_stream;
    [Test]
    procedure Seek_32_seek_to_end_of_stream;
    [Test]
    procedure Seek_32_seek_from_end_of_stream;
    [Test]
    procedure Seek_32_seek_from_current_position_in_stream;
    [Test]
    procedure Seek_32_get_current_position_in_stream;

    [Test]
    procedure Seek_64_seek_to_start_of_stream;
    [Test]
    procedure Seek_64_seek_from_start_of_stream;
    [Test]
    procedure Seek_64_seek_to_end_of_stream;
    [Test]
    procedure Seek_64_seek_from_end_of_stream;
    [Test]
    procedure Seek_64_seek_from_current_position_in_stream;
    [Test]
    procedure Seek_64_get_current_position_in_stream;

    [Test]
    procedure Read_1st_16_bytes_from_B2_array_succeeds;
    [Test]
    procedure Read_all_bytes_from_text_array_succeeds;
    [Test]
    procedure Read_of_1_byte_from_empty_stream_fails;
    [Test]
    procedure Read_of_5_bytes_from_B1_array_at_position_8_fails;

    [Test]
    procedure Read64_1st_16_bytes_from_B2_array_succeeds;
    [Test]
    procedure Read64_all_bytes_from_text_array_succeeds;
    [Test]
    procedure Read64_of_1_byte_from_empty_stream_fails;
    [Test]
    procedure Read64_of_5_bytes_from_B1_array_at_position_8_fails;

    // Testing Size method implicitly performs further tests on Seek
    [Test]
    procedure Size_prop_set_changes_size_of_underlying_stream;
    [Test]
    procedure Size_prop_get_reflects_size_of_underlying_stream;

    [Test]
    procedure Write_append_4_bytes_to_end_of_B1_array_succeeds;
    [Test]
    procedure Write_overwrite_1_byte_at_position_4_in_B1_array_succeeds;
    [Test]
    procedure Write_append_2_bytes_to_empty_stream_succeeds;

    [Test]
    procedure Write64_append_4_bytes_to_end_of_B1_array_succeeds;
    [Test]
    procedure Write64_overwrite_1_byte_at_position_4_in_B1_array_succeeds;
    [Test]
    procedure Write64_append_2_bytes_to_empty_stream_succeeds;
  end;

implementation

{ TTestStreamsWrapper }

procedure TTestStreamsWrapper.BaseStream_echos_underlying_stream;
begin
  Assert.AreEqual(fB1Stream.Bytes, (fSWB1.BaseStream as TBytesStream).Bytes);
end;

procedure TTestStreamsWrapper.ctor_without_wrapped_stream_ownership_doesnt_free_stream_on_destruction;
begin
  var IC := TInstanceCountedStream.InstanceCount;
  var WS := TStreamWrapper.Create(fICStream, False);
  WS.Free;
  Assert.AreEqual(IC, TInstanceCountedStream.InstanceCount);
end;

procedure TTestStreamsWrapper.ctor_with_wrapped_stream_ownership_frees_stream_on_destruction;
begin
  var IC := TInstanceCountedStream.InstanceCount;
  var WS := TStreamWrapper.Create(fICStream, True);
  WS.Free;
  fICStream := nil;
  Assert.AreEqual(IC - 1, TInstanceCountedStream.InstanceCount);
end;

class function TTestStreamsWrapper.GetStreamBytes(
  const Stm: TBytesStream): TBytes;
begin
  // NOTE: TBytesStream.Bytes often has a length larger than stream size, and is
  //       padded with zero bytes. So, to get number of bytes actually in the
  //       stream, we need to truncate .Bytes to stream size.
  Result := Copy(Stm.Bytes, 0, Stm.Size);
end;

procedure TTestStreamsWrapper.Position_get_reflects_underlying_stream;
begin
  const Pos = Int64(10);
  fB1Stream.Position := Pos;
  Assert.AreEqual(Pos, fSWB1.Position);
end;

procedure TTestStreamsWrapper.Position_set_reflected_in_underlying_stream;
begin
  const Pos = Int64(8);
  fSWB1.Position := Pos;
  Assert.AreEqual(Pos, fB1Stream.Position);
end;

procedure TTestStreamsWrapper.Read64_1st_16_bytes_from_B2_array_succeeds;
begin
  var B16: TBytes;
  SetLength(B16, 16);
  var BytesRead := fSWB2.Read64(B16, 0, 16);
  Assert.AreEqual(Int64(16), BytesRead, 'Check number of bytes read');
  Assert.AreEqual(Copy(fB2Stream.Bytes, 0, 16), B16, 'Check content');
end;

procedure TTestStreamsWrapper.Read64_all_bytes_from_text_array_succeeds;
begin
  var TB: TBytes;
  SetLength(TB, fTextStream.Size);
  var BytesRead := fSWText.Read64(TB, 0, fTextStream.Size);
  Assert.AreEqual(fTextStream.Size, Int64(BytesRead), 'Check number of bytes read');
  Assert.AreEqual(GetStreamBytes(fTextStream), TB, 'Check content');
end;

procedure TTestStreamsWrapper.Read64_of_1_byte_from_empty_stream_fails;
begin
  var B: TBytes;
  SetLength(B, 1);
  var BytesRead := fSWEmpty.Read64(B, 0, 1);
  Assert.AreEqual(Int64(0), BytesRead, 'Check no bytes read');
end;

procedure TTestStreamsWrapper.Read64_of_5_bytes_from_B1_array_at_position_8_fails;
begin
  const Pos = Int64(8);
  const BytesToRead = Int32(5);
  fSWB1.Position := Pos;
  var Bytes: TBytes;
  SetLength(Bytes, BytesToRead);
  var BytesRead := fSWB1.Read64(Bytes, 0, BytesToRead);
  Assert.AreEqual(Int64(2), BytesRead, 'Check not enough bytes read');
  // B1 = [42,56,72,96,1,99,5,128,120,255];
  Assert.AreEqual(TBytes.Create(120,255), Copy(Bytes, 0, 2), 'Check bytes read');
end;

procedure TTestStreamsWrapper.Read_1st_16_bytes_from_B2_array_succeeds;
begin
  var B16: TBytes;
  SetLength(B16, 16);
  var BytesRead := fSWB2.Read(B16, 16);
  Assert.AreEqual(16, BytesRead, 'Check number of bytes read');
  Assert.AreEqual(Copy(fB2Stream.Bytes, 0, 16), B16, 'Check content');
end;

procedure TTestStreamsWrapper.Read_all_bytes_from_text_array_succeeds;
begin
  var TB: TBytes;
  SetLength(TB, fTextStream.Size);
  var BytesRead := fSWText.Read(TB[0], fTextStream.Size);
  Assert.AreEqual(fTextStream.Size, Int64(BytesRead), 'Check number of bytes read');
  Assert.AreEqual(GetStreamBytes(fTextStream), TB, 'Check content');
end;

procedure TTestStreamsWrapper.Read_of_1_byte_from_empty_stream_fails;
begin
  var B: Byte;
  var BytesRead := fSWEmpty.Read(B, 1);
  Assert.AreEqual(0, BytesRead, 'Check no bytes read');
end;

procedure TTestStreamsWrapper.Read_of_5_bytes_from_B1_array_at_position_8_fails;
begin
  const Pos = Int64(8);
  const BytesToRead = Int32(5);
  fSWB1.Position := Pos;
  var Bytes: TBytes;
  SetLength(Bytes, BytesToRead);
  var BytesRead := fSWB1.Read(Bytes[0], BytesToRead);
  Assert.AreEqual(2, BytesRead, 'Check not enough bytes read');
  // B1: TBytes = [42,56,72,96,1,99,5,128,120,255];
  Assert.AreEqual(TBytes.Create(120,255), Copy(Bytes, 0, 2), 'Check bytes read');
end;

procedure TTestStreamsWrapper.Seek_32_get_current_position_in_stream;
begin
  var Pos := fSWB1.Seek(Int32(0), soFromCurrent);
  Assert.AreEqual(Int64(Pos), fB1Stream.Position);
end;

procedure TTestStreamsWrapper.Seek_32_seek_from_current_position_in_stream;
begin
  const FromPos: Int32 = 5;
  const Offset: Int32 = -2;
  const ExpectedPos: Int32 = FromPos + Offset;
  fB1Stream.Position := FromPos;
  var Pos := fSWB1.Seek(Offset, soFromCurrent);
  Assert.AreEqual(ExpectedPos, Pos, 'Seek result');
  Assert.AreEqual(Int64(ExpectedPos), fB1Stream.Position, 'Underlying stream position');
end;

procedure TTestStreamsWrapper.Seek_32_seek_from_end_of_stream;
begin
  var B1Size := Int32(fSWB1.Size);
  const RequiredOffset: Int32 = -4;
  var ExpectedPos: Int32 := B1Size + RequiredOffset;
  var Pos := fSWB1.Seek(RequiredOffset, soFromEnd);
  Assert.AreEqual(ExpectedPos, Pos, 'Seek result');
  Assert.AreEqual(Int64(ExpectedPos), fB1Stream.Position, 'Underlying stream position');
end;

procedure TTestStreamsWrapper.Seek_32_seek_from_start_of_stream;
begin
  const RequiredOffset: Int32 = 4;
  const ExpectedPos: Int32 = 4;
  var Pos := fSWB1.Seek(RequiredOffset, soFromBeginning);
  Assert.AreEqual(ExpectedPos, Pos, 'Seek result');
  Assert.AreEqual(Int64(ExpectedPos), fB1Stream.Position, 'Underlying stream position');
end;

procedure TTestStreamsWrapper.Seek_32_seek_to_end_of_stream;
begin
  var Pos := fSWB1.Seek(Int32(0), soFromEnd);
  const ExpectedPos: Int32 = fSWB1.Size;
  Assert.AreEqual(ExpectedPos, Pos, 'Seek result');
  Assert.AreEqual(Int64(ExpectedPos), fB1Stream.Position, 'Underlying stream position');
end;

procedure TTestStreamsWrapper.Seek_32_seek_to_start_of_stream;
begin
  var Pos := fSWB1.Seek(Int32(0), soFromBeginning);
  const ExpectedPos: Int32 = 0;
  Assert.AreEqual(ExpectedPos, Pos, 'Seek result');
  Assert.AreEqual(Int64(ExpectedPos), fB1Stream.Position, 'Underlying stream position');
end;

procedure TTestStreamsWrapper.Seek_64_get_current_position_in_stream;
begin
  var Pos := fSWB1.Seek(Int64(0), TSeekOrigin.soCurrent);
  Assert.AreEqual(Pos, fB1Stream.Position);
end;

procedure TTestStreamsWrapper.Seek_64_seek_from_current_position_in_stream;
begin
  const FromPos: Int64 = 5;
  const Offset: Int64 = -2;
  const ExpectedPos: Int64 = FromPos + Offset;
  fB1Stream.Position := FromPos;
  var Pos := fSWB1.Seek(Offset, TSeekOrigin.soCurrent);
  Assert.AreEqual(ExpectedPos, Pos, 'Seek result');
  Assert.AreEqual(ExpectedPos, fB1Stream.Position, 'Underlying stream position');
end;

procedure TTestStreamsWrapper.Seek_64_seek_from_end_of_stream;
begin
  var B1Size: Int64 := fSWB1.Size;
  const RequiredOffset: Int64 = -4;
  var ExpectedPos: Int64 := B1Size + RequiredOffset;
  var Pos := fSWB1.Seek(RequiredOffset, TSeekOrigin.soEnd);
  Assert.AreEqual(ExpectedPos, Pos, 'Seek result');
  Assert.AreEqual(ExpectedPos, fB1Stream.Position, 'Underlying stream position');
end;

procedure TTestStreamsWrapper.Seek_64_seek_from_start_of_stream;
begin
  const RequiredOffset: Int64 = 4;
  const ExpectedPos: Int64 = 4;
  var Pos := fSWB1.Seek(RequiredOffset, TSeekOrigin.soBeginning);
  Assert.AreEqual(ExpectedPos, Pos, 'Seek result');
  Assert.AreEqual(ExpectedPos, fB1Stream.Position, 'Underlying stream position');
end;

procedure TTestStreamsWrapper.Seek_64_seek_to_end_of_stream;
begin
  var Pos := fSWB1.Seek(Int64(0), TSeekOrigin.soEnd);
  const ExpectedPos: Int64 = fSWB1.Size;
  Assert.AreEqual(ExpectedPos, Pos, 'Seek result');
  Assert.AreEqual(ExpectedPos, fB1Stream.Position, 'Underlying stream position');
end;

procedure TTestStreamsWrapper.Seek_64_seek_to_start_of_stream;
begin
  var Pos := fSWB1.Seek(Int64(0), soFromBeginning);
  const ExpectedPos: Int64 = 0;
  Assert.AreEqual(ExpectedPos, Pos, 'Seek result');
  Assert.AreEqual(ExpectedPos, fB1Stream.Position, 'Underlying stream position');
end;

procedure TTestStreamsWrapper.Setup;
begin
  fICStream := TInstanceCountedStream.Create([42,56]);
  fEmptyStream := TBytesStream.Create;
  fB1Stream := TBytesStream.Create(B1);
  SetLength(B2, 256);
  for var I := 0 to 255 do
    B2[I] := I;
  fB2Stream := TBytesStream.Create(B2);
  fTextStream := TBytesStream.Create(TEncoding.UTF8.GetBytes(T2));

  fSWEmpty := TStreamWrapper.Create(fEmptyStream);
  fSWB1 := TStreamWrapper.Create(fB1Stream, False);
  fSWB2 := TStreamWrapper.Create(fB2Stream, False);
  fSWText := TStreamWrapper.Create(fTextStream, False);
end;

procedure TTestStreamsWrapper.Size_prop_get_reflects_size_of_underlying_stream;
begin
  Assert.AreEqual(Int64(256), fSWB2.Size, 'Size of SWB2 is 256');
  fB2Stream.Size := 0;
  Assert.AreEqual(Int64(0), fSWB2.Size, 'Size of SWB2 is 0 after truncating underlying stream');
end;

procedure TTestStreamsWrapper.Size_prop_set_changes_size_of_underlying_stream;
begin
  fSWB1.Size := 0;
  Assert.AreEqual(Int64(0), fB1Stream.Size, 'Set SWB1 size to 0');
  fSWB2.Size := 300;
  Assert.AreEqual(Int64(300), fB2Stream.Size, 'Set SWB2 size to 300');
end;

procedure TTestStreamsWrapper.TearDown;
begin
  fSWText.Free;
  fSWB1.Free;
  fSWB2.Free;

  fTextStream.Free;
  fB2Stream.Free;
  fB1Stream.Free;
  fEmptyStream.Free;

  fICStream.Free;
end;

procedure TTestStreamsWrapper.Write64_append_2_bytes_to_empty_stream_succeeds;
begin
  var Bytes := TBytes.Create($FF, $FF);
  fSWEmpty.Write64(Bytes, 0, Length(Bytes));
  Assert.AreEqual(Int64(2), fSWEmpty.Size, 'Check size after write');
  Assert.AreEqual(Int64(2), fEmptyStream.Size, 'Check underlying stream size after write');
  Assert.AreEqual(Bytes, GetStreamBytes(fEmptyStream), 'Check bytes written');
end;

procedure TTestStreamsWrapper.Write64_append_4_bytes_to_end_of_B1_array_succeeds;
begin
  var B1Size := fSWB1.Size;
  var BytesToWrite := TBytes.Create($FF, $FE, $FD, $FC);
  fSWB1.Position := fSWB1.Size;
  var BytesWritten := fSWB1.Write64(BytesToWrite, 0, 4);
  Assert.AreEqual(Int64(4), BytesWritten, 'Check number of bytes written');
  Inc(B1Size, 4);
  Assert.AreEqual(Int64(B1Size), fSWB1.Size, 'Check size of stream');
  var ExpectedBytes := Concat(B1, BytesToWrite);
  Assert.AreEqual(ExpectedBytes, GetStreamBytes(fB1Stream), 'Check stream content');
end;

procedure TTestStreamsWrapper.Write64_overwrite_1_byte_at_position_4_in_B1_array_succeeds;
begin
  var B := TBytes.Create($ff);
  var B1Size := fSWB1.Size;
  fSWB1.Position := 4;
  var BytesWritten := fSWB1.Write64(B, 0, 1);
  var ExpectedBytes := Copy(B1);
  ExpectedBytes[4] := $ff;
  Assert.AreEqual(Int64(1), BytesWritten, 'Number of bytes written');
  Assert.AreEqual(B1Size, fSWB1.Size, 'Stream size unchanged');
  Assert.AreEqual(ExpectedBytes, GetStreamBytes(fB1Stream), 'Check content');
end;

procedure TTestStreamsWrapper.Write_append_2_bytes_to_empty_stream_succeeds;
begin
  var Bytes := TBytes.Create($FF, $FF);
  fSWEmpty.WriteData($FFFF);  // use method that calls .Write
  Assert.AreEqual(Int64(2), fSWEmpty.Size, 'Check size after write');
  Assert.AreEqual(Int64(2), fEmptyStream.Size, 'Check underlying stream size after write');
  Assert.AreEqual(Bytes, GetStreamBytes(fEmptyStream), 'Check bytes written');
end;

procedure TTestStreamsWrapper.Write_append_4_bytes_to_end_of_B1_array_succeeds;
begin
  var B1Size := fSWB1.Size;
  var BytesToWrite := TBytes.Create($FF, $FE, $FD, $FC);
  fSWB1.Position := fSWB1.Size;
  var BytesWritten := fSWB1.Write(BytesToWrite, 4);
  Assert.AreEqual(Int64(4), BytesWritten, 'Check number of bytes written');
  Inc(B1Size, 4);
  Assert.AreEqual(Int64(B1Size), fSWB1.Size, 'Check size of stream');
  var ExpectedBytes := Concat(B1, BytesToWrite);
  Assert.AreEqual(ExpectedBytes, GetStreamBytes(fB1Stream), 'Check stream content');
end;

procedure TTestStreamsWrapper.Write_overwrite_1_byte_at_position_4_in_B1_array_succeeds;
begin
  var B: UInt8 := $ff;
  var B1Size := fSWB1.Size;
  fSWB1.Position := 4;
  var BytesWritten := fSWB1.Write(B, 1);
  var ExpectedBytes := Copy(B1);
  ExpectedBytes[4] := $ff;
  Assert.AreEqual(Int64(1), BytesWritten, 'Number of bytes written');
  Assert.AreEqual(B1Size, fSWB1.Size, 'Stream size unchanged');
  Assert.AreEqual(ExpectedBytes, GetStreamBytes(fB1Stream), 'Check content');
end;

{ TInstanceCountedStream }

class constructor TInstanceCountedStream.Create;
begin
  fInstanceCount := 0;
end;

constructor TInstanceCountedStream.Create(const ABytes: TBytes);
begin
  inherited;
  Inc(fInstanceCount);
end;

destructor TInstanceCountedStream.Destroy;
begin
  Dec(fInstanceCount);
  inherited;
end;

initialization
  TDUnitX.RegisterTestFixture(TTestStreamsWrapper);

end.

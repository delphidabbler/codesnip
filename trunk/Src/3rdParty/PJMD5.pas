{
 * PJMD5.pas
 *
 * A Delphi Pascal implementation of the MD5 Message-Digest algorithm. See
 * RFC1321 (http://www.faqs.org/rfcs/rfc1321.html).
 *
 * This work is derived from the RSA Data Security, Inc. MD5 Message-Digest
 * Algorithm. The copyright notice is included in this project's documentation.
 * The notice must be included with any redistribution of this work or any
 * derived work.
 *
 * $Rev$
 * $Date$
 *
 * ***** BEGIN LICENSE BLOCK *****
 *
 * Version: MPL 1.1/GPL 2.0/LGPL 2.1
 *
 * The contents of this file are subject to the Mozilla Public License Version
 * 1.1 (the "License"); you may not use this file except in compliance with the
 * License. You may obtain a copy of the License at http://www.mozilla.org/MPL/
 *
 * Software distributed under the License is distributed on an "AS IS" basis,
 * WITHOUT WARRANTY OF ANY KIND, either express or implied. See the License for
 * the specific language governing rights and limitations under the License.
 *
 * Portions of the code are based on the reference implementation from the RFC,
 * translated into Pascal from the original C. The MD5 algorithm and the
 * reference implementation are Copyright (C) 1991-2, RSA Data Security, Inc.
 * Created 1991. All rights reserved.
 *
 * The Original Pascal Code is PJMD5.pas.
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
 * Alternatively, the contents of this file may be used under the terms of
 * either the GNU General Public License Version 2 or later (the "GPL"), or
 * the GNU Lesser General Public License Version 2.1 or later (the "LGPL"),
 * in which case the provisions of the GPL or the LGPL are applicable instead
 * of those above. If you wish to allow use of your version of this file only
 * under the terms of either the GPL or the LGPL, and not to allow others to
 * use your version of this file under the terms of the MPL, indicate your
 * decision by deleting the provisions above and replace them with the notice
 * and other provisions required by the GPL or the LGPL. If you do not delete
 * the provisions above, a recipient may use your version of this file under
 * the terms of any one of the MPL, the GPL or the LGPL.
 *
 * ***** END LICENSE BLOCK *****
}

unit PJMD5;

// Delphi 2009 or later is required to compile: requires Unicode support
{$UNDEF CANCOMPILE}
{$IFDEF CONDITIONALEXPRESSIONS}
  {$IF CompilerVersion >= 20.0}
    {$DEFINE CANCOMPILE}
  {$IFEND}
{$ENDIF}
{$IFNDEF CANCOMPILE}
  {$MESSAGE FATAL 'Delphi 2009 or later required'}
{$ENDIF}

{$WARN UNSAFE_CODE OFF}
{$WARN UNSAFE_TYPE OFF}
{$WARN UNSAFE_CAST OFF}

interface

uses
  SysUtils, Classes;

type

  ///  Class of exception raised by TPJMD5 and TPJMD5Digest.
  EPJMD5 = class(Exception);

  ///  <summary>
  ///  Record that encapsulates an MD5 digest.
  ///  </summary>
  ///  <remarks>
  ///  Provides alternative ways of accessing the digest data: as a byte array,
  ///  using the Bytes[] field, as a LongWord array, using the LongWords[] field
  ///  or as four named LongWord fields: A, B, C and D. Overloaded operators
  ///  permit equality testing with, and implicit casting to, strings and byte
  ///  arrays.
  ///  </remarks>
  TPJMD5Digest = record
  strict private
    ///  Read accessor for Parts[] property.
    function GetLongWord(Idx: Integer): LongWord;
  public
    // ** NOTE XML Doc can't see doc comments for these operator overloads
    // Tests all combinations of TPJMD5Digest, string and TBytes records for
    // equality.
    class operator Equal(const D1, D2: TPJMD5Digest): Boolean;
    class operator Equal(const D: TPJMD5Digest; const B: TBytes): Boolean;
    class operator Equal(const B: TBytes; const D: TPJMD5Digest): Boolean;
    class operator Equal(const D: TPJMD5Digest; const S: string): Boolean;
    class operator Equal(const S: string; const D: TPJMD5Digest): Boolean;
    // Tests all combinations of TPJMD5Digest, string and TBytes records for
    // inequality.
    class operator NotEqual(const D1, D2: TPJMD5Digest): Boolean;
    class operator NotEqual(const D: TPJMD5Digest; const B: TBytes): Boolean;
    class operator NotEqual(const B: TBytes; const D: TPJMD5Digest): Boolean;
    class operator NotEqual(const D: TPJMD5Digest; const S: string): Boolean;
    class operator NotEqual(const S: string; const D: TPJMD5Digest): Boolean;
    // Converts a TPJMD5Digest record to a (hexadecimal) string.
    class operator Implicit(const D: TPJMD5Digest): string;
    // Creates a TPJMD5Digest record by decoding a string, which must have 32
    // characters and contain only hex digits.
    class operator Implicit(const S: string): TPJMD5Digest;
    // Creates a byte array containing the digest data.
    class operator Implicit(const D: TPJMD5Digest): TBytes;
    // Creates a TPJMD5Digest record from Byte array B, which must have at
    // least 16 elements. If B has more than 16 elements, elements from B[16]
    // onwards are ignored.
    class operator Implicit(const B: TBytes): TPJMD5Digest;
    ///  <summary>
    ///  Default array property that permits access to the longwords of digest
    ///  by index.
    ///  </summary>
    ///  <remarks>
    ///  Provided mainly to permit the record to be directly indexed: D[Idx],
    ///  D.Parts[Idx] and D.LongWords[Idx] are all equivalent if D is a
    ///  TPJMD5Digest.
    ///  </remarks>
    property Parts[Idx: Integer]: LongWord read GetLongWord; default;
    ///  <summary>
    ///  Variant record that provides access to the digest as an array of bytes,
    ///  an array of LongWords or as LongWord fields named A, B, C and D.
    ///  </summary>
    case Integer of
      0: (Bytes: packed array[0..15] of Byte);
      1: (LongWords: packed array[0..3] of LongWord);
      2: (A, B, C, D: LongWord);
  end;

  ///  <summary>
  ///  Class that implements the MD5 message digest algorithm.
  ///  </summary>
  ///  <remarks>
  ///  Digests can be created with one call to one of the overloaded Calculate
  ///  class methods. Multiple data sources can be added to a digest using the
  ///  overloaded Process methods. Each method updates the current digest.
  ///  Finalize ends processing and creates the digest which is accessed via the
  ///  Digest property.
  ///  </remarks>
  TPJMD5 = class(TObject)
  strict private
    const
      ///  Size, in LongWords, of a data block (chunk).
      BlockSize = 16;
      ///  Size, in bytes, of a data chunk.
      ChunkSize = SizeOf(LongWord) * BlockSize;
    type
      ///  Data chunk. One chunk is processed at a time by Transform method.
      TMDChunk = array[0..Pred(ChunkSize)] of Byte;
      ///  Buffer storing bytes to be processed.
      TMDBuffer = record
        ///  Stores data being (or to be) processed
        Data: TMDChunk;
        ///  Current position in data storage
        Cursor: Byte;
        ///  Informs if Data buffer is empty
        function IsEmpty: Boolean;
        ///  Informs if Data buffer is full of data
        function IsFull: Boolean;
        ///  Returns amount of space remaining in Data buffer
        function SpaceRemaining: Byte;
        ///  Copies Count bytes starting at index StartIdx from Bytes array into
        ///  Data buffer.
        procedure Copy(const Bytes: array of Byte; const StartIdx: Cardinal;
          const Count: Cardinal);
        ///  Clears and zeroes the Data buffer
        procedure Clear;
      end;
    const
      ///  Default size of buffer for block reads from streams and files.
      DefReadBufferSize = 64 * 1024;  // 64Kb
    type
      ///  <summary>
      ///  Buffer used to read data from streams and files.
      ///  </summary>
      TMDReadBuffer = record
        ///  Pointer to buffer
        Buffer: Pointer;
        ///  Size of buffer
        Size: Cardinal;
        ///  Allocates a buffer of given size. Frees any existing buffer.
        procedure Alloc(const ASize: Cardinal);
        ///  Releases (de-allocates) the buffer.
        procedure Release;
      end;
    var
      ///  Current state of digest
      fState: TPJMD5Digest;
      ///  Value of Digest property
      fDigest: TPJMD5Digest;
      ///  Value of Finalized property
      fFinalized: Boolean;
      ///  Number of bytes processed
      fByteCount: UINT64;
      ///  Buffer that stores unprocessed data
      fBuffer: TMDBuffer;
      ///  Buffer used to read data from streams etc
      fReadBuffer: TMDReadBuffer;
      ///  Value of ReadBufferSize property
      fReadBufferSize: Cardinal;
    ///  <summary>
    ///  Read accessor for Digest property. Finalizes digest and returns it.
    ///  </summary>
    function GetDigest: TPJMD5Digest;
    ///  <summary>
    ///  Transforms byte array Bytes from starting position StartIdx, updating
    ///  digest.
    ///  </summary>
    procedure Transform(const Bytes: array of Byte; const StartIdx: Cardinal);
    ///  <summary>
    ///  Updates digest by processing Count bytes from byte array X, starting at
    ///  index StartIdx. Any unprocessed bytes are buffered.
    ///  </summary>
    procedure Update(const X: array of Byte; const StartIdx, Count: Cardinal);
    ///  <summary>
    ///  Helper method to perform calculations of digests for most Calculate
    ///  methods. Calls anonymous method DoProcess to perform the actual
    ///  processing.
    ///  </summary>
    class function DoCalculate(const DoProcess: TProc<TPJMD5>): TPJMD5Digest;
  public
    ///  <summary>
    ///  Object constructor. Sets up object and begins a new digest.
    ///  </summary>
    constructor Create;
    ///  <summary>
    ///  Object destructor. Tears down object
    ///  </summary>
    destructor Destroy; override;
    ///  <summary>
    ///  Calculates a digest of Count bytes read from byte array X, starting at
    ///  index StartIdx. Raises a EPJMD5 exception if there are less than Count
    ///  bytes from StartIdx to the end of the array.
    ///  </summary>
    ///  <remarks>
    ///  If StartIdx is beyond the end of the array then no data is processed.
    ///  </remarks>
    class function Calculate(const X: TBytes; const StartIdx, Count: Cardinal):
      TPJMD5Digest; overload;
    ///  <summary>
    ///  Calculates a digest of Count bytes read from byte array X. Raises an
    ///  EPJMD5 exception if Count is greater than the number of elements in X.
    ///  </summary>
    class function Calculate(const X: TBytes; const Count: Cardinal):
      TPJMD5Digest; overload;
    ///  <summary>
    ///  Calculates a digest of all the bytes of byte array X.
    ///  </summary>
    class function Calculate(const X: TBytes): TPJMD5Digest; overload;
    ///  <summary>
    ///  Calculates a digest of Count bytes read from untyped buffer Buf. Buf
    ///  must contain at least Count bytes (this is not checked).
    ///  </summary>
    class function Calculate(const Buf; const Count: Cardinal): TPJMD5Digest;
      overload;
    ///  <summary>
    ///  Calculates a digest of the characters of RawByteString S.
    ///  </summary>
    class function Calculate(const S: RawByteString): TPJMD5Digest; overload;
    ///  <summary>
    ///  Calculates a digest of the bytes of UnicodeString S according to the
    ///  given encoding.
    ///  </summary>
    class function Calculate(const S: UnicodeString;
      const Encoding: TEncoding): TPJMD5Digest; overload;
    ///  <summary>
    ///  Calculates a digest of the bytes of UnicodeString S according to the
    ///  default encoding.
    ///  </summary>
    class function Calculate(const S: UnicodeString): TPJMD5Digest; overload;
    ///  <summary>
    ///  Calculates a digest of the bytes from the current position to the end
    ///  of the given Stream.
    ///  </summary>
    ///  <remarks>
    ///  The stream is read in chunks of size DefReadBufferSize.
    ///  </remarks>
    class function Calculate(const Stream: TStream): TPJMD5Digest; overload;
    ///  <summary>
    ///  Calculates a digest of up to Count bytes from the current position in
    ///  the given stream. If Count is greater than available bytes in stream,
    ///  a EPJMD5 exception is raised.
    ///  </summary>
    ///  <remarks>
    ///  The stream is read in chunks of size DefReadBufferSize.
    ///  </remarks>
    class function Calculate(const Stream: TStream;
      const Count: Int64): TPJMD5Digest; overload;
    ///  <summary>
    ///  Calculates a digest of all the bytes of the named file.
    ///  </summary>
    ///  <remarks>
    ///  The file is read in chunks of size DefReadBufferSize.
    ///  </remarks>
    class function CalculateFile(const FileName: TFileName): TPJMD5Digest;
    ///  <summary>
    ///  Adds Count bytes from byte array X, starting at index StartIdx, to the
    ///  digest. If there are less than Count bytes from StartIdx to the end of
    ///  the array then an EPJMD5 exception is raised.
    ///  </summary>
    ///  <remarks>
    ///  If StartIdx is beyond the end of the array or if Count is zero then
    ///  there is nothing to do and the digest remains unchanged.
    ///  </remarks>
    procedure Process(const X: TBytes; const StartIdx, Count: Cardinal);
      overload;
    ///  <summary>
    ///  Adds Count bytes from a byte array X to the digest. If Count is greater
    ///  than the size of the array an EPJMD5 exception is raised.
    ///  </summary>
    procedure Process(const X: TBytes; const Count: Cardinal); overload;
    ///  <summary>
    ///  Adds all the bytes from a byte array X to the digest.
    ///  </summary>
    procedure Process(const X: TBytes); overload;
    ///  <summary>
    ///  Adds Count bytes from untyped buff Buf to the digest. Buf must contain
    ///  sufficient data (this is not checked).
    ///  </summary>
    procedure Process(const Buf; const Count: Cardinal); overload;
    ///  <summary>
    ///  Adds all the characters from RawByteString S as bytes to the digest.
    ///  </summary>
    procedure Process(const S: RawByteString); overload;
    ///  <summary>
    ///  Adds bytes from UnicodeString S according to the given Encoding to the
    ///  digest.
    ///  </summary>
    procedure Process(const S: UnicodeString; const Encoding: TEncoding);
      overload;
    ///  <summary>
    ///  Adds bytes from UnicodeString S according to the Default encoding to
    ///  the digest.
    ///  </summary>
    procedure Process(const S: UnicodeString); overload;
    ///  <summary>
    ///  Adds bytes to the digest from a stream starting from the current
    ///  position up to the end of the stream.
    ///  </summary>
    ///  <remarks>
    ///  The stream is read in chunks of size ReadBufferSize.
    ///  </remarks>
    procedure Process(const Stream: TStream); overload;
    ///  <summary>
    ///  Adds up to Count bytes from to the digest from a stream starting from
    ///  the current position. If Count is greater than available bytes in
    ///  stream an EPJMD5 exception is raised. If Count is -ve it is treated as
    ///  if it were 0.
    ///  </summary>
    ///  <remarks>
    ///  The stream is read in chunks of size ReadBufferSize.
    ///  </remarks>
    procedure Process(const Stream: TStream; const Count: Int64); overload;
    ///  <summary>
    ///  Adds all the bytes from the named file to the digest.
    ///  </summary>
    ///  <remarks>
    ///  The file is read in chunks of size ReadBufferSize.
    ///  </remarks>
    procedure ProcessFile(const FileName: TFileName);
    ///  <summary>
    ///  Discards any current digest, whether finalized or not.
    ///  </summary>
    ///  <remarks>
    ///  Sets the Finalized property to False.
    ///  </remarks>
    procedure Reset;
    ///  <summary>
    ///  Finalizes the current digest.
    ///  </summary>
    ///  <remarks>
    ///  Sets the Finalized property to True. An exception will be raised if
    ///  any Process method is called after finalization.
    ///  </remarks>
    procedure Finalize;
    ///  <summary>
    ///  Returns the MD5 digest.
    ///  </summary>
    ///  <remarks>
    ///  Referencing this property finalizes the digest by calling Finalize. The
    ///  Finalized property is set to True.
    ///  </remarks>
    property Digest: TPJMD5Digest read GetDigest;
    ///  <summary>
    ///  Gets and sets the size of buffer used when reading from stream or
    ///  files.
    ///  </summary>
    property ReadBufferSize: Cardinal
      read fReadBufferSize write fReadBufferSize default DefReadBufferSize;
    ///  <summary>
    ///  Informs if the digest has been finalized.
    ///  </summary>
    ///  <remarks>
    ///  It is an error to process any further data once the digest has been
    ///  finalized.
    ///  </remarks>
    property Finalized: Boolean read fFinalized;
  end;

implementation

uses
  Math;

// Copies the bytes of LongWord array LWords into an array of bytes Bytes. Low
// order bytes are copied first. The size of Bytes must be the same as the size
// of LWords in bytes.
procedure LongWordsToBytes(const LWords: array of LongWord;
  out Bytes: array of Byte);
var
  I, J: Cardinal;
begin
  Assert(Length(Bytes) = SizeOf(LongWord) * Length(LWords));
  J := 0;
  for I := 0 to Length(LWords) - 1 do
  begin
    Bytes[J] := LWords[I] and $FF;
    Bytes[J + 1] := (LWords[I] shr 8) and $FF;
    Bytes[J + 2] := (LWords[I] shr 16) and $FF;
    Bytes[J + 3] := (LWords[I] shr 24) and $FF;
    Inc(J, 4);
  end;
end;

// Copies an array of bytes Bytes, starting at index StartIdx, into an array
// LWords of LongWord. Assumes that the number of bytes after StartIndex in
// Bytes is >= the size of LWords. Bytes are copied from LongWord values low
// order first.
procedure BytesToLongWords(const Bytes: array of Byte; const StartIdx: Cardinal;
  out LWords: array of LongWord);
var
  I, J: Cardinal;
begin
  J := StartIdx;
  for I := 0 to Length(LWords) - 1 do
  begin
    LWords[I] := Bytes[J] or (Bytes[J + 1] shl 8)
      or (Bytes[J + 2] shl 16) or (Bytes[J + 3] shl 24);
    Inc(J, 4);
  end;
end;

// F, G, H and I are basic MD5 functions.

function F(const X, Y, Z: LongWord): LongWord; inline;
begin
  Result := (X and Y) or ((not X) and Z);
end;

function G(const X, Y, Z: LongWord): LongWord; inline;
begin
  Result := (X and Z) or (Y and not Z);
end;

function H(const X, Y, Z: LongWord): LongWord; inline;
begin
  Result := X xor Y xor Z;
end;

function I(const X, Y, Z: LongWord): LongWord; inline;
begin
  Result := Y xor (X or not Z);
end;

// Rotates LongWord X left by N bits
function RotateLeft(const X: LongWord; const N: Byte): LongWord; inline;
begin
  Result := (X shl N) or (X shr (32 - N));
end;

// FF, GG, HH, and II transformations for rounds 1, 2, 3, and 4.

procedure FF(var A: LongWord; const B, C, D, X: LongWord; const S: Byte;
  const AC: LongWord);
begin
  A := RotateLeft(A + F(B, C, D) + X + AC, S) + B;
end;

procedure GG(var A: LongWord; const B, C, D, X: LongWord; const S: Byte;
  const AC: LongWord);
begin
  A := RotateLeft(A + G(B, C, D) + X + AC, S) + B;
end;

procedure HH(var A: LongWord; const B, C, D, X: LongWord; const S: Byte;
  const AC: LongWord);
begin
  A := RotateLeft(A + H(B, C, D) + X + AC, S) + B;
end;

procedure II(var A: LongWord; const B, C, D, X: LongWord; const S: Byte;
  const AC: LongWord);
begin
  A := RotateLeft(A + I(B, C, D) + X + AC, S) + B;
end;

const
  // Padding applied to end of data stream is a subset of these bytes
  PADDING: array[0..63] of Byte = (
    $80, $00, $00, $00, $00, $00, $00, $00,
    $00, $00, $00, $00, $00, $00, $00, $00,
    $00, $00, $00, $00, $00, $00, $00, $00,
    $00, $00, $00, $00, $00, $00, $00, $00,
    $00, $00, $00, $00, $00, $00, $00, $00,
    $00, $00, $00, $00, $00, $00, $00, $00,
    $00, $00, $00, $00, $00, $00, $00, $00,
    $00, $00, $00, $00, $00, $00, $00, $00
  );

resourcestring
  // Error messages
  sAlreadyFinalized = 'Can''t update a finalised digest';
  sBadMD5StrLen = 'Can''t cast string of length %d to TPJMD5Digest';
  sBadMD5StrChars = 'Can''t cast string %s to TPJMD5Digest: invalid hex digits';
  sByteArrayTooShort = 'Can''t cast TBytes array of length %d to TPJMD5Digest: '
    + 'it is too short';
  sStreamTooShort = 'Can''t read %0:d bytes from stream. '
    + 'Only %1:d bytes remaining';
  sTBytesTooShort = 'Can''t read %0:d bytes from array of length %1:d';
  sTBytesIndexTooShort = 'Can''t read %0:d bytes from array of length %1:d '
    + 'starting at index %2:d';

{ TPJMD5 }

class function TPJMD5.Calculate(const Buf; const Count: Cardinal): TPJMD5Digest;
begin
  // Can't call DoCalculate with callback anonymous method because Buf is not
  // recognised by the compiler in the callback.
  with Create do
    try
      Process(Buf, Count);
      Result := Digest;
    finally
      Free;
    end;
end;

class function TPJMD5.Calculate(const S: RawByteString): TPJMD5Digest;
begin
  Result := DoCalculate(
    procedure(Instance: TPJMD5) begin Instance.Process(S); end
  );
end;

class function TPJMD5.Calculate(const X: TBytes): TPJMD5Digest;
begin
  Result := DoCalculate(
    procedure(Instance: TPJMD5) begin Instance.Process(X); end
  );
end;

class function TPJMD5.Calculate(const X: TBytes;
  const Count: Cardinal): TPJMD5Digest;
begin
  Result := DoCalculate(
    procedure(Instance: TPJMD5) begin Instance.Process(X, Count); end
  );
end;

class function TPJMD5.Calculate(const X: TBytes; const StartIdx,
  Count: Cardinal): TPJMD5Digest;
begin
  Result := DoCalculate(
    procedure(Instance: TPJMD5) begin Instance.Process(X, StartIdx, Count); end
  );
end;

class function TPJMD5.Calculate(const Stream: TStream): TPJMD5Digest;
begin
  Result := DoCalculate(
    procedure(Instance: TPJMD5) begin Instance.Process(Stream); end
  );
end;

class function TPJMD5.Calculate(const Stream: TStream;
  const Count: Int64): TPJMD5Digest;
begin
  Result := DoCalculate(
    procedure(Instance: TPJMD5) begin Instance.Process(Stream, Count); end
  );
end;

class function TPJMD5.Calculate(const S: UnicodeString): TPJMD5Digest;
begin
  Result := DoCalculate(
    procedure(Instance: TPJMD5) begin Instance.Process(S); end
  );
end;

class function TPJMD5.Calculate(const S: UnicodeString;
  const Encoding: TEncoding): TPJMD5Digest;
begin
  Result := DoCalculate(
    procedure(Instance: TPJMD5) begin Instance.Process(S, Encoding); end
  );
end;

class function TPJMD5.CalculateFile(const FileName: TFileName): TPJMD5Digest;
begin
  Result := DoCalculate(
    procedure(Instance: TPJMD5) begin Instance.ProcessFile(FileName); end
  );
end;

constructor TPJMD5.Create;
begin
  inherited Create;
  Reset;
  fReadBuffer.Release;
  fReadBufferSize := DefReadBufferSize;
end;

destructor TPJMD5.Destroy;
begin
  fBuffer.Clear;
  fReadBuffer.Release;
  inherited;
end;

class function TPJMD5.DoCalculate(const DoProcess: TProc<TPJMD5>): TPJMD5Digest;
var
  Instance: TPJMD5;
begin
  Instance := TPJMD5.Create;
  try
    DoProcess(Instance);
    Result := Instance.Digest;
  finally
    Instance.Free;
  end;
end;

procedure TPJMD5.Finalize;
var
  Index: Cardinal;
  PadLen: Cardinal;
  ByteCount: UINT64;
  BitCount: UINT64;
  EncodedBitCount: array[0..7] of Byte;
begin
  if fFinalized then
    Exit;

  // Store byte count
  ByteCount := fByteCount;

  // Pad the data and write padding to digest
  Index := fByteCount mod 64;
  if Index < (64 - 8) then
    // write bytes to take length to 56 mod 24
    PadLen := (64 - 8) - Index
  else
    // at or beyond 56th byte: need 128-8-cur_pos bytes
    PadLen := (128 - 8) - Index;
  Update(PADDING, 0, PadLen);

  // Write the bit count and let it wrap round
  {$IFOPT R+}
    {$DEFINE RANGECHECKS}
    {$R-}
  {$ELSE}
    {$UNDEF RANGECHECKS}
  {$ENDIF}
  BitCount := 8 * ByteCount;
  {$IFDEF RANGECHECKS}
    {$R+}
  {$ENDIF}
  LongWordsToBytes(Int64Rec(BitCount).Cardinals, EncodedBitCount);
  Update(EncodedBitCount, 0, SizeOf(EncodedBitCount));

  Assert(fBuffer.IsEmpty);

  fDigest := fState;
  fFinalized := True;
end;

function TPJMD5.GetDigest: TPJMD5Digest;
begin
  if not fFinalized then
    Finalize;
  Result := fDigest;
end;

procedure TPJMD5.Process(const S: UnicodeString; const Encoding: TEncoding);
begin
  Process(Encoding.GetBytes(S));
end;

procedure TPJMD5.Process(const S: UnicodeString);
begin
  Process(S, TEncoding.Default);
end;

procedure TPJMD5.Process(const Buf; const Count: Cardinal);
begin
  Update(TBytes(@Buf), 0, Count);
end;

procedure TPJMD5.Process(const S: RawByteString);
begin
  Process(Pointer(S)^, Length(S) * SizeOf(AnsiChar));
end;

procedure TPJMD5.Process(const Stream: TStream);
begin
  Process(Stream, Stream.Size - Stream.Position);
end;

procedure TPJMD5.Process(const Stream: TStream; const Count: Int64);
var
  BytesRead: Cardinal;
  BytesToRead: Int64;
begin
  if Count > Stream.Size - Stream.Position then
    raise EPJMD5.CreateFmt(
      sStreamTooShort, [Count, Stream.Size - Stream.Position]
    );
  BytesToRead := Max(Count, 0); // prevent Count < 0: use 0 in this case
  fReadBuffer.Alloc(fReadBufferSize);
  while BytesToRead > 0 do
  begin
    BytesRead := Stream.Read(
      fReadBuffer.Buffer^, Min(fReadBuffer.Size, BytesToRead)
    );
    Update(TBytes(fReadBuffer.Buffer), 0, BytesRead);
    Dec(BytesToRead, BytesRead);
  end;
end;

procedure TPJMD5.Process(const X: TBytes);
begin
  Update(X, 0, Length(X));
end;

procedure TPJMD5.Process(const X: TBytes; const Count: Cardinal);
begin
  if Count = 0 then
    Exit;
  if Count > Cardinal(Length(X)) then
    raise EPJMD5.CreateFmt(sTBytesTooShort, [Count, Length(X)]);
  Update(X, 0, Count);
end;

procedure TPJMD5.Process(const X: TBytes; const StartIdx, Count: Cardinal);
begin
  if (StartIdx >= Cardinal(Length(X))) or (Count = 0) then
    Exit;
  if StartIdx + Count > Cardinal(Length(X)) then
    raise EPJMD5.CreateFmt(sTBytesIndexTooShort, [Count, Length(X), StartIdx]);
  Update(X, StartIdx, Count);
end;

procedure TPJMD5.ProcessFile(const FileName: TFileName);
var
  Stm: TFileStream;
begin
  Stm := TFileStream.Create(FileName, fmOpenRead or fmShareDenyNone);
  try
    Process(Stm);
  finally
    Stm.Free;
  end;
end;

procedure TPJMD5.Reset;
begin
  fState.A := $67452301;
  fState.B := $efcdab89;
  fState.C := $98badcfe;
  fState.D := $10325476;
  FillChar(fDigest, SizeOf(fDigest), 0);
  fBuffer.Clear;  // flags buffer as empty
  fFinalized := False;
  fByteCount := 0;
end;

procedure TPJMD5.Transform(const Bytes: array of Byte;
  const StartIdx: Cardinal);
const
  // Constants for transformation routines FF, GG, HH and II.
  S11 =  7;   S12 = 12;   S13 = 17;   S14 = 22;
  S21 =  5;   S22 =  9;   S23 = 14;   S24 = 20;
  S31 =  4;   S32 = 11;   S33 = 16;   S34 = 23;
  S41 =  6;   S42 = 10;   S43 = 15;   S44 = 21;
var
  Block: array[0..Pred(BlockSize)] of LongWord;
  A, B, C, D: LongWord;
begin
  Assert(SizeOf(Block) = SizeOf(TMDChunk));
  Assert(UInt64(Length(Bytes)) - StartIdx >= SizeOf(TMDChunk));

  // Store states
  A := fState.A;
  B := fState.B;
  C := fState.C;
  D := fState.D;

  BytesToLongWords(Bytes, StartIdx, Block);

  // Round 1
  FF(A, B, C, D, Block[ 0], S11, $d76aa478); // 1
  FF(D, A, B, C, Block[ 1], S12, $e8c7b756); // 2
  FF(C, D, A, B, Block[ 2], S13, $242070db); // 3
  FF(B, C, D, A, Block[ 3], S14, $c1bdceee); // 4
  FF(A, B, C, D, Block[ 4], S11, $f57c0faf); // 5
  FF(D, A, B, C, Block[ 5], S12, $4787c62a); // 6
  FF(C, D, A, B, Block[ 6], S13, $a8304613); // 7
  FF(B, C, D, A, Block[ 7], S14, $fd469501); // 8
  FF(A, B, C, D, Block[ 8], S11, $698098d8); // 9
  FF(D, A, B, C, Block[ 9], S12, $8b44f7af); // 10
  FF(C, D, A, B, Block[10], S13, $ffff5bb1); // 11
  FF(B, C, D, A, Block[11], S14, $895cd7be); // 12
  FF(A, B, C, D, Block[12], S11, $6b901122); // 13
  FF(D, A, B, C, Block[13], S12, $fd987193); // 14
  FF(C, D, A, B, Block[14], S13, $a679438e); // 15
  FF(B, C, D, A, Block[15], S14, $49b40821); // 16

  // Round 2
  GG(A, B, C, D, Block[ 1], S21, $f61e2562); // 17
  GG(D, A, B, C, Block[ 6], S22, $c040b340); // 18
  GG(C, D, A, B, Block[11], S23, $265e5a51); // 19
  GG(B, C, D, A, Block[ 0], S24, $e9b6c7aa); // 20
  GG(A, B, C, D, Block[ 5], S21, $d62f105d); // 21
  GG(D, A, B, C, Block[10], S22,  $2441453); // 22
  GG(C, D, A, B, Block[15], S23, $d8a1e681); // 23
  GG(B, C, D, A, Block[ 4], S24, $e7d3fbc8); // 24
  GG(A, B, C, D, Block[ 9], S21, $21e1cde6); // 25
  GG(D, A, B, C, Block[14], S22, $c33707d6); // 26
  GG(C, D, A, B, Block[ 3], S23, $f4d50d87); // 27
  GG(B, C, D, A, Block[ 8], S24, $455a14ed); // 28
  GG(A, B, C, D, Block[13], S21, $a9e3e905); // 29
  GG(D, A, B, C, Block[ 2], S22, $fcefa3f8); // 30
  GG(C, D, A, B, Block[ 7], S23, $676f02d9); // 31
  GG(B, C, D, A, Block[12], S24, $8d2a4c8a); // 32

  // Round 3
  HH(A, B, C, D, Block[ 5], S31, $fffa3942); // 33
  HH(D, A, B, C, Block[ 8], S32, $8771f681); // 34
  HH(C, D, A, B, Block[11], S33, $6d9d6122); // 35
  HH(B, C, D, A, Block[14], S34, $fde5380c); // 36
  HH(A, B, C, D, Block[ 1], S31, $a4beea44); // 37
  HH(D, A, B, C, Block[ 4], S32, $4bdecfa9); // 38
  HH(C, D, A, B, Block[ 7], S33, $f6bb4b60); // 39
  HH(B, C, D, A, Block[10], S34, $bebfbc70); // 40
  HH(A, B, C, D, Block[13], S31, $289b7ec6); // 41
  HH(D, A, B, C, Block[ 0], S32, $eaa127fa); // 42
  HH(C, D, A, B, Block[ 3], S33, $d4ef3085); // 43
  HH(B, C, D, A, Block[ 6], S34,  $4881d05); // 44
  HH(A, B, C, D, Block[ 9], S31, $d9d4d039); // 45
  HH(D, A, B, C, Block[12], S32, $e6db99e5); // 46
  HH(C, D, A, B, Block[15], S33, $1fa27cf8); // 47
  HH(B, C, D, A, Block[ 2], S34, $c4ac5665); // 48

  // Round 4
  II(A, B, C, D, Block[ 0], S41, $f4292244); // 49
  II(D, A, B, C, Block[ 7], S42, $432aff97); // 50
  II(C, D, A, B, Block[14], S43, $ab9423a7); // 51
  II(B, C, D, A, Block[ 5], S44, $fc93a039); // 52
  II(A, B, C, D, Block[12], S41, $655b59c3); // 53
  II(D, A, B, C, Block[ 3], S42, $8f0ccc92); // 54
  II(C, D, A, B, Block[10], S43, $ffeff47d); // 55
  II(B, C, D, A, Block[ 1], S44, $85845dd1); // 56
  II(A, B, C, D, Block[ 8], S41, $6fa87e4f); // 57
  II(D, A, B, C, Block[15], S42, $fe2ce6e0); // 58
  II(C, D, A, B, Block[ 6], S43, $a3014314); // 59
  II(B, C, D, A, Block[13], S44, $4e0811a1); // 60
  II(A, B, C, D, Block[ 4], S41, $f7537e82); // 61
  II(D, A, B, C, Block[11], S42, $bd3af235); // 62
  II(C, D, A, B, Block[ 2], S43, $2ad7d2bb); // 63
  II(B, C, D, A, Block[ 9], S44, $eb86d391); // 64

  // Update state from results
  Inc(fState.A, A);
  Inc(fState.B, B);
  Inc(fState.C, C);
  Inc(fState.D, D);

  // For security
  FillChar(Block, SizeOf(Block), 0);
end;

procedure TPJMD5.Update(const X: array of Byte;
  const StartIdx, Count: Cardinal);
var
  BytesLeft: Cardinal;
  BytesToCopy: Cardinal;
begin
  if fFinalized then
    raise EPJMD5.Create(sAlreadyFinalized);
  BytesLeft := Count;
  if not fBuffer.IsEmpty then
  begin
    // buffer contains some data: we must add our new data here until buffer is
    // full or we have no more data
    // we add either sufficient data to fill buffer or all we've got if its less
    // than remaining space in buffer
    BytesToCopy := Min(fBuffer.SpaceRemaining, BytesLeft);
    fBuffer.Copy(X, StartIdx + Count - BytesLeft, BytesToCopy);
    Inc(fByteCount, BytesToCopy);
    Dec(BytesLeft, BytesToCopy);
    if fBuffer.IsFull then
    begin
      // if we filled buffer we Transform the digest from it and empty it
      Transform(fBuffer.Data, 0);
      fBuffer.Clear;
    end;
  end;
  // if we have more than a buffers worth of data we Transform the digest from
  // it until there's less than a buffer's worth left
  while BytesLeft >= ChunkSize do
  begin
    Transform(X, StartIdx + Count - BytesLeft);
    Inc(fByteCount, ChunkSize);
    Dec(BytesLeft, ChunkSize);
  end;
  // the following assertion must be true because:
  // case 1: buffer is empty and above loop only terminates when X has less
  //         than ChunkSize bytes remaining
  // case 2: X had less than enough bytes to fill buffer so they were all copied
  //         there => X has no bytes left
  Assert((BytesLeft = 0) or (fBuffer.IsEmpty and (BytesLeft < ChunkSize)));
  // above assertion implies following because:
  // case 1: if ByteLeft = 0 it must be <= space remaining in buffer
  // case 2: if ByteLeft <> 0 it is less than ChunkSize which is equal to
  //         buffer space remaining since buffer is empty
  Assert(BytesLeft <= fBuffer.SpaceRemaining);
  if BytesLeft > 0 then
  begin
    // we have bytes left over (which must be less than space remaining)
    // we copy all the remaining bytes to the buffer and, if it gets full we
    // Transform the digest from the buffer and empty it
    fBuffer.Copy(X, StartIdx + Count - BytesLeft, BytesLeft);
    Inc(fByteCount, BytesLeft);
    if fBuffer.IsFull then
    begin
      Transform(fBuffer.Data, 0);
      fBuffer.Clear;
    end;
  end;
end;

{ TPJMD5.TMDBuffer }

procedure TPJMD5.TMDBuffer.Clear;
var
  I: Integer;
begin
  for I := 0 to Length(Data) - 1 do
    Data[I] := 0;
  Cursor := 0;
end;

procedure TPJMD5.TMDBuffer.Copy(const Bytes: array of Byte;
  const StartIdx: Cardinal; const Count: Cardinal);
var
  Idx: Integer;
begin
  Assert(Count <= SpaceRemaining);
  for Idx := StartIdx to StartIdx + Count - 1 do
  begin
    Data[Cursor] := Bytes[Idx];
    Inc(Cursor);
  end;
end;

function TPJMD5.TMDBuffer.IsEmpty: Boolean;
begin
  Result := Cursor = 0;
end;

function TPJMD5.TMDBuffer.IsFull: Boolean;
begin
  Result := SpaceRemaining = 0;
end;

function TPJMD5.TMDBuffer.SpaceRemaining: Byte;
begin
  Result := SizeOf(Data) - Cursor;
end;

{ TPJMD5.TMDReadBuffer }

procedure TPJMD5.TMDReadBuffer.Alloc(const ASize: Cardinal);
begin
  if (ASize <> Size) then
  begin
    Release;
    if ASize > 0 then
    begin
      GetMem(Buffer, ASize);
      Size := ASize;
    end;
  end;
end;

procedure TPJMD5.TMDReadBuffer.Release;
begin
  if Assigned(Buffer) then
  begin
    FreeMem(Buffer);
    Size := 0;
  end;
end;

{ TPJMD5Digest }

class operator TPJMD5Digest.Equal(const D1, D2: TPJMD5Digest): Boolean;
var
  Idx: Integer;
begin
  Result := True;
  for Idx := Low(D1.LongWords) to High(D1.LongWords) do
    if D1.LongWords[Idx] <> D2.LongWords[Idx] then
      Exit(False);
end;

class operator TPJMD5Digest.Equal(const B: TBytes;
  const D: TPJMD5Digest): Boolean;
begin
  Result := D = B;
end;

class operator TPJMD5Digest.Equal(const D: TPJMD5Digest;
  const B: TBytes): Boolean;
var
  DB: TPJMD5Digest;
begin
  if Length(B) <> Length(D.Bytes) then
    Exit(False);
  DB := B;
  Result := DB = D;
end;

class operator TPJMD5Digest.Equal(const D: TPJMD5Digest;
  const S: string): Boolean;
var
  Idx: Integer;
  B: Integer;
  DS: TPJMD5Digest;
begin
  if Length(S) <> 2 * Length(D.Bytes) then
    Exit(False);
  Idx := 1;
  while Idx < Length(S) do
  begin
    if not TryStrToInt('$' + S[Idx] + S[Idx + 1], B) then
      Exit(False);
    Inc(Idx, 2);
  end;
  DS := S;
  Result := DS = D;
end;

class operator TPJMD5Digest.Equal(const S: string;
  const D: TPJMD5Digest): Boolean;
begin
  Result := D = S;
end;

function TPJMD5Digest.GetLongWord(Idx: Integer): LongWord;
begin
  Assert((Idx >= Low(LongWords)) and (Idx <= High(LongWords)));
  Result := Self.LongWords[Idx];
end;

class operator TPJMD5Digest.Implicit(const D: TPJMD5Digest): string;
var
  B: Byte;
const
  Digits: array[0..15] of Char = (
    '0', '1', '2', '3', '4', '5', '6', '7',
    '8', '9', 'a', 'b', 'c', 'd', 'e', 'f'
  );
begin
  Result := '';
  for B in D.Bytes do
    Result := Result + Digits[(B shr 4) and $0f] + Digits[B and $0f];
end;

class operator TPJMD5Digest.Implicit(const S: string): TPJMD5Digest;
var
  Idx: Integer;
  B: Integer;
  ByteStr: string;
begin
  FillChar(Result, SizeOf(Result), 0);
  if Length(S) <> 2 * Length(Result.Bytes) then
    raise EPJMD5.CreateFmt(sBadMD5StrLen, [Length(S)]);
  // we know string is correct length (32)
  for Idx := 0 to Pred(Length(Result.Bytes)) do
  begin
    ByteStr := S[2 * Idx + 1] + S[2 * Idx + 2];
    if not TryStrToInt('$' + ByteStr, B) then
      raise EPJMD5.CreateFmt(sBadMD5StrChars, [S]);
    Result.Bytes[Idx] := Byte(B);
  end;
end;

class operator TPJMD5Digest.Implicit(const B: TBytes): TPJMD5Digest;
var
  Idx: Integer;
begin
  Assert(Low(B) = Low(Result.Bytes));
  if Length(B) < Length(Result.Bytes) then
    raise EPJMD5.CreateFmt(sByteArrayTooShort, [Length(B)]);
  for Idx := Low(Result.Bytes) to High(Result.Bytes) do
    Result.Bytes[Idx] := B[Idx];
end;

class operator TPJMD5Digest.Implicit(const D: TPJMD5Digest): TBytes;
var
  Idx: Integer;
begin
  SetLength(Result, Length(D.Bytes));
  Assert(Low(D.Bytes) = Low(Result));
  for Idx := Low(D.Bytes) to High(D.Bytes) do
    Result[Idx] := D.Bytes[Idx];
end;

class operator TPJMD5Digest.NotEqual(const D: TPJMD5Digest;
  const B: TBytes): Boolean;
begin
  Result := not (D = B);
end;

class operator TPJMD5Digest.NotEqual(const B: TBytes;
  const D: TPJMD5Digest): Boolean;
begin
  Result := not (B = D);
end;

class operator TPJMD5Digest.NotEqual(const D1, D2: TPJMD5Digest): Boolean;
begin
  Result := not (D1 = D2);
end;

class operator TPJMD5Digest.NotEqual(const D: TPJMD5Digest;
  const S: string): Boolean;
begin
  Result := not (D = S);
end;

class operator TPJMD5Digest.NotEqual(const S: string;
  const D: TPJMD5Digest): Boolean;
begin
  Result := not (S = D);
end;

end.


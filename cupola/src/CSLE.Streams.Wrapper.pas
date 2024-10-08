{
  This Source Code Form is subject to the terms of the Mozilla Public License,
  v. 2.0. If a copy of the MPL was not distributed with this file, You can
  obtain one at http://mozilla.org/MPL/2.0/

  Copyright (C) 2000-2024, Peter Johnson (http://delphidabbler.com).

  Defines the TStreamWrapper class. This is a base class for descendant classes
  that "wrap" a TStream class to provide some form of filter or additional
  functionality. The wrapped TStream is used to do physical i/o. TStreamWrapper
  simply replicates the facilities in the wrapped stream - it is for descendant
  classes to add functionality.

  NOTE:
    This unit based on original code from https://github.com/ddablib/streams
}

unit CSLE.Streams.Wrapper;

interface

uses
  // Delphi
  System.SysUtils,
  System.Classes;

type
  ///  <summary>Base class for descendant classes that "wrap" a TStream class to
  ///  provide some form of filter or additional functionality. The wrapped
  ///  TStream is used to do physical i/o. This base class simply replicates the
  ///  facilities in the wrapped stream - it is for descendant classes to add
  ///  functionality.</summary>
  ///  <remarks>Wrapping a TStream rather than adding functionality by extending
  ///  the class means that the functionality provided by the wrapper class can
  ///  be applied to any TStream descendant.</remarks>
  TStreamWrapper = class(TStream)
  strict private
    var
      ///  <summary>Reference to wrapped stream.</summary>
      fBaseStream: TStream;
      ///  <summary>Records whether wrapped stream is to be freed when this
      ///  object is destroyed.</summary>
      fCloseStream: Boolean;
  strict protected

    ///  <summary>Sets the size of the wrapped stream.</summary>
    ///  <param name="NewSize">[in] New size of stream.</param>
    ///  <remarks>
    ///  <para>If the wrapped stream does not support the <c>SetSize</c>
    ///  operation then the stream's size is not changed.</para>
    ///  <para>See also the overloaded version that takes a 64 bit size.</para>
    ///  </remarks>
    procedure SetSize(NewSize: Int32); override; deprecated;

    ///  <summary>Sets the size of the wrapped stream.</summary>
    ///  <param name="NewSize">[in] New size of stream.</param>
    ///  <remarks>
    ///  <para>If the wrapped stream does not support the <c>SetSize</c>
    ///  operation then the stream's size is not changed.</para>
    ///  <para>If the wrapped stream does not support 64 bit <c>SetSize</c> then
    ///  <c>NewSize</c> is truncated to 32 bits.</para>
    ///  <para>See also the overloaded version that takes a 32 bit size.</para>
    ///  </remarks>
    procedure SetSize(const NewSize: Int64); override;

  public

    ///  <summary>Object constructor. Creates a <c>TStream</c> descendant object
    ///  that wraps another stream and optionally takes ownership of it.
    ///  </summary>
    ///  <param name="Stream">[in] Stream to be wrapped.</param>
    ///  <param name="CloseStream">[in] Flag that indicates whether
    ///  <c>Stream</c> is to be freed when this object is be destroyed
    ///  (<c>True</c>) or whether caller retains responsibility for freeing
    ///  <c>Stream</c> (<c>False</c>).</param>
    constructor Create(const Stream: TStream;
      const CloseStream: Boolean = False); virtual;

    ///  <summary>Tears down object. Frees wrapped stream iff <c>CloseStream</c>
    ///  parameter of constructor was <c>True</c></summary>
    destructor Destroy; override;

    ///  <summary>Reads data from wrapped stream into a buffer.</summary>
    ///  <param name="Buffer">[in/out] Buffer that receives data read from
    ///  stream. Must have size of at least <c>Count</c> bytes.</param>
    ///  <param name="Count">[in] Number of bytes to be read.</param>
    ///  <returns>Number of bytes actually read.</returns>
    ///  <remarks>If return value is less than <c>Count</c> then end of stream
    ///  has been reached.</remarks>
    function Read(var Buffer; Count: Int32): Int32; override;

    ///  <summary>Reads data from wrapped stream into a byte array.</summary>
    ///  <param name="Buffer">[in] Array of bytes that receives data read from
    ///  stream. Must have size of at least <c>Count</c> elements.</param>
    ///  <param name="Count">[in] Number of bytes to be read.</param>
    ///  <returns>Number of bytes actually read.</returns>
    ///  <remarks>If return value is less than <c>Count</c> then end of stream
    ///  has been reached.</remarks>
    function Read64(Buffer: TBytes; Offset, Count: Int64): Int64; override;

    ///  <summary>Writes data from a buffer to wrapped stream.</summary>
    ///  <param name="Buffer">[in] Buffer containg date to be written. Must
    ///  contain at least <c>Count</c> bytes of data.</param>
    ///  <param name="Count">[in] Number of bytes of data to be written.</param>
    ///  <returns>Number of bytes actually written.</returns>
    ///  <remarks>If the return value is less than <c>Count</c> then the stream
    ///  is full and not all the data could be written.</remarks>
    function Write(const Buffer; Count: Int32): Int32; override;

    ///  <summary>Writes data from a byte array to wrapped stream.</summary>
    ///  <param name="Buffer">[in] Array of bytes containing data to be written.
    ///  Must have at least <c>Count</c> bytes elements.</param>
    ///  <param name="Count">[in] Number of bytes of data to be written.</param>
    ///  <returns>Number of bytes actually written.</returns>
    ///  <remarks>If the return value is less than <c>Count</c> then the stream
    ///  is full and not all the data could be written.</remarks>
    function Write64(const Buffer: TBytes; Offset, Count: Int64): Int64;
      override;

    ///  <summary>Sets the underlying stream's position.</summary>
    ///  <param name="Offset">[in] New stream position relative to position
    ///  defined by <c>Offset</c>.</param>
    ///  <param name="Origin">[in] Specifies origin that <c>Offset</c> relates
    ///  to. For details of values see documentation of <c>TStream.Seek</c>.
    ///  </param>
    ///  <returns>New stream position (value of <c>Position</c> property).
    ///  </returns>
    ///  <remarks>
    ///  <para>If the wrapped stream does not support changing the stream
    ///  position an exception will be raised.</para>
    ///  <para>See also the overloaded version that takes a 64 bit size.</para>
    ///  </remarks>
    function Seek(Offset: Int32; Origin: UInt16): Int32; override;

    ///  <summary>Sets the underlying stream's position.</summary>
    ///  <param name="Offset">[in] New stream position relative to position
    ///  defined by <c>Offset</c>.</param>
    ///  <param name="Origin">[in] Specifies origin that <c>Offset</c> relates
    ///  to. For details of values see documentation of <c>TSeekOrigin</c>.
    ///  </param>
    ///  <returns>New stream position (value of <c>Position</c> property).
    ///  </returns>
    ///  <remarks>
    ///  <para>If the wrapped stream does not support changing the stream
    ///  position an exception will be raised.</para>
    ///  <para>If the wrapped stream does not support 64 bit <c>Seek</c> then
    ///  the 32 bit version will be called instead and <c>Offset</c> may be
    ///  truncated.</para>
    ///  <para>See also the overloaded version that takes a 32 bit size.</para>
    ///  </remarks>
    function Seek(const Offset: Int64; Origin: TSeekOrigin): Int64; override;

    ///  <summary>Reference to the wrapped stream object.</summary>
    ///  <remarks>Enables caller and sub-classes to access the wrapped stream.
    ///  </remarks>
    property BaseStream: TStream read fBaseStream;
  end;

implementation

{ TStreamWrapper }

constructor TStreamWrapper.Create(const Stream: TStream;
  const CloseStream: Boolean);
begin
  inherited Create;
  fBaseStream := Stream;
  fCloseStream := CloseStream;
end;

destructor TStreamWrapper.Destroy;
begin
  if fCloseStream then
    fBaseStream.Free;
  inherited Destroy;
end;

function TStreamWrapper.Read(var Buffer; Count: Int32): Int32;
begin
  Result := fBaseStream.Read(Buffer, Count);
end;

function TStreamWrapper.Read64(Buffer: TBytes; Offset, Count: Int64): Int64;
begin
  Result := fBaseStream.Read64(Buffer, Offset, Count);
end;

function TStreamWrapper.Seek(const Offset: Int64; Origin: TSeekOrigin): Int64;
begin
  Result := fBaseStream.Seek(Offset, Origin);
end;

function TStreamWrapper.Seek(Offset: Int32; Origin: UInt16): Int32;
begin
  Result := fBaseStream.Seek(Offset, Origin);
end;

procedure TStreamWrapper.SetSize(const NewSize: Int64);
begin
  fBaseStream.Size := NewSize;
end;

procedure TStreamWrapper.SetSize(NewSize: Int32);
begin
  // according to comments in TStream.SetSize if we implement 64 bit version of
  // SetSize, our 32 bit implementation must call it
  SetSize(Int64(NewSize));
end;

function TStreamWrapper.Write(const Buffer; Count: Int32): Int32;
begin
  Result := fBaseStream.Write(Buffer, Count);
end;

function TStreamWrapper.Write64(const Buffer: TBytes; Offset,
  Count: Int64): Int64;
begin
  Result := fBaseStream.Write64(Buffer, Offset, Count);
end;

end.


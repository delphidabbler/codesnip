{ ##
  @FILE                     UPipe.pas
  @COMMENTS                 Class that encapsulates an unamed pipe and can read
                            and write the pipe.
  @PROJECT_NAME             CodeSnip
  @PROJECT_DESC             Offline viewer for routines from the online
                            DelphiDabbler CodeSnip database.
  @DEPENDENCIES             None.
  @HISTORY(
    @REVISION(
      @VERSION              0.1
      @DATE                 11/01/2006
      @COMMENTS             Original version.
    )
    @REVISION(
      @VERSION              1.0
      @DATE                 25/05/2006
      @COMMENTS             + Improved and corrected comments.
                            + Relocated and rationalised $WARN directives.
                            + Localised error messages.
    )
  )
}


{
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
 * The Original Code is UPipe.pas
 *
 * The Initial Developer of the Original Code is Peter Johnson
 * (http://www.delphidabbler.com/).
 *
 * Portions created by the Initial Developer are Copyright (C) 2006 Peter
 * Johnson. All Rights Reserved.
 *
 * Contributor(s): None
 *
 * ***** END LICENSE BLOCK *****
}


unit UPipe;

{$WARN UNSAFE_TYPE OFF}
{$WARN UNSAFE_CODE OFF}

interface


uses
  // Delphi
  Classes, Windows;

type

  {
  TPipe:
    Class that encapsulates an unamed pipe and can read and write the pipe.
  }
  TPipe = class(TObject)
  private
    fReadHandle: THandle;
      {Handle used to read the pipe}
    fWriteHandle: THandle;
      {Handle used to write the pipe}
  public
    constructor Create(const Size: LongWord = 0);
      {Class constructor. Creates a new pipe with inheritable handles.
        @param Size Required size of pipe. If Size is 0 default pipe size used.
      }
    destructor Destroy; override;
      {Class destructor. Tears down object.
      }
    function AvailableDataSize: LongWord;
      {Gets size of data available for reading from pipe.
        @return Number of bytes of available data.
        @exception EInOutError raised if pipe's read handle is closed or there
          is an error testing the pipe.
      }
    function ReadData(var Buf; const BufSize: LongWord;
      var BytesRead: LongWord): Boolean;
      {Reads data from pipe into a buffer.
        @param Buf Buffer receiving data. Must have capacity of at least BufSize
          bytes.
        @param BufSize Size of buffer or number of bytes requested.
        @param BytesRead Set to number of bytes actually read.
        @return True if some data was read, false if not.
        @exception EInOutError raised if pipe's read handle is closed or there
          is an error checking the pipe.
      }
    procedure CopyToStream(const Stm: TStream; Count: LongWord = 0);
      {Copies data from pipe to a stream.
        @param Stm Stream that receives data.
        @param Count Number of bytes to copy. If 0 then all remaining data in
          pipe is copied to stream.
        @exception EInOutError raised if there is an error reading the pipe.
      }
    procedure CopyFromStream(const Stm: TStream; Count: LongWord = 0);
      {Copies data from a stream into the pipe.
        @param Stm Stream from which to copy data.
        @param Count Number of bytes to copy. If 0 then all remaining data in
          stream is copied.
        @exception EInOutError raised if pipe's write handle is closed.
      }
    function WriteData(const Buf; const BufSize: LongWord): LongWord;
      {Writes data from buffer to pipe.
        @param Buf Buffer containing data to be written.
        @param BufSize Number of bytes to write from buffer. Buf must have
          capacity of at least BufSize bytes.
        @return Number of bytes actually written.
        @exception EInOutError raised if pipe's write handle has been closed.
      }
    procedure CloseWriteHandle;
      {Closes the pipe's write handle if it is open. This effectively signals
      EOF to any reader of the pipe. After calling this method no further data
      may be written to the pipe.
      }
    property ReadHandle: THandle read fReadHandle;
      {Handle used to read data from the pipe. Should be non-zero}
    property WriteHandle: THandle read fWriteHandle;
      {Handle used to write data to the pipe. Should not be used when 0.
      CloseWriteHandle closes and zeros this handle}
  end;


implementation


uses
  // Delphi
  SysUtils;


resourcestring
  // Error messages
  sBadReadHandle  = 'Can''t read pipe: handle closed';
  sCantPeekPipe   = 'Can''t read pipe: peek attempt failed';
  sPipeReadError  = 'Error reading pipe';
  sBadWriteHandle = 'Can''t write to stream: handle closed';


{ TPipe }

function TPipe.AvailableDataSize: LongWord;
  {Gets size of data available for reading from pipe.
    @return Number of bytes of available data.
    @exception EInOutError raised if pipe's read handle is closed or there is an
      error testing the pipe.
  }
begin
  if fReadHandle = 0 then
    raise EInOutError.Create(sBadReadHandle);
  if not PeekNamedPipe(fReadHandle, nil, 0, nil, @Result, nil) then
    raise EInOutError.Create(sCantPeekPipe);
end;

procedure TPipe.CloseWriteHandle;
  {Closes the pipe's write handle if it is open. This effectively signals EOF to
  any reader of the pipe. After calling this method no further data may be
  written to the pipe.
  }
begin
  if fWriteHandle <> 0 then
  begin
    CloseHandle(fWriteHandle);
    fWriteHandle := 0;
  end;
end;

procedure TPipe.CopyFromStream(const Stm: TStream; Count: LongWord);
  {Copies data from a stream into the pipe.
    @param Stm Stream from which to copy data.
    @param Count Number of bytes to copy. If 0 then all remaining data in stream
      is copied.
    @exception EInOutError raised if pipe's write handle is closed.
  }
var
  BytesToWrite: LongWord;       // adjusted number of bytes to write
  Buf: array[0..4095] of Byte;  // buffer used in copying from pipe to stream
begin
  // Determine how much to copy
  if Count = 0 then
    Count := Stm.Size - Stm.Position;
  // Copy data one bufferful at a time
  while Count > 0 do
  begin
    if Count > SizeOf(Buf) then
      BytesToWrite := SizeOf(Buf)
    else
      BytesToWrite := Count;
    Stm.ReadBuffer(Buf, BytesToWrite);
    WriteData(Buf, BytesToWrite);
    Dec(Count, BytesToWrite);
  end;
end;

procedure TPipe.CopyToStream(const Stm: TStream; Count: LongWord);
  {Copies data from pipe to a stream.
    @param Stm Stream that receives data.
    @param Count Number of bytes to copy. If 0 then all remaining data in pipe
      is copied to stream.
    @exception EInOutError raised if there is an error reading the pipe.
  }
var
  AvailBytes: LongWord;           // number of bytes in pipe
  BytesToRead: LongWord;          // decreasing count of remaining bytes
  BytesRead: LongWord;            // bytes read in each loop
  Buf: array[0..4095] of Byte;    // buffer used to read from stream
begin
  // Determine how much should be read
  AvailBytes := AvailableDataSize;
  if (Count = 0) or (Count > AvailBytes) then
    Count := AvailBytes;
  // Copy data one bufferful at a time
  while Count > 0 do
  begin
    if Count > SizeOf(Buf) then
      BytesToRead := SizeOf(Buf)
    else
      BytesToRead := Count;
    ReadData(Buf, BytesToRead, BytesRead);
    if BytesRead <> BytesToRead then
      raise EInOutError.Create(sPipeReadError);
    Stm.WriteBuffer(Buf, BytesRead);
    Dec(Count, BytesRead);
  end;
end;

constructor TPipe.Create(const Size: LongWord = 0);
  {Class constructor. Creates a new pipe with inheritable handles.
    @param Size Required size of pipe. If Size is 0 default pipe size used.
  }
var
  Security: TSecurityAttributes;  // file's security attributes
begin
  inherited Create;
  // Set up security structure so file handle is inheritable (for Windows NT)
  Security.nLength := SizeOf(Security);
  Security.lpSecurityDescriptor := nil;
  Security.bInheritHandle := True;
  // Create the pipe
  CreatePipe(fReadHandle, fWriteHandle, @Security, Size);
end;

destructor TPipe.Destroy;
  {Class destructor. Tears down object.
  }
begin
  CloseHandle(fReadHandle);
  CloseWriteHandle;
  inherited;
end;

function TPipe.ReadData(var Buf; const BufSize: LongWord;
  var BytesRead: LongWord): Boolean;
  {Reads data from pipe into a buffer.
    @param Buf Buffer receiving data. Must have capacity of at least BufSize
      bytes.
    @param BufSize Size of buffer or number of bytes requested.
    @param BytesRead Set to number of bytes actually read.
    @return True if some data was read, false if not.
    @exception EInOutError raised if pipe's read handle is closed or there is an
      error checking the pipe.
  }
var
  BytesToRead: DWORD;   // number of bytes to actually read
begin
  BytesToRead := AvailableDataSize;
  if BytesToRead > 0 then
  begin
    if BytesToRead > BufSize then
      BytesToRead := BufSize;
    ReadFile(fReadHandle, Buf, BytesToRead, BytesRead, nil);
    Result := BytesRead > 0;
  end
  else
    Result := False;
end;

function TPipe.WriteData(const Buf; const BufSize: LongWord): LongWord;
  {Writes data from buffer to pipe.
    @param Buf Buffer containing data to be written.
    @param BufSize Number of bytes to write from buffer. Buf must have capacity
      of at least BufSize bytes.
    @return Number of bytes actually written.
    @exception EInOutError raised if pipe's write handle has been closed.
  }
begin
  if fWriteHandle = 0 then
    raise EInOutError.Create(sBadWriteHandle);
  WriteFile(fWriteHandle, Buf, BufSize, Result, nil);
end;

end.


{
 * UTextStreamReader.pas
 *
 * Defines class that performs character based access to a stream.
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
 * The Original Code is UTextStreamReader.pas
 *
 * The Initial Developer of the Original Code is Peter Johnson
 * (http://www.delphidabbler.com/).
 *
 * Portions created by the Initial Developer are Copyright (C) 2005-2010 Peter
 * Johnson. All Rights Reserved.
 *
 * Contributor(s)
 *   NONE
 *
 * ***** END LICENSE BLOCK *****
}


unit UTextStreamReader;


interface


uses
  // Delphi
  Classes,
  // Project
  UConsts;


type

  {
  TTextStreamReader:
    Class that performs character based access to a stream. Copies entire stream
    into an internal buffer and then reads data from that buffer one character
    at a time. Characters can be put back on the stream. CRLF pairs are
    converted on the fly to single EOL character.
  }
  TTextStreamReader = class(TObject)
  strict private
    var
      fBuffer: string;
        {Stores all data read from stream}
      fIdx: Integer;
        {Cursor into buffer that indexes next character to be read}
    function GetCh: Char;
      {Read accessor for Ch property.
        @return Last character read, or EOF at end of file.
      }
  public
    const
      EOF = #0;   // character indicating end of file
      EOL = LF;   // character indicating end of line
    constructor Create(const Stm: TStream);
      {Class constructor. Creates reader for a stream and reads first character.
        @param Stm [in] Stream to be read.
      }
    function NextChar: Char;
      {Fetches next character from buffer.
        @return Character read (EOF at end of buffer and LF at end of line).
      }
    procedure PutBackChar;
      {Puts last read character back on the stream.
      }
    property Ch: Char read GetCh;
      {Last character read from stream (EOF if at end of stream and LF at end
      of line}
  end;


implementation


uses
  // Delphi
  SysUtils;


{ TTextStreamReader }

constructor TTextStreamReader.Create(const Stm: TStream);
  {Class constructor. Creates reader for a stream and reads first character.
    @param Stm [in] Stream to be read.
  }
begin
  inherited Create;
  // Read stream into buffer
  SetLength(fBuffer, Stm.Size div SizeOf(Char));
  Stm.ReadBuffer(Pointer(fBuffer)^, Stm.Size);
  // Set cursor to just before start of buffer then read first char
  fIdx := 0;
  NextChar;
end;

function TTextStreamReader.GetCh: Char;
  {Read accessor for Ch property.
    @return Last character read, or EOF at end of file.
  }
begin
  if fIdx <= Length(fBuffer) then
  begin
    // We are within buffer: get char at current position
    Result := fBuffer[fIdx];
    if CharInSet(Result, [CR, LF]) then
      // Char is one of EOL chars => return EOL
      Result := EOL;
  end
  else
    // Beyond end of buffer => say so
    Result := EOF;
end;

function TTextStreamReader.NextChar: Char;
  {Fetches next character from buffer.
    @return Character read (EOF at end of buffer and EOL at end of line).
  }
begin
  if fIdx < Length(fBuffer) then
  begin
    // We have remaining chars: increment cursor to next char position in buffer
    Inc(fIdx);
    if fBuffer[fIdx] = CR then
    begin
      // New char is CR: check to see if followed by LF and skip over CR if so
      if (fIdx < Length(fBuffer)) and (fBuffer[fIdx + 1] = LF) then
        Inc(fIdx);
    end;
  end
  else
    // No remaining chars: place cursor beyond end of buffer => EOF
    fIdx := Length(fBuffer) + 1;
  // Return chracter just read or EOF
  Result := GetCh;
end;

procedure TTextStreamReader.PutBackChar;
  {Puts last read character back on the stream.
  }
begin
  if fIdx > 1 then
  begin
    // fIdx in [2..Length(fBuffer) + 1] => we can move back in buffer
    // this means that putting back char at EOF points to last character
    Dec(fIdx);
    if fBuffer[fIdx] = LF then
    begin
      // New current char is LF. If preceeded by CR we will have skipped over CR
      // when read, so go back two chars in buffer if so
      if (fIdx > 1) and (fBuffer[fIdx - 1] = CR) then
        Dec(fIdx);
    end;
  end;
end;

end.


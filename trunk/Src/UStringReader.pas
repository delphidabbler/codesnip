{
 * UStringReader.pas
 *
 * Defines class that performs character based access to a stream.
 *
 * $Rev$
 * $Date$
 *
 * Originally named UTextStreamReader.pas.
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
 * The Original Code is UStringReader.pas
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


unit UStringReader;


interface


uses
  // Project
  UConsts;


type
  ///  <summary>
  ///  Class that performs character based access to a string. Reads data from
  ///  the string one character at a time, treating CRLF pairs as a single EOL
  ///  character. Characters can be put back onto the string.
  ///  </summary>
  TStringReader = class(TObject)
  strict private
    ///  String being read.
    var fBuffer: string;
    ///  Cursor that indexes next character to be read.
    var fIdx: Integer;
    /// Read accessor for Ch property. Returns last character read or EOF.
    function GetCh: Char;
  public
    ///  Character indicating end of file.
    const EOF = #0;
    ///  Character indicating end of line.
    const EOL = LF;
    ///  Object constructor. Records string to be read. Initialises Ch property
    ///  by reading first character.
    constructor Create(const Str: string);
    ///  Fetches next character from string and returns it. Returns EOL at end
    ///  of line and EOF at end of file.
    function NextChar: Char;
    ///  Puts last character read back on end of string.
    procedure PutBackChar;
    ///  Last character read from stream (EOL at end of line and EOF at end of
    ///  file).
    property Ch: Char read GetCh;
  end;


implementation


uses
  // Delphi
  SysUtils;


{ TStringReader }

constructor TStringReader.Create(const Str: string);
begin
  inherited Create;
  fBuffer := Str;
  // set cursor to just before start of buffer then read first char
  fIdx := 0;
  NextChar;
end;

function TStringReader.GetCh: Char;
begin
  if fIdx <= Length(fBuffer) then
  begin
    // within buffer: get char at current position
    Result := fBuffer[fIdx];
    if CharInSet(Result, [CR, LF]) then
      // char is one of EOL chars => return EOL
      Result := EOL;
  end
  else
    Result := EOF;
end;

function TStringReader.NextChar: Char;
begin
  if fIdx < Length(fBuffer) then
  begin
    // there are more chars: increment cursor to next char position
    Inc(fIdx);
    if fBuffer[fIdx] = CR then
    begin
      // new char is CR: skip it if followed by LF
      if (fIdx < Length(fBuffer)) and (fBuffer[fIdx + 1] = LF) then
        Inc(fIdx);
    end;
  end
  else
    // no remaining chars: place cursor beyond end of buffer => EOF
    fIdx := Length(fBuffer) + 1;
  // return character just read or EOF
  Result := GetCh;
end;

procedure TStringReader.PutBackChar;
begin
  if fIdx > 1 then
  begin
    // fIdx in [2..Length(fBuffer) + 1] => we can move back in buffer
    // this permits putting back char at EOF
    Dec(fIdx);
    if fBuffer[fIdx] = LF then
    begin
      // new current char is LF: if preceeded by CR we will have skipped over CR
      // when read so we go back two chars in buffer
      if (fIdx > 1) and (fBuffer[fIdx - 1] = CR) then
        Dec(fIdx);
    end;
  end;
end;

end.


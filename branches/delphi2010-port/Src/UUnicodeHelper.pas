{
 * UUnicodeHelper.pas
 *
 * Classes and functions that assist with handling Unicode in compilers that
 * natively support it while maintaining compatibility with non-Uncode versions
 * of the compiler.
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
 * The Original Code is UUnicodeHelper.pas
 *
 * The Initial Developer of the Original Code is Peter Johnson
 * (http://www.delphidabbler.com/).
 *
 * Portions created by the Initial Developer are Copyright (C) 2009 Peter
 * Johnson. All Rights Reserved.
 *
 * Contributor(s)
 *   NONE
 *
 * ***** END LICENSE BLOCK *****
}


unit UUnicodeHelper;

{ TODO -cDocs : Comment this unit. }

interface

uses
  SysUtils, Classes;

{$IFDEF UNICODE}
const
  Latin1CodePage = 1252;
{$ENDIF}

type

  {$IFDEF UNICODE}
  Latin1String = type AnsiString(Latin1CodePage);
  {$ELSE}
  Latin1String = type AnsiString;
  {$ENDIF}

  TStringStreamEx = class(TStringStream)
  public
    constructor Create; overload;
    constructor Create(const AString: string); overload;
  end;

  {$IF not Declared(TBytes)}
  TBytes = array of Byte;
  {$IFEND}

function IsLatin1Char(C: Char): Boolean;
function Latin1BytesOf(const AString: string): TBytes;
function ASCIIBytesOf(const AString: string): TBytes;
function StringToLatin1String(const S: string): Latin1String;

{$IFDEF UNICODE}
type
  TLatin1Encoding = class(TMBCSEncoding)
  public
    constructor Create; override;
  end;
{$ENDIF}

function IsLetter(C: Char): Boolean;
function IsDigit(C: Char): Boolean;
function IsHexDigit(C: Char): Boolean;
function IsAlphaNumeric(C: Char): Boolean;
function IsWhiteSpace(C: Char): Boolean;
function IsCharInSet(C: Char; const CharSet: TSysCharSet): Boolean;
function ToUpperCase(C: Char): Char;

implementation

{$IFDEF UNICODE}
uses
  // Delphi
  Character;
{$ENDIF}

  {$IFDEF UNICODE}
  {$ELSE}
  {$ENDIF}

function IsLetter(C: Char): Boolean;
begin
  {$IFDEF UNICODE}
  Result := TCharacter.IsLetter(C);
  {$ELSE}
  Result := C in ['A'..'Z', 'a'..'z'];
  {$ENDIF}
end;

function IsDigit(C: Char): Boolean;
begin
  {$IFDEF UNICODE}
  Result := TCharacter.IsDigit(C);
  {$ELSE}
  Result := C in ['0'..'9'];
  {$ENDIF}
end;

function IsHexDigit(C: Char): Boolean;
begin
  Result := IsCharInSet(C, ['A'..'F', 'a'..'f', '0'..'9']);
end;

function IsAlphaNumeric(C: Char): Boolean;
begin
  {$IFDEF UNICODE}
  Result := TCharacter.IsLetterOrDigit(C);
  {$ELSE}
  Result := IsLetter(C) or IsDigit(C);
  {$ENDIF}
end;

function IsWhiteSpace(C: Char): Boolean;
begin
  {$IFDEF UNICODE}
  Result := TCharacter.IsWhiteSpace(C);
  {$ELSE}
  Result := (C = ' ') or ((C >= #$09) and (C <= #$0D))
  {$ENDIF}
end;

function IsCharInSet(C: Char; const CharSet: TSysCharSet): Boolean;
begin
  {$IFDEF UNICODE}
  Result := CharInSet(C, CharSet);
  {$ELSE}
  Result := C in CharSet;
  {$ENDIF}
end;

function ToUpperCase(C: Char): Char;
begin
  {$IFDEF UNICODE}
  Result := TCharacter.ToUpper(C);
  {$ELSE}
  Result := UpCase(C);
  {$ENDIF}
end;

{$IFNDEF UNICODE}
function AnsiStringBytesOf(const AString: string): TBytes;
var
  Len: Integer;
begin
  Len := Length(AString);
  SetLength(Result, Len);
  Move(AString[1], Result[0], Len);
end;
{$ENDIF}

function IsLatin1Char(C: Char): Boolean;
begin
  Result := Integer(C) <= $FF;
end;

function Latin1BytesOf(const AString: string): TBytes;
{$IFDEF UNICODE}
var
  Encoding: TEncoding;
begin
  Encoding := TLatin1Encoding.Create;
  try
    Result := Encoding.GetBytes(AString);
  finally
    FreeAndNil(Encoding);
  end;
end;
{$ELSE}
begin
  Result := AnsiStringBytesOf(AString);
end;
{$ENDIF}

function ASCIIBytesOf(const AString: string): TBytes;
begin
  {$IFDEF UNICODE}
  Result := TEncoding.ASCII.GetBytes(AString);
  {$ELSE}
  Result := AnsiStringBytesOf(AString);
  {$ENDIF}
end;

function BytesToAnsiString(const Bytes: TBytes): AnsiString;
var
  Len: Integer;
begin
  Len := Length(Bytes);
  SetLength(Result, Len);
  Move(Bytes[0], Result[1], Len);
end;

function StringToLatin1String(const S: string): Latin1String;
begin
  {$IFDEF UNICODE}
  Result := BytesToAnsiString(Latin1BytesOf(S));
  {$ELSE}
  Result := S;
  {$ENDIF}
end;

{ TStringStreamEx }

constructor TStringStreamEx.Create;
begin
  Create('');
end;

constructor TStringStreamEx.Create(const AString: string);
begin
  {$IFDEF UNICODE}
  inherited Create(AString, TEncoding.Unicode);
  {$ELSE}
  inherited Create(AString);
  {$ENDIF}
end;

{$IFDEF UNICODE}
{ TLatin1Encoding }

constructor TLatin1Encoding.Create;
begin
  inherited Create(Latin1CodePage);
end;
{$ENDIF}

end.


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

{$IFNDEF UNICODE}
function BytesOf(const AString: string): TBytes;
{$ENDIF}

function Latin1BytesOf(const AString: string): TBytes;
function ASCIIBytesOf(const AString: string): TBytes;

{$IFDEF UNICODE}
type
  TLatin1Encoding = class(TMBCSEncoding)
  public
    constructor Create; override;
  end;
{$ENDIF}

implementation

{$IFNDEF UNICODE}
function BytesOf(const AString: string): TBytes;
var
  Len: Integer;
begin
  Len := Length(AString);
  SetLength(Result, Len);
  Move(AString[1], Result[0], Len);
end;
{$ENDIF}

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
  Result := BytesOf(AString);
end;
{$ENDIF}

function ASCIIBytesOf(const AString: string): TBytes;
begin
  {$IFDEF UNICODE}
  Result := TEncoding.ASCII.GetBytes(AString);
  {$ELSE}
  Result := BytesOf(AString);
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


{
 * NsWebServices.UCharEncodings.pas
 *
 * Implements a class that defines and provides access to character sets and
 * encodings supported when accessing web services.
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
 * The Original Code is NsWebServices.UCharEncodings.pas
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
 * ***** END LICENSE BLOCK *****
}


unit NsWebServices.UCharEncodings;

interface

uses
  SysUtils,
  UBaseObjects;

type

  // Class of exception raised by TWSCharEncodings
  EWSCharEncodings = class(Exception);

  TWSCharEncodings = class(TNoConstructObject)
  strict private
    type
      // Maps an encoding by name onto a representative code
      TCharEncoding = record
      public
        Name: string; // name of encoding, e.g. ISO-8859-1 or UTF-8
        Code: Word;   // code page or id if encoding is not ANSI
        constructor Create(const AName: string; const ACode: Word);
      end;
    // constants used to identify encodings that map to predefined encodings of
    // TEncoding
    const UTF8CodePage = 65001;
    const ASCIICodePage = 20127;
    const UTF16LECodePage = $FFFE;  // not ANSI => no code page: use BOM instead
    const UTF16BECodePage = $FEFF;  // not ANSI => no code page: use BOM instead
    // value used when code page not recognised
    const ErrorCodePage = 0;
    // default encoding to use if none specified
    const DefaultEncoding = 'ISO-8859-1';
    // supported encodings
    class var fEncodings: array of TCharEncoding;
    class function IndexOf(const CharSet: string): Integer;
    class function CharCode(CharSet: string): Word;
  public
    class constructor Create;
    class destructor Destroy;
    class function Supported(const CharSet: string): Boolean;
      {Checks if a character set is supported.
        @param CharSet [in] String specifying character set. '' is interpreted
          as DefaultEncoding.
        @return True if character set is supported, False if not.
      }
    class function GetEncoding(const CharSet: string): TEncoding;
      {Creates an encoding for a character set. Call must free the encoding if
      it is not a default encoding.
        @param CharSet [in] String specifying character set. '' is interpreted
          as DefaultEncoding.
        @return Appropriate encoding instance.
        @except Raises EWSCharEncodings if CharSet is not supported.
      }
    class function AcceptCharSet: string;
      {Returns comma separated list of accepted character sets suitable for
      inclusion in http headers.
        @return Required list.
      }
  end;

implementation

{ TWSCharEncodings }

class function TWSCharEncodings.AcceptCharSet: string;
var
  Idx: Integer;
begin
  Result := '';
  for Idx := Low(fEncodings) to High(fEncodings) do
  begin
    if Result <> '' then
      Result := Result + ', ';
    Result := Result + fEncodings[Idx].Name;
  end;
end;

class function TWSCharEncodings.CharCode(CharSet: string): Word;
var
  Idx: Integer;
begin
  if CharSet = '' then
    CharSet := DefaultEncoding;
  Idx := IndexOf(CharSet);
  if Idx >= 0 then
    Result := fEncodings[Idx].Code
  else
    Result := 0
end;

class constructor TWSCharEncodings.Create;
begin
  SetLength(fEncodings, 6);
  fEncodings[0] := TCharEncoding.Create('UTF-8', UTF8CodePage);
  fEncodings[1] := TCharEncoding.Create('UTF-16LE', UTF16LECodePage);
  fEncodings[2] := TCharEncoding.Create('UTF-16BE', UTF16BECodePage);
  fEncodings[3] := TCharEncoding.Create('ISO-8859-1', 28591);  // latin 1
  fEncodings[4] := TCharEncoding.Create('Windows-1252', 1252);
  fEncodings[5] := TCharEncoding.Create('ASCII', ASCIICodePage);
  // NOTE: UTF-16 has been left out because some problems with PHP's
  // mb_convert_encoding() with UTF-16 have been found, so it's not safe to
  // accept this encoding
end;

class destructor TWSCharEncodings.Destroy;
begin
  SetLength(fEncodings, 0);
end;

class function TWSCharEncodings.GetEncoding(const CharSet: string): TEncoding;
resourcestring
  sErrorMsg = 'Unsupported Character Encoding: %s';
var
  Code: Word;
begin
  Code := CharCode(CharSet);
  case Code of
    ErrorCodePage: raise EWSCharEncodings.CreateFmt(sErrorMsg, [CharSet]);
    UTF8CodePage: Exit(TEncoding.UTF8);
    UTF16LECodePage: Exit(TEncoding.Unicode);
    UTF16BECodePage: Exit(TEncoding.BigEndianUnicode);
    ASCIICodePage: Exit(TEncoding.ASCII);
    else Exit(TMBCSEncoding.Create(Code));
  end;
end;

class function TWSCharEncodings.IndexOf(const CharSet: string): Integer;
var
  Idx: Integer;
begin
  for Idx := Low(fEncodings) to High(fEncodings) do
  begin
    if AnsiSameText(fEncodings[Idx].Name, CharSet) then
      Exit(Idx);
  end;
  Exit(-1);
end;

class function TWSCharEncodings.Supported(const CharSet: string): Boolean;
begin
  Result := CharCode(CharSet) = ErrorCodePage;
end;

{ TWSCharEncodings.TCharEncoding }

constructor TWSCharEncodings.TCharEncoding.Create(const AName: string;
  const ACode: Word);
begin
  Name := AName;
  Code := ACode;
end;

end.

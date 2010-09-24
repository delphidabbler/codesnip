{
 * Web.UCharEncodings.pas
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
 * The Original Code is Web.UCharEncodings.pas, formerly
 * NsWebServices.UCharEncodings.pas.
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


unit Web.UCharEncodings;


interface


uses
  // Delphi
  SysUtils,
  // Project
  UBaseObjects;


type

  {
  EWebCharEncodings:
    Class of exception raised by TWebCharEncodings.
  }
  EWebCharEncodings = class(Exception);

  {
  TWebCharEncodings:
    Static class that defines and provides access to character sets and
    encodings supported when accessing web services.
  }
  TWebCharEncodings = class(TNoConstructObject)
  strict private
    type
      {
      TCharEncoding:
        Maps an encoding by name onto a representative code page.
      }
      TCharEncoding = record
      public
        Name: string; // Name of encoding, e.g. ISO-8859-1 or UTF-8
        Code: Word;   // Code page or id if encoding is not ANSI
        constructor Create(const AName: string; const ACode: Word);
          {Constructor. Initialises record's fields.
            @param AName [in] Value for Name field.
            @param ACode [in] Value for Code filed.
          }
      end;
    // Constants used to identify encodings that map to predefined encodings of
    // TEncoding. UTF16 code pages are not ANSI: we use BOM as id.
    const
      UTF8CodePage = 65001;     // UTF-8
      ASCIICodePage = 20127;    // ASCII
      UTF16LECodePage = $FFFE;  // UTF-16 Little endian
      UTF16BECodePage = $FEFF;  // UTF-16 Big endian
      ErrorCodePage = 0;        // Used when code page not recognised
    const DefaultEncoding = 'ISO-8859-1'; // encoding used when none specified
    class var fEncodings: array of TCharEncoding; // list of supported encodings
    class function IndexOf(const CharSet: string): Integer;
      {Looks up index of a character set in list of supported encodings.
        @param CharSet [in] String indentifying Character set to look up.
        @return Index of CharSet in list of encodings or -1 if not found.
      }
    class function CharCode(CharSet: string): Word;
      {Gets character code associated with a character set.
        @param CharSet [in] String indentifying Character set to look up. '' is
          interpreted as DefaultEncoding.
        @return Required character code or 0 if character set is not supported.
      }
  public
    class constructor Create;
      {Class constructor. Initialises array of supported encodings.
      }
    class function Supported(const CharSet: string): Boolean;
      {Checks if a character set is supported.
        @param CharSet [in] String specifying character set. '' is interpreted
          as DefaultEncoding.
        @return True if character set is supported, False if not.
      }
    class function GetEncoding(const CharSet: string): TEncoding;
      {Creates an encoding for a character set. Caller must free the encoding if
      it is not a default encoding.
        @param CharSet [in] String specifying character set. '' is interpreted
          as DefaultEncoding.
        @return Appropriate encoding instance.
        @except Raises EWebCharEncodings if CharSet is not supported.
      }
    class function AcceptCharSet: string;
      {Returns comma separated list of accepted character sets suitable for
      inclusion in http headers.
        @return Required list.
      }
  end;


implementation


{ TWebCharEncodings }

class function TWebCharEncodings.AcceptCharSet: string;
  {Returns comma separated list of accepted character sets suitable for
  inclusion in http headers.
    @return Required list.
  }
var
  Encoding: TCharEncoding;  // each encoding in list of supported encodings
begin
  Result := '';
  for Encoding in fEncodings do
  begin
    if Result <> '' then
      Result := Result + ', ';
    Result := Result + Encoding.Name;
  end;
end;

class function TWebCharEncodings.CharCode(CharSet: string): Word;
  {Gets character code associated with a character set.
    @param CharSet [in] String indentifying Character set to look up. '' is
      interpreted as DefaultEncoding.
    @return Required character code or 0 if character set is not supported.
  }
var
  Idx: Integer; // index of CharSet in list of supported encodings
begin
  if CharSet = '' then
    CharSet := DefaultEncoding;
  Idx := IndexOf(CharSet);
  if Idx >= 0 then
    Result := fEncodings[Idx].Code
  else
    Result := ErrorCodePage;
end;

class constructor TWebCharEncodings.Create;
  {Class constructor. Initialises array of supported encodings.
  }
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

class function TWebCharEncodings.GetEncoding(const CharSet: string): TEncoding;
  {Creates an encoding for a character set. Caller must free the encoding if it
  is not a default encoding.
    @param CharSet [in] String specifying character set. '' is interpreted
      as DefaultEncoding.
    @return Appropriate encoding instance.
    @except Raises EWebCharEncodings if CharSet is not supported.
  }
resourcestring
  // error message
  sErrorMsg = 'Unsupported Character Encoding: %s';
var
  Code: Word; // character code associated with CharSet
begin
  Code := CharCode(CharSet);
  case Code of
    ErrorCodePage: raise EWebCharEncodings.CreateFmt(sErrorMsg, [CharSet]);
    UTF8CodePage: Exit(TEncoding.UTF8);
    UTF16LECodePage: Exit(TEncoding.Unicode);
    UTF16BECodePage: Exit(TEncoding.BigEndianUnicode);
    ASCIICodePage: Exit(TEncoding.ASCII);
    else Exit(TMBCSEncoding.Create(Code));
  end;
end;

class function TWebCharEncodings.IndexOf(const CharSet: string): Integer;
  {Looks up index of a character set in list of supported encodings.
    @param CharSet [in] String indentifying Character set to look up.
S    @return Index of CharSet in list of encodings or -1 if not found.
  }
var
  Idx: Integer; // loops thru all encodings
begin
  for Idx := Low(fEncodings) to High(fEncodings) do
  begin
    if AnsiSameText(fEncodings[Idx].Name, CharSet) then
      Exit(Idx);
  end;
  Exit(-1);
end;

class function TWebCharEncodings.Supported(const CharSet: string): Boolean;
  {Checks if a character set is supported.
    @param CharSet [in] String specifying character set. '' is interpreted as
      DefaultEncoding.
    @return True if character set is supported, False if not.
  }
begin
  Result := CharCode(CharSet) = ErrorCodePage;
end;

{ TWebCharEncodings.TCharEncoding }

constructor TWebCharEncodings.TCharEncoding.Create(const AName: string;
  const ACode: Word);
  {Constructor. Initialises record's fields.
    @param AName [in] Value for Name field.
    @param ACode [in] Value for Code filed.
  }
begin
  Name := AName;
  Code := ACode;
end;

end.


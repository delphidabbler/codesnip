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


interface


uses
  // Delphi
  SysUtils, Classes;


{$IFDEF UNICODE}
const
  Latin1CodePage = 1252;    // Code page for the Latin-1 character set
  ASCIICodePage = 20127;    // Code page for the ASCII character set
{$ENDIF}

type

  {
  Latin1String:
    String in the Latin-1 encoding. Encoding is implement only on Unicode
    versions of Delphi. Ansi version simply use AnsiString.
  }
  {$IFDEF UNICODE}
  Latin1String = type AnsiString(Latin1CodePage);
  ASCIIString = type AnsiString(ASCIICodePage);
  {$ELSE}
  Latin1String = type AnsiString;
  ASCIIString = type AnsiString;
  {$ENDIF}

  {
  TStringStreamEx:
    Extension of TStringStream that uses the Unicode encoding on Unicode
    versions of Delphi.
  }
  TStringStreamEx = class(TStringStream)
  public
    constructor Create; overload;
      {Class constructor. Creates an empty string string.
      }
    constructor Create(const AString: string); overload;
      {Class constructor. Creates a string stream containing a string.
        @param AString [in] String to be stored in string stream.
      }
  end;

  {$IF not Declared(TBytes)}
  {
  TBytes:
    Array of bytes.
  }
  TBytes = array of Byte;
  {$IFEND}


function Latin1BytesOf(const AString: string): TBytes;
  {Converts a string into an array of bytes from the latin-1 character set.
    @param AString [in] String to be converted.
    @return Required array of bytes.
  }

function ASCIIBytesOf(const AString: string): TBytes;
  {Converts a string into an array of bytes from the ASCII character set.
    @param AString [in] String to be converted.
    @return Required array of bytes.
  }

function StringToLatin1String(const S: string): Latin1String;
  {Converts a string to a latin 1 string. On Ansi Delphi versions string is
  unchanged.
    @param S [in] String to be converted.
    @return Converted string.
  }

function StringToASCIIString(const S: string): ASCIIString;
  {Converts a string to a ASCII string.
    @param S [in] String to be converted.
    @return Converted string.
  }


{$IFDEF UNICODE}
type
  {
  TLatin1Encoding:
    Provides encoding support for the Latin-1 character set.
  }
  TLatin1Encoding = class(TMBCSEncoding)
  strict private
    class var fGC: IInterface;      // garbage collector: auto-frees fInstance
    class var fInstance: TEncoding; // stores singleton object of this class
    class function GetInstance: TEncoding; static;
      {Gets reference to singleton instance of this class.
        @return Reference to singleton object.
      }
  public
    constructor Create; override;
      {Class constructor. Sets up object for latin-1 code page.
      }
    class property Instance: TEncoding read GetInstance;
      {Singleton instance of class. Must not be freed}
  end;

function Latin1Encoding: TEncoding;
  {Returns singleton instance of TLatin1Encoding.
    @return Required instance.
  }
{$ENDIF}

function IsLetter(C: Char): Boolean;
  {Checks whether a character is defined as a letter.
    @param C [in] Character to be tested.
    @return True if character is a letter, False if not.
  }

function IsDigit(C: Char): Boolean;
  {Checks whether a character is defined as a digit.
    @param C [in] Character to be tested.
    @return True if character is a digit, False if not.
  }

function IsHexDigit(C: Char): Boolean;
  {Checks whether a character is defined as a hex digit.
    @param C [in] Character to be tested.
    @return True if character is a hex digit, False if not.
  }

function IsAlphaNumeric(C: Char): Boolean;
  {Checks whether a character is defined as a letter or digit.
    @param C [in] Character to be tested.
    @return True if character is alphanumeric, False if not.
  }

function IsWhiteSpace(C: Char): Boolean;
  {Checks whether a character is defined as whitespace.
    @param C [in] Character to be tested.
    @return True if character is whitespace, False if not.
  }

function IsCharInSet(C: Char; const CharSet: TSysCharSet): Boolean;
  {Checks whether a character is a member of a character set.
    @param C [in] Character to be tested.
    @param CharSet [in] Required character set.
    @return True if character is a letter, False if not.
  }

function ToUpperCase(C: Char): Char;
  {Converts a character to upper case.
    @param C [in] Character to be converted.
    @return Upper cased character. Characters other than lower case letters are
      unchanged.
  }


implementation


uses
  {$IFDEF UNICODE}
  // Delphi
  Character,
  // Project
  UGC;
  {$ELSE}
  UConsts;
  {$ENDIF}


function IsLetter(C: Char): Boolean;
  {Checks whether a character is defined as a letter.
    @param C [in] Character to be tested.
    @return True if character is a letter, False if not.
  }
begin
  {$IFDEF UNICODE}
  Result := TCharacter.IsLetter(C);
  {$ELSE}
  Result := C in ['A'..'Z', 'a'..'z'];
  {$ENDIF}
end;

function IsDigit(C: Char): Boolean;
  {Checks whether a character is defined as a digit.
    @param C [in] Character to be tested.
    @return True if character is a digit, False if not.
  }
begin
  {$IFDEF UNICODE}
  Result := TCharacter.IsDigit(C);
  {$ELSE}
  Result := C in ['0'..'9'];
  {$ENDIF}
end;

function IsHexDigit(C: Char): Boolean;
  {Checks whether a character is defined as a hex digit.
    @param C [in] Character to be tested.
    @return True if character is a hex digit, False if not.
  }
begin
  Result := IsCharInSet(C, ['A'..'F', 'a'..'f', '0'..'9']);
end;

function IsAlphaNumeric(C: Char): Boolean;
  {Checks whether a character is defined as a letter or digit.
    @param C [in] Character to be tested.
    @return True if character is alphanumeric, False if not.
  }
begin
  {$IFDEF UNICODE}
  Result := TCharacter.IsLetterOrDigit(C);
  {$ELSE}
  Result := IsLetter(C) or IsDigit(C);
  {$ENDIF}
end;

function IsWhiteSpace(C: Char): Boolean;
  {Checks whether a character is defined as whitespace.
    @param C [in] Character to be tested.
    @return True if character is whitespace, False if not.
  }
begin
  {$IFDEF UNICODE}
  Result := TCharacter.IsWhiteSpace(C);
  {$ELSE}
  Result := IsCharInSet(C, [TAB, LF, VTAB, FF, CR, ' ']);
  {$ENDIF}
end;

function IsCharInSet(C: Char; const CharSet: TSysCharSet): Boolean;
  {Checks whether a character is a member of a character set.
    @param C [in] Character to be tested.
    @param CharSet [in] Required character set.
    @return True if character is a letter, False if not.
  }
begin
  {$IFDEF UNICODE}
  Result := CharInSet(C, CharSet);
  {$ELSE}
  Result := C in CharSet;
  {$ENDIF}
end;

function ToUpperCase(C: Char): Char;
  {Converts a character to upper case.
    @param C [in] Character to be converted.
    @return Upper cased character. Characters other than lower case letters are
      unchanged.
  }
begin
  {$IFDEF UNICODE}
  Result := TCharacter.ToUpper(C);
  {$ELSE}
  Result := UpCase(C);
  {$ENDIF}
end;

{$IFNDEF UNICODE}
function AnsiStringBytesOf(const AString: string): TBytes;
  {Converts an ansi string to an array of bytes representing the content of the
  string.
    @param AString [in] String to be converted.
    @return Array containing bytes of string.
  }
var
  Len: Integer; // length of string
begin
  Len := Length(AString);
  SetLength(Result, Len);
  Move(AString[1], Result[0], Len);
end;
{$ENDIF}

{$IFDEF UNICODE}
function Latin1Encoding: TEncoding;
  {Returns singleton instance of TLatin1Encoding.
    @return Required instance.
  }
begin
  Result := TLatin1Encoding.Instance;
end;
{$ENDIF}

function Latin1BytesOf(const AString: string): TBytes;
  {Converts a string into an array of bytes from the latin-1 character set.
    @param AString [in] String to be converted.
    @return Required array of bytes.
  }
begin
  {$IFDEF UNICODE}
  Result := Latin1Encoding.GetBytes(AString);
  {$ELSE}
  Result := AnsiStringBytesOf(AString);
  {$ENDIF}
end;

function ASCIIBytesOf(const AString: string): TBytes;
  {Converts a string into an array of bytes from the ASCII character set.
    @param AString [in] String to be converted.
    @return Required array of bytes.
  }
{$IFDEF UNICODE}
begin
  Result := TEncoding.ASCII.GetBytes(AString);
end;
{$ELSE}
var
  Idx: Integer; // loops thru bytes of Result
begin
  Result := AnsiStringBytesOf(AString);
  // flag invalid bytes with '?'
  for Idx := Low(Result) to High(Result) do
    if Result[Idx] > $7F  then
      Result[Idx] := Ord('?');
end;
{$ENDIF}

function BytesToASCIIString(const Bytes: TBytes): ASCIIString;
  {Creates an ansi string from an array of bytes.
    @param Bytes [in] Array to be converted to string.
    @return Required string.
  }
var
  Len: Integer; // length of byte array
begin
  Len := Length(Bytes);
  SetLength(Result, Len);
  Move(Bytes[0], Result[1], Len);
end;

function BytesToAnsiString(const Bytes: TBytes): AnsiString;
  {Creates an ansi string from an array of bytes.
    @param Bytes [in] Array to be converted to string.
    @return Required string.
  }
var
  Len: Integer; // length of byte array
begin
  Len := Length(Bytes);
  SetLength(Result, Len);
  Move(Bytes[0], Result[1], Len);
end;

function StringToLatin1String(const S: string): Latin1String;
  {Converts a string to a latin 1 string. On Ansi Delphi versions string is
  unchanged.
    @param S [in] String to be converted.
    @return Converted string.
  }
begin
  {$IFDEF UNICODE}
  Result := BytesToAnsiString(Latin1BytesOf(S));
  {$ELSE}
  Result := S;
  {$ENDIF}
end;

function StringToASCIIString(const S: string): ASCIIString;
  {Converts a string to a ASCII string.
    @param S [in] String to be converted.
    @return Converted string.
  }
begin
  Result := BytesToASCIIString(ASCIIBytesOf(S));
end;


{ TStringStreamEx }

constructor TStringStreamEx.Create;
  {Class constructor. Creates an empty string string.
  }
begin
  Create('');
end;

constructor TStringStreamEx.Create(const AString: string);
  {Class constructor. Creates a string stream containing a string.
    @param AString [in] String to be stored in string stream.
  }
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
  {Class constructor. Sets up object for latin-1 code page.
  }
begin
  inherited Create(Latin1CodePage);
end;

class function TLatin1Encoding.GetInstance: TEncoding;
  {Gets reference to singleton instance of this class.
    @return Reference to singleton object.
  }
begin
  if not Assigned(fInstance) then
  begin
    fInstance := TLatin1Encoding.Create;
    TGC.GCLocalObj(fGC, fInstance); // add fInstance to GC to be auto-freed
  end;
  Result := fInstance;
end;
{$ENDIF}

end.


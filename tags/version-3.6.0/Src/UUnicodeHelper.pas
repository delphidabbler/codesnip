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
 * Portions created by the Initial Developer are Copyright (C) 2009-2010 Peter
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


const
  Windows1252CodePage = 1252; // Code page for the Windows-1252 character set
  ASCIICodePage = 20127;      // Code page for the ASCII character set

type

  {
  Windows1252String:
    String using the Windows-1252 code page
  }
  Windows1252String = type AnsiString(Windows1252CodePage);
  {
  ASCIIString:
    String using the ASCII code page.
  }
  ASCIIString = type AnsiString(ASCIICodePage);


function Windows1252BytesOf(const AString: string): TBytes;
  {Converts a string into an array of bytes from the Windows-1252 character set.
    @param AString [in] String to be converted.
    @return Required array of bytes.
  }

function ASCIIBytesOf(const AString: string): TBytes;
  {Converts a string into an array of bytes from the ASCII character set.
    @param AString [in] String to be converted.
    @return Required array of bytes.
  }

function StringToWindows1252String(const S: string): Windows1252String;
  {Converts a string to a Windows-1252 string.
    @param S [in] String to be converted.
    @return Converted string.
  }

function StringToASCIIString(const S: string): ASCIIString;
  {Converts a string to a ASCII string.
    @param S [in] String to be converted.
    @return Converted string.
  }

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
  // Delphi
  Character,
  // Project
  UGC;


type
  {
  TWindows1252Encoding:
    Provides encoding support for the Windows-1252 character set.
  }
  TWindows1252Encoding = class(TMBCSEncoding)
  strict private
    class var fGC: IInterface;      // garbage collector: auto-frees fInstance
    class var fInstance: TEncoding; // stores singleton object of this class
    class function GetInstance: TEncoding; static;
      {Gets reference to singleton instance of this class.
        @return Reference to singleton object.
      }
  public
    constructor Create; override;
      {Class constructor. Sets up object for Windows-1252 code page.
      }
    class property Instance: TEncoding read GetInstance;
      {Singleton instance of class. Must not be freed}
  end;


function IsLetter(C: Char): Boolean;
  {Checks whether a character is defined as a letter.
    @param C [in] Character to be tested.
    @return True if character is a letter, False if not.
  }
begin
  Result := TCharacter.IsLetter(C);
end;

function IsDigit(C: Char): Boolean;
  {Checks whether a character is defined as a digit.
    @param C [in] Character to be tested.
    @return True if character is a digit, False if not.
  }
begin
  Result := TCharacter.IsDigit(C);
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
  Result := TCharacter.IsLetterOrDigit(C);
end;

function IsWhiteSpace(C: Char): Boolean;
  {Checks whether a character is defined as whitespace.
    @param C [in] Character to be tested.
    @return True if character is whitespace, False if not.
  }
begin
  Result := TCharacter.IsWhiteSpace(C);
end;

function IsCharInSet(C: Char; const CharSet: TSysCharSet): Boolean;
  {Checks whether a character is a member of a character set.
    @param C [in] Character to be tested.
    @param CharSet [in] Required character set.
    @return True if character is a letter, False if not.
  }
begin
  Result := CharInSet(C, CharSet);
end;

function ToUpperCase(C: Char): Char;
  {Converts a character to upper case.
    @param C [in] Character to be converted.
    @return Upper cased character. Characters other than lower case letters are
      unchanged.
  }
begin
  Result := TCharacter.ToUpper(C);
end;

function Windows1252Encoding: TEncoding;
  {Returns singleton instance of TWindows1252Encoding.
    @return Required instance.
  }
begin
  Result := TWindows1252Encoding.Instance;
end;

function Windows1252BytesOf(const AString: string): TBytes;
  {Converts a string into an array of bytes from the Windows-1252 character set.
    @param AString [in] String to be converted.
    @return Required array of bytes.
  }
begin
  Result := Windows1252Encoding.GetBytes(AString);
end;

function ASCIIBytesOf(const AString: string): TBytes;
  {Converts a string into an array of bytes from the ASCII character set.
    @param AString [inString to be converted.
    @return Required array of bytes.
  }
begin
  Result := TEncoding.ASCII.GetBytes(AString);
end;

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

function StringToWindows1252String(const S: string): Windows1252String;
  {Converts a string to a Windows-1252 string.
    @param S [in] String to be converted.
    @return Converted string.
  }
begin
  Result := BytesToAnsiString(Windows1252BytesOf(S));
end;

function StringToASCIIString(const S: string): ASCIIString;
  {Converts a string to a ASCII string.
    @param S [in] String to be converted.
    @return Converted string.
  }
begin
  Result := BytesToASCIIString(ASCIIBytesOf(S));
end;

{ TWindows1252Encoding }

constructor TWindows1252Encoding.Create;
  {Class constructor. Sets up object for Windows-1252 code page.
  }
begin
  inherited Create(Windows1252CodePage);
end;

class function TWindows1252Encoding.GetInstance: TEncoding;
  {Gets reference to singleton instance of this class.
    @return Reference to singleton object.
  }
begin
  if not Assigned(fInstance) then
  begin
    fInstance := TWindows1252Encoding.Create;
    TGC.GCLocalObj(fGC, fInstance); // add fInstance to GC to be auto-freed
  end;
  Result := fInstance;
end;

end.


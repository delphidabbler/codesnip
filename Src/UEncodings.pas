{
 * UEncodings.pas
 *
 * Provides support for certain character encodings used by the program.
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
 * The Original Code is UEncodings.pas, formerly UUnicodeHelper.pas
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


unit UEncodings;


interface


uses
  // Delphi
  SysUtils;


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

function StringToWindows1252String(const S: string): Windows1252String;
  {Converts a string to a Windows-1252 string.
    @param S [in] String to be converted.
    @return Converted string.
  }

function StringToASCIIString(const S: string): ASCIIString;
  {Converts a string to an ASCII string.
    @param S [in] String to be converted.
    @return Converted string.
  }


implementation


uses
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
      {Object constructor. Sets up object for Windows-1252 code page.
      }
    class property Instance: TEncoding read GetInstance;
      {Singleton instance of class. Must not be freed}
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

function BytesToAnsiString(const Bytes: TBytes; const CP: Word): RawByteString;
  {Converts an array of bytes to an ANSI raw byte string.
  NOTE: Based on Stack Overflow posting at <URL:http://bit.ly/bAvtGd>.
    @param Bytes [in] Array of bytes to be converted to ANSI string.
    @param CP [in] Code page of required ANSI string. Bytes must contain valid
      bytes for this code page.
    @return Required string with specified code page.
  }
begin
  SetLength(Result, Length(Bytes));
  if Length(Bytes) > 0 then
  begin
    Move(Bytes[0], Result[1], Length(Bytes));
    SetCodePage(Result, CP, False);
  end;
end;

function StringToWindows1252String(const S: string): Windows1252String;
  {Converts a string to a Windows-1252 string.
    @param S [in] String to be converted.
    @return Converted string.
  }
begin
  Result := BytesToAnsiString(Windows1252BytesOf(S), Windows1252CodePage);
end;

function StringToASCIIString(const S: string): ASCIIString;
  {Converts a string to an ASCII string.
    @param S [in] String to be converted.
    @return Converted string.
  }
begin
  Result := BytesToAnsiString(TEncoding.ASCII.GetBytes(S), ASCIICodePage);
end;

{ TWindows1252Encoding }

constructor TWindows1252Encoding.Create;
  {Object constructor. Sets up object for Windows-1252 code page.
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


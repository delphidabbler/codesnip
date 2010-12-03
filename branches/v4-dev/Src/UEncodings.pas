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
  SysUtils, Generics.Defaults, Generics.Collections,
  // Project
  UBaseObjects;


type
  ///  <summary>
  ///  Static class that maintains a list of named character sets and enables
  ///  the associated encoding object to be created. For single byte character
  ///  access is provided to the matching code page, and vice versa. Also
  ///  exposes useful constants.
  ///  </summary>
  TEncodingHelper = class(TNoConstructObject)
  strict private
    ///  Type of closure used to create a TEncoding instance.
    type TEncodingFactoryFn = reference to function: TEncoding;
    ///  Class that maps char set names onto encoding factory closures.
    type TEncodingMap = class(TDictionary<string,TEncodingFactoryFn>);
    ///  Class that maps char set names onto single byte char set code pages.
    type TCodePageNameMap = class(TDictionary<string,Integer>);
    ///  Type of pair of entries in TCodePageNameMap dictionary.
    type TCodePageNameMapPair = TPair<string,Integer>;
    ///  Class that maps code pages to char set names.
    type TCodePageValueMap = class(TDictionary<Integer,string>);
    ///  Map of char set names onto encoding factory closures
    class var fEncodingMap: TEncodingMap;
    ///  Map of char set names onto single byte char set code pages.
    class var fCodePageNameMap: TCodePageNameMap;
    ///  Map of code pages to char set names.
    class var fCodePageValueMap: TCodePageValueMap;
  public
    // Constants storing names of recognised character sets
    ///  <summary>Default character set name: empty string</summary>
    const DefaultCharSetName = '';
    ///  ASCII character set
    const ASCIICharSetName = 'ASCII';
    ///  ISO-8859-1 character set
    const ISO88591CharSetName = 'ISO-8859-1';
    ///  UTF-8 character set
    const UTF8CharSetName = 'UTF-8';
    ///  UTF-16 character set
    const UTF16CharSetName = 'UTF-16';
    ///  UTF-16 big endian character set
    const UTF16BECharSetName = 'UTF-16BE';
    ///  UTF-16 little endian character set
    const UTF16LECharSetName = 'UTF-16LE';
    ///  Windows 1252 character set
    const Windows1252CharSetName = 'Windows-1252';
    // Constants storing code pages of recognised single byte character sets
    ///  ASCII code page
    const ASCIICodePage = 20127;
    ///  ISO-8859-1 code page
    const ISO88591CodePage = 28591;
    ///  UTF-8 code page
    const UTF8CodePage = 65001;
    ///  Windows-1252 code page
    const Windows1252CodePage = 1252;
  public
    ///  <summary>Initialises encoding maps.</summary>
    class constructor Create;
    ///  <summary>Frees encoding maps.</summary>
    class destructor Destroy;
    ///  <summary>
    ///  Frees an given encoding providing it is not a standard encoding.
    ///  </summary>
    ///  <param name="Encoding">Encoding to be freed if necessary.</param>
    class procedure FreeEncoding(const Encoding: TEncoding);
    ///  <summary>
    ///  Checks if a character set name is supported.
    ///  </summary>
    ///  <param name="Name">Character set name to test.</param>
    ///  <returns>True if character set supported, False if not.</returns>
    class function IsSupported(const Name: string): Boolean;
    ///  <summary>
    ///  Creates an encoding instance that is associated with a character set
    ///  name.
    ///  </summary>
    ///  <param name="Name">Name of character set.</param>
    ///  <returns>New encoding instance for the character set.</returns>
    ///  <remarks>
    ///  <para>Caller is responsible for freeing the encoding if it is not a
    ///  standard encoding.</para>
    ///  <para>Exception raised if character set not supported.</para>
    ///  </remarks>
    class function GetEncoding(const Name: string): TEncoding; overload;
    ///  <summary>
    ///  Gets an encoding that is associated with a code page.
    ///  </summary>
    ///  <param name="CodePage">Code page of required encoding.</param>
    ///  <returns>New encoding instance for the code page.</returns>
    ///  <remarks>
    ///  <para>Caller is responsible for freeing the encoding if it is not a
    ///  standard encoding.</para>
    ///  <para>Exception raised if code page not supported.</para>
    ///  </remarks>
    class function GetEncoding(const CodePage: Integer): TEncoding; overload;
    ///  <summary>
    ///  Checks if there is a code page associated with a character set name.
    ///  </summary>
    ///  <param name="Name">Name of character set.</param>
    ///  <returns>True if there is an associated code page, False if not.
    ///  </returns>
    class function HasCodePage(const Name: string): Boolean;
    ///  <summary>
    ///  Checks if a code page is supported.
    ///  </summary>
    ///  <param name="CodePage">Code page to be checked.</param>
    ///  <returns>True if the code page is supported, False if not.</returns>
    class function IsCodePageSupported(const CodePage: Integer): Boolean;
    ///  <summary>
    ///  Gets the code page associated with a character set name.
    ///  </summary>
    ///  <param name="Name">Name of character set to check.</param>
    ///  <returns>Code page associated with character set.</returns>
    ///  <remarks>Exception raised if character set is not supported.</remarks>
    class function GetCodePage(const Name: string): Integer;
    ///  <summary>
    ///  Gets the character set name associated with a code page
    ///  </summary>
    ///  <param name="CodePage">Code page for which character set name required.
    ///  </param>
    ///  <returns>Required character set name.</returns>
    ///  <remarks>Exception raised if code page not supported.</remarks>
    class function GetCodePageName(const CodePage: Integer): string;
  end;

  ///  Ansi string using the ASCII code page
  ASCIIString = type AnsiString(20127);


///  <summary>
///  Converts a Unicode string into an ANSI string using the ASCII code page.
///  </summary>
function StringToASCIIString(const S: string): ASCIIString;


implementation


uses
  // Project
  UGC, ULocales;


///  <summary>
///  Converts as array of bytes to an ANSI raw byte string.
///  </summary>
///  <param name="Bytes">Byte array to convert.</param>
///  <param name="CP">Code page of returned ANSI string.</param>
///  <returns>ANSI string with requested code page.</returns>
///  <remarks>
///  <para>Caller must ensure that the byte array has the correct format for
///  the requested code page.</para>
///  <para>Based on Stack Overflow posting at http://bit.ly/bAvtGd.</para>
///  </remarks>
function BytesToAnsiString(const Bytes: TBytes; const CP: Word): RawByteString;
begin
  SetLength(Result, Length(Bytes));
  if Length(Bytes) > 0 then
  begin
    Move(Bytes[0], Result[1], Length(Bytes));
    SetCodePage(Result, CP, False);
  end;
end;

function StringToASCIIString(const S: string): ASCIIString;
begin
  Result := BytesToAnsiString(
    TEncoding.ASCII.GetBytes(S), TEncodingHelper.ASCIICodePage
  );
end;

{ TEncodingHelper }

class constructor TEncodingHelper.Create;

  // Returns a closure that can create a multibyte encoding for a given code
  // page.
  function MBCSFactoryFn(CodePage: Integer): TEncodingFactoryFn;
  begin
    Result := function: TEncoding
    begin
      Result := TMBCSEncoding.Create(CodePage);
    end;
  end;

var
  // References to various encoding factory functions
  DefaultFactoryFn: TEncodingFactoryFn;
  ASCIIFactoryFn: TEncodingFactoryFn;
  UTF8FactoryFn: TEncodingFactoryFn;
  UTF16FactoryFn: TEncodingFactoryFn;
  UTF16BEFactoryFn: TEncodingFactoryFn;
  CodePagePair: TCodePageNameMapPair;
begin
  // Set references to appropriate encoding factory functions
  DefaultFactoryFn :=
    function: TEncoding begin Result := TEncoding.Default; end;
  ASCIIFactoryFn :=
    function: TEncoding begin Result := TEncoding.ASCII; end;
  UTF8FactoryFn :=
    function: TEncoding begin Result := TEncoding.UTF8; end;
  UTF16FactoryFn :=
    function: TEncoding begin Result := TEncoding.Unicode; end;
  UTF16BEFactoryFn :=
    function: TEncoding begin Result := TEncoding.BigEndianUnicode; end;

  // Create & populate map of char set names to encoding factory functions
  fEncodingMap := TEncodingMap.Create;
  fEncodingMap.Add(DefaultCharSetName, DefaultFactoryFn);
  fEncodingMap.Add(ASCIICharSetName, ASCIIFactoryFn);
  fEncodingMap.Add(ISO88591CharSetName, MBCSFactoryFn(ISO88591CodePage));
  fEncodingMap.Add(UTF8CharSetName, UTF8FactoryFn);
  fEncodingMap.Add(Windows1252CharSetName, MBCSFactoryFn(Windows1252CodePage));
  fEncodingMap.Add(UTF16LECharSetName, UTF16FactoryFn);
  fEncodingMap.Add(UTF16BECharSetName, UTF16BEFactoryFn);
  fEncodingMap.Add(UTF16CharSetName, UTF16FactoryFn);

  // Create & populate map of single byte char set names to code pages
  fCodePageNameMap := TCodePageNameMap.Create;
  fCodePageNameMap.Add(DefaultCharSetName, DefaultAnsiCodePage);
  fCodePageNameMap.Add(ASCIICharSetName, ASCIICodePage);
  fCodePageNameMap.Add(ISO88591CharSetName, ISO88591CodePage);
  fCodePageNameMap.Add(UTF8CharSetName, UTF8CodePage);
  fCodePageNameMap.Add(Windows1252CharSetName, Windows1252CodePage);

  // Create & populate reverse map of char set names to code pages
  // values are same as added to fCodePageNameMap with key and value reversed
  // (except we ignore default code page)
  fCodePageValueMap := TCodePageValueMap.Create;
  for CodePagePair in fCodePageNameMap do
    if CodePagePair.Key <> DefaultCharSetName then
      fCodePageValueMap.Add(CodePagePair.Value, CodePagePair.Key);

end;

class destructor TEncodingHelper.Destroy;
begin
  fCodePageValueMap.Free;
  fCodePageNameMap.Free;
  fEncodingMap.Free;
end;

class procedure TEncodingHelper.FreeEncoding(const Encoding: TEncoding);
begin
  if not TEncoding.IsStandardEncoding(Encoding) then
    Encoding.Free;
end;

class function TEncodingHelper.GetCodePage(const Name: string): Integer;
begin
  Result := fCodePageNameMap[Name];
end;

class function TEncodingHelper.GetCodePageName(const CodePage: Integer): string;
begin
  Result := fCodePageValueMap[CodePage];
end;

class function TEncodingHelper.GetEncoding(const CodePage: Integer): TEncoding;
var
  Name: string;   // name of char set with this code page
begin
  Name := GetCodePageName(CodePage);
  Result := GetEncoding(Name);
end;

class function TEncodingHelper.GetEncoding(const Name: string): TEncoding;
begin
  Result := fEncodingMap[Name]();
end;

class function TEncodingHelper.HasCodePage(const Name: string): Boolean;
begin
  Result := fCodePageNameMap.ContainsKey(Name);
end;

class function TEncodingHelper.IsCodePageSupported(
  const CodePage: Integer): Boolean;
begin
  Result := fCodePageValueMap.ContainsKey(CodePage);
end;

class function TEncodingHelper.IsSupported(const Name: string): Boolean;
begin
  Result := fEncodingMap.ContainsKey(Name);
end;

end.


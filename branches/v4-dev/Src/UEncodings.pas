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
  SysUtils, Types, Generics.Defaults, Generics.Collections,
  // Project
  UBaseObjects, UIStringList;


type
  ///  <summary>
  ///  Static class that maintains a list of named character sets and enables
  ///  the associated encoding object to be created. For single byte character
  ///  access is provided to the matching code page, and vice versa. Also
  ///  exposes useful constants.
  ///  </summary>
  TEncodingHelper = class(TNoConstructObject)
  strict private
    ///  <summary>Type of closure used to create a TEncoding instance.</summary>
    type TEncodingFactoryFn = reference to function: TEncoding;
    ///  <summary>Class that maps char set names onto encoding factory closures.
    ///  </summary>
    type TEncodingMap = class(TDictionary<string,TEncodingFactoryFn>);
    ///  <summary>Class that maps character set names onto code pages.</summary>
    type TCodePageNameMap = class(TDictionary<string,Integer>);
    ///  <summary>Key / value pair in TCodePageNameMap dictionary.</summary>
    type TCodePageNameMapPair = TPair<string,Integer>;
    ///  <summary>Class that maps code pages to character set names.</summary>
    type TCodePageValueMap = class(TDictionary<Integer,string>);
    ///  Map of char set names onto encoding factory closures.
    class var fEncodingMap: TEncodingMap;
    ///  Map of char set names onto single byte char set code pages.
    class var fCodePageNameMap: TCodePageNameMap;
    ///  Map of code pages to char set names.
    class var fCodePageValueMap: TCodePageValueMap;
    ///  <summary>
    ///  Read accessor for CharSets property. Creates list of supported
    ///  character sets.
    ///  </summary>
    class function GetCharSets: TStringDynArray; static;
  public
    // Constants storing names of recognised character sets
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
  strict protected
    ///  <summary>
    ///  Checks if a character set is wanted in the current class.
    ///  </summary>
    ///  <param name="CharSet">Name of a character set to be tested</param>
    ///  <returns>Always true</returns>
    ///  <remarks>
    ///  <para>A wanted character set is a sub-set of those registered in the
    ///  class constructor. Descendant classes may need to exclude some
    ///  characters sets from those they support. This is done by returning
    ///  False from this method when called with character set names that are
    ///  not supported.</para>
    ///  <para>All known character sets are wanted in this class, so True is
    ///  always returned.</para>
    ///  </remarks>
    class function IsWantedCharSet(const CharSet: string): Boolean; virtual;
    ///  <summary>
    ///  Checks if a named character set is known to the class.
    ///  </summary>
    ///  <param name="CharSet">Name of a character set to be tested.</param>
    ///  <returns>True if character set is known to class, False if not.
    ///  </returns>
    ///  <remarks>
    ///  A known character set is one that is registered in the class
    ///  constructor.
    ///  </remarks>
    class function IsKnownCharSet(const CharSet: string): Boolean;
    ///  <summary>
    ///  Checks if a code page is known to the class.
    ///  </summary>
    ///  <param name="CodePage">Code page to be tested.</param>
    ///  <returns>True if code page is known to this class.</returns>
    ///  <remarks>
    ///  A known code page is one that is registered in the class constructor.
    ///  </remarks>
    class function IsKnownCodePage(const CodePage: Integer): Boolean;
  public
    ///  <summary>Initialises encoding maps.</summary>
    class constructor Create;
    ///  <summary>Frees encoding maps.</summary>
    class destructor Destroy;
    ///  <summary>
    ///  Returns name of character set to use by default when empty string is
    ///  specified as a character set name.
    ///  </summary>
    ///  <returns>Required default character set name.</returns>
    ///  <remarks>
    ///  Descendant classes can override to change the default character set.
    ///  </remarks>
    class function DefaultCharSet: string; virtual;
    ///  <summary>
    ///  Frees an given encoding providing it is not a standard encoding.
    ///  </summary>
    ///  <param name="Encoding">Encoding to be freed if necessary.</param>
    ///  <remarks>Use this method to free any encoding created by this class,
    ///  or an encoding created elsewhere to avoid the overhead of testing
    ///  for a standard encoding before freeing.</remarks>
    class procedure FreeEncoding(const Encoding: TEncoding);
    ///  <summary>
    ///  Checks if a named character set is supported.
    ///  </summary>
    ///  <param name="Name">Name of character set to test.</param>
    ///  <returns>True if character set supported, False if not.</returns>
    class function IsSupportedCharSet(const CharSet: string): Boolean;
    ///  <summary>
    ///  Checks if a code page is supported.
    ///  </summary>
    ///  <param name="CodePage">Code page to be checked.</param>
    ///  <returns>True if the code page is supported, False if not.</returns>
    class function IsSupportedCodePage(const CodePage: Integer): Boolean;
    ///  <summary>
    ///  Creates an encoding instance that is associated with a named character
    ///  set.
    ///  </summary>
    ///  <param name="CharSet">Name of character set.</param>
    ///  <returns>New encoding instance for the character set.</returns>
    ///  <remarks>
    ///  <para>Caller is responsible for freeing the encoding if it is not a
    ///  standard encoding.</para>
    ///  <para>Exception raised if character set not supported.</para>
    ///  </remarks>
    class function GetEncoding(const CharSet: string): TEncoding; overload;
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
    ///  <param name="CharSet">Name of character set to check.</param>
    ///  <returns>True if there is an associated code page, False if not or if
    ///  character set not supported.
    ///  </returns>
    class function HasCodePage(const CharSet: string): Boolean;
    ///  <summary>
    ///  Gets the code page associated with a named character set.
    ///  </summary>
    ///  <param name="CharSet">Name of character set to check.</param>
    ///  <returns>Code page associated with character set.</returns>
    ///  <remarks>Exception raised if character set is not supported.</remarks>
    class function GetCodePage(const CharSet: string): Integer;
    ///  <summary>
    ///  Gets the character set name associated with a code page
    ///  </summary>
    ///  <param name="CodePage">Code page for which character set name required.
    ///  </param>
    ///  <returns>Required character set name.</returns>
    ///  <remarks>Exception raised if code page not supported.</remarks>
    class function GetCodePageName(const CodePage: Integer): string;
    ///  <summary>
    ///  Returns an array names of supported character sets.
    ///  </summary>
    ///  <returns>String array of required character set names.</returns>
    class function CharSets: TStringDynArray;
  end;

  ///  Ansi string using the ASCII code page.
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

resourcestring
  // Error messages
  sBadCharSet = 'Character set %s not supported';
  sBadCodePage = 'Code page %d not supported';

class function TEncodingHelper.CharSets: TStringDynArray;
var
  CharSet: string;
  I: Integer;
begin
  SetLength(Result, fEncodingMap.Count);
  I := 0;
  for CharSet in fEncodingMap.Keys do
  begin
    if IsWantedCharSet(CharSet) then
    begin
      Result[I] := CharSet;
      Inc(I);
    end;
  end;
  SetLength(Result, I);
end;

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
  // CharSet, CodePage pair
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
  fEncodingMap.Add(ASCIICharSetName, ASCIIFactoryFn);
  fEncodingMap.Add(ISO88591CharSetName, MBCSFactoryFn(ISO88591CodePage));
  fEncodingMap.Add(UTF8CharSetName, UTF8FactoryFn);
  fEncodingMap.Add(Windows1252CharSetName, MBCSFactoryFn(Windows1252CodePage));
  fEncodingMap.Add(UTF16LECharSetName, UTF16FactoryFn);
  fEncodingMap.Add(UTF16BECharSetName, UTF16BEFactoryFn);
  fEncodingMap.Add(UTF16CharSetName, UTF16FactoryFn);

  // Create & populate map of single byte char set names to code pages
  fCodePageNameMap := TCodePageNameMap.Create;
  fCodePageNameMap.Add(ASCIICharSetName, ASCIICodePage);
  fCodePageNameMap.Add(ISO88591CharSetName, ISO88591CodePage);
  fCodePageNameMap.Add(UTF8CharSetName, UTF8CodePage);
  fCodePageNameMap.Add(Windows1252CharSetName, Windows1252CodePage);

  // Create & populate reverse map of char set names to code pages
  // values are same as added to fCodePageNameMap with key and value reversed
  // (except we ignore default code page)
  fCodePageValueMap := TCodePageValueMap.Create;
  for CodePagePair in fCodePageNameMap do
    fCodePageValueMap.Add(CodePagePair.Value, CodePagePair.Key);
end;

class function TEncodingHelper.DefaultCharSet: string;
begin
  Result := UTF8CharSetName;
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

class function TEncodingHelper.GetCharSets: TStringDynArray;
var
  CharSet: string;
  I: Integer;
begin
  SetLength(Result, fEncodingMap.Count);
  I := 0;
  for CharSet in fEncodingMap.Keys do
  begin
    if IsWantedCharSet(CharSet) then
    begin
      Result[I] := CharSet;
      Inc(I);
    end;
  end;
  SetLength(Result, I);
end;

class function TEncodingHelper.GetCodePage(const CharSet: string): Integer;
begin
  if CharSet = '' then
    Result := fCodePageNameMap[DefaultCharSet]
  else if IsSupportedCharSet(CharSet) then
    Result := fCodePageNameMap[CharSet]
  else
    raise ENotSupportedException.CreateFmt(sBadCharSet, [CharSet]);
end;

class function TEncodingHelper.GetCodePageName(const CodePage: Integer): string;
begin
  if not IsSupportedCodePage(CodePage) then
    raise ENotSupportedException.CreateFmt(sBadCodePage, [CodePage]);
  Result := fCodePageValueMap[CodePage];
end;

class function TEncodingHelper.GetEncoding(const CodePage: Integer): TEncoding;
begin
  if not IsSupportedCodePage(CodePage) then
    raise ENotSupportedException.CreateFmt(sBadCodePage, [CodePage]);
  Result := GetEncoding(GetCodePageName(CodePage));
end;

class function TEncodingHelper.GetEncoding(const CharSet: string): TEncoding;
begin
  if CharSet = '' then
    Result := fEncodingMap[DefaultCharSet]()
  else if IsSupportedCharSet(CharSet) then
    Result := fEncodingMap[CharSet]()
  else
    raise ENotSupportedException.CreateFmt(sBadCharSet, [CharSet]);
end;

class function TEncodingHelper.HasCodePage(const CharSet: string): Boolean;
begin
  Result := fCodePageNameMap.ContainsKey(CharSet) and IsWantedCharSet(CharSet);
end;

class function TEncodingHelper.IsKnownCharSet(const CharSet: string): Boolean;
begin
  Result := fEncodingMap.ContainsKey(CharSet);
end;

class function TEncodingHelper.IsKnownCodePage(
  const CodePage: Integer): Boolean;
begin
  Result := fCodePageValueMap.ContainsKey(CodePage);
end;

class function TEncodingHelper.IsSupportedCharSet(
  const CharSet: string): Boolean;
begin
  Result := IsKnownCharSet(CharSet) and IsWantedCharSet(CharSet);
end;

class function TEncodingHelper.IsSupportedCodePage(
  const CodePage: Integer): Boolean;
begin
  if not IsKnownCodePage(CodePage) then
    Exit(False);
  Result := IsWantedCharSet(fCodePageValueMap[CodePage]);
end;

class function TEncodingHelper.IsWantedCharSet(const CharSet: string): Boolean;
begin
  Result := True;
end;

end.


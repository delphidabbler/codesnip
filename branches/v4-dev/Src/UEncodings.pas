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


const
  { TODO: change code that uses these to use TCharEncodings consts the remove }
  Windows1252CodePage = 1252; // Code page for the Windows-1252 character set
  ASCIICodePage = 20127;      // Code page for the ASCII character set

type

  ///  Ansi string using the Windows-1252 code page
  Windows1252String = type AnsiString(Windows1252CodePage);

  ///  Ansi string using the ASCII code page
  ASCIIString = type AnsiString(ASCIICodePage);


///  Encodes a Unicode string into an array of bytes using the Windows-1252
///  code page.
function Windows1252BytesOf(const AString: string): TBytes;

///  Converts a Unicode string into an ANSI string using the Windows-1252 code
///  page.
function StringToWindows1252String(const S: string): Windows1252String;

///  Converts a Unicode string into an ANSI string using the ASCII code page.
function StringToASCIIString(const S: string): ASCIIString;


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
    ///  Default character set name: empty string
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
    ///  Initialises maps
    class constructor Create;
    ///  Frees maps
    class destructor Destroy;
    ///  Frees the given encoding providing it is not a standard encoding.
    class procedure FreeEncoding(const Encoding: TEncoding);
    ///  Checks if a given char set name is supported.
    class function IsSupported(const Name: string): Boolean;
    ///  Gets an encoding instance that is associated with a char set name.
    class function GetEncoding(const Name: string): TEncoding; overload;
    ///  Gets an encoding that is associated with a code page.
    class function GetEncoding(const CodePage: Integer): TEncoding; overload;
    ///  Checks if there is a code page associated with a char set name.
    class function HasCodePage(const Name: string): Boolean;
    ///  Checks if a specified code page is supported.
    class function IsCodePageSupported(const CodePage: Integer): Boolean;
    ///  Gets the code page associated with a char set name.
    class function GetCodePage(const Name: string): Integer;
    ///  Gets the char set name associated with a code page
    class function GetCodePageName(const CodePage: Integer): string;
  end;

implementation


uses
  // Project
  UGC, ULocales;


type
  // TODO: remove this class when all code changed to use TEncodingHelper
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
begin
  Result := BytesToAnsiString(Windows1252BytesOf(S), Windows1252CodePage);
end;

function StringToASCIIString(const S: string): ASCIIString;
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

{ TEncodingHelper }

class constructor TEncodingHelper.Create;

  // ---------------------------------------------------------------------------
  // Returns a closure that can create a multibyte encoding for a given code
  // page.
  function MBCSFactoryFn(CodePage: Integer): TEncodingFactoryFn;
  begin
    Result := function: TEncoding
    begin
      Result := TMBCSEncoding.Create(CodePage);
    end;
  end;
  // ---------------------------------------------------------------------------

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
  // values are same as added to fCodePageNameMap, with key and value reversed
  fCodePageValueMap := TCodePageValueMap.Create;
  for CodePagePair in fCodePageNameMap do
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


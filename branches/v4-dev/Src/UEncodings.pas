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
  ///  Enumeration of identifiers of supported encodings.
  ///  </summary>
  TEncodingType = (
    etSysDefault, // default ANSI encoding
    etASCII,      // ASCII
    etISO88591,   // ISO-8859-1
    etUTF8,       // UTF-8
    etUTF16,      // UTF-16
    etUTF16BE,    // UTF-16BE
    etUTF16LE,    // UTF-16LE
    etWindows1252 // Windows-1252
  );

type
  ///  <summary>
  ///  Static class that maintains a list of named character sets and enables
  ///  the associated encoding object to be created. For single byte character
  ///  access is provided to the matching code page, and vice versa. Also
  ///  exposes useful constants.
  ///  </summary>
  { TODO: Restore or delete commented out methods }
  TEncodingHelper = class(TNoConstructObject)
  strict private
    type
      ///  <summary>Type of closure used to create a TEncoding instance.
      ///  </summary>
      TEncodingFactoryFn = reference to function: TEncoding;
    type
      ///  <summary>
      ///  Record storing required information about an encoding.
      ///  </summary>
      TEncodingInfo = record
        ///  <summary>Character set of encoding.</summary>
        ///  <remarks>Set to '' for default ANSI encoding.</remarks>
        CharSet: string;
        ///  <summary>Flag true if ANSI encoding.</summary>
        ///  <remarks>When False CodePage field has no meaning.</remarks>
        IsAnsi: Boolean;
        ///  <summary>Code page of encoding.</summary>
        ///  <remarks>Should be ignored when IsAnsi is False.</remarks>
        CodePage: Integer;
        ///  <summary>Closure used to create TEncoding instances for the
        ///  encoding.</summary>
        FactoryFn: TEncodingFactoryFn;
      end;
    class var
      ///  <summary>Map of encoding types onto related information records.
      ///  </summary>
      fMap: array[TEncodingType] of TEncodingInfo;
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
    ///  Looks up a specified character set in fMap.
    ///  </summary>
    ///  <param name="CharSet">string [in] Character set.</param>
    ///  <param name="EncodingType">TEncodingType [out] Set to encoding type
    ///  of character set it is known. Undefined otherwise.</param>
    ///  <returns>True if character set is known, False if not.</returns>
    class function LookupCharSet(const CharSet: string;
      out EncodingType: TEncodingType): Boolean;
    ///  <summary>
    ///  Looks up a specified character set in fMap.
    ///  </summary>
    ///  <param name="CharSet">string [in] Character set.</param>
    ///  <param name="EncodingType">TEncodingType [out] Set to encoding type
    ///  of character set if it is both known and wanted. Undefined otherwise.
    ///  </param>
    ///  <returns>True if character set is both known and wanted, False if
    ///  not.</returns>
    class function LookupValidCharSet(const CharSet: string;
      out EncodingType: TEncodingType): Boolean;
    ///  <summary>
    ///  Looks up a specified code page in fMap.
    ///  </summary>
    ///  <param name="CodePage">Integer [in] Code page.</param>
    ///  <param name="EncodingType">TEncodingType [out] Set to encoding type
    ///  of code page if it is known. Undefined otherwise.</param>
    ///  <returns>True if code page is known, False if not.</returns>
    class function LookupCodePage(const CodePage: Integer;
      out EncodingType: TEncodingType): Boolean;
    ///  <summary>
    ///  Looks up a specified code page in fMap.
    ///  </summary>
    ///  <param name="CodePage">Integer [in] Code page.</param>
    ///  <param name="EncodingType">TEncodingType [out] Set to encoding type
    ///  of code page if it is both known and wanted. Undefined otherwise.
    ///  </param>
    ///  <returns>True if code page is both known and wanted, False if not.
    ///  </returns>
    class function LookupValidCodePage(const CodePage: Integer;
      out EncodingType: TEncodingType): Boolean;
    ///  <summary>
    ///  Gets the code page of a character set, if any.
    ///  </summary>
    ///  <param name="CharSet">string [in] Character set.</param>
    ///  <param name="CodePage">Integer [out] Set to code page of character set
    ///  if it is know, wanted and is an ANSI character set. Undefined
    ///  otherwise.</param>
    ///  <returns>True if character set known, wanted and is an ANSI character
    ///  set. False if not.
    ///  </returns>
    class function GetValidCodePage(const CharSet: string;
      out CodePage: Integer): Boolean;
  public
    // Constants storing names of recognised character sets
    const
      ///  <summary>ASCII character set name.</summary>
      ASCIICharSetName = 'ASCII';
      ///  <summary>ISO-8859-1 character set name.</summary>
      ISO88591CharSetName = 'ISO-8859-1';
      ///  <summary>UTF-8 character set name.</summary>
      UTF8CharSetName = 'UTF-8';
      ///  <summary>UTF-16 character set name.</summary>
      UTF16CharSetName = 'UTF-16';
      ///  <summary>UTF-16 big endian character set name.</summary>
      UTF16BECharSetName = 'UTF-16BE';
      ///  <summary>UTF-16 little endian character set name.</summary>
      UTF16LECharSetName = 'UTF-16LE';
      ///  <summary>Windows 1252 character set name.</summary>
      Windows1252CharSetName = 'Windows-1252';
      // Constants storing code pages of recognised single byte character sets
      ///  <summary>ASCII code page.</summary>
      ASCIICodePage = 20127;
      ///  <summary>ISO-8859-1 code page.</summary>
      ISO88591CodePage = 28591;
      ///  <summary>UTF-8 code page.</summary>
      UTF8CodePage = 65001;
      ///  <summary>Windows-1252 code page.</summary>
      Windows1252CodePage = 1252;
    ///  <summary>Initialises encoding maps.</summary>
    class constructor Create;
    ///  <summary>
    ///  Returns name of character set to use by default when empty string is
    ///  specified as a character set name.
    ///  </summary>
    ///  <returns>string - Required default character set name.</returns>
    ///  <remarks>
    ///  Descendant classes can override to change the default character set.
    ///  </remarks>
    class function DefaultCharSet: string; virtual;
    ///  <summary>
    ///  Frees an given encoding providing it is not a standard encoding.
    ///  </summary>
    ///  <param name="Encoding">TEncoding [in] Encoding to be freed if
    ///  necessary.</param>
    ///  <remarks>Use this method to free any encoding created by this class,
    ///  or an encoding created elsewhere to avoid the overhead of testing
    ///  for a standard encoding before freeing.</remarks>
    class procedure FreeEncoding(const Encoding: TEncoding);
(*
    ///  <summary>
    ///  Checks if a named character set is supported.
    ///  </summary>
    ///  <param name="Name">string [in] Name of character set to test.</param>
    ///  <returns>True if character set supported, False if not.</returns>
    class function IsSupportedCharSet(const CharSet: string): Boolean;
    ///  <summary>
    ///  Checks if a code page is supported.
    ///  </summary>
    ///  <param name="CodePage">Integer [in] Code page to be checked.</param>
    ///  <returns>True if the code page is supported, False if not.</returns>
    class function IsSupportedCodePage(const CodePage: Integer): Boolean;
    ///  <summary>
    ///  Creates an encoding instance for a specified encoding type.
    ///  </summary>
    ///  <param name="EncType">TEncodingType [in] Encoding type.</param>
    ///  <returns>New encoding instance for the type.</returns>
    ///  <remarks>
    ///  Caller is responsible for freeing the encoding if it is not a standard
    ///  encoding.
    ///  </remarks>
*)
    class function GetEncoding(const EncType: TEncodingType): TEncoding;
      overload;
    ///  <summary>
    ///  Creates an encoding instance that is associated with a named character
    ///  set.
    ///  </summary>
    ///  <param name="CharSet">string [in] Name of character set.</param>
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
    ///  <param name="CodePage">Integer [in] Code page.</param>
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
    ///  <param name="CharSet">string [in] Name of character set.</param>
    ///  <returns>True if there is an associated code page, False if not or if
    ///  character set not supported.
    ///  </returns>
(*
    class function HasCodePage(const CharSet: string): Boolean;
    ///  <summary>
    ///  Gets the code page associated with a named character set.
    ///  </summary>
    ///  <param name="CharSet">string [in] Name of character set.</param>
    ///  <returns>Code page associated with character set.</returns>
    ///  <remarks>Exception raised if character set is not supported.</remarks>
    class function GetCodePage(const CharSet: string): Integer;
    ///  <summary>
    ///  Gets the character set name associated with a code page
    ///  </summary>
    ///  <param name="CodePage">Integer [in] Code page.</param>
    ///  <returns>Required character set name.</returns>
    ///  <remarks>Exception raised if code page not supported.</remarks>
    class function GetCodePageName(const CodePage: Integer): string;
*)
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
///  <param name="Bytes">TBytes [in] Byte array to convert.</param>
///  <param name="CP">Integer [in] Code page of returned ANSI string.</param>
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
  I: Integer;
  EncInfo: TEncodingInfo;
begin
  SetLength(Result, Length(fMap));  // overestimate result size
  I := 0;
  for EncInfo in fMap do
  begin
    if (EncInfo.CharSet <> '') and IsWantedCharSet(EncInfo.CharSet) then
    begin
      Result[I] := EncInfo.CharSet;
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

  // Populate map for all encodings
  with fMap[etSysDefault] do
  begin
    CharSet := '';
    IsAnsi := True;
    CodePage := ULocales.DefaultAnsiCodePage;
    FactoryFn := DefaultFactoryFn;
  end;
  with fMap[etASCII] do
  begin
    CharSet := ASCIICharSetName;
    IsAnsi := True;
    CodePage := ASCIICodePage;
    FactoryFn := ASCIIFactoryFn;
  end;
  with fMap[etISO88591] do
  begin
    CharSet := ISO88591CharSetName;
    IsAnsi := True;
    CodePage := ISO88591CodePage;
    FactoryFn := MBCSFactoryFn(ISO88591CodePage);
  end;
  with fMap[etUTF8] do
  begin
    CharSet := UTF8CharSetName;
    IsAnsi := True;
    CodePage := UTF8CodePage;
    FactoryFn := UTF8FactoryFn;
  end;
  with fMap[etUTF16] do
  begin
    CharSet := UTF16CharSetName;
    IsAnsi := False;
    CodePage := 0;
    FactoryFn := UTF16FactoryFn;
  end;
  with fMap[etUTF16BE] do
  begin
    CharSet := UTF16BECharSetName;
    IsAnsi := False;
    CodePage := 0;
    FactoryFn := UTF16BEFactoryFn;
  end;
  with fMap[etUTF16LE] do
  begin
    CharSet := UTF16LECharSetName;
    IsAnsi := False;
    CodePage := 0;
    FactoryFn := UTF16FactoryFn;
  end;
  with fMap[etWindows1252] do
  begin
    CharSet := Windows1252CharSetName;
    IsAnsi := True;
    CodePage := Windows1252CodePage;
    FactoryFn := MBCSFactoryFn(Windows1252CodePage);
  end;
end;

class function TEncodingHelper.DefaultCharSet: string;
begin
  Result := UTF8CharSetName;
end;

class procedure TEncodingHelper.FreeEncoding(const Encoding: TEncoding);
begin
  if not TEncoding.IsStandardEncoding(Encoding) then
    Encoding.Free;
end;

(*
class function TEncodingHelper.GetCodePage(const CharSet: string): Integer;
begin
  if not GetValidCodePage(CharSet, Result) then
    raise ENotSupportedException.CreateFmt(sBadCharSet, [CharSet]);
end;

class function TEncodingHelper.GetCodePageName(const CodePage: Integer): string;
var
  EncType: TEncodingType;
begin
  if not LookupValidCodePage(CodePage, EncType) then
    raise ENotSupportedException.CreateFmt(sBadCodePage, [CodePage]);
  Result := fMap[EncType].CharSet;
end;
*)
class function TEncodingHelper.GetEncoding(const CharSet: string): TEncoding;
var
  EncType: TEncodingType;
begin
  if not LookupValidCharSet(CharSet, EncType) then
    raise ENotSupportedException.CreateFmt(sBadCharSet, [CharSet]);
  Result := GetEncoding(EncType);
end;

class function TEncodingHelper.GetEncoding(const CodePage: Integer): TEncoding;
var
  EncType: TEncodingType;
begin
  if not LookupValidCodePage(CodePage, EncType) then
    raise ENotSupportedException.CreateFmt(sBadCodePage, [CodePage]);
  Result := GetEncoding(EncType);
end;

class function TEncodingHelper.GetEncoding(
  const EncType: TEncodingType): TEncoding;
begin
  Result := fMap[EncType].FactoryFn();
end;

class function TEncodingHelper.GetValidCodePage(const CharSet: string;
  out CodePage: Integer): Boolean;
var
  EncType: TEncodingType;
begin
  if not LookupValidCharSet(CharSet, EncType) then
    Exit(False);
  if not fMap[EncType].IsAnsi then
    Exit(False);
  CodePage := fMap[EncType].CodePage;
  Result := True;
end;

(*
class function TEncodingHelper.HasCodePage(const CharSet: string): Boolean;
var
  Dummy: Integer;
begin
  Result := GetValidCodePage(CharSet, Dummy);
end;

class function TEncodingHelper.IsSupportedCharSet(
  const CharSet: string): Boolean;
var
  Dummy: TEncodingType;
begin
  Result := LookupValidCharSet(CharSet, Dummy);
end;

class function TEncodingHelper.IsSupportedCodePage(
  const CodePage: Integer): Boolean;
var
  Dummy: TEncodingType;
begin
  Result := LookupValidCodePage(CodePage, Dummy);
end;
*)

class function TEncodingHelper.IsWantedCharSet(const CharSet: string): Boolean;
begin
  Result := True;
end;

class function TEncodingHelper.LookupCharSet(const CharSet: string;
  out EncodingType: TEncodingType): Boolean;
var
  EncType: TEncodingType;
begin
  if CharSet = '' then  // we never find default char set
    Exit(False);
  for EncType := Low(TEncodingType) to High(TEncodingType) do
  begin
    if AnsiSameText(CharSet, fMap[EncType].CharSet) then
    begin
      EncodingType := EncType;
      Exit(True);
    end;
  end;
  Result := False;
end;

class function TEncodingHelper.LookupCodePage(const CodePage: Integer;
  out EncodingType: TEncodingType): Boolean;
var
  EncType: TEncodingType;
begin
  for EncType := Low(TEncodingType) to High(TEncodingType) do
  begin
    if fMap[EncType].IsAnsi and (fMap[EncType].CodePage = CodePage) then
    begin
      EncodingType := EncType;
      Exit(True);
    end;
  end;
  Result := False;
end;

class function TEncodingHelper.LookupValidCharSet(const CharSet: string;
  out EncodingType: TEncodingType): Boolean;
begin
  Result := LookupCharSet(CharSet, EncodingType) and IsWantedCharSet(CharSet);
end;

class function TEncodingHelper.LookupValidCodePage(const CodePage: Integer;
  out EncodingType: TEncodingType): Boolean;
begin
  Result := LookupCodePage(CodePage, EncodingType)
    and IsWantedCharSet(fMap[EncodingType].CharSet);
end;

end.


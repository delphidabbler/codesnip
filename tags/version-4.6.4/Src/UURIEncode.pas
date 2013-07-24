{
 * This Source Code Form is subject to the terms of the Mozilla Public License,
 * v. 2.0. If a copy of the MPL was not distributed with this file, You can
 * obtain one at http://mozilla.org/MPL/2.0/
 *
 * Copyright (C) 2010-2012, Peter Johnson (www.delphidabbler.com).
 *
 * $Rev$
 * $Date$
 *
 * Routines that can encode and decode URIs according to RFC 3986.
}


unit UURIEncode;


interface


const
  // Chars reserved for URIs: see RFC 3986 section 2.2
  // generic: reserved by generic URI syntax
  cURIGenReservedChars = [
    ':', '/', '?', '#', '[', ']', '@'
  ];
  // may be reserved by different URI schemes
  cURISubReservedChars = [
    '!', '$', '&', '''', '(', ')', '*', '+', ',', ';', '='
  ];
  // % character treated as reserved because it is used in percent encoding and
  // must therefore be percent encoded if to be used literally in URI:
  // see RFC 3986 section 2.4
  cPercent = '%';
  // all the reserved chars: union of above
  cURIReservedChars =
    cURIGenReservedChars + cURISubReservedChars + [cPercent];

  // Unreserved URI chars: see RFC 3986 section 2.3
  cURLUnreservedChars = [
    'A'..'Z', 'a'..'z', '0'..'9', '-', '_', '.', '~'
  ];

  // Special reserved char used to encode spaces in query string encoding
  cPlus = '+';


function URIEncode(const S: UTF8String): string; overload;
  {URI encodes a string.
    @param S [in] String of UTF-8 characters to be encoded.
    @return Encoded string. Contains only ASCII unreserved characters and "%".
  }
function URIEncode(const S: UnicodeString): string; overload;
  {URI encodes a string.
    @param S [in] String of Unicode characters to be encoded.
    @return Encoded string. Contains only ASCII unreserved characters and "%".
  }
function URIEncode(const S: AnsiString): string; overload;
  {URI encodes a string.
    @param S [in] String of Unicode characters to be encoded.
    @return Encoded string. Contains only ASCII unreserved characters and "%".
  }

function URIEncodeQueryString(const S: UTF8String): string; overload;
  {URI encodes a query string component. Spaces in original string are encoded
  as "+".
    @param S [in] String of UTF-8 characters to be encoded.
    @return Encoded string. Contains only ASCII unreserved characters and "%".
  }
function URIEncodeQueryString(const S: UnicodeString): string; overload;
  {URI encodes a query string component. Spaces in original string are encoded
  as "+".
    @param S [in] String of Unicode characters to be encoded.
    @return Encoded string. Contains only ASCII unreserved characters and "%".
  }
function URIEncodeQueryString(const S: AnsiString): string; overload;
  {URI encodes a query string component. Spaces in original string are encoded
  as "+".
    @param S [in] String of ANSI characters to be encoded.
    @return Encoded string. Contains only ASCII unreserved characters and "%".
  }

function URIDecode(const Str: string): string;
  {Decodes a URI encoded string.
    @param Str [in] String to be decoded. *May* but should not contain
      characters outside the unreserved character set per RFC 3986.
    @return Decoded string.
    @except EConvertError raised if Str contains malformed % escape sequences.
  }

function URIDecodeQueryString(const Str: string): string;
  {Decodes a URI encoded query string where spaces have been encoded as '+'.
    @param Str [in] String to be decoded. *May* but should not contain
      characters outside the unreserved character set per RFC 3986.
    @return Decoded string.
    @except EConvertError raised if Str contains malformed % escape sequences.
  }


implementation


uses
  // Delphi
  SysUtils,
  // Project
  UStrUtils;


resourcestring
  // Error messages
  rsEscapeError = 'String to be decoded contains invalid % escape sequence';

const
  // Percent encoding of space character
  cPercentEncodedSpace = '%20';

{
  Internally, URIDecode operates on UTF-8 strings for both input and output.

  This lets us deal easily with any multi-byte characters in the input. There
  SHOULDN'T be any such characters since all characters in a percent encoded URI
  should map onto the unreserved characters from the ASCII character set.
  However we need to allow for badly encoded URIs that *may* contain characters
  outside this expected set.

  UTF-8 also lets perform an easy test for '%' characters in input. Since '%'
  can never occur and a UTF-8 continuation character we can simply test for the
  actual character without worrying about if it is part of of a multibyte
  character.

  We use UTF-8 for the output string since UTF-8 should have been used to encode
  the URI in the first place, therefore percent-encoded octets may map onto
  UTF-8 continuation bytes. Using any other string type would give erroneous
  results.

  We convert interim UTF-8 result into the native string type before returning.
}

function URIDecode(const Str: string): string;
  {Decodes a URI encoded string.
    @param Str [in] String to be decoded. *May* but should not contain
      characters outside the unreserved character set per RFC 3986.
    @return Decoded string.
    @except EConvertError raised if Str contains malformed % escape sequences.
  }

  // ---------------------------------------------------------------------------
  function CountPercent(const S: UTF8String): Integer;
    {Counts number of '%' characters in a UTF8 string.
      @param S [in] String for which '%' characters to be counted.
      @return Number of '%' characters in S.
    }
  var
    Idx: Integer; // loops thru all octets of S
  begin
    Result := 0;
    for Idx := 1 to Length(S) do
      if S[Idx] = cPercent then
        Inc(Result);
  end;
  // ---------------------------------------------------------------------------

var
  SrcUTF8: UTF8String;  // input string as UTF-8
  SrcIdx: Integer;      // index into source UTF-8 string
  ResUTF8: UTF8String;  // output string as UTF-8
  ResIdx: Integer;      // index into result UTF-8 string
  Hex: string;          // hex component of % encoding
  ChValue: Integer;     // character ordinal value from a % encoding
begin
  // Convert input string to UTF-8
  SrcUTF8 := UTF8Encode(Str);
  // Size the decoded UTF-8 string: each 3 byte sequence starting with '%' is
  // replaced by a single byte. All other bytes are copied unchanged.
  SetLength(ResUTF8, Length(SrcUTF8) - 2 * CountPercent(SrcUTF8));
  SrcIdx := 1;
  ResIdx := 1;
  while SrcIdx <= Length(SrcUTF8) do
  begin
    if SrcUTF8[SrcIdx] = cPercent then
    begin
      // % encoding: decode following two hex chars into required code point
      if Length(SrcUTF8) < SrcIdx + 2 then
        raise EConvertError.Create(rsEscapeError);  // malformed: too short
      Hex := '$' + string(SrcUTF8[SrcIdx + 1] + SrcUTF8[SrcIdx + 2]);
      if not TryStrToInt(Hex, ChValue) then
        raise EConvertError.Create(rsEscapeError);  // malformed: not valid hex
      ResUTF8[ResIdx] := AnsiChar(ChValue);
      Inc(ResIdx);
      Inc(SrcIdx, 3);
    end
    else
    begin
      // plain char or UTF-8 continuation character: copy unchanged
      ResUTF8[ResIdx] := SrcUTF8[SrcIdx];
      Inc(ResIdx);
      Inc(SrcIdx);
    end;
  end;
  // Convert back to native string type for result
  Result := UTF8ToString(ResUTF8);
end;

function URIDecodeQueryString(const Str: string): string;
  {Decodes a URI encoded query string where spaces have been encoded as '+'.
    @param Str [in] String to be decoded. *May* but should not contain
      characters outside the unreserved character set per RFC 3986.
    @return Decoded string.
    @except EConvertError raised if Str contains malformed % escape sequences.
  }
begin
  // First replace plus signs with spaces. We use percent-encoded spaces here
  // because string is still URI encoded and space is not one of unreserved
  // chars and therefor should be percent-encoded. Finally we decode the
  // percent-encoded string.
  Result := URIDecode(StrReplace(Str, cPlus, cPercentEncodedSpace));
end;

{
  Extract from RFC 3986 section 2.5:
    "the data should first be encoded as octets according to the UTF-8 character
    encoding [STD63]; then only those octets that do not correspond to
    characters in the unreserved set should be percent-encoded.

  These means we can simply scan a UTF-8 string and encode anything we find that
  isn't in the unreserved set. We need't worry about any continuation bytes in
  the UTF-8 encoding because all continuation bytes are greater than $80, and
  all unreserved characters are from a sub-set of ASCII and therefore have an
  ordinal value of less than $80. So we needn't worry about detecting lead and
  continuation bytes.

  For details of the UTF-8 encoding see http://en.wikipedia.org/wiki/UTF-8

  NOTE:
    URIEncode should be applied to the component parts of the URI before they
    are combined, not to the whole URI. See RFC 3986 section 2.4
}

function URIEncode(const S: UTF8String): string; overload;
  {URI encodes a string.
    @param S [in] String of UTF-8 characters to be encoded.
    @return Encoded string. Contains only ASCII unreserved characters and "%".
  }
var
  Ch: AnsiChar; // each character in S
begin
  // Just scan the string an octet at a time looking for chars to encode
  Result := '';
  for Ch in S do
    if CharInSet(Ch,  cURLUnreservedChars) then
      Result := Result + WideChar(Ch)
    else
      Result := Result + '%' + IntToHex(Ord(Ch), 2);
end;

function URIEncode(const S: UnicodeString): string; overload;
  {URI encodes a string.
    @param S [in] String of Unicode characters to be encoded.
    @return Encoded string. Contains only ASCII unreserved characters and "%".
  }
begin
  Result := URIEncode(UTF8Encode(S));
end;

function URIEncode(const S: AnsiString): string; overload;
  {URI encodes a string.
    @param S [in] String of ANSI characters to be encoded.
    @return Encoded string. Contains only ASCII unreserved characters and "%".
  }
begin
  Result := URIEncode(UTF8Encode(S));
end;

function URIEncodeQueryString(const S: UTF8String): string; overload;
  {URI encodes a query string component. Spaces in original string are encoded
  as "+".
    @param S [in] String of UTF-8 characters to be encoded.
    @return Encoded string. Contains only ASCII unreserved characters and "%".
  }
begin
  // First we URI encode the string. This so any existing '+' symbols get
  // encoded because we use them to replace spaces and we can't confuse '+'
  // already in URI with those that we add. After this step spaces get encoded
  // as %20. So next we replace all occurences of %20 with '+'.
  Result := StrReplace(URIEncode(S), cPercentEncodedSpace, cPlus);
end;

function URIEncodeQueryString(const S: UnicodeString): string; overload;
  {URI encodes a query string component. Spaces in original string are encoded
  as "+".
    @param S [in] String of Unicode characters to be encoded.
    @return Encoded string. Contains only ASCII unreserved characters and "%".
  }
begin
  Result := URIEncodeQueryString(UTF8Encode(S));
end;

function URIEncodeQueryString(const S: AnsiString): string; overload;
  {URI encodes a query string component. Spaces in original string are encoded
  as "+".
    @param S [in] String of ANSI characters to be encoded.
    @return Encoded string. Contains only ASCII unreserved characters and "%".
  }
begin
  Result := URIEncodeQueryString(UTF8Encode(S));
end;

end.


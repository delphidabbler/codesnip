{
 * This Source Code Form is subject to the terms of the Mozilla Public License,
 * v. 2.0. If a copy of the MPL was not distributed with this file, You can
 * obtain one at https://mozilla.org/MPL/2.0/
 *
 * Copyright (C) 2005-2022, Peter Johnson (gravatar.com/delphidabbler).
 *
 * Utility functions used when processing RTF.
}


unit URTFUtils;


interface


uses
  // Delphi
  SysUtils, Classes, ComCtrls,
  // Project
  UEncodings;


const
  ///  <summary>RTF version.</summary>
  cRTFVersion = 1;


type
  ///  <summary>Enumeration containing identifiers for each supported RTF
  ///  control word.</summary>
  TRTFControl = (
    rcRTF,                  // RTF version
    rcAnsi,                 // use ANSI character set
    rcAnsiCodePage,         // specifies ANSI code page
    rcDefFontNum,           // default font number
    rcDefLanguage,          // default language
    rcFontTable,            // introduces font table
    rcFontPitch,            // font pitch
    rcFontCharset,          // font character set
    rcFontFamilyNil,        // unknown font family
    rcFontFamilyRoman,      // serif, proportional fonts
    rcFontFamilySwiss,      // sans-serif, proportional fonts
    rcFontFamilyModern,     // fixed pitch serif and sans-serif fonts
    rcFontFamilyScript,     // script fonts
    rcFontFamilyDecor,      // decorative fonts
    rcFontFamilyTech,       // technical, symbol and maths fonts
    rcColorTable,           // introduces colour table
    rcRed,                  // defines red colour component
    rcGreen,                // defines gree colour component
    rcBlue,                 // defines blue colour component
    rcInfo,                 // introduces information group
    rcTitle,                // sets document title
    rcPard,                 // resets to default paragraph format
    rcPar,                  // begins new paragraph
    rcPlain,                // reset font (character) formatting properties
    rcFontNum,              // font number (index to font table)
    rcForeColorNum,         // foreground colour number (index to colour table)
    rcBold,                 // sets or toggles bold style
    rcItalic,               // sets or toggles italic style
    rcUnderline,            // sets or toggles underline style
    rcFontSize,             // font size in 1/2 points
    rcSpaceBefore,          // space before paragraphs in twips
    rcSpaceAfter,           // space after paragraph in twips
    rcUnicodeChar,          // defines a Unicode character as signed 16bit value
    rcUnicodePair,          // introduces pair of ANSI and Unicode destinations
    rcUnicodeDest,          // introduces Unicode destination
    rcIgnore,               // denotes following control can be ignored
    rcFirstLineOffset,      // first line indent in twips (relative to \li)
    rcLeftIndent,           // left indent in twips
    rcTabStop               // sets a tab stop in twips
  );

type
  ///  <summary>Encapsulate rich text markup code.</summary>
  ///  <remarks>Valid rich text markup contains only ASCII characters.</remarks>
  TRTF = record
  strict private
    var
      ///  <summary>Byte array that stores RTF code as bytes</summary>
      fData: TBytes;
  public
    ///  <summary>Initialises record from raw binary data.</summary>
    ///  <param name="ABytes">TBytes [in] Array storing RTF code as bytes.
    ///  </param>
    constructor Create(const ABytes: TBytes); overload;
    ///  <summary>Initialises record from a stream of character data.</summary>
    ///  <param name="AStream">TStream [in] Stream containing RTF code.</param>
    ///  <param name="AEncoding">TEncoding [in] Encoding to be used to decode
    ///  characters in stream.</param>
    ///  <param name="ReadAll">Boolean [in] Flag that indicates if the whole
    ///  stream is to be read (True) or stream is to be read from current
    ///  position (False).</param>
    constructor Create(const AStream: TStream; const ReadAll: Boolean = False);
      overload;
    ///  <summary>Initialises record from ASCII RTF code.</summary>
    ///  <param name="ARTFCode">ASCIIString [in] ASCII string containing RTF
    ///  code.</param>
    constructor Create(const ARTFCode: ASCIIString); overload;
    ///  <summary>Initialises record from RTF code stored in Unicode string.
    ///  </summary>
    ///  <param name="AStr">UnicodeString [in] Unicode string containing ASCII
    ///  code.</param>
    ///  <remarks>An exception is raised if AStr contains any non-ASCII
    ///  characters.</remarks>
    constructor Create(const AStr: UnicodeString); overload;
    ///  <summary>Initialises record from encoded data.</summary>
    ///  <param name="AData">TEncodedData [in] Encoded data record containing
    ///  RTF code.</param>
    ///  <remarks>An exception is raised if AData contains any non-ASCII
    ///  characters.</remarks>
    constructor Create(const AData: TEncodedData); overload;
    ///  <summary>Returns RTF code as an array of bytes.</summary>
    function ToBytes: TBytes;
    ///  <summary>Returns RTF code as an ASCII string.</summary>
    function ToRTFCode: ASCIIString;
    ///  <summary>Returns RTF code as a Unicode string.</summary>
    ///  <remarks>Returned string contains only ASCII characters.</remarks>
    function ToString: UnicodeString;
    ///  <summary>Copies RTF code to a stream, optionally overwriting any
    ///  existing content in the stream.</summary>
    procedure ToStream(const Stream: TStream; const Overwrite: Boolean = False);
    ///  <summary>Checks if a Unicode string contains only valid RTF characters.
    ///  </summary>
    function IsValidRTFCode(const AStr: UnicodeString): Boolean;
  end;

type
  ///  <summary>Class of exception raised by TRTF</summary>
  ERTF = class(Exception);

type
  ///  <summary>Static method record that assists in working with rich edit
  ///  VCL controls.</summary>
  TRichEditHelper = record
  public
    ///  <summary>Loads RTF code into a rich edit control, replacing existing
    ///  content.</summary>
    ///  <param name="RE">TRichEdit [in] Rich edit control.</param>
    ///  <param name="RTF">TRTF [in] Contains rich text code to be loaded.
    ///  </param>
    class procedure Load(const RE: TRichEdit; const RTF: TRTF); static;
  end;


///  <summary>Returns a parameterless RTF control word of given kind.</summary>
function RTFControl(const Ctrl: TRTFControl): ASCIIString; overload;

///  <summary>Returns a parameterised RTF control word of given kind with given
///  parameter value.</summary>
function RTFControl(const Ctrl: TRTFControl;
  const Param: SmallInt): ASCIIString; overload;

///  <summary>Returns an RTF escape sequence for the given ANSI character.
///  </summary>
function RTFEscape(const Ch: AnsiChar): ASCIIString;

///  <summary>returns an RTF hexadecimal escape sequence for given ANSI
///  character.</summary>
function RTFHexEscape(const Ch: AnsiChar): ASCIIString;

///  <summary>Encodes given text for given code page so that any incompatible
///  characters are replaced by suitable control words.</summary>
function RTFMakeSafeText(const TheText: string; const CodePage: Integer):
  ASCIIString;

///  <summary>Creates an RTF destination in a Unicode safe way.</summary>
///  <param name="DestCtrl">TRTFControl [in] Destination control.</param>
///  <param name="DestText">string [in] Text of destination.</param>
///  <param name="CodePage">Integer [in] Code page to use for encoding.</param>
///  <returns>ASCIIString. Destination RTF, containing ANSI and Unicode
///  sub-destinations if necessary.</returns>
///  <remarks>If DestText contains only characters supported by the given code
///  page then a normal destination is returned, containing only the given text.
///  Should any characters in DestText be incompatible with the code page then
///  two sub-destinations are created, one ANSI only and the other containing
///  Unicode characters.</remarks>
function RTFUnicodeSafeDestination(const DestCtrl: TRTFControl;
  const DestText: string; const CodePage: Integer): ASCIIString;


implementation


uses
  // Delphi
  Windows, RichEdit,
  // Project
  UExceptions;


const
  // Map of RTF control ids to control word
  cControls: array[TRTFControl] of ASCIIString = (
    'rtf', 'ansi', 'ansicpg', 'deff', 'deflang', 'fonttbl', 'fprq', 'fcharset',
    'fnil', 'froman', 'fswiss', 'fmodern', 'fscript', 'fdecor', 'ftech',
    'colortbl', 'red', 'green', 'blue', 'info', 'title', 'pard', 'par', 'plain',
    'f', 'cf', 'b', 'i', 'ul', 'fs', 'sb', 'sa', 'u', 'upr', 'ud', '*',
    'fi', 'li', 'tx'
  );

function RTFControl(const Ctrl: TRTFControl): ASCIIString;
begin
  Result := '\' + cControls[Ctrl];
end;

function RTFControl(const Ctrl: TRTFControl;
  const Param: SmallInt): ASCIIString;
begin
  Result := RTFControl(Ctrl) + StringToASCIIString(IntToStr(Param));
end;

function RTFEscape(const Ch: AnsiChar): ASCIIString;
begin
  Result := AnsiChar('\') + Ch;
end;

function RTFHexEscape(const Ch: AnsiChar): ASCIIString;
begin
  Result := StringToASCIIString('\''' + IntToHex(Ord(Ch), 2));
end;

function RTFMakeSafeText(const TheText: string; const CodePage: Integer):
  ASCIIString;
var
  Ch: Char;                     // each Unicode character in TheText
  AnsiChars: TArray<AnsiChar>;  // translation of a Ch into ANSI code page
  AnsiCh: AnsiChar;             // each ANSI char in AnsiChars
begin
  Result := '';
  for Ch in TheText do
  begin
    if WideCharToChar(Ch, CodePage, AnsiChars) then
    begin
      for AnsiCh in AnsiChars do
      begin
        if (AnsiCh < #$20) or ((AnsiCh >= #$7F) and (AnsiCh <= #$FF)) then
          Result := Result + RTFHexEscape(AnsiCh)
        else if (Ch = '{') or (Ch = '\') or (Ch = '}') then
          Result := Result + RTFEscape(AnsiCh)
        else
          Result := Result + ASCIIString(AnsiCh);
      end;
    end
    else
      Result := Result + RTFControl(rcUnicodeChar, SmallInt(Ord(Ch))) + ' ?';
  end;
end;

function RTFUnicodeSafeDestination(const DestCtrl: TRTFControl;
  const DestText: string; const CodePage: Integer): ASCIIString;

  ///  Makes a destination for DestCtrl using given text.
  function MakeDestination(const S: string): ASCIIString;
  begin
    Result := '{'
      + RTFControl(DestCtrl) + ' '
      + RTFMakeSafeText(S, CodePage)
      + '}'
  end;

var
  Encoding: TEncoding;  // encoding for CodePage
  AnsiStr: string;      // Unicode string containing only characters of CodePage
begin
  if CodePageSupportsString(DestText, CodePage) then
    // All chars of DestText supported in code page => RTF text won't have any
    // \u characters => we can just output destination as normal
    Result := MakeDestination(DestText)
  else
  begin
    // DestText contains characters not supported by code page. We create twin
    // destinations, one ANSI only and the other that includes Unicode
    // characters.
    Encoding := TMBCSEncoding.Create(CodePage);
    try
      // Create a Unicode string that contains only characters supported in
      // given code page (+ some "error" characters (e.g. "?")
      AnsiStr := Encoding.GetString(Encoding.GetBytes(DestText));
    finally
      Encoding.Free;
    end;
    Result := '{'
      + RTFControl(rcUnicodePair)
      + MakeDestination(AnsiStr)    // ANSI only destination
      + '{'
      + RTFControl(rcIgnore)
      + RTFControl(rcUnicodeDest)
      + MakeDestination(DestText)   // Unicode destinatation
      + '}'
      + '}';
  end;
end;

{ TRTF }

constructor TRTF.Create(const AStream: TStream; const ReadAll: Boolean);
var
  ByteCount: Integer;
begin
  if ReadAll then
    AStream.Position := 0;
  ByteCount := AStream.Size - AStream.Position;
  SetLength(fData, ByteCount);
  AStream.ReadBuffer(Pointer(fData)^, ByteCount);
end;

constructor TRTF.Create(const ABytes: TBytes);
begin
  fData := Copy(ABytes);
end;

constructor TRTF.Create(const AData: TEncodedData);
resourcestring
  sErrorMsg = 'Encoded data must contain only valid ASCII characters';
var
  DataStr: string;
begin
  if AData.EncodingType = etASCII then
    fData := Copy(AData.Data)
  else
  begin
    DataStr := AData.ToString;
    if not IsValidRTFCode(DataStr) then
      raise ERTF.Create(sErrorMsg);
    fData := TEncoding.ASCII.GetBytes(DataStr);
  end;
end;

constructor TRTF.Create(const ARTFCode: ASCIIString);
begin
  fData := BytesOf(ARTFCode);
end;

constructor TRTF.Create(const AStr: UnicodeString);
resourcestring
  sErrorMsg = 'String "%s" must contain only valid ASCII characters';
begin
  if not IsValidRTFCode(AStr) then
    raise ERTF.CreateFmt(sErrorMsg, [AStr]);
  fData := TEncoding.ASCII.GetBytes(AStr);
end;

function TRTF.IsValidRTFCode(const AStr: UnicodeString): Boolean;
begin
  Result := EncodingSupportsString(AStr, TEncoding.ASCII);
end;

function TRTF.ToBytes: TBytes;
begin
  Result := Copy(fData);
end;

function TRTF.ToRTFCode: ASCIIString;
begin
  Result := BytesToASCIIString(fData);
end;

procedure TRTF.ToStream(const Stream: TStream; const Overwrite: Boolean);
begin
  if Overwrite then
  begin
    Stream.Size := Length(fData);
    Stream.Position := 0;
  end;
  Stream.WriteBuffer(Pointer(fData)^, Length(fData));
end;

function TRTF.ToString: UnicodeString;
begin
  Result := TEncoding.ASCII.GetString(fData);
end;

{ TRichEditHelper }

class procedure TRichEditHelper.Load(const RE: TRichEdit; const RTF: TRTF);
var
  Stream: TStream;
begin
  RE.PlainText := False;
  Stream := TMemoryStream.Create;
  try
    RTF.ToStream(Stream);
    Stream.Position := 0;
    // must set MaxLength or long documents may not display
    RE.MaxLength := Stream.Size;
    RE.Lines.LoadFromStream(Stream, TEncoding.ASCII);
  finally
    Stream.Free;
  end;
end;

end.


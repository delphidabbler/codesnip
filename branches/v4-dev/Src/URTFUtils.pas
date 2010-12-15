{
 * URTFUtils.pas
 *
 * Utility functions used when processing RTF.
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
 * The Original Code is URTFUtils.pas
 *
 * The Initial Developer of the Original Code is Peter Johnson
 * (http://www.delphidabbler.com/).
 *
 * Portions created by the Initial Developer are Copyright (C) 2005-2010 Peter
 * Johnson. All Rights Reserved.
 *
 * Contributor(s)
 *   NONE
 *
 * ***** END LICENSE BLOCK *****
}


unit URTFUtils;


interface


uses
  // Delphi
  SysUtils, Classes, ComCtrls,
  // Project
  UEncodings;


const
  // Constants relating to RTF
  cRTFVersion = 1;          // RTF version


type

  {
  TRTFControl:
    Identifiers for each RTF control word supported by the program.
  }
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
    rcIgnore                // denotes following control can be ignored
  );

type
  ///  <summary>
  ///  Record that encapsulate rich text markup code.
  ///  </summary>
  ///  <remarks>
  ///  Valid rich text markup contains only ASCII characters.
  ///  </remarks>
  TRTF = record
  strict private
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
    constructor Create(const AStream: TStream; const AEncoding: TEncoding;
      const ReadAll: Boolean = False); overload;
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
  ///  <summary>
  ///  Record containing only static methods that assist in working with rich
  ///  edit controls.
  ///  </summary>
  TRichEditHelper = record
  strict private
    ///  <summary>
    ///  Callback routine used to read data from a stream that is being inserted
    ///  into a rich edit control.
    ///  </summary>
    ///  <param name="Stream">TStream [in] Reference to stream being read.
    ///  (Actual param is documented as a DWORD, but we cast to TStream).
    ///  </param>
    ///  <param name="pBuff">Pointer [in] Pointer to buffer to receive data read
    ///  from stream.</param>
    ///  <param name="cb">LongInt [in] Number of bytes requested to be read.
    ///  </param>
    ///  <param name="pcb">PLongInt [in] Pointer to variable to receive number
    ///  of bytes actually read from stream.</param>
    ///  <returns>LongWord - Success / error code. 0 indicates succcess. None
    ///  zero indicates an error condition.</returns>
    ///  <remarks>
    ///  <para>This routine is called by a rich edit control when processing the
    ///  EM_STREAMIN message.</para>
    ///  <para>NOTE: we can use this method is a simple callback function
    ///  because static functions have no additional hidden parameters.</para>
    ///  </remarks>
    class function EditStreamReader(Stream: TStream; pBuff: Pointer;
      cb: LongInt; pcb: PLongInt): LongWord; stdcall; static;
    ///  <summary>
    ///  Inserts a stream of rich text code to current selection position in a
    ///  rich edit control. If text is selected in the control the selection is
    ///  first deleted.
    ///  </summary>
    ///  <param name="RE">TRichEdit [in] Rich edit control in which RTF code is
    ///  to be inserted.</param>
    ///  <param name="Stream">TStream [in] Stream containing valid rich text
    ///  code to be inserted.</param>
    class procedure InsertStream(const RE: TRichEdit; const Stream: TStream);
      static;
  public
    ///  <summary>
    ///  Inserts RTF code into rich edit control at current selection position.
    ///  </summary>
    ///  <param name="RE">TRichEdit [in] Rich edit control.</param>
    ///  <param name="RTF">TRTF [in] Contains rich text code to be inserted.
    ///  </param>
    ///  <remarks>
    ///  Any text selected in RE will be deleted before insertion takes place.
    ///  </remarks>
    class procedure Insert(const RE: TRichEdit; const RTF: TRTF); overload;
      static;
    ///  <summary>
    ///  Inserts RTF code into rich edit control, replacing first occurence of
    ///  some given text.
    ///  </summary>
    ///  <param name="RE">TRichEdit [in] Rich edit control.</param>
    ///  <param name="RTF">TRTF [in] Contains rich text code to be inserted.
    ///  </param>
    ///  <param name="ReplaceText">string [in] Text to be replaced.</param>
    ///  <remarks>
    ///  Only the first occurence of text is replaced. If text is not found then
    ///  no insertion takes place.
    ///  </remarks>
    class procedure Insert(const RE: TRichEdit; const RTF: TRTF;
      const ReplaceText: string); overload; static;
    ///  <summary>
    ///  Loads RTF code into a rich edit control, replacing existing content.
    ///  </summary>
    ///  <param name="RE">TRichEdit [in] Rich edit control.</param>
    ///  <param name="RTF">TRTF [in] Contains rich text code to be loaded.
    ///  </param>
    class procedure Load(const RE: TRichEdit; const RTF: TRTF); static;
    ///  <summary>
    ///  Saves RTF code from a richedit control.
    ///  </summary>
    ///  <param name="RE">TRichEdit [in] Rich edit control.</param>
    ///  <returns>TRTF containing RTF code.</returns>
    class function Save(const RE: TRichEdit): TRTF; static;
  end;


function RTFControl(const Ctrl: TRTFControl): ASCIIString; overload;
  {Creates a parameterless RTF control word.
    @param Ctrl [in] Id of required control word.
    @return RTF control.
  }

function RTFControl(const Ctrl: TRTFControl;
  const Param: SmallInt): ASCIIString; overload;
  {Creates an RTF control word with parameter.
    @param Ctrl [in] Id of required control word.
    @param Param [in] Required parameter.
    @return RTF control.
  }

function RTFEscape(const Ch: AnsiChar): ASCIIString;
  {Creates RTF escape sequence for a character.
    @param Ch [in] Character to escape.
    @return Escape sequence.
  }

function RTFHexEscape(const Ch: AnsiChar): ASCIIString;
  {Creates RTF hexadecimal escape sequence for a character.
    @param Ch [in] Character to escape.
    @return Escape sequence.
  }

function RTFMakeSafeText(const TheText: string; const CodePage: Integer):
  ASCIIString;
  {Encodes text so that any RTF-incompatible characters are replaced with
  suitable control words.
    @param TheText [in] Text to be encoded.
    @param CodePage [in] Code page to use for encoding.
    @return Encoded text.
  }

function RTFUnicodeSafeDestination(const DestCtrl: TRTFControl;
  const DestText: string; const CodePage: Integer): ASCIIString;
  {Creates a destination in a Unicode safe way. If text contains only
  characters supported by the given code page a normal destination is written,
  containing only the given text. If, however, any characters incompatible with
  the code page are in the text two sub-destinations are written, one ANSI only
  and the other containing Unicode characters.
    @param DestCtrl [in] Destination control.
    @param DestText [in] Text of destination.
    @param CodePage [in] Code page to use for encoding.
    @return Destination RTF, with special Unicode sub-destination if needed.
  }


implementation


uses
  // Delphi
  StrUtils, Windows, RichEdit,
  // Project
  UExceptions;


const
  // Map of RTF control ids to control word
  cControls: array[TRTFControl] of ASCIIString = (
    'rtf', 'ansi', 'ansicpg', 'deff', 'deflang', 'fonttbl', 'fprq', 'fcharset',
    'fnil', 'froman', 'fswiss', 'fmodern', 'fscript', 'fdecor', 'ftech',
    'colortbl', 'red', 'green', 'blue', 'info', 'title', 'pard', 'par', 'plain',
    'f', 'cf', 'b', 'i', 'ul', 'fs', 'sb', 'sa', 'u', 'upr', 'ud', '*'
  );

function RTFControl(const Ctrl: TRTFControl): ASCIIString;
  {Creates a parameterless RTF control word.
    @param Ctrl [in] Id of required control word.
    @return RTF control.
  }
begin
  Result := '\' + cControls[Ctrl];
end;

function RTFControl(const Ctrl: TRTFControl;
  const Param: SmallInt): ASCIIString;
  {Creates an RTF control word with parameter.
    @param Ctrl [in] Id of required control word.
    @param Param [in] Required parameter.
    @return RTF control.
  }
begin
  Result := RTFControl(Ctrl) + StringToASCIIString(IntToStr(Param));
end;

function RTFEscape(const Ch: AnsiChar): ASCIIString;
  {Creates RTF escape sequence for a character.
    @param Ch [in] Character to escape.
    @return Escape sequence.
  }
begin
  Result := AnsiChar('\') + Ch;
end;

function RTFHexEscape(const Ch: AnsiChar): ASCIIString;
  {Creates RTF hexadecimal escape sequence for a character.
    @param Ch [in] Character to escape.
    @return Escape sequence.
  }
begin
  Result := StringToASCIIString('\''' + IntToHex(Ord(Ch), 2));
end;

function RTFMakeSafeText(const TheText: string; const CodePage: Integer):
  ASCIIString;
  {Encodes text so that any RTF-incompatible characters are replaced with
  suitable control words.
    @param TheText [in] Text to be encoded.
    @param CodePage [in] Code page to use for encoding.
    @return Encoded text.
  }
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
  {Creates a destination in a Unicode safe way. If text contains only
  characters supported by the given code page a normal destination is written,
  containing only the given text. If, however, any characters incompatible with
  the code page are in the text two sub-destinations are written, one ANSI only
  and the other containing Unicode characters.
    @param DestCtrl [in] Destination control.
    @param DestText [in] Text of destination.
    @param CodePage [in] Code page to use for encoding.
    @return Destination RTF, with special Unicode sub-destination if needed.
  }

  function MakeDestination(const S: string): ASCIIString;
    {Makes a detination for control using given text.
      @param S [in] Text to include in control.
    }
  begin
    Result := '{'
      + RTFControl(DestCtrl) + ' '
      + RTFMakeSafeText(S, CodePage)
      + '}'
  end;

var
  Encoding: TEncoding;
  AnsiStr: string;
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

constructor TRTF.Create(const AStream: TStream; const AEncoding: TEncoding;
  const ReadAll: Boolean);
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

class function TRichEditHelper.EditStreamReader(Stream: TStream; pBuff: Pointer;
  cb: Integer; pcb: PLongInt): LongWord;
begin
  // Assume no error
  Result := $0000;
  try
    // Read required data from stream, recording bytes actually read
    pcb^ := Stream.Read(pBuff^, cb);
  except
    // Indicates error to calling routine
    Result := $FFFF;
  end;
end;

class procedure TRichEditHelper.Insert(const RE: TRichEdit;
  const RTF: TRTF);
var
  Stream: TStream;
begin
  Stream := TMemoryStream.Create;
  try
    RTF.ToStream(Stream);
    Stream.Position := 0;
    InsertStream(RE, Stream);
  finally
    Stream.Free;
  end;
end;

class procedure TRichEditHelper.Insert(const RE: TRichEdit; const RTF: TRTF;
  const ReplaceText: string);
var
  FoundIdx: Integer;  // index of ReplaceText in RE
begin
  FoundIdx := RE.FindText(ReplaceText, 0, MaxInt, []);
  if FoundIdx = -1 then
    Exit;
  RE.SelStart := FoundIdx;
  RE.SelLength := Length(ReplaceText);
  Insert(RE, RTF);
end;

class procedure TRichEditHelper.InsertStream(const RE: TRichEdit;
  const Stream: TStream);
const
  // Flags used in EM_STREAMIN message call
  cFlags = SFF_SELECTION or SF_RTF or SFF_PLAINRTF;
  // Bug error message
  cStreamErrMsg = 'TRTFCtrlHelper.InsertStream: Error inserting stream';
var
  EditStream: TEditStream;  // defines callback used to read inserted RTF
begin
  RE.Lines.BeginUpdate;
  try
    // Make sure rich edit control size is large enough to take inserted code
    RE.MaxLength := RE.MaxLength + Stream.Size;
    // Stream in the RTF via EM_STREAMIN message
    EditStream.dwCookie := DWORD(Stream);
    EditStream.dwError := $0000;
    EditStream.pfnCallback := @EditStreamReader;
    RE.Perform(EM_STREAMIN, cFlags, LPARAM(@EditStream));
    // Report any errors as a bug
    if EditStream.dwError <> $0000 then
      raise EBug.Create(cStreamErrMsg);
  finally
    RE.Lines.EndUpdate;
  end;
end;

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

class function TRichEditHelper.Save(const RE: TRichEdit): TRTF;
var
  Stream: TStream;
begin
  RE.PlainText := False;
  Stream := TMemoryStream.Create;
  try
    RE.Lines.SaveToStream(Stream, TEncoding.ASCII);
    Result := TRTF.Create(Stream, TEncoding.ASCII, True);
  finally
    Stream.Free;
  end;
end;

end.


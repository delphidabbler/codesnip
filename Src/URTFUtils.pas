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
  Classes, ComCtrls,
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
    rcUnicodeChar           // defines a Unicode character as 16bit value
  );


function IsValidRTFCode(const Content: string): Boolean;
  {Checks if document content is valid rich text code.
    @param Content [in] Document content to be checked.
    @return True if valid rich text.
  }

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

function RTFMakeSafeText(const TheText: string): ASCIIString;
  {Encodes text so that any RTF-incompatible characters are replaced with
  suitable control words.
    @param TheText [in] Text to be encoded.
    @return Encoded text.
  }

procedure RTFInsertString(const RE: TRichEdit; const RTF: ASCIIString);
  {Inserts rich text code to current selection position in a rich edit control.
  If text is selected in the control the selection is first deleted.
    @param RE [in] Rich edit control in which RTF code is to be inserted.
    @param RTF [in] String containing valid rich text code to be inserted.
  }

procedure RTFLoadFromStream(const RE: TRichEdit; const Stream: TStream);
  {Loads RTF code into an RTF control, replacing all the control's current
  content.
    @param RE [in] Rich edit control to receive RTF code.
    @param RTF [in] Stream containing valid rich text code.
  }

procedure RTFLoadFromString(const RE: TRichEdit; const RTF: ASCIIString);
  {Loads RTF code into an RTF control, replacing all the control's current
  content.
    @param RE [in] Rich edit control to receive RTF code.
    @param RTF [in] String containing valid rich text code.
  }

procedure RTFSaveToStream(const RE: TRichEdit; const Stream: TStream);
  {Saves RTF code from rich edit control into a stream.
    @param RE [in] Rich edit control whose content is to be saved.
    @param Stream [in] Stream that receives rich text code.
  }


implementation


uses
  // Delphi
  SysUtils, StrUtils, Windows, RichEdit,
  // Project
  UExceptions;


const
  // Map of RTF control ids to control word
  cControls: array[TRTFControl] of ASCIIString = (
    'rtf', 'ansi', 'ansicpg', 'deff', 'deflang', 'fonttbl', 'fprq', 'fcharset',
    'fnil', 'froman', 'fswiss', 'fmodern', 'fscript', 'fdecor', 'ftech',
    'colortbl', 'red', 'green', 'blue', 'info', 'title', 'pard', 'par', 'plain',
    'f', 'cf', 'b', 'i', 'ul', 'fs', 'sb', 'sa', 'u'
  );

function IsValidRTFCode(const Content: string): Boolean;
  {Checks if document content is valid rich text code.
    @param Content [in] Document content to be checked.
    @return True if valid rich text.
  }
begin
  Result := AnsiStartsText('{' + string(RTFControl(rcRTF, 1)), Content);
end;

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

function RTFMakeSafeText(const TheText: string): ASCIIString;
  {Encodes text so that any RTF-incompatible characters are replaced with
  suitable control words.
    @param TheText [in] Text to be encoded.
    @return Encoded text.
  }
var
  Ch: Char; // each character in text
begin
  Result := '';
  for Ch in TheText do
  begin
    if (Ch < #$20) or ((Ch >= #$7F) and (Ch <= #$FF)) then
      Result := Result + RTFHexEscape(AnsiChar(Ch))
    else if (Ch = '{') or (Ch = '\') or (Ch = '}') then
      Result := Result + RTFEscape(AnsiChar(Ch))
    else if Ord(Ch) > $FF then  // Unicode char
      Result := RTFControl(rcUnicodeChar, SmallInt(Ord(Ch))) + '?'
    else
      Result := Result + ASCIIString(AnsiChar(Ch));
  end;
end;

function EditStreamReader(Stream: TStream; pBuff: Pointer;
  cb: LongInt; pcb: PLongInt): DWORD; stdcall;
  {Callback routine used to read data from stream that is being inserted into a
  rich edit control. This routine is called by rich edit control when processing
  EM_STREAMIN message.
    @param Stream [in] Reference to TStream being read. (Actual param is
      supposed to be DWORD, but we cast to TStream).
    @param pBuff [in] Pointer to buffer to receive data read from stream.
    @param cb [in] Number of bytes requested to be read.
    @param pcb [in] Pointer to variable to receive number of bytes actually
      read from stream.
    @return Success / error code. 0 indicates succcess. None-zero indicates an
      error condition.
  }
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

procedure RTFInsertStream(const RE: TRichEdit; const Stream: TStream);
  {Inserts a stream of rich text code to current selection position in a rich
  edit control. If text is selected in the control the selection is first
  deleted.
    @param RE [in] Rich edit control in which RTF code is to be inserted.
    @param Stream [in] Stream containing valid rich text code to be inserted.
  }
const
  // Flags used in EM_STREAMIN message call
  cFlags = SFF_SELECTION or SF_RTF or SFF_PLAINRTF;
  // Bug error message
  cStreamErrMsg = 'RTFInsertStream: Error inserting stream';
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

procedure RTFInsertString(const RE: TRichEdit; const RTF: ASCIIString);
  {Inserts rich text code to current selection position in a rich edit control.
  If text is selected in the control the selection is first deleted.
    @param RE [in] Rich edit control in which RTF code is to be inserted.
    @param RTF [in] String containing valid rich text code to be inserted.
  }
var
  Stm: TStringStream; // stream onto RTF code string
begin
  Stm := TStringStream.Create(AnsiString(RTF));
  try
    RTFInsertStream(RE, Stm);
  finally
    Stm.Free;
  end;
end;

procedure RTFLoadFromStream(const RE: TRichEdit; const Stream: TStream);
  {Loads RTF code into an RTF control, replacing all the control's current
  content.
    @param RE [in] Rich edit control to receive RTF code.
    @param RTF [in] Stream containing valid rich text code.
  }
begin
  RE.PlainText := False;
  RE.MaxLength := Stream.Size; // long docs may not display if MaxLength not set
  RE.Lines.LoadFromStream(Stream);
end;

procedure RTFLoadFromString(const RE: TRichEdit; const RTF: ASCIIString);
  {Loads RTF code into an RTF control, replacing all the control's current
  content.
    @param RE [in] Rich edit control to receive RTF code.
    @param RTF [in] String containing valid rich text code.
  }
var
  Stm: TStringStream; // stream onto RTF code
begin
  // Load stream containing RTF code into control
  Stm := TStringStream.Create(AnsiString(RTF));
  try
    RTFLoadFromStream(RE, Stm);
  finally
    Stm.Free;
  end;
end;

procedure RTFSaveToStream(const RE: TRichEdit; const Stream: TStream);
  {Saves RTF code from rich edit control into a stream.
    @param RE [in] Rich edit control whose content is to be saved.
    @param Stream [in] Stream that receives rich text code.
  }
begin
  RE.Lines.SaveToStream(Stream);
end;

end.


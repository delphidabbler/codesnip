{
 * Hiliter.UPasParser.pas
 *
 * Defines a class that parses Pascal source files and splits into different
 * highlighting elements.
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
 * The Original Code is Hiliter.UPasParser.pas, formerly UHilitePasParser.pas
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


unit Hiliter.UPasParser;


interface


uses
  // Delphi
  Classes,
  // Project
  Hiliter.UGlobals;


type

  THilitePasParser = class;

  {
  TParseElementEvent:
    Type of event raised when a code element has been parsed that needs to be
    highlighted in a particular style. Should be handled by classes that render
    the highlighted text.
      @param Parser [in] Parser object that triggered event.
      @param Element [in] Identifies highlight style the text should be rendered
        in.
      @param ElemText [in] Text to be rendered.
  }
  TParseElementEvent = procedure(Parser: THilitePasParser;
    Element: THiliteElement; const ElemText: string) of object;

  {
  TParseLineEvent:
    Type of event raised at start and end of lines of highlighted code when
    parsing a Pascal file for highlighting.
      @param Parser [in] Parser object that triggered event.
  }
  TParseLineEvent = procedure(Parser: THilitePasParser) of object;

  {
  THilitePasParser:
    Parses pascal files and splits into different highlighting elements.
    Notifies owning objects of each element and other key parsing events.
  }
  THilitePasParser = class(TObject)
  private
    fOnElement: TParseElementEvent;
      {OnElement event handler}
    fOnLineBegin: TParseLineEvent;
      {OnLineBegin event handler}
    fOnLineEnd: TParseLineEvent;
      {OnLineEnd event handler}
  protected
    procedure DoElement(Elem: THiliteElement; const ElemText: string); virtual;
      {Triggers OnElement event.
        @param Elem [in] Element that has been parsed.
        @param ElemText [in] Text of parsed element.
      }
    procedure DoLineBegin; virtual;
      {Triggers OnLineBegin event.
      }
    procedure DoLineEnd; virtual;
      {Triggers OnLineEnd event.
      }
  public
    procedure Parse(const SrcStm: TStream);
      {Parses the Pascal source on a stream, triggering events as each line and
      token is parsed.
        @param SrcStm [in] Stream containing Pascal source.
      }
    property OnElement: TParseElementEvent
      read fOnElement write fOnElement;
      {Event triggered when a highlight element has been parsed. Users should
      render the element in response to this event}
    property OnLineBegin: TParseLineEvent
      read fOnLineBegin write fOnLineBegin;
      {Event triggered just before first element on a new line is parsed. Users
      should emit any output needed to open the new line}
    property OnLineEnd: TParseLineEvent
      read fOnLineEnd write fOnLineEnd;
      {Event triggered at the end of each line of code. Users should close the
      current line in response}
  end;


implementation


uses
  // Delphi
  SysUtils, Windows {for inlining},
  // Project
  Hiliter.UPasLexer;


{ THilitePasParser }

procedure THilitePasParser.DoElement(Elem: THiliteElement;
  const ElemText: string);
  {Triggers OnElement event.
    @param Elem [in] Element that has been parsed.
    @param ElemText [in] Text of parsed element.
  }
begin
  if Assigned(fOnElement) then
    fOnElement(Self, Elem, ElemText);
end;

procedure THilitePasParser.DoLineBegin;
  {Triggers OnLineBegin event.
  }
begin
  if Assigned(fOnLineBegin) then
    fOnLineBegin(Self);
end;

procedure THilitePasParser.DoLineEnd;
  {Triggers OnLineEnd event.
  }
begin
  if Assigned(fOnLineEnd) then
    fOnLineEnd(Self);
end;

procedure THilitePasParser.Parse(const SrcStm: TStream);
  {Parses the Pascal source on a stream, triggering events as each line and
  token is parsed.
    @param SrcStm [in] Stream containing Pascal source.
  }
var
  Lexer: THilitePasLexer;   // object that tokenises Pascal source
  Elem: THiliteElement;     // identifies a parsed highlight element
  InASM: Boolean;           // flag true when parsing assembler code
  BetweenLines: Boolean;    // flag true after EOL before new line starts
begin
  // Create lexical analyser that tokenises source code
  Lexer := THilitePasLexer.Create(SrcStm);
  try
    // Intialise state: not in assembler and line not started
    InASM := False;
    BetweenLines := True;
    // Read all tokens from source using analyser
    while Lexer.NextToken <> tkEOF do
    begin
      // We treat end of line separately from other tokens
      if Lexer.Token <> tkEOL then
      begin
        if InASM then
        begin
          // We are inside assembler code
          // assume assembler style then handle special cases
          Elem := heAssembler;
          case Lexer.Token of
            // we end assembler when an "end" keyword is encountered
            tkKeyword:
            begin
              if AnsiSameText(Lexer.TokenStr, 'end') then
              begin
                InASM := False;
                Elem := heReserved;
              end;
            end;
            // handle tokens that are not formatted as assembler
            // Note: we differ from Delphi editor here in that we don't format
            // strings specially in assembler - they take on attributes of
            // assembler code
            tkWhiteSpace:           Elem := heWhiteSpace;
            tkComment:              Elem := heComment;
            tkCompilerDir:          Elem := hePreProcessor;
            tkError:                Elem := heError;
          end;
        end
        else
        begin
          // Normal case: in standard Pascal code
          case Lexer.Token of
            // Keywords are always reserved. We need to check for "asm" keyword
            // and switch to assembler mode when found. "asm" keyword is
            // rendered as reserved word.
            tkKeyword:
            begin
              Elem := heReserved;
              if AnsiSameText(Lexer.TokenStr, 'asm') then
                InASM := True;
            end;
            // Map other tokens onto highlight elements
            tkDirective:            Elem := heReserved;
            tkComment:              Elem := heComment;
            tkCompilerDir:          Elem := hePreProcessor;
            tkIdentifier:           Elem := heIdentifier;
            tkString,
            tkChar:                 Elem := heString;
            tkNumber:               Elem := heNumber;
            tkFloat:                Elem := heFloat;
            tkHex:                  Elem := heHex;
            tkSymbol:               Elem := heSymbol;
            tkWhitespace:           Elem := heWhitespace;
            else                    Elem := heError;
          end;
        end;
        // Emit the element
        if BetweenLines then
        begin
          // we only start a new line when there's something to write on it
          DoLineBegin;
          BetweenLines := False;
        end;
        DoElement(Elem, Lexer.TokenStr);
      end
      else
      begin
        // End of line
        if BetweenLines then
          // we've never started this line: do it or we may skip blank line
          DoLineBegin;
        DoLineEnd;
        // we are now between lines
        BetweenLines := True;
      end;
    end;
  finally
    Lexer.Free;
  end;
end;

end.


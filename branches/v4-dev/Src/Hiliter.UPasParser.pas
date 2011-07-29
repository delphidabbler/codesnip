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
 * Portions created by the Initial Developer are Copyright (C) 2005-2011 Peter
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
  Generics.Collections,
  // Project
  Hiliter.UGlobals, Hiliter.UPasLexer;


type

  {
  THilitePasParser:
    Parses pascal files and splits into different highlighting elements.
    Notifies owning objects of each element and other key parsing events.
  }
  THilitePasParser = class(TObject)
  strict private
    type
      TParserState = (
        psBetweenLines,
        psInASM,
        psInProperty,
        psAfterProperty,
        psInExports,
        psInExternal
      );
    type
      TParserStates = set of TParserState;
    type
      TContextDirectives = class(TObject)
      strict private
        fMap: TDictionary<string,TParserStates>;
      public
        constructor Create;
        destructor Destroy; override;
        procedure Add(const Dir: string; const SupportedStates: TParserStates);
        function IsReserved(const Dir: string; const States: TParserStates):
          Boolean;
      end;
  public
    type
      {
      TParseElementEvent:
        Type of event raised when a code element has been parsed that needs to
        be highlighted in a particular style. Should be handled by classes that
        render the highlighted text.
          @param Parser [in] Parser object that triggered event.
          @param Element [in] Identifies highlight style the text should be
            rendered in.
          @param ElemText [in] Text to be rendered.
      }
      TParseElementEvent = procedure(Parser: THilitePasParser;
        Element: THiliteElement; const ElemText: string) of object;
    type
      {
      TParseLineEvent:
        Type of event raised at start and end of lines of highlighted code when
        parsing a Pascal file for highlighting.
          @param Parser [in] Parser object that triggered event.
      }
      TParseLineEvent = procedure(Parser: THilitePasParser) of object;
  strict private
    var
      fLexer: THilitePasLexer;   // object that tokenises Pascal source
      fOnElement: TParseElementEvent;
        {OnElement event handler}
      fOnLineBegin: TParseLineEvent;
        {OnLineBegin event handler}
      fOnLineEnd: TParseLineEvent;
        {OnLineEnd event handler}
      fState: TParserStates;
    class var
      fContextDirs: TContextDirectives;
    function ParseASMElement: THiliteElement;
    function ParsePascalElement: THiliteElement;
    function IsTokenStr(const TokenStr: string): Boolean; inline;
    procedure EnsureLineBegun;
    procedure EmitEndOfLine;
    procedure EmitElement(const Elem: THiliteElement);
  strict protected
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
    class constructor Create;
    class destructor Destroy;
    destructor Destroy; override;
    procedure Parse(const Source: string);
      {Parses Pascal source code, triggering events as each line and token is
      parsed.
        @param Source [in] String containing Pascal source.
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
  // Project
  UComparers, UStrUtils;


{ THilitePasParser }

class constructor THilitePasParser.Create;
begin
  fContextDirs := TContextDirectives.Create;
  fContextDirs.Add('read', [psInProperty]);
  fContextDirs.Add('write', [psInProperty]);
  fContextDirs.Add('default', [psInProperty, psAfterProperty]);
  fContextDirs.Add('nodefault', [psInProperty]);
  fContextDirs.Add('stored', [psInProperty]);
  fContextDirs.Add('implements', [psInProperty]);
  fContextDirs.Add('readonly', [psInProperty]);
  fContextDirs.Add('writeonly', [psInProperty]);
  fContextDirs.Add('index', [psInProperty, psInExports, psInExternal]);
  fContextDirs.Add('name', [psInExports, psInExternal]);
  fContextDirs.Add('resident', [psInExports]);
  fContextDirs.Add('delayed', [psInExternal]);
  fContextDirs.Add('local', []); // Kylix directive: never reserved in Delphi
end;

destructor THilitePasParser.Destroy;
begin
  fLexer.Free;
  inherited;
end;

class destructor THilitePasParser.Destroy;
begin
  fContextDirs.Free;
end;

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

procedure THilitePasParser.EmitElement(const Elem: THiliteElement);
begin
  EnsureLineBegun;
  DoElement(Elem, fLexer.TokenStr);
end;

procedure THilitePasParser.EmitEndOfLine;
begin
  // if we never started this line do it or we may skip blank line
  EnsureLineBegun;
  DoLineEnd;
  Include(fState, psBetweenLines);
end;

procedure THilitePasParser.EnsureLineBegun;
begin
  if psBetweenLines in fState then
  begin
    DoLineBegin;
    Exclude(fState, psBetweenLines);
  end;
end;

function THilitePasParser.IsTokenStr(const TokenStr: string): Boolean;
begin
  Result := StrSameText(fLexer.TokenStr, TokenStr);
end;

procedure THilitePasParser.Parse(const Source: string);
  {Parses Pascal source code, triggering events as each line and token is
  parsed.
    @param Source [in] String containing Pascal source.
  }
var
  Elem: THiliteElement;     // identifies a parsed highlight element
begin
  // Create new lexical analyser to tokenise source code
  fLexer.Free;
  fLexer := THilitePasLexer.Create(Source);
  fState := [psBetweenLines];
  while fLexer.NextToken <> tkEOF do
  begin
    // We treat end of line separately from other tokens
    if fLexer.Token <> tkEOL then
    begin
      if psInASM in fState then
        // We are inside assembler code
        Elem := ParseASMElement
      else
        // Normal case: in standard Pascal code
        Elem := ParsePascalElement;
      EmitElement(Elem);
    end
    else
      EmitEndOfLine;
  end;
end;

function THilitePasParser.ParseASMElement: THiliteElement;
begin
  // Assume assembler style then handle special cases
  // Note: we differ from Delphi editor here in that we don't format strings
  // specially in assembler - they take on attributes of assembler code
  Result := heAssembler;
  case fLexer.Token of
    tkKeyword:
    begin
      if IsTokenStr('end') then
      begin
        // "end" keyword ends assembler code block
        Exclude(fState, psInASM);
        Result := heReserved;
      end;
    end;
    // handle tokens that are not formatted as assembler
    tkWhiteSpace:
      Result := heWhiteSpace;
    tkComment:
      Result := heComment;
    tkCompilerDir:
      Result := hePreProcessor;
    tkError:
      Result := heError;
  end;
end;

function THilitePasParser.ParsePascalElement: THiliteElement;
begin
  case fLexer.Token of
    tkKeyword:
    begin
      // Keywords are always reserved. We need to check for "asm" keyword
      // and switch to assembler mode when found. "asm" keyword is
      // rendered as reserved word.
      Result := heReserved;
      if IsTokenStr('asm') then
        Include(fState, psInASM);
      if IsTokenStr('property') then
        Include(fState, psInProperty);
      if IsTokenStr('exports') then
        Include(fState, psInExports);
    end;
    tkDirective:
    begin
      if IsTokenStr('external') then
        Include(fState, psInExternal);
      if fContextDirs.IsReserved(fLexer.TokenStr, fState) then
        Result := heReserved
      else
        Result := heIdentifier;
    end;
    tkComment:
      Result := heComment;
    tkCompilerDir:
      Result := hePreProcessor;
    tkIdentifier:
      Result := heIdentifier;
    tkString,
    tkChar:
      Result := heString;
    tkNumber:
      Result := heNumber;
    tkFloat:
      Result := heFloat;
    tkHex:
      Result := heHex;
    tkSymbol:
    begin
      Result := heSymbol;
      if IsTokenStr(';') then
      begin
        if psInProperty in fState then
        begin
          Exclude(fState, psInProperty);
          Include(fState, psAfterProperty);
        end
        else
          Exclude(fState, psAfterProperty);
        Exclude(fState, psInExports);
        Exclude(fState, psInExternal);
      end;
    end;
    tkWhitespace:
      Result := heWhitespace;
    else
      Result := heError;
  end;
end;

{ THilitePasParser.TContextDirectives }

procedure THilitePasParser.TContextDirectives.Add(const Dir: string;
  const SupportedStates: TParserStates);
begin
  fMap.Add(Dir, SupportedStates);
end;

constructor THilitePasParser.TContextDirectives.Create;
begin
  inherited Create;
  fMap := TDictionary<string,TParserStates>.Create(
    TTextEqualityComparer.Create
  );
end;

destructor THilitePasParser.TContextDirectives.Destroy;
begin
  fMap.Free;
  inherited;
end;

function THilitePasParser.TContextDirectives.IsReserved(const Dir: string;
  const States: TParserStates): Boolean;
begin
  if not fMap.ContainsKey(Dir) then
    Exit(True);
  Result := States * fMap[Dir] <> [];
end;

end.


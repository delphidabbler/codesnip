{
 * This Source Code Form is subject to the terms of the Mozilla Public License,
 * v. 2.0. If a copy of the MPL was not distributed with this file, You can
 * obtain one at https://mozilla.org/MPL/2.0/
 *
 * Copyright (C) 2005-2020, Peter Johnson (gravatar.com/delphidabbler).
 *
 * Defines a class that parses Pascal source files and splits into different
 * highlighting elements.
}


unit Hiliter.UPasParser;


interface


uses
  // Delphi
  Generics.Collections,
  // Project
  Hiliter.UGlobals, Hiliter.UPasLexer;


type

  ///  <summary>
  ///  Parses pascal source code into different highlighting elements.
  ///  </summary>
  ///  <remarks>
  ///  Objects using this class are notified of different elements via events.
  ///  </remarks>
  THilitePasParser = class(TObject)
  strict private
    type
      ///  <summary>Set of possible states of parser.</summary>
      TParserStates = set of (
        psPreamble,       // processing preamble white space / comments
        psInPackage,      // processing a package source file
        psBetweenLines,   // start of a new line is pending
        psInASM,          // processing assembler code
        psInProperty,     // processing a property declaration
        psAfterProperty,  // immediately after end of property declaration
        psInExports,      // processing an exports statement
        psInExternal,     // processing an external directive
        psKeywordEscape   // & encountered, expecting keyword to treat as ident
      );
    type
      ///  <summary>Object that maps directives whose highlighting depends on
      ///  context onto the compiler states where they are treated as reserved
      ///  words.</summary>
      TContextDirectives = class(TObject)
      strict private
        ///  <summary>Maps directive names onto compiler states where they are
        ///  treated as reserved words.</summary>
        fMap: TDictionary<string,TParserStates>;
      public
        ///  <summary>Object constructor.</summary>
        constructor Create;
        ///  <summary>Object destructor.</summary>
        destructor Destroy; override;
        ///  <summary>Adds a given directive to the map along with the compiler
        ///  states where it is to be treated as a reserved word.</summary>
        procedure Add(const Dir: string; const SupportedStates: TParserStates);
        ///  <summary>Checks if a directive is to be treated as a reserved word
        ///  in given compiler state.</summary>
        ///  <remarks>Any directive not included in map is deemed to always be
        ///  reserved.</remarks>
        function IsReserved(const Dir: string; const States: TParserStates):
          Boolean;
      end;
  public
    type
      ///  <summary>Type of event triggered when a code element has been parsed.
      ///  </summary>
      ///  <remarks>Provides information about the element to handler.</remarks>
      ///  <param name="Parser">THilitePasParser [in] Parser object that
      ///  triggered event.</param>
      ///  <param name="Element">THiliteElement [in] Highlight style to be used
      ///  to render ElemText.</param>
      ///  <param name="ElemText">string [in] Text to be rendered.</param>
      TParseElementEvent = procedure(Parser: THilitePasParser;
        Element: THiliteElement; const ElemText: string) of object;
    type
      ///  <summary>Type of event triggered at start and end of lines of code.
      ///  </summary>
      ///  <param name="Parser">THilitePasParser [in] Parser object that
      ///  triggered event.</param>
      TParseLineEvent = procedure(Parser: THilitePasParser) of object;
  strict private
    class var
      ///  <summary>Singleton object used to determine directive highlight
      ///  styles that depend on context.</summary>
      fContextDirs: TContextDirectives;
    var
      ///  <summary>Object that tokenises Pascal source.</summary>
      fLexer: THilitePasLexer;
      ///  <summary>Reference to OnElement event handler.</summary>
      fOnElement: TParseElementEvent;
      ///  <summary>Reference to OnLineBegin event handler.</summary>
      fOnLineBegin: TParseLineEvent;
      ///  <summary>Reference to OnLineEnd event handler.</summary>
      fOnLineEnd: TParseLineEvent;
      ///  <summary>Records current state of parser.</summary>
      fState: TParserStates;
    ///  <summary>Parses a code element within assembly code and returns
    ///  required highlight style.</summary>
    function ParseASMElement: THiliteElement;
    ///  <summary>Parses a code element within normal Pascal code and returns
    ///  required highlight style.</summary>
    function ParsePascalElement: THiliteElement;
    ///  <summary>Checks if given text is that of the current token.</summary>
    function IsTokenStr(const TokenStr: string): Boolean; inline;
    ///  <summary>Ensures that a line of output has been started.</summary>
    ///  <remarks>New lines are not started until they are actually required.
    ///  </remarks>
    procedure EnsureLineBegun;
    ///  <summary>Triggers required events and updates compiler state when an
    ///  end of line is encountered.</summary>
    procedure EmitEndOfLine;
    ///  <summary>Triggers required events and updates compiler state when an
    ///  element is to be written.</summary>
    procedure EmitElement(const Elem: THiliteElement);
  strict protected
    ///  <summary>Triggers OnElement event for given element text and
    ///  highlighting style.</summary>
    procedure DoElement(Elem: THiliteElement; const ElemText: string); virtual;
    ///  <summary>Triggers OnLineBegin event.</summary>
    procedure DoLineBegin; virtual;
    ///  <summary>Triggers OnLineEnd events.</summary>
    procedure DoLineEnd; virtual;
  public
    class constructor Create;
    class destructor Destroy;
    destructor Destroy; override;
    ///  <summary>Parses given Pascal source code, triggering events to inform
    ///  of progress.</summary>
    procedure Parse(const Source: string);
    ///  <summary>Event triggered when a Pascal element has been parsed to
    ///  inform caller of text to be output and highlight style required.
    ///  </summary>
    property OnElement: TParseElementEvent read fOnElement write fOnElement;
    ///  <summary>Event triggered just before first element on a new line is
    ///  parsed.</summary>
    ///  <remarks>Users should emit any output required to open a new line.
    ///  </remarks>
    property OnLineBegin: TParseLineEvent read fOnLineBegin write fOnLineBegin;
    ///  <summary>Event triggered at the end of each line of source code.
    ///  </summary>
    ///  <remarks>Users should emit any output required to end a line.</remarks>
    property OnLineEnd: TParseLineEvent read fOnLineEnd write fOnLineEnd;
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
  fContextDirs.Add('contains', [psInPackage]);
  fContextDirs.Add('requires', [psInPackage]);
  fContextDirs.Add('package', [psInPackage]);
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
begin
  if Assigned(fOnElement) then
    fOnElement(Self, Elem, ElemText);
end;

procedure THilitePasParser.DoLineBegin;
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
var
  Elem: THiliteElement;     // identifies a parsed highlight element
begin
  // Create new lexical analyser to tokenise source code
  fLexer.Free;
  fLexer := THilitePasLexer.Create(Source);
  fState := [psBetweenLines, psPreamble];
  while fLexer.NextToken <> tkEOF do
  begin
    if psPreamble in fState then
    begin
      if not (
        fLexer.Token in
         [tkComment, tkCompilerDir, tkWhitespace, tkEOL, tkError]
      ) then
      Exclude(fState, psPreamble);
      if IsTokenStr('package') then
        Include(fState, psInPackage);
    end;
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
var
  SaveState: TParserStates;
begin
  SaveState := fState;
  case fLexer.Token of
    tkKeyword:
    begin
      if IsTokenStr('asm') then
        Include(fState, psInASM);
      if IsTokenStr('property') then
        Include(fState, psInProperty);
      if IsTokenStr('exports') then
        Include(fState, psInExports);
      if psKeywordEscape in fState then
        Result := heIdentifier
      else
        Result := heReserved;
    end;
    tkDirective:
    begin
      if IsTokenStr('external') then
        Include(fState, psInExternal);
      if psKeywordEscape in fState then
        Result := heIdentifier
      else if fContextDirs.IsReserved(fLexer.TokenStr, fState) then
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
      if IsTokenStr('&') then
        Include(fState, psKeywordEscape);
    end;
    tkWhitespace:
      Result := heWhitespace;
    else
      Result := heError;
  end;
  if not (psKeywordEscape in (fState - SaveState)) then
    // remove keyword escape if it wasn't added in this call
    Exclude(fState, psKeywordEscape);
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


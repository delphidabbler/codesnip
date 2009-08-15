{
 * UHilitePasLexer.pas
 *
 * Defines class that analyses and tokenises Pascal source code.
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
 * The Original Code is UHilitePasLexer.pas
 *
 * The Initial Developer of the Original Code is Peter Johnson
 * (http://www.delphidabbler.com/).
 *
 * Portions created by the Initial Developer are Copyright (C) 2005-2009 Peter
 * Johnson. All Rights Reserved.
 *
 * Contributor(s)
 *   NONE
 *
 * ***** END LICENSE BLOCK *****
}


unit UHilitePasLexer;


interface


uses
  // Delphi
  Classes,
  // Project
  UTextStreamReader;


type

  {
  THilitePasToken:
    Tokens describing the different components of Pascal source code returned by
    the lexical analyser.
  }
  THilitePasToken = (
    tkKeyword,        // Pascal keyword
    tkComment,        // comment including opening and closing symbols
    tkCompilerDir,    // compiler directive including include comment symbols
    tkDirective,      // Pascal directive (Delphi 7)
    tkIdentifier,     // identifier: identifier that is not keyword or directive
    tkString,         // string literal including quotes
    tkChar,           // literal character "#" [Hex | Whole number ]
    tkNumber,         // integral whole number
    tkFloat,          // floating point number (may use 'E' notation)
    tkHex,            // hex digit "$" + { 0..9 | A..F }+
    tkSymbol,         // symbol (single or double character eg '=' and ':=')
    tkWhitespace,     // white space (spaces, tabs etc, excluding CR and LF)
    tkEOL,            // end of line (usually CRLF but CR and LF on own valid)
    tkEOF,            // end of file
    tkError           // error condition: shouldn't occur
  );

  {
  THilitePasLexer:
    Class that analyses and tokenises Pascal code.
  }
  THilitePasLexer = class(TObject)
  strict private
    fTokenStr: string;            // Text of last token read from input
    fToken: THilitePasToken;      // Kind of last token read from input
    fCommentState: record         // Records state of comment being processed
      InComment: Boolean;           // whether currently processing comment
      CommentType: THilitePasToken; // indicates comment or compiler directive
      CommentCloser: string;        // closing comment symbol ( *), } or EOL )
    end;
    fReader: TTextStreamReader;   // Object that reads characters from input
    procedure UpdateTokenStr; overload;
      {Appends current character in input to token string. Ignores EOF.
      }
    procedure UpdateTokenStr(const Ch: AnsiChar); overload;
      {Appends a character to token string. Ignores EOF.
        @param Ch [in] Character to append.
      }
    function ParseChar: THilitePasToken;
      {Analyses a literal character (made from # followed by number) from input
      and stores in token string.
        @return Token indicating literal char (tkChar).
      }
    function ParseCommentFromStart: THilitePasToken;
      {Begins parsing of a new comment or compiler directive.
        @return Token telling whether this is a comment or compiler directive
          (tkComment, tkCompilerDir).
      }
    function ParseCommentInterior: THilitePasToken;
      {Analyses body of comment after start or after resuming processing multi-
      line comments.
        @return Token telling whether this is a comment or compiler directive
          (tkComment, tkCompilerDir).
      }
    function ParseEOL: THilitePasToken;
      {Analyses end of line from input and stores in token string.
        @return End of line token (tkEOL).
      }
    function ParseHex: THilitePasToken;
      {Analyses a hexadecimal integer from input and stores in token string.
        @return Token indicating hexadecimal value (tkHex).
      }
    function ParseIdent: THilitePasToken;
      {Analyses an alphanumeric identifier from input and stores in token
      string. Checks if identifier is keyword or directive.
        @return Token representing identifier: tkKeyword, tkDirective or
          tkIdentifier.
      }
    function ParseNumber: THilitePasToken;
      {Analyses a number from input and stores in token string. Number can be
      integer or real.
        @return Appropriate token for number (tkNumber or tkFloat).
      }
    function ParseString: THilitePasToken;
      {Analyses a string literal from input and stores in token string.
        @return String token (tkString).
      }
    function ParseSymbol: THilitePasToken;
      {Determines whether the current symbol character on input represents a
      symbol or introduces some other syntactic entity (i.e. comment, string,
      character literal or a hex number). Analyses the input accordingly and
      stores the whole token in the token string.
        @return Token describing entity parsed.
      }
    function ParseUnknown: THilitePasToken;
      {Analyses an unrecognised entity from input and adds it to token string.
        @return Error token (tkError).
      }
    function ParseWhiteSpace: THilitePasToken;
      {Analyses a sequence of white space from input and appends space for each
      white space character read to token string.
        @return White space token (tkWhiteSpace).
      }
    function ParseWholeNumber: THilitePasToken;
      {Analyses a whole number from input and appends to token string.
        @return Whole number token (tkNumber).
      }
  public
    constructor Create(const Stm: TStream);
      {Class constructor. Sets up object to analyse code on a stream.
        @param Stm [in] Stream containing Pascal source.
      }
    destructor Destroy; override;
      {Class destructor. Tears down object.
      }
    function NextToken: THilitePasToken;
      {Gets and analyses next pascal token from input and stores details in
      token string.
        @return Token identifier for type of token read.
      }
    property TokenStr: string read fTokenStr;
      {Text that makes up the token last read from input}
    property Token: THilitePasToken read fToken;
      {Kind of token last read from input}
  end;


implementation


uses
  // Delphi
  SysUtils, Windows {for inlining},
  // Project
  UConsts, UExceptions;


const

  // Character constants
  cSpace            = ' ';
  cHexSymbol        = '$';
  cCharSymbol       = '#';
  cDecimalPoint     = '.';
  cCompilerDirChar  = '$';
  cStringDelim      = '''';
  cCloseParen       = ')';

  // Character sets
  cDigits           = ['0'..'9'];                                // valid digits
  cIdentStartChars  = ['A'..'Z', 'a'..'z', '_']; // chars that start identifiers
  cIdentChars       = cIdentStartChars + cDigits;   // interior identifier chars
  cHexDigits        = cDigits + ['A'..'F', 'a'..'f'];        // valid hex digits
  cExponents        = ['E', 'e'];                    // exponents used in floats
  cUnaryPlusMinus   = ['+', '-'];                                 // unary signs
  cWhiteSpace       = [#0..#32] - [CR, LF, #0];     // whitespace read as spaces
  cSingleSyms       = ['#', '$', '&', '''', '(',    // valid single char symbols
                       ')', '*', '+', ',', '-',
                       '.', '/', ':', ';', '<',
                       '=', '>', '@', '[', ']',
                       '^', '{', '}'];
  cDoubleSymOpeners = ['(', '*', '.', '/',     // start chars for double symbols
                       ':', '<', '>'];
  cSeparators       = cWhiteSpace + cSingleSyms    // chars that separate tokens
                      + [TTextStreamReader.EOL];

  // String tables
  cDoubleSyms: array[0..9] of string = (         // list of valid double symbols
    '(*', '(.', '*)', '.)', '..', '//', ':=', '<=', '>=', '<>'
  );
  cCommentOpeners: array[0..2] of string = (       // symbols that open comments
    '{', '(*', '//'
  );
  cCommentClosers: array[0..2] of string = (      // symbols that close comments
    // item at given index matches openers at same index
    '}',
    '*)',
    TTextStreamReader.EOL
  );
  cCompilerDirOpeners: array[0..1] of string = (    // comment symbols that open
    '{', '(*'                                             // compiler directives
  );
  cKeywords: array[0..68] of string = (     // table of keywords per Delphi 2006
    'and', 'array', 'as', 'asm',
    'begin',
    'case', 'class', 'const', 'constructor',
    'destructor', 'dispinterface', 'div', 'do', 'downto',
    'else', 'end', 'except', 'exports',
    'file', 'final', 'finalization', 'finally', 'for', 'function',
    'goto',
    'if', 'implementation', 'in', 'inherited',
    'initialization', 'inline', 'interface', 'is',
    'label', 'library',
    'mod',
    'nil', 'not',
    'object', 'of', 'or', 'out',
    'packed', 'procedure', 'program', 'property',
    'raise', 'record', 'repeat', 'resourcestring',
    'sealed', 'set', 'shl', 'shr', 'static', 'string',
    'then', 'threadvar', 'to', 'try', 'type',
    'unit', 'unsafe', 'until', 'uses',
    'var',
    'while', 'with',
    'xor'
  );

  cDirectives: array[0..45] of string = (                 // table of directives
    'absolute',     // used in variable declaration
    'abstract',     // method directive
    'assembly',     // flags routine as containing assembler
    'at',           // only occurs in raise statement
    'automated',    // used in class declarations
    'cdecl',        // calling convention
    'contains',     // package clause
    'default',      // used in property declarations
    'deprecated',   // portability directive
    'dispid',       // used in automated properties
    'dynamic',      // method directive
    'export',       // calling convention (ignored)
    'external',     // routine directive
    'far',          // calling convention (ignored)
    'forward',      // routine directive
    'implements',   // used in property declarations
    'index',        // used in property declarations and re DLLs
    'inline',       // flags a routine as inlinable
    'local',        // routine directive
    'message',      // method directive
    'name',         // used re DLLs
    'near',         // calling convention (ignored)
    'nodefault',    // used in property declarations
    'on',           // used in exception handlers
    'overload',     // method / routine directive
    'override',     // method directive
    'package',      // introduces a package
    'pascal',       // calling convention
    'platform',     // portability directive
    'private',      // used in class declarations
    'protected',    // used in class declarations
    'public',       // used in class declarations
    'published',    // used in class declarations
    'read',         // used in property declarations
    'readonly',     // property directive in dispinterfaces
    'register',     // calling convention
    'reintroduce',  // method directive
    'requires',     // package clause
    'resident',     // directive used in exports clauses (ignored)
    'safecall',     // calling convention
    'stdcall',      // calling convention
    'stored',       // used in property declarations
    'varargs',      // method / routine directive
    'virtual',      // method directive
    'write',        // used in property declarations
    'writeonly'     // property directive in dispinterfaces
  );

  // Maps symbols onto likely tokens or error if token shouldn't occur (eg
  // close comments).
  cSymToTokenMap: array[0..32] of record
    Symbol: string;           // symbol strings
    Token: THilitePasToken;   // related token
  end = (
    ( Symbol: cHexSymbol;   Token: tkHex;     ),
    ( Symbol: cCharSymbol;  Token: tkChar;    ),
    ( Symbol: '&';          Token: tkSymbol;  ),
    ( Symbol: '''';         Token: tkString;  ),
    ( Symbol: '(';          Token: tkSymbol;  ),
    ( Symbol: ')';          Token: tkSymbol;  ),
    ( Symbol: '*';          Token: tkSymbol;  ),
    ( Symbol: '+';          Token: tkSymbol;  ),
    ( Symbol: ',';          Token: tkSymbol;  ),
    ( Symbol: '-';          Token: tkSymbol;  ),
    ( Symbol: '.';          Token: tkSymbol;  ),
    ( Symbol: '/';          Token: tkSymbol;  ),
    ( Symbol: ':';          Token: tkSymbol;  ),
    ( Symbol: ';';          Token: tkSymbol;  ),
    ( Symbol: '<';          Token: tkSymbol;  ),
    ( Symbol: '=';          Token: tkSymbol;  ),
    ( Symbol: '>';          Token: tkSymbol;  ),
    ( Symbol: '@';          Token: tkSymbol;  ),
    ( Symbol: '[';          Token: tkSymbol;  ),
    ( Symbol: ']';          Token: tkSymbol;  ),
    ( Symbol: '^';          Token: tkSymbol;  ),
    ( Symbol: '{';          Token: tkComment; ),
    ( Symbol: '}';          Token: tkError;   ),
    ( Symbol: '(*';         Token: tkComment; ),
    ( Symbol: '*)';         Token: tkError;   ),
    ( Symbol: '(.';         Token: tkSymbol;  ),
    ( Symbol: '.)';         Token: tkSymbol;  ),
    ( Symbol: '..';         Token: tkSymbol;  ),
    ( Symbol: '//';         Token: tkComment; ),
    ( Symbol: ':=';         Token: tkSymbol;  ),
    ( Symbol: '<=';         Token: tkSymbol;  ),
    ( Symbol: '>=';         Token: tkSymbol;  ),
    ( Symbol: '<>';         Token: tkSymbol;  )
  );


var
  // Private objects used to store and search lists of symbols and keywords
  pvtKeywords: TStringList = nil;   // keywords list
  pvtDirectives: TStringList = nil; // directives list
  pvtDoubleSyms: TStringList = nil; // list of double symbols
  pvtSymMap: TStringList = nil;     // map of symbols to tokens


{ Helper routines }

function IndexInTable(const Str: string; const Table: array of string): Integer;
  {Gets the index of a string in a table.
    @param Str [in] String to search for.
    @param Table [in] Table of strings to search.
    @return Index of string in table or -1 if string not in table.
  }
var
  I: Integer;   // loops thru table
begin
  // Note: calling code assumes Table is zero based
  Result := -1;
  for I := Low(Table) to High(Table) do
    if AnsiSameText(Table[I], Str) then
    begin
      Result := I;
      Break;
    end;
end;

procedure InitStringList(out Strings: TStringList;
  const Table: array of string);
  {Creates and initialises a sorted string list from a table of values.
    @param Strings [out] String list we create and initialise.
    @param Table [in] Table of strings to place in string list.
  }
var
  Idx: Integer; // loops thru rows of table
begin
  Strings := TStringList.Create;
  for Idx := Low(Table) to High(Table) do
    Strings.Add(Table[Idx]);
  Strings.Sorted := True;
  Strings.CaseSensitive := False;
end;

procedure InitSymbolMap(out Strings: TStringList);
  {Initialises string list object used to map valid symbols to tokens.
    @param Strings [out] String list we create and initialises.
  }
var
  I: Integer; // loops thru entries in symbol map constant table.
begin
  Strings := TStringList.Create;
  // Use Objects[] property to store tokens with related string
  for I := Low(cSymToTokenMap) to High(cSymToTokenMap) do
    Strings.AddObject(
      cSymToTokenMap[I].Symbol, TObject(cSymToTokenMap[I].Token)
    );
end;

function IsDoubleSym(const Symbol: string): Boolean;
  {Checks if a symbol is a valid double character symbol.
    @param Symbol [in] Symbol to check.
    @return True if symbol is valid double character symbol.
  }
begin
  if not Assigned(pvtDoubleSyms) then
    InitStringList(pvtDoubleSyms, cDoubleSyms);
  Result := pvtDoubleSyms.IndexOf(Symbol) >= 0;
end;

function IsDirective(const Ident: string): Boolean;
  {Checks if an identifier is a directive.
    @param Ident [in] Identifier to check.
    @return True if Ident is a directive, false otherwise.
  }
begin
  if not Assigned(pvtDirectives) then
    InitStringList(pvtDirectives, cDirectives);
  Result := pvtDirectives.IndexOf(Ident) >= 0;
end;

function IsKeyword(const Ident: string): Boolean;
  {Checks if an identifier is a keyword.
    @param Ident [in] Identifier to check.
    @return True if Ident is a keyword, false otherwise.
  }
begin
  if not Assigned(pvtKeywords) then
    InitStringList(pvtKeywords, cKeywords);
  Result := pvtKeywords.IndexOf(Ident) >= 0;
end;

function SymbolToToken(const Symbol: string): THilitePasToken;
  {Gets the likely token associated with a symbol.
    @param Symbol [in] Symbol to check
    @return Token associated with symbol.
  }
var
  Idx: Integer;   // index of symbol in map table
begin
  // Assumes Str is a symbol (single or double)
  if not Assigned(pvtSymMap) then
    InitSymbolMap(pvtSymMap);
  Idx := pvtSymMap.IndexOf(Symbol);
  if Idx >= 0 then
    Result := THilitePasToken(pvtSymMap.Objects[Idx])
  else
    Result := tkError;
end;

function MatchingCommentCloser(const CommentOpener: string): string;
  {Given a comment opening symbol gets the matching closing comment symbol.
    @param CommentOpener [in] Opening comment we need to match.
    @return Closing comment symbol.
  }
var
  Idx: Integer; // index of opening / closing symbols in table
begin
  // Note: this code assumes cCommentXXX arrays are zero based
  Idx := IndexInTable(CommentOpener, cCommentOpeners);
  Assert(Idx >= 0, 'MatchingCommentCloser: invalid comment opener');
  Result := cCommentClosers[Idx];
end;

function IsCompilerDirOpener(const Str: string): Boolean;
  {Checks if text is a comment opening symbol that is valid for a compiler
  directive.
    @param Str [in] String we are check is a compiler directive opening symbol.
    @return True if is a compiler directive opening symbol.
  }
begin
  Result := IndexInTable(Str, cCompilerDirOpeners) >= 0;
end;


{ THilitePasLexer }

resourcestring
  // Error messages
  sBadHex = 'Bad hex digits';
  sBadCharLiteral = 'Invalid character literal';

constructor THilitePasLexer.Create(const Stm: TStream);
  {Class constructor. Sets up object to analyse code on a stream.
    @param Stm [in] Stream containing Pascal source.
  }
begin
  inherited Create;
  fReader := TTextStreamReader.Create(Stm);
end;

destructor THilitePasLexer.Destroy;
  {Class destructor. Tears down object.
  }
begin
  fReader.Free;
  inherited;
end;

function THilitePasLexer.NextToken: THilitePasToken;
  {Gets and analyses next Pascal token from input and stores details in token
  string.
    @return Token identifiing type of token read.
  }
begin
  // Reset token string
  fTokenStr := '';
  // Decide on method used to parse the token
  if not fCommentState.InComment then
  begin
    // We are not in a multi-line comment: process normally
    if fReader.Ch in cWhiteSpace then
      Result := ParseWhiteSpace
    else if fReader.Ch in cIdentStartChars then
      Result := ParseIdent
    else if fReader.Ch in cSingleSyms then
      Result := ParseSymbol
    else if fReader.Ch in cDigits then
      Result := ParseNumber
    else if fReader.Ch = TTextStreamReader.EOL then
      Result := ParseEOL
    else if fReader.Ch = TTextStreamReader.EOF then
      Result := tkEOF
    else
      Result := ParseUnknown;
  end
  else
  begin
    // We're in a multiline comment: char is either from inside comment or EOL
    if fReader.Ch <> TTextStreamReader.EOL then
      Result := ParseCommentInterior
    else
      Result := ParseEOL;
  end;
  // Record the token
  fToken := Result;
end;

function THilitePasLexer.ParseChar: THilitePasToken;
  {Analyses a literal character (made from # followed by number) from input and
  stores in token string.
    @return Token indicating literal char (tkChar).
  }
begin
  // This method called with token string already containing '#' and current
  // char is char after '#'
  // Numeric part can either by whole number or hex number
  if SymbolToToken(fReader.Ch) = tkHex then
  begin
    // Hex number ('$' detected)
    // store '$' and skip to next
    UpdateTokenStr;
    fReader.NextChar;
    // now read hex digits
    ParseHex;
  end
  else if fReader.Ch in cDigits then
    // This is whole number: parse it
    ParseWholeNumber
  else
    // Unexpected: error
    raise ECodeSnip.Create(sBadCharLiteral);
  Result := tkChar;
end;

function THilitePasLexer.ParseCommentFromStart: THilitePasToken;
  {Begins parsing of a new comment or compiler directive.
    @return Token telling whether this is a comment or compiler directive
      (tkComment, tkCompilerDir).
  }
begin
  // Token string contains comment opening symbol and current char is that which
  // follows opening symbol

  // Record information about the comment
  fCommentState.InComment := True;
  fCommentState.CommentCloser := MatchingCommentCloser(fTokenStr);
  // if char following opener is '$' we have compiler directive
  // (but only if comment opener is '{' or '(*' )
  if (fReader.Ch = cCompilerDirChar) and
    IsCompilerDirOpener(fTokenStr) then
    fCommentState.CommentType := tkCompilerDir
  else
    fCommentState.CommentType := tkComment;

  // Parse body of comment
  Result := ParseCommentInterior;
end;

function THilitePasLexer.ParseCommentInterior: THilitePasToken;
  {Analyses body of comment after start or after resuming processing multi-line
  comments.
    @return Token telling whether this is a comment or compiler directive
      (tkComment, tkCompilerDir).
  }
var
  Done: Boolean;  // flag true when we have finished comment
begin
  Assert(fCommentState.InComment,
    ClassName + '.ParseCommentInterior: called when not in comment');
  Assert(fCommentState.CommentType in [tkComment, tkCompilerDir],
    ClassName + '.ParseCommentInterior: invalid comment type');
  Assert(Length(fCommentState.CommentCloser) > 0,
    ClassName + '.ParseCommentInterior: invalid comment closer');

  Result := fCommentState.CommentType;

  // Loop thru all comment, looking for closing comment symbol
  Done := False;
  while (fReader.Ch <> TTextStreamReader.EOF) and not Done do
  begin
    if fReader.Ch = fCommentState.CommentCloser[1] then
    begin
      // We have encountered 1st char of a comment "closer"
      if Length(fCommentState.CommentCloser) = 1 then
      begin
        // Our closer is a single char: comment is closed
        Done := True;
        fCommentState.InComment := False;
        if fCommentState.CommentCloser = TTextStreamReader.EOL then
          // closer is EOL: put it back to be read later
          fReader.PutBackChar
        else
          // closer not EOL: add it to token string
          UpdateTokenStr;
      end
      else
      begin
        // Our possible closer has two chars
        // Record first char in token string
        UpdateTokenStr;
        // Peek ahead at next char
        fReader.NextChar;
        if fReader.Ch = fCommentState.CommentCloser[2] then
        begin
          // This is the expected closer: comment is closed
          Done := True;
          fCommentState.InComment := False;
          UpdateTokenStr;
        end
        else
          // False alarm: put back the char we peeked at
          fReader.PutBackChar;
      end;
    end
    else
    begin
      // Ordinary comment text
      if fReader.Ch = TTextStreamReader.EOL then
      begin
        // EOL: put it back and stop parsing
        // the comment stays open: we will continue processing after EOL handled
        Done := True;
        fReader.PutBackChar;
      end
      else
        // Not EOL: add char to token string
        UpdateTokenStr;
    end;
    fReader.NextChar;
  end;
  // If at EOF ensure that comment is closed
  if fReader.Ch = TTextStreamReader.EOF then
    fCommentState.InComment := False;
end;

function THilitePasLexer.ParseEOL: THilitePasToken;
  {Analyses end of line from input and stores in token string.
    @return End of line token (tkEOL).
  }
begin
  UpdateTokenStr(TTextStreamReader.EOL);
  Result := tkEOL;
  fReader.NextChar;
end;

function THilitePasLexer.ParseHex: THilitePasToken;
  {Analyses a hexadecimal integer from input and stores in token string.
    @return Token indicating hexadecimal value (tkHex).
  }
begin
  // Called with fTokenStr = '$' and fReader.Ch with char after '$'
  // Build string of hex digits
  while fReader.Ch in cHexDigits do
  begin
    UpdateTokenStr;
    fReader.NextChar;
  end;
  // Check that we ended in a valid way: error if not
  if not (fReader.Ch in cSeparators) then
    raise ECodeSnip.Create(sBadHex);
  Result := tkHex;
end;

function THilitePasLexer.ParseIdent: THilitePasToken;
  {Analyses an alphanumeric identifier from input and stores in token string.
  Checks if identifier is keyword or directive.
    @return Token representing identifier: tkKeyword, tkDirective or
      tkIdentifier.
  }
begin
  Assert(fReader.Ch in cIdentStartChars,
    ClassName + '.ParseIdent: identifier starting character expected');
  // Build identifier in token string
  while fReader.Ch in cIdentChars do
  begin
    UpdateTokenStr;
    fReader.NextChar;
  end;
  // Check if token is keyword or directive or is plain identifier
  if IsKeyword(fTokenStr) then
    Result := tkKeyword
  else if IsDirective(fTokenStr) then
    Result := tkDirective
  else
    Result := tkIdentifier;
end;

function THilitePasLexer.ParseNumber: THilitePasToken;
  {Analyses a number from input and stores in token string. Number can be
  integer or real.
    @return Appropriate token for number (tkNumber or tkFloat).
  }
var
  TempCh: AnsiChar; // temporary storage for a character read from input
begin
  Assert(fReader.Ch in cDigits, ClassName + '.ParseNumber: digit expected');
  // All numbers start with a whole number: read it
  ParseWholeNumber; // leaves current char as one immediately after number
  // Assume we have whole number and see if we can disprove it
  Result := tkNumber;
  if fReader.Ch = cDecimalPoint then
  begin
    // Char after whole number is a decimal point: this *may* indicate a float,
    // but may not since there are other symbols that start with '.'
    // Store the decimal point then read ahead to see what next char is
    TempCh := fReader.Ch;
    fReader.NextChar;
    if fReader.Ch in [cDecimalPoint, cCloseParen] then
    begin
      // decimal point was followed by '.' or ')' making valid two char symbols
      // .. and .) => we put back the read character and get out, leaving first
      // decimal point as current character and returning whole number
      fReader.PutBackChar;
      Exit;
    end;
    // Decimal point was valid: record in token string
    UpdateTokenStr(TempCh);
    // If we have digits after decimal point read them into token str
    // Note: there may not necessarily be digits after '.' (e.g. 2. is a valid
    // Delphi float)
    if fReader.Ch in cDigits then
      ParseWholeNumber;
    Result := tkFloat;
  end;
  if fReader.Ch in cExponents then
  begin
    // Next char is an exponent (e or E) that is present in numbers in
    // "scientific" notation. This can either follow whole number, follow
    // decimal point or follow digits after decimal point. I.e. 2e4, 2.e3 and
    // 2.0e4 are all valid, as is 2.0e-4 etc.
    // Record exponent in token string
    UpdateTokenStr;
    // Read chars after exponent (first may be unary + or -)
    fReader.NextChar;
    if fReader.Ch in cUnaryPlusMinus then
    begin
      UpdateTokenStr;
      fReader.NextChar;
    end;
    // Next comes whole number: get it
    ParseWholeNumber;
    Result := tkFloat;
  end;
end;

function THilitePasLexer.ParseString: THilitePasToken;
  {Analyses a string literal from input and stores in token string.
    @return String token (tkString).
  }
var
  Done: Boolean;  // flag true when done parsing string
begin
  // Note: token string already contains opening quote - current char is first
  // character of the string after the quote
  Done := False;
  // Loop thru characters until end of string found
  while (fReader.Ch <> TTextStreamReader.EOF) and not Done do
  begin
    UpdateTokenStr;
    if fReader.Ch = cStringDelim then
    begin
      // Could be closing quote or pair of quotes used to embed quote in string
      // we need to read ahead to check this
      fReader.NextChar;
      if fReader.Ch = cStringDelim then
        // this is a pair of quotes ('') => embeds quote in string => not done
        UpdateTokenStr
      else
      begin
        // not a pair of quotes => string completed
        Done := True;
        // put back char we read ahead
        fReader.PutBackChar;
      end;
    end;
    fReader.NextChar;
  end;
  Result := tkString;
end;

function THilitePasLexer.ParseSymbol: THilitePasToken;
  {Determines whether the current symbol character on input represents a symbol
  or introduces some other syntactic entity (i.e. comment, string, character
  literal or a hex number). Analyses the input accordingly and stores the whole
  token in the token string.
    @return Token describing entity parsed.
  }
var
  AToken: THilitePasToken; // token represented by the symbol
begin
  Assert(fReader.Ch in cSingleSyms,
    ClassName + '.ParseSymbol: symbol expected');
  // Add character that starts symbol to token string and read next char
  UpdateTokenStr;
  fReader.NextChar;
  // Check if char read is second char of a two char symbol and process if so
  if fReader.Ch in cSingleSyms then
  begin
    if IsDoubleSym(fTokenStr + fReader.Ch) then
    begin
      // this is 2 char symbol: store in token string and skip over
      UpdateTokenStr;
      fReader.NextChar;
    end
  end;
  // Token string now holds symbol: check which kind of token it represents
  // and parse accordingly
  AToken := SymbolToToken(TokenStr);
  case AToken of
    tkComment:
      Result := ParseCommentFromStart;
    tkString:
      Result := ParseString;
    tkChar:
      Result := ParseChar;
    tkHex:
      Result := ParseHex;
    else
      Result := AToken;       // no special processing: return token
  end;
end;

function THilitePasLexer.ParseUnknown: THilitePasToken;
  {Analyses an unrecognised entity from input and adds it to token string.
    @return Error token (tkError).
  }
begin
  Result := tkError;
  UpdateTokenStr;
  fReader.NextChar;
end;

function THilitePasLexer.ParseWhiteSpace: THilitePasToken;
  {Analyses a sequence of white space from input and appends space for each
  white space character read to token string.
    @return White space token (tkWhiteSpace).
  }
begin
  Assert(fReader.Ch in cWhiteSpace,
    ClassName + '.ParseWhiteSpace: current char not white space');
  while fReader.Ch in cWhiteSpace do
  begin
    UpdateTokenStr;
    fReader.NextChar;
  end;
  Result := tkWhiteSpace;
end;

function THilitePasLexer.ParseWholeNumber: THilitePasToken;
  {Analyses a whole number from input and appends to token string.
    @return Whole number token (tkNumber).
  }
begin
  Assert(fReader.Ch in cDigits,
    ClassName + '.ParseWholeNumber: current char not a digit');
  while fReader.Ch in cDigits do
  begin
    UpdateTokenStr;
    fReader.NextChar;
  end;
  Result := tkNumber;
end;

procedure THilitePasLexer.UpdateTokenStr;
  {Appends current character in input to token string. Ignores EOF.
  }
begin
  UpdateTokenStr(fReader.Ch);
end;

procedure THilitePasLexer.UpdateTokenStr(const Ch: AnsiChar);
  {Appends a character to token string. Ignores EOF.
    @param Ch [in] Character to append.
  }
begin
  // We don't store EOF character in token string
  if Ch <> TTextStreamReader.EOF then
    fTokenStr := fTokenStr + Ch;
end;


initialization


finalization

pvtKeywords.Free;
pvtDirectives.Free;
pvtDoubleSyms.Free;
pvtSymMap.Free;

end.


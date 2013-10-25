{
 * This Source Code Form is subject to the terms of the Mozilla Public License,
 * v. 2.0. If a copy of the MPL was not distributed with this file, You can
 * obtain one at http://mozilla.org/MPL/2.0/
 *
 * Copyright (C) 2005-2013, Peter Johnson (www.delphidabbler.com).
 *
 * $Rev$
 * $Date$
 *
 * Defines a class that analyses and tokenises Pascal source code.
}


unit CS.SourceCode.Pascal.Lexer;


interface


uses
  // Delphi
  SysUtils, Classes, Generics.Collections,
  // Project
  UStringReader;


type
  ///  <summary>Tokens describing the different components of Pascal source code
  ///  recognised by THilitePasLexer.</summary>
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
    tkError           // error condition: shouldn't occur in valid Pascal code
  );

type
  ///  <summary>Class that analyses and tokenises Pascal source code.</summary>
  THilitePasLexer = class(TObject)
  strict private
    type
      TEntityMap = class(TObject)
      strict private
        fMap: TDictionary<string,THilitePasToken>;
      public
        constructor Create;
        destructor Destroy; override;
        procedure Add(const Entity: string; const Token: THilitePasToken);
        function Lookup(const Entity: string): THilitePasToken;
      end;
    class var
      fEntityMap: TEntityMap;
    var
      ///  <summary>Text of last token read from input.</summary>
      fTokenStr: string;
      ///  <summary>Identifies last token read from input.</summary>
      fToken: THilitePasToken;
      ///  <summary>Records state of comment being processed.</summary>
      fCommentState: record
        ///  <summary>Informs whether currently processing comment.</summary>
        InComment: Boolean;
        ///  <summary>Indicates whether comment or compiler directive.</summary>
        CommentType: THilitePasToken;
        ///  <summary>Closing comment symbol.</summary>
        ///  <remarks>One of "*)", "}" or EOL.</remarks>
        CommentCloser: string;
      end;
      ///  <summary>Object that reads characters from input.</summary>
      fReader: TStringReader;
    ///  <summary>Appends current character in input to token string.</summary>
    ///  <remarks>Ignores EOF.</remarks>
    procedure UpdateTokenStr; overload;
    ///  <summary>Appends given character to token string.</summary>
    ///  <remarks>Ignores EOF.</remarks>
    procedure UpdateTokenStr(const Ch: Char); overload;
    ///  <summary>Analyses a literal character from input and stores in token
    ///  string.</summary>
    ///  <remarks>A literal character comprises a # followed by a number.
    ///  </remarks>
    ///  <returns>THilitePasToken. Token indicating literal character.</returns>
    function ParseChar: THilitePasToken;
    ///  <summary>Begins parsing of a new comment or compiler directive.
    ///  </summary>
    ///  <returns>THilitePasToken. Token informing whether this is a comment or
    ///  compiler directive.</returns>
    function ParseCommentFromStart: THilitePasToken;
    ///  <summary>Analyses body of comment following start or after resuming
    ///  processing multi-line comments.</summary>
    ///  <returns>THilitePasToken. Token informing whether this is a comment or
    ///  compiler directive.</returns>
    function ParseCommentInterior: THilitePasToken;
    ///  <summary>Analyses end of line from input and stores in token string.
    ///  </summary>
    ///  <returns>THilitePasToken. End of line token.</returns>
    function ParseEOL: THilitePasToken;
    ///  <summary>Analyses a hexadecimal integer from input and stores in token
    ///  string.</summary>
    ///  <returns>THilitePasToken. Token indicating hexadecimal value.</returns>
    function ParseHex: THilitePasToken;
    ///  <summary>Analyses an alphanumeric identifier from input and stores in
    ///  token string.</summary>
    ///  <returns>THilitePasToken. Token indicating if identifier is normal
    ///  identifier, keyword or directive.</returns>
    function ParseIdent: THilitePasToken;
    ///  <summary>Analyses a number from input and stores in token string.
    ///  </summary>
    ///  <remarks>Number can be integer or real.</remarks>
    ///  <returns>THilitePasToken. Token indicating whether an integer or real
    ///  number was parsed.</returns>
    function ParseNumber: THilitePasToken;
    ///  <summary>Analyses a string literal from input and stores in token
    ///  string.</summary>
    ///  <returns>THilitePasToken. Token indicating a string.</returns>
    function ParseString: THilitePasToken;
    ///  <summary>Parses a symbol from input and stores in token string.
    ///  </summary>
    ///  <remarks>Determines whether the current symbol character on input
    ///  represents a symbol or introduces some other syntactic entity (i.e.
    ///  comment, string, character literal or a hex number).</remarks>
    ///  <returns>THilitePasToken. Token describing parsed entity.</returns>
    function ParseSymbol: THilitePasToken;
    ///  <summary>Analyses an unrecognised entity from input and adds it to
    ///  token string.</summary>
    ///  <returns>THilitePasToken. Error token.</returns>
    function ParseUnknown: THilitePasToken;
    ///  <summary>Analyses a sequence of white space from input and appends to
    ///  token string.</summary>
    ///  <returns>THilitePasToken. White space token.</returns>
    function ParseWhiteSpace: THilitePasToken;
    ///  <summary>Analyses a whole number from input and appends to token
    ///  string.</summary>
    ///  <returns>THilitePasToken. Whole number token.</returns>
    function ParseWholeNumber: THilitePasToken;
  public
    class constructor Create;
    class destructor Destroy;
    ///  <summary>Object constructor. Sets up object to analyse given Pascal
    ///  source code string.</summary>
    constructor Create(const Source: string);
    ///  <summary>Object destructor.</summary>
    destructor Destroy; override;
    ///  <summary>Gets and analyses next pascal token from input and stores
    ///  details in token string.</summary>
    ///  <returns>THilitePasToken. Token identifying type of token.</returns>
    function NextToken: THilitePasToken;
    ///  <summary>Text of token last read from input.</summary>
    property TokenStr: string read fTokenStr;
    ///  <summary>Identifies type of last token read from input.</summary>
    property Token: THilitePasToken read fToken;
  end;


implementation


uses
  // Delphi
  Character,
  // Project
  UComparers, UConsts, UStrUtils, UUtils;


const

  // Character constants
  cDecimalPoint     = '.';
  cCompilerDirChar  = '$';
  cStringDelim      = SINGLEQUOTE;
  cCloseParen       = ')';
  cEOL = TStringReader.EOL;
  cEOF = TStringReader.EOF;

  // String tables
  cDoubleSyms: array[0..9] of string = (         // list of valid double symbols
    '(*', '(.', '*)', '.)', '..', '//', ':=', '<=', '>=', '<>'
  );
  cCommentOpeners: array[0..2] of string = (       // symbols that open comments
    '{', '(*', '//'
  );
  cCommentClosers: array[0..2] of string = (      // symbols that close comments
    // item at given index matches openers at same index
    '}', '*)', cEOL
  );
  cCompilerDirOpeners: array[0..1] of string = (    // comment symbols that open
    '{', '(*'                                             // compiler directives
  );

    // Notes about directives and symbols (from Delphi XE documentation)
  // * the words "at" and "or" have special meaning and should be treated as
  //   reserved words.
  // * "private", "protected", "public", "published" and "automated" act as
  //   reserved words within class type definitions but otherwise are treated as
  //   directives.
  // * "near", "far" and "resident" are obsolete.
  // * "inline" is used directive-style, but became a keyword in Turbo Pascal.
  // * "library" is a keyword when used as the 1st token in a project source
  //   file.
  // * "local" was a Kylix directive and is ignored by Delphi.
  // * "package" when used as the first token in a project indicates a package.
  //   Only in packages are "contains" and "requires" directives.

  // Table of keywords per Delphi XE documentation
  cKeywords: array[0..66] of string = (
    'and',            'array',          'as',             'asm',
    'at',             'begin',          'case',           'class',
    'const',          'constructor',    'destructor',     'dispinterface',
    'div',            'do',             'downto',         'else',
    'end',            'except',         'exports',        'file',
    'finalization',   'finally',        'for',            'function',
    'goto',           'if',             'implementation', 'in',
    'inherited',      'initialization', 'inline',         'interface',
    'is',             'label',          'library',        'mod',
    'nil',            'not',            'object',         'of',
    'on',             'or',             'out',            'packed',
    'procedure',      'program',        'property',       'raise',
    'record',         'repeat',         'resourcestring', 'set',
    'shl',            'shr',            'string',         'then',
    'threadvar',      'to',             'try',            'type',
    'unit',           'until',          'uses',           'var',
    'while',          'with',           'xor'
  );

  // Table of directives per Delphi XE documentation
  cDirectives: array[0..53] of string = (
    'absolute',       'abstract',       'assembler',      'automated',
    'cdecl',          'contains',       'default',        'delayed',
    'deprecated',     'dispid',         'dynamic',        'experimental',
    'export',         'external',       'far',            'final',
    'forward',        'helper',         'implements',     'index',
    'local',          'message',        'name',           'near',
    'nodefault',      'operator',       'overload',       'override',
    'package',        'pascal',         'platform',       'private',
    'protected',      'public',         'published',      'read',
    'readonly',       'reference',      'register',       'reintroduce',
    'requires',       'resident',       'safecall',       'sealed',
    'static',         'stdcall',        'stored',         'strict',
    'unsafe',         'varargs',        'virtual',        'winapi',
    'write',          'writeonly'
  );

  // Maps symbols onto likely tokens or error if token shouldn't occur (eg
  // close comments).
  cSymToTokenMap: array[0..32] of record
    Symbol: string;           // symbol strings
    Token: THilitePasToken;   // related token
  end = (
    ( Symbol: '$';  Token: tkHex;     ),
    ( Symbol: '#';  Token: tkChar;    ),
    ( Symbol: '&';  Token: tkSymbol;  ),
    ( Symbol: SINGLEQUOTE; Token: tkString;  ),
    ( Symbol: '(';  Token: tkSymbol;  ),
    ( Symbol: ')';  Token: tkSymbol;  ),
    ( Symbol: '*';  Token: tkSymbol;  ),
    ( Symbol: '+';  Token: tkSymbol;  ),
    ( Symbol: ',';  Token: tkSymbol;  ),
    ( Symbol: '-';  Token: tkSymbol;  ),
    ( Symbol: '.';  Token: tkSymbol;  ),
    ( Symbol: '/';  Token: tkSymbol;  ),
    ( Symbol: ':';  Token: tkSymbol;  ),
    ( Symbol: ';';  Token: tkSymbol;  ),
    ( Symbol: '<';  Token: tkSymbol;  ),
    ( Symbol: '=';  Token: tkSymbol;  ),
    ( Symbol: '>';  Token: tkSymbol;  ),
    ( Symbol: '@';  Token: tkSymbol;  ),
    ( Symbol: '[';  Token: tkSymbol;  ),
    ( Symbol: ']';  Token: tkSymbol;  ),
    ( Symbol: '^';  Token: tkSymbol;  ),
    ( Symbol: '{';  Token: tkComment; ),
    ( Symbol: '}';  Token: tkError;   ),
    ( Symbol: '(*'; Token: tkComment; ),
    ( Symbol: '*)'; Token: tkError;   ),
    ( Symbol: '(.'; Token: tkSymbol;  ),
    ( Symbol: '.)'; Token: tkSymbol;  ),
    ( Symbol: '..'; Token: tkSymbol;  ),
    ( Symbol: '//'; Token: tkComment; ),
    ( Symbol: ':='; Token: tkSymbol;  ),
    ( Symbol: '<='; Token: tkSymbol;  ),
    ( Symbol: '>='; Token: tkSymbol;  ),
    ( Symbol: '<>'; Token: tkSymbol;  )
  );


var
  // Private objects used to store and search lists of symbols and keywords
  pvtDoubleSyms: TStringList = nil; // list of double symbols


{ Helper routines }

///  <summary>Checks if given character is valid for inclusion in the body of a
///  Delphi identifier, after the first character.</summary>
function IsValidIdentBodyChar(const C: Char): Boolean; inline;
begin
  Result := TCharacter.IsLetterOrDigit(C) or (C = '_');
end;

///  <summary>Checks if given character is valid as a first character of a
///  Delphi identifier.</summary>
function IsValidIdentStartChar(const C: Char): Boolean; inline;
begin
  Result := TCharacter.IsLetter(C) or (C = '_');
end;

///  <summary>Checks if given character is a white space character other than
///  EOL or EOF characters.</summary>
function IsWhiteSpaceChar(const C: Char): Boolean; inline;
begin
  Result := TCharacter.IsWhiteSpace(C) and not CharInSet(C, [CR, LF, cEOF]);
end;

///  <summary>Checks if given character is a Delphi symbol.</summary>
function IsSymbolChar(const C: Char): Boolean; inline;
const
  // valid symbols
  cSymbols = [
    '#', '$', '&', SINGLEQUOTE, '(', ')', '*', '+', ',', '-', '.',
    '/', ':', ';', '<', '=', '>', '@', '[', ']', '^', '{', '}'
  ];
begin
  Result := CharInSet(C, cSymbols);
end;

///  <summary>Checks if given character is a valid exponent.</summary>
function IsExponentChar(const C: Char): Boolean; inline;
begin
  Result := CharInSet(C, ['E', 'e']);
end;

///  <summary>Checks if given character is a unary plus or minus operator.
///  </summary>
function IsUnaryPlusMinusChar(const C: Char): Boolean; inline;
begin
  Result := CharInSet(C, ['+', '-']);
end;

///  <summary>Checks if given character is a separator character.</summary>
function IsSeparatorChar(const C: Char): Boolean; inline;
begin
  Result := IsWhiteSpaceChar(C) or IsSymbolChar(C) or (C = cEOL);
end;

///  <summary>Returns index of given string in given table or -1 if string not
///  in table.</summary>
function IndexInTable(const Str: string; const Table: array of string): Integer;
var
  I: Integer;   // loops thru table
begin
  // Note: calling code assumes Table is zero based
  Result := -1;
  for I := Low(Table) to High(Table) do
    if StrSameText(Table[I], Str) then
    begin
      Result := I;
      Break;
    end;
end;

///  <summary>Creates and initialises a sorted string list from a given table of
///  values.</summary>
procedure InitStringList(out Strings: TStringList;
  const Table: array of string);
var
  Idx: Integer; // loops thru rows of table
begin
  Strings := TStringList.Create;
  for Idx := Low(Table) to High(Table) do
    Strings.Add(Table[Idx]);
  Strings.Sorted := True;
  Strings.CaseSensitive := False;
end;

///  <summary>Checks if given symbol is valid double character symbol.</summary>
function IsDoubleSym(const Symbol: string): Boolean;
begin
  if not Assigned(pvtDoubleSyms) then
    InitStringList(pvtDoubleSyms, cDoubleSyms);
  Result := pvtDoubleSyms.IndexOf(Symbol) >= 0;
end;

///  <summary>Returns the closing comment symbol that matches the given opening
///  comment symbol.</summary>
function MatchingCommentCloser(const CommentOpener: string): string;
var
  Idx: Integer; // index of opening / closing symbols in table
begin
  // Note: this code assumes cCommentXXX arrays are zero based
  Idx := IndexInTable(CommentOpener, cCommentOpeners);
  Assert(Idx >= 0, 'MatchingCommentCloser: invalid comment opener');
  Result := cCommentClosers[Idx];
end;

///  <param>Checks if given string is a comment opening symbol that is valid for
///  a compiler directive.</param>
function IsCompilerDirOpener(const Str: string): Boolean;
begin
  Result := IndexInTable(Str, cCompilerDirOpeners) >= 0;
end;

{ THilitePasLexer }

class constructor THilitePasLexer.Create;
var
  Entity: string; // each keyword and directive name
  Idx: Integer;   // loops trhough symbol to token map
begin
  fEntityMap := TEntityMap.Create;
  for Entity in cKeywords do
    fEntityMap.Add(Entity, tkKeyword);
  for Entity in cDirectives do
    fEntityMap.Add(Entity, tkDirective);
  for Idx := Low(cSymToTokenMap) to High(cSymToTokenMap) do
    fEntityMap.Add(cSymToTokenMap[Idx].Symbol, cSymToTokenMap[Idx].Token);
end;

constructor THilitePasLexer.Create(const Source: string);
begin
  inherited Create;
  fReader := TStringReader.Create(Source);
end;

class destructor THilitePasLexer.Destroy;
begin
  fEntityMap.Free;
end;

destructor THilitePasLexer.Destroy;
begin
  fReader.Free;
  inherited;
end;

function THilitePasLexer.NextToken: THilitePasToken;
begin
  // Reset token string
  fTokenStr := '';
  // Decide on method used to parse the token
  if not fCommentState.InComment then
  begin
    // We are not in a multi-line comment: process normally
    if IsWhiteSpaceChar(fReader.Ch) then
      Result := ParseWhiteSpace
    else if IsValidIdentStartChar(fReader.Ch) then
      Result := ParseIdent
    else if IsSymbolChar(fReader.Ch) then
      Result := ParseSymbol
    else if TCharacter.IsDigit(fReader.Ch) then
      Result := ParseNumber
    else if fReader.Ch = cEOL then
      Result := ParseEOL
    else if fReader.Ch = cEOF then
      Result := tkEOF
    else
      Result := ParseUnknown;
  end
  else
  begin
    // We're in a multiline comment: char is either from inside comment or EOL
    if fReader.Ch <> cEOL then
      Result := ParseCommentInterior
    else
      Result := ParseEOL;
  end;
  // Record the token
  fToken := Result;
end;

function THilitePasLexer.ParseChar: THilitePasToken;
begin
  // This method called with token string already containing '#' and current
  // char is char after '#'
  // Numeric part can either by whole number or hex number
  Result := tkChar;
  if fEntityMap.Lookup(fReader.Ch) = tkHex then
  begin
    // Hex number ('$' detected)
    // store '$' and skip to next
    UpdateTokenStr;
    fReader.NextChar;
    // now read hex digits
    ParseHex;
  end
  else if TCharacter.IsDigit(fReader.Ch) then
    // This is whole number: parse it
    ParseWholeNumber
  else
    // Not valid character: error token
    Result := tkError;
end;

function THilitePasLexer.ParseCommentFromStart: THilitePasToken;
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
  while (fReader.Ch <> cEOF) and not Done do
  begin
    if fReader.Ch = fCommentState.CommentCloser[1] then
    begin
      // We have encountered 1st char of a comment "closer"
      if Length(fCommentState.CommentCloser) = 1 then
      begin
        // Our closer is a single char: comment is closed
        Done := True;
        fCommentState.InComment := False;
        if fCommentState.CommentCloser[1] = cEOL then
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
      if fReader.Ch = cEOL then
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
  if fReader.Ch = cEOF then
    fCommentState.InComment := False;
end;

function THilitePasLexer.ParseEOL: THilitePasToken;
begin
  UpdateTokenStr(cEOL);
  Result := tkEOL;
  fReader.NextChar;
end;

function THilitePasLexer.ParseHex: THilitePasToken;
begin
  // Called with fTokenStr = '$' and fReader.Ch with char after '$'
  // Build string of hex digits
  while IsHexDigit(fReader.Ch) do
  begin
    UpdateTokenStr;
    fReader.NextChar;
  end;
  // Check that we ended in a valid way: error if not
  if not IsSeparatorChar(fReader.Ch) then
    Result := tkError
  else
    Result := tkHex;
end;

function THilitePasLexer.ParseIdent: THilitePasToken;
begin
  Assert(IsValidIdentStartChar(fReader.Ch),
    ClassName + '.ParseIdent: identifier starting character expected');
  // Build identifier in token string
  while IsValidIdentBodyChar(fReader.Ch) do
  begin
    UpdateTokenStr;
    fReader.NextChar;
  end;
  // Check if token is keyword or directive or is plain identifier
  Result := fEntityMap.Lookup(fTokenStr);
  if not (Result in [tkKeyword, tkDirective]) then
    Result := tkIdentifier;
end;

function THilitePasLexer.ParseNumber: THilitePasToken;
var
  TempCh: Char; // temporary storage for a character read from input
begin
  Assert(TCharacter.IsDigit(fReader.Ch),
    ClassName + '.ParseNumber: digit expected');
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
    if CharInSet(fReader.Ch, [cDecimalPoint, cCloseParen]) then
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
    if TCharacter.IsDigit(fReader.Ch) then
      ParseWholeNumber;
    Result := tkFloat;
  end;
  if IsExponentChar(fReader.Ch) then
  begin
    // Next char is an exponent (e or E) that is present in numbers in
    // "scientific" notation. This can either follow whole number, follow
    // decimal point or follow digits after decimal point. I.e. 2e4, 2.e3 and
    // 2.0e4 are all valid, as is 2.0e-4 etc.
    // Record exponent in token string
    UpdateTokenStr;
    // Read chars after exponent (first may be unary + or -)
    fReader.NextChar;
    if IsUnaryPlusMinusChar(fReader.Ch) then
    begin
      UpdateTokenStr;
      fReader.NextChar;
    end;
    // Next comes whole number: get it
    if TCharacter.IsDigit(fReader.Ch) then
    begin
      ParseWholeNumber;
      Result := tkFloat
    end
    else
      Result := tkError;
  end;
end;

function THilitePasLexer.ParseString: THilitePasToken;
var
  Done: Boolean;  // flag true when done parsing string
begin
  // Note: token string already contains opening quote - current char is first
  // character of the string after the quote
  Done := False;
  // Loop thru characters until end of string found
  while (fReader.Ch <> cEOF) and not Done do
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
var
  AToken: THilitePasToken; // token represented by the symbol
begin
  Assert(IsSymbolChar(fReader.Ch), ClassName + '.ParseSymbol: symbol expected');
  // Add character that starts symbol to token string and read next char
  UpdateTokenStr;
  fReader.NextChar;
  // Check if char read is second char of a two char symbol and process if so
  if IsSymbolChar(fReader.Ch) then
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
  AToken := fEntityMap.Lookup(TokenStr);
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
begin
  Result := tkError;
  UpdateTokenStr;
  fReader.NextChar;
end;

function THilitePasLexer.ParseWhiteSpace: THilitePasToken;
begin
  Assert(IsWhiteSpaceChar(fReader.Ch),
    ClassName + '.ParseWhiteSpace: current char not white space');
  while IsWhiteSpaceChar(fReader.Ch) do
  begin
    UpdateTokenStr;
    fReader.NextChar;
  end;
  Result := tkWhiteSpace;
end;

function THilitePasLexer.ParseWholeNumber: THilitePasToken;
begin
  Assert(TCharacter.IsDigit(fReader.Ch),
    ClassName + '.ParseWholeNumber: current char not a digit');
  while TCharacter.IsDigit(fReader.Ch) do
  begin
    UpdateTokenStr;
    fReader.NextChar;
  end;
  Result := tkNumber;
end;

procedure THilitePasLexer.UpdateTokenStr;
begin
  UpdateTokenStr(fReader.Ch);
end;

procedure THilitePasLexer.UpdateTokenStr(const Ch: Char);
begin
  if Ch <> cEOF then
    fTokenStr := fTokenStr + Ch;
end;

{ THilitePasLexer.TEntityMap }

constructor THilitePasLexer.TEntityMap.Create;
begin
  inherited Create;
  fMap := TDictionary<string,THilitePasToken>.Create(
    TTextEqualityComparer.Create
  );
end;

destructor THilitePasLexer.TEntityMap.Destroy;
begin
  fMap.Free;
  inherited;
end;

procedure THilitePasLexer.TEntityMap.Add(const Entity: string;
  const Token: THilitePasToken);
begin
  fMap.Add(Entity, Token);
end;

function THilitePasLexer.TEntityMap.Lookup(const Entity: string):
  THilitePasToken;
begin
  if fMap.ContainsKey(Entity) then
    Result := fMap[Entity]
  else
    Result := tkError;
end;

initialization


finalization

pvtDoubleSyms.Free;

end.


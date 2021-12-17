{
 * This Source Code Form is subject to the terms of the Mozilla Public License,
 * v. 2.0. If a copy of the MPL was not distributed with this file, You can
 * obtain one at https://mozilla.org/MPL/2.0/
 *
 * Copyright (C) 2012-2020, Peter Johnson (gravatar.com/delphidabbler).
 *
 * Static class that analyses and provides information about unit source code.
}


unit UUnitAnalyser;


interface


uses
  // Delphi
  SysUtils,
  // Project
  UBaseObjects, UExceptions;


type
  TUnitAnalyser = class sealed(TNoConstructObject)
  public
    class function RequiredEncoding(const SourceCode: string): TEncoding;
    class function UnitName(const SourceCode: string): string;
  end;

type
  EUnitAnalyser = class(ECodeSnip);

implementation


uses
  // Project
  Hiliter.UPasLexer, UEncodings, UStrUtils;

{ TUnitAnalyser }

class function TUnitAnalyser.RequiredEncoding(const SourceCode: string):
  TEncoding;
begin
  if EncodingSupportsString(SourceCode, TEncoding.Default) then
    Result := TEncoding.Default
  else
    Result := TEncoding.UTF8;
end;

class function TUnitAnalyser.UnitName(const SourceCode: string): string;

var
  Lexer: THilitePasLexer; // object used to tokenise Pascal source code

  ///  <summary>Skips over white space and command tokens in source code.
  ///  </summary>
  procedure SkipWhiteSpaceTokens;
  const
    WhiteSpaceTokens = [tkComment, tkCompilerDir, tkWhitespace, tkEOL];
  begin
    while Lexer.NextToken in WhiteSpaceTokens do
      ;
  end;

resourcestring
  // Error messages
  sNotAUnit = 'Source code is not a valid unit';
  sBadName = 'Invalid unit name found in source code';
begin
  Lexer := THilitePasLexer.Create(SourceCode);
  try
    // first Pascal token must be "unit" keyword
    SkipWhiteSpaceTokens;
    if (Lexer.Token <> tkKeyword)
      or not StrSameText(Lexer.TokenStr, 'unit') then
      raise EUnitAnalyser.Create(sNotAUnit);
    // next Pascal token must be unit name identifier
    SkipWhiteSpaceTokens;
    if (Lexer.Token <> tkIdentifier)
      or not IsValidIdent(Lexer.TokenStr, True) then
      raise EUnitAnalyser.Create(sBadName);
    Result := Lexer.TokenStr;
    // we also support dotted unit names: complication is that white space and
    // comments can separate identifiers from dots.
    SkipWhiteSpaceTokens;
    while (Lexer.Token = tkSymbol) and (Lexer.TokenStr = '.') do
    begin
      SkipWhiteSpaceTokens;
      if (Lexer.Token <> tkIdentifier)
        or not IsValidIdent(Lexer.TokenStr, True) then
        raise EUnitAnalyser.Create(sBadName);
      Result := Result + '.' + Lexer.TokenStr;
      SkipWhiteSpaceTokens;
    end;
  finally
    Lexer.Free;
  end;
end;

end.


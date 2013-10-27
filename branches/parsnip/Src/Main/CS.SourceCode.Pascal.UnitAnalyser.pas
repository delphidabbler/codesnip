{
 * This Source Code Form is subject to the terms of the Mozilla Public License,
 * v. 2.0. If a copy of the MPL was not distributed with this file, You can
 * obtain one at http://mozilla.org/MPL/2.0/
 *
 * Copyright (C) 2012-2013, Peter Johnson (www.delphidabbler.com).
 *
 * $Rev$
 * $Date$
 *
 * Provides methods that analyse and provide information about unit source code.
}


unit CS.SourceCode.Pascal.UnitAnalyser;


interface


uses
  // Delphi
  SysUtils,
  // Project
  UExceptions;


type
  TPascalUnitAnalyser = record
  public
    class function RequiredEncoding(const SourceCode: string): TEncoding;
      static;
    class function UnitName(const SourceCode: string): string; static;
  end;

type
  EPascalUnitAnalyser = class(ECodeSnip);

implementation


uses
  // Project
  CS.SourceCode.Pascal.Lexer,
  UEncodings,
  UStrUtils;

{ TPascalUnitAnalyser }

class function TPascalUnitAnalyser.RequiredEncoding(const SourceCode: string):
  TEncoding;
begin
  if EncodingSupportsString(SourceCode, TEncoding.Default) then
    Result := TEncoding.Default
  else
    Result := TEncoding.UTF8;
end;

class function TPascalUnitAnalyser.UnitName(const SourceCode: string): string;

var
  Lexer: TPascalLexer;  // object used to tokenise Pascal source code

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
  Lexer := TPascalLexer.Create(SourceCode);
  try
    // first Pascal token must be "unit" keyword
    SkipWhiteSpaceTokens;
    if (Lexer.Token <> tkKeyword)
      or not StrSameText(Lexer.TokenStr, 'unit') then
      raise EPascalUnitAnalyser.Create(sNotAUnit);
    // next Pascal token must be unit name identifier
    SkipWhiteSpaceTokens;
    if (Lexer.Token <> tkIdentifier)
      or not IsValidIdent(Lexer.TokenStr, True) then
      raise EPascalUnitAnalyser.Create(sBadName);
    Result := Lexer.TokenStr;
    // we also support dotted unit names: complication is that white space and
    // comments can separate identifiers from dots.
    SkipWhiteSpaceTokens;
    while (Lexer.Token = tkSymbol) and (Lexer.TokenStr = '.') do
    begin
      SkipWhiteSpaceTokens;
      if (Lexer.Token <> tkIdentifier)
        or not IsValidIdent(Lexer.TokenStr, True) then
        raise EPascalUnitAnalyser.Create(sBadName);
      Result := Result + '.' + Lexer.TokenStr;
      SkipWhiteSpaceTokens;
    end;
  finally
    Lexer.Free;
  end;
end;

end.


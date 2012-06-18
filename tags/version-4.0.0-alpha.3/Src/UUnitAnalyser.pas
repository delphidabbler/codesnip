{
 * UUnitAnalyser.pas
 *
 * Static class that analyses and provides information about unit source code.
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
 * The Original Code is UUnitAnalyser.pas
 *
 * The Initial Developer of the Original Code is Peter Johnson
 * (http://www.delphidabbler.com/).
 *
 * Portions created by the Initial Developer are Copyright (C) 2012 Peter
 * Johnson. All Rights Reserved.
 *
 * Contributor(s)
 *   NONE
 *
 * ***** END LICENSE BLOCK *****
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


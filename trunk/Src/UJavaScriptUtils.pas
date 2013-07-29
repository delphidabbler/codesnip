{
 * This Source Code Form is subject to the terms of the Mozilla Public License,
 * v. 2.0. If a copy of the MPL was not distributed with this file, You can
 * obtain one at http://mozilla.org/MPL/2.0/
 *
 * Copyright (C) 2006-2013, Peter Johnson (www.delphidabbler.com).
 *
 * $Rev$
 * $Date$
 *
 * Utilities that assist when working with JavaScript.
}


unit UJavaScriptUtils;


interface


type
  ///  <summary>Container for methods that assist in generating JavaScript code.
  ///  </summary>
  TJavaScript = record
  strict private
    ///  <summary>Converts the given string into a valid JavaScript string
    ///  literal, escaping characters as necessary.</summary>
    ///  <remarks>Both single quotes and double quotes are escaped, enabling the
    ///  string literal to be enclosed in either type of quote.</remarks>
    class function MakeSafeString(const S: string): string; static;
    ///  <summary>Converts the given integer into a literal numeric parameter
    ///  suitable for passing to a JavaScript function.</summary>
    class function LiteralParam(const I: Integer): string; overload; static;
    ///  <summary>Converts the given string into a literal string parameter
    ///  suitable for passing to a JavaScript function.</summary>
    ///  <remarks>The string is quoted and characters are escaped as necessary
    ///  to make the result a valid JavaScript string.</remarks>
    class function LiteralParam(const S: string): string; overload; static;
    ///  <summary>Converts the given floating point value into a literal numeric
    ///  parameter suitable for passing to a JavaScript function.</summary>
    class function LiteralParam(const F: Extended): string; overload; static;
    ///  <summary>Converts the given boolean value into either "true" or "false"
    ///  keyword suitable for passing to a JavaScript function.</summary>
    class function LiteralParam(const B: Boolean): string; overload; static;
  public
    ///  <summary>Creates and returns a JavaScript function call with a
    ///  parameter list comprising of literal values.</summary>
    ///  <param name="FnName">string [in] Name of function.</param>
    ///  <param name="Params">array of const [in] Dynamic array of literal
    ///  parameter values to be passed to the function. [] indicates a
    ///  parameterless function.</param>
    ///  <returns>string. Required JavaScript function.</returns>
    ///  <exception>EBug is raised if type of any value in Params has an
    ///  unsupported type. Valid types are Integer, Boolean, Extended,
    ///  UnicodeString and WideChar.</exception>
    class function LiteralFunc(const FnName: string;
      const Params: array of const): string; static;
  end;


implementation


uses
  // Delphi
  SysUtils, Classes,
  // Project
  UConsts, UExceptions, UStrUtils;


///  <summary>Replaces specified characters in a string with escape characters
///  in C format.</summary>
///  <param name="S">string [in] String to be escaped.</param>
///  <param name="EscapeChars">string [in] Escape characters to replace
///  characters from EscapableChars.</param>
///  <param name="EscapableChars">string [in] Characters to be escaped.</param>
///  <returns>string. String with all relevant characters escaped.</returns>
function CEscapeStr(const S: string; const EscapeChars,
  EscapableChars: string): string;
const
  cEscChar = '\';       // the C escape character
var
  EscCount: Integer;    // count of escaped characters in string
  Idx: Integer;         // loops thru string
  PRes: PChar;          // points to chars in result string
  EscCharPos: Integer;  // position of esc chars in EscapeChars & EscapableChars
begin
  // Check for empty string and treat specially (empty string crashes main code)
  if S = '' then
  begin
    Result := '';
    Exit;
  end;
  // Count escapable characters in string
  EscCount := 0;
  for Idx := 1 to Length(S) do
  begin
    if StrContainsStr(S[Idx], EscapableChars) then
      Inc(EscCount);
  end;
  // Set size of result string and get pointer to it
  SetLength(Result, Length(S) + EscCount);
  PRes := PChar(Result);
  // Replace escapable chars with the escaped version
  for Idx := 1 to Length(S) do
  begin
    EscCharPos := StrPos(S[Idx], EscapableChars);
    if EscCharPos > 0 then
    begin
      PRes^ := cEscChar;
      Inc(PRes);
      PRes^ := EscapeChars[EscCharPos];
    end
    else
      PRes^ := S[Idx];
    Inc(PRes);
  end;
  // copy last character (not processed in loop)
  PRes^ := S[Length(S)];
end;

{ TJavaScript }

class function TJavaScript.LiteralFunc(const FnName: string;
  const Params: array of const): string;
var
  Idx: Integer;           // loops thru all provided parameters
  ParamVar: TVarRec;      // value of a parameter from array
  ParamList: TStringList; // list of literal parameters for passing to function
  Param: string;          // a parameter in suitable form to pass to function
begin
  // We store literal parameters in a string
  ParamList := TStringList.Create;
  try
    for Idx := Low(Params) to High(Params) do
    begin
      // Handle each parameter according to type
      ParamVar := TVarRec(Params[Idx]);
      case ParamVar.VType of
        vtBoolean:
          Param := LiteralParam(ParamVar.VBoolean);
        vtInteger:
          Param := LiteralParam(ParamVar.VInteger);
        vtExtended:
          Param := LiteralParam(ParamVar.VExtended^);
        vtUnicodeString:
          Param := LiteralParam(PWideChar(ParamVar.VUnicodeString));
        vtWideChar:
          Param := LiteralParam(ParamVar.VWideChar);
        else
          raise EBug.Create(
            'TJavaScript.LiteralFunc: Unsupported parameter type'
          );
      end;
      // Store param in list
      ParamList.Add(Param);
    end;
    // Build function name and parameter list
    Result := FnName + '(' + StrJoin(ParamList, ', ') + ')';
  finally
    FreeAndNil(ParamList);
  end;
end;

class function TJavaScript.LiteralParam(const I: Integer): string;
begin
  Result := IntToStr(I);
end;

class function TJavaScript.LiteralParam(const S: string): string;
begin
  Result := DOUBLEQUOTE + MakeSafeString(S) + DOUBLEQUOTE;
end;

class function TJavaScript.LiteralParam(const B: Boolean): string;
begin
  if B then
    Result := 'true'
  else
    Result := 'false';
end;

class function TJavaScript.MakeSafeString(const S: string): string;
const
  EscapableChars = DOUBLEQUOTE + SINGLEQUOTE + '\' + LF + CR + TAB + FF
    + BACKSPACE;  // characters to be escaped
  EscapeChars = DOUBLEQUOTE + SINGLEQUOTE + '\nrtfb'; // escape characters
begin
  Result := CEscapeStr(
    StrUnixLineBreaks(S),   // convert CRLF to LF
    EscapeChars,
    EscapableChars
  );
end;

class function TJavaScript.LiteralParam(const F: Extended): string;
begin
  Result := FloatToStr(F);
end;

end.


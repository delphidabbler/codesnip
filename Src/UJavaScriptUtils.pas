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
  TJavaScript = record
  strict private
    class function LiteralParam(const I: Integer): string; overload; static;
      {Converts an integer into a literal numeric parameter suitable for passing to
      a JavaScript function.
        @param I [in] Value of parameter.
        @return Integer value as a string.
      }
    class function LiteralParam(const S: string): string; overload; static;
      {Converts a string into a literal string parameter suitable for passing to a
      JavaScript function.
        @param S [in] Value of parameter.
        @return Quoted string with embedded quotes and other control characters
          escaped.
      }
    class function LiteralParam(const F: Extended): string; overload; static;
      {Converts a floating point value into a literal numeric parameter suitable for
      passing to a JavaScript function.
        @param F [in] Value of parameter.
        @return Floating point value as string.
      }
    class function LiteralParam(const B: Boolean): string; overload; static;
      {Converts a Boolean value into a literal boolean parameter suitable for
      passing to a JavaScript function.
        @param B [in] Value of parameter.
        @return 'true' or 'false'.
      }
  public
    class function LiteralFunc(const FnName: string;
      const Params: array of const): string; static;
      {Creates a JavaScript function call with a parameter list comprising of
      literal values.
        @param FnName [in] Name of function.
        @param Params [in] Dynamic array of literal parameter values. [] indicates a
          parameterless function.
        @except EBug raised if type of any value in Params has an unsupported type.
          Valid types are Integer, Boolean, Extended and either AnsiString and
          PAnsiChar or UnicodeString and WideChar, depending on if compiled with
          Unicode support.
      }
  end;


implementation


uses
  // Delphi
  SysUtils, Classes,
  // Project
  UConsts, UExceptions, UStrUtils;


function CEscapeStr(const S: string; const EscapeChars,
  EscapableChars: string): string;
  {Replaces specified characters in a string with escape characters in C format.
    @param S [in] String to be escaped.
    @param EscapeChars [in] Escape characters to replace characters from
      EscapableChars.
    @param EscapableChars [in] Characters to be escaped.
    @return String with all relevant characters escaped.
  }
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
  {Creates a JavaScript function call with a parameter list comprising of
  literal values.
    @param FnName [in] Name of function.
    @param Params [in] Dynamic array of literal parameter values. [] indicates a
      parameterless function.
    @except EBug raised if type of any value in Params has an unsupported type.
      Valid types are Integer, Boolean, Extended and either AnsiString and
      PAnsiChar or UnicodeString and WideChar, depending on if compiled with
      Unicode support.
  }
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
          raise EBug.Create('JSLiteralFunc: Unsupported parameter type');
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
  {Converts an integer into a literal numeric parameter suitable for passing to
  a JavaScript function.
    @param I [in] Value of parameter.
    @return Integer value as a string.
  }
begin
  // Integer parameters are simply the number itself
  Result := IntToStr(I);
end;

class function TJavaScript.LiteralParam(const S: string): string;
  {Converts a string into a literal string parameter suitable for passing to a
  JavaScript function.
    @param S [in] Value of parameter.
    @return Quoted string with embedded quotes and other control characters
      escaped.
  }
const
  cQuote = '"';                               // quote to delimit string literal
  cEscapableChars = cQuote + '\' + LF + CR + TAB;    // characters to be escaped
  cEscapeChars = cQuote + '\nrt';                           // escape characters
begin
  Result :=
    cQuote +
    CEscapeStr(
      StrUnixLineBreaks(S),   // convert CRLF to LF
      cEscapeChars,
      cEscapableChars
    ) +
    cQuote;
end;

class function TJavaScript.LiteralParam(const B: Boolean): string;
  {Converts a Boolean value into a literal boolean parameter suitable for
  passing to a JavaScript function.
    @param B [in] Value of parameter.
    @return 'true' or 'false'.
  }
begin
  if B then
    Result := 'true'
  else
    Result := 'false';
end;

class function TJavaScript.LiteralParam(const F: Extended): string;
  {Converts a floating point value into a literal numeric parameter suitable for
  passing to a JavaScript function.
    @param F [in] Value of parameter.
    @return Floating point value as string.
  }
begin
  Result := FloatToStr(F);
end;

end.


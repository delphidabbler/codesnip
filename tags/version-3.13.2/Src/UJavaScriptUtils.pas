{
 * UJavaScriptUtils.pas
 *
 * Utilities that assist when working with JavaScript.
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
 * The Original Code is UJavaScriptUtils.pas
 *
 * The Initial Developer of the Original Code is Peter Johnson
 * (http://www.delphidabbler.com/).
 *
 * Portions created by the Initial Developer are Copyright (C) 2006-2010 Peter
 * Johnson. All Rights Reserved.
 *
 * Contributor(s)
 *   NONE
 *
 * ***** END LICENSE BLOCK *****
}


unit UJavaScriptUtils;


interface


function JSLiteralFunc(const FnName: string;
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


implementation


uses
  // Delphi
  SysUtils, Classes,
  // Project
  UConsts, UExceptions, UHTMLDocHelper, UUtils;


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
    if SysUtils.AnsiPos(S[Idx], EscapableChars) > 0 then
      Inc(EscCount);
  end;
  // Set size of result string and get pointer to it
  SetLength(Result, Length(S) + EscCount);
  PRes := PChar(Result);
  // Replace escapable chars with the escaped version
  for Idx := 1 to Length(S) do
  begin
    EscCharPos := SysUtils.AnsiPos(S[Idx], EscapableChars);
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

function LiteralParam(const I: Integer): string; overload;
  {Converts an integer into a literal numeric parameter suitable for passing to
  a JavaScript function.
    @param I [in] Value of parameter.
    @return Integer value as a string.
  }
begin
  // Integer parameters are simply the number itself
  Result := IntToStr(I);
end;

function LiteralParam(const S: string): string; overload;
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
      UUtils.UnixLineBreaks(S),   // convert CRLF to LF
      cEscapeChars,
      cEscapableChars
    ) +
    cQuote;
end;

function LiteralParam(const F: Extended): string; overload;
  {Converts a floating point value into a literal numeric parameter suitable for
  passing to a JavaScript function.
    @param F [in] Value of parameter.
    @return Floating point value as string.
  }
begin
  Result := FloatToStr(F);
end;

function LiteralParam(const B: Boolean): string; overload;
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

function JSLiteralFunc(const FnName: string;
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
    Result := FnName + '(' + JoinStr(ParamList, ', ') + ')';
  finally
    FreeAndNil(ParamList);
  end;
end;

end.


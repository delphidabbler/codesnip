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


uses
  // Project
  UEncodings;


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
    ///  <summary>Loads JavaScript code for the script named in HTML resources
    ///  and returns the script as string.</summary>
    ///  <param name="ScriptName">string [in] Name of resource containing
    ///  script.</param>
    ///  <param name="EncType">TEncodingType [in] Denotes type of encoding used
    ///  for requested script within resources.</param>
    ///  <returns>string. Required JavaScript code.</returns>
    ///  <remarks>We sometimes need to load scripts into strings and then embed
    ///  in HTML document since linking to external resource script doesn't seem
    ///  to work in IE 9 (see bug report
    ///  https://sourceforge.net/p/codesnip/bugs/84/).</remarks>
    class function LoadScript(const ScriptName: string;
      const EncType: TEncodingType): string; static;
  end;


implementation


uses
  // Delphi
  SysUtils, Classes,
  // Project
  UConsts, UExceptions, UResourceUtils, UStrUtils;


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
    ParamList.Free;
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

class function TJavaScript.LiteralParam(const F: Extended): string;
begin
  Result := FloatToStr(F);
end;

class function TJavaScript.LoadScript(const ScriptName: string;
  const EncType: TEncodingType): string;
begin
  Result := LoadResourceAsString(HInstance, ScriptName, RT_HTML, EncType);
end;

class function TJavaScript.MakeSafeString(const S: string): string;
const
  EscapableChars = DOUBLEQUOTE + SINGLEQUOTE + '\' + LF + CR + TAB + FF
    + BACKSPACE;  // characters to be escaped
  EscapeChars = DOUBLEQUOTE + SINGLEQUOTE + '\nrtfb'; // escape characters
begin
  Result := StrBackslashEscape(
    StrUnixLineBreaks(S), EscapableChars, EscapeChars
  );
end;

end.


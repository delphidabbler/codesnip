{
 * UTestUnit.pas
 *
 * Implements a class that generates Pascal units for use in test compiling
 * snippets.
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
 * The Original Code is UTestUnit.pas
 *
 * The Initial Developer of the Original Code is Peter Johnson
 * (http://www.delphidabbler.com/).
 *
 * Portions created by the Initial Developer are Copyright (C) 2006-2011 Peter
 * Johnson. All Rights Reserved.
 *
 * Contributor(s)
 *   NONE
 *
 * ***** END LICENSE BLOCK *****
}


unit UTestUnit;


interface


uses
  // Project
  DB.USnippet;


type

  ///  <summary>
  ///  Class that generates Pascal units for use in test compiling snippets.
  ///  </summary>
  TTestUnit = class(TObject)
  strict private
    var
      ///  <summary>Reference to snippet for which test unit is required.
      ///  </summary>
      fSnippet: TSnippet;
    ///  <summary>Generates name of test unit, based on snippet being tested.
    ///  </summary>
    ///  <remarks>Returned unit name contains only valid ASCII characters. Any
    ///  invalid characters are replaced with '_'.</remarks>
    function UnitName: string;
    ///  <summary>Builds and returns fully specified unit file name.</summary>
    function UnitFileName: string;
  public
    ///  <summary>Sets up object to create test unit for given snippet.
    ///  </summary>
    constructor Create(const Snippet: TSnippet);
    ///  <summary>Generates source code of test unit.</summary>
    function GenerateUnitSource: string;
    ///  <summary>Saves generated source to file.</summary>
    ///  <param name="FileName">string [out] Set to name of unit file. Base name
    ///  is name of unit.</param>
    ///  <remarks>
    ///  <para>Base file name contains only valid ASCII characters.</para>
    ///  <para>File is saved in default ANSI encoding unless source code
    ///  contains any characters not valid in default ANSI code page. In this
    ///  case file is saved with UTF-8 encoding.</para>
    ///  </remarks>
    procedure SaveUnit(out FileName: string);
  end;


implementation


uses
  // Delphi
  SysUtils,
  // Project
  UEncodings, UIOUtils, USourceGen, USystemInfo, UUtils;


{ TTestUnit }

constructor TTestUnit.Create(const Snippet: TSnippet);
begin
  Assert(Assigned(Snippet), ClassName + '.Create: Snippet is nil');
  inherited Create;
  fSnippet := Snippet;
end;

function TTestUnit.GenerateUnitSource: string;
begin
  with TSourceGen.Create do
    try
      IncludeSnippet(fSnippet);
      // Must use Self.UnitName below for Delphis that defined TObject.UnitName
      // otherwise the TObject version is used.
      Result := UnitAsString(Self.UnitName);
    finally
      Free;
    end;
end;

procedure TTestUnit.SaveUnit(out FileName: string);
var
  SourceCode: string;
  Encoding: TEncoding;
begin
  FileName := UnitFileName;
  SourceCode := GenerateUnitSource;
  // If all of the source code is supported by default ANSI code page we use
  // that to write file, otherwise we use UTF-8.
  // Preference for default ANSI encoding is because early Delphis can only read
  // source code files in this format. Later versions that can handle unicode
  // characters in units also support UTF-8 format source code files.
  if EncodingSupportsString(SourceCode, TEncoding.Default) then
    Encoding := TEncoding.Default
  else
    Encoding := TEncoding.UTF8;
  TFileIO.WriteAllText(FileName, SourceCode, Encoding, True);
end;

function TTestUnit.UnitFileName: string;
const
  cPasExt = '.pas'; // file extension for Pascal unit:
begin
  // Unit file name is in temp folder
  Result := IncludeTrailingPathDelimiter(TSystemFolders.Temp)
    + UnitName + cPasExt;
end;

function TTestUnit.UnitName: string;
const
  cUnitPrefix = 'U_'; // unit file name prefix
var
  I: Integer;
  Ch: Char;
begin
  // Unit name is same as Snippet being tested, but with prefix to make unique
  Result := cUnitPrefix + fSnippet.Name;
  // We ensure only ASCII characters are used in unit name. Any unsuitable
  // characters are replaced by underscore.
  // This is done because unit name is also used as unit file name. If we took
  // no action would could have a Unicode file name. Earlier versions of Delphi
  // can't cope with Unicode file names and claim the file can't be found. Some
  // compilers like Delphi 2006 that can handle non-ANSI characters in source
  // code still cannot handle those characters in the file name.
  // Some valid ANSI characters are not handled by some compilers, hence we have
  // fallen back to ASCII.
  for I := 1 to Length(Result) do
  begin
    Ch := Result[I];
    if not EncodingSupportsString(Ch, TEncoding.ASCII) then
      Result[I] := '_';
  end;
end;

end.


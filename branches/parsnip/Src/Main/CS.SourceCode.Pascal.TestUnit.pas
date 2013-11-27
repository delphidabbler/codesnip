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
 * Implements a class that generates Pascal units for use when test compiling
 * snippets.
}


unit CS.SourceCode.Pascal.TestUnit;


interface


uses
  // Delphi
  SysUtils,
  // Project
  DB.USnippet;


type

  ///  <summary>Class that generates Pascal units for use when test compiling
  ///  snippets.</summary>
  TPascalTestUnit = class(TObject)
  strict private
    const
      ///  <summary>Name used for all test units.</summary>
      TestUnitName = 'CodeSnipTestUnit';
    var
      ///  <summary>Reference to snippet for which test unit is required.
      ///  </summary>
      fSnippet: TSnippet;
    ///  <summary>Builds and returns fully specified unit file name.</summary>
    function UnitFileName: string;
    ///  <summary>Returns the type of encoding needed to store the given unit
    ///  source code in a file.</summary>
    ///  <remarks>The encoding will be the default ANSI encoding unless the
    ///  source code contains characters outside the ANSI code page, when the
    ///  UTF-8 encoding is returned.</remarks>
    function RequiredFileEncoding(const SourceCode: string): TEncoding;
  public
    ///  <summary>Sets up object to create Pascal test unit for given snippet.
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
  // Project
  CS.Database.Types,
  CS.SourceCode.Pascal.SourceGen,
  UEncodings,
  UIOUtils,
  USystemInfo,
  UUtils;


{ TPascalTestUnit }

constructor TPascalTestUnit.Create(const Snippet: TSnippet);
begin
  Assert(Assigned(Snippet), ClassName + '.Create: Snippet is nil');
  inherited Create;
  fSnippet := Snippet;
end;

function TPascalTestUnit.GenerateUnitSource: string;
begin
  if fSnippet.Kind <> skUnit then
  begin
    with TPascalSourceGen.Create do
      try
        IncludeSnippet(fSnippet.ID);
        Result := UnitAsString(TestUnitName);
      finally
        Free;
      end;
  end
  else
    Result := fSnippet.SourceCode;
end;

function TPascalTestUnit.RequiredFileEncoding(const SourceCode: string):
  TEncoding;
begin
  if EncodingSupportsString(SourceCode, TEncoding.Default) then
    Result := TEncoding.Default
  else
    Result := TEncoding.UTF8;
end;

procedure TPascalTestUnit.SaveUnit(out FileName: string);
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
  Encoding := RequiredFileEncoding(SourceCode);
  try
    TFileIO.WriteAllText(FileName, SourceCode, Encoding, True);
  finally
    TEncodingHelper.FreeEncoding(Encoding);
  end;
end;

function TPascalTestUnit.UnitFileName: string;
const
  PasExt = '.pas';  // file extension for Pascal unit:
begin
  // Unit file name is in temp folder
  Result := IncludeTrailingPathDelimiter(TSystemFolders.Temp)
    + TestUnitName + PasExt;
end;

end.


{
 * This Source Code Form is subject to the terms of the Mozilla Public License,
 * v. 2.0. If a copy of the MPL was not distributed with this file, You can
 * obtain one at https://mozilla.org/MPL/2.0/
 *
 * Copyright (C) 2009-2021, Peter Johnson (gravatar.com/delphidabbler).
 *
 * Implements an extension of TMemIniFile that loads its data from a file and
 * strips enclosing quotes from string data.
}


unit UIniDataLoader;


interface


uses
  // Delphi
  Types,
  IniFiles,
  // Project
  UExceptions;


type
  ///  <summary>Extension of <c>TMemIniFile</c> that loads its data from a file.
  ///  Any quotes enclosing values read from the ini file are stripped.
  ///  </summary>
  TDatabaseIniFile = class(TMemIniFile)
  public
    ///  <summary>Object constructor. Sets up ini file object and loads data
    ///  into it from a file.</summary>
    ///  <param name="AFileName"><c>string</c> [in] Name of ini file.</param>
    constructor Create(const AFileName: string);
    ///  <summary>Retrieves a string value from an ini file.</summary>
    ///  <param name="Section"><c>string</c> [in] Section containing value.
    ///  </param>
    ///  <param name="Ident"><c>string</c> [in] Identifier of value.</param>
    ///  <param name="Default"><c>string</c> [in] Default value used if
    ///  <c>Ident</c> has no associated value.</param>
    ///  <returns><c>string</c> containing the required value, with any
    ///  enclosing quotes removed.</returns>
    ///  <remarks>Overrides the method in base class to strip enclosing quotes.
    ///  </remarks>
    function ReadString(const Section, Ident, Default: string): string;
      override;
    ///  <summary>Loads ini object's data from a string array.</summary>
    ///  <param name="AStrings"><c>TStringDynArray</c> [in] Array of strings to
    ///  be loaded.</param>
    ///  <remarks>Overloads inherited <c>SetStrings</c> method.</remarks>
    procedure SetStrings(const AStrings: TStringDynArray); overload;
  end;

type
  ///  <summary>Type of exception raised by TDatabaseIniFile.</summary>
  EDatabaseIniFile = class(ECodeSnip);


implementation


uses
  // Delphi
  SysUtils,
  Classes,
  IOUtils,
  // Project
  UConsts,
  UIOUtils;

{ TDatabaseIniFile }

constructor TDatabaseIniFile.Create(const AFileName: string);
resourcestring
  sFileNotFound = 'File "%s" does not exist.';
begin
  inherited Create(AFileName);
  if not TFile.Exists(AFileName) then
    raise EDatabaseIniFile.CreateFmt(
      sFileNotFound, [ExtractFileName(AFileName)]
    );
  SetStrings(TFileIO.ReadAllLines(AFileName, TEncoding.UTF8, True));
end;

function TDatabaseIniFile.ReadString(const Section, Ident,
  Default: string): string;
begin
  // Read string from ini
  Result := inherited ReadString(Section, Ident, Default);
  // Strip any leading and trailing quotes
  if (Length(Result) > 1) and (Result[1] = DOUBLEQUOTE)
    and (Result[Length(Result)] = DOUBLEQUOTE) then
    Result := Copy(Result, 2, Length(Result) - 2);
end;

procedure TDatabaseIniFile.SetStrings(const AStrings: TStringDynArray);
var
  SL: TStringList;
  Str: string;
begin
  SL := TStringList.Create;
  try
    for Str in AStrings do
      SL.Add(Str);
    SetStrings(SL);
  finally
    FreeAndNil(SL);
  end;
end;

end.


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
  Classes, IniFiles,
  // Project
  UBaseObjects, UExceptions, UIStringList, UMainDBFileReader;


type
  ///  <summary>Extension of <c>TMemIniFile</c> that loads its data from a file.
  ///  Any quotes enclosing values read from the ini file are stripped.
  ///  </summary>
  TDatabaseIniFile = class(TMemIniFile)
  strict private
    var
      ///  <summary>Loads database file using correct encoding.</summary>
      fFileReader: TMainDBFileReader;
  public
    ///  <summary>Object constructor. Sets up ini file object and loads data
    ///  into it from a file.</summary>
    ///  <param name="FileReader">TMainDBFileReader [in] Object used to read
    ///  database text files using correct encoding.</param>
    ///  <param name="AFileName"><c>string</c> [in] Name of ini file.</param>
    constructor Create(const FileReader: TMainDBFileReader;
      const AFileName: string);
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
    ///  <summary>Loads ini object's data from a string list.</summary>
    ///  <param name="Strings">IStringList [in] Strings to be loaded.</param>
    ///  <remarks>Overloads inherited <c>SetStrings</c> to load data from a
    ///  IStringList as well as TStringList.</remarks>
    procedure SetStrings(const Strings: IStringList); overload;
  end;

type
  ///  <summary>Type of exception raised by TDatabaseIniFile.</summary>
  EDatabaseIniFile = class(ECodeSnip);


implementation


uses
  // Delphi
  SysUtils,
  IOUtils,
  // Project
  UConsts,
  UStrUtils,
  UVersionInfo;

{ TDatabaseIniFile }

constructor TDatabaseIniFile.Create(const FileReader: TMainDBFileReader;
  const AFileName: string);
resourcestring
  sFileNotFound = 'File "%s" does not exist.';
begin
  inherited Create(AFileName);
  fFileReader := FileReader;
  if not TFile.Exists(AFileName) then
    raise EDatabaseIniFile.CreateFmt(
      sFileNotFound, [ExtractFileName(AFileName)]
    );
  SetStrings(fFileReader.ReadAllStrings(AFileName));
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

procedure TDatabaseIniFile.SetStrings(const Strings: IStringList);
var
  SL: TStringList;  // string list use to call inherited method
begin
  SL := TStringList.Create;
  try
    Strings.CopyTo(SL);
    SetStrings(SL);
  finally
    FreeAndNil(SL);
  end;
end;

end.


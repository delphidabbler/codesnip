{
 * This Source Code Form is subject to the terms of the Mozilla Public License,
 * v. 2.0. If a copy of the MPL was not distributed with this file, You can
 * obtain one at https://mozilla.org/MPL/2.0/
 *
 * Copyright (C) 2009-2021, Peter Johnson (gravatar.com/delphidabbler).
 *
 * Implements an extension of TMemIniFile that loads the ini data from a set of
 * associated files and pre-processes the data.
}


unit UIniDataLoader;


interface


uses
  // Delphi
  Classes, IniFiles,
  // Project
  UBaseObjects, UExceptions, UIStringList, UMainDBFileReader;


type
  ///  <summary>
  ///  Extension of TMemIniFile that loads the ini data from a set of associated
  ///  files. Any quotes enclosing values read from ini file are stripped. Files
  ///  are pre-processed, and modified according to any pre-processing
  ///  directives.
  ///  </summary>
  TDatabaseIniFile = class(TMemIniFile)
  strict private
    var
      ///  <summary>Loads database files using correct encoding.</summary>
      fFileReader: TMainDBFileReader;
    ///  <summary>
    ///  Concatenates the content of a list of text files.
    ///  </summary>
    ///  <param name="FileNames">IStringList [in] List of text files.</param>
    ///  <returns>IStringList containing concatenation of lines read from the
    ///  files.</returns>
    function LoadFiles(const FileNames: IStringList): IStringList;
  public
    ///  <summary>
    ///  Object constructor. Sets up ini file object and loads data into it from
    ///  a set of associated files.
    ///  </summary>
    ///  <param name="FileReader">TMainDBFileReader [in] Object used to read
    ///  database text files using correct encoding.</param>
    ///  <param name="FileName">string [in] Base name of associated files
    ///  containing data.</param>
    constructor Create(const FileReader: TMainDBFileReader;
      const FileName: string);
    ///  <summary>
    ///  Retrieves a string value from an ini file.
    ///  </summary>
    ///  <param name="Section">string [in] Section containing value.</param>
    ///  <param name="Ident">string [in] Identifier of value.</param>
    ///  <param name="Default">string [in] Default value used if ident is not
    ///  present or not assigned.</param>
    ///  <returns>string containing required value, with any enclosing quotes
    ///  removed.</returns>
    ///  <remarks>
    ///  Overrides method in base class to strip enclosing quotes.
    ///  </remarks>
    function ReadString(const Section, Ident, Default: string): string;
      override;
    ///  <summary>
    ///  Loads ini object's data from a string list.
    ///  </summary>
    ///  <param name="Strings">IStringList [in] Strings to be loaded.</param>
    ///  <remarks>
    ///  Overloads inherited SetStrings to load data from IStringList as well
    ///  as TStringList.
    ///  </remarks>
    procedure SetStrings(const Strings: IStringList); overload;
  end;

type
  ///  <summary>Type of exception raised by TDatabaseIniFile.</summary>
  EDatabaseIniFile = class(ECodeSnip);


implementation


uses
  // Delphi
  SysUtils,
  // Project
  UConsts, UStrUtils, UVersionInfo;


type
  {
  TDatabaseFileMapper:
    Static class that gets a list of file names associated with a specified file
    name. Used to associate database file names visible only to early versions
    of CodeSnip with file names visible to later versions.
  }
  TDatabaseFileMapper = class(TNoConstructObject)
  strict private
    class function AlternateFileName(const FileName, InnerExt: string): string;
      {Creates an alternate file name for a specified file and an "inner"
      extension that is inserted before the file's current extension.
        @param FileName [in] File name to form basis of alternate name.
        @param InnerExt [in] "Inner" extension that comes between file name and
          its last extension.
        @return Alternate file name.
      }
  public
    class function GetRelatedFiles(const FileName: string): IStringList;
      {Builds a list of file names associated with a file name.
        @param FileName [in] Original file name.
        @return List of associated files, all of which exist. Original file name
          may be omitted from the list if it doesn't exist.
      }
  end;

{ TDatabaseIniFile }

constructor TDatabaseIniFile.Create(const FileReader: TMainDBFileReader;
  const FileName: string);
var
  Files: IStringList; // List of associated file names
resourcestring
  // Error message
  sMissingCatFile = 'Neither category file "%s" nor its alternate exists.';
begin
  inherited Create(FileName);
  fFileReader := FileReader;
  // get list of associated files
  Files := TDatabaseFileMapper.GetRelatedFiles(FileName);
  if Files.Count = 0 then
    raise EDatabaseIniFile.CreateFmt(
      sMissingCatFile, [ExtractFileName(FileName)]
    );
  // load ini file from concatenated contents of Files after running through
  // pre-processor
  SetStrings(LoadFiles(Files));
end;

function TDatabaseIniFile.LoadFiles(const FileNames: IStringList): IStringList;
var
  FileName: string; // each file name is list
begin
  Result := TIStringList.Create;
  for FileName in FileNames do
    Result.Add(fFileReader.ReadAllStrings(FileName));
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

{ TDatabaseFileMapper }

function RemoveFileExt(const FileName: string): string;
  {Removes an extension from a file name.
    @param FileName [in] File name from which extension is to be removed.
    @return File name without extension.
  }
begin
  if StrContainsStr('.', FileName) then
    Result := SysUtils.ChangeFileExt(FileName, '')
  else
    Result := FileName;
end;

class function TDatabaseFileMapper.AlternateFileName(
  const FileName, InnerExt: string): string;
  {Creates an alternate file name for a specified file and an "inner" extension
  that is inserted before the file's current extension.
    @param FileName [in] File name to form basis of alternate name.
    @param InnerExt [in] "Inner" extension that comes between file name and its
      last extension.
    @return Alternate file name.
  }
var
  BaseName: string;   // base of file name without extension
  Extension: string;  // extension common to all names
begin
  Extension := ExtractFileExt(FileName);
  BaseName := RemoveFileExt(ExtractFileName(FileName));
  Result := ExtractFilePath(FileName) + BaseName + InnerExt + Extension;
end;

class function TDatabaseFileMapper.GetRelatedFiles(
  const FileName: string): IStringList;
  {Builds a list of file names associated with a file name.
    @param FileName [in] Original file name.
    @return List of associated files, all of which exist. Original file name may
      be omitted from the list if it doesn't exist.
  }
var
  AltFileName: string;  // an alternate file name
  InnerExt: string;     // each inner extension
const
  // "Inner" extensions to be interposed between filename and its extension
  InnerExts: array[0..1] of string = ('.3', '.4');
begin
  Result := TIStringList.Create;
  if FileExists(FileName) then
    Result.Add(FileName);
  for InnerExt in InnerExts do
  begin
    AltFileName := AlternateFileName(FileName, InnerExt);
    if (AltFileName <> '') and FileExists(AltFileName) then
      Result.Add(AltFileName);
  end;
end;

end.


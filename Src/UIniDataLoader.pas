{
 * This Source Code Form is subject to the terms of the Mozilla Public License,
 * v. 2.0. If a copy of the MPL was not distributed with this file, You can
 * obtain one at http://mozilla.org/MPL/2.0/
 *
 * Copyright (C) 2009-2020, Peter Johnson (gravatar.com/delphidabbler).
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

type
  {
  TDatabasePreprocessor:
    Static class used to pre-process ini file, acting on pre-processor
    instructions. Does not support nesting of directives. Supported instructions
    are:
      #if-ver-eq <version>
        Checks if application version is equal to <version>.
      #if-ver-neq <version>
        Checks if application version is not equal to <version>.
      #if-ver-lt <version>
        Checks if application version is less than <version>.
      #if-ver-gt <version>
        Checks if application version is greater than <version>.
      #if-ver-inrange <version-lo> <version-hi>
        Checks if application version is in the range of versions specified from
        <version-lo> to <version-hi>, inclusive.
      #end-if
        Ends a block started by any of the #if- instructions above.
      When a #if- instruction evaluates true then all the following lines in the
      ini file up to the #end-if instruction and included. If it evaluates false
      the enclosed lines are ignored.
      Version numbers are in form 9.9.9.9. Minor version numbers are optional
      and are filled with zeroes, so specifying 3 is equivalent to 3.0.0.0 and
      2.1 is equivalent to 2.1.0.0.
      Unrecognised instructions are ignored and are not included in the output.
  }
  TDatabasePreprocessor = class(TNoConstructObject)
  strict private
    type
      {
      TVerCompareMethod:
        Type of methods used to compare version numbers.
        @param Ver1 [in] First version number to compare.
        @param Ver2 [in] Second version number to compare.
        @return True if required condition applies, False if not.
      }
      TVerCompareMethod = function(Ver1, Ver2: TVersionNumber): Boolean
        of object;
    const
      // Symbols that prefix every pre-processor instruction
      cPreProcPrefix = ';#';
      // Pre-processor instructions / directives
      cEndIf         = cPreProcPrefix + 'end-if';         // ends if instruction
      cIfVerEQ       = cPreProcPrefix + 'if-ver-eq';      // appver = param
      cIfVerNEQ      = cPreProcPrefix + 'if-ver-neq';     // appver <> param
      cIfVerLT       = cPreProcPrefix + 'if-ver-lt';      // appver < param
      cIfVerGT       = cPreProcPrefix + 'if-ver-gt';      // appver > param
      cIfVerInRange  = cPreProcPrefix + 'if-ver-inrange'; // appver in range
    class function ProcessVerEQ(const Lines: IStringList;
      var LineIdx: Integer): IStringList;
      {Processes a ifvereq pre-processor directive. Includes lines before endif
      directive only if condition is met, i.e. app version is equal to version
      specified in directive.
        @param Lines [in] Lines of ini file.
        @param LineIdx [in/out] In: index of current line before processing.
          Out: index of last line processed.
      }
    class function ProcessVerNEQ(const Lines: IStringList;
      var LineIdx: Integer): IStringList;
      {Processes a ifverneq pre-processor directive. Includes lines before endif
      directive only if condition is met, i.e. app version is not equal to
      version specified in directive.
        @param Lines [in] Lines of ini file.
        @param LineIdx [in/out] In: index of current line before processing.
          Out: index of last line processed.
      }
    class function ProcessVerLT(const Lines: IStringList;
      var LineIdx: Integer): IStringList;
      {Processes a ifverlt pre-processor directive. Includes lines before endif
      directive only if condition is met, i.e. app version is less than version
      specified in directive.
        @param Lines [in] Lines of ini file.
        @param LineIdx [in/out] In: index of current line before processing.
          Out: index of last line processed.
      }
    class function ProcessVerGT(const Lines: IStringList;
      var LineIdx: Integer): IStringList;
      {Processes a ifvergt pre-processor directive. Includes lines before endif
      directive only if condition is met, i.e. app version is greater than
      version specified in directive.
        @param Lines [in] Lines of ini file.
        @param LineIdx [in/out] In: index of current line before processing.
          Out: index of last line processed.
      }
    class function ProcessVerInRange(const Lines: IStringList;
      var LineIdx: Integer): IStringList;
      {Processes ifverinrange pre-processor directive. Includes lines before
      endif directive only if condition is met, i.e. app version is in range of
      version numbers specified in directive.
        @param Lines [in] Lines of ini file.
        @param LineIdx [in/out] In: index of current kine before processing.
          Out: index of last line processed.
      }
    class function CompareEQ(Ver1, Ver2: TVersionNumber): Boolean;
      {Compares two version numbers to check if first is equal to second.
        @param Ver1 [in] First version number to compare.
        @param Ver2 [in] Second version number to compare.
        @return True if Ver1 = Ver2, False if not.
      }
    class function CompareNEQ(Ver1, Ver2: TVersionNumber): Boolean;
      {Compares two version numbers to check if first is not equal to second.
        @param Ver1 [in] First version number to compare.
        @param Ver2 [in] Second version number to compare.
        @return True if Ver1 <> Ver2, False if not.
      }
    class function CompareGT(Ver1, Ver2: TVersionNumber): Boolean;
      {Compares two version numbers to check if first is greater than second.
        @param Ver1 [in] First version number to compare.
        @param Ver2 [in] Second version number to compare.
        @return True if Ver1 > Ver2, False if not.
      }
    class function CompareLT(Ver1, Ver2: TVersionNumber): Boolean;
      {Compares two version numbers to check if first is less than second.
        @param Ver1 [in] First version number to compare.
        @param Ver2 [in] Second version number to compare.
        @return True if Ver1 < Ver2, False if not.
      }
    class function ProcessVerCompare(const Lines: IStringList;
      var LineIdx: Integer; const CompareFn: TVerCompareMethod): IStringList;
      {Processes a version comparison directive.
        @param Lines [in] Lines of ini file.
        @param LineIdx [in/out] In: index of current line before processing.
          Out: index of last line processed.
        @param CompareFn [in] Method to used to compare application version with
          version number read from directive to determine if directive executes.
      }
    class function ProcessToEndIf(const Lines: IStringList;
      var LineIdx: Integer; const RecordLines: Boolean): IStringList;
  public
    class function PreProcess(Lines: IStringList): IStringList;
      {Performs pre-processing.
        @param Lines [in] Lines of text to be pre-processed.
        @return Lines after preprocessing.
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
  SetStrings(
    TDatabasePreprocessor.PreProcess(
      LoadFiles(Files)
    )
  );
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

{ TDatabasePreprocessor }

class function TDatabasePreprocessor.CompareEQ(Ver1,
  Ver2: TVersionNumber): Boolean;
  {Compares two version numbers to check if first is equal to second.
    @param Ver1 [in] First version number to compare.
    @param Ver2 [in] Second version number to compare.
    @return True if Ver1 = Ver2, False if not.
  }
begin
  Result := Ver1 = Ver2;
end;

class function TDatabasePreprocessor.CompareGT(Ver1,
  Ver2: TVersionNumber): Boolean;
  {Compares two version numbers to check if first is greater than second.
    @param Ver1 [in] First version number to compare.
    @param Ver2 [in] Second version number to compare.
    @return True if Ver1 > Ver2, False if not.
  }
begin
  Result := Ver1 > Ver2;
end;

class function TDatabasePreprocessor.CompareLT(Ver1,
  Ver2: TVersionNumber): Boolean;
  {Compares two version numbers to check if first is less than second.
    @param Ver1 [in] First version number to compare.
    @param Ver2 [in] Second version number to compare.
    @return True if Ver1 < Ver2, False if not.
  }
begin
  Result := Ver1 < Ver2;
end;

class function TDatabasePreprocessor.CompareNEQ(Ver1,
  Ver2: TVersionNumber): Boolean;
  {Compares two version numbers to check if first is not equal to second.
    @param Ver1 [in] First version number to compare.
    @param Ver2 [in] Second version number to compare.
    @return True if Ver1 <> Ver2, False if not.
  }
begin
  Result := Ver1 <> Ver2;
end;

class function TDatabasePreprocessor.PreProcess(Lines: IStringList):
  IStringList;
  {Performs pre-processing.
    @param Lines [in] Lines of text to be pre-processed.
    @return Lines after preprocessing.
  }
var
  LineIdx: Integer; // indexes each line
  Line: string;     // trimmed content of a line
begin
  Result := TIStringList.Create;
  LineIdx := 0;
  while LineIdx < Lines.Count do
  begin
    Line := StrTrim(Lines[LineIdx]);
    // Check for pre-processor instructions
    if StrStartsStr(cIfVerLT, Line) then
      Result.Add(ProcessVerLT(Lines, LineIdx))
    else if StrStartsStr(cIfVerGT, Line) then
      Result.Add(ProcessVerGT(Lines, LineIdx))
    else if StrStartsStr(cIfVerInRange, Line) then
      Result.Add(ProcessVerInRange(Lines, LineIdx))
    else if StrStartsStr(cIfVerEQ, Line) then
      Result.Add(ProcessVerEQ(Lines, LineIdx))
    else if StrStartsStr(cIfVerNEQ, Line) then
      Result.Add(ProcessVerNEQ(Lines, LineIdx))
    else if StrStartsStr(cPreProcPrefix, Line) then
      // ignore unknown pre-proc dirs
    else
      // no pre-processor, just use trimmed line
      Result.Add(Line);
    Inc(LineIdx);
  end;
end;

class function TDatabasePreprocessor.ProcessToEndIf(const Lines: IStringList;
  var LineIdx: Integer; const RecordLines: Boolean): IStringList;
begin
  Inc(LineIdx);
  Result := TIStringList.Create;
  while (LineIdx < Lines.Count) and (StrTrim(Lines[LineIdx]) <> cEndIf) do
  begin
    if RecordLines then
      Result.Add(Lines[LineIdx]);
    Inc(LineIdx);
  end;
end;

class function TDatabasePreprocessor.ProcessVerCompare(const Lines: IStringList;
  var LineIdx: Integer; const CompareFn: TVerCompareMethod): IStringList;
  {Processes a version comparison directive.
    @param Lines [in] Lines of ini file.
    @param LineIdx [in/out] In: index of current line before processing. Out:
      index of last line processed.
    @param CompareFn [in] Method to used to compare application version with
      version number read from directive to determine if directive executes.
  }
var
  InstParts: IStringList;   // parts of the pre-processor instruction
  Ver: TVersionNumber;      // version number from directive
begin
  // Get version number parameter
  InstParts := TIStringList.Create(Lines[LineIdx], ' ', False, True);
  if InstParts.Count >= 2 then
    Ver := InstParts[1]  // implicit conversion of string to TVersionNumber
  else
    Ver := TVersionNumber.Nul;  // 0.0.0.0
  // Process lines up to endif
  Result := ProcessToEndIf(
    Lines, LineIdx, CompareFn(TVersionInfo.ProductVerNum, Ver)
  );
end;

class function TDatabasePreprocessor.ProcessVerEQ(const Lines: IStringList;
  var LineIdx: Integer): IStringList;
  {Processes a ifvereq pre-processor directive. Includes lines before endif
  directive only if condition is met, i.e. app version is equal to version
  specified in directive.
    @param Lines [in] Lines of ini file.
    @param LineIdx [in/out] In: index of current line before processing. Out:
      index of last line processed.
  }
begin
  Result := ProcessVerCompare(Lines, LineIdx, CompareEQ);
end;

class function TDatabasePreprocessor.ProcessVerGT(const Lines: IStringList;
  var LineIdx: Integer): IStringList;
  {Processes a ifvergt pre-processor directive. Includes lines before endif
  directive only if condition is met, i.e. app version is greater than
  version specified in directive.
    @param Lines [in] Lines of ini file.
    @param LineIdx [in/out] In: index of current line before processing. Out:
      index of last line processed.
  }
begin
  Result := ProcessVerCompare(Lines, LineIdx, CompareGT);
end;

class function TDatabasePreprocessor.ProcessVerInRange(const Lines: IStringList;
  var LineIdx: Integer): IStringList;
  {Processes ifverinrange pre-processor directive. Includes lines before endif
  directive only if condition is met, i.e. app version is in range of version
  numbers specified in directive.
    @param Lines [in] Lines of ini file.
    @param LineIdx [in/out] In: index of current kine before processing. Out:
      index of last line processed.
  }
var
  InstParts: IStringList;   // parts of the pre-processor instruction
  VerLo: TVersionNumber;    // version number from directive
  VerHi: TVersionNumber;    // version number from directive
  IncludeContents: Boolean; // flag true if enclosed lines to be included
begin
  Result := TIStringList.Create;
  // Get version number parameter
  InstParts := TIStringList.Create(Lines[LineIdx], ' ', False, True);
  if InstParts.Count < 3 then
    IncludeContents := False
  else
  begin
    VerLo := InstParts[1];  // implicit conversion of string to TVersionNumber
    VerHi := InstParts[2];  // implicit conversion of string to TVersionNumber
    IncludeContents := (TVersionInfo.ProductVerNum >= VerLo) and
      (TVersionInfo.ProductVerNum <= VerHi);
  end;
  // Process lines up to endif
  Result := ProcessToEndIf(Lines, LineIdx, IncludeContents);
end;

class function TDatabasePreprocessor.ProcessVerLT(const Lines: IStringList;
  var LineIdx: Integer): IStringList;
  {Processes a ifverlt pre-processor directive. Includes lines before endif
  directive only if condition is met, i.e. app version is less than version
  specified in directive.
    @param Lines [in] Lines of ini file.
    @param LineIdx [in/out] In: index of current line before processing. Out:
      index of last line processed.
  }
begin
  Result := ProcessVerCompare(Lines, LineIdx, CompareLT);
end;

class function TDatabasePreprocessor.ProcessVerNEQ(const Lines: IStringList;
  var LineIdx: Integer): IStringList;
  {Processes a ifverneq pre-processor directive. Includes lines before endif
  directive only if condition is met, i.e. app version is not equal to version
  specified in directive.
    @param Lines [in] Lines of ini file.
    @param LineIdx [in/out] In: index of current line before processing. Out:
      index of last line processed.
  }
begin
  Result := ProcessVerCompare(Lines, LineIdx, CompareNEQ);
end;

end.


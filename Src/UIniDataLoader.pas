{
 * UIniDataLoader.pas
 *
 * Implements an extension of TMemIniFile that loads the ini data from a set of
 * associated files and pre-processes the data.
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
 * The Original Code is UIniDataLoader.pas
 *
 * The Initial Developer of the Original Code is Peter Johnson
 * (http://www.delphidabbler.com/).
 *
 * Portions created by the Initial Developer are Copyright (C) 2009-2012 Peter
 * Johnson. All Rights Reserved.
 *
 * Contributor(s)
 *   NONE
 *
 * ***** END LICENSE BLOCK *****
}


unit UIniDataLoader;


interface


uses
  // Delphi
  Classes, IniFiles,
  // Project
  UBaseObjects, UExceptions, UIStringList;


type
  {
  TDatabaseIniFile:
    Extension of TMemIniFile that loads the ini data from a set of associated
    files. Any quotes enclosing values read from ini file are stripped. Files
    are pre-processed, and modified according to any pre-processing directives.
  }
  TDatabaseIniFile = class(TMemIniFile)
  strict private
    procedure CopyFileToStream(const FileName: string; const Stream: TStream);
      {Appends a file's contents to a stream.
        @param FileName [in] Name of file to copy.
        @param Stream [in] Stream that receives file's content.
      }
    procedure CopyFilesToStream(const FileNames: IStringList;
      const Stream: TStream);
      {Copies content of a list of file to a stream.
        @param FileNames [in] List of name of files to be copied
        @param Stream [in] Stream that receives files' contents.
      }
    procedure LoadFromStream(const Stream: TStream);
      {Load ini data from a stream.
        @param Stream [in] Stream containing ini data.
      }
  public
    constructor Create(const FileName: string);
      {Class constructor. Sets up object and loads its data from a set of
      associated files.
        @param FileName [in] Base file name for associated files containing
          data.
      }
    function ReadString(const Section, Ident, Default: string): string;
      override;
      {Retrieves a string value from an ini file.
        @param Section [in] Section containing desired value.
        @param Ident [in] Identifier of desired value.
        @param Default [in] Default value used if Ident is not present or not
          assigned.
        @return Required value, with any enclosing quotes removed.
      }
    procedure SetStrings(const Strings: IStringList); overload;
      {Adapts inherited SetStrings to load data from IStringList instead of
      TStringList.
        @param Strings [in] Strings to be loaded.
      }
  end;

  {
  EDatabaseIniFile:
  }
  EDatabaseIniFile = class(ECodeSnip);


implementation


uses
  // Delphi
  SysUtils, StrUtils,
  // Project
  UVersionInfo;


type
  {
  TDatabaseFileMapper:
    Static class that gets a list of file names associated with a specified file
    name. Used to associate database file names visible only to early versions
    of CodeSnip with file names visible to later versions.
  }
  TDatabaseFileMapper = class(TNoConstructObject)
  strict private
    class function AlternateFileName(const FileName: string): string;
      {Creates an alternate file name for a specified file that will be
      recognised by v3 of CodeSnip or later.
        @param FileName [in] Base file name.
        @return Alternate file name.
      }
  public
    class function GetRelatedFiles(const FileName: string): IStringList;
      {Builds a list of file names associated with a base file name.
        @param FileName [in] Base file name.
        @return List of associated files, all of which exist. List may not
          include base file name if it doesn't exist.
      }
  end;

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

procedure TDatabaseIniFile.CopyFilesToStream(const FileNames: IStringList;
  const Stream: TStream);
  {Copies content of a list of file to a stream.
    @param FileNames [in] List of name of files to be copied
    @param Stream [in] Stream that receives files' contents.
  }
var
  FileName: string; // each file name in list
begin
  for FileName in FileNames do
    CopyFileToStream(FileName, Stream);
end;

procedure TDatabaseIniFile.CopyFileToStream(const FileName: string;
  const Stream: TStream);
  {Appends a file's contents to a stream.
    @param FileName [in] Name of file to copy.
    @param Stream [in] Stream that receives file's content.
  }
var
  FS: TFileStream;  // stream onto file
begin
  FS := TFileStream.Create(FileName, fmOpenRead or fmShareDenyNone);
  try
    Stream.CopyFrom(FS, FS.Size);
  finally
    FreeAndNil(FS);
  end;
end;

constructor TDatabaseIniFile.Create(const FileName: string);
  {Class constructor. Sets up object and loads its data from a set of associated
  files.
    @param FileName [in] Base file name for associated files containing data.
  }
var
  Stream: TStream;    // Stream that receives data from files
  Files: IStringList; // List of associated file names
resourcestring
  // Error message
  sMissingCatFile = 'Neither category file "%s" nor its alternate exists.';
begin
  inherited Create(FileName);
  Stream := TMemoryStream.Create;
  try
    // Get list of file names containing data
    Files := TDatabaseFileMapper.GetRelatedFiles(FileName);
    if Files.Count = 0 then
      raise EDatabaseIniFile.CreateFmt(
        sMissingCatFile, [ExtractFileName(FileName)]
      );
    // Load ini data via intermediate streams
    CopyFilesToStream(TDatabaseFileMapper.GetRelatedFiles(FileName), Stream);
    Stream.Position := 0;
    LoadFromStream(Stream);
  finally
    FreeAndNil(Stream);
  end;
end;

procedure TDatabaseIniFile.LoadFromStream(const Stream: TStream);
  {Load ini data from a stream.
    @param Stream [in] Stream containing ini data.
  }
var
  Strings: TStringList;         // string list that contains ini data as text
  PreProcStrings: IStringList;  // string list of data after pre-processing
begin
  Strings := TStringList.Create;
  try
    Strings.LoadFromStream(Stream);
    PreProcStrings := TDatabasePreprocessor.PreProcess(
      TIStringList.Create(Strings)
    );
    SetStrings(PreProcStrings);
  finally
    FreeAndNil(Strings);
  end;
end;

function TDatabaseIniFile.ReadString(const Section, Ident,
  Default: string): string;
  {Retrieves a string value from an ini file.
    @param Section [in] Section containing desired value.
    @param Ident [in] Identifier of desired value.
    @param Default [in] Default value used if Ident is not present or not
      assigned.
    @return Required value, with any enclosing quotes removed.
  }
const
  cQuote = '"';   // quote character
begin
  // Read string from ini
  Result := inherited ReadString(Section, Ident, Default);
  // Strip any leading and trailing quotes
  if (Length(Result) > 1) and (Result[1] = cQuote)
    and (Result[Length(Result)] = cQuote) then
    Result := Copy(Result, 2, Length(Result) - 2);
end;

procedure TDatabaseIniFile.SetStrings(const Strings: IStringList);
  {Adapts inherited SetStrings to load data from IStringList instead of
  TStringList.
    @param Strings [in] Strings to be loaded.
  }
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
  if SysUtils.AnsiPos('.', FileName) > 0 then
    Result := SysUtils.ChangeFileExt(FileName, '')
  else
    Result := FileName;
end;

class function TDatabaseFileMapper.AlternateFileName(
  const FileName: string): string;
  {Creates an alternate file name for a specified file that will be recognised
  by v3 of CodeSnip or later.
    @param FileName [in] Base file name.
    @return Alternate file name.
  }
var
  BaseName: string;   // base of file name without extension
  Extension: string;  // extension common to all names
const
  cV3Ext = '.3';  // first extension for v3 database files
begin
  // Get base file name without extension
  Extension := ExtractFileExt(FileName);
  BaseName := RemoveFileExt(ExtractFileName(FileName));
  // Build alternate file name
  Result := ExtractFilePath(FileName) + BaseName + cV3Ext + Extension;
end;

class function TDatabaseFileMapper.GetRelatedFiles(
  const FileName: string): IStringList;
  {Builds a list of file names associated with a base file name.
    @param FileName [in] Base file name.
    @return List of associated files, all of which exist. List may not include
      base file name if it doesn't exist.
  }
var
  AltFileName: string;  // an alternate file name
begin
  Result := TIStringList.Create;
  if FileExists(FileName) then
    Result.Add(FileName);
  AltFileName := AlternateFileName(FileName);
  if (AltFileName <> '') and FileExists(AltFileName) then
    Result.Add(AltFileName);
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
    Line := Trim(Lines[LineIdx]);
    // Check for pre-processor instructions
    if AnsiStartsStr(cIfVerLT, Line) then
      Result.Add(ProcessVerLT(Lines, LineIdx))
    else if AnsiStartsStr(cIfVerGT, Line) then
      Result.Add(ProcessVerGT(Lines, LineIdx))
    else if AnsiStartsStr(cIfVerInRange, Line) then
      Result.Add(ProcessVerInRange(Lines, LineIdx))
    else if AnsiStartsStr(cIfVerEQ, Line) then
      Result.Add(ProcessVerEQ(Lines, LineIdx))
    else if AnsiStartsStr(cIfVerNEQ, Line) then
      Result.Add(ProcessVerNEQ(Lines, LineIdx))
    else if AnsiStartsStr(cPreProcPrefix, Line) then
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
  while (LineIdx < Lines.Count) and (Trim(Lines[LineIdx]) <> cEndIf) do
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


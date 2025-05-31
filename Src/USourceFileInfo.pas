{
 * This Source Code Form is subject to the terms of the Mozilla Public License,
 * v. 2.0. If a copy of the MPL was not distributed with this file, You can
 * obtain one at https://mozilla.org/MPL/2.0/
 *
 * Copyright (C) 2006-2025, Peter Johnson (gravatar.com/delphidabbler).
 *
 * Implements class that provides information about types of source code output
 * that are supported.
}


unit USourceFileInfo;


interface


uses
  // Delphi
  Generics.Collections,
  // Project
  UEncodings;


type
  ///  <summary>
  ///  Enumeration of file types that can be used for source code output.
  ///  </summary>
  TSourceFileType = (
    sfText,     // plain text files
    sfPascal,   // pascal files (either .pas for units or .inc for include files
    sfHTML5,    // HTML 5 files
    sfXHTML,    // XHTML files
    sfRTF,      // rich text files
    sfMarkdown  // Markdown files
  );

type
  ///  <summary>
  ///  Record that stores information about an encoding for use by save source
  ///  dialog boxes.
  ///  </summary>
  TSourceFileEncoding = record
  strict private
    fEncodingType: TEncodingType; // Value of EncodingType property
    fDisplayName: string;         // Value of DisplayName property
  public
    ///  <summary>Sets the value of the <c>EncodingType</c> property.</summary>
    ///  <remarks>The <c>DisplayName</c> property is dependent on the value of
    ///  the <c>EncodingType</c> property and so can't be set explicitly.
    ///  </remarks>
    constructor Create(const AEncodingType: TEncodingType);

    ///  <summary>Type of this encoding.</summary>
    property EncodingType: TEncodingType read fEncodingType;

    ///  <summary>Description of encoding for display in dialog box.</summary>
    property DisplayName: string read fDisplayName;
  end;

type
  ///  <summary>Array of source file encoding records.</summary>
  TSourceFileEncodings = array of TSourceFileEncoding;

type
  ///  <summary>
  ///  Record that stores information about a source file type required by save
  ///  source dialog boxes.
  ///  </summary>
  TSourceFileTypeInfo = record
  strict private
    fExtension: string;               // Value of Extension property
    fDisplayName: string;             // Value of DisplayName property
    fEncodings: TSourceFileEncodings; // Value of Encodings property
  public
    ///  <summary>Sets values of properties.</summary>
    constructor Create(const AExtension, ADisplayName: string;
      const AEncodingTypes: array of TEncodingType);
    ///  <summary>File extension associated with this file type.</summary>
    property Extension: string read fExtension;
    ///  <summary>Name of file extension to display in save dialog box.
    ///  </summary>
    property DisplayName: string read fDisplayName;
    ///  <summary>Encodings supported by this file type.</summary>
    property Encodings: TSourceFileEncodings read fEncodings;
  end;

type
  ///  <summary>
  ///  Class that provides information about types of source code output that
  ///  are supported.
  ///  </summary>
  TSourceFileInfo = class(TObject)
  strict private
    var
      ///  <summary>Stores information about the different source code output
      ///  types required by save source dialog boxes.</summary>
      fFileTypeInfo: TDictionary<TSourceFileType,TSourceFileTypeInfo>;
      ///  <summary>Maps a one-based index of a file filter within the current
      ///  filter string to the corresponding <c>TSourceFileType</c> that was
      ///  used to create the filter string entry.</summary>
      fFilterIdxToFileTypeMap: TDictionary<Integer,TSourceFileType>;
      ///  <summary>Value of DefaultFileName property.</summary>
      fDefaultFileName: string;
      ///  <summary>Value of <c>RequirePascalDefFileName</c> property.</summary>
      fRequirePascalDefFileName: Boolean;
      ///  <summary>Filter string for use in open / save dialog boxes from
      ///  descriptions and file extensions of each supported file type.
      ///  </summary>
      fFilterString: string;
    ///  <summary>Generates a new filter string and filter index to file type
    ///  map from the current state of the <c>FileTypeInfo</c> property.
    ///  </summary>
    ///  <remarks>This method MUST be called every time the <c>FileTypeInfo</c>
    ///  property is updated.</remarks>
    procedure GenerateFilterInfo;
    ///  <summary>Read accessor for FileTypeInfo property.</summary>
    ///  <exception>Raises <c>EListError</c> if <c>FileType</c> is not contained
    ///  in the property.</exception>
    function GetFileTypeInfo(const FileType: TSourceFileType):
      TSourceFileTypeInfo;
    ///  <summary>Write accessor for FileTypeInfo property.</summary>
    procedure SetFileTypeInfo(const FileType: TSourceFileType;
      const Info: TSourceFileTypeInfo);
    ///  <summary>Write access for DefaultFileName property.</summary>
    ///  <remarks>Converts new property value into a valid Pascal identifier if
    ///  necessary.</remarks>
    procedure SetDefaultFileName(const Value: string);
  public
    constructor Create;
    destructor Destroy; override;

    ///  <summary>Returns filter string for use in open / save dialog boxes from
    ///  descriptions and file extensions of each supported file type.</summary>
    function FilterString: string;

    ///  <summary>Returns the file type associated with a file filter at the
    ///  given one-based index within the current filter string.</summary>
    function FileTypeFromFilterIdx(const Idx: Integer): TSourceFileType;

    ///  <summary>Checks if a file type is supported.</summary>
    ///  <param name="FileType"><c>TSourceFileType</c> [in] File type to check.
    ///  </param>
    ///  <returns><c>Boolean</c>. <c>True</c> if file type is supported,
    ///  <c>False</c> if not.</returns>
    function SupportsFileType(const FileType: TSourceFileType): Boolean;

    ///  <summary>Information about each supported file type that is of use to
    ///  save source dialog boxes.</summary>
    ///  <exception>A <c>EListError</c> exception is raised if no information
    ///  relating to <c>FileType</c> has been stored in this property.
    ///  </exception>
    property FileTypeInfo[const FileType: TSourceFileType]: TSourceFileTypeInfo
      read GetFileTypeInfo write SetFileTypeInfo;

    ///  <summary>Default source code file name.</summary>
    ///  <remarks>If, and only if, <c>RequirePascalDefFileName</c> is
    ///  <c>True</c> the default file name is modified so that name is a valid
    ///  Pascal identifier.</remarks>
    property DefaultFileName: string
      read fDefaultFileName write SetDefaultFileName;

    ///  <summary>Determines whether any value assigned to
    ///  <c>DefaultFileName</c> is converted to a valid Pascal identifier or
    ///  not.</summary>
    property RequirePascalDefFileName: Boolean
      read fRequirePascalDefFileName write fRequirePascalDefFileName
      default True;
  end;


implementation


uses
  // Delphi
  SysUtils, Windows {for inlining}, Character,
  // Project
  ULocales,
  UStrUtils;


{ TSourceFileInfo }

constructor TSourceFileInfo.Create;
begin
  inherited Create;
  fFileTypeInfo := TDictionary<TSourceFileType,TSourceFileTypeInfo>.Create;
  fFilterIdxToFileTypeMap := TDictionary<Integer,TSourceFileType>.Create;
  fRequirePascalDefFileName := True;
end;

destructor TSourceFileInfo.Destroy;
begin
  fFilterIdxToFileTypeMap.Free;
  fFileTypeInfo.Free;
  inherited;
end;

function TSourceFileInfo.FileTypeFromFilterIdx(
  const Idx: Integer): TSourceFileType;
begin
  Result := fFilterIdxToFileTypeMap[Idx];
end;

function TSourceFileInfo.FilterString: string;
begin
  Result := fFilterString;
end;

procedure TSourceFileInfo.GenerateFilterInfo;
const
  cFilterFmt = '%0:s (*%1:s)|*%1:s';  // format string for creating file filter
var
  FT: TSourceFileType;  // loops thru all source file types
  FilterIdx: Integer;   // current index in filter string
begin
  fFilterIdxToFileTypeMap.Clear;
  FilterIdx := 1;     // filter index is one based
  fFilterString := '';
  for FT := Low(TSourceFileType) to High(TSourceFileType) do
  begin
    if not fFileTypeInfo.ContainsKey(FT) then
      Continue;
    if fFilterString <> '' then
      fFilterString := fFilterString + '|';
    fFilterString := fFilterString + Format(
      cFilterFmt, [fFileTypeInfo[FT].DisplayName, fFileTypeInfo[FT].Extension]
    );
    fFilterIdxToFileTypeMap.Add(FilterIdx, FT);
    Inc(FilterIdx);
  end;
end;

function TSourceFileInfo.GetFileTypeInfo(
  const FileType: TSourceFileType): TSourceFileTypeInfo;
begin
  Result := fFileTypeInfo[FileType];
end;

procedure TSourceFileInfo.SetDefaultFileName(const Value: string);
var
  Idx: Integer; // loops through characters of filename
begin
  if fRequirePascalDefFileName then
  begin
    // convert to "camel" case
    fDefaultFileName := StrStripWhiteSpace(StrCapitaliseWords(Value));
    // replaces invalid Pascal identifier characters with underscore
    if (fDefaultFileName <> '')
      and not TCharacter.IsLetter(fDefaultFileName[1])
      and (fDefaultFileName[1] <> '_') then
      fDefaultFileName[1] := '_';
    for Idx := 2 to Length(fDefaultFileName) do
      if not TCharacter.IsLetterOrDigit(fDefaultFileName[Idx])
        and (fDefaultFileName[Idx] <> '_') then
        fDefaultFileName[Idx] := '_';
    Assert((fDefaultFileName <> '') and IsValidIdent(fDefaultFileName),
      ClassName + '.SetFileName: Not a valid identifier');
  end
  else
    fDefaultFileName := Value;
end;

procedure TSourceFileInfo.SetFileTypeInfo(const FileType: TSourceFileType;
  const Info: TSourceFileTypeInfo);
begin
  if fFileTypeInfo.ContainsKey(FileType) then
    fFileTypeInfo[FileType] := Info
  else
    fFileTypeInfo.Add(FileType, Info);
  GenerateFilterInfo;
end;

function TSourceFileInfo.SupportsFileType(const FileType: TSourceFileType):
  Boolean;
begin
  Result := fFileTypeInfo.ContainsKey(FileType);
end;

{ TSourceFileTypeInfo }

constructor TSourceFileTypeInfo.Create(const AExtension, ADisplayName: string;
  const AEncodingTypes: array of TEncodingType);
var
  I: Integer;
begin
  fExtension := AExtension;
  fDisplayName := ADisplayName;
  SetLength(fEncodings, Length(AEncodingTypes));
  for I := 0 to Pred(Length(AEncodingTypes)) do
    fEncodings[I] := TSourceFileEncoding.Create(AEncodingTypes[I]);
end;

{ TSourceFileEncoding }

constructor TSourceFileEncoding.Create(const AEncodingType: TEncodingType);
resourcestring
  // Display names associated with each TEncodingType value
  sASCIIEncodingName = 'ASCII';
  sISO88591Name = 'ISO-8859-1';
  sUTF8Name = 'UTF-8';
  sUnicodeName = 'UTF-16';
  sUTF16BEName = 'UTF-16 Big Endian';
  sUTF16LEName = 'UTF-16 Little Endian';
  sWindows1252Name = 'Windows-1252';
  sSysDefaultName = 'ANSI Code Page %d';
begin
  fEncodingType := AEncodingType;
  case fEncodingType of
    etASCII:
      fDisplayName := sASCIIEncodingName;
    etISO88591:
      fDisplayName := sISO88591Name;
    etUTF8:
      fDisplayName := sUTF8Name;
    etUnicode:
      fDisplayName := sUnicodeName;
    etUTF16BE:
      fDisplayName := sUTF16BEName;
    etUTF16LE:
      fDisplayName := sUTF16LEName;
    etWindows1252:
      fDisplayName := sWindows1252Name;
    etSysDefault:
      fDisplayName := Format(sSysDefaultName, [ULocales.DefaultAnsiCodePage]);
    else
      fDisplayName := '';
  end;
  Assert(fDisplayName <> '',
    'TSourceFileEncoding.Create: Unrecognised encoding type');
end;

end.


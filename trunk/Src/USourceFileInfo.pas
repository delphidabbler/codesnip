{
 * USourceFileInfo.pas
 *
 * Implements class that provides information about types of source code output
 * that are supported.
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
 * The Original Code is USourceFileInfo.pas
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


unit USourceFileInfo;


interface


uses
  // Project
  UEncodings;


type
  ///  <summary>
  ///  Enumeration of file types that can be used for source code output.
  ///  </summary>
  TSourceFileType = (
    sfText,     // plain text files
    sfPascal,   // pascal files (either .pas for units or .inc for include files
    sfHTML,     // HTML files
    sfRTF       // rich text files
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
    ///  <summary>Sets values of properties.</summary>
    constructor Create(const AEncodingType: TEncodingType;
      const ADisplayName: string);
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
      const AEncodings: array of TSourceFileEncoding);
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
      //   types required by save source dialog boxes.</summary>
      fFileTypeInfo: array[TSourceFileType] of TSourceFileTypeInfo;
      //   <summary>Value of DefaultFileName property.</summary>
      fDefaultFileName: string;
    ///  <summary>Read accessor for FileTypeInfo property.</summary>
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
    ///  <summary>Builds filter string for use in open / save dialog boxes from
    ///  descriptions and file extensions of each supported file type.</summary>
    function FilterString: string;
    ///  <summary>Finds source file type associated with a file extension.
    ///  </summary>
    function FileTypeFromExt(const Ext: string): TSourceFileType;
    ///  <summary>Array of information about each supported file type that is
    ///  of use to save source dialog boxes.</summary>
    property FileTypeInfo[const FileType: TSourceFileType]: TSourceFileTypeInfo
      read GetFileTypeInfo write SetFileTypeInfo;
    ///  <summary>Default source code file name.</summary>
    ///  <remarks>Must be a valid Pascal identifier. Invalid characters are
    ///  replaced by underscores.</remarks>
    property DefaultFileName: string
      read fDefaultFileName write SetDefaultFileName;
  end;


implementation


uses
  // Delphi
  SysUtils, Windows {for inlining}, Character,
  // Project
  UStrUtils;


{ TSourceFileInfo }

function TSourceFileInfo.FileTypeFromExt(const Ext: string): TSourceFileType;
var
  FT: TSourceFileType;  // loops thru all source file types
begin
  // Assume text file type if extension not recognised
  Result := sfText;
  for FT := Low(TSourceFileType) to High(TSourceFileType) do
  begin
    if StrSameText(Ext, fFileTypeInfo[FT].Extension) then
    begin
      Result := FT;
      Break;
    end;
  end;
end;

function TSourceFileInfo.FilterString: string;
const
  cFilterFmt = '%0:s (*%1:s)|*%1:s';  // format string for creating file filter
var
  FT: TSourceFileType;  // loops thru all source file types
begin
  Result := '';
  for FT := Low(TSourceFileType) to High(TSourceFileType) do
  begin
    if Result <> '' then
      Result := Result + '|';
    Result := Result + Format(
      cFilterFmt, [fFileTypeInfo[FT].DisplayName, fFileTypeInfo[FT].Extension]
    );
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
end;

procedure TSourceFileInfo.SetFileTypeInfo(const FileType: TSourceFileType;
  const Info: TSourceFileTypeInfo);
begin
  fFileTypeInfo[FileType] := Info;
end;

{ TSourceFileTypeInfo }

constructor TSourceFileTypeInfo.Create(const AExtension, ADisplayName: string;
  const AEncodings: array of TSourceFileEncoding);
var
  I: Integer;
begin
  fExtension := AExtension;
  fDisplayName := ADisplayName;
  SetLength(fEncodings, Length(AEncodings));
  for I := 0 to Pred(Length(AEncodings)) do
    fEncodings[I] := AEncodings[I];
end;

{ TSourceFileEncoding }

constructor TSourceFileEncoding.Create(const AEncodingType: TEncodingType;
  const ADisplayName: string);
begin
  fEncodingType := AEncodingType;
  fDisplayName := ADisplayName;
end;

end.


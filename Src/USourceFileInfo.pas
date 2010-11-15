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
 * Portions created by the Initial Developer are Copyright (C) 2006-2010 Peter
 * Johnson. All Rights Reserved.
 *
 * Contributor(s)
 *   NONE
 *
 * ***** END LICENSE BLOCK *****
}


unit USourceFileInfo;


interface


type

  {
  TSourceFileType:
    Enumeration of file types that can be used for source code output.
  }
  TSourceFileType = (
    sfText,     // plain text files
    sfPascal,   // pascal files (either .pas for units or .inc for include files
    sfHTML,     // HTML files
    sfRTF       // rich text files
  );

  {
  TSourceFileInfo:
    Class that provides information about types of source code output that are
    supported.
  }
  TSourceFileInfo = class(TObject)
  private
    fInfo: array[TSourceFileType] of record
      Desc: string; // file description
      Ext: string;  // file extension
    end;
    fFileName: string;
      {Stores description and file extension associated with each type of source
      file type}
    function GetDescription(FT: TSourceFileType): string;
      {Read accessor for Descriptions property.
        @param FT [in] File type for which we need description.
        @return Required description.
      }
    function GetFileExtension(FT: TSourceFileType): string;
      {Read accessor for FileExtensions property.
        @param FT [in] File type for which we need extension.
        @return Required extension.
      }
    procedure SetDescription(FT: TSourceFileType; const Value: string);
      {Write accessor for Descriptions property.
        @param FT [in] File type for which we are setting description.
        @param Value [in] New property value.
      }
    procedure SetFileExtension(FT: TSourceFileType; const Value: string);
      {Write accessor for FileExtensions property.
        @param FT [in] File type for which we are setting extension.
        @param Value [in] New property value.
      }
    procedure SetFileName(const Value: string);
      {Write accessor for FileName property. Converts assigned value into a
      valid Pascal identifier if necessary.
        @param Value [in] New property value.
      }
  public
    function FilterString: string;
      {Builds filter string for use in open / save dialog boxes from
      descriptions and file extensions of each supported file type.
        @return Required filter string.
      }
    function FileTypeFromExt(const Ext: string): TSourceFileType;
      {Finds source file type associated with a file extension.
        @param Ext [in] File extension for which we want source file type.
        @return Source file type. sfText if extension not recognised.
      }
    property Descriptions[FT: TSourceFileType]: string
      read GetDescription write SetDescription;
      {Description of each supported source file type}
    property FileExtensions[FT: TSourceFileType]: string
      read GetFileExtension write SetFileExtension;
      {File extensions of each supported source file type}
    property FileName: string
      read fFileName write SetFileName;
      {Default source code file name. Must be valid Pascal identifier: converted
      to valid character if necessary}
  end;


implementation


uses
  // Delphi
  SysUtils, Windows {for inlining}, Character,
  // Project
  UUtils;


{ TSourceFileInfo }

function TSourceFileInfo.FileTypeFromExt(const Ext: string): TSourceFileType;
  {Finds source file type associated with a file extension.
    @param Ext [in] File extension for which we want source file type.
    @return Source file type. sfText if extension not recognised.
  }
var
  FT: TSourceFileType;  // loops thru all source file types
begin
  // Assume text file type if extension not recognised
  Result := sfText;
  for FT := Low(TSourceFileType) to High(TSourceFileType) do
  begin
    if AnsiSameText(Ext, fInfo[FT].Ext) then
    begin
      Result := FT;
      Break;
    end;
  end;
end;

function TSourceFileInfo.FilterString: string;
  {Builds filter string for use in open / save dialog boxes from descriptions
  and file extensions of each supported file type.
    @return Required filter string.
  }
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
      cFilterFmt, [Descriptions[FT], FileExtensions[FT]]
    );
  end;
end;

function TSourceFileInfo.GetDescription(FT: TSourceFileType): string;
  {Read accessor for Descriptions property.
    @param FT [in] File type for which we need description.
    @return Required description.
  }
begin
  Result := fInfo[FT].Desc;
end;

function TSourceFileInfo.GetFileExtension(FT: TSourceFileType): string;
  {Read accessor for FileExtensions property.
    @param FT [in] File type for which we need extension.
    @return Required extension.
  }
begin
  Result := fInfo[FT].Ext;
end;

procedure TSourceFileInfo.SetDescription(FT: TSourceFileType;
  const Value: string);
  {Write accessor for Descriptions property.
    @param FT [in] File type for which we are setting description.
    @param Value [in] New property value.
  }
begin
  fInfo[FT].Desc := Value;
end;

procedure TSourceFileInfo.SetFileExtension(FT: TSourceFileType;
  const Value: string);
  {Write accessor for FileExtensions property.
    @param FT [in] File type for which we are setting extension.
    @param Value [in] New property value.
  }
begin
  fInfo[FT].Ext := Value;
end;

procedure TSourceFileInfo.SetFileName(const Value: string);
  {Write accessor for FileName property. Converts assigned value into a valid
  Pascal identifier if necessary.
    @param Value [in] New property value.
  }
var
  Idx: Integer; // loops through characters of filename
begin
  // convert to "camel" case
  fFileName := StripWhiteSpace(CapitaliseWords(Value));
  // replaces invalid Pascal identifier characters with underscore
  if (fFileName <> '')
    and not TCharacter.IsLetter(fFileName[1]) and (fFileName[1] <> '_') then
    fFileName[1] := '_';
  for Idx := 2 to Length(fFileName) do
    if not TCharacter.IsLetterOrDigit(fFileName[Idx])
      and (fFileName[Idx] <> '_') then
      fFileName[Idx] := '_';
  Assert((fFileName <> '') and IsValidIdent(fFileName),
    ClassName + '.SetFileName: Not a valid identifier');
end;

end.


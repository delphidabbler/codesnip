{
 * This Source Code Form is subject to the terms of the Mozilla Public License,
 * v. 2.0. If a copy of the MPL was not distributed with this file, You can
 * obtain one at https://mozilla.org/MPL/2.0/
 *
 * Copyright (C) 2024, Peter Johnson (gravatar.com/delphidabbler).
 *
 * Class that read and write category information from and to files.
}

unit DB.IO.Categories;

interface

uses
  // Delphi
  SysUtils,
  Generics.Collections,
  // Project
  DB.UCategory,
  UExceptions,
  UTabSeparatedFileIO;

type
  ///  <summary>Base class for category storage reader and writer classes.
  ///  </summary>
  TCategoryStorage = class abstract(TObject)
  strict protected
    const
      ///  <summary>Watermark that is present on the first line of a valid
      ///  categories file.</summary>
      Watermark = #$25BA + ' CodeSnip Categories v1 ' + #$25C4;
  end;

  ///  <summary>Reads category information from storage.</summary>
  TCategoryStorageReader = class sealed(TCategoryStorage)
  public
    type
      ///  <summary>Key / Value pair associating the category ID (key) with the
      ///  category data (value).</summary>
      TCategoryIDAndData = TPair<string,TCategoryData>;
  strict private
    var
      ///  <summary>Object that read data from a tab delimited UTF8 text file.
      ///  </summary>
      fFileReader: TTabSeparatedReader;
      ///  <summary>List of category data read from file.</summary>
      fCatData: TList<TCategoryIDAndData>;
    ///  <summary>Parses fields that have been split out from each text line.
    ///  </summary>
    ///  <exception><c>ECategoryStorageReader</c> raised if the fields are not
    ///  valid.</exception>
    procedure ParseFields(AFields: TArray<string>);
  public
    ///  <summary>Creates object to read from file <c>AFileName</c>.</summary>
    constructor Create(const AFileName: string);
    ///  <summary>Object destructor.</summary>
    destructor Destroy; override;
    ///  <summary>Reads data about each category defined in file.</summary>
    ///  <returns><c>TArray&lt;TCategoryIDAndData&gt;</c>. Array of category
    ///  data.</returns>
    ///  <exception><c>ECategoryStorageReader</c> raised if the file can't be
    ///  read or if its contents are invalid.</exception>
    function Read: TArray<TCategoryIDAndData>;
  end;

  ///  <summary>Class of exception raised by <c>TCategoryStorageReader</c>.
  ///  </summary>
  ECategoryStorageReader = class(ECodeSnip);

  ///  <summary>Writes category information to storage.</summary>
  TCategoryStorageWriter = class sealed(TCategoryStorage)
  strict private
    var
      ///  <summary>Object that writes data to a tab delimited UTF8 text file.
      ///  </summary>
      fFileWriter: TTabSeparatedFileWriter;
  public
    ///  <summary>Creates object to write to file <c>AFileName</c>.</summary>
    constructor Create(const AFileName: string);
    ///  <summary>Object destructor.</summary>
    destructor Destroy; override;
    ///  <summary>Writes information about each category in <c>ACategoryList</c>
    ///  </summary>
    procedure Write(const ACategoryList: TCategoryList);
  end;

implementation

uses
  // Project
  UStrUtils;

{ TCategoryStorageReader }

constructor TCategoryStorageReader.Create(const AFileName: string);
begin
  Assert(not StrIsEmpty(AFileName), ClassName + '.Create: AFileName is empty');
  inherited Create;
  fFileReader := TTabSeparatedReader.Create(AFileName, Watermark);
  fCatData := TList<TCategoryIDAndData>.Create;
end;

destructor TCategoryStorageReader.Destroy;
begin
  fCatData.Free;
  fFileReader.Free;
  inherited;
end;

procedure TCategoryStorageReader.ParseFields(AFields: TArray<string>);
resourcestring
  sMalformedLine = 'Malformed line in categories file';
var
  CatID: string;
  Data: TCategoryData;
begin
  if Length(AFields) <> 2 then
    raise ECategoryStorageReader.Create(sMalformedLine);
  if StrIsEmpty(AFields[0]) or StrIsEmpty(AFields[1]) then
    raise ECategoryStorageReader.Create(sMalformedLine);
  CatID := StrTrim(AFields[0]);
  Data.Init;
  Data.Desc := StrTrim(AFields[1]);
  fCatData.Add(TCategoryIDAndData.Create(CatID, Data));
end;

function TCategoryStorageReader.Read: TArray<TCategoryIDAndData>;
begin
  fCatData.Clear;
  try
    fFileReader.Read(ParseFields);
  except
    on E: ETabSeparatedReader do
      raise ECategoryStorageReader.Create(E);
    else
      raise;
  end;
  Result := fCatData.ToArray;
end;

{ TCategoryStorageWriter }

constructor TCategoryStorageWriter.Create(const AFileName: string);
begin
  Assert(not StrIsEmpty(AFileName), ClassName + '.Create: AFileName is empty');
  inherited Create;
  fFileWriter := TTabSeparatedFileWriter.Create(AFileName, Watermark);
end;

destructor TCategoryStorageWriter.Destroy;
begin
  fFileWriter.Free;
  inherited;
end;

procedure TCategoryStorageWriter.Write(const ACategoryList: TCategoryList);
var
  Cat: TCategory;
begin
  for Cat in ACategoryList do
    fFileWriter.WriteLine(TArray<string>.Create(Cat.ID, Cat.Description));
end;

end.

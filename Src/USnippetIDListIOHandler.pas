{
 * This Source Code Form is subject to the terms of the Mozilla Public License,
 * v. 2.0. If a copy of the MPL was not distributed with this file, You can
 * obtain one at https://mozilla.org/MPL/2.0/
 *
 * Copyright (C) 2013-2021, Peter Johnson (gravatar.com/delphidabbler).
 *
 * Provides objects that can read and write files containing snippet ID lists.
}


unit USnippetIDListIOHandler;


interface


uses
  // Delphi
  SysUtils,
  // Project
  UExceptions,
  USnippetIDs;


type

  TSnippetIDListFileReader = class(TObject)
  strict private
    var
      fWatermark: string;
      fSnippetIDs: ISnippetIDList;
    procedure ParseLine(AFields: TArray<string>);
  public
    constructor Create(const Watermark: string);
    function ReadFile(const FileName: string): ISnippetIDLIst;
  end;

type

  ESnippetIDListFileReader = class(ECodeSnip);

type

  TSnippetIDListFileWriter = class(TObject)
  strict private
    var
      fWatermark: string;
      fBuilder: TStringBuilder;
  public
    constructor Create(const Watermark: string);
    destructor Destroy; override;
    procedure WriteFile(const FileName: string; SnippetIDs: ISnippetIDList);
  end;

type

  ESnippetIDListFileWriter = class(ECodeSnip);


implementation


uses
  // Delphi
  Classes,
  // Project
  DB.UCollections,
  UStrUtils,
  UTabSeparatedFileIO;


{ TSnippetIDListFileReader }

constructor TSnippetIDListFileReader.Create(const Watermark: string);
begin
  inherited Create;
  fSnippetIDs := TSnippetIDList.Create;
  fWatermark := Watermark;
end;

procedure TSnippetIDListFileReader.ParseLine(AFields: TArray<string>);
resourcestring
  sBadFileFormat = 'Invalid snippet ID list file format';
var
  Key: string;
  CollectionHex: string;
  CollectionID: TCollectionID;
begin
  Key := StrTrim(AFields[0]);
  if Key = '' then
    raise ESnippetIDListFileReader.Create(sBadFileFormat);
  CollectionHex := StrTrim(AFields[1]);
  if CollectionHex = '' then
    raise ESnippetIDListFileReader.Create(sBadFileFormat);
  CollectionID := TCollectionID.CreateFromHexString(CollectionHex);
  fSnippetIDs.Add(TSnippetID.Create(Key, CollectionID));
end;

function TSnippetIDListFileReader.ReadFile(const FileName: string):
  ISnippetIDLIst;
var
  TSVReader: TTabSeparatedReader;
begin
  fSnippetIDs.Clear;
  TSVReader := TTabSeparatedReader.Create(FileName, fWatermark);
  try
    try
      TSVReader.Read(ParseLine);
    except
      on E: ETabSeparatedReader do
        raise ESnippetIDListFileReader.Create(E);
      on E: ECollectionID do
        raise ESnippetIDListFileReader.Create(E);
      else
        raise;
    end;
  finally
    TSVReader.Free;
  end;
  Result := fSnippetIDs;
end;

{ TSnippetIDListFileWriter }

constructor TSnippetIDListFileWriter.Create(const Watermark: string);
begin
  inherited Create;
  fWatermark := Watermark;
end;

destructor TSnippetIDListFileWriter.Destroy;
begin
  fBuilder.Free;
  inherited;
end;

procedure TSnippetIDListFileWriter.WriteFile(const FileName: string;
  SnippetIDs: ISnippetIDList);
var
  TSVWriter: TTabSeparatedFileWriter;
  SnippetID: TSnippetID;
begin
  TSVWriter := TTabSeparatedFileWriter.Create(FileName, fWaterMark);
  try
    try
      for SnippetID in SnippetIDs do
        TSVWriter.WriteLine([SnippetID.Key, SnippetID.CollectionID.ToHexString]);
    finally
      TSVWriter.Free;
    end;
  except
    on E: EStreamError do
      raise ESnippetIDListFileWriter.Create(E);
    else
      raise;
  end;
end;

end.

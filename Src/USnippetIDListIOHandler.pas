{
 * This Source Code Form is subject to the terms of the Mozilla Public License,
 * v. 2.0. If a copy of the MPL was not distributed with this file, You can
 * obtain one at http://mozilla.org/MPL/2.0/
 *
 * Copyright (C) 2013-2016, Peter Johnson (www.delphidabbler.com).
 *
 * $Rev$
 * $Date$
 *
 * Provides objects that can read and write files containing snippet ID lists.
}


unit USnippetIDListIOHandler;


interface


uses
  // Delphi
  SysUtils,
  // Project
  CS.Database.Types,
  UExceptions,
  UIStringList;


type

  TSnippetIDListFileReader = class(TObject)
  strict private
    var
      fWatermark: string;
      fSnippetIDs: ISnippetIDList;
      ///  <summary>Lines of text from file.</summary>
      ///  <remarks>Must be stripped of blank lines.</remarks>
      fLines: IStringList;
    ///  <summary>Parses watermark and non-blank lines in file.</summary>
    ///  <remarks>Can handle both old-style files (that had a tab-separated
    ///  snippet ID and a user-defined flag on each line) and new-style files
    ///  that simply have one snippet ID per line.</remarks>
    procedure Parse;
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
    procedure CreateContent(const SnippetIDs: ISnippetIDList);
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
  CS.Database.Snippets,
  UConsts,
  UIOUtils,
  UStrUtils;


{ TSnippetIDListFileReader }

constructor TSnippetIDListFileReader.Create(const Watermark: string);
begin
  inherited Create;
  fSnippetIDs := TSnippetIDList.Create;
  fLines := TIStringList.Create;
  fWatermark := Watermark;
end;

procedure TSnippetIDListFileReader.Parse;
resourcestring
  sBadFileFormat = 'Invalid snippet ID list file format';
  sMissingName = 'Snippet name missing on line' + EOL2 + '"%s"';
  sBadSnippetID = 'Malformed snippet ID' + EOL2 + '"%s"';
var
  Line: string;       // each line in fLines
  Name: string;       // name of each snippet
begin
  fSnippetIDs.Clear;
  if (fLines.Count <= 1) or (fLines[0] <> fWatermark) then
    raise ESnippetIDListFileReader.Create(sBadFileFormat);
  fLines.Delete(0);
  for Line in fLines do
  begin
    Name := StrTrim(Line);
    if Name = '' then
      raise ESnippetIDListFileReader.CreateFmt(sMissingName, [Line]);
    if not TSnippetID.IsValidIDString(Name) then
      raise ESnippetIDListFileReader.CreateFmt(sBadSnippetID, [Name]);
    fSnippetIDs.Add(TSnippetID.Create(Name));
  end;
end;

function TSnippetIDListFileReader.ReadFile(const FileName: string):
  ISnippetIDLIst;
begin
  try
    fLines.SetText(
      TFileIO.ReadAllText(FileName, TEncoding.UTF8, True),
      CRLF,
      False,
      True
    );
  except
    on E: EStreamError do
      raise ESnippetIDListFileReader.Create(E);
    on E: EIOUtils do
      raise ESnippetIDListFileReader.Create(E);
    else
      raise;
  end;
  Parse;
  Result := fSnippetIDs;
end;

{ TSnippetIDListFileWriter }

constructor TSnippetIDListFileWriter.Create(const Watermark: string);
begin
  inherited Create;
  fBuilder := TStringBuilder.Create;
  fWatermark := Watermark;
end;

procedure TSnippetIDListFileWriter.CreateContent(
  const SnippetIDs: ISnippetIDList);
var
  SnippetID: TSnippetID;
begin
  fBuilder.Clear;
  fBuilder.AppendLine(fWatermark);
  // One snippet ID is written per line
  for SnippetID in SnippetIDs do
    fBuilder.AppendLine(SnippetID.ToString);
end;

destructor TSnippetIDListFileWriter.Destroy;
begin
  fBuilder.Free;
  inherited;
end;

procedure TSnippetIDListFileWriter.WriteFile(const FileName: string;
  SnippetIDs: ISnippetIDList);
begin
  CreateContent(SnippetIDs);
  try
    TFileIO.WriteAllText(FileName, fBuilder.ToString, TEncoding.UTF8, True);
  except
    on E: EStreamError do
      raise ESnippetIDListFileWriter.Create(E);
    else
      raise;
  end;
end;

end.

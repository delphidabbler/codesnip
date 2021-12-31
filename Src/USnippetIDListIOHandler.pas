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
  SysUtils,
  UExceptions, UIStringList, USnippetIDs;


type

  TSnippetIDListFileReader = class(TObject)
  strict private
    var
      fWatermark: string;
      fSnippetIDs: ISnippetIDList;
      ///  <summary>Lines of text from file.</summary>
      ///  <remarks>Must be stripped of blank lines.</remarks>
      fLines: IStringList;
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
  UConsts, UIOUtils, UStrUtils;


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
  sMissingUserDef = 'Snippet database specifier missing on line'
    + EOL2 + '"%s"';
  sBadUserDef = 'Unknown snippet database specifier on line'
    + EOL2 + '"%s"';
var
  Line: string;         // each line in fLines
  Name: string;         // name of each snippet
  UserDefStr: string;   // user defined value of each snippet as string
  UserDefInt: Integer;  // user defined value of each snippet as integer
begin
  fSnippetIDs.Clear;
  if (fLines.Count <= 1) or (fLines[0] <> fWatermark) then
    raise ESnippetIDListFileReader.Create(sBadFileFormat);
  fLines.Delete(0);
  for Line in fLines do
  begin
    StrSplit(Line, TAB, Name, UserDefStr);
    Name := StrTrim(Name);
    UserDefStr := StrTrim(UserDefStr);
    if Name = '' then
      raise ESnippetIDListFileReader.CreateFmt(sMissingName, [Line]);
    if UserDefStr = '' then
      raise ESnippetIDListFileReader.CreateFmt(sMissingUserDef, [Line]);
    if not TryStrToInt(UserDefStr, UserDefInt)
      or not (UserDefInt in [0, 1]) then
      raise ESnippetIDListFileReader.CreateFmt(sBadUserDef, [Line]);
    fSnippetIDs.Add(TSnippetID.Create(Name, Boolean(UserDefInt)));
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
  for SnippetID in SnippetIDs do
  begin
    fBuilder.Append(SnippetID.Name);
    fBuilder.Append(TAB);
    // NOTE: TStringBuilder.Append(Boolean) override not used here since ordinal
    // value wanted instead of "True" or "False" or localised equivalent.
    fBuilder.Append(Ord(SnippetID.UserDefined));
    fBuilder.AppendLine;
  end;
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

{
 * This Source Code Form is subject to the terms of the Mozilla Public License,
 * v. 2.0. If a copy of the MPL was not distributed with this file, You can
 * obtain one at http://mozilla.org/MPL/2.0/
 *
 * Copyright (C) 2012, Peter Johnson (www.delphidabbler.com).
 *
 * $Rev$
 * $Date$
 *
 * Provides objects that can read and write snippet selection files.
}


unit USelectionIOHandler;


interface


uses
  // Delphi
  SysUtils,
  // Project
  UExceptions, UIStringList, USnippetIDs;


type

  TSelectionFileReader = class(TObject)
  strict private
    var
      fSnippetIDs: ISnippetIDList;
      ///  <summary>Lines of text from file.</summary>
      ///  <remarks>Must be stripped of blank blanks.</remarks>
      fLines: IStringList;
    procedure Parse;
  public
    constructor Create;
    function ReadFile(const FileName: string): ISnippetIDLIst;
  end;

type

  ESelectionFileReader = class(ECodeSnip);

type

  TSelectionFileWriter = class(TObject)
  strict private
    var
      fBuilder: TStringBuilder;
    procedure CreateContent(const SnippetIDs: ISnippetIDList);
  public
    constructor Create;
    destructor Destroy; override;
    procedure WriteFile(const FileName: string; SnippetIDs: ISnippetIDList);
  end;


implementation


uses
  // Project
  UConsts, UIOUtils, UStrUtils;


const
  ///  <summary>File watermark. Uses characters that will be interpreted wrongly
  ///  if not UTF8 format.</summary>
  SelectionFileWatermark = #$25BA + ' CodeSnip Selections v1 ' + #$25C4;

{ TSelectionFileReader }

constructor TSelectionFileReader.Create;
begin
  inherited Create;
  fSnippetIDs := TSnippetIDList.Create;
  fLines := TIStringList.Create;
end;

procedure TSelectionFileReader.Parse;
resourcestring
  sBadFileFormat = 'Invalid selection file format';
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
  if (fLines.Count <= 1) or (fLines[0] <> SelectionFileWatermark) then
    raise ESelectionFileReader.Create(sBadFileFormat);
  fLines.Delete(0);
  for Line in fLines do
  begin
    StrSplit(Line, TAB, Name, UserDefStr);
    Name := StrTrim(Name);
    UserDefStr := StrTrim(Name);
    if Name = '' then
      raise ESelectionFileReader.CreateFmt(sMissingName, [Line]);
    if UserDefStr = '' then
      raise ESelectionFileReader.CreateFmt(sMissingUserDef, [Line]);
    if not TryStrToInt(UserDefStr, UserDefInt)
      and not (UserDefInt in [0, 1]) then
      raise ESelectionFileReader.CreateFmt(sBadUserDef, [Line]);
    fSnippetIDs.Add(TSnippetID.Create(Name, Boolean(UserDefInt)));
  end;
end;

function TSelectionFileReader.ReadFile(const FileName: string): ISnippetIDLIst;
begin
  fLines.SetText(
    TFileIO.ReadAllText(FileName, TEncoding.UTF8, True),
    CRLF,
    False,
    True
  );
  Parse;
  Result := fSnippetIDs;
end;

{ TSelectionFileWriter }

constructor TSelectionFileWriter.Create;
begin
  inherited Create;
  fBuilder := TStringBuilder.Create;
end;

procedure TSelectionFileWriter.CreateContent(const SnippetIDs: ISnippetIDList);
var
  SnippetID: TSnippetID;
begin
  fBuilder.Clear;
  fBuilder.AppendLine(SelectionFileWatermark);
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

destructor TSelectionFileWriter.Destroy;
begin
  fBuilder.Free;
  inherited;
end;

procedure TSelectionFileWriter.WriteFile(const FileName: string;
  SnippetIDs: ISnippetIDList);
begin
  CreateContent(SnippetIDs);
  TFileIO.WriteAllText(FileName, fBuilder.ToString, TEncoding.UTF8, True);
end;

end.


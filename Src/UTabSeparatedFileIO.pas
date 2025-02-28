{
 * This Source Code Form is subject to the terms of the Mozilla Public License,
 * v. 2.0. If a copy of the MPL was not distributed with this file, You can
 * obtain one at https://mozilla.org/MPL/2.0/
 *
 * Copyright (C) 2024, Peter Johnson (gravatar.com/delphidabbler).
 *
 * Classes that can read and write UTF8 files containing a watermark line
 * followed by lines of tabbed separated values.
}

unit UTabSeparatedFileIO;

interface

uses
  // Delphi
  SysUtils,
  // Project
  UExceptions,
  UIStringList;

type

  ///  <summary>Class that writes UTF8 files beginning with a watermark line
  ///  followed by and blank line then lines of tabbed separated values.
  ///  </summary>
  TTabSeparatedFileWriter = class(TObject)
  strict private
    var
      fFileName: string;
      fBuilder: TStringBuilder;
    ///  <summary>Convert any characters in <c>AValue</c> that can't appear in a
    ///  field into C-style escape sequences.</summary>
    function Escape(const AValue: string): string;
    ///  <summary>Outputs the watermark <c>AWatermark</c> on a line on its own.
    ///  </summary>
    procedure WriteWatermark(const AWaterMark: string);
  public
    ///  <summary>Creates object to write file <c>AFileName</c> with watermark
    ///  <c>AWatermark</c>.</summary>
    constructor Create(const AFileName: string; const AWatermark: string);
    ///  <summary>Object destructor.</summary>
    destructor Destroy; override;
    ///  <summary>Writes data to file just before the object is destroyed.
    ///  </summary>
    procedure BeforeDestruction; override;
    ///  <summary>Writes a line of data with tabbed separated fields.</summary>
    ///  <param name="AFields"><c>array of string</c> [in] Fields to be written.
    ///  </param>
    procedure WriteLine(const AFields: array of string);
  end;

  ///  <summary>Class that reads UTF8 files beginning with a watermark line
  ///  followed by lines of tabbed separated values.</summary>
  ///  <remarks>Any blank lines are ignored.</remarks>
  TTabSeparatedReader = class(TObject)
  public
    type
      ///  <summary>Type of callback trigged when each line is pardsed.
      ///  </summary>
      ///  <param name="AFields"><c>TArray&lt;string&gt;</c> [in] Fields defined
      ///  in the line.</param>
      TLineCallback = reference to procedure(AFields: TArray<string>);
  strict private
    var
      fFileName: string;
      fWatermark: string;
      fLines: IStringList;
    ///  <summary>Un-escapes any a C style escape sequences in <c>AValue</c>.
    ///  </summary>
    function UnEscape(const AValue: string): string;
    ///  <summary>Reads file and splits into lines.</summary>
    ///  <exception><c>ETabSeparatedReader</c> raised if file can't be read.
    ///  </exception>
    procedure ReadFile;
    ///  <summary>Removes all blank lines from the data.</summary>
    procedure PreProcess;
    ///  <summary>Parses the data. See the <c>Read</c> method for details of the
    ///  <c>ALineCallback</c> parameter.</summary>
    procedure Parse(const ALineCallback: TLineCallback);
  public
    ///  <summary>Creates object to read file <c>AFileName</c> which must begin
    ///  with watermark <c>AWatermark</c>.</summary>
    constructor Create(const AFileName: string; const AWatermark: string);
    ///  <summary>Reads all data from the file.</summary>
    ///  <param name="ALineCallback"><c>TLineCallback</c> [in] Callback called
    ///  for each line of data read. Passed an array of fields read from the
    ///  line. Caller must process the fields in this callback.</param>
    ///  <exception><c>ETabSeparatedReader</c> raised if file can't be read or
    ///  if watermark is not valid.</exception>
    procedure Read(const ALineCallback: TLineCallback);
  end;

  ETabSeparatedReader = class(ECodeSnip);

implementation

uses
  // Delphi
  Classes,
  // Project
  UConsts,
  UIOUtils,
  UStrUtils;

const
  EscapeableChars = TAB + CR + LF;
  EscapedChars = 'trn';

{ TTabSeparatedFileWriter }

procedure TTabSeparatedFileWriter.BeforeDestruction;
begin
  inherited;
  TFileIO.WriteAllText(fFileName, fBuilder.ToString, TEncoding.UTF8, True);
end;

constructor TTabSeparatedFileWriter.Create(const AFileName, AWatermark: string);
begin
  Assert(not StrIsEmpty(AFileName), ClassName + '.Create: AFileName is empty');
  Assert(not StrIsEmpty(AWatermark, True),
    ClassName + '.Create: AWatermark is empty');

  inherited Create;
  fFileName := StrTrim(AFileName);
  fBuilder := TStringBuilder.Create;
  WriteWatermark(AWaterMark);
end;

destructor TTabSeparatedFileWriter.Destroy;
begin
  fBuilder.Free;
  inherited;
end;

function TTabSeparatedFileWriter.Escape(const AValue: string): string;
begin
  // Perform character escaping per
  // https://en.wikipedia.org/wiki/Tab-separated_values
  Result := StrCEscapeStr(AValue, EscapedChars, EscapeableChars);
end;

procedure TTabSeparatedFileWriter.WriteLine(const AFields: array of string);
var
  Data: TStringList;
  Item: string;
begin
  Data := TStringList.Create;
  try
    // escape each data item in array
    for Item in AFields do
      Data.Add(Escape(Item));
    fBuilder.AppendLine(StrJoin(Data, TAB, True));
  finally
    Data.Free;
  end;
end;

procedure TTabSeparatedFileWriter.WriteWatermark(const AWaterMark: string);
begin
  if StrTrim(AWaterMark) <> '' then
  begin
    fBuilder.AppendLine(AWaterMark);
    fBuilder.AppendLine;
  end;
end;

{ TTabSeparatedReader }

constructor TTabSeparatedReader.Create(const AFileName, AWatermark: string);
begin
  Assert(not StrIsEmpty(AFileName), ClassName + '.Create: AFileName is empty');
  Assert(not StrIsEmpty(AWatermark, True),
    ClassName + '.Create: AWatermark is empty');

  inherited Create;
  fFileName := StrTrim(AFileName);
  fWatermark := StrTrim(AWatermark);
  fLines := TIStringList.Create;
end;

procedure TTabSeparatedReader.Parse(const ALineCallback: TLineCallback);
resourcestring
  sBadFileFormat = 'Invalid tab separated list file format';
var
  Line: string;
  Fields: TStringList;
  Idx: Integer;
begin
  // check for watermark in 1st line
  if (fLines.Count < 1) or (fLines[0] <> fWatermark) then
    raise ETabSeparatedReader.Create(sBadFileFormat);
  // delete watermark line
  fLines.Delete(0);
  for Line in fLines do
  begin
    Fields := TStringList.Create;
    try
      StrExplode(Line, TAB, Fields);
      for Idx := 0 to Pred(Fields.Count) do
        Fields[Idx] := UnEscape(Fields[Idx]);
      ALineCallback(Fields.ToStringArray);
    finally
      Fields.Free;
    end;
  end;
end;

procedure TTabSeparatedReader.PreProcess;
var
  Idx: Integer;
begin
  for Idx := Pred(fLines.Count) downto 0 do
    if StrIsEmpty(fLines[Idx], True) then
      fLines.Delete(Idx);
end;

procedure TTabSeparatedReader.ReadFile;
begin
  try
    fLines.SetText(
      TFileIO.ReadAllText(fFileName, TEncoding.UTF8, True), CRLF, False, True
    );
  except
    on E: EStreamError do
      raise ETabSeparatedReader.Create(E);
    on E: EIOUtils do
      raise ETabSeparatedReader.Create(E);
    else
      raise;
  end;
end;

procedure TTabSeparatedReader.Read(const ALineCallback: TLineCallback);
begin
  ReadFile;
  Preprocess;
  Parse(ALineCallback);
end;

function TTabSeparatedReader.UnEscape(const AValue: string): string;
begin
  // Perform character un-escaping per
  // https://en.wikipedia.org/wiki/Tab-separated_values
  Result := StrCUnEscapeStr(AValue, EscapedChars, EscapeableChars);
end;

end.


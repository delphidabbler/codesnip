{
  This Source Code Form is subject to the terms of the Mozilla Public License,
  v. 2.0. If a copy of the MPL was not distributed with this file, You can
  obtain one at https://mozilla.org/MPL/2.0/

  Copyright (C) 2024, Peter Johnson (gravatar.com/delphidabbler).

  Class that encapsulate data in ini file format.

  Note that the Delphi RTL IniFile implementation is not suitable for this
  purpose since we need an ini file format slightly different to that supported
  by the RTL. Further, the RTL only supports reading / writing ini data from
  and to files, where we require the ability to read & parse / write ini data
  data from and to non-file data storage.
}


//  Ini data format
//  ===============
//
//  This format consists of zero or more lines of text. Each line is either
//  an ini data statement, a blank line or a comment line.
//  A blank line is a line that is either empty or contains only white space.
//  A comment line is a line whose first non white space character is a
//  semicolor, followed by zero or more characters of any kind.
//  Blank and comment lines are ignored.
//
//  The following description assumes that all blank and comment lines have been
//  stripped away and that the resulting data is non-empty.
//  {U} is the set of all Unicode characters
//  <UP> any Unicode printable characters
//  <UW> any Unicode non-control white space characters
//  <UP-"x"> is any Unicode printable character except character x
//
//  data := section [ eol data ]
//  section := section-statement [ key-value-statements ]
//  section-statement := "[" [white-space] section-id [white-space] "]" eol
//  section-id := identifier
//  key-value-statements := key-value-statement [ key-value-statements ]
//  key-value-statement := key-id "=" [ value-part ]  eol
//  key-id := <UP-";"> [ [ white-space] identifier ]
//  value-part := [ white-space ] value | quoted-value [ white-space ]
//  value := printable-text [white-space printable-text]
//  quoted-value := """ [white-space] value [white-space] """
//  identifier := printable-text [white-space printable-text]
//  printable-text := <UP> [ printable-text ]
//  white-space := <UW> [ white-space ]
//
//  Here is an example of valid ini data (lines begin at column 7)
//
//    ; Comment
//
//    [   Top section  ]
//    Alice = FOO
//    Bob = "   BAR with spaces "
//    Charlie = "" quoted string""
//    [empty]
//
//    [©]
//    Date=2024-10-17 23:12:00
//
//          ;
//          ; Indented comment
//    [$last_section$]
//
//    ;Ignore=This value
//      Alice    = Baz is here
//    MissingValue    =
//    question? = 42
//    the_answer=56
//    quoted_num="666"
//
// After stripping out blank and comment lines we get the following data (ignore
// the angle brackets entries: they are there to delimit the string to
// make spaces visible):
//
//    | Section          | Key            | Value                 |
//    |------------------|----------------|-----------------------|
//    | <Top section>    | <Alice>        | <FOO>                 |
//    | <Top section>    | <Bob>          | <   BAR with spaces > |
//    | <Top section>    | <Charlie>      | <" quoted string">    |
//    | <empty> †        | --             | --                    |
//    | <©>              | <Date>         | <2024-10-17 23:12:00> |
//    | <$last_section$> | <Alice>        | <Baz is here>         |
//    | <$last_section$> | <MissingValue> | <>                    |
//    | <$last_section$> | <question?>    | <42>                  |
//    | <$last_section$> | <the_answer>   | <56>                  |
//    | <$last_section$> | <quoted_num>   | <666>                 |
//    |------------------|----------------|-----------------------|
//
// † Although section "empty" has no key-value pairs the section itself will be
//   recored.
//
// Key/value pairs should be unique within a section. Where there are duplicates
// the value of later keys will overwrite earlier values.
//
// Section identifiers should also be unique. Where the are duplicate sections
// all Key/value pairs are merged, with values of any duplicated keys
// overwriting the earlier values.
//
// The specification ignores spaces surrounding section identifiers, key
// identifiers and values. Spaces around values can be maintained by enclosing
// the value in double quotes.

unit CSLE.IniData;

interface

uses
  System.Classes,
  System.IniFiles,
  System.Generics.Collections,
  CSLE.Exceptions;

type
  TIniData = class(TObject)
  strict private
    const
      CommentOpener = ';';
      SectionOpener = '[';
      SectionCloser = ']';
      KeyValueSeparator = '=';
      BooleanValueNames: array[Boolean] of string = ('False','True');
    type
      TSectionData = class (TDictionary<string,string>)
      public
        constructor Create;
      end;
      TSections = class(TObjectDictionary<string,TSectionData>)
      public
        constructor Create;
      end;
      TIniDataParser = class(TObject)
      strict private
        var
          fIniData: TIniData;
          fLines: TStringList;
        function IsEndOfList(const LineIdx: Integer): Boolean;
        ///  <summary>Trim lines and remove empty lines and comments.</summary>
        procedure PreprocessLines;
        procedure ParseSections;
        procedure ParseSectionContent(const SectionName: string;
          var LineIdx: Integer);
      public
        constructor Create(const AIniData: TIniData);
        destructor Destroy; override;
        procedure Parse(const AStr: string);
      end;
    var
      fSections: TSections;
    procedure Parse(const AStr: string);
    function DequoteString(const S: string): string;
//    function EnquoteString(const S: string): string;
    function InternalRead(const ASection, AKey: string): string;
    function InternalReadDequotedString(const ASection, AKey: string): string;
    procedure InternalWrite(const ASection, AKey, AValue: string);
//    procedure InternalWriteEnquotedString(const ASection, AKey, AValue: string);
    procedure EnsureSectionExists(const ASection: string);
    class function IsValidSectionOrKeyName(const AName: string): Boolean;
  public
    constructor Create;
    destructor Destroy; override;

    class function IsValidSectionName(const AName: string): Boolean;
    class function IsValidKeyName(const AName: string): Boolean;

    procedure LoadFromString(const AStr: string);
//    function SaveToString: string;

    function ReadString(const ASection, AKey, ADefaultValue: string): string;
//    procedure WriteString(const ASection, AKey, AValue: string);

    function ReadInteger(const ASection, AKey: string;
      const ADefaultValue: Integer): Integer;
//    procedure WriteInteger(const ASection, AKey: string; const AValue: Integer);

//    function ReadBoolean(const ASection, AKey: string;
//      const ADefaultValue: Boolean): Boolean;
//    procedure WriteBoolean(const ASection, AKey: string; const AValue: Boolean);

//    procedure DeleteSection(const ASection: string);
//    procedure DeleteSectionContent(const ASection: string);
//    procedure DeleteKey(const ASection, AKey: string);
//    procedure Clear;
    function GetSectionNames: TArray<string>;
    function GetSectionKeys(const ASection: string): TArray<string>;

    function IsEmpty: Boolean;

  end;

  EIniData = class(EExpected);

implementation

uses
  System.SysUtils,
  System.Character,
  System.Hash,
  System.Generics.Defaults,
  CSLE.Consts;

{ TIniData }

//procedure TIniData.Clear;
//begin
//  fSections.Clear;
//end;

constructor TIniData.Create;
begin
  inherited Create;
  fSections := TSections.Create;
end;

function TIniData.DequoteString(const S: string): string;
begin
  // Strip any leading and trailing quotes
  if (S.Length >= 2) and (S[1] = DOUBLEQUOTE)
    and (S[S.Length] = DOUBLEQUOTE) then
    Result := Copy(S, 2, S.Length - 2)
  else
    Result := S;
end;

//procedure TIniData.DeleteKey(const ASection, AKey: string);
//begin
//  if fSections.ContainsKey(ASection) then
//  begin
//    var ASectionData := fSections[ASection];
//    if ASectionData.ContainsKey(AKey) then
//      ASectionData.Remove(AKey);
//  end;
//end;

//procedure TIniData.DeleteSection(const ASection: string);
//begin
//  if fSections.ContainsKey(ASection) then
//    fSections.Remove(ASection);
//end;

//procedure TIniData.DeleteSectionContent(const ASection: string);
//begin
//  if fSections.ContainsKey(ASection) then
//  begin
//    var ASectionData := fSections[ASection];
//    ASectionData.Clear;
//  end;
//end;

destructor TIniData.Destroy;
begin
  fSections.Free;
  inherited;
end;

//function TIniData.EnquoteString(const S: string): string;
//begin
//  if S.Length >= 2 then
//  begin
//    if ((S[1] = DOUBLEQUOTE) and (S[S.Length] = DOUBLEQUOTE))
//      or (S[1].IsWhiteSpace or S[S.Length].IsWhiteSpace) then
//      // we surround S in double quotes for two reasons:
//      // 1: if S is enclosed in double quote, because outer double quotes
//      //    are always stripped when reading
//      // 2: if S either begins or ends with whitespace because leading and
//      //    trailing whitespace is stripped when reading.
//      Result := DOUBLEQUOTE + S + DOUBLEQUOTE
//    else
//      Result := S;
//  end;
//end;

procedure TIniData.EnsureSectionExists(const ASection: string);
begin
  if not IsValidSectionName(ASection) then
    raise EIniData.CreateFmt('Invalid section name: "%s"', [ASection]);
  if not fSections.ContainsKey(ASection) then
    fSections.Add(ASection, TSectionData.Create);
end;

function TIniData.GetSectionKeys(const ASection: string): TArray<string>;
begin
  if not IsValidSectionName(ASection) then
    raise EIniData.CreateFmt('Invalid section name: "%s"', [ASection]);
  if not fSections.ContainsKey(ASection) then
    raise EIniData.CreateFmt('Section does not exist: "%s"', [ASection]);
  Result := fSections[ASection].Keys.ToArray;
end;

function TIniData.GetSectionNames: TArray<string>;
begin
  Result := fSections.Keys.ToArray;
end;

function TIniData.InternalRead(const ASection, AKey: string): string;
begin
  if not IsValidSectionName(ASection) then
    raise EIniData.CreateFmt('Invalid section name: "%s"', [ASection]);
  if not IsValidKeyName(AKey) then
    raise EIniData.CreateFmt('Invlaid key: "%s"', [AKey]);
  Result := string.Empty;
  if fSections.ContainsKey(ASection) then
  begin
    var SectionData := fSections[ASection];
    if SectionData.ContainsKey(AKey) then
      Result := SectionData[AKey];
  end;
end;

function TIniData.InternalReadDequotedString(const ASection,
  AKey: string): string;
begin
  Result := DequoteString(InternalRead(ASection, AKey));
end;

procedure TIniData.InternalWrite(const ASection, AKey, AValue: string);
begin
  // Add section if not present: raises exception on bad section name
  EnsureSectionExists(ASection);
  if not IsValidKeyName(AKey) then
    raise EIniData.CreateFmt('Invlaid key: "%s"', [AKey]);
  // Add key/value pair, keeping any leading or trailing spaces in value
  var Data := fSections[ASection];
  Data.AddOrSetValue(AKey, AValue);
end;

//procedure TIniData.InternalWriteEnquotedString(const ASection, AKey,
//  AValue: string);
//begin
//  InternalWrite(ASection, AKey, EnquoteString(AValue));
//end;

function TIniData.IsEmpty: Boolean;
begin
  Result := fSections.IsEmpty;
end;

class function TIniData.IsValidKeyName(const AName: string): Boolean;
begin
  Result := IsValidSectionOrKeyName(AName) and (AName[1] <> CommentOpener);
end;

class function TIniData.IsValidSectionName(const AName: string): Boolean;
begin
  Result := IsValidSectionOrKeyName(AName);
end;

class function TIniData.IsValidSectionOrKeyName(const AName: string): Boolean;
begin
  var TrimmedName := AName.Trim;
  if TrimmedName.Length < AName.Length then
    Exit(False);  // AName started and/or ended with whitespace
  if TrimmedName.IsEmpty then
    Exit(False);
  for var Ch in AName do
    if Ch.IsControl then
      Exit(False);
  Result := True;
end;

procedure TIniData.LoadFromString(const AStr: string);
begin
  Parse(AStr);
end;

procedure TIniData.Parse(const AStr: string);
begin
  fSections.Clear;
  var Parser := TIniDataParser.Create(Self);
  try
    Parser.Parse(AStr);
  finally
    Parser.Free;
  end;
end;

//function TIniData.ReadBoolean(const ASection, AKey: string;
//  const ADefaultValue: Boolean): Boolean;
//begin
//  Result := ADefaultValue;
//  var Value := InternalRead(ASection, AKey);
//  for var B := Low(Boolean) to High(Boolean) do
//    if SameText(BooleanValueNames[B], Value, loInvariantLocale) then
//      Exit(B);
//end;

function TIniData.ReadInteger(const ASection, AKey: string;
  const ADefaultValue: Integer): Integer;
begin
  var Value := InternalReadDequotedString(ASection, AKey);
  if not TryStrToInt(Value, Result) then
    Result := ADefaultValue;
end;

function TIniData.ReadString(const ASection, AKey,
  ADefaultValue: string): string;
begin
  // Read string from ini, dequoted if necessary
  Result := InternalReadDequotedString(ASection, AKey);
  // Return default if string is empty
  if Result.IsEmpty then
    Exit(ADefaultValue);
end;

//function TIniData.SaveToString: string;
//begin
//  Result := string.Empty;
//  for var Section in fSections do
//  begin
//    if not Section.Key.IsEmpty then
//      Result := Result + SectionOpener + Section.Key
//        + SectionCloser + sLineBreak;
//    for var KVPair in Section.Value do
//      Result := Result + KVPair.Key + KeyValueSeparator
//        + EnquoteString(KVPair.Value) + sLineBreak;
//  end;
//end;

//procedure TIniData.WriteBoolean(const ASection, AKey: string;
//  const AValue: Boolean);
//begin
//  InternalWriteEnquotedString(ASection, AKey, BooleanValueNames[AValue]);
//end;

//procedure TIniData.WriteInteger(const ASection, AKey: string;
//  const AValue: Integer);
//begin
//  InternalWriteEnquotedString(ASection, AKey, AValue.ToString);
//end;

//procedure TIniData.WriteString(const ASection, AKey, AValue: string);
//begin
//  InternalWrite(ASection, AKey, AValue);
//end;

{ TIniData.TSections }

constructor TIniData.TSections.Create;
begin
  inherited Create(
    [doOwnsValues],
    TDelegatedEqualityComparer<string>.Create(
      function(const Left, Right: string): Boolean
      begin
       Result := SameText(Left, Right, loInvariantLocale);
      end,
      function(const Value: string): Integer
      begin
        Result := THashBobJenkins.GetHashValue(Value);
      end
    )
  );
end;

{ TIniData.TSectionData }

constructor TIniData.TSectionData.Create;
begin
  inherited Create(
    TDelegatedEqualityComparer<string>.Create(
      function(const Left, Right: string): Boolean
      begin
        Result := SameText(Left, Right, loInvariantLocale);
      end,
      function(const Value: string): Integer
      begin
        Result := THashBobJenkins.GetHashValue(Value);
      end
    )
  );
end;

{ TIniData.TIniDataParser }

constructor TIniData.TIniDataParser.Create(const AIniData: TIniData);
begin
  inherited Create;
  Assert(Assigned(AIniData));
  fIniData := AIniData;
  fLines := TStringList.Create;
  fLines.LineBreak := sLineBreak;
end;

destructor TIniData.TIniDataParser.Destroy;
begin
  fLines.Free;
  inherited;
end;

function TIniData.TIniDataParser.IsEndOfList(const LineIdx: Integer): Boolean;
begin
  Result := LineIdx >= fLines.Count;
end;

procedure TIniData.TIniDataParser.Parse(const AStr: string);
begin
  fLines.Text := AStr;
  PreprocessLines;
  ParseSections;
end;

procedure TIniData.TIniDataParser.ParseSectionContent(const SectionName: string;
  var LineIdx: Integer);
begin
  while not IsEndOfList(LineIdx)
    and not fLines[LineIdx].StartsWith(SectionOpener) do
  begin
    if not fLines[LineIdx].Contains(KeyValueSeparator) then
      raise EIniData.CreateFmt(
        'Malformed Key/Value pair in section %s', [SectionName]
      );
    fIniData.InternalWrite(
      SectionName,
      fLines.Names[LineIdx].Trim,
      fLines.ValueFromIndex[LineIdx].Trim
    );
    Inc(LineIdx);
  end;
end;

procedure TIniData.TIniDataParser.ParseSections;
begin
  var LineIdx := 0;

  if IsEndOfList(LineIdx) then
    Exit;

  if not fLines[LineIdx].StartsWith(SectionOpener) then
    raise EIniData.Create('Malformed INI data: expecting section');

  while not IsEndOfList(LineIdx)
    and fLines[LineIdx].StartsWith(SectionOpener) do
  begin
    var Line := fLines[LineIdx];
    Assert(Line.Length > 0);   // we should have stripped out blank lines

    if not Line.EndsWith(SectionCloser) then
      raise EIniData.CreateFmt(
        'Malformed INI section name: no closing %s', [SectionCloser]
      );

    var SectionName: string := string.Empty;
    if Line.Length > 2 then
      SectionName := Copy(Line, 2, Line.Length - 2).Trim;

    fIniData.EnsureSectionExists(SectionName); // raises exception for bad name

    Inc(LineIdx);
    ParseSectionContent(SectionName, LineIdx);
  end;
end;

procedure TIniData.TIniDataParser.PreprocessLines;
begin
  for var I := Pred(fLines.Count) downto 0 do
  begin
    var Line := fLines[I].Trim;
    if Line.IsEmpty or (Line[1] = CommentOpener) then
      // trimmed line is either comment or empty: remove it
      fLines.Delete(I)
    else
      // trimmed line is necessary: copy back into lines
      fLines[I] := Line;
  end;
end;

end.

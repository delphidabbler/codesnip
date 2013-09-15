{
 * This Source Code Form is subject to the terms of the Mozilla Public License,
 * v. 2.0. If a copy of the MPL was not distributed with this file, You can
 * obtain one at http://mozilla.org/MPL/2.0/
 *
 * Copyright (C) 2013, Peter Johnson (www.delphidabbler.com).
 *
 * $Rev$
 * $Date$
 *
 * Class that can store and load source code languages.
}


unit CS.SourceCode.Languages.Persist;

interface

uses
  SysUtils,
  Generics.Collections,
  CS.SourceCode.Languages,
  UBaseObjects,
  UExceptions,
  UIStringList;

type
  TSourceCodeLanguagesIO = class(TNoConstructObject)
  strict private
    type
      { TODO: There is a lot code here that is common with
              TSyntaxHiliteThemesParser - pull out into base class, possibly in
              a CS.Config.SimpleLineBasedParser or similar unit. }
      TParser = class(TObject)
      strict private
        type
          ///  <summary>Type of list used to record languages read from file.
          ///  </summary>
          TLangList = TList<TSourceCodeLanguage>;
        var
          ///  <summary>Lines of language "file".</summary>
          fLines: IStringList;
          ///  <summary>Index or cursor of line being processed.</summary>
          fLineIdx: Integer;
        ///  <summary>Initialises parser ready to start parsing the given
        ///  language "source code".</summary>
        procedure Init(const Source: string);
        ///  <summary>Moves line cursor to next line of code, skipping blank and
        ///  comment lines.</summary>
        procedure NextLine;
        ///  <summary>Returns line currently being processed or empty string if
        ///  all lines have been processed.</summary>
        function CurrentLine: string;
        function CurrentStatement: string;
        function CurrentParameter: string;
        function IsValidIdent(const S: string): Boolean;
        procedure ValidateIdent(const Statement, Ident: string;
          Existing: IStringList);
        procedure ParseWatermark;
      public
        constructor Create;
        procedure Parse(const Content: string; const IsBuiltIn: Boolean;
          const Langs: TSourceCodeLanguages);
        procedure ParseLanguages(const LangList: TLangList;
          const IsBuiltIn: Boolean);
        procedure ParseLanguage(var Language: TSourceCodeLanguage);
      end;
  strict private
    class procedure InternalLoad(const Content: string;
      const IsBuiltIn: Boolean; const Langs: TSourceCodeLanguages);
  public
    class procedure Save(const Langs: TSourceCodeLanguages;
      const FileName: string);
    class procedure Load(const Langs: TSourceCodeLanguages;
      const FileName: string);
    class procedure LoadFromResources(const Langs: TSourceCodeLanguages;
      const ResName: string; const ResType: PChar);
  end;

  ESourceCodeLanguagesIO = class(ECodeSnip);

implementation

uses
  // Delphi
  Classes,
  Character,
  IOUtils,
  // Project
  CS.SourceCode.Hiliter.Brushes,
  UConsts,
  UEncodings,
  UIOUtils,
  UResourceUtils,
  UStructs,
  UStrUtils;

const
  ///  <summary>Watermark that is present on the first line of a valid
  ///  languages file.</summary>
  Watermark = #$25BA + ' CodeSnip Source Code Languages v1 ' + #$25C4;
  KwdLanguage = 'Language';
  KwdTabSize = 'TabSize';
  KwdBrush = 'Brush';
  LanguageKwds: array[0..1] of string = (KwdTabSize, KwdBrush);

resourcestring
  sBadWatermark = 'Invalid or missing watermark line';
  sMissingLangStatement = 'Expected a LANGUAGE statememt';
  sMissingID = 'Identifier expected in %s';
  sBadID = 'Invalid identifier "%0:s" encountered in %1:s';
  sDuplicateID = 'Duplicate identifier "%0:s" encountered in %1:s';
  sMissingTabSize = 'Missing tab size parameter in TABSIZE statement';
  sBadTabSize = 'Invalid tab size "%s" in TABSIZE statement';
  sMissingBrushID = 'Missing brush ID in BRUSH statement';
  sBadBrushID = 'Invalid brush ID "%s" in BRUSH statement';


{ TSourceCodeLanguagesIO }

class procedure TSourceCodeLanguagesIO.InternalLoad(const Content: string;
  const IsBuiltIn: Boolean; const Langs: TSourceCodeLanguages);
var
  Parser: TParser;
begin
  Parser := TParser.Create;
  try
    Parser.Parse(Content, IsBuiltIn, Langs);
  finally
    Parser.Free;
  end;
end;

class procedure TSourceCodeLanguagesIO.Load(const Langs: TSourceCodeLanguages;
  const FileName: string);
begin
  if not TFile.Exists(FileName) then
    Exit;
  try
    InternalLoad(
      TFileIO.ReadAllText(FileName, TEncoding.UTF8, True), False, Langs
    );
  except
    on E: EStreamError do
      raise ESourceCodeLanguagesIO.Create(E);
    on E: EIOUtils do
      raise ESourceCodeLanguagesIO.Create(E);
    else
      raise;
  end;
end;

class procedure TSourceCodeLanguagesIO.LoadFromResources(
  const Langs: TSourceCodeLanguages; const ResName: string;
  const ResType: PChar);
var
  Content: string;
begin
  Content := LoadResourceAsString(HInstance, ResName, ResType, etUTF8, True);
  InternalLoad(Content, True, Langs);
end;

class procedure TSourceCodeLanguagesIO.Save(const Langs: TSourceCodeLanguages;
  const FileName: string);
var
  SB: TStringBuilder;
  Language: TSourceCodeLanguage;
begin
  SB := TStringBuilder.Create;
  try
    SB.AppendLine(Watermark);
    SB.AppendLine;
    for Language in Langs do
    begin
      SB.Append(Format('%s %s', [KwdLanguage, Language.ID.ToString]));
      if Language.ID.ToString <> Language.FriendlyName then
        SB.Append(' ' + Language.FriendlyName);
      SB.AppendLine;
      SB.AppendLine(Format('  %s %d', [KwdTabSize, Language.EditorTabSize]));
      SB.AppendLine(Format('  %s %s', [KwdBrush, Language.HiliterBrushID]));
    end;
    TDirectory.CreateDirectory(TPath.GetDirectoryName(FileName));
    try
      TFileIO.WriteAllText(FileName, SB.ToString, TEncoding.UTF8, True);
    except
      on E: EStreamError do
        raise ESourceCodeLanguagesIO.Create(E);
      else
        raise;
    end;
  finally
    SB.Free;
  end;
end;

{ TSourceCodeLanguagesIO.TParser }

constructor TSourceCodeLanguagesIO.TParser.Create;
begin
  fLines := TIStringList.Create;
  Init('');
end;

function TSourceCodeLanguagesIO.TParser.CurrentLine: string;
begin
  if fLineIdx < fLines.Count then
    Result := fLines[fLineIdx]
  else
    Result := EmptyStr;
end;

function TSourceCodeLanguagesIO.TParser.CurrentParameter: string;
var
  Dummy: string;
begin
  StrSplit(CurrentLine, ' ', Dummy, Result);
  Result := StrTrim(Result);
end;

function TSourceCodeLanguagesIO.TParser.CurrentStatement: string;
var
  Dummy: string;
begin
  StrSplit(CurrentLine, ' ', Result, Dummy);
  Result := StrTrim(Result);
end;

procedure TSourceCodeLanguagesIO.TParser.Init(const Source: string);
begin
  fLineIdx := -1;
  fLines.Clear;
  fLines.Add(Source, EOL, True, True);
  NextLine;
end;

function TSourceCodeLanguagesIO.TParser.IsValidIdent(const S: string): Boolean;
var
  C: Char;
begin
  // An identifier contains only non-whitespace ASCII characters
  for C in S do
    if TCharacter.IsWhiteSpace(C) then
      Exit(False);
  Result := True;
end;

procedure TSourceCodeLanguagesIO.TParser.NextLine;
begin
  if fLineIdx = fLines.Count then
    Exit;
  Inc(fLineIdx);
  while (fLineIdx < fLines.Count) and
    ((fLines[fLineIdx] = EmptyStr) or StrStartsStr('#', fLines[fLineIdx])) do
    Inc(fLineIdx);
end;

procedure TSourceCodeLanguagesIO.TParser.Parse(const Content: string;
  const IsBuiltIn: Boolean; const Langs: TSourceCodeLanguages);
var
  LangList: TLangList;
  Lang: TSourceCodeLanguage;
begin
  Init(Content);
  ParseWatermark;
  // We read languages into a temporary list rather than directly into Langs so
  // that Langs is not modified at all should a parsing error occur. Only if
  // parsing is successful do we update Langs.
  LangList := TLangList.Create;
  try
    while CurrentStatement <> EmptyStr do
    begin
      if not StrSameText(CurrentStatement, KwdLanguage) then
        raise ESourceCodeLanguagesIO.Create(sMissingLangStatement);
      ParseLanguages(LangList, IsBuiltIn);
      NextLine;
    end;
    for Lang in LangList do
      Langs.Update(Lang);
  finally
    LangList.Free;
  end;
end;

procedure TSourceCodeLanguagesIO.TParser.ParseLanguage(
  var Language: TSourceCodeLanguage);
var
  TabSize: Integer;
  TabRange: TRange;
begin
  while StrMatchText(CurrentStatement, LanguageKwds) do
  begin
    if StrSameText(CurrentStatement, KwdTabSize) then
    begin
      if CurrentParameter = EmptyStr then
        raise ESourceCodeLanguagesIO.Create(sMissingTabSize);
      if not TryStrToInt(CurrentParameter, TabSize) then
        raise ESourceCodeLanguagesIO.CreateFmt(sBadTabSize, [CurrentParameter]);
      TabRange := TRange.Create(1, High(Byte));
      if not TabRange.Contains(TabSize) then
        raise ESourceCodeLanguagesIO.CreateFmt(sBadTabSize, [CurrentParameter]);
      Language.EditorTabSize := Byte(TabSize);
    end
    else if StrSameText(CurrentStatement, KwdBrush) then
    begin
      if CurrentParameter = EmptyStr then
        raise ESourceCodeLanguagesIO.Create(sMissingBrushID);
      if not TSyntaxHiliterBrush.IsValidBrushID(CurrentParameter) then
        raise ESourceCodeLanguagesIO.CreateFmt(sBadBrushID, [CurrentParameter]);
      Language.HiliterBrushID := CurrentParameter;
    end;
    NextLine;
  end;
end;

procedure TSourceCodeLanguagesIO.TParser.ParseLanguages(
  const LangList: TLangList; const IsBuiltIn: Boolean);
var
  LangIDs: IStringList;
  LangIDStr: string;
  LangID: TSourceCodeLanguageID;
  LangFriendlyName: string;
  Language: TSourceCodeLanguage;
begin
  LangIDs := TIStringList.Create;
  while StrSameText(CurrentStatement, KwdLanguage) do
  begin
    StrSplit(CurrentParameter, ' ', LangIDStr, LangFriendlyName);
    LangIDStr := StrTrim(LangIDStr);
    ValidateIdent(KwdLanguage, LangIDStr, LangIDs);
    LangID := TSourceCodeLanguageID.Create(LangIDStr);
    LangIDs.Add(LangIDStr);
    LangFriendlyName := StrTrim(LangFriendlyName);
    if LangFriendlyName = EmptyStr then
      LangFriendlyName := LangIDStr;
    NextLine;
    Language := TSourceCodeLanguage.Create(LangID, LangFriendlyName, IsBuiltIn);
    ParseLanguage(Language);
    LangList.Add(Language);
  end;
end;

procedure TSourceCodeLanguagesIO.TParser.ParseWatermark;
begin
  if CurrentLine <> Watermark then
    raise ESourceCodeLanguagesIO.Create(sBadWatermark);
  NextLine;
end;

procedure TSourceCodeLanguagesIO.TParser.ValidateIdent(const Statement,
  Ident: string; Existing: IStringList);
begin
  if Ident = EmptyStr then
    raise ESourceCodeLanguagesIO.CreateFmt(sMissingID, [StrToUpper(Statement)]);
  if not IsValidIdent(Ident) then
    raise ESourceCodeLanguagesIO.CreateFmt(
      sBadID, [Ident, StrToUpper(Statement)]
    );
  if Existing.Contains(Ident) then
    raise ESourceCodeLanguagesIO.CreateFmt(
      sDuplicateID, [Ident, StrToUpper(Statement)]
    )
end;

end.

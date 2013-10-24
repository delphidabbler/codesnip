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
 * Classes that can store and load syntax highlighter themes.
}


unit CS.SourceCode.Hiliter.Themes.Persist;


interface


uses
  // Delphi
  SysUtils,
  Generics.Collections,
  // Project
  UBaseObjects,
  UExceptions,
  UIStringList,
  CS.SourceCode.Hiliter.Themes;

type

  TSyntaxHiliteThemesParser = class(TObject)
  strict private
    type
      ///  <summary>Type of list used to store themes.</summary>
      TThemeList = TList<TSyntaxHiliteTheme>;
    var
      ///  <summary>Lines of themes "file".</summary>
      fLines: IStringList;
      ///  <summary>Index or cursor of line being processed.</summary>
      fLineIdx: Integer;
    ///  <summary>Initialises parser ready to start parsing the given theme
    ///  "source code".</summary>
    procedure Init(const Source: string);
    ///  <summary>Moves line cursor to next line of code, skipping blank and
    ///  comment lines.</summary>
    procedure NextLine;
    ///  <summary>Returns line currently being processed or empty string if all
    ///  lines have been processed.</summary>
    function CurrentLine: string;

    function CurrentStatement: string;
    function CurrentParameter: string;

    procedure ValidateIdent(const Statement, Ident: string;
      Existing: IStringList; FormatChecker: TFunc<string,Boolean>);

    procedure ParseWatermark;

    ///  <summary>Parses all themes in file and stores them in a list.</summary>
    ///  <param name="ThemeList">TThemeList [in] List to receive themese.
    ///  </param>
    ///  <param name="IsBuiltIn">Boolean [in] Informs if the themes being parsed
    ///  are built in (True) or user-defined (False).</para>
    ///  <remarks>Assumes that the current line is a "Theme" statement.
    ///  </remarks>
    procedure ParseThemes(const ThemeList: TThemeList;
      const IsBuiltIn: Boolean);

    ///  <summary>Parses the given theme's properties and updates the theme
    ///  accordingly.</summary>
    ///  <remarks>Assumes the current line is one of the theme property
    ///  statements.</remarks>
    procedure ParseTheme(const Theme: TSyntaxHiliteTheme);

    ///  <summary>Parses a consecutive sequence of brush styles within the given
    ///  theme and adds them to the given theme.</summary>
    ///  <remarks>Assumes that the current line is a "Brush" statement.
    ///  </remarks>
    procedure ParseBrushStyles(const Theme: TSyntaxHiliteTheme);

    ///  <summary>Parses all attribute styles within the given brush style and
    ///  adds them to the brush style.</summary>
    ///  <remarks>Assumes that the current line is an "Attr" statement.
    ///  </remarks>
    procedure ParseAttrStyles(const BrushStyle: TSyntaxHiliteBrushStyle);

    function ParseAttrStyle(const AttrID, Data: string): TSyntaxHiliteAttrStyle;

  public
    constructor Create;
    destructor Destroy; override;
    procedure Parse(const Content: string; const IsBuiltIn: Boolean;
      const Themes: TSyntaxHiliteThemes);
  end;

  TSyntaxHiliteThemesIO = class(TNoConstructObject)
  strict private
    class procedure WriteBrushStyle(const SB: TStringBuilder;
      const BrushID: string; const BrushStyle: TSyntaxHiliteBrushStyle);
    class procedure InternalLoadThemes(const Content: string;
      const IsBuiltIn: Boolean; const Themes: TSyntaxHiliteThemes);
  public
    class procedure SaveThemes(const Themes: TSyntaxHiliteThemes;
      const FileName: string);
    class procedure LoadThemes(const Themes: TSyntaxHiliteThemes;
      const FileName: string);
    class procedure LoadThemesFromResources(const Themes: TSyntaxHiliteThemes;
      const ResName: string; const ResType: PChar);
  end;

  ESyntaxHiliteThemesIO = class(ECodeSnip);


implementation

uses
  // Delphi
  Graphics,
  IOUtils,
  Classes,
  Character,
  // Project
  UConsts,
  UEncodings,
  UIOUtils,
  UResourceUtils,
  UStrUtils;

const
  ///  <summary>Map of syntax highlighter font styles to identifiers used in a
  ///  themes file.</summary>
  ///  <remarks>There is an empty entry for fsStrikeout since that style is not
  ///  permitted in theme files.</remarks>
  FontStyleMap: array[TFontStyle] of string = (
    'bold', 'italic', 'underline', ''
  );
  ///  <summary>Watermark that is present on the first line of a valid
  ///  themes file.</summary>
  Watermark = #$25BA + ' CodeSnip Syntax Highlight Themes v1 ' + #$25C4;

  // Names of themes file keywords
  KwdTheme = 'Theme';
  KwdDefaultBackground = 'DefaultBackground';
  KwdDefaultForeground = 'DefaultForeground';
  KwdFontName = 'FontName';
  KwdFontSize = 'FontSize';
  KwdBrush = 'Brush';
  KwdAttr = 'Attr';

  ThemeKwds: array[0..4] of string = (
    KwdBrush, KwdFontName, KwdFontSize,
    KwdDefaultBackground, KwdDefaultForeground
  );

resourcestring
  // I/O and parsing error messages
  sMissingThemeStatement = 'THEME statement expected';
  sMissingAttrData = 'Missing data for ATTR "%s"';
  sMissingFriendlyThemeName = 'Missing friendly name in THEME statement';
  sMissingID = 'Missing identifier in %s statement';
  sBadID = 'Malformed identifier "%0:s" in %1:s statement';
  sBadUserThemeID = 'User defined THEME identifier "%s" must not start with '
    + 'an underscore';
  sBadBuiltInThemeID = 'Built in THEME identifier "%s" must begin with an '
    + 'underscore';
  sBadAttrColour = 'Invalid colour in ATTR "%s"';
  sBadFontStyle = 'Invalid font style in ATTR "%s"';
  sBadWatermark = 'Invalid or missing watermark line';
  sDuplicateID = 'Duplicate identifier "%0:s" in %1:s statement';
  sBadDefaultColour = 'Invalid or missing colour in "%s" statement';
  sBadThemeFontSize = 'Invalid or missing font size in FONTSIZE statement';
  sMissingThemeFontName = 'Missing font name in FONTNAME statement';

{ TSyntaxHiliteThemesIO }

class procedure TSyntaxHiliteThemesIO.InternalLoadThemes(const Content: string;
  const IsBuiltIn: Boolean; const Themes: TSyntaxHiliteThemes);
var
  Parser: TSyntaxHiliteThemesParser;
begin
  Parser := TSyntaxHiliteThemesParser.Create;
  try
    Parser.Parse(Content, IsBuiltIn, Themes);
  finally
    Parser.Free;
  end;
end;

class procedure TSyntaxHiliteThemesIO.LoadThemes(
  const Themes: TSyntaxHiliteThemes; const FileName: string);
begin
  if not TFile.Exists(FileName) then
    Exit;
  try
    InternalLoadThemes(
      TFileIO.ReadAllText(FileName, TEncoding.UTF8, True), False, Themes
    );
  except
    on E: EStreamError do
      raise ESyntaxHiliteThemesIO.Create(E);
    on E: EIOUtils do
      raise ESyntaxHiliteThemesIO.Create(E);
    else
      raise;
  end;
end;

class procedure TSyntaxHiliteThemesIO.LoadThemesFromResources(
  const Themes: TSyntaxHiliteThemes; const ResName: string;
  const ResType: PChar);
var
  Content: string;
begin
  Content := LoadResourceAsString(HInstance, ResName, ResType, etUTF8, True);
  InternalLoadThemes(Content, True, Themes);
end;

class procedure TSyntaxHiliteThemesIO.SaveThemes(
  const Themes: TSyntaxHiliteThemes; const FileName: string);
var
  Theme: TSyntaxHiliteTheme;
  BrushID: string;
  SB: TStringBuilder;

  function FormatDefaultColour(const Colour: TColor): string;
  begin
    if Colour = clNone then
      Exit('-');
    Result := IntToHex(Integer(Colour), 8);
  end;

begin
  SB := TStringBuilder.Create;
  try
    SB.AppendLine(Watermark);
    SB.AppendLine;
    for Theme in Themes do
    begin
      // Don't save built-in themes
      if Theme.BuiltIn then
        Continue;
      SB.AppendLine(
        Format('%s %s %s', [KwdTheme, Theme.ID.ToString, Theme.FriendlyName])
      );
      SB.AppendLine(
        Format(
          '  %s %s',
          [KwdDefaultBackground, FormatDefaultColour(Theme.DefaultBackground)]
        )
      );
      SB.AppendLine(
        Format(
          '  %s %s',
          [KwdDefaultForeground, FormatDefaultColour(Theme.DefaultForeground)]
        )
      );
      SB.AppendLine(
        Format('  %s %s', [KwdFontName, Theme.FontName])
      );
      SB.AppendLine(
        Format('  %s %d', [KwdFontSize, Theme.FontSize])
      );
      WriteBrushStyle(SB, '*', Theme.DefaultBrushStyle);
      for BrushID in Theme.SupportedBrushes do
        WriteBrushStyle(SB, BrushID, Theme.BrushStyles[BrushID]);
      SB.AppendLine;
    end;
    TDirectory.CreateDirectory(TPath.GetDirectoryName(FileName));
    try
      TFileIO.WriteAllText(FileName, SB.ToString, TEncoding.UTF8, True);
    except
      on E: EStreamError do
        raise ESyntaxHiliteThemesIO.Create(E);
      else
        raise;
    end;
  finally
    SB.Free;
  end;
end;

class procedure TSyntaxHiliteThemesIO.WriteBrushStyle(const SB: TStringBuilder;
  const BrushID: string; const BrushStyle: TSyntaxHiliteBrushStyle);
var
  AttrID: string;
  AttrIDs: TArray<string>;
  AttrStyle: TSyntaxHiliteAttrStyle;

  function FormatAttrColour(const Colour: TColor): string;
  begin
    if Colour = clDefault then
      Exit('*');
    Result := IntToHex(Integer(Colour), 8);
  end;

  function FormatAttrFontStyles(const FontStyles: TSyntaxHiliteFontStyles):
    string;
  var
    FontStyle: TFontStyle;
    SL: IStringList;
  begin
    if FontStyles.IsDefault then
      Exit('*');
    SL := TIStringList.Create;
    for FontStyle in FontStyles.Styles do
      SL.Add(FontStyleMap[FontStyle]);
    Result := '{' + SL.GetText(',', False) + '}';
  end;

begin
  SB.AppendLine(Format('  %s %s', [KwdBrush, BrushID]));
  AttrIDs := BrushStyle.SupportedAttrs;
  for AttrID in AttrIDs do
  begin
    AttrStyle := BrushStyle.AttrStyles[AttrID];
    SB.Append(Format('    %s %s ', [KwdAttr, AttrID]));
    SB.Append(FormatAttrColour(AttrStyle.Background));
    SB.Append(',');
    SB.Append(FormatAttrColour(AttrStyle.Foreground));
    SB.Append(',');
    SB.Append(FormatAttrFontStyles(AttrStyle.FontStyles));
    SB.AppendLine;
  end;
end;

{ TSyntaxHiliteThemesParser }

constructor TSyntaxHiliteThemesParser.Create;
begin
  inherited Create;
  fLines := TIStringList.Create;
  Init('');
end;

function TSyntaxHiliteThemesParser.CurrentLine: string;
begin
  if fLineIdx < fLines.Count then
    Result := fLines[fLineIdx]
  else
    Result := EmptyStr;
end;

function TSyntaxHiliteThemesParser.CurrentParameter: string;
var
  Dummy: string;
begin
  StrSplit(CurrentLine, ' ', Dummy, Result);
  Result := StrTrim(Result);
end;

function TSyntaxHiliteThemesParser.CurrentStatement: string;
var
  Dummy: string;
begin
  StrSplit(CurrentLine, ' ', Result, Dummy);
  Result := StrTrim(Result);
end;

destructor TSyntaxHiliteThemesParser.Destroy;
begin

  inherited;
end;

procedure TSyntaxHiliteThemesParser.Init(const Source: string);
begin
  fLineIdx := -1;
  fLines.Clear;
  fLines.Add(Source, EOL, True, True);
  NextLine;
end;

procedure TSyntaxHiliteThemesParser.NextLine;
begin
  if fLineIdx = fLines.Count then
    Exit;
  Inc(fLineIdx);
  while (fLineIdx < fLines.Count) and
    ((fLines[fLineIdx] = EmptyStr) or StrStartsStr('#', fLines[fLineIdx])) do
    Inc(fLineIdx);
end;

procedure TSyntaxHiliteThemesParser.Parse(const Content: string;
  const IsBuiltIn: Boolean; const Themes: TSyntaxHiliteThemes);
var
  ThemeList: TThemeList;
  Theme: TSyntaxHiliteTheme;
begin
  Init(Content);
  ParseWatermark;
  // We read themes into a temporary theme list rather than directly into Themes
  // so that Themes is not modified at all should a parsing error occur. Only if
  // parsing is successful do we update Themes.
  ThemeList := TThemeList.Create;
  try
    try
      while CurrentStatement <> EmptyStr do
      begin
        if not StrSameText(CurrentStatement, KwdTheme) then
          raise ESyntaxHiliteThemesIO.Create(sMissingThemeStatement);
        ParseThemes(ThemeList, IsBuiltIn);
        NextLine;
      end;
    except
      for Theme in ThemeList do
        Theme.Free;
      raise;
    end;
    for Theme in ThemeList do
      Themes.Add(Theme);
  finally
    ThemeList.Free;
  end;
end;

function TSyntaxHiliteThemesParser.ParseAttrStyle(const AttrID, Data: string):
  TSyntaxHiliteAttrStyle;


  function ParseColour(const Field: string): TColor;
  var
    ColourInt: Integer;
  begin
    if Field = EmptyStr then
      raise ESyntaxHiliteThemesIO.CreateFmt(sBadAttrColour, [AttrID]);
    if Field = '*' then
      Exit(clDefault);
    if not TryStrToInt(HexDisplayPrefix + Field, ColourInt) then
      raise ESyntaxHiliteThemesIO.CreateFmt(sBadAttrColour, [AttrID]);
    Result := TColor(ColourInt);
  end;

  function ParseFontStyle(const Field: string): TSyntaxHiliteFontStyles;

    function LookupStyle(const StyleStr: string;
      out Style: TFontStyle): Boolean;
    var
      I: TFontStyle;
    begin
      for I := Low(FontStyleMap) to High(FontStyleMap) do
      begin
        if I = fsStrikeout then
          Continue;   // we ignore strikeout: not permitted in themes
        if StrSameText(StyleStr, FontStyleMap[I]) then
        begin
          Style := I;
          Exit(True);
        end;
      end;
      Exit(False);
    end;

  var
    StyleStrings: IStringList;
    StyleString: string;
    FontStyle: TFontStyle;
    FontStyles: TFontStyles;
  begin
    if Field = EmptyStr then
      raise ESyntaxHiliteThemesIO.CreateFmt(sBadFontStyle, [AttrID]);
    if Field = '*' then
      Exit(TSyntaxHiliteFontStyles.CreateDefault);
    if (Length(Field) < 2)
      or (Field[1] <> '{') or (Field[Length(Field)] <> '}') then
      raise Exception.CreateFmt(sBadFontStyle, [AttrID]);
    StyleStrings := TIStringList.Create(
      StrSlice(Field, 2, Length(Field) - 2), ',', False, True
    );
    FontStyles := [];
    for StyleString in StyleStrings do
    begin
      if not LookupStyle(StyleString, FontStyle) then
        raise Exception.CreateFmt(sBadFontStyle, [AttrID]);
      Include(FontStyles, FontStyle);
    end;
    Result := TSyntaxHiliteFontStyles.CreateStyles(FontStyles);
  end;

var
  BGColour: string;
  FGColour: string;
  FontStyles: string;
  Fields: string;
begin
  Fields := StrTrim(Data);
  if Data = EmptyStr then
    raise ESyntaxHiliteThemesIO.CreateFmt(sMissingAttrData, [AttrID]);
  BGColour := StrTrim(StrPop(Fields, ','));
  FGColour := StrTrim(StrPop(Fields, ','));
  FontStyles := StrTrim(Fields);
  Result := TSyntaxHiliteAttrStyle.Create(
    ParseColour(BGColour), ParseColour(FGColour), ParseFontStyle(FontStyles)
  );
end;

procedure TSyntaxHiliteThemesParser.ParseAttrStyles(
  const BrushStyle: TSyntaxHiliteBrushStyle);
var
  AttrID: string;
  AttrData: string;
  AttrStyle: TSyntaxHiliteAttrStyle;
  AttrIDs: IStringList;
begin
  AttrIDs := TIStringList.Create;
  while StrSameText(CurrentStatement, KwdAttr) do
  begin
    StrSplit(CurrentParameter, ' ', AttrID, AttrData);
    AttrID := StrTrim(AttrID);
    ValidateIdent(
      KwdAttr,
      AttrID,
      AttrIDs,
      function (S: string): Boolean
      begin
        Result := TSyntaxHiliteAttrStyle.IsValidIDString(S)
      end
    );
    AttrIDs.Add(AttrID);
    AttrStyle := ParseAttrStyle(AttrID, AttrData);
    NextLine;
    BrushStyle.Add(AttrID, AttrStyle);
  end;
end;

procedure TSyntaxHiliteThemesParser.ParseBrushStyles(
  const Theme: TSyntaxHiliteTheme);
var
  BrushID: string;
  BrushStyle: TSyntaxHiliteBrushStyle;
  BrushIDs: IStringList;
begin
  BrushIDs := TIStringList.Create;
  while StrSameText(CurrentStatement, KwdBrush) do
  begin
    BrushID := CurrentParameter;
    ValidateIdent(
      KwdBrush,
      BrushID,
      BrushIDs,
      function (S: string): Boolean
      begin
        Result := (S = '*') or TSyntaxHiliteBrushStyle.IsValidIDString(S)
      end
    );
    BrushIDs.Add(BrushID);
    NextLine;
    if BrushID = '*' then
      ParseAttrStyles(Theme.DefaultBrushStyle)
    else
    begin
      BrushStyle := TSyntaxHiliteBrushStyle.Create;
      try
        ParseAttrStyles(BrushStyle);
        Theme.AddBrushStyle(BrushID, BrushStyle);
      except
        BrushStyle.Free;
        raise
      end;
    end;
  end;
end;

procedure TSyntaxHiliteThemesParser.ParseTheme(const Theme: TSyntaxHiliteTheme);

  function ParseDefaultColour(const Field: string): TColor;
  var
    ColourInt: Integer;
  begin
    if (Field = EmptyStr) or (Field = '*') then
      raise ESyntaxHiliteThemesIO.CreateFmt(
        sBadDefaultColour, [CurrentStatement]
      );
    if Field = '-' then
      Exit(clNone);
    if not TryStrToInt(HexDisplayPrefix + Field, ColourInt) then
      raise ESyntaxHiliteThemesIO.CreateFmt(
        sBadDefaultColour, [CurrentStatement]
      );
    Result := TColor(ColourInt);
  end;

  function GetFontSize(const Field: string): Integer;
  begin
    if not TryStrToInt(StrTrim(Field), Result) then
      raise ESyntaxHiliteThemesIO.Create(sBadThemeFontSize);
  end;

begin
  while StrMatchText(CurrentStatement, ThemeKwds) do
  begin
    if StrSameText(CurrentStatement, KwdBrush) then
      ParseBrushStyles(Theme)
    else if StrSameText(CurrentStatement, KwdDefaultBackground) then
    begin
      Theme.DefaultBackground := ParseDefaultColour(CurrentParameter);
      NextLine;
    end
    else if StrSameText(CurrentStatement, KwdDefaultForeground) then
    begin
      Theme.DefaultForeground := ParseDefaultColour(CurrentParameter);
      NextLine;
    end
    else if StrSameText(CurrentStatement, KwdFontName) then
    begin
      if CurrentParameter = EmptyStr then
        raise ESyntaxHiliteThemesIO.Create(sMissingThemeFontName);
      Theme.FontName := CurrentParameter;
      NextLine;
    end
    else if StrSameText(CurrentStatement, KwdFontSize) then
    begin
      Theme.FontSize := GetFontSize(CurrentParameter);
      NextLine;
    end;
  end;
end;

procedure TSyntaxHiliteThemesParser.ParseThemes(const ThemeList: TThemeList;
  const IsBuiltIn: Boolean);
var
  ThemeIDStr: string;
  ThemeFriendlyName: string;
  Theme: TSyntaxHiliteTheme;
  ThemeIDStrs: IStringList;
begin
  ThemeIDStrs := TIStringList.Create;
  while StrSameText(CurrentStatement, KwdTheme) do
  begin
    StrSplit(CurrentParameter, ' ', ThemeIDStr, ThemeFriendlyName);
    ThemeIDStr := StrTrim(ThemeIDStr);
    if not IsBuiltIn and StrStartsStr('_', ThemeIDStr) then
      raise ESyntaxHiliteThemesIO.CreateFmt(sBadUserThemeID, [ThemeIDStr]);
    if IsBuiltIn and not StrStartsStr('_', ThemeIDStr) then
      raise ESyntaxHiliteThemesIO.CreateFmt(sBadBuiltInThemeID, [ThemeIDStr]);
    ValidateIdent(
      KwdTheme,
      ThemeIDStr,
      ThemeIDStrs,
      function (S: string): Boolean
      begin
        Result := TSyntaxHiliteTheme.IsValidIDString(S, IsBuiltIn);
      end
    );
    ThemeIDStrs.Add(ThemeIDStr);
    ThemeFriendlyName := StrTrim(ThemeFriendlyName);
    if ThemeFriendlyName = EmptyStr then
      raise ESyntaxHiliteThemesIO.Create(sMissingFriendlyThemeName);
    NextLine;
    Theme := TSyntaxHiliteTheme.Create(
      TSyntaxHiliteThemeID.Create(ThemeIDStr), ThemeFriendlyName
    );
    try
      Theme.BuiltIn := IsBuiltIn;
      ParseTheme(Theme);
      ThemeList.Add(Theme);
    except
      Theme.Free;
      raise;
    end;
  end;
end;

procedure TSyntaxHiliteThemesParser.ParseWatermark;
begin
  if CurrentLine <> Watermark then
    raise ESyntaxHiliteThemesIO.Create(sBadWatermark);
  NextLine;
end;

procedure TSyntaxHiliteThemesParser.ValidateIdent(const Statement,
  Ident: string; Existing: IStringList; FormatChecker: TFunc<string,Boolean>);
begin
  if Ident = EmptyStr then
    raise ESyntaxHiliteThemesIO.CreateFmt(sMissingID, [StrToUpper(Statement)]);
  if not FormatChecker(Ident) then
    raise ESyntaxHiliteThemesIO.CreateFmt(
      sBadID, [Ident, StrToUpper(Statement)]
    );
  if Existing.Contains(Ident) then
    raise ESyntaxHiliteThemesIO.CreateFmt(
      sDuplicateID, [Ident, StrToUpper(Statement)]
    )
end;

end.

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
 * Classes and types used to encapsulate syntax highlighter styling themes.
}


unit CS.SourceCode.Hiliter.Themes;

interface

uses
  // Delphi
  Generics.Collections,
  Generics.Defaults,
  Graphics,
  // Project
  UStructs;

type

  TSyntaxHiliteFontStyles = record
  strict private
    var
      fStyles: TFontStyles;
      fIsDefault: Boolean;
    function GetStyles: TFontStyles;
  public
    class function CreateStyles(const FS: TFontStyles): TSyntaxHiliteFontStyles;
      static;
    class function CreateDefault: TSyntaxHiliteFontStyles; static;
    class function CreateNull: TSyntaxHiliteFontStyles; static;
    class operator Implicit(const S: TSyntaxHiliteFontStyles): TFontStyles;
    function IsNull: Boolean;
    property Styles: TFontStyles read GetStyles;
    property IsDefault: Boolean read fIsDefault;
  end;

  TSyntaxHiliteAttrStyle = record
    Background: TColor;
    Foreground: TColor;
    FontStyles: TSyntaxHiliteFontStyles;
    constructor Create(ABackground, AForeground: TColor;
      AFontStyles: TSyntaxHiliteFontStyles);
    class function CreateNull: TSyntaxHiliteAttrStyle; static;
    class function IsValidIDString(const S: string): Boolean; static;
    function IsNull: Boolean;
    function Clone(const IgnoreColour: Boolean = False): TSyntaxHiliteAttrStyle;
    class operator Equal(const Left, Right: TSyntaxHiliteAttrStyle): Boolean;
    class operator NotEqual(const Left, Right: TSyntaxHiliteAttrStyle): Boolean;
  end;

  TSyntaxHiliteBrushStyle = class(TObject)
  strict private
    var
      fAttrStyles: TDictionary<string,TSyntaxHiliteAttrStyle>;
    function GetSupportedAttrs: TArray<string>;
    function GetAttrStyle(const AttrId: string): TSyntaxHiliteAttrStyle;
    procedure SetAttrStyle(const AttrID: string;
      const NewStyle: TSyntaxHiliteAttrStyle);
  public
    constructor Create;
    destructor Destroy; override;
    procedure Assign(const Src: TSyntaxHiliteBrushStyle;
      const IgnoreColour: Boolean = False);
    function Clone(const IgnoreColour: Boolean = False):
      TSyntaxHiliteBrushStyle;
    procedure Add(const AttrID: string; const Style: TSyntaxHiliteAttrStyle);
    procedure Clear;
    function IsAttrSupported(const AttrID: string): Boolean;
    class function IsValidIDString(const S: string): Boolean;
    function GetEnumerator: TEnumerator<TPair<string,TSyntaxHiliteAttrStyle>>;
    property SupportedAttrs: TArray<string> read GetSupportedAttrs;
    // AttrStyles => return null attr if style doesn't exist
    property AttrStyles[const AttrId: string]: TSyntaxHiliteAttrStyle
      read GetAttrStyle write SetAttrStyle;
  end;

  TSyntaxHiliteThemeID = record
  strict private
    const
      DefaultID = '_DEFAULT_';
    var
      fID: string;
  public
    type
      TComparer = class(TComparer<TSyntaxHiliteThemeID>)
        function Compare(const Left, Right: TSyntaxHiliteThemeID): Integer;
          override;
      end;
      TEqualityComparer = class(TEqualityComparer<TSyntaxHiliteThemeID>)
      public
        function Equals(const Left, Right: TSyntaxHiliteThemeID): Boolean;
          override;
        function GetHashCode(const Value: TSyntaxHiliteThemeID): Integer;
          override;
      end;
  public
    constructor Create(const AID: string);
    class function CreateDefault: TSyntaxHiliteThemeID; static; inline;
    class function CreateNull: TSyntaxHiliteThemeID; static; inline;
    class operator Equal(const Left, Right: TSyntaxHiliteThemeID): Boolean;
      inline;
    class operator NotEqual(const Left, Right: TSyntaxHiliteThemeID): Boolean;
      inline;
    class function Compare(const Left, Right: TSyntaxHiliteThemeID): Integer;
      static; inline;
    function CompareTo(const Other: TSyntaxHiliteThemeID): Integer; inline;
    function ToString: string; inline;
    function Hash: Integer; inline;
    function IsNull: Boolean;
  end;

  TSyntaxHiliteTheme = class(TObject)
  strict private
    const
      MinFontSize = 6;
      MaxFontSize = 32;
      ///  <summary>Default theme font size.</summary>
      ///  <remarks>Must be between MinFontSize and MaxFontSize.</remarks>
      DefaultFontSize = 9;
  strict private
    var
      fID: TSyntaxHiliteThemeID;
      fFriendlyName: string;
      fFontName: string;
      fFontSize: Integer;
      fDefaultForeground: TColor;
      fDefaultBackground: TColor;
      fDefaultBrushStyle: TSyntaxHiliteBrushStyle;
      fBrushStyles: TObjectDictionary<string,TSyntaxHiliteBrushStyle>;
      fBuiltIn: Boolean;
    procedure SetFontName(const FontName: string);
    procedure SetFontSize(const FontSize: Integer);
    function GetBrushStyle(const BrushID: string): TSyntaxHiliteBrushStyle;
    function GetSupportedBrushes: TArray<string>;
    procedure SetDefaultBrushStyle(const Value: TSyntaxHiliteBrushStyle);
    function GetBaseStyle: TSyntaxHiliteAttrStyle;
    procedure SetStyles(const Src: TSyntaxHiliteTheme;
      const IgnoreColour: Boolean);
    function CascadeAttrStyles(const ParentAttr, ChildAttr:
      TSyntaxHiliteAttrStyle): TSyntaxHiliteAttrStyle;
  public
    constructor Create(const ThemeID: TSyntaxHiliteThemeID;
      const FriendlyName: string; const IsBuiltIn: Boolean);
    destructor Destroy; override;
    procedure Assign(const Src: TSyntaxHiliteTheme;
      const IgnoreColour: Boolean = False; const KeepID: Boolean = False);
    function Clone(const IgnoreColour: Boolean = False): TSyntaxHiliteTheme;
      overload;
    function Clone(const NewID: TSyntaxHiliteThemeID;
      const IgnoreColour: Boolean = False): TSyntaxHiliteTheme; overload;
    procedure ResetDefaultFont;
    function IsNull: Boolean; virtual;
    function IsBrushSupported(const BrushID: string): Boolean;
    class function IsValidIDString(const S: string; const IsBuiltIn: Boolean):
      Boolean;
    class function CompareIDs(const Left, Right: TSyntaxHiliteTheme):
      Integer; overload; inline;
    class function CompareIDs(const Left, Right: TSyntaxHiliteThemeID): Integer;
      overload;
    class function ValidFontSizes: TRange;
    // GetStyle => replaces any "default" style place markers with actual values
    //             from default or common style.
    function GetStyle(const BrushId, AttrId: string): TSyntaxHiliteAttrStyle;
    function GetDefaultStyle(const AttrID: string): TSyntaxHiliteAttrStyle;
    function GetEnumerator: TEnumerator<TPair<string,TSyntaxHiliteBrushStyle>>;
    function IsBaseStyle(const Style: TSyntaxHiliteAttrStyle): Boolean;
    property ID: TSyntaxHiliteThemeID read fID;
    property FriendlyName: string read fFriendlyName write fFriendlyName;
    property FontName: string read fFontName write SetFontName;
    property FontSize: Integer read fFontSize write SetFontSize;
    property DefaultBackground: TColor
      read fDefaultBackground write fDefaultBackground;
    property DefaultForeground: TColor
      read fDefaultForeground write fDefaultForeground;
    property DefaultBrushStyle: TSyntaxHiliteBrushStyle
      read fDefaultBrushStyle write SetDefaultBrushStyle;
    // This makes copy of given style
    procedure AddBrushStyle(const BrushID: string;
      const Style: TSyntaxHiliteBrushStyle);
    property SupportedBrushes: TArray<string> read GetSupportedBrushes;
    property BrushStyles[const BrushID: string]: TSyntaxHiliteBrushStyle
      read GetBrushStyle; default;
    property BuiltIn: Boolean read fBuiltIn write fBuiltIn;
    property BaseStyle: TSyntaxHiliteAttrStyle read GetBaseStyle;
  end;

  TSyntaxHiliteThemes = class(TObject)
  strict private
    type
      TNullTheme = class(TSyntaxHiliteTheme)
      public
        constructor Create;
        function IsNull: Boolean; override;
      end;
  strict private
    class var
      fNullTheme: TSyntaxHiliteTheme;
    var
      fThemes: TObjectDictionary<TSyntaxHiliteThemeID,TSyntaxHiliteTheme>;
    function GetTheme(const ID: TSyntaxHiliteThemeID): TSyntaxHiliteTheme;
    function GetDefaultTheme: TSyntaxHiliteTheme;
    class function GetNullTheme: TSyntaxHiliteTheme; static;
  public
    constructor Create;
    destructor Destroy; override;
    class destructor Destroy;
    procedure Assign(const Src: TSyntaxHiliteThemes);
    procedure Add(const Theme: TSyntaxHiliteTheme);
    procedure Clear;
    procedure Delete(const ThemeID: TSyntaxHiliteThemeID);
    function HasTheme(const ThemeID: TSyntaxHiliteThemeID): Boolean;
    function SupportedThemes: TArray<TSyntaxHiliteThemeID>;
    function UniqueIDString: TSyntaxHiliteThemeID;
    function FindThemeByFriendlyName(const AName: string): TSyntaxHiliteTheme;
    function GetEnumerator: TEnumerator<TSyntaxHiliteTheme>;
    ///  <summary>Array of themes, indexed by theme id string.</summary>
    ///  <remarks>Callers must NOT free theme instances obtained from this
    ///  property. Use the Delete method to dispose of a theme.</remarks>
    property Themes[const ID: TSyntaxHiliteThemeID]: TSyntaxHiliteTheme
      read GetTheme; default;
    ///  <summary>Returns the default theme instance, or if none exists, the
    ///  null theme instance.</summary>
    ///  <remarks>Callers must NOT free this object.</remarks>
    property DefaultTheme: TSyntaxHiliteTheme read GetDefaultTheme;
    ///  <summary>Provides a null theme instance.</summary>
    ///  <remarks>Callers must NOT free this object.</remarks>
    class property NullTheme: TSyntaxHiliteTheme read GetNullTheme;
  end;

implementation

uses
  SysUtils,
  Character,
  CS.SourceCode.Hiliter.Brushes,
  CS.Utils.Hashes,
  UComparers,
  UExceptions,
  UFontHelper,
  UStrUtils;

{ TSyntaxHiliteFontStyles }

class function TSyntaxHiliteFontStyles.CreateDefault: TSyntaxHiliteFontStyles;
begin
  Result.fIsDefault := True;
  Result.fStyles := [];
end;

class function TSyntaxHiliteFontStyles.CreateNull: TSyntaxHiliteFontStyles;
begin
  Result := CreateStyles([]);
end;

class function TSyntaxHiliteFontStyles.CreateStyles(
  const FS: TFontStyles): TSyntaxHiliteFontStyles;
begin
  Assert(not (fsStrikeout in FS),
    'TSyntaxHiliteFontStyles.CreateStyles: Strikeout font style not allowed');
  Result.fIsDefault := False;
  Result.fStyles := FS;
end;

function TSyntaxHiliteFontStyles.GetStyles: TFontStyles;
begin
  Assert(not IsDefault, 'TSyntaxHiliteFontStyles.GetStyles: Style is default');
  Result := fStyles;
end;

class operator TSyntaxHiliteFontStyles.Implicit(
  const S: TSyntaxHiliteFontStyles): TFontStyles;
begin
  Result := S.Styles;
end;

function TSyntaxHiliteFontStyles.IsNull: Boolean;
begin
  Result := not IsDefault and (Styles = []);
end;

{ TSyntaxHiliteAttrStyle }

function TSyntaxHiliteAttrStyle.Clone(const IgnoreColour: Boolean):
  TSyntaxHiliteAttrStyle;
begin
  if IgnoreColour then
    Result := TSyntaxHiliteAttrStyle.Create(clNone, clNone, FontStyles)
  else
    Result := TSyntaxHiliteAttrStyle.Create(Background, Foreground, FontStyles);
end;

constructor TSyntaxHiliteAttrStyle.Create(ABackground, AForeground: TColor;
  AFontStyles: TSyntaxHiliteFontStyles);
begin
  Background := ABackground;
  Foreground := AForeground;
  FontStyles := AFontStyles;
end;

class function TSyntaxHiliteAttrStyle.CreateNull: TSyntaxHiliteAttrStyle;
begin
  Result := TSyntaxHiliteAttrStyle.Create(
    clNone, clNone, TSyntaxHiliteFontStyles.CreateNull
  );
end;

class operator TSyntaxHiliteAttrStyle.Equal(const Left,
  Right: TSyntaxHiliteAttrStyle): Boolean;
begin
  Result := (Left.Background = Right.Background)
    and (Left.Foreground = Right.Foreground)
    and (Left.FontStyles.Styles = Right.FontStyles.Styles);
end;

function TSyntaxHiliteAttrStyle.IsNull: Boolean;
begin
  Result := (Background = clNone)
    and (ForeGround = clNone)
    and FontStyles.IsNull;
end;

class function TSyntaxHiliteAttrStyle.IsValidIDString(const S: string): Boolean;
var
  Ch: Char;
begin
  // Requirements are those for a SynEdit attribute name, i.e. one or more of
  // 'A'..'Z', 'a'..'z', '0'..'9', '_' or '-'
  if S = EmptyStr then
    Exit(False);
  for Ch in S do
  begin
    if not CharInSet(Ch, ['A'..'Z', 'a'..'z', '0'..'9', '_', '-']) then
      Exit(False);
  end;
  Result := True;
end;

class operator TSyntaxHiliteAttrStyle.NotEqual(const Left,
  Right: TSyntaxHiliteAttrStyle): Boolean;
begin
  Result := not (Left = Right);
end;

{ TSyntaxHiliteBrushStyle }

procedure TSyntaxHiliteBrushStyle.Add(const AttrID: string;
  const Style: TSyntaxHiliteAttrStyle);
begin
  fAttrStyles.Add(AttrID, Style);
end;

procedure TSyntaxHiliteBrushStyle.Assign(const Src: TSyntaxHiliteBrushStyle;
  const IgnoreColour: Boolean);
var
  AttrStylePair: TPair<string,TSyntaxHiliteAttrStyle>;
begin
  Clear;
  for AttrStylePair in Src.fAttrStyles do
    fAttrStyles.Add(AttrStylePair.Key, AttrStylePair.Value.Clone(IgnoreColour));
end;

procedure TSyntaxHiliteBrushStyle.Clear;
begin
  fAttrStyles.Clear;
end;

function TSyntaxHiliteBrushStyle.Clone(const IgnoreColour: Boolean):
  TSyntaxHiliteBrushStyle;
begin
  Result := TSyntaxHiliteBrushStyle.Create;
  Result.Assign(Self, IgnoreColour);
end;

constructor TSyntaxHiliteBrushStyle.Create;
begin
  inherited Create;
  fAttrStyles := TDictionary<string,TSyntaxHiliteAttrStyle>.Create(
    TTextEqualityComparer.Create
  )
end;

destructor TSyntaxHiliteBrushStyle.Destroy;
begin
  fAttrStyles.Free;
  inherited;
end;

function TSyntaxHiliteBrushStyle.GetAttrStyle(
  const AttrId: string): TSyntaxHiliteAttrStyle;
begin
  if fAttrStyles.ContainsKey(AttrId) then
    Result := fAttrStyles[AttrId]
  else
    Result := TSyntaxHiliteAttrStyle.CreateNull;
end;

function TSyntaxHiliteBrushStyle.GetEnumerator:
  TEnumerator<TPair<string, TSyntaxHiliteAttrStyle>>;
begin
  Result := fAttrStyles.GetEnumerator;
end;

function TSyntaxHiliteBrushStyle.GetSupportedAttrs: TArray<string>;
begin
  Result := fAttrStyles.Keys.ToArray;
end;

function TSyntaxHiliteBrushStyle.IsAttrSupported(const AttrID: string): Boolean;
begin
  Result := fAttrStyles.ContainsKey(AttrID);
end;

class function TSyntaxHiliteBrushStyle.IsValidIDString(
  const S: string): Boolean;
begin
  Result := TSyntaxHiliterBrush.IsValidBrushID(S);
end;

procedure TSyntaxHiliteBrushStyle.SetAttrStyle(const AttrID: string;
  const NewStyle: TSyntaxHiliteAttrStyle);
begin
  if fAttrStyles.ContainsKey(AttrID) then
    fAttrStyles[AttrID] := NewStyle
  else
    fAttrStyles.Add(AttrID, NewStyle);
end;

{ TSyntaxHiliteThemeID }

class function TSyntaxHiliteThemeID.Compare(const Left,
  Right: TSyntaxHiliteThemeID): Integer;
begin
  Result := StrCompareText(Left.fID, Right.fID);
end;

function TSyntaxHiliteThemeID.CompareTo(const Other: TSyntaxHiliteThemeID):
  Integer;
begin
  Result := Compare(Self, Other);
end;

constructor TSyntaxHiliteThemeID.Create(const AID: string);
begin
  fID := AID;
end;

class function TSyntaxHiliteThemeID.CreateDefault: TSyntaxHiliteThemeID;
begin
  Result := TSyntaxHiliteThemeID.Create(DefaultID);
end;

class function TSyntaxHiliteThemeID.CreateNull: TSyntaxHiliteThemeID;
begin
  Result := TSyntaxHiliteThemeID.Create(EmptyStr);
end;

class operator TSyntaxHiliteThemeID.Equal(const Left,
  Right: TSyntaxHiliteThemeID): Boolean;
begin
  Result := Compare(Left, Right) = 0;
end;

function TSyntaxHiliteThemeID.Hash: Integer;
begin
  Result := Integer(PaulLarsonHash(fID));
end;

function TSyntaxHiliteThemeID.IsNull: Boolean;
begin
  Result := fID = EmptyStr;
end;

class operator TSyntaxHiliteThemeID.NotEqual(const Left,
  Right: TSyntaxHiliteThemeID): Boolean;
begin
  Result := Compare(Left, Right) <> 0;
end;

function TSyntaxHiliteThemeID.ToString: string;
begin
  Result := fID;
end;

{ TSyntaxHiliteThemeID.TComparer }

function TSyntaxHiliteThemeID.TComparer.Compare(const Left,
  Right: TSyntaxHiliteThemeID): Integer;
begin
  Result := TSyntaxHiliteThemeID.Compare(Left, Right);
end;

{ TSyntaxHiliteThemeID.TEqualityComparer }

function TSyntaxHiliteThemeID.TEqualityComparer.Equals(const Left,
  Right: TSyntaxHiliteThemeID): Boolean;
begin
  Result := Left = Right;
end;

function TSyntaxHiliteThemeID.TEqualityComparer.GetHashCode(
  const Value: TSyntaxHiliteThemeID): Integer;
begin
  Result := Value.Hash;
end;

{ TSyntaxHiliteTheme }

procedure TSyntaxHiliteTheme.AddBrushStyle(const BrushID: string;
  const Style: TSyntaxHiliteBrushStyle);
begin
  fBrushStyles.Add(BrushID, Style);
end;

procedure TSyntaxHiliteTheme.Assign(const Src: TSyntaxHiliteTheme;
  const IgnoreColour: Boolean; const KeepID: Boolean);
begin
  if not KeepID then
    fID := Src.ID;
  fFriendlyName := Src.FriendlyName;
  fBuiltIn := Src.BuiltIn;
  fFontName := Src.FontName;
  fFontSize := Src.FontSize;
  fDefaultForeground := Src.DefaultForeground;
  fDefaultBackground := Src.DefaultBackground;
  SetStyles(Src, IgnoreColour);
end;

function TSyntaxHiliteTheme.CascadeAttrStyles(const ParentAttr,
  ChildAttr: TSyntaxHiliteAttrStyle): TSyntaxHiliteAttrStyle;
begin
  Result := ChildAttr;
  if ChildAttr.Background = clDefault then
    Result.Background := ParentAttr.Background;
  if ChildAttr.Foreground = clDefault then
    Result.Foreground := ParentAttr.Foreground;
  if ChildAttr.FontStyles.IsDefault then
    Result.FontStyles := ParentAttr.FontStyles;
end;

function TSyntaxHiliteTheme.Clone(const IgnoreColour: Boolean):
  TSyntaxHiliteTheme;
begin
  Result := TSyntaxHiliteTheme.Create(ID, FriendlyName, BuiltIn);
  Result.Assign(Self, IgnoreColour);
end;

function TSyntaxHiliteTheme.Clone(const NewID: TSyntaxHiliteThemeID;
  const IgnoreColour: Boolean): TSyntaxHiliteTheme;
begin
  Result := Clone(IgnoreColour);
  Result.fID := NewID;
end;

class function TSyntaxHiliteTheme.CompareIDs(const Left,
  Right: TSyntaxHiliteThemeID): Integer;
begin
  Result := TSyntaxHiliteThemeID.Compare(Left, Right);
end;

class function TSyntaxHiliteTheme.CompareIDs(const Left,
  Right: TSyntaxHiliteTheme): Integer;
begin
  Result := CompareIDs(Left.ID, Right.ID);
end;

constructor TSyntaxHiliteTheme.Create(const ThemeID: TSyntaxHiliteThemeID;
  const FriendlyName: string; const IsBuiltIn: Boolean);
begin
  inherited Create;
  fBrushStyles := TObjectDictionary<string,TSyntaxHiliteBrushStyle>.Create(
    [doOwnsValues],
    TTextEqualityComparer.Create
  );
  fID := ThemeID;
  fFriendlyName := FriendlyName;
  fBuiltIn := IsBuiltIn;
  fFontName := TFontHelper.GetDefaultMonoFontName;
  fFontSize := DefaultFontSize;
  fDefaultForeground := clNone;
  fDefaultBackground := clNone;
  fDefaultBrushStyle := TSyntaxHiliteBrushStyle.Create;
end;

destructor TSyntaxHiliteTheme.Destroy;
begin
  fDefaultBrushStyle.Free;
  fBrushStyles.Free; // frees value objects
  inherited;
end;

function TSyntaxHiliteTheme.GetBaseStyle: TSyntaxHiliteAttrStyle;
begin
  Result := TSyntaxHiliteAttrStyle.Create(
    fDefaultBackground, fDefaultForeground, TSyntaxHiliteFontStyles.CreateNull
  );
end;

function TSyntaxHiliteTheme.GetBrushStyle(const BrushID: string):
  TSyntaxHiliteBrushStyle;
begin
  Assert(fBrushStyles.ContainsKey(BrushID),
    ClassName + '.GetBrushStyle:: BrushID does not exist');
  Result := fBrushStyles[BrushID];
end;

function TSyntaxHiliteTheme.GetDefaultStyle(const AttrID: string):
  TSyntaxHiliteAttrStyle;
begin
  Result := TSyntaxHiliteAttrStyle.Create(
    fDefaultBackground, fDefaultForeground, TSyntaxHiliteFontStyles.CreateNull
  );
  if fDefaultBrushStyle.IsAttrSupported(AttrId) then
    Result := CascadeAttrStyles(Result, fDefaultBrushStyle.AttrStyles[AttrId]);
end;

function TSyntaxHiliteTheme.GetEnumerator:
  TEnumerator<TPair<string, TSyntaxHiliteBrushStyle>>;
begin
  Result := fBrushStyles.GetEnumerator;
end;

function TSyntaxHiliteTheme.GetStyle(const BrushId,
  AttrId: string): TSyntaxHiliteAttrStyle;
var
  BrushStyle: TSyntaxHiliteBrushStyle;
begin
  Result := GetDefaultStyle(AttrId);
  if not fBrushStyles.ContainsKey(BrushId) then
    Exit;
  BrushStyle := fBrushStyles[BrushId];
  if not BrushStyle.IsAttrSupported(AttrId) then
    Exit;
  Result := CascadeAttrStyles(Result, BrushStyle.AttrStyles[AttrId]);
end;

function TSyntaxHiliteTheme.GetSupportedBrushes: TArray<string>;
begin
  Result := fBrushStyles.Keys.ToArray;
end;

function TSyntaxHiliteTheme.IsBaseStyle(const Style: TSyntaxHiliteAttrStyle):
  Boolean;
begin
  Result := Style = GetBaseStyle;
end;

function TSyntaxHiliteTheme.IsBrushSupported(const BrushID: string): Boolean;
begin
  Result := fBrushStyles.ContainsKey(BrushID);
end;

function TSyntaxHiliteTheme.IsNull: Boolean;
begin
  Result := False;
end;

class function TSyntaxHiliteTheme.IsValidIDString(const S: string;
  const IsBuiltIn: Boolean): Boolean;

  function IsValidBody(const S: string): Boolean;
  var
    Idx: Integer;
  begin
    if not TCharacter.IsLetter(S[1]) then
      Exit(False);
    for Idx := 2 to Length(S) do
      if not TCharacter.IsLetterOrDigit(S[Idx])
        and not TCharacter.IsPunctuation(S[Idx]) then
        Exit(False);
    Result := True;
  end;

begin
  if IsBuiltIn then
  begin
    if Length(S) < 3 then
      Exit(False);
    if (S[1] <> '_') and (S[Length(S)] <> '_') then
      Exit(False);
    Result := IsValidBody(StrSlice(S, 2, Length(S) - 2));
  end
  else
  begin
    if S = EmptyStr then
      Exit(False);
    Result := IsValidBody(S);
  end;
end;

procedure TSyntaxHiliteTheme.ResetDefaultFont;
begin
  fFontName := TFontHelper.GetDefaultMonoFontName;
  fFontSize := DefaultFontSize;
end;

procedure TSyntaxHiliteTheme.SetDefaultBrushStyle(
  const Value: TSyntaxHiliteBrushStyle);
begin
  fDefaultBrushStyle.Assign(Value);
end;

procedure TSyntaxHiliteTheme.SetFontName(const FontName: string);
begin
  if TFontHelper.FontExists(FontName) then
    fFontName := FontName
  else
    fFontName := TFontHelper.GetDefaultMonoFontName;
end;

procedure TSyntaxHiliteTheme.SetFontSize(const FontSize: Integer);
begin
  fFontSize := ValidFontSizes.Constrain(FontSize);
end;

procedure TSyntaxHiliteTheme.SetStyles(const Src: TSyntaxHiliteTheme;
  const IgnoreColour: Boolean);
var
  BrushStylePair: TPair<string,TSyntaxHiliteBrushStyle>;
  ClonedBrushStyle: TSyntaxHiliteBrushStyle;
  ClonedDefBrushStyle: TSyntaxHiliteBrushStyle;
begin
  fBrushStyles.Clear;
  for BrushStylePair in Src.fBrushStyles do
  begin
    ClonedBrushStyle := BrushStylePair.Value.Clone(IgnoreColour);
    fBrushStyles.Add(BrushStylePair.Key, ClonedBrushStyle);
  end;
  ClonedDefBrushStyle := Src.DefaultBrushStyle.Clone(IgnoreColour);
  try
    SetDefaultBrushStyle(ClonedDefBrushStyle);
  finally
    ClonedDefBrushStyle.Free;
  end;
end;

class function TSyntaxHiliteTheme.ValidFontSizes: TRange;
begin
  Result := TRange.Create(MinFontSize, MaxFontSize);
end;

{ TSyntaxHiliteThemes }

procedure TSyntaxHiliteThemes.Add(const Theme: TSyntaxHiliteTheme);
begin
  fThemes.Add(Theme.ID, Theme);
end;

procedure TSyntaxHiliteThemes.Assign(const Src: TSyntaxHiliteThemes);
var
  ThemeID: TSyntaxHiliteThemeID;  // each theme ID supported by Src
begin
  Clear;
  for ThemeID in Src.SupportedThemes do
    fThemes.Add(ThemeID, Src.GetTheme(ThemeID).Clone);
end;

procedure TSyntaxHiliteThemes.Clear;
begin
  fThemes.Clear;
end;

constructor TSyntaxHiliteThemes.Create;
begin
  inherited Create;
  fThemes := TObjectDictionary<TSyntaxHiliteThemeID,TSyntaxHiliteTheme>.Create(
    [doOwnsValues],
    TSyntaxHiliteThemeID.TEqualityComparer.Create
  );
end;

procedure TSyntaxHiliteThemes.Delete(const ThemeID: TSyntaxHiliteThemeID);
begin
  if fThemes.ContainsKey(ThemeID) then
    fThemes.Remove(ThemeID);
end;

class destructor TSyntaxHiliteThemes.Destroy;
begin
  fNullTheme.Free;
end;

function TSyntaxHiliteThemes.FindThemeByFriendlyName(const AName: string):
  TSyntaxHiliteTheme;
var
  Theme: TSyntaxHiliteTheme;
begin
  for Theme in Self do
    if StrSameText(Theme.FriendlyName, AName) then
      Exit(Theme);
  Result := nil;
end;

destructor TSyntaxHiliteThemes.Destroy;
begin
  fThemes.Free;
  inherited;
end;

function TSyntaxHiliteThemes.GetDefaultTheme: TSyntaxHiliteTheme;
begin
  if fThemes.ContainsKey(TSyntaxHiliteThemeID.CreateDefault) then
    Result := GetTheme(TSyntaxHiliteThemeID.CreateDefault)
  else
    Result := GetNullTheme;
end;

function TSyntaxHiliteThemes.GetEnumerator: TEnumerator<TSyntaxHiliteTheme>;
begin
  Result := fThemes.Values.GetEnumerator;
end;

class function TSyntaxHiliteThemes.GetNullTheme: TSyntaxHiliteTheme;
begin
  if not Assigned(fNullTheme) then
    fNullTheme := TNullTheme.Create;
  Result := fNullTheme;
end;

function TSyntaxHiliteThemes.GetTheme(const ID: TSyntaxHiliteThemeID):
  TSyntaxHiliteTheme;
begin
  Result := fThemes[ID];
end;

function TSyntaxHiliteThemes.HasTheme(const ThemeID: TSyntaxHiliteThemeID):
  Boolean;
begin
  Result := fThemes.ContainsKey(ThemeID);
end;

function TSyntaxHiliteThemes.SupportedThemes: TArray<TSyntaxHiliteThemeID>;
begin
  Result := fThemes.Keys.ToArray;
end;

function TSyntaxHiliteThemes.UniqueIDString: TSyntaxHiliteThemeID;
const
  Base = 'Theme';
var
  Suffix: Cardinal;
begin
  Suffix := 0;
  repeat
    Inc(Suffix);
    Result := TSyntaxHiliteThemeID.Create(Base + IntToStr(Suffix));
  until not fThemes.ContainsKey(Result);
end;

{ TSyntaxHiliteThemes.TNullTheme }

constructor TSyntaxHiliteThemes.TNullTheme.Create;
begin
  inherited Create(TSyntaxHiliteThemeID.CreateNull, EmptyStr, False);
end;

function TSyntaxHiliteThemes.TNullTheme.IsNull: Boolean;
begin
  Result := True;
end;

end.


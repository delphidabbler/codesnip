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


unit CS.Hiliter.Themes;

interface

uses
  Generics.Collections,
  Graphics;

type

  TSyntaxHiliteFontStyle = (hfsDefault, hfsBold, hfsItalic, hfsUnderline);

  TSyntaxHiliteFontStyles = set of TSyntaxHiliteFontStyle;

  TSyntaxHiliteAttrStyle = record
    Background: TColor;
    Foreground: TColor;
    FontStyles: TSyntaxHiliteFontStyles;
    constructor Create(ABackground, AForeground: TColor;
      AFontStyles: TSyntaxHiliteFontStyles);
    class function CreateNull: TSyntaxHiliteAttrStyle; static;
    function IsNull: Boolean;
    class operator Equal(const Left, Right: TSyntaxHiliteAttrStyle): Boolean;
    class operator NotEqual(const Left, Right: TSyntaxHiliteAttrStyle): Boolean;
  end;

  TSyntaxHiliteBrushStyle = class(TObject)
  strict private
    var
      fAttrStyles: TDictionary<string,TSyntaxHiliteAttrStyle>;
    function GetSupportedAttrs: TArray<string>;
    function GetAttrStyle(const AttrId: string): TSyntaxHiliteAttrStyle;
  public
    constructor Create;
    destructor Destroy; override;
    procedure Assign(const Src: TSyntaxHiliteBrushStyle);
    procedure Add(const AttrID: string; const Style: TSyntaxHiliteAttrStyle);
    procedure Clear;
    function IsAttrSupported(const AttrID: string): Boolean;
    function GetEnumerator: TEnumerator<TPair<string,TSyntaxHiliteAttrStyle>>;
    property SupportedAttrs: TArray<string> read GetSupportedAttrs;
    // AttrStyles => return null attr if style doesn't exist
    property AttrStyles[const AttrId: string]: TSyntaxHiliteAttrStyle
      read GetAttrStyle;
  end;

  TSyntaxHiliteTheme = class(TObject)
  strict private
    var
      fID: string;
      fFriendlyName: string;
      fDefaultBrushStyle: TSyntaxHiliteBrushStyle;
      fBrushStyles: TObjectDictionary<string,TSyntaxHiliteBrushStyle>;
      fBuiltIn: Boolean;
    function GetBrushStyle(const BrushID: string): TSyntaxHiliteBrushStyle;
    function GetSupportedBrushes: TArray<string>;
    procedure SetDefaultBrushStyle(const Value: TSyntaxHiliteBrushStyle);
  public
    constructor Create(const ThemeID: string; const FriendlyName: string;
      const IsBuiltIn: Boolean);
    destructor Destroy; override;
    procedure Assign(const Src: TSyntaxHiliteTheme);
    procedure SetStyles(const Src: TSyntaxHiliteTheme);
    function IsBrushSupported(const BrushID: string): Boolean;
    // GetStyle => return Default style if no brush or attr found
    //          => return null style if no default style
    function GetStyle(const BrushId, AttrId: string): TSyntaxHiliteAttrStyle;
    function GetEnumerator: TEnumerator<TPair<string,TSyntaxHiliteBrushStyle>>;
    property ID: string read fID;
    property FriendlyName: string read fFriendlyName write fFriendlyName;
    property DefaultBrushStyle: TSyntaxHiliteBrushStyle
      read fDefaultBrushStyle write SetDefaultBrushStyle;
    // This makes copy of given style
    procedure AddBrushStyle(const BrushID: string;
      const Style: TSyntaxHiliteBrushStyle);
    property SupportedBrushes: TArray<string> read GetSupportedBrushes;
    // BrushStyles => return Default brush style if BrushID not supported
    property BrushStyles[const BrushID: string]: TSyntaxHiliteBrushStyle
      read GetBrushStyle; default;
    property BuiltIn: Boolean read fBuiltIn;
  end;

  TSyntaxHiliteThemes = class(TObject)
  strict private
    var
      fThemes: TObjectDictionary<string,TSyntaxHiliteTheme>;
    function GetTheme(const ID: string): TSyntaxHiliteTheme;
  public
    constructor Create;
    destructor Destroy; override;
    procedure Add(const Theme: TSyntaxHiliteTheme);
    procedure Clear;
    procedure Delete(const ThemeID: string);
    function HasTheme(const ThemeID: string): Boolean;
    function SupportedThemes: TArray<string>;
    function GetEnumerator: TEnumerator<TSyntaxHiliteTheme>;
    property Themes[const ID: string]: TSyntaxHiliteTheme
      read GetTheme; default;
  end;

implementation

uses
  UComparers;

{ TSyntaxHiliteAttrStyle }

constructor TSyntaxHiliteAttrStyle.Create(ABackground, AForeground: TColor;
  AFontStyles: TSyntaxHiliteFontStyles);
begin
  Background := ABackground;
  Foreground := AForeground;
  FontStyles := AFontStyles;
end;

class function TSyntaxHiliteAttrStyle.CreateNull: TSyntaxHiliteAttrStyle;
begin
  Result := TSyntaxHiliteAttrStyle.Create(clNone, clNone, []);
end;

class operator TSyntaxHiliteAttrStyle.Equal(const Left,
  Right: TSyntaxHiliteAttrStyle): Boolean;
begin
  Result := (Left.Background = Right.Background)
    and (Left.Foreground = Right.Background)
    and (Left.FontStyles = Right.FontStyles);
end;

function TSyntaxHiliteAttrStyle.IsNull: Boolean;
begin
  Result := (Background = clNone)
    and (ForeGround = clNone)
    and (FontStyles = []);
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

procedure TSyntaxHiliteBrushStyle.Assign(const Src: TSyntaxHiliteBrushStyle);
var
  AttrStylePair: TPair<string,TSyntaxHiliteAttrStyle>;
begin
  Clear;
  for AttrStylePair in Src.fAttrStyles do
    fAttrStyles.Add(AttrStylePair.Key, AttrStylePair.Value);
end;

procedure TSyntaxHiliteBrushStyle.Clear;
begin
  fAttrStyles.Free;
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

{ TSyntaxHiliteTheme }

procedure TSyntaxHiliteTheme.AddBrushStyle(const BrushID: string;
  const Style: TSyntaxHiliteBrushStyle);
begin
  fBrushStyles.Add(BrushID, Style);
end;

procedure TSyntaxHiliteTheme.Assign(const Src: TSyntaxHiliteTheme);
begin
  fID := Src.ID;
  fFriendlyName := Src.FriendlyName;
  fBuiltIn := Src.BuiltIn;
  SetStyles(Src);
end;

constructor TSyntaxHiliteTheme.Create(const ThemeID: string;
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
  fDefaultBrushStyle := TSyntaxHiliteBrushStyle.Create;
end;

destructor TSyntaxHiliteTheme.Destroy;
begin
  fDefaultBrushStyle.Free;
  fBrushStyles.Free; // frees value objects
  inherited;
end;

function TSyntaxHiliteTheme.GetBrushStyle(
  const BrushID: string): TSyntaxHiliteBrushStyle;
begin
  if fBrushStyles.ContainsKey(BrushID) then
    Result := fBrushStyles[BrushID]
  else
    Result := fDefaultBrushStyle;
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
  BrushStyle := GetBrushStyle(BrushId);
  Result := BrushStyle.AttrStyles[AttrId];
end;

function TSyntaxHiliteTheme.GetSupportedBrushes: TArray<string>;
begin
  Result := fBrushStyles.Keys.ToArray;
end;

function TSyntaxHiliteTheme.IsBrushSupported(const BrushID: string): Boolean;
begin
  Result := fBrushStyles.ContainsKey(BrushID);
end;

procedure TSyntaxHiliteTheme.SetDefaultBrushStyle(
  const Value: TSyntaxHiliteBrushStyle);
begin
  fDefaultBrushStyle.Assign(Value);
end;

procedure TSyntaxHiliteTheme.SetStyles(const Src: TSyntaxHiliteTheme);
var
  BrushStylePair: TPair<string,TSyntaxHiliteBrushStyle>;
  ClonedBrushStyle: TSyntaxHiliteBrushStyle;
begin
  fBrushStyles.Clear;
  for BrushStylePair in Src.fBrushStyles do
  begin
    ClonedBrushStyle := TSyntaxHiliteBrushStyle.Create;
    ClonedBrushStyle.Assign(BrushStylePair.Value);
    fBrushStyles.Add(BrushStylePair.Key, ClonedBrushStyle);
  end;
  fDefaultBrushStyle.Assign(Src.DefaultBrushStyle);
end;

{ TSyntaxHiliteThemes }

procedure TSyntaxHiliteThemes.Add(const Theme: TSyntaxHiliteTheme);
begin
  fThemes.Add(Theme.ID, Theme);
end;

procedure TSyntaxHiliteThemes.Clear;
begin
  fThemes.Clear;
end;

constructor TSyntaxHiliteThemes.Create;
begin
  inherited Create;
  fThemes := TObjectDictionary<string,TSyntaxHiliteTheme>.Create(
    [doOwnsValues],
    TTextEqualityComparer.Create
  );
end;

procedure TSyntaxHiliteThemes.Delete(const ThemeID: string);
begin
  if fThemes.ContainsKey(ThemeID) then
    fThemes.Remove(ThemeID);
end;

destructor TSyntaxHiliteThemes.Destroy;
begin
  fThemes.Free;
  inherited;
end;

function TSyntaxHiliteThemes.GetEnumerator: TEnumerator<TSyntaxHiliteTheme>;
begin
  Result := fThemes.Values.GetEnumerator;
end;

function TSyntaxHiliteThemes.GetTheme(const ID: string): TSyntaxHiliteTheme;
begin
  Result := fThemes[ID];
end;

function TSyntaxHiliteThemes.HasTheme(const ThemeID: string): Boolean;
begin
  Result := fThemes.ContainsKey(ThemeID);
end;

function TSyntaxHiliteThemes.SupportedThemes: TArray<string>;
begin
  Result := fThemes.Keys.ToArray;
end;

end.

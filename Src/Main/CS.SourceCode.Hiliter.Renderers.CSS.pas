{
 * This Source Code Form is subject to the terms of the Mozilla Public License,
 * v. 2.0. If a copy of the MPL was not distributed with this file, You can
 * obtain one at http://mozilla.org/MPL/2.0/
 *
 * Copyright (C) 2006-2016, Peter Johnson (www.delphidabbler.com).
 *
 * $Rev$
 * $Date$
 *
 * Defines a class that generates CSS code that enables syntax highlighted
 * source code rendered as HTML to be styled according to a given highlighter
 * theme.
}


unit CS.SourceCode.Hiliter.Renderers.CSS;


interface


uses
  // Project
  CS.SourceCode.Hiliter.Brushes,
  CS.SourceCode.Hiliter.Themes,
  UCSSBuilder;


type

  ///  <summary>
  ///  Class that generates CSS code that enables syntax highlighted source code
  ///  rendered as HTML to be styled according to a given highlighter theme.
  ///  </summary>
  ///  <remarks>
  ///  Also exposes the names of the CSS class names used.
  ///  </remarks>
  THiliterCSS = class(TObject)
  strict private
    var
      ///  <summary>Syntax highlighter theme for which suitable CSS is to be
      ///  generated.</summary>
      fTheme: TSyntaxHiliteTheme;
    ///  <summary>Builds a CSS class for styling a given highlighter attribute
    ///  within a given highlighter brush.</summary>
    ///  <param name="BrushID"><c>string</c> [in] ID of highlighter brush.
    ///  </param>
    ///  <param name="AttrID"><c>string</c> [in] ID of attribute within brush
    ///  for which CSS is required.</param>
    ///  <param name="CSSBuilder"><c>TCSSBuilder</c> [in] Object used to build
    ///  the CSS.</param>
    procedure BuildAttrCSS(const BrushID, AttrID: string;
      const CSSBuilder: TCSSBuilder);
    ///  <summary>Builds a CSS class for styling that is common to the whole
    ///  theme.</summary>
    ///  <param name="CSSBuilder"><c>TCSSBuilder</c> [in] Object used to build
    ///  the CSS.</param>
    procedure BuildCommonThemeCSS(const CSSBuilder: TCSSBuilder);
  public
    ///  <summary>Constructs object instance to generate CSS required to style a
    ///  given highlighter theme.</summary>
    ///  <param name="Theme"><c>TSyntaxHiliteTheme</c> [in] Theme that specifies
    ///  syntax highlighter style.</param>
    constructor Create(const Theme: TSyntaxHiliteTheme);
    ///  <summary>Gets name of CSS class that applies to all highlighted code.
    ///  </summary>
    ///  <returns><c>string</c>. Required class name.</returns>
    class function GetMainCSSClassName: string;
    ///  <summary>Gets name of a CSS class for a given highlighter attribute
    ///  within a given highlighter brush.</summary>
    ///  <param name="BrushID"><c>string</c> [in] ID of highlighter brush.
    ///  </param>
    ///  <param name="AttrID"><c>string</c> [in] ID of attribute within brush
    ///  for which CSS class is required.</param>
    ///  <returns><c>string</c>. Required class name.</returns>
    class function GetElemCSSClassName(const BrushID, AttrID: string): string;
    ///  <summary>Builds CSS code required to style all the attributes of a
    ///  given highlighter brush.</summary>
    ///  <param name="ABrush"><c>TSyntaxHiliterBrush</c> [in] Syntax highlighter
    ///  brush for which CSS is required.</param>
    ///  <param name="CSSBuilder"><c>TCSSBuilder</c> [in] Object used to build
    ///  the CSS.</param>
    ///  <remarks>The CSS generated includes a class for every attribute of the
    ///  brush that needs to be individually styled.</remarks>
    procedure BuildBrushCSS(const ABrush: TSyntaxHiliterBrush;
      const CSSBuilder: TCSSBuilder);
    ///  <summary>Builds CSS code required to style the whole theme.</summary>
    ///  <param name="CSSBuilder"><c>TCSSBuilder</c> [in] Object used to build
    ///  the CSS.</param>
    ///  <remarks>The CSS generated includes a class for styling that applies to
    ///  the whole theme along with a class for every attribute of every brush
    ///  in the theme that needs to be individually styled.</remarks>
    procedure BuildThemeCSS(const CSSBuilder: TCSSBuilder);
  end;


implementation


uses
  // Delphi
  SysUtils,
  Graphics,
  // Project
  UCSSUtils,
  UStrUtils;


{ THiliterCSS }

procedure THiliterCSS.BuildAttrCSS(const BrushID, AttrID: string;
  const CSSBuilder: TCSSBuilder);
var
  AttrStyle: TSyntaxHiliteAttrStyle;
begin
  AttrStyle := fTheme.GetStyle(BrushID, AttrID);
  // We only create CSS class if element attribute's style differs from base
  // style
  if fTheme.IsBaseStyle(AttrStyle) then
    Exit;
  with CSSBuilder.AddSelector('.' + GetElemCSSClassName(BrushID, AttrID)) do
  begin
    if AttrStyle.Background <> fTheme.DefaultBackground then
      AddProperty(TCSS.BackgroundColorProp(AttrStyle.Background));
    if AttrStyle.Foreground <> fTheme.DefaultForeground then
      AddProperty(TCSS.ColorProp(AttrStyle.Foreground));
    AddProperty(TCSS.FontWeightProp(AttrStyle.FontStyles));
    AddProperty(TCSS.FontStyleProp(AttrStyle.FontStyles));
    AddProperty(TCSS.TextDecorationProp(AttrStyle.FontStyles));
  end;
end;

procedure THiliterCSS.BuildBrushCSS(const ABrush: TSyntaxHiliterBrush;
  const CSSBuilder: TCSSBuilder);
var
  Attrs: TArray<TSyntaxHiliterAttr>;
  Attr: TSyntaxHiliterAttr;
begin
  BuildCommonThemeCSS(CSSBuilder);
  Attrs := ABrush.SupportedAttrs;
  for Attr in Attrs do
    BuildAttrCSS(ABrush.ID, Attr.ID, CSSBuilder);
end;

procedure THiliterCSS.BuildCommonThemeCSS(const CSSBuilder: TCSSBuilder);
begin
  if CSSBuilder.Selectors['.' + GetMainCSSClassName] = nil then
  begin
    with CSSBuilder.AddSelector('.' + GetMainCSSClassName) do
    begin
      AddProperty(TCSS.FontFamilyProp(fTheme.FontName, cfgMonoSpace));
      AddProperty(TCSS.FontSizeProp(fTheme.FontSize));
      if fTheme.DefaultBackground <> clNone then
        AddProperty(TCSS.BackgroundColorProp(fTheme.DefaultBackground));
      if fTheme.DefaultForeground <> clNone then
        AddProperty(TCSS.ColorProp(fTheme.DefaultForeground));
    end;
  end;
end;

procedure THiliterCSS.BuildThemeCSS(const CSSBuilder: TCSSBuilder);
var
  BrushID: string;
  Brush: TSyntaxHiliterBrush;
  Attr: TSyntaxHiliterAttr;
begin
  BuildCommonThemeCSS(CSSBuilder);
  for BrushID in TSyntaxHiliterBrushes.SupportedBrushIDs do
  begin
    Brush := TSyntaxHiliterBrushes.CreateBrush(BrushID);
    try
      for Attr in Brush.SupportedAttrs do
        BuildAttrCSS(Brush.ID, Attr.ID, CSSBuilder);
    finally
      Brush.Free;
    end;
  end;
end;

constructor THiliterCSS.Create(const Theme: TSyntaxHiliteTheme);
begin
  inherited Create;
  Assert(Assigned(Theme), ClassName + '.Create: Theme is nil');
  fTheme := Theme;
end;

class function THiliterCSS.GetElemCSSClassName(const BrushID, AttrID: string):
  string;
var
  I: Integer;
begin
  Result := StrToLower(BrushID + '-' + AttrID);
  if Result = '-' then
    Result := '_null_'
  else
  begin
    if not CharInSet(Result[1], ['a'..'z', '_']) then
      Result[1] := '_';
    for I := 2 to Length(Result) do
    begin
      if not CharInSet(Result[I], ['a'..'z', '0'..'9', '-', '_']) then
        Result[I] := '_';
    end;
  end;
end;

class function THiliterCSS.GetMainCSSClassName: string;
begin
  Result := 'highlighted-source';
end;

end.


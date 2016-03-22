{
 * This Source Code Form is subject to the terms of the Mozilla Public License,
 * v. 2.0. If a copy of the MPL was not distributed with this file, You can
 * obtain one at http://mozilla.org/MPL/2.0/
 *
 * Copyright (C) 2006-2013, Peter Johnson (www.delphidabbler.com).
 *
 * $Rev$
 * $Date$
 *
 * Defines a class that generates CSS code to enable syntax highlighted source
 * to be displayed in HTML. CSS code uses a highlighter's attributes. Access to
 * CSS class names is also provided.
}


unit CS.SourceCode.Hiliter.Renderers.CSS;


interface


uses
  // Project
  CS.SourceCode.Hiliter.Brushes,
  CS.SourceCode.Hiliter.Themes,
  UCSSBuilder;

// TODO: fix documentation comments

type

  {
  THiliterCSS:
    Class generates CSS code to enable syntax highlighted source to be displayed
    in HTML. CSS code uses a highlighter's attributes. Access to CSS class names
    is also provided.
  }
  THiliterCSS = class(TObject)
  strict private
    var
      fTheme: TSyntaxHiliteTheme;
      {Highlighter for which CSS is to be generated}
    procedure BuildAttrCSS(const BrushID, AttrID: string;
      const CSSBuilder: TCSSBuilder);
      {Builds CSS class for an attribute.
        @param Elem [in] Highlighter element for which CSS is required.
        @param CSSBuilder [in] Object used to build and store the CSS.
      }
    procedure BuildCommonThemeCSS(const CSSBuilder: TCSSBuilder);
  public
    constructor Create(const Theme: TSyntaxHiliteTheme);
      {Class constructor. Sets up object ready to generate code for a syntax
      highlighter.
        @param HiliterAttrs [in] Attributes to be used in highlighter.
      }
    class function GetMainCSSClassName: string;
      {Gets name of main CSS class used for all highlighted code.
        @return Required class name.
      }
    class function GetElemCSSClassName(const BrushID, AttrID: string): string;
      {Gets name of CSS class associated with a highlighter element.
        @param Elem [in] Identifies element for which class name required.
        @return Required class name.
      }
    procedure BuildBrushCSS(const ABrush: TSyntaxHiliterBrush;
      const CSSBuilder: TCSSBuilder);
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
  // We only create CSS class if element attribute's style is non-null
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
  // Add font definition in main class
  BuildCommonThemeCSS(CSSBuilder);
  Attrs := ABrush.SupportedAttrs;
  for Attr in Attrs do
    BuildAttrCSS(ABrush.ID, Attr.ID, CSSBuilder);
end;

procedure THiliterCSS.BuildCommonThemeCSS(const CSSBuilder: TCSSBuilder);
begin
  // Add font definition in main class
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
  {Class constructor. Sets up object ready to generate code for a syntax
  highlighter.
    @param HiliterAttrs [in] Attributes to be used in highlighter.
  }
begin
  inherited Create;
  Assert(Assigned(Theme), ClassName + '.Create: Theme is nil');
  fTheme := Theme;
end;

class function THiliterCSS.GetElemCSSClassName(const BrushID, AttrID: string):
  string;
  {Gets name of CSS class associated with a highlighter element.
    @param Elem [in] Identifies element for which class name required.
    @return Required class name.
  }
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
  {Gets name of main CSS class used for all highlighted code.
    @return Required class name.
  }
begin
  Result := 'highlighted-source';
end;

end.


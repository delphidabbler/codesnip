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


unit CS.Hiliter.Renderers.CSS;


interface


uses
  // Project
  CS.Hiliter.Brushes,
  CS.Hiliter.Themes,
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
    fTheme: TSyntaxHiliteTheme;
    fBrush: TSyntaxHiliterBrush;
      {Highlighter for which CSS is to be generated}
    // TODO: rename BuildElemCSS as BuildAttrCSS
    procedure BuildElemCSS(const AttrID: string; const CSSBuilder: TCSSBuilder);
      {Builds CSS class for a highlighter element.
        @param Elem [in] Highlighter element for which CSS is required.
        @param CSSBuilder [in] Object used to build and store the CSS.
      }
  public
    constructor Create(const Brush: TSyntaxHiliterBrush;
      const Theme: TSyntaxHiliteTheme);
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
    procedure BuildCSS(const CSSBuilder: TCSSBuilder);
      {Builds all CSS classes for a highlighter.
        @param CSSBuilder [in] Object used to build and store required CSS.
      }
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

procedure THiliterCSS.BuildCSS(const CSSBuilder: TCSSBuilder);
  {Builds all CSS classes for a highlighter.
    @param CSSBuilder [in] Object used to build and store required CSS.
  }
var
  Attrs: TArray<TSyntaxHiliterAttr>;
  Attr: TSyntaxHiliterAttr;
begin
  // Add font definition in main class
  with CSSBuilder.AddSelector('.' + GetMainCSSClassName) do
  begin
    AddProperty(TCSS.FontFamilyProp(fTheme.FontName, cfgMonoSpace));
    AddProperty(TCSS.FontSizeProp(fTheme.FontSize));
    if fTheme.DefaultBackground <> clNone then
      AddProperty(TCSS.BackgroundColorProp(fTheme.DefaultBackground));
    if fTheme.DefaultForeground <> clNone then
      AddProperty(TCSS.BackgroundColorProp(fTheme.DefaultForeground));
  end;
  Attrs := fBrush.SupportedAttrs;
  for Attr in Attrs do
    BuildElemCSS(Attr.ID, CSSBuilder);
end;

procedure THiliterCSS.BuildElemCSS(const AttrID: string;
  const CSSBuilder: TCSSBuilder);
  {Builds CSS class for a highlighter element.
    @param Elem [in] Highlighter element for which CSS is required.
    @param CSSBuilder [in] Object used to build and output the CSS.
  }
var
  AttrStyle: TSyntaxHiliteAttrStyle;
begin
  AttrStyle := fTheme.GetStyle(fBrush.ID, AttrID);
  // We only create CSS class if element attribute's style is non-null
  if fTheme.IsBaseStyle(AttrStyle) then
    Exit;
  with CSSBuilder.AddSelector('.' + GetElemCSSClassName(fBrush.ID, AttrID)) do
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

constructor THiliterCSS.Create(const Brush: TSyntaxHiliterBrush;
  const Theme: TSyntaxHiliteTheme);
  {Class constructor. Sets up object ready to generate code for a syntax
  highlighter.
    @param HiliterAttrs [in] Attributes to be used in highlighter.
  }
begin
  inherited Create;
  Assert(Assigned(Brush), ClassName + '.Create: Brush is nil');
  Assert(Assigned(Theme), ClassName + '.Create: Theme is nil');
  fBrush := Brush;
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


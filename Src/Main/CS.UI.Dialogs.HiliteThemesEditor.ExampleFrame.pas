{
 * This Source Code Form is subject to the terms of the Mozilla Public License,
 * v. 2.0. If a copy of the MPL was not distributed with this file, You can
 * obtain one at http://mozilla.org/MPL/2.0/
 *
 * Copyright (C) 2016, Peter Johnson (www.delphidabbler.com).
 *
 * Implements a frame used to display highlighter examples in Hilite Themes
 * Editor dialogue box.
}


unit CS.UI.Dialogs.HiliteThemesEditor.ExampleFrame;

interface

uses
  // Delphi
  OleCtrls, SHDocVw, Classes, Controls, ExtCtrls,

  // Project
  CS.SourceCode.Hiliter.Themes,
  FrBrowserBase,
  UEncodings;

type
  THiliteThemesExampleFrame = class(TBrowserBaseFrame)
  strict private
    const
      CSSClassName = 'example';
      ExampleText = 'Quick brown fox <=>+-.!@$#';
    function GenerateHTML(const Style: TSyntaxHiliteAttrStyle;
      const FontName: string; const FontSize: Integer): string;
    function GenerateCSS(const Style: TSyntaxHiliteAttrStyle;
      const FontName: string; const FontSize: Integer): string;
  public
    constructor Create(AOwner: TComponent); override;
    procedure Display(Theme: TSyntaxHiliteTheme; const BrushID, AttrID: string);
  end;

implementation

uses
  SysUtils,
  Graphics,

  Browser.UUIMGr,
  UCSSBuilder,
  UCSSUtils,
  UHTMLBuilder,
  UNulDropTarget;

{$R *.dfm}

{ THiliteThemesExampleFrame }

constructor THiliteThemesExampleFrame.Create(AOwner: TComponent);
begin
  inherited;
  // Set up browser control to be "read-only"
//  WBController.UIMgr.ScrollbarStyle := sbsHide;   // no scroll bars
  WBController.UIMgr.AllowTextSelection := False; // can't select text
  WBController.UIMgr.DropTarget :=                // inhibit drag drop
    TNulDropTarget.Create;
  WBController.UIMgr.OnUpdateCSS := nil;          // don't handle this event
end;

procedure THiliteThemesExampleFrame.Display(Theme: TSyntaxHiliteTheme;
  const BrushID, AttrID: string);
var
  Style: TSyntaxHiliteAttrStyle;
  HTML: string;
begin
  if BrushID = TSyntaxHiliteTheme.DefaultBrushID then
    Style := Theme.GetDefaultStyle(AttrID)
  else
    Style := Theme.GetStyle(BrushID, AttrID);
  HTML := GenerateHTML(Style, Theme.FontName, Theme.FontSize);
  WBController.IOMgr.LoadFromString(HTML);
end;

function THiliteThemesExampleFrame.GenerateCSS(
  const Style: TSyntaxHiliteAttrStyle;
  const FontName: string; const FontSize: Integer): string;
var
  Builder: TCSSBuilder;
begin
  Builder := TCSSBuilder.Create;
  try
    // CSS applied to body tag
    with Builder.AddSelector('body') do
    begin
      AddProperty(TCSS.MarginProp(2));
    end;
    // CSS for div that wraps example text
    with Builder.AddSelector('.' + CSSClassName) do
    begin
      AddProperty(TCSS.FontFamilyProp(FontName, cfgMonoSpace));
      AddProperty(TCSS.FontSizeProp(FontSize));
      if Style.Background <> clNone then
        AddProperty(TCSS.BackgroundColorProp(Style.Background))
      else
        AddProperty(TCSS.BackgroundColorProp(clWindow));
      if Style.Foreground <> clNone then
        AddProperty(TCSS.ColorProp(Style.Foreground))
      else
        AddProperty(TCSS.ColorProp(clWindowText));
      AddProperty(TCSS.FontWeightProp(Style.FontStyles));
      AddProperty(TCSS.FontStyleProp(Style.FontStyles));
      AddProperty(TCSS.TextDecorationProp(Style.FontStyles));
    end;
    Result := Builder.AsString;
  finally
    Builder.Free;
  end;
end;

function THiliteThemesExampleFrame.GenerateHTML(
  const Style: TSyntaxHiliteAttrStyle; const FontName: string;
  const FontSize: Integer): string;
var
  Builder: THTMLBuilder;
begin
  Builder := THTMLBuilder.Create;
  try
    Builder.CSS := GenerateCSS(Style, FontName, FontSize);
    Builder.OpenDiv(CSSClassName);
    Builder.AddText(ExampleText);
    Builder.CloseDiv;
    Result := Builder.HTMLDocument;
  finally
    Builder.Free;
  end;
end;

end.

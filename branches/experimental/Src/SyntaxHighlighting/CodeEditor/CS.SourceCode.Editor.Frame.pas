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
 * Implements a frame that contains and controls access to a TSynEdit control.
}


unit CS.SourceCode.Editor.Frame;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs,

  SynEdit,
  SynEditHighlighter,

  CS.SourceCode.Languages,
  CS.SourceCode.Hiliter.Brushes,
  CS.SourceCode.Hiliter.Themes;

type
  TTCodeEditorFrame = class(TFrame)
  strict private
    var
      fSynEditCmp: TSynEdit;
      fTheme: TSyntaxHiliteTheme;
      fBrush: TSyntaxHiliterBrush;
    function GetSourceCode: string;
    procedure SetSourceCode(const Code: string);
    procedure SetTheme(const ATheme: TSyntaxHiliteTheme);
    procedure SetBrush(const ABrush: TSyntaxHiliterBrush);
    function GetTabSize: Integer;
    procedure SetTabSize(const ATabSize: Integer);
    procedure ApplyTheme;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    procedure ApplyLanguage(const Language: TSourceCodeLanguage);
    property Theme: TSyntaxHiliteTheme read fTheme write SetTheme;
    property Brush: TSyntaxHiliterBrush read fBrush write SetBrush;
    property TabSize: Integer read GetTabSize write SetTabSize;
    property SourceCode: string read GetSourceCode write SetSourceCode;
  end;

implementation

{$R *.dfm}

{ TTCodeEditorFrame }

procedure TTCodeEditorFrame.ApplyLanguage(const Language: TSourceCodeLanguage);
var
  Brush: TSyntaxHiliterBrush;
begin
  Brush := TSyntaxHiliterBrushes.CreateBrush(Language.HiliterBrushID);
  try
    SetBrush(Brush);
  finally
    Brush.Free;
  end;
  SetTabSize(Language.EditorTabSize);
end;

procedure TTCodeEditorFrame.ApplyTheme;
var
  Highlighter: TSynCustomHighlighter;
  I: Integer;
  BrushID: string;
  AttrID: string;
  AttrStyle: TSyntaxHiliteAttrStyle;
begin
  if not Assigned(fTheme) then
    Exit;
  fSynEditCmp.Font.Name := fTheme.FontName;
  fSynEditCmp.Font.Size := fTheme.FontSize;
  if fTheme.DefaultForeground = clNone then
    fSynEditCmp.Font.Color := clWindowText
  else
    fSynEditCmp.Font.Color := fTheme.DefaultForeground;
  if fTheme.DefaultBackground = clNone then
    fSynEditCmp.Color := clWindow
  else
    fSynEditCmp.Color := fTheme.DefaultBackground;
  Highlighter := fSynEditCmp.Highlighter;
  if not Assigned(Highlighter) then
    Exit;
  BrushID := fBrush.ID;
  Assert(Highlighter.GetLanguageName = fBrush.ID,
    ClassName + '.ApplyThemes: Highlighter language name <> brush ID');
  for I := 0 to Pred(Highlighter.AttrCount) do
  begin
    AttrID := Highlighter.Attribute[I].Name;
    AttrStyle := fTheme.GetStyle(fBrush.ID, AttrID);
    Highlighter.Attribute[I].Background := AttrStyle.Background;
    Highlighter.Attribute[I].Foreground := AttrStyle.Foreground;
    Highlighter.Attribute[I].Style := AttrStyle.FontStyles;
  end;
end;

constructor TTCodeEditorFrame.Create(AOwner: TComponent);
begin
  inherited;
  fBrush := TSyntaxHiliterBrushes.CreateNullBrush;
  fSynEditCmp := TSynEdit.Create(Self);
  fSynEditCmp.Parent := Self;
  fSynEditCmp.Align := alClient;
  fSynEditCmp.WantReturns := True;
  fSynEditCmp.WantTabs := True;
  fSynEditCmp.Gutter.LeftOffset := 0; // change this if use glyphs in gutter
  fSynEditCmp.Gutter.ShowLineNumbers := True; // TODO: make option
  fSynEditCmp.WordWrap := False;
  fSynEditCmp.TabWidth := 2;
  fSynEditCmp.ActiveLineColor := clNone; {default} // TODO: make option
  fSynEditCmp.Options := [
    eoAutoIndent, eoDragDropEditing, eoEnhanceEndKey, eoGroupUndo,
    eoScrollPastEol, eoShowScrollHint, eoSmartTabDelete, eoTabsToSpaces
  ];  // = default - [eoSmartTab]
  // No highligher (default): setting .Highlighter = nil turns off highlighting
  fSynEditCmp.Highlighter := nil;
  // Turn off book mark keys & glyphs
  { TODO: could make option, in which case need to restore default gutter's
          right offset. If option included glyphs we'll need some nicer ones
          to give to BookmMarkOptions.BookmarkImages }
  fSynEditCmp.BookMarkOptions.EnableKeys := False;
  fSynEditCmp.BookMarkOptions.GlyphsVisible := False;
  fSynEditCmp.Gutter.Font.Color := clGray;
  ApplyTheme;
end;

destructor TTCodeEditorFrame.Destroy;
var
  OldHighlighter: TSynCustomHighlighter;
begin
  OldHighlighter := fSynEditCmp.Highlighter;
  if Assigned(OldHighlighter) then
  begin
    fSynEditCmp.Highlighter := nil;
    OldHighlighter.Free;
  end;
  fBrush.Free;
  inherited;
end;

function TTCodeEditorFrame.GetSourceCode: string;
begin
  Result := fSynEditCmp.Text;
end;

function TTCodeEditorFrame.GetTabSize: Integer;
begin
  Result := fSynEditCmp.TabWidth;
end;

procedure TTCodeEditorFrame.SetBrush(const ABrush: TSyntaxHiliterBrush);
var
  OldBrush: TSyntaxHiliterBrush;
  OldHighlighter: TSynCustomHighlighter;
begin
  Assert(Assigned(ABrush), ClassName + '.SetBrush: ABrush not assigned');
  OldBrush := fBrush;
  fBrush := ABrush.Clone;
  OldBrush.Free;
  OldHighlighter := fSynEditCmp.Highlighter;
  fSynEditCmp.Highlighter := ABrush.CreateHighlighter;
  OldHighlighter.Free;  // may be nil, but that's OK with Free
  ApplyTheme;
end;

procedure TTCodeEditorFrame.SetSourceCode(const Code: string);
begin
  fSynEditCmp.Text := Code;
end;

procedure TTCodeEditorFrame.SetTabSize(const ATabSize: Integer);
begin
  fSynEditCmp.TabWidth := ATabSize;
end;

procedure TTCodeEditorFrame.SetTheme(const ATheme: TSyntaxHiliteTheme);
begin
  Assert(Assigned(ATheme), ClassName + '.ATheme not assigned');
  fTheme := ATheme;
  ApplyTheme;
end;

end.


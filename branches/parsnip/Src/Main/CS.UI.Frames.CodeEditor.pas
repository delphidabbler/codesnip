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


unit CS.UI.Frames.CodeEditor;


interface


uses
  // Delphi
  Classes,
  Forms,
  // 3rd party
  SynEdit,
  // Project
  CS.Components.EditCtrls,
  CS.SourceCode.Languages,
  CS.SourceCode.Hiliter.Brushes,
  CS.SourceCode.Hiliter.Themes;


type
  TCodeEditorFrame = class(TFrame)
  strict private
    var
      fSynEditCmp: TSynEditEx;
      fTheme: TSyntaxHiliteTheme;
      fBrush: TSyntaxHiliterBrush;
      fFontSize: Integer;
      fUseThemeFontSize: Boolean;
    function GetSourceCode: string;
    procedure SetSourceCode(const Code: string);
    procedure SetTheme(const ATheme: TSyntaxHiliteTheme);
    procedure SetBrush(const ABrush: TSyntaxHiliterBrush);
    function GetTabSize: Integer;
    procedure SetTabSize(const ATabSize: Integer);
    procedure SetUseThemeFontSize(const AFlag: Boolean);
    function GetFontSize: Integer;
    procedure SetFontSize(const ASize: Integer);
    function GetTextHint: string;
    procedure SetTextHint(const ATextHint: string);
    procedure ApplyTheme;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    procedure SetFocus; override;
    procedure Clear;
    procedure ApplyLanguage(const Language: TSourceCodeLanguage);
    property Theme: TSyntaxHiliteTheme read fTheme write SetTheme;
    property Brush: TSyntaxHiliterBrush read fBrush write SetBrush;
    property TabSize: Integer read GetTabSize write SetTabSize;
    property SourceCode: string read GetSourceCode write SetSourceCode;
    property UseThemeFontSize: Boolean read fUseThemeFontSize write
      SetUseThemeFontSize default True;
    property FontSize: Integer read GetFontSize write SetFontSize;
    property TextHint: string read GetTextHint write SetTextHint;
  end;


implementation


uses
  // Delphi
  SysUtils,
  Graphics,
  Controls,
  // 3rd party
  SynEditHighlighter,
  // Project
  UStrUtils;

{$R *.dfm}

{ TTCodeEditorFrame }

procedure TCodeEditorFrame.ApplyLanguage(const Language: TSourceCodeLanguage);
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

procedure TCodeEditorFrame.ApplyTheme;
var
  Highlighter: TSynCustomHighlighter;
  I: Integer;
  BrushID: string;
  AttrID: string;
  AttrStyle: TSyntaxHiliteAttrStyle;
begin
  fSynEditCmp.Font.Name := fTheme.FontName;
  if fUseThemeFontSize then
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

procedure TCodeEditorFrame.Clear;
begin
  fSynEditCmp.Clear;
end;

constructor TCodeEditorFrame.Create(AOwner: TComponent);
begin
  inherited;
  fTheme := TSyntaxHiliteThemes.NullTheme;
  fBrush := TSyntaxHiliterBrushes.CreateNullBrush;
  fSynEditCmp := TSynEditEx.Create(Self);
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
          to give to BookMarkOptions.BookmarkImages }
  fSynEditCmp.BookMarkOptions.EnableKeys := False;
  fSynEditCmp.BookMarkOptions.GlyphsVisible := False;
  fSynEditCmp.Gutter.Font.Color := clGray;
  fFontSize := fTheme.FontSize; // default FontSize = default theme font size
  fUseThemeFontSize := True;
  ApplyTheme;
end;

destructor TCodeEditorFrame.Destroy;
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

function TCodeEditorFrame.GetFontSize: Integer;
begin
  Result := fSynEditCmp.Font.Size;
end;

function TCodeEditorFrame.GetSourceCode: string;
var
  Line: string;
  SB: TStringBuilder;
begin
  // Work around bug in either TSynEdit or TStrings which means that accessing
  // fSynEditCmp.Text can cause a buffer overrun error that can include spurious
  // characters from raw memory to be included at end of source code string.
  // We also strip all trailing space from code lines
  SB := TStringBuilder.Create;
  try
    for Line in fSynEditCmp.Lines do
      SB.AppendLine(StrTrimRight(Line));  // trim trailing spaces from line
    Result := StrTrimRight(SB.ToString);  // remove trailing EOL character
  finally
    SB.Free;
  end;
end;

function TCodeEditorFrame.GetTabSize: Integer;
begin
  Result := fSynEditCmp.TabWidth;
end;

function TCodeEditorFrame.GetTextHint: string;
begin
  Result := fSynEditCmp.TextHint;
end;

procedure TCodeEditorFrame.SetBrush(const ABrush: TSyntaxHiliterBrush);
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

procedure TCodeEditorFrame.SetFocus;
begin
  inherited;
  fSynEditCmp.SetFocus;
end;

procedure TCodeEditorFrame.SetFontSize(const ASize: Integer);
begin
  fFontSize := ASize;
  SetUseThemeFontSize(False);
end;

procedure TCodeEditorFrame.SetSourceCode(const Code: string);
begin
  fSynEditCmp.Text := Code;
end;

procedure TCodeEditorFrame.SetTabSize(const ATabSize: Integer);
begin
  fSynEditCmp.TabWidth := ATabSize;
end;

procedure TCodeEditorFrame.SetTextHint(const ATextHint: string);
begin
  fSynEditCmp.TextHint := ATextHint;
end;

procedure TCodeEditorFrame.SetTheme(const ATheme: TSyntaxHiliteTheme);
begin
  Assert(Assigned(ATheme), ClassName + '.ATheme not assigned');
  fTheme := ATheme;
  ApplyTheme;
end;

procedure TCodeEditorFrame.SetUseThemeFontSize(const AFlag: Boolean);
begin
  if AFlag = fUseThemeFontSize then
    Exit;
  fUseThemeFontSize := AFlag;
  if AFlag then
    fSynEditCmp.Font.Size := fTheme.FontSize
  else
    fSynEditCmp.Font.Size := fFontSize;
end;

end.


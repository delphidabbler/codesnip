unit CS.CodeEditor.Frame;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs,

  SynEdit, SynEditHighlighter,

  CS.Hiliter.Brushes,
  CS.Hiliter.Themes;

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
    procedure ApplyTheme;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    property Theme: TSyntaxHiliteTheme read fTheme write SetTheme;
    property Brush: TSyntaxHiliterBrush read fBrush write SetBrush;
    property SourceCode: string read GetSourceCode write SetSourceCode;
  end;

implementation

{$R *.dfm}

{ TTCodeEditorFrame }

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
    Highlighter.Attribute[I].Style := AttrStyle.ConvertFontStyles;
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
  fSynEditCmp.WordWrap := False;              // TODO: make option
  // TODO: set tab stop according to highlighter - needs separate option
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

procedure TTCodeEditorFrame.SetTheme(const ATheme: TSyntaxHiliteTheme);
begin
  Assert(Assigned(ATheme), ClassName + '.ATheme not assigned');
  fTheme := ATheme;
  ApplyTheme;
end;

end.


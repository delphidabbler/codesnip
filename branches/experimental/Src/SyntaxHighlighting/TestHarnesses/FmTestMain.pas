unit FmTestMain;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, StdCtrls, ComCtrls,

  Generics.Collections,

  CS.Hiliter.Themes;

type
  TMainTestForm = class(TForm)
    pcMain: TPageControl;
    tsThemesLoader: TTabSheet;
    btnLoadUserThemes: TButton;
    edLoadedThemes: TMemo;
    btnLoadDefaultThemes: TButton;
    btnDisplayThemes: TButton;
    btnSaveAllThemes: TButton;
    btnClearThemes: TButton;
    procedure btnLoadUserThemesClick(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure btnLoadDefaultThemesClick(Sender: TObject);
    procedure btnDisplayThemesClick(Sender: TObject);
    procedure btnSaveAllThemesClick(Sender: TObject);
    procedure btnClearThemesClick(Sender: TObject);
  private
    fThemes: TSyntaxHiliteThemes;
  end;

var
  MainTestForm: TMainTestForm;

implementation

uses
  CS.Hiliter.Themes.Persist;

{$R *.dfm}

procedure TMainTestForm.btnClearThemesClick(Sender: TObject);
begin
  if not Assigned(fThemes) then
    Exit;
  fThemes.Clear;
  btnLoadDefaultThemes.Enabled := True;
  btnLoadUserThemes.Enabled := True;
end;

procedure TMainTestForm.btnDisplayThemesClick(Sender: TObject);
var
  T: TSyntaxHiliteTheme;
  BS: TPair<string,TSyntaxHiliteBrushStyle>;

  procedure AddLine(const S: string);
  begin
    edLoadedThemes.Lines.Add(S);
  end;

  procedure AddLineFmt(const Fmt: string; const Args: array of const);
  begin
    AddLine(Format(Fmt, Args));
  end;

  procedure DisplayBrushStyle(const BS: TSyntaxHiliteBrushStyle);
  var
    Attr: TPair<string,TSyntaxHiliteAttrStyle>;
    FS: TSyntaxHiliteFontStyle;
  const
    ///  <summary>Map of syntax highlighter font styles to identifiers used in a
    ///  themes file.</summary>
    FontStyleMap: array[TSyntaxHiliteFontStyle] of string = (
      '*', 'bold', 'italic', 'underline'
    );
  begin
    for Attr in BS do
    begin
      AddLineFmt('    ATTR: %s', [Attr.Key]);
      AddLineFmt('      BG Colour: %s', [ColorToString(Attr.Value.Background)]);
      AddLineFmt('      FG Colour: %s', [ColorToString(Attr.Value.Foreground)]);
      AddLine('      Font Styles');
      for FS in Attr.Value.FontStyles do
        AddLineFmt('        %s', [FontStyleMap[FS]]);
    end;
  end;

begin
  edLoadedThemes.Clear;
  for T in fThemes do
  begin
    AddLineFmt('THEME: %s (%s)', [T.ID, T.FriendlyName]);
    AddLine('  DEFAULT-BRUSH');
    DisplayBrushStyle(T.DefaultBrushStyle);
    for BS in T do
    begin
      AddLineFmt('  BRUSH: %s', [BS.Key]);
      DisplayBrushStyle(BS.Value);
    end;
  end;
end;

procedure TMainTestForm.btnLoadDefaultThemesClick(Sender: TObject);
begin
  btnLoadDefaultThemes.Enabled := False;
  if not Assigned(fThemes) then
    fThemes := TSyntaxHiliteThemes.Create;
  TSyntaxHiliteThemesIO.LoadThemesFromResources(
    fThemes, 'HILITETHEMES', RT_RCDATA
  );
end;

procedure TMainTestForm.btnLoadUserThemesClick(Sender: TObject);
begin
  btnLoadUserThemes.Enabled := False;
  if not Assigned(fThemes) then
    fThemes := TSyntaxHiliteThemes.Create;
  TSyntaxHiliteThemesIO.LoadThemes(
    fThemes,
    ExtractFilePath(ParamStr(0)) +
      '..\Src\SyntaxHighlighting\Highlighters\TestThemes.txt'
  );
end;

procedure TMainTestForm.btnSaveAllThemesClick(Sender: TObject);
begin
  TSyntaxHiliteThemesIO.SaveThemes(
    fThemes,
    ExtractFilePath(ParamStr(0)) +
      '..\Src\SyntaxHighlighting\Highlighters\SavedThemes.txt'
  );
end;

procedure TMainTestForm.FormCreate(Sender: TObject);
begin
  fThemes := TSyntaxHiliteThemes.Create;
end;

procedure TMainTestForm.FormDestroy(Sender: TObject);
begin
  fThemes.Free;
end;

end.

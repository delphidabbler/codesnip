unit FmTestMain;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, StdCtrls, ComCtrls,

  Generics.Collections,

  CS.Hiliter.Themes,
  CS.Hiliter.Brushes;

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
    tsBrushAttrs: TTabSheet;
    cbChooseTheme: TComboBox;
    lblChooseTheme: TLabel;
    lblChooseBrush: TLabel;
    cbChooseBrush: TComboBox;
    btnDisplayBrushAttrs: TButton;
    edBrushAttrs: TMemo;
    procedure btnLoadUserThemesClick(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure btnLoadDefaultThemesClick(Sender: TObject);
    procedure btnDisplayThemesClick(Sender: TObject);
    procedure btnSaveAllThemesClick(Sender: TObject);
    procedure btnClearThemesClick(Sender: TObject);
    procedure FormShow(Sender: TObject);
    procedure btnDisplayBrushAttrsClick(Sender: TObject);
  private
    fThemes: TSyntaxHiliteThemes;
    // Array of theme IDs with same index as theme's entry in cbChooseTheme
    fUIThemeIDs: TArray<string>;
    // Array of brush IDs with same index as brush's entry in cbChooseBrush
    fUIBrushIDs: TArray<string>;
    // Sets fUIThemeIDs and populates combo with theme friendly names
    procedure PopulateChooseThemeCombo;
    // Sets fUIBrushIDs and populates combo with brush friendly names
    procedure PopulateChooseBrushCombo;
    // Gets ID of theme corresponding to that selected in cbChooseTheme
    function GetSelectedThemeID: string;
    // Gets ID of brush corresponding to that selected in cbChooseBrush
    function GetSelectedBrushID: string;
  end;

var
  MainTestForm: TMainTestForm;

implementation

uses
  UIStringList,
  CS.Hiliter.Themes.Persist;

{$R *.dfm}

const
  ///  <summary>Map of syntax highlighter font styles to identifiers used in a
  ///  themes file.</summary>
  FontStyleMap: array[TSyntaxHiliteFontStyle] of string = (
    '*', 'bold', 'italic', 'underline'
  );

procedure TMainTestForm.btnClearThemesClick(Sender: TObject);
begin
  if not Assigned(fThemes) then
    Exit;
  fThemes.Clear;
  btnLoadDefaultThemes.Enabled := True;
  btnLoadUserThemes.Enabled := True;
  PopulateChooseThemeCombo;
end;

procedure TMainTestForm.btnDisplayBrushAttrsClick(Sender: TObject);
var
  Brush: TSyntaxHiliterBrush;
  Attr: TSyntaxHiliterAttr;
  Attrs: TArray<TSyntaxHiliterAttr>;
  Theme: TSyntaxHiliteTheme;
  AttrStyle: TSyntaxHiliteAttrStyle;
  FS: TSyntaxHiliteFontStyle;
  FSParams: IStringList;
begin
  edBrushAttrs.Clear;
  Theme := fThemes[GetSelectedThemeID];
  Brush := TSyntaxHiliterBrushes.CreateBrush(GetSelectedBrushID);
  try
    edBrushAttrs.Lines.Add(
      Format(
        'Theme: %1:s (Brush: %0:s)',
        [Brush.FriendlyName, Theme.FriendlyName]
      )
    );
    if Theme.IsBrushSupported(Brush.ID) then
      edBrushAttrs.Lines.Add('  (has some customisation)')
    else
      edBrushAttrs.Lines.Add('  (using default brush only)');
    Attrs := Brush.SupportedAttrs;
    for Attr in Attrs do
    begin
      edBrushAttrs.Lines.Add(Attr.FriendlyName + ':');
      AttrStyle := Theme.GetStyle(Brush.ID, Attr.ID);
      edBrushAttrs.Lines.Add(
        Format('      Background: %s', [ColorToString(AttrStyle.Background)])
      );
      edBrushAttrs.Lines.Add(
        Format('      Foreground: %s', [ColorToString(AttrStyle.Foreground)])
      );;
      FSParams := TIStringList.Create;
      for FS in AttrStyle.FontStyles do
        FSParams.Add(FontStyleMap[FS]);
      if FSParams.Count = 0 then
        edBrushAttrs.Lines.Add('      Font Styles: {}')
      else if AttrStyle.FontStyles = [hfsDefault] then
        edBrushAttrs.Lines.Add(
          '      Font Styles: *'
        )
      else
        edBrushAttrs.Lines.Add(
          '      Font Styles: {' + FSParams.GetText(',', False) + '}'
        );
      end;
  finally
    Brush.Free;
  end;
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
  PopulateChooseThemeCombo;
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
  PopulateChooseThemeCombo;
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
  fUIBrushIDs := TSyntaxHiliterBrushes.SupportedBrushIDs;
end;

procedure TMainTestForm.FormDestroy(Sender: TObject);
begin
  fThemes.Free;
end;

procedure TMainTestForm.FormShow(Sender: TObject);
begin
  PopulateChooseBrushCombo;
end;

function TMainTestForm.GetSelectedBrushID: string;
var
  SelIdx: Integer;
begin
  SelIdx := cbChooseBrush.ItemIndex;
  if SelIdx = -1 then
    raise Exception.Create('No brush selected');
  Result := fUIBrushIDs[SelIdx];
end;

function TMainTestForm.GetSelectedThemeID: string;
var
  SelIdx: Integer;
begin
  SelIdx := cbChooseTheme.ItemIndex;
  if SelIdx = -1 then
    raise Exception.Create('No theme selected');
  Result := fUIThemeIDs[SelIdx];
end;

procedure TMainTestForm.PopulateChooseBrushCombo;
var
  I: Integer;
  Brush: TSyntaxHiliterBrush;
begin
  fUIBrushIDs := TSyntaxHiliterBrushes.SupportedBrushIDs;
  cbChooseBrush.Clear;
  for I := 0 to Pred(Length(fUIBrushIDs)) do
  begin
    Brush := TSyntaxHiliterBrushes.CreateBrush(fUIBrushIDs[I]);
    try
      cbChooseBrush.Items.Add(Brush.FriendlyName);
    finally
      Brush.Free;
    end;
  end;
  if cbChooseBrush.Items.Count > 0 then
    cbChooseBrush.ItemIndex := 0;
end;

procedure TMainTestForm.PopulateChooseThemeCombo;
var
  ThemeID: string;
begin
  cbChooseTheme.Clear;
  if not Assigned(fThemes) then
    Exit;
  fUIThemeIDs := fThemes.SupportedThemes;
  for ThemeID in fUIThemeIDs do
    cbChooseTheme.Items.Add(fThemes[ThemeID].FriendlyName);
  if cbChooseTheme.Items.Count > 0 then
    cbChooseTheme.ItemIndex := 0;
end;

end.

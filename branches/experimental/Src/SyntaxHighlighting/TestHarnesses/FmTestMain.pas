unit FmTestMain;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, StdCtrls, ComCtrls,

  Generics.Collections,

  CS.SourceCode.Languages,
  CS.SourceCode.Hiliter.Themes,
  CS.SourceCode.Hiliter.Brushes,
  CS.SourceCode.Editor.Frame;

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
    tsHighlighting: TTabSheet;
    cbChooseTheme: TComboBox;
    lblChooseTheme: TLabel;
    lblChooseBrush: TLabel;
    cbChooseBrush: TComboBox;
    pcHighlighting: TPageControl;
    tsBrushAttrs: TTabSheet;
    tsCodeEditor: TTabSheet;
    btnDisplayBrushAttrs: TButton;
    edBrushAttrs: TMemo;
    btnDisplaySource: TButton;
    btnChangeTheme: TButton;
    tsRendering: TTabSheet;
    pcRendering: TPageControl;
    tsMockRendering: TTabSheet;
    btnRenderSyntaxHilite: TButton;
    btnRenderNull: TButton;
    edMockRender: TMemo;
    tsXHTMLFragSource: TTabSheet;
    edXTHMLFragSource: TMemo;
    tsXHTMLDocSource: TTabSheet;
    edXHTMLDocSource: TMemo;
    tsRTFDocSource: TTabSheet;
    edRTFDocSource: TMemo;
    TabSheet1: TTabSheet;
    btnLoadUserLangs: TButton;
    edLoadedLangs: TMemo;
    btnLoadDefaultLangs: TButton;
    btnDisplayLangs: TButton;
    btnSaveAllLangs: TButton;
    btnClearLangs: TButton;
    lblChooseLang: TLabel;
    cbChooseLang: TComboBox;
    btnDisplaySourceForLang: TButton;
    lblLangBrush: TLabel;
    frmCodeEditor: TCodeEditorFrame;
    procedure btnLoadUserThemesClick(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure btnLoadDefaultThemesClick(Sender: TObject);
    procedure btnDisplayThemesClick(Sender: TObject);
    procedure btnSaveAllThemesClick(Sender: TObject);
    procedure btnClearThemesClick(Sender: TObject);
    procedure FormShow(Sender: TObject);
    procedure btnDisplayBrushAttrsClick(Sender: TObject);
    procedure btnDisplaySourceClick(Sender: TObject);
    procedure btnChangeThemeClick(Sender: TObject);
    procedure btnRenderSyntaxHiliteClick(Sender: TObject);
    procedure btnRenderNullClick(Sender: TObject);
    procedure btnLoadUserLangsClick(Sender: TObject);
    procedure btnClearLangsClick(Sender: TObject);
    procedure btnDisplayLangsClick(Sender: TObject);
    procedure btnSaveAllLangsClick(Sender: TObject);
    procedure btnLoadDefaultLangsClick(Sender: TObject);
    procedure btnDisplaySourceForLangClick(Sender: TObject);
    procedure cbChooseLangChange(Sender: TObject);
  private
    fLanguages: TSourceCodeLanguages;
    fThemes: TSyntaxHiliteThemes;
    // Array of theme IDs with same index as theme's entry in cbChooseTheme
    fUIThemeIDs: TArray<string>;
    // Array of brush IDs with same index as brush's entry in cbChooseBrush
    fUIBrushIDs: TArray<string>;
    // Array of language IDs with same index as language's entry in cbChooseLang
    fUILangIDs: TArray<TSourceCodeLanguageID>;
    // Sets fUIThemeIDs and populates combo with theme friendly names
    procedure PopulateChooseThemeCombo;
    // Sets fUIBrushIDs and populates combo with brush friendly names
    procedure PopulateChooseBrushCombo;
    // Sets fUILangIDs and populates combo with brush friendly names
    procedure PopulateChooseLangCombo;
    // Gets ID of theme corresponding to that selected in cbChooseTheme
    function GetSelectedThemeID: string;
    // Gets ID of brush corresponding to that selected in cbChooseBrush
    function GetSelectedBrushID: string;
    // Gets ID of language corresponding to that selected in cbChooseLang
    function GetSelectedLangID: TSourceCodeLanguageID;

    procedure DoRendering(Brush: TSyntaxHiliterBrush);
    // Performs highlight rendering using a mock highlighter that shows each
    // element
    procedure DoMockRendering(Brush: TSyntaxHiliterBrush);
    procedure DoXHTMLFragSourceRendering(Brush: TSyntaxHiliterBrush);
    procedure DoXHTMLDocSourceRendering(Brush: TSyntaxHiliterBrush);
    procedure DoRTFDocSourceRendering(Brush: TSyntaxHiliterBrush);
  end;

var
  MainTestForm: TMainTestForm;

implementation

uses
  UStrUtils,
  UIStringList,
  UHTMLBuilder,
  CS.SourceCode.Languages.Persist,
  CS.SourceCode.Hiliter.Themes.Persist,
  CS.SourceCode.Hiliter.Parser,
  CS.SourceCode.Hiliter.Renderers;

{$R *.dfm}

const
  ///  <summary>Map of syntax highlighter font styles to identifiers used in a
  ///  themes file.</summary>
  ///  <remarks>There is an empty entry for fsStrikeout since that style is not
  ///  permitted in theme files.</remarks>
  FontStyleMap: array[TFontStyle] of string = (
    'bold', 'italic', 'underline', ''
  );

  ///  <summary>Map of font styles to symbolic names.</summary>
  UIFontStyleMap: array[TFontStyle] of string = (
    'fsBold', 'fsItalic', 'fsUnderline', 'fsStrikeOut'
  );

type
  TMockRenderer = class(TInterfacedObject, IHiliteRenderer2)
  strict private
    var
      fMemo: TMemo;
    procedure AddLine(const S: string); overload;
    procedure AddLine(const Fmt: string; const Args: array of const); overload;
  public
    constructor Create(Memo: TMemo);
    procedure Initialise;
    procedure Finalise;
    procedure BeginLine;
    procedure EndLine;
    procedure BeforeElem(const ElemInfo: TSyntaxHiliteElemInfo);
    procedure WriteElemText(const Text: string);
    procedure AfterElem(const ElemInfo: TSyntaxHiliteElemInfo);
  end;


{ TMainTestForm }

procedure TMainTestForm.btnClearLangsClick(Sender: TObject);
begin
  if not Assigned(fLanguages) then
    Exit;
  fLanguages.Clear;
  btnLoadDefaultLangs.Enabled := True;
  btnLoadUserLangs.Enabled := True;
  PopulateChooseLangCombo;
end;

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
  FS: TFontStyle;
  FSParams: IStringList;
  FontStyles: TFontStyles;
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
      FontStyles := AttrStyle.FontStyles;
      FSParams := TIStringList.Create;
      for FS in FontStyles do
        FSParams.Add(UIFontStyleMap[FS]);
      if AttrStyle.FontStyles.IsDefault then
        edBrushAttrs.Lines.Add(
          '      Font Styles: ERROR - Shouldn''t have hfsDefault here'
        )
      else
        edBrushAttrs.Lines.Add(
          '      Font Styles: [' + FSParams.GetText(',', False) + ']'
        );
      end;
  finally
    Brush.Free;
  end;
end;

procedure TMainTestForm.btnDisplayLangsClick(Sender: TObject);

  procedure AddLine(const S: string);
  begin
    edLoadedLangs.Lines.Add(S);
  end;

  procedure AddLineFmt(const Fmt: string; const Args: array of const);
  begin
    AddLine(Format(Fmt, Args));
  end;

var
  Lang: TSourceCodeLanguage;
begin
  edLoadedLangs.Clear;
  if not Assigned(fLanguages) then
    Exit;
  for Lang in fLanguages do
  begin
    AddLineFmt('LANGUAGE: %s - "%s"', [Lang.ID.ToString, Lang.FriendlyName]);
    AddLineFmt('  TAB-SIZE: %d', [Lang.EditorTabSize]);
    AddLineFmt('  BRUSH-ID: %s', [Lang.HiliterBrushID]);
    AddLineFmt('  BUILT-IN: %s', [BoolToStr(Lang.BuiltIn, True)]);
  end;
end;

procedure TMainTestForm.btnDisplaySourceClick(Sender: TObject);
var
  Theme: TSyntaxHiliteTheme;
  Brush: TSyntaxHiliterBrush;
begin
  Theme := fThemes.Themes[GetSelectedThemeID];
  Brush := TSyntaxHiliterBrushes.CreateBrush(GetSelectedBrushID);
  try
    frmCodeEditor.Theme := Theme;
    frmCodeEditor.Brush := Brush;
    frmCodeEditor.SourceCode := Brush.SampleSourceCode;
  finally
    Brush.Free;
  end;
end;

procedure TMainTestForm.btnDisplaySourceForLangClick(Sender: TObject);
var
  Brush: TSyntaxHiliterBrush;
begin
  frmCodeEditor.Theme := fThemes.Themes[GetSelectedThemeID];
  frmCodeEditor.ApplyLanguage(
    fLanguages.Languages[GetSelectedLangID]
  );
  Brush := TSyntaxHiliterBrushes.CreateBrush(
    fLanguages.Languages[GetSelectedLangID].HiliterBrushID
  );
  try
    frmCodeEditor.SourceCode := Brush.SampleSourceCode;
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
    FS: TFontStyle;
  begin
    for Attr in BS do
    begin
      AddLineFmt('    ATTR: %s', [Attr.Key]);
      AddLineFmt('      BG Colour: %s', [ColorToString(Attr.Value.Background)]);
      AddLineFmt('      FG Colour: %s', [ColorToString(Attr.Value.Foreground)]);
      AddLine('      Font Styles');
      if Attr.Value.FontStyles.IsDefault then
        AddLine('        *')
      else
        for FS in Attr.Value.FontStyles.Styles do
          AddLineFmt('        %s', [FontStyleMap[FS]]);
    end;
  end;

begin
  edLoadedThemes.Clear;
  for T in fThemes do
  begin
    AddLineFmt('THEME: %s (%s)', [T.ID, T.FriendlyName]);
    AddLineFmt('  FONT-NAME: %s', [T.FontName]);
    AddLineFmt('  FONT-SIZE: %d', [T.FontSize]);
    AddLineFmt(
      '  DEFAULT-BACKGROUND: %s', [ColorToString(T.DefaultBackground)]
    );
    AddLineFmt(
      '  DEFAULT-FOREGROUND: %s', [ColorToString(T.DefaultForeground)]
    );
    AddLine('  DEFAULT-BRUSH');
    DisplayBrushStyle(T.DefaultBrushStyle);
    for BS in T do
    begin
      AddLineFmt('  BRUSH: %s', [BS.Key]);
      DisplayBrushStyle(BS.Value);
    end;
  end;
end;

procedure TMainTestForm.btnLoadDefaultLangsClick(Sender: TObject);
begin
  btnLoadDefaultLangs.Enabled := False;
  if not Assigned(fLanguages) then
    fLanguages := TSourceCodeLanguages.Create;
  TSourceCodeLanguagesIO.LoadFromResources(
    fLanguages, 'SOURCECODELANGUAGES', RT_RCDATA
  );
  PopulateChooseLangCombo;
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

procedure TMainTestForm.btnLoadUserLangsClick(Sender: TObject);
begin
  btnLoadUserLangs.Enabled := False;
  if not Assigned(fLanguages) then
    fLanguages := TSourceCodeLanguages.Create;
  TSourceCodeLanguagesIO.Load(
    fLanguages,
    ExtractFilePath(ParamStr(0)) +
      '..\Src\SyntaxHighlighting\Languages\TestSourceCodeLanguages.txt'
  );
  PopulateChooseLangCombo;
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

procedure TMainTestForm.btnRenderNullClick(Sender: TObject);
var
  Brush: TSyntaxHiliterBrush;
begin
  Brush := TSyntaxHiliterBrushes.CreateNullBrush;
  try
    DoRendering(Brush);
  finally
    Brush.Free;
  end;
end;

procedure TMainTestForm.btnRenderSyntaxHiliteClick(Sender: TObject);
var
  Brush: TSyntaxHiliterBrush;
begin
  Brush := TSyntaxHiliterBrushes.CreateBrush(GetSelectedBrushID);
  try
    DoRendering(Brush);
  finally
    Brush.Free;
  end;
end;

procedure TMainTestForm.btnSaveAllLangsClick(Sender: TObject);
begin
  if not Assigned(fLanguages) then
    Exit;
  TSourceCodeLanguagesIO.Save(
    fLanguages,
    ExtractFilePath(ParamStr(0)) +
      '..\Src\SyntaxHighlighting\Languages\SavedSourceCodeLanguages.txt'
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

procedure TMainTestForm.cbChooseLangChange(Sender: TObject);
begin
  lblLangBrush.Caption := 'Selected brush ID = '
    + fLanguages[GetSelectedLangID].HiliterBrushID;
end;

procedure TMainTestForm.DoMockRendering(Brush: TSyntaxHiliterBrush);
var
  M: IHiliteRenderer2;
begin
  M := TMockRenderer.Create(edMockRender);
  TSyntaxHiliter.Hilite(Brush.SampleSourceCode, Brush, M);
end;

procedure TMainTestForm.DoRendering(Brush: TSyntaxHiliterBrush);
begin
  if pcRendering.ActivePage = tsMockRendering then
    DoMockRendering(Brush)
  else if pcRendering.ActivePage = tsXHTMLFragSource then
    DoXHTMLFragSourceRendering(Brush)
  else if pcRendering.ActivePage = tsXHTMLDocSource then
    DoXHTMLDocSourceRendering(Brush)
  else if pcRendering.ActivePage = tsRTFDocSource then
    DoRTFDocSourceRendering(Brush);
end;

procedure TMainTestForm.DoRTFDocSourceRendering(Brush: TSyntaxHiliterBrush);
begin
  edRTFDocSource.Text := TRTFDocumentHiliter.Hilite(
    Brush.SampleSourceCode, Brush, fThemes[GetSelectedThemeID], 'TEST TITLE'
  ).ToString;
end;

procedure TMainTestForm.DoXHTMLDocSourceRendering(Brush: TSyntaxHiliterBrush);
begin
  edXHTMLDocSource.Text := TXHTMLDocumentHiliter.Hilite(
    Brush.SampleSourceCode, Brush, fThemes[GetSelectedThemeID], 'TEST TITLE'
  ).ToString;
end;

procedure TMainTestForm.DoXHTMLFragSourceRendering(Brush: TSyntaxHiliterBrush);
var
  M: IHiliteRenderer2;
  B: THTMLBuilder;
begin
  B := THTMLBuilder.Create;
  try
    M := THTMLHiliteRenderer.Create(B, Brush, fThemes[GetSelectedThemeID]);
    TSyntaxHiliter.Hilite(Brush.SampleSourceCode, Brush, M);
    edXTHMLFragSource.Text := B.HTMLFragment;
  finally
    B.Free;
  end;
end;

procedure TMainTestForm.btnChangeThemeClick(Sender: TObject);
begin
  frmCodeEditor.Theme := fThemes.Themes[GetSelectedThemeID];
end;

procedure TMainTestForm.FormCreate(Sender: TObject);
begin
  fThemes := TSyntaxHiliteThemes.Create;
  fUIBrushIDs := TSyntaxHiliterBrushes.SupportedBrushIDs;
end;

procedure TMainTestForm.FormDestroy(Sender: TObject);
begin
  fThemes.Free;
  fLanguages.Free;
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

function TMainTestForm.GetSelectedLangID: TSourceCodeLanguageID;
var
  SelIdx: Integer;
begin
  SelIdx := cbChooseLang.ItemIndex;
  if SelIdx = -1 then
    raise Exception.Create('No language selected');
  Result := fUILangIDs[SelIdx];
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

procedure TMainTestForm.PopulateChooseLangCombo;
var
  LangID: TSourceCodeLanguageID;
begin
  cbChooseLang.Clear;
  if not Assigned(fLanguages) then
    Exit;
  fUILangIDs := fLanguages.SupportedLanguageIDs;
  for LangID in fUILangIDs do
    cbChooseLang.Items.Add(fLanguages[LangID].FriendlyName);
  if cbChooseLang.Items.Count > 0 then
  begin
    cbChooseLang.ItemIndex := 0;
    cbChooseLangChange(cbChooseLang);
  end;
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

{ TMockRenderer }

procedure TMockRenderer.AddLine(const S: string);
begin
  fMemo.Lines.Add(S);
end;

procedure TMockRenderer.AddLine(const Fmt: string; const Args: array of const);
begin
  AddLine(Format(Fmt, Args));
end;

procedure TMockRenderer.AfterElem(const ElemInfo: TSyntaxHiliteElemInfo);
begin
  AddLine('AFTER-ELEM [%s.%s]', [ElemInfo.BrushID, ElemInfo.AttrID]);
end;

procedure TMockRenderer.BeforeElem(const ElemInfo: TSyntaxHiliteElemInfo);
begin
  AddLine('BEFORE-ELEM [%s.%s]', [ElemInfo.BrushID, ElemInfo.AttrID]);
end;

procedure TMockRenderer.BeginLine;
begin
  AddLine('START-LINE');
end;

constructor TMockRenderer.Create(Memo: TMemo);
begin
  inherited Create;
  fMemo := Memo;
end;

procedure TMockRenderer.EndLine;
begin
  AddLine('END-LINE');
end;

procedure TMockRenderer.Finalise;
begin
  AddLine('FINALISE');
end;

procedure TMockRenderer.Initialise;
begin
  fMemo.Clear;
  AddLine('INITIALISE');
end;

procedure TMockRenderer.WriteElemText(const Text: string);
var
  S: string;
begin
  S := StrReplace(Text, #13, '<CR>');
  S := StrReplace(S, #10, '<LF>');
  S := StrReplace(S, #9, '<TAB>');
  AddLine('ELEM-TEXT "%s"', [Text]);
end;

end.


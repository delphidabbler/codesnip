program CSHighlighterTests;

{$R 'Themes.res' '..\Highlighters\Themes.rc'}
{$R 'Languages.res' '..\Languages\Languages.rc'}

uses
  Forms,
  FmTestMain in 'FmTestMain.pas' {MainTestForm},
  CS.SourceCode.Hiliter.Themes in '..\Highlighters\CS.SourceCode.Hiliter.Themes.pas',
  CS.SourceCode.Hiliter.Themes.Persist in '..\Highlighters\CS.SourceCode.Hiliter.Themes.Persist.pas',
  UExceptions in '..\..\TrunkSrc\UExceptions.pas',
  UComparers in '..\..\TrunkSrc\UComparers.pas',
  UStrUtils in '..\..\TrunkSrc\UStrUtils.pas',
  UConsts in '..\..\TrunkSrc\UConsts.pas',
  UBaseObjects in '..\..\TrunkSrc\UBaseObjects.pas',
  UStructs in '..\..\TrunkSrc\UStructs.pas',
  UIStringList in '..\..\TrunkSrc\UIStringList.pas',
  IntfCommon in '..\..\TrunkSrc\IntfCommon.pas',
  UEncodings in '..\..\TrunkSrc\UEncodings.pas',
  ULocales in '..\..\TrunkSrc\ULocales.pas',
  USystemInfo in '..\..\TrunkSrc\USystemInfo.pas',
  PJSysInfo in '..\..\TrunkSrc\3rdParty\PJSysInfo.pas',
  UIOUtils in '..\..\TrunkSrc\UIOUtils.pas',
  UResourceUtils in '..\..\TrunkSrc\UResourceUtils.pas',
  UURIEncode in '..\..\TrunkSrc\UURIEncode.pas',
  CS.SourceCode.Hiliter.Brushes in '..\Highlighters\CS.SourceCode.Hiliter.Brushes.pas',
  SynEditHighlighter in '..\SynEditUnits\SynEditHighlighter.pas',
  SynEditTypes in '..\SynEditUnits\SynEditTypes.pas',
  SynEditMiscClasses in '..\SynEditUnits\SynEditMiscClasses.pas',
  SynEditKeyConst in '..\SynEditUnits\SynEditKeyConst.pas',
  SynUnicode in '..\SynEditUnits\SynUnicode.pas',
  SynEditTextBuffer in '..\SynEditUnits\SynEditTextBuffer.pas',
  SynEditMiscProcs in '..\SynEditUnits\SynEditMiscProcs.pas',
  SynEditHighlighterOptions in '..\SynEditUnits\SynEditHighlighterOptions.pas',
  SynHighlighterMulti in '..\SynEditUnits\SynHighlighterMulti.pas',
  SynRegExpr in '..\SynEditUnits\SynRegExpr.pas',
  SynEditStrConst in '..\SynEditUnits\SynEditStrConst.pas',
  SynHighlighterHtml in '..\SynEditUnits\SynHighlighterHtml.pas',
  SynHighlighterJScript in '..\SynEditUnits\SynHighlighterJScript.pas',
  SynHighlighterPas in '..\SynEditUnits\SynHighlighterPas.pas',
  SynHighlighterPHP in '..\SynEditUnits\SynHighlighterPHP.pas',
  CS.SourceCode.Editor.Frame in '..\CodeEditor\CS.SourceCode.Editor.Frame.pas' {CodeEditorFrame: TFrame},
  SynEdit in '..\SynEditUnits\SynEdit.pas',
  SynTextDrawer in '..\SynEditUnits\SynTextDrawer.pas',
  SynEditKeyCmds in '..\SynEditUnits\SynEditKeyCmds.pas',
  SynEditKbdHandler in '..\SynEditUnits\SynEditKbdHandler.pas',
  SynEditWordWrap in '..\SynEditUnits\SynEditWordWrap.pas',
  CS.SourceCode.Hiliter.Parser in '..\Highlighters\CS.SourceCode.Hiliter.Parser.pas',
  CS.SourceCode.Hiliter.Renderers in '..\Highlighters\CS.SourceCode.Hiliter.Renderers.pas',
  UHTMLBuilder in '..\..\TrunkSrc\UHTMLBuilder.pas',
  UHTMLUtils in '..\..\TrunkSrc\UHTMLUtils.pas',
  UCSSUtils in '..\..\TrunkSrc\UCSSUtils.pas',
  URTFBuilder in '..\..\TrunkSrc\URTFBuilder.pas',
  URTFStyles in '..\..\TrunkSrc\URTFStyles.pas',
  URTFUtils in '..\..\TrunkSrc\URTFUtils.pas',
  UUtils in '..\..\TrunkSrc\UUtils.pas',
  UCSSBuilder in '..\..\TrunkSrc\UCSSBuilder.pas',
  CS.SourceCode.Hiliter.Renderers.CSS in '..\Highlighters\CS.SourceCode.Hiliter.Renderers.CSS.pas',
  CS.SourceCode.Languages in '..\Languages\CS.SourceCode.Languages.pas',
  CS.SourceCode.Languages.Persist in '..\Languages\CS.SourceCode.Languages.Persist.pas';

{$R *.res}

begin
  ReportMemoryLeaksOnShutdown := True;
  Application.Initialize;
  Application.MainFormOnTaskbar := True;
  Application.CreateForm(TMainTestForm, MainTestForm);
  Application.Run;
end.

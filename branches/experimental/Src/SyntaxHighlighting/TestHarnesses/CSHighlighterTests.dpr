program CSHighlighterTests;

{$R 'Themes.res' '..\Highlighters\Themes.rc'}

uses
  Forms,
  FmTestMain in 'FmTestMain.pas' {MainTestForm},
  CS.Hiliter.Themes in '..\Highlighters\CS.Hiliter.Themes.pas',
  CS.Hiliter.Themes.Persist in '..\Highlighters\CS.Hiliter.Themes.Persist.pas',
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
  UURIEncode in '..\..\TrunkSrc\UURIEncode.pas';

{$R *.res}

begin
  ReportMemoryLeaksOnShutdown := True;
  Application.Initialize;
  Application.MainFormOnTaskbar := True;
  Application.CreateForm(TMainTestForm, MainTestForm);
  Application.Run;
end.

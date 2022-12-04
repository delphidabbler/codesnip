program CodeSnip.Caboli;

uses
  Vcl.Forms,
  UThemesEx in 'UThemesEx.pas',
  UMultiCastEvents in 'UMultiCastEvents.pas',
  UMessageWindow in 'UMessageWindow.pas',
  UHiddenWindow in 'UHiddenWindow.pas',
  UStructs in 'UStructs.pas',
  IntfAligner in 'IntfAligner.pas',
  UNulFormAligner in 'UNulFormAligner.pas',
  UAppInfo in 'UAppInfo.pas',
  UBaseObjects in 'UBaseObjects.pas',
  USettings in 'USettings.pas',
  UIStringList in 'UIStringList.pas',
  IntfCommon in 'IntfCommon.pas',
  UStrUtils in 'UStrUtils.pas',
  UConsts in 'UConsts.pas',
  UExceptions in 'UExceptions.pas',
  UHexUtils in 'UHexUtils.pas',
  UIOUtils in 'UIOUtils.pas',
  UUtils in 'UUtils.pas',
  PJSysInfo in '3rdParty\PJSysInfo.pas',
  USystemInfo in 'USystemInfo.pas',
  PJVersionInfo in '3rdParty\PJVersionInfo.pas',
  UI.Forms.Root in 'UI.Forms.Root.pas' {RootForm},
  UControlStateMgr in 'UControlStateMgr.pas',
  UKeysHelper in 'UKeysHelper.pas',
  UMenus in 'UMenus.pas',
  UClassHelpers in 'UClassHelpers.pas',
  UHelpMgr in 'UHelpMgr.pas',
  UI.Forms.Main in 'UI.Forms.Main.pas' {MainForm};

{$R *.res}

begin
  TThemeServicesEx.SetAppropriateThemeMode;
  Application.Initialize;
  Application.MainFormOnTaskbar := True;
  Application.CreateForm(TMainForm, MainForm);
  Application.Run;
end.

program CodeSnip5;

uses
  Vcl.Forms,
  Vcl.Themes,
  Vcl.Styles,
  CS.UI.Forms.Root in 'CS.UI.Forms.Root.pas' {RootForm},
  CS.UI.Forms.Main in 'CS.UI.Forms.Main.pas' {MainForm},
  UConsts in '..\..\Src\UConsts.pas',
  UStrUtils in '..\..\Src\UStrUtils.pas',
  IntfCommon in '..\..\Src\IntfCommon.pas',
  UBaseObjects in '..\..\Src\UBaseObjects.pas',
  UStructs in '..\..\Src\UStructs.pas',
  UExceptions in '..\..\Src\UExceptions.pas',
  UIStringList in '..\..\Src\UIStringList.pas';

{$R *.res}

begin
  Application.Initialize;
  Application.MainFormOnTaskbar := True;
  Application.CreateForm(TMainForm, MainForm);
  TStyleManager.TrySetStyle('Windows10 Dark');
  Application.Run;
end.

program CodeSnip5;

uses
  Vcl.Forms,
  Vcl.Themes,
  Vcl.Styles,
  CS.UI.Forms.Root in 'CS.UI.Forms.Root.pas' {RootForm},
  CS.UI.Forms.Main in 'CS.UI.Forms.Main.pas' {MainForm};

{$R *.res}

begin
  Application.Initialize;
  Application.MainFormOnTaskbar := True;
  Application.CreateForm(TMainForm, MainForm);
  TStyleManager.TrySetStyle('Windows10 Dark');
  Application.Run;
end.

program CodeSnip5;

uses
  Vcl.Forms,
  Vcl.Themes,
  Vcl.Styles,
  CS.UI.Forms.Root in 'CS.UI.Forms.Root.pas' {RootForm};

{$R *.res}

begin
  Application.Initialize;
  Application.MainFormOnTaskbar := True;
  TStyleManager.TrySetStyle('Windows10 Dark');
  Application.Run;
end.

program CodeSnip.Caboli;

uses
  Vcl.Forms,
  UThemesEx in 'UThemesEx.pas',
  UMultiCastEvents in 'UMultiCastEvents.pas',
  UMessageWindow in 'UMessageWindow.pas',
  UHiddenWindow in 'UHiddenWindow.pas',
  UStructs in 'UStructs.pas';

{$R *.res}

begin
  TThemeServicesEx.SetAppropriateThemeMode;
  Application.Initialize;
  Application.MainFormOnTaskbar := True;
  Application.Run;
end.

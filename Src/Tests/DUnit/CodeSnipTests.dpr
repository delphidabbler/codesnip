{
 * Delphi DUnit Test Project for CodeSnip
 * --------------------------------------
 *
 * This project provides a DUnit test framework for some of the source code from
 * the CodeSnip project.
 *
 * The project can be compiled either a GUI application (the default) or as a
 * console application. To compile as a console application the
 * CONSOLE_TESTRUNNER symbol must be defined.
 *
 * Some tests will not be available unless the TESTING symbol is defined, which
 * it is by default when compiling from the Delphi IDE.
 *
 * $Rev$
 * $Date$
}


program CodeSnipTests;

{$IFDEF CONSOLE_TESTRUNNER}
{$APPTYPE CONSOLE}
{$ENDIF}

uses
  Forms,
  TestFramework,
  GUITestRunner,
  TextTestRunner,
  TestUStrUtils in 'TestUStrUtils.pas',
  UStrUtils in '..\..\UStrUtils.pas',
  UConsts in '..\..\UConsts.pas',
  TestUUtils in 'TestUUtils.pas',
  UUtils in '..\..\UUtils.pas',
  UBaseObjects in '..\..\UBaseObjects.pas',
  TestUBaseObjects in 'TestUBaseObjects.pas',
  USingleton in '..\..\USingleton.pas',
  TestUSingleton in 'TestUSingleton.pas',
  TestUURIEncode in 'TestUURIEncode.pas',
  UURIEncode in '..\..\UURIEncode.pas';

{$R *.RES}

begin
  ReportMemoryLeaksOnShutdown := DebugHook <> 0;
  Application.Initialize;
  if IsConsole then
    with TextTestRunner.RunRegisteredTests do
      Free
  else
    GUITestRunner.RunRegisteredTests;
end.


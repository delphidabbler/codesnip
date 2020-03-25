{
 * Delphi DUnit Test Project for CodeSnip
 * --------------------------------------
 *
 * This project provides a DUnit test framework for some of the source code from
 * the CodeSnip project. It can be compiled as either a GUI or Console
 * application.
 *
 * Console Application
 * ~~~~~~~~~~~~~~~~~~~
 *
 * The CONSOLE_TESTRUNNER symbol must be defined to create a console
 * application. From the IDE define the symbol in the conditional defines entry
 * in project options. To compile from the command line pass the -D switch to
 * the compiler to define the symbol. The command line compiler also need to be
 * told the location of the DUnit binaries. This is usually $(BDS)\lib. Use the
 * following command line:
 *
 *   DCC32 -DCONSOLE_TESTRUNNER -B -R<path-to-dunit-binaries> CodeSnipTests
 *
 * For example (all on one line):
 *
 *   DCC32 -DCONSOLE_TESTRUNNER -B
 *     -R"C:\Program Files\Embarcadero\RAD Studio\7.0\lib" CodeSnipTests
 *
 * GUI Application
 * ~~~~~~~~~~~~~~~
 *
 * The CONSOLE_TESTRUNNER symbol must not be defined to compile a GUI
 * application. From the IDE make sure the symbol is removed from the
 * conditional defines entry in project options if necessary. To compile from
 * the command line do this:
 *
 *   DCC32 -B -R<path-to-dunit-binaries> CodeSnipTests
 *
 * Note that once again the path to the DUnit binaries is required.
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
  UStrUtils in '..\..\..\Src\UStrUtils.pas',
  UConsts in '..\..\..\Src\UConsts.pas',
  TestUUtils in 'TestUUtils.pas',
  UUtils in '..\..\..\Src\UUtils.pas',
  UBaseObjects in '..\..\..\Src\UBaseObjects.pas',
  TestUBaseObjects in 'TestUBaseObjects.pas',
  USingleton in '..\..\..\Src\USingleton.pas',
  TestUSingleton in 'TestUSingleton.pas',
  TestUContainers in 'TestUContainers.pas',
  UContainers in '..\..\..\Src\UContainers.pas';

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


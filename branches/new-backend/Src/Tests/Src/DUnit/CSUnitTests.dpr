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
 * the compiler:
 *
 *   DCC32 -DCONSOLE_TESTRUNNER -B CSUnitTests
 *
 * GUI Application
 * ~~~~~~~~~~~~~~~
 *
 * The CONSOLE_TESTRUNNER symbol must not be defined to compile a GUI
 * application. From the IDE make sure the symbol is removed from the
 * conditional defines entry in project options if necessary. To compile from
 * the command line do this:
 *
 *   DCC32 -B CSUnitTests
 *
 * -----------------------------------------------------------------------------
 * $Rev$
 * $Date$
 * -----------------------------------------------------------------------------
}


program CSUnitTests;

{$IFDEF CONSOLE_TESTRUNNER}
{$APPTYPE CONSOLE}
{$ENDIF}

uses
  Forms,
  TestFramework,
  GUITestRunner,
  TextTestRunner,
  UBaseObjects in '..\..\..\UBaseObjects.pas',
  TestUBaseObjects in 'TestUBaseObjects.pas';
{$R *.RES}

begin
  Application.Initialize;
  if IsConsole then
    with TextTestRunner.RunRegisteredTests do
      Free
    else
      GUITestRunner.RunRegisteredTests;
end.


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
 * the compiler to define the symbole. The command line compiler also need to be
 * told the location of DUnit's .dfm file. (usually $(BDS)\lib). Use the
 * following command line:
 *
 *   DCC32 -DCONSOLE_TESTRUNNER -B -R<path-to-dunit-binaries> CSUnitTests
 *
 * For example (all on one line):
 *
 *   DCC32 -DCONSOLE_TESTRUNNER -B
 *     -R"C:\Program Files\Embarcadero\RAD Studio\7.0\lib" CSUnitTests
 *
 * GUI Application
 * ~~~~~~~~~~~~~~~
 *
 * The CONSOLE_TESTRUNNER symbol must not be defined to compile a GUI
 * application. From the IDE make sure the symbol is removed from the
 * conditional defines entry in project options if necessary. To compile from
 * the command line do this:
 *
 *   DCC32 -B -R<path-to-dunit-binaries> CSUnitTests
 *
 * Note that once again the path to the DUnit binaries is required.
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
  TestUBaseObjects in 'TestUBaseObjects.pas',
  TestUSingleton in 'TestUSingleton.pas',
  TestNsDatabaseUCookies in 'TestNsDatabaseUCookies.pas',
  TestNsDatabaseUDataItem in 'TestNsDatabaseUDataItem.pas',
  TestNsDatabaseUDataPool in 'TestNsDatabaseUDataPool.pas',
  NsDatabase.UCookies in '..\..\..\Src\Experimental\NsDatabase.UCookies.pas',
  NsDatabase.UDataItem in '..\..\..\Src\Experimental\NsDatabase.UDataItem.pas',
  NsDatabase.UDataPool in '..\..\..\Src\Experimental\NsDatabase.UDataPool.pas',
  UBaseObjects in '..\..\..\Src\UBaseObjects.pas',
  UExceptions in '..\..\..\Src\UExceptions.pas',
  USingleton in '..\..\..\Src\Experimental\USingleton.pas',
  NSDatabase.UDatabase in '..\..\..\Src\Experimental\NSDatabase.UDatabase.pas',
  TestNSDatabaseUDatabase in 'TestNSDatabaseUDatabase.pas',
  UContainers in '..\..\..\Src\UContainers.pas',
  TestUOrderedList in 'TestUOrderedList.pas',
  UOrderedDictionary in '..\..\..\Src\Experimental\UOrderedDictionary.pas',
  TestUOrderedDictionary in 'TestUOrderedDictionary.pas';

{$R *.RES}

begin
  Application.Initialize;
  if IsConsole then
    with TextTestRunner.RunRegisteredTests do
      Free
    else
      GUITestRunner.RunRegisteredTests;
end.


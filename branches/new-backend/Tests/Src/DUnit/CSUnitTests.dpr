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

{$WARN SYMBOL_PLATFORM OFF}

uses
  Forms,
  TestFramework,
  GUITestRunner,
  TextTestRunner,
  TestUBaseObjects in 'TestUBaseObjects.pas',
  TestDBUCookies in 'TestDBUCookies.pas',
  TestDBUDataItem in 'TestDBUDataItem.pas',
  TestDBUDataPool in 'TestDBUDataPool.pas',
  DB.UCookies in '..\..\..\Src\Experimental\DB.UCookies.pas',
  DB.UDataItem in '..\..\..\Src\Experimental\DB.UDataItem.pas',
  DB.UDataPool in '..\..\..\Src\Experimental\DB.UDataPool.pas',
  UBaseObjects in '..\..\..\Src\UBaseObjects.pas',
  UExceptions in '..\..\..\Src\UExceptions.pas',
  USingleton in '..\..\..\Src\Experimental\USingleton.pas',
  DB.UDatabase in '..\..\..\Src\Experimental\DB.UDatabase.pas',
  TestDBUDatabase in 'TestDBUDatabase.pas',
  UContainers in '..\..\..\Src\UContainers.pas',
  TestUContainers in 'TestUContainers.pas',
  DB.UObjectDestructionMgr in '..\..\..\Src\Experimental\DB.UObjectDestructionMgr.pas',
  TestDBUObjectDestructionMgr in 'TestDBUObjectDestructionMgr.pas',
  DB.USnippet in '..\..\..\Src\Experimental\DB.USnippet.pas',
  DB.UConsts in '..\..\..\Src\Experimental\DB.UConsts.pas',
  TestDBUSnippet in 'TestDBUSnippet.pas',
  DB.UBaseGroup in '..\..\..\Src\Experimental\DB.UBaseGroup.pas',
  DB.UCategoryGroup in '..\..\..\Src\Experimental\DB.UCategoryGroup.pas',
  TestDBUCategoryGroup in 'TestDBUCategoryGroup.pas',
  UTestHelpers in 'UTestHelpers.pas',
  TestDBUInitialLetterGroup in 'TestDBUInitialLetterGroup.pas',
  DB.UInitialLetterGroup in '..\..\..\Src\Experimental\DB.UInitialLetterGroup.pas',
  DB.USnippetKindGroup in '..\..\..\Src\Experimental\DB.USnippetKindGroup.pas',
  TestDBUSnippetKindGroup in 'TestDBUSnippetKindGroup.pas',
  UStructs in '..\..\..\Src\UStructs.pas';

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


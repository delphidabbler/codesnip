program CodeSnip.Cupola.Tests;

{$IFNDEF TESTINSIGHT}
{$APPTYPE CONSOLE}
{$ENDIF}
{$STRONGLINKTYPES ON}
uses
  System.SysUtils,
  {$IFDEF TESTINSIGHT}
  TestInsight.DUnitX,
  {$ELSE}
  DUnitX.Loggers.Console,
  DUnitX.Loggers.Xml.NUnit,
  {$ENDIF }
  DUnitX.TestFramework,
  CSLE.Utils.Conversions in '..\src\CSLE.Utils.Conversions.pas',
  Test.Utils.Conversions in 'Test.Utils.Conversions.pas',
  Test.Utils.Dates in 'Test.Utils.Dates.pas',
  CSLE.Utils.Dates in '..\src\CSLE.Utils.Dates.pas',
  Test.TextData in 'Test.TextData.pas',
  CSLE.TextData in '..\src\CSLE.TextData.pas',
  CSLE.Streams.Wrapper in '..\src\CSLE.Streams.Wrapper.pas',
  Test.Streams.Wrapper in 'Test.Streams.Wrapper.pas',
  CSLE.Exceptions in '..\src\CSLE.Exceptions.pas',
  CSLE.SourceCode.Language in '..\src\CSLE.SourceCode.Language.pas',
  Test.SourceCode.Language in 'Test.SourceCode.Language.pas',
  CSLE.Snippets.ID in '..\src\CSLE.Snippets.ID.pas',
  Test.Snippets.ID in 'Test.Snippets.ID.pas',
  CSLE.Snippets.Tag in '..\src\CSLE.Snippets.Tag.pas',
  Grijjy.Collections in '..\src\vendor\grijjy-foundation\Grijjy.Collections.pas',
  Test.Snippets.Tag in 'Test.Snippets.Tag.pas',
  CSLE.Snippets.Format in '..\src\CSLE.Snippets.Format.pas',
  Test.Snippets.Format in 'Test.Snippets.Format.pas',
  CSLE.Snippets.Markup in '..\src\CSLE.Snippets.Markup.pas',
  Test.Snippets.Markup in 'Test.Snippets.Markup.pas';

{$IFNDEF TESTINSIGHT}
var
  runner: ITestRunner;
  results: IRunResults;
  logger: ITestLogger;
  nunitLogger : ITestLogger;
{$ENDIF}
begin
{$IFDEF TESTINSIGHT}
  TestInsight.DUnitX.RunRegisteredTests;
{$ELSE}
  try
    //Check command line options, will exit if invalid
    TDUnitX.CheckCommandLine;
    //Create the test runner
    runner := TDUnitX.CreateRunner;
    //Tell the runner to use RTTI to find Fixtures
    runner.UseRTTI := True;
    //When true, Assertions must be made during tests;
    runner.FailsOnNoAsserts := True;

    //tell the runner how we will log things
    //Log to the console window if desired
    if TDUnitX.Options.ConsoleMode <> TDunitXConsoleMode.Off then
    begin
      logger := TDUnitXConsoleLogger.Create(TDUnitX.Options.ConsoleMode = TDunitXConsoleMode.Quiet);
      runner.AddLogger(logger);
    end;
    //Generate an NUnit compatible XML File
    nunitLogger := TDUnitXXMLNUnitFileLogger.Create(TDUnitX.Options.XMLOutputFile);
    runner.AddLogger(nunitLogger);

    //Run tests
    results := runner.Execute;
    if not results.AllPassed then
      System.ExitCode := EXIT_ERRORS;

    {$IFNDEF CI}
    //We don't want this happening when running under CI.
    if TDUnitX.Options.ExitBehavior = TDUnitXExitBehavior.Pause then
    begin
      System.Write('Done.. press <Enter> key to quit.');
      System.Readln;
    end;
    {$ENDIF}
  except
    on E: Exception do
      System.Writeln(E.ClassName, ': ', E.Message);
  end;
{$ENDIF}
end.

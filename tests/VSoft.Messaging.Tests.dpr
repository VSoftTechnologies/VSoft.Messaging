program VSoft.Messaging.Tests;

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
  {$ENDIF}
  DUnitX.TestFramework,
  VSoft.Messaging.Internal in '..\src\VSoft.Messaging.Internal.pas',
  VSoft.Messaging in '..\src\VSoft.Messaging.pas',
  VSoft.Messaging.Channel in '..\src\VSoft.Messaging.Channel.pas',
  VSoft.Messaging.Dispatchers in '..\src\VSoft.Messaging.Dispatchers.pas',
  Testing.Helpers in 'Testing.Helpers.pas',
  Testing.Messages in 'Testing.Messages.pas',
  Testing.Receivers in 'Testing.Receivers.pas',
  Tests.Wrapper in 'Tests.Wrapper.pas',
  Tests.Channel in 'Tests.Channel.pas',
  Tests.ThreadedDispatcher in 'Tests.ThreadedDispatcher.pas',
  Tests.UIDispatcher in 'Tests.UIDispatcher.pas',
  Tests.Filters in 'Tests.Filters.pas',
  Tests.Lifetime in 'Tests.Lifetime.pas',
  Tests.Backpressure in 'Tests.Backpressure.pas',
  Tests.VclTargets in 'Tests.VclTargets.pas',
  Tests.Stress in 'Tests.Stress.pas',
  Tests.ChannelBackpressure in 'Tests.ChannelBackpressure.pas',
  Tests.DispatcherSemantics in 'Tests.DispatcherSemantics.pas',
  Tests.Reentrancy in 'Tests.Reentrancy.pas',
  Tests.Flush in 'Tests.Flush.pas';

{$IFNDEF TESTINSIGHT}
var
  runner : ITestRunner;
  results : IRunResults;
  logger : ITestLogger;
  nunitLogger : ITestLogger;
{$ENDIF}
begin
{$IFDEF TESTINSIGHT}
  TestInsight.DUnitX.RunRegisteredTests;
{$ELSE}
  try
    TDUnitX.CheckCommandLine;
    runner := TDUnitX.CreateRunner;
    //the fixtures register themselves
    runner.UseRTTI := false;
    //some tests only check that nothing raises
    runner.FailsOnNoAsserts := false;

    logger := TDUnitXConsoleLogger.Create(true);
    runner.AddLogger(logger);

    nunitLogger := TDUnitXXMLNUnitFileLogger.Create(TDUnitX.Options.XMLOutputFile);
    runner.AddLogger(nunitLogger);

    results := runner.Execute;
    if not results.AllPassed then
      System.ExitCode := EXIT_ERRORS;

    //--exitbehavior:Pause keeps the window open when run from the IDE
    if TDUnitX.Options.ExitBehavior = TDUnitXExitBehavior.Pause then
    begin
      System.Write('Done.. press <Enter> key to quit.');
      System.Readln;
    end;
  except
    on e : Exception do
    begin
      System.Writeln(e.ClassName, ': ', e.Message);
      System.ExitCode := EXIT_ERRORS;
    end;
  end;
{$ENDIF}
end.

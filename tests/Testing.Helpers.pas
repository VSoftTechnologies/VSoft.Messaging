unit Testing.Helpers;

interface

uses
  System.SysUtils,
  System.Classes,
  System.SyncObjs;

const
  //how long a test waits for something that should happen before failing
  cWaitTimeout = 5000;

type
  //Runs a proc on its own thread. FreeOnTerminate is false so the test can wait for it and free it.
  TTestThread = class(TThread)
  private
    FProc : TProc;
    FErrorMessage : string;
  protected
    procedure Execute; override;
  public
    constructor Create(const proc : TProc);
    //class name and message of any exception the proc raised, empty if none
    property ErrorMessage : string read FErrorMessage;
  end;

  //thread safe list of strings, used to record what happened on other threads
  TTestLog = class
  private
    FLock : TCriticalSection;
    FItems : TStringList;
  public
    constructor Create;
    destructor Destroy; override;
    procedure Add(const value : string);
    procedure Clear;
    function Count : integer;
    //the items separated by commas
    function Text : string;
  end;

//Waits until condition returns true, processing TThread.Synchronize/Queue calls while it waits so
//UI dispatchers can deliver. Must be called on the main thread. Returns the final value of condition.
function WaitUntil(const condition : TFunc<boolean>; const timeoutMs : integer = cWaitTimeout) : boolean;

//Processes TThread.Synchronize/Queue calls for the given time. Must be called on the main thread.
procedure PumpFor(const milliseconds : integer);

function StartThread(const proc : TProc) : TTestThread;

//Waits for the thread to finish (processing synchronize calls while waiting) and frees it.
//If it does not finish in time it is left running rather than blocking the whole test run,
//the reference is cleared and false is returned.
function FinishThread(var thread : TTestThread; const timeoutMs : integer = cWaitTimeout) : boolean; overload;
function FinishThread(var thread : TTestThread; out errorMessage : string; const timeoutMs : integer = cWaitTimeout) : boolean; overload;

//IMessageChannel.OnError is a plain procedure, so the errors it reports are recorded in a global log.
procedure RecordChannelError(const AMessage : string);
//records the error and then raises, to test an OnError handler that fails
procedure RaisingChannelError(const AMessage : string);
function ChannelErrors : TTestLog;

//Puts the global queue depth back to the value it had when the test started, so a test that leaks
//depth (review finding H4) does not shift the backpressure thresholds seen by the tests after it.
//Only call it once every dispatcher the test created has been destroyed.
procedure RestoreGlobalQueueDepth(const depth : integer);

implementation

uses
  System.Diagnostics,
  VSoft.Messaging.Internal;

var
  GChannelErrors : TTestLog;

{ TTestThread }

constructor TTestThread.Create(const proc : TProc);
begin
  FProc := proc;
  inherited Create(false);
end;

procedure TTestThread.Execute;
begin
  try
    FProc();
  except
    on e : Exception do
      FErrorMessage := e.ClassName + ': ' + e.Message;
  end;
end;

{ TTestLog }

constructor TTestLog.Create;
begin
  inherited Create;
  FLock := TCriticalSection.Create;
  FItems := TStringList.Create;
end;

destructor TTestLog.Destroy;
begin
  FItems.Free;
  FLock.Free;
  inherited;
end;

procedure TTestLog.Add(const value : string);
begin
  FLock.Acquire;
  try
    FItems.Add(value);
  finally
    FLock.Release;
  end;
end;

procedure TTestLog.Clear;
begin
  FLock.Acquire;
  try
    FItems.Clear;
  finally
    FLock.Release;
  end;
end;

function TTestLog.Count : integer;
begin
  FLock.Acquire;
  try
    result := FItems.Count;
  finally
    FLock.Release;
  end;
end;

function TTestLog.Text : string;
var
  i : integer;
begin
  result := '';
  FLock.Acquire;
  try
    for i := 0 to FItems.Count - 1 do
    begin
      if i > 0 then
        result := result + ',';
      result := result + FItems[i];
    end;
  finally
    FLock.Release;
  end;
end;

function WaitUntil(const condition : TFunc<boolean>; const timeoutMs : integer) : boolean;
var
  stopwatch : TStopwatch;
begin
  stopwatch := TStopwatch.StartNew;
  result := condition();
  while (not result) and (stopwatch.ElapsedMilliseconds < timeoutMs) do
  begin
    //waits up to 5ms for a synchronize request, and runs any that arrive
    CheckSynchronize(5);
    result := condition();
  end;
end;

procedure PumpFor(const milliseconds : integer);
var
  stopwatch : TStopwatch;
begin
  stopwatch := TStopwatch.StartNew;
  while stopwatch.ElapsedMilliseconds < milliseconds do
    CheckSynchronize(5);
end;

function StartThread(const proc : TProc) : TTestThread;
begin
  result := TTestThread.Create(proc);
end;

function FinishThread(var thread : TTestThread; out errorMessage : string; const timeoutMs : integer) : boolean;
var
  localThread : TTestThread;
begin
  errorMessage := '';
  result := true;
  if thread = nil then
    exit;
  localThread := thread;
  result := WaitUntil(
    function : boolean
    begin
      result := localThread.Finished;
    end, timeoutMs);
  if result then
  begin
    errorMessage := thread.ErrorMessage;
    FreeAndNil(thread);
  end
  else
    thread := nil; //stuck, leave it running rather than block the test run
end;

function FinishThread(var thread : TTestThread; const timeoutMs : integer) : boolean;
var
  errorMessage : string;
begin
  result := FinishThread(thread, errorMessage, timeoutMs);
end;

procedure RecordChannelError(const AMessage : string);
begin
  GChannelErrors.Add(AMessage);
end;

procedure RaisingChannelError(const AMessage : string);
begin
  GChannelErrors.Add(AMessage);
  raise Exception.Create('OnError handler failed while reporting : ' + AMessage);
end;

function ChannelErrors : TTestLog;
begin
  result := GChannelErrors;
end;

procedure RestoreGlobalQueueDepth(const depth : integer);
begin
  while TMessagingControl.GetGlobalQueueDepth > depth do
    TMessagingControl.DecrementGlobalQueueDepth;
  while TMessagingControl.GetGlobalQueueDepth < depth do
    TMessagingControl.IncrementGlobalQueueDepth;
end;

initialization
  GChannelErrors := TTestLog.Create;

finalization
  GChannelErrors.Free;

end.

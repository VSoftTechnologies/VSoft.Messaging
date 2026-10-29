unit Tests.Reentrancy;

interface

uses
  DUnitX.TestFramework,
  VSoft.Messaging;

type
  //Handlers that free their dispatcher, process synchronize calls, or send messages themselves.
  [TestFixture]
  TReentrancyTests = class
  private
    FChannel : IMessageChannel;
    FBaselineDepth : integer;
    FMaxBurst : integer;
  public
    [Setup]
    procedure Setup;
    [TearDown]
    procedure TearDown;

    [Test]
    procedure UIHandlerFreesItsOwnReceiver_DoesNotHang;
    [Test]
    procedure UIHandlerThatProcessesSynchronize_DoesNotNestItsOwnBatches;
    [Test]
    procedure DrainStuckBehindANestedCheckSynchronize_IsHandledByAMainThreadFlush;
    [Test]
    procedure SendFromAHandlerToItsOwnDispatcher_IsHandledStraightAway;
    [Test]
    procedure SendFromAUIHandlerToItsOwnDispatcher_IsHandledStraightAway;
    [Test]
    procedure SendCycleBetweenAWorkerAndTheMainThread_IsReportedInsteadOfHanging;
  end;

implementation

uses
  System.SysUtils,
  System.Classes,
  System.SyncObjs,
  VSoft.Messaging.Internal,
  Testing.Helpers,
  Testing.Messages,
  Testing.Receivers;

{ TReentrancyTests }

procedure TReentrancyTests.Setup;
begin
  ChannelErrors.Clear;
  FMaxBurst := TMessagingOptions.MaxBurst;
  FBaselineDepth := TMessagingControl.GetGlobalQueueDepth;
  FChannel := TMessageChannelFactory.CreateChannel;
  FChannel.OnError := RecordChannelError;
end;

procedure TReentrancyTests.TearDown;
begin
  FChannel := nil;
  TMessagingOptions.MaxBurst := FMaxBurst;
  RestoreGlobalQueueDepth(FBaselineDepth);
end;

procedure TReentrancyTests.UIHandlerFreesItsOwnReceiver_DoesNotHang;
var
  receiver : TOwningReceiver;
  destroyed : boolean;
begin
  destroyed := false;
  receiver := TOwningReceiver.Create(FChannel, true);
  receiver.OnDestroyed :=
    procedure
    begin
      destroyed := true;
    end;
  FChannel.Queue.PostMessage<TFreeSelfMessage>(TFreeSelfMessage.Create(0));
  //the handler runs on this thread, inside WaitUntil, and frees the receiver and its dispatcher there
  Assert.IsTrue(WaitUntil(
    function : boolean
    begin
      result := destroyed;
    end, 3000), 'a UI receiver that freed itself from its own handler was not destroyed');
end;

procedure TReentrancyTests.UIHandlerThatProcessesSynchronize_DoesNotNestItsOwnBatches;
var
  receiver : TOwningReceiver;
  otherChannel : IMessageChannel;
  slowReceiver : TOwningReceiver;
  poster : TTestThread;
  values : TArray<integer>;
  i : integer;
begin
  //one message per drain, so the rest are still queued while the first handler runs
  TMessagingOptions.MaxBurst := 1;
  otherChannel := TMessageChannelFactory.CreateChannel;
  slowReceiver := TOwningReceiver.Create(otherChannel);
  receiver := TOwningReceiver.Create(FChannel, true);
  try
    poster := StartThread(
      procedure
      var
        j : integer;
      begin
        FChannel.Queue.PostMessage<TActionMessage>(TActionMessage.Create(
          procedure
          begin
            //as FinalBuilder does with Application.ProcessMessages inside a UI handler
            PumpFor(100);
            //a flush on the main thread handles queued UI dispatcher messages while it waits, but
            //must not start on the dispatcher whose handler is running below it
            otherChannel.Queue.PostMessage<TActionMessage>(TActionMessage.Create(
              procedure
              begin
                Sleep(150);
              end));
            otherChannel.Flush(cWaitTimeout);
          end));
        for j := 1 to 5 do
          FChannel.Queue.PostMessage<TValueMessage>(TValueMessage.Create(j));
      end);
    Assert.IsTrue(FinishThread(poster), 'the posting thread did not finish');
    Assert.IsTrue(WaitUntil(
      function : boolean
      begin
        result := receiver.ValueCount = 5;
      end), 'not every message was delivered');
    Assert.AreEqual(1, receiver.MaxActiveHandlers, 'a handler of the same dispatcher ran nested inside another');
    values := receiver.Values;
    for i := 0 to 4 do
      Assert.AreEqual(i + 1, values[i], 'messages were handled out of order');
  finally
    receiver.Free;
    slowReceiver.Free;
    otherChannel := nil;
  end;
end;

procedure TReentrancyTests.DrainStuckBehindANestedCheckSynchronize_IsHandledByAMainThreadFlush;
var
  receiver : TOwningReceiver;
  queuer : TTestThread;
  flushDone : boolean;
  flushed : boolean;
begin
  flushDone := false;
  flushed := false;
  receiver := TOwningReceiver.Create(FChannel, true);
  try
    //The flush proc and the UI dispatcher's drain are queued together, so one CheckSynchronize picks up
    //both and runs the flush first. A nested CheckSynchronize cannot reach the drain behind it, so the
    //flush has to handle the UI dispatcher's message itself.
    queuer := StartThread(
      procedure
      begin
        TThread.Queue(nil,
          procedure
          begin
            flushed := FChannel.Flush(3000);
            flushDone := true;
          end);
        FChannel.Queue.PostMessage<TValueMessage>(TValueMessage.Create(1));
      end);
    //wait without processing synchronize calls, so both procs are queued before any of them is taken
    while not queuer.Finished do
      Sleep(1);
    queuer.Free;
    Assert.IsTrue(WaitUntil(
      function : boolean
      begin
        result := flushDone;
      end), 'the flush did not finish');
    Assert.IsTrue(flushed, 'the flush timed out waiting for a UI dispatcher whose drain was stuck');
    Assert.AreEqual(1, receiver.ValueCount);
  finally
    receiver.Free;
  end;
end;

procedure TReentrancyTests.SendFromAHandlerToItsOwnDispatcher_IsHandledStraightAway;
var
  receiver : TOwningReceiver;
  countAfterSend : integer;
  threadIds : TArray<TThreadID>;
begin
  countAfterSend := -1;
  receiver := TOwningReceiver.Create(FChannel);
  try
    FChannel.Queue.PostMessage<TActionMessage>(TActionMessage.Create(
      procedure
      begin
        //the dispatcher's queue cannot move while this handler runs, so this has to be handled now
        FChannel.Queue.SendMessage<TValueMessage>(TValueMessage.Create(1));
        countAfterSend := receiver.ValueCount;
      end));
    Assert.IsTrue(WaitUntil(
      function : boolean
      begin
        result := receiver.ActionCount = 1;
      end, 3000), 'a handler that sent to its own dispatcher did not finish');
    Assert.AreEqual(1, countAfterSend, 'the message sent from the handler had not been handled when SendMessage returned');
    threadIds := receiver.ThreadIds;
    Assert.IsTrue(threadIds[0] = threadIds[1], 'the sent message was handled on a different thread');
  finally
    receiver.Free;
  end;
end;

procedure TReentrancyTests.SendFromAUIHandlerToItsOwnDispatcher_IsHandledStraightAway;
var
  receiver : TOwningReceiver;
  countAfterSend : integer;
begin
  countAfterSend := -1;
  receiver := TOwningReceiver.Create(FChannel, true);
  try
    FChannel.Queue.PostMessage<TActionMessage>(TActionMessage.Create(
      procedure
      begin
        FChannel.Queue.SendMessage<TValueMessage>(TValueMessage.Create(1));
        countAfterSend := receiver.ValueCount;
      end));
    Assert.IsTrue(WaitUntil(
      function : boolean
      begin
        result := receiver.ActionCount = 1;
      end, 3000), 'a UI handler that sent to its own dispatcher did not finish');
    Assert.AreEqual(1, countAfterSend, 'the message sent from the handler had not been handled when SendMessage returned');
  finally
    receiver.Free;
  end;
end;

procedure TReentrancyTests.SendCycleBetweenAWorkerAndTheMainThread_IsReportedInsteadOfHanging;
var
  uiReceiver : TOwningReceiver;
  workerChannel : IMessageChannel;
  workerReceiver : TOwningReceiver;
  uiHandlerDone : boolean;
begin
  uiHandlerDone := false;
  workerChannel := TMessageChannelFactory.CreateChannel;
  workerReceiver := TOwningReceiver.Create(workerChannel);
  uiReceiver := TOwningReceiver.Create(FChannel, true);
  try
    //The UI handler (main thread) sends to the worker and waits. The worker's handler sends back to the
    //UI dispatcher, which cannot run until the UI handler below it returns.
    FChannel.Queue.PostMessage<TActionMessage>(TActionMessage.Create(
      procedure
      begin
        workerChannel.Queue.SendMessage<TActionMessage>(TActionMessage.Create(
          procedure
          begin
            FChannel.Queue.SendMessage<TValueMessage>(TValueMessage.Create(1));
          end));
        uiHandlerDone := true;
      end));
    Assert.IsTrue(WaitUntil(
      function : boolean
      begin
        result := uiHandlerDone;
      end, 5000), 'the sends deadlocked');
    Assert.IsTrue(WaitUntil(
      function : boolean
      begin
        result := uiReceiver.ValueCount = 1;
      end), 'the message that could not be sent was not delivered later');
    Assert.Contains(ChannelErrors.Text, 'deadlock', 'the send that would have deadlocked was not reported');
  finally
    uiReceiver.Free;
    workerReceiver.Free;
    workerChannel := nil;
  end;
end;

initialization
  TDUnitX.RegisterTestFixture(TReentrancyTests);

end.

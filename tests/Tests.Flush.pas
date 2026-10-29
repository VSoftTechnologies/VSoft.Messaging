unit Tests.Flush;

interface

uses
  DUnitX.TestFramework,
  VSoft.Messaging;

type
  //IMessageChannel.Flush and IMessageDispatcher.WaitUntilIdle
  [TestFixture]
  TFlushTests = class
  private
    FChannel : IMessageChannel;
    FBaselineDepth : integer;
  public
    [Setup]
    procedure Setup;
    [TearDown]
    procedure TearDown;

    [Test]
    procedure Flush_NothingPosted_ReturnsTrueStraightAway;
    [Test]
    procedure Flush_WaitsUntilPostedMessagesHaveBeenHandled;
    [Test]
    procedure Flush_TimesOut_ReturnsFalse;
    [Test]
    procedure Flush_DoesNotWaitForMessagesPostedAfterIt;
    [Test]
    procedure Flush_WaitsForMessagesQueuedOnADispatcherThatUnsubscribed;
    [Test]
    procedure Flush_FromAHandlerOfTheChannel_ReturnsFalse;
    [Test]
    procedure Flush_OnTheMainThread_HandlesUIDispatchers;
    [Test]
    procedure WaitUntilIdle_NothingQueued_ReturnsTrue;
    [Test]
    procedure WaitUntilIdle_WaitsUntilQueuedMessagesHaveBeenHandled;
    [Test]
    procedure WaitUntilIdle_FromItsOwnHandler_ReturnsFalse;
    [Test]
    procedure WaitUntilIdle_UIDispatcherOnTheMainThread_ReturnsTrue;
  end;

implementation

uses
  System.SysUtils,
  System.Classes,
  System.SyncObjs,
  System.Diagnostics,
  VSoft.Messaging.Internal,
  Testing.Helpers,
  Testing.Messages,
  Testing.Receivers;

//posts an action that signals entered then holds the dispatcher thread until gate is set
procedure PostGate(const channel : IMessageChannel; const entered : TEvent; const gate : TEvent);
begin
  channel.Queue.PostMessage<TActionMessage>(TActionMessage.Create(
    procedure
    begin
      entered.SetEvent;
      gate.WaitFor(cWaitTimeout);
    end));
end;

{ TFlushTests }

procedure TFlushTests.Setup;
begin
  ChannelErrors.Clear;
  FBaselineDepth := TMessagingControl.GetGlobalQueueDepth;
  FChannel := TMessageChannelFactory.CreateChannel;
  FChannel.OnError := RecordChannelError;
end;

procedure TFlushTests.TearDown;
begin
  FChannel := nil;
  RestoreGlobalQueueDepth(FBaselineDepth);
end;

procedure TFlushTests.Flush_NothingPosted_ReturnsTrueStraightAway;
var
  receiver : TOwningReceiver;
begin
  receiver := TOwningReceiver.Create(FChannel);
  try
    Assert.IsTrue(FChannel.Flush(0));
  finally
    receiver.Free;
  end;
end;

procedure TFlushTests.Flush_WaitsUntilPostedMessagesHaveBeenHandled;
var
  receiver : TOwningReceiver;
  gate : TEvent;
  entered : TEvent;
  flusher : TTestThread;
  flushed : boolean;
  countAtFlush : integer;
  finishedWhileHeld : boolean;
  i : integer;
begin
  gate := TEvent.Create(nil, true, false, '');
  entered := TEvent.Create(nil, true, false, '');
  receiver := TOwningReceiver.Create(FChannel);
  flusher := nil;
  flushed := false;
  countAtFlush := -1;
  try
    PostGate(FChannel, entered, gate);
    Assert.IsTrue(entered.WaitFor(cWaitTimeout) = wrSignaled, 'the handler did not start');
    for i := 1 to 5 do
      FChannel.Queue.PostMessage<TValueMessage>(TValueMessage.Create(i));
    flusher := StartThread(
      procedure
      begin
        flushed := FChannel.Flush(cWaitTimeout);
        countAtFlush := receiver.ValueCount;
      end);
    Sleep(200);
    finishedWhileHeld := flusher.Finished;
    gate.SetEvent;
    Assert.IsTrue(FinishThread(flusher), 'the flush did not return');
    Assert.IsFalse(finishedWhileHeld, 'the flush returned while a message was still being handled');
    Assert.IsTrue(flushed);
    Assert.AreEqual(5, countAtFlush, 'the flush returned before every message posted before it was handled');
  finally
    gate.SetEvent;
    FinishThread(flusher);
    receiver.Free;
    entered.Free;
    gate.Free;
  end;
end;

procedure TFlushTests.Flush_TimesOut_ReturnsFalse;
var
  receiver : TOwningReceiver;
  gate : TEvent;
  entered : TEvent;
  stopwatch : TStopwatch;
begin
  gate := TEvent.Create(nil, true, false, '');
  entered := TEvent.Create(nil, true, false, '');
  receiver := TOwningReceiver.Create(FChannel);
  try
    PostGate(FChannel, entered, gate);
    Assert.IsTrue(entered.WaitFor(cWaitTimeout) = wrSignaled, 'the handler did not start');
    stopwatch := TStopwatch.StartNew;
    Assert.IsFalse(FChannel.Flush(200), 'the flush should time out while the message is held');
    Assert.IsTrue(stopwatch.ElapsedMilliseconds >= 150, 'the flush returned before its timeout');
  finally
    gate.SetEvent;
    receiver.Free;
    entered.Free;
    gate.Free;
  end;
end;

procedure TFlushTests.Flush_DoesNotWaitForMessagesPostedAfterIt;
var
  receiver : TOwningReceiver;
  firstGate : TEvent;
  secondGate : TEvent;
  entered : TEvent;
  flusher : TTestThread;
  flushed : boolean;
begin
  firstGate := TEvent.Create(nil, true, false, '');
  secondGate := TEvent.Create(nil, true, false, '');
  entered := TEvent.Create(nil, true, false, '');
  receiver := TOwningReceiver.Create(FChannel);
  flusher := nil;
  flushed := false;
  try
    PostGate(FChannel, entered, firstGate);
    Assert.IsTrue(entered.WaitFor(cWaitTimeout) = wrSignaled, 'the handler did not start');
    flusher := StartThread(
      procedure
      begin
        flushed := FChannel.Flush(cWaitTimeout);
      end);
    Sleep(100);
    //posted after the flush started, and held, so the flush must not wait for it
    FChannel.Queue.PostMessage<TActionMessage>(TActionMessage.Create(
      procedure
      begin
        secondGate.WaitFor(cWaitTimeout);
      end));
    firstGate.SetEvent;
    Assert.IsTrue(FinishThread(flusher, 2000), 'the flush waited for a message posted after it started');
    Assert.IsTrue(flushed);
  finally
    firstGate.SetEvent;
    secondGate.SetEvent;
    FinishThread(flusher);
    receiver.Free;
    entered.Free;
    secondGate.Free;
    firstGate.Free;
  end;
end;

procedure TFlushTests.Flush_WaitsForMessagesQueuedOnADispatcherThatUnsubscribed;
var
  receiver : TOwningReceiver;
  gate : TEvent;
  entered : TEvent;
  flusher : TTestThread;
  flushed : boolean;
  countAtFlush : integer;
  finishedWhileHeld : boolean;
begin
  gate := TEvent.Create(nil, true, false, '');
  entered := TEvent.Create(nil, true, false, '');
  receiver := TOwningReceiver.Create(FChannel);
  flusher := nil;
  flushed := false;
  countAtFlush := -1;
  try
    PostGate(FChannel, entered, gate);
    Assert.IsTrue(entered.WaitFor(cWaitTimeout) = wrSignaled, 'the handler did not start');
    FChannel.Queue.PostMessage<TValueMessage>(TValueMessage.Create(1));
    FChannel.Queue.PostMessage<TValueMessage>(TValueMessage.Create(2));
    //as FinalBuilder does at the end of a build : unsubscribe, keeping the queued messages
    receiver.Dispatcher.Channel := nil;
    flusher := StartThread(
      procedure
      begin
        flushed := FChannel.Flush(cWaitTimeout);
        countAtFlush := receiver.ValueCount;
      end);
    Sleep(200);
    finishedWhileHeld := flusher.Finished;
    gate.SetEvent;
    Assert.IsTrue(FinishThread(flusher), 'the flush did not return');
    Assert.IsFalse(finishedWhileHeld, 'the flush ignored messages queued on a dispatcher that had unsubscribed');
    Assert.IsTrue(flushed);
    Assert.AreEqual(2, countAtFlush);
  finally
    gate.SetEvent;
    FinishThread(flusher);
    receiver.Free;
    entered.Free;
    gate.Free;
  end;
end;

procedure TFlushTests.Flush_FromAHandlerOfTheChannel_ReturnsFalse;
var
  receiver : TOwningReceiver;
  flushed : boolean;
  elapsed : Int64;
begin
  flushed := true;
  elapsed := -1;
  receiver := TOwningReceiver.Create(FChannel);
  try
    FChannel.Queue.PostMessage<TActionMessage>(TActionMessage.Create(
      procedure
      var
        stopwatch : TStopwatch;
      begin
        stopwatch := TStopwatch.StartNew;
        flushed := FChannel.Flush(2000);
        elapsed := stopwatch.ElapsedMilliseconds;
      end));
    Assert.IsTrue(WaitUntil(
      function : boolean
      begin
        result := receiver.ActionCount = 1;
      end, 5000), 'the handler did not finish');
    Assert.IsFalse(flushed, 'a flush from a handler of the channel cannot succeed');
    Assert.IsTrue(elapsed < 1000, 'a flush from a handler of the channel should return straight away');
  finally
    receiver.Free;
  end;
end;

procedure TFlushTests.Flush_OnTheMainThread_HandlesUIDispatchers;
var
  receiver : TOwningReceiver;
  i : integer;
begin
  receiver := TOwningReceiver.Create(FChannel, true);
  try
    for i := 1 to 30 do
      FChannel.Queue.PostMessage<TValueMessage>(TValueMessage.Create(i));
    Assert.IsTrue(FChannel.Flush(cWaitTimeout), 'the flush timed out');
    Assert.AreEqual(30, receiver.ValueCount, 'the flush returned before the UI dispatcher handled every message');
  finally
    receiver.Free;
  end;
end;

procedure TFlushTests.WaitUntilIdle_NothingQueued_ReturnsTrue;
var
  receiver : TOwningReceiver;
begin
  receiver := TOwningReceiver.Create(FChannel);
  try
    Assert.IsTrue(receiver.Dispatcher.WaitUntilIdle(1000));
  finally
    receiver.Free;
  end;
end;

procedure TFlushTests.WaitUntilIdle_WaitsUntilQueuedMessagesHaveBeenHandled;
var
  receiver : TOwningReceiver;
  gate : TEvent;
  entered : TEvent;
  waiter : TTestThread;
  idle : boolean;
  countWhenIdle : integer;
  finishedWhileHeld : boolean;
  i : integer;
begin
  gate := TEvent.Create(nil, true, false, '');
  entered := TEvent.Create(nil, true, false, '');
  receiver := TOwningReceiver.Create(FChannel);
  waiter := nil;
  idle := false;
  countWhenIdle := -1;
  try
    PostGate(FChannel, entered, gate);
    Assert.IsTrue(entered.WaitFor(cWaitTimeout) = wrSignaled, 'the handler did not start');
    for i := 1 to 5 do
      FChannel.Queue.PostMessage<TValueMessage>(TValueMessage.Create(i));
    waiter := StartThread(
      procedure
      begin
        idle := receiver.Dispatcher.WaitUntilIdle(cWaitTimeout);
        countWhenIdle := receiver.ValueCount;
      end);
    Sleep(200);
    finishedWhileHeld := waiter.Finished;
    gate.SetEvent;
    Assert.IsTrue(FinishThread(waiter), 'WaitUntilIdle did not return');
    Assert.IsFalse(finishedWhileHeld, 'WaitUntilIdle returned while a message was still being handled');
    Assert.IsTrue(idle);
    Assert.AreEqual(5, countWhenIdle);
  finally
    gate.SetEvent;
    FinishThread(waiter);
    receiver.Free;
    entered.Free;
    gate.Free;
  end;
end;

procedure TFlushTests.WaitUntilIdle_FromItsOwnHandler_ReturnsFalse;
var
  receiver : TOwningReceiver;
  idle : boolean;
begin
  idle := true;
  receiver := TOwningReceiver.Create(FChannel);
  try
    FChannel.Queue.PostMessage<TActionMessage>(TActionMessage.Create(
      procedure
      begin
        idle := receiver.Dispatcher.WaitUntilIdle(2000);
      end));
    Assert.IsTrue(WaitUntil(
      function : boolean
      begin
        result := receiver.ActionCount = 1;
      end, 5000), 'the handler did not finish');
    Assert.IsFalse(idle, 'a dispatcher cannot become idle while one of its own handlers waits for it');
  finally
    receiver.Free;
  end;
end;

procedure TFlushTests.WaitUntilIdle_UIDispatcherOnTheMainThread_ReturnsTrue;
var
  receiver : TOwningReceiver;
  i : integer;
begin
  receiver := TOwningReceiver.Create(FChannel, true);
  try
    for i := 1 to 30 do
      FChannel.Queue.PostMessage<TValueMessage>(TValueMessage.Create(i));
    Assert.IsTrue(receiver.Dispatcher.WaitUntilIdle(cWaitTimeout), 'WaitUntilIdle timed out');
    Assert.AreEqual(30, receiver.ValueCount);
  finally
    receiver.Free;
  end;
end;

initialization
  TDUnitX.RegisterTestFixture(TFlushTests);

end.

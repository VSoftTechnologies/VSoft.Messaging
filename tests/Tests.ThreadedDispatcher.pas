unit Tests.ThreadedDispatcher;

interface

uses
  DUnitX.TestFramework,
  VSoft.Messaging;

type
  [TestFixture]
  TThreadedDispatcherTests = class
  private
    FChannel : IMessageChannel;
    FBaselineDepth : integer;
  public
    [Setup]
    procedure Setup;
    [TearDown]
    procedure TearDown;

    [Test]
    procedure Handler_RunsOnTheDispatcherThread;
    [Test]
    procedure Handler_AlwaysRunsOnTheSameThread;
    [Test]
    procedure EachDispatcher_HasItsOwnThread;
    [Test]
    procedure PostMessage_DeliversInOrder;
    [Test]
    procedure PostMessage_FromSeveralThreads_KeepsEachThreadsOrder;
    [Test]
    procedure PostMessage_MaxBurstOfOne_DeliversEverything;
    [Test]
    procedure Disabled_DoesNotReceive_OthersStillDo;
    [Test]
    procedure ReEnabled_ReceivesNewMessages;
    [Test]
    procedure NoTarget_DoesNotReceiveOrRaise;
    [Test]
    procedure TargetSetLater_Receives;
    [Test]
    procedure Channel_ReturnsTheAssignedChannel;
    [Test]
    procedure Channel_SetToNil_StopsDelivery;
    [Test]
    procedure Channel_SwitchedToAnotherChannel_MovesTheSubscription;
    [Test]
    procedure Channel_SetToTheSameChannelAgain_DeliversOnce;
    [Test]
    procedure Channel_SetToTheSameChannelAgain_KeepsTheDeliveryOrder;
    [Test]
    procedure SendMessage_DoesNotRunAlongsideAPostedMessage;
    [Test]
    procedure HandlerRaisesNonExceptionObject_DispatcherKeepsRunning;
    [Test]
    procedure OnErrorRaises_DispatcherKeepsRunning;
    [Test]
    procedure DisabledDuringABurst_RestOfTheBurstIsNotDelivered;
    [Test]
    procedure TargetClearedDuringABurst_NoErrorsAreReported;
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

//returns a proc that posts count values, each one threadIndex * 10000 + n, so the
//receiver can tell which thread sent it and in what order
function MakePoster(const channel : IMessageChannel; const threadIndex : integer; const count : integer) : TProc;
begin
  result :=
    procedure
    var
      i : integer;
    begin
      for i := 1 to count do
        channel.Queue.PostMessage<TValueMessage>(TValueMessage.Create(threadIndex * 10000 + i));
    end;
end;

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

{ TThreadedDispatcherTests }

procedure TThreadedDispatcherTests.Setup;
begin
  ChannelErrors.Clear;
  FBaselineDepth := TMessagingControl.GetGlobalQueueDepth;
  FChannel := TMessageChannelFactory.CreateChannel;
  FChannel.OnError := RecordChannelError;
end;

procedure TThreadedDispatcherTests.TearDown;
begin
  FChannel := nil;
  RestoreGlobalQueueDepth(FBaselineDepth);
end;

procedure TThreadedDispatcherTests.Handler_RunsOnTheDispatcherThread;
var
  receiver : TOwningReceiver;
  threadIds : TArray<TThreadID>;
begin
  receiver := TOwningReceiver.Create(FChannel);
  try
    FChannel.Queue.PostMessage<TValueMessage>(TValueMessage.Create(1));
    Assert.IsTrue(WaitUntil(
      function : boolean
      begin
        result := receiver.ValueCount = 1;
      end), 'the message was not delivered');
    threadIds := receiver.ThreadIds;
    Assert.IsTrue(threadIds[0] <> MainThreadID, 'the handler ran on the posting (main) thread');
  finally
    receiver.Free;
  end;
end;

procedure TThreadedDispatcherTests.Handler_AlwaysRunsOnTheSameThread;
var
  receiver : TOwningReceiver;
  threadIds : TArray<TThreadID>;
  i : integer;
begin
  receiver := TOwningReceiver.Create(FChannel);
  try
    for i := 1 to 50 do
      FChannel.Queue.PostMessage<TValueMessage>(TValueMessage.Create(i));
    Assert.IsTrue(WaitUntil(
      function : boolean
      begin
        result := receiver.ValueCount = 50;
      end), 'not every message was delivered');
    threadIds := receiver.ThreadIds;
    for i := 1 to High(threadIds) do
      Assert.IsTrue(threadIds[i] = threadIds[0], Format('message %d was handled on a different thread', [i + 1]));
  finally
    receiver.Free;
  end;
end;

procedure TThreadedDispatcherTests.EachDispatcher_HasItsOwnThread;
var
  first : TOwningReceiver;
  second : TOwningReceiver;
  firstThreads : TArray<TThreadID>;
  secondThreads : TArray<TThreadID>;
begin
  first := TOwningReceiver.Create(FChannel);
  second := TOwningReceiver.Create(FChannel);
  try
    FChannel.Queue.PostMessage<TValueMessage>(TValueMessage.Create(1));
    Assert.IsTrue(WaitUntil(
      function : boolean
      begin
        result := (first.ValueCount = 1) and (second.ValueCount = 1);
      end), 'the message was not delivered to both');
    firstThreads := first.ThreadIds;
    secondThreads := second.ThreadIds;
    Assert.IsTrue(firstThreads[0] <> secondThreads[0], 'two dispatchers shared a thread');
  finally
    second.Free;
    first.Free;
  end;
end;

procedure TThreadedDispatcherTests.PostMessage_DeliversInOrder;
const
  cCount = 500;
var
  receiver : TOwningReceiver;
  values : TArray<integer>;
  i : integer;
begin
  receiver := TOwningReceiver.Create(FChannel);
  try
    for i := 1 to cCount do
      FChannel.Queue.PostMessage<TValueMessage>(TValueMessage.Create(i));
    Assert.IsTrue(WaitUntil(
      function : boolean
      begin
        result := receiver.ValueCount = cCount;
      end), 'not every message was delivered');
    values := receiver.Values;
    for i := 0 to cCount - 1 do
      Assert.AreEqual(i + 1, values[i], Format('message %d was out of order', [i + 1]));
  finally
    receiver.Free;
  end;
end;

procedure TThreadedDispatcherTests.PostMessage_FromSeveralThreads_KeepsEachThreadsOrder;
const
  cThreadCount = 4;
  cPerThread = 250;
var
  receiver : TOwningReceiver;
  posters : array[1..cThreadCount] of TTestThread;
  lastSeen : array[1..cThreadCount] of integer;
  values : TArray<integer>;
  i : integer;
  threadIndex : integer;
  sequence : integer;
begin
  receiver := TOwningReceiver.Create(FChannel);
  try
    for i := 1 to cThreadCount do
      posters[i] := StartThread(MakePoster(FChannel, i, cPerThread));
    for i := 1 to cThreadCount do
      Assert.IsTrue(FinishThread(posters[i]), 'a posting thread did not finish');
    Assert.IsTrue(WaitUntil(
      function : boolean
      begin
        result := receiver.ValueCount = cThreadCount * cPerThread;
      end), 'not every message was delivered');
    for i := 1 to cThreadCount do
      lastSeen[i] := 0;
    values := receiver.Values;
    for i := 0 to High(values) do
    begin
      threadIndex := values[i] div 10000;
      sequence := values[i] mod 10000;
      Assert.AreEqual(lastSeen[threadIndex] + 1, sequence, Format('thread %d message %d was out of order', [threadIndex, sequence]));
      lastSeen[threadIndex] := sequence;
    end;
  finally
    receiver.Free;
  end;
end;

procedure TThreadedDispatcherTests.PostMessage_MaxBurstOfOne_DeliversEverything;
var
  receiver : TOwningReceiver;
  savedMaxBurst : integer;
  i : integer;
begin
  savedMaxBurst := TMessagingOptions.MaxBurst;
  TMessagingOptions.MaxBurst := 1;
  receiver := TOwningReceiver.Create(FChannel);
  try
    for i := 1 to 100 do
      FChannel.Queue.PostMessage<TValueMessage>(TValueMessage.Create(i));
    Assert.IsTrue(WaitUntil(
      function : boolean
      begin
        result := receiver.ValueCount = 100;
      end), 'not every message was delivered');
  finally
    receiver.Free;
    TMessagingOptions.MaxBurst := savedMaxBurst;
  end;
end;

procedure TThreadedDispatcherTests.Disabled_DoesNotReceive_OthersStillDo;
var
  disabled : TOwningReceiver;
  enabled : TOwningReceiver;
begin
  disabled := TOwningReceiver.Create(FChannel);
  enabled := TOwningReceiver.Create(FChannel);
  try
    disabled.Dispatcher.Enabled := false;
    Assert.IsFalse(disabled.Dispatcher.Enabled);
    FChannel.Queue.PostMessage<TValueMessage>(TValueMessage.Create(1));
    FChannel.Queue.SendMessage<TValueMessage>(TValueMessage.Create(2));
    Assert.IsTrue(WaitUntil(
      function : boolean
      begin
        result := enabled.ValueCount = 2;
      end), 'the enabled dispatcher did not receive both messages');
    PumpFor(50);
    Assert.AreEqual(0, disabled.ValueCount);
  finally
    enabled.Free;
    disabled.Free;
  end;
end;

procedure TThreadedDispatcherTests.ReEnabled_ReceivesNewMessages;
var
  receiver : TOwningReceiver;
  values : TArray<integer>;
begin
  receiver := TOwningReceiver.Create(FChannel);
  try
    receiver.Dispatcher.Enabled := false;
    FChannel.Queue.PostMessage<TValueMessage>(TValueMessage.Create(1));
    receiver.Dispatcher.Enabled := true;
    FChannel.Queue.PostMessage<TValueMessage>(TValueMessage.Create(2));
    Assert.IsTrue(WaitUntil(
      function : boolean
      begin
        result := receiver.ValueCount = 1;
      end), 'the message posted after re-enabling was not delivered');
    PumpFor(50);
    values := receiver.Values;
    Assert.AreEqual(1, integer(Length(values)));
    Assert.AreEqual(2, values[0]);
  finally
    receiver.Free;
  end;
end;

procedure TThreadedDispatcherTests.NoTarget_DoesNotReceiveOrRaise;
var
  dispatcher : IMessageDispatcher;
begin
  dispatcher := TMessageDispatcherFactory.CreateDispatcher;
  try
    Assert.IsNull(dispatcher.Target);
    dispatcher.Channel := FChannel;
    FChannel.Queue.PostMessage<TValueMessage>(TValueMessage.Create(1));
    FChannel.Queue.SendMessage<TValueMessage>(TValueMessage.Create(2));
    PumpFor(100);
    Assert.AreEqual(0, ChannelErrors.Count, ChannelErrors.Text);
    Assert.AreEqual(FBaselineDepth, TMessagingControl.GetGlobalQueueDepth, 'a message was queued for a dispatcher with no target');
  finally
    dispatcher.Channel := nil;
  end;
end;

procedure TThreadedDispatcherTests.TargetSetLater_Receives;
var
  dispatcher : IMessageDispatcher;
  receiver : TTestReceiver;
begin
  receiver := TTestReceiver.Create;
  dispatcher := TMessageDispatcherFactory.CreateDispatcher;
  try
    dispatcher.Channel := FChannel;
    FChannel.Queue.SendMessage<TValueMessage>(TValueMessage.Create(1));
    dispatcher.Target := receiver;
    Assert.IsTrue(dispatcher.Target = receiver);
    FChannel.Queue.SendMessage<TValueMessage>(TValueMessage.Create(2));
    FChannel.Queue.PostMessage<TValueMessage>(TValueMessage.Create(3));
    Assert.IsTrue(WaitUntil(
      function : boolean
      begin
        result := receiver.ValueCount = 2;
      end), 'messages sent after the target was set were not delivered');
  finally
    dispatcher.Channel := nil;
    dispatcher := nil;
    receiver.Free;
  end;
end;

procedure TThreadedDispatcherTests.Channel_ReturnsTheAssignedChannel;
var
  receiver : TOwningReceiver;
begin
  receiver := TOwningReceiver.Create(FChannel);
  try
    Assert.IsTrue(receiver.Dispatcher.Channel = FChannel);
    receiver.Dispatcher.Channel := nil;
    Assert.IsTrue(receiver.Dispatcher.Channel = nil);
  finally
    receiver.Free;
  end;
end;

procedure TThreadedDispatcherTests.Channel_SetToNil_StopsDelivery;
var
  receiver : TOwningReceiver;
begin
  receiver := TOwningReceiver.Create(FChannel);
  try
    receiver.Dispatcher.Channel := nil;
    FChannel.Queue.PostMessage<TValueMessage>(TValueMessage.Create(1));
    FChannel.Queue.SendMessage<TValueMessage>(TValueMessage.Create(2));
    PumpFor(100);
    Assert.AreEqual(0, receiver.ValueCount);
  finally
    receiver.Free;
  end;
end;

procedure TThreadedDispatcherTests.Channel_SwitchedToAnotherChannel_MovesTheSubscription;
var
  receiver : TOwningReceiver;
  otherChannel : IMessageChannel;
begin
  otherChannel := TMessageChannelFactory.CreateChannel;
  receiver := TOwningReceiver.Create(FChannel);
  try
    receiver.Dispatcher.Channel := otherChannel;
    FChannel.Queue.SendMessage<TValueMessage>(TValueMessage.Create(1));
    Assert.AreEqual(0, receiver.ValueCount, 'the old channel still delivered');
    otherChannel.Queue.SendMessage<TValueMessage>(TValueMessage.Create(2));
    Assert.AreEqual(1, receiver.ValueCount, 'the new channel did not deliver');
  finally
    receiver.Free;
    otherChannel := nil;
  end;
end;

procedure TThreadedDispatcherTests.Channel_SetToTheSameChannelAgain_DeliversOnce;
var
  receiver : TOwningReceiver;
begin
  receiver := TOwningReceiver.Create(FChannel);
  try
    receiver.Dispatcher.Channel := FChannel;
    FChannel.Queue.SendMessage<TValueMessage>(TValueMessage.Create(1));
    Assert.AreEqual(1, receiver.ValueCount);
  finally
    receiver.Free;
  end;
end;

procedure TThreadedDispatcherTests.Channel_SetToTheSameChannelAgain_KeepsTheDeliveryOrder;
var
  log : TTestLog;
  first : TOwningReceiver;
  second : TOwningReceiver;
begin
  log := TTestLog.Create;
  first := TOwningReceiver.Create(FChannel, false, 'first', log);
  second := TOwningReceiver.Create(FChannel, false, 'second', log);
  try
    //SendMessage dispatches to threaded dispatchers on this thread, in subscription order
    FChannel.Queue.SendMessage<TValueMessage>(TValueMessage.Create(1));
    Assert.AreEqual('first,second', log.Text, 'precondition : delivery should follow subscription order');
    log.Clear;
    first.Dispatcher.Channel := FChannel;
    FChannel.Queue.SendMessage<TValueMessage>(TValueMessage.Create(2));
    Assert.AreEqual('first,second', log.Text, 'setting the same channel again moved the dispatcher to the end of the delivery order (review L9)');
  finally
    second.Free;
    first.Free;
    log.Free;
  end;
end;

procedure TThreadedDispatcherTests.SendMessage_DoesNotRunAlongsideAPostedMessage;
var
  receiver : TOwningReceiver;
  gate : TEvent;
  entered : TEvent;
  sender : TTestThread;
  valueCountWhileBlocked : integer;
begin
  gate := TEvent.Create(nil, true, false, '');
  entered := TEvent.Create(nil, true, false, '');
  receiver := TOwningReceiver.Create(FChannel);
  sender := nil;
  try
    PostGate(FChannel, entered, gate);
    Assert.IsTrue(entered.WaitFor(cWaitTimeout) = wrSignaled, 'the posted handler did not start');
    sender := StartThread(
      procedure
      begin
        FChannel.Queue.SendMessage<TValueMessage>(TValueMessage.Create(1));
      end);
    Sleep(300);
    valueCountWhileBlocked := receiver.ValueCount;
    gate.SetEvent;
    Assert.IsTrue(FinishThread(sender), 'SendMessage did not return');
    Assert.AreEqual(1, receiver.ValueCount);
    Assert.AreEqual(0, valueCountWhileBlocked, 'SendMessage ran a handler on the sending thread while the dispatcher thread was still handling a posted message for the same target (review M1)');
  finally
    gate.SetEvent;
    FinishThread(sender);
    receiver.Free;
    entered.Free;
    gate.Free;
  end;
end;

procedure TThreadedDispatcherTests.HandlerRaisesNonExceptionObject_DispatcherKeepsRunning;
var
  receiver : TOwningReceiver;
begin
  receiver := TOwningReceiver.Create(FChannel);
  try
    FChannel.Queue.PostMessage<TRaiseObjectMessage>(TRaiseObjectMessage.Create(0));
    FChannel.Queue.PostMessage<TValueMessage>(TValueMessage.Create(1));
    Assert.IsTrue(WaitUntil(
      function : boolean
      begin
        result := receiver.ValueCount = 1;
      end, 2000), 'the dispatcher stopped delivering after a handler raised an object that does not descend from Exception (review M5)');
  finally
    receiver.Free;
  end;
end;

procedure TThreadedDispatcherTests.OnErrorRaises_DispatcherKeepsRunning;
var
  receiver : TOwningReceiver;
begin
  FChannel.OnError := RaisingChannelError;
  receiver := TOwningReceiver.Create(FChannel);
  try
    FChannel.Queue.PostMessage<TRaiseMessage>(TRaiseMessage.Create('boom'));
    FChannel.Queue.PostMessage<TValueMessage>(TValueMessage.Create(1));
    Assert.IsTrue(WaitUntil(
      function : boolean
      begin
        result := receiver.ValueCount = 1;
      end, 2000), 'the dispatcher stopped delivering after the OnError handler raised (review M5)');
    Assert.AreEqual(1, ChannelErrors.Count);
  finally
    receiver.Free;
  end;
end;

procedure TThreadedDispatcherTests.DisabledDuringABurst_RestOfTheBurstIsNotDelivered;
var
  receiver : TOwningReceiver;
  gate : TEvent;
  entered : TEvent;
  disabledByHandler : boolean;
begin
  gate := TEvent.Create(nil, true, false, '');
  entered := TEvent.Create(nil, true, false, '');
  receiver := TOwningReceiver.Create(FChannel);
  try
    //hold the dispatcher thread so everything below is queued and dequeued as one burst
    PostGate(FChannel, entered, gate);
    Assert.IsTrue(entered.WaitFor(cWaitTimeout) = wrSignaled, 'the first handler did not start');
    disabledByHandler := false;
    FChannel.Queue.PostMessage<TActionMessage>(TActionMessage.Create(
      procedure
      begin
        receiver.Dispatcher.Enabled := false;
        disabledByHandler := true;
      end));
    FChannel.Queue.PostMessage<TValueMessage>(TValueMessage.Create(1));
    FChannel.Queue.PostMessage<TValueMessage>(TValueMessage.Create(2));
    FChannel.Queue.PostMessage<TValueMessage>(TValueMessage.Create(3));
    gate.SetEvent;
    Assert.IsTrue(WaitUntil(
      function : boolean
      begin
        result := disabledByHandler;
      end), 'the disabling handler did not run');
    PumpFor(200);
    Assert.AreEqual(0, receiver.ValueCount, 'messages were delivered after the dispatcher was disabled, because they were in the same burst (review M6)');
  finally
    gate.SetEvent;
    receiver.Free;
    entered.Free;
    gate.Free;
  end;
end;

procedure TThreadedDispatcherTests.TargetClearedDuringABurst_NoErrorsAreReported;
var
  receiver : TOwningReceiver;
  gate : TEvent;
  entered : TEvent;
  targetCleared : boolean;
begin
  gate := TEvent.Create(nil, true, false, '');
  entered := TEvent.Create(nil, true, false, '');
  receiver := TOwningReceiver.Create(FChannel);
  try
    PostGate(FChannel, entered, gate);
    Assert.IsTrue(entered.WaitFor(cWaitTimeout) = wrSignaled, 'the first handler did not start');
    targetCleared := false;
    FChannel.Queue.PostMessage<TActionMessage>(TActionMessage.Create(
      procedure
      begin
        receiver.Dispatcher.Target := nil;
        targetCleared := true;
      end));
    FChannel.Queue.PostMessage<TValueMessage>(TValueMessage.Create(1));
    FChannel.Queue.PostMessage<TValueMessage>(TValueMessage.Create(2));
    FChannel.Queue.PostMessage<TValueMessage>(TValueMessage.Create(3));
    gate.SetEvent;
    Assert.IsTrue(WaitUntil(
      function : boolean
      begin
        result := targetCleared;
      end), 'the handler that clears the target did not run');
    PumpFor(200);
    Assert.AreEqual(0, ChannelErrors.Count, 'dispatching the rest of the burst to a nil target raised errors : ' + ChannelErrors.Text + ' (review M6)');
  finally
    gate.SetEvent;
    receiver.Free;
    entered.Free;
    gate.Free;
  end;
end;

initialization
  TDUnitX.RegisterTestFixture(TThreadedDispatcherTests);

end.

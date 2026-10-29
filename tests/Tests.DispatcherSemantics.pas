unit Tests.DispatcherSemantics;

interface

uses
  DUnitX.TestFramework,
  VSoft.Messaging;

type
  //What Enabled, Target and Channel do to queued and running messages, and other dispatcher behaviour.
  [TestFixture]
  TDispatcherSemanticsTests = class
  private
    FChannel : IMessageChannel;
    FBaselineDepth : integer;
    FBaselinePayloads : integer;
  public
    [Setup]
    procedure Setup;
    [TearDown]
    procedure TearDown;

    [Test]
    procedure Disable_DoesNotWaitForARunningHandler;
    [Test]
    procedure Disable_DiscardsQueuedMessagesAndGivesBackTheirAccounting;
    [Test]
    procedure ReEnable_OnlyNewPostsAreDelivered;
    [Test]
    procedure ClearTarget_WaitsForARunningHandler;
    [Test]
    procedure ClearTarget_DiscardsQueuedMessages;
    [Test]
    procedure ClearChannelInsideAHandler_QueuedMessagesAreStillDelivered;
    [Test]
    procedure ClearChannelInsideAUIHandler_QueuedMessagesAreStillDelivered;
    [Test]
    procedure TargetThatOverridesDispatch_ReceivesEveryMessage;
    [Test]
    procedure PendingCount_CountsQueuedAndRunningMessages;
    [Test]
    procedure SubscribedThroughTheChannel_ReleasingTheDispatcherFreesIt;
    [Test]
    procedure MessageIdOutOfRange_RaisesAssertion;
    [Test]
    procedure FilterIdOutOfRange_RaisesAssertion;
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

type
  //counts every call to Dispatch, then dispatches as normal
  TDispatchCountingReceiver = class(TTestReceiver)
  private
    FDispatchCount : integer;
  public
    procedure Dispatch(var message); override;
    function DispatchCount : integer;
  end;

procedure TDispatchCountingReceiver.Dispatch(var message);
begin
  TInterlocked.Increment(FDispatchCount);
  inherited;
end;

function TDispatchCountingReceiver.DispatchCount : integer;
begin
  result := TInterlocked.CompareExchange(FDispatchCount, 0, 0);
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

procedure PostPayload(const channel : IMessageChannel);
var
  payload : IInterface;
  msg : TPayloadMessage;
begin
  payload := TTrackedPayload.Create;
  msg := TPayloadMessage.Create(payload);
  channel.Queue.PostMessage<TPayloadMessage>(msg);
end;

function SubscribeThroughTheChannel(const channel : IMessageChannel; const target : TObject) : Pointer;
var
  dispatcher : IMessageDispatcher;
begin
  dispatcher := TTrackedDispatcher.Create(target);
  channel.Subscribe(dispatcher);
  result := Pointer(dispatcher);
end;

{ TDispatcherSemanticsTests }

procedure TDispatcherSemanticsTests.Setup;
begin
  ChannelErrors.Clear;
  FBaselineDepth := TMessagingControl.GetGlobalQueueDepth;
  FBaselinePayloads := TTrackedPayload.LiveCount;
  FChannel := TMessageChannelFactory.CreateChannel;
  FChannel.OnError := RecordChannelError;
end;

procedure TDispatcherSemanticsTests.TearDown;
begin
  FChannel := nil;
  RestoreGlobalQueueDepth(FBaselineDepth);
end;

procedure TDispatcherSemanticsTests.Disable_DoesNotWaitForARunningHandler;
var
  receiver : TOwningReceiver;
  gate : TEvent;
  entered : TEvent;
  disabler : TTestThread;
begin
  gate := TEvent.Create(nil, true, false, '');
  entered := TEvent.Create(nil, true, false, '');
  receiver := TOwningReceiver.Create(FChannel);
  disabler := nil;
  try
    PostGate(FChannel, entered, gate);
    Assert.IsTrue(entered.WaitFor(cWaitTimeout) = wrSignaled, 'the handler did not start');
    disabler := StartThread(
      procedure
      begin
        receiver.Dispatcher.Enabled := false;
      end);
    //the handler is still held, so a disable that waited for it would not finish
    Assert.IsTrue(FinishThread(disabler, 2000), 'Enabled := false waited for the running handler');
    Assert.IsFalse(receiver.Dispatcher.Enabled);
  finally
    gate.SetEvent;
    FinishThread(disabler);
    receiver.Free;
    entered.Free;
    gate.Free;
  end;
end;

procedure TDispatcherSemanticsTests.Disable_DiscardsQueuedMessagesAndGivesBackTheirAccounting;
var
  receiver : TOwningReceiver;
  gate : TEvent;
  entered : TEvent;
  i : integer;
begin
  gate := TEvent.Create(nil, true, false, '');
  entered := TEvent.Create(nil, true, false, '');
  receiver := TOwningReceiver.Create(FChannel);
  try
    PostGate(FChannel, entered, gate);
    Assert.IsTrue(entered.WaitFor(cWaitTimeout) = wrSignaled, 'the handler did not start');
    for i := 1 to 3 do
      PostPayload(FChannel);
    Assert.AreEqual(FBaselineDepth + 4, TMessagingControl.GetGlobalQueueDepth, 'precondition : three queued and one running');
    receiver.Dispatcher.Enabled := false;
    //the queued messages are released straight away, the running one when its handler returns
    Assert.AreEqual(FBaselinePayloads, TTrackedPayload.LiveCount, 'the payloads of discarded messages were not released');
    Assert.AreEqual(FBaselineDepth + 1, TMessagingControl.GetGlobalQueueDepth, 'discarded messages were not taken off the depth');
    gate.SetEvent;
    Assert.IsTrue(WaitUntil(
      function : boolean
      begin
        result := TMessagingControl.GetGlobalQueueDepth = FBaselineDepth;
      end), 'the running message was not taken off the depth');
    Assert.AreEqual(0, receiver.PayloadCount, 'a discarded message was delivered');
  finally
    gate.SetEvent;
    receiver.Free;
    entered.Free;
    gate.Free;
  end;
end;

procedure TDispatcherSemanticsTests.ReEnable_OnlyNewPostsAreDelivered;
var
  receiver : TOwningReceiver;
  gate : TEvent;
  entered : TEvent;
  values : TArray<integer>;
begin
  gate := TEvent.Create(nil, true, false, '');
  entered := TEvent.Create(nil, true, false, '');
  receiver := TOwningReceiver.Create(FChannel);
  try
    PostGate(FChannel, entered, gate);
    Assert.IsTrue(entered.WaitFor(cWaitTimeout) = wrSignaled, 'the handler did not start');
    FChannel.Queue.PostMessage<TValueMessage>(TValueMessage.Create(1));
    receiver.Dispatcher.Enabled := false;
    receiver.Dispatcher.Enabled := true;
    FChannel.Queue.PostMessage<TValueMessage>(TValueMessage.Create(2));
    gate.SetEvent;
    Assert.IsTrue(WaitUntil(
      function : boolean
      begin
        result := receiver.ValueCount = 1;
      end), 'the message posted after re-enabling was not delivered');
    PumpFor(100);
    values := receiver.Values;
    Assert.AreEqual(1, integer(Length(values)), 'a message discarded by disabling was delivered after re-enabling');
    Assert.AreEqual(2, values[0]);
  finally
    gate.SetEvent;
    receiver.Free;
    entered.Free;
    gate.Free;
  end;
end;

procedure TDispatcherSemanticsTests.ClearTarget_WaitsForARunningHandler;
var
  receiver : TOwningReceiver;
  gate : TEvent;
  entered : TEvent;
  clearer : TTestThread;
  clearedWhileRunning : boolean;
begin
  gate := TEvent.Create(nil, true, false, '');
  entered := TEvent.Create(nil, true, false, '');
  receiver := TOwningReceiver.Create(FChannel);
  clearer := nil;
  try
    PostGate(FChannel, entered, gate);
    Assert.IsTrue(entered.WaitFor(cWaitTimeout) = wrSignaled, 'the handler did not start');
    clearer := StartThread(
      procedure
      begin
        receiver.Dispatcher.Target := nil;
      end);
    Sleep(300);
    clearedWhileRunning := clearer.Finished;
    gate.SetEvent;
    Assert.IsTrue(FinishThread(clearer), 'Target := nil did not return once the handler finished');
    Assert.IsFalse(clearedWhileRunning, 'Target := nil returned while the old target''s handler was still running');
    Assert.IsTrue(receiver.Dispatcher.Target = nil);
  finally
    gate.SetEvent;
    FinishThread(clearer);
    receiver.Free;
    entered.Free;
    gate.Free;
  end;
end;

procedure TDispatcherSemanticsTests.ClearTarget_DiscardsQueuedMessages;
var
  receiver : TOwningReceiver;
  gate : TEvent;
  entered : TEvent;
  clearer : TTestThread;
  i : integer;
begin
  gate := TEvent.Create(nil, true, false, '');
  entered := TEvent.Create(nil, true, false, '');
  receiver := TOwningReceiver.Create(FChannel);
  clearer := nil;
  try
    PostGate(FChannel, entered, gate);
    Assert.IsTrue(entered.WaitFor(cWaitTimeout) = wrSignaled, 'the handler did not start');
    for i := 1 to 3 do
      PostPayload(FChannel);
    clearer := StartThread(
      procedure
      begin
        receiver.Dispatcher.Target := nil;
      end);
    //discarded before it waits for the running handler
    Assert.IsTrue(WaitUntil(
      function : boolean
      begin
        result := TTrackedPayload.LiveCount = FBaselinePayloads;
      end, 2000), 'the payloads of discarded messages were not released');
    gate.SetEvent;
    Assert.IsTrue(FinishThread(clearer), 'Target := nil did not return');
    Assert.IsTrue(WaitUntil(
      function : boolean
      begin
        result := TMessagingControl.GetGlobalQueueDepth = FBaselineDepth;
      end), 'discarded messages were not taken off the depth');
    Assert.AreEqual(0, receiver.PayloadCount, 'a discarded message was delivered');
  finally
    gate.SetEvent;
    FinishThread(clearer);
    receiver.Free;
    entered.Free;
    gate.Free;
  end;
end;

procedure TDispatcherSemanticsTests.ClearChannelInsideAHandler_QueuedMessagesAreStillDelivered;
var
  receiver : TOwningReceiver;
  gate : TEvent;
  entered : TEvent;
  cleared : boolean;
  values : TArray<integer>;
begin
  gate := TEvent.Create(nil, true, false, '');
  entered := TEvent.Create(nil, true, false, '');
  receiver := TOwningReceiver.Create(FChannel);
  cleared := false;
  try
    PostGate(FChannel, entered, gate);
    Assert.IsTrue(entered.WaitFor(cWaitTimeout) = wrSignaled, 'the handler did not start');
    //as FinalBuilder does at the end of a build, with messages still queued behind it
    FChannel.Queue.PostMessage<TActionMessage>(TActionMessage.Create(
      procedure
      begin
        receiver.Dispatcher.Channel := nil;
        cleared := true;
      end));
    FChannel.Queue.PostMessage<TValueMessage>(TValueMessage.Create(1));
    FChannel.Queue.PostMessage<TValueMessage>(TValueMessage.Create(2));
    gate.SetEvent;
    Assert.IsTrue(WaitUntil(
      function : boolean
      begin
        result := receiver.ValueCount = 2;
      end), 'messages queued before the channel was cleared were not delivered');
    Assert.IsTrue(cleared);
    FChannel.Queue.PostMessage<TValueMessage>(TValueMessage.Create(3));
    PumpFor(100);
    values := receiver.Values;
    Assert.AreEqual(2, integer(Length(values)), 'a message posted after the channel was cleared was delivered');
  finally
    gate.SetEvent;
    receiver.Free;
    entered.Free;
    gate.Free;
  end;
end;

procedure TDispatcherSemanticsTests.ClearChannelInsideAUIHandler_QueuedMessagesAreStillDelivered;
var
  receiver : TOwningReceiver;
  values : TArray<integer>;
begin
  receiver := TOwningReceiver.Create(FChannel, true);
  try
    //nothing is delivered until this thread processes synchronize calls, so all of these are queued
    FChannel.Queue.PostMessage<TActionMessage>(TActionMessage.Create(
      procedure
      begin
        receiver.Dispatcher.Channel := nil;
      end));
    FChannel.Queue.PostMessage<TValueMessage>(TValueMessage.Create(1));
    FChannel.Queue.PostMessage<TValueMessage>(TValueMessage.Create(2));
    Assert.IsTrue(WaitUntil(
      function : boolean
      begin
        result := receiver.ValueCount = 2;
      end), 'messages queued before the channel was cleared were not delivered');
    values := receiver.Values;
    Assert.AreEqual(1, values[0]);
    Assert.AreEqual(2, values[1]);
  finally
    receiver.Free;
  end;
end;

procedure TDispatcherSemanticsTests.TargetThatOverridesDispatch_ReceivesEveryMessage;
var
  receiver : TDispatchCountingReceiver;
  dispatcher : IMessageDispatcher;
begin
  receiver := TDispatchCountingReceiver.Create;
  dispatcher := TMessageDispatcherFactory.CreateDispatcher(receiver);
  try
    dispatcher.Channel := FChannel;
    FChannel.Queue.PostMessage<TValueMessage>(TValueMessage.Create(1));
    //a target that overrides Dispatch sees messages it has no handler for too
    FChannel.Queue.PostMessage<TUnhandledMessage>(TUnhandledMessage.Create(2));
    Assert.IsTrue(WaitUntil(
      function : boolean
      begin
        result := receiver.DispatchCount = 2;
      end), 'Dispatch was not called for every message');
    Assert.AreEqual(1, receiver.ValueCount, 'the overridden Dispatch did not reach the message handler');
  finally
    dispatcher.Channel := nil;
    dispatcher := nil;
    receiver.Free;
  end;
end;

procedure TDispatcherSemanticsTests.PendingCount_CountsQueuedAndRunningMessages;
var
  receiver : TOwningReceiver;
  gate : TEvent;
  entered : TEvent;
  i : integer;
begin
  gate := TEvent.Create(nil, true, false, '');
  entered := TEvent.Create(nil, true, false, '');
  receiver := TOwningReceiver.Create(FChannel);
  try
    Assert.AreEqual(0, receiver.Dispatcher.PendingCount);
    PostGate(FChannel, entered, gate);
    Assert.IsTrue(entered.WaitFor(cWaitTimeout) = wrSignaled, 'the handler did not start');
    for i := 1 to 3 do
      FChannel.Queue.PostMessage<TValueMessage>(TValueMessage.Create(i));
    Assert.AreEqual(4, receiver.Dispatcher.PendingCount, 'three queued and one running');
    gate.SetEvent;
    Assert.IsTrue(WaitUntil(
      function : boolean
      begin
        result := receiver.Dispatcher.PendingCount = 0;
      end), 'PendingCount did not return to 0');
  finally
    gate.SetEvent;
    receiver.Free;
    entered.Free;
    gate.Free;
  end;
end;

procedure TDispatcherSemanticsTests.SubscribedThroughTheChannel_ReleasingTheDispatcherFreesIt;
var
  receiver : TTestReceiver;
  baseline : integer;
begin
  baseline := TTrackedDispatcher.LiveCount;
  receiver := TTestReceiver.Create;
  try
    //the dispatcher's Channel is never set, the channel only knows it through Subscribe
    SubscribeThroughTheChannel(FChannel, receiver);
    Assert.AreEqual(baseline, TTrackedDispatcher.LiveCount, 'a dispatcher subscribed through the channel was kept alive by it');
    FChannel.Queue.PostMessage<TValueMessage>(TValueMessage.Create(1));
    PumpFor(100);
    Assert.AreEqual(0, receiver.ValueCount, 'a released dispatcher still received messages');
  finally
    receiver.Free;
  end;
end;

procedure TDispatcherSemanticsTests.MessageIdOutOfRange_RaisesAssertion;
var
  msg : TValueMessage;
begin
  msg := TValueMessage.Create(1);
  msg.MsgID := $C000;
  Assert.WillRaise(
    procedure
    begin
      FChannel.Queue.PostMessage<TValueMessage>(msg);
    end, EAssertionFailed);
  msg.MsgID := 0;
  Assert.WillRaise(
    procedure
    begin
      FChannel.Queue.SendMessage<TValueMessage>(msg);
    end, EAssertionFailed);
end;

procedure TDispatcherSemanticsTests.FilterIdOutOfRange_RaisesAssertion;
var
  dispatcher : IMessageDispatcher;
begin
  dispatcher := TMessageDispatcherFactory.CreateDispatcher;
  Assert.WillRaise(
    procedure
    begin
      dispatcher.IncludeFilter := MessageIds([$C000]);
    end, EAssertionFailed);
  Assert.WillRaise(
    procedure
    begin
      dispatcher.ExcludeFilter := MessageIds([0]);
    end, EAssertionFailed);
end;

initialization
  TDUnitX.RegisterTestFixture(TDispatcherSemanticsTests);

end.

unit Tests.Lifetime;

interface

uses
  DUnitX.TestFramework,
  VSoft.Messaging;

type
  //Leaks and object lifetime. Messages and dispatchers are created in separate routines so
  //compiler temporaries cannot keep references alive until the end of a test method.
  [TestFixture]
  TLifetimeTests = class
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
    procedure PostedMessage_PayloadIsReleasedAfterDelivery;
    [Test]
    procedure SentMessage_PayloadIsReleasedAfterDelivery;
    [Test]
    procedure PostedMessage_ChannelDisabled_PayloadIsReleased;
    [Test]
    procedure PostedMessage_NoDispatchers_PayloadIsReleased;
    [Test]
    procedure PostedMessage_FilteredOut_PayloadIsReleased;
    [Test]
    procedure PostedMessage_SeveralDispatchers_PayloadIsReleasedOnce;
    [Test]
    procedure DestroyDispatcher_WithQueuedMessages_ReleasesTheirPayloads;
    [Test]
    procedure DestroyReceiver_WhileItsHandlerRuns_WaitsForTheHandler;
    [Test]
    procedure DestroyReceiver_DocumentedTeardown_NothingIsDeliveredAfterwards;
    [Test]
    procedure DocumentedTeardown_FreesTheDispatcherAndTheChannel;
    [Test]
    procedure ReleaseDispatcher_WithoutClearingItsChannel_DispatcherIsFreed;
    [Test]
    procedure ReleaseChannelAndDispatcher_WithoutClearingTheChannel_BothAreFreed;
    [Test]
    procedure DestroyReceiver_DuringSendMessage_WaitsForTheHandler;
    [Test]
    procedure HandlerFreesItsOwnReceiver_DoesNotHang;
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

procedure PostPayload(const channel : IMessageChannel);
var
  payload : IInterface;
  msg : TPayloadMessage;
begin
  payload := TTrackedPayload.Create;
  msg := TPayloadMessage.Create(payload);
  channel.Queue.PostMessage<TPayloadMessage>(msg);
end;

procedure SendPayload(const channel : IMessageChannel);
var
  payload : IInterface;
  msg : TPayloadMessage;
begin
  payload := TTrackedPayload.Create;
  msg := TPayloadMessage.Create(payload);
  channel.Queue.SendMessage<TPayloadMessage>(msg);
end;

procedure PostGate(const channel : IMessageChannel; const entered : TEvent; const gate : TEvent; const finished : TEvent);
begin
  channel.Queue.PostMessage<TActionMessage>(TActionMessage.Create(
    procedure
    begin
      entered.SetEvent;
      gate.WaitFor(cWaitTimeout);
      finished.SetEvent;
    end));
end;

//creates a tracked dispatcher subscribed to channel and drops the local reference, as the
//README example does. Returns the dispatcher as a raw pointer so the test can clean it up.
function SubscribeAndForgetDispatcher(const channel : IMessageChannel; const target : TObject) : Pointer;
var
  dispatcher : IMessageDispatcher;
begin
  dispatcher := TTrackedDispatcher.Create(target);
  dispatcher.Channel := channel;
  result := Pointer(dispatcher);
end;

//creates a tracked channel with a subscribed tracked dispatcher and drops both references
//without clearing the dispatcher's channel. Returns the dispatcher as a raw pointer.
function CreateAndForgetChannelAndDispatcher(const target : TObject) : Pointer;
var
  channel : IMessageChannel;
begin
  channel := TTrackedChannel.Create;
  result := SubscribeAndForgetDispatcher(channel, target);
end;

//the documented teardown : clear the channel, then release
procedure CreateAndTearDownChannelAndDispatcher(const target : TObject);
var
  channel : IMessageChannel;
  dispatcher : IMessageDispatcher;
begin
  channel := TTrackedChannel.Create;
  dispatcher := TTrackedDispatcher.Create(target);
  dispatcher.Channel := channel;
  channel.Queue.SendMessage<TValueMessage>(TValueMessage.Create(1));
  dispatcher.Channel := nil;
  dispatcher := nil;
  channel := nil;
end;

//unsubscribes and releases a dispatcher the test only has a raw pointer to
procedure ReleaseForgottenDispatcher(const dispatcherRef : Pointer);
var
  dispatcher : IMessageDispatcher;
begin
  dispatcher := IMessageDispatcher(dispatcherRef);
  dispatcher.Channel := nil;
  dispatcher := nil;
end;

{ TLifetimeTests }

procedure TLifetimeTests.Setup;
begin
  ChannelErrors.Clear;
  FBaselineDepth := TMessagingControl.GetGlobalQueueDepth;
  FBaselinePayloads := TTrackedPayload.LiveCount;
  FChannel := TMessageChannelFactory.CreateChannel;
  FChannel.OnError := RecordChannelError;
end;

procedure TLifetimeTests.TearDown;
begin
  FChannel := nil;
  RestoreGlobalQueueDepth(FBaselineDepth);
end;

procedure TLifetimeTests.PostedMessage_PayloadIsReleasedAfterDelivery;
var
  receiver : TOwningReceiver;
  baseline : integer;
begin
  baseline := FBaselinePayloads;
  receiver := TOwningReceiver.Create(FChannel);
  try
    PostPayload(FChannel);
    Assert.IsTrue(WaitUntil(
      function : boolean
      begin
        result := receiver.PayloadCount = 1;
      end), 'the message was not delivered');
    Assert.IsTrue(WaitUntil(
      function : boolean
      begin
        result := TTrackedPayload.LiveCount = baseline;
      end), 'the payload of a delivered message was not released');
  finally
    receiver.Free;
  end;
end;

procedure TLifetimeTests.SentMessage_PayloadIsReleasedAfterDelivery;
var
  receiver : TOwningReceiver;
begin
  receiver := TOwningReceiver.Create(FChannel);
  try
    SendPayload(FChannel);
    Assert.AreEqual(1, receiver.PayloadCount);
    Assert.AreEqual(FBaselinePayloads, TTrackedPayload.LiveCount, 'the payload of a sent message was not released');
  finally
    receiver.Free;
  end;
end;

procedure TLifetimeTests.PostedMessage_ChannelDisabled_PayloadIsReleased;
var
  receiver : TOwningReceiver;
begin
  receiver := TOwningReceiver.Create(FChannel);
  try
    FChannel.Enabled := false;
    PostPayload(FChannel);
    Assert.AreEqual(FBaselinePayloads, TTrackedPayload.LiveCount, 'the payload of a dropped message was not released');
  finally
    receiver.Free;
  end;
end;

procedure TLifetimeTests.PostedMessage_NoDispatchers_PayloadIsReleased;
begin
  PostPayload(FChannel);
  SendPayload(FChannel);
  Assert.AreEqual(FBaselinePayloads, TTrackedPayload.LiveCount, 'the payload of a message nobody received was not released');
end;

procedure TLifetimeTests.PostedMessage_FilteredOut_PayloadIsReleased;
var
  receiver : TOwningReceiver;
begin
  receiver := TOwningReceiver.Create(FChannel);
  try
    receiver.Dispatcher.ExcludeFilter := MessageIds([TEST_MSG_PAYLOAD]);
    PostPayload(FChannel);
    Assert.AreEqual(FBaselinePayloads, TTrackedPayload.LiveCount, 'the payload of a filtered message was not released');
    Assert.AreEqual(0, receiver.PayloadCount);
  finally
    receiver.Free;
  end;
end;

procedure TLifetimeTests.PostedMessage_SeveralDispatchers_PayloadIsReleasedOnce;
var
  first : TOwningReceiver;
  second : TOwningReceiver;
  third : TOwningReceiver;
  baseline : integer;
begin
  baseline := FBaselinePayloads;
  first := TOwningReceiver.Create(FChannel);
  second := TOwningReceiver.Create(FChannel, true);
  third := TOwningReceiver.Create(FChannel);
  try
    PostPayload(FChannel);
    Assert.IsTrue(WaitUntil(
      function : boolean
      begin
        result := (first.PayloadCount = 1) and (second.PayloadCount = 1) and (third.PayloadCount = 1);
      end), 'not every dispatcher received the message');
    Assert.IsTrue(WaitUntil(
      function : boolean
      begin
        result := TTrackedPayload.LiveCount = baseline;
      end), 'the payload was not released once every dispatcher had handled it');
  finally
    third.Free;
    second.Free;
    first.Free;
  end;
end;

procedure TLifetimeTests.DestroyDispatcher_WithQueuedMessages_ReleasesTheirPayloads;
var
  receiver : TOwningReceiver;
  gate : TEvent;
  entered : TEvent;
  finished : TEvent;
  i : integer;
begin
  gate := TEvent.Create(nil, true, false, '');
  entered := TEvent.Create(nil, true, false, '');
  finished := TEvent.Create(nil, true, false, '');
  receiver := TOwningReceiver.Create(FChannel);
  try
    PostGate(FChannel, entered, gate, finished);
    Assert.IsTrue(entered.WaitFor(cWaitTimeout) = wrSignaled, 'the first handler did not start');
    for i := 1 to 5 do
      PostPayload(FChannel);
    Assert.AreEqual(FBaselinePayloads + 5, TTrackedPayload.LiveCount, 'precondition : the payloads should be queued');
    //disable so the queued messages stay queued once the first handler returns
    receiver.Dispatcher.Enabled := false;
    gate.SetEvent;
    Assert.IsTrue(finished.WaitFor(cWaitTimeout) = wrSignaled, 'the first handler did not finish');
  finally
    gate.SetEvent;
    receiver.Free;
    finished.Free;
    entered.Free;
    gate.Free;
  end;
  Assert.AreEqual(FBaselinePayloads, TTrackedPayload.LiveCount, 'payloads of messages still queued when the dispatcher was destroyed were not released');
end;

procedure TLifetimeTests.DestroyReceiver_WhileItsHandlerRuns_WaitsForTheHandler;
var
  receiver : TOwningReceiver;
  gate : TEvent;
  entered : TEvent;
  finished : TEvent;
  destroyer : TTestThread;
  destroyerStarted : boolean;
  destroyed : boolean;
  destroyedWhileHandlerRan : boolean;
begin
  gate := TEvent.Create(nil, true, false, '');
  entered := TEvent.Create(nil, true, false, '');
  finished := TEvent.Create(nil, true, false, '');
  receiver := TOwningReceiver.Create(FChannel);
  destroyer := nil;
  destroyerStarted := false;
  destroyed := false;
  try
    receiver.OnDestroyed :=
      procedure
      begin
        destroyed := true;
      end;
    PostGate(FChannel, entered, gate, finished);
    Assert.IsTrue(entered.WaitFor(cWaitTimeout) = wrSignaled, 'the handler did not start');
    destroyerStarted := true;
    destroyer := StartThread(
      procedure
      begin
        receiver.Free;
      end);
    Sleep(300);
    destroyedWhileHandlerRan := destroyed;
    gate.SetEvent;
    Assert.IsTrue(FinishThread(destroyer), 'destroying the receiver did not finish');
    Assert.IsFalse(destroyedWhileHandlerRan, 'the receiver was destroyed while its handler was still running');
    Assert.IsTrue(destroyed);
  finally
    gate.SetEvent;
    if destroyerStarted then
      FinishThread(destroyer)
    else
      receiver.Free;
    finished.Free;
    entered.Free;
    gate.Free;
  end;
end;

procedure TLifetimeTests.DestroyReceiver_DocumentedTeardown_NothingIsDeliveredAfterwards;
var
  receiver : TOwningReceiver;
  log : TTestLog;
  i : integer;
begin
  log := TTestLog.Create;
  try
    receiver := TOwningReceiver.Create(FChannel, false, 'receiver', log);
    for i := 1 to 20 do
      FChannel.Queue.PostMessage<TValueMessage>(TValueMessage.Create(i));
    receiver.Free;
    log.Clear;
    for i := 1 to 20 do
      FChannel.Queue.PostMessage<TValueMessage>(TValueMessage.Create(i));
    FChannel.Queue.SendMessage<TValueMessage>(TValueMessage.Create(0));
    PumpFor(100);
    Assert.AreEqual(0, log.Count, 'a message was delivered to a receiver that had been destroyed');
    Assert.AreEqual(0, ChannelErrors.Count, ChannelErrors.Text);
  finally
    log.Free;
  end;
end;

procedure TLifetimeTests.DocumentedTeardown_FreesTheDispatcherAndTheChannel;
var
  receiver : TTestReceiver;
  channelBaseline : integer;
  dispatcherBaseline : integer;
begin
  channelBaseline := TTrackedChannel.LiveCount;
  dispatcherBaseline := TTrackedDispatcher.LiveCount;
  receiver := TTestReceiver.Create;
  try
    CreateAndTearDownChannelAndDispatcher(receiver);
    Assert.AreEqual(1, receiver.ValueCount, 'precondition : the message should be delivered');
    Assert.AreEqual(dispatcherBaseline, TTrackedDispatcher.LiveCount, 'the dispatcher was not freed');
    Assert.AreEqual(channelBaseline, TTrackedChannel.LiveCount, 'the channel was not freed');
  finally
    receiver.Free;
  end;
end;

procedure TLifetimeTests.ReleaseDispatcher_WithoutClearingItsChannel_DispatcherIsFreed;
var
  receiver : TTestReceiver;
  dispatcherBaseline : integer;
  dispatcherRef : Pointer;
  leaked : boolean;
begin
  dispatcherBaseline := TTrackedDispatcher.LiveCount;
  receiver := TTestReceiver.Create;
  try
    dispatcherRef := SubscribeAndForgetDispatcher(FChannel, receiver);
    leaked := TTrackedDispatcher.LiveCount <> dispatcherBaseline;
    //stop the leaked dispatcher pointing at the receiver before it is freed
    if leaked then
      ReleaseForgottenDispatcher(dispatcherRef);
    Assert.IsFalse(leaked, 'a dispatcher released without clearing its Channel was not freed, the channel and dispatcher keep each other alive (review H2)');
  finally
    receiver.Free;
  end;
end;

procedure TLifetimeTests.ReleaseChannelAndDispatcher_WithoutClearingTheChannel_BothAreFreed;
var
  receiver : TTestReceiver;
  channelBaseline : integer;
  dispatcherBaseline : integer;
  dispatcherRef : Pointer;
  dispatcherLeaked : boolean;
  channelLeaked : boolean;
begin
  channelBaseline := TTrackedChannel.LiveCount;
  dispatcherBaseline := TTrackedDispatcher.LiveCount;
  receiver := TTestReceiver.Create;
  try
    dispatcherRef := CreateAndForgetChannelAndDispatcher(receiver);
    dispatcherLeaked := TTrackedDispatcher.LiveCount <> dispatcherBaseline;
    channelLeaked := TTrackedChannel.LiveCount <> channelBaseline;
    //releasing the dispatcher's channel breaks the cycle, which frees both
    if dispatcherLeaked then
      ReleaseForgottenDispatcher(dispatcherRef);
    Assert.IsFalse(dispatcherLeaked or channelLeaked, 'releasing every reference to a channel and its dispatcher, without clearing Channel first, freed neither (review H2)');
    Assert.AreEqual(channelBaseline, TTrackedChannel.LiveCount, 'cleanup : the channel was not freed');
  finally
    receiver.Free;
  end;
end;

procedure TLifetimeTests.DestroyReceiver_DuringSendMessage_WaitsForTheHandler;
var
  receiver : TOwningReceiver;
  gate : TEvent;
  entered : TEvent;
  sender : TTestThread;
  destroyer : TTestThread;
  destroyerStarted : boolean;
  destroyed : boolean;
  destroyedWhileHandlerRan : boolean;
begin
  gate := TEvent.Create(nil, true, false, '');
  entered := TEvent.Create(nil, true, false, '');
  receiver := TOwningReceiver.Create(FChannel);
  sender := nil;
  destroyer := nil;
  destroyerStarted := false;
  destroyed := false;
  try
    receiver.OnDestroyed :=
      procedure
      begin
        destroyed := true;
      end;
    //the raw action handler does not touch the receiver after the action, so freeing it early is safe here
    sender := StartThread(
      procedure
      begin
        FChannel.Queue.SendMessage<TRawActionMessage>(TRawActionMessage.Create(
          procedure
          begin
            entered.SetEvent;
            gate.WaitFor(cWaitTimeout);
          end));
      end);
    Assert.IsTrue(entered.WaitFor(cWaitTimeout) = wrSignaled, 'the handler did not start');
    destroyerStarted := true;
    destroyer := StartThread(
      procedure
      begin
        receiver.Free;
      end);
    WaitUntil(
      function : boolean
      begin
        result := destroyed;
      end, 300);
    destroyedWhileHandlerRan := destroyed;
    gate.SetEvent;
    Assert.IsTrue(FinishThread(sender), 'SendMessage did not return');
    Assert.IsTrue(FinishThread(destroyer), 'destroying the receiver did not finish');
    Assert.IsFalse(destroyedWhileHandlerRan, 'the receiver was freed while a SendMessage handler was still running on it, its teardown does not wait for synchronous dispatches (review H3)');
  finally
    gate.SetEvent;
    FinishThread(sender);
    if destroyerStarted then
      FinishThread(destroyer)
    else
      receiver.Free;
    entered.Free;
    gate.Free;
  end;
end;

procedure TLifetimeTests.HandlerFreesItsOwnReceiver_DoesNotHang;
var
  receiver : TOwningReceiver;
  destroyed : boolean;
begin
  destroyed := false;
  receiver := TOwningReceiver.Create(FChannel);
  receiver.OnDestroyed :=
    procedure
    begin
      destroyed := true;
    end;
  FChannel.Queue.PostMessage<TFreeSelfMessage>(TFreeSelfMessage.Create(0));
  //when this fails the dispatcher thread is left blocked waiting for itself, and is leaked
  Assert.IsTrue(WaitUntil(
    function : boolean
    begin
      result := destroyed;
    end, 2000), 'a receiver that freed itself from its own handler never finished being destroyed, its dispatcher waits for its own thread to end (review H5)');
end;

initialization
  TDUnitX.RegisterTestFixture(TLifetimeTests);

end.

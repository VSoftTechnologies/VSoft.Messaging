unit Tests.UIDispatcher;

interface

uses
  DUnitX.TestFramework,
  VSoft.Messaging;

type
  //UI dispatchers deliver through TThread.Synchronize, so these tests run on the main thread
  //and process synchronize calls (WaitUntil / PumpFor) while they wait.
  [TestFixture]
  TUIDispatcherTests = class
  private
    FChannel : IMessageChannel;
    FBaselineDepth : integer;
  public
    [Setup]
    procedure Setup;
    [TearDown]
    procedure TearDown;

    [Test]
    procedure PostMessage_FromABackgroundThread_HandlerRunsOnTheMainThread;
    [Test]
    procedure PostMessage_NotDeliveredUntilTheMainThreadProcessesSynchronize;
    [Test]
    procedure PostMessage_DeliversInOrder;
    [Test]
    procedure SendMessage_FromTheMainThread_IsSynchronous;
    [Test]
    procedure SendMessage_FromABackgroundThread_HandlerRunsOnTheMainThread;
    [Test]
    procedure SendMessage_DispatcherCreatedOnABackgroundThread_HandlerRunsOnTheMainThread;
    [Test]
    procedure SendMessage_FromTheMainThread_DoesNotOvertakeQueuedMessages;
    [Test]
    procedure Disabled_DoesNotReceive;
    [Test]
    procedure SeveralUIDispatchers_AllReceiveOnTheMainThread;
    [Test]
    procedure HandlerException_LaterMessagesAreStillDelivered;
    [Test]
    procedure HandlerException_IsReportedToOnError;
    [Test]
    procedure Destroy_WithUndeliveredMessages_ReturnsWithoutDeliveringThem;
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

function MakePoster(const channel : IMessageChannel; const count : integer) : TProc;
begin
  result :=
    procedure
    var
      i : integer;
    begin
      for i := 1 to count do
        channel.Queue.PostMessage<TValueMessage>(TValueMessage.Create(i));
    end;
end;

//waits for a thread without processing synchronize calls, so nothing is delivered to UI dispatchers
procedure WaitWithoutPumping(var thread : TTestThread);
begin
  while not thread.Finished do
    Sleep(1);
  FreeAndNil(thread);
end;

{ TUIDispatcherTests }

procedure TUIDispatcherTests.Setup;
begin
  ChannelErrors.Clear;
  FBaselineDepth := TMessagingControl.GetGlobalQueueDepth;
  FChannel := TMessageChannelFactory.CreateChannel;
  FChannel.OnError := RecordChannelError;
end;

procedure TUIDispatcherTests.TearDown;
begin
  FChannel := nil;
  RestoreGlobalQueueDepth(FBaselineDepth);
end;

procedure TUIDispatcherTests.PostMessage_FromABackgroundThread_HandlerRunsOnTheMainThread;
var
  receiver : TOwningReceiver;
  poster : TTestThread;
  threadIds : TArray<TThreadID>;
  i : integer;
begin
  receiver := TOwningReceiver.Create(FChannel, true);
  try
    poster := StartThread(MakePoster(FChannel, 10));
    Assert.IsTrue(FinishThread(poster), 'the posting thread did not finish');
    Assert.IsTrue(WaitUntil(
      function : boolean
      begin
        result := receiver.ValueCount = 10;
      end), 'not every message was delivered');
    threadIds := receiver.ThreadIds;
    for i := 0 to High(threadIds) do
      Assert.IsTrue(threadIds[i] = MainThreadID, Format('message %d was not handled on the main thread', [i + 1]));
  finally
    receiver.Free;
  end;
end;

procedure TUIDispatcherTests.PostMessage_NotDeliveredUntilTheMainThreadProcessesSynchronize;
var
  receiver : TOwningReceiver;
begin
  receiver := TOwningReceiver.Create(FChannel, true);
  try
    FChannel.Queue.PostMessage<TValueMessage>(TValueMessage.Create(1));
    Sleep(100);
    Assert.AreEqual(0, receiver.ValueCount, 'the handler ran without the main thread processing synchronize');
    Assert.IsTrue(WaitUntil(
      function : boolean
      begin
        result := receiver.ValueCount = 1;
      end), 'the message was not delivered');
  finally
    receiver.Free;
  end;
end;

procedure TUIDispatcherTests.PostMessage_DeliversInOrder;
const
  cCount = 300;
var
  receiver : TOwningReceiver;
  poster : TTestThread;
  values : TArray<integer>;
  i : integer;
begin
  receiver := TOwningReceiver.Create(FChannel, true);
  try
    poster := StartThread(MakePoster(FChannel, cCount));
    Assert.IsTrue(FinishThread(poster), 'the posting thread did not finish');
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

procedure TUIDispatcherTests.SendMessage_FromTheMainThread_IsSynchronous;
var
  receiver : TOwningReceiver;
  threadIds : TArray<TThreadID>;
begin
  receiver := TOwningReceiver.Create(FChannel, true);
  try
    FChannel.Queue.SendMessage<TValueMessage>(TValueMessage.Create(1));
    Assert.AreEqual(1, receiver.ValueCount, 'SendMessage returned before the handler ran');
    threadIds := receiver.ThreadIds;
    Assert.IsTrue(threadIds[0] = MainThreadID);
  finally
    receiver.Free;
  end;
end;

procedure TUIDispatcherTests.SendMessage_FromABackgroundThread_HandlerRunsOnTheMainThread;
var
  receiver : TOwningReceiver;
  sender : TTestThread;
  threadIds : TArray<TThreadID>;
begin
  receiver := TOwningReceiver.Create(FChannel, true);
  try
    sender := StartThread(
      procedure
      begin
        FChannel.Queue.SendMessage<TValueMessage>(TValueMessage.Create(1));
      end);
    Assert.IsTrue(FinishThread(sender), 'the sending thread did not finish');
    Assert.IsTrue(WaitUntil(
      function : boolean
      begin
        result := receiver.ValueCount = 1;
      end), 'the message was not delivered');
    threadIds := receiver.ThreadIds;
    Assert.IsTrue(threadIds[0] = MainThreadID, 'the handler did not run on the main thread');
  finally
    receiver.Free;
  end;
end;

procedure TUIDispatcherTests.SendMessage_DispatcherCreatedOnABackgroundThread_HandlerRunsOnTheMainThread;
var
  receiver : TTestReceiver;
  dispatcher : IMessageDispatcher;
  worker : TTestThread;
  threadIds : TArray<TThreadID>;
begin
  receiver := TTestReceiver.Create;
  try
    //the dispatcher is created, subscribed and sent to all on the same background thread
    worker := StartThread(
      procedure
      begin
        dispatcher := TMessageDispatcherFactory.CreateUIDispatcher(receiver);
        dispatcher.Channel := FChannel;
        FChannel.Queue.SendMessage<TValueMessage>(TValueMessage.Create(1));
      end);
    Assert.IsTrue(FinishThread(worker), 'the worker thread did not finish');
    Assert.IsTrue(WaitUntil(
      function : boolean
      begin
        result := receiver.ValueCount = 1;
      end), 'the message was not delivered');
    threadIds := receiver.ThreadIds;
    Assert.IsTrue(threadIds[0] = MainThreadID, 'a UI dispatcher created on a background thread ran its handler on that thread, not the main thread (review M3)');
  finally
    if dispatcher <> nil then
      dispatcher.Channel := nil;
    dispatcher := nil;
    receiver.Free;
  end;
end;

procedure TUIDispatcherTests.SendMessage_FromTheMainThread_DoesNotOvertakeQueuedMessages;
var
  receiver : TOwningReceiver;
  poster : TTestThread;
  values : TArray<integer>;
begin
  receiver := TOwningReceiver.Create(FChannel, true);
  try
    poster := StartThread(MakePoster(FChannel, 3));
    WaitWithoutPumping(poster);
    //1..3 are queued and waiting for the main thread
    Assert.AreEqual(0, receiver.ValueCount, 'precondition : nothing should be delivered yet');
    FChannel.Queue.SendMessage<TValueMessage>(TValueMessage.Create(4));
    Assert.IsTrue(WaitUntil(
      function : boolean
      begin
        result := receiver.ValueCount = 4;
      end), 'not every message was delivered');
    values := receiver.Values;
    Assert.AreEqual('1,2,3,4', Format('%d,%d,%d,%d', [values[0], values[1], values[2], values[3]]),
      'a message sent from the main thread was handled before messages posted earlier (review M3)');
  finally
    receiver.Free;
  end;
end;

procedure TUIDispatcherTests.Disabled_DoesNotReceive;
var
  disabled : TOwningReceiver;
  enabled : TOwningReceiver;
begin
  disabled := TOwningReceiver.Create(FChannel, true);
  enabled := TOwningReceiver.Create(FChannel, true);
  try
    disabled.Dispatcher.Enabled := false;
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

procedure TUIDispatcherTests.SeveralUIDispatchers_AllReceiveOnTheMainThread;
const
  cReceiverCount = 5;
var
  receivers : array[1..cReceiverCount] of TOwningReceiver;
  poster : TTestThread;
  threadIds : TArray<TThreadID>;
  i : integer;
  j : integer;
  allDelivered : boolean;
begin
  for i := 1 to cReceiverCount do
    receivers[i] := TOwningReceiver.Create(FChannel, true);
  try
    poster := StartThread(MakePoster(FChannel, 20));
    Assert.IsTrue(FinishThread(poster), 'the posting thread did not finish');
    allDelivered := WaitUntil(
      function : boolean
      var
        k : integer;
      begin
        result := true;
        for k := 1 to cReceiverCount do
          result := result and (receivers[k].ValueCount = 20);
      end);
    Assert.IsTrue(allDelivered, 'not every dispatcher received every message');
    for i := 1 to cReceiverCount do
    begin
      threadIds := receivers[i].ThreadIds;
      for j := 0 to High(threadIds) do
        Assert.IsTrue(threadIds[j] = MainThreadID, 'a handler did not run on the main thread');
    end;
  finally
    for i := cReceiverCount downto 1 do
      receivers[i].Free;
  end;
end;

procedure TUIDispatcherTests.HandlerException_LaterMessagesAreStillDelivered;
var
  receiver : TOwningReceiver;
begin
  receiver := TOwningReceiver.Create(FChannel, true);
  try
    FChannel.Queue.PostMessage<TRaiseMessage>(TRaiseMessage.Create('boom'));
    FChannel.Queue.PostMessage<TValueMessage>(TValueMessage.Create(1));
    Assert.IsTrue(WaitUntil(
      function : boolean
      begin
        result := receiver.ValueCount = 1;
      end), 'the message after the failing one was not delivered');
  finally
    receiver.Free;
  end;
end;

procedure TUIDispatcherTests.HandlerException_IsReportedToOnError;
var
  receiver : TOwningReceiver;
begin
  receiver := TOwningReceiver.Create(FChannel, true);
  try
    FChannel.Queue.PostMessage<TRaiseMessage>(TRaiseMessage.Create('boom'));
    FChannel.Queue.PostMessage<TValueMessage>(TValueMessage.Create(1));
    Assert.IsTrue(WaitUntil(
      function : boolean
      begin
        result := receiver.ValueCount = 1;
      end), 'the message after the failing one was not delivered');
    Assert.AreEqual('boom', ChannelErrors.Text, 'an exception raised by a UI handler was swallowed instead of being reported to OnError (review M5)');
  finally
    receiver.Free;
  end;
end;

procedure TUIDispatcherTests.Destroy_WithUndeliveredMessages_ReturnsWithoutDeliveringThem;
var
  log : TTestLog;
  receiver : TOwningReceiver;
  poster : TTestThread;
begin
  log := TTestLog.Create;
  try
    receiver := TOwningReceiver.Create(FChannel, true, 'receiver', log);
    try
      poster := StartThread(MakePoster(FChannel, 50));
      WaitWithoutPumping(poster);
      Assert.AreEqual(0, log.Count, 'precondition : nothing should be delivered yet');
    finally
      //the destructor waits for the dispatcher thread, processing synchronize calls while it does
      receiver.Free;
    end;
    Assert.AreEqual(0, log.Count, 'messages were delivered while the receiver was being destroyed');
  finally
    log.Free;
  end;
end;

initialization
  TDUnitX.RegisterTestFixture(TUIDispatcherTests);

end.

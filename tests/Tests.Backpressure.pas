unit Tests.Backpressure;

interface

uses
  DUnitX.TestFramework,
  VSoft.Messaging;

type
  [TestFixture]
  TMessagingOptionsTests = class
  private
    FMaxBurst : integer;
    FBlockedThreshold : integer;
    FUnblockThreshold : integer;
  public
    [Setup]
    procedure Setup;
    [TearDown]
    procedure TearDown;

    [Test]
    procedure Options_HaveTheDocumentedDefaults;
    [Test]
    procedure MaxBurst_LessThanOne_IsClampedToOne;
    [Test]
    procedure BlockedThreshold_LessThanTwo_IsClampedToTwo;
    [Test]
    procedure UnblockThreshold_NotBelowBlockedThreshold_IsClampedBelowIt;
    [Test]
    procedure UnblockThreshold_Negative_IsClampedToZero;
    [Test]
    procedure BlockedThreshold_LoweredBelowUnblockThreshold_UnblockThresholdFollows;
  end;

  //The global queue depth and the pushback that blocks posting threads when it gets too high.
  //The thresholds are absolute, so every test puts the depth back where it found it.
  [TestFixture]
  TBackpressureTests = class
  private
    FChannel : IMessageChannel;
    FBaselineDepth : integer;
    FMaxBurst : integer;
    FBlockedThreshold : integer;
    FUnblockThreshold : integer;
  public
    [Setup]
    procedure Setup;
    [TearDown]
    procedure TearDown;

    [Test]
    procedure GlobalQueueDepth_ReturnsToBaselineOnceMessagesAreDelivered;
    [Test]
    procedure GlobalQueueDepth_CountsMessagesWaitingInQueues;
    [Test]
    procedure GlobalQueueDepth_DispatcherDestroyedWithQueuedMessages_ReturnsToBaseline;
    [Test]
    procedure GlobalQueueDepth_UIDispatcherDestroyedWithQueuedMessages_ReturnsToBaseline;
    [Test]
    procedure Pushback_BackgroundPosterBlocksAtTheThresholdAndResumesWhenDrained;
    [Test]
    procedure Pushback_MainThreadPosterIsNeverBlocked;
    [Test]
    procedure Pushback_HandlerPostingWhileTheQueueIsFull_DoesNotDeadlock;
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

//restores the thresholds in an order the setters accept
procedure RestoreOptions(const maxBurst : integer; const blockedThreshold : integer; const unblockThreshold : integer);
begin
  TMessagingOptions.BlockedThreshold := blockedThreshold;
  TMessagingOptions.UnblockThreshold := unblockThreshold;
  TMessagingOptions.MaxBurst := maxBurst;
end;

{ TMessagingOptionsTests }

procedure TMessagingOptionsTests.Setup;
begin
  FMaxBurst := TMessagingOptions.MaxBurst;
  FBlockedThreshold := TMessagingOptions.BlockedThreshold;
  FUnblockThreshold := TMessagingOptions.UnblockThreshold;
end;

procedure TMessagingOptionsTests.TearDown;
begin
  RestoreOptions(FMaxBurst, FBlockedThreshold, FUnblockThreshold);
end;

procedure TMessagingOptionsTests.Options_HaveTheDocumentedDefaults;
begin
  Assert.AreEqual(20, TMessagingOptions.MaxBurst);
  Assert.AreEqual(100, TMessagingOptions.BlockedThreshold);
  Assert.AreEqual(50, TMessagingOptions.UnblockThreshold);
end;

procedure TMessagingOptionsTests.MaxBurst_LessThanOne_IsClampedToOne;
begin
  TMessagingOptions.MaxBurst := 0;
  Assert.AreEqual(1, TMessagingOptions.MaxBurst);
  TMessagingOptions.MaxBurst := -10;
  Assert.AreEqual(1, TMessagingOptions.MaxBurst);
  TMessagingOptions.MaxBurst := 5;
  Assert.AreEqual(5, TMessagingOptions.MaxBurst);
end;

procedure TMessagingOptionsTests.BlockedThreshold_LessThanTwo_IsClampedToTwo;
begin
  TMessagingOptions.BlockedThreshold := 1;
  Assert.AreEqual(2, TMessagingOptions.BlockedThreshold);
  TMessagingOptions.BlockedThreshold := -1;
  Assert.AreEqual(2, TMessagingOptions.BlockedThreshold);
  TMessagingOptions.BlockedThreshold := 200;
  Assert.AreEqual(200, TMessagingOptions.BlockedThreshold);
end;

procedure TMessagingOptionsTests.UnblockThreshold_NotBelowBlockedThreshold_IsClampedBelowIt;
begin
  TMessagingOptions.BlockedThreshold := 100;
  TMessagingOptions.UnblockThreshold := 100;
  Assert.AreEqual(99, TMessagingOptions.UnblockThreshold);
  TMessagingOptions.UnblockThreshold := 150;
  Assert.AreEqual(99, TMessagingOptions.UnblockThreshold);
  TMessagingOptions.UnblockThreshold := 10;
  Assert.AreEqual(10, TMessagingOptions.UnblockThreshold);
end;

procedure TMessagingOptionsTests.UnblockThreshold_Negative_IsClampedToZero;
begin
  TMessagingOptions.BlockedThreshold := 100;
  TMessagingOptions.UnblockThreshold := -5;
  Assert.AreEqual(0, TMessagingOptions.UnblockThreshold, 'a negative unblock threshold can never be reached, so blocked posters always wait for the full timeout (review M4)');
end;

procedure TMessagingOptionsTests.BlockedThreshold_LoweredBelowUnblockThreshold_UnblockThresholdFollows;
begin
  TMessagingOptions.BlockedThreshold := 100;
  TMessagingOptions.UnblockThreshold := 50;
  TMessagingOptions.BlockedThreshold := 10;
  Assert.IsTrue(TMessagingOptions.UnblockThreshold < 10,
    Format('lowering BlockedThreshold to 10 left UnblockThreshold at %d, so posters blocked at 10 wait for a depth of %d that may never come (review M4)', [TMessagingOptions.UnblockThreshold, TMessagingOptions.UnblockThreshold]));
end;

{ TBackpressureTests }

procedure TBackpressureTests.Setup;
begin
  ChannelErrors.Clear;
  FMaxBurst := TMessagingOptions.MaxBurst;
  FBlockedThreshold := TMessagingOptions.BlockedThreshold;
  FUnblockThreshold := TMessagingOptions.UnblockThreshold;
  FBaselineDepth := TMessagingControl.GetGlobalQueueDepth;
  FChannel := TMessageChannelFactory.CreateChannel;
  FChannel.OnError := RecordChannelError;
end;

procedure TBackpressureTests.TearDown;
begin
  FChannel := nil;
  RestoreOptions(FMaxBurst, FBlockedThreshold, FUnblockThreshold);
  RestoreGlobalQueueDepth(FBaselineDepth);
end;

procedure TBackpressureTests.GlobalQueueDepth_ReturnsToBaselineOnceMessagesAreDelivered;
var
  first : TOwningReceiver;
  second : TOwningReceiver;
  poster : TTestThread;
begin
  first := TOwningReceiver.Create(FChannel);
  second := TOwningReceiver.Create(FChannel, true);
  try
    poster := StartThread(MakePoster(FChannel, 200));
    Assert.IsTrue(FinishThread(poster), 'the posting thread did not finish');
    Assert.IsTrue(WaitUntil(
      function : boolean
      begin
        result := (first.ValueCount = 200) and (second.ValueCount = 200);
      end), 'not every message was delivered');
    //a message is taken off the depth just after its last handler returns
    Assert.IsTrue(WaitUntil(
      function : boolean
      begin
        result := TMessagingControl.GetGlobalQueueDepth = FBaselineDepth;
      end), 'the global queue depth did not return to its baseline');
  finally
    second.Free;
    first.Free;
  end;
end;

procedure TBackpressureTests.GlobalQueueDepth_CountsMessagesWaitingInQueues;
var
  receiver : TOwningReceiver;
  gate : TEvent;
  entered : TEvent;
  finished : TEvent;
  i : integer;
begin
  TMessagingOptions.MaxBurst := 1;
  gate := TEvent.Create(nil, true, false, '');
  entered := TEvent.Create(nil, true, false, '');
  finished := TEvent.Create(nil, true, false, '');
  receiver := TOwningReceiver.Create(FChannel);
  try
    PostGate(FChannel, entered, gate, finished);
    Assert.IsTrue(entered.WaitFor(cWaitTimeout) = wrSignaled, 'the first handler did not start');
    for i := 1 to 5 do
      FChannel.Queue.PostMessage<TValueMessage>(TValueMessage.Create(i));
    //the five queued messages plus the one being handled, which counts until its handler returns
    Assert.AreEqual(FBaselineDepth + 6, TMessagingControl.GetGlobalQueueDepth, 'the queued messages were not counted');
    gate.SetEvent;
    Assert.IsTrue(WaitUntil(
      function : boolean
      begin
        result := receiver.ValueCount = 5;
      end), 'not every message was delivered');
    Assert.IsTrue(WaitUntil(
      function : boolean
      begin
        result := TMessagingControl.GetGlobalQueueDepth = FBaselineDepth;
      end), 'the global queue depth did not return to its baseline');
  finally
    gate.SetEvent;
    receiver.Free;
    finished.Free;
    entered.Free;
    gate.Free;
  end;
end;

procedure TBackpressureTests.GlobalQueueDepth_DispatcherDestroyedWithQueuedMessages_ReturnsToBaseline;
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
      FChannel.Queue.PostMessage<TValueMessage>(TValueMessage.Create(i));
    //disable so the five stay queued when the first handler returns
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
  Assert.AreEqual(FBaselineDepth, TMessagingControl.GetGlobalQueueDepth, 'messages still queued when the dispatcher was destroyed are never taken off the global queue depth (review H4)');
end;

procedure TBackpressureTests.GlobalQueueDepth_UIDispatcherDestroyedWithQueuedMessages_ReturnsToBaseline;
var
  receiver : TOwningReceiver;
  poster : TTestThread;
begin
  receiver := TOwningReceiver.Create(FChannel, true);
  try
    poster := StartThread(MakePoster(FChannel, 30));
    //wait without processing synchronize calls, so nothing is delivered
    while not poster.Finished do
      Sleep(1);
    poster.Free;
    Assert.AreEqual(0, receiver.ValueCount, 'precondition : nothing should be delivered yet');
  finally
    receiver.Free;
  end;
  Assert.AreEqual(FBaselineDepth, TMessagingControl.GetGlobalQueueDepth, 'messages still queued when the UI dispatcher was destroyed are never taken off the global queue depth (review H4)');
end;

procedure TBackpressureTests.Pushback_BackgroundPosterBlocksAtTheThresholdAndResumesWhenDrained;
var
  receiver : TOwningReceiver;
  gate : TEvent;
  entered : TEvent;
  finished : TEvent;
  poster : TTestThread;
  postedCount : integer;
  postedWhileBlocked : integer;
begin
  Assert.AreEqual(0, FBaselineDepth, 'precondition : the thresholds below assume an empty global queue');
  TMessagingOptions.MaxBurst := 1;
  TMessagingOptions.BlockedThreshold := 5;
  TMessagingOptions.UnblockThreshold := 2;
  gate := TEvent.Create(nil, true, false, '');
  entered := TEvent.Create(nil, true, false, '');
  finished := TEvent.Create(nil, true, false, '');
  receiver := TOwningReceiver.Create(FChannel);
  poster := nil;
  postedCount := 0;
  try
    //hold the only consumer so posted messages stay queued
    PostGate(FChannel, entered, gate, finished);
    Assert.IsTrue(entered.WaitFor(cWaitTimeout) = wrSignaled, 'the first handler did not start');
    poster := StartThread(
      procedure
      var
        i : integer;
      begin
        for i := 1 to 10 do
        begin
          FChannel.Queue.PostMessage<TValueMessage>(TValueMessage.Create(i));
          TInterlocked.Increment(postedCount);
        end;
      end);
    //the message being handled counts, so 4 posts take the depth to the blocked threshold of 5
    //and the 5th post blocks (the poster checks before its own message counts)
    Assert.IsTrue(WaitUntil(
      function : boolean
      begin
        result := TInterlocked.CompareExchange(postedCount, 0, 0) >= 4;
      end), 'the poster did not get going');
    Sleep(300);
    postedWhileBlocked := TInterlocked.CompareExchange(postedCount, 0, 0);
    Assert.AreEqual(4, postedWhileBlocked, 'the poster should block once the queue depth reaches the blocked threshold');
    gate.SetEvent;
    Assert.IsTrue(FinishThread(poster), 'the poster was not released once the queue drained');
    Assert.IsTrue(WaitUntil(
      function : boolean
      begin
        result := receiver.ValueCount = 10;
      end), 'not every message was delivered');
    Assert.IsTrue(WaitUntil(
      function : boolean
      begin
        result := TMessagingControl.GetGlobalQueueDepth = FBaselineDepth;
      end), 'the global queue depth did not return to its baseline');
  finally
    gate.SetEvent;
    FinishThread(poster);
    receiver.Free;
    finished.Free;
    entered.Free;
    gate.Free;
  end;
end;

procedure TBackpressureTests.Pushback_MainThreadPosterIsNeverBlocked;
var
  receiver : TOwningReceiver;
  gate : TEvent;
  entered : TEvent;
  finished : TEvent;
  i : integer;
begin
  TMessagingOptions.MaxBurst := 1;
  TMessagingOptions.BlockedThreshold := FBaselineDepth + 5;
  TMessagingOptions.UnblockThreshold := FBaselineDepth + 2;
  gate := TEvent.Create(nil, true, false, '');
  entered := TEvent.Create(nil, true, false, '');
  finished := TEvent.Create(nil, true, false, '');
  receiver := TOwningReceiver.Create(FChannel);
  try
    PostGate(FChannel, entered, gate, finished);
    Assert.IsTrue(entered.WaitFor(cWaitTimeout) = wrSignaled, 'the first handler did not start');
    //if the main thread were throttled this loop would block for the full pushback timeout
    for i := 1 to 10 do
      FChannel.Queue.PostMessage<TValueMessage>(TValueMessage.Create(i));
    //the ten posts plus the message being handled
    Assert.AreEqual(FBaselineDepth + 11, TMessagingControl.GetGlobalQueueDepth);
    gate.SetEvent;
    Assert.IsTrue(WaitUntil(
      function : boolean
      begin
        result := receiver.ValueCount = 10;
      end), 'not every message was delivered');
  finally
    gate.SetEvent;
    receiver.Free;
    finished.Free;
    entered.Free;
    gate.Free;
  end;
end;

procedure TBackpressureTests.Pushback_HandlerPostingWhileTheQueueIsFull_DoesNotDeadlock;
var
  receiver : TOwningReceiver;
  gate : TEvent;
  entered : TEvent;
  i : integer;
  delivered : boolean;
begin
  Assert.AreEqual(0, FBaselineDepth, 'precondition : the thresholds below assume an empty global queue');
  TMessagingOptions.MaxBurst := 1;
  TMessagingOptions.BlockedThreshold := 3;
  TMessagingOptions.UnblockThreshold := 1;
  gate := TEvent.Create(nil, true, false, '');
  entered := TEvent.Create(nil, true, false, '');
  receiver := TOwningReceiver.Create(FChannel);
  try
    //the first handler waits for the gate, then posts a follow up message from the dispatcher thread
    FChannel.Queue.PostMessage<TActionMessage>(TActionMessage.Create(
      procedure
      begin
        entered.SetEvent;
        gate.WaitFor(cWaitTimeout);
        FChannel.Queue.PostMessage<TValueMessage>(TValueMessage.Create(99));
      end));
    Assert.IsTrue(entered.WaitFor(cWaitTimeout) = wrSignaled, 'the first handler did not start');
    //the main thread is never throttled, so this takes the depth past the blocked threshold
    for i := 1 to 5 do
      FChannel.Queue.PostMessage<TValueMessage>(TValueMessage.Create(i));
    gate.SetEvent;
    delivered := WaitUntil(
      function : boolean
      begin
        result := receiver.ValueCount = 6;
      end, 3000);
  finally
    gate.SetEvent;
    //if the dispatcher thread is stuck in the pushback, let it go
    FChannel.ReleaseWaiters;
    WaitUntil(
      function : boolean
      begin
        result := receiver.ValueCount = 6;
      end);
    receiver.Free;
    entered.Free;
    gate.Free;
  end;
  Assert.IsTrue(delivered, 'a handler that posted while the global queue was over the blocked threshold stalled, its dispatcher thread waited for its own queue to drain (review H6)');
end;

initialization
  TDUnitX.RegisterTestFixture(TMessagingOptionsTests);
  TDUnitX.RegisterTestFixture(TBackpressureTests);

end.

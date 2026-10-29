unit Tests.ChannelBackpressure;

interface

uses
  DUnitX.TestFramework,
  VSoft.Messaging;

type
  //Per channel backpressure : thresholds, release, cancellation, hooks and stats.
  //Every test runs with MaxBurst 1 and the channel's thresholds at 3/1, so holding the consumer in one
  //handler and posting 2 more messages from a background thread blocks the 3rd post.
  [TestFixture]
  TChannelBackpressureTests = class
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
    procedure SlowChannel_DoesNotThrottleAnotherChannel;
    [Test]
    procedure Thresholds_FollowTheOptionsUntilSetForTheChannel;
    [Test]
    procedure SetThresholds_ValidatesThePair;
    [Test]
    procedure ThresholdsRaisedWhileBlocked_ReleasesTheBlockedPoster;
    [Test]
    procedure DefaultThresholdsRaisedWhileBlocked_ReleasesTheBlockedPoster;
    [Test]
    procedure ReleaseWaiters_LetsTheBlockedPostGoAhead;
    [Test]
    procedure CancelledToken_ReleasesTheBlockedPosterAndPostsNoLongerBlock;
    [Test]
    procedure BackpressureDisabled_PostsDoNotBlock;
    [Test]
    procedure BackpressureDisabledWhileBlocked_ReleasesTheBlockedPoster;
    [Test]
    procedure PushbackTimeout_ThePostGoesAheadAndTheHookIsCalled;
    [Test]
    procedure OnPushbackWait_IsCalledOncePerBlockedPost;
    [Test]
    procedure Stats_CountPostsAndDepth;
    [Test]
    procedure Stats_CountPushbackWaits;
    [Test]
    procedure PostNoDispatcherAccepts_IsNotThrottled;
  end;

implementation

uses
  System.SysUtils,
  System.Classes,
  System.SyncObjs,
  VSoft.CancellationToken,
  VSoft.Messaging.Internal,
  Testing.Helpers,
  Testing.Messages,
  Testing.Receivers;

type
  //Holds the channel's only consumer inside a handler and runs a poster that blocks on the backpressure.
  TBlockedPoster = class
  private
    FChannel : IMessageChannel;
    FGate : TEvent;
    FEntered : TEvent;
    FReceiver : TOwningReceiver;
    FPoster : TTestThread;
    FPostedCount : integer;
    FTotal : integer;
  public
    constructor Create(const channel : IMessageChannel; const total : integer);
    destructor Destroy; override;
    //Holds the consumer, starts the poster and returns true if it stops after expectedCount posts.
    function Start(const expectedCount : integer) : boolean;
    function PostedCount : integer;
    procedure OpenGate;
    function FinishPoster(const timeoutMs : integer) : boolean;
    property Receiver : TOwningReceiver read FReceiver;
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

{ TBlockedPoster }

constructor TBlockedPoster.Create(const channel : IMessageChannel; const total : integer);
begin
  inherited Create;
  FChannel := channel;
  FTotal := total;
  FGate := TEvent.Create(nil, true, false, '');
  FEntered := TEvent.Create(nil, true, false, '');
  FReceiver := TOwningReceiver.Create(channel);
end;

destructor TBlockedPoster.Destroy;
begin
  //let the consumer drain, which releases the poster
  OpenGate;
  FinishPoster(10000);
  FReceiver.Free;
  FEntered.Free;
  FGate.Free;
  FChannel := nil;
  inherited;
end;

function TBlockedPoster.Start(const expectedCount : integer) : boolean;
var
  gate : TEvent;
  entered : TEvent;
  channel : IMessageChannel;
  total : integer;
  poster : TBlockedPoster;
begin
  gate := FGate;
  entered := FEntered;
  channel := FChannel;
  total := FTotal;
  poster := Self;
  FChannel.Queue.PostMessage<TActionMessage>(TActionMessage.Create(
    procedure
    begin
      entered.SetEvent;
      gate.WaitFor(cWaitTimeout);
    end));
  if FEntered.WaitFor(cWaitTimeout) <> wrSignaled then
    exit(false);
  FPoster := StartThread(
    procedure
    var
      i : integer;
    begin
      for i := 1 to total do
      begin
        channel.Queue.PostMessage<TValueMessage>(TValueMessage.Create(i));
        TInterlocked.Increment(poster.FPostedCount);
      end;
    end);
  WaitUntil(
    function : boolean
    begin
      result := poster.PostedCount >= expectedCount;
    end);
  Sleep(200);
  result := PostedCount = expectedCount;
end;

function TBlockedPoster.PostedCount : integer;
begin
  result := TInterlocked.CompareExchange(FPostedCount, 0, 0);
end;

procedure TBlockedPoster.OpenGate;
begin
  FGate.SetEvent;
end;

function TBlockedPoster.FinishPoster(const timeoutMs : integer) : boolean;
begin
  result := FinishThread(FPoster, timeoutMs);
end;

{ TChannelBackpressureTests }

procedure TChannelBackpressureTests.Setup;
begin
  ChannelErrors.Clear;
  FMaxBurst := TMessagingOptions.MaxBurst;
  FBlockedThreshold := TMessagingOptions.BlockedThreshold;
  FUnblockThreshold := TMessagingOptions.UnblockThreshold;
  FBaselineDepth := TMessagingControl.GetGlobalQueueDepth;
  TMessagingOptions.MaxBurst := 1;
  FChannel := TMessageChannelFactory.CreateChannel;
  FChannel.OnError := RecordChannelError;
  FChannel.SetThresholds(3, 1);
end;

procedure TChannelBackpressureTests.TearDown;
begin
  FChannel := nil;
  TMessagingOptions.SetThresholds(FBlockedThreshold, FUnblockThreshold);
  TMessagingOptions.MaxBurst := FMaxBurst;
  RestoreGlobalQueueDepth(FBaselineDepth);
end;

procedure TChannelBackpressureTests.SlowChannel_DoesNotThrottleAnotherChannel;
var
  blocked : TBlockedPoster;
  otherChannel : IMessageChannel;
  otherReceiver : TOwningReceiver;
  otherPoster : TTestThread;
begin
  otherChannel := TMessageChannelFactory.CreateChannel;
  otherChannel.SetThresholds(3, 1);
  otherReceiver := TOwningReceiver.Create(otherChannel);
  blocked := TBlockedPoster.Create(FChannel, 10);
  try
    Assert.IsTrue(blocked.Start(2), 'precondition : the poster on the slow channel should block');
    otherPoster := StartThread(MakePoster(otherChannel, 50));
    Assert.IsTrue(FinishThread(otherPoster, 3000), 'a poster on another channel was held up by the slow channel');
    Assert.IsTrue(WaitUntil(
      function : boolean
      begin
        result := otherReceiver.ValueCount = 50;
      end), 'the other channel did not deliver every message');
    Assert.AreEqual(2, blocked.PostedCount, 'the poster on the slow channel should still be blocked');
  finally
    blocked.Free;
    otherReceiver.Free;
    otherChannel := nil;
  end;
end;

procedure TChannelBackpressureTests.Thresholds_FollowTheOptionsUntilSetForTheChannel;
var
  channel : IMessageChannel;
begin
  channel := TMessageChannelFactory.CreateChannel;
  Assert.AreEqual(TMessagingOptions.BlockedThreshold, channel.BlockedThreshold);
  Assert.AreEqual(TMessagingOptions.UnblockThreshold, channel.UnblockThreshold);
  TMessagingOptions.SetThresholds(40, 20);
  Assert.AreEqual(40, channel.BlockedThreshold, 'a channel without its own thresholds should follow TMessagingOptions');
  Assert.AreEqual(20, channel.UnblockThreshold);
  channel.SetThresholds(10, 4);
  Assert.AreEqual(10, channel.BlockedThreshold);
  Assert.AreEqual(4, channel.UnblockThreshold);
  Assert.AreEqual(40, TMessagingOptions.BlockedThreshold, 'setting a channel''s thresholds changed the defaults');
  Assert.AreEqual(20, TMessagingOptions.UnblockThreshold);
  channel.ResetThresholds;
  Assert.AreEqual(40, channel.BlockedThreshold, 'ResetThresholds should go back to following TMessagingOptions');
  Assert.AreEqual(20, channel.UnblockThreshold);
end;

procedure TChannelBackpressureTests.SetThresholds_ValidatesThePair;
begin
  FChannel.SetThresholds(1, 5);
  Assert.AreEqual(2, FChannel.BlockedThreshold);
  Assert.AreEqual(1, FChannel.UnblockThreshold);
  FChannel.SetThresholds(10, -3);
  Assert.AreEqual(10, FChannel.BlockedThreshold);
  Assert.AreEqual(0, FChannel.UnblockThreshold);
  FChannel.SetThresholds(10, 20);
  Assert.AreEqual(10, FChannel.BlockedThreshold);
  Assert.AreEqual(9, FChannel.UnblockThreshold);
end;

procedure TChannelBackpressureTests.ThresholdsRaisedWhileBlocked_ReleasesTheBlockedPoster;
var
  blocked : TBlockedPoster;
begin
  blocked := TBlockedPoster.Create(FChannel, 10);
  try
    Assert.IsTrue(blocked.Start(2), 'precondition : the poster should block');
    FChannel.SetThresholds(100, 50);
    Assert.IsTrue(blocked.FinishPoster(3000), 'raising the thresholds did not release the blocked poster');
    Assert.AreEqual(10, blocked.PostedCount);
  finally
    blocked.Free;
  end;
end;

procedure TChannelBackpressureTests.DefaultThresholdsRaisedWhileBlocked_ReleasesTheBlockedPoster;
var
  blocked : TBlockedPoster;
begin
  FChannel.ResetThresholds;
  TMessagingOptions.SetThresholds(3, 1);
  blocked := TBlockedPoster.Create(FChannel, 10);
  try
    Assert.IsTrue(blocked.Start(2), 'precondition : the poster should block');
    TMessagingOptions.SetThresholds(100, 50);
    Assert.IsTrue(blocked.FinishPoster(3000), 'raising the default thresholds did not release the blocked poster');
    Assert.AreEqual(10, blocked.PostedCount);
  finally
    blocked.Free;
  end;
end;

procedure TChannelBackpressureTests.ReleaseWaiters_LetsTheBlockedPostGoAhead;
var
  blocked : TBlockedPoster;
begin
  blocked := TBlockedPoster.Create(FChannel, 10);
  try
    Assert.IsTrue(blocked.Start(2), 'precondition : the poster should block');
    FChannel.ReleaseWaiters;
    Assert.IsTrue(WaitUntil(
      function : boolean
      begin
        result := blocked.PostedCount >= 3;
      end, 2000), 'ReleaseWaiters did not release the blocked post');
    Sleep(200);
    Assert.AreEqual(3, blocked.PostedCount, 'the next post should block again, the backlog has not drained');
  finally
    blocked.Free;
  end;
end;

procedure TChannelBackpressureTests.CancelledToken_ReleasesTheBlockedPosterAndPostsNoLongerBlock;
var
  blocked : TBlockedPoster;
  tokenSource : ICancellationTokenSource;
begin
  tokenSource := TCancellationTokenSourceFactory.Create;
  FChannel.PushbackCancellation := tokenSource.Token;
  Assert.IsTrue(FChannel.PushbackCancellation = tokenSource.Token);
  blocked := TBlockedPoster.Create(FChannel, 10);
  try
    Assert.IsTrue(blocked.Start(2), 'precondition : the poster should block');
    tokenSource.Cancel;
    //the consumer is still held, so every remaining post has to go through without blocking
    Assert.IsTrue(blocked.FinishPoster(3000), 'cancelling the token did not release the blocked poster, or a later post blocked');
    Assert.AreEqual(10, blocked.PostedCount);
  finally
    blocked.Free;
  end;
end;

procedure TChannelBackpressureTests.BackpressureDisabled_PostsDoNotBlock;
var
  blocked : TBlockedPoster;
begin
  FChannel.BackpressureEnabled := false;
  Assert.IsFalse(FChannel.BackpressureEnabled);
  blocked := TBlockedPoster.Create(FChannel, 10);
  try
    Assert.IsTrue(blocked.Start(10), 'a post blocked with backpressure disabled');
  finally
    blocked.Free;
  end;
end;

procedure TChannelBackpressureTests.BackpressureDisabledWhileBlocked_ReleasesTheBlockedPoster;
var
  blocked : TBlockedPoster;
begin
  blocked := TBlockedPoster.Create(FChannel, 10);
  try
    Assert.IsTrue(blocked.Start(2), 'precondition : the poster should block');
    FChannel.BackpressureEnabled := false;
    Assert.IsTrue(blocked.FinishPoster(3000), 'disabling backpressure did not release the blocked poster');
    Assert.AreEqual(10, blocked.PostedCount);
  finally
    blocked.Free;
  end;
end;

procedure TChannelBackpressureTests.PushbackTimeout_ThePostGoesAheadAndTheHookIsCalled;
var
  blocked : TBlockedPoster;
  timeoutCount : integer;
  waited : Int64;
begin
  timeoutCount := 0;
  waited := 0;
  FChannel.PushbackTimeout := 600;
  Assert.AreEqual(Cardinal(600), FChannel.PushbackTimeout);
  FChannel.OnPushbackTimeout :=
    procedure(const waitedMs : Int64)
    begin
      waited := waitedMs;
      TInterlocked.Increment(timeoutCount);
    end;
  blocked := TBlockedPoster.Create(FChannel, 10);
  try
    Assert.IsTrue(blocked.Start(2), 'precondition : the poster should block');
    Assert.IsTrue(WaitUntil(
      function : boolean
      begin
        result := blocked.PostedCount >= 3;
      end, 3000), 'the blocked post did not go ahead after the pushback timeout');
    Assert.IsTrue(TInterlocked.CompareExchange(timeoutCount, 0, 0) >= 1, 'OnPushbackTimeout was not called');
    Assert.IsTrue(waited >= 550, Format('OnPushbackTimeout reported %d ms', [waited]));
    Assert.IsTrue(FChannel.Stats.PushbackTimeouts >= 1);
  finally
    blocked.Free;
  end;
end;

procedure TChannelBackpressureTests.OnPushbackWait_IsCalledOncePerBlockedPost;
var
  blocked : TBlockedPoster;
  notifications : integer;
begin
  notifications := 0;
  FChannel.PushbackWaitNotifyAfter := 100;
  Assert.AreEqual(Cardinal(100), FChannel.PushbackWaitNotifyAfter);
  FChannel.OnPushbackWait :=
    procedure(const waitedMs : Int64)
    begin
      TInterlocked.Increment(notifications);
    end;
  blocked := TBlockedPoster.Create(FChannel, 10);
  try
    Assert.IsTrue(blocked.Start(2), 'precondition : the poster should block');
    Sleep(300);
    Assert.AreEqual(1, TInterlocked.CompareExchange(notifications, 0, 0), 'OnPushbackWait should be called once for a blocked post');
  finally
    blocked.Free;
  end;
end;

procedure TChannelBackpressureTests.Stats_CountPostsAndDepth;
var
  receiver : TOwningReceiver;
  i : integer;
  stats : TChannelStats;
begin
  receiver := TOwningReceiver.Create(FChannel);
  try
    for i := 1 to 20 do
      FChannel.Queue.PostMessage<TValueMessage>(TValueMessage.Create(i));
    Assert.IsTrue(WaitUntil(
      function : boolean
      begin
        result := FChannel.Stats.Depth = 0;
      end), 'the channel depth did not return to 0');
    stats := FChannel.Stats;
    Assert.AreEqual(Int64(20), stats.PostCount);
    Assert.IsTrue(stats.PeakDepth >= 1);
    Assert.AreEqual(Int64(0), stats.PushbackWaits, 'posts from the main thread never wait');
    Assert.AreEqual(20, receiver.ValueCount);
  finally
    receiver.Free;
  end;
end;

procedure TChannelBackpressureTests.Stats_CountPushbackWaits;
var
  blocked : TBlockedPoster;
  stats : TChannelStats;
begin
  blocked := TBlockedPoster.Create(FChannel, 10);
  try
    Assert.IsTrue(blocked.Start(2), 'precondition : the poster should block');
    stats := FChannel.Stats;
    //the message being handled and the two posts
    Assert.AreEqual(3, stats.Depth);
    Assert.AreEqual(Int64(1), stats.PushbackWaits);
    blocked.OpenGate;
    Assert.IsTrue(blocked.FinishPoster(5000), 'the poster was not released');
    Assert.IsTrue(WaitUntil(
      function : boolean
      begin
        result := FChannel.Stats.Depth = 0;
      end), 'the channel depth did not return to 0');
    stats := FChannel.Stats;
    Assert.IsTrue(stats.TotalBlockedMs > 0);
    Assert.IsTrue(stats.LongestBlockedMs > 0);
    Assert.IsTrue(stats.LongestBlockedMs <= stats.TotalBlockedMs);
  finally
    blocked.Free;
  end;
end;

procedure TChannelBackpressureTests.PostNoDispatcherAccepts_IsNotThrottled;
var
  blocked : TBlockedPoster;
  poster : TTestThread;
begin
  blocked := TBlockedPoster.Create(FChannel, 10);
  try
    Assert.IsTrue(blocked.Start(2), 'precondition : the channel should be blocking posters');
    blocked.Receiver.Dispatcher.ExcludeFilter := MessageIds([TEST_MSG_OTHER]);
    poster := StartThread(
      procedure
      begin
        //excluded by the only dispatcher, and a message it has no handler for
        FChannel.Queue.PostMessage<TOtherMessage>(TOtherMessage.Create(1));
        FChannel.Queue.PostMessage<TUnhandledMessage>(TUnhandledMessage.Create(1));
      end);
    Assert.IsTrue(FinishThread(poster, 2000), 'a post no dispatcher accepts was blocked by backpressure');
  finally
    blocked.Free;
  end;
end;

initialization
  TDUnitX.RegisterTestFixture(TChannelBackpressureTests);

end.

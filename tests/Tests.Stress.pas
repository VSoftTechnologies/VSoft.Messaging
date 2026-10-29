unit Tests.Stress;

interface

uses
  DUnitX.TestFramework,
  VSoft.Messaging;

type
  //Load and race tests. The races they probe (review H3, M2) are timing dependent, so a pass does
  //not prove they are fixed. Exclude them with --exclude:Stress
  [TestFixture]
  [Category('Stress')]
  TStressTests = class
  private
    FChannel : IMessageChannel;
    FBaselineDepth : integer;
    FBlockedThreshold : integer;
    FUnblockThreshold : integer;
  public
    [Setup]
    procedure Setup;
    [TearDown]
    procedure TearDown;

    [Test]
    procedure ManyPostersManyDispatchers_EveryMessageIsDelivered;
    [Test]
    procedure ReceiversCreatedAndDestroyedWhilePosting_NoErrors;
    [Test]
    procedure FiltersChangedWhilePosting_NoErrors;
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

{ TStressTests }

procedure TStressTests.Setup;
begin
  ChannelErrors.Clear;
  FBlockedThreshold := TMessagingOptions.BlockedThreshold;
  FUnblockThreshold := TMessagingOptions.UnblockThreshold;
  FBaselineDepth := TMessagingControl.GetGlobalQueueDepth;
  FChannel := TMessageChannelFactory.CreateChannel;
  FChannel.OnError := RecordChannelError;
end;

procedure TStressTests.TearDown;
begin
  FChannel := nil;
  TMessagingOptions.BlockedThreshold := FBlockedThreshold;
  TMessagingOptions.UnblockThreshold := FUnblockThreshold;
  RestoreGlobalQueueDepth(FBaselineDepth);
end;

procedure TStressTests.ManyPostersManyDispatchers_EveryMessageIsDelivered;
const
  cPosterCount = 4;
  cPerPoster = 500;
  cReceiverCount = 5;
  cExpected = cPosterCount * cPerPoster;
var
  receivers : array[1..cReceiverCount] of TOwningReceiver;
  posters : array[1..cPosterCount] of TTestThread;
  i : integer;
  allDelivered : boolean;
begin
  //the last receiver uses a UI dispatcher, delivered while WaitUntil / FinishThread pump synchronize
  for i := 1 to cReceiverCount do
    receivers[i] := TOwningReceiver.Create(FChannel, i = cReceiverCount);
  try
    for i := 1 to cPosterCount do
      posters[i] := StartThread(MakePoster(FChannel, cPerPoster));
    for i := 1 to cPosterCount do
      Assert.IsTrue(FinishThread(posters[i], 30000), 'a posting thread did not finish');
    allDelivered := WaitUntil(
      function : boolean
      var
        k : integer;
      begin
        result := true;
        for k := 1 to cReceiverCount do
          result := result and (receivers[k].ValueCount = cExpected);
      end, 30000);
    Assert.IsTrue(allDelivered, 'not every dispatcher received every message');
    Assert.AreEqual(0, ChannelErrors.Count, ChannelErrors.Text);
    //a message is taken off the depth just after its last handler returns
    Assert.IsTrue(WaitUntil(
      function : boolean
      begin
        result := TMessagingControl.GetGlobalQueueDepth = FBaselineDepth;
      end), 'the global queue depth did not return to its baseline');
  finally
    for i := cReceiverCount downto 1 do
      receivers[i].Free;
  end;
end;

procedure TStressTests.ReceiversCreatedAndDestroyedWhilePosting_NoErrors;
var
  poster : TTestThread;
  stopPosting : boolean;
  posterError : string;
  receiver : TOwningReceiver;
  i : integer;
begin
  //the poster posts as fast as it can, keep the pushback out of the way so it races the teardowns
  TMessagingOptions.BlockedThreshold := MaxInt - 1;
  TMessagingOptions.UnblockThreshold := MaxInt - 2;
  stopPosting := false;
  poster := StartThread(
    procedure
    begin
      while not stopPosting do
        FChannel.Queue.PostMessage<TValueMessage>(TValueMessage.Create(1));
    end);
  try
    for i := 1 to 300 do
    begin
      receiver := TOwningReceiver.Create(FChannel);
      receiver.Free;
    end;
  finally
    stopPosting := true;
    Assert.IsTrue(FinishThread(poster, posterError), 'the posting thread did not finish');
  end;
  Assert.AreEqual('', posterError, 'posting raised while receivers were being destroyed');
  Assert.AreEqual(0, ChannelErrors.Count, 'a message was dispatched to a receiver after it was destroyed : ' + ChannelErrors.Text + ' (review H3)');
end;

procedure TStressTests.FiltersChangedWhilePosting_NoErrors;
var
  receiver : TOwningReceiver;
  poster : TTestThread;
  stopPosting : boolean;
  posterError : string;
  i : integer;
begin
  TMessagingOptions.BlockedThreshold := MaxInt - 1;
  TMessagingOptions.UnblockThreshold := MaxInt - 2;
  receiver := TOwningReceiver.Create(FChannel);
  try
    stopPosting := false;
    poster := StartThread(
      procedure
      begin
        while not stopPosting do
        begin
          FChannel.Queue.PostMessage<TValueMessage>(TValueMessage.Create(1));
          FChannel.Queue.PostMessage<TOtherMessage>(TOtherMessage.Create(2));
        end;
      end);
    try
      for i := 1 to 5000 do
      begin
        if Odd(i) then
        begin
          receiver.Dispatcher.IncludeFilter := MessageIds([TEST_MSG_VALUE, TEST_MSG_OTHER]);
          receiver.Dispatcher.ExcludeFilter := MessageIds([TEST_MSG_TEXT]);
        end
        else
        begin
          receiver.Dispatcher.IncludeFilter := nil;
          receiver.Dispatcher.ExcludeFilter := nil;
        end;
      end;
    finally
      stopPosting := true;
      Assert.IsTrue(FinishThread(poster, posterError), 'the posting thread did not finish');
    end;
    Assert.AreEqual('', posterError, 'posting raised while the filters were being changed (review M2)');
    Assert.AreEqual(0, ChannelErrors.Count, ChannelErrors.Text);
  finally
    receiver.Free;
  end;
end;

initialization
  TDUnitX.RegisterTestFixture(TStressTests);

end.

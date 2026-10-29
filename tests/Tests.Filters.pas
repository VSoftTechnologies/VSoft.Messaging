unit Tests.Filters;

interface

uses
  DUnitX.TestFramework,
  VSoft.Messaging;

type
  //SendMessage dispatches to threaded dispatchers on the calling thread, so most of these are synchronous
  [TestFixture]
  TFilterTests = class
  private
    FChannel : IMessageChannel;
    FBaselineDepth : integer;
  public
    [Setup]
    procedure Setup;
    [TearDown]
    procedure TearDown;

    [Test]
    procedure NoFilters_EveryMessageIsDelivered;
    [Test]
    procedure Filters_AreEmptyByDefault;
    [Test]
    procedure IncludeFilter_OnlyListedMessagesAreDelivered;
    [Test]
    procedure ExcludeFilter_ListedMessagesAreNotDelivered;
    [Test]
    procedure IncludeAndExcludeFilter_ExcludeWins;
    [Test]
    procedure IncludeFilter_ReturnsTheIdsThatWereSet;
    [Test]
    procedure ExcludeFilter_ReturnsTheIdsThatWereSet;
    [Test]
    procedure IncludeFilter_SetAgain_ReplacesThePreviousIds;
    [Test]
    procedure ExcludeFilter_SetAgain_ReplacesThePreviousIds;
    [Test]
    procedure IncludeFilter_SetToEmpty_RemovesTheFilter;
    [Test]
    procedure ExcludeFilter_SetToEmpty_RemovesTheFilter;
    [Test]
    procedure IncludeFilter_DuplicateIds_AreStoredOnce;
    [Test]
    procedure Filters_AreAppliedToPostedMessages;
    [Test]
    procedure Filters_OnlyAffectTheirOwnDispatcher;
  end;

implementation

uses
  System.SysUtils,
  System.Generics.Collections,
  VSoft.Messaging.Internal,
  Testing.Helpers,
  Testing.Messages,
  Testing.Receivers;

function SortedIds(const ids : TArray<Cardinal>) : string;
var
  sorted : TArray<Cardinal>;
  i : integer;
begin
  sorted := Copy(ids, 0, Length(ids));
  TArray.Sort<Cardinal>(sorted);
  result := '';
  for i := 0 to High(sorted) do
  begin
    if i > 0 then
      result := result + ',';
    result := result + IntToStr(sorted[i]);
  end;
end;

procedure SendValueAndOther(const channel : IMessageChannel);
begin
  channel.Queue.SendMessage<TValueMessage>(TValueMessage.Create(1));
  channel.Queue.SendMessage<TOtherMessage>(TOtherMessage.Create(2));
end;

{ TFilterTests }

procedure TFilterTests.Setup;
begin
  ChannelErrors.Clear;
  FBaselineDepth := TMessagingControl.GetGlobalQueueDepth;
  FChannel := TMessageChannelFactory.CreateChannel;
  FChannel.OnError := RecordChannelError;
end;

procedure TFilterTests.TearDown;
begin
  FChannel := nil;
  RestoreGlobalQueueDepth(FBaselineDepth);
end;

procedure TFilterTests.NoFilters_EveryMessageIsDelivered;
var
  receiver : TOwningReceiver;
begin
  receiver := TOwningReceiver.Create(FChannel);
  try
    SendValueAndOther(FChannel);
    Assert.AreEqual(1, receiver.ValueCount);
    Assert.AreEqual(1, receiver.OtherCount);
  finally
    receiver.Free;
  end;
end;

procedure TFilterTests.Filters_AreEmptyByDefault;
var
  receiver : TOwningReceiver;
begin
  receiver := TOwningReceiver.Create(FChannel);
  try
    Assert.AreEqual(0, integer(Length(receiver.Dispatcher.IncludeFilter)));
    Assert.AreEqual(0, integer(Length(receiver.Dispatcher.ExcludeFilter)));
  finally
    receiver.Free;
  end;
end;

procedure TFilterTests.IncludeFilter_OnlyListedMessagesAreDelivered;
var
  receiver : TOwningReceiver;
begin
  receiver := TOwningReceiver.Create(FChannel);
  try
    receiver.Dispatcher.IncludeFilter := MessageIds([TEST_MSG_OTHER]);
    SendValueAndOther(FChannel);
    Assert.AreEqual(0, receiver.ValueCount, 'a message that is not in the include filter was delivered');
    Assert.AreEqual(1, receiver.OtherCount, 'a message in the include filter was not delivered');
  finally
    receiver.Free;
  end;
end;

procedure TFilterTests.ExcludeFilter_ListedMessagesAreNotDelivered;
var
  receiver : TOwningReceiver;
begin
  receiver := TOwningReceiver.Create(FChannel);
  try
    receiver.Dispatcher.ExcludeFilter := MessageIds([TEST_MSG_OTHER]);
    SendValueAndOther(FChannel);
    Assert.AreEqual(1, receiver.ValueCount, 'a message that is not excluded was not delivered');
    Assert.AreEqual(0, receiver.OtherCount, 'an excluded message was delivered');
  finally
    receiver.Free;
  end;
end;

procedure TFilterTests.IncludeAndExcludeFilter_ExcludeWins;
var
  receiver : TOwningReceiver;
begin
  receiver := TOwningReceiver.Create(FChannel);
  try
    receiver.Dispatcher.IncludeFilter := MessageIds([TEST_MSG_VALUE, TEST_MSG_OTHER]);
    receiver.Dispatcher.ExcludeFilter := MessageIds([TEST_MSG_OTHER]);
    SendValueAndOther(FChannel);
    Assert.AreEqual(1, receiver.ValueCount);
    Assert.AreEqual(0, receiver.OtherCount);
  finally
    receiver.Free;
  end;
end;

procedure TFilterTests.IncludeFilter_ReturnsTheIdsThatWereSet;
var
  receiver : TOwningReceiver;
begin
  receiver := TOwningReceiver.Create(FChannel);
  try
    receiver.Dispatcher.IncludeFilter := MessageIds([TEST_MSG_OTHER, TEST_MSG_VALUE]);
    Assert.AreEqual(SortedIds(MessageIds([TEST_MSG_VALUE, TEST_MSG_OTHER])), SortedIds(receiver.Dispatcher.IncludeFilter));
  finally
    receiver.Free;
  end;
end;

procedure TFilterTests.ExcludeFilter_ReturnsTheIdsThatWereSet;
var
  receiver : TOwningReceiver;
begin
  receiver := TOwningReceiver.Create(FChannel);
  try
    receiver.Dispatcher.ExcludeFilter := MessageIds([TEST_MSG_TEXT, TEST_MSG_OTHER]);
    Assert.AreEqual(SortedIds(MessageIds([TEST_MSG_OTHER, TEST_MSG_TEXT])), SortedIds(receiver.Dispatcher.ExcludeFilter));
  finally
    receiver.Free;
  end;
end;

procedure TFilterTests.IncludeFilter_SetAgain_ReplacesThePreviousIds;
var
  receiver : TOwningReceiver;
begin
  receiver := TOwningReceiver.Create(FChannel);
  try
    receiver.Dispatcher.IncludeFilter := MessageIds([TEST_MSG_OTHER]);
    receiver.Dispatcher.IncludeFilter := MessageIds([TEST_MSG_VALUE]);
    Assert.AreEqual(SortedIds(MessageIds([TEST_MSG_VALUE])), SortedIds(receiver.Dispatcher.IncludeFilter));
    SendValueAndOther(FChannel);
    Assert.AreEqual(1, receiver.ValueCount);
    Assert.AreEqual(0, receiver.OtherCount);
  finally
    receiver.Free;
  end;
end;

procedure TFilterTests.ExcludeFilter_SetAgain_ReplacesThePreviousIds;
var
  receiver : TOwningReceiver;
begin
  receiver := TOwningReceiver.Create(FChannel);
  try
    receiver.Dispatcher.ExcludeFilter := MessageIds([TEST_MSG_OTHER]);
    receiver.Dispatcher.ExcludeFilter := MessageIds([TEST_MSG_VALUE]);
    Assert.AreEqual(SortedIds(MessageIds([TEST_MSG_VALUE])), SortedIds(receiver.Dispatcher.ExcludeFilter));
    SendValueAndOther(FChannel);
    Assert.AreEqual(0, receiver.ValueCount);
    Assert.AreEqual(1, receiver.OtherCount);
  finally
    receiver.Free;
  end;
end;

procedure TFilterTests.IncludeFilter_SetToEmpty_RemovesTheFilter;
var
  receiver : TOwningReceiver;
begin
  receiver := TOwningReceiver.Create(FChannel);
  try
    receiver.Dispatcher.IncludeFilter := MessageIds([TEST_MSG_OTHER]);
    receiver.Dispatcher.IncludeFilter := nil;
    Assert.AreEqual(0, integer(Length(receiver.Dispatcher.IncludeFilter)));
    SendValueAndOther(FChannel);
    Assert.AreEqual(1, receiver.ValueCount);
    Assert.AreEqual(1, receiver.OtherCount);
  finally
    receiver.Free;
  end;
end;

procedure TFilterTests.ExcludeFilter_SetToEmpty_RemovesTheFilter;
var
  receiver : TOwningReceiver;
begin
  receiver := TOwningReceiver.Create(FChannel);
  try
    receiver.Dispatcher.ExcludeFilter := MessageIds([TEST_MSG_OTHER]);
    receiver.Dispatcher.ExcludeFilter := nil;
    Assert.AreEqual(0, integer(Length(receiver.Dispatcher.ExcludeFilter)));
    SendValueAndOther(FChannel);
    Assert.AreEqual(1, receiver.ValueCount);
    Assert.AreEqual(1, receiver.OtherCount);
  finally
    receiver.Free;
  end;
end;

procedure TFilterTests.IncludeFilter_DuplicateIds_AreStoredOnce;
var
  receiver : TOwningReceiver;
begin
  receiver := TOwningReceiver.Create(FChannel);
  try
    receiver.Dispatcher.IncludeFilter := MessageIds([TEST_MSG_VALUE, TEST_MSG_VALUE]);
    Assert.AreEqual(1, integer(Length(receiver.Dispatcher.IncludeFilter)));
    SendValueAndOther(FChannel);
    Assert.AreEqual(1, receiver.ValueCount);
  finally
    receiver.Free;
  end;
end;

procedure TFilterTests.Filters_AreAppliedToPostedMessages;
var
  receiver : TOwningReceiver;
begin
  receiver := TOwningReceiver.Create(FChannel);
  try
    receiver.Dispatcher.ExcludeFilter := MessageIds([TEST_MSG_VALUE]);
    //posted in order, so once the other message arrives the value message has had its chance
    FChannel.Queue.PostMessage<TValueMessage>(TValueMessage.Create(1));
    FChannel.Queue.PostMessage<TOtherMessage>(TOtherMessage.Create(2));
    Assert.IsTrue(WaitUntil(
      function : boolean
      begin
        result := receiver.OtherCount = 1;
      end), 'the message that is not excluded was not delivered');
    Assert.AreEqual(0, receiver.ValueCount, 'an excluded message was delivered');
  finally
    receiver.Free;
  end;
end;

procedure TFilterTests.Filters_OnlyAffectTheirOwnDispatcher;
var
  filtered : TOwningReceiver;
  unfiltered : TOwningReceiver;
begin
  filtered := TOwningReceiver.Create(FChannel);
  unfiltered := TOwningReceiver.Create(FChannel);
  try
    filtered.Dispatcher.IncludeFilter := MessageIds([TEST_MSG_OTHER]);
    SendValueAndOther(FChannel);
    Assert.AreEqual(0, filtered.ValueCount);
    Assert.AreEqual(1, unfiltered.ValueCount);
    Assert.AreEqual(1, unfiltered.OtherCount);
  finally
    unfiltered.Free;
    filtered.Free;
  end;
end;

initialization
  TDUnitX.RegisterTestFixture(TFilterTests);

end.

unit Tests.Channel;

interface

uses
  DUnitX.TestFramework,
  VSoft.Messaging;

type
  [TestFixture]
  TChannelTests = class
  private
    FChannel : IMessageChannel;
    FBaselineDepth : integer;
  public
    [Setup]
    procedure Setup;
    [TearDown]
    procedure TearDown;

    [Test]
    procedure CreateChannel_IsEnabledByDefault;
    [Test]
    procedure Properties_KeepTheirValues;
    [Test]
    procedure PostMessage_IsDeliveredToASubscribedDispatcher;
    [Test]
    procedure PostMessage_IsDeliveredToEveryDispatcher;
    [Test]
    procedure PostMessage_ReturnsBeforeTheHandlerCompletes;
    [Test]
    procedure SendMessage_HandlerHasCompletedWhenItReturns;
    [Test]
    procedure SendMessage_IsDeliveredToEveryDispatcher;
    [Test]
    procedure PostMessage_ChannelDisabled_IsDropped;
    [Test]
    procedure SendMessage_ChannelDisabled_IsDropped;
    [Test]
    procedure Enabled_ReEnabled_DeliveryResumes;
    [Test]
    procedure Subscribe_SameDispatcherTwice_DeliversOnce;
    [Test]
    procedure UnSubscribe_StopsDelivery;
    [Test]
    procedure UnSubscribe_NotSubscribed_DoesNotRaise;
    [Test]
    procedure PostMessage_NoDispatchers_DoesNotRaise;
    [Test]
    procedure Queue_ACopyOfTheHelper_StillDelivers;
    [Test]
    procedure HandlerException_IsReportedToOnError;
    [Test]
    procedure HandlerException_LaterMessagesAreStillDelivered;
    [Test]
    procedure HandlerException_NoOnErrorAssigned_LaterMessagesAreStillDelivered;
    [Test]
    procedure UnhandledMessage_IsIgnoredWithoutError;
    [Test]
    procedure PostMessage_ChangesMadeAfterPostingAreNotSeen;
    [Test]
    procedure PostMessage_DifferentMessageTypes_EachReachesItsHandler;
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

{ TChannelTests }

procedure TChannelTests.Setup;
begin
  ChannelErrors.Clear;
  FBaselineDepth := TMessagingControl.GetGlobalQueueDepth;
  FChannel := TMessageChannelFactory.CreateChannel;
  FChannel.OnError := RecordChannelError;
end;

procedure TChannelTests.TearDown;
begin
  FChannel := nil;
  RestoreGlobalQueueDepth(FBaselineDepth);
end;

procedure TChannelTests.CreateChannel_IsEnabledByDefault;
begin
  Assert.IsTrue(FChannel.Enabled);
end;

procedure TChannelTests.Properties_KeepTheirValues;
var
  errorHandler : TOnErrorMethod;
begin
  FChannel.OwnerID := 'owner';
  Assert.AreEqual('owner', FChannel.OwnerID);

  FChannel.IncludeParentID := true;
  Assert.IsTrue(FChannel.IncludeParentID);
  FChannel.IncludeParentID := false;
  Assert.IsFalse(FChannel.IncludeParentID);

  FChannel.Enabled := false;
  Assert.IsFalse(FChannel.Enabled);
  FChannel.Enabled := true;
  Assert.IsTrue(FChannel.Enabled);

  errorHandler := FChannel.OnError;
  Assert.IsTrue(@errorHandler = @RecordChannelError);
  FChannel.OnError := nil;
  errorHandler := FChannel.OnError;
  Assert.IsFalse(Assigned(errorHandler));
end;

procedure TChannelTests.PostMessage_IsDeliveredToASubscribedDispatcher;
var
  receiver : TOwningReceiver;
  values : TArray<integer>;
begin
  receiver := TOwningReceiver.Create(FChannel);
  try
    FChannel.Queue.PostMessage<TValueMessage>(TValueMessage.Create(42, 'hello'));
    Assert.IsTrue(WaitUntil(
      function : boolean
      begin
        result := receiver.ValueCount = 1;
      end), 'the message was not delivered');
    values := receiver.Values;
    Assert.AreEqual(42, values[0]);
    Assert.AreEqual('hello', receiver.LastText);
  finally
    receiver.Free;
  end;
end;

procedure TChannelTests.PostMessage_IsDeliveredToEveryDispatcher;
var
  first : TOwningReceiver;
  second : TOwningReceiver;
  third : TOwningReceiver;
begin
  first := TOwningReceiver.Create(FChannel);
  second := TOwningReceiver.Create(FChannel);
  third := TOwningReceiver.Create(FChannel);
  try
    FChannel.Queue.PostMessage<TValueMessage>(TValueMessage.Create(1));
    Assert.IsTrue(WaitUntil(
      function : boolean
      begin
        result := (first.ValueCount = 1) and (second.ValueCount = 1) and (third.ValueCount = 1);
      end), 'not every dispatcher received the message');
  finally
    third.Free;
    second.Free;
    first.Free;
  end;
end;

procedure TChannelTests.PostMessage_ReturnsBeforeTheHandlerCompletes;
var
  receiver : TOwningReceiver;
  gate : TEvent;
  handlerDone : boolean;
begin
  gate := TEvent.Create(nil, true, false, '');
  receiver := TOwningReceiver.Create(FChannel);
  try
    handlerDone := false;
    FChannel.Queue.PostMessage<TActionMessage>(TActionMessage.Create(
      procedure
      begin
        gate.WaitFor(cWaitTimeout);
        handlerDone := true;
      end));
    Assert.IsFalse(handlerDone, 'PostMessage waited for the handler');
    gate.SetEvent;
    Assert.IsTrue(WaitUntil(
      function : boolean
      begin
        result := handlerDone;
      end), 'the handler did not run');
  finally
    gate.SetEvent;
    receiver.Free;
    gate.Free;
  end;
end;

procedure TChannelTests.SendMessage_HandlerHasCompletedWhenItReturns;
var
  receiver : TOwningReceiver;
  handlerDone : boolean;
begin
  receiver := TOwningReceiver.Create(FChannel);
  try
    handlerDone := false;
    FChannel.Queue.SendMessage<TActionMessage>(TActionMessage.Create(
      procedure
      begin
        Sleep(50);
        handlerDone := true;
      end));
    Assert.IsTrue(handlerDone, 'SendMessage returned before the handler completed');
  finally
    receiver.Free;
  end;
end;

procedure TChannelTests.SendMessage_IsDeliveredToEveryDispatcher;
var
  first : TOwningReceiver;
  second : TOwningReceiver;
begin
  first := TOwningReceiver.Create(FChannel);
  second := TOwningReceiver.Create(FChannel);
  try
    FChannel.Queue.SendMessage<TValueMessage>(TValueMessage.Create(1));
    Assert.AreEqual(1, first.ValueCount);
    Assert.AreEqual(1, second.ValueCount);
  finally
    second.Free;
    first.Free;
  end;
end;

procedure TChannelTests.PostMessage_ChannelDisabled_IsDropped;
var
  receiver : TOwningReceiver;
begin
  receiver := TOwningReceiver.Create(FChannel);
  try
    FChannel.Enabled := false;
    FChannel.Queue.PostMessage<TValueMessage>(TValueMessage.Create(1));
    PumpFor(100);
    Assert.AreEqual(0, receiver.ValueCount);
  finally
    receiver.Free;
  end;
end;

procedure TChannelTests.SendMessage_ChannelDisabled_IsDropped;
var
  receiver : TOwningReceiver;
begin
  receiver := TOwningReceiver.Create(FChannel);
  try
    FChannel.Enabled := false;
    FChannel.Queue.SendMessage<TValueMessage>(TValueMessage.Create(1));
    Assert.AreEqual(0, receiver.ValueCount);
  finally
    receiver.Free;
  end;
end;

procedure TChannelTests.Enabled_ReEnabled_DeliveryResumes;
var
  receiver : TOwningReceiver;
  values : TArray<integer>;
begin
  receiver := TOwningReceiver.Create(FChannel);
  try
    FChannel.Enabled := false;
    FChannel.Queue.PostMessage<TValueMessage>(TValueMessage.Create(1));
    FChannel.Enabled := true;
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

procedure TChannelTests.Subscribe_SameDispatcherTwice_DeliversOnce;
var
  receiver : TOwningReceiver;
begin
  receiver := TOwningReceiver.Create(FChannel);
  try
    FChannel.Subscribe(receiver.Dispatcher);
    FChannel.Queue.SendMessage<TValueMessage>(TValueMessage.Create(1));
    Assert.AreEqual(1, receiver.ValueCount);
  finally
    receiver.Free;
  end;
end;

procedure TChannelTests.UnSubscribe_StopsDelivery;
var
  receiver : TOwningReceiver;
begin
  receiver := TOwningReceiver.Create(FChannel);
  try
    FChannel.UnSubscribe(receiver.Dispatcher);
    FChannel.Queue.SendMessage<TValueMessage>(TValueMessage.Create(1));
    FChannel.Queue.PostMessage<TValueMessage>(TValueMessage.Create(2));
    PumpFor(100);
    Assert.AreEqual(0, receiver.ValueCount);
  finally
    receiver.Free;
  end;
end;

procedure TChannelTests.UnSubscribe_NotSubscribed_DoesNotRaise;
var
  dispatcher : IMessageDispatcher;
begin
  dispatcher := TMessageDispatcherFactory.CreateDispatcher;
  Assert.WillNotRaise(
    procedure
    begin
      FChannel.UnSubscribe(dispatcher);
    end);
end;

procedure TChannelTests.PostMessage_NoDispatchers_DoesNotRaise;
begin
  Assert.WillNotRaise(
    procedure
    begin
      FChannel.Queue.PostMessage<TValueMessage>(TValueMessage.Create(1));
      FChannel.Queue.SendMessage<TValueMessage>(TValueMessage.Create(2));
    end);
  Assert.AreEqual(FBaselineDepth, TMessagingControl.GetGlobalQueueDepth);
end;

procedure TChannelTests.Queue_ACopyOfTheHelper_StillDelivers;
var
  receiver : TOwningReceiver;
  helper : TChannelHelper;
begin
  receiver := TOwningReceiver.Create(FChannel);
  try
    helper := FChannel.Queue;
    helper.SendMessage<TValueMessage>(TValueMessage.Create(7));
    Assert.AreEqual(1, receiver.ValueCount);
  finally
    receiver.Free;
  end;
end;

procedure TChannelTests.HandlerException_IsReportedToOnError;
var
  receiver : TOwningReceiver;
begin
  receiver := TOwningReceiver.Create(FChannel);
  try
    FChannel.Queue.PostMessage<TRaiseMessage>(TRaiseMessage.Create('boom'));
    Assert.IsTrue(WaitUntil(
      function : boolean
      begin
        result := ChannelErrors.Count = 1;
      end), 'OnError was not called');
    Assert.AreEqual('boom', ChannelErrors.Text);
  finally
    receiver.Free;
  end;
end;

procedure TChannelTests.HandlerException_LaterMessagesAreStillDelivered;
var
  receiver : TOwningReceiver;
begin
  receiver := TOwningReceiver.Create(FChannel);
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

procedure TChannelTests.HandlerException_NoOnErrorAssigned_LaterMessagesAreStillDelivered;
var
  receiver : TOwningReceiver;
begin
  FChannel.OnError := nil;
  receiver := TOwningReceiver.Create(FChannel);
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

procedure TChannelTests.UnhandledMessage_IsIgnoredWithoutError;
var
  receiver : TOwningReceiver;
begin
  receiver := TOwningReceiver.Create(FChannel);
  try
    FChannel.Queue.PostMessage<TUnhandledMessage>(TUnhandledMessage.Create(1));
    FChannel.Queue.PostMessage<TValueMessage>(TValueMessage.Create(2));
    Assert.IsTrue(WaitUntil(
      function : boolean
      begin
        result := receiver.ValueCount = 1;
      end), 'the message after the unhandled one was not delivered');
    Assert.AreEqual(0, ChannelErrors.Count, ChannelErrors.Text);
  finally
    receiver.Free;
  end;
end;

procedure TChannelTests.PostMessage_ChangesMadeAfterPostingAreNotSeen;
var
  receiver : TOwningReceiver;
  gate : TEvent;
  msg : TValueMessage;
  values : TArray<integer>;
begin
  gate := TEvent.Create(nil, true, false, '');
  receiver := TOwningReceiver.Create(FChannel);
  try
    //hold the dispatcher so the message is read after the sender has changed its copy
    FChannel.Queue.PostMessage<TActionMessage>(TActionMessage.Create(
      procedure
      begin
        gate.WaitFor(cWaitTimeout);
      end));
    msg := TValueMessage.Create(1, 'original');
    FChannel.Queue.PostMessage<TValueMessage>(msg);
    msg.Value := 2;
    msg.Text := 'changed';
    gate.SetEvent;
    Assert.IsTrue(WaitUntil(
      function : boolean
      begin
        result := receiver.ValueCount = 1;
      end), 'the message was not delivered');
    values := receiver.Values;
    Assert.AreEqual(1, values[0]);
    Assert.AreEqual('original', receiver.LastText);
  finally
    gate.SetEvent;
    receiver.Free;
    gate.Free;
  end;
end;

procedure TChannelTests.PostMessage_DifferentMessageTypes_EachReachesItsHandler;
var
  receiver : TOwningReceiver;
begin
  receiver := TOwningReceiver.Create(FChannel);
  try
    FChannel.Queue.PostMessage<TValueMessage>(TValueMessage.Create(1));
    FChannel.Queue.PostMessage<TOtherMessage>(TOtherMessage.Create(2));
    FChannel.Queue.PostMessage<TOtherMessage>(TOtherMessage.Create(3));
    FChannel.Queue.PostMessage<TTextMessage>(TTextMessage.Create('text'));
    Assert.IsTrue(WaitUntil(
      function : boolean
      begin
        result := (receiver.ValueCount = 1) and (receiver.OtherCount = 2) and (receiver.TextCount = 1);
      end), 'not every message reached its handler');
    Assert.AreEqual('text', receiver.LastText);
  finally
    receiver.Free;
  end;
end;

initialization
  TDUnitX.RegisterTestFixture(TChannelTests);

end.

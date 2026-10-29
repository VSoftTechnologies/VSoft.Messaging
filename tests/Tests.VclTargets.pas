unit Tests.VclTargets;

interface

uses
  DUnitX.TestFramework,
  Vcl.Forms,
  VSoft.Messaging;

type
  //A form with a window handle as a dispatch target. TWinControl.DefaultHandler treats any
  //message the form has no handler for as a TMessage and writes TMessage.Result into it.
  //Every test sends from the main thread, so dispatch to the form happens on this thread.
  [TestFixture]
  TVclTargetTests = class
  private
    FChannel : IMessageChannel;
    FBaselineDepth : integer;
    FForm : TForm;
    FFormDispatcher : IMessageDispatcher;
  public
    [Setup]
    procedure Setup;
    [TearDown]
    procedure TearDown;

    [Test]
    procedure UnhandledByAWindowedControl_LaterReceiversSeeTheWholePayload;
    [Test]
    procedure UnhandledByAWindowedControl_UIDispatcherSendStaysSynchronous;
  end;

implementation

uses
  System.SysUtils,
  VSoft.Messaging.Internal,
  Testing.Helpers,
  Testing.Messages,
  Testing.Receivers;

{ TVclTargetTests }

procedure TVclTargetTests.Setup;
begin
  ChannelErrors.Clear;
  FBaselineDepth := TMessagingControl.GetGlobalQueueDepth;
  FChannel := TMessageChannelFactory.CreateChannel;
  FChannel.OnError := RecordChannelError;
  FForm := TForm.CreateNew(nil);
  FForm.HandleNeeded;
  //subscribed first, so the form sees each message before the receivers do
  FFormDispatcher := TMessageDispatcherFactory.CreateDispatcher(FForm);
  FFormDispatcher.Channel := FChannel;
end;

procedure TVclTargetTests.TearDown;
begin
  FFormDispatcher.Channel := nil;
  FFormDispatcher := nil;
  FForm.Free;
  FChannel := nil;
  RestoreGlobalQueueDepth(FBaselineDepth);
end;

procedure TVclTargetTests.UnhandledByAWindowedControl_LaterReceiversSeeTheWholePayload;
var
  receiver : TOwningReceiver;
begin
  receiver := TOwningReceiver.Create(FChannel);
  try
    FChannel.Queue.SendMessage<TTwoStringMessage>(TTwoStringMessage.Create('first', 'second'));
    Assert.AreEqual(1, receiver.TwoStringCount, 'the message was not delivered');
    Assert.AreEqual('first', receiver.LastFirst);
    Assert.AreEqual('second', receiver.LastSecond, 'the form''s DefaultHandler wrote TMessage.Result over the message payload (review H1)');
  finally
    receiver.Free;
  end;
end;

procedure TVclTargetTests.UnhandledByAWindowedControl_UIDispatcherSendStaysSynchronous;
var
  receiver : TOwningReceiver;
begin
  receiver := TOwningReceiver.Create(FChannel, true);
  try
    //a send from the main thread to a UI dispatcher created on the main thread is dispatched immediately,
    //unless the thread id stored in the message wrapper has been overwritten
    FChannel.Queue.SendMessage<TTextMessage>(TTextMessage.Create('hello'));
    Assert.AreEqual(1, receiver.TextCount, 'the send to the UI dispatcher became asynchronous after the form ignored the message, its DefaultHandler overwrote the thread id in the message wrapper (review H1)');
    Assert.AreEqual('hello', receiver.LastText);
  finally
    receiver.Free;
  end;
end;

initialization
  TDUnitX.RegisterTestFixture(TVclTargetTests);

end.

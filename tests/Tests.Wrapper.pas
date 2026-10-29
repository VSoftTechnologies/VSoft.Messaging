unit Tests.Wrapper;

interface

uses
  DUnitX.TestFramework;

type
  [TestFixture]
  TMessageWrapperTests = class
  public
    [Test]
    procedure Id_IsTheMessageIdOfTheRecord;
    [Test]
    procedure ThreadID_IsTheThreadThatCreatedIt;
    [Test]
    procedure ThreadID_CreatedOnAnotherThread_IsThatThread;
    [Test]
    procedure MessagePtr_PointsAtACopyOfTheRecord;
    [Test]
    procedure Release_ReleasesManagedFieldsOfTheRecord;
  end;

  [TestFixture]
  TChannelHelperTests = class
  public
    [Test]
    procedure PostMessage_CallsThePostProcOnce;
    [Test]
    procedure SendMessage_CallsTheSendProcOnce;
    [Test]
    procedure PostMessage_PassesAWrappedCopyOfTheMessage;
    [Test]
    procedure PostMessage_NonRecordType_RaisesAssertion;
    [Test]
    procedure SendMessage_NonRecordType_RaisesAssertion;
    [Test]
    procedure Create_NilPostProc_RaisesAssertion;
    [Test]
    procedure Create_NilSendProc_RaisesAssertion;
  end;

implementation

uses
  System.SysUtils,
  System.Classes,
  VSoft.Messaging,
  VSoft.Messaging.Internal,
  Testing.Helpers,
  Testing.Messages,
  Testing.Receivers;

type
  PValueMessage = ^TValueMessage;

  //stands in for the channel, records what the helper passes on
  TProcRecorder = class
  private
    FPostCount : integer;
    FSendCount : integer;
    FLastMessage : IMessage;
  public
    procedure Post(const msg : IMessage);
    procedure Send(const msg : IMessage);
    property PostCount : integer read FPostCount;
    property SendCount : integer read FSendCount;
    property LastMessage : IMessage read FLastMessage;
  end;

{ TProcRecorder }

procedure TProcRecorder.Post(const msg : IMessage);
begin
  Inc(FPostCount);
  FLastMessage := msg;
end;

procedure TProcRecorder.Send(const msg : IMessage);
begin
  Inc(FSendCount);
  FLastMessage := msg;
end;

//creating the wrapper in a separate routine means no compiler temporaries outlive it
procedure WrapPayload(out wrapper : IMessage);
var
  payload : IInterface;
  msg : TPayloadMessage;
begin
  payload := TTrackedPayload.Create;
  msg := TPayloadMessage.Create(payload);
  wrapper := TVSMessageWrapper<TPayloadMessage>.Create(msg);
end;

{ TMessageWrapperTests }

procedure TMessageWrapperTests.Id_IsTheMessageIdOfTheRecord;
var
  wrapper : IMessage;
begin
  wrapper := TVSMessageWrapper<TValueMessage>.Create(TValueMessage.Create(1));
  Assert.AreEqual(Word(TEST_MSG_VALUE), wrapper.Id);
end;

procedure TMessageWrapperTests.ThreadID_IsTheThreadThatCreatedIt;
var
  wrapper : IMessage;
begin
  wrapper := TVSMessageWrapper<TValueMessage>.Create(TValueMessage.Create(1));
  Assert.IsTrue(wrapper.ThreadID = TThread.CurrentThread.ThreadID);
end;

procedure TMessageWrapperTests.ThreadID_CreatedOnAnotherThread_IsThatThread;
var
  wrapper : IMessage;
  creatorThreadId : TThreadID;
  worker : TTestThread;
begin
  creatorThreadId := 0;
  worker := StartThread(
    procedure
    begin
      creatorThreadId := TThread.CurrentThread.ThreadID;
      wrapper := TVSMessageWrapper<TValueMessage>.Create(TValueMessage.Create(1));
    end);
  Assert.IsTrue(FinishThread(worker), 'worker thread did not finish');
  Assert.IsTrue(creatorThreadId <> MainThreadID);
  Assert.IsTrue(wrapper.ThreadID = creatorThreadId);
end;

procedure TMessageWrapperTests.MessagePtr_PointsAtACopyOfTheRecord;
var
  msg : TValueMessage;
  wrapper : IMessage;
begin
  msg := TValueMessage.Create(1, 'original');
  wrapper := TVSMessageWrapper<TValueMessage>.Create(msg);
  msg.Value := 2;
  msg.Text := 'changed';
  Assert.AreEqual(1, PValueMessage(wrapper.MessagePtr)^.Value);
  Assert.AreEqual('original', PValueMessage(wrapper.MessagePtr)^.Text);
  Assert.IsTrue(wrapper.MessagePtr <> @msg);
end;

procedure TMessageWrapperTests.Release_ReleasesManagedFieldsOfTheRecord;
var
  baseline : integer;
  wrapper : IMessage;
begin
  baseline := TTrackedPayload.LiveCount;
  WrapPayload(wrapper);
  Assert.AreEqual(baseline + 1, TTrackedPayload.LiveCount);
  wrapper := nil;
  Assert.AreEqual(baseline, TTrackedPayload.LiveCount, 'the payload was not released with the wrapper');
end;

{ TChannelHelperTests }

procedure TChannelHelperTests.PostMessage_CallsThePostProcOnce;
var
  recorder : TProcRecorder;
  helper : TChannelHelper;
begin
  recorder := TProcRecorder.Create;
  try
    helper := TChannelHelper.Create(recorder.Post, recorder.Send);
    helper.PostMessage<TValueMessage>(TValueMessage.Create(1));
    Assert.AreEqual(1, recorder.PostCount);
    Assert.AreEqual(0, recorder.SendCount);
  finally
    recorder.Free;
  end;
end;

procedure TChannelHelperTests.SendMessage_CallsTheSendProcOnce;
var
  recorder : TProcRecorder;
  helper : TChannelHelper;
begin
  recorder := TProcRecorder.Create;
  try
    helper := TChannelHelper.Create(recorder.Post, recorder.Send);
    helper.SendMessage<TValueMessage>(TValueMessage.Create(1));
    Assert.AreEqual(0, recorder.PostCount);
    Assert.AreEqual(1, recorder.SendCount);
  finally
    recorder.Free;
  end;
end;

procedure TChannelHelperTests.PostMessage_PassesAWrappedCopyOfTheMessage;
var
  recorder : TProcRecorder;
  helper : TChannelHelper;
  msg : TValueMessage;
begin
  recorder := TProcRecorder.Create;
  try
    helper := TChannelHelper.Create(recorder.Post, recorder.Send);
    msg := TValueMessage.Create(42, 'hello');
    helper.PostMessage<TValueMessage>(msg);
    Assert.IsTrue(recorder.LastMessage <> nil);
    Assert.AreEqual(Word(TEST_MSG_VALUE), recorder.LastMessage.Id);
    Assert.AreEqual(42, PValueMessage(recorder.LastMessage.MessagePtr)^.Value);
    Assert.AreEqual('hello', PValueMessage(recorder.LastMessage.MessagePtr)^.Text);
    Assert.IsTrue(recorder.LastMessage.ThreadID = TThread.CurrentThread.ThreadID);
  finally
    recorder.Free;
  end;
end;

procedure TChannelHelperTests.PostMessage_NonRecordType_RaisesAssertion;
var
  recorder : TProcRecorder;
  helper : TChannelHelper;
begin
  recorder := TProcRecorder.Create;
  try
    helper := TChannelHelper.Create(recorder.Post, recorder.Send);
    Assert.WillRaise(
      procedure
      begin
        helper.PostMessage<integer>(42);
      end, EAssertionFailed);
    Assert.AreEqual(0, recorder.PostCount);
  finally
    recorder.Free;
  end;
end;

procedure TChannelHelperTests.SendMessage_NonRecordType_RaisesAssertion;
var
  recorder : TProcRecorder;
  helper : TChannelHelper;
begin
  recorder := TProcRecorder.Create;
  try
    helper := TChannelHelper.Create(recorder.Post, recorder.Send);
    Assert.WillRaise(
      procedure
      begin
        helper.SendMessage<string>('not a record');
      end, EAssertionFailed);
    Assert.AreEqual(0, recorder.SendCount);
  finally
    recorder.Free;
  end;
end;

procedure TChannelHelperTests.Create_NilPostProc_RaisesAssertion;
var
  recorder : TProcRecorder;
  helper : TChannelHelper;
begin
  recorder := TProcRecorder.Create;
  try
    Assert.WillRaise(
      procedure
      begin
        helper := TChannelHelper.Create(nil, recorder.Send);
      end, EAssertionFailed);
  finally
    recorder.Free;
  end;
end;

procedure TChannelHelperTests.Create_NilSendProc_RaisesAssertion;
var
  recorder : TProcRecorder;
  helper : TChannelHelper;
begin
  recorder := TProcRecorder.Create;
  try
    Assert.WillRaise(
      procedure
      begin
        helper := TChannelHelper.Create(recorder.Post, nil);
      end, EAssertionFailed);
  finally
    recorder.Free;
  end;
end;

initialization
  TDUnitX.RegisterTestFixture(TMessageWrapperTests);
  TDUnitX.RegisterTestFixture(TChannelHelperTests);

end.

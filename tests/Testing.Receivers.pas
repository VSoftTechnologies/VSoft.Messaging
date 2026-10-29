unit Testing.Receivers;

interface

uses
  System.SysUtils,
  System.Classes,
  System.SyncObjs,
  System.Generics.Collections,
  VSoft.Messaging,
  VSoft.Messaging.Channel,
  VSoft.Messaging.Dispatchers,
  Testing.Helpers,
  Testing.Messages;

type
  //Records everything it receives. Handlers run on dispatcher threads, so all state is locked.
  TTestReceiver = class
  private
    FLock : TCriticalSection;
    FName : string;
    FSharedLog : TTestLog;
    FValues : TList<integer>;
    FThreadIds : TList<TThreadID>;
    FLastText : string;
    FOtherCount : integer;
    FPayloadCount : integer;
    FActionCount : integer;
    FTextCount : integer;
    FTwoStringCount : integer;
    FLastFirst : string;
    FLastSecond : string;
    FActiveHandlers : integer;
    FMaxActiveHandlers : integer;
    FOnDestroyed : TProc;
    procedure EnterHandler;
    procedure LeaveHandler;
  protected
    procedure HandleValue(var msg : TValueMessage); message TEST_MSG_VALUE;
    procedure HandleOther(var msg : TOtherMessage); message TEST_MSG_OTHER;
    procedure HandlePayload(var msg : TPayloadMessage); message TEST_MSG_PAYLOAD;
    procedure HandleAction(var msg : TActionMessage); message TEST_MSG_ACTION;
    procedure HandleRawAction(var msg : TRawActionMessage); message TEST_MSG_RAW_ACTION;
    procedure HandleFreeSelf(var msg : TFreeSelfMessage); message TEST_MSG_FREE_SELF;
    procedure HandleTwoStrings(var msg : TTwoStringMessage); message TEST_MSG_TWO_STRINGS;
    procedure HandleText(var msg : TTextMessage); message TEST_MSG_TEXT;
    procedure HandleRaise(var msg : TRaiseMessage); message TEST_MSG_RAISE;
    procedure HandleRaiseObject(var msg : TRaiseObjectMessage); message TEST_MSG_RAISE_OBJECT;
  public
    //sharedLog is optional, when set the receiver adds its name to it for every value message
    constructor Create(const receiverName : string = ''; const sharedLog : TTestLog = nil);
    destructor Destroy; override;

    function Values : TArray<integer>;
    function ValueCount : integer;
    function LastText : string;
    function OtherCount : integer;
    function PayloadCount : integer;
    function ActionCount : integer;
    function TextCount : integer;
    function TwoStringCount : integer;
    function LastFirst : string;
    function LastSecond : string;
    //the thread each handler ran on, in the order they ran
    function ThreadIds : TArray<TThreadID>;
    //the most handlers that were running on this receiver at the same time
    function MaxActiveHandlers : integer;

    property Name : string read FName;
    //called at the end of the destructor, once the receiver has torn down its dispatcher
    property OnDestroyed : TProc read FOnDestroyed write FOnDestroyed;
  end;

  //Owns its dispatcher and tears it down in its destructor the way the demos do.
  TOwningReceiver = class(TTestReceiver)
  private
    FDispatcher : IMessageDispatcher;
  public
    constructor Create(const channel : IMessageChannel; const useUIDispatcher : boolean = false; const receiverName : string = ''; const sharedLog : TTestLog = nil);
    destructor Destroy; override;
    property Dispatcher : IMessageDispatcher read FDispatcher;
  end;

  //counts live instances so tests can see when message payloads are released
  TTrackedPayload = class(TInterfacedObject)
  private
  class var
    FLiveCount : integer;
  public
    constructor Create;
    destructor Destroy; override;
    class function LiveCount : integer;
  end;

  //counts live instances so tests can see when a dispatcher is freed
  TTrackedDispatcher = class(TThreadedMessageDispatcher)
  private
  class var
    FLiveCount : integer;
  public
    constructor Create(const target : TObject = nil); override;
    destructor Destroy; override;
    class function LiveCount : integer;
  end;

  //counts live instances so tests can see when a channel is freed
  TTrackedChannel = class(TVSoftMessageChannel)
  private
  class var
    FLiveCount : integer;
  public
    constructor Create;
    destructor Destroy; override;
    class function LiveCount : integer;
  end;

implementation

{ TTestReceiver }

constructor TTestReceiver.Create(const receiverName : string; const sharedLog : TTestLog);
begin
  inherited Create;
  FName := receiverName;
  FSharedLog := sharedLog;
  FLock := TCriticalSection.Create;
  FValues := TList<integer>.Create;
  FThreadIds := TList<TThreadID>.Create;
end;

destructor TTestReceiver.Destroy;
begin
  if Assigned(FOnDestroyed) then
    FOnDestroyed();
  FThreadIds.Free;
  FValues.Free;
  FLock.Free;
  inherited;
end;

procedure TTestReceiver.EnterHandler;
begin
  FLock.Acquire;
  try
    FThreadIds.Add(TThread.CurrentThread.ThreadID);
    Inc(FActiveHandlers);
    if FActiveHandlers > FMaxActiveHandlers then
      FMaxActiveHandlers := FActiveHandlers;
  finally
    FLock.Release;
  end;
end;

procedure TTestReceiver.LeaveHandler;
begin
  FLock.Acquire;
  try
    Dec(FActiveHandlers);
  finally
    FLock.Release;
  end;
end;

procedure TTestReceiver.HandleValue(var msg : TValueMessage);
begin
  EnterHandler;
  try
    FLock.Acquire;
    try
      FValues.Add(msg.Value);
      FLastText := msg.Text;
    finally
      FLock.Release;
    end;
    if FSharedLog <> nil then
      FSharedLog.Add(FName);
  finally
    LeaveHandler;
  end;
end;

procedure TTestReceiver.HandleOther(var msg : TOtherMessage);
begin
  EnterHandler;
  try
    TInterlocked.Increment(FOtherCount);
  finally
    LeaveHandler;
  end;
end;

procedure TTestReceiver.HandlePayload(var msg : TPayloadMessage);
begin
  EnterHandler;
  try
    TInterlocked.Increment(FPayloadCount);
  finally
    LeaveHandler;
  end;
end;

procedure TTestReceiver.HandleAction(var msg : TActionMessage);
begin
  EnterHandler;
  try
    if Assigned(msg.Action) then
      msg.Action();
    TInterlocked.Increment(FActionCount);
  finally
    LeaveHandler;
  end;
end;

procedure TTestReceiver.HandleRawAction(var msg : TRawActionMessage);
begin
  //must not touch Self after the action, it may have freed the receiver
  msg.Action();
end;

procedure TTestReceiver.HandleFreeSelf(var msg : TFreeSelfMessage);
begin
  Free;
end;

procedure TTestReceiver.HandleTwoStrings(var msg : TTwoStringMessage);
begin
  EnterHandler;
  try
    FLock.Acquire;
    try
      FLastFirst := msg.First;
      FLastSecond := msg.Second;
      Inc(FTwoStringCount);
    finally
      FLock.Release;
    end;
  finally
    LeaveHandler;
  end;
end;

procedure TTestReceiver.HandleText(var msg : TTextMessage);
begin
  EnterHandler;
  try
    FLock.Acquire;
    try
      FLastText := msg.Text;
      Inc(FTextCount);
    finally
      FLock.Release;
    end;
  finally
    LeaveHandler;
  end;
end;

procedure TTestReceiver.HandleRaise(var msg : TRaiseMessage);
begin
  EnterHandler;
  try
    raise Exception.Create(msg.Text);
  finally
    LeaveHandler;
  end;
end;

procedure TTestReceiver.HandleRaiseObject(var msg : TRaiseObjectMessage);
begin
  EnterHandler;
  try
    raise TObject.Create;
  finally
    LeaveHandler;
  end;
end;

function TTestReceiver.Values : TArray<integer>;
begin
  FLock.Acquire;
  try
    result := FValues.ToArray;
  finally
    FLock.Release;
  end;
end;

function TTestReceiver.ValueCount : integer;
begin
  FLock.Acquire;
  try
    result := FValues.Count;
  finally
    FLock.Release;
  end;
end;

function TTestReceiver.LastText : string;
begin
  FLock.Acquire;
  try
    result := FLastText;
  finally
    FLock.Release;
  end;
end;

function TTestReceiver.OtherCount : integer;
begin
  result := TInterlocked.CompareExchange(FOtherCount, 0, 0);
end;

function TTestReceiver.PayloadCount : integer;
begin
  result := TInterlocked.CompareExchange(FPayloadCount, 0, 0);
end;

function TTestReceiver.ActionCount : integer;
begin
  result := TInterlocked.CompareExchange(FActionCount, 0, 0);
end;

function TTestReceiver.TextCount : integer;
begin
  FLock.Acquire;
  try
    result := FTextCount;
  finally
    FLock.Release;
  end;
end;

function TTestReceiver.TwoStringCount : integer;
begin
  FLock.Acquire;
  try
    result := FTwoStringCount;
  finally
    FLock.Release;
  end;
end;

function TTestReceiver.LastFirst : string;
begin
  FLock.Acquire;
  try
    result := FLastFirst;
  finally
    FLock.Release;
  end;
end;

function TTestReceiver.LastSecond : string;
begin
  FLock.Acquire;
  try
    result := FLastSecond;
  finally
    FLock.Release;
  end;
end;

function TTestReceiver.ThreadIds : TArray<TThreadID>;
begin
  FLock.Acquire;
  try
    result := FThreadIds.ToArray;
  finally
    FLock.Release;
  end;
end;

function TTestReceiver.MaxActiveHandlers : integer;
begin
  FLock.Acquire;
  try
    result := FMaxActiveHandlers;
  finally
    FLock.Release;
  end;
end;

{ TOwningReceiver }

constructor TOwningReceiver.Create(const channel : IMessageChannel; const useUIDispatcher : boolean; const receiverName : string; const sharedLog : TTestLog);
begin
  inherited Create(receiverName, sharedLog);
  if useUIDispatcher then
    FDispatcher := TMessageDispatcherFactory.CreateUIDispatcher(Self)
  else
    FDispatcher := TMessageDispatcherFactory.CreateDispatcher(Self);
  FDispatcher.Channel := channel;
end;

destructor TOwningReceiver.Destroy;
begin
  if FDispatcher <> nil then
    FDispatcher.Channel := nil;
  FDispatcher := nil;
  inherited;
end;

{ TTrackedPayload }

constructor TTrackedPayload.Create;
begin
  inherited Create;
  TInterlocked.Increment(FLiveCount);
end;

destructor TTrackedPayload.Destroy;
begin
  TInterlocked.Decrement(FLiveCount);
  inherited;
end;

class function TTrackedPayload.LiveCount : integer;
begin
  result := TInterlocked.CompareExchange(FLiveCount, 0, 0);
end;

{ TTrackedDispatcher }

constructor TTrackedDispatcher.Create(const target : TObject);
begin
  inherited Create(target);
  TInterlocked.Increment(FLiveCount);
end;

destructor TTrackedDispatcher.Destroy;
begin
  TInterlocked.Decrement(FLiveCount);
  inherited;
end;

class function TTrackedDispatcher.LiveCount : integer;
begin
  result := TInterlocked.CompareExchange(FLiveCount, 0, 0);
end;

{ TTrackedChannel }

constructor TTrackedChannel.Create;
begin
  inherited Create;
  TInterlocked.Increment(FLiveCount);
end;

destructor TTrackedChannel.Destroy;
begin
  TInterlocked.Decrement(FLiveCount);
  inherited;
end;

class function TTrackedChannel.LiveCount : integer;
begin
  result := TInterlocked.CompareExchange(FLiveCount, 0, 0);
end;

end.

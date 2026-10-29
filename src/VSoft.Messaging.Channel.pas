{***************************************************************************}
{                                                                           }
{           VSoft.Messaging                                                 }
{                                                                           }
{           Copyright (C) 2010 Vincent Parrett                              }
{                                                                           }
{           vincent@finalbuilder.com                                        }
{           https://www.finalbuilder.com                                    }
{                                                                           }
{***************************************************************************}
{                                                                           }
{  Licensed under the Apache License, Version 2.0 (the "License");          }
{  you may not use this file except in compliance with the License.         }
{  You may obtain a copy of the License at                                  }
{                                                                           }
{      http://www.apache.org/licenses/LICENSE-2.0                           }
{                                                                           }
{  Unless required by applicable law or agreed to in writing, software      }
{  distributed under the License is distributed on an "AS IS" BASIS,        }
{  WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied. }
{  See the License for the specific language governing permissions and      }
{  limitations under the License.                                           }
{                                                                           }
{***************************************************************************}

unit VSoft.Messaging.Channel;

interface

uses
  System.SysUtils,
  System.Classes,
  System.SyncObjs,
  System.Generics.Collections,
  VSoft.CancellationToken,
  VSoft.Messaging.Internal,
  VSoft.Messaging;

type
  //this is used by a channel proxy in FinalBuilder.
  IMessageChannelPost = interface
  ['{63A6480D-67E5-42C3-853B-2E6843715BC4}']
    procedure PostMessage(const message : IMessage);
  end;

  //dkPost : counted in the channel depth and throttled by backpressure
  //dkSend : SendMessage to a single dispatcher, the sender waits for it
  //dkBarrier : WaitUntilIdle, no message, completes when the dispatcher reaches it
  TDeliveryKind = (dkPost, dkSend, dkBarrier);

  //One message on its way to the dispatchers that accepted it. It completes when every one of them
  //has called Done, which gives back its accounting and releases the message.
  IDelivery = interface
  ['{0B0D4C49-2D5B-4C34-9C0E-3F2D7A5E6B11}']
    function GetKind : TDeliveryKind;
    function GetMessageId : Word;
    function GetMessagePtr : Pointer;
    //the TChannelState it was posted on, nil for barriers
    function GetStatePtr : Pointer;
    function GetDoneEvent : TEvent;
    procedure AddPending;
    procedure Done;
    procedure ReportError(const exceptionClassName : string; const errorMessage : string);
    property Kind : TDeliveryKind read GetKind;
    property MessageId : Word read GetMessageId;
    property MessagePtr : Pointer read GetMessagePtr;
    property StatePtr : Pointer read GetStatePtr;
    property DoneEvent : TEvent read GetDoneEvent;
  end;

  IChannelState = interface;

  //the part of a dispatcher that channels deliver to
  IDispatcherCore = interface
  ['{5F3A9E2C-7B41-4D8E-A6C2-1E9B8D0F4A37}']
    //true if a message with this id would be queued now
    function Accepts(const messageId : Word) : boolean;
    //queues the delivery, adding to its pending count, if this dispatcher accepts it
    function TryEnqueue(const delivery : IDelivery) : boolean;
    //handles the message now, on the calling thread. Only used when the calling thread is already
    //running one of this dispatcher's handlers, so waiting for the queue would deadlock.
    procedure DispatchInline(const delivery : IDelivery);
    //the thread running one of this dispatcher's handlers, 0 if none is running
    function GetDispatchThread : TThreadID;
    function GetCoreObject : Pointer;
    procedure AddedToChannel(const state : IChannelState);
    procedure RemovedFromChannel(const state : IChannelState);
  end;

  //implemented by dispatchers, lets a channel find the core to deliver to
  IDispatcherCoreProvider = interface
  ['{9D1C7B5E-3A2F-4E6D-8B4C-2F7E1A9D5C63}']
    function GetCore : IDispatcherCore;
  end;

  IChannelState = interface
  ['{2E8B4F1D-6C3A-4B9E-9D5F-7A1C3E8B2D46}']
    procedure RemoveSubscriber(const core : IDispatcherCore);
  end;

  IPushbackGate = interface
  ['{C4A1E8B2-5D7F-4C3E-8A9B-6E2D1F7C3B58}']
    procedure Open;
    function IsOpen : boolean;
    function GetEvent : TEvent;
  end;

  TFlushWaiter = class
  private
    FEpoch : Int64;
    FRemaining : integer;
    FEvent : TEvent;
  public
    constructor Create(const epoch : Int64; const remaining : integer);
    destructor Destroy; override;
  end;

  //Everything about a channel that deliveries and dispatchers need. Kept separate from
  //TVSoftMessageChannel so nothing the library holds on to keeps the channel object alive.
  TChannelState = class(TInterfacedObject, IChannelState, IChannelQueue)
  private
    FLock : TCriticalSection;
    FSubscribers : TArray<IDispatcherCore>; //copy on write
    FClosed : boolean;
    FEnabled : boolean;
    FOnError : TOnErrorMethod;
    FOnErrorEx : TMessagingErrorProc;

    FDepth : integer;
    FPeakDepth : integer;
    FPostCount : Int64;
    FWaitCount : Int64;
    FTimeoutCount : Int64;
    FTotalBlockedMs : Int64;
    FLongestBlockedMs : Int64;

    FHasThresholds : boolean;
    FBlockedThreshold : integer;
    FUnblockThreshold : integer;
    FBackpressureEnabled : boolean;
    FCancellation : ICancellationToken;
    FPushbackTimeout : Cardinal;
    FWaitNotifyAfter : Cardinal;
    FOnPushbackWait : TPushbackNotifyProc;
    FOnPushbackTimeout : TPushbackNotifyProc;
    FGate : IPushbackGate;

    FEpoch : Int64;
    FFlushWaiters : TList<TFlushWaiter>;

    procedure GetThresholdsLocked(out blocked : integer; out unblock : integer);
    procedure CheckGateLocked;
    function GetSubscribers : TArray<IDispatcherCore>;
    function AnyAccepts(const subscribers : TArray<IDispatcherCore>; const messageId : Word) : boolean;
    procedure WaitForCapacity;
    function IsCancelledLocked : boolean;
    procedure WaitForSend(const delivery : IDelivery; const core : IDispatcherCore);
  protected
    //IChannelQueue
    procedure PostMessage(const message : IMessage);
    procedure SendMessage(const message : IMessage);
    //IChannelState
    procedure RemoveSubscriber(const core : IDispatcherCore);
  public
    constructor Create;
    destructor Destroy; override;
    //removes every subscriber, called when the channel object is destroyed
    procedure Close;

    procedure AddSubscriber(const core : IDispatcherCore);
    procedure DeliveryCreated(out epoch : Int64);
    procedure DeliveryCompleted(const epoch : Int64);
    //reports to OnError and OnErrorEx, an exception raised by either is swallowed
    procedure ReportError(const exceptionClassName : string; const errorMessage : string; const messageId : Word);
    //IMessageChannel.HandleError : only calls OnError, and lets its exceptions through as it always has
    procedure HandleError(const errorMessage : string);

    function GetEnabled : boolean;
    procedure SetEnabled(const value : boolean);
    function GetOnError : TOnErrorMethod;
    procedure SetOnError(const value : TOnErrorMethod);
    function GetOnErrorEx : TMessagingErrorProc;
    procedure SetOnErrorEx(const value : TMessagingErrorProc);
    function GetBlockedThreshold : integer;
    function GetUnblockThreshold : integer;
    procedure SetThresholds(const blocked : integer; const unblock : integer);
    procedure ResetThresholds;
    function GetBackpressureEnabled : boolean;
    procedure SetBackpressureEnabled(const value : boolean);
    function GetPushbackCancellation : ICancellationToken;
    procedure SetPushbackCancellation(const value : ICancellationToken);
    function GetPushbackTimeout : Cardinal;
    procedure SetPushbackTimeout(const value : Cardinal);
    function GetPushbackWaitNotifyAfter : Cardinal;
    procedure SetPushbackWaitNotifyAfter(const value : Cardinal);
    function GetOnPushbackWait : TPushbackNotifyProc;
    procedure SetOnPushbackWait(const value : TPushbackNotifyProc);
    function GetOnPushbackTimeout : TPushbackNotifyProc;
    procedure SetOnPushbackTimeout(const value : TPushbackNotifyProc);
    function GetStats : TChannelStats;
    procedure ReleaseWaiters;
    function Flush(const timeoutMs : Cardinal) : boolean;
  end;

  TVSoftMessageChannel = class(TInterfacedObject, IMessageChannel, IMessageChannelPost)
  private
    FState : TChannelState;
    FStateRef : IChannelState;
    FQueue : IChannelQueue;
    FOwnerID : string;
    FIncludeParentID : boolean;
  protected
    function GetOnError : TOnErrorMethod;
    procedure SetOnError(const value : TOnErrorMethod);
    function GetOnErrorEx : TMessagingErrorProc;
    procedure SetOnErrorEx(const value : TMessagingErrorProc);

    function GetOwnerID: string;
    procedure SetOwnerID(const Value: string);

    function GetEnabled: boolean;
    procedure SetEnabled(const Value: boolean);
    function GetIncludeParentID: boolean;
    procedure SetIncludeParentID(const Value: boolean);
    function GetQueue : TChannelHelper;

    function GetBlockedThreshold : integer;
    function GetUnblockThreshold : integer;
    function GetBackpressureEnabled : boolean;
    procedure SetBackpressureEnabled(const value : boolean);
    function GetPushbackCancellation : ICancellationToken;
    procedure SetPushbackCancellation(const value : ICancellationToken);
    function GetPushbackTimeout : Cardinal;
    procedure SetPushbackTimeout(const value : Cardinal);
    function GetPushbackWaitNotifyAfter : Cardinal;
    procedure SetPushbackWaitNotifyAfter(const value : Cardinal);
    function GetOnPushbackWait : TPushbackNotifyProc;
    procedure SetOnPushbackWait(const value : TPushbackNotifyProc);
    function GetOnPushbackTimeout : TPushbackNotifyProc;
    procedure SetOnPushbackTimeout(const value : TPushbackNotifyProc);
    function GetStats : TChannelStats;

    procedure PostMessage(const message : IMessage);
    procedure SendMessage(const message : IMessage);

    procedure Subscribe(const value: IMessageDispatcher);
    procedure UnSubscribe(const value: IMessageDispatcher);

    procedure HandleError(const errorMessage : string);

    procedure SetThresholds(const blocked : integer; const unblock : integer);
    procedure ResetThresholds;
    procedure ReleaseWaiters;
    function Flush(const timeoutMs : Cardinal = cWaitForever) : boolean;
  public
    constructor Create;
    destructor Destroy; override;
  end;

  //Tracks which dispatchers the current thread is running handlers for. Frames live on the stack
  //of the dispatching code, the thread var only holds raw pointers.
  PDispatchFrame = ^TDispatchFrame;
  TDispatchFrame = record
    State : Pointer;
    Core : Pointer;
    Prev : PDispatchFrame;
  end;

procedure PushDispatchFrame(var frame : TDispatchFrame; const state : Pointer; const core : Pointer);
procedure PopDispatchFrame(var frame : TDispatchFrame);
//how many handlers of this core the current thread is running (more than 1 when they are nested)
function CountDispatchFrames(const core : Pointer) : integer;

//a barrier for IMessageDispatcher.WaitUntilIdle, holding one pending count for the caller
function CreateBarrierDelivery : IDelivery;

implementation

uses
  {$IFDEF MSWINDOWS}
  Winapi.Windows,
  {$ENDIF}
  System.Diagnostics,
  VSoft.Messaging.Dispatchers;

const
  //blocked posters re-check the backlog this often, so a missed wake up only costs one slice
  cPushbackSliceMs = 50;

type
  TDelivery = class(TInterfacedObject, IDelivery)
  private
    FKind : TDeliveryKind;
    FState : TChannelState;
    FStateRef : IChannelState;
    FMessage : IMessage;
    FMessageId : Word;
    FMessagePtr : Pointer;
    FEpoch : Int64;
    FPending : integer;
    FDoneEvent : TEvent;
  protected
    function GetKind : TDeliveryKind;
    function GetMessageId : Word;
    function GetMessagePtr : Pointer;
    function GetStatePtr : Pointer;
    function GetDoneEvent : TEvent;
    procedure AddPending;
    procedure Done;
    procedure ReportError(const exceptionClassName : string; const errorMessage : string);
  public
    //starts with one pending count, held by the creator until it has handed the delivery out
    constructor Create(const kind : TDeliveryKind; const state : TChannelState; const message : IMessage);
    destructor Destroy; override;
  end;

  TPushbackGate = class(TInterfacedObject, IPushbackGate)
  private
    FEvent : TEvent;
  protected
    procedure Open;
    function IsOpen : boolean;
    function GetEvent : TEvent;
  public
    constructor Create;
    destructor Destroy; override;
  end;

threadvar
  GDispatchFrame : PDispatchFrame;

var
  //SendMessage cycle detection : which dispatcher each sending thread is waiting on
  GSendWaitLock : TCriticalSection;
  GSendWaits : TDictionary<TThreadID, IDispatcherCore>;

procedure PushDispatchFrame(var frame : TDispatchFrame; const state : Pointer; const core : Pointer);
begin
  frame.State := state;
  frame.Core := core;
  frame.Prev := GDispatchFrame;
  GDispatchFrame := @frame;
end;

procedure PopDispatchFrame(var frame : TDispatchFrame);
begin
  GDispatchFrame := frame.Prev;
end;

function CountDispatchFrames(const core : Pointer) : integer;
var
  frame : PDispatchFrame;
begin
  result := 0;
  frame := GDispatchFrame;
  while frame <> nil do
  begin
    if frame.Core = core then
      Inc(result);
    frame := frame.Prev;
  end;
end;

function IsDispatchingState(const state : Pointer) : boolean;
var
  frame : PDispatchFrame;
begin
  result := false;
  frame := GDispatchFrame;
  while frame <> nil do
  begin
    if frame.State = state then
      exit(true);
    frame := frame.Prev;
  end;
end;

function IsDispatchingAnyOf(const cores : TArray<IDispatcherCore>) : boolean;
var
  frame : PDispatchFrame;
  i : integer;
begin
  result := false;
  frame := GDispatchFrame;
  while frame <> nil do
  begin
    for i := 0 to High(cores) do
    begin
      if cores[i].GetCoreObject = frame.Core then
        exit(true);
    end;
    frame := frame.Prev;
  end;
end;

function CreateBarrierDelivery : IDelivery;
begin
  result := TDelivery.Create(dkBarrier, nil, nil);
end;

//Registers that this thread is about to wait on core, unless that would complete a cycle of threads
//waiting on each other's dispatchers, in which case it returns false and registers nothing.
function TryRegisterSendWait(const core : IDispatcherCore) : boolean;
var
  me : TThreadID;
  current : IDispatcherCore;
  dispatchThread : TThreadID;
  i : integer;
begin
  result := true;
  me := CurrentThreadId;
  GSendWaitLock.Acquire;
  try
    current := core;
    for i := 1 to 64 do
    begin
      dispatchThread := current.GetDispatchThread;
      if dispatchThread = 0 then
        break;
      if dispatchThread = me then
        exit(false);
      if not GSendWaits.TryGetValue(dispatchThread, current) then
        break;
    end;
    GSendWaits.AddOrSetValue(me, core);
  finally
    GSendWaitLock.Release;
  end;
end;

procedure UnregisterSendWait;
begin
  GSendWaitLock.Acquire;
  try
    GSendWaits.Remove(CurrentThreadId);
  finally
    GSendWaitLock.Release;
  end;
end;

{ TDelivery }

constructor TDelivery.Create(const kind : TDeliveryKind; const state : TChannelState; const message : IMessage);
begin
  inherited Create;
  FKind := kind;
  FState := state;
  FStateRef := state;
  FMessage := message;
  if message <> nil then
  begin
    FMessageId := message.Id;
    FMessagePtr := message.MessagePtr;
  end;
  FPending := 1;
  if kind <> dkPost then
    FDoneEvent := TEvent.Create(nil, true, false, '');
  if (kind = dkPost) and (state <> nil) then
    state.DeliveryCreated(FEpoch);
end;

destructor TDelivery.Destroy;
begin
  FDoneEvent.Free;
  inherited;
end;

procedure TDelivery.AddPending;
begin
  TInterlocked.Increment(FPending);
end;

procedure TDelivery.Done;
begin
  if TInterlocked.Decrement(FPending) <> 0 then
    exit;
  //release the payload before anyone waiting on this delivery wakes up
  FMessage := nil;
  FMessagePtr := nil;
  if (FKind = dkPost) and (FState <> nil) then
    FState.DeliveryCompleted(FEpoch);
  if FDoneEvent <> nil then
    FDoneEvent.SetEvent;
end;

function TDelivery.GetDoneEvent : TEvent;
begin
  result := FDoneEvent;
end;

function TDelivery.GetKind : TDeliveryKind;
begin
  result := FKind;
end;

function TDelivery.GetMessageId : Word;
begin
  result := FMessageId;
end;

function TDelivery.GetMessagePtr : Pointer;
begin
  result := FMessagePtr;
end;

function TDelivery.GetStatePtr : Pointer;
begin
  result := FState;
end;

procedure TDelivery.ReportError(const exceptionClassName : string; const errorMessage : string);
begin
  if FState <> nil then
    FState.ReportError(exceptionClassName, errorMessage, FMessageId);
end;

{ TPushbackGate }

constructor TPushbackGate.Create;
begin
  inherited Create;
  FEvent := TEvent.Create(nil, true, false, '');
end;

destructor TPushbackGate.Destroy;
begin
  FEvent.Free;
  inherited;
end;

function TPushbackGate.GetEvent : TEvent;
begin
  result := FEvent;
end;

function TPushbackGate.IsOpen : boolean;
begin
  result := FEvent.WaitFor(0) = wrSignaled;
end;

procedure TPushbackGate.Open;
begin
  FEvent.SetEvent;
end;

{ TFlushWaiter }

constructor TFlushWaiter.Create(const epoch : Int64; const remaining : integer);
begin
  inherited Create;
  FEpoch := epoch;
  FRemaining := remaining;
  FEvent := TEvent.Create(nil, true, false, '');
end;

destructor TFlushWaiter.Destroy;
begin
  FEvent.Free;
  inherited;
end;

{ TChannelState }

constructor TChannelState.Create;
begin
  inherited Create;
  FLock := TCriticalSection.Create;
  FFlushWaiters := TList<TFlushWaiter>.Create;
  FEnabled := true;
  FBackpressureEnabled := true;
  FPushbackTimeout := cDefaultPushbackTimeout;
  FWaitNotifyAfter := cWaitForever;
end;

destructor TChannelState.Destroy;
begin
  FFlushWaiters.Free;
  FLock.Free;
  inherited;
end;

procedure TChannelState.Close;
var
  subscribers : TArray<IDispatcherCore>;
  i : integer;
begin
  FLock.Acquire;
  try
    FClosed := true;
    subscribers := FSubscribers;
    FSubscribers := nil;
    if FGate <> nil then
    begin
      FGate.Open;
      FGate := nil;
    end;
  finally
    FLock.Release;
  end;
  for i := 0 to High(subscribers) do
    subscribers[i].RemovedFromChannel(Self);
end;

procedure TChannelState.AddSubscriber(const core : IDispatcherCore);
var
  i : integer;
  count : integer;
  newSubscribers : TArray<IDispatcherCore>;
begin
  FLock.Acquire;
  try
    if FClosed then
      exit;
    count := Length(FSubscribers);
    for i := 0 to count - 1 do
    begin
      if FSubscribers[i] = core then
        exit;
    end;
    SetLength(newSubscribers, count + 1);
    for i := 0 to count - 1 do
      newSubscribers[i] := FSubscribers[i];
    newSubscribers[count] := core;
    FSubscribers := newSubscribers;
  finally
    FLock.Release;
  end;
  core.AddedToChannel(Self);
end;

procedure TChannelState.RemoveSubscriber(const core : IDispatcherCore);
var
  i : integer;
  j : integer;
  index : integer;
  newSubscribers : TArray<IDispatcherCore>;
begin
  FLock.Acquire;
  try
    index := -1;
    for i := 0 to High(FSubscribers) do
    begin
      if FSubscribers[i] = core then
      begin
        index := i;
        break;
      end;
    end;
    if index < 0 then
      exit;
    SetLength(newSubscribers, Length(FSubscribers) - 1);
    j := 0;
    for i := 0 to High(FSubscribers) do
    begin
      if i <> index then
      begin
        newSubscribers[j] := FSubscribers[i];
        Inc(j);
      end;
    end;
    FSubscribers := newSubscribers;
  finally
    FLock.Release;
  end;
  core.RemovedFromChannel(Self);
end;

function TChannelState.GetSubscribers : TArray<IDispatcherCore>;
begin
  FLock.Acquire;
  try
    result := FSubscribers;
  finally
    FLock.Release;
  end;
end;

function TChannelState.AnyAccepts(const subscribers : TArray<IDispatcherCore>; const messageId : Word) : boolean;
var
  i : integer;
begin
  result := false;
  for i := 0 to High(subscribers) do
  begin
    if subscribers[i].Accepts(messageId) then
      exit(true);
  end;
end;

procedure TChannelState.PostMessage(const message : IMessage);
var
  subscribers : TArray<IDispatcherCore>;
  delivery : IDelivery;
  i : integer;
begin
  if not FEnabled then
    exit;
  subscribers := GetSubscribers;
  //a post nobody would accept is dropped here, and is never throttled
  if not AnyAccepts(subscribers, message.Id) then
    exit;
  WaitForCapacity;
  //the subscribers may have changed while we waited
  subscribers := GetSubscribers;
  delivery := TDelivery.Create(dkPost, Self, message);
  try
    for i := 0 to High(subscribers) do
      subscribers[i].TryEnqueue(delivery);
  finally
    delivery.Done;
  end;
end;

procedure TChannelState.SendMessage(const message : IMessage);
var
  subscribers : TArray<IDispatcherCore>;
  core : IDispatcherCore;
  delivery : IDelivery;
  queued : boolean;
  messageId : Word;
  i : integer;
begin
  if not FEnabled then
    exit;
  messageId := message.Id;
  subscribers := GetSubscribers;
  //one dispatcher at a time, in subscription order
  for i := 0 to High(subscribers) do
  begin
    core := subscribers[i];
    if not core.Accepts(messageId) then
      continue;
    delivery := TDelivery.Create(dkSend, Self, message);
    if CountDispatchFrames(core.GetCoreObject) > 0 then
    begin
      //this thread is running one of the dispatcher's handlers, so its queue cannot move until we return
      try
        core.DispatchInline(delivery);
      finally
        delivery.Done;
      end;
      continue;
    end;
    try
      queued := core.TryEnqueue(delivery);
    finally
      delivery.Done;
    end;
    if queued then
      WaitForSend(delivery, core);
  end;
end;

procedure TChannelState.WaitForSend(const delivery : IDelivery; const core : IDispatcherCore);
begin
  if not TryRegisterSendWait(core) then
  begin
    //the dispatcher is (indirectly) waiting on this thread, so it stays queued as a post would
    ReportError('', 'SendMessage would have deadlocked : a dispatcher it sent to is waiting on the sending thread. The message was queued without waiting.', delivery.MessageId);
    exit;
  end;
  try
    MessagingWait(delivery.DoneEvent, cWaitForever);
  finally
    UnregisterSendWait;
  end;
end;

procedure TChannelState.DeliveryCreated(out epoch : Int64);
begin
  FLock.Acquire;
  try
    Inc(FDepth);
    if FDepth > FPeakDepth then
      FPeakDepth := FDepth;
    Inc(FPostCount);
    epoch := FEpoch;
  finally
    FLock.Release;
  end;
  TMessagingControl.IncrementGlobalQueueDepth;
end;

procedure TChannelState.DeliveryCompleted(const epoch : Int64);
var
  i : integer;
  waiter : TFlushWaiter;
begin
  TMessagingControl.DecrementGlobalQueueDepth;
  FLock.Acquire;
  try
    Dec(FDepth);
    CheckGateLocked;
    for i := FFlushWaiters.Count - 1 downto 0 do
    begin
      waiter := FFlushWaiters[i];
      //only deliveries created before the flush count towards it
      if epoch < waiter.FEpoch then
      begin
        Dec(waiter.FRemaining);
        if waiter.FRemaining <= 0 then
        begin
          FFlushWaiters.Delete(i);
          waiter.FEvent.SetEvent;
        end;
      end;
    end;
  finally
    FLock.Release;
  end;
end;

procedure TChannelState.GetThresholdsLocked(out blocked : integer; out unblock : integer);
begin
  if FHasThresholds then
  begin
    blocked := FBlockedThreshold;
    unblock := FUnblockThreshold;
  end
  else
  begin
    blocked := TMessagingOptions.BlockedThreshold;
    unblock := TMessagingOptions.UnblockThreshold;
  end;
end;

procedure TChannelState.CheckGateLocked;
var
  blocked : integer;
  unblock : integer;
begin
  if FGate = nil then
    exit;
  GetThresholdsLocked(blocked, unblock);
  if (not FBackpressureEnabled) or (FDepth <= unblock) or IsCancelledLocked then
  begin
    FGate.Open;
    FGate := nil;
  end;
end;

function TChannelState.IsCancelledLocked : boolean;
begin
  result := (FCancellation <> nil) and FCancellation.IsCancelled;
end;

function WaitOnGate(const gate : IPushbackGate; const cancellation : ICancellationToken; const timeoutMs : Cardinal) : boolean;
{$IFDEF MSWINDOWS}
var
  handles : array[0..1] of THandle;
  waitResult : Cardinal;
{$ENDIF}
begin
  {$IFDEF MSWINDOWS}
  if cancellation <> nil then
  begin
    handles[0] := gate.GetEvent.Handle;
    handles[1] := cancellation.Handle;
    waitResult := WaitForMultipleObjects(2, @handles[0], false, timeoutMs);
    result := (waitResult = WAIT_OBJECT_0) or (waitResult = WAIT_OBJECT_0 + 1);
    exit;
  end;
  {$ENDIF}
  result := gate.GetEvent.WaitFor(timeoutMs) = wrSignaled;
  if (not result) and (cancellation <> nil) then
    result := cancellation.IsCancelled;
end;

procedure TChannelState.WaitForCapacity;
var
  gate : IPushbackGate;
  cancellation : ICancellationToken;
  blocked : integer;
  unblock : integer;
  timeout : Cardinal;
  notifyAfter : Cardinal;
  notified : boolean;
  timedOut : boolean;
  released : boolean;
  stopwatch : TStopwatch;
  elapsed : Int64;
  waitProc : TPushbackNotifyProc;
  timeoutProc : TPushbackNotifyProc;
begin
  //the main thread and the dispatcher threads drain the queues, blocking them could deadlock
  if IsMainThread or IsDispatcherThread then
    exit;
  FLock.Acquire;
  try
    if (not FBackpressureEnabled) or IsCancelledLocked then
      exit;
    if FGate = nil then
    begin
      GetThresholdsLocked(blocked, unblock);
      if FDepth >= blocked then
        FGate := TPushbackGate.Create;
    end;
    gate := FGate;
    if gate = nil then
      exit;
    Inc(FWaitCount);
    cancellation := FCancellation;
    timeout := FPushbackTimeout;
    notifyAfter := FWaitNotifyAfter;
  finally
    FLock.Release;
  end;

  notified := false;
  timedOut := false;
  stopwatch := TStopwatch.StartNew;
  repeat
    released := WaitOnGate(gate, cancellation, cPushbackSliceMs);
    if not released then
    begin
      //re-evaluate with the current thresholds and settings, which also covers a missed wake up
      FLock.Acquire;
      try
        CheckGateLocked;
        cancellation := FCancellation;
        timeout := FPushbackTimeout;
        notifyAfter := FWaitNotifyAfter;
        waitProc := FOnPushbackWait;
      finally
        FLock.Release;
      end;
      released := gate.IsOpen or ((cancellation <> nil) and cancellation.IsCancelled);
    end;
    if not released then
    begin
      //a UI dispatcher's drain may be stuck behind a nested CheckSynchronize, queue it again
      RescheduleStaleUIDispatchers;
      elapsed := stopwatch.ElapsedMilliseconds;
      if (not notified) and (notifyAfter <> cWaitForever) and (elapsed >= Int64(notifyAfter)) then
      begin
        notified := true;
        if Assigned(waitProc) then
        begin
          try
            waitProc(elapsed);
          except
            //a notification must not stop the post
          end;
        end;
      end;
      if (timeout <> cWaitForever) and (elapsed >= Int64(timeout)) then
        timedOut := true;
    end;
  until released or timedOut;

  elapsed := stopwatch.ElapsedMilliseconds;
  FLock.Acquire;
  try
    Inc(FTotalBlockedMs, elapsed);
    if elapsed > FLongestBlockedMs then
      FLongestBlockedMs := elapsed;
    if timedOut then
      Inc(FTimeoutCount);
    timeoutProc := FOnPushbackTimeout;
  finally
    FLock.Release;
  end;
  if timedOut and Assigned(timeoutProc) then
  begin
    try
      timeoutProc(elapsed);
    except
      //a notification must not stop the post
    end;
  end;
end;

procedure TChannelState.ReleaseWaiters;
begin
  FLock.Acquire;
  try
    if FGate <> nil then
    begin
      FGate.Open;
      FGate := nil;
    end;
  finally
    FLock.Release;
  end;
end;

function TChannelState.Flush(const timeoutMs : Cardinal) : boolean;
var
  waiter : TFlushWaiter;
begin
  //a handler of this channel cannot wait for its own message to finish
  if IsDispatchingState(Self) or IsDispatchingAnyOf(GetSubscribers) then
    exit(false);
  FLock.Acquire;
  try
    if FDepth = 0 then
      exit(true);
    Inc(FEpoch);
    //every delivery outstanding now was created before the new epoch
    waiter := TFlushWaiter.Create(FEpoch, FDepth);
    FFlushWaiters.Add(waiter);
  finally
    FLock.Release;
  end;
  try
    result := MessagingWait(waiter.FEvent, timeoutMs);
  finally
    FLock.Acquire;
    try
      FFlushWaiters.Remove(waiter);
    finally
      FLock.Release;
    end;
    waiter.Free;
  end;
end;

procedure TChannelState.ReportError(const exceptionClassName : string; const errorMessage : string; const messageId : Word);
var
  onError : TOnErrorMethod;
  onErrorEx : TMessagingErrorProc;
  info : TMessagingErrorInfo;
begin
  FLock.Acquire;
  try
    onError := FOnError;
    onErrorEx := FOnErrorEx;
  finally
    FLock.Release;
  end;
  if Assigned(onError) then
  begin
    try
      onError(errorMessage);
    except
      //an error handler that fails must not take the dispatcher down
    end;
  end;
  if Assigned(onErrorEx) then
  begin
    info.ExceptionClassName := exceptionClassName;
    info.ErrorMessage := errorMessage;
    info.MessageId := messageId;
    try
      onErrorEx(info);
    except
      //an error handler that fails must not take the dispatcher down
    end;
  end;
end;

procedure TChannelState.HandleError(const errorMessage : string);
var
  onError : TOnErrorMethod;
begin
  FLock.Acquire;
  try
    onError := FOnError;
  finally
    FLock.Release;
  end;
  if Assigned(onError) then
    onError(errorMessage);
end;

function TChannelState.GetEnabled : boolean;
begin
  result := FEnabled;
end;

procedure TChannelState.SetEnabled(const value : boolean);
begin
  FEnabled := value;
end;

function TChannelState.GetOnError : TOnErrorMethod;
begin
  FLock.Acquire;
  try
    result := FOnError;
  finally
    FLock.Release;
  end;
end;

procedure TChannelState.SetOnError(const value : TOnErrorMethod);
begin
  FLock.Acquire;
  try
    FOnError := value;
  finally
    FLock.Release;
  end;
end;

function TChannelState.GetOnErrorEx : TMessagingErrorProc;
begin
  FLock.Acquire;
  try
    result := FOnErrorEx;
  finally
    FLock.Release;
  end;
end;

procedure TChannelState.SetOnErrorEx(const value : TMessagingErrorProc);
begin
  FLock.Acquire;
  try
    FOnErrorEx := value;
  finally
    FLock.Release;
  end;
end;

function TChannelState.GetBlockedThreshold : integer;
var
  unblock : integer;
begin
  FLock.Acquire;
  try
    GetThresholdsLocked(result, unblock);
  finally
    FLock.Release;
  end;
end;

function TChannelState.GetUnblockThreshold : integer;
var
  blocked : integer;
begin
  FLock.Acquire;
  try
    GetThresholdsLocked(blocked, result);
  finally
    FLock.Release;
  end;
end;

procedure TChannelState.SetThresholds(const blocked : integer; const unblock : integer);
var
  newBlocked : integer;
  newUnblock : integer;
begin
  newBlocked := blocked;
  newUnblock := unblock;
  TMessagingOptions.ValidateThresholds(newBlocked, newUnblock);
  FLock.Acquire;
  try
    FHasThresholds := true;
    FBlockedThreshold := newBlocked;
    FUnblockThreshold := newUnblock;
    CheckGateLocked;
  finally
    FLock.Release;
  end;
end;

procedure TChannelState.ResetThresholds;
begin
  FLock.Acquire;
  try
    FHasThresholds := false;
    CheckGateLocked;
  finally
    FLock.Release;
  end;
end;

function TChannelState.GetBackpressureEnabled : boolean;
begin
  FLock.Acquire;
  try
    result := FBackpressureEnabled;
  finally
    FLock.Release;
  end;
end;

procedure TChannelState.SetBackpressureEnabled(const value : boolean);
begin
  FLock.Acquire;
  try
    FBackpressureEnabled := value;
    CheckGateLocked;
  finally
    FLock.Release;
  end;
end;

function TChannelState.GetPushbackCancellation : ICancellationToken;
begin
  FLock.Acquire;
  try
    result := FCancellation;
  finally
    FLock.Release;
  end;
end;

procedure TChannelState.SetPushbackCancellation(const value : ICancellationToken);
begin
  FLock.Acquire;
  try
    FCancellation := value;
    CheckGateLocked;
  finally
    FLock.Release;
  end;
end;

function TChannelState.GetPushbackTimeout : Cardinal;
begin
  FLock.Acquire;
  try
    result := FPushbackTimeout;
  finally
    FLock.Release;
  end;
end;

procedure TChannelState.SetPushbackTimeout(const value : Cardinal);
begin
  FLock.Acquire;
  try
    FPushbackTimeout := value;
  finally
    FLock.Release;
  end;
end;

function TChannelState.GetPushbackWaitNotifyAfter : Cardinal;
begin
  FLock.Acquire;
  try
    result := FWaitNotifyAfter;
  finally
    FLock.Release;
  end;
end;

procedure TChannelState.SetPushbackWaitNotifyAfter(const value : Cardinal);
begin
  FLock.Acquire;
  try
    FWaitNotifyAfter := value;
  finally
    FLock.Release;
  end;
end;

function TChannelState.GetOnPushbackWait : TPushbackNotifyProc;
begin
  FLock.Acquire;
  try
    result := FOnPushbackWait;
  finally
    FLock.Release;
  end;
end;

procedure TChannelState.SetOnPushbackWait(const value : TPushbackNotifyProc);
begin
  FLock.Acquire;
  try
    FOnPushbackWait := value;
  finally
    FLock.Release;
  end;
end;

function TChannelState.GetOnPushbackTimeout : TPushbackNotifyProc;
begin
  FLock.Acquire;
  try
    result := FOnPushbackTimeout;
  finally
    FLock.Release;
  end;
end;

procedure TChannelState.SetOnPushbackTimeout(const value : TPushbackNotifyProc);
begin
  FLock.Acquire;
  try
    FOnPushbackTimeout := value;
  finally
    FLock.Release;
  end;
end;

function TChannelState.GetStats : TChannelStats;
begin
  FLock.Acquire;
  try
    result.Depth := FDepth;
    result.PeakDepth := FPeakDepth;
    result.PostCount := FPostCount;
    result.PushbackWaits := FWaitCount;
    result.PushbackTimeouts := FTimeoutCount;
    result.TotalBlockedMs := FTotalBlockedMs;
    result.LongestBlockedMs := FLongestBlockedMs;
  finally
    FLock.Release;
  end;
end;

{ TVSoftMessageChannel }

constructor TVSoftMessageChannel.Create;
begin
  inherited;
  FState := TChannelState.Create;
  FStateRef := FState;
  FQueue := FState;
end;

destructor TVSoftMessageChannel.Destroy;
begin
  if FState <> nil then
    FState.Close;
  FQueue := nil;
  FStateRef := nil;
  inherited;
end;

function TVSoftMessageChannel.Flush(const timeoutMs : Cardinal) : boolean;
begin
  result := FState.Flush(timeoutMs);
end;

function TVSoftMessageChannel.GetBackpressureEnabled : boolean;
begin
  result := FState.GetBackpressureEnabled;
end;

function TVSoftMessageChannel.GetBlockedThreshold : integer;
begin
  result := FState.GetBlockedThreshold;
end;

function TVSoftMessageChannel.GetEnabled: boolean;
begin
  result := FState.GetEnabled;
end;

function TVSoftMessageChannel.GetIncludeParentID: boolean;
begin
  result := FIncludeParentID;
end;

function TVSoftMessageChannel.GetOnError: TOnErrorMethod;
begin
  result := FState.GetOnError();
end;

function TVSoftMessageChannel.GetOnErrorEx : TMessagingErrorProc;
begin
  result := FState.GetOnErrorEx();
end;

function TVSoftMessageChannel.GetOnPushbackTimeout : TPushbackNotifyProc;
begin
  result := FState.GetOnPushbackTimeout();
end;

function TVSoftMessageChannel.GetOnPushbackWait : TPushbackNotifyProc;
begin
  result := FState.GetOnPushbackWait();
end;

function TVSoftMessageChannel.GetOwnerID: string;
begin
  result := FOwnerID;
end;

function TVSoftMessageChannel.GetPushbackCancellation : ICancellationToken;
begin
  result := FState.GetPushbackCancellation;
end;

function TVSoftMessageChannel.GetPushbackTimeout : Cardinal;
begin
  result := FState.GetPushbackTimeout;
end;

function TVSoftMessageChannel.GetPushbackWaitNotifyAfter : Cardinal;
begin
  result := FState.GetPushbackWaitNotifyAfter;
end;

function TVSoftMessageChannel.GetQueue: TChannelHelper;
begin
  result := TChannelHelper.Create(FQueue);
end;

function TVSoftMessageChannel.GetStats : TChannelStats;
begin
  result := FState.GetStats;
end;

function TVSoftMessageChannel.GetUnblockThreshold : integer;
begin
  result := FState.GetUnblockThreshold;
end;

procedure TVSoftMessageChannel.HandleError(const errorMessage: string);
begin
  FState.HandleError(errorMessage);
end;

procedure TVSoftMessageChannel.PostMessage(const message : IMessage);
begin
  FState.PostMessage(message);
end;

procedure TVSoftMessageChannel.ReleaseWaiters;
begin
  FState.ReleaseWaiters;
end;

procedure TVSoftMessageChannel.ResetThresholds;
begin
  FState.ResetThresholds;
end;

procedure TVSoftMessageChannel.SendMessage(const message: IMessage);
begin
  FState.SendMessage(message);
end;

procedure TVSoftMessageChannel.SetBackpressureEnabled(const value : boolean);
begin
  FState.SetBackpressureEnabled(value);
end;

procedure TVSoftMessageChannel.SetEnabled(const Value: boolean);
begin
  FState.SetEnabled(value);
end;

procedure TVSoftMessageChannel.SetIncludeParentID(const Value: boolean);
begin
  FIncludeParentID := value;
end;

procedure TVSoftMessageChannel.SetOnError(const Value: TOnErrorMethod);
begin
  FState.SetOnError(value);
end;

procedure TVSoftMessageChannel.SetOnErrorEx(const value : TMessagingErrorProc);
begin
  FState.SetOnErrorEx(value);
end;

procedure TVSoftMessageChannel.SetOnPushbackTimeout(const value : TPushbackNotifyProc);
begin
  FState.SetOnPushbackTimeout(value);
end;

procedure TVSoftMessageChannel.SetOnPushbackWait(const value : TPushbackNotifyProc);
begin
  FState.SetOnPushbackWait(value);
end;

procedure TVSoftMessageChannel.SetOwnerID(const Value: string);
begin
  FOwnerID := value;
end;

procedure TVSoftMessageChannel.SetPushbackCancellation(const value : ICancellationToken);
begin
  FState.SetPushbackCancellation(value);
end;

procedure TVSoftMessageChannel.SetPushbackTimeout(const value : Cardinal);
begin
  FState.SetPushbackTimeout(value);
end;

procedure TVSoftMessageChannel.SetPushbackWaitNotifyAfter(const value : Cardinal);
begin
  FState.SetPushbackWaitNotifyAfter(value);
end;

procedure TVSoftMessageChannel.SetThresholds(const blocked : integer; const unblock : integer);
begin
  FState.SetThresholds(blocked, unblock);
end;

procedure TVSoftMessageChannel.Subscribe(const value: IMessageDispatcher);
var
  provider : IDispatcherCoreProvider;
begin
  if Supports(value, IDispatcherCoreProvider, provider) then
    FState.AddSubscriber(provider.GetCore);
end;

procedure TVSoftMessageChannel.UnSubscribe(const value: IMessageDispatcher);
var
  provider : IDispatcherCoreProvider;
begin
  if Supports(value, IDispatcherCoreProvider, provider) then
    FState.RemoveSubscriber(provider.GetCore);
end;

initialization
  GSendWaitLock := TCriticalSection.Create;
  GSendWaits := TDictionary<TThreadID, IDispatcherCore>.Create;

finalization
  FreeAndNil(GSendWaits);
  FreeAndNil(GSendWaitLock);

end.

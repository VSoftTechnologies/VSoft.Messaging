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

unit VSoft.Messaging.Dispatchers;

interface

uses
  System.Classes,
  System.SysUtils,
  System.SyncObjs,
  System.Generics.Collections,
  VSoft.Messaging.Internal,
  VSoft.Messaging,
  VSoft.Messaging.Channel;

type
  //The part of a dispatcher that channels deliver to : the queue, the target and the filters.
  //Channels only ever hold cores, never the dispatcher objects users hold, so releasing a
  //dispatcher always frees it (its destructor detaches the core).
  TDispatcherCore = class(TInterfacedObject, IDispatcherCore)
  private
    FLock : TCriticalSection;
    FTarget : TObject;
    FTargetClass : TClass;
    FDispatchOverridden : boolean;
    FEnabled : boolean;
    FDetached : boolean;
    FIncludeFilter : TDictionary<Cardinal, byte>;
    FExcludeFilter : TDictionary<Cardinal, byte>;
    FQueue : TQueue<IDelivery>;
    FBatch : TArray<IDelivery>;
    FBatchCount : integer;
    FActiveDispatches : integer;
    FDispatchThread : TThreadID;
    FIdleEvent : TEvent;
    FChannels : TList<IChannelState>;
    function FilterAcceptsLocked(const messageId : Word) : boolean;
    function HasHandlerLocked(const messageId : Word) : boolean;
    function AcceptsLocked(const messageId : Word) : boolean;
    function TakeQueueLocked : TArray<IDelivery>;
    procedure DoneAll(const deliveries : TArray<IDelivery>);
    procedure Deliver(const target : TObject; const dispatchOverridden : boolean; const delivery : IDelivery);
    procedure DispatchOne(const delivery : IDelivery; const fromBatch : boolean);
    //waits for a handler running on another thread to finish
    function WaitForInFlight(const timeoutMs : Cardinal) : boolean;
  protected
    //set when work is queued for the processor, cleared by ProcessBatch once the queue is empty
    FWakePending : boolean;
    FWakeRequestedAt : Int64;

    //IDispatcherCore
    function Accepts(const messageId : Word) : boolean;
    function TryEnqueue(const delivery : IDelivery) : boolean;
    procedure DispatchInline(const delivery : IDelivery);
    function GetDispatchThread : TThreadID;
    function GetCoreObject : Pointer;
    procedure AddedToChannel(const state : IChannelState);
    procedure RemovedFromChannel(const state : IChannelState);

    //called without the lock held when work is queued and the processor is not already due to run
    procedure Wake; virtual; abstract;
    //stops whatever processes the queue, once it has been detached. At shutdown it waits at most
    //shutdownWaitMs for it to stop
    procedure StopProcessing(const shutdown : boolean; const shutdownWaitMs : Cardinal); virtual; abstract;
    //handles up to maxCount queued messages on the calling thread, returns true if more are queued
    function ProcessBatch(const maxCount : integer) : boolean;
  public
    constructor Create(const target : TObject);
    destructor Destroy; override;
    procedure Start; virtual;
    //Unsubscribes from every channel, discards the queue and waits for a handler running on another
    //thread to finish. When called from one of its own handlers it does not wait for that handler.
    procedure Detach(const shutdown : boolean);

    function GetTarget : TObject;
    procedure SetTarget(const value : TObject);
    function GetEnabled : boolean;
    procedure SetEnabled(const value : boolean);
    function GetIncludeFilter : TArray<Cardinal>;
    procedure SetIncludeFilter(const value : TArray<Cardinal>);
    function GetExcludeFilter : TArray<Cardinal>;
    procedure SetExcludeFilter(const value : TArray<Cardinal>);
    function GetPendingCount : integer;
    function WaitUntilIdle(const timeoutMs : Cardinal) : boolean;
  end;

  TMessageDispatcherBase = class(TInterfacedObject, IMessageDispatcher, IDispatcherCoreProvider)
  private
    FCore : TDispatcherCore;
    FCoreRef : IDispatcherCore;
    FChannel : IMessageChannel;
  protected
    function CreateCore(const target : TObject) : TDispatcherCore; virtual; abstract;
    //IDispatcherCoreProvider
    function GetCore : IDispatcherCore;
    //IMessageDispatcher
    procedure SetChannel(const value: IMessageChannel);
    function GetChannel: IMessageChannel;
    procedure SetTarget(const value : TObject);
    function GetTarget : TObject;
    procedure SetIncludeFilter(const includeFilter : TArray<Cardinal>);
    function GetIncludeFilter : TArray<Cardinal>;
    procedure SetExcludeFilter(const excludeFilter : TArray<Cardinal>);
    function GetExcludeFilter : TArray<Cardinal>;
    function GetEnabled : boolean;
    procedure SetEnabled(const value : boolean);
    function GetPendingCount : integer;
    function WaitUntilIdle(const timeoutMs : Cardinal = cWaitForever) : boolean;
  public
    constructor Create(const target : TObject = nil);virtual;
    destructor Destroy; override;
    property Enabled : boolean read GetEnabled write SetEnabled;
  end;

  //handlers run on a thread of its own
  TThreadedMessageDispatcher = class(TMessageDispatcherBase)
  protected
    function CreateCore(const target : TObject) : TDispatcherCore; override;
  public
    constructor Create(const target : TObject = nil);override;
  end;

  //handlers run on the main thread, delivered through TThread.Queue so CheckSynchronize services them
  TUIMessageDispatcher = class(TThreadedMessageDispatcher)
  protected
    function CreateCore(const target : TObject) : TDispatcherCore; override;
  end;

//Waits for the event. While it waits it re-queues UI dispatcher drains that got stuck, and on the
//main thread it handles queued UI dispatcher messages, synchronize calls and sent window messages
//(as TThread.WaitFor does), so it cannot deadlock on a handler that needs the main thread.
function MessagingWait(const event : TEvent; const timeoutMs : Cardinal) : boolean;

//queues the drain of any UI dispatcher whose last drain has not run for a while
procedure RescheduleStaleUIDispatchers;

implementation

uses
  {$IFDEF MSWINDOWS}
  Winapi.Windows,
  {$ENDIF}
  System.Diagnostics;

const
  //waits are split into slices this long, so stuck drains are noticed
  cWaitSliceMs = 50;
  //a UI drain queued this long ago that has not run is queued again
  cStaleDrainMs = 200;
  //how long unit finalization waits for each dispatcher
  cShutdownWaitMs = 2000;

type
  THandlerProc = procedure(var message) of object;

  TThreadedDispatcherCore = class;

  TDispatcherWorker = class(TThread)
  private
    FCore : TThreadedDispatcherCore;
    FCoreRef : IDispatcherCore;
    FWakeEvent : TEvent;
    FFinishedEvent : TEvent;
  protected
    procedure Execute; override;
    procedure DoTerminate; override;
  public
    constructor Create(const core : TThreadedDispatcherCore);
    destructor Destroy; override;
    procedure Wake;
  end;

  TThreadedDispatcherCore = class(TDispatcherCore)
  private
    FWorker : TDispatcherWorker;
  protected
    procedure Wake; override;
    procedure StopProcessing(const shutdown : boolean; const shutdownWaitMs : Cardinal); override;
  public
    procedure Start; override;
  end;

  TUIDispatcherCore = class(TDispatcherCore)
  private
    FRunning : boolean;
    function MakeDrainProc : TThreadProcedure;
    procedure QueueDrain;
  protected
    procedure Wake; override;
    procedure StopProcessing(const shutdown : boolean; const shutdownWaitMs : Cardinal); override;
  public
    //Handles up to MaxBurst queued messages. Main thread only, does nothing if a drain of this
    //dispatcher is already running further up the stack.
    procedure Drain;
    procedure RescheduleIfStale;
  end;

  //TThread.Queue runs the proc straight away when called on the main thread, so drains requested on
  //the main thread are queued from this thread instead.
  TUIScheduler = class(TThread)
  private
    FLock : TCriticalSection;
    FPending : TList<IDispatcherCore>;
    FWakeEvent : TEvent;
    FFinishedEvent : TEvent;
  protected
    procedure Execute; override;
    procedure DoTerminate; override;
  public
    constructor Create;
    destructor Destroy; override;
    procedure Request(const core : IDispatcherCore);
    procedure Wake;
  end;

var
  GRegistryLock : TCriticalSection;
  GCores : TList<IDispatcherCore>;
  GUICores : TList<IDispatcherCore>;
  GUIScheduler : TUIScheduler;
  GClock : TStopwatch;
  GShuttingDown : boolean;

function NowMs : Int64;
begin
  result := GClock.ElapsedMilliseconds;
end;

procedure RegisterCore(const core : TDispatcherCore);
begin
  GRegistryLock.Acquire;
  try
    GCores.Add(core);
    if core is TUIDispatcherCore then
      GUICores.Add(core);
  finally
    GRegistryLock.Release;
  end;
end;

procedure UnregisterCore(const core : TDispatcherCore);
var
  coreRef : IDispatcherCore;
begin
  //keep it alive until the lock is released, removing it may drop the last reference
  coreRef := core;
  GRegistryLock.Acquire;
  try
    GCores.Remove(coreRef);
    GUICores.Remove(coreRef);
  finally
    GRegistryLock.Release;
  end;
end;

function UICoresSnapshot : TArray<IDispatcherCore>;
begin
  GRegistryLock.Acquire;
  try
    if GUICores <> nil then
      result := GUICores.ToArray
    else
      result := nil;
  finally
    GRegistryLock.Release;
  end;
end;

procedure RequestUIDrain(const core : IDispatcherCore);
begin
  GRegistryLock.Acquire;
  try
    if GShuttingDown then
      exit;
    if GUIScheduler = nil then
      GUIScheduler := TUIScheduler.Create;
    GUIScheduler.Request(core);
  finally
    GRegistryLock.Release;
  end;
end;

procedure RescheduleStaleUIDispatchers;
var
  cores : TArray<IDispatcherCore>;
  i : integer;
begin
  cores := UICoresSnapshot;
  for i := 0 to High(cores) do
    TUIDispatcherCore(cores[i].GetCoreObject).RescheduleIfStale;
end;

procedure DrainUIDispatchers;
var
  cores : TArray<IDispatcherCore>;
  i : integer;
begin
  cores := UICoresSnapshot;
  for i := 0 to High(cores) do
    TUIDispatcherCore(cores[i].GetCoreObject).Drain;
end;

{$IFDEF MSWINDOWS}
//main thread only : waits for the event while running synchronize calls and letting other threads'
//SendMessage calls to this thread's windows through, as TThread.WaitFor does
function WaitPumping(const event : TEvent; const timeoutMs : Cardinal) : boolean;
var
  handles : array[0..1] of THandle;
  waitResult : Cardinal;
  msg : TMsg;
begin
  handles[0] := event.Handle;
  handles[1] := SyncEvent;
  waitResult := MsgWaitForMultipleObjects(2, handles, false, timeoutMs, QS_SENDMESSAGE);
  case waitResult of
    WAIT_OBJECT_0 :
      result := true;
    WAIT_OBJECT_0 + 1 :
    begin
      CheckSynchronize;
      result := event.WaitFor(0) = wrSignaled;
    end;
    WAIT_OBJECT_0 + 2 :
    begin
      PeekMessage(msg, 0, 0, 0, PM_NOREMOVE);
      result := event.WaitFor(0) = wrSignaled;
    end;
  else
    result := false;
  end;
end;
{$ELSE}
function WaitPumping(const event : TEvent; const timeoutMs : Cardinal) : boolean;
begin
  CheckSynchronize(timeoutMs);
  result := event.WaitFor(0) = wrSignaled;
end;
{$ENDIF}

function MessagingWait(const event : TEvent; const timeoutMs : Cardinal) : boolean;
var
  onMainThread : boolean;
  stopwatch : TStopwatch;
  elapsed : Int64;
  slice : Cardinal;
begin
  onMainThread := IsMainThread;
  stopwatch := TStopwatch.StartNew;
  repeat
    if onMainThread then
      DrainUIDispatchers
    else
      RescheduleStaleUIDispatchers;
    slice := cWaitSliceMs;
    if timeoutMs <> cWaitForever then
    begin
      elapsed := stopwatch.ElapsedMilliseconds;
      if elapsed >= Int64(timeoutMs) then
        exit(event.WaitFor(0) = wrSignaled);
      if Int64(timeoutMs) - elapsed < Int64(slice) then
        slice := Cardinal(Int64(timeoutMs) - elapsed);
    end;
    if onMainThread then
      result := WaitPumping(event, slice)
    else
      result := event.WaitFor(slice) = wrSignaled;
  until result;
end;

function OverridesDispatch(const target : TObject) : boolean;
var
  dispatchMethod : procedure(var message) of object;
begin
  dispatchMethod := target.Dispatch;
  result := TMethod(dispatchMethod).Code <> @TObject.Dispatch;
end;

{ TDispatcherCore }

constructor TDispatcherCore.Create(const target : TObject);
begin
  inherited Create;
  FLock := TCriticalSection.Create;
  FQueue := TQueue<IDelivery>.Create;
  FChannels := TList<IChannelState>.Create;
  FIdleEvent := TEvent.Create(nil, true, true, '');
  FEnabled := true;
  SetTarget(target);
end;

destructor TDispatcherCore.Destroy;
var
  remaining : TArray<IDelivery>;
begin
  //only reachable once detached, so this is normally empty
  remaining := TakeQueueLocked;
  DoneAll(remaining);
  FIncludeFilter.Free;
  FExcludeFilter.Free;
  FChannels.Free;
  FQueue.Free;
  FIdleEvent.Free;
  FLock.Free;
  inherited;
end;

procedure TDispatcherCore.Start;
begin
  RegisterCore(Self);
end;

function TDispatcherCore.GetCoreObject : Pointer;
begin
  result := Self;
end;

function TDispatcherCore.GetDispatchThread : TThreadID;
begin
  FLock.Acquire;
  try
    result := FDispatchThread;
  finally
    FLock.Release;
  end;
end;

function TDispatcherCore.FilterAcceptsLocked(const messageId : Word) : boolean;
begin
  result := false;
  if (FIncludeFilter <> nil) and (not FIncludeFilter.ContainsKey(messageId)) then
    exit;
  if (FExcludeFilter <> nil) and FExcludeFilter.ContainsKey(messageId) then
    exit;
  result := true;
end;

function TDispatcherCore.HasHandlerLocked(const messageId : Word) : boolean;
begin
  if FDispatchOverridden then
    exit(true);
  //TObject.Dispatch only looks up handlers for ids 1..$BFFF
  if (messageId = 0) or (messageId >= $C000) then
    exit(false);
  result := GetDynaMethod(FTargetClass, SmallInt(messageId)) <> nil;
end;

function TDispatcherCore.AcceptsLocked(const messageId : Word) : boolean;
begin
  result := (not FDetached) and FEnabled and (FTarget <> nil) and FilterAcceptsLocked(messageId) and HasHandlerLocked(messageId);
end;

function TDispatcherCore.Accepts(const messageId : Word) : boolean;
begin
  FLock.Acquire;
  try
    result := AcceptsLocked(messageId);
  finally
    FLock.Release;
  end;
end;

function TDispatcherCore.TryEnqueue(const delivery : IDelivery) : boolean;
var
  needsWake : boolean;
begin
  needsWake := false;
  FLock.Acquire;
  try
    if delivery.Kind = dkBarrier then
      result := not FDetached
    else
      result := AcceptsLocked(delivery.MessageId);
    if result then
    begin
      delivery.AddPending;
      FQueue.Enqueue(delivery);
      if not FWakePending then
      begin
        FWakePending := true;
        FWakeRequestedAt := NowMs;
        needsWake := true;
      end;
    end;
  finally
    FLock.Release;
  end;
  if needsWake then
    Wake;
end;

function TDispatcherCore.TakeQueueLocked : TArray<IDelivery>;
var
  i : integer;
begin
  SetLength(result, FQueue.Count);
  for i := 0 to High(result) do
    result[i] := FQueue.Dequeue;
end;

procedure TDispatcherCore.DoneAll(const deliveries : TArray<IDelivery>);
var
  i : integer;
begin
  for i := 0 to High(deliveries) do
    deliveries[i].Done;
end;

function TDispatcherCore.ProcessBatch(const maxCount : integer) : boolean;
var
  count : integer;
  i : integer;
  delivery : IDelivery;
begin
  FLock.Acquire;
  try
    count := FQueue.Count;
    if count > maxCount then
      count := maxCount;
    if Length(FBatch) < count then
      SetLength(FBatch, count);
    for i := 0 to count - 1 do
      FBatch[i] := FQueue.Dequeue;
    FBatchCount := count;
  finally
    FLock.Release;
  end;

  for i := 0 to count - 1 do
  begin
    delivery := FBatch[i];
    FBatch[i] := nil;
    DispatchOne(delivery, true);
    delivery := nil;
  end;

  FLock.Acquire;
  try
    result := FQueue.Count > 0;
    if not result then
      FWakePending := false;
  finally
    FLock.Release;
  end;
end;

procedure TDispatcherCore.DispatchOne(const delivery : IDelivery; const fromBatch : boolean);
var
  target : TObject;
  dispatchOverridden : boolean;
  canDispatch : boolean;
  frame : TDispatchFrame;
begin
  canDispatch := false;
  target := nil;
  dispatchOverridden := false;
  if delivery.Kind <> dkBarrier then
  begin
    FLock.Acquire;
    try
      //checked for every message, so nothing is delivered once disabled, detached or the target cleared
      canDispatch := (not FDetached) and FEnabled and (FTarget <> nil);
      if canDispatch then
      begin
        target := FTarget;
        dispatchOverridden := FDispatchOverridden;
        Inc(FActiveDispatches);
        FDispatchThread := CurrentThreadId;
        FIdleEvent.ResetEvent;
      end;
    finally
      FLock.Release;
    end;
  end;

  if canDispatch then
  begin
    PushDispatchFrame(frame, delivery.StatePtr, Self);
    try
      Deliver(target, dispatchOverridden, delivery);
    finally
      PopDispatchFrame(frame);
    end;
  end;

  //the accounting, and the release of the message, happen before anyone waiting for this handler wakes
  delivery.Done;

  FLock.Acquire;
  try
    if fromBatch then
      Dec(FBatchCount);
    if canDispatch then
    begin
      Dec(FActiveDispatches);
      if FActiveDispatches = 0 then
      begin
        FDispatchThread := 0;
        FIdleEvent.SetEvent;
      end;
    end;
  finally
    FLock.Release;
  end;
end;

procedure TDispatcherCore.Deliver(const target : TObject; const dispatchOverridden : boolean; const delivery : IDelivery);
var
  handler : Pointer;
  handlerProc : THandlerProc;
  className : string;
begin
  try
    if dispatchOverridden then
      target.Dispatch(PByte(delivery.MessagePtr)^)
    else
    begin
      //call the message method directly, Dispatch would pass messages without one to DefaultHandler
      handler := GetDynaMethod(target.ClassType, SmallInt(delivery.MessageId));
      if handler <> nil then
      begin
        TMethod(handlerProc).Code := handler;
        TMethod(handlerProc).Data := target;
        handlerProc(PByte(delivery.MessagePtr)^);
      end;
    end;
  except
    on e : Exception do
      delivery.ReportError(e.ClassName, e.Message);
    else
    begin
      if ExceptObject <> nil then
        className := ExceptObject.ClassName
      else
        className := '';
      delivery.ReportError(className, 'A message handler raised an object that is not an Exception');
    end;
  end;
end;

procedure TDispatcherCore.DispatchInline(const delivery : IDelivery);
begin
  //balanced by the Done in DispatchOne
  delivery.AddPending;
  DispatchOne(delivery, false);
end;

function TDispatcherCore.WaitForInFlight(const timeoutMs : Cardinal) : boolean;
var
  ownFrames : integer;
  mustWait : boolean;
  stopwatch : TStopwatch;
  slice : Cardinal;
begin
  //handlers of this dispatcher that the current thread is running cannot be waited for
  ownFrames := CountDispatchFrames(Self);
  stopwatch := TStopwatch.StartNew;
  repeat
    FLock.Acquire;
    try
      mustWait := FActiveDispatches > ownFrames;
    finally
      FLock.Release;
    end;
    if not mustWait then
      exit(true);
    slice := cWaitSliceMs;
    if timeoutMs <> cWaitForever then
    begin
      if stopwatch.ElapsedMilliseconds >= Int64(timeoutMs) then
        exit(false);
    end;
    MessagingWait(FIdleEvent, slice);
  until false;
end;

procedure TDispatcherCore.Detach(const shutdown : boolean);
var
  discarded : TArray<IDelivery>;
  channels : TArray<IChannelState>;
  i : integer;
  shutdownWaitMs : Cardinal;
begin
  FLock.Acquire;
  try
    if FDetached then
      exit;
    FDetached := true;
    FTarget := nil;
    FTargetClass := nil;
    discarded := TakeQueueLocked;
    channels := FChannels.ToArray;
    FChannels.Clear;
  finally
    FLock.Release;
  end;
  UnregisterCore(Self);
  for i := 0 to High(channels) do
    channels[i].RemoveSubscriber(Self);
  DoneAll(discarded);
  if shutdown then
  begin
    //a handler still stuck after the wait will not let its thread finish either, so don't wait twice
    if WaitForInFlight(cShutdownWaitMs) then
      shutdownWaitMs := cShutdownWaitMs
    else
      shutdownWaitMs := 0;
  end
  else
  begin
    WaitForInFlight(cWaitForever);
    shutdownWaitMs := 0;
  end;
  StopProcessing(shutdown, shutdownWaitMs);
end;

procedure TDispatcherCore.AddedToChannel(const state : IChannelState);
var
  detached : boolean;
begin
  FLock.Acquire;
  try
    detached := FDetached;
    if (not detached) and (FChannels.IndexOf(state) < 0) then
      FChannels.Add(state);
  finally
    FLock.Release;
  end;
  if detached then
    state.RemoveSubscriber(Self);
end;

procedure TDispatcherCore.RemovedFromChannel(const state : IChannelState);
begin
  FLock.Acquire;
  try
    FChannels.Remove(state);
  finally
    FLock.Release;
  end;
end;

function TDispatcherCore.GetTarget : TObject;
begin
  FLock.Acquire;
  try
    result := FTarget;
  finally
    FLock.Release;
  end;
end;

procedure TDispatcherCore.SetTarget(const value : TObject);
var
  discarded : TArray<IDelivery>;
begin
  discarded := nil;
  FLock.Acquire;
  try
    if FDetached then
      exit;
    FTarget := value;
    if value <> nil then
    begin
      //cached so posting threads never touch the target itself
      FTargetClass := value.ClassType;
      FDispatchOverridden := OverridesDispatch(value);
    end
    else
    begin
      FTargetClass := nil;
      FDispatchOverridden := false;
      discarded := TakeQueueLocked;
    end;
  finally
    FLock.Release;
  end;
  DoneAll(discarded);
  if value = nil then
    WaitForInFlight(cWaitForever);
end;

function TDispatcherCore.GetEnabled : boolean;
begin
  FLock.Acquire;
  try
    result := FEnabled;
  finally
    FLock.Release;
  end;
end;

procedure TDispatcherCore.SetEnabled(const value : boolean);
var
  discarded : TArray<IDelivery>;
begin
  discarded := nil;
  FLock.Acquire;
  try
    FEnabled := value;
    if not value then
      discarded := TakeQueueLocked;
  finally
    FLock.Release;
  end;
  DoneAll(discarded);
end;

function CreateFilter(const ids : TArray<Cardinal>) : TDictionary<Cardinal, byte>;
var
  id : Cardinal;
begin
  result := nil;
  if Length(ids) = 0 then
    exit;
  result := TDictionary<Cardinal, byte>.Create;
  for id in ids do
  begin
    Assert((id > 0) and (id < $C000), 'message ids must be in the range 1..$BFFF');
    result.AddOrSetValue(id, 0);
  end;
end;

function FilterIds(const filter : TDictionary<Cardinal, byte>) : TArray<Cardinal>;
begin
  if filter <> nil then
    result := filter.Keys.ToArray
  else
    SetLength(result, 0);
end;

function TDispatcherCore.GetIncludeFilter : TArray<Cardinal>;
begin
  FLock.Acquire;
  try
    result := FilterIds(FIncludeFilter);
  finally
    FLock.Release;
  end;
end;

procedure TDispatcherCore.SetIncludeFilter(const value : TArray<Cardinal>);
var
  newFilter : TDictionary<Cardinal, byte>;
  oldFilter : TDictionary<Cardinal, byte>;
begin
  newFilter := CreateFilter(value);
  FLock.Acquire;
  try
    oldFilter := FIncludeFilter;
    FIncludeFilter := newFilter;
  finally
    FLock.Release;
  end;
  oldFilter.Free;
end;

function TDispatcherCore.GetExcludeFilter : TArray<Cardinal>;
begin
  FLock.Acquire;
  try
    result := FilterIds(FExcludeFilter);
  finally
    FLock.Release;
  end;
end;

procedure TDispatcherCore.SetExcludeFilter(const value : TArray<Cardinal>);
var
  newFilter : TDictionary<Cardinal, byte>;
  oldFilter : TDictionary<Cardinal, byte>;
begin
  newFilter := CreateFilter(value);
  FLock.Acquire;
  try
    oldFilter := FExcludeFilter;
    FExcludeFilter := newFilter;
  finally
    FLock.Release;
  end;
  oldFilter.Free;
end;

function TDispatcherCore.GetPendingCount : integer;
begin
  FLock.Acquire;
  try
    result := FQueue.Count + FBatchCount;
  finally
    FLock.Release;
  end;
end;

function TDispatcherCore.WaitUntilIdle(const timeoutMs : Cardinal) : boolean;
var
  barrier : IDelivery;
  queued : boolean;
begin
  //one of our own handlers cannot wait for the queue behind it
  if CountDispatchFrames(Self) > 0 then
    exit(false);
  barrier := CreateBarrierDelivery;
  try
    queued := TryEnqueue(barrier);
  finally
    barrier.Done;
  end;
  if not queued then
    exit(true);
  result := MessagingWait(barrier.DoneEvent, timeoutMs);
end;

{ TDispatcherWorker }

constructor TDispatcherWorker.Create(const core : TThreadedDispatcherCore);
begin
  FCore := core;
  FCoreRef := core;
  FWakeEvent := TEvent.Create(nil, false, false, '');
  FFinishedEvent := TEvent.Create(nil, true, false, '');
  inherited Create(false);
end;

destructor TDispatcherWorker.Destroy;
begin
  inherited;
  FWakeEvent.Free;
  FFinishedEvent.Free;
end;

procedure TDispatcherWorker.Execute;
begin
  MarkAsDispatcherThread;
  while not Terminated do
  begin
    FWakeEvent.WaitFor(cWaitForever);
    while (not Terminated) and FCore.ProcessBatch(TMessagingOptions.MaxBurst) do
      ;
  end;
end;

procedure TDispatcherWorker.DoTerminate;
begin
  //called even when the thread was terminated before Execute got to run (TThread skips Execute then)
  FCore := nil;
  //may free the core, if it was detached from one of its own handlers
  FCoreRef := nil;
  FFinishedEvent.SetEvent;
  inherited;
end;

procedure TDispatcherWorker.Wake;
begin
  FWakeEvent.SetEvent;
end;

{ TThreadedDispatcherCore }

procedure TThreadedDispatcherCore.Start;
begin
  inherited;
  FWorker := TDispatcherWorker.Create(Self);
end;

procedure TThreadedDispatcherCore.Wake;
begin
  //under the lock so StopProcessing cannot free the worker while we use it
  FLock.Acquire;
  try
    if FWorker <> nil then
      FWorker.Wake;
  finally
    FLock.Release;
  end;
end;

procedure TThreadedDispatcherCore.StopProcessing(const shutdown : boolean; const shutdownWaitMs : Cardinal);
var
  worker : TDispatcherWorker;
  finished : boolean;
begin
  FLock.Acquire;
  try
    worker := FWorker;
    FWorker := nil;
  finally
    FLock.Release;
  end;
  if worker = nil then
    exit;
  worker.Terminate;
  if worker.ThreadID = CurrentThreadId then
  begin
    //detached from one of its own handlers : it cannot wait for itself, so it finishes once the
    //handler returns and frees itself
    worker.FreeOnTerminate := true;
    worker.Wake;
    exit;
  end;
  worker.Wake;
  if shutdown then
  begin
    finished := worker.FFinishedEvent.WaitFor(shutdownWaitMs) = wrSignaled;
    //freeing waits for the thread to exit, which a library cannot do while it is being unloaded
    if finished and (not IsLibrary) then
      worker.Free;
  end
  else
  begin
    MessagingWait(worker.FFinishedEvent, cWaitForever);
    worker.Free;
  end;
end;

{ TUIDispatcherCore }

function TUIDispatcherCore.MakeDrainProc : TThreadProcedure;
var
  coreRef : IDispatcherCore;
begin
  //the proc keeps the core alive until it has run
  coreRef := Self;
  result :=
    procedure
    begin
      try
        TUIDispatcherCore(coreRef.GetCoreObject).Drain;
      except
        //a queued proc that raises stops CheckSynchronize running the procs queued after it
      end;
    end;
end;

procedure TUIDispatcherCore.QueueDrain;
var
  drainProc : TThreadProcedure;
begin
  if IsMainThread then
    RequestUIDrain(Self)
  else
  begin
    drainProc := MakeDrainProc();
    TThread.Queue(nil, drainProc);
  end;
end;

procedure TUIDispatcherCore.Wake;
begin
  QueueDrain;
end;

procedure TUIDispatcherCore.StopProcessing(const shutdown : boolean; const shutdownWaitMs : Cardinal);
begin
  //no thread of its own, any drain still queued finds it detached and does nothing
end;

procedure TUIDispatcherCore.Drain;
var
  more : boolean;
  requeue : boolean;
begin
  FLock.Acquire;
  try
    //a drain of this dispatcher further up the stack, nesting would break the message order
    if FRunning then
      exit;
    if FDetached or (FQueue.Count = 0) then
    begin
      FWakePending := false;
      exit;
    end;
    FRunning := true;
  finally
    FLock.Release;
  end;
  more := false;
  try
    more := ProcessBatch(TMessagingOptions.MaxBurst);
  finally
    FLock.Acquire;
    try
      FRunning := false;
      requeue := more and (not FDetached);
      if requeue then
        FWakeRequestedAt := NowMs;
    finally
      FLock.Release;
    end;
  end;
  //let the main thread do other work before the next batch
  if requeue then
    QueueDrain;
end;

procedure TUIDispatcherCore.RescheduleIfStale;
var
  stale : boolean;
begin
  FLock.Acquire;
  try
    stale := FWakePending and (not FRunning) and (not FDetached) and (FQueue.Count > 0) and (NowMs - FWakeRequestedAt > cStaleDrainMs);
    if stale then
      FWakeRequestedAt := NowMs;
  finally
    FLock.Release;
  end;
  if stale then
    QueueDrain;
end;

{ TUIScheduler }

constructor TUIScheduler.Create;
begin
  FLock := TCriticalSection.Create;
  FPending := TList<IDispatcherCore>.Create;
  FWakeEvent := TEvent.Create(nil, false, false, '');
  FFinishedEvent := TEvent.Create(nil, true, false, '');
  inherited Create(false);
end;

destructor TUIScheduler.Destroy;
begin
  inherited;
  FPending.Free;
  FWakeEvent.Free;
  FFinishedEvent.Free;
  FLock.Free;
end;

procedure TUIScheduler.Execute;
var
  pending : TArray<IDispatcherCore>;
  drainProc : TThreadProcedure;
  i : integer;
begin
  MarkAsDispatcherThread;
  while not Terminated do
  begin
    FWakeEvent.WaitFor(cWaitForever);
    FLock.Acquire;
    try
      pending := FPending.ToArray;
      FPending.Clear;
    finally
      FLock.Release;
    end;
    if not Terminated then
    begin
      for i := 0 to High(pending) do
      begin
        drainProc := TUIDispatcherCore(pending[i].GetCoreObject).MakeDrainProc();
        TThread.Queue(nil, drainProc);
      end;
    end;
    drainProc := nil;
    pending := nil;
  end;
end;

procedure TUIScheduler.DoTerminate;
begin
  //called even when the thread was terminated before Execute got to run
  FFinishedEvent.SetEvent;
  inherited;
end;

procedure TUIScheduler.Request(const core : IDispatcherCore);
begin
  FLock.Acquire;
  try
    if FPending.IndexOf(core) < 0 then
      FPending.Add(core);
  finally
    FLock.Release;
  end;
  FWakeEvent.SetEvent;
end;

procedure TUIScheduler.Wake;
begin
  FWakeEvent.SetEvent;
end;

{ TMessageDispatcherBase }

constructor TMessageDispatcherBase.Create(const target : TObject);
begin
  inherited Create;
  FCore := CreateCore(target);
  FCoreRef := FCore;
  FCore.Start;
end;

destructor TMessageDispatcherBase.Destroy;
begin
  if FCore <> nil then
    FCore.Detach(false);
  FChannel := nil;
  FCoreRef := nil;
  FCore := nil;
  inherited;
end;

function TMessageDispatcherBase.GetCore : IDispatcherCore;
begin
  result := FCoreRef;
end;

function TMessageDispatcherBase.GetChannel: IMessageChannel;
begin
  result := FChannel;
end;

procedure TMessageDispatcherBase.SetChannel(const value: IMessageChannel);
var
  oldChannel : IMessageChannel;
begin
  //setting the same channel again is a no-op, so it keeps its place in the delivery order
  if Pointer(value) = Pointer(FChannel) then
    exit;
  oldChannel := FChannel;
  FChannel := value;
  //only unsubscribes, messages already queued are still delivered
  if oldChannel <> nil then
    oldChannel.UnSubscribe(Self);
  if value <> nil then
    value.Subscribe(Self);
end;

function TMessageDispatcherBase.GetEnabled: boolean;
begin
  result := FCore.GetEnabled;
end;

procedure TMessageDispatcherBase.SetEnabled(const value: boolean);
begin
  FCore.SetEnabled(value);
end;

function TMessageDispatcherBase.GetExcludeFilter: TArray<Cardinal>;
begin
  result := FCore.GetExcludeFilter;
end;

procedure TMessageDispatcherBase.SetExcludeFilter(const excludeFilter: TArray<Cardinal>);
begin
  FCore.SetExcludeFilter(excludeFilter);
end;

function TMessageDispatcherBase.GetIncludeFilter: TArray<Cardinal>;
begin
  result := FCore.GetIncludeFilter;
end;

procedure TMessageDispatcherBase.SetIncludeFilter(const includeFilter: TArray<Cardinal>);
begin
  FCore.SetIncludeFilter(includeFilter);
end;

function TMessageDispatcherBase.GetPendingCount : integer;
begin
  result := FCore.GetPendingCount;
end;

function TMessageDispatcherBase.GetTarget: TObject;
begin
  result := FCore.GetTarget;
end;

procedure TMessageDispatcherBase.SetTarget(const value: TObject);
begin
  FCore.SetTarget(value);
end;

function TMessageDispatcherBase.WaitUntilIdle(const timeoutMs : Cardinal) : boolean;
begin
  result := FCore.WaitUntilIdle(timeoutMs);
end;

{ TThreadedMessageDispatcher }

constructor TThreadedMessageDispatcher.Create(const target : TObject);
begin
  inherited Create(target);
end;

function TThreadedMessageDispatcher.CreateCore(const target : TObject) : TDispatcherCore;
begin
  result := TThreadedDispatcherCore.Create(target);
end;

{ TUIMessageDispatcher }

function TUIMessageDispatcher.CreateCore(const target : TObject) : TDispatcherCore;
begin
  result := TUIDispatcherCore.Create(target);
end;

procedure ShutdownDispatchers;
var
  cores : TArray<IDispatcherCore>;
  scheduler : TUIScheduler;
  i : integer;
begin
  GRegistryLock.Acquire;
  try
    GShuttingDown := true;
    cores := GCores.ToArray;
    scheduler := GUIScheduler;
    GUIScheduler := nil;
  finally
    GRegistryLock.Release;
  end;
  //dispatchers that were never released : stop them before the units their targets live in are gone
  for i := 0 to High(cores) do
    TDispatcherCore(cores[i].GetCoreObject).Detach(true);
  cores := nil;
  if scheduler <> nil then
  begin
    scheduler.Terminate;
    scheduler.Wake;
    if (scheduler.FFinishedEvent.WaitFor(cShutdownWaitMs) = wrSignaled) and (not IsLibrary) then
      scheduler.Free;
  end;
end;

initialization
  GClock := TStopwatch.StartNew;
  GRegistryLock := TCriticalSection.Create;
  GCores := TList<IDispatcherCore>.Create;
  GUICores := TList<IDispatcherCore>.Create;

finalization
  ShutdownDispatchers;
  //anything still registered was detached above and leaked by its owner, so the lists are left alone
  //if a dispatcher did not stop in time
  GRegistryLock.Acquire;
  try
    if GCores.Count = 0 then
      FreeAndNil(GCores);
    if GUICores.Count = 0 then
      FreeAndNil(GUICores);
  finally
    GRegistryLock.Release;
  end;

end.

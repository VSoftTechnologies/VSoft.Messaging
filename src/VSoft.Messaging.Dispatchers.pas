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
  Generics.Collections,
  System.SyncObjs,
  VSoft.Messaging.Internal,
  VSoft.Messaging;

type
  IMessageDispatcherPost = interface
    ['{58C8F6CF-C9B5-4EBB-9972-06B4902A43E2}']
    procedure PostMessage(const Message: IMessage);
    procedure SendMessage(const Message: IMessage);
  end;


  TMessageDispatcherBase = class(TInterfacedObject,IMessageDispatcher,IMessageDispatcherPost)
  private
    FOwnerThreadId : TThreadID;
    FTarget     : TObject;
    FQueue      : TQueue<IMessage>;
    FChannel    : IMessageChannel;
    FQueueLock  : TCriticalSection;
    FEnabled    : boolean;
    FIncludeFilter : TDictionary<Cardinal,byte>;
    FExcludeFilter : TDictionary<Cardinal,byte>;
  protected
    procedure SetChannel(const value: IMessageChannel);
    function GetChannel: IMessageChannel;
    procedure SetTarget(const value : TObject);
    function GetTarget : TObject;
    procedure SetIncludeFilter(const includeFilter : TArray<Cardinal>);
    function GetIncludeFilter : TArray<Cardinal>;
    procedure SetExcludeFilter(const excludeFilter : TArray<Cardinal>);
    function GetExcludeFilter : TArray<Cardinal>;


    function DequeueAtMost(const count : integer) : TArray<IMessage>;

    function GetEnabled : boolean;
    procedure SetEnabled(const value : boolean);

    function WillAcceptMessage(const id : Cardinal) : boolean;

    procedure DoPostMessage(const Message: IMessage);virtual;abstract;
    procedure DoSendMessage(const Message: IMessage);virtual;


    procedure PostMessage(const Message: IMessage);
    procedure SendMessage(const Message: IMessage);
  public
    constructor Create(const target : TObject = nil);virtual;
    destructor Destroy; override;
    property Enabled : boolean read FEnabled write FEnabled;
  end;


  TThreadedMessageDispatcher = class;

  TMessageDispatcherThread = class(TThread)
  private
    FDispatcher : TThreadedMessageDispatcher;
    FTrigger : TEvent;
    FErrored : boolean;
  protected
    procedure  Execute; override;

  public
    constructor Create(const AOwner : TThreadedMessageDispatcher);
    destructor Destroy; override;
    procedure Trigger;
    procedure Shutdown;
  end;


  TMessageDispatcherThreadClass = class of TMessageDispatcherThread;

  TThreadedMessageDispatcher = class(TMessageDispatcherBase)
  private
    FThread : TMessageDispatcherThread;
  protected
    function GetThreadClass : TMessageDispatcherThreadClass;virtual;
  public
    constructor Create(const target : TObject = nil);override;
    destructor Destroy;override;
    procedure DoPostMessage(const Message : IMessage);override;
  end;

  TUIMessageDispatcher = class(TThreadedMessageDispatcher)
  protected

    function GetThreadClass : TMessageDispatcherThreadClass; override;
    procedure DoSendMessage(const Message: IMessage); override;
  end;

  TMessageDispatcherUIThread = class(TMessageDispatcherThread)
  protected
    procedure Execute;override;
  end;

implementation

uses
  VSoft.Messaging.Channel;



{ TThreadedMessageDispatcher }

constructor TThreadedMessageDispatcher.Create(const target : TObject);
begin
  inherited Create(target);
  FThread := GetThreadClass.Create(Self);
end;

destructor TThreadedMessageDispatcher.Destroy;
begin
  FEnabled := False;
  FTarget := nil;
  SetChannel(nil);
  FThread.Shutdown;
  FThread.WaitFor;
  FThread.Free;
  inherited;
end;

function TThreadedMessageDispatcher.GetThreadClass: TMessageDispatcherThreadClass;
begin
  result := TMessageDispatcherThread;
end;

procedure TThreadedMessageDispatcher.DoPostMessage(const Message: IMessage);
begin
  if FEnabled and (FTarget <> nil) and WillAcceptMessage(message.Id) then
  begin
    //Add To Queue
    FQueueLock.Acquire;
    try
      FQueue.Enqueue(message);
      TMessagingControl.IncrementGlobalQueueDepth;
    finally
      FQueueLock.Release;
    end;
    //Start a processing cycle
    FThread.Trigger;
  end;
end;


{ TMessageDispatcherThread }

constructor TMessageDispatcherThread.Create(const AOwner: TThreadedMessageDispatcher);
begin
  FDispatcher := AOwner;
  FTrigger := TEvent.Create(nil,false,false,'');
  FErrored := False;
  inherited Create;
end;

destructor TMessageDispatcherThread.Destroy;
begin
  FTrigger.Free;
  inherited;
end;

procedure TMessageDispatcherThread.Execute;
var
  msgs : TArray<IMessage>;
  i : integer;
  channel : IMessageChannel;
begin
  while not Terminated do
  begin
    FTrigger.WaitFor(INFINITE);
    if Self.Terminated then
    begin
      FDispatcher := nil;
      exit;
    end;
    while (not Self.Terminated) and (FDispatcher.FTarget <> nil) and (FDispatcher.FQueue.Count > 0) and FDispatcher.Enabled do
    begin
      msgs := FDispatcher.DequeueAtMost(TMessagingOptions.MaxBurst);
      for i := 0 to Length(msgs) -1 do
      begin
        try
          FDispatcher.FTarget.Dispatch(msgs[i].MessagePtr^);
        except
          on e : Exception do
          begin
            channel := FDispatcher.FChannel;  //keep the instance alive.
            if (channel <> nil) then
                 channel.HandleError(e.Message);
          end;
        end;
        msgs[i] := nil;
      end;
    end;
  end;
  FDispatcher := nil;
end;

procedure TMessageDispatcherThread.Shutdown;
begin
  Self.Terminate;
  Trigger;
end;


procedure TMessageDispatcherThread.Trigger;
begin
  FTrigger.SetEvent;
end;

{ TMessageDispatcherBase }

constructor TMessageDispatcherBase.Create(const target : TObject = nil);
begin
  inherited Create;
  FQueue := TQueue<IMessage>.Create;
  FTarget := target;
  FQueueLock := TCriticalSection.Create;
  FEnabled := True;
  FChannel := nil;

  FOwnerThreadId := TThread.CurrentThread.ThreadID;
end;

function TMessageDispatcherBase.DequeueAtMost(const count: integer): TArray<IMessage>;
var
  i: Integer;
  actualLen : integer;
begin
  SetLength(result,count);
  actualLen :=0;
  FQueueLock.Acquire;
  try
    for i := 0 to count - 1 do
    begin
      if FQueue.Count > 0 then
      begin
        Result[i] := FQueue.Dequeue;
        TMessagingControl.DecrementGlobalQueueDepth;
        Inc(actualLen);
      end
      else
      begin
        break;
      end;
    end;
  finally
    FQueueLock.Release;
  end;
  SetLength(result,actualLen);
end;

destructor TMessageDispatcherBase.Destroy;
begin
  FEnabled := False;
  SetChannel(nil);
  FQueue.Free;
  FQueueLock.Free;
  inherited;
end;

procedure TMessageDispatcherBase.DoSendMessage(const Message: IMessage);
begin
  if FEnabled and (FTarget <> nil) and WillAcceptMessage(message.Id) then
  begin
    FTarget.Dispatch(Message.MessagePtr^);
  end;
end;

function TMessageDispatcherBase.GetChannel: IMessageChannel;
begin
  result := FChannel;
end;


function TMessageDispatcherBase.GetEnabled: boolean;
begin
  result := FEnabled;
end;

function TMessageDispatcherBase.GetExcludeFilter: TArray<Cardinal>;
begin
  SetLength(result,0);
  if FExcludeFilter <> nil then
    result := FExcludeFilter.Keys.ToArray;
end;

function TMessageDispatcherBase.GetIncludeFilter: TArray<Cardinal>;
begin
  SetLength(result,0);
  if FIncludeFilter <> nil then
    result := FIncludeFilter.Keys.ToArray;
end;

function TMessageDispatcherBase.GetTarget: TObject;
begin
  result := FTarget;
end;


procedure TMessageDispatcherBase.PostMessage(const Message: IMessage);
begin
  DoPostMessage(Message);
end;

procedure TMessageDispatcherBase.SendMessage(const Message: IMessage);
begin
  DoSendMessage(Message);
end;

procedure TMessageDispatcherBase.SetChannel(const value: IMessageChannel);
var
  oldChannel : IMessageChannel;
begin
  oldChannel := FChannel;
  if (oldChannel <> nil) or  (value <> nil) then
  begin
    if (oldChannel <> nil)  then
      oldChannel.UnSubscribe(Self);
    FChannel := value;
    if FChannel <> nil then
      FChannel.Subscribe(Self);
  end;
end;

procedure TMessageDispatcherBase.SetEnabled(const value: boolean);
begin
  FEnabled := value;
end;

procedure TMessageDispatcherBase.SetExcludeFilter(const excludeFilter: TArray<Cardinal>);
var
  key : Cardinal;
begin
  if Length(excludeFilter) > 0 then
  begin
    if FExcludeFilter <> nil then
      FExcludeFilter.Clear
    else
      FExcludeFilter := TDictionary<Cardinal,byte>.Create;
      
    for key in excludeFilter do
    begin
      FExcludeFilter.AddOrSetValue(key,0);
    end;
  end
  else if FExcludeFilter <> nil then
    FreeAndNil(FExcludeFilter);
end;

procedure TMessageDispatcherBase.SetIncludeFilter(const includeFilter: TArray<Cardinal>);
var
  key : Cardinal;
begin
  if Length(includeFilter) > 0 then
  begin
    if FIncludeFilter <> nil then
      FIncludeFilter.Clear
    else
      FIncludeFilter := TDictionary<Cardinal,byte>.Create;
      
    for key in includeFilter do
    begin
      FIncludeFilter.AddOrSetValue(key,0);
    end;
  end
  else if FIncludeFilter <> nil then
    FreeAndNil(FIncludeFilter);
end;

procedure TMessageDispatcherBase.SetTarget(const value: TObject);
begin
  FTarget := Value;
end;

function TMessageDispatcherBase.WillAcceptMessage(const id: Cardinal): boolean;
begin
  result := false;
  if FIncludeFilter <> nil then
  begin
    if not FIncludeFilter.ContainsKey(id) then
      exit;
  end;
  if FExcludeFilter <> nil then
  begin
    if FExcludeFilter.ContainsKey(id) then
      exit;
  end;
  result := true;  
end;

{ TMessageDispatcherUIThread }

procedure TMessageDispatcherUIThread.Execute;
var
  threadProc : TThreadProcedure;
begin
  threadProc := procedure
                var
                  i : integer;
                  msgs : TArray<IMessage>;
                begin
                  msgs := FDispatcher.DequeueAtMost(TMessagingOptions.MaxBurst);
                  if Length(msgs) > 0 then
                  begin
                    for i := 0 to Length(msgs) -1 do
                    begin
                      if not FDispatcher.Enabled or Self.Terminated then
                        break;
                      try
                        FDispatcher.FTarget.Dispatch(msgs[i].MessagePtr^);
                      except
                        //not much we can do here!
                      end;
                      msgs[i] := nil;
                      //potential tight loop, yield occasionally
                      if TMessagingOptions.MaxBurst > 10 then
                      begin
                        if (i mod 10) = 0 then
                          TThread.Yield;
                      end;
                    end;
                  end;
                end;


  while not Terminated do
  begin
    FTrigger.WaitFor(INFINITE);
    if Self.Terminated then
    begin
      FDispatcher := nil;
      exit;
    end;
    while (not Self.Terminated) and (FDispatcher.FTarget <> nil) and (FDispatcher.FQueue.Count > 0) and FDispatcher.Enabled do
    begin
      //since we are queuing messages and want them to complete processing before the next one is dispatched, we cannot use TThread.Queue here.
      TThread.Synchronize(nil,threadProc);
      //calls SwitchToThread - Causes the calling thread to yield execution to another thread that is ready to run on the current processor.
      //The operating system selects the next thread to be executed.
      TThread.Yield;
    end;
  end;
  FDispatcher := nil;
end;



{ TUIMessageDispatcher }

procedure TUIMessageDispatcher.DoSendMessage(const Message: IMessage);
begin
  //if the message wasn't sent from the main thread then queue it!
  if Message.ThreadID <> FOwnerThreadId then
    DoPostMessage(Message)
  else
    inherited DoSendMessage(Message);
end;

function TUIMessageDispatcher.GetThreadClass: TMessageDispatcherThreadClass;
begin
  result := TMessageDispatcherUIThread;
end;

end.

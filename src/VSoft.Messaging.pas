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

unit VSoft.Messaging;

interface

uses
  VSoft.CancellationToken,
  VSoft.Messaging.Internal;

const
  //pass as a timeout to wait without a time limit
  cWaitForever = Cardinal($FFFFFFFF);

  //default for IMessageChannel.PushbackTimeout
  cDefaultPushbackTimeout = 10 * 60 * 1000;

type
  {$M+} //for mocking
  TMessageProc = procedure (const msg : IMessage) of object;

  TOnErrorMethod = procedure(const AMessage : string);

  //Describes an exception raised by a message handler, or a problem the library had to work around.
  TMessagingErrorInfo = record
    //class name of the exception, empty when the error was not an exception
    ExceptionClassName : string;
    ErrorMessage : string;
    //id of the message being handled, 0 if not known
    MessageId : Word;
  end;

  TMessagingErrorProc = reference to procedure(const info : TMessagingErrorInfo);

  //called on the posting thread, waitedMs is how long the post has been blocked so far
  TPushbackNotifyProc = reference to procedure(const waitedMs : Int64);

  TChannelStats = record
    //posts not yet handled by every dispatcher that accepted them
    Depth : integer;
    PeakDepth : integer;
    PostCount : Int64;
    //posts that had to wait for the backlog to drain, and how many of those gave up waiting
    PushbackWaits : Int64;
    PushbackTimeouts : Int64;
    TotalBlockedMs : Int64;
    LongestBlockedMs : Int64;
  end;

  IMessageDispatcher = interface;
  IMessageChannel = interface;

  //What TChannelHelper posts through. Implemented by the channel internals.
  IChannelQueue = interface
  ['{6C2E1E4B-5E0B-4E2C-9C79-7D8B1F0A3D21}']
    procedure PostMessage(const message : IMessage);
    procedure SendMessage(const message : IMessage);
  end;

  TChannelHelper = record
  private
    FQueue : IChannelQueue;
    FPostMessageProc : TMessageProc;
    FSendMessageProc : TMessageProc;
    procedure DoPost(const msg : IMessage);
    procedure DoSend(const msg : IMessage);
  public
    //async
//    procedure PostMessage<T : record>(const message : T); //12.3 breaks the record constraint
    procedure PostMessage<T>(const message : T);
    //sync
//    procedure SendMessage<T : record>(const message : T); //12.3 breaks the record constraint
    procedure SendMessage<T>(const message : T);
    constructor Create(const postMessageProc : TMessageProc; const sendMessageProc : TMessageProc); overload;
    constructor Create(const queue : IChannelQueue); overload;
  end;

  IMessageChannel = interface
  ['{1C1C844F-A48B-4546-8E5B-C0B056F6343A}']
    function GetEnabled: boolean;
    procedure SetEnabled(const Value: boolean);
    function GetOwnerID : string;
    procedure SetOwnerID(const value : string);
    function GetOnError : TOnErrorMethod;
    procedure SetOnError(const value : TOnErrorMethod);
    function GetOnErrorEx : TMessagingErrorProc;
    procedure SetOnErrorEx(const value : TMessagingErrorProc);
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

    procedure Subscribe(const value: IMessageDispatcher);
    procedure UnSubscribe(const value: IMessageDispatcher);
    procedure HandleError(const errorMessage : string);

    //Overrides TMessagingOptions.BlockedThreshold/UnblockThreshold for this channel.
    //unblock is clamped to 0..blocked - 1
    procedure SetThresholds(const blocked : integer; const unblock : integer);
    //goes back to following TMessagingOptions
    procedure ResetThresholds;
    //lets every post that is currently blocked by backpressure go ahead
    procedure ReleaseWaiters;
    //Waits until every message posted to this channel before the call has been handled, or discarded,
    //by every dispatcher that accepted it. On the main thread it keeps processing synchronize calls and
    //UI dispatchers while it waits. Returns false on timeout, or when called from a handler of a
    //message posted to this channel (that message cannot finish while the handler waits).
    function Flush(const timeoutMs : Cardinal = cWaitForever) : boolean;

    //when enabled is false, postmessage just ignores the message.
    property Enabled : boolean read GetEnabled write SetEnabled;
    //FB specific, used in a proxy channel only.
    property IncludeParentID : boolean read GetIncludeParentID write SetIncludeParentID;
    //FB specific, we use it for project id on variable message channel.
    property OwnerID : string read GetOwnerID write SetOwnerID;
    //called on the dispatcher thread when a handler raises
    property OnError : TOnErrorMethod read GetOnError write SetOnError;
    //as OnError, with the exception class and message id
    property OnErrorEx : TMessagingErrorProc read GetOnErrorEx write SetOnErrorEx;
    //Hacky work around for non generic interfaces not supporting generics.
    property Queue : TChannelHelper read GetQueue;

    //Backpressure : once this many posts are waiting to be handled, a thread other than the main
    //thread or a dispatcher thread that posts waits until the backlog is down to UnblockThreshold.
    property BlockedThreshold : integer read GetBlockedThreshold;
    property UnblockThreshold : integer read GetUnblockThreshold;
    property BackpressureEnabled : boolean read GetBackpressureEnabled write SetBackpressureEnabled;
    //while this token is cancelled posts never wait, and posts that are waiting go ahead
    property PushbackCancellation : ICancellationToken read GetPushbackCancellation write SetPushbackCancellation;
    //a blocked post gives up waiting after this many ms (cWaitForever to never give up)
    property PushbackTimeout : Cardinal read GetPushbackTimeout write SetPushbackTimeout;
    //OnPushbackWait is called once a post has been blocked for this many ms
    property PushbackWaitNotifyAfter : Cardinal read GetPushbackWaitNotifyAfter write SetPushbackWaitNotifyAfter;
    property OnPushbackWait : TPushbackNotifyProc read GetOnPushbackWait write SetOnPushbackWait;
    property OnPushbackTimeout : TPushbackNotifyProc read GetOnPushbackTimeout write SetOnPushbackTimeout;
    property Stats : TChannelStats read GetStats;
  end;

  IMessageDispatcher = interface
  ['{20D6B29C-242C-4BF5-BE2D-7CD755927BC5}']
    procedure SetChannel(const value: IMessageChannel);
    function GetChannel: IMessageChannel;
    procedure SetTarget(const value : TObject);
    function GetTarget : TObject;
    function GetEnabled : boolean;
    procedure SetEnabled(const value : boolean);
    procedure SetIncludeFilter(const includeFilter : TArray<Cardinal>);
    function GetIncludeFilter : TArray<Cardinal>;
    procedure SetExcludeFilter(const excludeFilter : TArray<Cardinal>);
    function GetExcludeFilter : TArray<Cardinal>;
    function GetPendingCount : integer;

    //Waits until every message queued for this dispatcher before the call has been handled.
    //Returns false on timeout, or when called from one of this dispatcher's own handlers.
    function WaitUntilIdle(const timeoutMs : Cardinal = cWaitForever) : boolean;

    //Setting Channel to nil only unsubscribes, messages already queued are still delivered.
    property Channel : IMessageChannel read GetChannel write SetChannel;
    //Setting Target to nil discards queued messages and waits for a handler that is running on
    //another thread to finish, so once it returns the old target receives nothing more.
    property Target : TObject read GetTarget write SetTarget;
    //Disabling discards queued messages, it does not wait for a handler that is already running.
    property Enabled : boolean read GetEnabled write SetEnabled;
    property IncludeFilter : TArray<Cardinal> read GetIncludeFilter write SetIncludeFilter;
    property ExcludeFilter : TArray<Cardinal> read GetExcludeFilter write SetExcludeFilter;
    //messages queued or being handled
    property PendingCount : integer read GetPendingCount;
  end;
  {$M-}

  TMessageFiller = array[1..12] of Byte;
  {
    TMessageFiller keeps message records laid out the way they always have been. Messages are
    only ever passed to their message handler methods, never to DefaultHandler, so it is no
    longer needed for messages handled on forms or controls.
    e.g:

    type
      TMyMessage = record
        MsgID : Cardinal; //note, keep the ids within 1..$BFFF
        Filler : TMessageFiller;
        //message payload here
        MyName : string;
      end;
  }

  TMessageID = Cardinal;


  TMessageChannelFactory = class
  public
    class function CreateChannel : IMessageChannel;
  end;

  TMessageDispatcherFactory = class
  public
    class function CreateDispatcher(const target : TObject = nil) : IMessageDispatcher;
    class function CreateUIDispatcher(const target : TObject = nil) : IMessageDispatcher;
  end;

  TMessagingOptions = class
  private
    class var
      FMaxBurst           : integer; // Maximum number of messages to dequeue and process in one go.
      FBlockedThreshold   : integer; // Default for channels : start blocking posters at this many waiting posts
      FUnblockedThreshold : integer; // Default for channels : release them at this many
  protected
    class procedure SetMaxBurst(const Value: integer); static;
    class procedure SetBlockedThreshold(const Value: integer); static;
    class procedure SetUnblockThreshold(const Value: integer); static;
    class constructor Create;
  public
    //Sets both thresholds. blocked is at least 2, unblock is clamped to 0..blocked - 1
    class procedure SetThresholds(const blocked : integer; const unblock : integer); static;
    //applies the rules above to a pair of thresholds
    class procedure ValidateThresholds(var blocked : integer; var unblock : integer); static;

    class property MaxBurst : integer read FMaxBurst write SetMaxBurst;  // Maximum number of messages to dequeue and process in one go.
    //Lowering BlockedThreshold below UnblockThreshold lowers UnblockThreshold too
    class property BlockedThreshold : integer read FBlockedThreshold write SetBlockedThreshold;
    class property UnblockThreshold : integer read FUnblockedThreshold write SetUnblockThreshold;
  end;

implementation

uses
  System.Classes,
  System.TypInfo,
  VSoft.Messaging.Channel,
  VSoft.Messaging.Dispatchers;

const
  cDefaultMaxBurst           = 20;  // Maximum number of messages to dequeue and process in one go.
  cDefaultBlockedThreshold   = 100; // Start waiting for queues to unblock at this channel depth
  cDefaultUnblockedThreshold = 50;  // Stop waiting at this depth (must be at least one less than block threshold)



{ TChannelHelper }

constructor TChannelHelper.Create(const postMessageProc : TMessageProc; const sendMessageProc : TMessageProc);
begin
  Assert(Assigned(postMessageProc), 'postMessageProc cannot be nil!');
  Assert(Assigned(sendMessageProc), 'sendMessageProc cannot be nil!');
  FQueue := nil;
  FPostMessageProc := postMessageProc;
  FSendMessageProc := sendMessageProc
end;

constructor TChannelHelper.Create(const queue : IChannelQueue);
begin
  Assert(queue <> nil, 'queue cannot be nil!');
  FQueue := queue;
  FPostMessageProc := nil;
  FSendMessageProc := nil;
end;

procedure TChannelHelper.DoPost(const msg : IMessage);
begin
  if FQueue <> nil then
    FQueue.PostMessage(msg)
  else
    FPostMessageProc(msg);
end;

procedure TChannelHelper.DoSend(const msg : IMessage);
begin
  if FQueue <> nil then
    FQueue.SendMessage(msg)
  else
    FSendMessageProc(msg);
end;

procedure TChannelHelper.PostMessage<T>(const message: T);
var
  msg : IMessage;
begin
  //12.3 breaks the record constraint so doing runtime test instead.
  //GetTypeKind is not available before XE7, tkMRecord was added in 10.3
  Assert(PTypeInfo(TypeInfo(T)).Kind in [tkRecord{$IF CompilerVersion >= 33.0}, tkMRecord{$IFEND}]);
  //TObject.Dispatch only looks up handlers for ids 1..$BFFF
  Assert((PWord(@message)^ <> 0) and (PWord(@message)^ < $C000), 'message ids must be in the range 1..$BFFF');
  msg := TVSMessageWrapper<T>.Create(message);
  DoPost(msg);
end;

procedure TChannelHelper.SendMessage<T>(const message: T);
var
  msg : IMessage;
begin
  //12.3 breaks the record constraint so doing runtime test instead.
  //GetTypeKind is not available before XE7, tkMRecord was added in 10.3
  Assert(PTypeInfo(TypeInfo(T)).Kind in [tkRecord{$IF CompilerVersion >= 33.0}, tkMRecord{$IFEND}]);
  Assert((PWord(@message)^ <> 0) and (PWord(@message)^ < $C000), 'message ids must be in the range 1..$BFFF');
  msg := TVSMessageWrapper<T>.Create(message);
  DoSend(msg);
end;

{ TMessageChannelFactory }

class function TMessageChannelFactory.CreateChannel: IMessageChannel;
begin
  result := TVSoftMessageChannel.Create;
end;

{ TMessageDispatcherFactory }

class function TMessageDispatcherFactory.CreateDispatcher(const target : TObject): IMessageDispatcher;
begin
  result := TThreadedMessageDispatcher.Create(target);
end;

class function TMessageDispatcherFactory.CreateUIDispatcher(const target : TObject = nil): IMessageDispatcher;
begin
  result := TUIMessageDispatcher.Create(target);
end;

{ TMessagingOptions }

class constructor TMessagingOptions.Create;
begin
  FMaxBurst := cDefaultMaxBurst;
  FBlockedThreshold := cDefaultBlockedThreshold;
  FUnblockedThreshold := cDefaultUnblockedThreshold;
end;

class procedure TMessagingOptions.ValidateThresholds(var blocked : integer; var unblock : integer);
begin
  if blocked < 2 then
    blocked := 2;
  if unblock < 0 then
    unblock := 0
  else if unblock > blocked - 1 then
    unblock := blocked - 1;
end;

class procedure TMessagingOptions.SetThresholds(const blocked : integer; const unblock : integer);
var
  newBlocked : integer;
  newUnblock : integer;
begin
  newBlocked := blocked;
  newUnblock := unblock;
  ValidateThresholds(newBlocked, newUnblock);
  FBlockedThreshold := newBlocked;
  FUnblockedThreshold := newUnblock;
end;

class procedure TMessagingOptions.SetBlockedThreshold(const Value: integer);
var
  newBlocked : integer;
  newUnblock : integer;
begin
  newBlocked := Value;
  newUnblock := FUnblockedThreshold;
  ValidateThresholds(newBlocked, newUnblock);
  FBlockedThreshold := newBlocked;
  FUnblockedThreshold := newUnblock;
end;

class procedure TMessagingOptions.SetMaxBurst(const Value: integer);
begin
  if value > 0 then
    FMaxBurst := Value
  else
    FMaxBurst := 1;
end;

class procedure TMessagingOptions.SetUnblockThreshold(const Value: integer);
var
  newBlocked : integer;
  newUnblock : integer;
begin
  newBlocked := FBlockedThreshold;
  newUnblock := Value;
  ValidateThresholds(newBlocked, newUnblock);
  FUnblockedThreshold := newUnblock;
end;

end.

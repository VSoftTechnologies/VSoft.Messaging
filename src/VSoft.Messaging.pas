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
  VSoft.Messaging.Internal;

type
  {$M+} //for mocking
  TMessageProc = procedure (const msg : IMessage) of object;

  TOnErrorMethod = procedure(const AMessage : string);

  IMessageDispatcher = interface;
  IMessageChannel = interface;

  TChannelHelper = record
  private
    FPostMessageProc : TMessageProc;
    FSendMessageProc : TMessageProc;
  public
    //async
    procedure PostMessage<T : record>(const message : T);
    //sync
    procedure SendMessage<T : record>(const message : T);
    constructor Create(const postMessageProc : TMessageProc; const sendMessageProc : TMessageProc);
  end;

  IMessageChannel = interface
  ['{1C1C844F-A48B-4546-8E5B-C0B056F6343A}']
    function GetEnabled: boolean;
    procedure SetEnabled(const Value: boolean);
    function GetOwnerID : string;
    procedure SetOwnerID(const value : string);
    function GetOnError : TOnErrorMethod;
    procedure SetOnError(const value : TOnErrorMethod);
    function GetIncludeParentID: boolean;
    procedure SetIncludeParentID(const Value: boolean);
    function GetQueue : TChannelHelper;

    procedure Subscribe(const value: IMessageDispatcher);
    procedure UnSubscribe(const value: IMessageDispatcher);
    procedure HandleError(const errorMessage : string);

    //when enabled is false, postmessage just ignores the message.
    property Enabled : boolean read GetEnabled write SetEnabled;
    //FB specific, used in a proxy channel only.
    property IncludeParentID : boolean read GetIncludeParentID write SetIncludeParentID;
    //FB specific, we use it for project id on variable message channel.
    property OwnerID : string read GetOwnerID write SetOwnerID;
    property OnError : TOnErrorMethod read GetOnError write SetOnError;
    //Hacky work around for non generic interfaces not supporting generics.
    property Queue : TChannelHelper read GetQueue;
  end;

  //TODO : add filtering so we can avoid calling dispatch on messages we don't want to handle
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

    property Channel : IMessageChannel read GetChannel write SetChannel;
    property Target : TObject read GetTarget write SetTarget;
    property Enabled : boolean read GetEnabled write SetEnabled;
    property IncludeFilter : TArray<Cardinal> read GetIncludeFilter write SetIncludeFilter;
    property ExcludeFilter : TArray<Cardinal> read GetExcludeFilter write SetExcludeFilter;
  end;
  {$M-}

  TMessageFiller = array[1..12] of Byte;
  {
    TMessageFiller is needed in all messages to ensure the message is at
    least as big as TMessage in WinApi.Windows,
    e.g:

    type
      TMyMessage = record
        MsgID : Cardinal; //note, keep the ids within word range.
        Filler : TMessageFiller;
        //message payload here
        MyName : string;
      end;

      This is required, because a message may end up being passed to a winproc (if you use this
      on forms or controls), which assumes at least a TMessage in size.

      On non windows platforms, you can probably get away with (not tested yet) :

      TMyMessage = record
        MsgID : Word;
        //message payload goes here.
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
      FMaxBurst           : integer;  // Maxium number of message to dequeue and process in one go.
      FBlockedThreshold   : integer; // Start waiting for queues to unblock at this global queue depth
      FUnblockedThreshold : integer;
  protected
    class procedure SetMaxBurst(const Value: integer); static;  
    class procedure SetBlockedThreshold(const Value: integer); static;
    class procedure SetUnblockThreashold(const Value: integer); static;
    class constructor Create;
  public
    class property MaxBurst : integer read FMaxBurst write SetMaxBurst;  // Maxium number of message to dequeue and process in one go.
    class property BlockedThreshold : integer read FBlockedThreshold write SetBlockedThreshold; // Start waiting for queues to unblock at this global queue depth
    class property UnblockThreshold : integer read FUnblockedThreshold write SetUnblockThreashold; //Stop waiting at this queue depth  (must be at least one less than block threshold)
  end;

implementation

uses
  System.Classes,
  VSoft.Messaging.Channel,
  VSoft.Messaging.Dispatchers;

const
  cDefaultMaxBurst           = 20;  // Maxium number of message to dequeue and process in one go.
  cDefaultBlockedThreshold   = 100; // Start waiting for queues to unblock at this global queue depth
  cDefaultUnblockedThreshold = 50;  // Stop waiting at this queue depth  (must be at least one less than block threshold)
  


{ TChannelHelper }

constructor TChannelHelper.Create(const postMessageProc : TMessageProc; const sendMessageProc : TMessageProc);
begin
  Assert(Assigned(postMessageProc), 'postMessageProc cannot be nil!');
  Assert(Assigned(sendMessageProc), 'sendMessageProc cannot be nil!');
  FPostMessageProc := postMessageProc;
  FSendMessageProc := sendMessageProc
end;

procedure TChannelHelper.PostMessage<T>(const message: T);
var
  msg : IMessage;
begin
  msg := TVSMessageWrapper<T>.Create(message);
  FPostMessageProc(msg);
end;

procedure TChannelHelper.SendMessage<T>(const message: T);
var
  msg : IMessage;
begin
  msg := TVSMessageWrapper<T>.Create(message);
  FSendMessageProc(msg);
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

class procedure TMessagingOptions.SetBlockedThreshold(const Value: integer);
begin
  if value > 1  then
    FBlockedThreshold := Value
  else
    FBlockedThreshold := 2;
end;

class procedure TMessagingOptions.SetMaxBurst(const Value: integer);
begin
  if value > 0 then
    FMaxBurst := Value
  else
    FMaxBurst := 1; 
end;

class procedure TMessagingOptions.SetUnblockThreashold(const Value: integer);
begin
  if value < FBlockedThreshold -1 then
    FUnblockedThreshold := Value
  else
    FUnblockedThreshold := FBlockedThreshold -1; 
end;

end.

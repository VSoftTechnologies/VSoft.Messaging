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
  classes,
  Generics.Collections,
  VSoft.WeakReference,
  VSoft.Messaging.Internal,
  VSoft.Messaging;

type
  //this is used by a channel proxy in FinalBuider.
  IMessageChannelPost = interface
  ['{63A6480D-67E5-42C3-853B-2E6843715BC4}']
    procedure PostMessage(const message : IMessage);
  end;

  TVSoftMessageChannel = class(TWeakReferencedObject,IMessageChannel, IMessageChannelPost)
  private
    FDispatchersLock : TObject;
    FDispatchers : TList<IMessageDispatcher>;
    FEnabled  : boolean;
    FOnError : TOnErrorMethod;
    FOwnerID : string;
    FIncludeParentID : boolean;
    FHelper : TChannelHelper;
  protected
    function GetOnError : TOnErrorMethod;
    procedure SetOnError(const value : TOnErrorMethod);

    function GetOwnerID: string;
    procedure SetOwnerID(const Value: string);

    function GetEnabled: boolean;
    procedure SetEnabled(const Value: boolean);
    function GetIncludeParentID: boolean;
    procedure SetIncludeParentID(const Value: boolean);
    function GetQueue : TChannelHelper;

    procedure PostMessage(const message : IMessage);
    procedure SendMessage(const message : IMessage);

    procedure Subscribe(const value: IMessageDispatcher);
    procedure UnSubscribe(const value: IMessageDispatcher);

    procedure HandleError(const errorMessage : string);
  public
    constructor Create;
    destructor Destroy; override;
  end;

implementation

uses
  System.Rtti,
  System.SysUtils,
  VSoft.Messaging.Dispatchers;

{ TVSoftMessageChannel }
constructor TVSoftMessageChannel.Create;
begin
  inherited;
  FDispatchers := TList<IMessageDispatcher>.Create;
  FDispatchersLock := TObject.Create;
  FEnabled := true;
  FHelper := TChannelHelper.Create(self.PostMessage, self.SendMessage);
end;

destructor TVSoftMessageChannel.Destroy;
begin
  if FDispatchers <> nil then
    FDispatchers.Clear;
  FreeAndNil(FDispatchers);
  FreeAndNil(FDispatchersLock);
  inherited;
end;

function TVSoftMessageChannel.GetEnabled: boolean;
begin
  result := FEnabled;
end;

function TVSoftMessageChannel.GetIncludeParentID: boolean;
begin
  result := FIncludeParentID;
end;

function TVSoftMessageChannel.GetOnError: TOnErrorMethod;
begin
  result := FOnError;
end;

function TVSoftMessageChannel.GetOwnerID: string;
begin
  result := FOwnerID;
end;

function TVSoftMessageChannel.GetQueue: TChannelHelper;
begin
  result := FHelper;
end;

procedure TVSoftMessageChannel.HandleError(const errorMessage: string);
begin
  if Assigned(FOnError) then
    FOnError(errorMessage);
end;

procedure TVSoftMessageChannel.PostMessage(const message : IMessage);
var
  dispatchers : TArray<IMessageDispatcher>;
  dispatcher: IMessageDispatcher;
  i : integer;
begin
  if FEnabled  then
  begin
    TMessagingControl.PushbackIfNeeded;
    MonitorEnter(FDispatchersLock);
    try
      //make sure the list of handlers doesn't change
      dispatchers := FDispatchers.ToArray();
    finally
      MonitorExit(FDispatchersLock);
    end;
    for i := 0 to Length(dispatchers) -1 do
    begin
      dispatcher := dispatchers[i];
      if dispatcher.Enabled then
      (dispatcher as IMessageDispatcherPost).PostMessage(message);
    end;
  end;
end;



procedure TVSoftMessageChannel.SendMessage(const message: IMessage);
var
  dispatchers : TArray<IMessageDispatcher>;
  dispatcher: IMessageDispatcher;
  i : integer;
begin
  if FEnabled  then
  begin
    //TMessagingControl.PushbackIfNeeded;
    MonitorEnter(FDispatchersLock);
    try
      //make sure the list of handlers doesn't change
      dispatchers := FDispatchers.ToArray();
    finally
      MonitorExit(FDispatchersLock);
    end;
    for i := 0 to Length(dispatchers) -1 do
    begin
      dispatcher := dispatchers[i];
      if dispatcher.Enabled then
      (dispatcher as IMessageDispatcherPost).SendMessage(message);
    end;
  end;
end;

procedure TVSoftMessageChannel.SetEnabled(const Value: boolean);
begin
  FEnabled := value;
end;

procedure TVSoftMessageChannel.SetIncludeParentID(const Value: boolean);
begin
  FIncludeParentID := value;
end;

procedure TVSoftMessageChannel.SetOnError(const Value: TOnErrorMethod);
begin
  FOnError := value;
end;

procedure TVSoftMessageChannel.SetOwnerID(const Value: string);
begin
  FOwnerID := value;
end;

procedure TVSoftMessageChannel.Subscribe(const value: IMessageDispatcher);
begin
  MonitorEnter(FDispatchersLock);
  try
  if not FDispatchers.Contains(value) then
    FDispatchers.Add(value);
  finally
    MonitorExit(FDispatchersLock);
  end;
end;

procedure TVSoftMessageChannel.UnSubscribe(const value: IMessageDispatcher);
begin
  MonitorEnter(FDispatchersLock);
  try
    if FDispatchers.Contains(value) then
      FDispatchers.Remove(value);
  finally
    MonitorExit(FDispatchersLock);
  end;
end;

end.

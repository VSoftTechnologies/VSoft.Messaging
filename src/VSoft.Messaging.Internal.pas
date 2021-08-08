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

unit VSoft.Messaging.Internal;

interface

uses
  System.Classes,
  System.SyncObjs;

type
  //used to control pushback etc
  TMessagingControl = class
  private
    class var
      FGlobalQueueDepth : integer;
      FUnblockedSignal : TEvent;
  public
    class constructor Create;
    class destructor Destroy;

    class function GetGlobalQueueDepth : integer;
    class procedure PushbackIfNeeded;
    class procedure IncrementGlobalQueueDepth;
    class procedure DecrementGlobalQueueDepth;
  end;

  {$M+} //for mocking

  IMessage = interface
    ['{8E3E653D-D9B7-4B2B-81FA-9E6A035B3D4B}']
    function MessagePtr : Pointer;
    function GetMessageID : Word;
    function GetThreadID : TThreadID;
    property Id : Word read GetMessageID;
    property ThreadID : TThreadID read GetThreadID;
  end;
  {$M-}

  //used to transport the message from the channel to the dispatchers
  //wraps up the message record.
  TVSMessageWrapper<T> = class(TInterfacedObject,IMessage)
  private
    FMsgData: T;
    FThreadID : TThreadID;
  protected
    function MessagePtr : Pointer;
    function GetMessageID : Word;
    function GetThreadID : TThreadID;
  public
    constructor Create(const AMsgData: T);
    destructor Destroy;override;
  end;

implementation

uses
  VSoft.Messaging;

{ TMessagingControl }

class constructor TMessagingControl.Create;
begin
  FUnblockedSignal := TEvent.Create(nil, True, True, '', False); // Manual reset only
end;

class procedure TMessagingControl.DecrementGlobalQueueDepth;
begin
  if TInterlocked.Decrement(FGlobalQueueDepth) = TMessagingOptions.UnblockThreshold then
    FUnblockedSignal.SetEvent; // Unblock "PushbackIfNeeded"
end;

class destructor TMessagingControl.Destroy;
begin
  FUnblockedSignal.Free;
end;

class function TMessagingControl.GetGlobalQueueDepth: integer;
begin
  Result := FGlobalQueueDepth;
end;

class procedure TMessagingControl.IncrementGlobalQueueDepth;
begin
  if TInterlocked.Increment(FGlobalQueueDepth) = TMessagingOptions.BlockedThreshold then
    FUnblockedSignal.ResetEvent; // Block "PushbackIfNeeded"
end;

class procedure TMessagingControl.PushbackIfNeeded;
var
  res : TWaitResult;
begin
  if FGlobalQueueDepth < TMessagingOptions.BlockedThreshold then
    exit;

  if MainThreadID = TThread.CurrentThread.ThreadID then
    exit; // Never pushback from main thread, it could deadlock the app.

  res := FUnblockedSignal.WaitFor(10 * 60 * 1000); // If queue has been jammed for 10 minutes, something has gone very wrong

  if res = wrTimeout then
  begin
    FUnblockedSignal.SetEvent; //unblock anyway;
  end;
end;


{ TVSMessageWrapper<T> }

constructor TVSMessageWrapper<T>.Create(const AMsgData: T);
begin
  inherited Create;
  Assert(SizeOf(AMsgData) > 12);
  FMsgData := AMsgData;
  FThreadID := TThread.Current.ThreadID;
end;

destructor TVSMessageWrapper<T>.Destroy;
begin
  inherited;
end;

function TVSMessageWrapper<T>.GetMessageID: Word;
begin
  result := PWord(@FMsgData)^;
end;

function TVSMessageWrapper<T>.GetThreadID: TThreadID;
begin
  result := FThreadID;
end;

function TVSMessageWrapper<T>.MessagePtr: Pointer;
begin
  result := @FMsgData;
end;



end.

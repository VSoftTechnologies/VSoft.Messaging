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
  TMessagingControl = class
  private
    class var
      FGlobalQueueDepth : integer;
  public
    //The number of posted messages, across every channel, that have not yet been handled (or discarded)
    //by every dispatcher that accepted them. 0 means everything posted so far has been handled.
    //Prefer IMessageChannel.Flush or IMessageDispatcher.WaitUntilIdle to polling this.
    class function GetGlobalQueueDepth : integer;
    //Kept for compatibility. They only adjust the value returned by GetGlobalQueueDepth.
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
  end;

//The calling thread's id. Unlike TThread.CurrentThread it does not create a TExternalThread
//object for threads that were not started by Delphi.
function CurrentThreadId : TThreadID;

function IsMainThread : boolean;

//True on threads the library runs message handlers on. Backpressure never blocks these threads,
//since they are the ones that drain the queues.
function IsDispatcherThread : boolean;
procedure MarkAsDispatcherThread;

implementation

{$IFDEF MSWINDOWS}
uses
  Winapi.Windows;
{$ENDIF}

threadvar
  GIsDispatcherThread : boolean;

function CurrentThreadId : TThreadID;
begin
  {$IFDEF MSWINDOWS}
  result := Winapi.Windows.GetCurrentThreadId;
  {$ELSE}
  result := TThread.CurrentThread.ThreadID;
  {$ENDIF}
end;

function IsMainThread : boolean;
begin
  result := CurrentThreadId = MainThreadID;
end;

function IsDispatcherThread : boolean;
begin
  result := GIsDispatcherThread;
end;

procedure MarkAsDispatcherThread;
begin
  GIsDispatcherThread := true;
end;

{ TMessagingControl }

class procedure TMessagingControl.DecrementGlobalQueueDepth;
begin
  TInterlocked.Decrement(FGlobalQueueDepth);
end;

class function TMessagingControl.GetGlobalQueueDepth: integer;
begin
  result := TInterlocked.CompareExchange(FGlobalQueueDepth, 0, 0);
end;

class procedure TMessagingControl.IncrementGlobalQueueDepth;
begin
  TInterlocked.Increment(FGlobalQueueDepth);
end;

{ TVSMessageWrapper<T> }

constructor TVSMessageWrapper<T>.Create(const AMsgData: T);
begin
  inherited Create;
  //the record must start with the message id
  Assert(SizeOf(AMsgData) >= SizeOf(Word));
  FMsgData := AMsgData;
  FThreadID := CurrentThreadId;
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

program ShutdownCheck;

//Checks what happens at shutdown to dispatchers their owners never released (review finding M8).
//It leaks a threaded dispatcher whose handler is stuck, an idle one, and a UI dispatcher with messages
//that were never delivered, then exits. The process should end within a few seconds (the library
//waits at most 2 seconds for each stuck dispatcher) with exit code 0 and without an access violation.
//
//Build from this folder, e.g. with Delphi 13 :
//  dcc32 -B -NSSystem;Winapi -U..\..\src;<VSoft.CancellationToken source folder> ShutdownCheck.dpr

{$APPTYPE CONSOLE}

uses
  System.SysUtils,
  System.Classes,
  System.SyncObjs,
  VSoft.Messaging.Internal,
  VSoft.Messaging,
  VSoft.Messaging.Channel,
  VSoft.Messaging.Dispatchers;

const
  MSG_STUCK = $0400 + $1000;
  MSG_VALUE = $0400 + $1001;

type
  TCheckMessage = record
    MsgID : TMessageID;
    Filler : TMessageFiller;
    Value : integer;
  end;

  TStuckReceiver = class
  protected
    procedure HandleStuck(var msg : TCheckMessage); message MSG_STUCK;
  end;

  TValueReceiver = class
  protected
    procedure HandleValue(var msg : TCheckMessage); message MSG_VALUE;
  end;

var
  GStuckEntered : TEvent;

procedure TStuckReceiver.HandleStuck(var msg : TCheckMessage);
begin
  GStuckEntered.SetEvent;
  //much longer than the library waits at shutdown
  Sleep(10000);
end;

procedure TValueReceiver.HandleValue(var msg : TCheckMessage);
begin
end;

function MakeMessage(const id : Cardinal) : TCheckMessage;
begin
  FillChar(result, SizeOf(result), 0);
  result.MsgID := id;
end;

//the reference is never released, so the dispatcher is still alive when the units are finalized
procedure Leak(const dispatcher : IMessageDispatcher);
begin
  dispatcher._AddRef;
end;

procedure Run;
var
  channel : IMessageChannel;
  stuck : IMessageDispatcher;
  idle : IMessageDispatcher;
  ui : IMessageDispatcher;
  i : integer;
begin
  GStuckEntered := TEvent.Create(nil, true, false, '');
  channel := TMessageChannelFactory.CreateChannel;

  stuck := TMessageDispatcherFactory.CreateDispatcher(TStuckReceiver.Create);
  stuck.Channel := channel;
  idle := TMessageDispatcherFactory.CreateDispatcher(TValueReceiver.Create);
  idle.Channel := channel;
  //this program never processes synchronize calls, so nothing posted to this one is delivered
  ui := TMessageDispatcherFactory.CreateUIDispatcher(TValueReceiver.Create);
  ui.Channel := channel;

  channel.Queue.PostMessage<TCheckMessage>(MakeMessage(MSG_STUCK));
  for i := 1 to 10 do
    channel.Queue.PostMessage<TCheckMessage>(MakeMessage(MSG_VALUE));
  if GStuckEntered.WaitFor(5000) <> wrSignaled then
    raise Exception.Create('the stuck handler did not start');

  Leak(stuck);
  Leak(idle);
  Leak(ui);
  Writeln('leaked 3 dispatchers, one of them stuck in a handler, exiting');
end;

begin
  try
    Run;
  except
    on e : Exception do
    begin
      Writeln(e.ClassName, ' : ', e.Message);
      ExitCode := 1;
    end;
  end;
end.

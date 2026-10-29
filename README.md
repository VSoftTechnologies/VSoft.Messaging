# VSoft.Messaging

VSoft.Messaging is a library that provides an internal synchronous/asynchronous publish/subscribe messaging system for Delphi applications.

This library is used by [FinalBuilder](https://www.finalbuilder.com/finalbuilder) and has been working well for many years.

It supports Delphi XE2 or later, Win32 and Win64, and depends on [VSoft.CancellationToken](https://github.com/VSoftTechnologies/VSoft.CancellationToken).

## How it works

Messages are records. Receivers handle them with `message` methods, the same way windows messages are handled in Delphi, so a receiver can be any object. The library looks the handler up with the same dynamic method table `TObject.Dispatch` uses, and calls it directly.

A message is only ever passed to a handler for its id. Messages a receiver has no handler for are not queued for it at all, and are never passed to `DefaultHandler`. That makes forms, frames and controls safe receivers on Win64 too. If a receiver overrides `Dispatch` itself, every message is passed to its `Dispatch`.

### Messages

Messages are defined as records, with a MsgId field first.

````delphi
//create a unique message id in the range 1..$BFFF
//(on windows keep it above WM_USER, so it cannot be mistaken for a windows message)

const PROJECT_OPENED_MSG = WM_USER + $1000;
type
  TProjectOpenedMessage = record
    MsgID  : TMessageID;
    Filler : TMessageFiller;
    //payload starts here.
    ProjectName   : string;
  public
    constructor Create(const theProjectName : string);
  end;

implementation

constructor TProjectOpenedMessage.Create(const theProjectName : string);
begin
  MsgID := PROJECT_OPENED_MSG;  //assign our message id
  ProjectName := theProjectName; //store the payload.
end;

````

`TMessageID` is defined as `TMessageID = Cardinal;`. Only the low word is used, so ids must be in the range 1..$BFFF (`TObject.Dispatch` only looks up handlers in that range). The library asserts this when a message is posted or sent, and when filters are set.

The `Filler` field is no longer needed, since messages are never passed to `DefaultHandler`. It is kept so existing message records keep their layout. A message can be as small as a `Word` id.

A message is copied when it is posted, so changing your copy afterwards has no effect. Every receiver of a post gets a reference to the same copy, so handlers should treat messages as read only.

### Channels

Messages are sent through a channel object. To create a channel, use the `TMessageChannelFactory.CreateChannel` method

````delphi
var
  channel : IMessageChannel;
begin
  channel := TMessageChannelFactory.CreateChannel;
...
````

### Sending Messages

Example :

````delphi
procedure SendProjectOpenedMessage(const projectName : string)
var
  msg : TProjectOpenedMessage;
begin
  msg := TProjectOpenedMessage.Create(projectName);
  FChannel.Queue.PostMessage<TProjectOpenedMessage>(msg); //async
  //or
  FChannel.Queue.SendMessage<TProjectOpenedMessage>(msg); //sync
end;
````

Note that the reason the `PostMessage` and `SendMessage` are on the channel.Queue property is purely to work around the generics limitations in delphi, where non-generic interfaces cannot have generic methods (records can, so Queue is actually a record).

`Queue.PostMessage` sends the message asynchronously, ie the method returns before the message has been received.

`Queue.SendMessage` delivers the message to each dispatcher in turn, in the order they subscribed, and waits until that dispatcher has handled it before moving on. The handlers run where they always run : on the dispatcher's thread, or on the main thread for UI dispatchers, after any messages already queued for them. Some things to know :

- When `SendMessage` waits on the main thread it keeps processing `TThread.Synchronize`/`Queue` calls and UI dispatchers, so other handlers can run during the call.
- When `SendMessage` is called from inside one of a dispatcher's own handlers, that dispatcher handles the message straight away, since its queue cannot move until the handler returns.
- When waiting would deadlock, because the dispatcher being sent to is itself waiting on the sending thread, the message is queued without waiting, and the channel's `OnError`/`OnErrorEx` is told.
- A send from a background thread to a UI dispatcher waits for the main thread, just like `TThread.Synchronize` does.

### Receiving Messages

For an object to receive the messages, it needs to create a Dispatcher, and hook the dispatcher to itself and the channel

````delphi
type
  TReceiver = class
  private
    FDispatcher : IMessageDispatcher;
  public
    constructor Create(const channel : IMessageChannel);
  end;

implementation

constructor TReceiver.Create(const channel : IMessageChannel);
begin
  FDispatcher := TMessageDispatcherFactory.CreateDispatcher;

  // tell the dispatcher where to dispatch the messages to.
  FDispatcher.Target := Self;

  //hook up the dispatcher to the channel.
  FDispatcher.Channel := channel;
end;

````

The channel never holds a reference to the dispatcher you hold, so releasing it is all the cleanup you need. Releasing the last reference unsubscribes the dispatcher, discards the messages queued for it, and waits for a handler that is running on another thread to finish. Once it returns, the target receives nothing more and can be freed. A handler can free its own receiver.

The other properties :

- `Channel := nil` only unsubscribes. Messages already queued are still delivered, so a dispatcher can be detached from inside a handler and attached to another channel later.
- `Target := nil` discards the queued messages and waits for a handler running on another thread to finish.
- `Enabled := false` discards the queued messages, and does not wait for a handler that is already running. Only messages posted after it is enabled again are delivered.
- `IncludeFilter`/`ExcludeFilter` restrict which message ids are queued. They apply when a message is posted.
- `PendingCount` is the number of messages queued or being handled.

`TMessageDispatcherFactory.CreateDispatcher` runs the handlers on a thread of its own, the same thread for every message.

If you are handling messages on forms/frames/controls where you will be making UI updates, use `TMessageDispatcherFactory.CreateUIDispatcher`. Its handlers run on the main thread, delivered with `TThread.Queue`, so the main thread must process synchronize calls (VCL and FMX applications do, a console application or a service has to call `CheckSynchronize`). UI dispatchers do not have a thread each. Messages to a dispatcher are always handled in the order they were posted, and a handler that processes messages (e.g. `Application.ProcessMessages`) never has another handler of the same dispatcher run inside it.

### Handling Messages

Handling messages is just like handling windows messages in Delphi

````delphi
type
  TReceiver = class
  private
    ...
    procedure ProjectOpened(var msg : TProjectOpenedMessage); message PROJECT_OPENED_MSG;
    ...
  end;
implementation

procedure TReceiver.ProjectOpened(var msg : TProjectOpenedMessage);
begin
    //use msg.ProjectName payload here.
end;
````

### Errors

An exception raised by a handler is caught, and reported to the channel's `OnError` (a plain procedure, called with the message) and `OnErrorEx` (an anonymous method, called with the exception class, message and message id). Both are called on the thread the handler ran on. An exception raised by the error handlers is ignored, and the dispatcher carries on with the next message.

### Waiting for messages to be handled

`channel.Flush(timeoutMs)` waits until every message posted to the channel before the call has been handled, or discarded, by every dispatcher that accepted it. That includes messages still queued on a dispatcher that has since unsubscribed. `dispatcher.WaitUntilIdle(timeoutMs)` does the same for one dispatcher. On the main thread both keep processing synchronize calls and UI dispatchers while they wait. Both return false on timeout, and straight away when called from a handler that the wait would depend on.

## Backpressure

When something posts messages much faster than they are handled, the queues grow without limit, and whatever reports on the work (a UI, a log) ends up far behind the work itself. Backpressure slows the poster down instead.

Each channel counts the messages posted to it that have not yet been handled by every dispatcher that accepted them (its depth). Once the depth reaches the channel's blocked threshold, a post waits until the depth is down to the unblock threshold, then goes ahead. This only applies to threads other than the main thread and the library's own dispatcher threads, so a handler that posts never blocks, and posts nobody accepts are never held up.

````delphi
//this channel's thresholds, otherwise it follows TMessagingOptions.BlockedThreshold/UnblockThreshold (100/50)
channel.SetThresholds(50, 25);

//while the token is cancelled posts never wait, and posts already waiting go ahead
channel.PushbackCancellation := cancellationTokenSource.Token;

//a blocked post gives up waiting and goes ahead after this long (default 10 minutes)
channel.PushbackTimeout := 60000;
channel.OnPushbackTimeout := procedure(const waitedMs : Int64) begin ... end;

//called once a post has been blocked this long
channel.PushbackWaitNotifyAfter := 5000;
channel.OnPushbackWait := procedure(const waitedMs : Int64) begin ... end;

channel.ReleaseWaiters;               //let the posts that are waiting now go ahead
channel.BackpressureEnabled := false; //no blocking at all

stats := channel.Stats; //depth, peak depth, posts, waits, timeouts, time blocked
````

`TMessagingControl.GetGlobalQueueDepth` (in VSoft.Messaging.Internal) is the depth summed over every channel.

## Upgrading from 0.4.x

- `IMessageChannel` has new members. Classes that implement it themselves (e.g. a proxy channel) need to implement them, usually by forwarding to the real channel.
- Depth counts each post once, until every dispatcher that accepted it has handled it. It used to count each post once per dispatcher, until the dispatcher took it off its queue. A threshold of 100 now means 100 posts behind the slowest dispatcher, and `GetGlobalQueueDepth` = 0 means everything posted has been handled.
- Backpressure is per channel. A backlog on one channel no longer slows posters on another.
- Messages without a handler are no longer passed to `DefaultHandler`.
- `SendMessage` runs the handlers on the dispatcher's thread rather than the sending thread, and waits for UI dispatchers rather than posting to them.
- Disabling a dispatcher discards its queued messages.
- `TMessagingControl.PushbackIfNeeded`, `IMessageDispatcherPost` and the dispatcher thread classes are gone.
- The library now depends on VSoft.CancellationToken.

## Tests

`tests\VSoft.Messaging.Tests.dproj` is a DUnitX test project (Delphi 13, packages via DPM). The `Stress` category can be skipped with `--exclude:Stress`. `tests\ShutdownCheck` is a small console program that checks shutdown with dispatchers that were never released.

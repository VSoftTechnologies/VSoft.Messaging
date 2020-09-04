# VSoft.Messaging

VSoft.Messaging is a libary that provides an internal synchronous/asynchronous publish/subscribe messaging system for Delphi applications.

This library is used by [FinalBuilder](https://www.finalbuilder.com/finalbuilder) and has been working well for many years.

## How it works

This library takes advantage of `TObject.Dispatch` and dynamic methods which were introduced back in delphi 1.0. The Dispatch method is how windows messages are dispatched to message handler methods on TObject descendants. 

Whilst `TObject.Dispatch` was originally added for the purpose of dispatching windows messages, it's not actually tied to windows. That means this library should work on any platform that Delphi supports. That said, I have only tested this on Win32/Win64 (vcl and fmx).

### Messages

Messages are defined as records, with a MsgId field.

````delphi
//create a unique message id.
//on windows this needs to be higher than WM_USER

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

`TMessageID` is defined as `TMessageID = Cardinal;`



The Filler field is only needed if you are handling messages in VCL or FMX (on Windows) applications, and then only if you are handling them on Forms, Frames or wincontrols, ie anything with a Windproc. This is because the DefaultHandler in `TControl`, `TWinControl` and `TCustomForm` casts the Message parameter to `TMessage`. If you are using this on other platforms, you can use Word for the MsgId field, as that is all that is used by `TObject.Dispatch`

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
  msg := TProjectOpened.Create(projectName);
  FChannel.Queue.PostMessage(msg); //async
  //or
  FChannel.Queue.SendMessage(msg); //sync
end;
````

Note that the reason the `PostMessage` and `SendMessage` are on the channel.Queue property is purely to work around the generics limitations in delphi, where non-generic interfaces cannot have generic methods (records can, so Queue is actually a record).

`Queue.PostMessage` sends the message Asynchronously, ie the method returns before the message has been received.

`Queue.SendMessage` returns after all dispatchers have finished dispatching the message. Note `SendMessage` should not be used to send messages to dispatchers that operate on the user interface, use `PostMessage` instead.  

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

NOTE:  If you are handling messages on forms/frames/controls where you will be making UI updates, then you should use `TMessageDispatcherFactory.CreateUIDispatcher` - this ensures that messages are only dispatched on the main thread (calls TThread.Queue ) . For performance reasons CreateUIDispatcher should not be used for non ui code, and you should avoid using too many ui dispatchers (ie m)

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

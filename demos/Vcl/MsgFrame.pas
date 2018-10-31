unit MsgFrame;

interface

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants, System.Classes,
  Vcl.Graphics, Vcl.Controls, Vcl.Forms, Vcl.Dialogs, Vcl.StdCtrls,
  VSoft.Messaging,
  MsgDemoMessages, Vcl.ComCtrls;

type
  TMsgRecieverFrame = class(TFrame)
    LogMemo: TMemo;
    ProgressBar1: TProgressBar;
  private
    { Private declarations }
    FDispatcher : IMessageDispatcher;
  protected
    procedure Hello(var msg : THelloMessage); message DEMO_MSG_HELLO;
    procedure GoodBye(var msg : THelloMessage); message DEMO_MSG_GOODBYE;
    procedure Progress(var msg : TProgressMessage); message DEMO_MSG_PROGRESS;
  public
    { Public declarations }
    procedure Init(const channel : IMessageChannel);
    constructor Create(AOwner: TComponent); override;
    destructor Destroy;override;
  end;

implementation

{$R *.dfm}

{ TMsgRecieverFrame }

constructor TMsgRecieverFrame.Create(AOwner: TComponent);
begin
  inherited;
  FDispatcher := TMessageDispatcherFactory.CreateUIDispatcher; //note it's  a UI message dispatcher, it will use Synchronize
  FDispatcher.Target := Self; //Dispatch the messages to this object.

end;

destructor TMsgRecieverFrame.Destroy;
begin
  FDispatcher.Channel := nil;
  FDispatcher := nil;
  inherited;
end;

procedure TMsgRecieverFrame.GoodBye(var msg: THelloMessage);
begin
  LogMemo.Lines.Add('Goodbye ' + msg.Name);
end;

procedure TMsgRecieverFrame.Hello(var msg: THelloMessage);
begin
  LogMemo.Lines.Add('Hello ' + msg.Name);
end;

procedure TMsgRecieverFrame.Init(const channel: IMessageChannel);
begin
  //hook up the dispatcher to the channel
  FDispatcher.Channel := channel;
end;

procedure TMsgRecieverFrame.Progress(var msg: TProgressMessage);
begin
  ProgressBar1.Position := msg.Progress;

end;

end.

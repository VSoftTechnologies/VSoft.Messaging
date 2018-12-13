unit MsgDemoFMXFrame;

interface

uses
  System.SysUtils, System.Types, System.UITypes, System.Classes, System.Variants, 
  FMX.Types, FMX.Graphics, FMX.Controls, FMX.Forms, FMX.Dialogs, FMX.StdCtrls,
  FMX.Controls.Presentation, FMX.Memo, VSoft.Messaging, MsgDemoMessages, FMX.Layouts;

type
  TFrame1 = class(TFrame)
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
    procedure SetExcludeFilter(const filter : TArray<Cardinal>);
    constructor Create(AOwner: TComponent); override;
    destructor Destroy;override;
  end;

implementation

{$R *.fmx}

{ TFrame1 }

constructor TFrame1.Create(AOwner: TComponent);
begin
  inherited;
  FDispatcher := TMessageDispatcherFactory.CreateUIDispatcher; //note it's  a UI message dispatcher, it will use Synchronize
  FDispatcher.Target := Self; //Dispatch the messages to this object.

end;

procedure TFrame1.Init(const channel: IMessageChannel);
begin
  //hook up the dispatcher to the channel
  FDispatcher.Channel := channel;

end;


destructor TFrame1.Destroy;
begin
  FDispatcher.Channel := nil;
  FDispatcher := nil;
  inherited;
end;

procedure TFrame1.GoodBye(var msg: THelloMessage);
begin
  LogMemo.Lines.Add('Goodbye ' + msg.Name);
end;

procedure TFrame1.Hello(var msg: THelloMessage);
begin
  LogMemo.Lines.Add('Hello ' + msg.Name);
end;


procedure TFrame1.Progress(var msg: TProgressMessage);
begin
  ProgressBar1.Value := msg.Progress;
end;

procedure TFrame1.SetExcludeFilter(const filter: TArray<Cardinal>);
begin
  FDispatcher.ExcludeFilter := filter;
end;

end.

unit Main;

interface

uses
  System.SysUtils, System.Types, System.UITypes, System.Classes, System.Variants,
  FMX.Types, FMX.Controls, FMX.Forms, FMX.Graphics, FMX.Dialogs,
  VSoft.Messaging, FMX.Controls.Presentation, FMX.ScrollBox, FMX.Memo,
  MsgDemoFMXFrame, MsgDemoMessages, FMX.StdCtrls;

type
  TForm2 = class(TForm)
    rx1: TFrame1;
    rx2: TFrame1;
    helloButton: TButton;
    goodbyeButton: TButton;
    thdSendButton: TButton;
    Button1: TButton;
    chkAsync: TCheckBox;
    procedure FormCreate(Sender: TObject);
    procedure helloButtonClick(Sender: TObject);
    procedure goodbyeButtonClick(Sender: TObject);
    procedure thdSendButtonClick(Sender: TObject);
    procedure Button1Click(Sender: TObject);
  private
    { Private declarations }
    FChannel : IMessageChannel;
  public
    { Public declarations }
  end;

var
  Form2: TForm2;

implementation

{$R *.fmx}

procedure TForm2.Button1Click(Sender: TObject);
var
  msg : THelloMessage;
begin
  msg := THelloMessage.Create('world');
  FChannel.Queue.SendMessage(msg);
end;

procedure TForm2.FormCreate(Sender: TObject);
begin
  FChannel := TMessageChannelFactory.CreateChannel;
  rx1.Init(FChannel);
  rx2.Init(FChannel);
end;

procedure TForm2.goodbyeButtonClick(Sender: TObject);
var
  msg : TGoodByeMessage;
begin
  msg := TGoodByeMessage.Create('world');
  FChannel.Queue.PostMessage(msg);
end;

procedure TForm2.helloButtonClick(Sender: TObject);
var
  msg : THelloMessage;
begin
  msg := THelloMessage.Create('world');
  FChannel.Queue.PostMessage(msg);
end;

procedure TForm2.thdSendButtonClick(Sender: TObject);
var
  async : boolean;
  proc : TProc;
begin
  async := chkAsync.IsChecked;

  proc := procedure
    var
      msg : TProgressMessage;
      i: Integer;
    begin
      for i := 1 to 100 do
      begin
        msg := TProgressMessage.Create(i);
        if async then
          FChannel.Queue.PostMessage(msg)
        else
          FChannel.Queue.SendMessage(msg);
       TThread.Sleep(10);
      end;
    end;
  TThread.CreateAnonymousThread(proc).Start;
end;

end.

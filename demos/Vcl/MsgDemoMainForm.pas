unit MsgDemoMainForm;

interface

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants, System.Classes, Vcl.Graphics,
  Vcl.Controls, Vcl.Forms, Vcl.Dialogs,
  VSoft.Messaging, Vcl.StdCtrls, MsgFrame;

type
  TForm2 = class(TForm)
    rx1: TMsgRecieverFrame;
    rx2: TMsgRecieverFrame;
    helloButton: TButton;
    goodbyeButton: TButton;
    thrdSendButton: TButton;
    chkAsync: TCheckBox;
    chkExclude: TCheckBox;
    procedure FormCreate(Sender: TObject);
    procedure helloButtonClick(Sender: TObject);
    procedure goodbyeButtonClick(Sender: TObject);
    procedure thrdSendButtonClick(Sender: TObject);
    procedure chkExcludeClick(Sender: TObject);
  private
    FChannel : IMessageChannel;
  public

  end;

var
  Form2: TForm2;

implementation

{$R *.dfm}

uses
  MsgDemoMessages;

procedure TForm2.chkExcludeClick(Sender: TObject);
begin
  if chkExclude.Checked then
    rx1.SetExcludeFilter([DEMO_MSG_GOODBYE])
  else
    rx1.SetExcludeFilter([]);
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
  if chkAsync.Checked then
    FChannel.Queue.PostMessage(msg)
  else
    FChannel.Queue.SendMessage(msg)
end;

procedure TForm2.helloButtonClick(Sender: TObject);
var
  msg : THelloMessage;
begin
  msg := THelloMessage.Create('world');
  if chkAsync.Checked then
    FChannel.Queue.PostMessage(msg)
  else
    FChannel.Queue.SendMessage(msg)
end;

procedure TForm2.thrdSendButtonClick(Sender: TObject);
var
  async : boolean;
  proc : TProc;
begin
  async := chkAsync.Checked;

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
       // TThread.Sleep(40);
      end;
    end;
  TThread.CreateAnonymousThread(proc).Start;

end;

end.

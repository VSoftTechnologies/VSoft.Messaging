program MsgDemo;

uses
  Vcl.Forms,
  MsgDemoMainForm in 'MsgDemoMainForm.pas' {Form2},
  MsgFrame in 'MsgFrame.pas' {MsgRecieverFrame: TFrame},
  MsgDemoMessages in 'MsgDemoMessages.pas';

{$R *.res}

begin
  Application.Initialize;
  Application.MainFormOnTaskbar := True;
  Application.CreateForm(TForm2, Form2);
  Application.Run;
end.

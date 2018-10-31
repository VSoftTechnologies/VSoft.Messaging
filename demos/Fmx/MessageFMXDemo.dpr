program MessageFMXDemo;

uses
  System.StartUpCopy,
  FMX.Forms,
  Main in 'Main.pas' {Form2},
  MsgDemoMessages in '..\Vcl\MsgDemoMessages.pas',
  MsgDemoFMXFrame in 'MsgDemoFMXFrame.pas' {Frame1: TFrame};

{$R *.res}

begin
  Application.Initialize;
  Application.CreateForm(TForm2, Form2);
  Application.Run;
end.

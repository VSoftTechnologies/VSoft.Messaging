object Form2: TForm2
  Left = 0
  Top = 0
  Caption = 'Form2'
  ClientHeight = 412
  ClientWidth = 697
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Tahoma'
  Font.Style = []
  OldCreateOrder = False
  OnCreate = FormCreate
  PixelsPerInch = 96
  TextHeight = 13
  inline rx1: TMsgRecieverFrame
    Left = 16
    Top = 15
    Width = 289
    Height = 218
    TabOrder = 0
    ExplicitLeft = 16
    ExplicitTop = 15
    ExplicitWidth = 289
    inherited LogMemo: TMemo
      Width = 289
      ExplicitWidth = 289
    end
    inherited ProgressBar1: TProgressBar
      Width = 289
      ExplicitWidth = 289
    end
  end
  inline rx2: TMsgRecieverFrame
    Left = 328
    Top = 15
    Width = 289
    Height = 218
    TabOrder = 1
    ExplicitLeft = 328
    ExplicitTop = 15
    ExplicitWidth = 289
    inherited LogMemo: TMemo
      Width = 289
      ExplicitWidth = 289
    end
    inherited ProgressBar1: TProgressBar
      Width = 289
      ExplicitWidth = 289
    end
  end
  object helloButton: TButton
    Left = 16
    Top = 248
    Width = 105
    Height = 25
    Caption = 'Say Hello'
    TabOrder = 2
    OnClick = helloButtonClick
  end
  object goodbyeButton: TButton
    Left = 191
    Top = 248
    Width = 106
    Height = 25
    Caption = 'Say GoodBye'
    TabOrder = 3
    OnClick = goodbyeButtonClick
  end
  object thrdSendButton: TButton
    Left = 16
    Top = 288
    Width = 156
    Height = 25
    Caption = 'PostMessage from Thread'
    TabOrder = 4
    OnClick = thrdSendButtonClick
  end
  object sayHelloSyncButton: TButton
    Left = 328
    Top = 248
    Width = 137
    Height = 25
    Caption = 'Say Hello Sync'
    TabOrder = 5
    OnClick = sayHelloSyncButtonClick
  end
  object chkAsync: TCheckBox
    Left = 191
    Top = 292
    Width = 97
    Height = 17
    Caption = 'Async'
    Checked = True
    State = cbChecked
    TabOrder = 6
  end
end

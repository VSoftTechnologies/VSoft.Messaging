unit MsgDemoMessages;

interface

uses
  VSoft.Messaging;

type
  THelloMessage = record
    MsgID  : Cardinal;
    Filler : TMessageFiller;
    Name   : string;
  public
    constructor Create(const theName : string);
  end;

  TGoodByeMessage = record
    MsgID  : TMessageID;
    Filler : TMessageFiller;
    Name   : string;
  public
    constructor Create(const theName : string);
  end;

  TProgressMessage = record
    MsgID     : TMessageID;
    Filler    : TMessageFiller;
    Progress  : integer;
  public
    constructor Create(const AProgress : integer);
  end;

const
  WM_USER             = $0400; //declaring here so we don't have to reference winapi.messages

  DEMO_MSG_FIRST = WM_USER + $1000;// make sure this doesn't clash any other custom messages.
                                   // Note that if you are not sending these messages to classes that
                                   // have wndprocs (controls, forms, frames etc) then you don't need
                                   // to offset them with WM_USER + xxx.

  DEMO_MSG_HELLO    = DEMO_MSG_FIRST;
  DEMO_MSG_GOODBYE  = DEMO_MSG_FIRST + 1;
  DEMO_MSG_PROGRESS = DEMO_MSG_FIRST + 2;

implementation

{ TMessageHello }

constructor THelloMessage.Create(const theName: string);
begin
  MsgID := DEMO_MSG_HELLO;
  Name := theName;
end;

{ TMessageGoodBye }

constructor TGoodByeMessage.Create(const theName: string);
begin
  MsgID := DEMO_MSG_GOODBYE;
  Name  := theName;
end;

{ TProgressMessage }

constructor TProgressMessage.Create(const AProgress: integer);
begin
  MsgID := DEMO_MSG_PROGRESS;
  Progress := AProgress;
end;

end.

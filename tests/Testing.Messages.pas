unit Testing.Messages;

interface

uses
  System.SysUtils,
  VSoft.Messaging;

const
  //WM_USER + $1000 as the demos do, so these ids are safe to send to forms and controls
  TEST_MSG_BASE         = $0400 + $1000;
  TEST_MSG_VALUE        = TEST_MSG_BASE;
  TEST_MSG_OTHER        = TEST_MSG_BASE + 1;
  TEST_MSG_PAYLOAD      = TEST_MSG_BASE + 2;
  TEST_MSG_ACTION       = TEST_MSG_BASE + 3;
  TEST_MSG_RAW_ACTION   = TEST_MSG_BASE + 4;
  TEST_MSG_FREE_SELF    = TEST_MSG_BASE + 5;
  TEST_MSG_TWO_STRINGS  = TEST_MSG_BASE + 6;
  TEST_MSG_TEXT         = TEST_MSG_BASE + 7;
  TEST_MSG_RAISE        = TEST_MSG_BASE + 8;
  TEST_MSG_RAISE_OBJECT = TEST_MSG_BASE + 9;
  //no receiver has a handler for this one
  TEST_MSG_UNHANDLED    = TEST_MSG_BASE + 10;

type
  TValueMessage = record
    MsgID  : TMessageID;
    Filler : TMessageFiller;
    Value  : integer;
    Text   : string;
  public
    constructor Create(const theValue : integer; const theText : string = '');
  end;

  TOtherMessage = record
    MsgID  : TMessageID;
    Filler : TMessageFiller;
    Value  : integer;
  public
    constructor Create(const theValue : integer);
  end;

  //carries an interface so tests can see when the message is released
  TPayloadMessage = record
    MsgID   : TMessageID;
    Filler  : TMessageFiller;
    Payload : IInterface;
  public
    constructor Create(const thePayload : IInterface);
  end;

  //the handler runs Action, which lets a test decide what happens on the dispatcher thread
  TActionMessage = record
    MsgID  : TMessageID;
    Filler : TMessageFiller;
    Action : TProc;
  public
    constructor Create(const theAction : TProc);
  end;

  //like TActionMessage, but the handler does not touch the receiver after running Action,
  //so the action may free the receiver
  TRawActionMessage = record
    MsgID  : TMessageID;
    Filler : TMessageFiller;
    Action : TProc;
  public
    constructor Create(const theAction : TProc);
  end;

  //the handler frees the receiver it runs on
  TFreeSelfMessage = record
    MsgID  : TMessageID;
    Filler : TMessageFiller;
    Unused : integer;
  public
    constructor Create(const theUnused : integer);
  end;

  //two pointer sized payload fields, the second sits where TMessage.Result is on Win64
  TTwoStringMessage = record
    MsgID  : TMessageID;
    Filler : TMessageFiller;
    First  : string;
    Second : string;
  public
    constructor Create(const theFirst : string; const theSecond : string);
  end;

  //a single pointer sized payload field, the same shape as the README example message
  TTextMessage = record
    MsgID  : TMessageID;
    Filler : TMessageFiller;
    Text   : string;
  public
    constructor Create(const theText : string);
  end;

  //the handler raises an Exception with Text as the message
  TRaiseMessage = record
    MsgID  : TMessageID;
    Filler : TMessageFiller;
    Text   : string;
  public
    constructor Create(const theText : string);
  end;

  //the handler raises an object that does not descend from Exception
  TRaiseObjectMessage = record
    MsgID  : TMessageID;
    Filler : TMessageFiller;
    Unused : integer;
  public
    constructor Create(const theUnused : integer);
  end;

  TUnhandledMessage = record
    MsgID  : TMessageID;
    Filler : TMessageFiller;
    Value  : integer;
  public
    constructor Create(const theValue : integer);
  end;

//builds a filter array, open array literals cannot be assigned to TArray<Cardinal> before XE7
function MessageIds(const ids : array of Cardinal) : TArray<Cardinal>;

implementation

function MessageIds(const ids : array of Cardinal) : TArray<Cardinal>;
var
  i : integer;
begin
  SetLength(result, Length(ids));
  for i := 0 to High(ids) do
    result[i] := ids[i];
end;

{ TValueMessage }

constructor TValueMessage.Create(const theValue : integer; const theText : string);
begin
  MsgID := TEST_MSG_VALUE;
  FillChar(Filler, SizeOf(Filler), 0);
  Value := theValue;
  Text := theText;
end;

{ TOtherMessage }

constructor TOtherMessage.Create(const theValue : integer);
begin
  MsgID := TEST_MSG_OTHER;
  FillChar(Filler, SizeOf(Filler), 0);
  Value := theValue;
end;

{ TPayloadMessage }

constructor TPayloadMessage.Create(const thePayload : IInterface);
begin
  MsgID := TEST_MSG_PAYLOAD;
  FillChar(Filler, SizeOf(Filler), 0);
  Payload := thePayload;
end;

{ TActionMessage }

constructor TActionMessage.Create(const theAction : TProc);
begin
  MsgID := TEST_MSG_ACTION;
  FillChar(Filler, SizeOf(Filler), 0);
  Action := theAction;
end;

{ TRawActionMessage }

constructor TRawActionMessage.Create(const theAction : TProc);
begin
  MsgID := TEST_MSG_RAW_ACTION;
  FillChar(Filler, SizeOf(Filler), 0);
  Action := theAction;
end;

{ TFreeSelfMessage }

constructor TFreeSelfMessage.Create(const theUnused : integer);
begin
  MsgID := TEST_MSG_FREE_SELF;
  FillChar(Filler, SizeOf(Filler), 0);
  Unused := theUnused;
end;

{ TTwoStringMessage }

constructor TTwoStringMessage.Create(const theFirst : string; const theSecond : string);
begin
  MsgID := TEST_MSG_TWO_STRINGS;
  FillChar(Filler, SizeOf(Filler), 0);
  First := theFirst;
  Second := theSecond;
end;

{ TTextMessage }

constructor TTextMessage.Create(const theText : string);
begin
  MsgID := TEST_MSG_TEXT;
  FillChar(Filler, SizeOf(Filler), 0);
  Text := theText;
end;

{ TRaiseMessage }

constructor TRaiseMessage.Create(const theText : string);
begin
  MsgID := TEST_MSG_RAISE;
  FillChar(Filler, SizeOf(Filler), 0);
  Text := theText;
end;

{ TRaiseObjectMessage }

constructor TRaiseObjectMessage.Create(const theUnused : integer);
begin
  MsgID := TEST_MSG_RAISE_OBJECT;
  FillChar(Filler, SizeOf(Filler), 0);
  Unused := theUnused;
end;

{ TUnhandledMessage }

constructor TUnhandledMessage.Create(const theValue : integer);
begin
  MsgID := TEST_MSG_UNHANDLED;
  FillChar(Filler, SizeOf(Filler), 0);
  Value := theValue;
end;

end.

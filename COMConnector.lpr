program COMConnector;

{$mode objfpc}{$H+}

uses
  {$IFDEF UNIX}{$IFDEF UseCThreads}
  cthreads,
  {$ENDIF}{$ENDIF}
  Classes, SysUtils, CustApp, Registry, SynaSer;

type

  { TCOMConnector }

  TCOMConnector = class(TCustomApplication)
  protected
    procedure DoRun; override;
  public
    constructor Create(TheOwner: TComponent); override;
    destructor Destroy; override;
    procedure WriteHelp; virtual;
    procedure WriteSerError(ser: TBlockSerial; COMPort: string); virtual;

  end;

{ TCOMConnector }

procedure TCOMConnector.DoRun;
var
 command, COMPort, RateString, DeviceEcho, commandSend : string;
 Reg : TRegistry;
 COMPorts : TStringList;
 ser : TBlockSerial;
 i, baud, bits, stop : integer;
 HasRate, softflow, hardflow, NoClose : boolean;
 RateArray : TStringArray;
 parity : char;
begin

 // parse parameters
 if HasOption('h', 'help') then begin
  WriteHelp;
  Terminate;
  exit;
 end;

 if not HasOption('p', 'port') then
 begin
  writeln('You must specify the COM port using the option "-p" or "--port"' + LineEnding);
  Terminate;
  exit;
 end
 else
 begin
  COMPort:= GetOptionValue('p', 'port');
 end;

 if HasOption('r', 'rate') then
 begin
  RateString:= GetOptionValue('r', 'rate');
  // if string does not contain any comma
  if Pos(RateString, ',') = -1 then
  begin
   writeln('The rate must be a list separatey by ",". Check your "-r" or "--rate" parameter.' + LineEnding);
   Terminate;
   exit;
  end;
  // read the different values out of string
  RateArray:= RateString.Split(',');
  try
   baud:= StrToInt(RateArray[0]);
  except
   writeln('The baud rate must be a number. Check your "-r" or "--rate" parameter.' + LineEnding);
   Terminate;
   exit;
  end;
  if (baud < 50) or (baud > 4e6) then
  begin
   writeln('The baud rate must be a number between 50 and 4000000. Check your "-r" or "--rate" parameter.'
           + LineEnding);
   Terminate;
   exit;
  end;
  try
   bits:= StrToInt(RateArray[1]);
  except
   writeln('The bit rate must be a number. Check your "-r" or "--rate" parameter.' + LineEnding);
   Terminate;
   exit;
  end;
  if (RateArray[2][1] <> 'N') and (RateArray[2][1] <> 'O')
   and (RateArray[2][1] <> 'E') and (RateArray[2][1] <> 'M')
   and (RateArray[2][1] <> 'S') then
  begin
   writeln('The parity must either be "N", "O", "E", "M" or "S". Check your "-r" or "--rate" parameter.'
           + LineEnding);
   Terminate;
   exit;
  end;
  parity:= RateArray[2][1];
  if (RateArray[3] <> 'SB1') and (RateArray[3] <> 'SB1andHalf')
   and (RateArray[3] <> 'SB2') then
  begin
   writeln('Stop bits must either be "SB1", "SB1andHalf" or "SB2" a number. Check your "-r" or "--rate" parameter.'
           + LineEnding);
   Terminate;
   exit;
  end;
  if (RateArray[3] = 'SB1') then
   stop:= 0
  else if (RateArray[3] = 'SB1andHalf') then
   stop:= 1
  else if (RateArray[3] = 'SB3') then
   stop:= 2;
  try
   softflow:= StrToBool(RateArray[4]);
  except
   writeln('Softflow must be "True" or "False". Check your "-r" or "--rate" parameter.' + LineEnding);
   Terminate;
   exit;
  end;
  try
   hardflow:= StrToBool(RateArray[5]);
  except
   writeln('Hardflow must be "True" or "False". Check your "-r" or "--rate" parameter.' + LineEnding);
   Terminate;
   exit;
  end;
  HasRate:= True;
 end
 else
  HasRate:= False;

 if (not HasOption('o', 'open')) and (not HasOption('s', 'send')) then
 begin
  writeln('You must either specify a command to be sent using the option "-s" or "--send" or use the option "-o".'
          + LineEnding);
  Terminate;
  exit;
 end
 else if HasOption('s', 'send') then
 begin
  command:= GetOptionValue('s', 'send');
  // if no command
  if command = '' then
  begin
   writeln('There is no command to be sent. You must specify a command when using the option "-s" or "--send".'
           + LineEnding);
   Terminate;
   exit;
  end;
 end;

try
 // determine all possible COM ports
 COMPorts:= TStringList.Create;
 try
  Reg:= TRegistry.Create;
  Reg.RootKey:= HKEY_LOCAL_MACHINE;
  if Reg.OpenKeyReadOnly('HARDWARE\DEVICEMAP\SERIALCOMM') then
  begin
   COMPorts.Clear;
   Reg.GetValueNames(COMPorts);
   for i:= 0 to COMPorts.Count - 1 do
     COMPorts[i]:= Reg.ReadString(COMPorts[i]);
    COMPorts.Sorted:= true;
  end;
 finally
  Reg.Free;
 end;

 // check if COM port exists
 if COMPorts.IndexOf('COM' + COMPort) = -1 then
 begin
  writeln('The specified port COM' + COMPort + ' does not exist' + LineEnding);
  COMPort:= '';
  exit;
 end;

 // open a connection
 if (COMPort <> '') and (HasOption('o', 'open')) then
 begin
  NoClose:= False;
 try
  ser:= TBlockSerial.Create;
  ser.DeadlockTimeout:= 3000; //set timeout to 3 s
  if HasRate = False then
   ser.config(9600, 8, 'N', SB1, False, False)
  else
   ser.config(baud, bits, parity, stop, softflow, hardflow);
  ser.Connect('COM' + COMPort);

  if ser.LastError <> 0 then
  begin
   WriteSerError(ser, COMPort);
   // ser.LastError cannot be evaluated in the finally block
   // therefore we store its result as boolean
   if ser.LastError = 9997 then
    NoClose:= True;
   exit;
  end
  else
  begin
   // output connected port
   writeln('Connection to port COM' + COMPort
           + ' could sucessfully be established.' + LineEnding);
   // receive output from device
   DeviceEcho:= ser.Recvstring(1000);
   if DeviceEcho <> '' then
    writeln('The device sent back:' + LineEnding + DeviceEcho);
  end;

 finally
  if NoClose then
   exit; // we cannot close socket or free when the connection timed out
  ser.CloseSocket;
  ser.Free;
  exit;
 end;

 end; // end if send

 // send a command
 if (COMPort <> '') and (HasOption('s', 'send')) then
 begin
  NoClose:= False;
 try
  ser:= TBlockSerial.Create;
  ser.DeadlockTimeout:= 3000; //set timeout to 3 s
  if HasRate = False then
   ser.config(9600, 8, 'N', SB1, False, False)
  else
   ser.config(baud, bits, parity, stop, softflow, hardflow);
  ser.Connect('COM' + COMPort);

  if ser.LastError <> 0 then
  begin
   WriteSerError(ser, COMPort);
   // ser.LastError cannot be evaluated in the finally block
   // therefore we store its result as boolean
   if ser.LastError = 9997 then
    NoClose:= True;
   exit;
  end;
  // send command
  commandSend:= command + LineEnding;
  ser.SendString(commandSend);
  if ser.LastError <> 0 then
   WriteSerError(ser, COMPort)
  else
  begin
   // output success
   writeln('The command "' + command + '" was sucessfully sent to port COM'
           + COMPort + '.' + LineEnding);
   // receive output from device
   DeviceEcho:= ser.Recvstring(1000);
   if DeviceEcho <> '' then
    writeln('The device sent back:' + LineEnding + DeviceEcho);
  end;

 finally
  if NoClose then
   exit; // we cannot close socket or free when the connection timed out
  ser.CloseSocket;
  ser.Free;
  exit;
 end;

 end; // end if open

finally //free StringList
 if Assigned(COMPorts) then
    FreeAndNil(COMPorts);
 Terminate;
end;

end;

constructor TCOMConnector.Create(TheOwner: TComponent);
begin
  inherited Create(TheOwner);
  StopOnException:=True;
end;

destructor TCOMConnector.Destroy;
begin
  inherited Destroy;
end;

procedure TCOMConnector.WriteHelp;
begin
  { add your help code here }
  writeln('Usage: ', ExeName, ' -p [-r] [-o] -s');
  writeln('Parameters:');
  writeln('-p <port> or --port=<port>');
  writeln(' (<port> is the number of the COM port, mandatory)');
  writeln('-r <rate> or --rate=<rate>');
  writeln(' (optional, if not specified this <rate> will be used: "9600,8,N,SB1,False,False"');
  writeln(' (<rate> = <baud rate>,<bit rate>,<parity>,<stop>,<softflow>,<hardflow>:)');
  writeln('  (<baud rate> number between 50 and 4000000)');
  writeln('  (<bit rate> number)');
  writeln('  (<parity> communication parity character, either');
  writeln('   "N" (None), "O" (Odd), "E" (Even), "M" (Mark) or "S" (Space))');
  writeln('  (<stop> number of stop bits, either "SB1", "SB1andHalf" or "SB2")');
  writeln('  (<softflow> if XON/XOFF handshake, either "True" or "False")');
  writeln('  (<hardflow> if CTS/RTS handshake, either "True" or "False"');
  writeln('-o or --open');
  writeln(' (opens a connection without sending, intended to check connections,');
  writeln('  mandatory if option -s is not used)');
  writeln('-s <command> or --send=<command>');
  writeln(' (<command> to be sent via the <port>, mandatory if option -o is not used)');
  writeln('');
end;

procedure TCOMConnector.WriteSerError(ser: TBlockSerial; COMPort: string);
begin
  writeln('COM' + COMPort + ' error: ' + ser.LastErrorDesc + LineEnding);
end;

var
  Application: TCOMConnector;

{$R *.res}

begin
  Application:= TCOMConnector.Create(nil);
  Application.Title:='JT COM Connector';
  Application.Run;
  Application.Free;
end.


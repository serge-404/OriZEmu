/////////////////////////////////////////////////////////////////////////
//                                                                     //
//      Orion/Z (Orion-128 + Z80-CARD-II) emulator, version 1.9        //
//                                                                     //
//   Author: Sergey A.  <a-s-m@km.ru>                                  //
//                                                                     //
//   Copyright (C) 2006-2016 Sergey A.                                 //
//                                                                     //
//   This program is free software; you can redistribute it and/or     //
//                  modify it in any ways.                             //
//   This program is distributed "AS IS" in the hope that it will be   //
//   useful, but WITHOUT ANY WARRANTY; without even the implied        //
//   warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  //
//                                                                     //
/////////////////////////////////////////////////////////////////////////


{***********************************************

    Serial port (RS-232) emulation

 ***********************************************}

{
Отправка байта - запись в порт F764.

Прием байта - чтение порта F764.

Контроль состояния передачи - порт F765 на чтение:
D7 - TX_Ready (0 = идет отправка байта, порт не готов. 1 = можно записывать следующий байт для отправки)
D6 - RX_Status (0 = нет нового принятого байта. 1 = принят новый байт. После считывания байта из F764 этот бит сбрасывается в 0).

Управление режимами - порт F765 на запись:

D6 - "1" = разрешает прерывание по приему байта. На ШД ставится 0FDh.
D0, D1 порта F765 - Скорости UART:
"00" - 4800 (по умолчанию после сброса)
"01" - 9600
"10" - 19200
"11" - 38400

8-N-2, flow control - off.
}

unit mod232;

interface

Uses Windows, SysUtils, classes, CPDrv {ComDrv32 unit}, Forms;

{$I 'OrionZEm.inc'}

const
  UART_ADDR0 = $F764;  // data register
  UART_ADDR1 = $F765;  // status register
  RX_MAX = 2048;       // RX Buffer size
  arrbr: array[0..3] of TBaudRate = (br4800, br9600, br19200, br38400);

Type
  TUART = class(TObject)
    RXBuffer: array[0..RX_MAX] of byte;
    FCPDrv: TCommPortDriver;
    FPortName: string;
    FExists: boolean;
    FIntCount: integer;
    FIntMode: boolean;
    FRXCount: integer;
    FIntDataReaded: integer;
  private
    function GetPort0: byte;
    function GetPort1: byte;
    procedure SetPort0(const Value: byte);
    procedure SetPort1(const Value: byte);
    function GetPortName: string;
    procedure SetPortName(const Value: string);
    procedure ReceiveDataEvent( Sender: TObject; DataPtr: pointer; DataSize: DWORD );
    procedure SetIntCount(const Value: integer);
  public
    constructor Create;
    destructor Destroy; override;
    procedure Reset;
    procedure EnumComPorts(Ports: TStrings);
    property CPDrv:TCommPortDriver read FCPDrv;
    property IntCount:integer read FIntCount write SetIntCount;         // True then generate Z80 interrupt (0FDh IM2 vector)
    property Exists:boolean read FExists write FExists;
    property IntMode:boolean read FIntMode;
    property IntDataReaded:integer read FIntDataReaded;
    property PortName:string read GetPortName write SetPortName;        // ComPort Name
    property Port0:byte read GetPort0 write SetPort0;                   // F764 - data register
    property Port1:byte read GetPort1 write SetPort1;                   // F765 - control register
  end;

var
  FUART: TUART;
  ComPortName: string;
  ComPortExists: boolean;

implementation

{ TUART }

procedure TUART.EnumComPorts(Ports: TStrings);
var
  KeyHandle: HKEY;
  ErrCode, Index: Integer;
  ValueName, Data: string;
  ValueLen, DataLen, ValueType: DWORD;
  TmpPorts: TStringList;
begin
  ErrCode := RegOpenKeyEx(
    HKEY_LOCAL_MACHINE,
    'HARDWARE\DEVICEMAP\SERIALCOMM',
    0,
    KEY_READ,
    KeyHandle);
  if ErrCode <> ERROR_SUCCESS then
    raise Exception.Create('Error: registry read');
  TmpPorts := TStringList.Create;
  try
    Index := 0;
    repeat
      ValueLen := 256;
      DataLen := 256;
      SetLength(ValueName, ValueLen);
      SetLength(Data, DataLen);
      ErrCode := RegEnumValue(
        KeyHandle,
        Index,
        PChar(ValueName),
        Cardinal(ValueLen),
        nil,
        @ValueType,
        PByte(PChar(Data)),
        @DataLen);
      if ErrCode = ERROR_SUCCESS then
      begin
        SetLength(Data, DataLen);
        TmpPorts.Add(Data);
        Inc(Index);
      end
      else
        if ErrCode <> ERROR_NO_MORE_ITEMS then
          raise Exception.Create('Error: registry read');
    until (ErrCode <> ERROR_SUCCESS) ;
    TmpPorts.Sort;
    Ports.Assign(TmpPorts);
  finally
    RegCloseKey(KeyHandle);
    TmpPorts.Free;
  end;
end;

constructor TUART.Create;
begin
  FCPDrv:=nil;
  FCPDrv:=TCommPortDriver.Create(Application);
  FPortName:='';
  FRXCount:=0;
  FExists:=False;
  FIntCount:=0;
  FIntMode:=False;
  FIntDataReaded:=0;
  FCPDrv.OnReceiveData:=ReceiveDataEvent;
  Reset;
end;

destructor TUART.Destroy;
begin
  if Assigned(FCPDrv) then begin
    FCPDrv.Disconnect;
//    FCPDrv.Free;                                  // Destroy controlled by Application
  end;
  inherited;
end;

function TUART.GetPort0: byte;                      // Get DATA register
begin
  if (not FExists) or (FRXCount=0) then
    result:=$FF
  else
  begin
    if not FCPDrv.Connected then FCPDrv.Connect;      // because do not operating on COM-port until emulated code attempt
    Result:=RXBuffer[0];
    FRXCount:=FRXCount-1;
    CopyMemory( @RXBuffer[0], @RXBuffer[1], FRXCount );
    if (IntCount>0) then
      IntCount:=IntCount-1;
    if (FIntDataReaded>0)and(FIntDataReaded>IntCount) then
      FIntDataReaded:=FIntDataReaded-1;
  end;
end;

function TUART.GetPort1: byte;                      // Get STATUS register
begin
  result:=$FF;
  if FExists then
  begin
    if not FCPDrv.Connected then FCPDrv.Connect;      // because do not operating on COM-port until emulated code attempt
    if FRXCount=0 then
      result:=result and $BF;                         // D6=0 if no incoming data
    if FCPDrv.OutFreeSpace=0 then
      result:=result and $7F;                         // D7=0 if still sending output byte
  end;
end;

function TUART.GetPortName: string;
begin
  Result:=ExtractFileName(FPortName);
end;

procedure TUART.Reset;
begin
  with FCPDrv do
  begin
    BaudRate:=br4800;      // 0=4800, 1=9600, 2=19200, 3=38400
    CheckLineStatus:=True;            // True;
    DataBits:=db8BITS;
    HwFlow:=hfNONE;
    PacketMode:=pmDiscard;
    Parity:=ptNONE;
    Port:=pnCustom;
    PortName:=FPortName;
    StopBits:=sb2BITS;
    SwFlow:=sfNONE;
    if Connected and (not FlushBuffers(True, True)) then
      MessageBox(0, 'Can not flush RS-232 buffers', 'Error', MB_ICONERROR+MB_OK);
  end;
end;

procedure TUART.SetPort0(const Value: byte);        // set DATA register
begin
  if FExists then
  begin
    if not FCPDrv.Connected then FCPDrv.Connect;    // because do not operating on COM-port until emulated code attempt
    if (FCPDrv.OutFreeSpace>0) and
       (FCPDrv.OutFreeSpace<=FCPDrv.OutBufSize) then
      FCPDrv.SendByte(Value);                       // otherwise data sent to /dev/null :)
  end;
end;

procedure TUART.SetPort1(const Value: byte);        // Set STATUS register
begin
  if FExists then
  begin
    if FCPDrv.BaudRate<>arrbr[Value and 3] then
    begin
      if FCPDrv.Connected then FCPDrv.Disconnect;
      FCPDrv.BaudRate:=arrbr[Value and 3];
      FCPDrv.Connect;
    end;
    FIntMode:=(Value and $40)<>0;
  end;
end;

procedure TUART.SetPortName(const Value: string);
begin
  if trim(Value)<>FPortName then
  begin
    FPortName:=trim(Value);
    if FPortName<>'' then
    begin
      if FPortName<>'\\.\'+ExtractFileName(FPortName) then
        FPortName:='\\.\'+ExtractFileName(FPortName);
    end;
    if FCPDrv.Connected then FCPDrv.Disconnect;
    FCPDrv.PortName:=FPortName;
  end;
end;

procedure TUART.ReceiveDataEvent(Sender: TObject; DataPtr: pointer; DataSize: DWORD);
begin
  FIntDataReaded:=FIntCount;
  if DataSize>=RX_MAX then
    DataSize:=RX_MAX;
  if DataSize+FRXCount>=RX_MAX then
    FRXCount:=RX_MAX-DataSize;                           // temporary solution for buffer overflow resolving
  CopyMemory( @RXBuffer[FRXCount], DataPtr, DataSize );
  FRXCount:=FRXCount+DataSize;
  FIntCount:=FRXCount;
  FIntDataReaded:=FIntCount;
end;

procedure TUART.SetIntCount(const Value: integer);
begin
  if Value>=0 then
    FIntCount := Value;
end;

initialization
  FUART:=nil;
  FUART:=TUART.Create;

finalization
  if Assigned(FUART) then FUART.Free;

end.

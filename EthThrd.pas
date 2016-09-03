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


{*****************************************************************

  EtherNet Layer2 emulation with destination TAP virtual adapter

 *****************************************************************}

unit EthThrd;

interface

{$I 'OrionZEm.inc'}

Uses   Windows, Messages, SysUtils, Classes, Registry;

const
  ETH_P_IP           = $0800;    // IPv4 protocol
  ETH_P_IP6          = $86dd;    // IPv6 protocol
  ETH_P_ARP          = $0806;    // ARP protocol
  ETH_HLEN           = 14;
  ETH_MTU            = 1500;
  FRAME_SIZE         = 1518;
  MAXFRAMEF_SZ       = 1638;
  FRAME_SIZE_FIELD   = 1634;

  ETH_HEADER_LEN     = ETH_HLEN;
  ETHTYPE_ARP        = ETH_P_ARP;
  ETHTYPE_IP         = ETH_P_IP;
  ETHTYPE_IP6        = ETH_P_IP6;

  MAC_ADDR_TYPE      = $0001;
  ARP_REQUEST        = $0001;
  ARP_REPLY          = $0002;
  ARP_OPCODE_REQUEST = ARP_REQUEST;
  ARP_OPCODE_REPLY   = ARP_REPLY;

  IP_HEADER_LEN      = 20;
  IP_PROTO_ICMP      = 1;
  IP_PROTO_TCP       = 6;
  IP_PROTO_UDP       = 17;

  UDP_HEADER_LEN     = 8;

  ICMP_HEADER_LEN    = 8;
  ICMP_TYPE_ECHOREPLY= 0;
  ICMP_TYPE_ECHOREQUEST = 8;

{
  TAP_IOCTL_GET_VERSION = TAP_CONTROL_CODE CTL_CODE (FILE_DEVICE_UNKNOWN, 2, METHOD_BUFFERED, FILE_ANY_ACCESS)

  TAP_IOCTL_SET_MEDIA_STATUS = CTL_CODE (FILE_DEVICE_UNKNOWN, 6, METHOD_BUFFERED, FILE_ANY_ACCESS)

#define CTL_CODE(DeviceType, Function, Method, Access) (
  ((DeviceType) << 16) | ((Access) << 14) | ((Function) << 2) | (Method)
)
}
  FILE_DEVICE_UNKNOWN = $00000022;
  METHOD_BUFFERED     = 0;
  FILE_ANY_ACCESS     = 0;

  TAP_IOCTL_SET_MEDIA_STATUS = (FILE_DEVICE_UNKNOWN shl 16) or (FILE_ANY_ACCESS shl 14) or (6 shl 2) or (METHOD_BUFFERED);
  TAP_IOCTL_GET_VERSION = (FILE_DEVICE_UNKNOWN shl 16) or (FILE_ANY_ACCESS shl 14) or (2 shl 2) or (METHOD_BUFFERED);

type
  TMacAddr = array [0..5] of char;

const
  MacBroadcast: TMacAddr = (#$FF, #$FF, #$FF, #$FF, #$FF, #$FF);

type
  TIPAddr = DWORD;

//----------------
// Ethernet header
//----------------

  TEthHeader = packed record
    dest:TMacAddr;              // destination eth addr
    src:TMacAddr;               // source ether addr
    proto:word;			// packet type ID field
  end;
  PEthHeader = ^TEthHeader;

//----------------
// ARP packet
//----------------

  TArpHeader = packed record
//    dest:TMacAddr;         //   Reverse these two                              +0
//    src:TMacAddr;          //   to answer ARP requests                         +6
//    proto:word;            // 0x0806                                           +0C
    hwtype:word;             // 0x0001                                           +0E
    protocol:word;           // 0x0800                                           +10
    hwlen:byte;              // 0x06                                             +12
    protolen:byte;           // 0x04                                             +13
    opcode:word;             // 0x0001 for ARP request, 0x0002 for ARP reply     +14
    shwaddr:TMacAddr;        //                                                  +16
    sipaddr:TIPAddr;         //                                                  +1C
    dhwaddr:TMacAddr;        //                                                  +20
    dipaddr:TIPAddr;         //                                                  +26
  end;
  PArpHeader = ^TArpHeader;

  TEthArpHeader = packed record                                                 // sizeof(TEthArpHeader)=$2A=42
     eth: TEthHeader;
     arp: TArpHeader;
  end;
  PEthArpHeader= ^TEthArpHeader;

  TNetIpHeader = packed record
    vhl: BYTE;
    tos: BYTE;
    len: WORD;
    ipid: WORD;
    ipoffset: WORD;
    ttl: BYTE;
    proto: BYTE;
    ipchksum: WORD;
    srcipaddr: DWORD;
    destipaddr: DWORD;
  end;
  PNetIpHeader = ^TNetIpHeader;

  TNetIcmpHeader = packed record
    itype:     BYTE;
    icode:     BYTE;
    icmpchksum:WORD;
    id:        WORD;
    seqno:     WORD;
  end;
  PNetIcmpHeader = ^TNetIcmpHeader;

  TNetUdpHeader = packed record
    srcport:   WORD;
    destport:  WORD;
    udplen:    WORD;
    udpchksum: WORD;
  end;
  PNetUdpHeader = ^TNetUdpHeader;

  TNetTcpHeader = packed record
    srcport:   WORD;
    destport:  WORD;
    seqno:     DWORD;
    ackno:     DWORD;
    tcpoffset: BYTE;
    flags:     BYTE;
    wnd:       WORD;
    tcpchksum: WORD;
    urgp:      WORD;
//     optdata[4]: BYTE ;
  end;
  PNetTcpHeader = ^TNetTcpHeader;

// Ethernet/IP header
  TNetEthIpHeader = packed record
    eth: TEthHeader;
    ip: TNetIpHeader;
  end;
  PNetEthIpHeader = ^TNetEthIpHeader;

// The IP header
  ip_hdr = TNetIpHeader;

// The IP/TCP headers
  tcpip_hdr = packed record
    ip: TNetIpHeader;
    tcp: TNetTcpHeader;
  end;

// The IP/ICMP headers
  icmpip_hdr = packed record
    ip: TNetIpHeader;
    icmp: TNetIcmpHeader;
  end;
  PICMPip_hdr = ^icmpip_hdr;

// The UDP and IP headers
  udpip_hdr = packed record
    ip: TNetIpHeader;
    udp: TNetUdpHeader;
  end;

  TFrame = array[0..MAXFRAMEF_SZ] of Byte;
  PFrame = ^TFrame;

  TEthThread = class(TThread)
  private
    FVersion: packed record
      major: DWORD;
      minor: DWORD;
      debug: DWORD;
    end;
    FMACAddr: TMacAddr;
    FTAPguid: string;
    FTAPHandle: THANDLE;
    FOverRead: TOverlapped;
    FOverWrite: TOverlapped;
    FReadEvent: THandle;
    FWriteEvent: THandle;
    FBufferSize: integer;
    FBufferUsed: integer;
    FFramesList: TThreadList;
    FFreeList: TList;
    FFreeMax: integer;
    FFreeBuf: array of TFrame;
    FInpCh: char;
    FOutCh: char;
    FDisCh: char;
    FInpFrames: integer;
    FOutFrames: integer;
    FDisFrames: integer;
    FPrevInpFrames: integer;
    FPrevOutFrames: integer;
    FPrevDisFrames: integer;
    FPromiscuous: boolean;
    FAcceptRunt: boolean;
    FBroadcast: boolean;
    FMulticast: boolean;
    procedure SetMacAddr(Value: PChar);
    procedure SetTAPguid(const Value: string);
    procedure OpenTAP();
    procedure CloseTAP();
    function GetFreeBuf:PFrame;
    procedure DisposeBuf(ptr:PFrame);
    function GetActive: boolean;
    procedure SetActive(const Value: boolean);
    procedure SetBufSize(const Value: integer);
    function GetMacAddr: PChar;
    function MacEqual(mac1, mac2: PChar):boolean;
    function IsBroadcast(mac: PChar):boolean;
    function IsMulticast(mac: PChar):boolean;
    procedure SetPromiscuous(const Value: boolean);
    procedure SetAcceptRunt(const Value: boolean);
    procedure SetBroadcast(const Value: boolean);
    procedure SetMulticast(const Value: boolean);
  protected
    procedure Execute; override;
  public
    constructor Create();
    destructor Destroy; override;
    procedure Start;
    procedure Stop;
    procedure Reset;
    function FrameLen(const Buffer: TFrame):integer;
    function GetPacket(var Buffer:TFrame):WORD;
    function PutPacket(const Buffer; WrSize:integer):boolean;
    function GetStat: string;
    function IsPhysical(mac: PChar):boolean;
    property VersionMajor: DWORD read FVersion.major;                            // TAP driver version
    property VersionMinor: DWORD read FVersion.minor;
    property Active: boolean read GetActive write SetActive;
    property BufSize: integer read FBufferSize write SetBufSize;
    property MACAddr: PChar read GetMacAddr write SetMacAddr;
    property TAPguid: string read FTAPguid write SetTAPguid;
    property Promiscuous: boolean read FPromiscuous write SetPromiscuous;
    property AcceptMulticast: boolean read FMulticast write SetMulticast;
    property AcceptBroadcast: boolean read FBroadcast write SetBroadcast;
    property AcceptRunt: boolean read FAcceptRunt write SetAcceptRunt;
    property InpFramesList: TThreadList read FFramesList;
    property InpFrames: integer read FInpFrames;
    property OutFrames: integer read FOutFrames;
    property DisFrames: integer read FDisFrames;	// discarded due free buffer not ready
  end;

  function htons(val:WORD):WORD;
  function htonl(val:DWORD): DWORD;
  function netChecksum(data:PBYTE; len:WORD): WORD;
  procedure GetConnectionInfo(Names, Guids: TStrings);
  procedure CreateEthThread;
  procedure DestroyEthThread;

var
  EthThread: TEthThread;

implementation

function StrWinError(Err:DWORD): string;
var lpMsgBuf: PChar;
begin
  FormatMessage(FORMAT_MESSAGE_ALLOCATE_BUFFER or
                FORMAT_MESSAGE_FROM_SYSTEM,
                nil, Err, 0,
                @lpMsgBuf, 0, nil);
  Result:=string(lpMsgBuf);
  LocalFree( HLOCAL(lpMsgBuf) );
end;

function LastWinError: string;
begin
   StrWinError(GetLastError);
end;

function htons(val:WORD):WORD;
begin
   result:=(val shl 8) or (val shr 8);
end;

function htonl(val:DWORD): DWORD;
begin
    result:=(htons(val shr 16) or DWORD(htons(val and $0000FFFF) shl 16));
end;

function netChecksum(data:PBYTE; len:WORD): WORD;
var sum: DWORD;
begin
    sum := 0;
    while (len>=2) do begin
        sum:=sum+(PWORD(data))^;
        inc(data); inc(data);
        len:=len-2;
    end;
    if (len>0) then
        sum:=sum+(PBYTE(data))^;

    while ( (sum shr 16) <> 0 ) do
        sum := WORD(sum) + WORD(sum shr 16);

    result:= WORD(sum xor $FFFF);
end;

procedure GetConnectionInfo(Names, Guids: TStrings);
const
  StartKey = '\SYSTEM\CurrentControlSet\Control\Network';
  SubKeyNetName = 'Class';
  SubKeyNetValue = 'Net';
  NameValueName = 'Name';
  ShowIconValueName = 'ShowIcon';
var
  Reg: TRegistry;
  KeyNames : TStringList;
  i : Integer;
  KeyFound : Boolean;
  Key : string;
begin
  Names.Clear;
  Guids.Clear;

  // Access the registry in read only mode
  Reg := TRegistry.Create(HKEY_LOCAL_MACHINE);
  KeyNames := TStringList.Create;
  try
    Reg.RootKey := HKEY_LOCAL_MACHINE;
    Reg.OpenKeyReadOnly(StartKey);

    // Need to find the child that has a value named
    // 'Class' which value is 'Net'
    Reg.GetKeyNames(KeyNames);
    i := 0;
    KeyFound := False;
    Key := StartKey;
    while (i < KeyNames.Count) and not KeyFound do
    begin
      Reg.OpenKeyReadOnly(Key+'\'+KeyNames[i]);
      if Reg.ReadString(SubKeyNetName) = SubKeyNetValue then
        KeyFound := True;
      Inc(i);
    end;

    // Found a key, open it and read its subkeys
    // which in turn contain the names we are looking for
    // in their Connection subkey
    if KeyFound then
    begin
      Key := '\'+Reg.CurrentPath;
      Reg.OpenKeyReadOnly(KeyNames[i-1]);
      Reg.GetKeyNames(KeyNames);
      for i := 0 to KeyNames.Count - 1 do
      begin
        Reg.OpenKeyReadOnly(Key+'\'+KeyNames[i]+'\Connection');
        if Reg.ValueExists(NameValueName) then
        begin
            Names.Add(Reg.ReadString(NameValueName));
            Guids.Add(KeyNames[i]);
        end;
      end;
    end;
  finally
    Reg.Free;
    KeyNames.Free;
  end;
end;

procedure CreateEthThread;
begin
  if not Assigned(EthThread) then
  begin
    EthThread:=TEthThread.Create;                 { create suspended }
    EthThread.Priority := tpLower;                { set the priority to normal }
  end;
end;

procedure DestroyEthThread;
begin
  if Assigned(EthThread) then with EthThread do begin
    if Suspended then Resume;
    Terminate;
    WaitFor;
    Active:=False;                      // CloseTAP
    Free;
  end;
  EthThread:=nil;
end;

{ TEthThread }

procedure TEthThread.Reset;
begin
  Stop;
  FInpCh:='I';
  FOutCh:='O';
  FDisCh:='D';
  FInpFrames:=0;
  FOutFrames:=0;
  FDisFrames:=0;
  FPrevInpFrames:=0;
  FPrevOutFrames:=0;
  FPrevDisFrames:=0;
  with FFramesList.LockList do
  try
    while (Count>0) do begin
      DisposeBuf(Items[0]);
      Delete(0);
    end;
  finally
    FFramesList.UnlockList;
  end;
end;

constructor TEthThread.Create();
var RandInt: Integer;
begin
  FBroadcast:=True;
  FMulticast:=False;
  FAcceptRunt:=False;             // False => ignore (skip) runt packets (packet length < 64 bytes)
  FPromiscuous:=False;
  Randomize();
  FFramesList:=TThreadList.Create;
  FFreeList:=TList.Create;
  Reset;
  FTAPguid:='';
  FTAPHandle:=INVALID_HANDLE_VALUE;
  FBufferUsed:=0;
  RandInt:=Random(MaxInt);
  FMACAddr[0]:=chr(0);
  FMACAddr[1]:=chr(255);
  FMACAddr[2]:=chr(lo(RandInt shr 24));
  FMACAddr[3]:=chr(lo(RandInt shr 16));
  FMACAddr[4]:=chr(lo(RandInt shr 8));
  FMACAddr[5]:=chr(lo(RandInt));
  FReadEvent:=CreateEvent(nil, False {True}, False, nil);
  FOverRead.Offset:=0;
  FOverRead.OffsetHigh:=0;
  FOverRead.hEvent := FReadEvent;
  if FOverRead.hEvent = 0 then
    raise Exception.Create('Error creating TAP read event');
  FWriteEvent:=CreateEvent(nil, False {True}, False, nil);
  FOverWrite.Offset:=0;
  FOverWrite.OffsetHigh:=0;
  FOverWrite.hEvent := FWriteEvent;
  if FOverRead.hEvent = 0 then
    raise Exception.Create('Error creating TAP write event');
  inherited Create(True);         // Create Suspended
end;

destructor TEthThread.Destroy;
begin
  CloseHandle(FWriteEvent);
  CloseHandle(FReadEvent);
  FFramesList.Free;
  FFreeList.Free;
  inherited;
end;

procedure TEthThread.Start;
begin
  if not Active then
    Active:=True;                     // OpenTap
  Resume;                             { now run the thread }
end;

procedure TEthThread.Stop;
begin
  Suspend;                            { pause the thread }
end;

function TEthThread.MacEqual(mac1, mac2: PChar):boolean;
begin
  Result:=False;
  if mac1[0]<>mac2[0] then exit;
  if mac1[1]<>mac2[1] then exit;
  if mac1[2]<>mac2[2] then exit;
  if mac1[3]<>mac2[3] then exit;
  if mac1[4]<>mac2[4] then exit;
  Result:=mac1[5]=mac2[5];
end;

function TEthThread.FrameLen(const Buffer: TFrame):integer;
begin
  Result:=PDWORD(@Buffer[FRAME_SIZE_FIELD])^;
end;

function TEthThread.GetPacket(var Buffer:TFrame):WORD;
 var PFrameBuf: PFrame;
 function min(a,b:integer):integer; begin if a<b then result:=a else result:=b; end;
begin
  Result:=0;
  PFrameBuf:=nil;
  if (not Active) then exit;
  with FFramesList.LockList do
  try
    if (Count>0) then begin
      PFrameBuf:=Items[0];
      Delete(0);
      Result:=WORD(FrameLen(PFrameBuf^));
    end;
  finally
    FFramesList.UnlockList;
  end;
  if Result<>0 then
    CopyMemory(@Buffer[0], PFrameBuf, min(Result, sizeof(TFrame)));
  if Assigned(PFrameBuf) then
    DisposeBuf(PFrameBuf);
end;

function TEthThread.PutPacket(const Buffer; WrSize:integer):boolean;
var dwWrited: DWORD;
    error: DWORD;
    res: LongBool;
begin
  Result:=False;
  if not Active then exit;
  res:=GetOverlappedResult(FTAPHandle, FOverWrite, dwWrited, FALSE);
  if (not res) and (GetLastError=ERROR_IO_INCOMPLETE) then
    WaitForSingleObject(FWriteEvent, INFINITE);

  res:=WriteFile(FTAPHandle, Buffer, WrSize, dwWrited, @FOverWrite);             
  if (not res) then begin
    error:=GetLastError;
    if (error=ERROR_IO_PENDING) then
    begin
      WaitForSingleObject(FWriteEvent, INFINITE);
      if not GetOverlappedResult(FTAPHandle, FOverWrite, dwWrited, FALSE) then
        raise Exception.CreateFmt('Error reading TAP-adapter, phase 0: %s', [LastWinError()]);
    end
    else
      raise Exception.CreateFmt('Error writing TAP-adapter: write failed: %s', [StrWinError(error)]);
  end;
  FOutFrames:=FOutFrames+1;
  Result:=True;
end;

procedure TEthThread.Execute;
var
 dwError: DWORD;
 PFrameBuf: PFrame;
 TmpFrameBuf: TFrame;
 dwRead: DWORD;
begin
 PFrameBuf:=nil;
 FreeOnTerminate := False;
 while not Terminated do
  if FTAPHandle=INVALID_HANDLE_VALUE then sleep(1) else
  begin
    dwRead:=0;
    if not Assigned(PFrameBuf) then begin
      PFrameBuf:=GetFreeBuf();
      if not Assigned(PFrameBuf) then PFrameBuf:=@TmpFrameBuf;
    end;
    if not ReadFile(FTAPHandle, PFrameBuf^, sizeof(TFrame), dwRead, @FOverRead) then
    begin
      dwError := GetLastError;
      if dwError = ERROR_IO_PENDING then begin
        WaitForSingleObject(FReadEvent, INFINITE);
        if not GetOverlappedResult(FTAPHandle, FOverRead, dwRead, False) then
          raise Exception.CreateFmt('Error reading TAP-adapter, phase 0: %s', [LastWinError()]);
      end
      else
        raise Exception.CreateFmt('Error waiting TAP adapter event: %s', [StrWinError(dwError)]);
    end;
    if (dwRead>=42) or ((dwRead>0)and FAcceptRunt) then                          // PRE+42+CRC=64
    begin
      with FFramesList.LockList do
      try
        if (PFrameBuf=@TmpFrameBuf) then
          FDisFrames:=FDisFrames+1
        else
          if MacEqual(pointer(PFrameBuf), @FMACAddr[0])                          // we are destination
             or (IsBroadcast(pointer(PFrameBuf)) and FBroadcast)                 // or broadcast accepted
             or (IsMulticast(pointer(PFrameBuf)) and FMulticast)                 // or multicast accepted
             or (IsPhysical(pointer(PFrameBuf)) and FPromiscuous) then           // or Promiscuous node on
          begin
            PDWORD(@(PFrameBuf^[FRAME_SIZE_FIELD]))^:=dwRead;
            Add(PFrameBuf);
            FInpFrames:=FInpFrames+1;
          end;
        PFrameBuf:=nil;
      finally
        FFramesList.UnlockList;
      end;
    end
  end;
end;

procedure TEthThread.OpenTAP;
var len, val: DWORD;
    hndl: THANDLE;
begin
  if (FTAPHandle<>INVALID_HANDLE_VALUE) then
    CloseTap();
  hndl:=CreateFile(PChar('\\.\Global\'+FTAPguid+'.tap'), GENERIC_READ or GENERIC_WRITE, {FILE_SHARE_READ or FILE_SHARE_WRITE}0, nil,
                   OPEN_EXISTING, FILE_ATTRIBUTE_SYSTEM or FILE_FLAG_OVERLAPPED, 0);
  if (hndl=INVALID_HANDLE_VALUE) then
    raise Exception.CreateFmt('Error opening TAP adapter: %s', [LastWinError()]);
  if not (DeviceIoControl(hndl, TAP_IOCTL_GET_VERSION,
                          @FVersion, sizeof (FVersion),
                          @FVersion, sizeof (FVersion), len, nil)) then
  begin
    CloseHandle(hndl);
    raise Exception.CreateFmt('Error TAP adapter - DeviceIoControl (1): %s', [LastWinError()]);
  end;
  val:=1;
  if (not DeviceIoControl(hndl, TAP_IOCTL_SET_MEDIA_STATUS,
                          @val, sizeof(val), @val, sizeof(val), len, nil)) then
  begin
    CloseHandle(hndl);
    raise Exception.CreateFmt('Error TAP adapter - DeviceIoControl (2): %s', [LastWinError()]);
  end;
  FTAPHandle:=hndl;
end;

procedure TEthThread.CloseTAP;
var len, val: DWORD;
begin
  if (FTAPHandle<>INVALID_HANDLE_VALUE) then
  begin
    val:=0;
    if (not DeviceIoControl(FTAPHandle, TAP_IOCTL_SET_MEDIA_STATUS,
                            @val, sizeof(val), @val, sizeof(val), len, nil)) then
      raise Exception.CreateFmt('Error TAP adapter - DeviceIoControl: %s', [LastWinError()]);
    CloseHandle(FTAPHandle);
  end;
  FTAPHandle:=INVALID_HANDLE_VALUE;
end;


procedure TEthThread.SetMacAddr(Value: PChar);
begin
    FMACAddr[0]:=Value[0];
    FMACAddr[1]:=Value[1];
    FMACAddr[2]:=Value[2];
    FMACAddr[3]:=Value[3];
    FMACAddr[4]:=Value[4];
    FMACAddr[5]:=Value[5];
end;

procedure TEthThread.SetPromiscuous(const Value: boolean);
begin
  FPromiscuous := Value;
end;

procedure TEthThread.SetTAPguid(const Value: string);
begin
  if (trim(FTAPguid)=trim(Value)) then exit;
  if (FTAPguid<>'') then
  begin
    CloseTAP;
    FTAPguid:='';
  end;
  FTAPguid := trim(Value);
end;

function TEthThread.GetActive: boolean;
begin
  Result:=FTAPHandle<>INVALID_HANDLE_VALUE;
end;

procedure TEthThread.SetActive(const Value: boolean);
begin
  If (Value) then
  begin
    if (not Active) then OpenTAP;
  end
  else CloseTAP;
end;

procedure TEthThread.SetBufSize(const Value: integer);
var i: integer;
begin
  FBufferSize:=Value;
  FFreeMax:=FBufferSize div MAXFRAMEF_SZ;
  SetLength(FFreeBuf, FFreeMax);
  FFreeList.Clear;
  for i:=0 to FFreeMax-1 do FFreeList.Add(@FFreeBuf[i][0]);
end;

procedure TEthThread.DisposeBuf(ptr:PFrame);
begin
  FFreeList.Add(ptr);
end;

function TEthThread.GetFreeBuf: PFrame;
begin
  if (FFreeList.Count=0) then
    Result:=nil
  else begin
    Result:=FFreeList.Items[0];
    FFreeList.Delete(0);
  end;
end;

function TEthThread.GetMacAddr: PChar;
begin
  Result:=@FMACAddr[0];
end;

function TEthThread.GetStat: string;
 function hsize(Actual: integer; var Prev:integer; var ch:char):string;
 begin
   if Actual<>Prev then
   begin
     Prev:=Actual;
     ch:=chr(ord(ch) xor $20);
   end;
   if Actual>=1024*1024 then
     result:=ch+format(':%dÌ', [Actual div (1024*1024)])
   else if Actual>=1024 then
     result:=ch+format(':%dK', [Actual div 1024])
   else
     result:=ch+format(':%d',  [Actual]);
 end;
begin
  Result:=format('%s, %s, %s', [hsize(FInpFrames, FPrevInpFrames, FInpCh),
                                hsize(FOutFrames, FPrevOutFrames, FOutCh),
                                hsize(FDisFrames, FPrevDisFrames, FDisCh)]);
end;

procedure TEthThread.SetAcceptRunt(const Value: boolean);
begin
  FAcceptRunt := Value;
end;

procedure TEthThread.SetBroadcast(const Value: boolean);
begin
  FBroadcast := Value;
end;

procedure TEthThread.SetMulticast(const Value: boolean);
begin
  FMulticast := Value;
end;

function TEthThread.IsBroadcast(mac: PChar): boolean;
begin
  Result:=MacEqual(mac, @MacBroadcast[0]);
end;

function TEthThread.IsMulticast(mac: PChar): boolean;
begin
  Result:=(ord(mac^) and 1 <> 0) and (mac^ <> #$FF);
end;

function TEthThread.IsPhysical(mac: PChar): boolean;
begin
  Result:=(not IsMulticast(mac)) and (not IsBroadcast(mac));
end;

initialization
  EthThread:=nil;

end.


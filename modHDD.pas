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

    IDE (ATA) emulation

 ***********************************************}

unit modHDD;

interface

Uses Windows, SysUtils, classes, mod8255, HDDUtils;


{$I 'OrionZEm.inc'}

//
// C,B-data (inout),  A-ctl (out)
//   (because if ORDOS exists, its treat ROM-DISK on the same port
//    and must not send dummy "random" signals into IDE)
//

const
  cfg_8255     = 3;     // $F503;    //8255 configuration register
  ide_8255_lsb = 2;     // $F502;    //pC - lower 8 bits
  ide_8255_msb = 1;     // $F501;    //pB - upper 8 bits
  ide_8255_ctl = 0;     // $F500;    //pA - control lines
  rd_ide_8255  = $8B;   //ide_8255_ctl out, ide_8255_lsb/msb input
  wr_ide_8255  = $80;   //all three ports output

//ide control lines for use with ide_8255_ctl.  Change these 8
//constants to reflect where each signal of the 8255 each of the
//ide control signals is connected.  All the control signals must
//be on the same port, but these 8 lines let you connect them to
//whichever pins on that port.

  ide_a0_line  = $01;       //direct from 8255 to ide interface
  ide_a1_line  = $02;       //direct from 8255 to ide interface
  ide_a2_line  = $04;       //direct from 8255 to ide interface
  ide_cs0_line = $08;       //inverter between 8255 and ide interface
  ide_cs1_line = $10;       //inverter between 8255 and ide interface
  ide_wr_line  = $20;       //inverter between 8255 and ide interface
  ide_rd_line  = $40;       //inverter between 8255 and ide interface
  ide_rst_line = $80;       //inverter between 8255 and ide interface

//------------------------------------------------------------------
// More symbolic constants... these should not be changed, unless of
// course the IDE drive interface changes, perhaps when drives get
// to 128G and the PC industry will do yet another kludge.

// Orion-PRO HDD-RTC card:
// 50H ; dannye CMOS
// 51H ; adres CMOS
  pro_astatus = $56;    // alxt.registr sostoqniq     -  R
  pro_control = $56;    // registr uprawleniq         - W
  pro_data_h  = $57;    // st.bajt registra dannyh    - WR
  pro_data_l  = $58;    // ml.bajt registra dannyh    - WR                   // adr=0
  pro_err     = $59;    // registr oshibok            -  R                   // adr=1
  pro_sec_cnt = $5A;    // s4et4ik seektorow          - W                    // adr=2
  pro_sector  = $5B;    // registr sektora            - W                    // adr=3
  pro_cyl_lsb = $5C;    // ml.bajt nom.cilindra       - W                    // adr=4
  pro_cyl_msb = $5D;    // st.bajt nom.cilindra       - W                    // adr=5
  pro_head    = $5E;    // registr golowki/ustrojstwa - W                    // adr=6
  pro_command = $5F;    // registr komand             - W                    // adr=7
  pro_status  = $5F;    // registr sostoqniq          -  R                   // adr=7

//some symbolic constants for the ide registers, which makes the
//code more readable than always specifying the address pins
//
  ide_data_h   =   pro_data_h;
  ide_data_l   =   pro_data_l;
  ide_data     =   ide_cs0_line;                                              // 8        // adr=0
  ide_err      =   ide_cs0_line + ide_a0_line;                                            // adr=1
  ide_sec_cnt  =   ide_cs0_line + ide_a1_line;                                            // adr=2
  ide_sector   =   ide_cs0_line + ide_a1_line + ide_a0_line;                              // adr=3
  ide_cyl_lsb  =   ide_cs0_line + ide_a2_line;                                            // adr=4
  ide_cyl_msb  =   ide_cs0_line + ide_a2_line + ide_a0_line;                              // adr=5
  ide_head     =   ide_cs0_line + ide_a2_line + ide_a1_line;                  // 14       // adr=6
  ide_command  =   ide_cs0_line + ide_a2_line + ide_a1_line + ide_a0_line;                // adr=7
  ide_status   =   ide_cs0_line + ide_a2_line + ide_a1_line + ide_a0_line;    // 15       // adr=7
  ide_control  =   ide_cs1_line + ide_a2_line + ide_a1_line;                              // adr=6
  ide_astatus  =   ide_cs1_line + ide_a2_line + ide_a1_line + ide_a0_line;                // adr=7

//
//IDE Command Constants.  These should never change.
//
  ide_cmd_recal    = $10;
  ide_cmd_read     = $20;
  ide_cmd_write    = $30;
  ide_cmd_seek     = $70;
  ide_cmd_reset    = $08;       // ATA4 !!!
  ide_cmd_id       = $EC;
  ide_cmd_spindown = $E0;
  ide_cmd_spinup   = $E1;
  ide_cmd_flush    = $E7;
//
// IDE status register bits
//
  ide_sts_ERR =   1;  // If this bit is set then an error has occurred while executing the latest command
  ide_sts_IDX = $02;  // index pulse. Each revolution of the disk this bit is pulsed to '1' once.
  ide_sts_ECC = $04;  // ECC bit. if this bit is set then an ECC correction on the data was executed
  ide_sts_DRQ = $08;  // If this bit is set then the disk either wants data (disk write) or has data (disk read)
  ide_sts_SKC = $10;  // Indicates that a seek has been executed with success
  ide_sts_WFT = $20;  // indicates a write error has happened.
  ide_sts_RDY = $40;  // indicates that the disk has finished its power-up. Allways wait for this bit to be active
  ide_sts_BSY = $80;  // This bit is set when the disk is doing something
//
  ERR_BBK   = $80;
  ERR_UNC   = $40;
  ERR_MC    = $20;
  ERR_IDNF  = $10;
  ERR_MCR   = $08;
  ERR_ABRT  = $04;
  ERR_TK0NF = $02;
  ERR_AMNF  = $01;
//
// head and device register: bit 4 : 0=master,1=slave
//
  ide_dev_master   = 0;
  ide_dev_slave    = $10;
  HddDeviceMaster  = 0;
  HddDeviceSlave   = 1;

  HDDPortNone = 0;
  HDDPortF500 = 1;
  HDDPortF600 = 2;

type
 TIdeAccess=procedure(Sender: TObject; Drive: byte; Op: char);
 TDevAccess=procedure(Sender: TObject; Op: char) of object;

 TIdeReg = packed record
              reg_data: word;
              reg_err: byte;
              reg_cnt: byte;
              reg_sec: byte;
              reg_cyl: word;
              reg_head: byte;
              reg_command: byte;
              reg_status: byte;
              reg_control: byte;
              reg_astatus: byte;
            end;

  TIdeParams = packed record
                FBufSize: integer;
                FBufPtr: integer;
                FMaster: boolean;
                FReadOnly: boolean;
                FLastOp: char;
                FPrevCtl: byte;
                FCtl:byte;
                FLsb:byte;
                FLsbr:byte;
                FMsb:byte;
                FMsbr:byte;
                c, h, s: word;
                Reserved: array[0..7] of integer;
               end;

  THdcTmp = packed record
              ImgMaster, ImgSlave: ShortString;
            end;

  TIdType = (idFile, idDrive, idPartition);

  TIdeDevice = class(TObject)
    FHandle: THANDLE;
    FMaxLBA: integer;
    FImgFile: ShortString;
    FIdfFile: ShortString;
    FReg: TIdeReg;
    FIDBuf: array[0..BLOCK_SIZE] of byte;
    FBuf: PChar;
    FParams: TIdeParams;
    FOnAccess: TDevAccess;
  private
    function GetLSB: byte;
    function GetMSB: byte;
    procedure SetCtl(const Value: byte);
    procedure SetLSB(const Value: byte);
    procedure SetMSB(const Value: byte);
    procedure IdeCommandOk;
    procedure IdeRead;
    procedure IdeWrite;
    procedure IdeReset;
    procedure IdeIdentify;
    procedure IdeIdentifyGen(IdType: TIdType; Buffer: PChar; ImageSize: int64; ImageFile, IdentifyFile: string; c,h,s: word);
    procedure IdeReadSectors;
    procedure IdeWriteSectors;
    procedure IdeNextSector;
    procedure CheckFBuf;
    procedure IdeCHSGen(Size: int64; var c,h,s: word);
    function IdeFlush:boolean;
    function IdeSeek:boolean;
    function GetActive: boolean;
    function GetImgFile(Index: Integer): ShortString;
    function GetIdeReg(Index: Integer): byte;                 // interface with IDE device
    procedure SetIdeReg(Index: Integer; const Value: byte);   // interface with IDE device
  public
    constructor Create(IsMaster, IsReadOnly: boolean; ImageFile:String); virtual;
    destructor Destroy; override;
    procedure SaveToStream(Stream: TStream);
    procedure ReadFromStream(Stream: TStream);
    property  IdeReg[Index:Integer]:byte read GetIdeReg write SetIdeReg;
    property  CTL:byte read FParams.FCtl write SetCtl;
    property  LSB:byte read GetLSB write SetLSB;
    property  MSB:byte read GetMSB write SetMSB;
    property  Active: boolean read GetActive;
    property  LastOp: char read FParams.FLastOp;
    property  ImgFile:ShortString read FImgFile;
    property  ImgRO:boolean read FParams.FReadOnly write FParams.FReadOnly;
    property  OnAccess:TDevAccess read FOnAccess write FOnAccess;
  end;

  TIdeController = class(T8255)
    FDevice: array[HddDeviceMaster..HddDeviceSlave] of TIdeDevice;
    FImageRO: array[HddDeviceMaster..HddDeviceSlave] of boolean;
    FReserved: array[0..7] of integer;
    FDevAccess: integer;
    FOnAccess: TIdeAccess;
  private
    function GetImageFile(Index: Integer): String;
    procedure SetImageFile(Index: Integer; const Value: String);
    procedure Dev0Access(Sender: TObject; Op: char);
    procedure Dev1Access(Sender: TObject; Op: char);
    procedure SetOnAccess(const Value: TIdeAccess);
    function GetImageRO(Index: Integer): boolean;
    procedure SetImageRO(Index: Integer; const Value: boolean);
    function GetIdeReg(Index: Integer): byte;
    procedure SetIdeReg(Index: Integer; const Value: byte);
  protected
    function GetPort(Index: Integer): byte; override;                // interface with device
    procedure SetPort(Index: Integer; const Value: byte); override;  // interface with device
  public
    constructor Create; override;
    destructor Destroy; override;
    procedure SaveToStream(Stream: TStream);   override;
    procedure ReadFromStream(Stream: TStream); override;
    procedure ResetController;
    property IdeReg[Index:Integer]:byte read GetIdeReg write SetIdeReg;
    property ImageFile[Index: Integer]:String read GetImageFile write SetImageFile;
    property ImageRO[Index: Integer]:boolean read GetImageRO write SetImageRO;
    property DevAccess:integer read FDevAccess write FDevAccess;
    property OnAccess:TIdeAccess read FOnAccess write SetOnAccess;
  end;

var
  HDDPort: integer;
  HDDImage: array [HddDeviceMaster..HddDeviceSlave] of String;
  HDDRO: array [HddDeviceMaster..HddDeviceSlave] of boolean;
  ProImage: array [HddDeviceMaster..HddDeviceSlave] of String;
  ProRO: array [HddDeviceMaster..HddDeviceSlave] of boolean;
  IdeController: TIdeController;
  IdeProController: TIdeController;

implementation

constructor TIdeController.Create;
begin
  inherited;
  Reset;
  FDevAccess:=0;
  FOnAccess:=nil;
  FDevice[HddDeviceMaster]:=nil;
  FDevice[HddDeviceSlave]:=nil;
end;

destructor TIdeController.Destroy;
begin
  inherited;
  if Assigned(FDevice[HddDeviceMaster]) then FDevice[HddDeviceMaster].Free;
  if Assigned(FDevice[HddDeviceSlave]) then FDevice[HddDeviceSlave].Free;
end;

procedure TIdeController.Dev0Access(Sender: TObject; Op: char);
begin
  if Assigned(FOnAccess) then
    FOnAccess(Self, FDevAccess, Op);
end;

procedure TIdeController.Dev1Access(Sender: TObject; Op: char);
begin
  if Assigned(FOnAccess) then
    FOnAccess(Self, FDevAccess+1, Op);
end;

function TIdeController.GetImageFile(Index: Integer): String;
begin
  if Assigned(FDevice[Index and 1]) then
    Result:=FDevice[Index and 1].ImgFile
  else
    Result:='';
end;

function TIdeController.GetImageRO(Index: Integer): boolean;
begin
  if Assigned(FDevice[Index and 1]) then
    Result:=FDevice[Index and 1].ImgRO
  else
    Result:=False;
end;

function TIdeController.GetPort(Index: Integer): byte;
begin
  Result:=$FF;
  if Assigned(FDevice[HddDeviceMaster]) and FDevice[HddDeviceMaster].Active then
    case Index of
      ide_8255_ctl: Result:=FDevice[HddDeviceMaster].CTL;
      ide_8255_msb: Result:=FDevice[HddDeviceMaster].MSB;
      ide_8255_lsb: Result:=FDevice[HddDeviceMaster].LSB;
    end
  else if Assigned(FDevice[HddDeviceSlave]) and FDevice[HddDeviceSlave].Active then
    case Index of
      ide_8255_ctl: Result:=FDevice[HddDeviceSlave].CTL;
      ide_8255_msb: Result:=FDevice[HddDeviceSlave].MSB;
      ide_8255_lsb: Result:=FDevice[HddDeviceSlave].LSB;
    end;
end;

procedure TIdeController.ReadFromStream(Stream: TStream);
var HdcTmp: THdcTmp;
begin
  inherited;
  Stream.Read(FImageRO, sizeof(FImageRO));
  Stream.Read(HdcTmp, sizeof(HdcTmp));
  Stream.Read(FReserved, sizeof(FReserved));
  ImageFile[HddDeviceMaster]:=HdcTmp.ImgMaster;
  if Assigned(FDevice[HddDeviceMaster]) then
    FDevice[HddDeviceMaster].ReadFromStream(Stream);
  ImageFile[HddDeviceSlave]:=HdcTmp.ImgSlave;
  if Assigned(FDevice[HddDeviceSlave]) then
    FDevice[HddDeviceSlave].ReadFromStream(Stream);
  OnAccess:=FOnAccess;
end;

procedure TIdeController.SaveToStream(Stream: TStream);
var HdcTmp: THdcTmp;
begin
  inherited;
  HdcTmp.ImgMaster:='';
  HdcTmp.ImgSlave:='';
  if Assigned(FDevice[HddDeviceMaster]) then
    HdcTmp.ImgMaster:=FDevice[HddDeviceMaster].ImgFile;
  if Assigned(FDevice[HddDeviceSlave]) then
    HdcTmp.ImgSlave:=FDevice[HddDeviceSlave].ImgFile;
  Stream.Write(FImageRO, sizeof(FImageRO));
  Stream.Write(HdcTmp, sizeof(HdcTmp));
  Stream.Write(FReserved, sizeof(FReserved));
  if Assigned(FDevice[HddDeviceMaster]) then
    FDevice[HddDeviceMaster].SaveToStream(Stream);
  if Assigned(FDevice[HddDeviceSlave]) then
    FDevice[HddDeviceSlave].SaveToStream(Stream);
end;

procedure TIdeController.SetImageFile(Index: Integer; const Value: String);
var ss: string;
begin
  if Assigned(FDevice[Index and 1]) and
     (FDevice[Index and 1].ImgFile=Value) then
    exit;
  begin
    if Assigned(FDevice[Index and 1]) then FDevice[Index and 1].Free;
    FDevice[Index and 1]:=nil;
    if trim(Value)='' then FImageRO[Index and 1]:=True
    else
    try
      FDevice[Index and 1]:=TIdeDevice.Create((Index and 1)=0, FImageRO[Index and 1], Value);
    except
      on E:Exception do
        begin
          FDevice[Index and 1]:=nil;
          ss:=E.Message; OemToCharBuff(@ss[1], @ss[1], Length(ss));
          MessageBox(0, PChar(Format('IDE Device %d not created: '#13#10#13#10'%s',
                                     [Index and 1, ss])),
                     'IdeDevice Error', MB_OK+MB_ICONSTOP);
        end;
    end;
  end;
end;

procedure TIdeController.SetImageRO(Index: Integer; const Value: boolean);
begin
  FImageRO[Index and 1]:=Value;
  if Assigned(FDevice[Index and 1]) then
    FDevice[Index and 1].ImgRO:=Value;
end;

procedure TIdeController.SetOnAccess(const Value: TIdeAccess);
begin
  FOnAccess := Value;
  if Assigned(FDevice[HddDeviceMaster]) then
  begin
    if Assigned(Value) then
      FDevice[HddDeviceMaster].OnAccess:=Dev0Access
    else
      FDevice[HddDeviceMaster].OnAccess:=nil;
  end;
  if Assigned(FDevice[HddDeviceSlave]) then
  begin
    if Assigned(Value) then
      FDevice[HddDeviceSlave].OnAccess:=Dev1Access
    else
      FDevice[HddDeviceSlave].OnAccess:=nil;
  end;
end;

procedure TIdeController.SetPort(Index: Integer; const Value: byte);
begin
  if Assigned(FDevice[HddDeviceMaster]) then
  case Index of
    ide_8255_ctl: FDevice[HddDeviceMaster].CTL:=Value;
    ide_8255_msb: FDevice[HddDeviceMaster].MSB:=Value;
    ide_8255_lsb: FDevice[HddDeviceMaster].LSB:=Value;
  end;
  if Assigned(FDevice[HddDeviceSlave]) then
  case Index of
    ide_8255_ctl: FDevice[HddDeviceSlave].CTL:=Value;
    ide_8255_msb: FDevice[HddDeviceSlave].MSB:=Value;
    ide_8255_lsb: FDevice[HddDeviceSlave].LSB:=Value;
  end;
end;

function TIdeController.GetIdeReg(Index: Integer): byte;
begin
  Result:=$FF;
  if Assigned(FDevice[HddDeviceMaster]) and FDevice[HddDeviceMaster].Active then
    Result:=FDevice[HddDeviceMaster].IdeReg[Index]
  else if Assigned(FDevice[HddDeviceSlave]) and FDevice[HddDeviceSlave].Active then
    Result:=FDevice[HddDeviceSlave].IdeReg[Index];
end;

procedure TIdeController.ResetController;
begin
  if Assigned(FDevice[HddDeviceMaster]) then FDevice[HddDeviceMaster].IdeReset;
  if Assigned(FDevice[HddDeviceSlave]) then FDevice[HddDeviceSlave].IdeReset;
end;

procedure TIdeController.SetIdeReg(Index: Integer; const Value: byte);
begin
  if Assigned(FDevice[HddDeviceMaster]) then
    FDevice[HddDeviceMaster].IdeReg[Index]:=Value;
  if Assigned(FDevice[HddDeviceSlave]) then
    FDevice[HddDeviceSlave].IdeReg[Index]:=Value;
end;

{ TIdeDevice }

constructor TIdeDevice.Create(IsMaster, IsReadOnly: boolean; ImageFile:String);
var cDrive: char;
    Geometry: TDISKGEOMETRY;
    phis, IdeOK: boolean;
    dwSizeLow, dwSizeHigh: DWORD;
    FDiskSize, FFreeSize: int64;
//    li: LARGE_INTEGER;
begin
  inherited Create;
  IdeReset;
  FBuf:=nil;
  FOnAccess:=nil;
  FHandle:=INVALID_HANDLE_VALUE;
  with FParams do
  begin
    FBufSize:=0;
    FPrevCtl:=0;
    FImgFile:='';
    FIdfFile:='';
    FMaster:=IsMaster;
    FReadOnly:=IsReadOnly;
    FImgFile:=ImageFile;
    FIdfFile:=ChangeFileExt(ImageFile, '.IDE');
  end;
  FillChar(FIdBuf, 512, 0);
  if IsDrive(ImageFile, @cDrive) then
  begin
    IdeOK:=(cDrive in ['0'..'9'])and
           GetIdeDiskIdentify(cDrive, @FIdBuf[0]);
    if not HDDOpen(cDrive, (cDrive='0') or IsReadOnly {18.06.2012 was:True}, FHandle, phis, FDiskSize, FFreeSize, @Geometry) then       // ReadOnly Allways
      raise Exception.Create(LastError);
    if IdeOK then
    begin
{      li.QuadPart:=FDiskSize;
      li.QuadPart:=li.QuadPart shr 9;   //   /512
      PLongWord(@FIdBuf[2*60])^ := li.LowPart; // lba
      PLongWord(@FIdBuf[2*57])^ := PLongWord(@FIdBuf[2*60])^;              ,
      PWord(@FIdBuf[2*7])^ := PWord(@FIdBuf[2*61])^;
      PWord(@FIdBuf[2*8])^ := PWord(@FIdBuf[2*60])^;
}    end
    else
    begin
      if phis then IdeIdentifyGen(idDrive, @FIdBuf[0], FDiskSize, cDrive+':', FIdfFile,
                                  (Geometry.Cylinders * Geometry.TracksPerCylinder) div 16,
                                  16,
                                  Geometry.SectorsPerTrack)
      else begin
        IdeCHSGen(FDiskSize, FParams.c, FParams.h, FParams.s);
        IdeIdentifyGen(idPartition, @FIdBuf[0], FDiskSize, cDrive+':',
                       FIdfFile, FParams.c, FParams.h, FParams.s);
      end;
    end;
  end
  else
  begin
    if not FileExists(ImageFile) then
      raise Exception.CreateFmt('File not found: %s', [ImageFile])
    else
    begin
      if FParams.FReadOnly then
        FHandle:=CreateFile(PChar(ImageFile), GENERIC_READ, 0, nil, OPEN_EXISTING, 0, 0)
      else
        FHandle:=CreateFile(PChar(ImageFile), GENERIC_READ or GENERIC_WRITE, 0, nil, OPEN_EXISTING, 0, 0);
      if (FHandle=INVALID_HANDLE_VALUE) then
        raise Exception.Create(LastError);
    end;
    dwSizeLow := GetFileSize(FHandle, @dwSizeHigh);
    if (dwSizeLow = $FFFFFFFF) and (GetLastError() <> NO_ERROR ) then
        raise Exception.Create(LastError);
    FDiskSize:=dwSizeHigh;
    FDiskSize:=(FDiskSize shl 32) + dwSizeLow;
    IdeCHSGen(FDiskSize, FParams.c, FParams.h, FParams.s);
    IdeIdentifyGen(idFile, @FIdBuf[0], FDiskSize, FImgFile,
                   FIdfFile, FParams.c, FParams.h, FParams.s);
  end;
  FMaxLBA:=FDiskSize div BLOCK_SIZE;
end;

destructor TIdeDevice.Destroy;
begin
  inherited;
  if FHandle<>INVALID_HANDLE_VALUE then
    CloseHandle(FHandle);
  if Assigned(FBuf) then FreeMem(FBuf);
end;

function TIdeDevice.GetLSB: byte;
begin
  Result:=FParams.FLsbr;
end;

function TIdeDevice.GetMSB: byte;
begin
  Result:=FParams.FMsbr;
end;

procedure TIdeDevice.IdeCommandOk;
begin
  FReg.reg_err := 0;
  FReg.reg_status := ide_sts_RDY or ide_sts_SKC;
end;

procedure TIdeDevice.IdeReset;
begin
  FParams.FCtl:=0;
  FParams.FPrevCtl:=0;
  FParams.FBufPtr:=-1;
  FReg.reg_control:=0;
  IdeCommandOk;
  FReg.reg_cnt:=1;
  FReg.reg_sec:=1;
  FReg.reg_cyl:=0;
  FReg.reg_head:=ide_dev_master;
  FReg.reg_status:=ide_sts_RDY;
end;

procedure TIdeDevice.IdeRead;
begin
 if Active then with FParams do
  if ((FReg.reg_control and 4)=4)
  then FLsbr:=ide_sts_BSY  // stay at "resetting" state until reg_control.D2=0
  else
   Case FCtl of
    ide_data     : if FBufPtr>=0 then
                   begin
                     FLsbr:=byte(FBuf[FBufPtr]);
                     inc(FBufPtr);
                     FMsbr:=byte(FBuf[FBufPtr]);
                     inc(FBufPtr);
                     if FBufPtr>=BLOCK_SIZE*FReg.reg_cnt then
                       IdeCommandOk;
                   end;
    ide_err      : FLsbr:=FReg.reg_err;
    ide_sec_cnt  : FLsbr:=FReg.reg_cnt;
    ide_sector   : FLsbr:=FReg.reg_sec;
    ide_cyl_lsb  : FLsbr:=lo(FReg.reg_cyl);
    ide_cyl_msb  : FLsbr:=hi(FReg.reg_cyl);
    ide_head     : FLsbr:=FReg.reg_head;
    ide_status, ide_astatus : FLsbr:=FReg.reg_status
    else raise Exception.CreateFmt('IdeRead: not supported register address: %d', [FCtl]);
   end;
end;

procedure TIdeDevice.IdeWrite;
begin
  with FParams do
  begin
   if Active and ((FReg.reg_control and 4)=4) and (FCtl<>ide_control) then exit;  // stay at "resetting" state until reg_control.D2=0
   Case FCtl of
    ide_control : if Active then
                  begin
                    if ((FReg.reg_control and 4)=0)and((FLsb and 4)=4) then
                      FReg.reg_status:=ide_sts_BSY;
                    if ((FReg.reg_control and 4)=4)and((FLsb and 4)=0) then
                      IdeReset();                                               // perform IdeReset on D2 bit
                    FReg.reg_control:=FLsb;
                  end;
    ide_data    : if Active and (FBufPtr>=0) then
                  begin
                    byte(FBuf[FBufPtr]):=FLsb;
                    inc(FBufPtr);
                    byte(FBuf[FBufPtr]):=FMsb;
                    inc(FBufPtr);
                    if FBufPtr>=BLOCK_SIZE*FReg.reg_cnt then
                      IdeWriteSectors();
                  end;
    ide_sec_cnt : FReg.reg_cnt := FLsb;
    ide_sector  : FReg.reg_sec := FLsb;
    ide_cyl_lsb : FReg.reg_cyl := (FReg.reg_cyl and $FF00) + FLsb;
    ide_cyl_msb : FReg.reg_cyl := (FReg.reg_cyl and $FF) + 256*FLsb;
    ide_head    : begin
                    if (FLsb and $A0)<>$A0 then
                      raise Exception.Create('Only 512 bytes sectors supported!');
                    FReg.reg_head := FLsb;
                    IdeCommandOk;
                  end;
    ide_command : if Active then
                  begin
                    FReg.reg_command:=FLsb;
                    if (FLsb and $FE)=ide_cmd_reset then
                      IdeReset()
                    else if (FLsb and $FE)=ide_cmd_read then
                      IdeReadSectors()
                    else if ((FLsb and $FE)=ide_cmd_write)and(not FReadOnly) then
                         begin
                           if IdeSeek() then
                           begin
                             FReg.reg_status := ide_sts_DRQ or ide_sts_SKC;
                             FBufPtr:=0;
                             CheckFBuf();
                           end;
                         end
                    else if (FLsb=ide_cmd_id) then
                      IdeIdentify()
                    else if (FLsb=ide_cmd_flush) then          // FLUSH CACHE
                         begin
                           if IdeFlush() then
                             IdeCommandOk()
                           else
                             FReg.reg_status := ide_sts_RDY or ide_sts_WFT or ide_sts_SKC or ide_sts_ERR; // 0x71
                         end
                    else if (FLsb=ide_cmd_recal) then
                      IdeCommandOk()
                    else if (FLsb=ide_cmd_seek) and IdeSeek() then
                      IdeCommandOk();
                  end;
    else raise Exception.CreateFmt('IdeWrite: not supported register address: %d', [FCtl]);
   end;
  end;
end;

procedure TIdeDevice.ReadFromStream(Stream: TStream);
begin
  Stream.Read(FReg, sizeof(FReg));
  Stream.Read(FParams, sizeof(FParams));
  FParams.FBufSize:=-1;                    //
  CheckFBuf();                             // reallocate  FBuf  array
  Stream.Read(FBuf^, FParams.FBufSize);
end;

procedure TIdeDevice.SaveToStream(Stream: TStream);
begin
  Stream.Write(FReg, sizeof(FReg));
  CheckFBuf();                             // allocate FBuf array if FBuf=nil
  Stream.Write(FParams, sizeof(FParams));
  Stream.Write(FBuf^, FParams.FBufSize);
end;

procedure TIdeDevice.SetCtl(const Value: byte);
begin
 with FParams do
  If FPrevCtl<>Value then
  begin
    if ((FPrevCtl and ide_wr_line)<>(Value and ide_wr_line)) and
        (Value and ide_wr_line <> 0) then                                        // по фронту импульса WRITE
      begin
        FCtl := Value and (not (ide_wr_line or ide_rd_line or ide_rst_line));    // register address
        IdeWrite();
      end
    else
    if ((FPrevCtl and ide_rd_line)<>(Value and ide_rd_line)) and
        (Value and ide_rd_line <> 0) then                                        // по фронту импульса READ
      begin
        FCtl := Value and (not (ide_wr_line or ide_rd_line or ide_rst_line));    // register address
        IdeRead()
      end
    else
    if ((FPrevCtl and ide_rst_line)<>(Value and ide_rst_line)) and
       (Value and ide_rst_line = 0) then                                         // по спаду импульса RESET
      IdeReset();
    FPrevCtl:=Value;
  end
end;

procedure TIdeDevice.SetLSB(const Value: byte);
begin
  FParams.FLsb:=Value;
end;

procedure TIdeDevice.SetMSB(const Value: byte);
begin
  FParams.FMsb:=Value;
end;

procedure TIdeDevice.IdeIdentify;
begin
  if Assigned(FOnAccess) then FOnAccess(Self, 'r');
  if not Assigned(FBuf) then
    GetMem(FBuf, BLOCK_SIZE*FReg.reg_cnt);
  CopyMemory(FBuf, @FIdBuf[0], BLOCK_SIZE);
  FParams.FBufPtr:=0;
  IdeCommandOk;
  FReg.reg_status := ide_sts_RDY or ide_sts_DRQ or ide_sts_SKC;
end;

function TIdeDevice.IdeFlush:boolean;
var writed: cardinal;
begin
  Result:=False;
  if FParams.FBufPtr<=0 then exit;
  Result:=IdeSeek() and WriteFile(FHandle, FBuf, FParams.FBufPtr, writed, nil);
  if Result then
  begin
    FReg.reg_status := ide_sts_RDY or ide_sts_SKC or ide_sts_ERR;
    FReg.reg_err := ERR_UNC;
  end
  else
  begin
    FReg.reg_err := 0;
    FReg.reg_status := ide_sts_RDY or ide_sts_SKC;
  end;
end;

procedure TIdeDevice.IdeReadSectors;
var bytesread: cardinal;
    cnt: byte;
    ptr: pointer;
begin
   CheckFBuf();
   cnt:=FReg.reg_cnt;
   ptr:=FBuf;
   if Assigned(FOnAccess) then FOnAccess(Self, 'r');
   while (cnt>0) and IdeSeek() do
   begin
     if not (ReadFile(FHandle, ptr^, BLOCK_SIZE, bytesread, nil)) then
     begin
       FReg.reg_status := ide_sts_RDY or ide_sts_ERR or ide_sts_SKC;     (* $51; *)
       FReg.reg_err := ERR_UNC;
     end
     else
     begin
       FReg.reg_err := 0;
       FReg.reg_status := ide_sts_RDY or ide_sts_DRQ or ide_sts_SKC;
     end;
     dec(cnt);
     if cnt>0 then IdeNextSector();
     ptr:=pointer(integer(ptr)+BLOCK_SIZE);
   end;
   FParams.FBufPtr:=0;
end;

function TIdeDevice.IdeSeek: boolean;
var ii, jj:integer;
begin
   ii:=0;
   Result:=True;
   if (FReg.reg_head and $40 <>0) then              // LBA
   begin
      ii:=FReg.reg_head and $0F; ii:=ii shl 24;
      jj:=FReg.reg_cyl;  ii:=ii+(jj shl 8)+FReg.reg_sec;
      if (ii >= FMaxLBA) then
      begin
        FReg.reg_status := ide_sts_RDY or ide_sts_ERR;
        FReg.reg_err := ERR_IDNF;
        Result:=False;
      end;
   end
   else
   begin
      if (FReg.reg_cyl >= FParams.c) or
         ((FReg.reg_head and $0F) >= FParams.h) or
         (FReg.reg_sec > FParams.s) then
      begin
        FReg.reg_status := ide_sts_RDY or ide_sts_ERR;
        FReg.reg_err := ERR_IDNF;
        Result:=False;
      end
      else
        ii := (FReg.reg_cyl * FParams.h + (FReg.reg_head and $0F)) * FParams.s + FReg.reg_sec - 1;
  end;
  if Result then
  begin
    Result:=DiskFileSeek(FHandle, ii, FILE_BEGIN);
    if not Result then
    begin
      FReg.reg_status := ide_sts_ERR or ide_sts_WFT or ide_sts_RDY;
      FReg.reg_err := ERR_IDNF;
    end;
  end;
end;

procedure TIdeDevice.IdeIdentifyGen(IdType: TIdType; Buffer: PChar; ImageSize: int64; ImageFile, IdentifyFile: string; c, h, s: word);
var FS: TFileStream;
    cs: byte;
    i: integer;
    li: LARGE_INTEGER;
begin
  FS:=nil;
  if (IdType=idFile) and FileExists(IdentifyFile) then
  begin
    try
      FS:=TFileStream.Create(IdentifyFile, fmOpenRead or fmShareDenyWrite);
      FS.Read(Buffer^, BLOCK_SIZE);
    finally
      if Assigned(FS) then FS.Free;
    end;
  end
  else
  begin
    if Length(ImageFile)>=IDE_MAX_MODEL then
      delete(ImageFile, 1, Length(ImageFile)-IDE_MAX_MODEL);
    StrLCopy(pointer(integer(Buffer)+54), PChar(ImageFile), 40);       // model
    ChangeByteOrder(pointer(integer(Buffer)+54), 40);
    case IdType of
      idFile:      ImageFile:='DRIVE IMAGE (FILE)';
      idDrive:     ImageFile:='LOCAL DRIVE (HDD)';
      idPartition: ImageFile:='LOCAL PARTITION';
    end;
    li.QuadPart:=ImageSize;
    li.QuadPart:=li.QuadPart shr 9;   //   /512
    StrLCopy(pointer(integer(Buffer)+20), PChar(ImageFile), 20);       // serial
    ChangeByteOrder(pointer(integer(Buffer)+20), 20);
    StrLCopy(pointer(integer(Buffer)+46),  '0999', 8);                 // firmware
    ChangeByteOrder(pointer(integer(Buffer)+46), 8);
    PWord(Buffer)^ := $045A;
    PWord(@Buffer[2*1])^ := c;
    PWord(@Buffer[2*3])^ := h;
    PWord(@Buffer[2*6])^ := s;
    PLongWord(@Buffer[2*60])^ := li.LowPart; // lba
    PLongWord(@Buffer[2*57])^ := PLongWord(@Buffer[2*60])^;
    PWord(@Buffer[2*7])^ := PWord(@Buffer[2*61])^;
    PWord(@Buffer[2*8])^ := PWord(@Buffer[2*60])^;
    PWord(@Buffer[2*20])^ := 3;                   // a dual ported multi-sector buffer capable of simultaneous transfers with a read caching capability
    PWord(@Buffer[2*21])^ := 512;                 // cache size=256k
    PWord(@Buffer[2*22])^ := 4;                   // ECC bytes
    PWord(@Buffer[2*49])^ := $200;                // LBA supported
    PWord(@Buffer[2*80])^ := $3E;                 // support specifications up to ATA-5
    PWord(@Buffer[2*81])^ := $13;                 // ATA/ATAPI-5 T13 1321D revision 3
    PWord(@Buffer[2*82])^ := $60;                 // supported look-ahead and write cache
// make checksum
    Buffer[510] := #$A5;
    cs := 0;
    for i:=0 to 511 do
      cs:=cs + PByte(@Buffer[i])^;
    Buffer[511] := chr(0-cs);
  end;
end;

procedure TIdeDevice.IdeCHSGen(Size: int64; var c, h, s: word);
begin
  Size:=Size div BLOCK_SIZE;
  h:=16;
  s:=127;
  while (Size mod (h*s)<>0) and (s>16) and (Size div (h*s) < 60000) do
    s:=s-1;
  c:=Size div (h*s);
end;

function TIdeDevice.GetActive: boolean;
begin
  Result:=FParams.FMaster=((FReg.reg_head and ide_dev_slave)=0);
end;

procedure TIdeDevice.IdeWriteSectors;
var writed: cardinal;
    cnt: byte;
    ptr: pointer;
begin
   CheckFBuf();
   cnt:=FReg.reg_cnt;
   ptr:=FBuf;
   if Assigned(FOnAccess) then FOnAccess(Self, 'w');
   while IdeSeek() and (cnt>0) do
   begin
     if not (WriteFile(FHandle, ptr^, BLOCK_SIZE, writed, nil)) then
     begin
       FReg.reg_status := ide_sts_RDY or ide_sts_SKC or ide_sts_ERR;
       FReg.reg_err := ERR_UNC;
     end
     else
     IdeCommandOk;
     dec(cnt);
     if cnt>0 then IdeNextSector();
     ptr:=pointer(integer(ptr)+BLOCK_SIZE);
   end;
   FParams.FBufPtr:=0;
end;

procedure TIdeDevice.CheckFBuf;
begin
   if FParams.FBufSize<BLOCK_SIZE*FReg.reg_cnt then
   begin
     FParams.FBufSize:=BLOCK_SIZE*FReg.reg_cnt;
     if Assigned(FBuf) then FreeMem(FBuf);
     GetMem(FBuf, FParams.FBufSize);
   end;
end;

procedure TIdeDevice.IdeNextSector;
var ii, jj: integer;
    head: byte;
begin
   if (FReg.reg_head and $40 <> 0) then  // LBA
   begin
     ii:=FReg.reg_head and $0F; ii:=ii shl 24;
     jj:=FReg.reg_cyl;  ii:=ii+(jj shl 8)+FReg.reg_sec;
     inc(ii);
     FReg.reg_head:=(ii shr 24) and $0F;
     FReg.reg_cyl:=(ii shr 8) and $FFFF;
     FReg.reg_sec:=ii and $FF;
   end
   else
   begin
     if (FReg.reg_sec < FParams.s) then
       inc(FReg.reg_sec)
     else
     begin
       FReg.reg_sec := 1;
       head := (FReg.reg_head and $0F) + 1;
       if (head < FParams.h) then
         FReg.reg_head := (FReg.reg_head and $F0)+head
       else
       begin
         FReg.reg_head := FReg.reg_head and $F0;
         inc(FReg.reg_cyl);
       end;
     end;
   end;
end;

function TIdeDevice.GetImgFile(Index: Integer): ShortString;
begin
  Result:=FImgFile;
end;

function TIdeDevice.GetIdeReg(Index: Integer): byte;
begin
  with FParams do begin
    if Index=ide_data then raise Exception.Create('Wrong IDE register: ide_data')
    else if Index=pro_data_h then Result:=FMsbr
    else
    begin
      case Index of
        pro_data_l: Index:=ide_data;
//        pro_control: Index:=ide_control;
        pro_astatus: Index:=ide_astatus;
        pro_err    : Index:=ide_err;
        pro_sec_cnt: Index:=ide_sec_cnt;
        pro_sector : Index:=ide_sector;
        pro_cyl_lsb: Index:=ide_cyl_lsb;
        pro_cyl_msb: Index:=ide_cyl_msb;
        pro_head   : Index:=ide_head;
//        pro_command: Index:=ide_command;
        pro_status : Index:=ide_status;
      end;
      FCtl:=Index;
      IdeRead;
      Result:=FLsbr;
    end;
  end;
end;

procedure TIdeDevice.SetIdeReg(Index: Integer; const Value: byte);
begin
  with FParams do begin
    if Index=ide_data then raise Exception.Create('Wrong IDE register: ide_data')
    else if Index=pro_data_h then FMsb:=Value
    else
    begin
      FLsb:=Value;
      case Index of
        pro_data_l: Index:=ide_data;
        pro_control: Index:=ide_control;
//        pro_astatus: Index:=ide_astatus;
        pro_err    : Index:=ide_err;
        pro_sec_cnt: Index:=ide_sec_cnt;
        pro_sector : Index:=ide_sector;
        pro_cyl_lsb: Index:=ide_cyl_lsb;
        pro_cyl_msb: Index:=ide_cyl_msb;
        pro_head   : Index:=ide_head;
        pro_command: Index:=ide_command;
//        pro_status : Index:=ide_status;
      end;
      FCtl:=Index;
      IdeWrite;
    end;
  end;
end;

initialization
  IdeController:=TIdeController.Create;
  IdeProController:=TIdeController.Create;
  IdeController.DevAccess:=0;
  IdeProController.DevAccess:=2;

finalization
  IdeController.Free;
  IdeProController.Free;

end.

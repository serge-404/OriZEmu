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

 SD-card (SPI) emulation: SDC V1 or MMC (CMD8 rejected), non-SDHC

 Issue CMD8 prior to first ACMD41 for initialization of HCSD
 makes the cards realize that the host supports the Physical
 Layer Version 2.00 and the card can enable new functions.

 *****************************************************************}

unit modSD;

interface


{$I 'OrionZEm.inc'}


Uses Windows, SysUtils, classes, Forms, HDDUtils;

const
{ HARDWARE - RTC/SD port $F762}
{
 RTC_DOUT equ 80h   74574 RTC/SD output bits
 RTC_CLK  equ 40h
 RTC_DIR  equ 20h
 RTC_CE   equ 10h
 RTC_DIN  equ 01h   74125 RTC/SD input bits
N8VEM:
 SD_PWR   equ 08h   reserved - SD poweron/poweroff
 SD_CS    equ 04h   NPN inverter, positive logic.
 SD_CLK   equ 02h
 SD_DOUT  equ 01h
 SD_DIN   equ 80h
MSX:
 MSXSD_PWR = F602.D3.WR reserved - SD poweron/poweroff
 MSXSD_CS  = F602.D2.WR
 MSXSD_CLK = F603.WR
 MSXSD_RD  = F603.RD
 MSXSD_WR  = F603.D7.WR
}
  SD_ADDR0 = $F762;
  SD_ADDR1 = $F763;
  SD_PWR   = 8;         // SD_ADDR0.D3 - positive logic, both schemes
  SD_CS    = 4;         // SD_ADDR0.D2 - positive logic (because NPN inverter before /CS), both schemes
  SD_CLK   = 2;         // SD_ADDR0.D1 - N8VEM
  SD_DOUT  = 1;         // SD_ADDR0.D0 - N8VEM
  SD_DIN   = $80;       // SD_ADDR0.D7 - N8VEM
  MSXSD_WR = $80;       // SD_ADDR1.D7 - MSX SD
  SD_BLOCK_SIZE = 512;

{ SPI }
  SPI_START_TOKEN = $FE;

{ R1 Responses, D7=0 allways }
  R1_IN_IDLE     = 1;
  R1_ERASE_RESET = 2;
  R1_ILLEGAL_CMD = 4;
  R1_CMD_CRC_ERR = 8;
  R1_ERA_SEQ_ERR = 16;
  R1_ADDRESS_ERR = 32;
  R1_PARAM_ERR   = 64;

{ ErrorToken [appears after R1 response if transfer error ] bits, D7:D5=0 allways }
  ERT_ERROR      = 1;
  ERT_CARD_CONTR = 2;
  ERT_MEDIA_ECC  = 4;
  ERT_OUTOFRANGE = 8;
  ERT_CARD_LOCK  = 16;

{ Write Responses, D7:D4=0 allways }
  WRITE_ACCEPTED = $05;                   // Data Accepted
  WRITE_CRCERROR = $0b;                   // Transmission CRC Error
  WRITE_DATAERROR= $0d;                   // Data Write Error

{ Implemented commands }
  CMD0 =   $40;   // GO_IDLE_STATE
  CMD1 =   $41;   // SEND_OP_COND
  CMD9 =   $49;   // SEND_CSD
  CMD10 =  $4A;   // SEND_CID
  CMD16 =  $50;   // SET_BLOCKLEN
  CMD17 =  $51;   // READ_BLOCK
  CMD18 =  $52;   // MULTI_READ_BLOCK
  CMD24 =  $58;   // WRITE_BLOCK
  ACMD41 = $69;   // SDC init
  CMD55 =  $77;   // APP_CMD
  CMD58 =  $7A;   // READ_OCR
  CMD59 =  $7B;   // CRC_ON_OFF
//
// TODO: CMD8 & HCSD

type
  TSpiState = (
    SPI_PWR_STATE,
    SPI_IDLE_STATE,
    SPI_ARG_X_LO,
    SPI_ARG_X_HI,
    SPI_ARG_Y_LO,
    SPI_ARG_Y_HI,
    SPI_ARG_CRC,
    SPI_RESPOND_SINGLE,
    SPI_RESPOND_MULTI,
    SPI_READ_SINGLE_BLOCK,
    SPI_READ_MULTIPLE_BLOCK,
    SPI_WRITE_SINGLE,
    SPI_WRITE_SINGLE_BLOCK,
    SPI_WRITE_SINGLE_CRC
  );

  TspiArgW = packed record                 // for WORD-wide addressing inside (spiArg:DWORD)
    spiArgY: word;
    spiArgX: word;
  end;
  PspiArgW = ^TspiArgW;

  TspiArgB = packed record                 // for BYTE-wide addressing inside (spiArg:DWORD)
    spiArgYlo: byte;
    spiArgYhi: byte;
    spiArgXlo: byte;
    spiArgXhi: byte;
  end;
  PspiArgB = ^TspiArgB;

  TCardIDRegister = packed record
    MID: byte;                     // Manufacturer ID       [binary]
    OID: word;                     // OEM/Application ID    [ACSII]
    PNM: array[0..4] of char;      // Product Name          [ACSII]
    PRV: byte;                     // Product Revision      [BCD]
    PSN: DWORD;                    // Serial Number         [binary]
    MDT: word;                     // Manufacturer Date     [BCD]
    CRC: byte;                     // CRC-7 checksum, D0=1  [binary]
  end;
  PCardIDRegister = ^TCardIDRegister;

  TCardSDRegister = packed record
    ZERO:              byte;       // D7=D6=00  D5:D0=reserved=00000
    TAAC:              byte;       // D7:D0=TAAC[7:0]
    NSAC:              byte;       // D7:D0=NSAC[7:0]
    TRAN_SPEED:        byte;       // D7:D0=TRAN_SPEED[7:0]
    CCC:               byte;       // D7:D0=CCC[11:4]
    CCC_READ_BL_LEN:   byte;       // D7:D4=CCC[3:0]  D3:D0=READ_BL_LEN[3:0]
    BITFIELD1_CSIZE:   byte;       // D7=RD_BL_PARTIAL  D6=WR_BL_MISALIGN  D5=RD_BL_MISALIGN  D4=DSR_IMP  D3:D2=reserved=00  D1:D0=CSIZE[11:10]
    CSIZE:             byte;       // D7:D0=C_SIZE[9:2]
    CSIZE_VDD:         byte;       // D7:D6=C_SIZE[1:0]  D5:D3=VDD_R_CURR_MIN[2:0]  D2:D0=VDD_R_CURR_MAX[2:0]
    VDD_CSIZE_MULT:    byte;       // D7:D5=VDD_W_CURR_MIN[2:0]  D4:D2=VDD_W_CURR_MAX[2:0]  D2:D0=C_SIZE_MULT[2:1]
    CSIZE_MULT_SECSIZE:byte;       // D7=C_SIZE_MULT[0]  D6=ERASE_BLK_EN  D5:D0=SECTOR_SIZE[6:1]
    SECSIZE_WPGRP_SIZE:byte;       // D7=SECTOR_SIZE[0]  D6:D0=WP_GRP_SIZE[6:0]
    WPGRP_ENA_WRBL_LEN:byte;       // D7=WP_GRP_ENABLE   D6:D5=reserved=00  D4:D2=R2W_FACTOR[2:0]  D1:D0=WRITE_BL_LEN[3:2]
    WRBL_LEN__ZERO:    byte;       // D7:D6=WRITE_BL_LEN[1:0]  D5=0  D4:D0=reserved=00000
    BITFIELD2:         byte;       // D7=FILE_FORMAT_GRP  D6=COPY  D5=PERM_WRITE_PROTECT  D4=TMP_WRITE_PROTECT  D3:D2=FILE_FORMAT[1:0]  D1:D0=reserved=00
    CRC:               byte;       // D7:D1= CRC-7 checksum   D0=1
  end;
  PCardSDRegister = ^TCardSDRegister;

  TSdAccess=procedure(Sender: TObject; Drive: byte; Op: char);

  TSDParams = packed record
  // hardware
    Scheme:         integer;       // 0=N8VEM (only software bit-by-bit)  1=MSX (bit->byte register on input, /CLK formed by /WR/RD)
    RDout:          byte;          // MSX scheme input 8-bit register
  // SD
    SPDR:           byte;          // SD Card DATA REGISTER (out)
    PrevR0:         byte;          // Port F762 register in N8VEM
    R0, R1:         byte;          // Port F762, F763 registers (both schemes)
    InpBitCount:    integer;       // variable for 8bit->1bit shift conversion
  // SPI Emulation
    spiArg:         DWORD;         // SPI command argument
    spiByte:        byte;          // SD Card DATA REGISTER (in)
    spiState:       TSpiState;
    spiCommand:     byte;
    spiByteCount:   DWORD;
    spiResponseBuffer: array [0..20] of byte;
    spiResponsePtr: PByte;
    spiResponseEnd: PByte;
  // SD image emulation
    FileOffset:     DWORD;         // offset inside file (0..MaxLBA-1)
    SectorIndex:    DWORD;         // offset inside sector (0..SectorSize-1)
    sectorSize:     DWORD;
    FReserved:         array[0..7] of integer;
  end;

  TSDController = class(TObject)
    SDHC: boolean;				// allways False (TODO)
    FImgFile: String;
    FImageRO: boolean;
    FHandle: THANDLE;
    FMaxLBA: integer;
    FOnAccess: TSdAccess;
    FBuf: PChar;                                //    SdSector: array[0..SD_BLOCK_SIZE] of byte;
    FParams: TSDParams;
  private
    procedure SetImageFile(const Value: String);
    procedure SetOnAccess(const Value: TSdAccess);
    procedure SetImageRO(const Value: boolean);
    // SPI
    procedure SDCheckFBuf;
    procedure rl(var bb:byte);                  // rotate left
    procedure UpdateSpi;                        // SPI StateMachine next step
    procedure SDSeekToOffset(offset: DWORD;
                             DoRead: boolean);  // seek file to offset, read 'sectorSize' bytes to 'SdSector' buffer
    function  SDCommit():boolean;               // write 'sectorSize' bytes from 'SdSector' buffer to file
    procedure SDWriteByte(value: byte);         // write byte to SdSector[SectorIndex]
    function  SDReadByte():byte;                // read byte from SdSector[SectorIndex]
    function  SDEnabled():boolean;
    procedure SDPowerOn;
    function  GetPort1: byte;                    // interface with device
    procedure SetPort1(const Value: byte);       // interface with device
    function  GetPort0: byte;                    // interface with device
    procedure SetPort0(const Value: byte);       // interface with device
    procedure SetScheme(const Value: integer);
  public
    constructor Create;
    destructor Destroy; override;
    procedure Reset;
    procedure SaveToStream(Stream: TStream);
    procedure ReadFromStream(Stream: TStream);
    property ImageFile:String read FImgFile write SetImageFile;
    property ImageRO:boolean read FImageRO write SetImageRO;
    property OnAccess:TSdAccess read FOnAccess write SetOnAccess;
    property Port0:byte read GetPort0 write SetPort0;                   // F762
    property Port1:byte read GetPort1 write SetPort1;                   // F763
    property Scheme:integer read FParams.Scheme write SetScheme;        // 0=N8VEM, 1=MSX
  end;

var SDController: TSDController;
    SDScheme: integer; 
    SDImage: String;
    SDRO: boolean;
    FIDBuf: array[0..SD_BLOCK_SIZE] of byte;

implementation

Uses modOrion;

{$IFDEF SD_DEBUG}
function ascii(ch:byte):char;
begin
   if (ch >= 32)and(ch<=127) then
     Resyult:=ch
   else
     Result:='.';
end;
{$ENDIF}

procedure SPI_DEBUG(fmt: string; const arr: array of const);
begin
{$IFDEF SD_DEBUG}
   modOrion.DebugInfo(Format(Fmt, arr),2);
{$ENDIF}
end;

function TSDController.SDEnabled():boolean;
begin
  Result:=FHandle<>INVALID_HANDLE_VALUE;
end;

function TSDController.GetPort0: byte;                // interface with device, port F762, SD Output
begin
  if SDEnabled() then
    case FParams.Scheme of
     0: Result:=FParams.SPDR or (not SD_DIN);         // N8VEM   mask to "1" all bits except SD_DIN=128=80h
     1: Result:=$FF;                                  // MSX
    end
  else
    Result:=$FF;
end;

procedure TSDController.SetPort0(const Value: byte);  // interface with device, port F762, SD input (bit->byte convertor), mode "SPI 0"
begin
  with Fparams do
  begin
    if ((Value and SD_PWR)<>(R0 and SD_PWR)) then // power off or on
    begin
      SDPowerOn;
      if Assigned(FOnAccess) then begin
        if ((Value and SD_PWR)=0) then
          FOnAccess(Self, 4, 'd')                     // display (d)own - power off
        else
          FOnAccess(Self, 4, 'u');                    // display (u)p - power on  
      end;
    end;
    case Scheme of
    0:                                                // N8VEM
     if Value<>R0 then
     begin
       PrevR0:=R0;
       R0:=Value;
       if ((R0 and SD_CLK)<>(PrevR0 and SD_CLK))and   // SD_CLK changing
          ((R0 and SD_PWR)<>0)and                     // питание на карту подано
           SDEnabled() then
       begin                                          // clock pin state changing
         if (PrevR0 and SD_CLK)=0 then
         begin                                        // фронт импульса CLK
           inc(InpBitCount);
           if (spiState<>SPI_PWR_STATE) then          // карта проинициализирована
           begin
             if ((R0 and SD_CS)<>0) then              // карта выбрана
             begin
               if (R0 and SD_DOUT)=0 then
                 spiByte:=spiByte and $FE             // защелкивание "0" по фронту (SPI 0)
               else
                 spiByte:=spiByte or 1;               // защелкивание "1" по фронту (SPI 0)
               if (spiState=SPI_IDLE_STATE) and
                  ((spiByte and $C0)=$40) then        // пришла команда
               begin
                 UpdateSpi;
                 InpBitCount:=0;
               end;
             end
             else                                     // карта не выбрана -> передача следующему в кольце (т.е. на выход)
             begin
               if (R0 and SD_DOUT)=0 then
                 SPDR:=SPDR and $FE
               else
                 SPDR:=SPDR or 1;
             end;
           end
           else UpdateSpi;                            // wait for initial 74 more clocks
         end
         else
         begin                                        // спад импульса CLK
           rl(SPDR);
           if (spiState<>SPI_PWR_STATE) and (InpBitCount=8) then
           begin
             InpBitCount:=0;                          // идем побайтно
             if ((R0 and SD_CS)<>0) then UpdateSpi;
           end;
           rl(spiByte);                               // сдвиг по спаду (SPI 0)
         end;
       end;
     end; {if}
    1: if Value<>R0 then
       begin
         PrevR0:=R0;
         R0:=Value;                                   // D1 = /CS
       end;
    end; {case}
  end; {with do}
end;

function TSDController.GetPort1: byte;                  // ld reg, (0F763h)  ;  reg <- (0F763h)
begin
 with FParams do
  case Scheme of
   0: Result:=Port0;                                    // N8VEM
   1: Result:=RDout;      //SPDR;                                     // MSX
  end;
end;

procedure TSDController.SetPort1(const Value: byte);    // ld (0F763h), reg  ;  (0F763h) <- reg
begin
  with FParams do
  case Scheme of
   0: Port0:=Value;                                     // N8VEM
   1: if ((R0 and SD_PWR)<>0) then                      // питание на карту подано
      begin                                             // MSX - и фронт и спад CLK формируются автоматически по сигналам /WR, /RD (в пределах одной команды CPU)
// фронт
        R1:=Value;
        inc(InpBitCount);
        if (spiState<>SPI_PWR_STATE) then              // карта проинициализирована
        begin
          if ((R0 and SD_CS)<>0) then                  // карта выбрана
          begin
            if ((R1 and MSXSD_WR)=0) then
              spiByte:=spiByte and $FE             // защелкивание "0"
            else
              spiByte:=spiByte or 1;               // защелкивание "1"
            if (spiState=SPI_IDLE_STATE) and
               ((spiByte and $C0)=$40) then        // пришла команда
              InpBitCount:=8;                           
          end
          else                                     // карта не выбрана -> передача следующему в кольце (т.е. на выход)
          begin
            if ((R1 and MSXSD_WR)=0) then
              SPDR:=SPDR and $FE
            else
              SPDR:=SPDR or 1;
          end;
        end
        else UpdateSpi;                            // wait for initial 74 more clocks
// спад
        rl(SPDR);
        RDout:=SPDR;
        if (spiState<>SPI_PWR_STATE) and (InpBitCount=8) then
        begin
          if ((R0 and SD_CS)<>0) then UpdateSpi;
          InpBitCount:=0;                          // идем побайтно
        end;
        rl(spiByte);                               // сдвиг по спаду (SPI 0)
      end; {case MSX}
  end; {case}
end;

procedure TSDController.SetScheme(const Value: integer);
begin
  FParams.Scheme := Value;
end;

procedure TSDController.UpdateSpi;
var
{$IFDEF SD_DEBUG}
    i, ofs: integer;
    buf: array [0..16] of byte;
{$ENDIF}
    C_Size, CSize_Mult: integer;
    CardSDRegister:TCardSDRegister;
begin
  with Fparams do
  begin
    // SPI state machine
    SPI_DEBUG('byte: %0.2x\n',[spiByte]);
    SPI_DEBUG('state: %d\n', [ord(spiState)]);
    case (spiState) of
      SPI_PWR_STATE:
        if ((R0 and SD_CS)=0) and (InpBitCount>=74) then
          spiState := SPI_IDLE_STATE;
      SPI_IDLE_STATE:
        if(spiByte = $0ff) then begin
            SPDR := $01;                         // echo back that we're ready
        end
        else begin
          spiCommand := spiByte;
          SPDR := $FF;
          spiState := SPI_ARG_X_HI;
        end;
      SPI_ARG_X_HI:
        begin
          SPI_DEBUG('x hi: %0.2X\n',[spiByte]);
          PspiArgB(@spiArg)^.spiArgXhi := spiByte;
          SPDR := $FF;
          spiState := SPI_ARG_X_LO;
        end;
      SPI_ARG_X_LO:
        begin
          SPI_DEBUG('x lo: %0.2X\n', [spiByte]);
          PspiArgB(@spiArg)^.spiArgXlo := spiByte;
          SPDR := $FF;
          spiState := SPI_ARG_Y_HI;
        end;
      SPI_ARG_Y_HI:
        begin
          SPI_DEBUG('y hi: %0.2X\n', [spiByte]);
          PspiArgB(@spiArg)^.spiArgYhi := spiByte;
          SPDR := $FF;
          spiState := SPI_ARG_Y_LO;
        end;
      SPI_ARG_Y_LO:
        begin
          SPI_DEBUG('y lo: %0.2X\n', [spiByte]);
          PspiArgB(@spiArg)^.spiArgYlo := spiByte;
          SPDR := $FF;
          spiState := SPI_ARG_CRC;
        end;
      SPI_ARG_CRC:
        begin
          SPI_DEBUG('SPI - CMD%d (%0.2X) X:%0.4X Y:%0.4X CRC: %0.2X\n',
                  [spiCommand xor $40, spiCommand, PspiArgW(@spiArg)^.spiArgX, PspiArgW(@spiArg)^.spiArgY, spiByte]);
        // ignore CRC and process commands
          case (spiCommand) of
            CMD0: begin                                 // $40 = GO_IDLE_STATE  (reset)
              SPDR := $FF;                              // does 8-bit NCR on next clock period   (8-clock wait)
              spiState := SPI_RESPOND_SINGLE;
              spiResponseBuffer[0] := $01;              // no errors, going idle
              spiResponsePtr := @spiResponseBuffer[0];
              spiResponseEnd := pointer(integer(spiResponsePtr)+1);
              spiByteCount := 0;
            end;
            CMD1: begin                                 // $41 = SEND_OP_COND  (init)
              SPDR := $FF;                              // does 8-bit NCR on next clock period  (8-clock wait)
              spiState := SPI_RESPOND_SINGLE;
              spiResponseBuffer[0] := $00;              // no error
              spiResponsePtr := @spiResponseBuffer[0];
              spiResponseEnd := pointer(integer(spiResponsePtr)+1);
              spiByteCount := 0;
            end;
            CMD17: begin                                // $51 = READ_BLOCK
              SPDR := $FF;                              // does 8-bit NCR on next clock period
              spiState := SPI_RESPOND_SINGLE;
              spiResponseBuffer[0] := $00;              // no error
              spiResponseBuffer[1] := SPI_START_TOKEN;  // start block
              spiResponsePtr := @spiResponseBuffer[0];
              spiResponseEnd := pointer(integer(spiResponsePtr)+2);
              SDSeekToOffset(spiArg, True);
              spiByteCount := sectorSize;               // SD_BLOCK_SIZE;
            end;
            CMD18: begin                                // $52 = MULTI_READ_BLOCK
              SPDR := $FF;                              // does 8-bit NCR on next clock period
              spiState := SPI_RESPOND_MULTI;
              spiResponseBuffer[0] := $00;              // no error
              spiResponseBuffer[1] := SPI_START_TOKEN;  // start block
              spiResponsePtr := @spiResponseBuffer[0];
              spiResponseEnd := pointer(integer(spiResponsePtr)+2);
              SDSeekToOffset(spiArg, true);
              spiByteCount := sectorSize;               // SD_BLOCK_SIZE;
            end;
            CMD24: begin                                // $58= WRITE_BLOCK
              SPDR := $FF;                              // does 8-bit NCR on next clock period
              spiState := SPI_WRITE_SINGLE;
              spiResponseBuffer[0] := $00;              // no error
              spiResponsePtr := @spiResponseBuffer[0];
              spiResponseEnd := pointer(integer(spiResponsePtr)+1);
              SDSeekToOffset(spiArg, False);
              spiByteCount := sectorSize;               // SD_BLOCK_SIZE;
            end;
            CMD9: begin                                 // $49 = SEND_CSD
              SPDR:=$FF;                                // does 8-bit NCR on next clock period   (8-clock wait)
              spiState := SPI_RESPOND_SINGLE;
              fillchar(spiResponseBuffer, sizeof(spiResponseBuffer), 0);
              spiResponseBuffer[0] := $00;              // R1 = no error, no idle
              spiResponseBuffer[1] := $FF;              // does 8-bit NCR on next clock period   (8-clock wait)
              spiResponseBuffer[2] := SPI_START_TOKEN;
              with PCardSDRegister(@spiResponseBuffer[3])^ do
              begin
                ZERO:=0;
                TAAC:=8;                   // D7:D0=TAAC[7:0]        time unit=0=1ns           timevalue=1=1.0
                NSAC:=1;                   // D7:D0=NSAC[7:0]        1=100 clock cycles
                TRAN_SPEED:=$32;           // is equal to 25 MHz
                CCC:=$11;                  // D7:D0=CCC[11:4]   ccc(4,8) - supported commands classes
                CCC_READ_BL_LEN:=$59;      // D7:D4=CCC[3:0]  ccc(0,2) - supported commands classes    D3:D0=READ_BL_LEN[3:0]  9=2^9=512bytes
{}              C_Size:=FMaxLBA div 16384;
                CSize_Mult:=0;
                while (C_Size<>0) do begin
                  inc(CSize_Mult);
                  C_Size:=C_Size shr 1;
                end;
                C_Size:=FMaxLBA div (1 shl (CSize_Mult+2)) - 1;
                C_Size:=C_Size shl 6;
{}              CSize_Mult:=CSize_Mult shl 7;
                BITFIELD1_CSIZE:=PspiArgB(@C_Size)^.spiArgXlo+$80;    // D7=RD_BL_PARTIAL=1  D6=WR_BL_MISALIGN=0  D5=RD_BL_MISALIGN=0  D4=DSR_IMP=0  D3:D2=reserved=00  D1:D0=CSIZE[11:10]
                CSIZE:=PspiArgB(@C_Size)^.spiArgYhi;                  // D7:D0=C_SIZE[9:2]
                CSIZE_VDD:=PspiArgB(@C_Size)^.spiArgYlo;              // D7:D6=C_SIZE[1:0]  D5:D3=VDD_R_CURR_MIN[2:0]=0=0.5mA  D2:D0=VDD_R_CURR_MAX[2:0]=0=1mA
                VDD_CSIZE_MULT:=PspiArgB(@CSize_Mult)^.spiArgYhi;     // D7:D5=VDD_W_CURR_MIN[2:0]=0=0.5mA  D4:D2=VDD_W_CURR_MAX[2:0]=0=1mA  D2:D0=C_SIZE_MULT[2:1]
                CSIZE_MULT_SECSIZE:=PspiArgB(@CSize_Mult)^.spiArgYlo; // D7=C_SIZE_MULT[0]  D6=ERASE_BLK_EN=0  D5:D0=SECTOR_SIZE[6:1]=0= 1 sector
                SECSIZE_WPGRP_SIZE:=0;                                // D7=SECTOR_SIZE[0]  D6:D0=WP_GRP_SIZE[6:0]=0= 1 sector
                WPGRP_ENA_WRBL_LEN:=2;   {000000 10}                  // D7=WP_GRP_ENABLE=0   D6:D5=reserved=00  D4:D2=R2W_FACTOR[2:0]=0  D1:D0=WRITE_BL_LEN[3:2] =10 {9} = 512bytes
                WRBL_LEN__ZERO:=$40;     {01 000000}                  // D7:D6=WRITE_BL_LEN[1:0] =01 {9} = 512bytes   D5=0  D4:D0=reserved=00000
                BITFIELD2:=$40;                                       // D7=FILE_FORMAT_GRP  D6=COPY  D5=PERM_WRITE_PROTECT  D4=TMP_WRITE_PROTECT  D3:D2=FILE_FORMAT[1:0]  D1:D0=reserved=00
                CRC:=1;                                               // D7:D1= CRC-7 checksum   D0=1
              end;
              spiResponseBuffer[20] := 1;                             // CRC=0 (0000000000000001)
              spiResponsePtr := @spiResponseBuffer[0];
              spiResponseEnd := pointer(integer(spiResponsePtr)+3+16+2);  // R1+NCR+START_TOKEN+CSD+CRC
              spiByteCount := 0;
            end;
            CMD10: begin                                // $4A = SEND_CID
              SPDR:=$FF;                                // does 8-bit NCR on next clock period   (8-clock wait)
              spiState := SPI_RESPOND_SINGLE;
              fillchar(spiResponseBuffer, sizeof(spiResponseBuffer), 0);
              spiResponseBuffer[0] := $00;              // R1 = no error, no idle
              spiResponseBuffer[1] := $FF;              // does 8-bit NCR on next clock period   (8-clock wait)
              spiResponseBuffer[2] := SPI_START_TOKEN;
              with PCardIDRegister(@spiResponseBuffer[3])^ do
              begin
                MID:=1;                                 // Manufacturer ID       [binary]
                OID:= ord('S')*256+ord('D');            // OEM/Application ID    [ACSII]
                StrPCopy(@PNM[0], 'FILE');              // Product Name          [ACSII]
                PRV:=$10;                               // Product Revision      [BCD]    "1.0"
                PSN:=$12345678;                         // Serial Number         [binary]
                MDT:=$1001;                             // Manufacturer Date     [BCD]    "Jan 2010"
                CRC:=1;                                 // D7:D1= CRC-7 checksum   D0=1
              end;
              spiResponseBuffer[20] := 1;               // CRC=0 (0000000000000001)
              spiResponsePtr := @spiResponseBuffer[0];
              spiResponseEnd := pointer(integer(spiResponsePtr)+3+16+2);  // R1+NCR+START_TOKEN+CID+CRC
              spiByteCount := 0;
            end;
            CMD58: begin                                // $7A = READ_OCR
{R3 response.  Returns OCR:}
{ R1[39:32] 32bit_OCR[31:0] }
{‘00000001’ '1 0 00000000001000 00000000 00000000'}
{‘00000001’ Ready, nonSDHC, 3.3V}
              SPDR:=$FF;                                // does 8-bit NCR on next clock period
              spiState := SPI_RESPOND_SINGLE;
              spiResponseBuffer[0] := $00;              // no errors, no idle     // $01
              spiResponseBuffer[1] := $80;
              spiResponseBuffer[2] := $08;
              spiResponseBuffer[3] := $00;
              spiResponseBuffer[4] := $00;
              spiResponsePtr := @spiResponseBuffer[0];
              spiResponseEnd := pointer(integer(spiResponsePtr)+5);
              spiByteCount := 0;
            end;
            CMD59: begin                                // $7B = CRC_ON_OFF (D0=1/0)  // in emulation CRC never calculated/checked
              SPDR := $FF;                              // does 8-bit NCR on next clock period  (8-clock wait)
              spiState := SPI_RESPOND_SINGLE;
              spiResponseBuffer[0] := $00;              // no error
              spiResponsePtr := @spiResponseBuffer[0];
              spiResponseEnd := pointer(integer(spiResponsePtr)+1);
              spiByteCount := 0;
            end;
            ACMD41: begin                               // $69 = SDC card init
              SPDR:=$FF;                                // does 8-bit NCR on next clock period
              spiState := SPI_RESPOND_SINGLE;
              spiResponseBuffer[0] := $00;              // no errors, no idle  - so, next command in ACMD chain
              spiResponsePtr := @spiResponseBuffer[0];
              spiResponseEnd := pointer(integer(spiResponsePtr)+1);
              spiByteCount := 0;
            end;
            CMD55: begin                                // $77 = APP_CMD
              SPDR := $FF;                              // does 8-bit NCR on next clock period   (8-clock wait)
              spiState := SPI_RESPOND_SINGLE;
              spiResponseBuffer[0] := $00;              // no errors, no idle  - so, next command in ACMD chain
              spiResponsePtr := @spiResponseBuffer[0];
              spiResponseEnd := pointer(integer(spiResponsePtr)+1);
              spiByteCount := 0;
            end;
            CMD16: begin                                // $50 = SET_BLOCKLEN
              SPDR := $FF;                              // does 8-bit NCR on next clock period   (8-clock wait)
              spiState := SPI_RESPOND_SINGLE;
              if PspiArgW(@spiArg)^.spiArgY>SD_BLOCK_SIZE then
                spiResponseBuffer[0] := R1_PARAM_ERR+R1_IN_IDLE             // error
              else begin
                spiResponseBuffer[0] := $00;            // no errors, no idle
                sectorSize:=PspiArgW(@spiArg)^.spiArgY;
              end;
              spiResponsePtr := @spiResponseBuffer[0];
              spiResponseEnd := pointer(integer(spiResponsePtr)+1);
              spiByteCount := 0;
            end
            else begin
              SPDR := $FF;                              // does 8-bit NCR on next clock period
              spiState := SPI_RESPOND_SINGLE;
              spiResponseBuffer[0] := R1_ILLEGAL_CMD+R1_IN_IDLE;       // illegal command
              spiResponsePtr := @spiResponseBuffer[0];
              spiResponseEnd := pointer(integer(spiResponsePtr)+1);
              spiByteCount := 0;
            end;
          end;  {case}
        end;
      SPI_RESPOND_SINGLE:
        begin
          SPDR := spiResponsePtr^;
          SPI_DEBUG('SPI - Respond: %0.2X\n', [SPDR]);
          inc(spiResponsePtr);
          if (spiResponsePtr = spiResponseEnd) then
          begin
            if (spiByteCount <> 0) then
              spiState := SPI_READ_SINGLE_BLOCK
            else
              spiState := SPI_IDLE_STATE;
          end;
        end;
      SPI_READ_SINGLE_BLOCK:
        begin
          SPDR := SDReadByte();
          dec(spiByteCount);
          if(spiByteCount = 0) then begin
            spiResponseBuffer[0] := $00; //CRC
            spiResponseBuffer[1] := $00; //CRC
            spiResponsePtr := @spiResponseBuffer[0];
            spiResponseEnd := pointer(integer(spiResponsePtr)+2);
            spiState := SPI_RESPOND_SINGLE;
          end;
        end;
      SPI_RESPOND_MULTI:
        begin
          SPDR := spiResponsePtr^;
          SPI_DEBUG('SPI - Respond: %0.2X\n', [SPDR]);
          inc(spiResponsePtr);
          if (spiResponsePtr = spiResponseEnd) then
            spiState := SPI_READ_MULTIPLE_BLOCK;
        end;
      SPI_READ_MULTIPLE_BLOCK:
        begin
          if (spiByte = $4C) then        //CMD12
          begin
            SPDR := SDReadByte();
            fillchar(spiResponseBuffer,9,$FF); // Q&D - return garbage in response to the whole command
            spiResponsePtr := @spiResponseBuffer[0];
            spiResponseEnd := pointer(integer(spiResponsePtr)+9);
            spiState := SPI_RESPOND_SINGLE;
            spiByteCount := 0;
          end
          else
          begin
            SPDR := SDReadByte();
            SPI_DEBUG('SPI - Data[%d]: %0.2X\n', [sectorSize {SD_BLOCK_SIZE} -spiByteCount, SPDR]);
            dec(spiByteCount);
            if (spiByteCount = 0) then
            begin
              spiResponseBuffer[0] := $00; //CRC
              spiResponseBuffer[1] := $00; //CRC
              spiResponseBuffer[2] := SPI_START_TOKEN; // start block
              spiResponsePtr := @spiResponseBuffer[0];
              spiResponseEnd := pointer(integer(spiResponsePtr)+3);
              spiArg:=spiArg+sectorSize;               // SD_BLOCK_SIZE;          // automatically move to next block
              SDSeekToOffset(spiArg, True);
              spiByteCount := sectorSize;              // SD_BLOCK_SIZE;
              spiState := SPI_RESPOND_MULTI;
            end;
          end;
        end;
      SPI_WRITE_SINGLE:
        begin
          SPDR := spiResponsePtr^;
          SPI_DEBUG('SPI - Respond: %0.2X\n', [SPDR]);
          if (spiResponsePtr <> spiResponseEnd) then
            inc(spiResponsePtr)
          else begin
            if spiByte<>SPI_START_TOKEN then
              spiResponsePtr^:=$FF
            else
            begin
              if (spiByteCount <> 0) then
                spiState := SPI_WRITE_SINGLE_BLOCK
              else
                spiState := SPI_IDLE_STATE;
            end;
          end;
        end;
      SPI_WRITE_SINGLE_BLOCK:
        begin
          SDWriteByte(spiByte);                                         // SPDR
          SPI_DEBUG('SPI - Data[%d]: %0.2X\n', [spiByteCount,SPDR]);
          SPDR := $FF;
          dec(spiByteCount);
          if (spiByteCount = 0) then
          begin
            spiResponseBuffer[0] := $FF; //NCR
            spiResponseBuffer[1] := WRITE_ACCEPTED;
            spiResponsePtr := @spiResponseBuffer[0];
            spiResponseEnd := pointer(integer(spiResponsePtr)+2);
            spiByteCount := 2;
            spiState := SPI_WRITE_SINGLE_CRC;
            if not SDCommit() then
              spiResponseBuffer[1]:=WRITE_DATAERROR;
          end;
        end;  
      SPI_WRITE_SINGLE_CRC:
        begin
          dec(spiByteCount);
          if (spiByteCount = 0) then                                    // just skip crc
            spiState := SPI_RESPOND_SINGLE;
        end;
    end; {case}
  end {with}
end; {update_spi}

constructor TSDController.Create;
begin
  SDPowerOn;
  SDHC:=False;
  FMaxLBA:=0;
  FOnAccess:=nil;
  FHandle:=INVALID_HANDLE_VALUE;
  Reset;
  SDCheckFBuf;
end;

destructor TSDController.Destroy;
begin
  inherited;
  if Assigned(FBuf) then
    FreeMem(FBuf);
  if FHandle<>INVALID_HANDLE_VALUE then
    CloseHandle(FHandle);
end;

procedure TSDController.ReadFromStream(Stream: TStream);
var TmpFName: ShortString;
begin
  Stream.Read(FImageRO, sizeof(FImageRO));
  Stream.Read(TmpFName, sizeof(TmpFName));
  ImageFile:=TmpFName;
  Stream.Read(FParams, sizeof(FParams));
  SDCheckFBuf();
  Stream.Read(FBuf^, SD_BLOCK_SIZE);
end;

procedure TSDController.SaveToStream(Stream: TStream);
var TmpFName: ShortString;
begin
  TmpFName:=FImgFile;
  Stream.Write(FImageRO, sizeof(FImageRO));
  Stream.Write(TmpFName, sizeof(TmpFName));
  Stream.Write(FParams, sizeof(FParams));
  Stream.Write(FBuf^, SD_BLOCK_SIZE);
end;

function TSDController.SDCommit: boolean;
var writed: cardinal;
    ptr: pointer;
begin
 with Fparams do begin
  SDCheckFBuf();
  ptr:=FBuf;
  if Assigned(FOnAccess) then FOnAccess(Self, 4, 'w');
  Result:=(WriteFile(FHandle, ptr^, sectorSize, writed, nil));
{  if not Result then
    Application.MessageBox(PChar('File write error: ' + FImgFile),
                           PChar(Application.Title),
                           MB_OK or MB_ICONEXCLAMATION);   }
  SectorIndex:=0;
 end;
end;

function TSDController.SDReadByte: byte;
begin
 Result:=0;
 with Fparams do begin
  if SectorIndex<SectorSize then begin
    Result:=byte(FBuf[SectorIndex]);
    inc(SectorIndex);
  end;
 end;
end;

procedure TSDController.SDSeekToOffset(offset: DWORD; DoRead: boolean);
var bytesread: cardinal;
    ptr: pointer;
    bb: boolean;
begin
 if FHandle=INVALID_HANDLE_VALUE then exit;
 with Fparams do begin
  if (offset>FMaxLBA*SD_BLOCK_SIZE-sectorSize) then
    raise Exception.CreateFmt('Error: offset (%d) > FileSize (%d) in file %s',
                              [offset, FMaxLBA*SD_BLOCK_SIZE, FImgFile]);
  SDCheckFBuf();
  ptr:=FBuf;
  if Assigned(FOnAccess) then FOnAccess(Self, 4, 'r');
  if SDHC then
    bb:=DiskFileSeek(FHandle, offset, FILE_BEGIN)
  else
    bb:=DiskFileSeekAbs(FHandle, offset, FILE_BEGIN);
  if not bb then
    raise Exception.CreateFmt('File seek error: %s', [FImgFile]);
  FParams.FileOffset:=offset;
  if DoRead and
     (not (ReadFile(FHandle, ptr^, sectorSize, bytesread, nil))) then
    raise Exception.CreateFmt('File read error: %s', [FImgFile]);
  SectorIndex:=0;
 end;
end;

procedure TSDController.SDWriteByte(value: byte);
begin
 with Fparams do
   if SectorIndex<SectorSize then begin
     SDCheckFBuf();
     FBuf[SectorIndex]:=char(value);
     inc(SectorIndex);
   end;
end;

procedure TSDController.SetImageFile(const Value: String);
var cDrive: char;
    Geometry: TDISKGEOMETRY;
    phis, IdeOK: boolean;
    dwSizeLow, dwSizeHigh: DWORD;
    FDiskSize, FFreeSize: int64;
begin
  if FImgFile<>trim(Value) then
  begin
    FImgFile:=trim(Value);
    if FImgFile='' then begin
      if FHandle<>INVALID_HANDLE_VALUE then
        CloseHandle(FHandle);
      FHandle:=INVALID_HANDLE_VALUE;
    end
    else
    begin
      if FHandle<>INVALID_HANDLE_VALUE then
        CloseHandle(FHandle);
      FHandle:=INVALID_HANDLE_VALUE;
      if IsDrive(FImgFile, @cDrive) then
      begin
        IdeOK:=(cDrive in ['0'..'9'])and
               GetIdeDiskIdentify(cDrive, @FIdBuf[0]);
        if not HDDOpen(cDrive, (cDrive='0') or ImageRO {18.06.2012 was:True}, FHandle, phis, FDiskSize, FFreeSize, @Geometry) then       // ReadOnly Allways
          raise Exception.Create(LastError);
      end
      else
      begin
        if not FileExists(FImgFile) then
          raise Exception.CreateFmt('File not found: %s', [FImgFile])
        else
        begin
          if FImageRO then
            FHandle:=CreateFile(PChar(FImgFile), GENERIC_READ, 0, nil, OPEN_EXISTING, 0, 0)
          else
            FHandle:=CreateFile(PChar(FImgFile), GENERIC_READ or GENERIC_WRITE, 0, nil, OPEN_EXISTING, 0, 0);
          if (FHandle=INVALID_HANDLE_VALUE) then
            raise Exception.Create(LastError);
        end;
        dwSizeLow := GetFileSize(FHandle, @dwSizeHigh);
        if (dwSizeLow = $FFFFFFFF) and (GetLastError() <> NO_ERROR ) then
            raise Exception.Create(LastError);
        FDiskSize:=dwSizeHigh;
        FDiskSize:=(FDiskSize shl 32) + dwSizeLow;
      end;
      FMaxLBA:=FDiskSize div SD_BLOCK_SIZE;     // sectorSize
      if FMaxLBA>=2097152 then begin		// TODO: init SDHC value
         FMaxLBA:=2097152-1;
         Application.MessageBox('Only non-SDHC cards emulated!'#13#10#13#10'Image file will be processed in beginner 1G only.',
                                PChar(Application.Title),
                                MB_OK);
      end;
    end;
  end;
end;

procedure TSDController.SetImageRO(const Value: boolean);
var str: string;
begin
  if FImageRO<>Value then with Fparams do
  begin
    FImageRO:=Value;
    str:=FImgFile;
    SetImageFile('');
    SetImageFile(str);
    SDSeekToOffset(FileOffset, True);
  end;
end;

procedure TSDController.SetOnAccess(const Value: TSdAccess);
begin
  FOnAccess := Value;
end;

procedure TSDController.rl(var bb: byte);       // сдвиг влево с циклическим переносом
begin
  if (bb and $80)=0 then
    bb:=(bb shl 1)
  else
    bb:=(bb shl 1) or 1;
end;

procedure TSDController.SDCheckFBuf;
begin
  if not Assigned(FBuf) then
     GetMem(FBuf, SD_BLOCK_SIZE+1);  // sectorSize+1
end;

procedure TSDController.Reset;
begin
  FParams.R0:=0;                     // MSX feature    $FF
end;

procedure TSDController.SDPowerOn;
begin
  with Fparams do
  begin
    SPDR:=$FF;
    RDout:=$FF;
    spiByte:=$FF;
    spiState:=SPI_PWR_STATE;
    InpBitCount:=0;
    sectorSize:=SD_BLOCK_SIZE;
    SectorIndex:=0;
    FileOffset:=0;
  end;
end;

initialization
  SdController:=TSdController.Create;

finalization
  SdController.Free;

end.


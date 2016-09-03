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


unit mod8019as;

{***********************************************

    RTL8019AS (Ethernet MAC+PHY) emulation

    simplefied version (no ints, no loopback)

 ***********************************************}

interface

{$I 'OrionZEm.inc'}

Uses Windows, SysUtils, classes, EthThrd;

const
  MEM_PAGE_SIZE = 256;
  MEM_PAGE_MAX  = $40;
  MEM_TOT_SIZE  = MEM_PAGE_SIZE*MEM_PAGE_MAX;
  ETHERDEV_RESET= $FD;                                  // also hardware reset if $FD writing to CR register (0F770h)

type
  TWORD2BYTE = packed record
    low: BYTE;
    hig: BYTE
  end;
  PWORD2BYTE = ^TWORD2BYTE;

  TNe2kReg = packed record
    CR:    BYTE;
    RPAGE: integer;
{}
    CLDA:  WORD;
    PSTART:WORD;
    PSTOP: WORD;
    BNRY:  WORD;        // Boundary Pointer, points to the next frame to be unloaded from the Buffer Ring (to client)
    TSR:   BYTE;
    TPSR:  WORD;
    NCR:   BYTE;
    TBC:   WORD;
    FIFO:  BYTE;
    ISR:   BYTE;
    CRDA:  WORD;
    RSA:  WORD;
    RBC:   WORD;
    RSR:   BYTE;
    RCR:   BYTE;
    CNTR0: BYTE;
    TCR:   BYTE;
    CNTR1: BYTE;
    DCR:   BYTE;
    CNTR2: BYTE;
    IMR:   BYTE;
{}
    PAR0:  BYTE;
    PAR1:  BYTE;
    PAR2:  BYTE;
    PAR3:  BYTE;
    PAR4:  BYTE;
    PAR5:  BYTE;
    CURR:  WORD;        // Current Page Pointer, points to the next available buffer area for the next incoming frame
    MAR0:  BYTE;
    MAR1:  BYTE;
    MAR2:  BYTE;
    MAR3:  BYTE;
    MAR4:  BYTE;
    MAR5:  BYTE;
    MAR6:  BYTE;
    MAR7:  BYTE;
{}
    FReadDMABufAddr: integer;
    FWriteDMABufAddr: integer;
    FDMAWordSize: integer;
  end;

  T8019Reg = packed record
    ID8019: WORD;               // Page0_R
    CR9346: BYTE;               // Page2
    BPAGE:  BYTE;
    CONFIG0:BYTE;
    CONFIG1:BYTE;
    CONFIG2:BYTE;
    CONFIG3:BYTE;
    TEST:   BYTE;
    CSNSAV: BYTE;
    HLTCLK: BYTE;
    INTR:   BYTE;
    FMWP:   BYTE;
    CONFIG4:BYTE;
  end;

  TNE2kDevice = class(TObject)
    RR: TNe2kReg;                                                                // internal registers: ne2000 standard
    FEthThread: TEthThread;
    FTmpDMABuf: TFrame;
    FReadDMABuf: TFrame;
    FWriteDMABuf: TFrame;
    FBuffer: array[0..MEM_TOT_SIZE] of BYTE;                                     // internal 16384 bytes buffer
    function GetData(Index: Integer): byte; virtual;                             // emulate CPU reading from internal registers
    procedure SetData(Index: Integer; const Value: byte); virtual;               // emulate CPU writing to internal registers
  private
    procedure StartOperation(CR:byte);
    procedure StopOperation(CR:byte);
    procedure CheckIncomingFrame;
    function GetNextBuffPage: pointer;
  public
    constructor Create; virtual;
    destructor Destroy; virtual;
    procedure Reset; virtual;                                                    // hardware reset (pin reset)
    procedure SaveToStream(Stream: TStream); virtual;
    procedure ReadFromStream(Stream: TStream); virtual;
    property EthThread: TEthThread read FEthThread write FEthThread;
    property Registers[Index: Integer]:byte read GetData write SetData; default; // interface with CPU
  end;

  T8019AS = class(TNE2kDevice)
    RRR: T8019Reg;                                                               // internal registers: rtl8019 specific
  private
    function GetData(Index: Integer): byte; override;
    procedure SetData(Index: Integer; const Value: byte); override;
  public
    constructor Create; override;
    procedure Reset; override;
    procedure SaveToStream(Stream: TStream); override;
    procedure ReadFromStream(Stream: TStream); override;
  end;

var
  F8019AS: T8019AS;

implementation

const
// NIC Page 0 read register assignments

  CR	= $00; // Command
  CLDA0	= $01; // Current Local DMA Address 0
  CLDA1	= $02; // Current Local DMA Address 1
  BNDRY	= $03; // Boundary Pointer
  TSR	= $04; // Transmit Status Register
  NCR	= $05; // Number of Collisions Register
  FIFO	= $06; // FIFO
  ISR	= $07; // Interupt Status Register
  CRDA0	= $08; // Current Remote DMA Address 0
  CRDA1	= $09; // Current Remote DMA Address 1
  RES1	= $0A; // Reserved
  RES2	= $0B; // Reserved
  RSR	= $0C; // Receive Status Register
  CNTR0	= $0D; // Tally Counter 0 (Frame Alignment Errors)
  CNTR1	= $0E; // Tally Counter 1 (CRC Errors)
  CNTR2	= $0F; // Tally Counter 2 (Missed Packet Errors)

// NIC Page 0 write register assignments

  PSTART= $01; // Page Start Register
  PSTOP	= $02; // Page Stop Register
  TPSR	= $04; // Transmit Page Start Address
  TBCR0	= $05; // Transmit Byte Count Register 0
  TBCR1	= $06; // Transmit Byte Count Register 1
  RSAR0	= $08; // Remote Start Address Register 0
  RSAR1	= $09; // Remote Start Address Register 1
  RBCR0	= $0A; // Remote Byte Count Register 0
  RBCR1	= $0B; // Remote Byte Count Register 1
  RCR	= $0C; // Receive Configuration Register
  TCR	= $0D; // Transmit Configuration Register
  DCR	= $0E; // Data Configuration Register
  IMR	= $0F; // Interrupt Mask Register

// NIC Page 1 register assignments     (R/W)

  PAR0	= $01; // Physical Address Register 0           [R] PSTART
  PAR1	= $02; // Physical Address Register 1           [R] PSTOP
  PAR2	= $03; // Physical Address Register 2
  PAR3	= $04; // Physical Address Register 3           [R] TPSR
  PAR4	= $05; // Physical Address Register 4
  PAR5	= $06; // Physical Address Register 5
  CURR	= $07; // Current Page Register
  MAR0	= $08; // Multicast Address Register 0
  MAR1	= $09; // Multicast Address Register 1
  MAR2	= $0A; // Multicast Address Register 2
  MAR3	= $0B; // Multicast Address Register 3
  MAR4	= $0C; // Multicast Address Register 4          [R] RCR
  MAR5	= $0D; // Multicast Address Register 5          [R] TCR
  MAR6	= $0E; // Multicast Address Register 6          [R] DCR
  MAR7	= $0F; // Multicast Address Register 7          [R] IMR

// NIC Page 3 (rtl8019 specific)
  CR9346  = $01;
  BPAGE   = $02;
  CONFIG0 = $03;
  CONFIG1 = $04;
  CONFIG2 = $05;
  CONFIG3 = $06;
  TEST    = $07;
  CSNSAV  = $08;
  HLTCLK  = $09;
  INTR    = $0B;
  FMWP    = $0C;
  CONFIG4 = $0D;

// NIC Other registers

  NIC_DATA  = $10; // Data Register for I/O port mode
  NIC_RESET = $18; // Reset Register                    - not implemented, use hardware reset only

  PAGE0_R   = 0;
  PAGE0_W   = 1;
  PAGE1_RW  = 2;
  PAGE2_R   = 3;
  PAGE3_RW  = 4;

// RTL8019/NE2000 CR Register Bit Definitions
  PS1        = $80;
  PS0        = $40;
  RD2        = $20;
  RD1        = $10;
  RD0        = $08;
  TXP        = $04;
  STA        = $02;     // START
  STP        = $01;     // STOP
// RTL8019/NE2000 ISR Register Bit Definitions
  RST        = $80;
  RDC        = $40;
  CNT        = $20;
  OVW        = $10;
  RXE        = $08;
  TXE        = $04;
  PTX        = $02;
  TPTX       = $01;             // at TSR register
//  PRX        = $01;           // see RSR bit0
// RTL8019/NE2000 DCR Register Bit Definitions
  FT1        = $40;
  FT0        = $20;
  ARM        = $10;
  LS         = $08;
  LAS        = $04;
  BOS        = $02;
  WTS        = $01;
// RTL8019/NE2000 RCR Register Bit Definitions
  MON        = $20;
  PRO        = $10;
  AM         = $08;
  AB         = $04;
  AR         = $02;
  SEP        = $01;
// RTL8019/NE2000 RSR Register Bit Definitions
  DFR        = $80;
  DIS        = $40;
  PHY        = $20;
  MPA        = $10;
  FAE        = $04;
  CRC        = $02;
  PRX        = $01;
// RTL8019/NE2000 TCR Register Bit Definitions
  FDU        = $80;    // full duplex
  PD         = $40;    // pad disable
  RLO        = $20;    // retry of late collisions
  LB1        = $04;    // loopback 1
  LB0        = $02;    // loopback 0
  CRCG       = $01;    // generate CRC
// RTL8019 EECR Register Bit Definitions
  EEM1       = $80;
  EEM0       = $40;
  EECS       = $08;
  EESK       = $04;
  EEDI       = $02;
  EEDO       = $01;

function min(a,b:integer):integer; begin if a<b then result:=a else result:=b; end;

{ T8019AS }

constructor TNE2kDevice.Create;
begin
  inherited;
  FEthThread:=nil;
  Reset;
end;

destructor TNE2kDevice.Destroy;
begin
  inherited;
  if Assigned(FEthThread) then
    FEthThread.Stop;
end;

procedure TNE2kDevice.Reset;       // Initialize all registers and ports: PowerUP or reset
begin
  inherited;
  FillChar(RR, sizeof(RR), 0);
  RR.RPage:=0;
  RR.CR:=(RD2 or STP);
  RR.DCR:=LAS;
  RR.FDMAWordSize:=1;
  RR.TCR:=$E0 or LB0;
  RR.TSR:=$22;
  RR.RCR:=$C0 or AB;
  RR.RSR:=$08 or PRX;
  RR.ISR:=RR.ISR or RST;
  if Assigned(FEthThread) then
  begin
    FEthThread.Reset;
    FEthThread.BufSize:=MEM_TOT_SIZE;
//    FEthThread.Start;
  end;  
end;

function TNE2kDevice.GetData(Index: Integer): byte;
begin
  Result:=$FF;
  if Index=CR then
    Result:=RR.CR
  else
  if Index=NIC_DATA then
  begin
    RR.ISR:=RR.ISR and (not RST);
    if (RR.FReadDMABufAddr<sizeof(FReadDMABuf)) then
    begin
      Result:=FReadDMABuf[RR.FReadDMABufAddr];
      RR.FReadDMABufAddr:=RR.FReadDMABufAddr+RR.FDMAWordSize;                     // in WordWidth (16bit bus) mode odd bytes are skipped
    end;
  end
  else
    case RR.RPage of
      0: case Index of
          $01: Result:=lo(RR.CLDA); 	// Current Local DMA Address 0              CLDA0  PAR0  PSTART  CR9346
          $02: Result:=hi(RR.CLDA); 	// Current Local DMA Address 1              CLDA1  PAR1  PSTOP   BPAGE
          $03: Result:=hi(RR.BNRY); 	// Boundary Pointer                         BNDRY  PAR2          CONFIG0
          $04: Result:=RR.TSR;   	// Transmit Status Register                 TSR    PAR3   TPSR   CONFIG1
          $05: Result:=RR.NCR; 	        // Number of Collisions Register            NCR    PAR4          CONFIG2
          $06: Result:=RR.FIFO; 	// FIFO                                     FIFO   PAR5          CONFIG3
          $07: begin
                 CheckIncomingFrame;
                 Result:=RR.ISR; 	// Interupt Status Register                 ISR    CURR
               end;
          $08: Result:=lo(RR.CRDA); 	// Current Remote DMA Address 0             CRDA0  MAR0          CSNSAV
          $09: Result:=hi(RR.CRDA); 	// Current Remote DMA Address 1             CRDA1  MAR1
          $0C: begin
                 CheckIncomingFrame;
                 Result:=RR.RSR;   	// Receive Status Register                  RSR    MAR4   RCR
               end;
          $0D: Result:=RR.CNTR0; 	// Tally Counter 0 (Frame Alignment Errors) CNTR0  MAR5   TCT    CONFIG4
          $0E: Result:=RR.CNTR1; 	// Tally Counter 1 (CRC Errors)             CNTR1  MAR6   DCR
          $0F: Result:=RR.CNTR2; 	// Tally Counter 2 (Missed Packet Errors)   CNTR2  MAR7   IMR
         end;
      1:begin
         if (index<$07) and Assigned(FEthThread) then
           CopyMemory(pointer(@RR.PAR0), FEthThread.MACAddr, 6);
         case Index of
          $01: Result:=RR.PAR0; 	// Physical Address Register 0              CLDA0  PAR0  PSTART  CR9346
          $02: Result:=RR.PAR1; 	// Physical Address Register 1              CLDA1  PAR1  PSTOP   BPAGE
          $03: Result:=RR.PAR2; 	// Physical Address Register 2              BNDRY  PAR2          CONFIG0
          $04: Result:=RR.PAR3;   	// Physical Address Register 3              TSR    PAR3   TPSR   CONFIG1
          $05: Result:=RR.PAR4;	        // Physical Address Register 4              NCR    PAR4          CONFIG2
          $06: Result:=RR.PAR5; 	// Physical Address Register 5              FIFO   PAR5          CONFIG3
          $07: Result:=hi(RR.CURR);     // Current Page Register                    ISR    CURR
          $08: Result:=RR.MAR0; 	// Multicast Address Register 0             CRDA0  MAR0          CSNSAV
          $09: Result:=RR.MAR1; 	// Multicast Address Register 1             CRDA1  MAR1
          $0A: Result:=RR.MAR2; 	// Multicast Address Register 2             RES1   MAR2
          $0B: Result:=RR.MAR3; 	// Multicast Address Register 3             RES2   MAR3          INTR
          $0C: Result:=RR.MAR4;   	// Multicast Address Register 4             RSR    MAR4   RCR
          $0D: Result:=RR.MAR5; 	// Multicast Address Register 5             CNTR0  MAR5   TCT    CONFIG4
          $0E: Result:=RR.MAR6; 	// Multicast Address Register 6             CNTR1  MAR6   DCR
          $0F: Result:=RR.MAR7; 	// Multicast Address Register 7             CNTR2  MAR7   IMR
         end;
        end;
      2: case Index of
          $01: Result:=RR.PSTART; 	//             CLDA0  PAR0  PSTART  CR9346
          $02: Result:=RR.PSTOP; 	//             CLDA1  PAR1  PSTOP   BPAGE
          $04: Result:=RR.TPSR;   	//             TSR    PAR3   TPSR   CONFIG1
          $0C: Result:=RR.RCR;   	//             RSR    MAR4   RCR
          $0D: Result:=RR.TCR;   	//             CNTR0  MAR5   TCT    CONFIG4
          $0E: Result:=RR.DCR;   	//             CNTR1  MAR6   DCR
          $0F: Result:=RR.IMR;   	//             CNTR2  MAR7   IMR
         end;
    end;
end;

procedure TNE2kDevice.SetData(Index: Integer; const Value: byte);
var tps, ii: integer;
 function MemAddr(bb: WORD):WORD;
 begin
   if bb>MEM_PAGE_MAX then
     bb:=bb and (MEM_PAGE_MAX-1);             { do boundary check at FBuffer access }
   Result:=bb shl 8;
 end;
begin
  if Index=CR then
  begin
    RR.CR:=Value or (RR.CR and TXP);
    RR.RPage:=Value shr 6;
    case Value and 3 of
      2: StartOperation(Value);
      1: StopOperation(Value);
    end;
  end
  else if Index=NIC_DATA then
  begin
    if (RR.CR and (RD1 or STA))<>0 then
    begin
      if RR.FWriteDMABufAddr=0 then
        RR.ISR:=RR.ISR and (not RDC);
      if (RR.FWriteDMABufAddr<sizeof(FWriteDMABuf))and((RR.ISR and RDC)=0) then
      begin
        FWriteDMABuf[RR.FWriteDMABufAddr]:=Value;
        RR.FWriteDMABufAddr:=RR.FWriteDMABufAddr+RR.FDMAWordSize;                // in WordWidth (16bit bus) mode odd bytes are skipped
        if (RR.FWriteDMABufAddr>=RR.RBC) then                                    // transfer from DMA to send buffer
        begin
          tps:=RR.TPSR mod MEM_TOT_SIZE;
          ii:=min(RR.FWriteDMABufAddr, RR.RBC);
          CopyMemory(@FBuffer[tps], @FWriteDMABuf[0], min(ii, MEM_TOT_SIZE-tps));
          if ii > MEM_TOT_SIZE-tps then
            CopyMemory(@FBuffer[0], @FWriteDMABuf[MEM_TOT_SIZE-tps], ii-(MEM_TOT_SIZE-tps));
          RR.ISR:=RR.ISR or RDC;
        end;
      end;
    end;
  end
  else
    case RR.RPage of
      0: case Index of
          $01: RR.PSTART:=MemAddr(Value);                 // Current Local DMA Address beg            PSTART  PAR0          CR9346
          $02: RR.PSTOP:=MemAddr(Value);	          // Current Local DMA Address end            PSTOP   PAR1          BPAGE
          $03: RR.BNRY:=MemAddr(Value);   	          // Boundary Pointer                         BNDRY   PAR2
          $04: RR.TPSR:=MemAddr(Value); 	          // Transmit Page Start Register             TPSR    PAR3          CONFIG1
          $05: PWORD2BYTE(pointer(@RR.TBC))^.low:=Value;  // Number of Collisions Register            TBCR0   PAR4          CONFIG2
          $06: PWORD2BYTE(pointer(@RR.TBC))^.hig:=Value;  //                                          TBCR1   PAR5          CONFIG3
          $07: begin
                 RR.ISR:=RR.ISR and (not Value);          // Interupt Status Register                 ISR     CURR          TEST
               end;
          $08: PWORD2BYTE(pointer(@RR.RSA))^.low:=Value;  // Current Remote DMA Address 0             RSAR0   MAR0
          $09: PWORD2BYTE(pointer(@RR.RSA))^.hig:=Value;  // Current Remote DMA Address 1             RSAR1   MAR1          HLTCLK
          $0A: PWORD2BYTE(pointer(@RR.RBC))^.low:=Value;  // Reserved                                 RBCR0   MAR2
          $0B: PWORD2BYTE(pointer(@RR.RBC))^.hig:=Value;  // Reserved                                 RBCR1   MAR3
          $0C: begin
                 RR.RCR:=Value;	                          // Receive Configuration Register           RCR     MAR4          FMWP
                 if Assigned(FEthThread) then
                 begin
                   FEthThread.Promiscuous:=(RR.RCR and PRO)<>0;
                   FEthThread.AcceptMulticast:=(RR.RCR and AM)<>0;
                   FEthThread.AcceptBroadcast:=(RR.RCR and AB)<>0;
                   FEthThread.AcceptRunt:=(RR.RCR and AR)<>0;
                 end;
               end;
          $0D: RR.TCR:=Value;	                          // Transmit configuration register          TCR     MAR5
          $0E: begin
                 RR.DCR:=Value;	                          // Data Configuration Register              DCR     MAR6
                 if (RR.DCR and WTS)=0 then
                   RR.FDMAWordSize:=1
                 else
                   RR.FDMAWordSize:=2;
               end;
          $0F: RR.IMR:=Value;	                          // Interrupt Mask Register                  IMR     MAR7
         end;
      1:begin
         case Index of
          $01: RR.PAR0:=Value; 	                // Physical Address Register 0              CLDA0  PAR0  PSTART  CR9346
          $02: RR.PAR1:=Value; 	                // Physical Address Register 1              CLDA1  PAR1  PSTOP   BPAGE
          $03: RR.PAR2:=Value; 	                // Physical Address Register 2              BNDRY  PAR2          CONFIG0
          $04: RR.PAR3:=Value;   	        // Physical Address Register 3              TSR    PAR3   TPSR   CONFIG1
          $05: RR.PAR4:=Value;	                // Physical Address Register 4              NCR    PAR4          CONFIG2
          $06: RR.PAR5:=Value; 	                // Physical Address Register 5              FIFO   PAR5          CONFIG3
          $07: RR.CURR:=MemAddr(Value);         // Current Page Register                    ISR    CURR
          $08: RR.MAR0:=Value; 	                // Multicast Address Register 0             CRDA0  MAR0          CSNSAV
          $09: RR.MAR1:=Value; 	                // Multicast Address Register 1             CRDA1  MAR1
          $0A: RR.MAR2:=Value; 	                // Multicast Address Register 2             RES1   MAR2
          $0B: RR.MAR3:=Value; 	                // Multicast Address Register 3             RES2   MAR3          INTR
          $0C: RR.MAR4:=Value;   	        // Multicast Address Register 4             RSR    MAR4   RCR
          $0D: RR.MAR5:=Value; 	                // Multicast Address Register 5             CNTR0  MAR5   TCT    CONFIG4
          $0E: RR.MAR6:=Value; 	                // Multicast Address Register 6             CNTR1  MAR6   DCR
          $0F: RR.MAR7:=Value; 	                // Multicast Address Register 7             CNTR2  MAR7   IMR
         end;
         if (index<$07) and Assigned(FEthThread) then
           FEthThread.MACAddr:=pointer(@RR.PAR0);
        end;
    end;
end;

procedure TNE2kDevice.ReadFromStream(Stream: TStream);
begin
  inherited;
  Stream.Read(RR, sizeof(RR));
end;

procedure TNE2kDevice.SaveToStream(Stream: TStream);
begin
  inherited;
  Stream.Write(RR, sizeof(RR));
end;

procedure TNE2kDevice.StartOperation(CR: byte);
var pc: PChar;
    sz: integer;
begin
  CheckIncomingFrame;
  RR.ISR:=RR.ISR and (not RST);
  if (CR and RD0)<>0 then                                                        // remote read
  begin
    if (CR and STA)<>0 then
    begin
      RR.FReadDMABufAddr:=0;
      pc:=@FBuffer[RR.RSA mod MEM_TOT_SIZE];
      sz:=min(sizeof(FReadDMABuf)-1, RR.RBC);
      if (RR.PSTOP>RR.RSA) then                                                  // do not read out of buffer
      begin
        CopyMemory(@FReadDMABuf[0], pc, min(sz, RR.PSTOP-RR.RSA));
        if sz>(RR.PSTOP-RR.RSA) then
          CopyMemory(@FReadDMABuf[RR.PSTOP-RR.RSA], @FBuffer[RR.PSTART mod MEM_TOT_SIZE], sz-(RR.PSTOP-RR.RSA));
        RR.ISR:=RR.ISR or RDC;
      end;
    end;
  end
  else if (CR and RD1)<>0 then                                                   // remote write
  begin
    if (CR and STA)<>0 then
      RR.FWriteDMABufAddr:=0;
  end
  else if (CR and STA)<>0 then                                                   // complete DMA operation or transmit
  begin
    if ((CR and RD2)<>0) then
      RR.FReadDMABufAddr:=0;
    if (CR and TXP)<>0 then                                                      // send
    begin
//      pc:=@FBuffer[RR.TPSR mod MEM_TOT_SIZE];
      if Assigned(FEthThread) and
        FEthThread.PutPacket({pc^}FWriteDMABuf, min(RR.TBC, sizeof(TFrame))) then              // actually send packet
      begin
        RR.ISR:=RR.ISR or PTX;
        RR.TSR:=RR.TSR or TPTX;
      end
      else
      begin
        RR.ISR:=RR.ISR and (not PTX);
        RR.TSR:=RR.TSR and (not TPTX);
      end;
      RR.CR:=RR.CR and (not TXP);
    end;
    RR.ISR:=RR.ISR or RDC;
  end;
end;

procedure TNE2kDevice.StopOperation(CR: byte);
begin
//  RR.ISR:=RR.ISR and (not RST);
end;

procedure TNE2kDevice.CheckIncomingFrame;
var
  len, adr: integer;
  RRCURR: WORD;
  pb, pc: PBYTE;
 procedure IncCntr(var Cntr:byte); begin if Cntr=255 then RR.ISR:=RR.ISR or CNT; Cntr:=Cntr+1; end;
begin
  if Assigned(FEthThread) then
  with FEthThread do
   begin
     Start;
     pc:=nil;
     adr:=0;
     RRCURR:=RR.CURR;
     len:=GetPacket(FTmpDMABuf);
     if len>0 then begin
                                                  // TODO: check frame CRC here, setup RSR^CRC, RR.CNTR1, ISR^RXE
       if IsPhysical(@FTmpDMABuf[0]) then
         RR.RSR:=RR.RSR and (not PHY)
       else
         RR.RSR:=RR.RSR or PHY;
       if (RR.RCR and MON)<>0 then
         RR.RSR:=RR.RSR or DIS
       else
       begin                                      // write to receive buffer
         RR.RSR:=RR.RSR and (not DIS);
         pb:=GetNextBuffPage;                     // get address and move buffer pointer (RR.CURR)
         if Assigned(pb) then
         begin                                    // When all the bytes are loaded,
           pc:=pb;
           inc(pb);                               // the RSR (Receive Status Register) status,
           inc(pb);                               // a pointer to the next frame
           PWORD(pointer(pb))^:=WORD(len+4);
           inc(pb);                               // and the byte count of the current frame are written into the 4-byte offset.
           inc(pb);
           CopyMemory(pb, @FTmpDMABuf[adr], MEM_PAGE_SIZE-4);
         end
         else
           len:=-1;
         adr:=adr+MEM_PAGE_SIZE-4;
         while adr<len do begin
           pb:=GetNextBuffPage;                      // get address and move buffer pointer (RR.CURR)
           if Assigned(pb) then
             CopyMemory(pb, @FTmpDMABuf[adr], MEM_PAGE_SIZE)
           else
             len:=-1;
           adr:=adr+MEM_PAGE_SIZE;
         end;
       end;
     end;
     if len=-1 then begin                            // Missed Packet because a lack of receive buffer
       RR.RSR:=RR.RSR or MPA;
       IncCntr(RR.CNTR2);
       RR.ISR:=RR.ISR or OVW;
       RR.ISR:=RR.ISR or RST;
       RR.CURR:=RRCURR;                              // restore buffer pointer (packet not stored partially)
     end
     else
       RR.RSR:=RR.RSR and (not MPA);
     if (RR.RSR and ({MPA or} FAE or CRC)<>0) then   // are some errors?  - ToVerify: MPA bit processing = how to ???
     begin
       RR.RSR:=RR.RSR and (not PRX);                 // reseived with errors
       RR.ISR:=RR.ISR and (not PRX);
     end
     else
     begin
       RR.RSR:=RR.RSR or PRX;                        // reseived with no errors
       if len>0 then
         RR.ISR:=RR.ISR or PRX;
     end;
     if Assigned(pc) then
     begin
       pc^:=RR.RSR;
       inc(pc);
       pc^:=hi(RR.CURR);
     end;
   end;
end;

function TNE2kDevice.GetNextBuffPage: pointer;
 function IncCURR(CUR:WORD):WORD;
 begin
  Result:=CUR+MEM_PAGE_SIZE;
  if Result>=RR.PSTOP then
    Result:=RR.PSTART;
 end;
begin
  Result:=nil;
  if hi(IncCURR(RR.CURR))=hi(RR.BNRY) then
    exit;
  Result:=@FBuffer[RR.CURR mod MEM_TOT_SIZE];
  RR.CURR:=IncCURR(RR.CURR);
end;

{ T8019AS }

constructor T8019AS.Create;
begin
  inherited;
end;

procedure T8019AS.Reset;
begin
  inherited;
  FillChar(RRR, sizeof(RRR), 0);
  RRR.ID8019:=$7050;
  RRR.CONFIG1:=$80;
  RRR.CONFIG3:=$01;
  RRR.HLTCLK:=$FF;
end;

procedure T8019AS.ReadFromStream(Stream: TStream);
begin
  inherited;
  Stream.Read(RRR, sizeof(RRR));
end;

procedure T8019AS.SaveToStream(Stream: TStream);
begin
  inherited;
  Stream.Write(RRR, sizeof(RRR));
end;

function T8019AS.GetData(Index: Integer): byte;
begin
  if (RR.RPAGE=0) and (Index=$0A) then
    Result:=lo(RRR.ID8019)               	// Reserved                                 RES1   MAR2
  else if (RR.RPAGE=0) and (Index=$0B) then
    Result:=hi(RRR.ID8019) 	                // Reserved                                 RES2   MAR3          INTR
  else if (RR.RPAGE=3) and (Index>0) then
    case Index of
      $01: Result:=RRR.CR9346; 	                //             CLDA0  PAR0  PSTART  CR9346
      $02: Result:=RRR.BPAGE; 	                //             CLDA1  PAR1  PSTOP   BPAGE
      $03: Result:=RRR.CONFIG0; 	        //             BNDRY  PAR2          CONFIG0
      $04: Result:=RRR.CONFIG1;   	        //             TSR    PAR3   TPSR   CONFIG1
      $05: Result:=RRR.CONFIG2;                 //             NCR    PAR4          CONFIG2
      $06: Result:=RRR.CONFIG3; 	        //             FIFO   PAR5          CONFIG3
      $07: Result:=RRR.TEST;                    //             ISR    CURR
      $08: Result:=RRR.CSNSAV; 	                //             CRDA0  MAR0          CSNSAV
      $0B: Result:=RRR.INTR; 	                //             RES2   MAR3          INTR
      $0D: Result:=RRR.CONFIG4; 	        //             CNTR0  MAR5   TCT    CONFIG4
    end
  else Result:=inherited GetData(Index);
end;

procedure T8019AS.SetData(Index: Integer; const Value: byte);
begin
  if (RR.RPAGE=3) and (Index>0) then
    case Index of
      $01: RRR.CR9346:=(RRR.CR9346 and 1)or(Value and $FE); 	        //             CLDA0  PAR0  PSTART  CR9346
      $02: RRR.BPAGE  := Value;                                         //             CLDA1  PAR1  PSTOP   BPAGE
      $04: if (RRR.CR9346 and $C0)=$C0 then
             RRR.CONFIG1:=(RRR.CONFIG1 and $7F)or(Value and $80);   	//             TSR    PAR3   TPSR   CONFIG1
      $05: if (RRR.CR9346 and $C0)=$C0 then
             RRR.CONFIG2:=(RRR.CONFIG2 and $1F)or(Value and $E0);   	//             TSR    PAR3   TPSR   CONFIG2
      $06: if (RRR.CR9346 and $C0)=$C0 then
             RRR.CONFIG3:=(RRR.CONFIG3 and $06)or(Value and $F9);   	//             TSR    PAR3   TPSR   CONFIG3
      $07: RRR.TEST   := Value;                                         //             ISR    CURR
      $09: RRR.HLTCLK := Value;	                                        //             CRDA0  MAR0          HLTCLK
      $0C: if (RRR.CR9346 and $C0)=$C0 then
             RRR.FMWP := Value;	                                        //             RES2   MAR3          FMWP
    end
  else inherited SetData(Index, Value);
end;

initialization
  F8019AS:=nil;

finalization
  if Assigned(F8019AS) then F8019AS.Free;

end.


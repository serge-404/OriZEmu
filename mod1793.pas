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

    WD1793 (1818ВГ93) emulation

 ***********************************************}

unit mod1793;

interface

Uses Windows, SysUtils, classes;


{$I 'OrionZEm.inc'}


const
  FDC_ADDR1 = $F700;
  FDC_ADDR2 = $F710;
  RGU_ADDR1 = $F720;
  RGU_ADDR2 = $F714;
  SECTOR_SIZE = 1024;   // only 1024 implemented
  ERR_READ_ONLY = -1;
  ERR_SEEK      = -2;
  ERR_NOT_READY = -3;
  ERR_SEEK_1793 = 255;
  ERR_SEC_1793  = 254;

  DISK_SIZE_5x2x80 = 819200;  // non-HD defaults
  _MAX_SECTORS = 5;
  _MAX_TRACKS = 80;


// only Type I, II commands emulated
// 1793 Commands                  b7 b6 b5 b4 b3 b2 b1 b0

  CmdRestore        = $00;   //   0  0  0  0  h  V  r1 r0
  CmdSeek           = $10;   //   0  0  0  1  h  V  r1 r0
  CmdStep	    = $20;   //   0  0  1  T  h  V  r1 r0
  CmdStepUpdTrk     = $30;
  CmdStepIn         = $40;   //   0  1  0  T  h  V  r1 r0
  CmdStepInUpdTrk   = $50;
  CmdStepOut        = $60;   //   0  1  1  T  h  V  r1 r0
  CmdStepOutUpdTrk  = $70;
  CmdReadSector     = $80;   //   1  0  0  m  S  E  C  0
  CmdReadSectorMul  = $90;
  CmdWriteSector    = $A0;   //   1  0  1  m  S  E  C  a0
  CmdWriteSectorMul = $B0;
{
Flag Summary
    r1 r0  Stepping Motor Rate
    V	   Track Number Verify Flag (0: no verify, 1: verify on dest track)
    h	   Head Load Flag (1: load head at beginning, 0: unload head)
    T	   Track Update Flag (0: no update, 1: update Track Register)
    a0	   Data Address Mark (0: FB, 1: F8 (deleted DAM))
    C      Side Compare Flag (0: disable side compare, 1: enable side comp)
    E      15 ms delay (0: no 15ms delay, 1: 15 ms delay)
    S      Side Compare Flag (0: compare for side 0, 1: compare for side 1)
    m      Multiple Record Flag (0: single record, 1: multiple records)
}
type
  TOnAccess=procedure(Sender: TObject; Op: string; BeginOp: boolean);

// Internal registers:
//
// dr		data register (r/w)
// tr		track register (r/w)
// sr		sector register (r/w)
// cr		command register (w)
// str		status register (r)
// isStepIn	flag indicating that previous step command was STEP IN
// side		side of floppy to read/write on, changeable by subclass
// drq		status of drq pin
// irq		status of irq pin
// byteCount	byte counter during read/write
// strRead	count read access to command register, reset by read from dr

  TFDCReg = packed record
              dr, tr, sr, cr, str: Byte;
              isStepIn, drq, irq, side: Byte;
              byteCount, strRead: integer;
            end;

  T1793 = class(TObject)
  private
    FDCReg: TFDCReg;
//
    procedure do_seek(new_track: byte);
    function readIO(offset: word): byte;
    procedure writeIo(offset: word; val: Byte);
//
    procedure setIrq;                           virtual;
    procedure resetIrq;                         virtual;
    function  driveReady: boolean;              virtual;
    function  seekError(new_track: byte): byte; virtual;
    function  writeProtect: boolean;            virtual;
    function  recordNotFound: byte;             virtual;
    function  MaxSectors: integer;              virtual;
    function  MaxTracks: integer;               virtual;
    procedure command(command:byte);            virtual;
    procedure SaveToStream(Stream: TStream);    virtual;
    procedure ReadFromStream(Stream: TStream);  virtual;
    function  readByte(index: integer): byte;   virtual; abstract;
    procedure writeByte(index: integer);        virtual; abstract;
  public
    constructor Create; virtual;
    procedure Reset; virtual;
    property Reg[offset: word]:byte read readIO write writeIO; default; // interface with CPU
  end;

  TSectorBuffer = array[0..SECTOR_SIZE] of byte;
  TDriveArray = array[0..1] of ShortString;

  TFDController = class(T1793)
    FRGU: byte;                  // регистр управления: D0,D1=drive, D4=!side, плотность всегда двойная
    FOnAccess: TOnAccess;
    FDrive: TDriveArray;
    FStream: array[0..1] of TFileStream;
    FReadOnly: array [0..1] of boolean;
    FHD: array [0..1] of boolean;
    FMaxSectors: array [0..1] of integer;
    FMaxTracks: array [0..1] of integer;
    sector_buffer: TSectorBuffer;
  private
    procedure SetRGU(const Value: byte);
    function GetRGU: byte;
    procedure SetDrive(Index: Integer; const Value: String);
    function GetDrive(Index: Integer): string;
    procedure SetReadOnly(Index: Integer; const Value: boolean);
    function GetReadOnly(Index: Integer): boolean;
    function  writeProtect: boolean;          override;
    function  recordNotFound: byte;           override;
    function  driveReady: boolean;            override;
    function  readByte(index: integer): byte; override;
    procedure writeByte(index: integer);      override;
    function  MaxSectors: integer;            override;
    function  MaxTracks: integer;             override;
    function ReadSector(Tr, Sec: byte):integer;   // read sector from "floppy"
    function WriteSector(Tr, Sec: byte):integer;  // write sector to "floppy"
  public
    constructor Create; override;
    destructor Destroy; override;
    procedure Reset; override;
    procedure SaveToStream(Stream: TStream); override;
    procedure ReadFromStream(Stream: TStream); override;
    property Drive[Index: Integer]:string read GetDrive write SetDrive;
    property ReadOnly[Index: Integer]:boolean read GetReadOnly write SetReadOnly;
    property RGU:byte read GetRGU write SetRGU;
    property OnAccess:TOnAccess read FOnAccess write FOnAccess;
  end;

var
  FDController: TFDController;
  FddHd: boolean;

implementation

Uses uPackOdi;

{ T1793 }

constructor T1793.Create;
begin
  inherited;
  Reset;
end;

procedure T1793.command(command: byte);
var
  type1, index: byte;
begin
 with FDCReg do
 begin
  index := 0;  // for simulating INDEX bit
  type1 := 0;
  if (str and $01 = 0) or (command and $f0 = $d0) then
  begin
    cr := command;
    byteCount := 0;
    case (cr and $f0) of
      $00: begin
             tr := 0;			// RESTORE
             type1 := 1;
             setIrq();
           end;
      $10: begin
             do_seek(dr);		// SEEK
             type1 := 1;
           end;
      $30: if (isStepIn<>0) then	// STEP with update
             do_seek(tr + 1)
           else
             do_seek(tr - 1);
      $20: begin
             type1 := 1;		// STEP
             setIrq();
           end;
      $50: begin
             do_seek(tr + 1);	        // STEP IN with update
             isStepIn := 1;
             type1 := 1;
           end;
      $40: begin
             isStepIn := 1;
             setIrq();
             type1 := 1;
           end;
      $70: begin
             if (tr<>0) then		// STEP OUT with update
               do_seek(tr - 1);
             isStepIn := 0;
             type1 := 1;
           end;
      $60: begin
             isStepIn := 0;		// STEP OUT
             setIrq();
             type1 := 1;
           end;
      $80: begin
             strRead := 0;
             if (recordNotFound()<>0) then      // READ SECTOR
               str := $10
             else
             begin
               byteCount := SECTOR_SIZE;
               drq := 1;
               str := $03;
             end;
           end;
      $e0,				        // READ TRACK
      $90: begin
             byteCount := SECTOR_SIZE*MaxSectors();  // READ SECTOR mult.
             drq := 1;
             str := $03;
           end;
      $a0: begin
             if (writeProtect()) then           // WRITE SECTOR
               str := $40
             else
             if (recordNotFound()<>0) then
               str := $10
             else
             begin
               byteCount := SECTOR_SIZE;
               drq := 1;
               str := $03;                      //$02;
             end;
           end;
      $f0,				        // WRITE TRACK
      $b0: begin
             if (writeProtect()) then           // WRITE SECTOR mult.
               str := $40
	     else
             begin
               byteCount := SECTOR_SIZE*MaxSectors();
               drq := 1;
               str := $03;
             end;
           end;
      $c0: if (recordNotFound()<>0) then	// READ ADDRESS
             str := $10
           else
           begin
             byteCount := 6;
             drq := 1;
             str := $03;
           end;
      $d0: begin
             drq := 0;           		// FORCE INTERRUPT
             str := str and $fc;
             byteCount := 0;
             setIrq();
           end;
    end;
    if (type1<>0) then
    begin
      if (driveReady()) then
      begin                      // set index every MAX_SECTORS reads
        index := (index + 1 ) mod MaxSectors();
        if writeProtect() then
          str := $64
        else
          str := $24;
        if (index=0) then
          str := str or $02
      end
      else
      begin
        tr := 1;      // ALWAYS SET TRACK TO 1
                      // so system info sector never will be found
        str := $80;
      end;
    end;
  end;
 end;
end;

procedure T1793.do_seek(new_track: byte);
begin
 with FDCReg do
 begin
  str := $20;    // SEEK
  if (seekError(new_track)<>0) then
    str := str or $10
  else
    tr := new_track;
  if (tr=0) then
    str := str or $04;
  setIrq();
 end;
end;

function T1793.driveReady: boolean;
begin
  result:=True;
end;

function T1793.readIO(offset: word): byte;
var index: integer;
begin
 with FDCReg do
 begin
  index:=0;           // emulate index hole of floppy disc
  case (offset and $03) of
    0: begin
         resetIrq();
         if ((cr and $e0) = $80) then
         begin
           inc(strRead);
           if (strRead = 32) then
           begin
             drq := 0;
             str := str and $fc;      // read finished reset drq and busy
           end;
         end;
// set index every MAX_SECTORS reads
	 if (str and $80 <> 0) then
           result:=str
         else
         begin
           index:=(index + 1 ) mod MaxSectors();
           if ((index=0) and (cr and $80 = 0)) then
             result:=str or $02
           else
             result:=str;
         end;
       end;
    1: result:=tr;
    2: result:=sr;
    3: begin
         if (cr and $f0 = $c0) then
         begin
           case (byteCount) of
             6: dr := tr;
             5: dr := side;
             4: dr := 1;       // sector
             3: dr := 3;       // sector length
             2: dr := $55;
             1: dr := $55
	     else dr := 0;
           end;
           dec(byteCount);
         end
         else
         begin
           strRead := 0;
           if (byteCount<>0) then
           begin
             dr := readByte(byteCount);
             dec(byteCount);
	   end;
         end;
         if (byteCount=0) then
         begin
           drq := 0;
           str := str and $fc;   // read finished reset drq and busy
         end;
         result:=dr;
       end;
       else result:=0; // default, should never be used!
  end;
 end;
end;

function T1793.recordNotFound: byte;
begin
  result:=0;
end;

procedure T1793.Reset;
begin
 with FDCReg do
 begin
  resetIrq();
  isStepIn := 1;
  drq := 0;
  side := 0;
  byteCount := 0;
  strRead := 0;
  str := 0;
  cr := 0;            // clear previous command
  command(0);         // execute RESTORE after a reset
 end;
end;

procedure T1793.resetIrq;
begin
  FDCReg.irq:=0;
end;

function T1793.seekError(new_track: byte): byte;
begin
  if new_track<MaxTracks() then
    result:=0
  else
    result:=ERR_SEEK_1793;
end;

procedure T1793.setIrq;
begin
  FDCReg.irq:=1;
end;

procedure T1793.writeIo(offset: word; val: Byte);
begin
 with FDCReg do
 begin
  case (offset and $03) of
    0: begin
         resetIrq();
         command(val);
       end;
    1: tr:=val;
    2: sr:=val;
    3: begin
         dr:=val;
         if (byteCount<>0) then
         begin
           writeByte(byteCount);
           dec(byteCount);
         end;
         if (byteCount=0) then
         begin
           drq:=0;
           str:=str and $fc;       // write finished reset drq and busy
         end;
       end;
  end;
 end;
end;

function T1793.writeProtect: boolean;  // should be reimplemented by subclass, return $40 if wp, otherwise 0
begin
  Result:=False;
end;

procedure T1793.SaveToStream(Stream: TStream);
begin
  Stream.Write(FDCReg, sizeof(FDCReg));
end;

procedure T1793.ReadFromStream(Stream: TStream);
begin
  Stream.Read(FDCReg, sizeof(FDCReg));
end;

function T1793.MaxSectors: integer;
begin
  Result:=_MAX_SECTORS;
end;

function T1793.MaxTracks: integer;
begin
  Result:=_MAX_TRACKS;
end;

{ TFDController }

constructor TFDController.Create;
begin
  inherited;
  FStream[0]:=nil;  FStream[1]:=nil;
  FDrive[0]:='';    FDrive[1]:='';
  FOnAccess:=nil;
  Reset;
  Vars^.USE_DPBLESS_DISKS:=0;                 {added for HD-formats support}
  FMaxSectors[0]:=_MAX_SECTORS;  FMaxSectors[1]:=_MAX_SECTORS;
  FMaxTracks[0]:=_MAX_TRACKS;    FMaxTracks[1]:=_MAX_TRACKS;
end;

destructor TFDController.Destroy;
begin
  if Assigned(FStream[0]) then FStream[0].Free;
  if Assigned(FStream[1]) then FStream[1].Free;
end;

procedure TFDController.SetDrive(Index: Integer; const Value: String);
  procedure ClearDrive;
  begin
    FDrive[Index]:='';
    if Assigned(FStream[Index]) then
      FStream[Index].Free;
    FStream[Index]:=nil;
  end;
begin
  if (FRGU and 3)>1 then exit;
  if trim(Value)='' then
    ClearDrive()
  else if FileExists(AnsiUpperCase(trim(Value))) and
          (FDrive[Index]<>AnsiUpperCase(trim(Value))) then
  begin
    FDrive[Index]:='';
    if Assigned(FStream[Index]) then
      FStream[Index].Free;
    FStream[Index]:=nil;
    try
      FReadOnly[Index]:=FReadOnly[Index] or (FileGetAttr(trim(Value)) and faReadOnly <> 0);
      if FReadOnly[Index] then
        FStream[Index]:=TFileStream.Create(trim(Value), fmOpenRead or fmShareDenyWrite)
      else
        FStream[Index]:=TFileStream.Create(trim(Value), fmOpenReadWrite or fmShareDenyWrite);
      FDrive[Index]:=AnsiUpperCase(trim(Value));
{added for HD-formats support}
      FHD[Index]:=FStream[Index].Size>DISK_SIZE_5x2x80;
      if FHD[Index] then           
      begin
        GetBOOT(FDrive[Index], FStream[Index], -1);
        if not Vars^.BOOT.BOOTvalid then                                            // wrong CRC
          ClearDrive
        else begin
          FMaxSectors[Index]:=Vars^.BOOT.DPB.SEC;
          FMaxTracks[Index]:=Vars^.BOOT.DPB.TRK;
        end;
      end;
{}
    except
    end;
  end;
end;

procedure TFDController.Reset;
begin
  inherited;
  FRGU:=0;
end;

function TFDController.GetDrive(Index: Integer): string;
begin
  Result:=FDrive[Index];
end;

procedure TFDController.SetReadOnly(Index: Integer; const Value: boolean);
begin
  FReadOnly[Index]:=Value;
end;

function TFDController.GetReadOnly(Index: Integer): boolean;
begin
  Result:=FReadOnly[Index];
end;

function TFDController.ReadSector(Tr, Sec: byte):integer; // read SEC_SIZE bytes from floppy
var side: integer;
    offset: integer;
    cc: char;
begin
  if FHD[FRGU and 1] then cc:='h' else cc:=' ';
  If Assigned(FOnAccess) then
    FOnAccess(Self, format('%s %s %s', [chr(ord('A')+(FRGU and 3)), 'r', cc]), True);
  Result:=ERR_NOT_READY;                   // Failed
  if not Assigned(FStream[FRGU and 1]) then exit;
  Result:=ERR_SEEK;
  side:=((FRGU and 16) shr 4) xor 1;               // 0 or 1
  offset:=((side + Tr*2)*MaxSectors() + Sec-1)*SECTOR_SIZE;
  if FStream[FRGU and 1].Seek(offset , soFromBeginning)=offset then
    Result:=FStream[FRGU and 1].Read(sector_buffer, SECTOR_SIZE)-SECTOR_SIZE;
  If Assigned(FOnAccess) then
    FOnAccess(Self, format('%s %s %s', [chr(ord('A')+(FRGU and 3)), 'r', cc]), False);
end;

function TFDController.readByte(index: integer): byte;
begin
  result:=0;
  if not Assigned(FStream[FRGU and 1]) then exit;
  if (index = SECTOR_SIZE) then
  begin
    if (ReadSector(FDCReg.tr, FDCReg.sr)<>0) then
    begin
      FDCReg.drq := 0;
      FDCReg.str := $10;
      FDCReg.byteCount := 0;
    end;
  end;
  result:=sector_buffer[SECTOR_SIZE - index];
end;

function TFDController.WriteSector(Tr, Sec: byte):integer; // write SEC_SIZE bytes to floppy
var side: integer;
    offset: integer;
    cc: char;
begin
  if FHD[FRGU and 1] then cc:='h' else cc:=' ';
  If Assigned(FOnAccess) then
    FOnAccess(Self, format('%s %s %s', [chr(ord('A')+(FRGU and 3)), 'w', cc]), True);
  Result:=ERR_NOT_READY;                   // Failed
  if not Assigned(FStream[FRGU and 1]) then exit;
  Result:=ERR_READ_ONLY;                   // Failed
  if not FReadOnly[FRGU and 1] then
  begin
    Result:=ERR_SEEK;
    side:=((FRGU and 16) shr 4) xor 1;             // 0 or 1
    offset:=((side + Tr*2)*MaxSectors() + Sec-1)*SECTOR_SIZE;
    if FStream[FRGU and 1].Seek(offset, soFromBeginning)=offset then
      Result:=FStream[FRGU and 1].Write(sector_buffer, SECTOR_SIZE)-SECTOR_SIZE;
  end;
  If Assigned(FOnAccess) then
    FOnAccess(Self, format('%s %s %s', [chr(ord('A')+(FRGU and 3)), 'w', cc]), False);
end;

procedure TFDController.writeByte(index: integer);
begin
  sector_buffer[SECTOR_SIZE - index] := FDCReg.dr;
  if (index = 1) then
  begin
    WriteSector(FDCReg.tr, FDCReg.sr);
  end;
end;

function TFDController.GetRGU: byte;
begin
  Result:=$FF;     // недоступен на чтение
end;

procedure TFDController.SetRGU(const Value: byte);
begin
  FRGU:=Value;
end;

function TFDController.driveReady: boolean;
begin
  Result:=Assigned(FStream[FRGU and 1]);
end;

function TFDController.writeProtect: boolean;
begin
  Result:=FReadOnly[FRGU and 1];
end;

procedure TFDController.ReadFromStream(Stream: TStream);
var l:Longint;
    xDrive: TDriveArray;
begin
  inherited;
  Stream.Read(FRGU, sizeof(FRGU));
  Stream.Read(FReadOnly, sizeof(FReadOnly));
  Stream.Read(FHD, sizeof(FHD));
  Stream.Read(xDrive, sizeof(TDriveArray));
  Stream.Read(sector_buffer, sizeof(sector_buffer));
  SetDrive(0, xDrive[0]);
  SetDrive(1, xDrive[1]);
  Stream.Read(l, sizeof(l));
  if Assigned(FStream[0]) and (l>=0) then
    FStream[0].Position:=l;
  Stream.Read(l, sizeof(l));
  if Assigned(FStream[1]) and (l>=0) then
    FStream[1].Position:=l;
end;

procedure TFDController.SaveToStream(Stream: TStream);
var l:Longint;
begin
  inherited;
  Stream.Write(FRGU, sizeof(FRGU));
  Stream.Write(FReadOnly, sizeof(FReadOnly));
  Stream.Write(FHD, sizeof(FHD));
  Stream.Write(FDrive, sizeof(TDriveArray));
  Stream.Write(sector_buffer, sizeof(sector_buffer));
  l:=-1;
  if Assigned(FStream[0]) then
    l:=FStream[0].Position;
  Stream.Write(l, sizeof(l));
  l:=-1;
  if Assigned(FStream[1]) then
    l:=FStream[1].Position;
  Stream.Write(l, sizeof(l));
end;

function TFDController.recordNotFound: byte;
begin
  if FHD[FRGU and 1] and ((FRGU and 8 = 0) or (not FddHd)) then
    result:=ERR_SEC_1793
  else
    result:=0;
end;

function TFDController.MaxSectors: integer;
begin
  Result:=FMaxSectors[FRGU and 1];
end;

function TFDController.MaxTracks: integer;
begin
  Result:=FMaxTracks[FRGU and 1];
end;

initialization
  FDController:=TFDController.Create;

finalization
  FDController.Free;

end.

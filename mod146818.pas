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


unit mod146818;

{***********************************************

    MC146818A (512ÂÈ1) emulation

 ***********************************************}

interface


{$I 'OrionZEm.inc'}


Uses Windows, Messages, SysUtils, Classes;

const
  FMC_ADDR60  = $F760;       // addres 512vi1 - Orion-128
  FMC_DATA61  = $F761;       // data CMOS     - Orion-128
  FMC_DATA50  = $50;         // data CMOS   - Orion-Pro
  FMC_ADDR51  = $51;         // addres CMOS - Orion-Pro
  K512viF760  = 1;
  K512vi50    = 2;
  DS1302F760  = 3;
  DS1302_50   = 4;

  last_day: array[0..11] of byte = (31, 28, 31, 30, 31, 30, 31, 31, 30, 31, 30, 31);

type
  TFCReg = packed record
             FAddr: byte;                               // index of current internal register operating (0..63)
             FDate: TDateTime;
             FUseSysTime: boolean;
             second, minute, hour: word;
             al_second, al_minute, al_hour: byte;
             weekday, day, month, year: word;
             A, B, C, D: byte;
             ram: array [$0E..$3F] of byte;
           end;

  T146818 = class(TObject)
    FCReg: TFCReg;
  private
    function GetData(Index: Integer): byte;             // emulate CPU reading from internal registers
    procedure SetData(Index: Integer; const Val: byte); // emulate CPU writing to internal registers
    function convert(val: Byte): byte;
    function convert_hour(val: Byte): byte;
    function convert_bin(val: Byte): byte;
    function increment(var reg: word; min, max: Byte): boolean;
    function increment_hour(var reg: word): boolean;
    function increment_day(var day: word; month, year: word): boolean;
    procedure SetSysTime;
    function GetRAM: string;
    procedure SetRAM(const Value: string);
  public
    constructor Create; virtual;
    procedure Reset;
    procedure SaveToStream(Stream: TStream);
    procedure ReadFromStream(Stream: TStream);
    procedure update_1_second;
    property Reg[Index: Integer]:byte read GetData write SetData; default;  // interface with CPU
    property UseSysTime:boolean read FCReg.FUseSysTime write FCReg.FUseSysTime;
    property Addr:byte read FCReg.FAddr write FCReg.FAddr;
    property MCRAM:string read GetRAM write SetRAM;
  end;

var
  F146818: T146818;
  MC146818RAM: string;
  DeltaDate: TDateTime = 0.0;                         // delta (shift value) from system time
  DeltaTime: TDateTime = 0.0;

function MIN(x,y: integer):integer;
function HexToInt(ss: string):integer;

implementation

function MIN(x,y: integer):integer;
begin
  if x>y then Result:=y else Result:=x;
end;

function HexToInt(ss: string):integer;
var k,l,m: integer;
begin
    Result:=-1;
    ss:=UpperCase(trim(ss));
    k:=Length(ss);
    if (k=0)or(k mod 2<>0) then exit;
    l:=1; Result:=0;
    repeat
      case ss[k] of
        '0'..'9': m:=StrToInt(ss[k]);
        'A'..'F': m:=ord(ss[k])-ord('A')+10;
        else m:=-1
      end;
      Result:=Result+m*l;
      l:=l*16;
      dec(k);
    until (k=0) or (Result<0);
end;

{ T146818 }

function BTST7(const Val: integer):boolean;
begin
  Result:=Val and $80 <> 0;
end;

function BTST0(const Val: integer):boolean;
begin
  Result:=Val and 1 <> 0;
end;

function BTST4(const Val: integer):boolean;
begin
  Result:=Val and $10 <> 0;
end;

function BTST5(const Val: integer):boolean;
begin
  Result:=Val and $40 <> 0;
end;

procedure BSET4(var Val: byte);
begin
  Val:=Val or $10;
end;

procedure BSET5(var Val: byte);
begin
  Val:=Val or $20;
end;

procedure BSET7(var Val: byte);
begin
  Val:=Val or $80;
end;

function T146818.convert(val: Byte): byte;
begin
 with FCReg do
 begin
  if (B and $04 <> 0) then
    Result:=val
  else
    Result:=((val div 10) shl 4) or (val mod 10);
 end;
end;

function T146818.convert_bin(val: Byte): byte;
begin
 with FCReg do
 begin
  if (B and $04 <> 0) then
    result:=val
  else
    result:=((val shr 4) * 10) or (val and $0f);
 end;
end;

function T146818.convert_hour(val: Byte): byte;
begin
 with FCReg do
 begin
  case (B and $06) of
    $00: //12 hour, BCD
         if (val >= 12) then
           result:=$80 or (((val-12) div 10) shl 4) or ((val-12) mod 10)
         else
           result:=((val div 10) shl 4) or (val mod 10);
    $02: //24 hour, BCD
         result:=((val div 10) shl 4) or (val mod 10);
    $04: //12 hour, binary
         if (val >= 12) then
           result:=(val - 12) or $80
         else
           result:=val;
     $06: //24 hour, binary
         result:=val
     else result:=1;  // this should NEVER happen
  end;
 end;
end;

constructor T146818.Create;
begin
  inherited;
  with FCReg do
  begin
    FUseSysTime:=True;
    A:=0;
    B:=$06;
  end;
  Reset;
end;

function T146818.GetData(Index: Integer): byte;
begin
 with FCReg do
 begin
  Index:=Index and $3F;
  case Index of
    $00: Result:=second;
    $01: Result:=al_second;
    $02: Result:=minute;
    $03: Result:=al_minute;
    $04: Result:=hour;
    $05: Result:=al_hour;
    $06: Result:=weekday;
    $07: Result:=day;
    $08: Result:=month;
    $09: Result:=year;
    $0a: Result:=A;
    $0b: Result:=B;
    $0c: begin
           Result:=C; C:=0;
         end;
    $0d: Result:=D
    else Result:=ram[Index];
  end;
 end;
end;

function T146818.GetRAM: string;
var i: integer;
begin
  Result:='';
  for i:=$0e to $3F do
    Result:=Result+IntToHex(FCReg.ram[i],2);
end;

function T146818.increment(var reg: word; min, max: Byte): boolean;
begin
 result:=False;
 with FCReg do
 begin
  if (B and $04 <> 0) then begin
// binary calculation
   inc(reg);
   if (reg > max) then begin
     reg := min;
     result:=True;
   end
  end
  else begin
// bcd calculation
    if ((reg and $0f) = 9) then
      reg := (reg and $f0) + $10
    else
      inc(reg);
    if (reg > convert(max)) then begin
      reg := min;
      result:=True;
    end
  end;
 end;
end;

function T146818.increment_day(var day: word; month, year: word): boolean;
var  binmonth: byte;
begin
   binmonth := convert_bin(month);
   if (binmonth < 1) then
     binmonth := 1;
   if (binmonth > 12) then
     binmonth := 12;
// if February leap year
   if ((binmonth = 2) and (convert_bin(year) mod 4 = 0)) then begin
     if (convert_bin(day) = 29) then begin			// switch to next month on 29. Febr.
       day := 1;
       result:=True;
       exit;
     end;
   end
   else if (convert_bin(day) = last_day[binmonth - 1]) then begin
           day := 1;
           result:=True;
           exit;
	end;
   with FCReg do
   begin
     if (B and $04 <> 0) then begin 		// binary calculation
       inc(day);
     end
     else begin		// bcd calculation
       if ((day and $0f) = 9) then
         day := (day and $f0) + $10
       else
         inc(day);
     end;
   end;
   Result:=False;
end;

function T146818.increment_hour(var reg: word): boolean;
begin
 Result:=False;
 with FCReg do
 begin
  case (B and $06) of
    $00: //12 hour, BCD
         if (reg = $12) then begin
           reg := $81;
         end
         else if (reg = $92) then begin
                reg := $01;
                result:=True;
//                exit;
         end
         else if ((reg and $0f) = 9) then begin
                reg := (reg and $f0) + $10
	      end
              else
                inc(reg);
    $02: //24 hour, BCD
         if ((reg and $0f) = 9) then begin
           reg := (reg and $f0) + $10;
         end
         else if (reg = $23) then begin
                reg := $00;
                result:=True;
//                exit;
              end
              else
                inc(reg);
    $04: //12 hour, binary
         if (reg = $0C) then begin
            reg := $81;
         end
         else if (reg = $8C) then begin
                reg := $01;
                result:=True;
//                exit;
              end else
                    inc(reg);
    $06: //24 hour, binary
         if (reg = $17) then begin
           reg := $00;
           result:=True;
//           exit;
         end
         else
           inc(reg)
  end;
 end;
end;

procedure T146818.Reset;
begin
 with FCReg do
 begin
  A := A and $7f;
  B := B and $87;
  C := 0;
  D := $80;
  FAddr := 0;
 end;
 SetSysTime();
end;

procedure T146818.SetSysTime;
var MSec: Word;
begin
// initialize clock registers with system time
 with FCReg do
 begin
  DecodeTime(Time()-DeltaTime, Hour, minute, second, MSec);
  minute:=convert(minute);
  second:=convert(second);
  Hour:=convert_hour(Hour);
  FDate:=Date();
  DecodeDate(FDate-DeltaDate, Year, Month, Day);
  weekday:=DayOfWeek(FDate);
  day:=convert(day);
  month:=convert(month{+1});
  year:=convert(year mod 100);
 end;
end;

procedure T146818.SetData(Index: Integer; const Val: byte);
var yy: word;
begin
 with FCReg do
 begin
  Index:=Index and $3F;
  case Index of
    $00: second    := val and 63;
    $01: al_second := val and 63;
    $02: minute    := val and 63;
    $03: al_minute := val and 63;
    $04: hour      := val and 31;
    $05: al_hour   := val and 31;
    $06: weekday   := val and 7;
    $07: day       := val and 31;
    $08: month     := val and 15;
    $09: year      := val and 127;
    $0a: A         := val and $7f;
    $0b: B         := val;
    else ram[Index]:= val;
  end;
  if Index<5 then
    DeltaTime:=Time()-EncodeTime(hour, minute, second, 0)
  else if Index<10 then
       begin
         if year<50 then yy:=2000+year else yy:=1900+year;
         DeltaDate:=Date()-EncodeDate(yy, month, day);
       end;
 end;
end;

procedure T146818.SetRAM(const Value: string);
var i: integer;
begin
  for i:=$0e to MIN($3F, (length(Value) div 2)+$0d) do
    FCReg.ram[i]:=HexToInt(copy(Value, (i-$0e)*2+1, 2));
end;

procedure T146818.update_1_second;
var dse_october: Byte;
begin
 with FCReg do
 begin
  if FCReg.FUseSysTime then begin
    SetSysTime();
    exit;
  end;
  dse_october:=0;
  // update only if SET bit is 0
  if (not BTST7(B)) then begin		// check for last sunday in april 1:59:59
    if (BTST0(B) and (hour = 1) and
        (convert_bin(minute) = 59) and
        (convert_bin(second) = 59) and
        (month = 4) and
        (weekday = 1) and
        (convert_bin(day) >= 24)) then
       begin
         hour	:= 3;
         minute	:= 0;
         second	:= 0;
       end
	// check for last sunday in october 1:59:59
    else if (BTST0(B) and (hour = 1) and
             (convert_bin(minute) = 59) and
             (convert_bin(second) = 59) and
             (convert_bin(month) = 10) and
             (weekday = 1) and
             (convert_bin(day) >= 25) and	(dse_october=0)) then
           begin
             dse_october := 1;
             hour	:= 1;
             minute := 0;
             second := 0;
	 end
         else begin     // do a normal update
           if (increment(second, 0, 59)) then
             if (increment(minute, 0, 59)) then
               if (increment_hour(hour)) then begin
                 increment(weekday, 1, 7);
                 if (increment_day(day, month, year)) then
                   if (increment(month, 1, 12)) then
                     increment(year, 0, 99);
	     end;
         end;
    BSET4(C); // set update ended interrupt flag
    if (BTST4(B)) then begin
      BSET7(C);
    end;
    // now check for an alarm
    if ((((al_second and $c0) = $c0) or (al_second = second)) and
        (((al_minute and $c0) = $c0) or (al_minute = minute)) and
        (((al_hour and $c0) = $c0) or (al_hour = hour))) then begin
      BSET5(C); // set alarm interrupt flag
      if (BTST5(B)) then begin
        BSET7(C);
      end;
    end;
  end;
 end;
end;

procedure T146818.ReadFromStream(Stream: TStream);
begin
  Stream.Read(FCReg, sizeof(FCReg));
end;

procedure T146818.SaveToStream(Stream: TStream);
begin
  Stream.Write(FCReg, sizeof(FCReg));
end;

initialization
  F146818:=T146818.Create;

finalization
  F146818.Free;

end.


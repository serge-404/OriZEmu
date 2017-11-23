unit modZ80;

{
' /*******************************************************************************
'
'   Complete Z80 emulation, including (as far as I know) the
'   correct emulation of bits 3 and 5 of F, and undocumented ops.
'   Please mail me if you find any bugs in the emulation!
'
'   Author: Chris Cowley <ccowley@grok.co.uk>
'
'   Copyright (C)1999-2000 Grok Developments Ltd.
'   http://www.grok.co.uk/
'
'   Translation to Delphi Object Pascal by
'           Jari Korhonen <jarit.korhonen@luukku.com>
'
'   This program is free software; you can redistribute it and/or
'   modify it under the terms of the GNU General Public License
'   as published by the Free Software Foundation; either version 2
'   of the License, or (at your option) any later version.
'   This program is distributed in the hope that it will be useful,
'   but WITHOUT ANY WARRANTY; without even the implied warranty of
'   MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
'   GNU General Public License for more details.
'
'   You should have received a copy of the GNU General Public License
'   along with this program; if not, write to the Free Software
'   Foundation, Inc., 59 Temple Place - Suite 330, Boston, MA  02111-1307, USA.
'
' *******************************************************************************/}


interface


{$I 'OrionZEm.inc'}


type
  TAfterInstruction=procedure();
  TAfterBreakPoint=procedure();
  TAfterOneSecond=procedure(CpuIdlePercent:integer);

var
  AfterInstruction:TAfterInstruction = nil;
  AfterBreakPoint:TAfterInstruction = nil;
  AfterOneSecond: TAfterOneSecond = nil;
  AfterHalfSecond: TAfterOneSecond = nil;

function getAF: integer;
function getBC: integer;
function getIR: integer;
function bitSet(bit : integer; val : integer): integer;
function getF: integer;

procedure cp_a(b : integer);
procedure setAF(v : integer);
procedure setF(b : integer);
procedure setBC(nn : integer);
procedure poppc;
procedure pushpc;
procedure Z80Reset;
procedure execute(var local_tstates: integer);

implementation

uses Windows, Forms, Sysutils, mmsystem, modOrion, modWaveout, modAY8912, mod146818, mod8255, mod232 {!!! mainwin};

procedure adc_a(b: integer);
    var
      wans: integer;
      ans: integer;
      c: integer;
begin

    If fC Then c := 1
    else c := 0;

    wans := regA + b + c;
    ans := wans And $FF;

    fS := (ans And F_S) <> 0;
    f3 := (ans And F_3) <> 0;
    f5 := (ans And F_5) <> 0;
    fZ := (ans = 0);
    fC := (wans And $100) <> 0;
    fPV := ((regA Xor ((Not b) And $FFFF)) And (regA Xor ans) And $80) <> 0;

    fH := (((regA And $F) + (b And $F) + c) And F_H) <> 0;
    fN := False;

    regA := ans;
end;


function adc16(a : integer; b : integer): integer;
    var c : integer; lans : integer; ans : integer;
begin

    If fC Then c := 1
    else c := 0;

    lans := a + b + c;
    ans := lans And $FFFF;

    fS := (ans And (F_S {* 256} shl 8)) <> 0;     // 20061220
    f3 := (ans And (F_3 {* 256} shl 8)) <> 0;
    f5 := (ans And (F_5 {* 256} shl 8)) <> 0;
    fZ := (ans = 0);
    fC := (lans And $10000) <> 0;
    fPV := ((a Xor ((Not b) And $FFFF)) And (a Xor ans) And $8000) <> 0;
    fH := (((a And $FFF) + (b And $FFF) + c) And $1000) <> 0;
    fN := False;

    adc16 := ans;
end;

procedure add_a(b : integer);
    var wans : integer; ans : integer;
begin

    wans := regA + b;
    ans := wans And $FF;

    fS := (ans And F_S) <> 0;
    f3 := (ans And F_3) <> 0;
    f5 := (ans And F_5) <> 0;
    fZ := (ans = 0);
    fC := (wans And $100) <> 0;
    fPV := ((regA Xor ((Not (b)) And $FFFF)) And (regA Xor ans) And $80) <> 0;
    fH := (((regA And $F) + (b And $F)) And F_H) <> 0;
    fN := False;

    regA := ans;
end;

function add16(a : integer; b : integer):integer;
    var lans : integer;
        ans : integer;
begin

    lans := a + b;
    ans := lans And $FFFF;

    f3 := (ans And (F_3 {* 256} shl 8)) <> 0;   // 20061220
    f5 := (ans And (F_5 {* 256} shl 8)) <> 0;
    fC := (lans And $10000) <> 0;
    fH := (((a And $FFF) + (b And $FFF)) And $1000) <> 0;
    fN := False;

    add16 := ans;
end;

procedure and_a(b : integer);
begin
    regA := (regA And b);

    fS := (regA And F_S) <> 0;
    f3 := (regA And F_3) <> 0;
    f5 := (regA And F_5) <> 0;
    fH := True;
    fPV := Parity[regA];
    fZ := (regA = 0);
    fN := False;
    fC := False;
end;

{ 20090118 }

procedure bit(b : integer; r : integer);
    var IsbitSet : Boolean;
begin
    IsbitSet := (r And b) <> 0;
    fN := False;
    fH := True;
    fS := IsbitSet And (b = F_S);
    f3 := (r and F_3) <> 0;
    f5 := (r and F_5) <> 0;
    fZ := Not IsbitSet;
    fPV := fZ;
end;

procedure bit_hl(b : integer; r : integer);
    var IsbitSet : Boolean;
        rr : integer;
begin
    IsbitSet := (r And b) <> 0;
    fN := False;
    fH := True;
    fS := IsbitSet And (b = F_S);
    rr:= (regHL shr 8) and $FF;
    f3 := (rr and F_3) <> 0;
    f5 := (rr and F_5) <> 0;
    fZ := Not IsbitSet;
    fPV := fZ;
end;

procedure bit_id(b : integer; r : integer; id : Integer); 
    var IsbitSet : Boolean;
begin
    IsbitSet := (r And b) <> 0;
    fN := False;
    fH := True;
    fS := IsbitSet And (b = F_S);
    f3 := ((id shr 8) and F_3) <> 0;
    f5 := ((id shr 8) and F_5) <> 0;
    fZ := Not IsbitSet;
    fPV := fZ;
end;

{ / 20090118 }

function bitRes(bit : integer; val : integer): integer;
begin
//    bitRes := val And (Not (bit) And $FFFF);   // 20061220
  bitRes := val And (bit Xor $FFFF);
end;

function bitSet(bit : integer; val : integer): integer;
begin
    bitSet := val Or bit;
end;

procedure ccf();
begin
    f3 := (regA And F_3) <> 0;
    f5 := (regA And F_5) <> 0;
    fH := fC;
    fN := False;
    fC := Not fC;
end;

procedure cp_a(b : integer);
    var a : integer; wans : integer; ans : integer;
begin

    a := regA;
    wans := a - b;
    ans := wans And $FF;

    fS := (ans And F_S) <> 0;
    f3 := (b And F_3) <> 0;
    f5 := (b And F_5) <> 0;
    fN := True;
    fZ := (ans = 0);
    fC := (wans And $100) <> 0;
    fH := (((a And $F) - (b And $F)) And F_H) <> 0;
    fPV := ((a Xor b) And (a Xor ans) And $80) <> 0;
end;

procedure cpl_a;
begin
    regA := (regA Xor $FF) And $FF;

    f3 := (regA And F_3) <> 0;
    f5 := (regA And F_5) <> 0;
    fH := True;
    fN := True;
end;

procedure sub_a(b : integer);
    var a : integer; wans : integer; ans : integer;
begin

    a := regA;
    wans := a - b;
    ans := wans And $FF;

    fS := (ans And F_S) <> 0;
    f3 := (ans And F_3) <> 0;
    f5 := (ans And F_5) <> 0;
    fZ := (ans = 0);
    fC := (wans And $100) <> 0;
    fPV := ((a Xor b) And (a Xor ans) And $80) <> 0;
    fH := (((a And $F) - (b And $F)) And F_H) <> 0;
    fN := True;

    regA := ans;
end;


procedure daa_a;
    var ans : integer; incr : integer; carry : Boolean;
begin

    incr := 0;
    ans := regA;
    carry := fC;

    If (fH = True) Or ((ans And $F) > $9) Then
        incr := incr Or $6;

    If (carry = True) Or (ans > $9F) Then
        incr := incr Or $60;

    If ((ans > $8F) And ((ans And $F) > 9)) Then
        incr := incr Or $60;

    If (ans > $99) Then
        carry := True;

    If (fN = True) Then
        sub_a(incr)
    Else
        add_a(incr);
    
    ans := regA;
    fC := carry;
    fPV := Parity[ans];
end;

function dec16(a : integer) : integer;
begin
    dec16 := (a - 1) And $FFFF;
end;

procedure ex_af_af;
    var t : integer;
begin
    t := getAF;
    setAF(regAF_);
    regAF_ := t;
end;

function rlc(ans : integer) : integer;
    var c : Boolean;
begin

    c := (ans And $80) <> 0;

    If c Then
        ans := (ans * 2) Or $1
    Else
        ans := (ans * 2);
    
    ans := ans And $FF;

    fS := (ans And F_S) <> 0;
    f3 := (ans And F_3) <> 0;
    f5 := (ans And F_5) <> 0;
    fZ := (ans = 0);
    fPV := Parity[ans];
    fH := False;
    fN := False;
    fC := c;

    rlc := ans;
end;

function nxtpcb: integer;
  var dummy: integer;
begin
    dummy := peekb(regPC);
    nxtpcb := dummy;
{$IFDEF USE_DEBUGGING}
    ReportDebug('nxtpcb', dummy, false);
{$ENDIF}
    regPC := (regPC + 1);
end;

procedure setD(l : integer);
begin
  regDE := (l {* 256} shl 8) Or (regDE And $FF);   // 20061220
end;

function getD: integer;
begin
  getD := regDE shr 8;
end;

procedure setE(l : integer);
begin
  regDE := (regDE And $FF00) Or l;
end;

function getE: integer;
begin
  getE := regDE And $FF;
end;

procedure setF(b : integer);
begin
    fS := (b And F_S) <> 0;
    fZ := (b And F_Z) <> 0;
    f5 := (b And F_5) <> 0;
    fH := (b And F_H) <> 0;
    f3 := (b And F_3) <> 0;
    fPV := (b And F_PV) <> 0;
    fN := (b And F_N) <> 0;
    fC := (b And F_C) <> 0;
end;

procedure setH(l : integer);
begin
    regHL := (l {* 256} shl 8) Or (regHL And $FF);  // 20061220
end;


procedure setL(l : integer);
begin
    regHL := (regHL And $FF00) Or l;
end;


function getF: integer;
var res: integer;
begin
    res := 0;
    If fS Then res := res + F_S;
    If fZ Then res := res + F_Z;
    If f5 Then res := res + F_5;
    If fH Then res := res + F_H;
    If f3 Then res := res + F_3;
    If fPV Then res := res + F_PV;
    If fN Then res := res + F_N;
    If fC Then res := res + F_C;
    getF := res;
end;


function getAF: integer;
begin
    getAF := (regA {* 256} shl 8) Or getF;   // 20061220
end;

function getBC: integer;
begin
    getBC := (regB {* 256} shl 8) Or regC;   // 20061220
end;

function getIR: integer;
begin
    getIR := (intI {* 256} shl 8) Or intR;   // 20061220
end;

function getH: integer;
begin
    getH := regHL shr 8;
end;

function getIDH: integer;
begin
    getIDH := (regID shr 8) And $FF;
end;

function getIDL: integer;
begin
    getIDL := regID And $FF;
end;


function getL: integer;
begin
    getL := regHL And $FF;
end;

procedure rrc_a;
    var c : Boolean;
begin
    c := (regA And $1) <> 0;

    If c Then
        regA := (regA shr 1) Or $80
    Else
        regA := regA shr 1;

    f3 := (regA And F_3) <> 0;
    f5 := (regA And F_5) <> 0;
    fN := False;
    fH := False;
    fC := c;
end;


function rl(ans : integer) : integer;
    var c : Boolean;
begin

    c := (ans And $80) <> 0;

    If fC Then
        ans := (ans * 2) Or $1
    Else
        ans := ans * 2;

    ans := ans And $FF;

    fS := (ans And F_S) <> 0;
    f3 := (ans And F_3) <> 0;
    f5 := (ans And F_5) <> 0;
    fZ := (ans = 0);
    fPV := Parity[ans];
    fH := False;
    fN := False;
    fC := c;

    rl := ans;
end;

procedure rl_a;
    var ans : integer; c : Boolean;
begin

    ans := regA;
    c := (ans And $80) <> 0;

    If fC Then
        ans := (ans * 2) Or $1
    Else
        ans := (ans * 2);

    ans := ans And $FF;

    f3 := (ans And F_3) <> 0;
    f5 := (ans And F_5) <> 0;
    fN := False;
    fH := False;
    fC := c;

    regA := ans;
end;


procedure rlc_a;
    var c : Boolean;
begin
    c := (regA And $80) <> 0;

    If c Then
        regA := (regA * 2) Or 1
    Else
        regA := (regA * 2);

    regA := regA And $FF;

    f3 := (regA And F_3) <> 0;
    f5 := (regA And F_5) <> 0;
    fN := False;
    fH := False;
    fC := c;
end;

function rr(ans : integer) : integer;
    var c : Boolean;
begin
    c := (ans And $1) <> 0;

    If fC Then
        ans := (ans shr 1) Or $80
    Else
        ans := (ans shr 1);

    fS := (ans And F_S) <> 0;
    f3 := (ans And F_3) <> 0;
    f5 := (ans And F_5) <> 0;
    fZ := (ans = 0);
    fPV := Parity[ans];
    fH := False;
    fN := False;
    fC := c;

    rr := ans;
end;

procedure rr_a;
    var ans : integer; c : Boolean;
begin
    ans := regA;
    c := (ans And $1) <> 0;

    If fC Then
        ans := (ans shr 1) Or $80
    Else
        ans := (ans shr 1);

    f3 := (ans And F_3) <> 0;
    f5 := (ans And F_5) <> 0;
    fN := False;
    fH := False;
    fC := c;

    regA := ans;
end;

function rrc(ans : integer) : integer;
    var c : Boolean;
begin
    c := (ans And $1) <> 0;

    If c Then
        ans := (ans shr 1) Or $80
    Else
        ans := (ans shr 1);

    fS := (ans And F_S) <> 0;
    f3 := (ans And F_3) <> 0;
    f5 := (ans And F_5) <> 0;
    fZ := (ans = 0);
    fPV := Parity[ans];
    fH := False;
    fN := False;
    fC := c;

    rrc := ans;
end;

procedure rrd_a;
    var ans : integer; t : integer; q : integer;
begin
    ans := regA;
    t := peekb(regHL);
    q := t;

    t := (t shr 4) Or (ans * 16);
    ans := (ans And $F0) Or (q And $F);
    pokeb(regHL, t);

    fS := (ans And F_S) <> 0;
    f3 := (ans And F_3) <> 0;
    f5 := (ans And F_5) <> 0;
    fZ := (ans = 0);
    fPV := Parity[ans];
    fH := False;
    fN := False;

    regA := ans;
end;



procedure sbc_a(b : integer);
    var a : integer; wans : integer; ans : integer; c : integer;
begin

    a := regA;

    If fC Then c := 1
    else c:=0;

    wans := a - b - c;
    ans := wans And $FF;

    fS := (ans And F_S) <> 0;
    f3 := (ans And F_3) <> 0;
    f5 := (ans And F_5) <> 0;
    fZ := (ans = 0);
    fC := (wans And $100) <> 0;
    fPV := ((a Xor b) And (a Xor ans) And $80) <> 0;
    fH := (((a And $F) - (b And $F) - c) And F_H) <> 0;
    fN := True;

    regA := ans;
end;

function sbc16(a : integer; b : integer) : integer;
    var c : integer; lans : integer; ans : integer;
begin

    If fC Then c := 1
    else c:=0;

    lans := a - b - c;
    ans := lans And $FFFF;

    fS := (ans And (F_S {* 256} shl 8)) <> 0;      // 20061220
    f3 := (ans And (F_3 {* 256} shl 8)) <> 0;
    f5 := (ans And (F_5 {* 256} shl 8)) <> 0;
    fZ := (ans = 0);
    fC := (lans And $10000) <> 0;
    fPV := ((a Xor b) And (a Xor ans) And $8000) <> 0;
    fH := (((a And $FFF) - (b And $FFF) - c) And $1000) <> 0;
    fN := True;

    sbc16 := ans;
end;

procedure scf;
begin
    f3 := (regA And F_3) <> 0;
    f5 := (regA And F_5) <> 0;
    fN := False;
    fH := False;
    fC := True;
end;

procedure setAF(v : integer);
begin
    regA := (v And $FF00) shr 8;
    setF(v And $FF);
end;

procedure setBC(nn : integer);
begin
    regB := (nn And $FF00) shr 8;
    regC := nn And $FF;
end;

function inc16(a : integer) : integer;
begin
    inc16 := (a + 1) And $FFFF;
end;

function nxtpcw : integer;
  var dummy: integer;
begin
    dummy := peekb(regPC) + (peekb(regPC + 1) {* 256} shl 8);  // 20061220
    nxtpcw := dummy;
{$IFDEF USE_DEBUGGING}
    ReportDebug('nxtpcw', dummy, false);
{$ENDIF}

    regPC := regPC + 2;
end;

function sla(ans : integer) : integer;
    var c : Boolean;
begin
    c := (ans And $80) <> 0;
    ans := (ans * 2) And $FF;

    fS := (ans And F_S) <> 0;
    f3 := (ans And F_3) <> 0;
    f5 := (ans And F_5) <> 0;
    fZ := (ans = 0);
    fPV := Parity[ans];
    fH := False;
    fN := False;
    fC := c;
    sla := ans;
end;

function sls(ans : integer) : integer;
    var c : Boolean;
begin
    c := (ans And $80) <> 0;
    ans := ((ans * 2) Or $1) And $FF;
    
    fS := (ans And F_S) <> 0;
    f3 := (ans And F_3) <> 0;
    f5 := (ans And F_5) <> 0;
    fZ := (ans = 0);
    fPV := Parity[ans];
    fH := False;
    fN := False;
    fC := c;

    sls := ans;
end;

function sra(ans : integer) : integer;
    var c : Boolean;
begin
    c := (ans And $1) <> 0;
    ans := (ans shr 1) Or (ans And $80);

    fS := (ans And F_S) <> 0;
    f3 := (ans And F_3) <> 0;
    f5 := (ans And F_5) <> 0;
    fZ := (ans = 0);
    fPV := Parity[ans];
    fH := False;
    fN := False;
    fC := c;

    sra := ans;
end;

function srl(ans : integer) : integer;
    var c : Boolean;
begin
    c := (ans And $1) <> 0;
    ans := ans shr 1;

    fS := (ans And F_S) <> 0;
    f3 := (ans And F_3) <> 0;
    f5 := (ans And F_5) <> 0;
    fZ := (ans = 0);
    fPV := Parity[ans];
    fH := False;
    fN := False;
    fC := c;

    srl := ans;
end;



procedure xor_a(b : integer);
begin
    regA := (regA Xor b) And $FF;

    fS := (regA And F_S) <> 0;
    f3 := (regA And F_3) <> 0;
    f5 := (regA And F_5) <> 0;
    fH := False;
    fPV := Parity[regA];
    fZ := (regA = 0);
    fN := False;
    fC := False;
end;

function execute_cb: integer;
    var xxx : integer;
begin

    intRTemp := intRTemp + 1;

    xxx := nxtpcb;
    Case xxx of
      0:  //RLC B
        begin
          regB := rlc(regB);
          execute_cb := 8;
        end;
      1:  //RLC C
        begin
          regC := rlc(regC);
          execute_cb := 8;
        end;
      2:  //RLC D
        begin
          setD(rlc(getD));
          execute_cb := 8;
        end;
      3:  //RLC E
        begin
          setE(rlc(getE));
          execute_cb := 8;
        end;
      4:  //RLC H
        begin
          setH(rlc(getH));
          execute_cb := 8;
        end;
      5:  //RLC L
        begin
          setL(rlc(getL));
          execute_cb := 8;
        end;
      6:  //RLC (HL)
        begin
          pokeb(regHL, rlc(peekb(regHL)));
          execute_cb := 15;
        end;
      7:  //RLC A
        begin
          regA := rlc(regA);
          execute_cb := 8;
        end;
      8:  //RRC B
        begin
          regB := rrc(regB);
          execute_cb := 8;
        end;
      9:  //RRC C
        begin
          regC := rrc(regC);
          execute_cb := 8;
        end;
      10: //RRC D
        begin
          setD(rrc(getD));
          execute_cb := 8;
        end;
      11: //RRC E
        begin
          setE(rrc(getE));
          execute_cb := 8;
        end;
      12: //RRC H
        begin
          setH(rrc(getH));
          execute_cb := 8;
        end;
      13: //RRC L
        begin
          setL(rrc(getL));
          execute_cb := 8;
        end;
      14: //RRC (HL)
        begin
          pokeb(regHL, rrc(peekb(regHL)));
          execute_cb := 15;
        end;
      15: //RRC A
        begin
          regA := rrc(regA);
          execute_cb := 8;
        end;
      16: //RL B
        begin
          regB := rl(regB);
          execute_cb := 8;
        end;
      17: //RL C
        begin
          regC := rl(regC);
          execute_cb := 8;
        end;
      18: //RL D
        begin
          setD(rl(getD));
          execute_cb := 8;
        end;
      19: //RL E
        begin
          setE(rl(getE));
          execute_cb := 8;
        end;
      20: //RL H
        begin
          setH(rl(getH));
          execute_cb := 8;
        end;
      21: //RL L
        begin
          setL(rl(getL));
          execute_cb := 8;
        end;
      22: //RL (HL)
        begin
          pokeb(regHL, rl(peekb(regHL)));
          execute_cb := 15;
        end;
      23: //RL A
        begin
          regA := rl(regA);
          execute_cb := 8;
        end;
      24: //RR B
        begin
          regB := rr(regB);
          execute_cb := 8;
        end;
      25: //RR C
        begin
          regC := rr(regC);
          execute_cb := 8;
        end;
      26: //RR D
        begin
          setD(rr(getD));
          execute_cb := 8;
        end;
      27: //RR E
        begin
          setE(rr(getE));
          execute_cb := 8;
        end;
      28: //RR H
        begin
          setH(rr(getH));
          execute_cb := 8;
        end;
      29: //RR L
        begin
          setL(rr(getL));
          execute_cb := 8;
        end;
      30: //RR (HL)
        begin
          pokeb(regHL, rr(peekb(regHL)));
          execute_cb := 15;
        end;
      31: //RR A
        begin
          regA := rr(regA);
          execute_cb := 8;
        end;
      32: //SLA B
        begin
          regB := sla(regB);
          execute_cb := 8;
        end;
      33: //SLA C
        begin
          regC := sla(regC);
          execute_cb := 8;
        end;
      34: //SLA D
        begin
          setD(sla(getD));
          execute_cb := 8;
        end;
      35: //SLA E
        begin
          setE(sla(getE));
          execute_cb := 8;
        end;
      36: //SLA H
        begin
          setH(sla(getH));
          execute_cb := 8;
        end;
      37: //SLA L
        begin
          setL(sla(getL));
          execute_cb := 8
        end;
      38: //SLA (HL)
        begin
          pokeb(regHL, sla(peekb(regHL)));
          execute_cb := 15;
        end;
      39: //SLA A
        begin
          regA := sla(regA);
          execute_cb := 8;
        end;
      40: //SRA B
        begin
          regB := sra(regB);
          execute_cb := 8;
        end;
      41: //SRA C
        begin
          regC := sra(regC);
          execute_cb := 8;
        end;
      42: //SRA D
        begin
          setD(sra(getD));
          execute_cb := 8;
        end;
      43: //SRA E
        begin
          setE(sra(getE));
          execute_cb := 8;
        end;
      44: //SRA H
        begin
          setH(sra(getH));
          execute_cb := 8;
        end;
      45: //SRA L
        begin
          setL(sra(getL));
          execute_cb := 8;
        end;
      46: //SRA (HL)
        begin
          pokeb(regHL, sra(peekb(regHL)));
          execute_cb := 15;
        end;
      47: //SRA A
        begin
          regA := sra(regA);
          execute_cb := 8;
        end;
      48: //SLS B
        begin
          regB := sls(regB);
          execute_cb := 8;
        end;
      49: //SLS C
        begin
          regC := sls(regC);
          execute_cb := 8;
        end;
      50: //SLS D
        begin
          setD(sls(getD));
          execute_cb := 8;
        end;
      51: //SLS E
        begin
          setE(sls(getE));
          execute_cb := 8;
        end;
      52: //SLS H
        begin
          setH(sls(getH));
          execute_cb := 8;
        end;
      53: //SLS L
        begin
          setL(sls(getL));
          execute_cb := 8;
        end;
      54: //SLS (HL)
        begin
          pokeb(regHL, sls(peekb(regHL)));
          execute_cb := 15;
        end;
      55: //SLS A
        begin
          regA := sls(regA);
          execute_cb := 8;
        end;
      56: //SRL B
        begin
          regB := srl(regB);
          execute_cb := 8;
        end;
      57: //SRL C
        begin
          regC := srl(regC);
          execute_cb := 8;
        end;
      58: //SRL D
        begin
          setD(srl(getD));
          execute_cb := 8;
        end;
      59: //SRL E
        begin
          setE(srl(getE));
          execute_cb := 8;
        end;
      60: //SRL H
        begin
          setH(srl(getH));
          execute_cb := 8;
        end;
      61: //SRL L
        begin
          setL(srl(getL));
          execute_cb := 8;
        end;
      62: //SRL (HL)
        begin
          pokeb(regHL, srl(peekb(regHL)));
          execute_cb := 15;
        end;
      63: //SRL A
        begin
          regA := srl(regA);
          execute_cb := 8;
        end;
      64: //BIT 0,B
        begin
          bit($1, regB);
          execute_cb := 8;
        end;
      65: //BIT 0,C
        begin
          bit(1, regC);
          execute_cb := 8;
        end;
      66: //BIT 0,D
        begin
          bit(1, getD);
          execute_cb := 8;
        end;
      67: //BIT 0,E
        begin
          bit(1, getE);
          execute_cb := 8;
        end;
      68: //BIT 0,H
        begin
          bit(1, getH);
          execute_cb := 8;
        end;
      69: //BIT 0,L
        begin
          bit(1, getL);
          execute_cb := 8;
        end;
      70: //BIT 0,(HL)
        begin
          bit_hl(1, peekb(regHL));	//20090118
          execute_cb := 12;
        end;
      71: //BIT 0,A
        begin
          bit(1, regA);
          execute_cb := 8;
        end;
      72: //BIT 1,B
        begin
          bit(2, regB);
          execute_cb := 8;
        end;
      73: //BIT 1,C
        begin
          bit(2, regC);
          execute_cb := 8;
        end;
      74: //BIT 1,D
        begin
          bit(2, getD);
          execute_cb := 8;
        end;
      75: //BIT 1,E
        begin
          bit(2, getE);
          execute_cb := 8;
        end;
      76: //BIT 1,H
        begin
          bit(2, getH);
          execute_cb := 8;
        end;
      77: //BIT 1,L
        begin
          bit(2, getL);
          execute_cb := 8;
        end;
      78: //BIT 1,(HL)
        begin
          bit_hl(2, peekb(regHL));	//20090118
          execute_cb := 12;
        end;
      79: //BIT 1,A
        begin
          bit(2, regA);
          execute_cb := 8;
        end;
      80: //BIT 2,B
        begin
          bit(4, regB);
          execute_cb := 8;
        end;
      81: //BIT 2,C
        begin
          bit(4, regC);
          execute_cb := 8;
        end;
      82: //BIT 2,D
        begin
          bit(4, getD);
          execute_cb := 8;
        end;
      83: //BIT 2,E
        begin
          bit(4, getE);
          execute_cb := 8;
        end;
      84: //BIT 2,H
        begin
          bit(4, getH);
          execute_cb := 8;
        end;
      85: //BIT 2,L
        begin
          bit(4, getL);
          execute_cb := 8;
        end;
      86: //BIT 2,(HL)
        begin
          bit_hl(4, peekb(regHL));	//20090118
          execute_cb := 12;
        end;
      87: //BIT 2,A
        begin
          bit(4, regA);
          execute_cb := 8;
        end;
      88: //BIT 3,B
        begin
          bit(8, regB);
          execute_cb := 8;
        end;
      89: //BIT 3,C
        begin
          bit(8, regC);
          execute_cb := 8;
        end;
      90: //BIT 3,D
        begin
          bit(8, getD);
          execute_cb := 8;
        end;
      91: //BIT 3,E
        begin
          bit(8, getE);
          execute_cb := 8;
        end;
      92: //BIT 3,H
        begin
          bit(8, getH);
          execute_cb := 8;
        end;
      93: //BIT 3,L
        begin
          bit(8, getL);
          execute_cb := 8;
        end;
      94: //BIT 3,(HL)
        begin
          bit_hl(8, peekb(regHL));	//20090118
          execute_cb := 12;
        end;
      95: //BIT 3,A
        begin
          bit(8, regA);
          execute_cb := 8;
        end;
      96: //BIT 4,B
        begin
          bit($10, regB);
          execute_cb := 8;
        end;
      97: //BIT 4,C
        begin
          bit($10, regC);
          execute_cb := 8;
        end;
      98: //BIT 4,D
        begin
          bit($10, getD);
          execute_cb := 8;
        end;
      99: //BIT 4,E
        begin
          bit($10, getE);
          execute_cb := 8;
        end;
      100:  //BIT 4,H
        begin
          bit($10, getH);
          execute_cb := 8;
        end;
      101:  //BIT 4,L
        begin
          bit($10, getL);
          execute_cb := 8;
        end;
      102:  //BIT 4,(HL)
        begin
          bit_hl($10, peekb(regHL));	//20090118
          execute_cb := 12;
        end;
      103:  //BIT 4,A
        begin
          bit($10, regA);
          execute_cb := 8;
        end;
      104:  //BIT 5,B
        begin
          bit($20, regB);
          execute_cb := 8;
        end;
      105:  //BIT 5,C
        begin
          bit($20, regC);
          execute_cb := 8;
        end;
      106:  //BIT 5,D
        begin
          bit($20, getD);
          execute_cb := 8;
        end;
      107:  //BIT 5,E
        begin
          bit($20, getE);
          execute_cb := 8;
        end;
      108:  //BIT 5,H
        begin
          bit($20, getH);
          execute_cb := 8;
        end;
      109:  //BIT 5,L
        begin
          bit($20, getL);
          execute_cb := 8;
        end;
      110:  //BIT 5,(HL)
        begin
          bit_hl($20, peekb(regHL));	//20090118
          execute_cb := 12;
        end;
      111:  //BIT 5,A
        begin
          bit($20, regA);
          execute_cb := 8;
        end;
      112:  //BIT 6,B
        begin
          bit($40, regB);
          execute_cb := 8;
        end;
      113:  //BIT 6,C
        begin
          bit($40, regC);
          execute_cb := 8;
        end;
      114:  //BIT 6,D
        begin
          bit($40, getD);
          execute_cb := 8;
        end;
      115:  //BIT 6,E
        begin
          bit($40, getE);
          execute_cb := 8;
        end;
      116:  //BIT 6,H
        begin
          bit($40, getH);
          execute_cb := 8;
        end;
      117:  //BIT 6,L
        begin
          bit($40, getL);
          execute_cb := 8;
        end;
      118:  //BIT 6,(HL)
        begin
          bit_hl($40, peekb(regHL));	//20090118
          execute_cb := 12;
        end;
      119:  //BIT 6,A
        begin
          bit($40, regA);
          execute_cb := 8;
        end;
      120:  //BIT 7,B
        begin
          bit($80, regB);
          execute_cb := 8;
        end;
      121:  //BIT 7,C
        begin
          bit($80, regC);
          execute_cb := 8;
        end;
      122:  //BIT 7,D
        begin
          bit($80, getD);
          execute_cb := 8;
        end;
      123:  //BIT 7,E
        begin
          bit($80, getE);
          execute_cb := 8;
        end;
      124:  //BIT 7,H
        begin
          bit($80, getH);
          execute_cb := 8;
        end;
      125:  //BIT 7,L
        begin
          bit($80, getL);
          execute_cb := 8;
        end;
      126:  //BIT 7,(HL)
        begin
          bit_hl($80, peekb(regHL));	//20090118
          execute_cb := 12;
        end;
      127:  //BIT 7,A
        begin
          bit($80, regA);
          execute_cb := 8;
        end;
      128:  //RES 0,B
        begin
          regB := bitRes(1, regB);
          execute_cb := 8;
        end;
      129:  //RES 0,C
        begin
          regC := bitRes(1, regC);
          execute_cb := 8;
        end;
      130:  //RES 0,D
        begin
          setD(bitRes(1, getD));
          execute_cb := 8;
        end;
      131:  //RES 0,E
        begin
          setE(bitRes(1, getE));
          execute_cb := 8;
        end;
      132:  //RES 0,H
        begin
          setH(bitRes(1, getH));
          execute_cb := 8;
        end;
      133:  //RES 0,L
        begin
          setL(bitRes(1, getL));
          execute_cb := 8;
        end;
      134:  //RES 0,(HL)
        begin
          pokeb(regHL, bitRes($1, peekb(regHL)));
          execute_cb := 15;
        end;
      135:  //RES 0,A
        begin
          regA := bitRes(1, regA);
          execute_cb := 8;
        end;
      136:  //RES 1,B
        begin
          regB := bitRes(2, regB);
          execute_cb := 8;
        end;
      137:  //RES 1,C
        begin
          regC := bitRes(2, regC);
          execute_cb := 8;
        end;
      138:  //RES 1,D
        begin
          setD(bitRes(2, getD));
          execute_cb := 8;
        end;
      139:  //RES 1,E
        begin
          setE(bitRes(2, getE));
          execute_cb := 8;
        end;
      140:  //RES 1,H
        begin
          setH(bitRes(2, getH));
          execute_cb := 8;
        end;
      141:  //RES 1,L
        begin
          setL(bitRes(2, getL));
          execute_cb := 8;
        end;
      142:  //RES 1,(HL)
        begin
          pokeb(regHL, bitRes(2, peekb(regHL)));
          execute_cb := 15;
        end;
      143:  //RES 1,A
        begin
          regA := bitRes(2, regA);
          execute_cb := 8;
        end;
      144:  //RES 2,B
        begin
          regB := bitRes(4, regB);
          execute_cb := 8;
        end;
      145:  //RES 2,C
        begin
          regC := bitRes(4, regC);
          execute_cb := 8;
        end;
      146:  //RES 2,D
        begin
          setD(bitRes(4, getD));
          execute_cb := 8;
        end;
      147:  //RES 2,E
        begin
          setE(bitRes(4, getE));
          execute_cb := 8;
        end;
      148:  //RES 2,H
        begin
          setH(bitRes(4, getH));
          execute_cb := 8;
        end;
      149:  //RES 2,L
        begin
          setL(bitRes(4, getL));
          execute_cb := 8;
        end;
      150:  //RES 2,(HL)
        begin
          pokeb(regHL, bitRes(4, peekb(regHL)));
          execute_cb := 15;
        end;
      151:  //RES 2,A
        begin
          regA := bitRes(4, regA);
          execute_cb := 8;
        end;
      152:  //RES 3,B
        begin
          regB := bitRes(8, regB);
          execute_cb := 8;
        end;
      153:  //RES 3,C
        begin
          regC := bitRes(8, regC);
          execute_cb := 8;
        end;
      154:  //RES 3,D
        begin
          setD(bitRes(8, getD));
          execute_cb := 8;
        end;
      155:  //RES 3,E
        begin
          setE(bitRes(8, getE));
          execute_cb := 8;
        end;
      156:  //RES 3,H
        begin
          setH(bitRes(8, getH));
          execute_cb := 8;
        end;
      157:  //RES 3,L
        begin
          setL(bitRes(8, getL));
          execute_cb := 8;
        end;
      158:  //RES 3,(HL)
        begin
          pokeb(regHL, bitRes(8, peekb(regHL)));
          execute_cb := 15;
        end;
      159:  //RES 3,A
        begin
          regA := bitRes(8, regA);
          execute_cb := 8;
        end;
      160:  //RES 4,B
        begin
          regB := bitRes($10, regB);
          execute_cb := 8;
        end;
      161:  //RES 4,C
        begin
          regC := bitRes($10, regC);
          execute_cb := 8;
        end;
      162:  //RES 4,D
        begin
          setD(bitRes($10, getD));
          execute_cb := 8;
        end;
      163:  //RES 4,E
        begin
          setE(bitRes($10, getE));
          execute_cb := 8;
        end;
      164:  //RES 4,H
        begin
          setH(bitRes($10, getH));
          execute_cb := 8;
        end;
      165:  //RES 4,L
        begin
          setL(bitRes($10, getL));
          execute_cb := 8;
        end;
      166:  //RES 4,(HL)
        begin
          pokeb(regHL, bitRes($10, peekb(regHL)));
          execute_cb := 15;
        end;
      167:  //RES 4,A
        begin
          regA := bitRes($10, regA);
          execute_cb := 8;
        end;
      168:  //RES 5,B
        begin
          regB := bitRes($20, regB);
          execute_cb := 8;
        end;
      169:  //RES 5,C
        begin
          regC := bitRes($20, regC);
          execute_cb := 8;
        end;
      170:  //RES 5,D
        begin
          setD(bitRes($20, getD));
          execute_cb := 8;
        end;
      171:  //RES 5,E
        begin
          setE(bitRes($20, getE));
          execute_cb := 8;
        end;
      172:  //RES 5,H
        begin
          setH( bitRes($20, getH));
          execute_cb := 8;
        end;
      173:  //RES 5,L
        begin
          setL(bitRes($20, getL));
          execute_cb := 8;
        end;
      174:  //RES 5,(HL)
        begin
          pokeb(regHL, bitRes($20, peekb(regHL)));
          execute_cb := 15;
        end;
      175:  //RES 5,A
        begin
          regA := bitRes($20, regA);
          execute_cb := 8;
        end;
      176:  //RES 6,B
        begin
          regB := bitRes($40, regB);
          execute_cb := 8;
        end;
      177:  //RES 6,C
        begin
          regC := bitRes($40, regC);
          execute_cb := 8;
        end;
      178:  //RES 6,D
        begin
          setD(bitRes($40, getD));
          execute_cb := 8;
        end;
      179:  //RES 6,E
        begin
          setE(bitRes($40, getE));
          execute_cb := 8;
        end;
      180:  //RES 6,H
        begin
          setH(bitRes($40, getH));
          execute_cb := 8;
        end;
      181:  //RES 6,L
        begin
          setL(bitRes($40, getL));
          execute_cb := 8;
        end;
      182:  //RES 6,(HL)
        begin
          pokeb(regHL, bitRes($40, peekb(regHL)));
          execute_cb := 15;
        end;
      183:  //RES 6,A
        begin
          regA := bitRes($40, regA);
          execute_cb := 8;
        end;
      184:  //RES 7,B
        begin
          regB := bitRes($80, regB);
          execute_cb := 8;
        end;
      185:  //RES 7,C
        begin
          regC := bitRes($80, regC);
          execute_cb := 8;
        end;
      186:  //RES 7,D
        begin
          setD(bitRes($80, getD));
          execute_cb := 8;
        end;
      187:  //RES 7,E
        begin
          setE(bitRes($80, getE));
          execute_cb := 8;
        end;
      188:  //RES 7,H
        begin
          setH(bitRes($80, getH));
          execute_cb := 8;
        end;
      189:  //RES 7,L
        begin
          setL(bitRes($80, getL));
          execute_cb := 8;
        end;
      190:  //RES 7,(HL)
        begin
          pokeb(regHL, bitRes($80, peekb(regHL)));
          execute_cb := 15;
        end;
      191:  //RES 7,A
        begin
          regA := bitRes($80, regA);
          execute_cb := 8;
        end;
      192:  //SET 0,B
        begin
          regB := bitSet(1, regB);
          execute_cb := 8;
        end;
      193:  //SET 0,C
        begin
          regC := bitSet(1, regC);
          execute_cb := 8;
        end;
      194:  //SET 0,D
        begin
          setD(bitSet(1, getD));
          execute_cb := 8;
        end;
      195:  //SET 0,E
        begin
          setE(bitSet(1, getE));
          execute_cb := 8;
        end;
      196:  //SET 0,H
        begin
          setH(bitSet(1, getH));
          execute_cb := 8;
        end;
      197:  //SET 0,L
        begin
          setL(bitSet(1, getL));
          execute_cb := 8
        end;
      198:  //SET 0,(HL)
        begin
          pokeb(regHL, bitSet(1, peekb(regHL)));
          execute_cb := 15;
        end;
      199:  //SET 0,A
        begin
          regA := bitSet(1, regA);
          execute_cb := 8;
        end;
      200:  //SET 1,B
        begin
          regB := bitSet(2, regB);
          execute_cb := 8;
        end;
      201:  //SET 1,C
        begin
          regC := bitSet(2, regC);
          execute_cb := 8;
        end;
      202:  //SET 1,D
        begin
          setD(bitSet(2, getD));
          execute_cb := 8;
        end;
      203:  //SET 1,E
        begin
          setE(bitSet(2, getE));
          execute_cb := 8;
        end;
      204:  //SET 1,H
        begin
          setH(bitSet(2, getH));
          execute_cb := 8;
        end;
      205:  //SET 1,L
        begin
          setL(bitSet(2, getL));
          execute_cb := 8;
        end;
      206:  //SET 1,(HL)
        begin
          pokeb(regHL, bitSet(2, peekb(regHL)));
          execute_cb := 15;
        end;
      207:  //SET 1,A
        begin
          regA := bitSet(2, regA);
          execute_cb := 8;
        end;
      208:  //SET 2,B
        begin
          regB := bitSet(4, regB);
          execute_cb := 8;
        end;
      209:  //SET 2,C
        begin
          regC := bitSet(4, regC);
          execute_cb := 8;
        end;
      210:  //SET 2,D
        begin
          setD(bitSet(4, getD));
          execute_cb := 8;
        end;
      211:  //SET 2,E
        begin
          setE(bitSet(4, getE));
          execute_cb := 8;
        end;
      212:  //SET 2,H
        begin
          setH(bitSet(4, getH));
          execute_cb := 8;
        end;
      213:  //SET 2,L
        begin
          setL(bitSet(4, getL));
          execute_cb := 8;
        end;
      214:  //SET 2,(HL)
        begin
          pokeb(regHL, bitSet($4, peekb(regHL)));
          execute_cb := 15;
        end;
      215:  //SET 2,A
        begin
          regA := bitSet(4, regA);
          execute_cb := 8;
        end;
      216:  //SET 3,B
        begin
          regB := bitSet(8, regB);
          execute_cb := 8;
        end;
      217:  //SET 3,C
        begin
          regC := bitSet(8, regC);
          execute_cb := 8;
        end;
      218:  //SET 3,D
        begin
          setD(bitSet(8, getD));
          execute_cb := 8;
        end;
      219:  //SET 3,E
        begin
          setE(bitSet(8, getE));
          execute_cb := 8;
        end;
      220:  //SET 3,H
        begin
          setH(bitSet(8, getH));
          execute_cb := 8;
        end;
      221:  //SET 3,L
        begin
          setL(bitSet(8, getL));
          execute_cb := 8;
        end;
      222:  //SET 3,(HL)
        begin
          pokeb(regHL, bitSet($8, peekb(regHL)));
          execute_cb := 15;
        end;
      223:  //SET 3,A
        begin
          regA := bitSet(8, regA);
          execute_cb := 8;
        end;
      224:  //SET 4,B
        begin
          regB := bitSet($10, regB);
          execute_cb := 8;
        end;
      225:  //SET 4,C
        begin
          regC := bitSet($10, regC);
          execute_cb := 8;
        end;
      226:  //SET 4,D
        begin
          setD(bitSet($10, getD));
          execute_cb := 8;
        end;
      227:  //SET 4,E
        begin
          setE(bitSet($10, getE));
          execute_cb := 8;
        end;
      228:  //SET 4,H
        begin
          setH(bitSet($10, getH));
          execute_cb := 8;
        end;
      229:  //SET 4,L
        begin
          setL(bitSet($10, getL));
          execute_cb := 8;
        end;
      230:  //SET 4,(HL)
        begin
          pokeb(regHL, bitSet($10, peekb(regHL)));
          execute_cb := 15;
        end;
      231:  //SET 4,A
        begin
          regA := bitSet($10, regA);
          execute_cb := 8;
        end;
      232:  //SET 5,B
        begin
          regB := bitSet($20, regB);
          execute_cb := 8;
        end;
      233:  //SET 5,C
        begin
          regC := bitSet($20, regC);
          execute_cb := 8;
        end;
      234:  //SET 5,D
        begin
          setD(bitSet($20, getD));
          execute_cb := 8;
        end;
      235:  //SET 5,E
        begin
          setE(bitSet($20, getE));
          execute_cb := 8;
        end;
      236:  //SET 5,H
        begin
          setH(bitSet($20, getH));
          execute_cb := 8;
        end;
      237:  //SET 5,L
        begin
          setL(bitSet($20, getL));
          execute_cb := 8;
        end;
      238:  //SET 5,(HL)
        begin
          pokeb(regHL, bitSet($20, peekb(regHL)));
          execute_cb := 15;
        end;
      239:  //SET 5,A
        begin
          regA := bitSet($20, regA);
          execute_cb := 8;
        end;
      240:  //SET 6,B
        begin
          regB := bitSet($40, regB);
          execute_cb := 8;
        end;
      241:  //SET 6,C
        begin
          regC := bitSet($40, regC);
          execute_cb := 8;
        end;
      242:  //SET 6,D
        begin
          setD(bitSet($40, getD));
          execute_cb := 8;
        end;
      243:  //SET 6,E
        begin
          setE(bitSet($40, getE));
          execute_cb := 8;
        end;
      244:  //SET 6,H
        begin
          setH(bitSet($40, getH));
          execute_cb := 8;
        end;
      245:  //SET 6,L
        begin
          setL(bitSet($40, getL));
          execute_cb := 8;
        end;
      246:  //SET 6,(HL)
        begin
          pokeb(regHL, bitSet($40, peekb(regHL)));
          execute_cb := 15;
        end;
      247:  //SET 6,A
        begin
          regA := bitSet($40, regA);
          execute_cb := 8;
        end;
      248:  //SET 7,B
        begin
          regB := bitSet($80, regB);
          execute_cb := 8;
        end;
      249:  //SET 7,C
        begin
          regC := bitSet($80, regC);
          execute_cb := 8;
        end;
      250:  //SET 7,D
        begin
          setD(bitSet($80, getD));
          execute_cb := 8;
        end;
      251:  //SET 7,E
        begin
          setE(bitSet($80, getE));
          execute_cb := 8;
        end;
      252:  //SET 7,H
        begin
          setH(bitSet($80, getH));
          execute_cb := 8;
        end;
      253:  //SET 7,L
        begin
          setL(bitSet($80, getL));
          execute_cb := 8;
        end;
      254:  //SET 7,(HL)
        begin
          pokeb(regHL, bitSet($80, peekb(regHL)));
          execute_cb := 15;
        end;
      255:  //SET 7,A
        begin
          regA := bitSet($80, regA);
          execute_cb := 8;
        end;
      Else
        begin
        execute_cb := 8;
        Application.MessageBox(
          PChar('Unknown CB instruction ' + inttostr(xxx) + ' at ' + inttostr(regPC)),
          PChar(Application.Title),
          MB_OK);
        end;
    End;
end;

procedure exx;
    var t : integer;
begin

    t := regHL;
    regHL := regHL_;
    regHL_ := t;

    t := regDE;
    regDE := regDE_;
    regDE_ := t;

    t := getBC;
    setBC(regBC_);
    regBC_ := t;
end;

function id_d : integer;
    var d : integer;
begin
    d := nxtpcb;
    If ((d And 128) = 128) Then d := -(256 - d);
    id_d := (regID + d) And $FFFF;
end;

procedure ld_a_i;
begin
    fS := (intI And F_S) <> 0;
    f3 := (intI And F_3) <> 0;
    f5 := (intI And F_5) <> 0;
    fZ := (intI = 0);
    fPV := intIFF2;
    fH := False;
    fN := False;
    regA := intI;
end;

procedure ld_a_r;
begin
    intRTemp := intRTemp And $7F;
    regA := (intR And $80) Or intRTemp;
    fS := (regA And F_S) <> 0;
    f3 := (regA And F_3) <> 0;
    f5 := (regA And F_5) <> 0;
    fZ := (regA = 0);
    fPV := intIFF2;
    fH := False;
    fN := False;
end;

procedure neg_a;
    var t : integer;
begin
    t := regA;
    regA := 0;
    sub_a(t);
end;

procedure rld_a;
    var ans : integer; t : integer; q : integer;
begin
    ans := regA;
    t := peekb(regHL);
    q := t;

    t := (t * 16) Or (ans And $F);
    ans := (ans And $F0) Or (q shr 4);
    pokeb(regHL, (t And $FF));

    fS := (ans And F_S) <> 0;
    f3 := (ans And F_3) <> 0;
    f5 := (ans And F_5) <> 0;
    fZ := (ans = 0);
    fPV := Parity[ans];
    fH := False;
    fN := False;

    regA := ans;
end;

procedure setIDH(byteval : integer);
begin
    regID := ((byteval {* 256} shl 8) And $FF00) Or (regID And $FF);  // 20061220
end;

procedure setIDL(byteval : integer);
begin
    regID := (regID And $FF00) Or (byteval And $FF);
end;

function in_bc() : integer;
    var ans : integer;
begin
    ans := inb(getBC);

    fZ := (ans = 0);
    fS := (ans And F_S) <> 0;
    f3 := (ans And F_3) <> 0;
    f5 := (ans And F_5) <> 0;
    fPV := Parity[ans];
    fN := False;
    fH := False;

    in_bc := ans;
end;

function inc8(ans : integer) : integer;
begin
    fPV := (ans = $7F);
    fH := (((ans And $F) + 1) And F_H) <> 0;

    ans := (ans + 1) And $FF;

    fS := (ans And F_S) <> 0;
    f3 := (ans And F_3) <> 0;
    f5 := (ans And F_5) <> 0;
    fZ := (ans = 0);
    fN := False;

    inc8 := ans;
end;

function dec8(ans : integer) : integer;
begin
    fPV := (ans = $80);
    fH := (((ans And $F) - 1) And F_H) <> 0;

    ans := (ans - 1) And $FF;

    fS := (ans And F_S) <> 0;
    f3 := (ans And F_3) <> 0;
    f5 := (ans And F_5) <> 0;
    fZ := (ans = 0);

    fN := True;

    dec8 := ans;
end;

function popw: integer;
begin
    popw := peekb(regSP) Or (peekb(regSP + 1) {* 256} shl 8);   // 20061220
    regSP := (regSP + 2 And $FFFF);
end;

procedure poppc;
begin
    regPC := popw;
end;

procedure pushw(word : integer);
begin
    regSP := (regSP - 2) And $FFFF;
    pokew(regSP, word);
end;

procedure pushpc;
begin
    pushw(regPC);
end;

procedure REFRESH(t : integer);
begin
    intRTemp := intRTemp + t;
end;

function qdec8(a : integer) : integer;
begin
    qdec8 := (a - 1) And $FF;
end;

function execute_ed(local_tstates : integer) : integer;
  var
    xxx : integer; count : integer; dest : integer; from : integer;
    TempLocal_tstates : integer; c : Boolean; b : integer;
begin

    intRTemp := intRTemp + 1;

    xxx := nxtpcb;
    Case xxx of
      0..63, 127..159, 164..167, 172..175, 180..183:  //NOP
        execute_ed := 8;
      64: //IN B,(c)
        begin
          regB := in_bc;
          execute_ed := 12;
        end;
      72: //IN C,(c)
        begin
          regC := in_bc();
          execute_ed := 12;
        end;
      80: //IN D,(c)
        begin
          setD(in_bc());
          execute_ed := 12;
        end;
      88: //IN E,(c)
        begin
          setE(in_bc());
          execute_ed := 12;
        end;
      96: //IN H,(c)
        begin
          setH(in_bc());
          execute_ed := 12;
        end;
      104:  //IN L,(c)
        begin
          setL(in_bc());
          execute_ed := 12;
        end;
      112:  //IN (c)
        begin
          in_bc;
          execute_ed := 12;
        end;
      120:  //IN A,(c)
        begin
          regA := in_bc;
          execute_ed := 12;
        end;
      65: //OUT (c),B
        begin
          outb(getBC, regB);
          execute_ed := 12;
        end;
      73: //OUT (c),C
        begin
          outb(getBC, regC);
          execute_ed := 12;
        end;
      81: //OUT (c),D
        begin
          outb(getBC, getD);
          execute_ed := 12;
        end;
      89: //OUT (c),E
        begin
          outb(getBC, getE);
          execute_ed := 12;
        end;
      97: //OUT (c),H
        begin
          outb(getBC, getH);
          execute_ed := 12;
        end;
      105:  //OUT (c),L
        begin
          outb(getBC, getL);
          execute_ed := 12;
        end;
      113:  //OUT (c),0
        begin
          outb(getBC, 0);
          execute_ed := 12;
        end;
      121:  //OUT (c),A
        begin
          outb(getBC, regA);
          execute_ed := 12;
        end;
      66:   //SBC HL,BC
        begin
          regHL := sbc16(regHL, getBC);
          execute_ed := 15;
        end;
      74:   //ADC HL,BC
        begin
          regHL := adc16(regHL, getBC);
          execute_ed := 15;
        end;
      82:   //SBC HL,DE
        begin
          regHL := sbc16(regHL, regDE);
          execute_ed := 15;
        end;
      90: //ADC HL,DE
        begin
          regHL := adc16(regHL, regDE);
          execute_ed := 15;
        end;
      98: //SBC HL,HL
        begin
          regHL := sbc16(regHL, regHL);
          execute_ed := 15;
        end;
      106:  //ADC HL,HL
        begin
          regHL := adc16(regHL, regHL);
          execute_ed := 15;
        end;
      114:  //SBC HL,SP
        begin
          regHL := sbc16(regHL, regSP);
          execute_ed := 15;
        end;
      122:  //ADC HL,SP
        begin
          regHL := adc16(regHL, regSP);
          execute_ed := 15;
        end;
      67:   //LD (nn),BC
        begin
          pokew(nxtpcw, getBC);
          execute_ed := 20;
        end;
      75:   //LD BC,(nn)
        begin
          setBC(peekw(nxtpcw));
          execute_ed := 20;
        end;
      83:   //LD (nn),DE
        begin
          pokew(nxtpcw, regDE);
          execute_ed := 20;
        end;
      91:   //LD DE,(nn)
        begin
          regDE := peekw(nxtpcw);
          execute_ed := 20;
        end;
      99:   //LD (nn),HL
        begin
          pokew(nxtpcw, regHL);
          execute_ed := 20;
        end;
      107:  //LD HL,(nn)
        begin
          regHL := peekw(nxtpcw);
          execute_ed := 20;
        end;
      115:  //LD (nn),SP
        begin
          pokew(nxtpcw, regSP);
          execute_ed := 20;
        end;
      123:  //LD SP,(nn)
        begin
          regSP := peekw(nxtpcw);
          execute_ed := 20;
        end;
      68, 76, 84, 92, 100, 108, 116, 124: //NEG
        begin
          neg_a;
          execute_ed := 8;
        end;
      69, 85, 101, 117: // RETn
        begin
          intIFF1 := intIFF2;
          poppc;
          execute_ed := 14;
        end;
      77, 93, 109, 125: //RETI
        begin
          // TOCHECK: according to the official Z80 docs, IFF2 does not get
          //          copied to IFF1 for RETI - but in a real Z80 it is
          intIFF1 := intIFF2;
          poppc;
          execute_ed := 14;
        end;

      70, 78, 102, 110: //IM 0
        begin
          intIM := 0;
          execute_ed := 8;
        end;
      86, 118:  //IM 1
        begin
          intIM := 1;
          execute_ed := 8;
        end;
      94, 126:  //IM 2
        begin
          intIM := 2;
          execute_ed := 8;
        end;
      71: //LD I,A
        begin
          intI := regA;
          execute_ed := 9;
        end;
      79: //LD R,A
        begin
          intR := regA;
          intRTemp := intR;
          execute_ed := 9;
        end;
      87: //LD A,I
        begin
          ld_a_i;
          execute_ed := 9;
        end;
      95: //LD A,R
        begin
          ld_a_r;
          execute_ed := 9;
        end;
      103:  //RRD
        begin
          rrd_a;
          execute_ed := 18;
        end;
      111:  //RLD
        begin
          rld_a;
          execute_ed := 18;
        end;
      160:  //LDI
        begin
          b:=peekb(regHL) + regA;
          f3:=(b and F_3)<>0;
          f5:=(b and $02)<>0;
          pokeb(regDE, peekb(regHL));
          regDE := inc16(regDE);
          regHL := inc16(regHL);
          setBC(dec16(getBC));
          fPV := (getBC <> 0);
          fH := False;
          fN := False;
          execute_ed := 16;
        end;
      161:  //CPI
        begin
          c := fC;
          cp_a(peekb(regHL));
          b:=(regA-peekb(regHL)-Byte(fH));
          f3:=(b and F_3)<>0;
          f5:=(b and $02)<>0;
          regHL := inc16(regHL);
          setBC(dec16(getBC));
          fPV := (getBC <> 0);
          fC := c;
          execute_ed := 16;
        end;
      162:  //INI
        begin
          pokeb(regHL, inb(getBC));
          b := qdec8(regB);
          regB := b;
          regHL := inc16(regHL);
          fZ := (b = 0);
          fN := True;
          execute_ed := 16;
        end;
      163:  //OUTI
        begin
          b := qdec8(regB);
          regB := b;
          outb(getBC, peekb(regHL));
          regHL := inc16(regHL);
          fZ := (b = 0);
          fN := True;
          execute_ed := 16;
        end;
      168:  //LDD
        begin
          b:=peekb(regHL) + regA;
          f3:=(b and F_3)<>0;
          f5:=(b and $02)<>0;
          pokeb(regDE, peekb(regHL));
          regDE := dec16(regDE);
          regHL := dec16(regHL);
          setBC(dec16(getBC));
          fPV := (getBC <> 0);
          fH := False;
          fN := False;
          execute_ed := 16;
        end;
      169:  //CPD
        begin
          c := fC;
          cp_a(peekb(regHL));
          b:=(regA-peekb(regHL)-Byte(fH));
          f3:=(b and F_3)<>0;
          f5:=(b and $02)<>0;
          regHL := dec16(regHL);
          setBC(dec16(getBC));
          fPV := (getBC <> 0);
          fC := c;
          execute_ed := 16;
        end;
      170:  //IND
        begin
          pokeb(regHL, inb(getBC));
          b := qdec8(regB);
          regB := b;
          regHL := dec16(regHL);
          fZ := (b = 0);
          fN := True;
          execute_ed := 16;
        end;
      171:  //OUTD
        begin
          count := qdec8(regB);
          regB := count;
          outb(getBC, peekb(regHL));
          regHL := dec16(regHL);
          fZ := (count = 0);
          fN := True;
          execute_ed := 16;
        end;
      176:  //LDIR   (repeat until BC=0)  OK
        begin
          b:=peekb(regHL) + regA;
          f3:=(b and F_3)<>0;
          f5:=(b and $02)<>0;
          pokeb(regDE, peekb(regHL));
          regDE := inc16(regDE);
          regHL := inc16(regHL);
          setBC(dec16(getBC));
          fPV := (getBC <> 0);
          fH := False;
          fN := False;
          TempLocal_tstates := 24;
          intRTemp := intRTemp + 1;
          regPC := regPC - 2;
          If getBC = 0 Then
             begin
               regPC := regPC + 2;
               TempLocal_tstates := TempLocal_tstates - 8;
             End;
          execute_ed := TempLocal_tstates;
        end;
      177:  //CPIR   (repeat until BC=0)  OK
        begin
          c := fC;
          cp_a(peekb(regHL));
          b:=(regA-peekb(regHL)-Byte(fH));
          f3:=(b and F_3)<>0;
          f5:=(b and $02)<>0;
          regHL := inc16(regHL);
          setBC(dec16(getBC));
          fPV := (getBC <> 0);
          fC := c;
          intRTemp := intRTemp + 1;
          If fPV And (fZ = False) Then
             begin
               regPC := regPC - 2;
               execute_ed := 24;
             end
             Else
               execute_ed := 16;
        end;
      178:  //INIR   (repeat until B=0)  OK
        begin
          pokeb(regHL, inb(getBC));
          b := qdec8(regB);
          regB := b;
          regHL := inc16(regHL);

          fZ := True;
          fN := True;
          intRTemp := intRTemp + 1;
          If (b <> 0) Then
          begin
            regPC := regPC - 2;
            execute_ed := 24;
          end
          Else
            execute_ed := 16;
        end;
      179:  //OTIR   (repeat until B=0)  OK
        begin
          b := qdec8(regB);
          regB := b;
          outb(getBC, peekb(regHL));
          regHL := inc16(regHL);
          intRTemp := intRTemp + 1;
          fZ := True;
          fN := True;
          If (b <> 0) Then
          begin
            regPC := regPC - 2;
            execute_ed := 24;
          end
          Else
            execute_ed := 16;
        end;
      184:  //LDDR   (repeat until BC=0)  OK
        begin
          b:=peekb(regHL) + regA;
          f3:=(b and F_3)<>0;
          f5:=(b and $02)<>0;
          pokeb(regDE, peekb(regHL));
          regDE := dec16(regDE);
          regHL := dec16(regHL);
          setBC(dec16(getBC));
          fPV := (getBC <> 0);
          fH := False;
          fN := False;
          TempLocal_tstates := 24;
          intRTemp := intRTemp + 1;
          regPC := regPC - 2;
          If getBC = 0 Then
             begin
             regPC := regPC + 2;
             TempLocal_tstates := TempLocal_tstates - 8;
             End;
          execute_ed := TempLocal_tstates;
        end;
      185:  //CPDR   (repeat until BC=0)  OK
        begin
          c := fC;
          cp_a(peekb(regHL));

          b:=(regA-peekb(regHL)-Byte(fH));
          f3:=(b and F_3)<>0;
          f5:=(b and $02)<>0;
          regHL := dec16(regHL);
          setBC(dec16(getBC));
          fPV := (getBC <> 0);
          fC := c;
          intRTemp := intRTemp + 1;
          If (fPV) And (fZ = False) Then
             begin
             regPC := regPC - 2;
             execute_ed := 24;
             end
             Else
             execute_ed := 16;
        end;
      186:  //INDR   (repeat until B=0)  OK
        begin
          pokeb(regHL, inb(getBC));
          b := qdec8(regB);
          regB := b;
          regHL := dec16(regHL);
          intRTemp := intRTemp + 1;
          fZ := True;
          fN := True;
          If (b <> 0) Then
          begin
            regPC := regPC - 2;
            execute_ed := 24;
          end
          Else
            execute_ed := 16;
        end;
      187:  //OTDR   (repeat until B=0)  OK
        begin
          b := qdec8(regB);
          regB := b;
          outb(getBC, peekb(regHL));
          regHL := dec16(regHL);
          intRTemp := intRTemp + 1;
          fZ := True;
          fN := True;
          If (b <> 0) Then
          begin
            regPC := regPC - 2;
            execute_ed := 24;
          end
          Else
            execute_ed := 16;
        end;
      Else
      begin
        Application.MessageBox(PChar('Unknown ED instruction ' + inttostr(xxx) + ' at ' + inttostr(regPC)),
          PChar(Application.Title),
          MB_OK);
        execute_ed := 8;
      end;
    End;
end;

procedure execute_id_cb(op : integer;  z : integer);
begin
    Case op of
      0:  //RLC B
        begin
          op := rlc(peekb(z));
          regB := op;
          pokeb(z, op);
        end;
      1:  //RLC C
        begin
          op := rlc(peekb(z));
          regC := op;
          pokeb(z, op);
        end;
      2:  //RLC D
        begin
          op := rlc(peekb(z));
          setD(op);
          pokeb(z, op);
        end;
      3:  //RLC E
        begin
          op := rlc(peekb(z));
          setE(op);
          pokeb(z, op);
        end;
      4:  //RLC H
        begin
          op := rlc(peekb(z));
          setH(op);
          pokeb(z, op);
        end;
      5:  //RLC L
        begin
          op := rlc(peekb(z));
          setL(op);
          pokeb(z, op);
        end;
      6:  //RLC (HL)
        pokeb(z, rlc(peekb(z)));
      7:  //RLC A
        begin
          op := rlc(peekb(z));
          regA := op;
          pokeb(z, op);
        end;
      8:  //RRC B
        begin
          op := rrc(peekb(z));
          regB := op;
          pokeb(z, op);
        end;
      9:  //RRC C
        begin
          op := rrc(peekb(z));
          regC := op;
          pokeb(z, op);
        end;
      10:  // RRC D
        begin
          op := rrc(peekb(z));
          setD(op);
          pokeb(z, op);
        end;
      11: //RRC E
        begin
          op := rrc(peekb(z));
          setE(op);
          pokeb(z, op);
        end;
      12: //RRC H
        begin
          op := rrc(peekb(z));
          setH(op);
          pokeb(z, op);
        end;
      13: //RRC L
        begin
          op := rrc(peekb(z));
          setL(op);
          pokeb(z, op);
        end;
      14: //RRC (HL)
        pokeb(z, rrc(peekb(z)));
      15: //RRC A
        begin
          op := rrc(peekb(z));
          regA := op;
          pokeb(z, op);
        end;
      16: //RL B
        begin
          op := rl(peekb(z));
          regB := op;
          pokeb(z, op);
        end;
      17: //RL C
        begin
          op := rl(peekb(z));
          regC := op;
          pokeb(z, op);
        end;
      18: //RL D
        begin
          op := rl(peekb(z));
          setD(op);
          pokeb(z, op);
        end;
      19: //RL E
        begin
          op := rl(peekb(z));
          setE(op);
          pokeb(z, op);
        end;
      20: //RL H
        begin
          op := rl(peekb(z));
          setH(op);
          pokeb(z, op);
        end;
      21: //RL L
        begin
          op := rl(peekb(z));
          setL(op);
          pokeb(z, op);
        end;
      22: //RL (HL)
        pokeb(z, rl(peekb(z)));
      23: //RL A
        begin
          op := rl(peekb(z));
          regA := op;
          pokeb(z, op);
        end;
      24: //RR B
        begin
          op := rr(peekb(z));
          regB := op;
          pokeb(z, op);
        end;
      25: //RR C
        begin
          op := rr(peekb(z));
          regC := op;
          pokeb(z, op);
        end;
      26: //RR D
        begin
          op := rr(peekb(z));
          setD(op);
          pokeb(z, op);
        end;
      27: //RR E
        begin
          op := rr(peekb(z));
          setE(op);
          pokeb(z, op);
        end;
      28: //RR H
        begin
          op := rr(peekb(z));
          setH(op);
          pokeb(z, op);
        end;
      29: //RR L
        begin
          op := rr(peekb(z));
          setL(op);
          pokeb(z, op);
        end;
      30: //RR (HL)
        pokeb(z, rr(peekb(z)));
      31: //RR A
        begin
          op := rr(peekb(z));
          regA := op;
          pokeb(z, op);
        end;
      32: //SLA B
        begin
          op := sla(peekb(z));
          regB := op;
          pokeb(z, op);
        end;
      33: //SLA C
        begin
          op := sla(peekb(z));
          regC := op;
          pokeb(z, op);
        end;
      34: //SLA D
        begin
          op := sla(peekb(z));
          setD(op);
          pokeb(z, op);
        end;
      35: //SLA E
        begin
          op := sla(peekb(z));
          setE(op);
          pokeb(z, op);
        end;
      36: //SLA H
        begin
          op := sla(peekb(z));
          setH(op);
          pokeb(z, op);
        end;
      37: //SLA L
        begin
          op := sla(peekb(z));
          setL(op);
          pokeb(z, op);
        end;
      38: //SLA (HL)
        pokeb(z, sla(peekb(z)));
      39: //SLA A
        begin
          op := sla(peekb(z));
          regA := op;
          pokeb(z, op);
        end;
      40: //SRA B
        begin
          op := sra(peekb(z));
          regB := op;
          pokeb(z, op);
        end;
      41: //SRA C
        begin
          op := sra(peekb(z));
          regC := op;
          pokeb(z, op);
        end;
      42: //SRA D
        begin
          op := sra(peekb(z));
          setD(op);
          pokeb(z, op);
        end;
      43: //SRA E
        begin
          op := sra(peekb(z));
          setE(op);
          pokeb(z, op);
        end;
      44: //SRA H
        begin
          op := sra(peekb(z));
          setH(op);
          pokeb(z, op);
        end;
      45: //SRA L
        begin
          op := sra(peekb(z));
          setL(op);
          pokeb(z, op);
        end;
      46: //SRA (HL)
        pokeb(z, sra(peekb(z)));
      47: //SRA A
        begin
          op := sra(peekb(z));
          regA := op;
          pokeb(z, op);
        end;
      48: //SLS B
        begin
          op := sls(peekb(z));
          regB := op;
          pokeb(z, op);
        end;
      49: //SLS C
        begin
          op := sls(peekb(z));
          regC := op;
          pokeb(z, op);
        end;
      50: //SLS D
        begin
          op := sls(peekb(z));
          setD(op);
          pokeb(z, op);
        end;
      51: //SLS E
        begin
          op := sls(peekb(z));
          setE(op);
          pokeb(z, op);
        end;
      52: //SLS H
        begin
          op := sls(peekb(z));
          setH(op);
          pokeb(z, op);
        end;
      53: //SLS L
        begin
          op := sls(peekb(z));
          setL(op);
          pokeb(z, op);
        end;
      54: //SLS (HL)
        pokeb(z, sls(peekb(z)));
      55: //SLS A
        begin
          op := sls(peekb(z));
          regA := op;
          pokeb(z, op);
        end;
      56: //SRL B
        begin
          op := srl(peekb(z));
          regB := op;
          pokeb(z, op);
        end;
      57: //SRL C
        begin
          op := srl(peekb(z));
          regC := op;
          pokeb(z, op);
        end;
      58: //SRL D
        begin
          op := srl(peekb(z));
          setD(op);
          pokeb(z, op);
        end;
      59: //SRL E
        begin
          op := srl(peekb(z));
          setE(op);
          pokeb(z, op);
        end;
      60: //SRL H
        begin
          op := srl(peekb(z));
          setH(op);
          pokeb(z, op);
        end;
      61: //SRL L
        begin
          op := srl(peekb(z));
          setL(op);
          pokeb(z, op);
        end;
      62: //SRL (ID)
        pokeb(z, srl(peekb(z)));
      63: //SRL A
        begin
          op := srl(peekb(z));
          regA := op;
          pokeb(z, op);
        end;
      64..71: //BIT 0,B
        bit_id($1, peekb(z), z);  //20090118
      72..79: //BIT 1,B
        bit_id($2, peekb(z), z);  //20090118
      80..87: //BIT 2,B
        bit_id($4, peekb(z), z);  //20090118
      88..95: //BIT 3,B
        bit_id($8, peekb(z), z);  //20090118
      96..103:  //BIT 4,B
        bit_id($10, peekb(z), z);  //20090118
      104..111:  //BIT 5,B
        bit_id($20, peekb(z), z);  //20090118
      112..119: //BIT 6,B
        bit_id($40, peekb(z), z);  //20090118
      120..127: //BIT 7,B
        bit_id($80, peekb(z), z);  //20090118
      128:  //RES 0,(ID+y)->B
        begin
          regB := bitRes(1, peekb(z));
          pokeb(z, regB);
        end;
      129:  //RES 0,(ID+y)->C
        begin
          regC := bitRes(1, peekb(z));
          pokeb(z, regC);
        end;
      130:  //RES 0,(ID+y)->D
        begin
          setD(bitRes(1, peekb(z)));
          pokeb(z, getD);
        end;
      131:  //RES 0,(ID+y)->E
        begin
          setE(bitRes(1, peekb(z)));
          pokeb(z, getE);
        end;
      132:  //RES 0,(ID+y)->H
        begin
          setH(bitRes(1, peekb(z)));
          pokeb(z, getH);
        end;
      133:  //RES 0,(ID+y)->L
        begin
          setL(bitRes(1, peekb(z)));
          pokeb(z, getL);
        end;
      134:  //RES 0,(HL)
        pokeb(z, bitRes($1, peekb(z)));
      135:  //RES 0,(ID+y)->A
        begin
          regA := bitRes(1, peekb(z));
          pokeb(z, regA);
        end;
      136:  //RES 1,(ID+y)->B
        begin
          regB := bitRes(2, peekb(z));
          pokeb(z, regB);
        end;
      137:  //RES 1,(ID+y)->C
        begin
          regC := bitRes(2, peekb(z));
          pokeb(z, regC);
        end;
      138:  //RES 1,(ID+y)->D
        begin
          setD(bitRes(2, peekb(z)));
          pokeb(z, getD);
        end;
      139:  //RES 1,(ID+y)->E
        begin
          setE(bitRes(2, peekb(z)));
          pokeb(z, getE);
        end;
      140:  //RES 1,(ID+y)->H
        begin
          setH(bitRes(2, peekb(z)));
          pokeb(z, getH);
        end;
      141:  //RES 1,(ID+y)->L
        begin
          setL(bitRes(2, peekb(z)));
          pokeb(z, getL);
        end;
      142:  //RES 1,(HL)
        pokeb(z, bitRes($2, peekb(z)));
      143:  //RES 1,(ID+y)->A
        begin
          regA := bitRes(2, peekb(z));
          pokeb(z, regA);
        end;
      144:  //RES 2,(ID+y)->B
        begin
          regB := bitRes(4, peekb(z));
          pokeb(z, regB);
        end;
      145:  //RES 2,(ID+y)->C
        begin
          regC := bitRes(4, peekb(z));
          pokeb(z, regC);
        end;
      146:  //RES 2,(ID+y)->D
        begin
          setD(bitRes(4, peekb(z)));
          pokeb(z, getD);
        end;
      147:  //RES 2,(ID+y)->E
        begin
          setE(bitRes(4, peekb(z)));
          pokeb(z, getE);
        end;
      148:  //RES 2,(ID+y)->H
        begin
          setH(bitRes(4, peekb(z)));
          pokeb(z, getH);
        end;
      149:  //RES 2,(ID+y)->L
        begin
          setL(bitRes(4, peekb(z)));
          pokeb(z, getL);
        end;
      150:  //RES 2,(HL)
        pokeb(z, bitRes($4, peekb(z)));
      151:  //RES 2,(ID+y)->A
        begin
          regA := bitRes(4, peekb(z));
          pokeb(z, regA);
        end;
      152:  //RES 3,(ID+y)->B
        begin
          regB := bitRes(8, peekb(z));
          pokeb(z, regB);
        end;
      153:  //RES 3,(ID+y)->C
        begin
          regC := bitRes(8, peekb(z));
          pokeb(z, regC);
        end;
      154:  //RES 3,(ID+y)->D
        begin
          setD(bitRes(8, peekb(z)));
          pokeb(z, getD);
        end;
      155:  //RES 3,(ID+y)->E
        begin
          setE(bitRes(8, peekb(z)));
          pokeb(z, getE);
        end;
      156:  //RES 3,(ID+y)->H
        begin
          setH(bitRes(8, peekb(z)));
          pokeb(z, getH);
        end;
      157:  //RES 3,(ID+y)->L
        begin
          setL(bitRes(8, peekb(z)));
          pokeb(z, getL);
        end;
      158:  //RES 3,(HL)
        pokeb(z, bitRes($8, peekb(z)));
      159:  //RES 3,(ID+y)->A
        begin
          regA := bitRes(8, peekb(z));
          pokeb(z, regA);
        end;
      160:  //RES 4,(ID+y)->B
        begin
          regB := bitRes($10, peekb(z));
          pokeb(z, regB);
        end;
      161:  //RES 4,(ID+y)->C
        begin
          regC := bitRes($10, peekb(z));
          pokeb(z, regC);
        end;
      162:  //RES 4,(ID+y)->D
        begin
          setD(bitRes($10, peekb(z)));
          pokeb(z, getD);
        end;
      163:  //RES 4,(ID+y)->E
        begin
          setE(bitRes($10, peekb(z)));
          pokeb(z, getE);
        end;
      164:  //RES 4,(ID+y)->H
        begin
          setH(bitRes($10, peekb(z)));
          pokeb(z, getH);
        end;
      165:  //RES 4,(ID+y)->L
        begin
          setL(bitRes($10, peekb(z)));
          pokeb(z, getL);
        end;
      166:  //RES 4,(HL)
        pokeb(z, bitRes($10, peekb(z)));
      167:  //RES 4,(ID+y)->A
        begin
          regA := bitRes($10, peekb(z));
          pokeb(z, regA);
        end;
      168:  //RES 5,(ID+y)->B
        begin
          regB := bitRes($20, peekb(z));
          pokeb(z, regB);
        end;
      169:  //RES 5,(ID+y)->C
        begin
          regC := bitRes($20, peekb(z));
          pokeb(z, regC);
        end;
      170:  //RES 5,(ID+y)->D
        begin
          setD(bitRes($20, peekb(z)));
          pokeb(z, getD);
        end;
      171:  //RES 5,(ID+y)->E
        begin
          setE(bitRes($20, peekb(z)));
          pokeb(z, getE);
        end;
      172:  //RES 5,(ID+y)->H
        begin
          setH(bitRes($20, peekb(z)));
          pokeb(z, getH);
        end;
      173:  //RES 5,(ID+y)->L
        begin
          setL(bitRes($20, peekb(z)));
          pokeb(z, getL);
        end;
      174:  //RES 5,(HL)
        pokeb(z, bitRes($20, peekb(z)));
      175:  //RES 5,(ID+y)->A
        begin
          regA := bitRes($20, peekb(z));
          pokeb(z, regA);
        end;
      176:  //RES 6,(ID+y)->B
        begin
          regB := bitRes($40, peekb(z));
          pokeb(z, regB);
        end;
      177:  //RES 6,(ID+y)->C
        begin
          regC := bitRes($40, peekb(z));
          pokeb(z, regC);
        end;
      178:  //RES 6,(ID+y)->D
        begin
          setD(bitRes($40, peekb(z)));
          pokeb(z, getD);
        end;
      179:  //RES 6,(ID+y)->E
        begin
          setE(bitRes($40, peekb(z)));
          pokeb(z, getE);
        end;
      180:  //RES 6,(ID+y)->H
        begin
          setH(bitRes($40, peekb(z)));
          pokeb(z, getH);
        end;
      181:  //RES 6,(ID+y)->L
        begin
          setL(bitRes($40, peekb(z)));
          pokeb(z, getL);
        end;
      182:  //RES 6,(HL)
        pokeb(z, bitRes($40, peekb(z)));
      183:  //RES 6,(ID+y)->A
        begin
          regA := bitRes($40, peekb(z));
          pokeb(z, regA);
        end;
      184:  //RES 6,(ID+y)->B
        begin
          regB := bitRes($80, peekb(z));
          pokeb(z, regB);
        end;
      185:  //RES 6,(ID+y)->C
        begin
          regC := bitRes($80, peekb(z));
          pokeb(z, regC);
        end;
      186:  //RES 6,(ID+y)->D
        begin
          setD(bitRes($80, peekb(z)));
          pokeb(z, getD);
        end;
      187:  //RES 6,(ID+y)->E
        begin
          setE(bitRes($80, peekb(z)));
          pokeb(z, getE);
        end;
      188:  //RES 6,(ID+y)->H
        begin
          setH(bitRes($80, peekb(z)));
          pokeb(z, getH);
        end;
      189:  //RES 6,(ID+y)->L
        begin
          setL(bitRes($80, peekb(z)));
          pokeb(z, getL);
        end;
      190:  //RES 7,(HL)
        pokeb(z, bitRes($80, peekb(z)));
      191:  //RES 7,(ID+y)->A
        begin
          regA := bitRes($80, peekb(z));
          pokeb(z, regA);
        end;
      192:  //SET 0,(ID+y)->B
        begin
          regB := bitSet(1, peekb(z));
          pokeb(z, regB);
        end;
      193:  //SET 0,(ID+y)->C
        begin
          regC := bitSet(1, peekb(z));
          pokeb(z, regC);
        end;
      194:  //SET 0,(ID+y)->D
        begin
          setD(bitSet(1, peekb(z)));
          pokeb(z, getD);
        end;
      195:  //SET 0,(ID+y)->E
        begin
          setE(bitSet(1, peekb(z)));
          pokeb(z, getE);
        end;
      196:  //SET 0,(ID+y)->H
        begin
          setH(bitSet(1, peekb(z)));
          pokeb(z, getH);
        end;
      197:  //SET 0,(ID+y)->L
        begin
          setL(bitSet(1, peekb(z)));
          pokeb(z, getL);
        end;
      198:  //SET 0,(HL)
        pokeb(z, bitSet($1, peekb(z)));
      199:  //SET 0,(ID+y)->A
        begin
          regA := bitSet(1, peekb(z));
          pokeb(z, regA);
        end;
      200:  //SET 1,(ID+y)->B
        begin
          regB := bitSet(2, peekb(z));
          pokeb(z, regB);
        end;
      201:  //SET 1,(ID+y)->C
        begin
          regC := bitSet(2, peekb(z));
          pokeb(z, regC);
        end;
      202:  //SET 1,(ID+y)->D
        begin
          setD(bitSet(2, peekb(z)));
          pokeb(z, getD);
        end;
      203:  //SET 1,(ID+y)->E
        begin
          setE(bitSet(2, peekb(z)));
          pokeb(z, getE);
        end;
      204:  //SET 1,(ID+y)->H
        begin
          setH(bitSet(2, peekb(z)));
          pokeb(z, getH);
        end;
      205:  //SET 1,(ID+y)->L
        begin
          setL(bitSet(2, peekb(z)));
          pokeb(z, getL);
        end;
      206:  //SET 1,(HL)
        pokeb(z, bitSet($2, peekb(z)));
      207:  //SET 1,(ID+y)->A
        begin
          regA := bitSet(2, peekb(z));
          pokeb(z, regA);
        end;
      208:  //SET 2,(ID+y)->B
        begin
          regB := bitSet(4, peekb(z));
          pokeb(z, regB);
        end;
      209:  //SET 2,(ID+y)->C
        begin
          regC := bitSet(4, peekb(z));
          pokeb(z, regC);
        end;
      210:  //SET 2,(ID+y)->D
        begin
          setD(bitSet(4, peekb(z)));
          pokeb(z, getD);
        end;
      211:  //SET 2,(ID+y)->E
        begin
          setE(bitSet(4, peekb(z)));
          pokeb(z, getE);
        end;
      212:  //SET 2,(ID+y)->H
        begin
          setH(bitSet(4, peekb(z)));
          pokeb(z, getH);
        end;
      213:  //SET 2,(ID+y)->L
        begin
          setL(bitSet(4, peekb(z)));
          pokeb(z, getL);
        end;
      214:  //SET 2,(HL)
        pokeb(z, bitSet($4, peekb(z)));
      215:  //SET 2,(ID+y)->A
        begin
          regA := bitSet(4, peekb(z));
          pokeb(z, regA);
        end;
      216:  //SET 3,(ID+y)->B
        begin
          regB := bitSet(8, peekb(z));
          pokeb(z, regB);
        end;
      217:  //SET 3,(ID+y)->C
        begin
          regC := bitSet(8, peekb(z));
          pokeb(z, regC);
        end;
      218:  //SET 3,(ID+y)->D
        begin
          setD(bitSet(8, peekb(z)));
          pokeb(z, getD);
        end;
      219:  //SET 3,(ID+y)->E
        begin
          setE(bitSet(8, peekb(z)));
          pokeb(z, getE);
        end;
      220:  //SET 3,(ID+y)->H
        begin
          setH(bitSet(8, peekb(z)));
          pokeb(z, getH);
        end;
      221:  //SET 3,(ID+y)->L
        begin
          setL(bitSet(8, peekb(z)));
          pokeb(z, getL);
        end;
      222:  //SET 3,(HL)
        pokeb(z, bitSet($8, peekb(z)));
      223:  //SET 3,(ID+y)->A
        begin
          regA := bitSet(8, peekb(z));
          pokeb(z, regA);
        end;
      224:  //SET 4,(ID+y)->B
        begin
          regB := bitSet($10, peekb(z));
          pokeb(z, regB);
        end;
      225:  //SET 4,(ID+y)->C
        begin
          regC := bitSet($10, peekb(z));
          pokeb(z, regC);
        end;
      226:  //SET 4,(ID+y)->D
        begin
          setD(bitSet($10, peekb(z)));
          pokeb(z, getD);
        end;
      227:  //SET 4,(ID+y)->E
        begin
          setE(bitSet($10, peekb(z)));
          pokeb(z, getE);
        end;
      228:  //SET 4,(ID+y)->H
        begin
          setH(bitSet($10, peekb(z)));
          pokeb(z, getH);
        end;
      229:  //SET 4,(ID+y)->L
        begin
          setL(bitSet($10, peekb(z)));
          pokeb(z, getL);
        end;
      230:  //SET 4,(HL)
        pokeb(z, bitSet($10, peekb(z)));
      231:  //SET 4,(ID+y)->A
        begin
          regA := bitSet($10, peekb(z));
          pokeb(z, regA);
        end;
      232:  //SET 5,(ID+y)->B
        begin
          regB := bitSet($20, peekb(z));
          pokeb(z, regB);
        end;
      233:  //SET 5,(ID+y)->C
        begin
          regC := bitSet($20, peekb(z));
          pokeb(z, regC);
        end;
      234:  //SET 5,(ID+y)->D
        begin
          setD(bitSet($20, peekb(z)));
          pokeb(z, getD);
        end;
      235:  //SET 5,(ID+y)->E
        begin
          setE(bitSet($20, peekb(z)));
          pokeb(z, getE);
        end;
      236:  //SET 5,(ID+y)->H
        begin
          setH(bitSet($20, peekb(z)));
          pokeb(z, getH);
        end;
      237:  //SET 5,(ID+y)->L
        begin
          setL(bitSet($20, peekb(z)));
          pokeb(z, getL);
        end;
      238:  //SET 5,(HL)
        pokeb(z, bitSet($20, peekb(z)));
      239:  //SET 5,(ID+y)->A
        begin
          regA := bitSet($20, peekb(z));
          pokeb(z, regA);
        end;
      240:  //SET 6,(ID+y)->B
        begin
          regB := bitSet($40, peekb(z));
          pokeb(z, regB);
        end;
      241:  //SET 6,(ID+y)->C
        begin
          regC := bitSet($40, peekb(z));
          pokeb(z, regC);
        end;
      242:  //SET 6,(ID+y)->D
        begin
          setD(bitSet($40, peekb(z)));
          pokeb(z, getD);
        end;
      243:  //SET 6,(ID+y)->E
        begin
          setE(bitSet($40, peekb(z)));
          pokeb(z, getE);
        end;
      244:  //SET 6,(ID+y)->H
        begin
          setH(bitSet($40, peekb(z)));
          pokeb(z, getH);
        end;
      245:  //SET 6,(ID+y)->L
        begin
          setL(bitSet($40, peekb(z)));
          pokeb(z, getL);
        end;
      246:  //SET 6,(HL)
        pokeb(z, bitSet($40, peekb(z)));
      247:  //SET 6,(ID+y)->A
        begin
          regA := bitSet($40, peekb(z));
          pokeb(z, regA);
        end;
      248:  //SET 7,(ID+y)->B
        begin
          regB := bitSet($80, peekb(z));
          pokeb(z, regB);
        end;
      249:  //SET 7,(ID+y)->C
        begin
          regC := bitSet($80, peekb(z));
          pokeb(z, regC);
        end;
      250:  //SET 7,(ID+y)->D
        begin
          setD(bitSet($80, peekb(z)));
          pokeb(z, getD);
        end;
      251:  //SET 7,(ID+y)->E
        begin
          setE(bitSet($80, peekb(z)));
          pokeb(z, getE);
        end;
      252:  //SET 7,(ID+y)->H
        begin
          setH(bitSet($80, peekb(z)));
          pokeb(z, getH);
        end;
      253:  //SET 7,(ID+y)->L
        begin
          setL(bitSet($80, peekb(z)));
          pokeb(z, getL);
        end;
      254:  //SET 7,(HL)
        pokeb(z, bitSet($80, peekb(z)));
      255:  //SET 7,A
        begin
          regA := bitSet($80, peekb(z));
          pokeb(z, regA);
        end;
    End;
end;


function UARTinterrupt : integer;
begin
    Result:=0;
    If intIFF1 = False Then
      UARTinterrupt := 0
    Else begin
       Case intIM of
          0, 1:
            begin
              pushpc;
//              intIFF1 := False;
              intIFF2 := False;
              regPC := 56;
              UARTinterrupt := 13;
            end;
          2:
            begin
              pushpc;
//              intIFF1 := False;
              intIFF2 := False;
              regPC := (intI {* 256} shl 8) + $FD;   // Default UART IM2 vector
              regPC := peekw(regPC);
              UARTinterrupt := 19;
            end;
          else
              UARTinterrupt := 0;
       End;
       FUART.IntCount:=FUART.IntCount-1;
    end;
end;


function specinterrupt : integer;
    var lSleep : integer;
{$IFDEF USE_SOUND}
        lCounter: integer;
        PDst : Pointer;
        PSrc : Pointer;
{$ENDIF}
begin
    Result:=0;
    interruptCounter := interruptCounter + 1;
    if (glTstatesPerInterrupt shr 2) + glBeeperCounter < 0 then
      glBeeperVal:=128;
    // If it's a maskable interrupt
    If intIFF1 = False Then
      specinterrupt := 0
    Else if (Z80CardMode>Z80CARD_MOSCOW) and ((MainPort[$FB] and pFB_int50_mask)=pFB_int50_on) then
    begin
{      if (Z80CardMode>=Z80_ORIONPRO_v2) then          // TEMPORARY !!!
        if peekw($F000)<>$F2D0 then begin
            CPUPaused:=true;
            frmMain.CPUSuspend;
        end;                                          // TEMPORARY !!!
}       Case intIM of
          0, 1:
            begin
              pushpc;
//              intIFF1 := False;
              intIFF2 := False;
              regPC := 56;
              specinterrupt := 13;
            end;
          2:
            begin
              pushpc;
//              intIFF1 := False;
              intIFF2 := False;
              if (Z80CardMode>=Z80_ORIONPRO_v2) then
                regPC := (intI {* 256} shl 8) and $FF00   
              else
                regPC := (intI {* 256} shl 8) Or $FF;   // 20061220
              regPC := peekw(regPC);
              specinterrupt := 19;
            end;
          else
              specinterrupt := 0;
        End;
    End;

    {$IFDEF USE_SOUND}
    If SoundEnabled Then
    begin
        glBufNum := glBufNum + 1;
        For lCounter := glWavePtr to WAV_BUFFER_SIZE do
            gcWaveOut[lCounter] := gcWaveOut[glWavePtr - 1];

        PDst := gtWavHdr[glBufNum].lpData;
        pSrc := @gcWaveOut[0];

        CopyMemory(PDst, PSrc, WAV_BUFFER_SIZE);

        waveOutWrite(glphWaveOut,
          @gtWavHdr[glBufNum], sizeof(gtWavHdr[glBufNum]));
          
        If glBufNum = NUM_WAV_BUFFERS Then glBufNum := 0;
    End;
    glWavePtr := 0;
    {$ENDIF}

    //Now we REALLY hog the processor
    //Application.ProcessMessages;

    //Keep the emulation running at the correct speed by
    // adding a delay to ensure that interrupts are
    // generated at the correct frequency
    lSleep := glInterruptTimer - integer(timeGetTime()) + glDelayOverage;
    If lSleep < 0 Then
    begin
        If glDelayOverage < -40 Then
            glDelayOverage := -40
        Else
            glDelayOverage := lSleep;
    end;

    If lSleep > 0 Then
    begin
        Sleep(lSleep);
        glDelayOverage := glDelayOverage + (glInterruptDelay - lSleep);
        If glDelayOverage > 0 Then glDelayOverage := 0;
    End
    else
      lSleep:=0;

    // PERFORM Z80 hardware functions

    if (interruptCounter mod 50 = 0) then
    begin
      F146818.update_1_second;
      PortF600.Flush;
      glTstatesPerInterrupt := GetCPUTstates();         // CPUTstates[MIN(CPUSpeedMode, SPEED_INF)];
      if Assigned(AfterOneSecond) then
        AfterOneSecond((xSleep * 100) div 1000);
      xSleep:=0;
    end
    else
    if (interruptCounter mod 20 = 0) then
    begin
      if Assigned(AfterHalfSecond) then
        AfterHalfSecond(0);
    end
    else
      inc(xSleep, lSleep);

    glInterruptTimer := integer(timeGetTime()) + glInterruptDelay;
end;

function interruptTriggered(tstates : integer) : Boolean;
begin
    interruptTriggered := (tstates >= 0);
end;

procedure or_a(b : integer);
begin
    regA := (regA Or b);

    fS := (regA And F_S) <> 0;
    f3 := (regA And F_3) <> 0;
    f5 := (regA And F_5) <> 0;
    fH := False;
    fPV := Parity[regA];
    fZ := (regA = 0);
    fN := False;
    fC := False;
end;


function execute_id: integer;
     var xxx : integer; lTemp : integer; op : integer;
begin

    intRTemp := intRTemp + 1;

    xxx := nxtpcb;

    Case xxx of
      0..8, 10..24, 26..32, 39, 40, 47..51, 55, 56, 58..67:
        begin
          regPC := dec16(regPC);
          intRTemp := intRTemp - 1;
          execute_id := 4;
        end;
      71..75, 79..83, 87..91, 95, 120..123, 127..131:
        begin
          regPC := dec16(regPC);
          intRTemp := intRTemp - 1;
          execute_id := 4;
        end;
      135..139, 143..147, 151..155, 159..163, 167..171:
        begin
          regPC := dec16(regPC);
          intRTemp := intRTemp - 1;
          execute_id := 4;
        end;
      175..179, 183..187, 191..202, 204..224, 226, 228:
        begin
          regPC := dec16(regPC);
          intRTemp := intRTemp - 1;
          execute_id := 4;
        end;
      230..232, 234..248:
        begin
          regPC := dec16(regPC);
          intRTemp := intRTemp - 1;
          execute_id := 4;
        end;
      9:  //ADD ID,BC
        begin
          regID := add16(regID, getBC);
          execute_id := 15;
        end;
      25: //ADD ID,DE
        begin
          regID := add16(regID, regDE);
          execute_id := 15;
        end;
      41: //ADD ID,ID
        begin
          lTemp := regID;
          regID := add16(lTemp, lTemp);
          execute_id := 15;
        end;
      57: //ADD ID,SP
        begin
          regID := add16(regID, regSP);
          execute_id := 15;
        end;
      33: //LD ID,nn
        begin
          regID := nxtpcw;
          execute_id := 14;
        end;
      34: //LD (nn),ID
        begin
          pokew(nxtpcw, regID);
          execute_id := 20;
        end;
      42: //LD ID,(nn)
        begin
          regID := peekw(nxtpcw);
          execute_id := 20;
        end;
      35: //INC ID
        begin
          regID := inc16(regID);
          execute_id := 10;
        end;
      43: //DEC ID
        begin
          regID := dec16(regID);
          execute_id := 10;
        end;
      36: //INC IDH
        begin
          setIDH(inc8(getIDH));
          execute_id := 9;
        end;
      44: //INC IDL
        begin
          setIDL(inc8(getIDL));
          execute_id := 9;
        end;
      52: //INC (ID+d)
        begin
          lTemp := id_d;
          pokeb(lTemp, inc8(peekb(lTemp)));
          execute_id := 23;
        end;
      37: //DEC IDH
        begin
          setIDH(dec8(getIDH));
          execute_id := 9;
        end;
      45: //DEC IDL
        begin
          setIDL(dec8(getIDL));
          execute_id := 9;
        end;
      53: //DEC (ID+d)
        begin
          lTemp := id_d;
          pokeb(lTemp, dec8(peekb(lTemp)));
          execute_id := 23;
        end;
      38: //LD IDH,n
        begin
          setIDH(nxtpcb);
          execute_id := 11;
        end;
      46: //LD IDL,n
        begin
          setIDL(nxtpcb);
          execute_id := 11;
        end;
      54: //LD (ID+d),n
        begin
          lTemp := id_d;
          pokeb(lTemp, nxtpcb);
          execute_id := 19;
        end;
      68: //LD B,IDH
        begin
          regB := getIDH;
          execute_id := 9;
        end;
      69: //LD B,IDL
        begin
          regB := getIDL;
          execute_id := 9;
        end;
      70: //LD B,(ID+d)
        begin
        regB := peekb(id_d);
        execute_id := 19;
        end;
      76: //LD C,IDH
        begin
          regC := getIDH;
          execute_id := 9;
        end;
      77: //LD C,IDL
        begin
          regC := getIDL;
          execute_id := 9;
        end;
      78: //LD C,(ID+d)
        begin
          regC := peekb(id_d);
          execute_id := 19;
        end;
      84: //LD D,IDH
        begin
          setD(getIDH);
          execute_id := 9;
        end;
      85: //LD D,IDL
        begin
          setD(getIDL);
          execute_id := 9;
        end;
      86: //LD D,(ID+d)
        begin
          setD(peekb(id_d));
          execute_id := 19;
        end;
      92: //LD E,IDH
        begin
          setE(getIDH);
          execute_id := 9;
        end;
      93: //LD E,IDL
        begin
          setE(getIDL);
          execute_id := 9;
        end;
      94: //LD E,(ID+d)
        begin
          setE(peekb(id_d));
          execute_id := 19;
        end;
      96: //LD IDH,B
        begin
          setIDH(regB);
          execute_id := 9;
        end;
      97: //LD IDH,C
        begin
          setIDH(regC);
          execute_id := 9;
        end;
      98: //LD IDH,D
        begin
          setIDH(getD);
          execute_id := 9;
        end;
      99: //LD IDH,E
        begin
          setIDH(getE);
          execute_id := 9;
        end;
      100:  //LD IDH,IDH
        execute_id := 9;
      101:  //LD IDH,IDL
        begin
          setIDH(getIDL);
          execute_id := 9;
        end;
      102:  //LD H,(ID+d)
        begin
          setH(peekb(id_d));
          execute_id := 19;
        end;
      103:  //LD IDH,A
        begin
          setIDH(regA);
          execute_id := 9;
        end;
      104:  //LD IDL,B
        begin
          setIDL(regB);
          execute_id := 9;
        end;
      105:  //LD IDL,C
        begin
          setIDL(regC);
          execute_id := 9;
        end;
      106:  //LD IDL,D
        begin
          setIDL(getD);
          execute_id := 9;
        end;
      107:  //LD IDL,E
        begin
          setIDL(getE);
          execute_id := 9;
        end;
      108:  //LD IDL,IDH
        begin
          setIDL(getIDH);
          execute_id := 9;
        end;
      109:  //LD IDL,IDL
        execute_id := 9;
      110:  //LD L,(ID+d)
        begin
          setL(peekb(id_d));
          execute_id := 19;
        end;
      111:  //LD IDL,A
        begin
          setIDL(regA);
          execute_id := 9;
        end;
      112:  //LD (ID+d),B
        begin
          pokeb(id_d, regB);
          execute_id := 19;
        end;
      113:  //LD (ID+d),C
        begin
          pokeb(id_d, regC);
          execute_id := 19;
        end;
      114:  //LD (ID+d),D
        begin
          pokeb(id_d, getD);
          execute_id := 19;
        end;
      115:  //LD (ID+d),E
        begin
          pokeb(id_d, getE);
          execute_id := 19;
        end;
      116:  //LD (ID+d),H
        begin
          pokeb(id_d, getH);
          execute_id := 19;
        end;
      117:  //LD (ID+d),L
        begin
          pokeb(id_d, getL);
          execute_id := 19;
        end;
      119:  //LD (ID+d),A
        begin
          pokeb(id_d, regA);
          execute_id := 19;
        end;
      124:  //LD A,IDH
        begin
          regA := getIDH;
          execute_id := 9;
        end;
      125:  //LD A,IDL
        begin
          regA := getIDL;
          execute_id := 9;
        end;
      126:  //LD A,(ID+d)
        begin
          regA := peekb(id_d);
          execute_id := 19;
        end;
      132:  //ADD A,IDH
        begin
          add_a(getIDH);
          execute_id := 9;
        end;
      133:  //ADD A,IDL
        begin
          add_a(getIDL);
          execute_id := 9;
        end;
      134:  //ADD A,(ID+d)
        begin
          add_a(peekb(id_d));
          execute_id := 19;
        end;

      140:  //ADC A,IDH
        begin
          adc_a(getIDH);
          execute_id := 9;
        end;
      141:  //ADC A,IDL
        begin
          adc_a(getIDL);
          execute_id := 9;
        end;
      142:  //ADC A,(ID+d)
        begin
          adc_a(peekb(id_d));
          execute_id := 19;
        end;
      148:  //SUB IDH
        begin
          sub_a(getIDH);
          execute_id := 9;
        end;
      149:  //SUB IDL
        begin
          sub_a(getIDL);
          execute_id := 9;
        end;
      150:  //SUB (ID+d)
        begin
          sub_a(peekb(id_d));
          execute_id := 19;
        end;
      156:  //SBC A,IDH
        begin
          sbc_a(getIDH);
          execute_id := 9;
        end;
      157:  //SBC A,IDL
        begin
          sbc_a(getIDL);
          execute_id := 9;
        end;
      158:  //SBC A,(ID+d)
        begin
          sbc_a(peekb(id_d));
          execute_id := 19;
        end;
      164:  //AND IDH
        begin
          and_a(getIDH);
          execute_id := 9;
        end;
      165:  //AND IDL
        begin
          and_a(getIDL);
          execute_id := 9;
        end;
      166:  //AND (ID+d)
        begin
          and_a(peekb(id_d));
          execute_id := 19;
        end;
      172:  //XOR IDH
        begin
          xor_a(getIDH);
          execute_id := 9;
        end;
      173:  //XOR IDL
        begin
          xor_a(getIDL);
          execute_id := 9;
        end;
      174:  //OR (ID+d)
        begin
          xor_a(peekb(id_d));
          execute_id := 19;
        end;
      180:  //OR IDH
        begin
          or_a(getIDH);
          execute_id := 9;
        end;
      181:  //OR IDL
        begin
          or_a(getIDL);
          execute_id := 9;
        end;
      182:  //OR (ID+d)
        begin
          or_a(peekb(id_d));
          execute_id := 19;
        end;
      188:  //CP IDH
        begin
          cp_a(getIDH);
          execute_id := 9;
        end;
      189:  //CP IDL
        begin
          cp_a(getIDL);
          execute_id := 9;
        end;
      190:  //CP (ID+d)
        begin
          cp_a(peekb(id_d));
          execute_id := 19;
        end;
      203:  //prefix CB
        begin
          lTemp := id_d;
          op := nxtpcb;
          execute_id_cb(op, lTemp);
          If ((op And $C0) = $40) Then execute_id := 20 Else execute_id := 23;
        end;
      225:  //POP ID
        begin
          regID := popw;
          execute_id := 14;
        end;
      227:  //EX (SP),ID
        begin
          lTemp := regID;
          regID := peekw(regSP);
          pokew(regSP, lTemp);
          execute_id := 23;
        end;
      229:  //PUSH ID
        begin
          pushw(regID);
          execute_id := 15;
        end;
      233:  //JP ID
        begin
          regPC := regID;
          execute_id := 8;
        end;
      249:  //LD SP,ID
        begin
          regSP := regID;
          execute_id := 10;
        end;
      Else
      begin
        execute_id := 8;
        Application.MessageBox(
          PChar('Unknown ID instruction ' + inttostr(xxx) + ' at ' + inttostr(regPC)),
          PChar(Application.Title),
          MB_OK);
      end;
    End;
end;

procedure execute(var local_tstates: integer);
    var
      bbb: byte;
      intCtrTemp : integer;
      d : integer; lTemp : integer;
      xxx : integer;
    {$IFDEF USE_SOUND}
    lOldTstates : integer;
    {$ENDIF}
begin

    //Execute one interrupt duration
    intCtrTemp := interruptCounter;

    repeat
{$IFDEF DEBUG}
      PrevLocalTstates:=local_tstates;
{$ENDIF}
      prevPC:=regPC;
      If (local_tstates >= 0) Then  // Trigger an interrupt
      begin
{$IFDEF USE_DEBUGGING}
          ReportDebug('Interrupt', 0, true);
{$ENDIF}

          local_tstates := local_tstates - glTstatesPerInterrupt;
          local_tstates := local_tstates - specinterrupt();
      end;

      intRTemp := intRTemp + 1;

      {$IFDEF USE_SOUND}
      lOldTstates := local_tstates;
      {$ENDIF}

      if FUART.Exists and FUART.IntMode and (FUART.IntCount>0) then
      begin
        while FUART.IntDataReaded>FUART.IntCount do
          bbb:=FUART.Port0;                                     // 20100420
        local_tstates := local_tstates + UARTinterrupt();
      end;

      xxx := nxtpcb;
      Case xxx of
        0:  //NOP
          local_tstates := local_tstates + 4;
        8:  //EX AF,AF'
          begin
            ex_af_af;
            local_tstates := local_tstates + 4;
          end;
        16: //DJNZ dis
          begin
            lTemp := qdec8(regB);

            regB := lTemp;
            If lTemp <> 0 Then
            begin
              d := nxtpcb;
              If (d And 128) = 128 Then
                d := -(256 - d);

              regPC := (regPC + d) And $FFFF;
              local_tstates := local_tstates + 13;
            end
            Else
            begin
              regPC := inc16(regPC);
              local_tstates := local_tstates + 8;
            End;
          end;
        24: //JR dis
          begin
            d := nxtpcb;
            If (d And 128) = 128 Then
              d := -(256 - d);
                
            regPC := (regPC + d) And $FFFF;
            local_tstates := local_tstates + 12;
          end;
        32: //JR NZ dis
          begin
            If fZ = False Then
            begin
              d := nxtpcb;
              If (d And 128) = 128 Then
                d := -(256 - d);

              regPC := ((regPC + d) And $FFFF);
              local_tstates := local_tstates + 12;
            end
            Else
            begin
              regPC := inc16(regPC);
              local_tstates := local_tstates + 7;
            End;
          end;
        40: //JR Z dis
          begin
            If fZ = True Then
            begin
              d := nxtpcb;
              If (d And 128) = 128  Then
                d := -(256 - d);

              regPC := ((regPC + d) And $FFFF);
              local_tstates := local_tstates + 12;
            end
            Else
            begin
              regPC := inc16(regPC);
              local_tstates := local_tstates + 7;
            End;
          end;
        48: //JR NC dis
          begin
            If fC = False Then
            begin
              d := nxtpcb;
              If (d And 128) = 128 Then
                d := -(256 - d);

              regPC := ((regPC + d) And $FFFF);
              local_tstates := local_tstates + 12;
            end
            Else
            begin
              regPC := inc16(regPC);
              local_tstates := local_tstates + 7;
            End;
          end;
        56: //JR C dis
          begin
            If fC = True Then
            begin
              d := nxtpcb;
              If (d And 128) = 128 Then
                d := -(256 - d);

              regPC := ((regPC + d) And $FFFF);
              local_tstates := local_tstates + 12;
            end
            Else
            begin
              regPC := inc16(regPC);
              local_tstates := local_tstates + 7;
            End;
          end;
        1:  //LD BC,nn
          begin
            setBC(nxtpcw);
            local_tstates := local_tstates + 10;
          end;
        9:  //ADD HL,BC
          begin
            regHL := add16(regHL, getBC);
            local_tstates := local_tstates + 11;
          end;
        17: //LD DE,nn
          begin
            regDE := nxtpcw;
            local_tstates := local_tstates + 10;
          end;
        25: //ADD HL,DE
          begin
            regHL := add16(regHL, regDE);
            local_tstates := local_tstates + 11;
          end;
        33: //LD HL,nn
          begin
            regHL := nxtpcw;
            local_tstates := local_tstates + 10;
          end;
        41: //ADD HL,HL
          begin
            regHL := add16(regHL, regHL);
            local_tstates := local_tstates + 11;
          end;
        49: //LD SP,nn
          begin
            regSP := nxtpcw;
            local_tstates := local_tstates + 10;
          end;
        57: //ADD HL,SP
          begin
            regHL := add16(regHL, regSP);
            local_tstates := local_tstates + 11;
          end;
        2:  //LD (BC),A
          begin
            pokeb(getBC, regA);
            local_tstates := local_tstates + 7;
          end;
        10: //LD A,(BC)
          begin
            regA := peekb(getBC);
            local_tstates := local_tstates + 7;
          end;
        18: //LD (DE),A
          begin
            pokeb(regDE, regA);
            local_tstates := local_tstates + 7;
          end;
        26: //LD A,(DE)
          begin
            regA := peekb(regDE);
            local_tstates := local_tstates + 7;
          end;
        34: //LD (nn),HL
          begin
            pokew(nxtpcw, regHL);
            local_tstates := local_tstates + 16;
          end;
        42: //LD HL,(nn)
          begin
            regHL := peekw(nxtpcw);
            local_tstates := local_tstates + 16;
          end;
        50: //LD (nn),A
          begin
            pokeb(nxtpcw, regA);
            local_tstates := local_tstates + 13;
          end;
        58: //LD A,(nn)
          begin
            regA := peekb(nxtpcw);
            local_tstates := local_tstates + 13;
          end;
        3:  //INC BC
          begin
            setBC(inc16(getBC));
            local_tstates := local_tstates + 6
          end;
        11: //DEC BC
          begin
            setBC(dec16(getBC));
            local_tstates := local_tstates + 6;
          end;
        19: //INC DE
          begin
            regDE := inc16(regDE);
            local_tstates := local_tstates + 6;
          end;
        27: //DEC DE
          begin
            regDE := dec16(regDE);
            local_tstates := local_tstates + 6;
          end;
        35: //INC HL
          begin
            regHL := inc16(regHL);
            local_tstates := local_tstates + 6;
          end;
        43: //DEC HL
          begin
            regHL := dec16(regHL);
            local_tstates := local_tstates + 6;
          end;
        51: //INC SP
          begin
            regSP := inc16(regSP);
            local_tstates := local_tstates + 6;
          end;
        59: //DEC SP
          begin
            regSP := dec16(regSP);
            local_tstates := local_tstates + 6;
          end;
        4:  //INC B
          begin
            regB := inc8(regB);
            local_tstates := local_tstates + 4;
          end;
        12: //INC C
          begin
            regC := inc8(regC);
            local_tstates := local_tstates + 4;
          end;
        20: //INC D
          begin
            setD(inc8(getD));
            local_tstates := local_tstates + 4;
          end;
        28: //INC E
          begin
            setE(inc8(getE));
            local_tstates := local_tstates + 4;
          end;
        36: //INC H
          begin
            setH(inc8(getH));
            local_tstates := local_tstates + 4;
          end;
        44: //INC L
          begin
            setL(inc8(getL));
            local_tstates := local_tstates + 4;
          end;
        52: //INC (HL)
          begin
            pokeb(regHL, inc8(peekb(regHL)));
            local_tstates := local_tstates + 11;
          end;
        60: //INC A
          begin
            regA := inc8(regA);
            local_tstates := local_tstates + 4;
          end;

        5:  //DEC B
          begin
            regB := dec8(regB);
            local_tstates := local_tstates + 4;
          end;
        13: //DEC C
          begin
            regC := dec8(regC);
            local_tstates := local_tstates + 4;
          end;
        21: //DEC D
          begin
            setD(dec8(getD));
            local_tstates := local_tstates + 4;
          end;
        29: //DEC E
          begin
            setE(dec8(getE));
            local_tstates := local_tstates + 4;
          end;
        37: //DEC H
          begin
            setH(dec8(getH));
            local_tstates := local_tstates + 4;
          end;
        45: //DEC L
          begin
            setL(dec8(getL));
            local_tstates := local_tstates + 4;
          end;
        53: //DEC (HL)
          begin
            pokeb(regHL, dec8(peekb(regHL)));
            local_tstates := local_tstates + 11;
          end;
        61: //DEC A
          begin
            regA := dec8(regA);
            local_tstates := local_tstates + 4;
          end;
        
        6 : //D B,n
          begin
            regB := nxtpcb;
            local_tstates := local_tstates + 7;
          end;
        14: //LD C,n
          begin
            regC := nxtpcb;
            local_tstates := local_tstates + 7;
          end;
        22: //LD D,n
          begin
            setD(nxtpcb);
            local_tstates := local_tstates + 7;
          end;
        30: //LD E,n
          begin
            setE(nxtpcb);
            local_tstates := local_tstates + 7;
          end;
        38: //LD H,n
          begin
            setH(nxtpcb);
            local_tstates := local_tstates + 7;
          end;
        46: //LD L,n
          begin
            setL(nxtpcb);
            local_tstates := local_tstates + 7;
          end;
        54: //LD (HL),n
          begin
            pokeb(regHL, nxtpcb);
            local_tstates := local_tstates + 10;
          end;
        62: //LD A,n
          begin
            regA := nxtpcb;
            local_tstates := local_tstates + 7;
          end;

        7 : //LCA
          begin
            rlc_a;
            local_tstates := local_tstates + 4;
          end;
        15: //RRCA
          begin
            rrc_a;
            local_tstates := local_tstates + 4;
          end;
        23: //RLA
          begin
            rl_a;
            local_tstates := local_tstates + 4;
          end;
        31: //RRA
          begin
            rr_a;
            local_tstates := local_tstates + 4;
          end;
        39: //DAA
          begin
            daa_a;
            local_tstates := local_tstates + 4;
          end;
        47: //CPL
          begin
            cpl_a;
            local_tstates := local_tstates + 4;
          end;
        55: //SCF
          begin
            scf;
            local_tstates := local_tstates + 4;
          end;
        63: //CCF
          begin
            ccf;
            local_tstates := local_tstates + 4;
          end;
        64: //LD B,B
          local_tstates := local_tstates + 4;
        65: //LD B,C
          begin
            regB := regC;
            local_tstates := local_tstates + 4;
          end;
        66: //LD B,D
          begin
            regB := getD;
            local_tstates := local_tstates + 4;
          end;
        67: //LD B,E
          begin
            regB := getE;
            local_tstates := local_tstates + 4;
          end;
        68: //LD B,H
          begin
            regB := getH;
            local_tstates := local_tstates + 4;
          end;
        69: //LD B,L
          begin
            regB := getL;
            local_tstates := local_tstates + 4;
          end;
        70: //LD B,(HL)
          begin
            regB := peekb(regHL);
            local_tstates := local_tstates + 7;
          end;
        71: //LD B,A
          begin
            regB := regA;
            local_tstates := local_tstates + 4;
          end;

        72: //LD C,B
          begin
            regC := regB;
            local_tstates := local_tstates + 4;
          end;
        73: //LD C,C
          local_tstates := local_tstates + 4;
        74: //LD C,D
          begin
            regC := getD;
            local_tstates := local_tstates + 4;
          end;
        75: //LD C,E
          begin
            regC := getE;
            local_tstates := local_tstates + 4;
          end;
        76: //LD C,H
          begin
            regC := getH;
            local_tstates := local_tstates + 4;
          end;
        77: //LD C,L
          begin
            regC := getL;
            local_tstates := local_tstates + 4;
          end;
        78: //LD C,(HL)
          begin
            regC := peekb(regHL);
            local_tstates := local_tstates + 7;
          end;
        79: //LD C,A
          begin
            regC := regA;
            local_tstates := local_tstates + 4;
          end;
        80: //LD D,B
          begin
            setD(regB);
            local_tstates := local_tstates + 4;
          end;
        81: //LD D,C
          begin
            setD(regC);
            local_tstates := local_tstates + 4;
          end;
        82: //LD D,D
          local_tstates := local_tstates + 4;
        83: //LD D,E
          begin
            setD(getE);
            local_tstates := local_tstates + 4;
          end;
        84: //LD D,H
          begin
            setD(getH);
            local_tstates := local_tstates + 4;
          end;
        85: //LD D,L
          begin
            setD(getL);
            local_tstates := local_tstates + 4;
          end;
        86: //LD D,(HL)
          begin
            setD(peekb(regHL));
            local_tstates := local_tstates + 7;
          end;
        87: //LD D,A
          begin
            setD(regA);
            local_tstates := local_tstates + 4;
          end;
        88: //LD E,B
          begin
            setE(regB);
            local_tstates := local_tstates + 4;
          end;
        89: //LD E,C
          begin
            setE(regC);
            local_tstates := local_tstates + 4;
          end;
        90: //LD E,D
          begin
            setE(getD);
            local_tstates := local_tstates + 4;
          end;
        91: //LD E,E
          local_tstates := local_tstates + 4;
        92: //LD E,H
          begin
            setE(getH);
            local_tstates := local_tstates + 4;
          end;
        93: //LD E,L
          begin
            setE(getL);
            local_tstates := local_tstates + 4;
          end;
        94: //LD E,(HL)
          begin
            setE(peekb(regHL));
            local_tstates := local_tstates + 7;
          end;
        95: //LD E,A
          begin
            setE(regA);
            local_tstates := local_tstates + 4;
          end;
        96: //LD H,B
          begin
            setH(regB);
            local_tstates := local_tstates + 4;
          end;
        97: //LD H,C
          begin
            setH(regC);
            local_tstates := local_tstates + 4;
          end;
        98: //LD H,D
          begin
            setH(getD);
            local_tstates := local_tstates + 4;
          end;
        99: //LD H,E
          begin
            setH(getE);
            local_tstates := local_tstates + 4;
          end;
        100:  //LD H,H
          local_tstates := local_tstates + 4;
        101:  //LD H,L
          begin
            setH(getL);
            local_tstates := local_tstates + 4;
          end;
        102:  //LD H,(HL)
          begin
            setH(peekb(regHL));
            local_tstates := local_tstates + 7;
          end;
        103:  //LD H,A
          begin
            setH(regA);
            local_tstates := local_tstates + 4;
          end;
        104:  //LD L,B
          begin
            setL(regB);
            local_tstates := local_tstates + 4;
          end;
        105:  //LD L,C
          begin
            setL(regC);
            local_tstates := local_tstates + 4;
          end;
        106:  //LD L,D
          begin
            setL(getD);
            local_tstates := local_tstates + 4;
          end;
        107:  //LD L,E
          begin
            setL(getE);
            local_tstates := local_tstates + 4;
          end;
        108:  //LD L,H
          begin
            setL(getH);
            local_tstates := local_tstates + 4;
          end;
        109:  //LD L,L
          local_tstates := local_tstates + 4;
        110:  //LD L,(HL)
          begin
            setL(peekb(regHL));
            local_tstates := local_tstates + 7;
          end;
        111:  //LD L,A
          begin
            setL(regA);
            local_tstates := local_tstates + 4;
          end;
        112:  //LD (HL),B
          begin
            pokeb(regHL, regB);
            local_tstates := local_tstates + 7;
          end;
        113:  //LD (HL),C
          begin
            pokeb(regHL, regC);
            local_tstates := local_tstates + 7;
          end;
        114:  //LD (HL),D
          begin
            pokeb(regHL, getD);
            local_tstates := local_tstates + 7;
          end;
        115:  //LD (HL),E
          begin
            pokeb(regHL, getE);
            local_tstates := local_tstates + 7;
          end;
        116:  //LD (HL),H
          begin
            pokeb(regHL, getH);
            local_tstates := local_tstates + 7;
          end;
        117:  //LD (HL),L
          begin
            pokeb(regHL, getL);
            local_tstates := local_tstates + 7;
          end;
        118:  //HALT
          begin
            lTemp := (((-local_tstates - 1) shr 2) + 1);
            local_tstates := local_tstates + (lTemp * 4);
            intRTemp := intRTemp + (lTemp - 1)
          end;
        119:  //LD (HL),A
          begin
            pokeb(regHL, regA);
            local_tstates := local_tstates + 7;
          end;
        120:  //LD A,B
          begin
            regA := regB;
            local_tstates := local_tstates + 4;
          end;
        121:  //LD A,C
          begin
            regA := regC;
            local_tstates := local_tstates + 4;
          end;
        122:  //LD A,D
          begin
            regA := getD;
            local_tstates := local_tstates + 4;
          end;
        123:  //LD A,E
          begin
            regA := getE;
            local_tstates := local_tstates + 4;
          end;
        124:  //LD A,H
          begin
            regA := getH;
            local_tstates := local_tstates + 4;
          end;
        125:  //LD A,L
          begin
            regA := getL;
            local_tstates := local_tstates + 4;
          end;
        126:  //LD A,(HL)
          begin
            regA := peekb(regHL);
            local_tstates := local_tstates + 7;
          end;
        127:  //LD A,A
          local_tstates := local_tstates + 4;
        128:  //ADD A,B
          begin
            add_a(regB);
            local_tstates := local_tstates + 4;
          end;
        129:  //ADD A,C
          begin
            add_a(regC);
            local_tstates := local_tstates + 4;
          end;
        130:  //ADD A,D
          begin
            add_a(getD);
            local_tstates := local_tstates + 4;
          end;
        131:  //ADD A,E
          begin
            add_a(getE);
            local_tstates := local_tstates + 4;
          end;
        132:  //ADD A,H
          begin
            add_a(getH);
            local_tstates := local_tstates + 4;
          end;
        133:  //ADD A,L
          begin
            add_a(getL);
            local_tstates := local_tstates + 4;
          end;
        134:  //ADD A,(HL)
          begin
            add_a(peekb(regHL));
            local_tstates := local_tstates + 7;
          end;
        135:  //ADD A,A
          begin
            add_a(regA);
            local_tstates := local_tstates + 4;
          end;
        136:  //ADC A,B
          begin
            adc_a(regB);
            local_tstates := local_tstates + 4;
          end;
        137:  //ADC A,C
          begin
            adc_a(regC);
            local_tstates := local_tstates + 4;
          end;
        138:  //ADC A,D
          begin
            adc_a(getD);
            local_tstates := local_tstates + 4;
          end;
        139:  //ADC A,E
          begin
            adc_a(getE);
            local_tstates := local_tstates + 4;
          end;
        140:  //ADC A,H
          begin
            adc_a(getH);
            local_tstates := local_tstates + 4;
          end;
        141:  //ADC A,L
          begin
            adc_a(getL);
            local_tstates := local_tstates + 4;
          end;
        142:  //ADC A,(HL)
          begin
            adc_a(peekb(regHL));
            local_tstates := local_tstates + 7;
          end;
        143:  //ADC A,A
          begin
            adc_a(regA);
            local_tstates := local_tstates + 4;
          end;
        144:  //SUB B
          begin
            sub_a(regB);
            local_tstates := local_tstates + 4;
          end;
        145:  //SUB C
          begin
            sub_a(regC);
            local_tstates := local_tstates + 4;
          end;
        146:  //SUB D
          begin
            sub_a(getD);
            local_tstates := local_tstates + 4;
          end;
        147:  //SUB E
          begin
            sub_a(getE);
            local_tstates := local_tstates + 4;
          end;
        148:  //SUB H
          begin
            sub_a(getH);
            local_tstates := local_tstates + 4;
          end;
        149:  //SUB L
          begin
            sub_a(getL);
            local_tstates := local_tstates + 4;
          end;
        150:  //SUB (HL)
          begin
            sub_a(peekb(regHL));
            local_tstates := local_tstates + 7;
          end;
        151:  //SUB A
          begin
            sub_a(regA);
            local_tstates := local_tstates + 4;
          end;
        152:  //SBC A,B
          begin
            sbc_a(regB);
            local_tstates := local_tstates + 4;
          end;
        153:  //SBC A,C
          begin
            sbc_a(regC);
            local_tstates := local_tstates + 4;
          end;
        154:  //SBC A,D
          begin
            sbc_a(getD);
            local_tstates := local_tstates + 4;
          end;
        155:  //SBC A,E
          begin
            sbc_a(getE);
            local_tstates := local_tstates + 4;
          end;
        156:  //SBC A,H
          begin
            sbc_a(getH);
            local_tstates := local_tstates + 4;
          end;
        157:  //SBC A,L
          begin
            sbc_a(getL);
            local_tstates := local_tstates + 4;
          end;
        158:  //SBC A,(HL)
          begin
            sbc_a(peekb(regHL));
            local_tstates := local_tstates + 7;
          end;
        159:  //SBC A,A
          begin
            sbc_a(regA);
            local_tstates := local_tstates + 4;
          end;
        160:  //AND B
          begin
            and_a(regB);
            local_tstates := local_tstates + 4;
          end;
        161:  //AND C
          begin
            and_a(regC);
            local_tstates := local_tstates + 4;
          end;
        162:  //AND D
          begin
            and_a(getD);
            local_tstates := local_tstates + 4;
          end;
        163:  //AND E
          begin
            and_a(getE);
            local_tstates := local_tstates + 4;
          end;
        164:  //AND H
          begin
            and_a(getH);
            local_tstates := local_tstates + 4;
          end;
        165:  //AND L
          begin
            and_a(getL);
            local_tstates := local_tstates + 4;
          end;
        166:  //AND (HL)
          begin
            and_a(peekb(regHL));
            local_tstates := local_tstates + 7;
          end;
        167:  //AND A
          begin
            and_a(regA);
            local_tstates := local_tstates + 4;
          end;
        168:  //XOR B
          begin
            xor_a(regB);
            local_tstates := local_tstates + 4;
          end;
        169:  //XOR C
          begin
            xor_a(regC);
            local_tstates := local_tstates + 4;
          end;
        170:  //XOR D
          begin
            xor_a(getD);
            local_tstates := local_tstates + 4;
          end;
        171:  //XOR E
          begin
            xor_a(getE);
            local_tstates := local_tstates + 4;
          end;
        172:  //XOR H
          begin
            xor_a(getH);
            local_tstates := local_tstates + 4;
          end;
        173:  //XOR L
          begin
            xor_a(getL);
            local_tstates := local_tstates + 4;
          end;
        174:  //XOR (HL)
          begin
            xor_a(peekb(regHL));
            local_tstates := local_tstates + 7;
          end;
        175:  //XOR A
          begin
            regA := 0;
            fS := False;
            f3 := False;
            f5 := False;
            fH := False;
            fPV := True;
            fZ := True;
            fN := False;
            fC := False;
            local_tstates := local_tstates + 4;
          end;
        176:  //OR B
          begin
            or_a(regB);
            local_tstates := local_tstates + 4;
          end;
        177:  //OR C
          begin
            or_a(regC);
            local_tstates := local_tstates + 4;
          end;
        178:  //OR D
          begin
            or_a(getD);
            local_tstates := local_tstates + 4;
          end;
        179:  //OR E
          begin
            or_a(getE);
            local_tstates := local_tstates + 4;
          end;
        180:  //OR H
          begin
            or_a(getH);
            local_tstates := local_tstates + 4;
          end;
        181:  //OR L
          begin
            or_a(getL);
            local_tstates := local_tstates + 4;
          end;
        182:  //OR (HL)
          begin
            or_a(peekb(regHL));
            local_tstates := local_tstates + 7;
          end;
        183:  //OR A
          begin
            or_a(regA);
            local_tstates := local_tstates + 4;
          end;
        184:  //CP B
          begin
            cp_a(regB);
            local_tstates := local_tstates + 4;
          end;
        185:  //CP C
          begin
            cp_a(regC);
            local_tstates := local_tstates + 4;
          end;
        186:  //CP D
          begin
            cp_a(getD);
            local_tstates := local_tstates + 4;
          end;
        187:  //CP E
          begin
            cp_a(getE);
            local_tstates := local_tstates + 4;
          end;
        188:  //CP H
          begin
            cp_a(getH);
            local_tstates := local_tstates + 4;
          end;
        189:  //CP L
          begin
            cp_a(getL);
            local_tstates := local_tstates + 4;
          end;
        190:  //CP (HL)
          begin
            cp_a(peekb(regHL));
            local_tstates := local_tstates + 7;
          end;
        191:  //CP A
          begin
            cp_a(regA);
            local_tstates := local_tstates + 4;
          end;
        192:  //RET NZ
          begin
            If fZ = False Then
            begin
              poppc;
              local_tstates := local_tstates + 11;
            end
            Else
              local_tstates := local_tstates + 5;
          end;
        200:  //RET Z
          begin
            If fZ Then
            begin
              poppc;
              local_tstates := local_tstates + 11;
            end
            Else
              local_tstates := local_tstates + 5;
          end;
        208:  //RET NC
          begin
            If fC = False Then
            begin
              poppc;
              local_tstates := local_tstates + 11;
            end
            Else
              local_tstates := local_tstates + 5;
          end;
        216:  //RET C
          begin
            If fC Then
            begin
              poppc;
              local_tstates := local_tstates + 11;
            end
            Else
              local_tstates := local_tstates + 5;
          end;
        224:  //RET PO
          begin
            If fPV = False Then
            begin
              poppc;
              local_tstates := local_tstates + 11;
            end
            Else
              local_tstates := local_tstates + 5;
          end;
        232:  //RET PE
          begin
            If fPV Then
            begin
              poppc;
              local_tstates := local_tstates + 11;
            end
            Else
              local_tstates := local_tstates + 5;
          end;
        240:  //RET P
          begin
            If fS = False Then
            begin
              poppc;
              local_tstates := local_tstates + 11;
            end
            Else
              local_tstates := local_tstates + 5;
          end;
        248:  //RET M
          begin
            If fS Then
            begin
              poppc;
              local_tstates := local_tstates + 11;
            end
            Else
              local_tstates := local_tstates + 5;
          end;
        193:  //POP BC
          begin
            setBC(popw);
            local_tstates := local_tstates + 10;
          end;
        201:  //RET
          begin
            poppc;
            local_tstates := local_tstates + 10;
          end;
        209:  //POP DE
          begin
            regDE := popw;
            local_tstates := local_tstates + 10;
          end;
        217:  //EXX
          begin
            exx;
            local_tstates := local_tstates + 4;
          end;
        225:  //POP HL
          begin
            regHL := popw;
            local_tstates := local_tstates + 10;
          end;
        233:  //JP HL
          begin
            regPC := regHL;
            local_tstates := local_tstates + 4;
          end;
        241:  //POP AF
          begin
            setAF(popw);
            local_tstates := local_tstates + 10;
          end;
        249:  //LD SP,HL
          begin
            regSP := regHL;
            local_tstates := local_tstates + 6;
          end;
        194:  //JP NZ,nn
          begin
            If fZ = False Then
              regPC := nxtpcw
            Else
              regPC := regPC + 2;

            local_tstates := local_tstates + 10;
          end;
        202:  //JP Z,nn
          begin
            If fZ Then
              regPC := nxtpcw
            Else
              regPC := regPC + 2;

            local_tstates := local_tstates + 10;
          end;
        210:  //JP NC,nn
          begin
            If fC = False Then
              regPC := nxtpcw
            Else
              regPC := regPC + 2;

            local_tstates := local_tstates + 10;
          end;
        218:  //JP C,nn
          begin
            If fC Then
              regPC := nxtpcw
            Else
              regPC := regPC + 2;

            local_tstates := local_tstates + 10;
          end;
        226:  //JP PO,nn
          begin
            If fPV = False Then
              regPC := nxtpcw
            Else
              regPC := regPC + 2;

            local_tstates := local_tstates + 10;
          end;
        234:  //JP PE,nn
          begin
            If fPV Then
              regPC := nxtpcw
            Else
              regPC := regPC + 2;

            local_tstates := local_tstates + 10;
          end;
        242:  //JP P,nn
          begin
            If fS = False Then
              regPC := nxtpcw
            Else
              regPC := regPC + 2;

            local_tstates := local_tstates + 10;
          end;
        250:  //JP M,nn
          begin
            If fS Then
              regPC := nxtpcw
            Else
              regPC := regPC + 2;

            local_tstates := local_tstates + 10;
          end;
        195:  //JP nn
          begin
            regPC := peekw(regPC);
            local_tstates := local_tstates + 10;
          end;
        203:  //prefix CB
          local_tstates := local_tstates + execute_cb;
        211:  //OUT (n),A
          begin
            outb(nxtpcb, regA);
            local_tstates := local_tstates + 11;
          end;
        219:  //IN A,(n)
          begin
            regA := inb((regA {* 256} shl 8) Or nxtpcb);   // 20061220
            local_tstates := local_tstates + 11;
          end;
        227:  //EX (SP),HL
          begin
            lTemp := regHL;
            regHL := peekw(regSP);
            pokew(regSP, lTemp);
            local_tstates := local_tstates + 19;
          end;
        235:  //EX DE,HL
          begin
            lTemp := regHL;
            regHL := regDE;
            regDE := lTemp;
            local_tstates := local_tstates + 4;
          end;
        243:  //DI
          begin
            if Z80CardMode=Z80CARD_MOSCOW then SetBeeper0;
            intIFF1 := False;
            intIFF2 := False;
            local_tstates := local_tstates + 4;
          end;
        251:  //EI
          begin
            if Z80CardMode=Z80CARD_MOSCOW then SetBeeper1;
            intIFF1 := True;
            intIFF2 := True;
            local_tstates := local_tstates + 4;
          end;
        196:  //CALL NZ,nn
          begin
            If fZ = False Then
            begin
//              pushw(regPC + 2);      // 20061220
//              regPC := nxtpcw;
              lTemp := nxtpcw;
              pushw(regPC);
              regPC := lTemp;
              local_tstates := local_tstates + 17;
            end
            Else
            begin
              regPC := regPC + 2;
              local_tstates := local_tstates + 10;
            End;
          end;
        204:  //CALL Z,nn
          begin
            If fZ Then
            begin
//              pushw(regPC + 2);      // 20061220
//              regPC := nxtpcw;
              lTemp := nxtpcw;
              pushw(regPC);
              regPC := lTemp;
              local_tstates := local_tstates + 17;
            end
            Else
            begin
              regPC := regPC + 2;
              local_tstates := local_tstates + 10;
            End;
          end;
        212:  //CALL NC,nn
          begin
            If fC = False Then
            begin
//              pushw(regPC + 2);      // 20061220
//              regPC := nxtpcw;
              lTemp := nxtpcw;
              pushw(regPC);
              regPC := lTemp;
              local_tstates := local_tstates + 17;
            end
            Else
            begin
              regPC := regPC + 2;
              local_tstates := local_tstates + 10;
            End;
          end;
        220:  //CALL C,nn
          begin
            If fC Then
            begin
//              pushw(regPC + 2);      // 20061220
//              regPC := nxtpcw;
              lTemp := nxtpcw;
              pushw(regPC);
              regPC := lTemp;
              local_tstates := local_tstates + 17;
            end
            Else
            begin
              regPC := regPC + 2;
              local_tstates := local_tstates + 10;
            End;
          end;
        228:  //CALL PO,nn
          begin
            If fPV = False Then
            begin
//              pushw(regPC + 2);      // 20061220
//              regPC := nxtpcw;
              lTemp := nxtpcw;
              pushw(regPC);
              regPC := lTemp;
              local_tstates := local_tstates + 17;
            end
            Else
            begin
              regPC := regPC + 2;
              local_tstates := local_tstates + 10;
            End
          end;
        236:  //CALL PE,nn
          begin
            If fPV Then
            begin
//              pushw(regPC + 2);      // 20061220
//              regPC := nxtpcw;
              lTemp := nxtpcw;
              pushw(regPC);
              regPC := lTemp;
              local_tstates := local_tstates + 17;
            end
            Else
            begin
              regPC := regPC + 2;
              local_tstates := local_tstates + 10;
            End;
          end;
        244:  //CALL P,nn
          begin
            If fS = False Then
            begin
//              pushw(regPC + 2);      // 20061220
//              regPC := nxtpcw;
              lTemp := nxtpcw;
              pushw(regPC);
              regPC := lTemp;
              local_tstates := local_tstates + 17;
            end
            Else
            begin
              regPC := regPC + 2;
              local_tstates := local_tstates + 10;
            End;
          end;
        252:  //CALL M,nn
          begin
            If fS Then
            begin
//              pushw(regPC + 2);      // 20061220
//              regPC := nxtpcw;
              lTemp := nxtpcw;
              pushw(regPC);
              regPC := lTemp;
              local_tstates := local_tstates + 17;
            end
            Else
            begin
              regPC := regPC + 2;
              local_tstates := local_tstates + 10;
            End;
          end;
        197:  //PUSH BC
          begin
            pushw(getBC);
            local_tstates := local_tstates + 11;
          end;
        205:  //CALL nn
          begin
//              pushw(regPC + 2);      // 20061220
//              regPC := nxtpcw;
              lTemp := nxtpcw;
              pushw(regPC);
              regPC := lTemp;
            local_tstates := local_tstates + 17;
          end;
        213:  //PUSH DE
          begin
            pushw(regDE);
            local_tstates := local_tstates + 11;
          end;
        221:  //prefix IX
          begin
            regID := regIX;
            local_tstates := local_tstates + execute_id;
            regIX := regID;
          end;
        229:  //PUSH HL
          begin
            pushw(regHL);
            local_tstates := local_tstates + 11;
          end;
        237:  //prefix ED
          local_tstates := local_tstates + execute_ed(local_tstates);
        245:  //PUSH AF
          begin
            pushw(getAF);
            local_tstates := local_tstates + 11;
          end;
        253:  //prefix IY
          begin
            regID := regIY;
            local_tstates := local_tstates + execute_id;
            regIY := regID;
          end;
        198:  //ADD A,n
          begin
            add_a(nxtpcb);
            local_tstates := local_tstates + 7;
          end;
        206:  //ADC A,n
          begin
            adc_a(nxtpcb);
            local_tstates := local_tstates + 7;
          end;
        214:  //SUB n
          begin
            sub_a(nxtpcb);
            local_tstates := local_tstates + 7;
          end;
        222:  //SBC n
          begin
            sbc_a(nxtpcb);
            local_tstates := local_tstates + 7;
          end;
        230:  //AND n
          begin
            and_a(nxtpcb);
            local_tstates := local_tstates + 7;
          end;
        238:  //XOR n
          begin
            xor_a(nxtpcb);
            local_tstates := local_tstates + 7;
          end;
        246:  //OR n
          begin
            or_a(nxtpcb);
            local_tstates := local_tstates + 7;
          end;
        254:  //CP n
          begin
            cp_a(nxtpcb);
            local_tstates := local_tstates + 7;
          end;
        199:  //RST 0
          begin
            pushpc;
            regPC := 0;
            local_tstates := local_tstates + 11;
          end;
        207:  //RST 8
          begin
            pushpc;
            regPC := 8;
            local_tstates := local_tstates + 11;
          end;
        215:  //RST 16
          begin
            pushpc;
            regPC := 16;
            local_tstates := local_tstates + 11;
          end;
        223:  //RST 24
          begin
            pushpc;
            regPC := 24;
            local_tstates := local_tstates + 11;
          end;
        231:  //RST 32
          begin
            pushpc;
            regPC := 32;
            local_tstates := local_tstates + 11;
          end;
        239:  //RST 40
          begin
            pushpc;
            regPC := 40;
            local_tstates := local_tstates + 11;
          end;
        247:  //RST 48
          begin
            pushpc;
            regPC := 48;
            local_tstates := local_tstates + 11;
          end;
        255:  //RST 56
          begin
            pushpc;
            regPC := 56;
            local_tstates := local_tstates + 11;
          end;
        Else
          Application.MessageBox(
            PChar('Unknown instruction ' + inttostr(xxx) + ' at ' + inttostr(regPC)),
            PChar(Application.Title),
            MB_OK);
        End;
{$IFDEF DEBUG}
        inc(TicksFromPrevSD, local_tstates-PrevLocalTstates);
{$ENDIF}
        if Assigned(AfterInstruction) then
           AfterInstruction();
{$IFDEF XDEBUG}
        if not tmpStart then
          tmpStart:=(BreakPoint_pF9=MainPort[$F9])and(RegPC=$100);
        if tmpStart and (tmpStop=$FFFF) then tmpStop:=peekw(peekw(1)+1);
        if tmpStop=RegPC then tmpStart:=False;
        if (BreakPoint_pF9=MainPort[$F9])and(RegPC=5) then
        begin
          tmpRet:=peekw(regSP);
          if tmpStart then DebugInfo(#13#10'Call BDOS', 1);
        end;
        if (BreakPoint_pF9=MainPort[$F9])and(RegPC=tmpRet)and(tmpStart) then DebugInfo('      Ret', 0);
{$ENDIF}
        if ((BreakPointPF9 or BreakPointPF9mask = MainPort[$F9] or BreakPointPF9mask) and
            (BreakPointAddr or BreakPointAddrMask = RegPC or BreakPointAddrMask) and
            ((not AnalyzeConditions) or (AnalyzeConditions and
                                         ((boolean(ConditionSP[0])and(ConditionSPvalue or ConditionSPmask=regSP or ConditionSPmask)) or
                                          (boolean(ConditionAF[0])and(ConditionAFvalue or ConditionAFmask=getAF or ConditionAFmask)) or
                                          (boolean(ConditionBC[0])and(ConditionBCvalue or ConditionBCmask=getBC or ConditionBCmask)) or
                                          (boolean(ConditionDE[0])and(ConditionDEvalue or ConditionDEmask=regDE or ConditionDEmask)) or
                                          (boolean(ConditionHL[0])and(ConditionHLvalue or ConditionHLmask=regHL or ConditionHLmask)) or
                                          (boolean(ConditionAF_[0])and(ConditionAF_value or ConditionAF_mask=regAF_ or ConditionAF_mask)) or
                                          (boolean(ConditionBC_[0])and(ConditionBC_value or ConditionBC_mask=regBC_ or ConditionBC_mask)) or
                                          (boolean(ConditionDE_[0])and(ConditionDE_value or ConditionDE_mask=regDE_ or ConditionDE_mask)) or
                                          (boolean(ConditionHL_[0])and(ConditionHL_value or ConditionHL_mask=regHL_ or ConditionHL_mask)) or
                                          (boolean(ConditionIX[0])and(ConditionIXvalue or ConditionIXmask=regIX or ConditionIXmask)) or
                                          (boolean(ConditionIY[0])and(ConditionIYvalue or ConditionIYmask=regIY or ConditionIYmask)) or
                                          (boolean(ConditionIR[0])and(ConditionIRvalue or ConditionIRmask=getIR or ConditionIRmask))
                                         )
                                        )
            )
           ) or
           ((BreakPointRetPF9=MainPort[$F9]) and (BreakPointRetAddr=RegPC)) then
        begin
          if Assigned(AfterBreakPoint) then
             AfterBreakPoint();
          BreakPointRetPF9:=$FF;
        end;
        inc(intR);
        intR:=intR and $7F;
{$IFDEF USE_SOUND}
        AddSoundWave(local_tstates - lOldTstates);
{$ENDIF}
    until (intCtrTemp <> interruptCounter)or CpuPaused;

end;



procedure Z80Reset;
begin
    prevPC:= 0;
    regPC := 0;
    regSP := 0;
    regA := 0;
    setF(0);
    setBC(0);
    regDE := 0;
    regHL := 0;

    exx;
    ex_af_af;

    regA := 0;
    setF(0);
    setBC(0);
    regDE := 0;
    regHL := 0;

    regIX := 0;
    regIY := 0;
    intR := 128;
    intRTemp := 0;

    intI := 0;
    intIFF1 := False;
    intIFF2 := False;
    intIM := 0;

    // 69888 tstates per interrupt = 3.50000 MHz
    glTstatesPerInterrupt := GetCPUTstates();           // CPUTstates[CPUSpeedMode];
{$IFDEF USE_SOUND}
    glWaveAddTStates := 158;
    AY8912_init(1773000, WAVE_FREQUENCY, 8);
{$ENDIF}
end;



end.

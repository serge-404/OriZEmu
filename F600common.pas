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


unit F600common;

interface

Uses Windows, Messages, SysUtils;

const
  stSectionName = 'F600Plugin';
  stPrnFileKey  = 'PrnFile';
  stPrnFNewKey  = 'PrnFNew';
  stPrnCPKey    = 'PrnCP';
  stPrnModeKey  = 'PrnScheme';
  stPrnFontKey  = 'PrnFont';
  stPrnFtSzKey  = 'PrnFontSize';
  stPrnPrnKey   = 'PrnPrn';
  stPrnBrdTop   = 'BorderTop';
  stPrnBrdLeft  = 'BorderLeft';
  stPrnBrdRight = 'BorderRight';
  stPrnBrdBot   = 'BorderBottom';
  MAX_BUF = 2048;

  koi866: array[$80..$ff] of byte =
 ($C4, $B3, $DA, $BF, $C0, $D9, $C3, $B4, $C2, $C1, $C5, $DF, $DC, $DB, $DD, $DE,
  $B0, $B1, $B2, $F4, $FE, $F9, $FB, $F7, $F3, $F2, $FF, $F5, $F8, $FD, $FA, $F6,
  $CD, $BA, $D5, $F1, $D6, $C9, $B8, $B7, $BB, $D4, $D3, $C8, $BE, $BD, $BC, $C6,
  $C7, $CC, $B5, $F0, $B6, $B9, $D1, $D2, $CB, $CF, $D0, $CA, $D8, $D7, $CE, $FC,
  $EE, $A0, $A1, $E6, $A4, $A5, $E4, $A3, $E5, $A8, $A9, $AA, $AB, $AC, $AD, $AE,
  $AF, $EF, $E0, $E1, $E2, $E3, $A6, $A2, $EC, $EB, $A7, $E8, $ED, $E9, $E7, $EA,
  $9E, $80, $81, $96, $84, $85, $94, $83, $95, $88, $89, $8A, $8B, $8C, $8D, $8E,
  $8F, $9F, $90, $91, $92, $93, $86, $82, $9C, $9B, $87, $98, $9D, $99, $97, $9A
);

type
  TPrnBuffer = array[0..MAX_BUF] of byte;

function Koi8to866(inp: byte):byte;
Function GetPrivateString(SectionName,KeyName,DefaultString:string):string;
Function GetPrivateInt(SectionName,KeyName:string;DefaultInt:Integer):integer;
procedure WritePrivateString(SectionName,KeyName,Str:string);
procedure WritePrivateInt(SectionName,KeyName:string;Int:Integer);
procedure GetIniSettings;

var IniName: string;
var PrnBuffer: TPrnBuffer;
    PrnFile, PrnFont: string;
    PrnMode, PrnFNew, PrnCP, PrnPrn, PrnFontSize: integer;
    BufCount: integer;
    prev_pC: byte;
    strobe_phase: integer;   //   {00000111122222}

implementation

Uses F600printer;

function Koi8to866(inp: byte):byte;
begin
  if inp<$80 then
    Result:=inp
  else Result:=koi866[inp];
end;

Function GetPrivateString(SectionName,KeyName,DefaultString:string):string;
var buf:array[0..MAX_PATH] of char;
begin
  GetPrivateProfileString(PChar(SectionName),PChar(KeyName),PChar(DefaultString),buf,sizeof(buf)-1,PChar(IniName));
  Result:=trim(StrPas(buf));
end;

Function GetPrivateInt(SectionName,KeyName:string;DefaultInt:Integer):integer;
begin
  Result:=GetPrivateProfileInt(PChar(SectionName),PChar(KeyName),DefaultInt,PChar(IniName));
end;

procedure WritePrivateString(SectionName,KeyName,Str:string);
begin
  WritePrivateProfileString(PChar(SectionName),PChar(KeyName),PChar(Str),PChar(IniName));
end;

procedure WritePrivateInt(SectionName,KeyName:string;Int:Integer);
begin
  WritePrivateProfileString(PChar(SectionName),PChar(KeyName),PChar(IntToStr(Int)),PChar(IniName));
end;

procedure GetIniSettings;
begin
  IniName:=ChangeFileExt(System.ParamStr(0),'.INI');
  If not FileExists(IniName) then IniName:=ExtractFileName(IniName);
  PrnFile:=GetPrivateString(stSectionName, stPrnFileKey, 'c:\prn_out.txt');
  PrnFNew:=GetPrivateInt(stSectionName, stPrnFNewKey, 0);
  PrnMode:=GetPrivateInt(stSectionName, stPrnModeKey, 0);
  PrnCP:=GetPrivateInt(stSectionName,   stPrnCPKey, 0);
  PrnFont:=GetPrivateString(stSectionName, stPrnFontKey, 'Courier');
  PrnFontSize:=GetPrivateInt(stSectionName, stPrnFtSzKey, 10);
  PrnPrn:=GetPrivateInt(stSectionName, stPrnPrnKey, 0);
  BorderTop  := GetPrivateInt(stSectionName, stPrnBrdTop,   50);
  BorderLeft := GetPrivateInt(stSectionName, stPrnBrdLeft,  50);
  BorderRight:= GetPrivateInt(stSectionName, stPrnBrdRight, 50);
  BorderBottom:=GetPrivateInt(stSectionName, stPrnBrdBot,   50);
end;

initialization
  PrnPrn:=0;
  PrnMode:=0;
  PrnFNew:=0;
  PrnFontSize:=8;
  Strobe_phase:=0;
  BufCount:=0;
  prev_pC:=0;

end.

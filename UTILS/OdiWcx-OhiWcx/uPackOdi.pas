/////////////////////////////////////////////////////////////////////////
//                                                                     //
//   Orion/Z (Orion-128 + Z80-CARD-II) emulator, version 1.06          //
//                                                                     //
//   Addon: TotalCommander archiver (WCX) plugin for serving ODI files //
//          (Orion Disk Image files). Allow copy/extract CP/M files    //
//          to/from ODI file "diskette" such simple as processing any  //
//          archives in TotalCommander interface. Version 1.0.alpha    //
//                                                                     //
//   How to install this plugin (32 bit only) in TotalCommander:       //
//          1. Unzip odi.wcx, system.bin to any directory              //
//                (usually c:\wincmd\Plugins)                          //
//          2. In Windows Commander 5.5 (or newer), choose             //
//                Configuration - Options                              //
//          3. Open the 'Packer' page                                  //
//          4. Click 'Configure packer extension DLLs'                 //
//          5. type the "ODI" extension                                //
//          6. Click 'new type', and select the  odi.wcx               //
//          7. Click OK.  Click OK                                     //
//          8. Repeat steps 2-7 for other supported (specified in      //
//               ODI.INI) formats - such as TRD, DSK, etc.             //
//                                                                     //
//   How to install this plugin (32 bit only) in Far Manager:          //
//          0. Install wcx.dll (plugin allowing TC WCX-plugins usage   //
//             in Far) to                                              //
//                   {FAR_DIR}\Plugins\Multiarc\Formats\WCX\           //
//             Install wcx.fmt to                                      //
//                   {FAR_DIR}\Plugins\Multiarc\Formats\               //
//          1. Unzip odi.wcx, system.bin to                            //
//                   {FAR_DIR}\Plugins\Multiarc\Formats\WCX\           //
//          1.2. To Support any other CP/M format (specified in INI),  //
//               just copy ODI.WCX, ODI.INI to files with              //
//               file name correcponding to format extension (for      //
//               example:  DSK.WCX, DSK.INI ;  TRD.WCX, TRD.INI) to    //
//                   {FAR_DIR}\Plugins\Multiarc\Formats\WCX\           //
//          2. Restart Far                                             //
//                                                                     //
//   Author: Sergey A.        <a-s-m@km.ru>                            //
//                                                                     //
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

unit uPackOdi;

interface

uses
  Windows, SysUtils, Classes, wcxhead;

//{$DEFINE DEBUG}

const
  DPBSize = 32;                 // actually 15 bytes
  LogBlockSize = 128;

  ERR_FILE_OPEN = -1;
  ERR_FILE_STRU = -2;
  ERR_FILE_SIZE = -3;
  ERR_FILE_SEEK = -4;

  ERR_NO_DISK_SPACE = -5;
  ERR_NO_DIR_SPACE  = -6;
  ERR_PACK_FILE     = -7;

  ERR_WRONG_DPB_CRC   = -8;
  ERR_WRONG_DISK_SIZE = -9;

  MAX_EXTENT = 511;
  ErrFileLimit = 'Error: file size limit';

type
  TFCBExtents = array[0..7] of word;
  TFCBExtents16 = array[0..15] of byte;
  PFCBExtents16 = ^TFCBExtents16;

  TFCBExtent = packed record
                 SerialN: word;
                 OrdinalN: word;
                 LogBlkCnt: byte;
                 FCBextents: TFCBExtents;     // used 2k extents chain. 0, E5E5 - unused
               end;
  PFCBExtent = ^TFCBExtent;

  TFileRec = record
               FileUser: integer;
               FileName: string;
               FileSize: integer;
               FileTime: integer;
               FileAttr: integer;
               FileAddr: integer;                     // For Ordos file  
               FExtents: TList;                       // list of PFCBExtent
             end;
  PFileRec = ^TFileRec;

  TJump = array [0..7] of byte;

  TBootDPB = packed record    // Disk Parameters Header (BOOT .. BOOT+1EH, BOOT+1FH=CRC)
//-------------------------------------------------------------------- Orion specific
               jump: TJump;
               PAGE1: byte;
               PAGE2: byte;
               LEN1:  byte;   // phisical sector size (1=256, 2=512, 3=1024)
               LEN2:  byte;   // sides (density?) (0=one_side, 1=double_sided)
               SEC:   word;   // phisical sectors per track
               TRK:   word;   // phisical tracks on disk (one side)
//-------------------------------------------------------------------- CP/M standard
               SPT:   word;   // logical sectors (128) per track
               BSH:   byte;   // Block Shift - Block Size is given by 128 * 2^(BSH)
               BLM:   byte;   // Block Mask - Block Size is given by 128 * (BLM +1)
               EXM:   byte;   // Extent Mask
               DSM:   word;   // user space size in kb = SEC * (TRK-OFF) - (CKS/8)
               DRM:   word;   // max quantity of file records (FCBs) in catalog
               AL:    word;   // 16-bit Directory Allocation Pattern
               CKS:   word;   // Directory Check Sum = catalog size (in logical blocks)
               OFF:   word;   // system tracks
               CRC:   byte;   // simple additional CRC beginning with 066h
             end;
  PBootDPB = ^TBootDPB;

const
  DPBdefault: TBootDPB =
    (jump: ($C3, $20, $00, $00, $53, $44, $43, $32);
     page1: $01;
     page2: $01;
     len1:  $03;
     len2:  $01;
     sec:   $0005;
     trk:   $0050;
     spt:   $0028;
     bsh:   $04;
     blm:   $0F;
     exm:   $00;
     dsm:   $0184;
     drm:   $007F;
     al:    $00C0;
     cks:   $0020;
     off:   $0004;
     crc:   $D3);
  SystemTracks = 'UseThis_ToAccess_SystemTracks';
  SystemBin = 'System.bin';
  SystemUser = 254;
  RomOffsSize = $10000;
  OrdosSize = 2048;
  ROMDISK_TOP = 65535-OrdosSize;
  OrdosUser =  SystemUser-1;
  OrdosFiles = SystemTracks+'\ORDOS_files';
  PROsignature = #$FF#$ED#$B0#$3E#$02#$D3#$09#$FF#0;    // to detect Orion-PRO microROMdisk (within ROM2 body) 
  ADRsignature = $1FF8-OrdosSize;

type
  TSystemBinRec = packed record                         // sizeof=32
                   Name:array[0..22]of char;
                   Size:integer;
                   Date:integer; // in format of FileGetDate() function
                   CRC:byte;
                 end;

  TFCBFileName = array[0..10] of char;

  TFCB = packed record
           User:       byte;
           FileName:   TFCBFileName;
           FCBordEX:   byte;                    // partial sequentional number (= "size div 128" for filesystems where each FCB addresses > 16384 bytes) , low 5 bits (D0..D4)
           dummy:      byte;
           FCBordS2:   byte;                    // partial sequentional number (= "size div 128" for filesystems where each FCB addresses > 16384 bytes) , high bits (D5, D6 and the rest)
           SizePartial:byte;                    // current part size in 128bytes logical blocks (= size mod 128)
           FCBextents: TFCBExtents;             // used 2k extents chain. 0, E5E5 - unused
         end;
  TAltFileTime = packed record                  // FileTime in AltairDos format. By bits:
                   YYMM: byte;                  // (1996 +) 4YY ; 4MM
                   DDH:  byte;
                   HMI:  byte;                  //          5DD ; 5HH ; 6MIN
                 end;
  TFormatRec = packed record
                 ext: string;
                 DPB: TBootDPB;
                 sys: string;
               end;
  PFormatRec = ^TFormatRec;

  TAltBOOT = packed record                                              // AltairDOS boot
               DPB:  TBootDPB;
               LBL:  array[0..15] of char;                              // 20..2Fh
               CODE: array[$30..$FC] of byte;
               SLBL: byte;                                              // $FD - label checksum
               SUNM: byte;                                              // $FE - usernames checksum
               STIM: byte;                                              // $FF - filetimes checksum
               UNM:  array[0..15, 0..15] of char;                       // user names (catalogs)
               TIM:  array[0..255] of TAltFileTime;                     // filetime array
             end;
  PAltBOOT = ^TAltBOOT;

  TExtBoot = packed record
               DPB:  TBootDPB;
               LBL:  array[0..15] of char;                              // 20..2Fh
               CODE: array[$30..$FC] of byte;
               SLBL: byte;                                              // $FD - label checksum
               SUNM: byte;                                              // $FE - usernames checksum
               STIM: byte;                                              // $FF - filetimes checksum
               UNM:  array[0..15, 0..15] of char;                       // user names (catalogs)
               TIM:  array[0..255] of TAltFileTime;                     // filetime array
               BOOTvalid, LBLvalid, UNMvalid, TIMvalid, Damaged, SystemBinValid: boolean;
               SystemBinRec: TSystemBinRec;
             end;
  PExtBOOT = ^TExtBOOT;

  TAllocationMap = array [0..16383] of word;     // $FFFF=unused extent, else - ordinal file number

  TScanCatalogCallBack = function(FS: TStream; SerialN: integer;
                                  var FCB: TFCB; PParam: pointer): boolean;

  TRomdiskType = (rtNone, rtPROmini, rtStandard);

  TVars = record
    BOOT: TExtBoot;
    AllocationMap: TAllocationMap;                                   // global
    CatalogMap: TAllocationMap;                                      // global
    FileList: TList;                                                 // global
    FormatList: TList;
    PhySectorSize:integer;                                    // global
    nSides: integer;                                             // global
    ExtentSize: integer;
    ExtentsInFCB: integer;
    LogBlkInExt:integer;
    ExtentBuf: array of AnsiChar;
    USE_DPBLESS_DISKS: integer;
{}
    TmpBuf: array[0..512] of byte;
    FileListPos: integer;
    FileToProcess: string;
    ArcFileName: string;
    IniFileName: string;
    PartitionN: DWORD;         {V1.06}
    PartitionOffs: DWORD;      {V1.01}
    PartitionRomOffs: DWORD;   {V1.05}
    RomdiskType: TRomdiskType;
    FileDate: integer;
    ROMArr: array [0..ROMDISK_TOP] of byte;
  end;
  PVars = ^TVars;

var
  Vars: PVars;

function GetVolumeName(): string;
function GetFCBordinalN(FCB:TFCB):integer;
procedure SetFCBordinalN(var FCB:TFCB; Value:integer);
function OdiCreateArchive(ArcFName: string): integer;
function OdiGetCatalog(OdiArchiveName: string):integer;
function OdiFileExtract(OdiArchiveName: string; PFRec: PFileRec; OutName: string): integer;
function OdiFileDelete(OdiArchiveName: string; FileToDelete: string):integer;
function OdiFilePack(OdiArchiveName, SrcFileName, ArchFileName: string):integer;
{}
function OpenArchive(var ArchiveData: TOpenArchiveData): THandle; stdcall;
function OpenArchivePart(ArcName: PChar; PartOffset: DWORD; PartN:DWORD): THandle; stdcall;
function ReadHeader(hArcData: THandle; var HeaderData: THeaderData): integer; stdcall;
function ProcessFile(hArcData: THandle; Operation: integer; DestPath, DestName: PChar): integer; stdcall;
function CloseArchive (hArcData: THandle): integer; stdcall;
function PackFiles(PackedFile, SubPath, SrcPath, AddList: PChar; Flags: integer): integer; stdcall;
function DeleteFiles(PackedFile, DeleteList: PChar): integer; stdcall;
function GetPackerCaps: integer; stdcall;
function CanYouHandleThisFile(FileName: PChar): boolean; stdcall;
procedure SetChangeVolProc(hArcData: THandle; pChangeVolProc1: TChangeVolProc); stdcall;
procedure SetProcessDataProc(hArcData: THandle; pProcessDataProc: TProcessDataProc); stdcall;
procedure ConfigurePacker(Parent: HWND; DllInstance:LongWord); stdcall;
function GetPartInfo(OdiArchiveName:PChar):PChar; stdcall;
procedure DisposeFileList(var List: TList);
function GetBOOT(FName: string; FS: TFileStream; FmtN: integer):boolean;

implementation

{
; ‘Œ–Ã¿“ Ã¿——»¬¿ DPB ¬ŒŒ“-—≈ “Œ–¿ (BOOT+8 .. BOOT+1EH)
;    0        1       2       3       4       5       6       7
;  +---------------------------------------------------------------+
;0 |PAGE=1  PAGE=1  lengt=3 const=1   SEC   const=0   TRK   const=0|
;  +---------------------------------------------------------------+
;8 |  SPT    SPT=0   BSH=4  BLM=0    EXM=0    DSM     DSM'   DRM   | 0F
;  +---------------------------------------------------------------+
;10|  DRM     AL0    AL1=0   CKS     CKS'=0  OFF=SYS  OFF'=0   SUMM|
;  +---------------------------------------------------------------+
;     10        11      12      13      14      15      16
;
BOOTDPB:DB      001H,001H,003H,001H,005H,000H,050H,000H         ; 5/80
        DB      028H,000H,004H,00FH,000H,085H,001H,07FH
        DB      000H,0C0H,000H,020H,000H,004H,000H
}

procedure DebugInfo(str: string);
{$IFDEF DEBUG}
var ff: system.text;
{$ENDIF}
begin
{$IFDEF DEBUG}
  AssignFile(ff, 'c:\temp\OdiArc.debug');
  if FileExists('c:\temp\OdiArc.debug') then
    Append(ff)
  else
    Rewrite(ff);
  write(ff, FormatDateTime('dd.mm.yyyy hh:nn:ss - ', Now()));
  writeln(ff, str);
  CloseFile(ff);
{$ENDIF}
end;

function MIN(x,y: integer):integer;
begin
  if x>y then Result:=y else Result:=x;
end;

//////////////////// strings utilities ////////////////////////

function AddSlash(str: string): string;
begin
  Result:=str;
  if (Length(Result)>0) and (Result[Length(Result)]<>'\')
  then Result:=Result+'\';
end;

function padr(stt:string;max:integer;ch:string):string;
begin
  if length(stt)>=max then padr:=copy(stt,1,max)
  else
  begin
    while length(stt)+length(ch)<=max do stt:=stt+ch;
    if length(stt)<max then stt:=stt+copy(ch,1,max-length(stt));
    padr:=stt;
  end;
end;

function chrtran(source:String; from_tbl, to_tbl: string):String;
var buf: String;
    xpos,i: integer;
begin
  buf:=source;
  i:=1;
  while i<=Length(buf) do
  begin
    xpos:=pos(buf[i], from_tbl);
    if (xpos>0) then
    begin
      if (xpos<=Length(to_tbl)) then
      begin
        buf[i]:=to_tbl[xpos];
        inc(i);
      end
      else delete(buf,i,1);
    end
    else inc(i);
  end;
  chrtran:=buf;
end;

function LeftSubstrList(var s:String; DelimList: string): String;
var ch:char;
    j:integer;
begin
  Result:='';
  if Length(s)<1 then exit;
  j:=0;
  if (s[1]=' ')then s:=TrimLeft(s);
  if (s[1]='"')or(s[1]='''') then
  begin
    inc(j);
    ch:=s[1];
    while (j<Length(s))and(s[j+1]<>ch) do inc(j);
      if (Length(s)>1)and(j>1) then Result:=copy(s, 2, j-1);
    if (j<Length(s))and(s[j+1]=ch) then inc(j);
  end
  else
  begin
    while (j<Length(s))and(pos(s[j+1], DelimList)=0)and(s[j+1]<>#9) do inc(j);
    Result:=copy(s, 1, j);
  end;
  while (j<Length(s))and((pos(s[j+1], DelimList)>0)or(s[j+1]=#9)) do inc(j);
  delete(s,1,j);
end;

Function LeftSubstr(var s:String): String;
begin
  Result:=LeftSubstrList(s, ' ,');
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
        else m:=0;
      end;
      Result:=Result+m*l;
      l:=l*16;
      dec(k);
    until (k=0) or (Result<0);
end;

/////////////////////////////////////////////////////////

function GetFCBordinalN(FCB:TFCB):integer;
begin
  Result:=32*(FCB.FCBordS2 and $0F)+(FCB.FCBordEX and $1F);
end;

procedure SetFCBordinalN(var FCB:TFCB; Value:integer);
begin
  FCB.FCBordS2:=(Value shr 5) and $0F;
  FCB.FCBordEX:=Value and $1F;
end;

procedure DisposeFormatList(var List: TList);
begin
  if Assigned(List) then
    while List.Count>0 do with List do
    begin
      dispose(PFormatRec(Items[Count-1]));
      Delete(Count-1);
    end;
end;

function DPBcrc(var DPB:TBootDPB):byte;
var ii:integer;
begin
  Result:=$66;
  for ii:=0 to DPBSize-2 do
    Result:=Result+byte(PChar(@DPB)[ii]);
end;

procedure GetIniSettings(IniFile:string);
var ss, ext, dpb, sys: string;
    PFrmRec: PFormatRec;
    i: integer;
begin
 with Vars^ do begin
  USE_DPBLESS_DISKS:=GetPrivateProfileInt('PARAMS', 'USE_DPBLESS_DISKS', 1, PChar(IniFileName));
  GetPrivateProfileString('PARAMS', 'FORMATS_LIST', '',
                          @TmpBuf[0], sizeof(TmpBuf)-1,PChar(IniFileName));
  TmpBuf[sizeof(TmpBuf)-1]:=0;
  ss:=trim(StrPas(@TmpBuf[0]));
  DisposeFormatList(FormatList);
  while ss<>'' do
  begin
    ext:=LeftSubstr(ss);
    GetPrivateProfileString('FORMATS', PChar(ext), '',
                            @TmpBuf[0], sizeof(TmpBuf)-1,PChar(IniFileName));
    TmpBuf[sizeof(TmpBuf)-1]:=0;
    sys:=trim(StrPas(@TmpBuf[0]));
    dpb:=LeftSubstr(sys);
    if dpb<>'' then
    begin
      if (sys<>'') and (not FileExists(sys)) then
        sys:=AddSlash(ExtractFilePath(IniFileName))+ExtractFileName(sys);
      new(PFrmRec);
      PFrmRec^.ext:=ext;
      PFrmRec^.sys:=sys;
      FillChar(TmpBuf, 32, 0);
      for i:=0 to MIN(22, length(dpb) div 2) do
        TmpBuf[i+8]:=HexToInt(copy(dpb, i*2+1, 2));
      Move(TmpBuf, PFrmRec^.DPB, sizeof(PFrmRec^.DPB));
      PFrmRec^.DPB.crc:=DPBcrc(PFrmRec^.DPB);
      FormatList.Add(PFrmRec);
    end;
  end;
 end;
end;

function OdiFreeDiskSpace: integer;  // free disk space in bytes
var i: integer;
    DirOffsetInLogBlocks: integer;
begin
 with Vars^ do begin
  Result:=0;
  DirOffsetInLogBlocks:=(BOOT.DPB.DRM+1) div (LogBlkInExt*(LogBlockSize div 32));
  for i:=DirOffsetInLogBlocks to BOOT.DPB.DSM+DirOffsetInLogBlocks-1 do
    if AllocationMap[i]=$FFFF then
      Result:=Result+ExtentSize;
 end;     
end;

function OdiFreeDirSpace: integer;  // free catalog space in FCBs
var i: integer;
begin
  Result:=0;
  with Vars^ do
   for i:=0 to BOOT.DPB.DRM do
    if CatalogMap[i]=$FFFF then
      inc(Result);
end;

function GetFreeExtent: integer;
var DirOffsetInLogBlocks: integer;
begin
 with Vars^ do begin
  DirOffsetInLogBlocks:=(BOOT.DPB.DRM+1) div (LogBlkInExt*(LogBlockSize div 32));
  Result:=DirOffsetInLogBlocks;
  while (Result < BOOT.DPB.DSM+DirOffsetInLogBlocks) and
        (AllocationMap[Result]<>$FFFF) do
      inc(Result);
  if Result=BOOT.DPB.DSM+DirOffsetInLogBlocks then
    Result:=-1;
 end;
end;

function GetFreeFCB: integer;
begin
 with Vars^ do begin
  Result:=0;
  while (Result < BOOT.DPB.DRM + 1) and
        (CatalogMap[Result]<>$FFFF) do
      inc(Result);
  if Result=BOOT.DPB.DRM + 1 then
    Result:=-1;
 end;
end;

procedure DisposeFileList(var List: TList);
var jj: integer;
begin
  if Assigned(List) then
    while List.Count>0 do with List do
    begin
      if Assigned(PFileRec(Items[Count-1])^.FExtents) then
      begin
        for jj:=0 to TList(PFileRec(Items[Count-1])^.FExtents).Count-1 do
          dispose(PFCBExtent(TList(PFileRec(Items[Count-1])^.FExtents).Items[jj]));
        TList(PFileRec(Items[Count-1])^.FExtents).Free;
      end;
      dispose(PFileRec(Items[Count-1]));
      Delete(Count-1);
    end;
end;

function XorCRC(buf:PChar; size:integer):byte;
var i:integer;
begin
  Result:=0;
  for i:=0 to size-1 do
    Result:=Result xor ord(buf[i]);
end;

function PartitionOffset:integer;
begin
 with Vars^ do
  Result:=PartitionOffs+PartitionRomOffs;
end;

function GetMainBOOT(FName: string; FS: TFileStream; FmtN: integer):boolean;
var d, CalculatedSize: integer;
begin
 with Vars^ do begin
  BOOT.SystemBinValid:=False;
  BOOT.Damaged:=False;
  FS.Seek(PartitionOffset, soFromBeginning);                                         {V1.01}
  FS.Read(BOOT, sizeof(TAltBoot));
  Result:=DPBcrc(BOOT.DPB)=BOOT.DPB.CRC;
  BOOT.BOOTvalid:=Result;
  BOOT.LBLvalid:=Result and
                (XorCRC(BOOT.LBL,sizeof(BOOT.LBL))=BOOT.SLBL);
  BOOT.UNMvalid:=Result and
                (XorCRC(@BOOT.UNM[0,0], sizeof(BOOT.UNM))=BOOT.SUNM);
  BOOT.TIMvalid:=Result and
                (XorCRC(PChar(pointer(@BOOT.TIM[0])),sizeof(BOOT.TIM))=BOOT.STIM);
  if (not Result)and(PartitionOffset=0)and(USE_DPBLESS_DISKS<>0) then           // 20160726: DPBless disks support is only for disk images (PartitionOffset=0)
  begin
    if FmtN<0 then
    begin
      FmtN:=0;
      while FmtN<FormatList.Count do
       with PFormatRec(FormatList.Items[FmtN])^ do
        if (AnsiUpperCase(chrtran(ExtractFileExt(FName), '.', ''))=AnsiUpperCase(ext))
        then
          break
        else
          inc(FmtN);
    end;
    if FmtN<FormatList.Count then    // format found
      with PFormatRec(FormatList.Items[FmtN])^ do
      begin
        if dpb.LEN2=0 then d:=1 else d:=2;
        CalculatedSize:=LogBlockSize*dpb.SPT*d*dpb.TRK;
        if (CalculatedSize>=FS.Size) then                                                               // 20120229  ">="
          Move(PFormatRec(FormatList.Items[FmtN])^.DPB.PAGE1, BOOT.DPB.PAGE1, sizeof(BOOT.DPB)-8)
        else
          Move(DPBdefault, BOOT.DPB, sizeof(BOOT.DPB));
      end
    else
      Move(DPBdefault, BOOT.DPB, sizeof(BOOT.DPB));
    BOOT.DPB.CRC:=DPBcrc(BOOT.DPB);
    BOOT.BOOTvalid:=True;
    BOOT.Damaged:=True;
    Result:=True;
  end;
  if Result then
  with BOOT do
  begin
    case DPB.LEN1 of
      0: PhySectorSize:=128;
      1: PhySectorSize:=256;
      2: PhySectorSize:=512
      else PhySectorSize:=1024;
    end;
    case DPB.LEN2 of
      0: nSides:=1
      else nSides:=2;
    end;
    LogBlkInExt := BOOT.DPB.BLM+1;
    ExtentSize := LogBlkInExt*LogBlockSize;
    ExtentsInFCB := (BOOT.DPB.EXM+1)*16384 div ExtentSize;
    SetLength(ExtentBuf, ExtentSize*ExtentsInFCB+1);
{1.4}
    FS.Seek(PhySectorSize * BOOT.DPB.SEC * BOOT.DPB.OFF + PartitionOffset - sizeof(BOOT.SystemBinRec), soFromBeginning);  // 20160909 - for sysgen special catalog/file
    FS.Read(BOOT.SystemBinRec, sizeof(BOOT.SystemBinRec));
    BOOT.SystemBinValid:=DPBcrc(PBootDPB(pointer(@BOOT.SystemBinRec))^)=BOOT.SystemBinRec.CRC;
{1.5}
    if Result and (BOOT.DPB.TRK*BOOT.DPB.SEC*PhySectorSize*nSides>(FS.Size-PartitionOffset)) then
    begin
       Result:=False;
    end;
  end;
 end;
end;

function GetBOOT(FName: string; FS: TFileStream; FmtN: integer):boolean;
var UDD:integer;
begin
  Result:=True;
  with Vars^ do begin
    UDD:=USE_DPBLESS_DISKS;
    USE_DPBLESS_DISKS:=0;
    if not GetMainBOOT(FName, FS, FmtN) then begin
      PartitionRomOffs:=RomOffsSize;
      if not GetMainBOOT(FName, FS, FmtN) then begin    {V1.05 - search for inROM filesystem with 65536 offset }
        PartitionRomOffs:=0;
        RomdiskType:=rtNone;
        USE_DPBLESS_DISKS:=UDD;
        Result:=GetMainBOOT(FName, FS, FmtN);
      end;
    end;
  end;
end;

procedure SetBOOT(OdiArchiveName: string);
var FS:TFileStream;
begin
 with Vars^ do begin
  if BOOT.Damaged then exit;
  FS:=nil;
  try
    FS:=TFileStream.Create(OdiArchiveName, fmOpenReadWrite or fmShareDenyWrite);
    FS.Seek(PartitionOffset, soFromBeginning);                                      {V1.01}
    FS.Write(BOOT, 256);
    if BOOT.UNMvalid then
      FS.Write(BOOT.UNM, sizeof(BOOT.UNM));
    if BOOT.TIMvalid then
      FS.Write(BOOT.TIM, sizeof(BOOT.TIM));
  finally
    if Assigned(FS) then FS.Free;
  end;
 end;
end;

function GetVolumeName(): string;
var bb: byte;
begin
 with Vars^ do begin
  if BOOT.LBLvalid then
  begin
    bb:=BOOT.CODE[$30];
    BOOT.CODE[$30]:=0;
    Result:=trim(StrPas(BOOT.LBL));
    BOOT.CODE[$30]:=bb;
  end
  else
    Result:='';
 end;
end;

function SetVolumeName(VolName: string): boolean;
var xsum:byte;
    i: integer;
begin
 with Vars^ do begin
  Result:=False;
  if (not BOOT.BOOTvalid) or (not BOOT.LBLvalid) then exit;
  xsum:=BOOT.Code[$30];
  StrPLCopy(BOOT.LBL, padr(VolName, sizeof(BOOT.LBL), ' '), sizeof(BOOT.LBL));
  BOOT.Code[$30]:=xsum;
  xsum:=0;
  for i:=0 to sizeof(BOOT.LBL)-1 do
    xsum:=xsum xor ord(BOOT.LBL[i]);
  BOOT.SLBL:=xsum;
  Result:=True;
 end;
end;

function ExtractFileUser(FName: string): byte;     // '...\USER_12\filename.ext' -> 12
var UPath: string;
    i: integer;
begin
  UPath:=ExtractFilePath(FName);
  Result:=OrdosUser;
  if pos(OrdosFiles+'\',UPath)<>0 then exit;                                // 20161019 special subdir for ORDOS files
  Result:=SystemUser;
  if pos(SystemTracks+'\',UPath)<>0 then exit;                              // 20160909 for sysgen special subdir
  Result:=$FF;
  while (UPath<>'') and (not (UPath[Length(Upath)] in ['0'..'9'])) do
    delete(UPath, Length(Upath), 1);
  i:=Length(Upath);
  while (i>0) and (UPath[i] in ['0'..'9']) do
    dec(i);
  if Length(UPath)>0 then
    Result:=StrTointDef(copy(UPath, i+1, 255), $FF);
end;

function IndexOfExtent(PRec: PFileRec; ExtN: integer): integer;
begin
  Result:=0;
  while (Result<TList(PRec^.FExtents).Count) and
        (PFCBExtent(TList(PRec^.FExtents).Items[Result])^.OrdinalN<>ExtN) do
    inc(Result);
  if Result=TList(PRec^.FExtents).Count then Result:=-1;
end;

function OdiFileExists(FileNm: string; var i: integer):boolean;    // FileNM like 'USER_0\FILENAME.EXT'
var usr: integer;
begin
 with Vars^ do begin
  usr:=ExtractFileUser(FileNm);
  if usr=$FF then
    usr:=15;
  FileNm:=AnsiUpperCase(PFileRec(FileList.Items[usr])^.FileName+'\'+FileNm);
  i:=0;
  while (i<FileList.Count) and
        ((PFileRec(FileList.Items[i])^.FileAttr and faDirectory <> 0) or
         (AnsiUpperCase(PFileRec(FileList.Items[i])^.FileName)<>FileNm)) do
    inc(i);
  Result:=i<FileList.Count;
 end;
end;

//FileTime = (year - 1980) << 25 | month << 21 | day << 16 | hour << 11 | minute << 5 | second/2;

function GetFTime(SN: integer): integer;
var dhm: integer;
begin
 with Vars^ do begin
  Result:=0;
  if (SN>255) or (not BOOT.TIMvalid) then exit;
  with BOOT.TIM[SN] do
  begin
    dhm:=(DDH shl 8) or HMI;
    Result:=Result or (((YYMM shr 4)+16) shl 25)     // year
                   or ((YYMM and 15) shl 21)         // month
                   or (dhm shl 5);                   // day, hour, minutes
  end;
 end;
end;

procedure SetFTime(SN: integer; FTime: integer);                           // Set file time in BOOT array
var dhm: word;
    i: integer;
    xsum: byte;
begin
 with Vars^ do begin
  if (SN>255) or (not BOOT.TIMvalid) then exit;
  with BOOT.TIM[SN] do
  begin
    YYMM:=(lo((FTime shr 25)-16) shl 4) or (lo(FTime shr 21) and 15);
    dhm:=LoWord(FTime shr 5);
    DDH:=hi(dhm);
    HMI:=lo(dhm);
  end;
  xsum:=0;
  for i:=0 to sizeof(BOOT.TIM)-1 do
    xsum:=xsum xor byte(PChar(@BOOT.TIM)[i]);
  BOOT.STIM:=xsum;
 end;
end;

function FileSetTime(List: TList; FName: string; FTime: integer): boolean; // Set file time in BOOT array
var i, jj: integer;
begin
 with Vars^ do begin
  i:=$FFFF;
  Result:=False;
  if BOOT.TIMvalid and
     OdiFileExists(AnsiUpperCase(FName), i)
  then with List do
  begin
    if Assigned(PFileRec(Items[i])^.FExtents) then
    begin
      for jj:=0 to TList(PFileRec(Items[i])^.FExtents).Count-1 do
        SetFTime(PFCBExtent(TList(PFileRec(Items[i])^.FExtents).Items[jj])^.SerialN, FTime);
      Result:=True;
    end;
  end;
 end;
end;

function FileGetTime(List: TList; FName: string): integer;                   // Get file time from BOOT array
var i: integer;
begin
 with Vars^ do begin
  i:=$FFFF;
  Result:=0;
  if BOOT.TIMvalid and
     OdiFileExists(AnsiUpperCase(FName), i)
  then with List do
    if Assigned(PFileRec(Items[i])^.FExtents) then
      Result:=GetFTime(PFCBExtent(TList(PFileRec(Items[i])^.FExtents).Items[0])^.SerialN);
 end;
end;

function ScanCatalog(OdiArchiveName: string; fmMode: word; ScanCallBack:TScanCatalogCallBack; PParam: pointer): integer;
var FS: TFileStream;
    i: integer;
    mask: WORD;
    FCB: TFCB;
begin
  Result:=-1;
  FS:=nil;
  with Vars^ do try
    FS:=TFileStream.Create(OdiArchiveName, fmMode or fmShareDenyWrite);
    FileDate:=FileGetDate(FS.Handle);
    GetBOOT(OdiArchiveName, FS, -1);
    if not BOOT.BOOTvalid then                                            // wrong CRC
      Result:=ERR_WRONG_DPB_CRC
    else
    if FS.Size<(PhySectorSize * BOOT.DPB.SEC * BOOT.DPB.TRK * nSides) then
      Result:=ERR_WRONG_DISK_SIZE
    else
    begin
//20120822
      mask:=$80;
      for i:=0 to 7 do
      begin
            if ((BOOT.DPB.AL and mask)<>0) and (AllocationMap[i]=$FFFF) then
              AllocationMap[i]:=$FFFE;                                          // reserved for catalog
            mask:=mask shr 1;
      end;
//20120822
      FS.Seek(PhySectorSize * BOOT.DPB.Sec * BOOT.DPB.Off + PartitionOffset, soFromBeginning);   // to catalog {V1.0}
      Result:=0;
      for i:=0 to BOOT.DPB.DRM do
      begin
        FS.Read(FCB, sizeof(FCB));
        if FCB.User<16 then
          if Assigned(ScanCallBack) and (not ScanCallBack(FS, i, FCB, PParam)) then
             break;
      end;
    end;
  finally
    if Assigned(FS) then FS.Free;
  end;
end;

function GetFCBExtent(var FCBExts:TFCBExtents; Index:integer):integer;  // get block number from FCB
begin
 with Vars^ do
  if ExtentsInFCB<16 then
    Result:=FCBExts[Index]
  else
    Result:=PFCBExtents16(@FCBExts[0])^[Index];
end;

function ScanCatalogList(FS: TStream; SerialN: integer; var FCB: TFCB; PParam: pointer): boolean;
var
  j, FIndex, FAttr: integer;
  FName: string;
  PFRec: PFileRec;
  PFCBExt: PFCBExtent;
begin
 with Vars^ do begin
  Result:=True;
  FName:=PFileRec(FileList.Items[FCB.User])^.FileName+'\';
  FAttr:=0;
  if FCB.FileName[8]>#127 then FAttr:=FAttr or faReadOnly;
  if FCB.FileName[9]>#127 then FAttr:=FAttr or faHidden;
  for j:=0 to sizeof(FCB.FileName)-1 do
  begin
    if (FCB.FileName[j]='\')or(FCB.FileName[j]='/') then FCB.FileName[j]:='-';
    if FCB.FileName[j]>' ' then
      FName:=FName+chr(ord(FCB.FileName[j]) and $7F);
    if j=7 then FName:=FName+'.';
  end;
  FName:=AnsiUpperCase(FName);
  FIndex:=0;
  while (FIndex<FileList.Count) and
        (AnsiUpperCase(PFileRec(FileList.Items[FIndex])^.FileName)<>Fname) do
    inc(FIndex);
  CatalogMap[SerialN]:=word(FIndex);
  if FIndex=FileList.Count then                   // not found - it`s first file`s partial FCB
  begin
    new(PFRec);
    new(PFCBExt);
    with PFRec^ do
    begin
      FileUser:=FCB.User;
      FileName:=FName;
      FileSize:=(GetFCBordinalN(FCB) * 128 + FCB.SizePartial) * LogBlockSize;
      FileTime:=GetFTime(SerialN);
      FileAttr:=FAttr;
      FExtents:=TList.Create;
      FExtents.Add(PFCBExt);
      PFCBExt^.SerialN:=SerialN;                          // ÒÒ˚ÎÍ‡ Ì‡ ˝ÎÂÏÂÌÚ Í‡Ú‡ÎÓ„‡
      PFCBExt^.OrdinalN:=GetFCBordinalN(FCB);
      PFCBExt^.LogBlkCnt:=FCB.SizePartial;
      PFCBExt^.FCBextents:=FCB.FCBextents;
    end;
    PFileRec(FileList.Items[FCB.User])^.FileName:=AnsiUpperCase(PFileRec(FileList.Items[FCB.User])^.FileName);
    FileList.Add(PFRec);
    inc(Result);
  end
  else if (IndexOfExtent(PFileRec(FileList.Items[FIndex]), GetFCBordinalN(FCB))<0) then  // check doubling
    with PFileRec(FileList.Items[FIndex])^ do                // found, just next FCB of existing file
    begin
      j:=(GetFCBordinalN(FCB) * 128 + FCB.SizePartial) * LogBlockSize;
      if j>FileSize then FileSize:=j;
      new(PFCBExt);
      FExtents.Add(PFCBExt);
      PFCBExt^.SerialN:=SerialN;                          // ÒÒ˚ÎÍ‡ Ì‡ ˝ÎÂÏÂÌÚ Í‡Ú‡ÎÓ„‡
      PFCBExt^.OrdinalN:=GetFCBordinalN(FCB);
      PFCBExt^.LogBlkCnt:=FCB.SizePartial;
      PFCBExt^.FCBextents:=FCB.FCBextents;
    end;
  for j:=0 to ExtentsInFCB-1 do begin
    FAttr:=GetFCBExtent(FCB.FCBExtents, j);
    if FAttr>0 then
      AllocationMap[FAttr]:=word(FIndex);
  end;
 end;
end;

function IsFLeter(ch: byte):boolean;
begin
  IsFLeter:=(ch>=32)and(ch<127);
end;

function GetOrdosFileList(list:TStrings): integer;
var ii: integer;
    ss: string;
begin
  Result:=0;
  with Vars^ do
   if IsFLeter(ROMArr[0]) then
    repeat
      ss:='';
      for ii:=0 to 7 do
        if IsFLeter(ROMArr[Result+ii]) then ss:=ss+chr(ROMArr[Result+ii]);
      if trim(ss)<>'' then list.AddObject(trim(ss)+'.ORD', pointer(Result));
      Result:=Result + 16 + PWord(@ROMArr[Result+10])^;
    until (Result>ROMDISK_TOP)or(not IsFLeter(ROMArr[Result]));
end;

procedure GetOrdosBlock(FS:TStream);
begin
  with Vars^ do
  if PartitionRomOffs<>0 then begin
    FS.Seek(PartitionOffs+OrdosSize, soFromBeginning);
    FS.ReadBuffer(ROMArr, sizeof(ROMArr));
    RomdiskType:=rtStandard;
    if StrPos(PChar(@ROMArr[ADRsignature]),PROsignature)=PChar(@ROMArr[ADRsignature]) then  {test for romdisk type: miniPRO or standard }
    begin // if miniPROromdisk
      RomdiskType:=rtPROmini;
      FS.Seek(PartitionOffs+$4D80, soFromBeginning);
      FS.ReadBuffer(ROMArr[0], $1270);
      FS.Seek(PartitionOffs+$1270, soFromBeginning);
      FS.ReadBuffer(ROMArr[$1270], $D90);     // $D80
    end;
  end;
end;

procedure PutOrdosBlock(FS:TStream);
begin
  with Vars^ do
  if PartitionRomOffs<>0 then begin
    case RomdiskType of
      rtStandard: begin
                    FS.Seek(PartitionOffs+OrdosSize, soFromBeginning);
                    FS.WriteBuffer(ROMArr, sizeof(ROMArr));
                  end;
      rtPROmini: begin
                    FS.Seek(PartitionOffs+$4D80, soFromBeginning);
                    FS.WriteBuffer(ROMArr[0], $1270);
                    FS.Seek(PartitionOffs+$1270, soFromBeginning);
                    FS.WriteBuffer(ROMArr[$1270], $D80);
                 end;
    end;
  end;
end;

procedure DelOrdosFile(FS:TStream; DelFName:string);
var ii, kk, aa, ss: integer;
    FL: TStringList;
begin
  with Vars^ do
  if PartitionRomOffs<>0 then try
    GetOrdosBlock(FS);
    FL:=TStringList.Create;
    if GetOrdosFileList(FL)>0 then begin
      ii:=FL.IndexOf(trim(DelFName));
      if ii>=0 then begin                      // file founded - then delete it
        aa:=integer(pointer(FL.Objects[ii]));                          // deleting file "address"
        ss:=16+PWord(@ROMArr[aa+10])^;                                 // deleting file size
        ii:=integer(pointer(FL.Objects[FL.Count-1]));                  // addres of last file
        kk:=16+PWord(@ROMArr[aa+10])^;                                 // last file size
        MoveMemory(@ROMArr[aa], @ROMArr[aa+ss], ii+kk-aa-ss);
        fillchar(ROMArr[ii+kk-ss], ss, $FF);
        PutOrdosBlock(FS);
      end;
    end;
  finally
    FL.Free;
  end;
end;

function PackOrdosFile(SrcFN:string; FSSrc:TStream; FS:TStream): integer;
var ii, max, next_ordos, datasize: integer;
    ss: string;
    FL: TStringList;
begin
  Result:=ERR_NO_DISK_SPACE;
  with Vars^ do try
    if PartitionRomOffs<>0 then begin
      GetOrdosBlock(FS);
      case RomdiskType of
        rtStandard: max:=sizeof(ROMArr);
        rtPROmini:  max:=$1270+$D80;
        else exit;
      end;
      FL:=TStringList.Create;
      next_ordos:=GetOrdosFileList(FL);
      if (next_ordos>=0) and (next_ordos<max-16) then begin
        if FSSrc.Read(PByte(@ROMArr[next_ordos])^, 16)=16 then          // read ordos header
        begin
          ss:='';
          for ii:=0 to 7 do
            if IsFLeter(ROMArr[next_ordos+ii]) then ss:=ss+chr(ROMArr[next_ordos+ii]);
          if (trim(ss)<>'') and (FL.IndexOf(trim(ss))>=0) then begin                       // file exists
            FL.Free;
            exit;
          end;
          datasize:=PWord(@ROMArr[next_ordos+10])^;
          if (datasize<max-next_ordos-16) then
          begin
            FSSrc.Read(PByte(@ROMArr[next_ordos+16])^, datasize);
            next_ordos:=next_ordos + datasize + 16;                         // 16 = ordos_header size
            Result:=0;
          end;
        end;
        ROMArr[next_ordos]:=$FF;   // end of ordos files chain
        if Result=0 then           // if success
          PutOrdosBlock(FS);
      end;
      FL.Free;
    end;
  except
    Result:=ERR_PACK_FILE;
    FL.Free;
  end;
end;

procedure GetOrdosFiles(OdiArchiveName:string);
var PFRec: PFileRec;
    FL: TStringList;
    FS: TFileStream;
    ii: integer;
begin
  FS:=nil;
  with Vars^ do try
    FS:=TFileStream.Create(OdiArchiveName, fmOpenRead or fmShareDenyWrite);
    GetOrdosBlock(FS);
    new(PFRec);                                 // 20161018 Special catalog for ORDOS files (within ROM)
    with PFRec^ do
    begin
      FileUser:=OrdosUser;
      FileName:=OrdosFiles;
      FileSize:=0;
      FileTime:=FileDate;
      FileAttr:=faDirectory;
      FExtents:=nil;
    end;
    FileList.Add(PFRec);
    FL:=TStringList.Create;
    if GetOrdosFileList(FL)>0 then
      for ii:=0 to FL.Count-1 do begin
        new(PFRec);                                 // add Ordos file to list
        with PFRec^ do
        begin
          FileUser:=OrdosUser;
          FileName:=OrdosFiles+'\'+FL[ii];
          FileSize:=16+PWord(@ROMArr[integer(pointer(FL.Objects[ii]))+10])^;
          FileTime:=FileDate;
          FileAttr:=0;      // faSysFile;
          FileAddr:=integer(pointer(FL.Objects[ii]));
          FExtents:=nil;
        end;
        FileList.Add(PFRec);
      end;
  finally
    if Assigned(FS) then FS.Free;
    FL.Free;
  end;
end;

function OdiGetCatalog(OdiArchiveName: string):integer;
var j: integer;
    PFRec: PFileRec;
begin
  with Vars^ do try
    DisposeFileList(FileList);
    FillChar(CatalogMap, sizeof(CatalogMap), $FF);
    FillChar(AllocationMap, sizeof(AllocationMap), $FF);
    for j:=0 to 15 do                           // users(subdirs)
    begin
      new(PFRec);
      with PFRec^ do
      begin
        FileUser:=j;
        FileName:='user_'+IntToStr(j);
        FileSize:=0;
        FileTime:=0;
        FileAttr:=faDirectory;
        FExtents:=nil;
      end;
      FileList.Add(PFRec);
    end;
    Result:=ScanCatalog(OdiArchiveName, fmOpenRead, ScanCatalogList, nil);
    if (Result>=0)and(BOOT.BOOTvalid) then begin
      new(PFRec);                                 // 20160909 Special catalog for sysgen (access system tracks)
      with PFRec^ do
      begin
        FileUser:=SystemUser;
        FileName:=SystemTracks;
        FileSize:=0;
        FileTime:=FileDate;
        FileAttr:=faDirectory;
        FExtents:=nil;
      end;
      FileList.Add(PFRec);
      if PartitionRomOffs<>0 then                 // 20161018 Special catalog for ORDOS files (within ROM)
        GetOrdosFiles(OdiArchiveName);
      new(PFRec);                                 // 0160909 Special file for sysgen (access system tracks)
      with PFRec^ do
      begin
        FileUser:=SystemUser;
        if BOOT.SystemBinValid then begin
          FileName:=SystemTracks+'\'+StrPas(BOOT.SystemBinRec.Name);
          FileSize:=BOOT.SystemBinRec.Size;
          FileTime:=BOOT.SystemBinRec.Date;
        end
        else begin
          FileName:=SystemTracks+'\'+SystemBin;
          FileSize:=PhySectorSize * BOOT.DPB.SEC * BOOT.DPB.OFF;
          FileTime:=FileDate;
        end;
        FileAttr:=0;      // faSysFile;
        FExtents:=nil;
      end;
      FileList.Add(PFRec);
    end;
  except
    Result:=-1;
  end;
end;

function ScanCatalogDelete(FS: TStream; SerialN: integer; var FCB: TFCB; PParam: pointer): boolean;
var DelUserN, E5: byte;
    DelFName: string;
    FName: string;
    j, jj: integer;
begin
  E5:=$E5;                                                    // deleted file flag
  FName:='';
  Result:=True;
  DelUserN:=ExtractFileUser(PString(PParam)^);
  DelFName:=ExtractFileName(PString(PParam)^);
  if DelUserN=OrdosUser then begin
    DelOrdosFile(FS, DelFName);
    exit;
  end;
  for j:=0 to sizeof(FCB.FileName)-1 do
  begin
    if (FCB.FileName[j]='\')or(FCB.FileName[j]='/') then FCB.FileName[j]:='-';
    if FCB.FileName[j]>' ' then
      FName:=FName+chr(ord(FCB.FileName[j]) and $7F);
    if j=7 then FName:=FName+'.';
  end;
  if (DelUserN=FCB.User) and (AnsiUpperCase(FName)=AnsiUpperCase(DelFName)) then    // delete FCB
  with Vars^ do try
    FS.Seek(0-sizeof(FCB), soFromCurrent);
    FS.Write(E5, 1);
    FS.Seek(sizeof(FCB)-1, soFromCurrent);
    CatalogMap[SerialN]:=$FFFF;
    for j:=0 to ExtentsInFCB-1 do
      if FCB.SizePartial<>0 then
      begin
        AllocationMap[GetFCBExtent(FCB.FCBextents, j)]:=$FFFF;
        if FCB.SizePartial>LogBlkInExt then
          FCB.SizePartial:=FCB.SizePartial-LogBlkInExt
        else
          FCB.SizePartial:=0;
      end;
    j:=0;
    while (j<FileList.Count) and
          (AnsiUpperCase(PFileRec(FileList.Items[j])^.FileName)<>AnsiUpperCase(PString(PParam)^)) do
      inc(j);
    if j<>FileList.Count then                   // found
    begin
      for jj:=0 to TList(PFileRec(FileList.Items[j])^.FExtents).Count-1 do
        dispose(PFCBExtent(TList(PFileRec(FileList.Items[j])^.FExtents).Items[jj]));
      TList(PFileRec(FileList.Items[j])^.FExtents).Free;
      dispose(PFileRec(FileList.Items[j]));
      FileList.Delete(j);
    end;
  except
    Result:=False;
  end;
end;

function OdiFileDelete(OdiArchiveName: string; FileToDelete: string):integer;
begin
  Result:=ScanCatalog(OdiArchiveName, fmOpenReadWrite, ScanCatalogDelete, @FileToDelete);
end;

function OdiFileExtract(OdiArchiveName: string; PFRec: PFileRec; OutName: string): integer;
var FS, FSOut: TFileStream;
    i, j, LogSize: integer;
    PFcbExt: PFCBExtent;
begin
 with Vars^ do begin
  Result:=ERR_FILE_OPEN;
  FS:=nil;
  FSOut:=nil;
  try
    FS:=TFileStream.Create(OdiArchiveName, fmOpenRead or fmShareDenyWrite);
    FSOut:=TFileStream.Create(OutName, fmCreate);
    i:=0;
    if PFRec^.FileUser=OrdosUser then begin
        FSOut.Write(ROMArr[PFRec^.FileAddr], PFRec^.FileSize);
        Result:=0;
      end
    else
    if PFRec^.FileUser=SystemUser then begin
        if PFRec^.FileSize>=sizeof(ExtentBuf) then
          SetLength(ExtentBuf, PFRec^.FileSize+1);
        FS.Seek(PartitionOffset, soFromBeginning);
        FS.Read(ExtentBuf[0], PFRec^.FileSize);
        FSOut.Write(ExtentBuf[0], PFRec^.FileSize);
        Result:=0;
      end
    else
    while i<PFRec^.FExtents.Count do
    begin
      PFcbExt:=PFRec^.FExtents.Items[i];    //SearchFCBExtent(PFRec, i);
      Result:=ERR_FILE_STRU;
      if PFcbExt=nil then
        break
      else
        with PFcbExt^ do
        begin
          j:=0;
          Result:=ERR_FILE_SIZE;
          if LogBlkCnt>$80 then
            break
          else
          begin
            if i=0 then
              LogSize:=OrdinalN*128 + LogBlkCnt
            else
              LogSize:=(OrdinalN-1-PFCBExtent(PFRec^.FExtents.Items[i-1])^.OrdinalN)*128 + LogBlkCnt;
            while LogSize>0 do
            begin
              Result:=ERR_FILE_SEEK;
              FS.Seek(PhySectorSize * BOOT.DPB.SEC * BOOT.DPB.OFF +
                      (GetFCBExtent(FCBextents, j) * ExtentSize) + PartitionOffset,    {V1.01}
                      soFromBeginning);
              FS.Read(ExtentBuf[0], ExtentSize);
              FSOut.Write(ExtentBuf[0], MIN(ExtentSize, LogSize*LogBlockSize));
              LogSize:=LogSize-LogBlkInExt;
              inc(j);
              Result:=0;
            end;
          end;
        end;
      inc(i);
    end;
  finally
    if Assigned(FS) then FS.Free;
    if Assigned(FSOut) then
    begin
      if Result=0 then
      begin
        FileSetAttr(OutName, PFRec^.FileAttr);
        if BOOT.TIMvalid then
          FileSetDate(FSOut.Handle, FileGetTime(FileList, PFRec^.FileName));
      end;
      FSOut.Free;
    end;
  end;
 end;
end;

procedure SetFCBExtent(var FCBExts:TFCBExtents; Index:integer; Value:word);
begin
 with Vars^ do
  if ExtentsInFCB<16 then
    FCBExts[Index]:=Value
  else
    PFCBExtents16(@FCBExts[0])^[Index]:=lo(Value);
end;

function SysgenCPM(NameIn, NameOut:string; FsIn,FsOut:TFileStream):integer;	//* sysgen */
var xJump: TJump;
    OutPos:integer;
begin
 with Vars^ do begin
  Result:=ERR_WRONG_DPB_CRC;
  SetLength(ExtentBuf, FsIn.Size+1);
  FsIn.Read(ExtentBuf[0], FsIn.Size);
  if DPBcrc(PBootDPB(@ExtentBuf[0])^)<>PBootDPB(@ExtentBuf[0])^.CRC     // exit if input file have not DPB
    then exit;
  with PExtBoot(@ExtentBuf[0])^ do
  try
    xJump:=DPB.jump;
    DPB:=BOOT.DPB;                                                      // save DPB info
    DPB.jump:=xJump;
    DPB.CRC:=DPBcrc(DPB);
    if BOOT.LBLvalid and
      (XorCRC(LBL,sizeof(LBL))=SLBL) then                               // if Volume Label exists on InpFile and on DestImage
    begin
      LBL:=BOOT.LBL;                                                    // then save it on DestImage
      SLBL:=BOOT.SLBL;
    end;
    if BOOT.UNMvalid and
      (XorCRC(@UNM[0,0], sizeof(UNM))=SUNM) then                        // if User Names exists on InpFile and on DestImage
    begin
      UNM:=BOOT.UNM;                                                    // then save it on DestImage
      SUNM:=BOOT.SUNM;
    end;
    if BOOT.TIMvalid and
      (XorCRC(PChar(@TIM),sizeof(TIM))=STIM) then                       // if FileDates exists on InpFile and on DestImage
    begin
      TIM:=BOOT.TIM;                                                    // then save it on DestImage
      STIM:=BOOT.STIM;
    end;
    OutPos:=FsOut.Position;
    FsOut.Write(ExtentBuf[0], FsIn.Size);                               // write system
    if (FsIn.Size)<(PhySectorSize*BOOT.DPB.Sec*BOOT.DPB.Off - sizeof(TSystemBinRec)) then begin
      FillChar(BOOT.SystemBinRec, sizeof(BOOT.SystemBinRec), 0);
      StrPLCopy(BOOT.SystemBinRec.Name, ExtractFileName(NameIn), sizeof(BOOT.SystemBinRec.Name)-1);
      BOOT.SystemBinRec.Size:=FsIn.Size;
      BOOT.SystemBinRec.Date:=FileGetDate(FsIn.Handle);
      BOOT.SystemBinRec.CRC:=DPBcrc(PBootDPB(@BOOT.SystemBinRec)^);
      BOOT.SystemBinValid:=True;
      FsOut.Seek(PhySectorSize*BOOT.DPB.Sec*BOOT.DPB.Off - sizeof(TSystemBinRec) + OutPos, soFromBeginning);
      FsOut.Write(BOOT.SystemBinRec, sizeof(BOOT.SystemBinRec));
    end;
    result:=0;
    GetBOOT(NameOut, FSOut, -1);
  except
    Result:=ERR_FILE_SEEK;
  end;
 end;
end;

function OdiFilePack(OdiArchiveName, SrcFileName, ArchFileName: string):integer;
var FS, FSSrc: TFileStream;
    FCB: TFCB;
    FreeExtent, FreeFCB, i, off, Attr, Readed, LogReaded, LogBlkCount: integer;
    FExists, flg: boolean;
begin
  Result:=-1;
  FS:=nil; FSSrc:=nil;
  with Vars^ do try
    FillChar(FCB, sizeof(FCB), 0);
    SrcFileName:=trim(SrcFileName);
    ArchFileName:=trim(ArchFileName);
    if ArchFileName='' then
      ArchFileName:=SrcFileName;
    FCB.User:=ExtractFileUser(ArchFileName);
    Result:=ERR_NO_DISK_SPACE;
    if FCB.User = OrdosUser then begin
      FSSrc:=TFileStream.Create(SrcFileName, fmOpenRead or fmShareDenyWrite);
      FS:=TFileStream.Create(OdiArchiveName, fmOpenReadWrite or fmShareDenyWrite);
      Result:=PackOrdosFile(SrcFileName,FSSrc,FS);
    end
    else
    if FCB.User = SystemUser then begin
      FSSrc:=TFileStream.Create(SrcFileName, fmOpenRead or fmShareDenyWrite);
      if (FSSrc.Size>sizeof(TAltBOOT))and(FSSrc.Size<=PhySectorSize*BOOT.DPB.Sec*BOOT.DPB.Off)and BOOT.BOOTvalid then begin
        FS:=TFileStream.Create(OdiArchiveName, fmOpenReadWrite or fmShareDenyWrite);
        FSSrc.Seek(0, soFromBeginning);
        FS.Seek(PartitionOffset, soFromBeginning);
        Result:=SysgenCPM(SrcFileName,OdiArchiveName,FSSrc,FS);
      end;
    end
    else begin
     FCB.User := FCB.User and $0F;
     StrPLCopy(FCB.FileName, padr(chrtran(ChangeFileExt(AnsiUpperCase(ExtractFileName(ArchFileName)), '.'), ' .', '_'), 8, ' '), 8);
     if FCB.FileName[0]=' ' then
     begin
       Result:=0;
       exit;
     end;
     StrPLCopy(@FCB.FileName[8], padr(chrtran(AnsiUpperCase(ExtractFileExt(ArchFileName)), ' .', '_'), 3, ' '), 3);
     Attr:=FileGetAttr(SrcFileName);
     if Attr and faReadOnly <> 0 then
       FCB.FileName[8]:=chr(ord(FCB.FileName[8]) or $80);
     if Attr and faHidden <> 0 then
       FCB.FileName[9]:=chr(ord(FCB.FileName[9]) or $80);
     FSSrc:=TFileStream.Create(SrcFileName, fmOpenRead or fmShareDenyWrite);
     off:=0; Attr:=0;
     FExists:=OdiFileExists(ArchFileName, i);
     if FExists then
     begin
       off:=PFileRec(FileList.Items[i])^.FileSize;
       Attr:=(PFileRec(FileList.Items[i])^.FileSize+LogBlockSize-1) div (ExtentSize*ExtentsInFCB);  // *8 ??? or *ExtentsInFCB ???
     end;
     if (FSSrc.Size<=OdiFreeDiskSpace()+off) then
     begin
       Result:=ERR_NO_DIR_SPACE;
       if ((FSSrc.Size+LogBlockSize-1) div (ExtentSize*ExtentsInFCB) <= OdiFreeDirSpace()+Attr) then  // *8 ??? or *ExtentsInFCB ???
       begin
         Result:=ERR_PACK_FILE;
         if FExists then
           OdiFileDelete(OdiArchiveName, ArchFileName);
        FS:=TFileStream.Create(OdiArchiveName, fmOpenReadWrite or fmShareDenyWrite);
        FSSrc.Seek(0, soFromBeginning);
        while FSSrc.Position<FSSrc.Size do
        begin
          for i:=0 to 7 do FCB.FCBextents[i]:=0;
          LogBlkCount:=0;
          off:=0;
          i:=0;
          flg:=false;
          Readed:=FSSrc.Read(ExtentBuf[0], ExtentSize*ExtentsInFCB);       // *8 ??? or *ExtentsInFCB ???
          ExtentBuf[Readed]:=#$1A;
          LogReaded:=Readed div LogBlockSize;
          if Readed mod LogBlockSize <>0 then
            inc(LogReaded);
          FreeFCB:=GetFreeFCB();
          while (LogReaded>0) and (i<ExtentsInFCB) do
          begin
            flg:=False;
            FreeExtent:=GetFreeExtent();
            FS.Seek((PhySectorSize * BOOT.DPB.Sec * BOOT.DPB.Off) +
                    (FreeExtent * ExtentSize) + PartitionOffset,                   {V1.01}
                    soFromBeginning);
            FS.Write(ExtentBuf[off], ExtentSize);
            SetFCBExtent(FCB.FCBextents, i, FreeExtent);
            AllocationMap[FreeExtent]:=FreeFCB;
            LogBlkCount:=LogBlkCount+min(LogBlkInExt, LogReaded);
            LogReaded:=LogReaded-LogBlkInExt;
            if (LogBlkCount>=128)and(LogReaded>0) then begin
              SetFCBordinalN(FCB, GetFCBordinalN(FCB)+1);
              if GetFCBordinalN(FCB)>MAX_EXTENT then
                raise Exception.Create(ErrFileLimit);
              LogBlkCount:=LogBlkCount mod 128;
              flg:=True;
            end;
            FCB.SizePartial:=LogBlkCount;
            off:=off+ExtentSize;
            i:=i+1;
          end;
          FS.Seek((PhySectorSize * BOOT.DPB.Sec * BOOT.DPB.Off) +
                  (FreeFCB * sizeof(FCB)) + PartitionOffset,                       {V1.01}
                  soFromBeginning);
          FS.Write(FCB, sizeof(FCB));
          if not flg then SetFCBordinalN(FCB, GetFCBordinalN(FCB)+1);
          if GetFCBordinalN(FCB)>MAX_EXTENT then
            raise Exception.Create(ErrFileLimit);
          ScanCatalogList(FS, FreeFCB, FCB, nil);                  // Add record to FileList
        end;
        Result:=0;
        FileSetTime(FileList, ArchFileName, FileGetDate(FSSrc.Handle));
       end;
     end;
    end;
  finally
    if Assigned(FS) then FS.Free;
    if Assigned(FSSrc) then FSSrc.Free;
  end;
end;

function GetPartInfo(OdiArchiveName:PChar):PChar; stdcall;
begin
 with Vars^ do begin
  Result:=@TmpBuf[0];
  TmpBuf[0]:=0;
  if not FileExists(OdiArchiveName) then exit;
  if OdiGetCatalog(OdiArchiveName)<0 then exit;
  StrLFmt(@TmpBuf[0], sizeof(TmpBuf)-1,
          '                Volume label: %s'#13#10+
          'Sectors per track(bytes): %d * %d'#13#10+
          '     Sides * Total tracks: %d * %d'#13#10+
          '             System tracks: %d'#13#10+
          '   Catalog size (bytes): %d'#13#10+
          'Total disk size (bytes):  %d  (%d free)'#13#10+
          '                    Total files: %d',
          [GetVolumeName(), BOOT.DPB.SEC, PhySectorSize, nSides,
           BOOT.DPB.TRK, BOOT.DPB.OFF, 32*(BOOT.DPB.DRM+1),
           BOOT.DPB.TRK*BOOT.DPB.SEC*PhySectorSize*nSides,
           OdiFreeDiskSpace(), FileList.Count-16]);
 end;
end;

function OdiGetInfo(OdiArchiveName: string): string;
begin
  Result:=string(GetPartInfo(PChar(OdiArchiveName)));
  if Result<>'' then
    Result:=Format('TotalCommander archiver (WCX) plugin for serving ODI files'#13#10+
                   '(Orion Disk Image files). Allow copy/extract CP/M files'#13#10+
                   'to/from ODI file "diskette" such simple as processing any'#13#10+
                   'archives with TotalCommander interface.'#13#10+
                   #13#10'FREEWARE Version 1.05.'+
                   #13#10'distributed "AS IS" WITHOUT ANY WARRANTY'#13#10+
                   #13#10'Copyright (C)2006-2016 Sergey A.'#13#10+
                   #13#10'Archive: `%s`'#13#10#13#10'%s',
                   [OdiArchiveName, Result]);
end;

function OdiCreateArchive(ArcFName: string): integer;
var FS, FSSys: TFileStream;
    i, Readed: integer;
begin
 with Vars^ do begin
  Result:=-1;
  FS:=nil; FSSys:=nil;
  i:=0;
  while i<FormatList.Count do
    with PFormatRec(FormatList.Items[i])^ do
    begin
      if (AnsiUpperCase(chrtran(ExtractFileExt(ArcFName), '.', ''))=AnsiUpperCase(ext))
      then
        break
      else
        inc(i);
    end;
  if i>=FormatList.Count then exit;           // format not found
  with PFormatRec(FormatList.Items[i])^ do
  try
    if sys='' then
    begin
      case DPB.LEN1 of
        0: PhySectorSize:=128;
        1: PhySectorSize:=256;
        2: PhySectorSize:=512
        else PhySectorSize:=1024;
      end;
      case DPB.LEN2 of
        0: nSides:=1
        else nSides:=2;
      end;
      LogBlkInExt := DPB.BLM+1;
      ExtentSize := LogBlkInExt*LogBlockSize;
      ExtentsInFCB := (DPB.EXM+1)*16384 div ExtentSize;
      SetLength(ExtentBuf, ExtentSize*ExtentsInFCB+1);
      if (PartitionOffset=0) then
        FS:=TFileStream.Create(ArcFName, fmCreate)
      else
        FS:=TFileStream.Create(ArcFName, fmOpenReadWrite or fmShareDenyWrite);
      FS.Seek(PartitionOffset, soFromBeginning);                       {V1.01}
      for i:=1 to DPB.TRK*DPB.SEC do
      begin
        FillChar(ExtentBuf[0], PhySectorSize*nSides, $E5);
        if i=1 then
          Move(DPB, ExtentBuf[0], sizeof(DPB));
        FS.Write(ExtentBuf[0], PhySectorSize*nSides);
      end;
      Result:=0;
    end
    else
    begin
      FSSys:=TFileStream.Create(Sys, fmOpenRead or fmShareDenyWrite);
      GetBOOT(Sys, FSSys, i);
      if not BOOT.BOOTvalid then                                            // wrong CRC
        Result:=ERR_WRONG_DPB_CRC
      else
      begin
        FS:=TFileStream.Create(ArcFName, fmCreate);
        FS.Seek(PartitionOffset, soFromBeginning);                     {V1.01}
        FSSys.Seek(0, soFromBeginning);
        Readed:=PhySectorSize*nSides;
        for i:=1 to BOOT.DPB.TRK*BOOT.DPB.SEC do
        begin
          FillChar(ExtentBuf[0], PhySectorSize*nSides, $E5);
          if Readed>=PhySectorSize*nSides then
            Readed:=FSSys.Read(ExtentBuf[0], PhySectorSize*nSides);
          if i=1 then
          begin
            SetVolumeName(ExtractFileName(ArcFName));
            Move(BOOT, ExtentBuf[0], 256);
          end;
          FS.Write(ExtentBuf[0], PhySectorSize*nSides);
        end;
        Result:=0;
      end;
    end;
  finally
    if Assigned(FS) then FS.Free;
    if Assigned(FSSys) then FSSys.Free;
  end;
 end;
end;

//////////////////////////////////////////////////////////////////////////////

function OpenArchivePart(ArcName: PChar; PartOffset: DWORD; PartN:DWORD): THandle; stdcall;
begin
 with Vars^ do begin
  PartitionN:=PartN;
  PartitionOffs:=PartOffset*512;
  PartitionRomOffs:=0;                                  // 65536 for ROM, 0 else
  RomdiskType:=rtNone;
  FileListPos := 0;
  ArcFileName := StrPas(ArcName);
  if not FileExists(ArcFileName) then
    OdiCreateArchive(ArcFileName);
  if OdiGetCatalog(ArcFileName)>=0 then
    Result := 1
  else
    Result := 0;
 end;
end;

function OpenArchive(var ArchiveData: TOpenArchiveData): THandle; stdcall;
begin
  Result:=OpenArchivePart(ArchiveData.ArcName, 0, 0);
  if Result=0 then
    ArchiveData.OpenResult := E_UNKNOWN_FORMAT;
end;

function ReadHeader(hArcData: THandle; var HeaderData: THeaderData): integer; stdcall;
begin
 with Vars^ do begin
  inc(FileListPos);
  if FileListPos = FileList.Count+1 then
  begin
    Result := E_END_ARCHIVE;
    FileListPos := 0;
  end
  else with PFileRec(FileList.Items[FileListPos-1])^ do
  begin
    Result := 0;
//    FileToProcess:=string(HeaderData.FileName);
    StrPCopy(HeaderData.FileName, FileName);
    HeaderData.FileAttr := FileAttr;
    HeaderData.PackSize := FileSize;
    HeaderData.UnpSize  := FileSize;
    HeaderData.FileTime := FileTime;
  end;
 end;
end;

function ProcessFile(hArcData: THandle; Operation: integer; DestPath, DestName: PChar): integer; stdcall;
var OutName: string;
//    ii: integer;
begin
 with Vars^ do begin
  if FileListPos = FileList.Count+1 then
    Result := E_END_ARCHIVE
  else
  begin
    if (Operation = PK_SKIP) or (Operation = PK_TEST) then
    begin
      Result := 0;
    end
    else
    begin
      if Assigned(DestPath) then
        OutName:=AddSlash(StrPas(DestPath))+StrPas(DestName)
      else
        OutName:=StrPas(DestName);
//      ii:=Length(FileToProcess);
//      while (ii>0)and(FileToProcess[ii]<>'\') do dec(ii);
//      while (ii>0)and(FileToProcess[ii]<>'\') do dec(ii);
//      delete(FileToProcess, 1, ii);
//      if OdiFileExists(FileToProcess, ii) then
      case OdiFileExtract(ArcFileName, PFileRec(FileList.Items[FileListPos-1]), OutName) of
                    0: Result:=0;
        ERR_FILE_STRU: Result:=E_BAD_DATA;
        ERR_FILE_SIZE: Result:=E_EREAD;
        ERR_FILE_SEEK: Result:=E_BAD_ARCHIVE
        else           Result:=E_EOPEN;
      end;
    end;
  end;
 end;
end;

function CloseArchive (hArcData: THandle): integer; stdcall;
begin
  Result := 0;
end;

function PackFiles(PackedFile, SubPath, SrcPath, AddList: PChar; Flags: integer): integer; stdcall;
begin
    Result := E_UNKNOWN_FORMAT;
    if not FileExists(PackedFile) then
      OdiCreateArchive(PackedFile);
    if OdiGetCatalog(PackedFile)<0 then
      exit;
    Result := 0;
    while AddList^ <> #0 do
    begin
      Result:=OdiFilePack(PackedFile,
                          AddSlash(StrPas(SrcPath))+AddList,
                          AddSlash(StrPas(SubPath))+AddList);
      if Result<0 then
        break
      else
        if (Flags and PK_PACK_MOVE_FILES) <> 0 then
          DeleteFile(AddSlash(StrPas(SrcPath))+AddList);
      Inc(AddList, StrLen(AddList) + 1);
    end;
    if Result=0 then
      SetBOOT(PackedFile);
end;

function DeleteFiles(PackedFile, DeleteList: PChar): integer; stdcall;
begin
  Result := E_UNKNOWN_FORMAT;
  if FileExists(PackedFile) then
  begin
    if OdiGetCatalog(PackedFile)<0 then
      exit;
    Result := 0;
    while DeleteList^ <> #0 do
    begin
      Result:=OdiFileDelete(PackedFile, DeleteList);
      if Result<0 then
        break;
      Inc(DeleteList, StrLen(DeleteList) + 1);
    end;
  end;
end;

function GetPackerCaps: integer; stdcall;
begin
  Result := PK_CAPS_NEW or
            PK_CAPS_MODIFY or
            PK_CAPS_MULTIPLE or
            PK_CAPS_DELETE or
            PK_CAPS_OPTIONS or
            PK_CAPS_BY_CONTENT;
end;

function CanYouHandleThisFile(FileName: PChar): boolean; stdcall;
begin
  Result := OdiGetCatalog(FileName)>=0;
end;

procedure SetChangeVolProc(hArcData: THandle; pChangeVolProc1: TChangeVolProc); stdcall;
begin
  DebugInfo('SetChangeVolProc');
end;

procedure SetProcessDataProc(hArcData: THandle; pProcessDataProc: TProcessDataProc); stdcall;
begin
  DebugInfo('SetProcessDataProc');
end;

procedure ConfigurePacker(Parent: HWND; DllInstance:LongWord); stdcall;
begin
  MessageBox(Parent, PChar(OdiGetInfo(Vars^.ArcFileName)), 'Information', MB_OK+MB_ICONINFORMATION);
end;

initialization
  new(Vars);
  with Vars^ do begin
    fillchar(Vars^,sizeof(TVars),0);
    PhySectorSize := $400;                                    // global
    nSides := 2;                                              // global
    ExtentSize := $800;
    ExtentsInFCB := 8;
    LogBlkInExt := 16;
    USE_DPBLESS_DISKS := 1;
    FileListPos := 0;
    PartitionOffs := 0;                                     // 0=for ODI, 1..x for OHI
    PartitionRomOffs := 0;                                  // 65536 for ROM, 0 else
    TmpBuf[sizeof(TmpBuf)-1] := 0;
    RomdiskType:=rtNone;
    FormatList:=TList.Create;
{$ifndef ORIONZEM}
    FileList:=TList.Create;
    if GetModuleFileName(hInstance, Pchar(@TmpBuf[0]), SizeOf(TmpBuf)-1)>0 then
      IniFileName:=ChangeFileExt(StrPas(PChar(@TmpBuf[0])), '.INI')
    else
      IniFileName:='Odi.ini';
    GetIniSettings(IniFileName);
{$endif}
  end;

finalization
  with Vars^ do begin
{$ifndef ORIONZEM}
    WritePrivateProfileString('PARAMS', 'USE_DPBLESS_DISKS',
                            PChar(IntToStr(USE_DPBLESS_DISKS)), PChar(IniFileName));
    if Assigned(FileList) then
    begin
      DisposeFileList(FileList);
      FileList.Free;
    end;
{$endif}
    if Assigned(FormatList) then
    begin
      DisposeFormatList(FormatList);
      FormatList.Free;
    end;
    fillchar(Vars^,sizeof(TVars),0);
  end;
  Dispose(Vars);
end.



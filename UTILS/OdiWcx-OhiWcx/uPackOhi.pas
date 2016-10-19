/////////////////////////////////////////////////////////////////////////
//                                                                     //
//   Orion/Z (Orion-128 + Z80-CARD-II) emulator.                       //
//                                                                     //
//   Addon: TotalCommander archiver (WCX) plugin for serving OHI files //
//          (Orion HDD Image files). Allow copy/extract CP/M files     //
//          to/from OHI "hdd image" such simple as processing any      //
//          archives in TotalCommander interface. PC MBR partitioning  //
//          scheme supported.    Version 1.05.                         //
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
//   Copyright (C)2016 Sergey A.                                       //
//                                                                     //
//   This program is free software; you can redistribute it and/or     //
//                  modify it in any ways.                             //
//   This program is distributed "AS IS" in the hope that it will be   //
//   useful, but WITHOUT ANY WARRANTY; without even the implied        //
//   warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  //
//                                                                     //
/////////////////////////////////////////////////////////////////////////

unit uPackOhi;

interface

uses
  Windows, SysUtils, Classes, wcxhead;

//{$DEFINE DEBUG}

const
  ERR_FILE_OPEN = -1;
  ERR_FILE_STRU = -2;
  ERR_FILE_SIZE = -3;
  ERR_FILE_SEEK = -4;

  ERR_NO_DISK_SPACE = -5;
  ERR_NO_DIR_SPACE  = -6;
  ERR_PACK_FILE     = -7;

  ERR_WRONG_DPB_CRC   = -8;
  ERR_WRONG_DISK_SIZE = -9;

  stSectionCommon        = 'COMMON';
  stLibList              = 'LibList';
  stOScode               = 'OScode';
  stDefFSSize            = 'DefaultFSsize';
  stOpenArchivePart      = 'OpenArchivePart';
  stReadHeader           = 'ReadHeader';
  stProcessFile          = 'ProcessFile';
  stCloseArchive         = 'CloseArchive';
  stPackFiles            = 'PackFiles';
  stDeleteFiles          = 'DeleteFiles';
  stGetPartInfo          = 'GetPartInfo';
  stCanYouHandleThisFile = 'CanYouHandleThisFile';

  MBR_Table = 446;
  MBR_PART_TYPE	= 4;

  PhySectorSize = 512;
  SystemSector = 'UseThis_ToAccess_MBR';
  SystemMBR = 'mbr.bin';
  SystemPartN = $FF;

  ZBootLoader: array [0..433] of byte =(
$C3, $08, $00, $00, $EE, $00, $00, $00, $0E, $05, $11, $1E, $F3, $AF, $12, $0D,
$28, $5D, $41, $26, $01, $3E, $02, $D6, $10, $10, $FC, $6F, $3E, $52, $BE, $20,
$EC, $23, $23, $23, $23, $E5, $D9, $E1, $11, $05, $00, $01, $08, $00, $ED, $B0,
$21, $08, $00, $1E, $20, $CD, $34, $F8, $C0, $D9, $21, $00, $EE, $3E, $C3, $BE,
$20, $CB, $3E, $66, $06, $1F, $86, $23, $10, $FC, $BE, $20, $C0, $23, $3A, $01,
$EE, $FE, $20, $20, $03, $21, $AD, $00, $C5, $06, $10, $CD, $A3, $00, $23, $10,
$FA, $21, $05, $00, $0E, $08, $ED, $B0, $21, $94, $01, $34, $C1, $18, $9E, $3A,
$1E, $F3, $B7, $3E, $FC, $C8, $21, $08, $00, $11, $EC, $00, $CD, $34, $F8, $C0,
$21, $BD, $00, $11, $00, $F2, $D5, $01, $F5, $00, $ED, $B0, $21, $36, $EE, $11,
$F5, $F2, $06, $14, $23, $CD, $A3, $00, $2B, $CD, $A3, $00, $23, $23, $10, $F4,
$AF, $12, $C9, $7E, $FE, $60, $38, $02, $E6, $5F, $12, $13, $C9, $43, $50, $2F,
$4D, $20, $4E, $4F, $20, $4C, $41, $42, $45, $4C, $20, $20, $20, $D3, $F9, $06,
$44, $21, $D9, $F2, $CD, $C9, $F2, $36, $0C, $CD, $B6, $F2, $3A, $D8, $F2, $CD,
$15, $F8, $CD, $B6, $F2, $21, $1E, $F3, $0E, $01, $7E, $B7, $28, $27, $CD, $B6,
$F2, $3A, $D6, $F2, $B9, $3E, $7F, $F5, $CC, $C1, $F2, $CD, $BF, $F2, $06, $10,
$CD, $C9, $F2, $CD, $BF, $F2, $F1, $CC, $C1, $F2, $11, $18, $00, $19, $0C, $3A,
$D7, $F2, $B9, $30, $D5, $11, $00, $18, $D5, $CD, $1B, $F8, $D1, $21, $D8, $F2,
$3C, $20, $0A, $1B, $7A, $B3, $20, $F0, $35, $3E, $0E, $20, $0F, $36, $05, $21,
$D6, $F2, $FE, $1A, $20, $09, $3E, $01, $BE, $30, $01, $35, $C3, $02, $F2, $FE,
$1B, $20, $09, $3A, $D7, $F2, $BE, $28, $F3, $34, $18, $F0, $FE, $0E, $20, $EC,
$46, $21, $14, $F3, $11, $18, $00, $19, $10, $FD, $72, $23, $72, $1E, $04, $D5,
$19, $1E, $20, $3E, $01, $D3, $F9, $E5, $CD, $34, $F8, $E1, $D1, $C0, $19, $11,
$0C, $00, $AF, $12, $1B, $01, $66, $08, $7E, $12, $81, $4F, $2B, $1B, $10, $F8,
$12, $C7, $C9, $3E, $0D, $CD, $C1, $F2, $3E, $0A, $18, $02, $3E, $20, $C5, $E5,
$CD, $0F, $F8, $E1, $C1, $C9, $E5, $7E, $B7, $28, $06, $CD, $C1, $F2, $23, $10,
$F6, $E1, $C9, $01, $00, $05, $1F, $53, $45, $4C, $45, $43, $54, $20, $42, $4F,
$4F, $54, $20, $50, $41, $52, $54, $49, $54, $49, $4F, $4E, $20, $4F, $4E, $20,
$0D, $0A);




type
  TOpenArchivePart=function(ArcName: PChar; PartOffset: DWORD): THandle; stdcall;
  TReadHeader=function(hArcData: THandle; var HeaderData: THeaderData): integer; stdcall;
  TProcessFile=function(hArcData: THandle; Operation: integer; DestPath, DestName: PChar): integer; stdcall;
  TCloseArchive=function(hArcData: THandle): integer; stdcall;
  TPackFiles=function(PackedFile, SubPath, SrcPath, AddList: PChar; Flags: integer): integer; stdcall;
  TDeleteFiles=function(PackedFile, DeleteList: PChar): integer; stdcall;
  TGetPackerCaps=function: integer; stdcall;
  TCanYouHandleThisFile=function(FileName: PChar): boolean; stdcall;
  TGetPartInfo=function(OdiArchiveName:PChar):PChar; stdcall;

  TFuncSet= record
              FOpenArchivePart: TOpenArchivePart;
              FReadHeader:      TReadHeader;
              FProcessFile:     TProcessFile;
              FCloseArchive:    TCloseArchive;
              FPackFiles:       TPackFiles;
              FDeleteFiles:     TDeleteFiles;
              FGetPartInfo:     TGetPartInfo;
              FCanYouHandleThisFile: TCanYouHandleThisFile;
            end;

  TArray16 = array [0..15] of byte;
  PArray16 = ^TArray16;

  TBootDPB = packed record    // Disk Parameters Header (BOOT .. BOOT+1EH, BOOT+1FH=CRC)
//-------------------------------------------------------------------- Orion specific
               JMP: array [0..7] of byte;
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
//-------------------------------------------------------------------- Orion specific
             end;
  PBootDPB = ^TBootDPB;

  TPartition=class(TCollectionItem)
  private
    FEnabled:  boolean;
    FActive:   boolean;
    FPartType: byte;
    FPartBeg:  DWORD;
    FPartSize: DWORD;
    FLibHandle:HMODULE;
    FFuncSet:  TFuncSet;
  protected
    function GetPartName(): string;
    function GetPartInfo(): string;
    function PartTypeStr(): string;
    function PartSizeStr(): string;
  public
    constructor Create(Collection: TCollection); override;
    destructor  Destroy; override;
    procedure   Update;
    function    GetFuncSet(LibName:string; var FSet: TFuncSet):HMODULE;
    property PartEnabled:boolean read FEnabled;
    property PartActive:boolean  read FActive;
    property PartType:byte   read FPartType;
    property PartBeg:DWORD   read FPartBeg;
    property PartSize:DWORD  read FPartSize;
    property PartInfo:string read GetPartInfo;
    property PartName:string read GetPartName;
    property LibHandle:HMODULE read FLibHandle;
  end;

  TPartitions = class(TCollection)
  private
    FLibList: string;
    FArcFName: string;
    FMBRScheme: boolean;
  protected
    function GetItem(Index: Integer): TPartition;
    procedure SetItem(Index: Integer; Value: TPartition);
    procedure SetArcFName(FName: string);
    procedure SetLibList(FList: string);
  public
    constructor Create; virtual;
    function AddPartition(PPartTab: PArray16): TPartition;
    property LibList:string read FLibList write SetLibList;
    property ArcFName:string read FArcFName write SetArcFName;
    property MBRScheme:boolean read FMBRScheme;
    property Items[Index: Integer]:TPartition read GetItem write SetItem; default;
  end;



function OhiCreateArchive(ArcFName: string): integer;
{}
function OpenArchive(var ArchiveData: TOpenArchiveData): THandle; stdcall;
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
procedure DisposeFileList(var List: TList);

implementation

type
  TFileRec = record
               FileName: string;
               FileSize: integer;
               FileTime: integer;
               FileAttr: integer;
             end;
  PFileRec = ^TFileRec;

  TVars = record
    FileList: TList;
    Partitions: TPartitions;
    TmpBuf: array[0..512] of byte;
    TmpBuf1k: array[0..1024] of byte;
    FileListPos: integer;
    DefaultFSsize: integer;    // 16Mb
    DefaultOScode: string;
    ArcFileName: string;
    IniFileName: string;
    BootDPB: TBootDPB;
  end;
  PVars = ^TVars;
  
var
  Vars: PVars;

procedure DebugInfo(str: string);
{$IFDEF DEBUG}
var ff: system.text;
{$ENDIF}
begin
{$IFDEF DEBUG}
  AssignFile(ff, 'c:\temp\OhiArc.debug');
  if FileExists('c:\temp\OhiArc.debug') then
    Append(ff)
  else
    Rewrite(ff);
  write(ff, FormatDateTime('dd.mm.yyyy hh:nn:ss - ', Now()));
  writeln(ff, str);
  CloseFile(ff);
{$ENDIF}
end;

//////////////////// strings utilities ////////////////////////

function AddSlash(str: string): string;
begin
  Result:=str;
  if (Length(Result)>0) and (Result[Length(Result)]<>'\')
  then Result:=Result+'\';
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

function GetPrivateString(SectionName,KeyName,DefaultValue:string):string;
var buf:array[0..1024] of char;
begin
  with Vars^ do
   GetPrivateProfileString(PChar(SectionName),PChar(KeyName),PChar(DefaultValue),
                           buf,sizeof(buf)-1,PChar(IniFileName));
   Result:=trim(StrPas(Buf));
end;

function GetPrivateInt(SectionName,KeyName:string;DefaultValue:Integer):integer;
begin
  with Vars^ do
   Result:=GetPrivateProfileInt(PChar(SectionName),PChar(KeyName),DefaultValue,PChar(IniFileName));
end;

function WritePrivateString(SectionName,KeyName,Value:string):boolean;
begin
  with Vars^ do
   Result:=WritePrivateProfileString(PChar(SectionName), PChar(KeyName),
                                     PChar(Value), PChar(IniFileName));
end;

function WritePrivateInt(SectionName,KeyName:string;Value:Integer):boolean;
begin
  with Vars^ do
   Result:=WritePrivateProfileString(PChar(SectionName),PChar(KeyName),
                                     PChar(IntToStr(Value)),PChar(IniFileName));
end;

/////////////////////////////////////////////////////////

function DPBcrc(DPB:PByte):byte;
var ii:integer;
begin
  Result:=$66;
  for ii:=0 to 30 do
  begin
    Result:=Result+DPB^;
    inc(DPB);
  end;
end;

procedure DisposeFileList(var List: TList);
begin
  if Assigned(List) then
    while List.Count>0 do with List do
    begin
      dispose(PFileRec(Items[Count-1]));
      Delete(Count-1);
    end;
end;

{
function GetVolumeName(): string;
var bb: byte;
begin
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

function SetVolumeName(VolName: string): boolean;
var xsum:byte;
    i: integer;
begin
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
}

function ExtractPartNum(FName: string): byte;     // 'PARTITION_1\USER_12\filename.ext' -> 1
var i: integer;
begin
  Result:=SystemPartN;
  i:=1;
  if pos(SystemSector+'\',FName)<>0 then exit;
  while (i<Length(FName)) and (not (FName[i] in ['0'..'9'])) do inc(i);
  if Length(FName)>0 then
    Result:=StrTointDef(copy(FName, i, 1), $FF);
end;

function OhiGetCatalog(OhiArchiveName: string):integer;
var j: integer;
    PFRec: PFileRec;
    Res, xRes: boolean;
    hArcData: THandle;
    HeaderData: THeaderData;
begin
  Res:=True;
  Result:=-1;
  with Vars^ do try
    DisposeFileList(FileList);
    Partitions.ArcFName:=OhiArchiveName;
    if Partitions.MBRScheme then begin
      for j:=0 to Partitions.Count-1 do with Partitions[j] do
      begin
        new(PFRec);
        with PFRec^ do
        begin
          FileName:=PartName;
          FileSize:=0;
          FileTime:=0;
          FileAttr:=faDirectory;
        end;
        FileList.Add(PFRec);
        if PartEnabled then
        begin
          hArcData:=FFuncSet.FOpenArchivePart(PChar(OhiArchiveName), PartBeg);
          xRes:=hArcData>0;
          Res:=Res and xRes;
          while (xRes and (FFuncSet.FReadHeader(hArcData, HeaderData)<>E_END_ARCHIVE) ) do
          begin
            new(PFRec);
            with PFRec^ do
            begin
              FileName:=AddSlash(PartName)+string(HeaderData.FileName);
              FileSize:=HeaderData.UnpSize;
              FileTime:=HeaderData.FileTime;
              FileAttr:=HeaderData.FileAttr;
            end;
            FileList.Add(PFRec);
          end;
        end;
      end;
     new(PFRec);                                 // 20160909 Special catalog for sysgen (access MBR)
     with PFRec^ do
     begin
      FileName:=SystemSector;
      FileSize:=0;
      FileTime:=0;
      FileAttr:=faDirectory;
     end;
     FileList.Add(PFRec);
     new(PFRec);                                 // 0160909 Special file for sysgen (access MBR)
     with PFRec^ do
     begin
        FileName:=SystemSector+'\'+SystemMBR;
        FileSize:=PhySectorSize;
        FileTime:=0;                            // FileGetDate(FSSrc.Handle)
        FileAttr:=0;                            // faSysFile;
      end;
      FileList.Add(PFRec);
    end;
    if Res and Partitions.MBRScheme then Result:=0;
  except
    Result:=-1;
  end;
end;

{ TPartition }

constructor TPartition.Create(Collection: TCollection);
begin
  inherited Create(Collection);
  FLibHandle:=0;
  with FFuncSet do begin
    FOpenArchivePart:=nil;
    FReadHeader:=nil;
    FProcessFile:=nil;
    FCloseArchive:=nil;
    FPackFiles:=nil;
    FDeleteFiles:=nil;
    FCanYouHandleThisFile:=nil;
    FGetPartInfo:=nil;
  end;
end;

destructor TPartition.Destroy;
begin
  if FLibHandle<>0 then
  begin
    FreeLibrary(FLibHandle);
    FLibHandle:=0;
  end;
  inherited Destroy;
end;

function TPartition.GetFuncSet(LibName: string; var FSet: TFuncSet): HMODULE;
var LName: string;
begin
  LName:=trim(LibName)+IntToStr(Index);
  if (FileExists(LName) and DeleteFile(LName)) or (not FileExists(LName)) then
    if not CopyFile(PChar(LibName), PChar(LName), False) then
      raise Exception.CreateFmt('Can not copy `%s`'#13#10'to   `%s`', [LibName, LName]);
  Result:=LoadLibrary(PChar(LName));
  if Result=0 then
    raise Exception.CreateFmt('Error during loading '#13#10#10'`%s`', [LName])
  else with FSet do begin
    FOpenArchivePart:=GetProcAddress(Result, stOpenArchivePart);
    FReadHeader:=GetProcAddress(Result, stReadHeader);
    FProcessFile:=GetProcAddress(Result, stProcessFile);
    FCloseArchive:=GetProcAddress(Result, stCloseArchive);
    FPackFiles:=GetProcAddress(Result, stPackFiles);
    FDeleteFiles:=GetProcAddress(Result, stDeleteFiles);
    FGetPartInfo:=GetProcAddress(Result, stGetPartInfo);
    FCanYouHandleThisFile:=GetProcAddress(Result, stCanYouHandleThisFile);
  end;
  if not ( Assigned(FSet.FOpenArchivePart) and
           Assigned(FSet.FReadHeader) and
           Assigned(FSet.FProcessFile) and
           Assigned(FSet.FCloseArchive) and
           Assigned(FSet.FPackFiles) and
           Assigned(FSet.FDeleteFiles) and
           Assigned(FSet.FGetPartInfo) and
           Assigned(FSet.FCanYouHandleThisFile) )
  then begin
         FreeLibrary(Result);
         raise Exception.CreateFmt('Error obtainig entry point in '#13#10#10'`%s`', [LName]);
       end;
end;

function TPartition.GetPartInfo: string;
begin
  Result:='';
  if FEnabled and Assigned(FFuncSet.FGetPartInfo) then
    Result:=FFuncSet.FGetPartInfo( PChar(TPartitions(Collection).ArcFName) );
end;

function TPartition.GetPartName: string;
begin
  Result:=Format('Partition_%d--%s,%s',[Index,PartTypeStr(),PartSizeStr()]);
end;

function TPartition.PartSizeStr: string;
var PSz:integer;
begin
  PSz:=FPartSize div 2;
  if (PSz<1024) then
    Result:=Format('%dK', [PSz])
  else if (FPartSize<1024*1024) then
    Result:=Format('%dM', [(PSz+512) div 1024])
  else
    Result:=Format('%dG', [(PSz+(512*1024)) div (1024*1024)])
end;

function TPartition.PartTypeStr: string;
begin
  case FPartType of
    0: Result:='';
    $01,$04,$06,$0B,$0C,$0E,$0F,$11,$14,$16,$1B,$1C,$1E,$8b,$8c: result:='FAT';
    $21: result:='UZIX';
    $52, $D8, $DB: result:='CPM';
    $FF, $02, $03: result:='XENIX';
    $07, $86, $87: result:='NTFS';
    $08, $09: result:='AIX-OS2';
    $63: result:='UNIX';
    $64, $65, $51: result:='Novell';
    $83, $85: result:='Linux';
    $0A: result:='OS2';
    $05: result:='extended';
    $A0: result:='hiber';
    $A5, $A6, $A9: result:='BSD';
    $BE: result:='Solaris';
    $82: result:='Linux-Solaris';
    $40: result:='VENIX';
    $C0: result:='CTOS'
    else result:='Unknown';
  end;
end;

procedure TPartition.Update;
var liblst, lib: string;
    FuncSet: TFuncSet;
    libHandle:HMODULE;
begin
 with Vars^ do begin
  liblst:=TPartitions(Collection).LibList;
  while liblst<>'' do begin
    lib:=LeftSubstr(liblst);
    if ExtractFileName(lib)=lib then
      lib:=AddSlash(ExtractFilePath(IniFileName))+lib;
    if FLibHandle<>0 then begin
      FreeLibrary(FLibHandle);
      FLibHandle:=0;
    end;
    LibHandle:=GetFuncSet(lib, FuncSet);
    FEnabled:=(FuncSet.FOpenArchivePart(PChar(TPartitions(Collection).ArcFName), PartBeg)>0)
              and FuncSet.FCanYouHandleThisFile(PChar(TPartitions(Collection).ArcFName));
    if FEnabled then
    begin
      FLibHandle:=libHandle;
      FFuncSet:=FuncSet;
      liblst:='';
    end
    else
      FreeLibrary(libHandle);
  end;
 end;
end;

{ TPartitions }

function TPartitions.AddPartition(PPartTab: PArray16): TPartition;
begin
  Result:=TPartition(Add);
  if Assigned(Result) then with Result do
  begin
    FActive:=  PPartTab^[0]=$80;
    FPartType:=PPartTab^[4];
    FPartBeg:= PDWORD(@PPartTab^[8])^;
    FPartSize:=PDWORD(@PPartTab^[12])^;
    Update;
  end;
end;

constructor TPartitions.Create;
begin
  inherited Create(TPartition);
  FLibList:='';
  FArcFName:='';
end;

function TPartitions.GetItem(Index: Integer): TPartition;
begin
  Result := TPartition(inherited Items[Index]);
end;

procedure TPartitions.SetArcFName(FName: string);
var i, pt: integer;
    FS: TFileStream;
begin
 with Vars^ do begin
  FName:=trim(FName);
  if FArcFName<>FName then
  begin
     FArcFName:=FName;
     for i:=Count-1 downto 0 do delete(i);
     FS:=nil;
     try
       FS:=TFileStream.Create(FName, fmOpenReadWrite or fmShareDenyWrite);
       FS.Seek(0, soFromBeginning);                                      {V1.01}
       FS.Read(TmpBuf, 512);
       FMBRScheme:=(TmpBuf[510]=85)and(TmpBuf[511]=170);                // 55 AA
     finally
       if Assigned(FS) then FS.Free;
     end;
     pt:=MBR_Table;
     if MBRScheme then for i:=0 to 3 do
       if TmpBuf[i*16+ pt + MBR_PART_TYPE]<>0 then                      // nondelete partitions
         AddPartition( PArray16(@TmpBuf[i*16+ pt]) );
  end;
 end;
end;

procedure TPartitions.SetItem(Index: Integer; Value: TPartition);
begin
  (inherited Items[Index]).Assign(TCollectionItem(Value));
end;

procedure TPartitions.SetLibList(FList: string);
begin
  FList:=trim(FList);
  if FLibList<>FList then
     FLibList:=FList;
end;

//////////////////////////////////////////////////////////////////////////////

function OpenArchive(var ArchiveData: TOpenArchiveData): THandle; stdcall;
begin
 with Vars^ do begin
  FileListPos := 0;
  ArcFileName := StrPas(ArchiveData.ArcName);
  if not FileExists(ArcFileName) then
    OhiCreateArchive(ArcFileName);
  if OhiGetCatalog(ArcFileName)>=0 then
    Result := 1
  else
  begin
    ArchiveData.OpenResult := E_UNKNOWN_FORMAT;
    Result := 0;
  end;
 end;
end;

function ReadHeader(hArcData: THandle; var HeaderData: THeaderData): integer; stdcall;
var PartN: integer;
    xHeaderData: THeaderData;
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
    Result:=0;
    StrPCopy(HeaderData.FileName, FileName);
    HeaderData.FileAttr := FileAttr;
    HeaderData.PackSize := FileSize;
    HeaderData.UnpSize  := FileSize;
    HeaderData.FileTime := FileTime;
//    if Length(FileName)>12 then begin
    if Pos('\',FileName)>0 then begin                                   // 20160726
      PartN:=ExtractPartNum( FileName );
      xHeaderData:=HeaderData;
      if PartN<SystemPartN then
        Partitions[PartN].FFuncSet.FReadHeader(hArcData, xHeaderData);
    end;
  end;
 end;
end;

function ProcessFile(hArcData: THandle; Operation: integer; DestPath, DestName: PChar): integer; stdcall;
var PartN: integer;
    OutName: string;
    FS, FSOut: TFileStream;
    TmpBuf:array[0..PhySectorSize] of byte;
begin
 with Vars^ do begin
  if FileListPos = FileList.Count+1 then
    Result := E_END_ARCHIVE
  else
  begin
    if (Operation = PK_SKIP) or (Operation = PK_TEST) then
      Result := 0
    else begin
      PartN:=ExtractPartNum( PFileRec(FileList.Items[FileListPos-1])^.FileName );
      if PartN=SystemPartN then begin                                                  // MBR access
        if Assigned(DestPath) then
          OutName:=AddSlash(StrPas(DestPath))+StrPas(DestName)
        else
          OutName:=StrPas(DestName);
        Result:=ERR_FILE_OPEN;
        FS:=nil;
        FSOut:=nil;
        try
          FS:=TFileStream.Create(ArcFileName, fmOpenRead or fmShareDenyWrite);
          FSOut:=TFileStream.Create(OutName, fmCreate);
          FS.Seek(0, soFromBeginning);
          FS.Read(TmpBuf, PhySectorSize);
          FSOut.Write(TmpBuf, PhySectorSize);
          Result:=0;
        finally
          if Assigned(FS) then FS.Free;
          if Assigned(FSOut) then FSOut.Free;
        end;
      end
      else begin                                                                       // partitions access
        if PartN<SystemPartN then
          Result:=Partitions[PartN].FFuncSet.FProcessFile(hArcData, Operation, DestPath, DestName);
      end
    end;
  end;
 end;
end;

function CloseArchive (hArcData: THandle): integer; stdcall;
begin
  Result := 0;
end;

function PackFiles(PackedFile, SubPath, SrcPath, AddList: PChar; Flags: integer): integer; stdcall;
var PartNum:byte;
    FS, FSOut: TFileStream;
    TmpBuf:array[0..PhySectorSize] of byte;
begin
 with Vars^ do begin
  Result := E_UNKNOWN_FORMAT;
  if not FileExists(PackedFile) then
    OhiCreateArchive(PackedFile);
  if OhiGetCatalog(PackedFile)>=0 then
  begin
    PartNum:=ExtractPartNum( AddSlash(StrPas(SubPath))+AddList );
    if (PartNum=SystemPartN) then
    try                                                                                         // write MBR.bin
      FS:=TFileStream.Create(AddSlash(StrPas(SrcPath))+AddList, fmOpenRead or fmShareDenyWrite);
      FSOut:=TFileStream.Create(string(PackedFile), fmOpenReadWrite or fmShareDenyWrite);
      FS.Read(TmpBuf, PhySectorSize);
      Result := ERR_FILE_STRU;
      if (FS.Size>=PhySectorSize)and(TmpBuf[$1FE]=$55)and(TmpBuf[$1FF]=$AA) then begin          // if valid MBR file
        FSOut.Read(TmpBuf, PhySectorSize);
        FS.Seek(0, soFromBeginning);
        FS.Read(TmpBuf, PhySectorSize-(16*4 + 2));
        TmpBuf[$1FE]:=$55;
        TmpBuf[$1FF]:=$AA;
        FSOut.Seek(0, soFromBeginning);
        FSOut.Write(TmpBuf, PhySectorSize);
        Result:=0;
      end;
    finally
      if Assigned(FS) then FS.Free;
      if Assigned(FSOut) then FSOut.Free;
    end
    else begin
      if (SubPath=nil)and(pos('\',string(AddList))=0) then
        PartNum:=0;
      Result:=Partitions[ PartNum ].FFuncSet.FPackFiles(PackedFile, SubPath, SrcPath, AddList, Flags);
    end;
  end;
 end;
end;

function DeleteFiles(PackedFile, DeleteList: PChar): integer; stdcall;
begin
  Result := E_UNKNOWN_FORMAT;
  with Vars^ do
   if FileExists(PackedFile) then
     if OhiGetCatalog(PackedFile)>=0 then
       Result:=Partitions[ ExtractPartNum( DeleteList ) ].FFuncSet.FDeleteFiles(PackedFile, DeleteList);
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
  Result := OhiGetCatalog(FileName)>=0;
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
var st: string;
    i: integer;
begin
 with Vars^ do begin
  st:='';
  for i:=0 to Partitions.Count-1 do
    with Partitions[i] do
      st:=st+#13#10#13#10+PartName+#13#10+PartInfo;
  MessageBox(Parent,
             PChar( Format('TotalCommander archiver (WCX) plugin for serving OHI files'#13#10+
                           '(Orion HDD Image files). Allow copy/extract CP/M files'#13#10+
                           'to/from OHI "hdd image" such simple as processing any'#13#10+
                           'archives with TotalCommander interface.'#13#10+
                           #13#10'FREEWARE Version 1.05,'+
                           #13#10'distributed "AS IS" WITHOUT ANY WARRANTY'#13#10+
                           #13#10'Copyright (C)2008-2016 Sergey A.'#13#10+
                           #13#10'Archive: `%s`'+st,
                           [ArcFileName]) ),
             'Information', MB_OK+MB_ICONINFORMATION);
 end;
end;

function CpmBootValid(buf: PByte; var crc: byte):boolean;
var i: integer;
begin
  crc:=$66;
  for i:=0 to 30 do
  begin
    crc:=crc+buf^;
    inc(buf);
  end;
  result:=(crc=buf^);
end;

function XorCRC(buf: PByte; cnt:integer): byte;
var ii: integer;
begin
  Result:=buf^;
  inc(buf);
  for ii:=1 to cnt-1 do
  begin
    Result:=Result xor buf^;
    inc(buf);
  end;
end;

function OhiCreateArchive(ArcFName: string): integer;   { Creating CP/M filesystem with 8k block, 256 fcbs and 24k system offset }
var i, readed: integer;
    FS, FSOS: TFileStream;
    psize: integer;                                     { partition size in 512k blocks }
begin
 with Vars^ do begin
  Result:=-1;
  readed:=0;
  psize:=DefaultFSsize div 512;
  if psize>65535 then begin
    psize:=65535;                     // limit filesystem size to 32Mb
    if (BootDPB.DSM>4096) then
    begin
      MessageBox(0, 'Filesystem size>32M - it can overflow AltairDOS ALV buffer -'#13#10'Partition size truncated to 32M',
                 'Warning', MB_ICONEXCLAMATION+MB_OK);
      exit;
    end;
  end;
  FillChar(TmpBuf, sizeof(TmpBuf), 0);
  Move(ZBootLoader, TmpBuf, sizeof(ZBootLoader));
  TmpBuf[510] := 85;     // 55
  TmpBuf[511] := 170;    // AA
  TmpBuf[MBR_Table + MBR_PART_TYPE] := $52;              { CP/M }
  PDWORD(@TmpBuf[MBR_Table + 8])^ := 1;
  PDWORD(@TmpBuf[MBR_Table + 12])^ := psize;
  FS:=nil;
  FSOS:=nil;
  try
    FS:=TFileStream.Create(ArcFName, fmCreate);
    FS.Write(TmpBuf, 512);                              { write MBR }
    FillChar(TmpBuf, sizeof(TmpBuf), $E5);
    for i:=2 to DefaultFSsize div 512 do
      FS.Write(TmpBuf, 512);                            { initialize drive image body }
{}
    FS.Seek(512, soFromBeginning);                      { goto sector 1 }
    FillChar(TmpBuf, sizeof(TmpBuf), 0);
    FillChar(BootDPB, 0, sizeof(BootDPB));
    if FileExists(DefaultOScode) then
    begin
      FSOS:=TFileStream.Create(DefaultOScode, fmOpenReadWrite or fmShareDenyWrite);
      if Assigned(FSOS) then begin
        FSOS.Seek(0, soFromBeginning);
        readed:=FSOS.Read(TmpBuf, 512);
      end;
      BootDPB.JMP:=PBootDPB(@TmpBuf[0])^.JMP;
    end
    else
    begin
      strpcopy(@TMPBuf[$20], 'NEW FILESYSTEM  ');
      BootDPB.JMP[1]:=$30;
      for i:=1 to 15 do		                // Create UserNames
        strlcopy(@TMPBuf[i*16+256], strfmt(@TMPBuf[8], 'USER_%d          ', [i]), 16);
      TmpBuf[$FD]:=XorCRC(@TMPBuf[$20], 16);	// calculate label crc
      TmpBuf[$FE]:=XorCRC(@TMPBuf[256], 256);	// calculate UserNames crc
    end;

    BootDPB.SPT:=192;			        // count of 128b sectors per track
    BootDPB.LEN1:=2;			        // phisizal sector length = 512b
    BootDPB.SEC:=BootDPB.SPT shr 2;		// track size in 512b sectors
    BootDPB.TRK:=psize div BootDPB.SEC;
    BootDPB.OFF:=1;			        // 24576 bytes
    BootDPB.BSH:=6;
    BootDPB.BLM:=63;			        // allocation_unit = 8192b
    BootDPB.DSM:=WORD(( DWORD(BootDPB.SPT) *(BootDPB.TRK-BootDPB.OFF)) shr 6)-1; // 128/8192=1/64
    if (BootDPB.DSM<256) then
      BootDPB.EXM:=7
    else BootDPB.EXM:=3;
    BootDPB.DRM:=255;			        // 8192 / 32 -1
    BootDPB.AL:=$80;			        // one block catalog size = 8192
    BootDPB.CKS:=0;			        // fixed disk

    CpmBootValid(@BootDPB, BootDPB.CRC);	// calculate bootsector crc
    Move(BootDPB, TmpBuf, 32);		        // Create BOOT sector

    if (BootDPB.DSM>4096) then
    begin
      MessageBox(0, 'Filesystem size>32M - it can overflow AltairDOS ALV buffer - exiting', 'Warning', MB_ICONEXCLAMATION+MB_OK);
      exit;
    end;
    if (readed=512) then
      if Assigned(FSOS) then readed:=FSOS.Read(TmpBuf1k, 1024);
    if XorCRC(@TmpBuf1k, 768)=TmpBuf[$FF] then begin              // if source contain filetime area
      FillChar(TmpBuf1k, 768, 0);
      TmpBuf[$FF]:=0;
    end;

    FS.Write(TmpBuf, 512);                       { write BOOTSECTOR, USERNAMES }
    FS.Write(TmpBuf1k, readed);                  { write FILEDATES & 256 bytes OS code}

    i:=3*4;                                      // 128b-sector counter (inside system tracks)
    readed:=readed - 512;
    while (readed=512) and (i<BootDPB.SPT*BootDPB.OFF) and Assigned(FSOS) do
    begin
      readed:=FSOS.Read(TmpBuf, 512);
      FS.Write(TmpBuf, readed);                  { write OS code}
    end;
{}
    Result:=0;
  finally
    if Assigned(FS) then FS.Free;
    if Assigned(FSOS) then FSOS.Free;
  end;
 end;
end;

initialization
  new(Vars);
  with Vars^ do begin
    fillchar(Vars^,sizeof(TVars),0);  //    FileListPos := 0;
    DefaultFSsize := 16777216;        // 16Mb
{}
    FileList:=TList.Create;
    Partitions:=TPartitions.Create;
    if GetModuleFileName(hInstance, Pchar(@TmpBuf[0]), SizeOf(TmpBuf)-1)>0 then
      IniFileName:=ChangeFileExt(StrPas(PChar(@TmpBuf[0])), '.INI')
    else
      IniFileName:='Ohi.ini';
    Partitions.LibList:=GetPrivateString(stSectionCommon, stLibList, 'odi.wcx');
    DefaultOScode:=trim(GetPrivateString(stSectionCommon, stOScode, 'system.hdd'));
    if ExtractFileName(DefaultOScode)=DefaultOScode then
      DefaultOScode:=AddSlash(ExtractFilePath(IniFileName))+DefaultOScode;
    DefaultFSsize:=GetPrivateInt(stSectionCommon, stDefFSSize, 16777216);    // 16Mb
  end;

finalization
  with Vars^ do begin
    WritePrivateString(stSectionCommon, stLibList, Partitions.LibList);
    WritePrivateString(stSectionCommon, stOsCode, DefaultOScode);
    WritePrivateInt(stSectionCommon, stDefFSSize, DefaultFSsize);
    Partitions.Free;
    if Assigned(FileList) then
    begin
      DisposeFileList(FileList);
      FileList.Free;
    end;
  end;
  fillchar(Vars^,sizeof(TVars),0);
  dispose(Vars);

end.


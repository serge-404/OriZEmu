program HddUtil;
{$APPTYPE CONSOLE}
uses
  Windows,
  SysUtils,
  HDDUtils in 'HDDUtils.pas';

{$R *.RES}

procedure WriteCon(const Fmt: string; const Args: array of const);
begin
    Write(Format(Fmt, Args));
end;

procedure ReadCon(var s: string);
begin
    Readln(s);
end;

procedure Usage;
begin
    WriteCon(#13#10'Hdd Drive Image Processing Utility by Sergey A., version 1.0, freeware.'#13#10#13#10+
             'Usage: HddUtil [<source> <destination>] [modifiers]'#13#10#13#10+
             'Details: HddUtil <drive> <image> [/s=<nn>] [/c=<nn>] [/q]'#13#10+
             '         HddUtil <image> <drive> [/s=<nn>] [/c=<nn>] [/q]'#13#10+
             '         HddUtil /list [<drive>]'#13#10#13#10+
             ' <drive>:  HddDrive or Partition, typically "0:".."9:" or "C:".."Z:"'#13#10+
             ' <image>:  Disk image (file) to create or write'#13#10+
             '    <nn>:  decimal number'#13#10+
             '     /s=:  512b-sector (LBA number) to beginning from'#13#10+
             '     /c=:  process count of 512b-sectors'#13#10+
             '      /q:  quet mode (no progress counter)'#13#10+
             '   /list:  show HddDrive(s) info'#13#10#13#10+
             'Example: HddUtil 0: c:\temp\drive_c_first200sectors.img /c=200 /q'#13#10,
             []
    );
    halt(EXIT_FAILURE);
END;

function PCharNStr(pc:PChar; N: integer): string;
var i: integer;
begin
  Result:='';
  i:=0;
  while (i<N) and (pc[i]<>#0) do
  begin
    Result:=Result+pc[i];
    inc(i);
  end;
end;


function max(a, b: ULONG):ULONG;
begin
  if a>b then max:=a else max:=b;
end;

procedure IdeIdentifyGen(Buffer: PChar; ImageSize: int64; c, h, s: word);
var cs: byte;
    i: integer;
    li: LARGE_INTEGER;
begin
  li.QuadPart:=ImageSize;
  li.QuadPart:=li.QuadPart shr 9;   //   /512
  with PIdSector(Buffer)^ do
  begin
    StrPCopy(sSerialNumber, 'UNKNOWN ');                     // serial
    ChangeByteOrder(sSerialNumber,SizeOf(sSerialNumber));
    StrPCopy(sFirmwareRev,  'UNKNOWN ');                     // firmware
    ChangeByteOrder(sFirmwareRev,SizeOf(sFirmwareRev));
    StrPCopy(sModelNumber,  'LOCAL DRIVE ');                 // model
    ChangeByteOrder(sModelNumber,SizeOf(sModelNumber));
  end;
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

function TextPartType(pt: byte): string;
begin
  case pt of
    0: Result:='';
    $01,$04,$06,$0B,$0C,$0E,$0F,$11,$14,$16,$1B,$1C,$1E,$8b,$8c: result:='FAT';
    $21: result:='UZIX';
    $52, $D8, $DB: result:='CP/M';
    $FF, $02, $03: result:='XENIX';
    $07, $86, $87: result:='NTFS';
    $08, $09: result:='AIX or OS/2';
    $63: result:='UNIX';
    $64, $65, $51: result:='Novell';
    $83, $85: result:='Linux';
    $0A: result:='OS/2';
    $05: result:='extended';
    $A0: result:='hiber';
    $A5, $A6, $A9: result:='*BSD';
    $BE: result:='Solaris';
    $82: result:='Linux or Solaris';
    $40: result:='VENIX';
    $C0: result:='CTOS'
    else result:='Unknown';
  end;
end;

const
  cmNone=0;
  cmRead=1;
  cmWrite=2;
  cmList=3;

var
  ss, PartInf: string;
  nCommand: integer = cmNone;
  Drive: Char = ' ';
  fQuet: boolean = false;
  fVerify: boolean = false;
  pcszFile: Array[0..BLOCK_SIZE] of char;
  buffer: Array[0..BLOCK_SIZE] of char;
  hfile: THANDLE = INVALID_HANDLE_VALUE;
  i: integer;
  iSector, iMaxSector, iSeek, iCount: int64;
  bytesread, dwRet: cardinal;
  IdSector: TIdSector;
  phis: boolean;
  Geometry: TDISKGEOMETRY;
  DiskSize, FreeSize: int64;
  hdd: THANDLE = INVALID_HANDLE_VALUE;
  LastErr: DWORD;

function GetDiskIdentify(Drive: char; ID_Sector: PIdSector; var PartInfo: string): boolean;
var ii:integer;
begin
  PartInfo:='';
  Result:=GetIdeDiskIdentify(Drive, ID_Sector);
  LastErr:=GetLastError();
  if not HDDOpen(Drive, True, hdd, phis, DiskSize, FreeSize, @Geometry) then       // ReadOnly Allways
    exit;
  if (not Result) and (LastErr=ERROR_NOT_SUPPORTED) then  //  The request is not suported  (LastErr=50)
  begin
    IdeIdentifyGen(pointer(ID_Sector), DiskSize,
                   (Geometry.Cylinders * Geometry.TracksPerCylinder) div 16,
                   16,
                   Geometry.SectorsPerTrack);
    Result:=True;
  end;
  if ReadFile(hdd, pcszFile, BLOCK_SIZE, bytesread, nil) then
  begin
    ii:=450;
    while ii<BLOCK_SIZE do begin
      if (Length(PartInfo)<>0) and (TextPartType(ord(pcszFile[ii]))<>'') then
        PartInfo:=PartInfo+', ';
      PartInfo:=PartInfo+TextPartType(ord(pcszFile[ii]));
      ii:=ii+16;
    end;
  end
  else
    WriteCon(#13#10'Error read HDD MBR'#13#10, []);
  if hdd<>INVALID_HANDLE_VALUE then
      CloseHandle(hdd);
  hdd:=INVALID_HANDLE_VALUE;
end;

begin
    FillChar(pcszFile, sizeof(pcszFile), #0);
    iSeek:=0; iCount:=high(iCount);
    for i:= 1 to ParamCount do
    begin
        if (pos('/LIST', AnsiUpperCase(ParamStr(i)))<>0) and (nCommand = cmNone) then
            nCommand := cmList
        else if (pos('/Q', AnsiUpperCase(ParamStr(i)))<>0) then
            fQuet := true
        else if (pos('/S', AnsiUpperCase(ParamStr(i)))<>0) then
             begin
               ss:=copy(ParamStr(i), (pos('/S', AnsiUpperCase(ParamStr(i)))), 255);
               if (Length(ss)>2) and (ss[3]='=') then
               begin
                 delete(ss, 1, 3);
                 iSeek := StrToIntDef(ss, 0);
               end;
             end
        else if (pos('/C', AnsiUpperCase(ParamStr(i)))<>0) then
             begin
               ss:=copy(ParamStr(i), (pos('/C', AnsiUpperCase(ParamStr(i)))), 255);
               if (Length(ss)>2) and (ss[3]='=') then
               begin
                 delete(ss, 1, 3);
                 iCount := StrToIntDef(ss, 0);
               end;
             end
        else if (ParamStr(i)[1] = '/') then
            Usage()
        else if (Drive = ' ') and IsDrive(ParamStr(i), @Drive) and (nCommand = cmNone) then
        begin
          if pcszFile[0]<>#0
            then nCommand := cmWrite
            else nCommand := cmRead;
        end
        else if (pcszFile[0]=#0) and (not IsDrive(ParamStr(i), nil)) { and (nCommand = cmNone) } then
             begin
               StrPCopy(pcszFile, ParamStr(i));
               if Drive = ' '
                 then nCommand := cmWrite
                 else nCommand := cmRead;
             end;
    end;

    if nCommand=cmList then
    begin
      iSeek:=0; iCount:=9;
      if Drive in ['0'..'9'] then
      begin
        iSeek:=ord(Drive)-ord('0'); iCount:=iSeek;
      end;
      for i:=iSeek to iCount do
      begin
        if GetDiskIdentify(chr(ord('0')+i), @IdSector, PartInf) then
        begin
          with IdSector do
          begin
            ChangeByteOrder(sSerialNumber,SizeOf(sSerialNumber));
            ChangeByteOrder(sFirmwareRev,SizeOf(sFirmwareRev));
            ChangeByteOrder(sModelNumber,SizeOf(sModelNumber));
          end;
          if (IdSector.wCapabilities and $200)<>0 then ss:='Supported' else ss:='Not supported';
          WriteCon('Drive %d:'#13#10' Model:%s  FirmwareRev:%s  SerialNumber:%s'#13#10+
                   ' Cyllinders: %d,  Heads: %d,  Sectors: %d,  Size: %d Mb,  LBA: %s'#13#10+
                   ' Partitions: %s'#13#10#13#10,
                   [i, trim(PCharNStr(IdSector.sModelNumber, sizeof(IdSector.sModelNumber))),
                       trim(PCharNStr(IdSector.sFirmwareRev, sizeof(IdSector.sFirmwareRev))),
                       trim(PCharNStr(IdSector.sSerialNumber, sizeof(IdSector.sSerialNumber))),
                    IdSector.wNumCyls, IdSector.wNumHeads, IdSector.wSectorsPerTrack,
                    max(IdSector.ulCurrentCapacity, IdSector.ulTotalAddressableSectors) div 2048, ss, PartInf]);
        end
        else if LastErr<>ERROR_FILE_NOT_FOUND then
          WriteCon(#13#10'Failed to get identify information for drive "%d" : %s'#13#10, [i, LastError()])
      end;
    end
    else if (nCommand = cmNone) or (Drive = ' ') then   // require a command and a drive
        Usage()
    else if (pcszFile[0]=#0) and (nCommand in [cmRead, cmWrite]) then   // read/write require image file
        Usage()
    else if not HDDOpen(Drive, (Drive='0')or(Drive='C'), hdd, phis, DiskSize, FreeSize, @Geometry) then
        WriteCon(#13#10'Failed to open HDD: %s'#13#10, [LastError()])
    else if (pcszFile[0]<>#0) and (not OpenImage(pcszFile, (nCommand = cmRead), hfile)) then
        WriteCon(#13#10'Failed to open image: %s'#13#10, [LastError()])
    else if (nCommand = cmWrite) and
            ( ((iCount-iSeek<>high(iCount)) and (GetFileSize(hfile, nil) < BLOCK_SIZE*(MIN(DiskSize div BLOCK_SIZE, iCount)-iSeek)) ) or
              ((iCount-iSeek=high(iCount)) and (GetFileSize(hfile, nil) <> DiskSize))
            ) then
      begin
        if iCount=high(iCount) then
          FreeSize:=DiskSize
        else
          FreeSize:=MIN(DiskSize, BLOCK_SIZE*(iCount-iSeek));
        WriteCon(#13#10'Image file of wrong size (should be %s bytes or "/c=%d" modifier must be used)'#13#10,
                 [IntToStr(FreeSize), GetFileSize(hfile, nil) div BLOCK_SIZE]);
      end
    else if (not DiskFileSeek(hdd, iSeek, FILE_BEGIN)) then
        WriteCon(#13#10'Failed to seek sector: %s'#13#10, [LastError()])
    else
      begin
        if (nCommand=cmWrite) then
        begin
          if (Drive='0') or (Drive='C') then
          begin
            WriteCon(#13#10'Writing of drive %s: disabled!'#13#10, [Drive]);
            halt(EXIT_FAILURE);
          end;
          WriteCon(#13#10'Sure to write image "%s" to HDD "%s" (Yes/No)? ', [pcszFile, Drive]);
          ReadCon(ss);
          if AnsiUpperCase(ss)<>'YES' then halt(0);
        end;                                                                    
        iSector:=0; iMaxSector:=MIN(DiskSize div BLOCK_SIZE, iCount);
        while iSector < iMaxSector do
        begin
            case nCommand of
                cmRead:
                  begin
                    if ((iSector and 31)=0) and (not fQuet)
                      then WriteCon(#13'Readed: %u of %u', [iSector, iMaxSector]);
                    if not (ReadFile(hdd, buffer, BLOCK_SIZE, bytesread, nil) and
                            WriteFile(hfile, buffer, BLOCK_SIZE, dwRet, nil)) then
                    begin
                      WriteCon(#13#10#13#10'Read failed: %s'#13#10, [LastError()]);
                      halt(EXIT_FAILURE);
                    end;
                  end;
                cmWrite:
                  begin
                    if ((iSector and 31)=0) and (not fQuet)
                      then WriteCon(#13'Writed: %u of %u', [iSector, iMaxSector]);
                    if not (ReadFile(hfile, buffer, BLOCK_SIZE, dwRet, nil) and
                            WriteFile(hdd, buffer, BLOCK_SIZE, bytesread, nil) ) then
                    begin
                      WriteCon(#13#10#13#10'Write failed: %s'#13#10, [LastError()]);
                      halt(EXIT_FAILURE);
                    end;
                  end;
            end; {case}
            inc(iSector);
        end; {while}
        WriteCon(#13'Complete.                               '#13#10, [iSector, iMaxSector]);
      end;
    if hfile<>INVALID_HANDLE_VALUE then
      CloseHandle(hfile);
    if hdd<>INVALID_HANDLE_VALUE then
      CloseHandle(hdd);
end.

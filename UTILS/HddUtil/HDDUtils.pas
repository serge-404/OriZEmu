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

unit HDDUtils;

interface

uses
  Windows,
  SysUtils;

const
  IDE_MAX_SERIAL   = 20;
  IDE_MAX_FIRMWARE = 8;
  IDE_MAX_MODEL    = 40;

type
  TSrbIoControl = packed record 
    HeaderLength : ULONG; 
    Signature    : Array[0..7] of Char; 
    Timeout      : ULONG;
    ControlCode  : ULONG;
    ReturnCode   : ULONG; 
    Length       : ULONG; 
  end; 
  SRB_IO_CONTROL = TSrbIoControl; 
  PSrbIoControl = ^TSrbIoControl; 

  TIDERegs = packed record 
    bFeaturesReg     : Byte; // Used for specifying SMART "commands". 
    bSectorCountReg  : Byte; // IDE sector count register 
    bSectorNumberReg : Byte; // IDE sector number register 
    bCylLowReg       : Byte; // IDE low order cylinder value 
    bCylHighReg      : Byte; // IDE high order cylinder value 
    bDriveHeadReg    : Byte; // IDE drive/head register 
    bCommandReg      : Byte; // Actual IDE command. 
    bReserved        : Byte; // reserved.  Must be zero. 
  end; 
  IDEREGS   = TIDERegs; 
  PIDERegs  = ^TIDERegs; 

  TSendCmdInParams = packed record 
    cBufferSize  : DWORD; 
    irDriveRegs  : TIDERegs; 
    bDriveNumber : Byte; 
    bReserved    : Array[0..2] of Byte; 
    dwReserved   : Array[0..3] of DWORD; 
    bBuffer      : Array[0..0] of Byte; 
  end; 
  SENDCMDINPARAMS   = TSendCmdInParams;
  PSendCmdInParams  = ^TSendCmdInParams;

  TIdSector = packed record
    wGenConfig                 : Word;
    wNumCyls                   : Word;
    wReserved                  : Word; 
    wNumHeads                  : Word; 
    wBytesPerTrack             : Word; 
    wBytesPerSector            : Word; 
    wSectorsPerTrack           : Word; 
    wVendorUnique              : Array[0..2] of Word; 
    sSerialNumber              : Array[0..IDE_MAX_SERIAL-1] of Char;      // w 10-19
    wBufferType                : Word; 
    wBufferSize                : Word; 
    wECCSize                   : Word; 
    sFirmwareRev               : Array[0..IDE_MAX_FIRMWARE-1] of Char;
    sModelNumber               : Array[0..IDE_MAX_MODEL-1] of Char;
    wMoreVendorUnique          : Word; 
    wDoubleWordIO              : Word;                                    // w48
    wCapabilities              : Word;
    wReserved1                 : Word;
    wPIOTiming                 : Word;
    wDMATiming                 : Word;
    wBS                        : Word;
    wNumCurrentCyls            : Word;                                    // w 54
    wNumCurrentHeads           : Word;
    wNumCurrentSectorsPerTrack : Word;
    ulCurrentCapacity          : ULONG;
    wMultSectorStuff           : Word;
    ulTotalAddressableSectors  : ULONG;                                   // w 60-61
    wSingleWordDMA             : Word;
    wMultiWordDMA              : Word;
    bReserved                  : Array[0..127] of Byte; 
  end; 
  PIdSector = ^TIdSector;

  TDISKGEOMETRY = record
    Cylinders: int64;
    MediaType: Integer;
    TracksPerCylinder: DWORD;
    SectorsPerTrack: DWORD;
    BytesPerSector: DWORD;
  end;
  PDISKGEOMETRY = ^TDISKGEOMETRY;

const
  IOCTL_DISK_GET_DRIVE_GEOMETRY = 458752;
  BLOCK_SIZE = 512;
  EXIT_FAILURE = 1;
  IDE_ID_FUNCTION = $EC;
  IDENTIFY_BUFFER_SIZE       = 512;
  DFP_RECEIVE_DRIVE_DATA        = $0007c088;
  IOCTL_SCSI_MINIPORT           = $0004d008;
  IOCTL_SCSI_MINIPORT_IDENTIFY  = $001b0501;
  DataSize = sizeof(TSendCmdInParams)-1+IDENTIFY_BUFFER_SIZE;
  BufferSize = SizeOf(SRB_IO_CONTROL)+DataSize;
  W9xBufferSize = IDENTIFY_BUFFER_SIZE+16;

function GetIdeDiskIdentify(Drive: char; IdSector: PIdSector): boolean;
function MIN(x,y: int64):int64;
function DiskFileSeek(hf: THANDLE; distance: int64; MoveMethod: DWORD):boolean;
function GetDiskSize(hDisk: THandle; var DiskSize: int64; Geometry: PDISKGEOMETRY): boolean;
function HDDOpen(cDrive: char; IsReadOnly: boolean; var hDevice: THANDLE;
  var phis:boolean; var DiskSize, FreeSize: int64; Geometry: PDISKGEOMETRY): boolean;
function OpenImage (pcsz_: PChar; fWrite_: boolean; var h: THANDLE): boolean;
function LastError: string;
function IsDrive(pcsz_: string; pnDrive_:PChar): boolean;
procedure ChangeByteOrder(ptr : PChar; Size : Integer );

implementation

function MIN(x,y: int64):int64;
begin
  if x>y then Result:=y else Result:=x;
end;

//---------------------------------------------------------------------
// DiskFileSeek: Allow seeking through large files (a disk opened as a
// file).
//---------------------------------------------------------------------
function DiskFileSeek(hf: THANDLE; distance: int64; MoveMethod: DWORD):boolean;
var seekDistance:int64;
    li: LARGE_INTEGER;
begin
  Result:=True;
  seekDistance := distance*512;
  li.QuadPart := seekDistance;
  li.LowPart := SetFilePointer(hf, li.LowPart, @li.HighPart, MoveMethod);
  if (li.LowPart = $FFFFFFFF) then
    Result:=GetLastError()=NO_ERROR;
end;

function GetDiskSize(hDisk: THandle; var DiskSize: int64; Geometry: PDISKGEOMETRY): boolean;
var
  Returned: DWORD;
  i64: int64;
begin
  Result:=False;
  if hDisk <> INVALID_HANDLE_VALUE then
  begin
    FillChar(Geometry^, Sizeof(TDISKGEOMETRY),0);
    Result:=DeviceIoControl(hDisk,IOCTL_DISK_GET_DRIVE_GEOMETRY,nil,0,Geometry,
                            sizeof(TDISKGEOMETRY),Returned,nil);
    if Result then
      with Geometry^ do
      begin
        i64:=Cylinders;
        DiskSize:=i64*TracksPerCylinder*SectorsPerTrack*BytesPerSector;
      end;
  end;
end;

function HDDOpen(cDrive: char; IsReadOnly: boolean; var hDevice: THANDLE;
  var phis:boolean; var DiskSize, FreeSize: int64; Geometry: PDISKGEOMETRY): boolean;
var
  _devicename: array[0..255] of char;
  TotalFree: int64;
  Access: Cardinal;
begin
    if IsReadOnly then
      Access:=GENERIC_READ
    else
      Access:=GENERIC_READ or GENERIC_WRITE;
    Result:=False;
    DiskSize:=0;
    hDevice:=INVALID_HANDLE_VALUE;
    case cDrive of
      '0'..'9': begin
                  StrPCopy(_devicename, '\\.\PhysicalDrive0');
                  _devicename[17] := cDrive;
                  phis:=True;
                end;
      'A'..'Z': begin
                  StrPCopy(_devicename, '\\.\A:');
                  _devicename[4] := cDrive;
                  phis:=False;
                end
      else exit;
    end;
    hDevice := CreateFile(_devicename,       // drive to open
                          Access,            // access type
                          FILE_SHARE_READ or // share mode
                          FILE_SHARE_WRITE,
                          nil,               // default security attributes
                          OPEN_EXISTING,     // disposition
                          0,                 // file attributes
                          0);                // do not copy file attributes
    Result := hDevice <> INVALID_HANDLE_VALUE;
    if Result then
    begin
      if phis then
      begin
        Result:=GetDiskSize(hDevice, DiskSize, Geometry);
        FreeSize:=DiskSize div 2;
      end
      else
        Result:=GetDiskFreeSpaceEx(@_devicename[4], FreeSize, DiskSize, @TotalFree);
    end;
end;

function OpenImage (pcsz_: PChar; fWrite_: boolean; var h: THANDLE): boolean;
begin
    if fWrite_
      then h:=CreateFile(pcsz_, GENERIC_WRITE, 0, nil, CREATE_ALWAYS, 0, 0)
      else h:=CreateFile(pcsz_, GENERIC_READ, 0, nil, OPEN_EXISTING, 0, 0);
    if (h<>INVALID_HANDLE_VALUE) then Result:=True
    else begin
      Result:=False;
      h:=0;
    end;
end;

function LastError: string;
var lpMsgBuf: PChar;
begin
  FormatMessage(FORMAT_MESSAGE_ALLOCATE_BUFFER or
                FORMAT_MESSAGE_FROM_SYSTEM,
                nil, GetLastError, 0,
                @lpMsgBuf, 0, nil);
  if GetLastError()<>0 then
  begin
     Result:=StrPas(lpMsgBuf);
     CharToOemBuff(@Result[1], @Result[1], Length(Result));
  end
  else Result:='no error';
  LocalFree( HLOCAL(lpMsgBuf) );
end;

function IsDrive(pcsz_: string; pnDrive_:PChar): boolean;
begin
    pcsz_:=AnsiUpperCase(pcsz_);
    if (Length(pcsz_)<>2) or (not (pcsz_[1] in ['0'..'9','C'..'Z'])) or (pcsz_[2]<>':')  then
      Result:=False
    else
    begin
      if (pnDrive_<>nil) then pnDrive_^ := pcsz_[1];
      Result:=True;
    end;
end;

procedure ChangeByteOrder(ptr : PChar; Size : Integer );
var   i : Integer;
      c : Char;
begin
    for i := 0 to (Size shr 1)-1 do
    begin
      c := ptr^;
      ptr^ := (ptr+1)^;
      (ptr+1)^ := c;
      Inc(ptr,2);
    end;
end;

//-------------------------------------------------------------
function GetIdeDiskIdentify(Drive: char; IdSector: PIdSector): boolean;
var
  hDevice : THandle;
  cbBytesReturned : DWORD;
  pInData : PSendCmdInParams;
  pOutData : Pointer; // PSendCmdOutParams
  Buffer : Array[0..BufferSize-1] of Byte;
  _devicename: array[0..255] of char;
  srbControl : TSrbIoControl absolute Buffer;
begin
  Result := False;
  if not (Drive in ['0'..'9']) then exit;
  FillChar(Buffer,BufferSize,#0);
  if Win32Platform=VER_PLATFORM_WIN32_NT then
    begin // Windows NT, Windows 2000
      // Get SCSI port handle
      StrPCopy(_devicename, '\\.\PhysicalDrive'+Drive);
      hDevice := CreateFile(_devicename,
                            GENERIC_READ or GENERIC_WRITE,
                            FILE_SHARE_READ or FILE_SHARE_WRITE,
                            nil, OPEN_EXISTING, 0, 0 );
      if hDevice=INVALID_HANDLE_VALUE then Exit;
      try
        srbControl.HeaderLength := SizeOf(SRB_IO_CONTROL);
        System.Move('SCSIDISK',srbControl.Signature,8);
        srbControl.Timeout      := 2;
        srbControl.Length       := DataSize;
        srbControl.ControlCode  := IOCTL_SCSI_MINIPORT_IDENTIFY;
        pInData := PSendCmdInParams(PChar(@Buffer)
                   +SizeOf(SRB_IO_CONTROL));
        pOutData := pInData;
        with pInData^ do
        begin
          cBufferSize  := IDENTIFY_BUFFER_SIZE;
          bDriveNumber := ord(Drive)-ord('0');
          with irDriveRegs do
          begin
            bFeaturesReg     := 0;
            bSectorCountReg  := 1;
            bSectorNumberReg := 1;
            bCylLowReg       := 0;
            bCylHighReg      := 0;
            bDriveHeadReg    := $A0 or ((bDriveNumber and 1) shl 4);
            bCommandReg      := IDE_ID_FUNCTION;
          end;
        end;
        Result:=DeviceIoControl( hDevice, IOCTL_SCSI_MINIPORT, @Buffer, BufferSize,
                                 @Buffer, BufferSize, cbBytesReturned, nil );
      finally
        if hDevice <> INVALID_HANDLE_VALUE then
          CloseHandle(hDevice);
      end;
    end
  else
    begin // Windows 95 OSR2, Windows 98
      hDevice := CreateFile( '\\.\SMARTVSD', 0, 0, nil,
        CREATE_NEW, 0, 0 );
      if hDevice=INVALID_HANDLE_VALUE then Exit;
      try
        pInData := PSendCmdInParams(@Buffer);
        pOutData := @pInData^.bBuffer;
        with pInData^ do
        begin
          cBufferSize  := IDENTIFY_BUFFER_SIZE;
          bDriveNumber := ord(Drive)-ord('0');
          with irDriveRegs do
          begin
            bFeaturesReg     := 0;
            bSectorCountReg  := 1;
            bSectorNumberReg := 1;
            bCylLowReg       := 0;
            bCylHighReg      := 0;
            bDriveHeadReg    := $A0;
            bCommandReg      := IDE_ID_FUNCTION;
          end;
        end;
        Result:=DeviceIoControl(hDevice, DFP_RECEIVE_DRIVE_DATA, pInData, SizeOf(TSendCmdInParams)-1,
                                pOutData, W9xBufferSize, cbBytesReturned, nil );
      finally
        if hDevice <> INVALID_HANDLE_VALUE then
          CloseHandle(hDevice);
      end;
    end;
  CopyMemory(IdSector, PIdSector(PChar(pOutData)+16), sizeof(TIdSector));
end;

end.


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


unit mainwin;

interface


{$I 'OrionZEm.inc'}


uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  ExtCtrls, StdCtrls, ComCtrls, Menus, ToolWin, MMSystem, Math,
  uIniMngr, ScrThrd, mod8019as, modWaveOut, Grids, Mask, ActnList, ImgList, StdActns;

const
 ODI_EXT='ODI';
 RKO_EXT='RKO';
 ORD_EXT='ORD';
 BRU_EXT='BRU';
 ODI_FILTER='Orion Disk Images (*.'+ODI_EXT+')|*.'+ODI_EXT+'|Any file (*.*)|*.*';
 ORD_FILTER='Orion ORDOS files (*.'+ORD_EXT+';*.'+BRU_EXT+';*.'+RKO_EXT+')|*.'+ORD_EXT+';*.'+BRU_EXT+';*.'+RKO_EXT+'|Any file (*.*)|*.*';
 TAG_STR='Orion/Z snapshot ';
 AutoSnapName='AutoSnap.ori';
 RAMDISK_TOP = 61439;

// ini sections
 stSectionParams = 'PARAMS';
 stSectionHard = 'HARDWARE';
 stSectionEth = 'ETHERNET';
 stReadOnly='  [ReadOnly]';

type
  TOFileType = (ftUnknown, ftSnapshot, ftDiskImage, ftRko, ftOrd, ftBru);

  TfrmMain = class(TForm)
    StatusBar: TStatusBar;
    ToolBar: TToolBar;
    ToolButtonPause: TToolButton;
    ToolButton2: TToolButton;
    ToolButtonReset: TToolButton;
    ToolButton4: TToolButton;
    ToolButtonZoom: TToolButton;
    ToolButton1: TToolButton;
    ToolButtonFloppyA: TToolButton;
    ToolButton5: TToolButton;
    ToolButtonFloppyB: TToolButton;
    ActionList1: TActionList;
    ImageList1: TImageList;
    ActPause: TAction;
    ActReset: TAction;
    ActZoom: TAction;
    ActFloppyA: TAction;
    ActFloppyB: TAction;
    OdiPopupMenu: TPopupMenu;
    ItemBrowse: TMenuItem;
    N1: TMenuItem;
    ItemRecent: TMenuItem;
    ToolButton7: TToolButton;
    ToolButtonSettings: TToolButton;
    ActSettings: TAction;
    OpenDialog: TOpenDialog;
    SaveDialog: TSaveDialog;
    ItemClear: TMenuItem;
    ToolButton3: TToolButton;
    ToolButton6: TToolButton;
    ActHelp: TAction;
    ToolButton8: TToolButton;
    ToolButtonSnapshot: TToolButton;
    SnapPopupMenu: TPopupMenu;
    Saveshapshot128k1: TMenuItem;
    Saveshapshot256k1: TMenuItem;
    Saveshapshot512k1: TMenuItem;
    N2: TMenuItem;
    Loadsnapshot1: TMenuItem;
    ItemInfo: TMenuItem;
    N3: TMenuItem;
    AutoSnapshot1: TMenuItem;
    ActScr: TAction;
    ToolButtonOpen: TToolButton;
    ToolButton10: TToolButton;
    pnScr: TPanel;
    pbDraw: TPaintBox;
    pnDbg: TPanel;
    Bevel1: TBevel;
    ToolButtonScr: TToolButton;
    ToolButtonDbg: TToolButton;
    ToolButton13: TToolButton;
    ToolButton14: TToolButton;
    ActDbg: TAction;
    ZoomPopupMenu: TPopupMenu;
    x1menuitem: TMenuItem;
    x2menuitem: TMenuItem;
    x25menuitem: TMenuItem;
    x3menuitem: TMenuItem;
    ActOpenSave: TAction;
    OpenPopupMenu: TPopupMenu;
    ItemLoad: TMenuItem;
    N4: TMenuItem;
    ItemSave: TMenuItem;
    N5: TMenuItem;
    Savescreenpicture1: TMenuItem;
    DebuggerMenu: TPopupMenu;
    ItemModify: TMenuItem;
    N6: TMenuItem;
    ItemSetCondition: TMenuItem;
    ItemClearCondition: TMenuItem;
    ActDbgStepInto: TAction;
    ActDbgStepOver: TAction;
    N7: TMenuItem;
    ItemPause: TMenuItem;
    Panel1: TPanel;
    MemDump: TMemo;
    Panel2: TPanel;
    Panel3: TPanel;
    Label3: TLabel;
    Label4: TLabel;
    SGRegMain: TStringGrid;
    SGRegAlter: TStringGrid;
    SGHistory: TStringGrid;
    cbBreakPoint: TCheckBox;
    cbConditions: TCheckBox;
    meBreakPoint: TMaskEdit;
    BtnStepInto: TButton;
    BtnStepOver: TButton;
    SGFlags: TStringGrid;
    SGPortDump: TStringGrid;
    sgPort1Dump: TStringGrid;
    sgPort2Dump: TStringGrid;
    Panel4: TPanel;
    MEDumpAddr: TMaskEdit;
    BtnModyByte: TButton;
    BtnSaveMem: TButton;
    MESaveCnt: TMaskEdit;
    Label2: TLabel;
    btnPageAddress: TButton;
    Label1: TLabel;
    Bevel2: TBevel;
    Label5: TLabel;
    PageAddressMenu: TPopupMenu;
    F9HL1: TMenuItem;
    F9HL2: TMenuItem;
    F9BC1: TMenuItem;
    F9IX1: TMenuItem;
    F9IY1: TMenuItem;
    F9SP1: TMenuItem;
    F9PC1: TMenuItem;
    F9Ix2: TMenuItem;
    ItemWrPause: TMenuItem;
    ItemModiPause: TMenuItem;
    procedure FormDestroy(Sender: TObject);
    procedure FormResize(Sender: TObject);
    procedure pbDrawPaint(Sender: TObject);
    procedure FormActivate(Sender: TObject);
    procedure FormClose(Sender: TObject; var Action: TCloseAction);
    procedure FormCloseQuery(Sender: TObject; var CanClose: Boolean);
    procedure MEDumpAddrChange(Sender: TObject);
    procedure BtnModyByteClick(Sender: TObject);
    procedure ActPauseExecute(Sender: TObject);
    procedure ActResetExecute(Sender: TObject);
    procedure ActZoomExecute(Sender: TObject);
    procedure ItemBrowseClick(Sender: TObject);
    procedure ToolButtonFloppyAClick(Sender: TObject);
    procedure ToolButtonFloppyBClick(Sender: TObject);
    procedure BtnSaveMemClick(Sender: TObject);
    procedure ItemClearClick(Sender: TObject);
    procedure cbBreakPointClick(Sender: TObject);
    procedure ItemRecentClick(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure ActSettingsExecute(Sender: TObject);
    procedure ActHelpExecute(Sender: TObject);
    procedure Saveshapshot128k1Click(Sender: TObject);
    procedure Saveshapshot256k1Click(Sender: TObject);
    procedure Saveshapshot512k1Click(Sender: TObject);
    procedure Loadsnapshot1Click(Sender: TObject);
    procedure ToolButtonSnapshotClick(Sender: TObject);
    procedure AutoSnapshot1Click(Sender: TObject);
    procedure ActScrExecute(Sender: TObject);
    procedure ActDbgExecute(Sender: TObject);
    procedure x1menuitemClick(Sender: TObject);
    procedure ActOpenSaveExecute(Sender: TObject);
    procedure ItemLoadClick(Sender: TObject);
    procedure ItemSaveClick(Sender: TObject);
    procedure Savescreenpicture1Click(Sender: TObject);
    procedure DebuggerMenuPopup(Sender: TObject);
    procedure ActDbgStepIntoExecute(Sender: TObject);
    procedure ActDbgStepOverExecute(Sender: TObject);
    procedure ItemModifyClick(Sender: TObject);
    procedure ItemClearConditionClick(Sender: TObject);
    procedure ItemSetConditionClick(Sender: TObject);
    procedure cbConditionsClick(Sender: TObject);
    procedure F9Click(Sender: TObject);
    procedure btnPageAddressClick(Sender: TObject);
    procedure ItemWrPauseClick(Sender: TObject);
  private
    { Private declarations }
    DisSL: TStringList;
    FMainIniFile: TAsofIniFile;
    IniManager: TIniManager;
    ScrBitmap: TBitmap;
    SX, SY: Integer;
    ScrThread: TScrThread;
    FDriveindex: integer;
    FLcdRusLat: integer;
    function  ProcessCmdLine: boolean;
    function disasm(PC:integer; var OP: string):integer;
    function CheckFileExists(FlName:string):string;
    function  DetectFileType(FName: string; var Fsz: integer):TOFileType;
    procedure SetFloppyHint(FDIndex: integer; isReadOnly: boolean);
    procedure CreateBitmap(aSX, aSY: Integer);
    procedure DeleteBitmap;
    procedure RecreateBitmap(aSX, aSY: Integer);
    procedure StartScrThread;
    procedure StopScrThread;
    procedure InitRAMArr;
    procedure InitEmulator;
    procedure InitSGRegPort;
    procedure InitDizSL;
    procedure AppKeyDown(var msg : TMsg; var Handled: Boolean);
    procedure MyIdleHandler(Sender: TObject; var Done: Boolean);
    procedure SaveSnapshot(Pages: integer; FName: string);
    procedure LoadSnapshot(FName: string);
    procedure ShowRusLat;
    procedure SetFormSize;
    procedure SetZoomChecks;
  public
    procedure CPUSuspend;
    procedure CPUResume;
    procedure InitEthernet;
    procedure ShowMemDump(PauseCPU: boolean);
    procedure ShowSGReg;
    procedure ShowSGPort;
    procedure BindIniParameters;
    procedure BlankOrionScreen;
    procedure DrawOrionScreen;
    procedure SetROMBIOS(FName: string); // load ROMF800.BIN into array
    procedure SetROMDISK(FName: string); // load ROMFDISK.BIN into array
    procedure SetROM1BIOS(FName: string); // load ROM1BIOS.BIN into array
    procedure SetROM2BIOS(FName: string); // load ROM2BIOS.BIN into array
  end;

  TBig = array[0..0] of Integer;

  BytePtr = ^byte;
  WordPtr = ^word;
  DWordPtr = ^longword;

var
  frmMain: TfrmMain;
  Scr: Pointer;

  FMaxRecent, ScrTop, ScrLeft: integer;
  FRestoreODI: boolean;
  FODI_DriveA, FODI_DriveB: string;
  FAutoSnapshot: boolean;
  GUIDList: TStringList;
  FNE2kDevice: TNE2kDevice;

procedure Scr480(outbyte: byte);
procedure chrtrn(var buf: string; from_tbl, to_tbl: string);
function padl(stt:string;max:integer;ch:string):string;
Function LeftSubstr(var s:String): String;
function AddSlash(str: string): string;
function GetHexMasked4(ch: char; var Mask: byte): byte;
function GetHexMasked16(st: string; var Mask: integer): integer;

implementation

{$R *.DFM}

Uses modOrion, modZ80, mod1793, mod8255, mod146818, modAY8912, modHdd, modSD, mod232, EthThrd, uCRC32, settswin,
  frmAbout, frmSaveOrd, frNewVal;

Var
  OriHeader: TOriHeader;

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

procedure CloseWaveOut();
var
  lRet: LongInt;
begin
    {lRet :=} waveOutReset(glphWaveOut);
    For lRet := 1 To NUM_WAV_BUFFERS do
        waveOutUnprepareHeader(glphWaveOut, @gtWavHdr[lRet], sizeof(gtWavHdr[lRet]));

    For lRet := 1 To NUM_WAV_BUFFERS do
    begin
        GlobalUnlock(ghMem[lRet]);
        GlobalFree(ghMem[lRet]);
    end;

    waveOutClose(glphWaveOut);
    glphWaveOut:=-1;
    SoundEnabled := False;
End;

Function InitializeWaveOut : Boolean;
{$IFDEF USE_SOUND}
  var lRet : integer; sErrMsg : String; lCounter : integer; lCounter2 : integer;
{$ENDIF}
begin
        If not SoundEnabled Then
        begin
          InitializeWaveOut := False;
          Exit;
        End;
{$IFDEF USE_SOUND}
//        If glphWaveOut<>-1 Then
//          CloseWaveOut();
        glBeeperVal := 128;
        With gtWavFormat do
        begin
            wFormatTag := WAVE_FORMAT_PCM;
            nChannels := 1;
            nSamplesPerSec := WAVE_FREQUENCY;
            nAvgBytesPerSec := WAVE_FREQUENCY;
            nBlockAlign := 1;
            wBitsPerSample := 8;
            cbSize := 0;
        End;

        lRet := waveOutOpen(@glphWaveOut, WAVE_MAPPER, @gtWavFormat, 0, 0, CALLBACK_NULL);
        If lRet <> MMSYSERR_NOERROR Then
        begin
            SetLength(sErrMsg,  256);
            waveOutGetErrorText(lRet, PChar(sErrMsg), 255);
            Application.MessageBox(
              PChar('Error initialising WaveOut device.' + #13#10#13#10 + sErrMsg),
              PChar(Application.Title),
              MB_OK or MB_ICONEXCLAMATION);
            InitializeWaveOut := False;
            Exit;
        End;

        For lCounter := 1 To NUM_WAV_BUFFERS do
        begin
            ghMem[lCounter] := GlobalAlloc(GPTR, WAV_BUFFER_SIZE+100);
            gpMem[lCounter] := GlobalLock(ghMem[lCounter]);
            gtWavHdr[lCounter].lpData := gpMem[lCounter];
            gtWavHdr[lCounter].dwBufferLength := WAV_BUFFER_SIZE;
            gtWavHdr[lCounter].dwUser := 0;
            gtWavHdr[lCounter].dwFlags := 0;
            gtWavHdr[lCounter].dwLoops := 0;
            gtWavHdr[lCounter].lpNext := nil;

            lRet := waveOutPrepareHeader(glphWaveOut,
              @gtWavHdr[lCounter],
              sizeof(gtWavHdr[lCounter]));     // WAVEHDR

            If lRet <> MMSYSERR_NOERROR Then
            begin
                SetLength(sErrMsg,  256);
                waveOutGetErrorText(lRet, PChar(sErrMsg), 255);
                Application.MessageBox(
                  PChar('Error preparing wave header.' + #13#10#13#10 + sErrMsg),
                  PChar(Application.Title),
                  MB_OK or MB_ICONEXCLAMATION);
                waveOutClose(glphWaveOut);
                For lCounter2 := 1 To NUM_WAV_BUFFERS do
                begin
                    GlobalUnlock(ghMem[lCounter2]);
                    GlobalFree(ghMem[lCounter2]);
                end;
                InitializeWaveOut := False;
                Exit;
            End;
        end;
        For lCounter := 0 To 48000 do
          gcWaveOut[lCounter] := glBeeperVal;
{$ENDIF}
        InitializeWaveOut := True;
End;

procedure TfrmMain.CreateBitmap(aSX, aSY: Integer);
var
  BInfo: tagBITMAPINFO;
begin
  // Создание DIB
  SX := aSX; SY := aSY;
  BInfo.bmiHeader.biSize := sizeof(tagBITMAPINFOHEADER);
  BInfo.bmiHeader.biWidth := SX;
  BInfo.bmiHeader.biHeight := -SY;
  BInfo.bmiHeader.biPlanes := 1;
  BInfo.bmiHeader.biBitCount := 32;
  BInfo.bmiHeader.biCompression := BI_RGB;
  ScrBitmap := TBitmap.Create();
  ScrBitmap.Handle := CreateDIBSection(Canvas.Handle, BInfo, DIB_RGB_COLORS, Scr, 0, 0);
  ZeroMemory(Scr, SX * SY * 4);
end;

procedure TfrmMain.DeleteBitmap;
begin
  // Удаление DIB
  ScrBitmap.FreeImage();
  ScrBitmap.Destroy;
end;

procedure TfrmMain.RecreateBitmap(aSX, aSY: Integer);
var
  BInfo: tagBITMAPINFO;
begin
  // Пересоздание DIB при изменении размеров "экрана"
  ScrBitmap.FreeImage();
  SX := aSX; SY := aSY;
  BInfo.bmiHeader.biSize := sizeof(tagBITMAPINFOHEADER);
  BInfo.bmiHeader.biWidth := SX;
  BInfo.bmiHeader.biHeight := -SY;
  BInfo.bmiHeader.biPlanes := 1;
  BInfo.bmiHeader.biBitCount := 32;
  BInfo.bmiHeader.biCompression := BI_RGB;
  ScrBitmap.Handle := CreateDIBSection(Canvas.Handle, BInfo, DIB_RGB_COLORS, Scr, 0, 0);
  ZeroMemory(Scr, SX * SY * 4);
end;

procedure TfrmMain.FormDestroy(Sender: TObject);
begin
  DeleteBitmap();
end;

procedure TfrmMain.StartScrThread;
begin
  if Assigned(ScrThread) then StopScrThread;
  ScrThread:=TScrThread.Create(ScrWidthArr[ScrWidth, 0], ScrHeightArr[0],
                               ScrWidthArr[ScrWidth, ScrZoom], ScrZoom);   { create suspended }
  ScrThread.Priority := tpLower;                 // tpNormal;              { set the priority to normal }
  ScrThread.Resume;                                                        { now run the thread }
end;

procedure TfrmMain.StopScrThread;
begin
  if not Assigned(ScrThread) then exit;
  if ScrThread.Suspended then ScrThread.Resume;
  ScrThread.Terminate;
  ScrThread.WaitFor;
  ScrThread.Free;
  ScrThread:=nil;
end;

procedure TfrmMain.DrawOrionScreen;
begin
  pbDrawPaint(Self);  // full Orion screen redraw
end;

procedure TfrmMain.BlankOrionScreen;
begin
  ReCreateBitmap(pbDraw.ClientWidth, pbDraw.ClientHeight);
end;

procedure TfrmMain.FormResize(Sender: TObject);
begin
  ReCreateBitmap(pbDraw.ClientWidth, pbDraw.ClientHeight);
  pbDraw.Canvas.Draw(0, 0, ScrBitmap);
end;

procedure TfrmMain.pbDrawPaint(Sender: TObject);
begin
  pbDraw.Canvas.Draw(0, 0, ScrBitmap);
end;

procedure initParity;
    var lCounter : integer; j : Byte; p : Boolean;
    var powval: integer;
begin
    For lCounter := 0 To 255 do
    begin
        p := True;
        For j := 0 To 7 do
        begin
            PowVal := Trunc(IntPower(2, j));
            If (lCounter And powVal) <> 0 Then p := Not p;
        end;
        Parity[lCounter] := p;
    end;
End;

procedure TfrmMain.InitEmulator;
var st:string;
begin
//  FillChar(MainPort, sizeof(MainPort), 0);
  MEDumpAddr.EditMask:=SetMemSize;
  OutB($0A, p0A_ROM1_MASK +  // ROM window 0000..1FFF
            p0A_FIX_F000);   // if  p0A_FIX_F000 (D6) = 1  then RAM  F000..FFFF = 1F segment 4k part always (with any pF9 combinations)
  OutB($F8, 0);
  OutB($F9, 0);
  OutB($08, 0);
  OutB($FA, 0);
  OutB($FB, pFB_disp_off +   // диспетчер 16к выключен  (D7=1)
            pFB_int50_off +  // прерывания выключены    (D6=0)
            pFB_TopRam_off); // F400..FFFF - порты+ПЗУ  (D5=0)
  OutB($FC, 0);
  OutB($FE, 0);
  try
    MainPort[$FF]:=0;
    PortF400.Reset;
    PortF500.Reset;
    PortF600.Reset;
    FDController.Reset;
    F146818.Reset;
    FUART.PortName:=ComPortName;
    FUART.Exists:=ComPortExists;
    FUART.Reset;
    if Assigned(FNE2kDevice) then FNE2kDevice.Reset;
{$IFDEF USE_SOUND}
    AY8912_init(1773000, WAVE_FREQUENCY, 8);
{$ENDIF}
    glInterruptDelay := 20;        // from ini ?
    initParity;
    Z80Reset;
    IdeProController.ResetController;
    SDController.Reset;
  finally
    timeBeginPeriod(1);
    glInterruptTimer := timeGetTime() + 20;
{$IFDEF USE_SOUND}
    if SoundEnabled then
      SoundEnabled:=SoundEnabled and InitializeWaveOut();
{$ENDIF}
    if Z80CardMode>=Z80_ORIONPRO_v2 then
      regPC:=ORIONPRO_ROM1BIOS_ADDR
    else
      regPC:=ORION_ROMBIOS_ADDR;
    st:=MEDumpAddr.EditMask;
    chrtrn(st, '9A', '00');
    MEDumpAddr.Text:=st;
  end;
end;

procedure TfrmMain.SetROMBIOS(FName: string);
var FStream: TFileStream;
begin
  if trim(FName)='' then exit;
  FStream:=TFileStream.Create(FName, fmOpenRead or fmShareDenyWrite);
  ROMBIOSlen:=MIN(FStream.Size, $800);
  FStream.ReadBuffer(ROMF800[$F800], ROMBIOSlen);
  FStream.Free;
end;

procedure TfrmMain.SetROM1BIOS(FName: string);
var FStream: TFileStream;
begin
  FillChar(ROM1BIOS, SizeOf(ROM1BIOS), $FF);
  if (trim(FName)='') or (not FileExists(FName)) then exit;
  FStream:=TFileStream.Create(FName, fmOpenRead or fmShareDenyWrite);
  ROM1BIOSlen:=MIN(FStream.Size, $10000);
  FStream.ReadBuffer(ROM1BIOS[0], ROM1BIOSlen);
  FStream.Free;
end;

procedure TfrmMain.SetROM2BIOS(FName: string);
var FStream: TFileStream;
begin
  FillChar(ROM2BIOS, SizeOf(ROM2BIOS), $FF);
  if (trim(FName)='') or (not FileExists(FName)) then exit;
  FStream:=TFileStream.Create(FName, fmOpenRead or fmShareDenyWrite);
  ROM2BIOSlen:=MIN(FStream.Size, $200000);
  FStream.ReadBuffer(ROM2BIOS[0], ROM2BIOSlen);
  FStream.Free;
end;

procedure TfrmMain.SetROMDISK(FName: string);
var FStream: TFileStream;
begin
  if trim(FName)='' then exit;
  FStream:=TFileStream.Create(FName, fmOpenRead or fmShareDenyWrite);
  ROMDISKlen:=FStream.Size;
  SetLength(ROMDISK, ROMDISKlen+1);
  FStream.ReadBuffer(ROMDISK[0], ROMDISKlen);
  FStream.Free;
end;

procedure TfrmMain.BindIniParameters;
begin
  Screen.Cursor:=crHourGlass;
  with IniManager do
  try
{ begin Orion-PRO specific }
    BindVariable(btString,  @ROM1BIOSfile,             stSectionHard,   'PRO_ROM1',      'ROM1.BIN');
    BindVariable(btString,  @ROM2BIOSfile,             stSectionHard,   'PRO_ROM2',      'ROM2.BIN');
    BindVariable(btInteger, @OrionPRO_DIP_SW,          stSectionHard,   'PRO_DIPSW',     '161');
{ end Orion-PRO specific }
    BindVariable(btString,  @ROMBIOSfile,              stSectionHard,   'ROMF800',       'ROMF800.BIN');
    BindVariable(btString,  @ROMDISKfile,              stSectionHard,   'ROMDISK',       'ROMDISK.BIN');
    BindVariable(btBoolean, @PFEEnabled,               stSectionHard,   'PortFErom',     '0');
    BindVariable(btString,  @MC146818RAM,              stSectionHard,   'MC146818RAM',   '525443');
    BindVariable(btDouble,  @DeltaTime,                stSectionHard,   'MC146818DT',    '0');
    BindVariable(btDouble,  @DeltaDate,                stSectionHard,   'MC146818DD',    '0');
    BindVariable(btBoolean, @SoundEnabled,             stSectionHard,   'SOUNDENABLED',  '1');
    BindVariable(btBoolean, @AYEnabled,                stSectionHard,   'AYENABLED',     '1');
    BindVariable(btBoolean, @RTCmode,                  stSectionHard,   'RTCMODE',       '0');
    BindVariable(btInteger, @CPUSpeedMode,             stSectionHard,   'CPUSPEEDMODE',  '1');
    BindVariable(btInteger, @MEMSizeMode,              stSectionHard,   'MEMSIZEMODE',   '2');
    BindVariable(btInteger, @Z80CardMode,              stSectionHard,   'Z80CARDMODE',   '1');
    BindVariable(btInteger, @KeyRusLat,                stSectionHard,   'KEYRUSLAT',   '119');
    BindVariable(btInteger, @KeybType,                 stSectionHard,   'KEYBTYPE',      '0'); // RK
    BindVariable(btBoolean, @KeyExtender,              stSectionHard,   'KEYEXTEND',     '0');
    BindVariable(btInteger, @KeyDelay,                 stSectionParams, 'KEYDELAY',      '1');
    BindVariable(btInteger, @HDDPort,                  stSectionHard,   'HDDPort',       '0'); // HDDPortNone
    BindVariable(btBoolean, @SDRO,                     stSectionHard,   'SDcardRO',      '1');
    BindVariable(btString,  @SDImage,                  stSectionHard,   'SDcard',        '');
    BindVariable(btInteger, @SDScheme,                 stSectionHard,   'SDScheme',      '1');
    BindVariable(btString,  @HDDImage[HddDeviceMaster],stSectionHard,   'HDDMaster',     '');
    BindVariable(btString,  @HDDImage[HddDeviceSlave], stSectionHard,   'HDDSlave',      '');
    BindVariable(btString,  @ProImage[HddDeviceMaster],stSectionHard,   'ProMaster',     '');
    BindVariable(btString,  @ProImage[HddDeviceSlave], stSectionHard,   'ProSlave',      '');
    BindVariable(btBoolean, @HDDRO[HddDeviceMaster],   stSectionHard,   'HDDMasterRO',   '1');
    BindVariable(btBoolean, @HDDRO[HddDeviceSlave],    stSectionHard,   'HDDSlaveRO',    '1');
    BindVariable(btBoolean, @ProRO[HddDeviceMaster],   stSectionHard,   'ProMasterRO',   '1');
    BindVariable(btBoolean, @ProRO[HddDeviceSlave],    stSectionHard,   'ProSlaveRO',    '1');
    BindVariable(btBoolean, @FddHd,                    stSectionHard,   'FDD_HD',        '1');
    BindVariable(btInteger, @FMaxRecent,               stSectionParams, 'MAXRECENT',     '8');
    BindVariable(btBoolean, @FRestoreODI,              stSectionParams, 'RESTOREODI',    '1');
    BindVariable(btBoolean, @FAutoSnapshot,            stSectionParams, 'AUTOSNAPSHOT',  '0');
    BindVariable(btString,  @FODI_DriveA,              stSectionParams, 'ODI_DRIVEA',    '');
    BindVariable(btString,  @FODI_DriveB,              stSectionParams, 'ODI_DRIVEB',    '');
    BindVariable(btString,  @F600Plugin,               stSectionParams, 'F600Plugin',    '');
    BindVariable(btInteger, @F600Index,                stSectionParams, 'F600Function',  '-1');
    BindVariable(btString,  @ComPortName,              stSectionHard,   'ComPortName',   'CNCB0');
    BindVariable(btBoolean, @ComPortExists,            stSectionHard,   'ComPortExists', '0');
    BindVariable(btInteger, @ScrZoom,                  stSectionParams, 'ScrZoom',       '1');
    BindVariable(btInteger, @ScrTop,                   stSectionParams, 'ScrTop',        '1');
    BindVariable(btInteger, @ScrLeft,                  stSectionParams, 'ScrLeft',       '1');
{Eth}
    BindVariable(btInteger, @EthMAC0,                  stSectionEth,    'MAC0',          '255');
    BindVariable(btInteger, @EthMAC1,                  stSectionEth,    'MAC1',          '0');
    BindVariable(btInteger, @EthMAC2,                  stSectionEth,    'MAC2',          '0');
    BindVariable(btInteger, @EthMAC3,                  stSectionEth,    'MAC3',          '0');
    BindVariable(btInteger, @EthMAC4,                  stSectionEth,    'MAC4',          '0');
    BindVariable(btInteger, @EthMAC5,                  stSectionEth,    'MAC5',          '0');
    BindVariable(btString,  @EthConnName,              stSectionEth,    'EthConnName',   '');
    BindVariable(btString,  @EthConnGUID,              stSectionEth,    'EthConnGUID',   '');
    BindVariable(btInteger, @EthMode,                  stSectionEth,    'EthMode',       '0');
{}
  finally
    Screen.Cursor:=crDefault;
  end;
end;

procedure OnIdeAccess(Sender: TObject; Drive: byte; Op: char);
var ss: string;
begin
  ss:='HDD: '+chr(ord('0')+Drive)+' '+Op;
  with frmMain.StatusBar do
  begin
    Panels[1].Text:=ss;
    if Panels[1].Bevel=pbRaised then Panels[1].Bevel:=pbLowered;
    Update;
  end;
  Application.ProcessMessages;
end;

procedure TfrmMain.FormActivate(Sender: TObject);
var ErrStr: string;
 function GetErrStr:string;
 begin
   Result:='';
   case Z80CardMode of
     Z80_ORIONPRO_v2, Z80_ORIONPRO_v3, Z80_ORIONPRO_v320:
       begin
         if not FileExists(ROM1BIOSfile) then Result:=#13#10+ROM1BIOSfile;
       end;
     Z80CARD_MOSCOW, Z80CARD_MINIMAL, Z80CARD_USUAL, Z80CARD_MAXIMAL:
       begin
         if not FileExists(ROMBIOSfile) then Result:=#13#10+ROMBIOSfile;
       end;
   end;
 end;
begin
  OnActivate:=nil;
  CPUSuspend;
  Application.OnMessage := AppKeyDown;
  CreateBitmap(pbDraw.ClientWidth, pbDraw.ClientHeight);
  pbDraw.Canvas.Draw(0, 0, ScrBitmap);
  Application.Title := Caption;
  try
    FMainIniFile:=TAsofIniFile.Create(CheckFileExists(ChangeFileExt(System.ParamStr(0),'.INI')));
    IniManager:=TIniManager.Create;
    IniManager.IniFileObj:=FMainIniFile;
    BindIniParameters;
    IniManager.GetAllValues;
    IniManager.GetAllProps;
    PrevScrZoom:=ScrZoom;
{Eth}
    EthMAC[0]:=chr(lo(EthMAC0));
    EthMAC[1]:=chr(lo(EthMAC1));
    EthMAC[2]:=chr(lo(EthMAC2));
    EthMAC[3]:=chr(lo(EthMAC3));
    EthMAC[4]:=chr(lo(EthMAC4));
    EthMAC[5]:=chr(lo(EthMAC5));
{}
    InitEthernet();
{}
    FillChar(CheckPort, sizeof(CheckPort), 0);
    InitEmulator();
    IniManager.RecentFilesMenuItem:=ItemRecent;
    IniManager.OnRecentFilesItemClick:=ItemRecentClick;
    IniManager.RecentFilesDropDownMax:=FMaxRecent;
    IniManager.GetRecentFilesSection;
    if ExtractFilePath(ROMBIOSfile)='' then
       ROMBIOSfile:=ExtractFilePath(FMainIniFile.Filename)+ROMBIOSfile;
    ErrStr:=GetErrStr();
    if (ErrStr<>'') then
    begin
      Application.MessageBox(PChar(ErrStr), 'File(s) not found, please correct INI:', MB_ICONERROR+MB_OK);
      frmSetts:=TfrmSetts.Create(Application);
      if (Assigned(frmSetts)) then with frmSetts do
      try
        PageControl1.ActivePage:=tsROM;
        ShowModal;
        Free;
      finally
        if (not CPUPaused) then CPUResume;
        frmSetts:=nil;
      end;
      ErrStr:=GetErrStr();
      if (ErrStr<>'') then begin
        Application.MessageBox(PChar(ErrStr), 'File(s) still not found:', MB_ICONERROR+MB_OK);
        Application.Terminate;
        exit;
      end;
    end;
    SetROMBIOS(ROMBIOSfile);
    SetROM1BIOS(ROM1BIOSfile);
    SetROM2BIOS(ROM2BIOSfile);
    if ExtractFilePath(ROMDISKfile)='' then
       ROMDISKfile:=ExtractFilePath(FMainIniFile.Filename)+ROMDISKfile;
    if not FileExists(ROMDISKfile) then
      ROMDISKlen:=0
    else
      SetROMDISK(ROMDISKfile);
    try
      F146818.MCRAM:=MC146818RAM;
      SDController.Scheme:=SDScheme;
      SDController.ImageRO:=SDRO;
      SDController.ImageFile:=SDImage;
      SDController.OnAccess:=OnIdeAccess;
      IdeController.ImageRO[0]:=HDDRO[0];
      IdeController.ImageRO[1]:=HDDRO[1];
      IdeController.ImageFile[0]:=HDDImage[0];
      IdeController.ImageFile[1]:=HDDImage[1];
      IdeController.OnAccess:=OnIdeAccess;
      IdeController.Reset;
      IdeProController.ImageRO[0]:=ProRO[0];
      IdeProController.ImageRO[1]:=ProRO[1];
      IdeProController.ImageFile[0]:=ProImage[0];
      IdeProController.ImageFile[1]:=ProImage[1];
      IdeProController.OnAccess:=OnIdeAccess;
      IdeProController.ResetController;
    except
      on E:Exception do
          Application.MessageBox(PChar(E.Message), 'Error', MB_OK+MB_ICONSTOP);
    end;
    PortF400.KbdType:=TKbdType(KeybType);
    PortF600.Plugin:=F600Plugin;
    PortF600.FuncIdx:=F600Index;
    InitSGRegPort;
    Top:=ScrTop; Left:=ScrLeft;
    SetZoomChecks;
    ShowSGReg;
    ShowSGPort;
    SetFormSize;               //StartScrThread;
    ShowMemDump(True);
    ShowRusLat;
    AutoSnapshot1.Checked:=FAutoSnapshot;
    if not ProcessCmdLine() then                        // allways last init operation (because LoadSnapshot)
      if FRestoreODI then
      begin
        if trim(FODI_DriveA)<>'' then
        begin
          FDController.Drive[0]:=trim(FODI_DriveA);
          ToolButtonFloppyA.Hint:=FODI_DriveA;
        end;
        if trim(FODI_DriveB)<>'' then
        begin
          FDController.Drive[1]:=trim(FODI_DriveB);
          ToolButtonFloppyB.Hint:=FODI_DriveB;
        end;
      end;
    CPUPaused:=GetKeyState(VK_CONTROL)<0;               // initial started PAUSED if CTRL key pressed
    if not CPUPaused then CPUResume;
  except
    raise                      // To  do
  end;
end;

procedure TfrmMain.FormClose(Sender: TObject; var Action: TCloseAction);
var pch: PChar;
begin
  if FAutoSnapshot then
    SaveSnapshot(8, ExtractFilePath(FMainIniFile.Filename)+AutoSnapName);
  CpuPaused:=True;
  CPUSuspend;
  StopScrThread;
  MC146818RAM:=F146818.MCRAM;
  if FRestoreODI then
  begin
    FODI_DriveA:=FDController.Drive[0];
    FODI_DriveB:=FDController.Drive[1];
  end;
  ComPortExists:=FUART.Exists;
  ComPortName:=FUART.PortName;
  F600Plugin:=PortF600.Plugin;
  F600Index:=PortF600.FuncIdx;
  PortF600.Plugin:='';
  SDRO       :=SDController.ImageRO;
  SDImage    :=SDController.ImageFile;
  HDDRO[0]   :=IdeController.ImageRO[0];
  HDDRO[1]   :=IdeController.ImageRO[1];
  HDDImage[0]:=IdeController.ImageFile[0];
  HDDImage[1]:=IdeController.ImageFile[1];
  ProRO[0]   :=IdeProController.ImageRO[0];
  ProRO[1]   :=IdeProController.ImageRO[1];
  ProImage[0]:=IdeProController.ImageFile[0];
  ProImage[1]:=IdeProController.ImageFile[1];
{Eth}
  pch:=@EthMAC[0];
  if Assigned(FNE2kDevice) then
  begin
    if Assigned(FNE2kDevice.EthThread) then
      pch:=FNE2kDevice.EthThread.MACAddr;
  end;
  EthMAC0:=ord(pch[0]);
  EthMAC1:=ord(pch[1]);
  EthMAC2:=ord(pch[2]);
  EthMAC3:=ord(pch[3]);
  EthMAC4:=ord(pch[4]);
  EthMAC5:=ord(pch[5]);
{}
  if ToolButtonFloppyA.Tag=1 then IniManager.RecentFilesAdd(ToolButtonFloppyA.Hint);
  if ToolButtonFloppyB.Tag=1 then IniManager.RecentFilesAdd(ToolButtonFloppyB.Hint);
  ScrTop:=Top; ScrLeft:=Left;
  IniManager.WriteRecentFilesSection;
  IniManager.WriteAll;                  // Variables and Properties
  IniManager.IniFileObj:=nil;
  IniManager.Free;
  FMainIniFile.Free;
  DestroyEthThread;
  DisSL.Free;
  If glphWaveOut<>-1 Then CloseWaveOut();
end;

procedure TfrmMain.InitSGRegPort;
  procedure PortDumpTitles(SG:TStringGrid; offs:integer);
  var ii:integer;
  begin
    for ii:=0 to 6 do with SG do
    begin
      Cells[0, ii] := IntToHex(offs+ii,2);
      if CheckPort[offs+ii]=1 then
        Cells[0,ii]:=Cells[0,ii]+'  @'
      else if CheckPort[offs+ii]=2 then
        Cells[0,ii]:=Cells[0,ii]+'  #';
    end;
  end;
begin
  with SGFlags do
  begin
    Cells[0, 0] := 'S';
    Cells[0, 1] := 'Z';
    Cells[0, 2] := 'H';
    Cells[0, 3] := 'PV';
    Cells[0, 4] := 'N';
    Cells[0, 5] := 'C';
  end;
  With SGRegMain do
  begin
    Cells[0, 0] := 'AF'+ConditionAF;
    Cells[0, 1] := 'BC'+ConditionBC;
    Cells[0, 2] := 'DE'+ConditionDE;
    Cells[0, 3] := 'HL'+ConditionHL;
    Cells[0, 4] := 'SP'+ConditionSP;
    Cells[0, 5] := 'PC';
  end;
  With SGRegAlter do
  begin
    Cells[0, 0] := 'AF'''+ConditionAF_;
    Cells[0, 1] := 'BC'''+ConditionBC_;
    Cells[0, 2] := 'DE'''+ConditionDE_;
    Cells[0, 3] := 'HL'''+ConditionHL_;
    Cells[0, 4] := 'IX'+ConditionIX;
    Cells[0, 5] := 'IY'+ConditionIY;
    Cells[0, 6] := 'IR'+ConditionIR;
  end;
  PortDumpTitles(SGPortDump,$F8);
  PortDumpTitles(SGPort1Dump,0);
  PortDumpTitles(SGPort2Dump,7);
end;

procedure TfrmMain.ShowSGReg;
var disPC, deltaPC: integer;
    DisStr: string;
  function HexStr(PC, delta: integer):string;
  begin
    Result:='';
    while delta>0 do
    begin
      Result:=Result+IntToHex(peekb(PC), 2);
      inc(PC);
      dec(delta);
    end;
  end;
begin
  with SGFlags do
  begin
    if fS  then Cells[1, 0]:='1' else Cells[1, 0]:=' ';
    if fZ  then Cells[1, 1]:='1' else Cells[1, 1]:=' ';
    if fH  then Cells[1, 2]:='1' else Cells[1, 2]:=' ';
    if fPV then Cells[1, 3]:='1' else Cells[1, 3]:=' ';
    if fN  then Cells[1, 4]:='1' else Cells[1, 4]:=' ';
    if fC  then Cells[1, 5]:='1' else Cells[1, 5]:=' ';
  end;
  With SGRegMain do
  begin
    Cells[1, 0] := IntToHex(getAF, 4);
    Cells[1, 1] := IntToHex(getBC, 4);
    Cells[1, 2] := IntToHex(regDE, 4);
    Cells[1, 3] := IntToHex(regHL, 4);
    Cells[1, 4] := IntToHex(regSP, 4);
    Cells[1, 5] := IntToHex(regPC, 4);
  end;
  With SGRegAlter do
  begin
    Cells[1, 0] := IntToHex(regAF_, 4);
    Cells[1, 1] := IntToHex(regBC_, 4);
    Cells[1, 2] := IntToHex(regDE_, 4);
    Cells[1, 3] := IntToHex(regHL_, 4);
    Cells[1, 4] := IntToHex(regIX, 4);
    Cells[1, 5] := IntToHex(regIY, 4);
    Cells[1, 6] := IntToHex(word(intI) shl 8 + intR, 4);
  end;
  With SGHistory do
  begin
    deltaPC:=Disasm(prevPC, DisStr)-prevPC;
    Cells[0, 0]:=IntToHex(prevPC, 4)+'  '+HexStr(prevPC, deltaPC);
    Cells[1, 0]:=DisStr;

    disPC:=RegPC;

    deltaPC:=Disasm(disPC, DisStr)-disPC;
    Cells[0, 1]:=IntToHex(disPC, 4)+'  '+HexStr(disPC, deltaPC);
    Cells[1, 1]:=DisStr;
    disPC:=disPC+deltaPC;

    deltaPC:=Disasm(disPC, DisStr)-disPC;
    Cells[0, 2]:=IntToHex(disPC, 4)+'  '+HexStr(disPC, deltaPC);;
    Cells[1, 2]:=DisStr;
    disPC:=disPC+deltaPC;

    deltaPC:=Disasm(disPC, DisStr)-disPC;
    Cells[0, 3]:=IntToHex(disPC, 4)+'  '+HexStr(disPC, deltaPC);
    Cells[1, 3]:=DisStr;
    Row:=1;
  end;
  if CpuPaused then
  begin
    ToolButtonPause.Down:=True;
    Update;
    Application.ProcessMessages;
  end;
end;

procedure TfrmMain.ShowSGPort;
begin
  With SGPortDump do
  begin
    Cells[1, 0] := IntToHex(MainPort[$F8], 2);
    Cells[1, 1] := IntToHex(MainPort[$F9], 2);
    Cells[1, 2] := IntToHex(MainPort[$FA], 2);
    Cells[1, 3] := IntToHex(MainPort[$FB], 2);
    Cells[1, 4] := IntToHex(MainPort[$FC], 2);
    Cells[1, 5] := IntToHex(MainPort[$FE], 2);
    Cells[1, 6] := IntToHex(MainPort[$FF], 2);
  end;
  With SGPort1Dump do
  begin
    Cells[1, 0] := IntToHex(inb($00), 2);
    Cells[1, 1] := IntToHex(MainPort[$01], 2);
    Cells[1, 2] := IntToHex(MainPort[$02], 2);
    Cells[1, 3] := IntToHex(MainPort[$03], 2);
    Cells[1, 4] := IntToHex(MainPort[$04], 2);
    Cells[1, 5] := IntToHex(MainPort[$05], 2);
    Cells[1, 6] := IntToHex(MainPort[$06], 2);
  end;
  With SGPort2Dump do
  begin
    Cells[1, 0] := IntToHex(MainPort[$07], 2);
    Cells[1, 1] := IntToHex(MainPort[$08], 2);
    Cells[1, 2] := IntToHex(MainPort[$09], 2);
    Cells[1, 3] := IntToHex(MainPort[$0A], 2);
    Cells[1, 4] := IntToHex(MainPort[$0B], 2);
    Cells[1, 5] := IntToHex(MainPort[$0C], 2);
    Cells[1, 6] := IntToHex(MainPort[$0D], 2);
  end;
end;

procedure TfrmMain.FormCloseQuery(Sender: TObject; var CanClose: Boolean);
begin
  Screen.Cursor:=crHourGlass;
  frmMain.Update;
  CPUPaused:=False;
  if Assigned(ScrThread) and ScrThread.Suspended then ScrThread.Resume;
end;

function padl(stt:string;max:integer;ch:string):string;
begin
  if length(stt)>=max then padl:=copy(stt,1,max)
  else
  begin
    while length(stt)+length(ch)<=max do stt:=ch+stt;
    if length(stt)<max then stt:=copy(ch,1,max-length(stt))+stt;
    padl:=stt;
  end;
end;

procedure chrtrn(var buf: string; from_tbl, to_tbl: string);
var xpos,i: integer;
begin
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
end;

procedure TfrmMain.ShowMemDump(PauseCPU: boolean);
var save_pF9, pF9: byte;
    Addr, i, j, k, dd: integer;
    st, stt, ast: string;
begin
//  SetDumpAddr;
  k:=pos(':',MEDumpAddr.Text)-1;
  pF9:=HexToInt(padl(copy(MEDumpAddr.Text, 1, k),2,'0'));
  Addr:=HexToInt(copy(MEDumpAddr.Text, k+2, 4));
  if Addr<0 then exit;
  Addr:=Addr and $FFF0;
  MemDump.Clear;
  st:='00000000 00000000 00000000 00000000  0000000000000000  ';
  if PauseCPU then
    CPUSuspend;
  save_pF9:=MainPort[$F9];
  OutB($F9, pF9);                            // select memory page
  for j:=0 to 15 do
  begin
    ast:=LongAddressStr(pF9,Addr);
    for i:=0 to 15 do
    begin
      stt:=padl(IntToHex(peekb(Addr), 2), 2, '0');
      case i of
        0..3:  dd:=1;
        4..7:  dd:=2;
        8..11: dd:=3
        else   dd:=4;
      end;
      st[i*2+dd]:=stt[1]; st[i*2+dd+1]:=stt[2];
      if peekb(Addr)>32 then st[i+38]:=chr(peekb(Addr)) else st[i+38]:='.';
      inc(Addr);
    end;
    MemDump.Lines.Add(ast+'  '+st);
  end;
  OutB($F9, save_pF9);
  if PauseCPU and (not CPUPaused) then CPUResume;             { now run the thread }
  MemDump.SelStart:=0;
  MemDump.SelLength:=1;
  MemDump.Update;
end;

procedure TfrmMain.BtnModyByteClick(Sender: TObject);
var st, stt:string;
    save_pF9, pF9, Addr: integer;
    xAddr, i, k, dd: integer;
    buf: array of byte;
begin
  k:=pos(':',MEDumpAddr.Text)-1;
  pF9:=HexToInt(padl(copy(MEDumpAddr.Text, 1, k),2,'0'));
  Addr:=HexToInt(copy(MEDumpAddr.Text, k+2, 4));
  if Addr<0 then exit;
  xAddr:=Addr;
  st:='                                             ';
  CPUSuspend;                             { pause the thread }
  save_pF9:=MainPort[$F9];
  OutB($F9, pF9);                            // select memory page
  for i:=(xAddr and $0F) to 15 do
  begin
    stt:=padl(IntToHex(peekb(xAddr), 2), 2, '0');
    case i of
      0..3:  dd:=9;
      4..7:  dd:=10;
      8..11: dd:=11
      else   dd:=12;
    end;
    st[i*2+dd]:=stt[1]; st[i*2+dd+1]:=stt[2];
    inc(xAddr);
  end;
  st:=Trim(st);
  stt:=InputBox('Modify bytes beginning from '+MEDumpAddr.Text,
                'Enter bytes sequence with leading zeros:', st);
  if stt<>st then
  begin
    chrtrn(stt, ' ', '');
    if Length(stt)=0 then exit;
    i:=Length(stt);
    i:=i-(i mod 2);
    dd:=0;
    SetLength(buf, i);
    while i>0 do
    begin
      st:=copy(stt, dd*2+1, 2);
      if HexToInt(st)<0 then
      begin
        ShowMessage('Wrong byte: `'+st+'`. No modification will be maked at all.');
        OutB($F9, save_pF9);
        if not CPUPaused then CPUResume;       { now run the thread }
        exit;
      end;
      buf[dd]:=Lo(HexToInt(st));
      dec(i, 2);
      inc(dd);
    end;
    for i:=0 to dd-1 do
      pokeb(Addr+i, buf[i]);
  end;
  OutB($F9, save_pF9);
  ShowMemDump(False);
  if not CPUPaused then CPUResume;             { now run the thread }
end;

procedure TfrmMain.AppKeyDown(var msg: TMsg; var Handled: Boolean);
begin
  case Msg.message of
    WM_KEYDOWN: if Msg.wParam=VK_PAUSE then
                    ActPauseExecute(Self)
                else
                if (frmSetts=nil) and (AboutBox=nil) and (pnScr.Visible) then
                begin
                  GetKeyboardState(KEYBRD);
                  Handled:=True;
                end;
    WM_KEYUP: if (frmSetts=nil) and (AboutBox=nil) and (pnScr.Visible) then
              begin
                 GetKeyboardState(KEYBRD);
                 if FLcdRusLat<>integer(PortF400.LcdRusLat) then
                   ShowRusLat();
                 Handled:=True;
              end;
  end;
end;

procedure TfrmMain.ActPauseExecute(Sender: TObject);
begin
  CPUPaused:=not CPUPaused;
  if CPUPaused then
      CPUSuspend
    else
      CPUResume;
  sleep(50);
end;

procedure TfrmMain.ActResetExecute(Sender: TObject);
begin
  BreakPointPF9:=$FF;
  CPUSuspend;
  InitEmulator;
  InitSGRegPort;
  CPUResume;
end;

procedure Scr480(outbyte: byte);
begin
  if (outbyte and $80 = 0) then ScrWidth:=SCR_WIDTH_384
    else begin
           if Z80CardMode>=Z80_ORIONPRO_v2 then
             ScrWidth:=SCR_WIDTH_512
           else
             ScrWidth:=SCR_WIDTH_480;
         end;
  if PrevScrWidth=ScrWidth then exit;
  with frmMain do
  begin
    StopScrThread;
    SetFormSize;
    PrevScrWidth:=ScrWidth;
    StartScrThread;
  end;
end;

procedure TfrmMain.ActZoomExecute(Sender: TObject);
begin
  ZoomPopupMenu.Popup(Left+ToolButtonZoom.Left, Top+pbDraw.Top+46);
  Update;
  Application.ProcessMessages;
end;

procedure TfrmMain.SetFloppyHint(FDIndex: integer; isReadOnly: boolean);
var ro:string;
    TB: TToolButton;
begin
    if FDIndex=0 then
    begin
      TB:=ToolButtonFloppyA;
      ro:='A';
    end
    else
    begin
      TB:=ToolButtonFloppyB;
      ro:='B';
    end;
    TB.Hint:=FDController.Drive[FDIndex];
    if TB.Hint='' then begin
      TB.Hint:='Select '+ODI_EXT+' file as floppy '+ro;
      TB.Tag:=0;
    end  
    else begin
      if isReadOnly then TB.Hint:=TB.Hint+stReadOnly;
      TB.Tag:=1;
    end;
end;

procedure TfrmMain.ItemBrowseClick(Sender: TObject);
var TB: TToolButton;
begin
  OpenDialog.Title:='Select file with FDD disk image';
  OpenDialog.DefaultExt:=ODI_EXT;
  OpenDialog.Filter:=ODI_FILTER;
  OpenDialog.FilterIndex:=1;
  if OpenDialog.Execute then
  begin
    FDController.ReadOnly[FDriveindex]:=ofReadOnly in OpenDialog.Options;
    FDController.Drive[FDriveindex]:=OpenDialog.FileName;
    if FDriveindex=0 then
      TB:=ToolButtonFloppyA
    else
      TB:=ToolButtonFloppyB;
    if TB.Tag=1 then IniManager.RecentFilesAdd(TB.Hint);
    SetFloppyHint(FDriveindex, ofReadOnly in OpenDialog.Options);
  end;
  Update;
  Application.ProcessMessages;
end;

procedure TfrmMain.ToolButtonFloppyAClick(Sender: TObject);
begin
  FDriveindex:=0;
  OdiPopupMenu.Popup(Left+ToolButtonFloppyA.Left, Top+pbDraw.Top+46);
  Update;
  Application.ProcessMessages;
end;

procedure TfrmMain.ToolButtonFloppyBClick(Sender: TObject);
begin
  FDriveindex:=1;
  OdiPopupMenu.Popup(Left+ToolButtonFloppyB.Left, Top+pbDraw.Top+46);
  Update;
  Application.ProcessMessages;
end;

procedure TfrmMain.BtnSaveMemClick(Sender: TObject);
var j, Addr: integer;
    save_pF9, pF9, bbb: byte;
    FS: TFileStream;
begin
  SaveDialog.DefaultExt:='dmp';
  SaveDialog.Title:='Specify file to save memory dump';
  if SaveDialog.Execute then
  begin
    CPUSuspend;
    save_pF9:=MainPort[$F9];
    pF9:=HexToInt(copy(MEDumpAddr.Text, 1, 2));
    Addr:=HexToInt(copy(MEDumpAddr.Text, 4, 4));
    if Addr<0 then exit;
    OutB($F9, pF9);                            // select memory page
    FS:=nil;
    FS:=TFileStream.Create(SaveDialog.Filename, fmCreate);
    try
      j:=0;
      while (j<StrToInt(trim(MESaveCnt.Text)))and(pF9<RAMPagesCount) do
      begin
        bbb:=peekb(Addr);
        FS.Write(bbb, 1);
        Inc(Addr);
        inc(j);
        if Addr>=$10000 then
        begin
          Addr:=0;
          inc(pF9);
          OutB($F9, pF9);                            // select memory page
        end;
      end;
    except
      MESaveCnt.SetFocus;
    end;
    if Assigned(FS) then FS.Free;
    OutB($F9, save_pF9);                            // select memory page
    if not CPUPaused then CPUResume;             { now run the thread }
  end;
end;

procedure TfrmMain.ItemClearClick(Sender: TObject);
begin
  FDController.Drive[FDriveindex]:='';
  SetFloppyHint(FDriveIndex, False);
end;

function GetHexMasked4(ch: char; var Mask: byte): byte;
begin
  if (ch in [' ','x','X','?','*']) then ch:='_';
  if ch='_' then Mask:=$FF else Mask:=0;
  Result:=StrToIntDef(ch,0);
end;

function GetHexMasked16(st: string; var Mask: integer): integer;
var i, xMask: integer;
begin
  Mask:=0;
  xMask:=$F;
  for i:=length(st) downto 1 do
  begin
    if (st[i] in [' ','x','X','?','*','_']) then
    begin
      st[i]:='0';
      Mask:=Mask or xMask;
    end;
    xMask:=xMask shl 4;
  end;
  Result:=HexToInt(st);
end;

procedure TfrmMain.cbBreakPointClick(Sender: TObject);
begin
  if cbBreakPoint.Checked then
  begin
    BreakPointPF9:=GetHexMasked4(MEBreakPoint.Text[1], BreakPointPF9mask);
    BreakPointAddr:=GetHexMasked16(copy(MEBreakPoint.Text, 3, 4), BreakPointAddrMask);
  end
  else begin
    BreakPointPF9:=$FF;
    BreakPointPF9mask:=0;
  end;
end;

procedure TfrmMain.ItemRecentClick(Sender: TObject);
var pp:integer;
begin
  if Assigned(Sender)and(Sender is TMenuItem) then
  begin
    pp:=pos(stReadOnly, (Sender as TMenuItem).Caption);
    FDController.ReadOnly[FDriveindex]:=(pp>0) and (pp=Length((Sender as TMenuItem).Caption)-Length(stReadOnly)+1);
    if pp>0 then
      FDController.Drive[FDriveindex]:=copy((Sender as TMenuItem).Caption, 1, pp-1)
    else
      FDController.Drive[FDriveindex]:=(Sender as TMenuItem).Caption;
    if FDriveindex=0 then
    begin
      ToolButtonFloppyA.Hint:=(Sender as TMenuItem).Caption;
      ToolButtonFloppyA.Tag:=1;
    end
    else
    begin
      ToolButtonFloppyB.Hint:=(Sender as TMenuItem).Caption;
      ToolButtonFloppyB.Tag:=1;
    end;
    Update;
    Application.ProcessMessages;
  end;
end;

procedure TfrmMain.CPUResume;
begin
  Application.OnIdle:=MyIdleHandler;
  StatusBar.Panels[StatusBar.Panels.Count-1].Text:='';
  ToolButtonPause.Down:=CPUPaused;
end;

procedure TfrmMain.CPUSuspend;
begin
  Application.OnIdle:=nil;
  StatusBar.Panels[StatusBar.Panels.Count-1].Text:='CPU Paused';
  ToolButtonPause.Down:=CPUPaused;
end;

procedure OnInstruction;
begin
  if CpuPaused then frmMain.CPUSuspend;
  if not frmMain.pnDbg.Visible then exit;
  frmMain.ShowSGReg;
  frmMain.ShowSGPort;
  Application.ProcessMessages;
end;

procedure OnBreakPoint;
begin
  CPUPaused:=True; 
  frmMain.CPUSuspend;
  frmMain.ShowSGReg;
  frmMain.ShowSGPort;
  Application.ProcessMessages;
end;

procedure CheckHddAccess(Value: integer);
begin
  with frmMain.StatusBar.Panels[1] do
    if Bevel=pbLowered then
    begin
      Bevel:=pbRaised;
      Text:='HDD:';
    end
    else if Length(Text)>4 then
           Bevel:=pbLowered;
  frmMain.StatusBar.Update;
  Application.ProcessMessages;
end;

procedure ShowEthStat;
begin
  if Assigned(EthThread) then
    frmMain.StatusBar.Panels[4].Text:='Eth: '+EthThread.GetStat;
end;

procedure OnCPUAccess(CpuIdlePercent: integer);
begin
  frmMain.StatusBar.Panels[3].Text:='Idle: '+IntToStr(CpuIdlePercent)+'%';
end;

procedure OnHalfSecond(Value: integer);
begin
  CheckHddAccess(Value);
end;

procedure OnOneSecond(CpuIdlePercent: integer);
begin
  OnCPUAccess(CpuIdlePercent);
  ShowEthStat();
end;

procedure OnFddAccess(Sender: TObject; Op: string; BeginOp: boolean);
begin
  if BeginOp then
    frmMain.StatusBar.Panels[0].Bevel:=pbLowered          // pbNone;
  else begin
    Op:='';
    sleep(80);
    frmMain.StatusBar.Panels[0].Bevel:=pbRaised;
  end;
  frmMain.StatusBar.Panels[0].Text:='FDD: '+Op;
  frmMain.StatusBar.Update;
  Application.ProcessMessages;
end;

procedure TfrmMain.FormCreate(Sender: TObject);
  var ThreadHandle: THandle;
begin
  ScrThread:=nil;
{$IFDEF USE_DEBUGGING}
  AssignFile(debugFile, 'OrionZEmDebug.txt');
  Rewrite(debugFile);
  ReportDebugCallCtr:=0;
{$ENDIF}
  ThreadHandle := GetCurrentThread;
  SetThreadPriority(ThreadHandle, THREAD_PRIORITY_HIGHEST);
  z80runstate := Z80RUNNING;
  Application.OnIdle:=MyIdleHandler;
  AfterInstruction:=OnInstruction;
  AfterBreakPoint:=OnBreakPoint;
  AfterOneSecond:=OnOneSecond;
  AfterHalfSecond:=OnHalfSecond;
  FDController.OnAccess:=OnFddAccess;
  InitRAMArr();
  InitEmulator();
  DisSL:=TStringList.Create;
  InitDizSL();
  Global_TStates := -glTstatesPerInterrupt;
end;

procedure TfrmMain.ActSettingsExecute(Sender: TObject);
begin
  frmSetts:=TfrmSetts.Create(Application);
  if (Assigned(frmSetts)) then with frmSetts do
  try
    ShowModal;
    Free;
  finally
    frmSetts:=nil;
  end;
end;

procedure TfrmMain.InitDizSL;
var i: integer;
    ss, sss: string;
begin
  DisSL.LoadFromFile(CheckFileExists(ChangeFileExt(System.ParamStr(0),'.DIS')));
  for i:=0 to DisSL.Count-1 do
  begin
    sss:=DisSL[i];
    ss:=LeftSubstr(sss);
    DisSL.Objects[i]:=pointer(HexToInt(LeftSubstr(ss)));
  end;
end;

function TfrmMain.CheckFileExists(FlName: string): string;
begin
    If FileExists(FlName) then
         Result:=FlName
    else if FileExists(ExtractFilePath(ParamStr(0))+FlName) then
            Result:=ExtractFilePath(ParamStr(0))+FlName
    else
         Result:=ExtractFileName(FlName);
end;

function TfrmMain.disasm(PC: integer; var OP: string): integer;
var idx, ii: integer;
    st, st1: string;
begin
  Result:=PC+1;
  OP:='';
  idx:=DisSL.IndexOfObject(pointer(peekb(PC)));
  if idx<0 then
    idx:=DisSL.IndexOfObject(pointer(256*peekb(PC)+peekb(PC+1)));
  if idx>=0 then
  begin
    st:=DisSL[idx];
    st1:=trim(copy(LeftSubstr(st), 3, 100));
    OP:=LeftSubstr(st);
    while st1<>'' do
    begin
      st:=LeftSubstr(st1);
      if Length(st)=1 then
      case st[1] of
        'm': begin
               idx:=pos('m', OP);
               if idx>0 then
               begin
                 insert(IntToHex(peekb(Result), 2), OP, idx);
                 insert(IntToHex(peekb(Result+1), 2), OP, idx);
                 delete(OP, idx+4, 1);
                 inc(Result, 2);
               end;
             end;
        'e': begin
               idx:=pos('e', OP);
               if idx>0 then
               begin
                 ii:=Result+Shortint(peekb(Result))+1;
                 insert(IntToHex(ii, 4), OP, idx);
                 delete(OP, idx+4, 1);
                 inc(Result);
               end;
             end
        else begin                          // 'n','d'
               idx:=pos(st[1], OP);
               if idx>0 then
               begin
                 insert(IntToHex(peekb(Result), 2), OP, idx);
                 delete(OP, idx+2, 1);
                 inc(Result);
               end;
             end;
      end
      else inc(Result);
    end;
  end;
end;

procedure TfrmMain.ActHelpExecute(Sender: TObject);
begin
  AboutBox:=TAboutBox.Create(Application);
  with AboutBox do
  try
    ShowModal;
    Free;
  finally
    AboutBox:=nil;
  end;  
end;

procedure TfrmMain.InitRAMArr;
var ii, kk: integer;
begin
  for ii:= 2 to RAMPagesCount-1 do
  begin
    kk:=0;
    while kk<$FFFF do
    begin
      RAMArr[ii,kk]:=$E5;   // TRASH in memory
      inc(kk, 32);
    end;
  end;
end;

function TfrmMain.DetectFileType(FName: string; var Fsz: integer): TOFileType;
var CRC: integer;
    FStream: TFileStream;
    FExt: string;
begin
  Result:=ftUnknown;
  FStream:=nil;
  try
    FStream:=TFileStream.Create(FName, fmOpenRead or fmShareDenyWrite);
    FExt:=AnsiUpperCase(ExtractFileExt(FName));
    Fsz:=FStream.Size;
    if (Fsz < RAMDISK_TOP)and(Fsz>31) then                     // filesize less ORDOS RAMDISK limit
      if (FExt='.'+RKO_EXT) then Result:=ftRko else
      if (FExt='.'+ORD_EXT) then Result:=ftOrd else
      if (FExt='.'+BRU_EXT) then Result:=ftBru;
    if (Result=ftUnknown )and
       (FSz>sizeof(TOriHeader)+$20000) and                     // cannot be small
       (FSz<100000000) then                                    // cannot be large
    with OriHeader do
    begin
      FStream.Read(OriHeader, sizeof(OriHeader));                       // snaphots have a CRC
      FStream.Read(CRC, sizeof(CRC));
      if (pos(TAG_STR, _tag)=1) and (CRC=CRC32(OriHeader, 0, sizeof(OriHeader))) then
        Result:=ftSnapshot
      else
        if (FExt='.'+ODI_EXT) and (FSz mod 1024 = 0)        // 'ODI' extension, mod sector size
          then Result:=ftDiskImage;
    end;
  finally
    if Assigned(FStream) then
      FStream.Free;
  end;
end;

function TfrmMain.ProcessCmdLine: boolean;
var ss: string;
    autosnap: boolean;
    param_n, next_ordos, fsize, datasize: integer;
    ft: TOFileType;
    FStream: TFileStream;
begin
  autosnap:=True;
  Result:=True;
  next_ordos:=0;
  for param_n:=1 to ParamCount() do
  begin
    ss:=CheckFileExists(ParamStr(param_n));
    ft:=DetectFileType(ss, fsize);
    case ft of
      ftSnapshot: if param_n=1 then
                  begin
                    LoadSnapshot(ss);
                    autosnap:=False;
                  end;
      ftDiskImage:
        if param_n<3 then
        begin
          FDriveindex:=param_n-1;
          FDController.Drive[FDriveindex]:=ss;
          SetFloppyHint(FDriveindex, False);
          autosnap:=False;
        end;
      ftRko, ftBru, ftOrd:                              // ordos_header[10..11] - ORDOS filesize
        if (next_ordos+fsize<RAMDISK_TOP) then
        begin
          FStream:=nil;
          try
            FStream:=TFileStream.Create(ss, fmOpenRead or fmShareDenyWrite);
            if (ft=ftRko) then
              FStream.Seek(77, soFromBeginning);                                // skip RKO header
            if FStream.Read(PByte(@RAMArr[1, next_ordos])^, 16)=16 then         // read ordos header
            begin
              datasize:=PWord(@RAMArr[1, next_ordos+10])^;
              if (datasize<RAMDISK_TOP-next_ordos-16) then
              begin
                FStream.Read(PByte(@RAMArr[1, next_ordos+16])^, datasize);
                next_ordos:=next_ordos + datasize + 16;                         // 16 = ordos_header size
                autosnap:=False;
              end;
            end;
          finally
            RAMArr[1, next_ordos]:=$FF;   // end of ordos files chain
            if Assigned(FStream) then
              FStream.Free;
          end;
        end;
    end;
  end;
  if FAutoSnapshot and autosnap then begin
    LoadSnapshot(ExtractFilePath(FMainIniFile.Filename)+AutoSnapName);
    exit;
  end;
  Result:=False;
end;

procedure TfrmMain.SaveSnapshot(Pages: integer; FName:string);
var CRC: integer;
    xPaused: boolean;
    FStream: TFileStream;
begin
  if trim(FName)='' then
  begin
    SaveDialog.Title:='Specify file for Orion snapshot';
    SaveDialog.DefaultExt:='ORI';
    SaveDialog.Filter:='Orion snapshots (*.ori)|*.ori|Any file (*.*)|*.*';
    SaveDialog.FilterIndex:=1;
    if SaveDialog.Execute then
      FName:=SaveDialog.FileName;
  end;
  if trim(FName)<>'' then
    with OriHeader do                        // snapshot header
    begin
      xPaused:=CPUPaused;
      CPUPaused:=True;
      CPUSuspend;
      StrFmt(_tag, TAG_STR+'%dk', [Pages*64]);
      _regA:=      regA;
      _regHL:=     regHL;
      _regB:=      regB;
      _regC:=      regC;
      _regDE:=     regDE;
      _fS:=        fS;
      _fZ:=        fZ;
      _f5:=        f5;
      _fH:=        fH;
      _f3:=        f3;
      _fPV:=       fPV;
      _fN:=        fN;
      _fC:=        fC;
      _regAF_:=    regAF_;
      _regHL_:=    regHL_;
      _regBC_:=    regBC_;
      _regDE_:=    regDE_;
      _regIX:=     regIX;
      _regIY:=     regIY;
      _regID:=     regID;
      _regSP:=     regSP;
      _regPC:=     regPC;
      _intI:=      intI;
      _intR:=      intR;
      _intRTemp:=  intRTemp;
      _intIFF1:=   intIFF1;
      _intIFF2:=   intIFF2;
      _intIM:=     intIM;
      _DeltaDate:= DeltaDate;
      _DeltaTime:= DeltaTime;
      _KeyDelay:=  KeyDelay;
      _KeyRusLat:= KeyRusLat;
      _MemSizeMode:=          MEMSizeMode;
      _CPUSpeedMode:=         CPUSpeedMode;
      _Z80CardMode:=          Z80CardMode;
//
      _SoundEnabled:=         SoundEnabled;
      _AyEnabled:=            AyEnabled;
      _glWavePtr:=            glWavePtr;
      _glWaveAddTStates:=     glWaveAddTStates;
      _AYPSG:=                AYPSG;
      _AY_OutNoise:=          AY_OutNoise;
      _VolA:=                 VolA;
      _VolB:=                 VolB;
      _VolC:=                 VolC;
      _lOut1:=                lOut1;
      _lOut2:=                lOut2;
      _lOut3:=                lOut3;
      _AY_Left:=              AY_Left;
      _AY_NextEvent:=         AY_NextEvent;
//
      _Global_TStates:=       Global_TStates;
      _z80runstate:=          z80runstate;
      _glTstatesPerInterrupt:=glTstatesPerInterrupt;
      _KeybType:=             KeybType;
      _KeyExtender:=          KeyExtender;
      _FddHD:=                FddHD;
      _HDDPort:=              HDDPort;
      _ScrZoom:=              ScrZoom;
      _SDScheme:=             SDScheme;
      _PFEEnabled:=           PFEEnabled;
      _ROMBIOSfile:=          ROMBIOSfile;
      _ROMDISKfile:=          ROMDISKfile;
      _ROMDISKlen:=           ROMDISKlen;
      _NPages:=               Pages;
//
      CRC:=CRC32(OriHeader, 0, sizeof(OriHeader));
//
      FStream:=nil;
      try
        FStream:=TFileStream.Create(FName, fmCreate);
        FStream.Seek(0, soFromBeginning);
        FStream.Write(OriHeader, sizeof(OriHeader));    // constant length part
        FStream.Write(CRC, sizeof(CRC));                // for filetype autodetect
        PortF400.SaveToStream(FStream);                 // variable length part
        PortF500.SaveToStream(FStream);
        PortF600.SaveToStream(FStream);
        FDController.SaveToStream(FStream);
        IdeController.SaveToStream(FStream);
        SDController.SaveToStream(FStream);                
        F146818.SaveToStream(FStream);
        FStream.Write(Parity, sizeof(Parity));
        FStream.Write(MainPort, sizeof(MainPort));
        FStream.Write(ROMF800, sizeof(ROMF800));
        FStream.WriteBuffer(ROMDISK[0], _ROMDISKlen);
        FStream.Write(RAMARR, Pages*RAM_PAGE_SIZE+1);
      finally
        if Assigned(FStream) then FStream.Free;
        CPUPaused:=xPaused;
        if not CPUPaused then
          CPUResume;
      end;
    end;
end;

procedure TfrmMain.LoadSnapshot(FName: string);
var FStream: TFileStream;
    CRC, ii: integer;
    xPaused: boolean;
    xMainPort: TMainPort;
begin
{$IFDEF FREEWARE}
  Application.MessageBox(
    'Snapshots are not allowed in FREEWARE version',
    PChar(Application.Title),
    MB_OK or MB_ICONEXCLAMATION);
  exit;
{$ENDIF}
  FStream:=nil;
  xPaused:=CPUPaused;
  if not FileExists(FName) then exit;
  if DetectFileType(FName, ii)<>ftSnapshot then
    Application.MessageBox(
      PChar('Snapshot file has wrong format:'#13#10#13#10+FName),
      PChar(Application.Title),
      MB_OK or MB_ICONEXCLAMATION)
  else
    with OriHeader do                        // snapshot header
    try
      CPUPaused:=True;
      CPUSuspend;
      sleep(50);
      FStream:=TFileStream.Create(FName, fmOpenRead or fmShareDenyWrite);
      FStream.Seek(0, soFromBeginning);
      FStream.Read(OriHeader, sizeof(OriHeader));
      FStream.Read(CRC, sizeof(CRC));
      PortF400.ReadFromStream(FStream);
      PortF500.ReadFromStream(FStream);
      PortF600.ReadFromStream(FStream);
      FDController.Drive[0]:='';
      FDController.Drive[1]:='';
      FDController.ReadFromStream(FStream);
      IdeController.ReadFromStream(FStream);              //sa
      SDController.ReadFromStream(FStream);              //sa
      SetFloppyHint(0, FDController.ReadOnly[0]);
      SetFloppyHint(1, FDController.ReadOnly[1]);
      F146818.ReadFromStream(FStream);
      FStream.Read(Parity, sizeof(Parity));
      FStream.Read(xMainPort, sizeof(MainPort));
      FStream.Read(ROMF800, sizeof(ROMF800));
      SetLength(ROMDISK, _ROMDISKlen+1);
      FStream.ReadBuffer(ROMDISK[0], _ROMDISKlen);
      FStream.Read(RAMARR, _NPages*RAM_PAGE_SIZE+1);
      regA:=      _regA;
      regHL:=     _regHL;
      regB:=      _regB;
      regC:=      _regC;
      regDE:=     _regDE;
      fS:=        _fS;
      fZ:=        _fZ;
      f5:=        _f5;
      fH:=        _fH;
      f3:=        _f3;
      fPV:=       _fPV;
      fN:=        _fN;
      fC:=        _fC;
      regAF_:=    _regAF_;
      regHL_:=    _regHL_;
      regBC_:=    _regBC_;
      regDE_:=    _regDE_;
      regIX:=     _regIX;
      regIY:=     _regIY;
      regID:=     _regID;
      regSP:=     _regSP;
      regPC:=     _regPC;
      intI:=      _intI;
      intR:=      _intR;
      intRTemp:=  _intRTemp;
      intIFF1:=   _intIFF1;
      intIFF2:=   _intIFF2;
      intIM:=     _intIM;
      DeltaDate:= _DeltaDate;
      DeltaTime:= _DeltaTime;
      KeyDelay:=  _KeyDelay;
      KeyRusLat:= _KeyRusLat;
      MEMSizeMode:=          _MemSizeMode;
      SetMemSize();
      CPUSpeedMode:=         _CPUSpeedMode;
      Z80CardMode:=          _Z80CardMode;
//
      SoundEnabled:=         _SoundEnabled;
      AyEnabled:=            _AyEnabled;
      glWavePtr:=            _glWavePtr;
      glWaveAddTStates:=     _glWaveAddTStates;
      AYPSG:=                _AYPSG;
      AY_OutNoise:=          _AY_OutNoise;
      VolA:=                 _VolA;
      VolB:=                 _VolB;
      VolC:=                 _VolC;
      lOut1:=                _lOut1;
      lOut2:=                _lOut2;
      lOut3:=                _lOut3;
      AY_Left:=              _AY_Left;
      AY_NextEvent:=         _AY_NextEvent;
//
      Global_TStates:=       _Global_TStates;
      z80runstate:=          _z80runstate;
      glTstatesPerInterrupt:=_glTstatesPerInterrupt;
      KeybType:=             _KeybType;
      KeyExtender:=          _KeyExtender;
      FddHD:=                _FddHD;
      HDDPort:=              _HDDPort;
      if ScrZoom<>_ScrZoom then SetFormSize;
      SDScheme:=             _SDScheme;
      PFEEnabled:=           _PFEEnabled;
      ROMBIOSfile:=          _ROMBIOSfile;
      ROMDISKfile:=          _ROMDISKfile;
      ROMDISKlen:=           _ROMDISKlen;
      for ii:=0 to $FC do
        outb(ii, xMainPort[ii]);
    finally
      if Assigned(FStream) then FStream.Free;
{$IFDEF USE_SOUND}
      if SoundEnabled then
        SoundEnabled:=SoundEnabled and InitializeWaveOut();
{$ENDIF}
      if not Assigned(ScrThread) then
        StartScrThread;
      sleep(50);
      CPUPaused:=xPaused;
      if not CPUPaused then
        CPUResume;
      SetZoomChecks;
    end;
end;

procedure TfrmMain.Saveshapshot128k1Click(Sender: TObject);
begin
  SaveSnapshot(2, '');
end;

procedure TfrmMain.Saveshapshot256k1Click(Sender: TObject);
begin
  SaveSnapshot(4, '');
end;

procedure TfrmMain.Saveshapshot512k1Click(Sender: TObject);
begin
  SaveSnapshot(8, '');
end;

procedure TfrmMain.Loadsnapshot1Click(Sender: TObject);
begin
  OpenDialog.Title:='Select file with Orion snapshot';
  OpenDialog.DefaultExt:='ORI';
  OpenDialog.Filter:='Orion snapshots (*.ori)|*.ori|Any file (*.*)|*.*';
  OpenDialog.FilterIndex:=1;
  if OpenDialog.Execute then
    LoadSnapshot(OpenDialog.FileName);
end;

procedure TfrmMain.ToolButtonSnapshotClick(Sender: TObject);
begin
  SnapPopupMenu.Popup(Left+ToolButtonSnapshot.Left, Top+pbDraw.Top+46);
  Update;
  Application.ProcessMessages;
end;

procedure TfrmMain.AutoSnapshot1Click(Sender: TObject);
begin
  FAutoSnapshot:=not FAutoSnapshot;
  AutoSnapshot1.Checked:=FAutoSnapshot;
end;

procedure TfrmMain.ShowRusLat;
begin
  if PortF400.LcdRusLat
    then StatusBar.Panels[2].Text:='KB: Pyc'
    else StatusBar.Panels[2].Text:='KB: Lat';
  FLcdRusLat:=integer(PortF400.LcdRusLat);
end;

// "main loop"
//It runs spectrum for one interrupt period (50 ms spectrum time)
//and tells then Windows it needs more processor cycles (Done:=False).
//This way it is a definite CPU hog, but should be fast too
//During setting screens, emulation is paused (z80runstate=Z80PAUSED)
procedure TfrmMain.MyIdleHandler(Sender: TObject; var Done: Boolean);
begin
  if (z80runstate=Z80RUNNING) then
  begin
    //Execute some z80 code
    execute(Global_TStates);
    Done := false;
  end
  else
  begin
    Sleep(100);
    Done := true;
  end;
end;

procedure TfrmMain.ActScrExecute(Sender: TObject);
begin
  pnScr.Visible:=True;
  pnDbg.Visible:=False;
  ToolButtonScr.Down:=pnScr.Visible;
  ToolButtonDbg.Down:=pnDbg.Visible;
end;

procedure TfrmMain.ActDbgExecute(Sender: TObject);
begin
  pnScr.Visible:=False;
  pnDbg.Visible:=True;
  ToolButtonScr.Down:=pnScr.Visible;
  ToolButtonDbg.Down:=pnDbg.Visible;
  ShowSGReg;
  ShowSGPort;
  ShowMemDump(True);
end;

procedure TfrmMain.SetFormSize;
var i, m, prev: integer;
    b: boolean;
    FResize: TNotifyEvent;
begin
  StopScrThread;
  prev:=ClientHeight;
  b:=pnScr.Visible;
  if not b then
    ActScrExecute(Self);
  FResize:=OnResize;
  OnResize:=nil;
  m:=ScrHeightArr[ScrZoom]+ToolBar.Height+StatusBar.Height+Bevel1.Height;
  ClientHeight:=m;
  if pbDraw.Height<ScrHeightArr[ScrZoom] then
  repeat
    i:=ClientHeight+1;
    ClientHeight:=i;
    if i<>ClientHeight then begin
      ClientHeight:=prev;
      ScrZoom:=PrevScrZoom;
      raise Exception.Create('Can not resize form Height');
    end;  
  until pbDraw.Height=ScrHeightArr[ScrZoom];
  m:=ScrWidthArr[ScrWidth, ScrZoom];
  Width:=m;
  if pbDraw.Width<m then
  repeat
    i:=Width+1;
    Width:=i;
    if i<>Width then
      raise Exception.Create('Can not resize form Width'#13#10'Use lower screen zoom.');
  until pbDraw.Width=m;
  OnResize:=FResize;
  FormResize(frmMain);
  if not b then
    ActDbgExecute(Self);
  StartScrThread;
  Update;
  Application.ProcessMessages;
end;

procedure TfrmMain.x1menuitemClick(Sender: TObject);
begin
  if (ScrZoom=TMenuItem(Sender).Tag)or
     (ScrHeightArr[TMenuItem(Sender).Tag]>GetDeviceCaps(GetDC(0), VERTRES))or
     (ScrWidthArr[ScrWidth, TMenuItem(Sender).Tag]>GetDeviceCaps(GetDC(0), HORZRES))
   then exit;
  PrevScrZoom:=ScrZoom;
  ScrZoom:=TMenuItem(Sender).Tag;
  if pnScr.Visible then
    SetFormSize
  else begin
    pnScr.Visible:=True;
    SetFormSize;
    pnScr.Visible:=False;
  end;
  SetZoomChecks;
end;

procedure TfrmMain.SetZoomChecks;
var i: integer;
begin
  for i:=0 to ZoomPopupMenu.Items.Count-1 do
    ZoomPopupMenu.Items[i].Checked:=ZoomPopupMenu.Items[i].Tag=ScrZoom;
end;

procedure TfrmMain.ActOpenSaveExecute(Sender: TObject);
begin
  OpenPopupMenu.Popup(Left+ToolButtonOpen.Left, Top+pbDraw.Top+46);
  Update;
  Application.ProcessMessages;
end;

function GetOrdosFileList(list:TStrings): integer;
var ii: integer;
    ss: string;
  function IsFLeter(ch: byte):boolean;
  begin
    IsFLeter:=(ch>=32)and(ch<127);
  end;
begin
  Result:=0;
  if IsFLeter(RAMArr[1, 1]) and IsFLeter(RAMArr[1, 2]) and IsFLeter(RAMArr[1, 3]) then
    repeat
      ss:='';
      for ii:=0 to 7 do
        if IsFLeter(RAMArr[1, Result+ii]) then ss:=ss+chr(RAMArr[1, Result+ii]);
      if trim(ss)<>'' then list.AddObject(ss, pointer(Result));
      Result:=Result + 16 + PWord(@RAMArr[1, Result+10])^;
    until (Result>RAMDISK_TOP)or(not IsFLeter(RAMArr[1, Result]));
end;

procedure TfrmMain.ItemLoadClick(Sender: TObject);
var param_n, next_ordos, fsize, datasize, ii: integer;
    FL: TStringList;
    ss: string;
    ft: TOFileType;
    FStream: TFileStream;
begin
  ii:=0;
  FL:=TStringList.Create;
  next_ordos:=GetOrdosFileList(FL);
  OpenDialog.Title:='Select ORDOS file(s) to load (ctrl+mouse)';
  OpenDialog.Options:=OpenDialog.Options + [ofAllowMultiSelect];
  OpenDialog.DefaultExt:=ORD_EXT;
  OpenDialog.Filter:=ORD_FILTER;
  OpenDialog.FilterIndex:=1;
  try
    if (next_ordos>=0) and (next_ordos<RAMDISK_TOP-16) and
       OpenDialog.Execute and
       (Application.MessageBox('Data in second RAM page (RAM-DISK B) will be repaced'#13#10'with data contained in loaded file(s).   OK to continue?',
                               'Warning: Confirm replace', MB_OKCANCEL+MB_ICONEXCLAMATION)=mrOk)
    then
    begin
      with OpenDialog.Files do for param_n:=0 to Count-1 do
      begin
        ss:=CheckFileExists(Strings[param_n]);
        ft:=DetectFileType(ss, fsize);
        case ft of
          ftRko, ftBru, ftOrd:                              // ordos_header[10..11] - ORDOS filesize
            if (next_ordos+fsize<RAMDISK_TOP) then
            begin
              FStream:=nil;
              try
                FStream:=TFileStream.Create(ss, fmOpenRead or fmShareDenyWrite);
                if (ft=ftRko) then
                  FStream.Seek(77, soFromBeginning);                                // skip RKO header
                if FStream.Read(PByte(@RAMArr[1, next_ordos])^, 16)=16 then         // read ordos header
                begin
                  datasize:=PWord(@RAMArr[1, next_ordos+10])^;
                  if (datasize<RAMDISK_TOP-next_ordos-16) then
                  begin
                    FStream.Read(PByte(@RAMArr[1, next_ordos+16])^, datasize);
                    next_ordos:=next_ordos + datasize + 16;                         // 16 = ordos_header size
                    inc(ii);
                  end;
                end;
              finally
                RAMArr[1, next_ordos]:=$FF;   // end of ordos files chain
                if Assigned(FStream) then
                  FStream.Free;
              end;
            end;
        end;
      end;
      Application.MessageBox(PChar(Format('%d files loaded. Please, restart ORDOS VC$ commander (press "F4").', [ii])),
                             'Information', MB_OK+MB_ICONINFORMATION);
    end;
  finally
    FL.Free;
    OpenDialog.Options:=OpenDialog.Options - [ofAllowMultiSelect];
  end;
  Update;
  Application.ProcessMessages;
end;

procedure TfrmMain.ItemSaveClick(Sender: TObject);
var ii: integer;
    FStream: TFileStream;
    ss: string;
begin
  FrmSave:=TFrmSave.Create(Application);
  with FrmSave do
  try
    if GetOrdosFileList(lbOrdFiles.Items)>0 then
    begin
      for ii:=0 to lbOrdFiles.Items.Count-1 do
        lbOrdFiles.Items[ii]:=lbOrdFiles.Items[ii]+
                              format('   (%d bytes)',
                                     [integer(PWord(@RAMArr[1, integer(pointer(lbOrdFiles.Items.Objects[ii]))+10])^)]);
      if (ShowModal=mrOk)and(lbOrdFiles.SelCount>0) then
      begin
        SaveDialog.Title:='Select catalog for ORDOS file(s)';
        SaveDialog.DefaultExt:=ORD_EXT;
        SaveDialog.Filter:='ORDOS file (*.ord)|*.ord|Any file (*.*)|*.*';
        SaveDialog.FilterIndex:=1;
        ii:=0;
        while not lbOrdFiles.Selected[ii] do inc(ii);
        ss:=lbOrdFiles.Items[ii];
        SaveDialog.FileName:=ChangeFileExt(LeftSubstr(ss), '.'+ORD_EXT);
        if SaveDialog.Execute then
          for ii:=0 to lbOrdFiles.Items.Count-1 do
            if lbOrdFiles.Selected[ii] then
            try
              FStream:=nil;
              if (lbOrdFiles.SelCount=1) then
                FStream:=TFileStream.Create(ChangeFileExt(SaveDialog.FileName, '.'+ORD_EXT), fmCreate)
              else
              begin
                ss:=lbOrdFiles.Items[ii];
                FStream:=TFileStream.Create(ChangeFileExt(LeftSubstr(ss), '.'+ORD_EXT), fmCreate);
              end;
              FStream.Write(PByte(@RAMArr[1, integer(pointer(lbOrdFiles.Items.Objects[ii])) ])^,
                            integer(PWord(@RAMArr[1, integer(pointer(lbOrdFiles.Items.Objects[ii]))+10])^)+16);
            finally
              if Assigned(FStream) then FStream.Free;
            end;
      end;
    end
    else
      Application.MessageBox('No files in RAM-DISK B.', 'Information', MB_OK+MB_ICONINFORMATION);
  finally
    Free;
    FrmSave:=nil;
  end;
end;

procedure TfrmMain.Savescreenpicture1Click(Sender: TObject);
var FS: TFileStream;
    TmpBitmap: TBitmap;
begin
  SaveDialog.DefaultExt:='bmp';
  SaveDialog.Title:='Specify file to save screenshot';
  if not SaveDialog.Execute then exit;
  TmpBitmap:=TBitmap.Create;
  TmpBitmap.Assign(ScrBitmap);
  TmpBitmap.PixelFormat:=pf8bit;
  FS:=TFileStream.Create(SaveDialog.Filename, fmCreate);
  try
    TmpBitmap.SaveToStream(FS);
  finally
    if Assigned(FS) then FS.Free;
    if Assigned(TmpBitmap) then TmpBitmap.Free;
  end;
end;

procedure TfrmMain.DebuggerMenuPopup(Sender: TObject);
begin
  ItemPause.Enabled:=not CPUPaused;
  ItemWrPause.Enabled:=CPUPaused and (SGPortDump.Focused or SGPort1Dump.Focused or SGPort2Dump.Focused);
  ItemModiPause.Enabled:=CPUPaused and (SGPortDump.Focused or SGPort1Dump.Focused or SGPort2Dump.Focused);
  ItemModify.Enabled:=CPUPaused;
  ItemSetCondition.Enabled:=CPUPaused and (not SGPortDump.Focused) and
                            (not SGPort1Dump.Focused) and (not SGPort2Dump.Focused);
  ItemClearCondition.Enabled:=CPUPaused and (not SGPortDump.Focused) and
                            (not SGPort1Dump.Focused) and (not SGPort2Dump.Focused);
end;

procedure TfrmMain.ActDbgStepIntoExecute(Sender: TObject);
begin
  if CpuPaused then
    CpuResume
  else
  begin
    CPUPaused:=True;
    CPUSuspend;
  end;
end;

procedure TfrmMain.ActDbgStepOverExecute(Sender: TObject);
begin
  if CpuPaused then
  begin
    if (peekb(RegPC) in [$C4, $CC, $CD, $D4, $DC, $E4, $EC, $F4, $FC]) then        // if CALL
    begin
      BreakPointRetPF9:=MainPort[$F9];
      BreakPointRetAddr:=RegPC+3;
      CPUPaused:=False;
    end
    else if (peekb(RegPC)=$ED)and((peekb(RegPC+1) in [$B0..$B3, $B8..$BB])) then   // LDIR&etc
    begin
      BreakPointRetPF9:=MainPort[$F9];
      BreakPointRetAddr:=RegPC+2;
      CPUPaused:=False;
    end;
    CpuResume;
  end
  else
  begin
    CPUPaused:=True;
    CPUSuspend;
  end;
end;

procedure TfrmMain.ItemModifyClick(Sender: TObject);
var st: string;
    ii, kk: integer;
begin
  if SGRegMain.Focused then
  begin
    st:=strictMask16;
    case SGRegMain.Row of
       0: setAF(GetValue16('AF', getAF, st, kk));
       1: setBC(GetValue16('BC', getBC, st, kk));
       2: regDE:=GetValue16('DE', regDE, st, kk);
       3: regHL:=GetValue16('HL', regHL, st, kk);
       4: regSP:=GetValue16('SP', regSP, st, kk);
       5: regPC:=GetValue16('PC', regPC, st, kk);
    end;
    ShowSGReg;
  end
  else if SGRegAlter.Focused then
  begin
    st:=strictMask16;
    case SGRegAlter.Row of
       0: regAF_:=GetValue16('AF''', regAF_, st, kk);
       1: regBC_:=GetValue16('BC''', regBC_, st, kk);
       2: regDE_:=GetValue16('DE''', regDE_, st, kk);
       3: regHL_:=GetValue16('HL''', regHL_, st, kk);
       4: regIX:=GetValue16('IX', regIX, st, kk);
       5: regIY:=GetValue16('IY', regIY, st, kk);
       6: begin
            ii:=GetValue16('IR', getIR, st, kk);
            intI := (ii and $FFFF) shr 8;
            intR := ii and $FF;
          end;
    end;
    ShowSGReg;
  end
  else if SGPortDump.Focused then
  begin
    st:=strictMask8;
    case SGPortDump.Row of
       0: outb($F8, GetValue16('port F8', MainPort[$F8], st, kk));
       1: outb($F9, GetValue16('port F9', MainPort[$F9], st, kk));
       2: outb($FA, GetValue16('port FA', MainPort[$FA], st, kk));
       3: outb($FB, GetValue16('port FB', MainPort[$FB], st, kk));
       4: outb($FC, GetValue16('port FC', MainPort[$FC], st, kk));
       5: outb($FE, GetValue16('port FE', MainPort[$FE], st, kk));
       6: outb($FF, GetValue16('port FF', MainPort[$FF], st, kk));
    end;
    ShowSGPort;
  end
  else if SGPort1Dump.Focused then
  begin
    st:=strictMask8;
    case SGPort1Dump.Row of
       0: outb($00, GetValue16('port 00', MainPort[$00], st, kk));
       1: outb($01, GetValue16('port 01', MainPort[$01], st, kk));
       2: outb($02, GetValue16('port 02', MainPort[$02], st, kk));
       3: outb($03, GetValue16('port 03', MainPort[$03], st, kk));
       4: outb($04, GetValue16('port 04', MainPort[$04], st, kk));
       5: outb($05, GetValue16('port 05', MainPort[$05], st, kk));
       6: outb($06, GetValue16('port 06', MainPort[$06], st, kk));
    end;
    ShowSGPort;
  end
  else if SGPort2Dump.Focused then
  begin
    st:=strictMask8;
    case SGPort2Dump.Row of
       0: outb($07, GetValue16('port 07', MainPort[$07], st, kk));
       1: outb($08, GetValue16('port 08', MainPort[$08], st, kk));
       2: outb($09, GetValue16('port 09', MainPort[$09], st, kk));
       3: outb($0A, GetValue16('port 0A', MainPort[$0A], st, kk));
       4: outb($0B, GetValue16('port 0B', MainPort[$0B], st, kk));
       5: outb($0C, GetValue16('port 0C', MainPort[$0C], st, kk));
       6: outb($0D, GetValue16('port 0D', MainPort[$0D], st, kk));
    end;
    ShowSGPort;
  end;
end;

procedure TfrmMain.ItemSetConditionClick(Sender: TObject);
var ii: integer;
    st: string;
begin
  st:=freeMask16;
  if SGRegMain.Focused then
    case SGRegMain.Row of
       0: begin
            ii:=GetValue16('AF condition', ConditionAFvalue, st, ConditionAFmask);
            if st<>'' then begin
              ConditionAF:=' ='+st;
              ConditionAFvalue:=ii;
            end;
          end;
       1: begin
            ii:=GetValue16('BC condition', ConditionBCvalue, st, ConditionBCmask);
            if st<>'' then begin
              ConditionBC:=' ='+st;
              ConditionBCvalue:=ii;
            end;
          end;
       2: begin
            ii:=GetValue16('DE condition', ConditionDEvalue, st, ConditionDEmask);
            if st<>'' then begin
              ConditionDE:=' ='+st;
              ConditionDEvalue:=ii;
            end;
          end;
       3: begin
            ii:=GetValue16('HL condition', ConditionHLvalue, st, ConditionHLmask);
            if st<>'' then begin
              ConditionHL:=' ='+st;
              ConditionHLvalue:=ii;
            end;
          end;
       4: begin
            ii:=GetValue16('SP condition', ConditionSPvalue, st, ConditionSPmask);
            if st<>'' then begin
              ConditionSP:=' ='+st;
              ConditionSPvalue:=ii;
            end;
          end;
    end
  else if SGRegAlter.Focused then
    case SGRegAlter.Row of
       0: begin
            ii:=GetValue16('AF'' condition', ConditionAF_value, st, ConditionAF_mask);
            if st<>'' then begin
              ConditionAF_:=' ='+st;
              ConditionAF_value:=ii;
            end;
          end;
       1: begin
            ii:=GetValue16('BC'' condition', ConditionBC_value, st, ConditionBC_mask);
            if st<>'' then begin
              ConditionBC_:=' ='+st;
              ConditionBC_value:=ii;
            end;
          end;
       2: begin
            ii:=GetValue16('DE'' condition', ConditionDE_value, st, ConditionDE_mask);
            if st<>'' then begin
              ConditionDE_:=' ='+st;
              ConditionDE_value:=ii;
            end;
          end;
       3: begin
            ii:=GetValue16('HL'' condition', ConditionHL_value, st, ConditionHL_mask);
            if st<>'' then begin
              ConditionHL_:=' ='+st;
              ConditionHL_value:=ii;
            end;
          end;
       4: begin
            ii:=GetValue16('IX condition', ConditionIXvalue, st, ConditionIXmask);
            if st<>'' then begin
              ConditionIX:=' ='+st;
              ConditionIXvalue:=ii;
            end;
          end;
       5: begin
            ii:=GetValue16('IY condition', ConditionIYvalue, st, ConditionIYmask);
            if st<>'' then begin
              ConditionIY:=' ='+st;
              ConditionIYvalue:=ii;
            end;
          end;
       6: begin
            ii:=GetValue16('IR condition', ConditionIRvalue, st, ConditionIRmask);
            if st<>'' then begin
              ConditionIR:=' ='+st;
              ConditionIRvalue:=ii;
            end;
          end;
    end;
  InitSGRegPort;
end;

procedure TfrmMain.ItemClearConditionClick(Sender: TObject);
begin
  if SGRegMain.Focused then
    case SGRegMain.Row of
       0: ConditionAF:='';
       1: ConditionBC:='';
       2: ConditionDE:='';
       3: ConditionHL:='';
       4: ConditionSP:='';
    end
  else if SGRegAlter.Focused then
    case SGRegAlter.Row of
       0: ConditionAF_:='';
       1: ConditionBC_:='';
       2: ConditionDE_:='';
       3: ConditionHL_:='';
       4: ConditionIX:='';
       5: ConditionIY:='';
       6: ConditionIR:='';
    end;
  InitSGRegPort;
end;

procedure TfrmMain.cbConditionsClick(Sender: TObject);
begin
  AnalyzeConditions:=cbConditions.Checked;
end;

procedure TfrmMain.F9Click(Sender: TObject);
 procedure SetAddr(page:byte; addr:word);
 begin
    MEDumpAddr.Text:=padl(IntToHex(Page, 2), 2, '0')+':'+padl(IntToHex(Addr, 4), 4, '0');
 end;
begin
  case (Sender as TMenuItem).Tag of
    0: SetAddr(MainPort[$F9], regHL);           // (F9):(HL)
    1: SetAddr(MainPort[$F9], regDE);           // (F9):(DE)
    2: SetAddr(MainPort[$F9], getBC);           // (F9):(BC)
    3: SetAddr(MainPort[$F9], regIX);           // (F9):(IX)
    4: SetAddr(MainPort[$F9], regIY);           // (F9):(IY)
    5: SetAddr(MainPort[$F9], regSP);           // (F9):(SP)
    6: SetAddr(MainPort[$F9], regPC);           // (F9):(PC)
    7: SetAddr(MainPort[$F9], getIR and $FF00); // (F9):(Ix)
  end;
  ShowMemDump(True);
end;

procedure TfrmMain.btnPageAddressClick(Sender: TObject);
var pt: TPoint;
begin
  pt.x:=1;
  pt.y:=(Sender as TControl).Height+1;
  pt:=(Sender as TControl).ClientToScreen(pt);
  PageAddressMenu.Popup(pt.x, pt.y);
{Left+(Sender as TControl).Left+(Sender as TControl).Width,
                        Top+(Sender as TControl).Top+(Sender as TControl).Height}
end;

procedure TfrmMain.InitEthernet;
begin
  if EthMode>0 then begin
    CreateEthThread;
    if EthMAC[0]=#$FF then                              // not defined in INI-file at startup
      CopyMemory(@EthMAC[0], EthThread.MACAddr, 6)
    else
      EthThread.MACAddr:=@EthMAC[0];
    case EthMode of
      1: begin
           F8019AS:=T8019AS.Create;
           FNE2kDevice:=F8019AS;
         end
      else raise Exception.CreateFmt('Wrong EthMode: %d', [EthMode]);
    end;
    if Assigned(EthThread) then
    begin
      FNE2kDevice.EthThread:=EthThread;
      EthThread.TAPguid:=EthConnGUID;
    end;
  end
  else
    DestroyEthThread;
end;

procedure TfrmMain.MEDumpAddrChange(Sender: TObject);
begin
  ShowMemDump(True);
end;

procedure TfrmMain.ItemWrPauseClick(Sender: TObject);
  procedure TriggerPortPause(SG: TStringGrid);
  begin
    with SG do begin
      if Length(Cells[0,SG.Row])>3 then
        Cells[0,SG.Row]:=copy(Cells[0,SG.Row],1,2)
      else
        if TMenuItem(Sender).tag=1 then
          Cells[0,SG.Row]:=Cells[0,SG.Row]+'  @'
        else
          Cells[0,SG.Row]:=Cells[0,SG.Row]+'  #';
      Update;
    end;
  end;
var PortOffset:integer;
begin
  PortOffset:=-1;
  if SGPortDump.Focused then
  begin
    PortOffset:=$F8+SGPortDump.Row;
    TriggerPortPause(SGPortDump);
  end
  else if SGPort1Dump.Focused then
  begin
    PortOffset:=$00+SGPortDump.Row;
    TriggerPortPause(SGPort1Dump);
  end
  else if SGPort2Dump.Focused then
  begin
    PortOffset:=$07+SGPortDump.Row;
    TriggerPortPause(SGPort2Dump);
  end;
  if (PortOffset>=0) then begin
    if CheckPort[Lo(PortOffset)]<>0 then
      CheckPort[Lo(PortOffset)]:=0
    else
      CheckPort[Lo(PortOffset)]:=TMenuItem(Sender).tag;
  end;
end;

initialization
  FNE2kDevice:=nil;
  GUIDList:=TStringList.Create;

finalization
  GUIDList.Free;

end.



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


unit settswin;

interface


{$I 'OrionZEm.inc'}


uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  StdCtrls, ExtCtrls, ComCtrls, Mask, Buttons;

type
  TfrmSetts = class(TForm)
    PageControl1: TPageControl;
    tsCPU: TTabSheet;
    tsKeyboard: TTabSheet;
    Panel1: TPanel;
    BtnOk: TButton;
    BtnCancel: TButton;
    rgZ80Card: TRadioGroup;
    Label1: TLabel;
    meKeyDelay: TMaskEdit;
    rgKeyboardType: TRadioGroup;
    tsROM: TTabSheet;
    edtRomBios: TEdit;
    btnRomBios: TButton;
    btnRomDisk: TButton;
    edtRomDisk: TEdit;
    tsAY8912: TTabSheet;
    cbSoundEnabled: TCheckBox;
    Label2: TLabel;
    cbRusLat: TComboBox;
    TSFdd: TTabSheet;
    Label3: TLabel;
    meRecentLimit: TMaskEdit;
    cbRestoreODI: TCheckBox;
    cbAYEnabled: TCheckBox;
    cbHighDensity: TCheckBox;
    TSIde: TTabSheet;
    BtnHDDMaster: TButton;
    BtnHDDSlave: TButton;
    EdtHDDMaster: TEdit;
    EdtHDDSlave: TEdit;
    IdePort: TLabel;
    cbHDDPort: TComboBox;
    cbMasterRO: TCheckBox;
    cbSlaveRO: TCheckBox;
    cbKeyExtender: TCheckBox;
    tsF600: TTabSheet;
    Label4: TLabel;
    cbxF600plugin: TComboBox;
    btnF600pluginCfg: TButton;
    gbOrion128: TGroupBox;
    gbOrionPro: TGroupBox;
    BtnRom1BIOS: TButton;
    EdtRom1Bios: TEdit;
    BtnRom2BIOS: TButton;
    EdtRom2Bios: TEdit;
    cbSW0: TCheckBox;
    cbSW1: TCheckBox;
    cbSW2: TCheckBox;
    cbSW3: TCheckBox;
    cbSW4: TCheckBox;
    cbSW5: TCheckBox;
    cbSW6: TCheckBox;
    cbSW7: TCheckBox;
    Label5: TLabel;
    Label9: TLabel;
    Label10: TLabel;
    Label11: TLabel;
    Label12: TLabel;
    Label13: TLabel;
    Label6: TLabel;
    Label7: TLabel;
    Label8: TLabel;
    SpeedButton1: TSpeedButton;
    BtnSDCard: TButton;
    EdtSDcard: TEdit;
    cbSDcardRO: TCheckBox;
    Label14: TLabel;
    Label15: TLabel;
    Label16: TLabel;
    Bevel1: TBevel;
    tsRS232: TTabSheet;
    PortComboBox: TComboBox;
    Label17: TLabel;
    Label18: TLabel;
    BaudRateComboBox: TComboBox;
    Label19: TLabel;
    DataBitsComboBox: TComboBox;
    Label20: TLabel;
    ParityComboBox: TComboBox;
    Label21: TLabel;
    StopBitsComboBox: TComboBox;
    Label22: TLabel;
    HwFlowComboBox: TComboBox;
    Label23: TLabel;
    SwFlowComboBox: TComboBox;
    Label24: TLabel;
    DTRControlComboBox: TComboBox;
    cbUARTExists: TCheckBox;
    Label25: TLabel;
    TabSheet1: TTabSheet;
    rgEthChip: TRadioGroup;
    Label26: TLabel;
    Label27: TLabel;
    cbTAPConnection: TComboBox;
    lblMAC: TLabel;
    cbPFE: TCheckBox;
    rgRTC: TRadioGroup;
    gbSound: TGroupBox;
    gbCPUclk: TGroupBox;
    cbCPUclk: TComboBox;
    gbRAMsz: TGroupBox;
    cbMEMsz: TComboBox;
    cbSDscheme: TComboBox;
    lblScheme: TLabel;
    Label28: TLabel;
    Label29: TLabel;
    Label30: TLabel;
    Bevel2: TBevel;
    Label31: TLabel;
    BtnProMaster: TButton;
    BtnProSlave: TButton;
    EdtProSlave: TEdit;
    EdtProMaster: TEdit;
    cbProMasterRO: TCheckBox;
    cbProSlaveRO: TCheckBox;
    procedure FormActivate(Sender: TObject);
    procedure BtnOkClick(Sender: TObject);
    procedure btnRomBiosClick(Sender: TObject);
    procedure cbSoundEnabledClick(Sender: TObject);
    procedure BtnHDDMasterClick(Sender: TObject);
    procedure BtnHDDSlaveClick(Sender: TObject);
    procedure btnF600pluginCfgClick(Sender: TObject);
    procedure SpeedButton1Click(Sender: TObject);
    procedure BtnSDCardClick(Sender: TObject);
    procedure BtnProMasterClick(Sender: TObject);
    procedure BtnProSlaveClick(Sender: TObject);
  private
    procedure FreePluginList(PluginList: TStrings);
    procedure GetPluginList(DllDir: string; PluginList: TStrings);
  end;

var
  frmSetts: TfrmSetts = nil;

implementation

Uses modOrion, mod8255, modHDD, modSD, mod1793, modF600, mod232, CPDrv, mainwin, EthThrd, mod8019as;

{$R *.DFM}

procedure TfrmSetts.FormActivate(Sender: TObject);
var i, idx: integer;
    st: string;
begin
  OnActivate:=nil;
  cbCPUclk.ItemIndex:=CPUSpeedMode;
  cbMEMsz.ItemIndex:=MEMSizeMode;
  rgZ80Card.ItemIndex:=Z80CardMode;
  cbPFE.Checked:=PFEEnabled;
  cbSoundEnabled.Checked:=SoundEnabled;
  cbAYenabled.Checked:=AyEnabled;
  cbAYEnabled.Enabled:=cbSoundEnabled.Checked;
  rgRTC.ItemIndex:=RTCmode;
  meKeyDelay.Text:=IntToStr(KeyDelay);
  for i:=0 to MaxRusLat do
    cbRusLat.Items.AddObject(KeyRusLatArr[i].name, pointer(KeyRusLatArr[i].code));
  cbRusLat.ItemIndex:=cbRusLat.Items.IndexOfObject(pointer(KeyRusLat));
  if cbRusLat.ItemIndex=-1 then cbRusLat.ItemIndex:=0;
  edtRomBios.Text:=trim(ROMBIOSfile);
  edtRomDisk.Text:=trim(ROMDISKfile);
  edtRom1Bios.Text:=trim(ROM1BIOSfile);
  edtRom2Bios.Text:=trim(ROM2BIOSfile);
  meRecentLimit.Text:=IntToStr(FMaxRecent);
  cbRestoreODI.Checked:=FRestoreODI;
  cbHDDPort.ItemIndex:=HDDPort;
  with IdeController do begin
    EdtHDDMaster.Text:=ImageFile[HddDeviceMaster];
    EdtHDDSlave.Text:=ImageFile[HddDeviceSlave];
    cbMasterRO.Checked:=ImageRO[HddDeviceMaster];
    cbSlaveRO.Checked:=ImageRO[HddDeviceSlave];
  end;
  with IdeProController do begin
    EdtProMaster.Text:=ImageFile[HddDeviceMaster];
    EdtProSlave.Text:=ImageFile[HddDeviceSlave];
    cbProMasterRO.Checked:=ImageRO[HddDeviceMaster];
    cbProSlaveRO.Checked:=ImageRO[HddDeviceSlave];
  end;
  with SDController do begin
    EdtSDCard.Text:=ImageFile;
    cbSDcardRO.Checked:=ImageRO;
    SDScheme:=SDController.Scheme;
    cbSDScheme.ItemIndex:=SDScheme;
  end;
  rgKeyboardType.ItemIndex:=KeybType and 3;
  cbKeyExtender.Checked:=KeyExtender;
  cbHighDensity.Checked:=FddHd;
{Port F600}
  GetPluginList(ExtractFilePath(System.ParamStr(0)), cbxF600plugin.Items);
  i:=0;
  st:=PortF600.Plugin;
  idx:=PortF600.FuncIdx;                         // function index
  with cbxF600plugin.Items do
    while (((Objects[i] as TStringList).Strings[0]<>st) or
            (integer(pointer((Objects[i] as TStringList).Objects[0]))<>idx)) and
          (i<Count-1) do
      inc(i);
  if i<cbxF600plugin.Items.Count then
    cbxF600plugin.ItemIndex:=i;
  cbSW0.Checked:=OrionPRO_DIP_SW and 1 = 0;
  cbSW1.Checked:=OrionPRO_DIP_SW and 2 = 0;
  cbSW2.Checked:=OrionPRO_DIP_SW and 4 = 0;
  cbSW3.Checked:=OrionPRO_DIP_SW and 8 = 0;
  cbSW4.Checked:=OrionPRO_DIP_SW and 16 = 0;
  cbSW5.Checked:=OrionPRO_DIP_SW and 32 = 0;
  cbSW6.Checked:=OrionPRO_DIP_SW and 64 = 0;
  cbSW7.Checked:=OrionPRO_DIP_SW and 128 = 0;
{RS232}
  cbUARTExists.Checked:=FUART.Exists;
  try
    FUART.EnumComPorts(PortComboBox.Items);
  except
  end;
  PortComboBox.Text := FUART.PortName;
  BaudRateComboBox.Text := IntToStr( FUART.CPDrv.BaudRateValue );
  DataBitsComboBox.ItemIndex := ord( FUART.CPDrv.DataBits );
  ParityComboBox.ItemIndex := ord( FUART.CPDrv.Parity );
  StopBitsComboBox.ItemIndex := ord( FUART.CPDrv.StopBits );
// Flow Control
  HwFlowComboBox.ItemIndex := ord( FUART.CPDrv.HwFlow );
  SwFlowComboBox.ItemIndex := ord( FUART.CPDrv.SwFlow );
  DTRControlComboBox.ItemIndex := ord( not FUART.CPDrv.EnableDTROnOpen );
{Ethernet}
  rgEthChip.ItemIndex:=EthMode;
  GetConnectionInfo(cbTAPConnection.Items, GUIDList);
  cbTAPConnection.ItemIndex:=cbTAPConnection.Items.IndexOf(EthConnName);
  if Assigned(EthThread) then
      CopyMemory(@EthMAC[0], EthThread.MACAddr, sizeof(TMacAddr));
  lblMAC.Caption:=IntToHex(ord(EthMAC[0]),2)+'-'+
                  IntToHex(ord(EthMAC[1]),2)+'-'+
                  IntToHex(ord(EthMAC[2]),2)+'-'+
                  IntToHex(ord(EthMAC[3]),2)+'-'+
                  IntToHex(ord(EthMAC[4]),2)+'-'+
                  IntToHex(ord(EthMAC[5]),2);
{}
end;

procedure TfrmSetts.BtnOkClick(Sender: TObject);
var ResetFlag: boolean;
    st: string;
    ii,pg,adr: integer;
begin
  ResetFlag:=((rgZ80Card.ItemIndex>=Z80_ORIONPRO_v2) and (Z80CardMode<Z80_ORIONPRO_v2)) or
             ((rgZ80Card.ItemIndex<Z80_ORIONPRO_v2) and (Z80CardMode>=Z80_ORIONPRO_v2)) or
             (cbMEMsz.ItemIndex<MEMSizeMode);
  try
    frmSetts.Cursor:=crSQLWait;
    frmSetts.Enabled:=false;
    frmSetts.Update;
    CPUSpeedMode:=cbCPUclk.ItemIndex;
    if MEMSizeMode<>cbMEMsz.ItemIndex then
    begin
      MEMSizeMode:=cbMEMsz.ItemIndex;
      if not ResetFlag then begin
        ii:=LongAddressParse(FrmMain.MEDumpAddr.Text, pg, adr);
        FrmMain.MEDumpAddr.EditMask:=SetMemSize;
        ii:=pos(':',FrmMain.MEDumpAddr.EditMask)-1;
        FrmMain.MEDumpAddr.Text:=padl(IntToHex(pg, 2), ii, '0')+':'+padl(IntToHex(adr, 4), 4, '0');
      end;
    end;
    SDScheme:=cbSDScheme.ItemIndex;
    Z80CardMode:=rgZ80Card.ItemIndex;
    PFEEnabled:=cbPFE.Checked;
    SoundEnabled:=cbSoundEnabled.Checked;
    AyEnabled:=cbAYenabled.Checked;
    RTCmode:=rgRTC.ItemIndex;
    KeyDelay:=StrToIntDef(trim(meKeyDelay.Text), 0);
    KeyRusLat:=integer(pointer(cbRusLat.Items.Objects[cbRusLat.ItemIndex]));
    if trim(edtRomBios.Text)<>trim(ROMBIOSfile) then
    begin
      ROMBIOSfile:=trim(edtRomBios.Text);
      frmMain.SetROMBIOS(ROMBIOSfile);
      ResetFlag:=True;
    end;
    if trim(ROMDISKfile)<>trim(edtRomDisk.Text) then
    begin
      ROMDISKfile:=trim(edtRomDisk.Text);
      frmMain.SetROMDisk(ROMDiskfile);
    end;
    if trim(edtRom1Bios.Text)<>trim(ROM1BIOSfile) then
    begin
      ROM1BIOSfile:=trim(edtRom1Bios.Text);
      frmMain.SetROM1BIOS(ROM1BIOSfile);
      ResetFlag:=True;
    end;
    if trim(edtRom2Bios.Text)<>trim(ROM2BIOSfile) then
    begin
      ROM2BIOSfile:=trim(edtRom2Bios.Text);
      frmMain.SetROM2BIOS(ROM2BIOSfile);
    end;
    FMaxRecent:=StrToIntDef(trim(meRecentLimit.Text), 0);
    FRestoreODI:=cbRestoreODI.Checked;
    HDDPort:=cbHDDPort.ItemIndex;
    with IdeController do begin
      ImageRO[HddDeviceMaster]:=cbMasterRO.Checked;
      ImageRO[HddDeviceSlave]:=cbSlaveRO.Checked;
      ImageFile[HddDeviceMaster]:=EdtHDDMaster.Text;
      ImageFile[HddDeviceSlave]:=EdtHDDSlave.Text;
    end;
    with IdeProController do begin
      ImageRO[HddDeviceMaster]:=cbProMasterRO.Checked;
      ImageRO[HddDeviceSlave]:=cbProSlaveRO.Checked;
      ImageFile[HddDeviceMaster]:=EdtProMaster.Text;
      ImageFile[HddDeviceSlave]:=EdtProSlave.Text;
    end;
    with SDController do begin
      ImageRO:=cbSDcardRO.Checked;
      ImageFile:=EdtSDcard.Text;
      SDScheme:=cbSDScheme.ItemIndex;
      SDController.Scheme:=SDScheme;
    end;
    KeybType:=rgKeyboardType.ItemIndex;
    PortF400.KbdType:=TKbdType(KeybType and 3);
    KeyExtender:=cbKeyExtender.Checked;
    FddHd:=cbHighDensity.Checked;
    with cbxF600plugin.Items.Objects[cbxF600plugin.ItemIndex] as TStringList do
    begin
      PortF600.Plugin:=Strings[0];
      PortF600.FuncIdx:=integer(pointer(Objects[0]));
    end;
    OrionPRO_DIP_SW:=255;
    if cbSW0.Checked then OrionPRO_DIP_SW:=OrionPRO_DIP_SW - 1;
    if cbSW1.Checked then OrionPRO_DIP_SW:=OrionPRO_DIP_SW - 2;
    if cbSW2.Checked then OrionPRO_DIP_SW:=OrionPRO_DIP_SW - 4;
    if cbSW3.Checked then OrionPRO_DIP_SW:=OrionPRO_DIP_SW - 8;
    if cbSW4.Checked then OrionPRO_DIP_SW:=OrionPRO_DIP_SW - 16;
    if cbSW5.Checked then OrionPRO_DIP_SW:=OrionPRO_DIP_SW - 32;
    if cbSW6.Checked then OrionPRO_DIP_SW:=OrionPRO_DIP_SW - 64;
    if cbSW7.Checked then OrionPRO_DIP_SW:=OrionPRO_DIP_SW -128;
    if cbSW7.Checked<>(OrionPRO_DIP_SW and 128 = 0) then
      ResetFlag:=True;
    FreePluginList(cbxF600plugin.Items);
{RS232}
    if (FUART.PortName <> trim(PortComboBox.Text)) or
       (FUART.Exists <> cbUARTExists.Checked) then
    begin
      FUART.Exists:=cbUARTExists.Checked;
      FUART.PortName := trim(PortComboBox.Text);
      FUART.CPDrv.Connect;
    end;
{Ethernet}
    st:=trim(cbTAPConnection.Text);
    ResetFlag:=ResetFlag or (rgEthChip.ItemIndex<>EthMode) or
              ((trim(EthConnName)<>st) and (st<>''));
    ii:=cbTAPConnection.Items.IndexOf(st);
    if (rgEthChip.ItemIndex>0) and (cbTAPConnection.Items.Count>0) and (ii=-1) then
      raise Exception.CreateFmt('Wrong TAP connection name: `%s`', [st]);
    if st<>'' then
    begin
      EthConnName:=st;
      EthConnGUID:=GUIDList.Strings[ii];
    end;
    if (rgEthChip.ItemIndex<>EthMode) then
    begin
      case EthMode of
        1: begin
             (FNE2kDevice as T8019AS).Free;
             FNE2kDevice:=nil;
           end;
      end;
      EthMode:=rgEthChip.ItemIndex;
      frmMain.InitEthernet;
    end;
{}
  finally
    frmSetts.Cursor:=crDefault;
    frmSetts.Enabled:=true;
    frmSetts.Update;
    ModalResult:=MrOK;
    if ResetFlag then frmMain.ActResetExecute(self);
  end;  
end;

procedure TfrmSetts.btnRomBiosClick(Sender: TObject);
begin
  with frmMain.OpenDialog do
  begin
    Title:='Select binary file with F800 ROMBIOS';
    DefaultExt:='BIN';
    Filter:='Orion ROM files (*.bin;*.rom)|*.bin;*.rom|Any file (*.*)|*.*';
    FilterIndex:=1;
    if Execute then
      case (Sender as TButton).Tag of
        0: EdtRomBios.Text:=FileName;
        1: EdtRomDisk.Text:=FileName;
        2: EdtRom1Bios.Text:=FileName;
        3: EdtRom2Bios.Text:=FileName;
      end;  
  end;
end;

procedure TfrmSetts.cbSoundEnabledClick(Sender: TObject);
begin
  cbAYEnabled.Enabled:=cbSoundEnabled.Checked;
end;

procedure TfrmSetts.BtnHDDMasterClick(Sender: TObject);
begin
  with frmMain.OpenDialog do
  begin
    Title:='Select binary file with Master HDD image';
    DefaultExt:='OHI';
    Filter:='Orion OHI files (*.ohi;*.img)|*.ohi;*.img|Any file (*.*)|*.*';
    FilterIndex:=1;
    if Execute then
      EdtHDDMaster.Text:=FileName;
  end;
end;

procedure TfrmSetts.BtnHDDSlaveClick(Sender: TObject);
begin
  with frmMain.OpenDialog do
  begin
    Title:='Select binary file with Slave HDD image';
    DefaultExt:='OHI';
    Filter:='Orion OHI files (*.ohi;*.img)|*.ohi;*.img|Any file (*.*)|*.*';
    FilterIndex:=1;
    if Execute then
      EdtHDDSlave.Text:=FileName;
  end;
end;

procedure TfrmSetts.FreePluginList(PluginList: TStrings);
var i:integer;
begin
  for i:=0 to PluginList.Count-1 do
    if Assigned(PluginList.Objects[i]) then
      (PluginList.Objects[i] as TStringList).Free;
  PluginList.Clear;
end;

procedure TfrmSetts.GetPluginList(DllDir: string; PluginList: TStrings);
var file_rec:TSearchRec;
    dll: HMODULE;
    adr: TF600Function;
    PCh: pointer;
    ss, st1, st2: String;
    i, j: integer;
begin
  DllDir:=AddSlash(trim(DllDir));
  FreePluginList(PluginList);
  i:=PluginList.AddObject('', TObject(TStringList.Create));                              // FunctionTitle
  (PluginList.Objects[i] as TStringList).AddObject('', pointer(-1)); 
  if (FindFirst(DllDir+'*.DLL',$27,file_rec)=0) then
  repeat
    dll:=LoadLibrary(PChar(DllDir+file_rec.name));
    if dll<>0 then
    begin
      adr:=GetProcAddress(dll,PChar(F600FuncName));
      if Assigned(adr) then
      begin
        PCh:=nil;
        j:=adr(0, F600Func_Enumerate, PCh);
        if Assigned(PCh) and (j>0) then with PluginList do begin
          ss:=StrPas(PCh);
          repeat
            st1:=LeftSubstr(ss);                                                          // function Title
            st2:=LeftSubstr(ss);                                                          // function index
            if (st1<>'') and (st2<>'') then
            begin
              i:=AddObject(st1, TObject(TStringList.Create));                             // FunctionTitle
              (Objects[i] as TStringList).AddObject(AnsiUpperCase(DllDir+file_rec.name),  // PluginFile
                                                    pointer(StrToIntDef(st2, 0)));        // FunctionIndex
            end;
            dec(j);
          until (ss='') or (j=0);
        end;
      end;
      FreeLibrary(dll);
    end;
  until FindNext(file_rec)<>0;
  FindClose(file_rec);
end;
 
procedure TfrmSetts.btnF600pluginCfgClick(Sender: TObject);
var ss: string;
    ii: integer;
begin
  ss:=PortF600.Plugin;
  ii:=PortF600.FuncIdx;
  with cbxF600plugin.Items.Objects[cbxF600plugin.ItemIndex] as TStringList do
  begin
    PortF600.Plugin:=Strings[0];
    PortF600.FuncIdx:=integer(pointer(Objects[0]));
  end;
  PortF600.ConfigurePlugin;
  PortF600.Plugin:=ss;
  PortF600.FuncIdx:=ii;
end;

procedure TfrmSetts.SpeedButton1Click(Sender: TObject);
begin
  Application.MessageBox('После включения компьютера (или нажатия кнопки "Сброс") режимы'#13#10+
                         'работы назначаются с учетом состояния DIP-переключателей SW1-SW8:'#13#10#13#10+
                         '  1 - Наличие дисковода:'#13#10+
                         '         On - есть,    Off - нет;'#13#10+
                         '  2 - Наличие жесткого диска:'#13#10+
                         '         On - есть,    Off - нет;'#13#10+
                         '  3 - Тип клавиатуры:'#13#10+
                         '         On - MC7007 (основная клавиатура), Off - PK-86;'#13#10+
                         '  4 - Рабочая страница ОЗУ для внутренней CP/M-80 из ROM2:'#13#10+
                         '         On - 2,       Off - 1;'#13#10+
                         '  5 - Наличие контроллера символьного дисплея:'#13#10+
                         '         On - есть,    Off - нет;'#13#10+
                         '  6 - Запуск внутреннего Меню после сброса:'#13#10+
                         '         On - есть,    Off - нет;'#13#10+
                         '  7 - Тип загрузки операционной системы:'#13#10+
                         '         On - внутренняя (ПЗУ), Off - внешняя;'#13#10+
                         '  8 - Режим работы:'#13#10+
                         '         On - "Pro" (CP/M-80),   Off - "Orion-128" (ORDOS)',
                         'About Orion-PRO DIP-switcher', MB_OK+MB_ICONINFORMATION);
end;

procedure TfrmSetts.BtnSDCardClick(Sender: TObject);
begin
  with frmMain.OpenDialog do
  begin
    Title:='Select binary file with SD-card image';
    DefaultExt:='OHI';
    Filter:='Orion OHI files (*.ohi;*.img)|*.ohi;*.img|Any file (*.*)|*.*';
    FilterIndex:=1;
    if Execute then
      EdtSDcard.Text:=FileName;
  end;
end;

procedure TfrmSetts.BtnProMasterClick(Sender: TObject);
begin
  with frmMain.OpenDialog do
  begin
    Title:='Select binary file with Master HDD image';
    DefaultExt:='OHI';
    Filter:='Orion OHI files (*.ohi;*.img)|*.ohi;*.img|Any file (*.*)|*.*';
    FilterIndex:=1;
    if Execute then
      EdtProMaster.Text:=FileName;
  end;
end;

procedure TfrmSetts.BtnProSlaveClick(Sender: TObject);
begin
  with frmMain.OpenDialog do
  begin
    Title:='Select binary file with Slave HDD image';
    DefaultExt:='OHI';
    Filter:='Orion OHI files (*.ohi;*.img)|*.ohi;*.img|Any file (*.*)|*.*';
    FilterIndex:=1;
    if Execute then
      EdtProSlave.Text:=FileName;
  end;
end;

end.

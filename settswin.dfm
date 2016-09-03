object frmSetts: TfrmSetts
  Left = 236
  Top = 127
  BorderIcons = [biSystemMenu]
  BorderStyle = bsSingle
  Caption = 'Settings'
  ClientHeight = 364
  ClientWidth = 514
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'MS Sans Serif'
  Font.Style = []
  OldCreateOrder = False
  Position = poScreenCenter
  OnActivate = FormActivate
  PixelsPerInch = 96
  TextHeight = 13
  object Label28: TLabel
    Left = 8
    Top = 173
    Width = 137
    Height = 13
    Caption = 'HDD Device "0" (8255 PPA)'
  end
  object Label29: TLabel
    Left = 8
    Top = 239
    Width = 137
    Height = 13
    Caption = 'HDD Device "1" (8255 PPA)'
  end
  object PageControl1: TPageControl
    Left = 0
    Top = 0
    Width = 514
    Height = 328
    ActivePage = tsCPU
    Align = alClient
    TabOrder = 0
    object tsCPU: TTabSheet
      Caption = 'CPU, MEM'
      object rgZ80Card: TRadioGroup
        Left = 8
        Top = 75
        Width = 491
        Height = 220
        Caption = ' Platform emulation mode '
        ItemIndex = 0
        Items.Strings = (
          
            'Z80 Card Moscow (Radio 96/4) - no int50, no dispatcher, sound EI' +
            '/DI'
          'Z80 Card II - minimal port 0FBh  (int50 Hz)'
          
            'Z80 Card II - typical  port 0FBh  (int50 Hz, 16k dispatcher, Ful' +
            'l RAM)'
          
            'Z80 Card II - maximal port 0FBh (int50 Hz, 16k dispatcher, Full ' +
            'RAM, RAM protect)'
          'Orion-Pro - v2.9    (RAM f000..ffff at segment 3)'
          'Orion-Pro - v3.10  (RAM f000..ffff at segment 31)'
          'Orion-Pro - v3.20  (v3.10 + Z80 CardII type port 0FBh)')
        TabOrder = 2
      end
      object gbCPUclk: TGroupBox
        Left = 8
        Top = 6
        Width = 233
        Height = 61
        Caption = ' CPU clock (MHz) '
        TabOrder = 0
        object cbCPUclk: TComboBox
          Left = 12
          Top = 24
          Width = 208
          Height = 21
          Style = csDropDownList
          ItemHeight = 13
          TabOrder = 0
          Items.Strings = (
            '2.5'
            '3.5'
            '5.0'
            '7.0'
            '10.0'
            '20.0')
        end
      end
      object gbRAMsz: TGroupBox
        Left = 264
        Top = 6
        Width = 233
        Height = 61
        Caption = ' RAM size (kb) '
        TabOrder = 1
        object cbMEMsz: TComboBox
          Left = 12
          Top = 24
          Width = 208
          Height = 21
          Style = csDropDownList
          ItemHeight = 13
          TabOrder = 0
          Items.Strings = (
            '128'
            '256'
            '512'
            '1024'
            '2048'
            '4096')
        end
      end
    end
    object tsKeyboard: TTabSheet
      Caption = 'Keyboard'
      ImageIndex = 1
      object Label1: TLabel
        Left = 8
        Top = 11
        Width = 72
        Height = 13
        Caption = 'KeyPress delay'
      end
      object Label2: TLabel
        Left = 220
        Top = 11
        Width = 91
        Height = 13
        Caption = 'Рус/Lat switch key'
      end
      object meKeyDelay: TMaskEdit
        Left = 96
        Top = 8
        Width = 33
        Height = 21
        Hint = 'Value in range 0..99'
        EditMask = '!99;1; '
        MaxLength = 2
        ParentShowHint = False
        ShowHint = True
        TabOrder = 0
        Text = '  '
      end
      object rgKeyboardType: TRadioGroup
        Left = 8
        Top = 71
        Width = 489
        Height = 96
        Caption = ' Keyboard type (depend on ROM BIOS type) '
        ItemIndex = 0
        Items.Strings = (
          'Radio RK-86              (standard)'
          'MS7007 over RK-86  (Sp-Computer_club=Leningrad scheme)'
          'MS7007 non RK-86   (Orion-Soft=Moscow scheme)')
        TabOrder = 3
      end
      object cbRusLat: TComboBox
        Left = 328
        Top = 8
        Width = 83
        Height = 21
        Style = csDropDownList
        ItemHeight = 0
        TabOrder = 1
      end
      object cbKeyExtender: TCheckBox
        Left = 8
        Top = 40
        Width = 401
        Height = 17
        Caption = 
          'Use key extender (extended ctrl+key combination for Pascal-alike' +
          ' text editors)'
        TabOrder = 2
      end
    end
    object tsROM: TTabSheet
      Caption = 'ROM'
      ImageIndex = 2
      object gbOrion128: TGroupBox
        Left = 2
        Top = 6
        Width = 499
        Height = 123
        Caption = ' Orion-128'
        TabOrder = 4
        object cbPFE: TCheckBox
          Left = 9
          Top = 93
          Width = 481
          Height = 17
          Caption = 
            'Emulate ROM-disk 64к-pages switched by 0FEh port bits D0..D3 (al' +
            'lows ROM-disk size over 64k)'
          TabOrder = 0
        end
      end
      object edtRomBios: TEdit
        Left = 149
        Top = 34
        Width = 344
        Height = 24
        TabOrder = 1
      end
      object btnRomBios: TButton
        Left = 11
        Top = 32
        Width = 129
        Height = 25
        Caption = 'F800 ROM-BIOS file'
        TabOrder = 0
        OnClick = btnRomBiosClick
      end
      object btnRomDisk: TButton
        Tag = 1
        Left = 11
        Top = 64
        Width = 129
        Height = 25
        Caption = '8255 ROM-disk file'
        TabOrder = 2
        OnClick = btnRomBiosClick
      end
      object edtRomDisk: TEdit
        Left = 148
        Top = 66
        Width = 344
        Height = 24
        TabOrder = 3
      end
      object gbOrionPro: TGroupBox
        Left = 2
        Top = 136
        Width = 499
        Height = 157
        Caption = ' Orion-Pro'
        TabOrder = 9
        TabStop = True
        object Label5: TLabel
          Tag = 3
          Left = 38
          Top = 94
          Width = 82
          Height = 13
          Caption = 'DIP - Switches ->'
        end
        object Label9: TLabel
          Left = 200
          Top = 110
          Width = 30
          Height = 13
          Caption = 'page2'
        end
        object Label10: TLabel
          Left = 221
          Top = 124
          Width = 20
          Height = 13
          Caption = 'term'
        end
        object Label11: TLabel
          Left = 239
          Top = 137
          Width = 26
          Height = 13
          Caption = 'menu'
        end
        object Label12: TLabel
          Left = 256
          Top = 110
          Width = 32
          Height = 13
          Caption = 'romOS'
        end
        object Label13: TLabel
          Left = 277
          Top = 124
          Width = 49
          Height = 13
          Caption = 'modePRO'
        end
        object Label6: TLabel
          Left = 144
          Top = 109
          Width = 15
          Height = 13
          Caption = 'fdd'
        end
        object Label7: TLabel
          Left = 165
          Top = 123
          Width = 18
          Height = 13
          Caption = 'hdd'
        end
        object Label8: TLabel
          Left = 183
          Top = 137
          Width = 36
          Height = 13
          Caption = 'kb7007'
        end
        object SpeedButton1: TSpeedButton
          Left = 336
          Top = 120
          Width = 23
          Height = 22
          Glyph.Data = {
            36040000424D3604000000000000360000002800000010000000100000000100
            2000000000000004000000000000000000000000000000000000FF00FF00FF00
            FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00
            FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00
            FF00FF00FF00FF00FF00FF00FF00FFFFFF007F7F7F000000FF007F7F7F00FFFF
            FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00
            FF00FF00FF0000FFFF00FFFFFF0000FFFF000000FF000000FF000000FF0000FF
            FF00FFFFFF0000FFFF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00
            FF0000FFFF00FFFFFF0000FFFF00FFFFFF007F7F7F000000FF007F7F7F00FFFF
            FF0000FFFF00FFFFFF0000FFFF00FF00FF00FF00FF00FF00FF00FF00FF0000FF
            FF00FFFFFF0000FFFF00FFFFFF0000FFFF00FFFFFF0000FFFF00FFFFFF0000FF
            FF00FFFFFF0000FFFF00FFFFFF0000FFFF00FF00FF00FF00FF00FF00FF00FFFF
            FF0000FFFF00FFFFFF0000FFFF00FFFFFF0000FFFF000000FF0000FFFF00FFFF
            FF0000FFFF00FFFFFF0000FFFF00FFFFFF00FF00FF00FF00FF00FFFFFF0000FF
            FF00FFFFFF0000FFFF00FFFFFF0000FFFF00FFFFFF000000FF007F7F7F0000FF
            FF00FFFFFF0000FFFF00FFFFFF0000FFFF00FFFFFF00FF00FF0000FFFF00FFFF
            FF0000FFFF00FFFFFF0000FFFF00FFFFFF0000FFFF000000FF000000FF00FFFF
            FF0000FFFF00FFFFFF0000FFFF00FFFFFF0000FFFF00FF00FF00FFFFFF0000FF
            FF00FFFFFF0000FFFF00FFFFFF0000FFFF00FFFFFF0000FFFF000000FF000000
            FF00FFFFFF0000FFFF00FFFFFF0000FFFF00FFFFFF00FF00FF0000FFFF00FFFF
            FF0000FFFF00FFFFFF007F7F7F007F7F7F0000FFFF00FFFFFF007F7F7F000000
            FF000000FF00FFFFFF0000FFFF00FFFFFF0000FFFF00FF00FF00FFFFFF0000FF
            FF00FFFFFF0000FFFF000000FF000000FF00FFFFFF0000FFFF007F7F7F000000
            FF000000FF0000FFFF00FFFFFF0000FFFF00FFFFFF00FF00FF00FF00FF00FFFF
            FF0000FFFF00FFFFFF000000FF000000FF007F7F7F00FFFFFF007F7F7F000000
            FF000000FF00FFFFFF0000FFFF00FFFFFF00FF00FF00FF00FF00FF00FF0000FF
            FF00FFFFFF0000FFFF00FFFFFF000000FF000000FF000000FF000000FF000000
            FF00FFFFFF0000FFFF00FFFFFF0000FFFF00FF00FF00FF00FF00FF00FF00FF00
            FF0000FFFF00FFFFFF0000FFFF00FFFFFF000000FF000000FF000000FF00FFFF
            FF0000FFFF00FFFFFF0000FFFF00FF00FF00FF00FF00FF00FF00FF00FF00FF00
            FF00FF00FF0000FFFF00FFFFFF0000FFFF00FFFFFF0000FFFF00FFFFFF0000FF
            FF00FFFFFF0000FFFF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00
            FF00FF00FF00FF00FF00FF00FF00FFFFFF0000FFFF00FFFFFF0000FFFF00FFFF
            FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00}
          OnClick = SpeedButton1Click
        end
        object cbSW0: TCheckBox
          Left = 145
          Top = 92
          Width = 17
          Height = 17
          TabOrder = 0
        end
        object cbSW1: TCheckBox
          Left = 163
          Top = 92
          Width = 17
          Height = 17
          TabOrder = 1
        end
        object cbSW2: TCheckBox
          Left = 181
          Top = 92
          Width = 17
          Height = 17
          TabOrder = 2
        end
        object cbSW3: TCheckBox
          Left = 199
          Top = 92
          Width = 17
          Height = 17
          TabOrder = 3
        end
        object cbSW4: TCheckBox
          Left = 218
          Top = 92
          Width = 17
          Height = 17
          TabOrder = 4
        end
        object cbSW5: TCheckBox
          Left = 237
          Top = 92
          Width = 17
          Height = 17
          TabOrder = 5
        end
        object cbSW6: TCheckBox
          Left = 255
          Top = 92
          Width = 17
          Height = 17
          TabOrder = 6
        end
        object cbSW7: TCheckBox
          Left = 273
          Top = 92
          Width = 17
          Height = 17
          TabOrder = 7
        end
      end
      object BtnRom1BIOS: TButton
        Tag = 2
        Left = 14
        Top = 162
        Width = 129
        Height = 25
        Caption = 'Rom1-BIOS file'
        TabOrder = 5
        OnClick = btnRomBiosClick
      end
      object EdtRom1Bios: TEdit
        Left = 152
        Top = 164
        Width = 342
        Height = 24
        Hint = 'Initial BIOS and base drivers'
        TabOrder = 6
      end
      object BtnRom2BIOS: TButton
        Tag = 3
        Left = 14
        Top = 194
        Width = 129
        Height = 25
        Caption = 'Rom2-BIOS file'
        TabOrder = 7
        OnClick = btnRomBiosClick
      end
      object EdtRom2Bios: TEdit
        Left = 151
        Top = 196
        Width = 342
        Height = 24
        Hint = 'Extended BIOS and drivers, OS, user programms'
        TabOrder = 8
      end
    end
    object tsAY8912: TTabSheet
      Caption = 'Sound, RTC'
      ImageIndex = 3
      object gbSound: TGroupBox
        Left = 8
        Top = 8
        Width = 489
        Height = 81
        Caption = ' Sound '
        TabOrder = 3
      end
      object cbSoundEnabled: TCheckBox
        Left = 16
        Top = 32
        Width = 369
        Height = 17
        Hint = 'Enable all sounds'
        Caption = 'Sound Enabled'
        ParentShowHint = False
        ShowHint = True
        TabOrder = 0
        OnClick = cbSoundEnabledClick
      end
      object cbAYEnabled: TCheckBox
        Left = 16
        Top = 56
        Width = 369
        Height = 17
        Hint = 'Enable AY-8912 sound'
        Caption = 'AY-8912 Enabled'
        ParentShowHint = False
        ShowHint = True
        TabOrder = 1
      end
      object rgRTC: TRadioGroup
        Left = 8
        Top = 112
        Width = 489
        Height = 129
        Caption = ' RTC '
        ItemIndex = 0
        Items.Strings = (
          'none'
          '512 VI1 / MC146818  (port F760h..F761h)'
          '512 VI1 / MC146818  (port 50h..51h - Orion-Pro)'
          'DS1307     (port F402h)')
        TabOrder = 2
      end
    end
    object TSFdd: TTabSheet
      Caption = 'FDD'
      ImageIndex = 4
      object Label3: TLabel
        Left = 8
        Top = 12
        Width = 76
        Height = 13
        Caption = 'Recent files limit'
      end
      object meRecentLimit: TMaskEdit
        Left = 109
        Top = 8
        Width = 33
        Height = 21
        Hint = 'Value in range 0..99'
        EditMask = '!99;1; '
        MaxLength = 2
        ParentShowHint = False
        ShowHint = True
        TabOrder = 0
        Text = '  '
      end
      object cbRestoreODI: TCheckBox
        Left = 8
        Top = 56
        Width = 409
        Height = 17
        Caption = 'Restore current ODI files in virtual FD-drives at next startup'
        TabOrder = 1
      end
      object cbHighDensity: TCheckBox
        Left = 8
        Top = 80
        Width = 401
        Height = 17
        Hint = 
          'Emulate High Density floppy formats (6,7,8,9,10  1k-sectors per ' +
          'track)'
        Caption = 'Emulate High Density floppy formats'
        ParentShowHint = False
        ShowHint = True
        TabOrder = 2
      end
    end
    object TSIde: TTabSheet
      Caption = 'IDE, SD'
      ImageIndex = 6
      object IdePort: TLabel
        Left = 274
        Top = 42
        Width = 40
        Height = 13
        Caption = 'IDE Port'
      end
      object Label14: TLabel
        Left = 8
        Top = 34
        Width = 137
        Height = 13
        Caption = 'HDD Device "0" (8255 PPA)'
      end
      object Label15: TLabel
        Left = 8
        Top = 98
        Width = 137
        Height = 13
        Caption = 'HDD Device "1" (8255 PPA)'
      end
      object Label16: TLabel
        Left = 48
        Top = 274
        Width = 80
        Height = 13
        Caption = 'HDD Device "4"'
      end
      object Bevel1: TBevel
        Left = 0
        Top = 236
        Width = 504
        Height = 3
      end
      object lblScheme: TLabel
        Left = 274
        Top = 279
        Width = 39
        Height = 13
        Caption = 'Scheme'
      end
      object Label30: TLabel
        Left = 8
        Top = 161
        Width = 158
        Height = 13
        Caption = 'HDD Device "2" (IDE-RTC PRO)'
      end
      object Bevel2: TBevel
        Left = 0
        Top = 121
        Width = 504
        Height = 3
      end
      object Label31: TLabel
        Left = 8
        Top = 211
        Width = 158
        Height = 13
        Caption = 'HDD Device "3" (IDE-RTC PRO)'
      end
      object BtnHDDMaster: TButton
        Left = 8
        Top = 5
        Width = 153
        Height = 25
        Caption = 'IDE Master image file'
        TabOrder = 0
        OnClick = BtnHDDMasterClick
      end
      object BtnHDDSlave: TButton
        Left = 8
        Top = 69
        Width = 153
        Height = 25
        Caption = 'IDE Slave image file'
        TabOrder = 3
        OnClick = BtnHDDSlaveClick
      end
      object EdtHDDMaster: TEdit
        Left = 168
        Top = 7
        Width = 331
        Height = 21
        TabOrder = 1
      end
      object EdtHDDSlave: TEdit
        Left = 168
        Top = 71
        Width = 330
        Height = 21
        TabOrder = 4
      end
      object cbHDDPort: TComboBox
        Left = 328
        Top = 39
        Width = 171
        Height = 21
        Style = csDropDownList
        ItemHeight = 13
        TabOrder = 6
        Items.Strings = (
          'none (no IDE if selected)'
          'F500 (no RomDisk if selected)'
          'F600 (no Printer if selected)')
      end
      object cbMasterRO: TCheckBox
        Left = 168
        Top = 32
        Width = 97
        Height = 17
        Caption = 'ReadOnly'
        TabOrder = 2
      end
      object cbSlaveRO: TCheckBox
        Left = 168
        Top = 96
        Width = 97
        Height = 17
        Caption = 'ReadOnly'
        TabOrder = 5
      end
      object BtnSDCard: TButton
        Left = 8
        Top = 245
        Width = 153
        Height = 26
        Caption = 'SD-card Device image file'
        TabOrder = 7
        OnClick = BtnSDCardClick
      end
      object EdtSDcard: TEdit
        Left = 167
        Top = 247
        Width = 331
        Height = 21
        TabOrder = 8
      end
      object cbSDcardRO: TCheckBox
        Left = 168
        Top = 274
        Width = 97
        Height = 18
        Caption = 'ReadOnly'
        TabOrder = 9
      end
      object cbSDscheme: TComboBox
        Left = 321
        Top = 274
        Width = 177
        Height = 21
        ItemHeight = 13
        TabOrder = 10
        Items.Strings = (
          'N8VEM  Juha SD (port F762)'
          'MSX  SD-MMC V1 (F762,F763)')
      end
      object BtnProMaster: TButton
        Left = 9
        Top = 133
        Width = 153
        Height = 25
        Caption = 'IDE Master image file'
        TabOrder = 11
        OnClick = BtnProMasterClick
      end
      object BtnProSlave: TButton
        Left = 9
        Top = 182
        Width = 153
        Height = 25
        Caption = 'IDE Slave image file'
        TabOrder = 12
        OnClick = BtnProSlaveClick
      end
      object EdtProSlave: TEdit
        Left = 169
        Top = 184
        Width = 330
        Height = 21
        TabOrder = 13
      end
      object EdtProMaster: TEdit
        Left = 169
        Top = 135
        Width = 331
        Height = 21
        TabOrder = 14
      end
      object cbProMasterRO: TCheckBox
        Left = 169
        Top = 159
        Width = 97
        Height = 17
        Caption = 'ReadOnly'
        TabOrder = 15
      end
      object cbProSlaveRO: TCheckBox
        Left = 169
        Top = 209
        Width = 97
        Height = 17
        Caption = 'ReadOnly'
        TabOrder = 16
      end
    end
    object tsF600: TTabSheet
      Caption = 'Port F600'
      ImageIndex = 6
      object Label4: TLabel
        Left = 8
        Top = 11
        Width = 128
        Height = 13
        Caption = 'Port F600 emulation plugin:'
      end
      object cbxF600plugin: TComboBox
        Left = 176
        Top = 8
        Width = 321
        Height = 21
        Style = csDropDownList
        ItemHeight = 0
        TabOrder = 0
      end
      object btnF600pluginCfg: TButton
        Left = 176
        Top = 40
        Width = 321
        Height = 25
        Caption = 'Configure selected plugin'
        TabOrder = 1
        OnClick = btnF600pluginCfgClick
      end
    end
    object tsRS232: TTabSheet
      Caption = 'RS-232'
      ImageIndex = 7
      object Label17: TLabel
        Left = 8
        Top = 64
        Width = 51
        Height = 13
        Alignment = taRightJustify
        Caption = 'Port name:'
        FocusControl = PortComboBox
      end
      object Label18: TLabel
        Left = 10
        Top = 132
        Width = 49
        Height = 13
        Alignment = taRightJustify
        Caption = 'Baud rate:'
        FocusControl = BaudRateComboBox
      end
      object Label19: TLabel
        Left = 14
        Top = 160
        Width = 45
        Height = 13
        Alignment = taRightJustify
        Caption = 'Data bits:'
        FocusControl = DataBitsComboBox
      end
      object Label20: TLabel
        Left = 30
        Top = 188
        Width = 29
        Height = 13
        Alignment = taRightJustify
        Caption = 'Parity:'
        FocusControl = ParityComboBox
      end
      object Label21: TLabel
        Left = 15
        Top = 216
        Width = 44
        Height = 13
        Alignment = taRightJustify
        Caption = 'Stop bits:'
        FocusControl = StopBitsComboBox
      end
      object Label22: TLabel
        Left = 262
        Top = 132
        Width = 71
        Height = 13
        Alignment = taRightJustify
        Caption = 'Hardware flow:'
        FocusControl = HwFlowComboBox
      end
      object Label23: TLabel
        Left = 266
        Top = 160
        Width = 67
        Height = 13
        Alignment = taRightJustify
        Caption = 'Software flow:'
        FocusControl = SwFlowComboBox
      end
      object Label24: TLabel
        Left = 272
        Top = 188
        Width = 61
        Height = 13
        Alignment = taRightJustify
        Caption = 'DTR control:'
        FocusControl = DTRControlComboBox
      end
      object Label25: TLabel
        Left = 32
        Top = 104
        Width = 217
        Height = 13
        Caption = 'UART properties (programmed via F765 port) :'
      end
      object PortComboBox: TComboBox
        Left = 64
        Top = 60
        Width = 114
        Height = 21
        ItemHeight = 13
        TabOrder = 1
        Items.Strings = (
          '\\.\COM1'
          '\\.\COM2'
          '\\.\COM3'
          '\\.\COM4'
          '\\.\COM5'
          '\\.\COM6'
          '\\.\COM7'
          '\\.\COM8'
          '\\.\COM9'
          '\\.\COM10'
          '\\.\COM11'
          '\\.\COM12'
          '\\.\COM13'
          '\\.\COM14'
          '\\.\COM15'
          '\\.\COM16')
      end
      object BaudRateComboBox: TComboBox
        Left = 64
        Top = 128
        Width = 114
        Height = 21
        Enabled = False
        ItemHeight = 13
        TabOrder = 2
        Items.Strings = (
          '110'
          '300'
          '600'
          '1200'
          '2400'
          '4800'
          '9600'
          '14400'
          '19200'
          '38400'
          '56000'
          '57600'
          '115200'
          '128000'
          '230400'
          '256000'
          '460800'
          '921600')
      end
      object DataBitsComboBox: TComboBox
        Left = 64
        Top = 156
        Width = 114
        Height = 21
        Style = csDropDownList
        Enabled = False
        ItemHeight = 13
        TabOrder = 3
        Items.Strings = (
          '5'
          '6'
          '7'
          '8')
      end
      object ParityComboBox: TComboBox
        Left = 64
        Top = 184
        Width = 114
        Height = 21
        Style = csDropDownList
        Enabled = False
        ItemHeight = 13
        TabOrder = 4
        Items.Strings = (
          'None'
          'Odd'
          'Even'
          'Mark'
          'Space')
      end
      object StopBitsComboBox: TComboBox
        Left = 64
        Top = 212
        Width = 114
        Height = 21
        Style = csDropDownList
        Enabled = False
        ItemHeight = 13
        TabOrder = 5
        Items.Strings = (
          '1'
          '1.5'
          '2')
      end
      object HwFlowComboBox: TComboBox
        Left = 338
        Top = 128
        Width = 114
        Height = 21
        Style = csDropDownList
        Enabled = False
        ItemHeight = 13
        TabOrder = 6
        Items.Strings = (
          'None'
          'None but RTS on'
          'RTS/CTS')
      end
      object SwFlowComboBox: TComboBox
        Left = 338
        Top = 156
        Width = 114
        Height = 21
        Style = csDropDownList
        Enabled = False
        ItemHeight = 13
        TabOrder = 7
        Items.Strings = (
          'None'
          'XON/XOFF')
      end
      object DTRControlComboBox: TComboBox
        Left = 338
        Top = 184
        Width = 114
        Height = 21
        Style = csDropDownList
        Enabled = False
        ItemHeight = 13
        TabOrder = 8
        Items.Strings = (
          'Standard'
          'Keep off')
      end
      object cbUARTExists: TCheckBox
        Left = 16
        Top = 16
        Width = 377
        Height = 17
        Caption = 'UART Exists (emulate RS-232 on ports F764,F765)'
        TabOrder = 0
      end
    end
    object TabSheet1: TTabSheet
      Caption = 'Ethernet'
      ImageIndex = 8
      object Label26: TLabel
        Left = 16
        Top = 16
        Width = 98
        Height = 16
        Caption = 'MAC address:'
        Font.Charset = DEFAULT_CHARSET
        Font.Color = clWindowText
        Font.Height = -15
        Font.Name = 'MS Sans Serif'
        Font.Style = [fsBold]
        ParentFont = False
      end
      object Label27: TLabel
        Left = 16
        Top = 48
        Width = 114
        Height = 16
        Caption = 'TAP connection:'
        Font.Charset = DEFAULT_CHARSET
        Font.Color = clWindowText
        Font.Height = -15
        Font.Name = 'MS Sans Serif'
        Font.Style = [fsBold]
        ParentFont = False
      end
      object lblMAC: TLabel
        Left = 147
        Top = 16
        Width = 5
        Height = 16
        Font.Charset = DEFAULT_CHARSET
        Font.Color = clWindowText
        Font.Height = -15
        Font.Name = 'MS Sans Serif'
        Font.Style = [fsBold]
        ParentFont = False
      end
      object rgEthChip: TRadioGroup
        Left = 13
        Top = 89
        Width = 482
        Height = 78
        Caption = ' Ethernet chip '
        Font.Charset = DEFAULT_CHARSET
        Font.Color = clWindowText
        Font.Height = -15
        Font.Name = 'MS Sans Serif'
        Font.Style = []
        ItemIndex = 0
        Items.Strings = (
          'none'
          
            'RTL8019AS (reset: 0FD->F770, registers: F770..F77F, data: F780..' +
            'F7FF)')
        ParentFont = False
        TabOrder = 1
      end
      object cbTAPConnection: TComboBox
        Left = 146
        Top = 46
        Width = 346
        Height = 24
        Hint = 'Select or type TAP connection name'
        Font.Charset = DEFAULT_CHARSET
        Font.Color = clWindowText
        Font.Height = -15
        Font.Name = 'MS Sans Serif'
        Font.Style = []
        ItemHeight = 0
        ParentFont = False
        ParentShowHint = False
        ShowHint = True
        TabOrder = 0
      end
    end
  end
  object Panel1: TPanel
    Left = 0
    Top = 328
    Width = 514
    Height = 36
    Align = alBottom
    TabOrder = 1
    object BtnOk: TButton
      Left = 61
      Top = 5
      Width = 97
      Height = 25
      Caption = 'OK'
      Default = True
      TabOrder = 0
      OnClick = BtnOkClick
    end
    object BtnCancel: TButton
      Left = 358
      Top = 5
      Width = 93
      Height = 25
      Cancel = True
      Caption = 'Cancel'
      ModalResult = 2
      TabOrder = 1
    end
  end
end

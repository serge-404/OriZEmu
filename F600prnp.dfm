object frmF600prnp: TfrmF600prnp
  Left = 331
  Top = 293
  BorderIcons = [biSystemMenu]
  BorderStyle = bsSingle
  Caption = 'Configure port F600 printer'
  ClientHeight = 338
  ClientWidth = 495
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
  object Label1: TLabel
    Left = 80
    Top = 123
    Width = 65
    Height = 13
    Caption = 'BORDERS ->'
  end
  object Label2: TLabel
    Left = 344
    Top = 123
    Width = 75
    Height = 13
    Caption = '(for laser printer)'
  end
  object btnFont: TButton
    Left = 16
    Top = 12
    Width = 161
    Height = 25
    Caption = 'Printer Font'
    TabOrder = 0
    OnClick = btnFontClick
  end
  object edtFontName: TEdit
    Left = 192
    Top = 14
    Width = 225
    Height = 21
    TabOrder = 1
  end
  object btnOK: TButton
    Left = 48
    Top = 305
    Width = 145
    Height = 25
    Caption = 'OK'
    TabOrder = 10
    OnClick = btnOKClick
  end
  object btnCancel: TButton
    Left = 280
    Top = 305
    Width = 145
    Height = 25
    Cancel = True
    Caption = 'Cancel'
    ModalResult = 2
    TabOrder = 11
  end
  object rgMode: TRadioGroup
    Left = 8
    Top = 221
    Width = 481
    Height = 73
    Caption = ' Centronix printer connection '
    Items.Strings = (
      'B0..B7 - data,  C0 - strobe, C7 - ready'
      'A0..A7 - data,  C7 - strobe, C3 - ready ')
    TabOrder = 9
  end
  object rgPrinter: TRadioGroup
    Left = 8
    Top = 51
    Width = 481
    Height = 50
    Caption = ' Printer '
    Columns = 2
    Items.Strings = (
      'Windows default printer'
      'select printer before printing')
    TabOrder = 3
  end
  object meFontSize: TMaskEdit
    Left = 440
    Top = 14
    Width = 34
    Height = 21
    EditMask = '!99;1; '
    MaxLength = 2
    TabOrder = 2
    Text = '  '
  end
  object rgCodePage: TRadioGroup
    Left = 8
    Top = 158
    Width = 481
    Height = 52
    Caption = ' source text CodePage '
    Columns = 2
    Items.Strings = (
      'cp866 (don`t decode)'
      'koi8-r')
    TabOrder = 8
  end
  object meBorderTop: TMaskEdit
    Left = 232
    Top = 110
    Width = 34
    Height = 19
    EditMask = '!99;1; '
    MaxLength = 2
    TabOrder = 4
    Text = '  '
  end
  object meBorderBottom: TMaskEdit
    Left = 232
    Top = 134
    Width = 34
    Height = 19
    EditMask = '!99;1; '
    MaxLength = 2
    TabOrder = 7
    Text = '  '
  end
  object meBorderLeft: TMaskEdit
    Left = 176
    Top = 121
    Width = 34
    Height = 19
    EditMask = '!99;1; '
    MaxLength = 2
    TabOrder = 5
    Text = '  '
  end
  object meBorderRight: TMaskEdit
    Left = 288
    Top = 121
    Width = 34
    Height = 19
    EditMask = '!99;1; '
    MaxLength = 2
    TabOrder = 6
    Text = '  '
  end
  object FontDialog: TFontDialog
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clWindowText
    Font.Height = -11
    Font.Name = 'MS Sans Serif'
    Font.Style = []
    MinFontSize = 0
    MaxFontSize = 0
  end
end

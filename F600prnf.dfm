object frmF600prnf: TfrmF600prnf
  Left = 580
  Top = 296
  BorderIcons = [biSystemMenu]
  BorderStyle = bsSingle
  Caption = 'Configure port F600 printer - print to file'
  ClientHeight = 295
  ClientWidth = 515
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
  object edtFileName: TEdit
    Left = 208
    Top = 18
    Width = 293
    Height = 21
    TabOrder = 1
  end
  object btnFileName: TButton
    Left = 16
    Top = 18
    Width = 177
    Height = 25
    Caption = 'Output File name'
    TabOrder = 0
    OnClick = btnFileNameClick
  end
  object btnOk: TButton
    Left = 72
    Top = 261
    Width = 129
    Height = 26
    Caption = 'OK'
    TabOrder = 4
    OnClick = btnOkClick
  end
  object btnCancel: TButton
    Left = 320
    Top = 261
    Width = 129
    Height = 26
    Cancel = True
    Caption = 'Cancel'
    ModalResult = 2
    TabOrder = 5
  end
  object rgMode: TRadioGroup
    Left = 16
    Top = 173
    Width = 485
    Height = 74
    Caption = ' Centronix printer connection '
    Items.Strings = (
      'B0..B7 - data,  C0 - strobe, C7 - ready'
      'A0..A7 - data,  C7 - strobe, C3 - ready ')
    TabOrder = 3
  end
  object rgFNew: TRadioGroup
    Left = 16
    Top = 56
    Width = 485
    Height = 48
    Caption = ' File open mode '
    Columns = 2
    Items.Strings = (
      'Append file'
      'Overwrite file')
    TabOrder = 2
  end
  object rgCodePage: TRadioGroup
    Left = 16
    Top = 112
    Width = 485
    Height = 52
    Caption = ' source text CodePage '
    Columns = 2
    Items.Strings = (
      'cp866'
      'koi8-r')
    TabOrder = 6
  end
  object SaveDialog: TSaveDialog
    DefaultExt = 'prn'
    Filter = 
      'any file (*.*)|*.*|output files (*.prn)|*.prn|text files (*.txt)' +
      '|*.txt'
    Title = 'Select file for printer output'
  end
end

object frmNewVal: TfrmNewVal
  Left = 275
  Top = 196
  BorderIcons = [biSystemMenu]
  BorderStyle = bsSingle
  Caption = 'Enter new value for  '
  ClientHeight = 90
  ClientWidth = 313
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'MS Sans Serif'
  Font.Style = []
  OldCreateOrder = False
  Position = poScreenCenter
  PixelsPerInch = 96
  TextHeight = 13
  object btnOK: TButton
    Left = 31
    Top = 56
    Width = 75
    Height = 25
    Caption = 'OK'
    Default = True
    ModalResult = 1
    TabOrder = 1
  end
  object btnCancel: TButton
    Left = 208
    Top = 56
    Width = 75
    Height = 25
    Cancel = True
    Caption = 'Cancel'
    ModalResult = 2
    TabOrder = 2
  end
  object meValue: TMaskEdit
    Left = 128
    Top = 16
    Width = 49
    Height = 28
    EditMask = 'aaaa;1;_'
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clWindowText
    Font.Height = -16
    Font.Name = 'MS Sans Serif'
    Font.Style = []
    MaxLength = 4
    ParentFont = False
    TabOrder = 0
    Text = '    '
  end
end

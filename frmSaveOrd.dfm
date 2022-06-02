object FrmSave: TFrmSave
  Left = 529
  Top = 255
  Width = 393
  Height = 345
  BorderIcons = [biSystemMenu]
  Caption = 'Select ORDOS file(s) to save (use ctrl+mouse)'
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'MS Sans Serif'
  Font.Style = []
  OldCreateOrder = False
  Position = poScreenCenter
  PixelsPerInch = 96
  TextHeight = 13
  object Panel1: TPanel
    Left = 0
    Top = 0
    Width = 377
    Height = 257
    Caption = 'Panel1'
    Color = clSilver
    TabOrder = 0
    object lbOrdFiles: TListBox
      Left = 8
      Top = 8
      Width = 361
      Height = 233
      Anchors = [akLeft, akTop, akRight, akBottom]
      ItemHeight = 13
      MultiSelect = True
      TabOrder = 0
    end
  end
  object Panel2: TPanel
    Left = 0
    Top = 264
    Width = 377
    Height = 41
    Color = clScrollBar
    TabOrder = 1
    object BtnCancel: TButton
      Left = 277
      Top = 8
      Width = 82
      Height = 25
      Anchors = [akRight, akBottom]
      Caption = 'Cancel'
      ModalResult = 2
      TabOrder = 0
    end
    object BtnSave: TButton
      Left = 16
      Top = 8
      Width = 89
      Height = 25
      Anchors = [akLeft, akBottom]
      Caption = 'Save'
      ModalResult = 1
      TabOrder = 1
    end
  end
end

object FrmSave: TFrmSave
  Left = 529
  Top = 255
  Width = 342
  Height = 344
  BorderIcons = [biSystemMenu]
  Caption = 'Select ORDOS file(s) to save (use ctrl+mouse)'
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
  object lbOrdFiles: TListBox
    Left = 8
    Top = 10
    Width = 317
    Height = 259
    Anchors = [akLeft, akTop, akRight, akBottom]
    ItemHeight = 13
    MultiSelect = True
    TabOrder = 0
  end
  object BtnSave: TButton
    Left = 8
    Top = 282
    Width = 75
    Height = 25
    Anchors = [akLeft, akBottom]
    Caption = 'Save'
    ModalResult = 1
    TabOrder = 1
  end
  object BtnCancel: TButton
    Left = 250
    Top = 282
    Width = 75
    Height = 25
    Anchors = [akRight, akBottom]
    Caption = 'Cancel'
    ModalResult = 2
    TabOrder = 2
  end
end

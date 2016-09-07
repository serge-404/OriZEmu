object Form1: TForm1
  Left = 192
  Top = 124
  Width = 710
  Height = 305
  Caption = 'Save ORD,BRU files (max 8kb total) to ROM2 BIOS'
  Color = clBtnFace
  Constraints.MinHeight = 280
  Constraints.MinWidth = 690
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'MS Sans Serif'
  Font.Style = []
  OldCreateOrder = False
  PixelsPerInch = 96
  TextHeight = 13
  object btnMake: TButton
    Left = 16
    Top = 216
    Width = 225
    Height = 33
    Anchors = [akLeft, akBottom]
    Caption = 'Install ORD into ROM'
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clWindowText
    Font.Height = -13
    Font.Name = 'MS Sans Serif'
    Font.Style = [fsBold]
    ParentFont = False
    TabOrder = 1
    OnClick = btnMakeClick
  end
  object btnCancel: TButton
    Left = 540
    Top = 216
    Width = 137
    Height = 33
    Anchors = [akRight, akBottom]
    Caption = 'Cancel'
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clWindowText
    Font.Height = -13
    Font.Name = 'MS Sans Serif'
    Font.Style = [fsBold]
    ModalResult = 2
    ParentFont = False
    TabOrder = 2
    OnClick = btnCancelClick
  end
  object btnORDFiles: TButton
    Left = 16
    Top = 64
    Width = 129
    Height = 41
    Caption = 'Select ORD files'
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clWindowText
    Font.Height = -13
    Font.Name = 'MS Sans Serif'
    Font.Style = [fsBold]
    ParentFont = False
    TabOrder = 0
    OnClick = btnORDFilesClick
  end
  object lbORDFiles: TListBox
    Left = 160
    Top = 64
    Width = 517
    Height = 129
    Anchors = [akLeft, akTop, akRight, akBottom]
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clWindowText
    Font.Height = -16
    Font.Name = 'MS Sans Serif'
    Font.Style = [fsBold]
    ItemHeight = 20
    ParentFont = False
    TabOrder = 3
  end
  object edROM2File: TEdit
    Left = 160
    Top = 8
    Width = 517
    Height = 28
    Anchors = [akLeft, akTop, akRight]
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clWindowText
    Font.Height = -16
    Font.Name = 'MS Sans Serif'
    Font.Style = [fsBold]
    ParentFont = False
    TabOrder = 4
    Text = '.\ROM\ROM2-321.ROM '
  end
  object bntROM2File: TButton
    Left = 16
    Top = 4
    Width = 129
    Height = 41
    Caption = 'Select ROM2 file'
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clWindowText
    Font.Height = -13
    Font.Name = 'MS Sans Serif'
    Font.Style = [fsBold]
    ParentFont = False
    TabOrder = 5
    OnClick = bntROM2FileClick
  end
  object bbtnUP: TBitBtn
    Left = 16
    Top = 128
    Width = 49
    Height = 41
    Caption = 'UP'
    TabOrder = 6
    OnClick = bbtnUPClick
  end
  object bbtnDown: TBitBtn
    Left = 96
    Top = 128
    Width = 49
    Height = 41
    Caption = 'DOWN'
    TabOrder = 7
    OnClick = bbtnDownClick
  end
  object OpenROM2Dialog: TOpenDialog
    Filter = 'ROM|*.ROM|BIN|*.BIN'
    Options = [ofEnableSizing]
    Title = 'Select file with ROM2 BIOS code'
    Left = 360
    Top = 224
  end
  object OpenORDDialog: TOpenDialog
    Filter = 'ORD|*.ORD|BRU|*.BRU'
    Options = [ofAllowMultiSelect, ofEnableSizing]
    Title = 'Select ORD,BRU files (max 8kb total) to place into ROM2 BIOS'
    Left = 456
    Top = 224
  end
end

inherited TextForm: TTextForm
  Left = 393
  Top = 265
  Width = 278
  Height = 591
  Caption = 'TextForm'
  OldCreateOrder = True
  PixelsPerInch = 96
  TextHeight = 13
  object Label7: TLabel
    Left = 7
    Top = 20
    Width = 49
    Height = 13
    Caption = 'Alignment:'
  end
  object FontLabel: TLabel
    Left = 7
    Top = 78
    Width = 49
    Height = 13
    Cursor = crHandPoint
    Caption = 'Edit Font'
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clBlue
    Font.Height = -11
    Font.Name = 'Verdana'
    Font.Style = [fsUnderline]
    ParentFont = False
    OnClick = FontLabelClick
  end
  object Label6: TLabel
    Left = 7
    Top = 98
    Width = 24
    Height = 13
    Caption = 'Text:'
  end
  object Label5: TLabel
    Left = 15
    Top = 355
    Width = 144
    Height = 13
    Anchors = [akLeft, akBottom]
    Caption = '$property(component.property)'
    WordWrap = True
  end
  object Label1: TLabel
    Left = 8
    Top = 336
    Width = 53
    Height = 13
    Caption = 'E.g macros'
  end
  object Label2: TLabel
    Left = 15
    Top = 371
    Width = 168
    Height = 13
    Anchors = [akLeft, akBottom]
    Caption = '$decplaces(value, num dec places)'
    WordWrap = True
  end
  object Label3: TLabel
    Left = 15
    Top = 387
    Width = 38
    Height = 13
    Anchors = [akLeft, akBottom]
    Caption = '$today()'
    WordWrap = True
  end
  object Label4: TLabel
    Left = 15
    Top = 403
    Width = 105
    Height = 13
    Anchors = [akLeft, akBottom]
    Caption = '$dayofyeartodate(doy)'
    WordWrap = True
  end
  object Label8: TLabel
    Left = 15
    Top = 419
    Width = 106
    Height = 13
    Anchors = [akLeft, akBottom]
    Caption = '$formatshortdate(date)'
    WordWrap = True
  end
  object Label9: TLabel
    Left = 15
    Top = 435
    Width = 151
    Height = 13
    Anchors = [akLeft, akBottom]
    Caption = '$adddaystodate(date, numdays)'
    WordWrap = True
  end
  object AlignmentCombo: TComboBox
    Left = 64
    Top = 17
    Width = 193
    Height = 21
    BevelKind = bkSoft
    Anchors = [akLeft, akTop, akRight]
    ItemHeight = 13
    TabOrder = 0
    OnChange = AlignmentComboChange
    Items.Strings = (
      'Left'
      'Centre'
      'Right')
  end
  object AutosizeCheckBox: TCheckBox
    Left = 6
    Top = 51
    Width = 69
    Height = 17
    Alignment = taLeftJustify
    Caption = 'Autosize?'
    Ctl3D = True
    ParentCtl3D = False
    TabOrder = 2
    OnClick = ToolbarCheckBoxClick
  end
  object TextEdit: TRichEdit
    Left = 7
    Top = 111
    Width = 250
    Height = 220
    Anchors = [akLeft, akTop, akRight, akBottom]
    BevelInner = bvNone
    BevelOuter = bvRaised
    BevelKind = bkFlat
    BorderStyle = bsNone
    ScrollBars = ssBoth
    TabOrder = 1
    WordWrap = False
    OnExit = TextEditExit
  end
  object FontDialog: TFontDialog
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clWindowText
    Font.Height = -11
    Font.Name = 'MS Sans Serif'
    Font.Style = []
    MinFontSize = 0
    MaxFontSize = 0
    Left = 144
    Top = 248
  end
end

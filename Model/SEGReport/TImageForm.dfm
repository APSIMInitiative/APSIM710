inherited ImageForm: TImageForm
  Height = 587
  Caption = 'ImageForm'
  OldCreateOrder = True
  PixelsPerInch = 96
  TextHeight = 13
  object FilenameLabel: TLabel
    Left = 13
    Top = 26
    Width = 45
    Height = 13
    Caption = 'Filename:'
  end
  object ImageFileEdit: TAdvEditBtn
    Left = 64
    Top = 22
    Width = 217
    Height = 21
    AutoFocus = False
    EditAlign = eaLeft
    EditType = etString
    ErrorMarkerPos = 0
    ErrorMarkerLen = 0
    ErrorColor = clRed
    ErrorFontColor = clWhite
    ExcelStyleDecimalSeparator = False
    Flat = False
    FlatLineColor = clBlack
    FlatParentColor = True
    FocusAlign = eaDefault
    FocusBorder = False
    FocusColor = clWindow
    FocusFontColor = clWindowText
    FocusLabel = False
    FocusWidthInc = 0
    ModifiedColor = clHighlight
    DisabledColor = clSilver
    URLColor = clBlue
    ReturnIsTab = False
    LengthLimit = 0
    TabOnFullLength = False
    Precision = 0
    LabelPosition = lpLeftTop
    LabelMargin = 4
    LabelTransparent = False
    LabelAlwaysEnabled = False
    LabelFont.Charset = DEFAULT_CHARSET
    LabelFont.Color = clWindowText
    LabelFont.Height = -11
    LabelFont.Name = 'MS Sans Serif'
    LabelFont.Style = []
    Lookup.CaseSensitive = False
    Lookup.Color = clWindow
    Lookup.DisplayCount = 4
    Lookup.Enabled = False
    Lookup.NumChars = 2
    Persistence.Enable = False
    Persistence.Location = plInifile
    BevelKind = bkSoft
    Anchors = [akLeft, akTop, akRight]
    BorderStyle = bsNone
    Color = clWindow
    Enabled = True
    HintShowLargeText = False
    OleDropTarget = False
    OleDropSource = False
    Signed = False
    TabOrder = 0
    Transparent = False
    Visible = True
    OnExit = ImageFileEditExit
    ButtonWidth = 18
    Etched = False
    Glyph.Data = {
      86030000424D8603000000000000B6000000280000000F0000000C0000000100
      200000000000D002000000000000000000001000000000000000000000000000
      80000080000000808000800000008000800080800000C0C0C000808080000000
      FF0000FF000000FFFF00FF000000FF00FF00FFFF0000FFFFFF0093714AFFB4AE
      A3FFC5D0D6FFC5D0D6FFC5D0D6FFC5D0D6FFC5D0D6FFC5D0D6FFC5D0D6FFC5D0
      D6FFC5D0D6FFC4D0D5FFC3CFD5FFB6B7B1FF906E48FFCAC7BFFFDCDDBF000000
      000000000000000000000000000000000000000000000000000000000000DCDD
      BF00DCDDBF00DCDDBF00CDD7DBFFBDBDB6FFEBF0F0FF00000000000000008080
      800080808000808080008080800080808000808080008080800000000000DCDD
      BF00DCDDBF00E1E8EAFFD9E2E4FFECF1F1FF00000000FFFFFF00000000008080
      800080808000808080008080800080808000808080008080800000000000DCDD
      BF00E3EAEBFFDBE2E5FFEDF2F2FF00000000FFFFFF00FFFFFF00000000008080
      800080808000808080008080800080808000808080008080800000000000E4EB
      ECFFDCE3E6FFEEF2F2FF00000000FFFFFF00FFFFFF00FFFFFF00000000000000
      0000000000000000000000000000000000000000000000000000E5EBECFFDDE3
      E6FFF3F6F6FF00000000FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFF
      FF00FFFFFF0000000000DCDDBF00DCDDBF00DCDDBF00EAF0F1FFE1E8EBFFF4F6
      F6FF00000000FFFFFF00FFFFFF00FFFFFF000000000000000000000000000000
      000000000000DCDDBF00DCDDBF00DCDDBF00EBF0F1FFE2E8EBFFF7F8F8FFDCDD
      BF00000000000000000000000000DCDDBF00DCDDBF00DCDDBF00DCDDBF00DCDD
      BF00000000000000000000000000EEF1F3FFE6EAEDFFFAFAFAFFDCDDBF00DCDD
      BF00DCDDBF00DCDDBF00DCDDBF00DCDDBF00DCDDBF00DCDDBF00DCDDBF00DCDD
      BF000000000000000000F2F4F5FFEAEEF0FFDCCEBFFFDCDDBF00DCDDBF00DCDD
      BF00DCDDBF00DCDDBF0000000000DCDDBF00DCDDBF00DCDDBF0000000000DCDD
      BF0000000000FDFEFEFFDBCEBFFFA27D55FFDCDDBF00DCDDBF00DCDDBF00DCDD
      BF00DCDDBF00DCDDBF00000000000000000000000000DCDDBF00DCDDBF00DCDD
      BF00E8DFD5FFA27D55FF}
    OnClickBtn = ImageFileEditClickBtn
  end
  object LinkCheckBox: TCheckBox
    Left = 13
    Top = 54
    Width = 97
    Height = 17
    Alignment = taLeftJustify
    Caption = 'Save as link'
    TabOrder = 1
    OnClick = LinkCheckBoxClick
  end
  object AutoSizeCheckBox: TCheckBox
    Left = 13
    Top = 78
    Width = 97
    Height = 17
    Alignment = taLeftJustify
    Caption = 'Autosize?'
    TabOrder = 2
    OnClick = AutoSizeCheckBoxClick
  end
  object CentreCheckBox: TCheckBox
    Left = 13
    Top = 104
    Width = 97
    Height = 16
    Alignment = taLeftJustify
    Caption = 'Centre?'
    TabOrder = 4
    OnClick = CentreCheckBoxClick
  end
  object StretchCheckBox: TCheckBox
    Left = 13
    Top = 130
    Width = 97
    Height = 17
    Alignment = taLeftJustify
    Caption = 'Stretch?'
    TabOrder = 3
    OnClick = StretchCheckBoxClick
  end
  object OpenPictureDialog: TOpenPictureDialog
    Left = 88
    Top = 200
  end
end

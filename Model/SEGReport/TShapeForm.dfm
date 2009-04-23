inherited ShapeForm: TShapeForm
  Caption = 'ShapeForm'
  OldCreateOrder = True
  PixelsPerInch = 96
  TextHeight = 13
  object Label5: TLabel
    Left = 13
    Top = 28
    Width = 34
    Height = 13
    Caption = 'Shape:'
  end
  object Label3: TLabel
    Left = 13
    Top = 59
    Width = 54
    Height = 13
    Caption = 'Pen colour:'
  end
  object Label4: TLabel
    Left = 13
    Top = 91
    Width = 62
    Height = 13
    Caption = 'Brush colour:'
  end
  object ShapeCombo: TComboBox
    Left = 80
    Top = 24
    Width = 217
    Height = 21
    BevelKind = bkSoft
    Anchors = [akLeft, akTop, akRight]
    ItemHeight = 13
    TabOrder = 0
    Text = 'Rectangle'
    OnChange = ShapeComboChange
    Items.Strings = (
      'Rectangle'
      'Circle'
      'Vertical line'
      'Horizontal line'
      'Top and Bottom'
      'Right and Left')
  end
  object PenColourCombo: TColorBox
    Left = 80
    Top = 56
    Width = 217
    Height = 22
    Anchors = [akLeft, akTop, akRight]
    BevelKind = bkSoft
    ItemHeight = 16
    TabOrder = 2
    OnChange = PenColourComboChange
  end
  object BrushColourCombo: TColorBox
    Left = 80
    Top = 88
    Width = 217
    Height = 22
    Anchors = [akLeft, akTop, akRight]
    BevelKind = bkSoft
    ItemHeight = 16
    TabOrder = 1
    OnChange = BrushColourComboChange
  end
end

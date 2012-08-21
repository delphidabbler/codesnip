inherited HiliterPrefsFrame: THiliterPrefsFrame
  Width = 393
  Height = 326
  ExplicitWidth = 393
  ExplicitHeight = 326
  DesignSize = (
    393
    326)
  object lblNotice: TLabel
    Left = 0
    Top = 301
    Width = 393
    Height = 18
    Alignment = taCenter
    Anchors = [akLeft, akTop, akRight]
    AutoSize = False
    Caption = 
      'Changes will be reflected in the main display only when you rest' +
      'art CodeSnip.'
  end
  object gbElements: TGroupBox
    Left = 0
    Top = 95
    Width = 393
    Height = 196
    Anchors = [akLeft, akTop, akRight]
    Caption = ' Syntactic elements '
    TabOrder = 3
    object lblElements: TLabel
      Left = 8
      Top = 20
      Width = 47
      Height = 13
      Caption = '&Elements:'
      FocusControl = lbElements
    end
    object lblColour: TLabel
      Left = 270
      Top = 20
      Width = 35
      Height = 13
      Caption = '&Colour:'
    end
    object lblExample: TLabel
      Left = 159
      Top = 116
      Width = 44
      Height = 13
      Caption = 'Example:'
    end
    object lbElements: TListBox
      Left = 8
      Top = 37
      Width = 145
      Height = 148
      ItemHeight = 13
      TabOrder = 0
      OnClick = lbElementsClick
    end
    object gbFontStyle: TGroupBox
      Left = 159
      Top = 20
      Width = 105
      Height = 87
      Caption = ' Font style '
      TabOrder = 1
      object chkBold: TCheckBox
        Left = 8
        Top = 18
        Width = 89
        Height = 17
        Caption = '&Bold'
        TabOrder = 0
        OnClick = ChkFontStyleClick
      end
      object chkItalics: TCheckBox
        Left = 8
        Top = 40
        Width = 89
        Height = 17
        Caption = '&Italics'
        TabOrder = 1
        OnClick = ChkFontStyleClick
      end
      object chkUnderline: TCheckBox
        Left = 8
        Top = 62
        Width = 89
        Height = 17
        Caption = '&Underline'
        TabOrder = 2
        OnClick = ChkFontStyleClick
      end
    end
    inline frmExample: TRTFShowCaseFrame
      Left = 159
      Top = 133
      Width = 226
      Height = 51
      TabOrder = 2
      ExplicitLeft = 159
      ExplicitTop = 133
      ExplicitWidth = 226
      ExplicitHeight = 51
      inherited reView: TRichEdit
        Width = 226
        Height = 51
        ExplicitWidth = 226
        ExplicitHeight = 51
      end
    end
  end
  object gbDocFont: TGroupBox
    Left = 0
    Top = 0
    Width = 241
    Height = 89
    Caption = ' Highlighter font '
    TabOrder = 0
    object lblFontName: TLabel
      Left = 8
      Top = 23
      Width = 55
      Height = 13
      Caption = 'Font &name:'
      FocusControl = cbFontName
      Transparent = True
    end
    object lblFontSize: TLabel
      Left = 8
      Top = 52
      Width = 47
      Height = 13
      Caption = 'Font si&ze:'
      FocusControl = cbFontSize
    end
    object cbFontName: TComboBox
      Left = 80
      Top = 20
      Width = 153
      Height = 21
      Style = csDropDownList
      Sorted = True
      TabOrder = 0
      OnChange = cbFontNameChange
    end
    object cbFontSize: TComboBox
      Left = 80
      Top = 48
      Width = 57
      Height = 21
      TabOrder = 1
      OnChange = cbFontSizeChange
    end
  end
  object btnReset: TButton
    Left = 247
    Top = 46
    Width = 146
    Height = 25
    Anchors = [akTop, akRight]
    Caption = '&Restore Defaults'
    TabOrder = 2
    OnClick = btnResetClick
  end
  object btnStyle: TBitBtn
    Left = 247
    Top = 3
    Width = 146
    Height = 25
    Anchors = [akTop, akRight]
    Caption = '&Use Predefined Styles'
    DoubleBuffered = True
    Glyph.Data = {
      F6000000424DF600000000000000760000002800000010000000100000000100
      0400000000008000000000000000000000001000000010000000000000000000
      80000080000000808000800000008000800080800000C0C0C000808080000000
      FF0000FF000000FFFF00FF000000FF00FF00FFFF0000FFFFFF00FFFFFFFFFFFF
      FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF00FFF
      FFFFFFFFFF0000FFFFFFFFFFF000000FFFFFFFFF00000000FFFFFFF000000000
      0FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF
      FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF}
    Layout = blGlyphRight
    ParentDoubleBuffered = False
    TabOrder = 1
    OnClick = btnStyleClick
  end
  object mnuStyles: TPopupMenu
    Left = 304
    Top = 144
    object miClassic: TMenuItem
      Caption = 'CodeSnip Classic'
      OnClick = StyleMenuClick
    end
    object miDelphi7: TMenuItem
      Caption = 'Delphi 7'
      OnClick = StyleMenuClick
    end
    object miDelphi2006: TMenuItem
      Caption = 'Delphi 2006'
      OnClick = StyleMenuClick
    end
    object miVisualStudio: TMenuItem
      Caption = 'Visual Studio'#8482
      OnClick = StyleMenuClick
    end
    object miSpacer: TMenuItem
      Caption = '-'
    end
    object miNoHilite: TMenuItem
      Caption = 'No Highlghter'
      OnClick = StyleMenuClick
    end
  end
end

inherited PrintingPrefsFrame: TPrintingPrefsFrame
  Width = 396
  Height = 311
  ExplicitWidth = 396
  ExplicitHeight = 311
  DesignSize = (
    396
    311)
  object lblInfo: TLabel
    Left = 0
    Top = 264
    Width = 329
    Height = 13
    Alignment = taCenter
    Anchors = [akLeft, akTop, akRight]
    Caption = 
      'Your changes will take effect the next time you start the applic' +
      'ation.'
  end
  object gpOutputOptions: TGroupBox
    Left = 0
    Top = 2
    Width = 393
    Height = 153
    Anchors = [akLeft, akTop, akRight]
    Caption = ' Document Formatting Options '
    TabOrder = 0
    DesignSize = (
      393
      153)
    object chkSyntaxPrint: TCheckBox
      Left = 8
      Top = 20
      Width = 195
      Height = 17
      Caption = '&Syntax highlight source code'
      TabOrder = 0
      OnClick = CheckboxClick
    end
    object chkUseColor: TCheckBox
      Left = 8
      Top = 44
      Width = 195
      Height = 17
      Caption = 'Use &colour'
      TabOrder = 1
      OnClick = CheckboxClick
    end
    inline frmPreview: TRTFShowCaseFrame
      Left = 184
      Top = 15
      Width = 199
      Height = 127
      Anchors = [akTop, akRight]
      TabOrder = 2
      ExplicitLeft = 184
      ExplicitTop = 15
      ExplicitWidth = 199
      ExplicitHeight = 127
      inherited reView: TRichEdit
        Width = 199
        Height = 127
        ExplicitWidth = 199
        ExplicitHeight = 127
      end
    end
  end
  object gpMargins: TGroupBox
    Left = 0
    Top = 161
    Width = 217
    Height = 81
    Caption = 'gpMargins'
    TabOrder = 1
    object lblLeft: TLabel
      Left = 8
      Top = 24
      Width = 23
      Height = 13
      Caption = '&Left:'
      FocusControl = edLeft
    end
    object lblTop: TLabel
      Left = 8
      Top = 52
      Width = 22
      Height = 13
      Caption = '&Top:'
      FocusControl = edTop
    end
    object lblRight: TLabel
      Left = 112
      Top = 24
      Width = 29
      Height = 13
      Caption = '&Right:'
      FocusControl = edRight
    end
    object lblBottom: TLabel
      Left = 112
      Top = 52
      Width = 38
      Height = 13
      Caption = '&Bottom:'
      FocusControl = edBottom
    end
    object edLeft: TEdit
      Left = 48
      Top = 20
      Width = 41
      Height = 21
      TabOrder = 0
      OnKeyPress = NumEditKeyPress
    end
    object edTop: TEdit
      Left = 48
      Top = 48
      Width = 41
      Height = 21
      TabOrder = 2
      OnKeyPress = NumEditKeyPress
    end
    object edRight: TEdit
      Left = 162
      Top = 20
      Width = 41
      Height = 21
      TabOrder = 1
      OnKeyPress = NumEditKeyPress
    end
    object edBottom: TEdit
      Left = 162
      Top = 48
      Width = 41
      Height = 21
      TabOrder = 3
      OnKeyPress = NumEditKeyPress
    end
  end
end

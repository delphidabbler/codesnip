inherited PrintingPrefsFrame: TPrintingPrefsFrame
  Width = 396
  Height = 311
  ExplicitWidth = 396
  ExplicitHeight = 311
  object gpOutputOptions: TGroupBox
    Left = 0
    Top = 0
    Width = 393
    Height = 153
    Caption = ' Document Formatting Options '
    TabOrder = 0
    object chkSyntaxPrint: TCheckBox
      Left = 8
      Top = 20
      Width = 161
      Height = 17
      Caption = '&Syntax highlight source code'
      TabOrder = 0
      OnClick = CheckboxClick
    end
    object chkUseColor: TCheckBox
      Left = 8
      Top = 44
      Width = 97
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
      TabOrder = 2
      TabStop = True
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
    Width = 188
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
      Left = 96
      Top = 24
      Width = 29
      Height = 13
      Caption = '&Right:'
      FocusControl = edRight
    end
    object lblBottom: TLabel
      Left = 96
      Top = 52
      Width = 38
      Height = 13
      Caption = '&Bottom:'
      FocusControl = edBottom
    end
    object edLeft: TEdit
      Left = 40
      Top = 20
      Width = 41
      Height = 21
      TabOrder = 0
      OnKeyPress = NumEditKeyPress
    end
    object edTop: TEdit
      Left = 40
      Top = 48
      Width = 41
      Height = 21
      TabOrder = 2
      OnKeyPress = NumEditKeyPress
    end
    object edRight: TEdit
      Left = 138
      Top = 20
      Width = 41
      Height = 21
      TabOrder = 1
      OnKeyPress = NumEditKeyPress
    end
    object edBottom: TEdit
      Left = 138
      Top = 48
      Width = 41
      Height = 21
      TabOrder = 3
      OnKeyPress = NumEditKeyPress
    end
  end
  object stInfo: TStaticText
    Left = 0
    Top = 256
    Width = 393
    Height = 17
    Alignment = taCenter
    AutoSize = False
    Caption = 
      'Your changes will take effect the next time you start the applic' +
      'ation.'
    TabOrder = 2
  end
end

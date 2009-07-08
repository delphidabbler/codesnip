inherited SourcePrefsFrame: TSourcePrefsFrame
  Width = 393
  Height = 327
  ExplicitWidth = 393
  ExplicitHeight = 327
  object gbSourceCode: TGroupBox
    Left = 0
    Top = 0
    Width = 393
    Height = 163
    Caption = ' Source code formatting '
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clWindowText
    Font.Height = -11
    Font.Name = 'MS Sans Serif'
    Font.Style = []
    ParentFont = False
    TabOrder = 0
    object lblCommentStyle: TLabel
      Left = 8
      Top = 24
      Width = 85
      Height = 13
      Caption = '&Commenting style:'
      FocusControl = cbCommentStyle
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clWindowText
      Font.Height = -11
      Font.Name = 'MS Sans Serif'
      Font.Style = []
      ParentFont = False
    end
    object cbCommentStyle: TComboBox
      Left = 114
      Top = 20
      Width = 198
      Height = 21
      Style = csDropDownList
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clWindowText
      Font.Height = -11
      Font.Name = 'MS Sans Serif'
      Font.Style = []
      ItemHeight = 0
      ParentFont = False
      TabOrder = 0
      OnChange = cbCommentStyleChange
    end
    inline frmPreview: TRTFShowCaseFrame
      Left = 114
      Top = 46
      Width = 198
      Height = 107
      TabOrder = 1
      TabStop = True
      ExplicitLeft = 114
      ExplicitTop = 46
      ExplicitWidth = 198
      ExplicitHeight = 107
      inherited reView: TRichEdit
        Width = 198
        Height = 107
        ExplicitWidth = 198
        ExplicitHeight = 107
      end
    end
  end
  object gbFileFormat: TGroupBox
    Left = 0
    Top = 171
    Width = 393
    Height = 81
    Caption = ' File formatting '
    TabOrder = 1
    object lblSnippetFileType: TLabel
      Left = 8
      Top = 24
      Width = 76
      Height = 13
      Caption = '&Ouput file type:'
    end
    object chkSyntaxHighlighting: TCheckBox
      Left = 114
      Top = 47
      Width = 180
      Height = 21
      Caption = 'Enable &syntax highlighting'
      TabOrder = 1
    end
    object cbSnippetFileType: TComboBox
      Left = 114
      Top = 20
      Width = 81
      Height = 21
      Style = csDropDownList
      ItemHeight = 0
      TabOrder = 0
      OnChange = cbSnippetFileTypeChange
    end
  end
end

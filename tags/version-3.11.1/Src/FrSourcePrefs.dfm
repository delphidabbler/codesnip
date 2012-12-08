inherited SourcePrefsFrame: TSourcePrefsFrame
  Width = 393
  Height = 327
  ExplicitWidth = 393
  ExplicitHeight = 327
  DesignSize = (
    393
    327)
  object gbSourceCode: TGroupBox
    Left = 0
    Top = 0
    Width = 393
    Height = 163
    Anchors = [akLeft, akTop, akRight]
    Caption = ' Source code formatting '
    TabOrder = 0
    object lblCommentStyle: TLabel
      Left = 8
      Top = 24
      Width = 89
      Height = 13
      Caption = '&Commenting style:'
      FocusControl = cbCommentStyle
    end
    object cbCommentStyle: TComboBox
      Left = 122
      Top = 20
      Width = 198
      Height = 21
      Style = csDropDownList
      TabOrder = 0
      OnChange = cbCommentStyleChange
    end
    inline frmPreview: TRTFShowCaseFrame
      Left = 122
      Top = 46
      Width = 198
      Height = 107
      TabOrder = 1
      ExplicitLeft = 122
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
    Anchors = [akLeft, akTop, akRight]
    Caption = ' File formatting '
    TabOrder = 1
    object lblSnippetFileType: TLabel
      Left = 8
      Top = 24
      Width = 76
      Height = 13
      Caption = '&Ouput file type:'
      FocusControl = cbSnippetFileType
    end
    object chkSyntaxHighlighting: TCheckBox
      Left = 122
      Top = 47
      Width = 247
      Height = 21
      Caption = 'Enable &syntax highlighting'
      TabOrder = 1
    end
    object cbSnippetFileType: TComboBox
      Left = 122
      Top = 20
      Width = 81
      Height = 21
      Style = csDropDownList
      TabOrder = 0
      OnChange = cbSnippetFileTypeChange
    end
  end
end

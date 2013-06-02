inherited FindXRefsDlg: TFindXRefsDlg
  Left = 426
  Top = 222
  Caption = 'Find Cross References'
  ExplicitWidth = 474
  ExplicitHeight = 374
  PixelsPerInch = 96
  TextHeight = 13
  inherited pnlBody: TPanel
    Width = 305
    Height = 249
    ExplicitWidth = 305
    ExplicitHeight = 249
    object lblDesc: TLabel
      Left = 0
      Top = 0
      Width = 123
      Height = 13
      Caption = 'Find cross references for '
    end
    object lblSnippetName: TLabel
      Left = 119
      Top = 0
      Width = 73
      Height = 13
      Caption = 'lblSnippetName'
    end
    object lblOverwriteSearch: TLabel
      Left = 0
      Top = 238
      Width = 220
      Height = 13
      AutoSize = False
      Caption = 
        'NOTE: Running this cross reference search will override your exi' +
        'sting search(es).'
      WordWrap = True
    end
    object chkRequired: TCheckBox
      Left = 0
      Top = 28
      Width = 305
      Height = 17
      Caption = 'Search for &required snippets'
      TabOrder = 0
      OnClick = SearchCheckClick
    end
    object chkSeeAlso: TCheckBox
      Left = 0
      Top = 114
      Width = 305
      Height = 17
      Caption = 'Search for "&see also" cross references'
      TabOrder = 3
      OnClick = SearchCheckClick
    end
    object chkIncludeSnippet: TCheckBox
      Left = 0
      Top = 196
      Width = 600
      Height = 17
      Caption = '&Include "%s" in search'
      TabOrder = 6
    end
    object chkRequiredRecurse: TCheckBox
      Left = 24
      Top = 50
      Width = 281
      Height = 17
      Caption = 'Search re&cursively'
      TabOrder = 1
    end
    object chkSeeAlsoRecurse: TCheckBox
      Left = 24
      Top = 136
      Width = 281
      Height = 17
      Caption = 'Search rec&ursively'
      TabOrder = 4
    end
    object chkSeeAlsoReverse: TCheckBox
      Left = 0
      Top = 161
      Width = 305
      Height = 17
      Caption = 'Search for snippets that cross-reference this one'
      TabOrder = 5
      OnClick = SearchCheckClick
    end
    object chkRequiredReverse: TCheckBox
      Left = 0
      Top = 74
      Width = 305
      Height = 17
      Caption = 'Search for snippets that depend on this one'
      TabOrder = 2
      OnClick = SearchCheckClick
    end
  end
  inherited btnCancel: TButton
    Left = 231
    ExplicitLeft = 231
  end
  inherited btnOK: TButton
    OnClick = btnOKClick
  end
end

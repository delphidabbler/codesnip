inherited FindXRefsDlg: TFindXRefsDlg
  Caption = 'Find Cross References'
  ExplicitWidth = 979
  ExplicitHeight = 822
  PixelsPerInch = 168
  TextHeight = 30
  inherited pnlBody: TPanel
    object lblDesc: TLabel
      Left = 0
      Top = 0
      Width = 232
      Height = 30
      Margins.Left = 5
      Margins.Top = 5
      Margins.Right = 5
      Margins.Bottom = 5
      Caption = 'Find cross references for '
    end
    object lblSnippetName: TLabel
      Left = 239
      Top = 0
      Width = 148
      Height = 30
      Margins.Left = 5
      Margins.Top = 5
      Margins.Right = 5
      Margins.Bottom = 5
      Caption = 'lblSnippetName'
    end
    object lblOverwriteSearch: TLabel
      Left = 0
      Top = 417
      Width = 385
      Height = 22
      Margins.Left = 5
      Margins.Top = 5
      Margins.Right = 5
      Margins.Bottom = 5
      AutoSize = False
      Caption = 
        'NOTE: Running this cross reference search will override your exi' +
        'sting search(es).'
      WordWrap = True
    end
    object chkRequired: TCheckBox
      Left = 0
      Top = 49
      Width = 534
      Height = 30
      Margins.Left = 5
      Margins.Top = 5
      Margins.Right = 5
      Margins.Bottom = 5
      Caption = 'Search for &required snippets'
      TabOrder = 0
      OnClick = SearchCheckClick
    end
    object chkSeeAlso: TCheckBox
      Left = 0
      Top = 200
      Width = 534
      Height = 29
      Margins.Left = 5
      Margins.Top = 5
      Margins.Right = 5
      Margins.Bottom = 5
      Caption = 'Search for "&see also" cross references'
      TabOrder = 3
      OnClick = SearchCheckClick
    end
    object chkIncludeSnippet: TCheckBox
      Left = 0
      Top = 343
      Width = 1050
      Height = 30
      Margins.Left = 5
      Margins.Top = 5
      Margins.Right = 5
      Margins.Bottom = 5
      Caption = '&Include "%s" in search'
      TabOrder = 6
    end
    object chkRequiredRecurse: TCheckBox
      Left = 42
      Top = 88
      Width = 492
      Height = 29
      Margins.Left = 5
      Margins.Top = 5
      Margins.Right = 5
      Margins.Bottom = 5
      Caption = 'Search re&cursively'
      TabOrder = 1
    end
    object chkSeeAlsoRecurse: TCheckBox
      Left = 42
      Top = 238
      Width = 492
      Height = 30
      Margins.Left = 5
      Margins.Top = 5
      Margins.Right = 5
      Margins.Bottom = 5
      Caption = 'Search rec&ursively'
      TabOrder = 4
    end
    object chkSeeAlsoReverse: TCheckBox
      Left = 0
      Top = 282
      Width = 534
      Height = 30
      Margins.Left = 5
      Margins.Top = 5
      Margins.Right = 5
      Margins.Bottom = 5
      Caption = 'Search for snippets that cross-reference this one'
      TabOrder = 5
      OnClick = SearchCheckClick
    end
    object chkRequiredReverse: TCheckBox
      Left = 0
      Top = 130
      Width = 534
      Height = 29
      Margins.Left = 5
      Margins.Top = 5
      Margins.Right = 5
      Margins.Bottom = 5
      Caption = 'Search for snippets that depend on this one'
      TabOrder = 2
      OnClick = SearchCheckClick
    end
  end
  inherited btnOK: TButton
    OnClick = btnOKClick
  end
end

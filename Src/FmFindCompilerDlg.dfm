inherited FindCompilerDlg: TFindCompilerDlg
  Caption = 'Find Compiler(s)'
  ExplicitWidth = 979
  ExplicitHeight = 822
  PixelsPerInch = 168
  TextHeight = 30
  inherited pnlBody: TPanel
    object lblCompilerVer: TLabel
      Left = 0
      Top = 0
      Width = 97
      Height = 30
      Margins.Left = 5
      Margins.Top = 5
      Margins.Right = 5
      Margins.Bottom = 5
      Caption = '&Compilers:'
      FocusControl = lbCompilerVers
    end
    object lblCriteria: TLabel
      Left = 294
      Top = 0
      Width = 71
      Height = 30
      Margins.Left = 5
      Margins.Top = 5
      Margins.Right = 5
      Margins.Bottom = 5
      Caption = 'C&riteria:'
      FocusControl = cbCriteria
    end
    object lbCompilerVers: TCheckListBox
      Left = 0
      Top = 33
      Width = 270
      Height = 304
      Margins.Left = 5
      Margins.Top = 5
      Margins.Right = 5
      Margins.Bottom = 5
      IntegralHeight = True
      ItemHeight = 30
      TabOrder = 0
      OnClickCheck = lbCompilerVersClickCheck
    end
    object cbCriteria: TComboBox
      Left = 294
      Top = 33
      Width = 289
      Height = 38
      Margins.Left = 5
      Margins.Top = 5
      Margins.Right = 5
      Margins.Bottom = 5
      Style = csDropDownList
      TabOrder = 3
      OnSelect = cbCriteriaSelect
    end
    object rgLogic: TRadioGroup
      Left = 294
      Top = 103
      Width = 289
      Height = 144
      Margins.Left = 5
      Margins.Top = 5
      Margins.Right = 5
      Margins.Bottom = 5
      Caption = 'Search &logic'
      ItemIndex = 1
      Items.Strings = (
        'Find all compilers'
        'Find any compiler')
      TabOrder = 4
      TabStop = True
    end
    object btnSelectAll: TButton
      Left = 0
      Top = 378
      Width = 114
      Height = 44
      Margins.Left = 5
      Margins.Top = 5
      Margins.Right = 5
      Margins.Bottom = 5
      Caption = 'Select &All'
      TabOrder = 1
      OnClick = btnSelectAllClick
    end
    object btnClearAll: TButton
      Left = 124
      Top = 378
      Width = 114
      Height = 44
      Margins.Left = 5
      Margins.Top = 5
      Margins.Right = 5
      Margins.Bottom = 5
      Caption = 'Cl&ear All'
      TabOrder = 2
      OnClick = btnClearAllClick
    end
    object rgScope: TRadioGroup
      Left = 294
      Top = 278
      Width = 289
      Height = 144
      Margins.Left = 5
      Margins.Top = 5
      Margins.Right = 5
      Margins.Bottom = 5
      Caption = 'Search &scope'
      ItemIndex = 0
      Items.Strings = (
        'Refine existing search'
        'Search whole database')
      TabOrder = 5
      TabStop = True
    end
  end
  inherited btnOK: TButton
    OnClick = btnOKClick
  end
end

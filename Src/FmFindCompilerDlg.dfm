inherited FindCompilerDlg: TFindCompilerDlg
  Left = 268
  Top = 121
  Caption = 'Find Compiler'
  ExplicitWidth = 474
  ExplicitHeight = 354
  PixelsPerInch = 96
  TextHeight = 13
  inherited pnlBody: TPanel
    Width = 249
    Height = 193
    ExplicitWidth = 249
    ExplicitHeight = 193
    object lblCompilerVer: TLabel
      Left = 0
      Top = 32
      Width = 48
      Height = 13
      Caption = '&Compilers:'
      FocusControl = lbCompilerVers
    end
    object lblCriteria: TLabel
      Left = 0
      Top = 4
      Width = 35
      Height = 13
      Caption = 'C&riteria:'
      FocusControl = cbCriteria
    end
    object lbCompilerVers: TCheckListBox
      Left = 66
      Top = 32
      Width = 113
      Height = 82
      OnClickCheck = lbCompilerVersClickCheck
      IntegralHeight = True
      ItemHeight = 13
      TabOrder = 1
    end
    object cbCriteria: TComboBox
      Left = 66
      Top = 0
      Width = 183
      Height = 21
      Style = csDropDownList
      TabOrder = 0
      OnSelect = cbCriteriaSelect
    end
    object rgLogic: TRadioGroup
      Left = 66
      Top = 128
      Width = 183
      Height = 65
      Caption = 'Search logic'
      ItemIndex = 1
      Items.Strings = (
        '&Find all compilers'
        'Fi&nd any compiler')
      TabOrder = 4
      TabStop = True
    end
    object btnSelectAll: TButton
      Left = 184
      Top = 32
      Width = 65
      Height = 25
      Caption = 'Select &All'
      TabOrder = 2
      OnClick = btnSelectAllClick
    end
    object btnClearAll: TButton
      Left = 184
      Top = 60
      Width = 65
      Height = 25
      Caption = 'Cl&ear All'
      TabOrder = 3
      OnClick = btnClearAllClick
    end
  end
  inherited btnOK: TButton
    OnClick = btnOKClick
  end
end

inherited FindCompilerDlg: TFindCompilerDlg
  Left = 268
  Top = 121
  Caption = 'Find Compiler'
  ExplicitWidth = 474
  ExplicitHeight = 356
  PixelsPerInch = 96
  TextHeight = 13
  inherited pnlBody: TPanel
    Width = 378
    Height = 273
    ExplicitWidth = 378
    ExplicitHeight = 273
    object lblCompilerVer: TLabel
      Left = 0
      Top = 0
      Width = 50
      Height = 13
      Caption = '&Compilers:'
      FocusControl = lbCompilerVers
    end
    object lblCriteria: TLabel
      Left = 168
      Top = 0
      Width = 39
      Height = 13
      Caption = 'C&riteria:'
      FocusControl = cbCriteria
    end
    object lbCompilerVers: TCheckListBox
      Left = 0
      Top = 16
      Width = 136
      Height = 186
      OnClickCheck = lbCompilerVersClickCheck
      IntegralHeight = True
      ItemHeight = 13
      TabOrder = 0
    end
    object cbCriteria: TComboBox
      Left = 168
      Top = 19
      Width = 165
      Height = 21
      Style = csDropDownList
      TabOrder = 3
      OnSelect = cbCriteriaSelect
    end
    object rgLogic: TRadioGroup
      Left = 168
      Top = 59
      Width = 165
      Height = 82
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
      Top = 216
      Width = 65
      Height = 25
      Caption = 'Select &All'
      TabOrder = 1
      OnClick = btnSelectAllClick
    end
    object btnClearAll: TButton
      Left = 71
      Top = 216
      Width = 65
      Height = 25
      Caption = 'Cl&ear All'
      TabOrder = 2
      OnClick = btnClearAllClick
    end
    object rgScope: TRadioGroup
      Left = 168
      Top = 159
      Width = 165
      Height = 82
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

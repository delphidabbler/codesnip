inherited SelectionSearchDlg: TSelectionSearchDlg
  Caption = 'Select Snippets'
  ExplicitWidth = 474
  ExplicitHeight = 354
  PixelsPerInch = 96
  TextHeight = 13
  inherited pnlBody: TPanel
    Width = 371
    Height = 293
    ExplicitWidth = 371
    ExplicitHeight = 293
    inline frmSelect: TSelectSnippetsFrame
      Left = 0
      Top = 0
      Width = 281
      Height = 293
      Align = alLeft
      TabOrder = 0
      TabStop = True
      ExplicitWidth = 281
      ExplicitHeight = 293
      inherited tvChecked: TTreeView
        Width = 281
        Height = 293
        ExplicitWidth = 281
        ExplicitHeight = 293
      end
    end
    object btnSelectAll: TButton
      Left = 287
      Top = 1
      Width = 83
      Height = 25
      Caption = '&Select All'
      TabOrder = 1
      OnClick = btnSelectAllClick
    end
    object btnClearAll: TButton
      Left = 287
      Top = 32
      Width = 83
      Height = 25
      Caption = '&Clear All'
      TabOrder = 2
      OnClick = btnClearAllClick
    end
    object btnUserDB: TButton
      Left = 287
      Top = 63
      Width = 83
      Height = 25
      Caption = '&User Defined'
      TabOrder = 3
      OnClick = btnUserDBClick
    end
    object btnMainDB: TButton
      Left = 287
      Top = 94
      Width = 83
      Height = 25
      Caption = '&Main'
      TabOrder = 4
      OnClick = btnMainDBClick
    end
    object btnExpandAll: TButton
      Left = 287
      Top = 174
      Width = 82
      Height = 25
      Caption = 'Expand Tree'
      TabOrder = 5
      OnClick = btnExpandAllClick
    end
    object btnCollapseAll: TButton
      Left = 287
      Top = 205
      Width = 82
      Height = 25
      Caption = 'Collapse Tree'
      TabOrder = 6
      OnClick = btnCollapseAllClick
    end
  end
  inherited btnOK: TButton
    OnClick = btnOKClick
  end
end

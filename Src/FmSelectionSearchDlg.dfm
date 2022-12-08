inherited SelectionSearchDlg: TSelectionSearchDlg
  Caption = 'Select Snippets'
  ExplicitWidth = 979
  ExplicitHeight = 822
  PixelsPerInch = 168
  TextHeight = 30
  inherited pnlBody: TPanel
    object lblOverwriteSearch: TLabel
      AlignWithMargins = True
      Left = 0
      Top = 608
      Width = 957
      Height = 23
      Margins.Left = 0
      Margins.Top = 14
      Margins.Right = 0
      Margins.Bottom = 0
      Align = alBottom
      AutoSize = False
      Caption = 'NOTE: Your selection will replace your current search(es).'
      WordWrap = True
      ExplicitTop = 490
      ExplicitWidth = 649
    end
    inline frmSelect: TSelectSnippetsFrame
      Left = 0
      Top = 0
      Width = 281
      Height = 594
      Margins.Left = 5
      Margins.Top = 5
      Margins.Right = 5
      Margins.Bottom = 5
      Align = alLeft
      TabOrder = 0
      TabStop = True
      ExplicitWidth = 281
      ExplicitHeight = 594
      inherited tvChecked: TTreeView
        Width = 281
        Height = 594
        ExplicitWidth = 281
        ExplicitHeight = 594
      end
    end
    object btnSelectAll: TButton
      Left = 502
      Top = 2
      Width = 146
      Height = 44
      Margins.Left = 5
      Margins.Top = 5
      Margins.Right = 5
      Margins.Bottom = 5
      Caption = '&Select All'
      TabOrder = 1
      OnClick = btnSelectAllClick
    end
    object btnClearAll: TButton
      Left = 502
      Top = 56
      Width = 146
      Height = 44
      Margins.Left = 5
      Margins.Top = 5
      Margins.Right = 5
      Margins.Bottom = 5
      Caption = '&Clear All'
      TabOrder = 2
      OnClick = btnClearAllClick
    end
    object btnUserDB: TButton
      Left = 502
      Top = 110
      Width = 146
      Height = 44
      Margins.Left = 5
      Margins.Top = 5
      Margins.Right = 5
      Margins.Bottom = 5
      Caption = '&User Defined'
      TabOrder = 3
      OnClick = btnUserDBClick
    end
    object btnMainDB: TButton
      Left = 502
      Top = 165
      Width = 146
      Height = 43
      Margins.Left = 5
      Margins.Top = 5
      Margins.Right = 5
      Margins.Bottom = 5
      Caption = '&Main'
      TabOrder = 4
      OnClick = btnMainDBClick
    end
    object btnExpandAll: TButton
      Left = 502
      Top = 305
      Width = 144
      Height = 43
      Margins.Left = 5
      Margins.Top = 5
      Margins.Right = 5
      Margins.Bottom = 5
      Caption = 'E&xpand All'
      TabOrder = 5
      OnClick = btnExpandAllClick
    end
    object btnCollapseAll: TButton
      Left = 502
      Top = 359
      Width = 144
      Height = 44
      Margins.Left = 5
      Margins.Top = 5
      Margins.Right = 5
      Margins.Bottom = 5
      Caption = 'C&ollapse All'
      TabOrder = 6
      OnClick = btnCollapseAllClick
    end
  end
  inherited btnOK: TButton
    OnClick = btnOKClick
  end
end

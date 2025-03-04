inherited SelectionSearchDlg: TSelectionSearchDlg
  Caption = 'Select Snippets'
  ExplicitWidth = 474
  ExplicitHeight = 375
  PixelsPerInch = 96
  TextHeight = 13
  inherited pnlBody: TPanel
    Width = 379
    Height = 293
    ExplicitWidth = 379
    ExplicitHeight = 293
    object lblOverwriteSearch: TLabel
      AlignWithMargins = True
      Left = 0
      Top = 280
      Width = 379
      Height = 13
      Margins.Left = 0
      Margins.Top = 8
      Margins.Right = 0
      Margins.Bottom = 0
      Align = alBottom
      AutoSize = False
      Caption = 'NOTE: Your selection will replace your current search(es).'
      WordWrap = True
      ExplicitWidth = 272
    end
    inline frmSelect: TSelectSnippetsFrame
      Left = 0
      Top = 0
      Width = 281
      Height = 272
      Align = alLeft
      TabOrder = 0
      TabStop = True
      ExplicitWidth = 281
      ExplicitHeight = 272
      inherited tvChecked: TTreeView
        Width = 281
        Height = 272
        ExplicitWidth = 281
        ExplicitHeight = 272
      end
    end
    object btnSelectAll: TButton
      Left = 287
      Top = 1
      Width = 91
      Height = 25
      Caption = '&Select All'
      TabOrder = 1
      OnClick = btnSelectAllClick
    end
    object btnClearAll: TButton
      Left = 287
      Top = 32
      Width = 91
      Height = 25
      Caption = '&Clear All'
      TabOrder = 2
      OnClick = btnClearAllClick
    end
    object btnExpandAll: TButton
      Left = 287
      Top = 174
      Width = 91
      Height = 25
      Caption = 'E&xpand All'
      TabOrder = 4
      OnClick = btnExpandAllClick
    end
    object btnCollapseAll: TButton
      Left = 287
      Top = 205
      Width = 91
      Height = 25
      Caption = 'C&ollapse All'
      TabOrder = 5
      OnClick = btnCollapseAllClick
    end
    object btnVaults: TBitBtn
      Left = 287
      Top = 63
      Width = 91
      Height = 25
      Caption = '&Vault'
      DoubleBuffered = True
      Glyph.Data = {
        F6000000424DF600000000000000760000002800000010000000100000000100
        0400000000008000000000000000000000001000000010000000000000000000
        80000080000000808000800000008000800080800000C0C0C000808080000000
        FF0000FF000000FFFF00FF000000FF00FF00FFFF0000FFFFFF00FFFFFFFFFFFF
        FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF00FFF
        FFFFFFFFFF0000FFFFFFFFFFF000000FFFFFFFFF00000000FFFFFFF000000000
        0FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF
        FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF}
      Layout = blGlyphRight
      ParentDoubleBuffered = False
      TabOrder = 3
      OnClick = btnVaultsClick
    end
  end
  inherited btnOK: TButton
    OnClick = btnOKClick
  end
  object mnuVaults: TPopupMenu
    Left = 72
    Top = 72
  end
end

inherited ListEditorDlg: TListEditorDlg
  Caption = 'ListEditorDlg'
  PixelsPerInch = 96
  TextHeight = 13
  inherited pnlBody: TPanel
    object lblInstructions: TLabel
      Left = 8
      Top = 0
      Width = 112
      Height = 13
      Caption = 'Enter one &item per line:'
      FocusControl = edList
    end
    object edList: TMemo
      Left = 88
      Top = 120
      Width = 185
      Height = 89
      ScrollBars = ssBoth
      TabOrder = 0
      WordWrap = False
    end
  end
end

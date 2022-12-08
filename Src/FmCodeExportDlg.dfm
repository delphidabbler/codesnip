inherited CodeExportDlg: TCodeExportDlg
  Caption = 'Export Snippets'
  ExplicitWidth = 979
  ExplicitHeight = 822
  PixelsPerInch = 168
  TextHeight = 30
  inherited pnlBody: TPanel
    object lblSnippets: TLabel
      Left = 0
      Top = 0
      Width = 285
      Height = 30
      Margins.Left = 5
      Margins.Top = 5
      Margins.Right = 5
      Margins.Bottom = 5
      Caption = 'Select &snippets to be exported:'
      FocusControl = frmSnippets
    end
    object lblFile: TLabel
      Left = 0
      Top = 406
      Width = 396
      Height = 30
      Margins.Left = 5
      Margins.Top = 5
      Margins.Right = 5
      Margins.Bottom = 5
      Caption = 'Save to &file: (click button to browse for file)'
      FocusControl = edFile
    end
    inline frmSnippets: TSelectUserSnippetsFrame
      Left = 0
      Top = 48
      Width = 369
      Height = 348
      Margins.Left = 5
      Margins.Top = 5
      Margins.Right = 5
      Margins.Bottom = 5
      TabOrder = 0
      TabStop = True
      ExplicitTop = 48
      ExplicitWidth = 369
      ExplicitHeight = 348
      inherited tvChecked: TTreeView
        Width = 369
        Height = 348
        ExplicitTop = 22
        ExplicitWidth = 369
        ExplicitHeight = 201
      end
    end
    object edFile: TEdit
      Left = 0
      Top = 439
      Width = 581
      Height = 38
      Margins.Left = 5
      Margins.Top = 5
      Margins.Right = 5
      Margins.Bottom = 5
      TabOrder = 1
    end
    object btnBrowse: TButton
      Left = 592
      Top = 439
      Width = 47
      Height = 37
      Margins.Left = 5
      Margins.Top = 5
      Margins.Right = 5
      Margins.Bottom = 5
      Caption = '...'
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clWindowText
      Font.Height = -19
      Font.Name = 'Tahoma'
      Font.Style = [fsBold]
      ParentFont = False
      TabOrder = 2
      OnClick = btnBrowseClick
    end
  end
  inherited btnOK: TButton
    OnClick = btnOKClick
  end
end

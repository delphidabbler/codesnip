inherited CodeExportDlg: TCodeExportDlg
  Caption = 'Export Snippets'
  ExplicitWidth = 474
  ExplicitHeight = 354
  PixelsPerInch = 96
  TextHeight = 13
  inherited pnlBody: TPanel
    Height = 277
    ExplicitHeight = 277
    object lblSnippets: TLabel
      Left = 0
      Top = 0
      Width = 151
      Height = 13
      Caption = 'Select &snippets to be exported:'
      FocusControl = frmSnippets
    end
    object lblFile: TLabel
      Left = 0
      Top = 232
      Width = 208
      Height = 13
      Caption = 'Save to &file: (click button to browse for file)'
      FocusControl = edFile
    end
    inline frmSnippets: TSelectUserSnippetsFrame
      Left = 0
      Top = 18
      Width = 369
      Height = 201
      TabOrder = 0
      TabStop = True
      ExplicitTop = 18
      ExplicitWidth = 369
      ExplicitHeight = 201
      inherited tvChecked: TTreeView
        Width = 369
        Height = 201
        ExplicitWidth = 369
        ExplicitHeight = 201
      end
    end
    object edFile: TEdit
      Left = 0
      Top = 251
      Width = 332
      Height = 21
      TabOrder = 1
    end
    object btnBrowse: TButton
      Left = 338
      Top = 251
      Width = 27
      Height = 21
      Caption = '...'
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clWindowText
      Font.Height = -11
      Font.Name = 'Tahoma'
      Font.Style = [fsBold]
      ParentFont = False
      TabOrder = 2
      OnClick = btnBrowseClick
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

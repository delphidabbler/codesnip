inherited CollectionBackupDlg: TCollectionBackupDlg
  Caption = 'Choose Collection & Backup File'
  ExplicitWidth = 474
  ExplicitHeight = 375
  PixelsPerInch = 96
  TextHeight = 13
  inherited pnlBody: TPanel
    object lblCollection: TLabel
      Left = 0
      Top = 7
      Width = 80
      Height = 13
      Caption = 'Select &collection:'
      FocusControl = cbCollection
    end
    object lblPath: TLabel
      Left = 0
      Top = 61
      Width = 180
      Height = 13
      Caption = 'Enter the full path of the back up &file:'
      FocusControl = edPath
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clWindowText
      Font.Height = -11
      Font.Name = 'Tahoma'
      Font.Style = []
      ParentFont = False
    end
    object cbCollection: TComboBox
      Left = 0
      Top = 24
      Width = 358
      Height = 21
      Style = csDropDownList
      TabOrder = 0
    end
    object edPath: TEdit
      Left = 0
      Top = 80
      Width = 325
      Height = 21
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clWindowText
      Font.Height = -11
      Font.Name = 'Tahoma'
      Font.Style = []
      ParentFont = False
      TabOrder = 1
    end
    object btnBrowse: TButton
      Left = 331
      Top = 80
      Width = 27
      Height = 21
      Caption = '...'
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clWindowText
      Font.Height = -11
      Font.Name = 'Tahoma'
      Font.Style = []
      ParentFont = False
      TabOrder = 2
      OnClick = btnBrowseClick
    end
  end
  inherited btnOK: TButton
    OnClick = btnOKClick
  end
end

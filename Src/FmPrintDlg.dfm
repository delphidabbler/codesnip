inherited PrintDlg: TPrintDlg
  Caption = 'Print'
  PixelsPerInch = 168
  TextHeight = 30
  inherited pnlBody: TPanel
    object gpOptions: TGroupBox
      Left = 0
      Top = 156
      Width = 366
      Height = 122
      Margins.Left = 5
      Margins.Top = 5
      Margins.Right = 5
      Margins.Bottom = 5
      Caption = ' Document Formatting Options '
      TabOrder = 1
      object chkSyntaxPrint: TCheckBox
        Left = 14
        Top = 35
        Width = 336
        Height = 30
        Margins.Left = 5
        Margins.Top = 5
        Margins.Right = 5
        Margins.Bottom = 5
        Caption = '&Syntax highlight source code'
        TabOrder = 0
      end
      object chkUseColor: TCheckBox
        Left = 14
        Top = 75
        Width = 336
        Height = 30
        Margins.Left = 5
        Margins.Top = 5
        Margins.Right = 5
        Margins.Bottom = 5
        Caption = 'Use &colour'
        TabOrder = 1
      end
    end
    object gpPrinter: TGroupBox
      Left = 0
      Top = 0
      Width = 618
      Height = 145
      Margins.Left = 5
      Margins.Top = 5
      Margins.Right = 5
      Margins.Bottom = 5
      Caption = ' Printer '
      TabOrder = 0
      object lblPrinters: TLabel
        Left = 14
        Top = 32
        Width = 159
        Height = 30
        Margins.Left = 5
        Margins.Top = 5
        Margins.Right = 5
        Margins.Bottom = 5
        Caption = '&Installed printers:'
        FocusControl = cbPrinters
      end
      object cbPrinters: TComboBox
        Left = 14
        Top = 65
        Width = 422
        Height = 43
        Margins.Left = 5
        Margins.Top = 5
        Margins.Right = 5
        Margins.Bottom = 5
        Style = csOwnerDrawFixed
        ItemHeight = 37
        TabOrder = 0
        OnDrawItem = cbPrintersDrawItem
      end
      object btnSetup: TButton
        Left = 459
        Top = 67
        Width = 145
        Height = 43
        Margins.Left = 5
        Margins.Top = 5
        Margins.Right = 5
        Margins.Bottom = 5
        Caption = 'Page Set&up...'
        TabOrder = 1
        OnClick = btnSetupClick
      end
    end
  end
  inherited btnHelp: TButton
    TabOrder = 4
  end
  inherited btnCancel: TButton
    TabOrder = 3
  end
  inherited btnOK: TButton
    TabOrder = 2
    OnClick = btnOKClick
  end
  object btnPreferences: TButton
    Left = 14
    Top = 532
    Width = 156
    Height = 44
    Margins.Left = 5
    Margins.Top = 5
    Margins.Right = 5
    Margins.Bottom = 5
    Caption = 'P&references...'
    TabOrder = 1
    OnClick = btnPrefencesClick
  end
  object ilPrinters: TImageList
    Left = 442
    Top = 186
  end
end

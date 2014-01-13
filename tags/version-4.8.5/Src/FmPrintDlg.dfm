inherited PrintDlg: TPrintDlg
  Left = 218
  Top = 126
  Caption = 'Print'
  ClientHeight = 363
  ClientWidth = 462
  ExplicitWidth = 468
  ExplicitHeight = 389
  PixelsPerInch = 96
  TextHeight = 13
  inherited pnlBody: TPanel
    Width = 353
    Height = 167
    ExplicitWidth = 353
    ExplicitHeight = 167
    object gpOptions: TGroupBox
      Left = 0
      Top = 89
      Width = 209
      Height = 70
      Caption = ' Document Formatting Options '
      TabOrder = 1
      object chkSyntaxPrint: TCheckBox
        Left = 8
        Top = 20
        Width = 192
        Height = 17
        Caption = '&Syntax highlight source code'
        TabOrder = 0
      end
      object chkUseColor: TCheckBox
        Left = 8
        Top = 43
        Width = 192
        Height = 17
        Caption = 'Use &colour'
        TabOrder = 1
      end
    end
    object gpPrinter: TGroupBox
      Left = 0
      Top = 0
      Width = 353
      Height = 83
      Caption = ' Printer '
      TabOrder = 0
      object lblPrinters: TLabel
        Left = 8
        Top = 18
        Width = 79
        Height = 13
        Caption = '&Installed printers:'
        FocusControl = cbPrinters
      end
      object cbPrinters: TComboBox
        Left = 8
        Top = 37
        Width = 241
        Height = 27
        Style = csOwnerDrawFixed
        ItemHeight = 21
        TabOrder = 0
        OnDrawItem = cbPrintersDrawItem
      end
      object btnSetup: TButton
        Left = 262
        Top = 38
        Width = 83
        Height = 25
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
    Left = 8
    Top = 304
    Width = 89
    Height = 25
    Caption = 'P&references...'
    TabOrder = 1
    OnClick = btnPrefencesClick
  end
  object ilPrinters: TImageList
    Left = 8
    Top = 200
  end
end

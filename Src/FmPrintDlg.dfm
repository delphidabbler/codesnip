inherited PrintDlg: TPrintDlg
  Left = 218
  Top = 126
  Caption = 'Print'
  ClientHeight = 633
  ClientWidth = 818
  Font.Height = -200
  ExplicitWidth = 842
  ExplicitHeight = 697
  PixelsPerInch = 168
  TextHeight = 265
  inherited bvlBottom: TBevel
    Left = 77
    Top = 2777
    Width = 3537
    Height = 23
    Margins.Left = 28
    Margins.Top = 28
    Margins.Right = 28
    Margins.Bottom = 28
    ExplicitLeft = 77
    ExplicitTop = 2777
    ExplicitWidth = 3537
    ExplicitHeight = 23
  end
  inherited pnlBody: TPanel
    Left = 77
    Top = 77
    Width = 618
    Height = 292
    Margins.Left = 28
    Margins.Top = 28
    Margins.Right = 28
    Margins.Bottom = 28
    ExplicitLeft = 77
    ExplicitTop = 77
    ExplicitWidth = 618
    ExplicitHeight = 292
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
        Width = 1515
        Height = 265
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
        Height = 27
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
    Left = 2928
    Top = 2851
    Width = 702
    Height = 236
    Margins.Left = 28
    Margins.Top = 28
    Margins.Right = 28
    Margins.Bottom = 28
    TabOrder = 4
    ExplicitLeft = 2928
    ExplicitTop = 2851
    ExplicitWidth = 702
    ExplicitHeight = 236
  end
  inherited btnCancel: TButton
    Left = 711
    Top = 931
    Width = 229
    Height = 77
    Margins.Left = 9
    Margins.Top = 9
    Margins.Right = 9
    Margins.Bottom = 9
    TabOrder = 3
    ExplicitLeft = 711
    ExplicitTop = 931
    ExplicitWidth = 229
    ExplicitHeight = 77
  end
  inherited btnOK: TButton
    Left = 469
    Top = 931
    Width = 229
    Height = 77
    Margins.Left = 9
    Margins.Top = 9
    Margins.Right = 9
    Margins.Bottom = 9
    TabOrder = 2
    OnClick = btnOKClick
    ExplicitLeft = 469
    ExplicitTop = 931
    ExplicitWidth = 229
    ExplicitHeight = 77
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
    Left = 8
    Top = 200
  end
end

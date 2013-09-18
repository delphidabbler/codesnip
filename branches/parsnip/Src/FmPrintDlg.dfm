inherited PrintDlg: TPrintDlg
  Left = 218
  Top = 126
  Caption = 'Print'
  ClientHeight = 363
  ClientWidth = 462
  ExplicitWidth = 468
  ExplicitHeight = 391
  PixelsPerInch = 96
  TextHeight = 13
  inherited pnlBody: TPanel
    Width = 414
    Height = 273
    ExplicitWidth = 414
    ExplicitHeight = 273
    object gpPrinter: TGroupBox
      Left = 0
      Top = 0
      Width = 413
      Height = 83
      Anchors = [akLeft, akTop, akRight]
      Caption = ' Printer '
      TabOrder = 0
      DesignSize = (
        413
        83)
      object lblPrinters: TLabel
        Left = 8
        Top = 18
        Width = 85
        Height = 13
        Caption = '&Installed printers:'
        FocusControl = cbPrinters
      end
      object cbPrinters: TComboBox
        Left = 8
        Top = 37
        Width = 300
        Height = 27
        Style = csOwnerDrawFixed
        Anchors = [akLeft, akTop, akRight]
        ItemHeight = 21
        TabOrder = 0
        OnDrawItem = cbPrintersDrawItem
      end
      object btnSetup: TButton
        Left = 320
        Top = 38
        Width = 83
        Height = 25
        Anchors = [akTop, akRight]
        Caption = 'Page Set&up...'
        TabOrder = 1
        OnClick = btnSetupClick
      end
    end
    object gpFormatOptions: TGroupBox
      Left = 0
      Top = 89
      Width = 414
      Height = 153
      Anchors = [akLeft, akTop, akRight]
      Caption = ' Document Formatting Options '
      TabOrder = 1
      DesignSize = (
        414
        153)
      object chkSyntaxHighlight: TCheckBox
        Left = 8
        Top = 20
        Width = 195
        Height = 17
        Caption = '&Syntax highlight source code'
        TabOrder = 0
        OnClick = FormatCheckBoxClick
      end
      object chkUseColor: TCheckBox
        Left = 8
        Top = 44
        Width = 195
        Height = 17
        Caption = 'Use &colour'
        TabOrder = 1
        OnClick = FormatCheckBoxClick
      end
      inline frmPreview: TRTFShowCaseFrame
        Left = 205
        Top = 15
        Width = 199
        Height = 127
        Anchors = [akTop, akRight]
        TabOrder = 2
        ExplicitLeft = 205
        ExplicitTop = 15
        ExplicitWidth = 199
        ExplicitHeight = 127
        inherited reView: TRichEdit
          Width = 199
          Height = 127
          ExplicitWidth = 199
          ExplicitHeight = 127
        end
      end
    end
  end
  inherited btnOK: TButton
    OnClick = btnOKClick
  end
  object ilPrinters: TImageList
    Left = 120
    Top = 304
  end
end

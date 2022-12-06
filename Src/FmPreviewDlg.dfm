inherited PreviewDlg: TPreviewDlg
  Left = 227
  Top = 147
  Caption = 'Preview'
  ClientHeight = 1844
  ClientWidth = 1101
  Font.Height = -200
  OnClose = FormClose
  ExplicitWidth = 1125
  ExplicitHeight = 1908
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
    Width = 1054
    Height = 2637
    Margins.Left = 28
    Margins.Top = 28
    Margins.Right = 28
    Margins.Bottom = 28
    ExplicitLeft = 77
    ExplicitTop = 77
    ExplicitWidth = 1054
    ExplicitHeight = 2637
    object pcViews: TPageControl
      Left = 0
      Top = 0
      Width = 1054
      Height = 2637
      Margins.Left = 5
      Margins.Top = 5
      Margins.Right = 5
      Margins.Bottom = 5
      Align = alClient
      Style = tsButtons
      TabOrder = 0
      object tsText: TTabSheet
        Margins.Left = 5
        Margins.Top = 5
        Margins.Right = 5
        Margins.Bottom = 5
        Caption = 'tsText'
        TabVisible = False
        inline frText: TTextPreviewFrame
          Left = 0
          Top = 0
          Width = 594
          Height = 271
          Margins.Left = 5
          Margins.Top = 5
          Margins.Right = 5
          Margins.Bottom = 5
          Align = alClient
          Font.Charset = DEFAULT_CHARSET
          Font.Color = clWindowText
          Font.Height = -21
          Font.Name = 'Segoe UI'
          Font.Style = []
          ParentFont = False
          TabOrder = 0
          TabStop = True
          ExplicitWidth = 594
          ExplicitHeight = 271
          inherited pnlView: TPanel
            Width = 594
            Height = 271
            ExplicitWidth = 594
            ExplicitHeight = 271
            inherited edDisplay: TMemo
              Width = 592
              Height = 269
              ExplicitWidth = 592
              ExplicitHeight = 269
            end
          end
        end
      end
      object tsRTF: TTabSheet
        Margins.Left = 5
        Margins.Top = 5
        Margins.Right = 5
        Margins.Bottom = 5
        Caption = 'tsRTF'
        ImageIndex = 1
        TabVisible = False
        inline frRTF: TRTFPreviewFrame
          Left = 0
          Top = 0
          Width = 1046
          Height = 2627
          Margins.Left = 5
          Margins.Top = 5
          Margins.Right = 5
          Margins.Bottom = 5
          Align = alClient
          Font.Charset = DEFAULT_CHARSET
          Font.Color = clWindowText
          Font.Height = -21
          Font.Name = 'Segoe UI'
          Font.Style = []
          ParentFont = False
          TabOrder = 0
          TabStop = True
          ExplicitWidth = 594
          ExplicitHeight = 1497
          inherited pnlView: TPanel
            Width = 1046
            Height = 2627
            ExplicitWidth = 594
            ExplicitHeight = 1497
            inherited reView: TRichEdit
              Width = 1044
              Height = 2625
              ExplicitWidth = 1044
              ExplicitHeight = 2625
            end
          end
        end
      end
      object tsHTML: TTabSheet
        Margins.Left = 5
        Margins.Top = 5
        Margins.Right = 5
        Margins.Bottom = 5
        Caption = 'tsHTML'
        ImageIndex = 2
        TabVisible = False
        inline frHTML: THTMLPreviewFrame
          Left = 0
          Top = 0
          Width = 594
          Height = 271
          Margins.Left = 5
          Margins.Top = 5
          Margins.Right = 5
          Margins.Bottom = 5
          Align = alClient
          Font.Charset = DEFAULT_CHARSET
          Font.Color = clWindowText
          Font.Height = -21
          Font.Name = 'Segoe UI'
          Font.Style = []
          ParentFont = False
          TabOrder = 0
          TabStop = True
          ExplicitWidth = 594
          ExplicitHeight = 271
          inherited pnlBrowser: TPanel
            Width = 594
            Height = 271
            ExplicitWidth = 594
            ExplicitHeight = 271
            inherited wbBrowser: TWebBrowser
              Width = 592
              Height = 269
              ExplicitWidth = 592
              ExplicitHeight = 269
              ControlData = {
                4C0000002F3D0000CD1B00000000000000000000000000000000000000000000
                000000004C000000000000000000000001000000E0D057007335CF11AE690800
                2B2E126208000000000000004C0000000114020000000000C000000000000046
                8000000000000000000000000000000000000000000000000000000000000000
                00000000000000000100000000000000000000000000000000000000}
            end
          end
        end
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
    Visible = False
    ExplicitLeft = 2928
    ExplicitTop = 2851
    ExplicitWidth = 702
    ExplicitHeight = 236
  end
  inherited btnClose: TButton
    Left = 711
    Top = 931
    Width = 229
    Height = 77
    Margins.Left = 9
    Margins.Top = 9
    Margins.Right = 9
    Margins.Bottom = 9
    ExplicitLeft = 711
    ExplicitTop = 931
    ExplicitWidth = 229
    ExplicitHeight = 77
  end
  object mnuPreview: TPopupMenu
    Left = 32
    Top = 296
    object miCopy: TMenuItem
      Action = actCopy
    end
    object miSelectAll: TMenuItem
      Action = actSelectAll
    end
  end
  object alPreview: TActionList
    Left = 72
    Top = 296
    object actCopy: TAction
      Caption = '&Copy'
      ShortCut = 16451
      OnExecute = actCopyExecute
      OnUpdate = actCopyUpdate
    end
    object actSelectAll: TAction
      Caption = 'Select All'
      ShortCut = 16449
      OnExecute = actSelectAllExecute
      OnUpdate = actSelectAllUpdate
    end
  end
end

inherited PreviewDlg: TPreviewDlg
  Caption = 'Preview'
  OnClose = FormClose
  ExplicitWidth = 991
  ExplicitHeight = 822
  PixelsPerInch = 168
  TextHeight = 30
  inherited pnlBody: TPanel
    object pcViews: TPageControl
      Left = 0
      Top = 0
      Width = 957
      Height = 631
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
          Width = 949
          Height = 621
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
          ExplicitWidth = 949
          ExplicitHeight = 621
          inherited pnlView: TPanel
            Width = 949
            Height = 621
            ExplicitWidth = 949
            ExplicitHeight = 621
            inherited reView: TRichEdit
              Width = 947
              Height = 619
              ExplicitWidth = 947
              ExplicitHeight = 619
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

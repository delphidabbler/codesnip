inherited PreviewDlg: TPreviewDlg
  Left = 227
  Top = 147
  Caption = 'Preview'
  ClientWidth = 624
  OnClose = FormClose
  ExplicitWidth = 630
  ExplicitHeight = 354
  PixelsPerInch = 96
  TextHeight = 13
  inherited pnlBody: TPanel
    Width = 602
    ExplicitWidth = 602
    object pcViews: TPageControl
      Left = 0
      Top = 0
      Width = 602
      Height = 281
      Align = alClient
      Style = tsButtons
      TabOrder = 0
      object tsText: TTabSheet
        Caption = 'tsText'
        TabVisible = False
        inline frText: TTextPreviewFrame
          Left = 0
          Top = 0
          Width = 594
          Height = 271
          Align = alClient
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
        Caption = 'tsRTF'
        ImageIndex = 1
        TabVisible = False
        inline frRTF: TRTFPreviewFrame
          Left = 0
          Top = 0
          Width = 594
          Height = 271
          Align = alClient
          TabOrder = 0
          TabStop = True
          ExplicitWidth = 594
          ExplicitHeight = 271
          inherited pnlView: TPanel
            Width = 594
            Height = 271
            ExplicitWidth = 594
            ExplicitHeight = 271
            inherited reView: TRichEdit
              Width = 592
              Height = 269
              ExplicitWidth = 592
              ExplicitHeight = 269
            end
          end
        end
      end
      object tsHTML: TTabSheet
        Caption = 'tsHTML'
        ImageIndex = 2
        TabVisible = False
        inline frHTML: THTMLPreviewFrame
          Left = 0
          Top = 0
          Width = 594
          Height = 271
          Align = alClient
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
    Visible = False
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

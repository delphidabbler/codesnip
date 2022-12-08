inherited CompilersDlg: TCompilersDlg
  Caption = 'Configure Compilers'
  ExplicitWidth = 979
  ExplicitHeight = 822
  PixelsPerInch = 168
  TextHeight = 30
  inherited pnlBody: TPanel
    object pbBanner: TPaintBox
      Left = 296
      Top = 0
      Width = 647
      Height = 40
      Margins.Left = 5
      Margins.Top = 5
      Margins.Right = 5
      Margins.Bottom = 5
      Color = clActiveCaption
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clCaptionText
      Font.Height = -21
      Font.Name = 'Tahoma'
      Font.Style = [fsBold]
      ParentColor = False
      ParentFont = False
    end
    object lbCompilers: TListBox
      Left = 0
      Top = 0
      Width = 285
      Height = 508
      Margins.Left = 5
      Margins.Top = 5
      Margins.Right = 5
      Margins.Bottom = 5
      Style = lbOwnerDrawFixed
      Ctl3D = True
      IntegralHeight = True
      ItemHeight = 63
      ParentCtl3D = False
      TabOrder = 0
    end
    object pcCompiler: TPageControl
      Left = 296
      Top = 51
      Width = 647
      Height = 460
      Margins.Left = 5
      Margins.Top = 5
      Margins.Right = 5
      Margins.Bottom = 5
      ActivePage = tsCompiler
      TabOrder = 1
      OnMouseDown = pcCompilerMouseDown
      object tsCompiler: TTabSheet
        Margins.Left = 5
        Margins.Top = 5
        Margins.Right = 5
        Margins.Bottom = 5
        Caption = 'Compiler'
        inline frmCompiler: TCompilersDlgCompilerFrame
          Left = 0
          Top = 0
          Width = 639
          Height = 415
          Margins.Left = 5
          Margins.Top = 5
          Margins.Right = 5
          Margins.Bottom = 5
          Align = alClient
          TabOrder = 0
          ExplicitWidth = 639
          ExplicitHeight = 415
          inherited edCompilerPath: TEdit
            Width = 597
            ExplicitWidth = 597
          end
          inherited btnBrowse: TButton
            Left = 608
            ExplicitLeft = 608
          end
        end
      end
      object tsSwitches: TTabSheet
        Margins.Left = 5
        Margins.Top = 5
        Margins.Right = 5
        Margins.Bottom = 5
        Caption = 'Switches'
        ImageIndex = 2
        inline frmSwitches: TCompilersDlgSwitchesFrame
          Left = 0
          Top = 0
          Width = 639
          Height = 415
          Margins.Left = 5
          Margins.Top = 5
          Margins.Right = 5
          Margins.Bottom = 5
          Align = alClient
          TabOrder = 0
          ExplicitWidth = 639
          ExplicitHeight = 415
          inherited lblExplainSwitches: TLabel
            Left = 498
            ExplicitLeft = 498
          end
          inherited btnDefSwitches: TButton
            Left = 570
            ExplicitLeft = 570
          end
          inherited btnAdd: TButton
            Left = 499
            ExplicitLeft = 499
          end
          inherited btnReplace: TButton
            Left = 570
            ExplicitLeft = 570
          end
          inherited btnDelete: TButton
            Left = 499
            ExplicitLeft = 499
          end
        end
      end
      object tsNamespaces: TTabSheet
        Margins.Left = 5
        Margins.Top = 5
        Margins.Right = 5
        Margins.Bottom = 5
        Caption = 'Namespaces'
        ImageIndex = 4
        inline frmNamespaces: TCompilersDlgNamespacesFrame
          Left = 0
          Top = 0
          Width = 639
          Height = 415
          Margins.Left = 5
          Margins.Top = 5
          Margins.Right = 5
          Margins.Bottom = 5
          Align = alClient
          TabOrder = 0
          ExplicitWidth = 639
          ExplicitHeight = 415
          inherited lblExplainNamespaces: TLabel
            Left = 498
            ExplicitLeft = 498
          end
          inherited btnDefSwitches: TButton
            Left = 570
            ExplicitLeft = 570
          end
          inherited btnAdd: TButton
            Left = 499
            ExplicitLeft = 499
          end
          inherited btnReplace: TButton
            Left = 570
            ExplicitLeft = 570
          end
          inherited btnDelete: TButton
            Left = 499
            ExplicitLeft = 499
          end
        end
      end
      object tsSearchDirs: TTabSheet
        Margins.Left = 5
        Margins.Top = 5
        Margins.Right = 5
        Margins.Bottom = 5
        Caption = 'Search Paths'
        ImageIndex = 3
        inline frmSearchDirs: TCompilersDlgSearchDirsFrame
          Left = 0
          Top = 0
          Width = 639
          Height = 415
          Margins.Left = 5
          Margins.Top = 5
          Margins.Right = 5
          Margins.Bottom = 5
          Align = alClient
          TabOrder = 0
          ExplicitWidth = 639
          ExplicitHeight = 415
          inherited btnUp: TSpeedButton
            Left = 612
            ExplicitLeft = 612
          end
          inherited btnDown: TSpeedButton
            Left = 612
            ExplicitLeft = 612
          end
          inherited lbPaths: TListBox
            Width = 599
            ExplicitWidth = 599
          end
          inherited edPath: TEdit
            Width = 599
            ExplicitWidth = 599
          end
          inherited btnBrowse: TButton
            Left = 608
            ExplicitLeft = 608
          end
        end
      end
      object tsLog: TTabSheet
        Margins.Left = 5
        Margins.Top = 5
        Margins.Right = 5
        Margins.Bottom = 5
        Caption = 'Output Log'
        ImageIndex = 1
        inline frmLog: TCompilersDlgLogFrame
          Left = 0
          Top = 0
          Width = 639
          Height = 415
          Margins.Left = 5
          Margins.Top = 5
          Margins.Right = 5
          Margins.Bottom = 5
          Align = alClient
          TabOrder = 0
          ExplicitWidth = 639
          ExplicitHeight = 415
          inherited vleLogPrefixes: TValueListEditor
            Width = 631
            ExplicitWidth = 631
          end
        end
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
  object btnDetect: TButton
    Left = 14
    Top = 532
    Width = 268
    Height = 44
    Margins.Left = 5
    Margins.Top = 5
    Margins.Right = 5
    Margins.Bottom = 5
    Caption = '&Detect Delphi Compilers'
    TabOrder = 1
    OnClick = btnDetectClick
  end
end

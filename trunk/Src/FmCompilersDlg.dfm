inherited CompilersDlg: TCompilersDlg
  Caption = 'Configure Compilers'
  ClientHeight = 362
  ClientWidth = 471
  ExplicitWidth = 477
  ExplicitHeight = 388
  PixelsPerInch = 96
  TextHeight = 13
  inherited pnlBody: TPanel
    Width = 449
    Height = 249
    ExplicitWidth = 449
    ExplicitHeight = 249
    object pbBanner: TPaintBox
      Left = 128
      Top = 0
      Width = 321
      Height = 23
      Color = clActiveCaption
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clCaptionText
      Font.Height = -11
      Font.Name = 'MS Sans Serif'
      Font.Style = [fsBold]
      ParentColor = False
      ParentFont = False
    end
    object lbCompilers: TListBox
      Left = 0
      Top = 0
      Width = 121
      Height = 244
      Style = lbOwnerDrawFixed
      Ctl3D = True
      IntegralHeight = True
      ItemHeight = 48
      ParentCtl3D = False
      TabOrder = 0
    end
    object pcCompiler: TPageControl
      Left = 127
      Top = 29
      Width = 321
      Height = 212
      ActivePage = tsCompiler
      TabOrder = 1
      OnMouseDown = pcCompilerMouseDown
      object tsCompiler: TTabSheet
        Caption = 'Compiler'
        inline frmCompiler: TCompilersDlgCompilerFrame
          Left = 0
          Top = 0
          Width = 313
          Height = 184
          Align = alClient
          TabOrder = 0
          ExplicitHeight = 184
          inherited lblCompilerPath: TLabel
            Width = 170
            ExplicitWidth = 170
          end
        end
      end
      object tsSwitches: TTabSheet
        Caption = 'Switches'
        ImageIndex = 2
        inline frmSwitches: TCompilersDlgSwitchesFrame
          Left = 0
          Top = 0
          Width = 313
          Height = 184
          Align = alClient
          TabOrder = 0
          ExplicitHeight = 184
          inherited lblSwitch: TLabel
            Width = 87
            ExplicitWidth = 87
          end
        end
      end
      object tsSearchDirs: TTabSheet
        Caption = 'Search Paths'
        ImageIndex = 3
        inline frmSearchDirs: TCompilersDlgSearchDirsFrame
          Left = 0
          Top = 0
          Width = 313
          Height = 184
          Align = alClient
          TabOrder = 0
          ExplicitHeight = 184
          inherited lblPaths: TLabel
            Width = 69
            ExplicitWidth = 69
          end
          inherited lblPath: TLabel
            Width = 78
            ExplicitWidth = 78
          end
        end
      end
      object tsLog: TTabSheet
        Caption = 'Output Log'
        ImageIndex = 1
        ExplicitLeft = 0
        ExplicitTop = 0
        ExplicitWidth = 0
        ExplicitHeight = 0
        inline frmLog: TCompilersDlgLogFrame
          Left = 0
          Top = 0
          Width = 313
          Height = 184
          Align = alClient
          TabOrder = 0
          ExplicitHeight = 184
          inherited lblLogPrefixes: TLabel
            Width = 77
            ExplicitWidth = 77
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
    Left = 8
    Top = 304
    Width = 153
    Height = 25
    Caption = '&Detect Delphi Compilers'
    TabOrder = 1
    OnClick = btnDetectClick
  end
end

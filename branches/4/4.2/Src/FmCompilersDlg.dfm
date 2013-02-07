inherited CompilersDlg: TCompilersDlg
  Caption = 'Configure Compilers'
  ClientHeight = 362
  ClientWidth = 471
  ExplicitWidth = 477
  ExplicitHeight = 390
  PixelsPerInch = 96
  TextHeight = 13
  inherited pnlBody: TPanel
    Width = 449
    Height = 260
    ExplicitWidth = 449
    ExplicitHeight = 260
    object pbBanner: TPaintBox
      Left = 128
      Top = 0
      Width = 321
      Height = 23
      Color = clActiveCaption
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clCaptionText
      Font.Height = -12
      Font.Name = 'Tahoma'
      Font.Style = [fsBold]
      ParentColor = False
      ParentFont = False
    end
    object lbCompilers: TListBox
      Left = 0
      Top = 0
      Width = 121
      Height = 256
      Style = lbOwnerDrawFixed
      Ctl3D = True
      IntegralHeight = True
      ItemHeight = 36
      ParentCtl3D = False
      TabOrder = 0
    end
    object pcCompiler: TPageControl
      Left = 127
      Top = 29
      Width = 321
      Height = 227
      ActivePage = tsCompiler
      TabOrder = 1
      OnMouseDown = pcCompilerMouseDown
      object tsCompiler: TTabSheet
        Caption = 'Compiler'
        inline frmCompiler: TCompilersDlgCompilerFrame
          Left = 0
          Top = 0
          Width = 313
          Height = 199
          Align = alClient
          TabOrder = 0
          ExplicitHeight = 199
        end
      end
      object tsSwitches: TTabSheet
        Caption = 'Switches'
        ImageIndex = 2
        inline frmSwitches: TCompilersDlgSwitchesFrame
          Left = 0
          Top = 0
          Width = 313
          Height = 199
          Align = alClient
          TabOrder = 0
          ExplicitHeight = 199
        end
      end
      object tsSearchDirs: TTabSheet
        Caption = 'Search Paths'
        ImageIndex = 3
        inline frmSearchDirs: TCompilersDlgSearchDirsFrame
          Left = 0
          Top = 0
          Width = 313
          Height = 199
          Align = alClient
          TabOrder = 0
          ExplicitHeight = 199
        end
      end
      object tsLog: TTabSheet
        Caption = 'Output Log'
        ImageIndex = 1
        inline frmLog: TCompilersDlgLogFrame
          Left = 0
          Top = 0
          Width = 313
          Height = 199
          Align = alClient
          TabOrder = 0
          ExplicitHeight = 199
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

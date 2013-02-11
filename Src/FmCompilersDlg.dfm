inherited CompilersDlg: TCompilersDlg
  Caption = 'Configure Compilers'
  ClientHeight = 381
  ClientWidth = 524
  ExplicitWidth = 530
  ExplicitHeight = 409
  PixelsPerInch = 96
  TextHeight = 13
  inherited pnlBody: TPanel
    Width = 497
    ExplicitWidth = 497
    object pbBanner: TPaintBox
      Left = 128
      Top = 0
      Width = 369
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
      Height = 292
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
      Width = 370
      Height = 263
      ActivePage = tsCompiler
      TabOrder = 1
      OnMouseDown = pcCompilerMouseDown
      object tsCompiler: TTabSheet
        Caption = 'Compiler'
        ExplicitWidth = 313
        ExplicitHeight = 199
        inline frmCompiler: TCompilersDlgCompilerFrame
          Left = 0
          Top = 0
          Width = 362
          Height = 235
          Align = alClient
          TabOrder = 0
          ExplicitHeight = 199
        end
      end
      object tsSwitches: TTabSheet
        Caption = 'Switches'
        ImageIndex = 2
        ExplicitWidth = 313
        ExplicitHeight = 199
        inline frmSwitches: TCompilersDlgSwitchesFrame
          Left = 0
          Top = 0
          Width = 362
          Height = 235
          Align = alClient
          TabOrder = 0
          ExplicitHeight = 199
        end
      end
      object tsNamespaces: TTabSheet
        Caption = 'Namespaces'
        ImageIndex = 4
        ExplicitWidth = 313
        ExplicitHeight = 199
        inline frmNamespaces: TCompilersDlgNamespacesFrame
          Left = 0
          Top = 0
          Width = 362
          Height = 235
          Align = alClient
          TabOrder = 0
          ExplicitWidth = 313
          ExplicitHeight = 199
        end
      end
      object tsSearchDirs: TTabSheet
        Caption = 'Search Paths'
        ImageIndex = 3
        ExplicitWidth = 313
        ExplicitHeight = 199
        inline frmSearchDirs: TCompilersDlgSearchDirsFrame
          Left = 0
          Top = 0
          Width = 362
          Height = 235
          Align = alClient
          TabOrder = 0
          ExplicitHeight = 199
        end
      end
      object tsLog: TTabSheet
        Caption = 'Output Log'
        ImageIndex = 1
        ExplicitWidth = 313
        ExplicitHeight = 199
        inline frmLog: TCompilersDlgLogFrame
          Left = 0
          Top = 0
          Width = 362
          Height = 235
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
